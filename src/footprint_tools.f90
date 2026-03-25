! ============================================================
!  footprint_tools.f90
!  Module + plain R-callable wrappers for rtorf.
!
!  Compile for R package: place in src/, R CMD build handles it.
!  Compile standalone:
!    gfortran -O2 -fopenmp -fopenmp-simd -c footprint_tools.f90
!
!  .Fortran() cannot see module procedures — use the r_* wrappers
!  at the bottom of this file.
! ============================================================

module footprint_tools

  use omp_lib
  implicit none

  real(8), parameter :: pi           = 3.14159265358979323846d0
  real(8), parameter :: earth_radius = 6371000.0d0

contains

  ! ----------------------------------------------------------
  !  haversine — great-circle distance in metres
  !  Declared  Single Instruction, Multiple Data (SIMD) so the compiler can vectorise call sites.
  ! ----------------------------------------------------------
  real(8) function haversine(lat1, lon1, lat2, lon2)
    !$omp declare simd(haversine)
    real(8), intent(in) :: lat1, lon1, lat2, lon2
    real(8) :: dlat, dlon, a, c, rlat1, rlat2

    rlat1 = lat1 * pi / 180.0d0
    rlat2 = lat2 * pi / 180.0d0
    dlat  = (lat2 - lat1) * pi / 180.0d0
    dlon  = (lon2 - lon1) * pi / 180.0d0

    a = sin(dlat/2.0d0)**2 + cos(rlat1) * cos(rlat2) * sin(dlon/2.0d0)**2
    c = 2.0d0 * atan2(sqrt(a), sqrt(1.0d0 - a))
    haversine = earth_radius * c
  end function haversine


  ! ----------------------------------------------------------
  !  grid_simple — bin-and-sum, the primary scientific output.
  !
  !  Uses OMP REDUCTION(+:grid_out) which is cleaner and faster
  !  than ATOMIC for small/medium grids: each thread keeps a
  !  private copy and they are summed at the barrier.
  !
  !  Parameters
  !  ----------
  !  n_part   : number of particles
  !  p_lat    : particle latitudes  (degrees)
  !  p_lon    : particle longitudes (degrees)
  !  p_foot   : particle foot values
  !  nx, ny   : grid dimensions (lon × lat)
  !  lon_min  : western edge of grid (degrees)
  !  lat_min  : southern edge of grid (degrees)
  !  res      : cell size (degrees) — assumed square
  !  n_threads: OMP thread count (pass 1 for serial)
  !  grid_out : [out] nx × ny summed footprint
  ! ----------------------------------------------------------
  subroutine grid_simple(n_part, p_lat, p_lon, p_foot, &
                         nx, ny, lon_min, lat_min, res, &
                         n_threads, grid_out)

    integer, intent(in)  :: n_part, nx, ny, n_threads
    real(8), intent(in)  :: p_lat(n_part), p_lon(n_part), p_foot(n_part)
    real(8), intent(in)  :: lon_min, lat_min, res
    real(8), intent(out) :: grid_out(nx, ny)

    integer :: k, i, j
    real(8) :: inv_res

    grid_out = 0.0d0
    inv_res  = 1.0d0 / res

    ! omp_set_num_threads is a process-wide side effect; set it
    ! in the R wrapper instead if you prefer finer control.
    call omp_set_num_threads(n_threads)

    !$OMP PARALLEL DO PRIVATE(k, i, j) &
    !$OMP SHARED(p_lon, p_lat, p_foot, lon_min, lat_min, inv_res) &
    !$OMP REDUCTION(+:grid_out)
    do k = 1, n_part
      ! floor() is correct for negative offsets;
      ! int() would misplace particles west/south of lon_min.
      i = floor((p_lon(k) - lon_min) * inv_res) + 1
      j = floor((p_lat(k) - lat_min) * inv_res) + 1

      if (i >= 1 .and. i <= nx .and. j >= 1 .and. j <= ny) then
        grid_out(i, j) = grid_out(i, j) + p_foot(k)
      end if
    end do
    !$OMP END PARALLEL DO

  end subroutine grid_simple


  ! ----------------------------------------------------------
  !  kernel_gaussian — per-particle Gaussian kernel-weighted sum.
  !
  !  Each particle spreads its foot value over neighbouring cells
  !  using a 2-D isotropic Gaussian.  The discrete weights include
  !  dlon*dlat so they integrate to ~1 over the truncated window,
  !  conserving total foot.
  !
  !  Two distance modes (use_haversine):
  !    .false. — Cartesian in degrees (fast, small domains)
  !              bandwidth in degrees
  !    .true.  — Haversine in metres  (accurate, large domains)
  !              bandwidth in metres
  !
  !  Parameters
  !  ----------
  !  n_pts      : number of particles
  !  lats, lons : particle coordinates (degrees)
  !  foot       : particle foot values
  !  n_lon,n_lat: grid dimensions
  !  grid_lon   : vector of grid cell-centre longitudes  (degrees)
  !  grid_lat   : vector of grid cell-centre latitudes   (degrees)
  !  lon_min    : western grid edge  (degrees)
  !  lat_min    : southern grid edge (degrees)
  !  lon_res    : cell width  in lon (degrees)
  !  lat_res    : cell height in lat (degrees)
  !  bandwidth  : kernel sigma (degrees if Cartesian, metres if Haversine)
  !  use_haversine: logical — see above
  !  n_threads  : OMP thread count
  !  grid_out   : [out] n_lon × n_lat smoothed footprint
  ! ----------------------------------------------------------
  subroutine kernel_gaussian(n_pts, lats, lons, foot,   &
                             n_lon, n_lat,               &
                             grid_lon, grid_lat,         &
                             lon_min, lat_min,           &
                             lon_res, lat_res,           &
                             bandwidth, use_haversine,   &
                             n_threads, grid_out)

    integer, intent(in) :: n_pts, n_lon, n_lat, n_threads
    real(8), intent(in) :: lats(n_pts), lons(n_pts), foot(n_pts)
    real(8), intent(in) :: grid_lon(n_lon), grid_lat(n_lat)
    real(8), intent(in) :: lon_min, lat_min, lon_res, lat_res
    real(8), intent(in) :: bandwidth
    logical, intent(in) :: use_haversine
    real(8), intent(out):: grid_out(n_lon, n_lat)

    integer :: i, j, k, i_min, i_max, j_min, j_max
    real(8) :: dist, dist_sq, weight, norm_factor
    real(8) :: bw_sq, search_radius
    real(8) :: lat_range, lon_range, cos_lat, dx, dy
    ! search_radius_sq has units matching the current distance mode;
    ! it is computed inside the loop to avoid cross-mode bugs.

    grid_out    = 0.0d0
    norm_factor = 1.0d0 / (2.0d0 * pi * bandwidth**2)
    bw_sq       = bandwidth**2
    search_radius = 3.0d0 * bandwidth    ! 3σ truncation

    call omp_set_num_threads(n_threads)

    !$omp parallel do default(none) &
    !$omp private(i, j, k, i_min, i_max, j_min, j_max, &
    !$omp         lat_range, lon_range, cos_lat,         &
    !$omp         dx, dy, dist, dist_sq, weight)         &
    !$omp shared(n_pts, lats, lons, foot, n_lon, n_lat,  &
    !$omp        grid_lon, grid_lat, lon_min, lat_min,   &
    !$omp        lon_res, lat_res, bandwidth, bw_sq,     &
    !$omp        search_radius, norm_factor,             &
    !$omp        use_haversine, grid_out)
    do i = 1, n_pts
      if (foot(i) == 0.0d0) cycle

      ! ---- Bounding box in grid indices ----------------------
      if (use_haversine) then
        ! search_radius is in metres; convert to degrees for index calc
        lat_range = search_radius / 111320.0d0
        cos_lat   = cos(lats(i) * pi / 180.0d0)
        lon_range = search_radius / (111320.0d0 * max(cos_lat, 0.01d0))
      else
        ! search_radius is already in degrees
        lat_range = search_radius
        lon_range = search_radius
      end if

      i_min = max(1,    int((lons(i) - lon_range - lon_min) / lon_res) + 1)
      i_max = min(n_lon,int((lons(i) + lon_range - lon_min) / lon_res) + 1)
      j_min = max(1,    int((lats(i) - lat_range - lat_min) / lat_res) + 1)
      j_max = min(n_lat,int((lats(i) + lat_range - lat_min) / lat_res) + 1)

      ! ---- Kernel accumulation over bounding box -------------
      do j = i_min, i_max
        do k = j_min, j_max

          if (use_haversine) then
            dist    = haversine(lats(i), lons(i), grid_lat(k), grid_lon(j))
            dist_sq = dist * dist
            ! Guard in metres²
            if (dist_sq > search_radius**2) cycle
          else
            dx      = grid_lon(j) - lons(i)
            dy      = grid_lat(k) - lats(i)
            dist_sq = dx*dx + dy*dy
            ! Guard in degrees²
            if (dist_sq > search_radius**2) cycle
          end if

          ! Gaussian weight; multiply by cell area (degrees²) so that
          ! sum of weights over the window integrates to ~1,
          ! conserving total foot.
          weight = norm_factor * dexp(-0.5d0 * dist_sq / bw_sq) &
                 * lon_res * lat_res

          !$omp atomic
          grid_out(j, k) = grid_out(j, k) + foot(i) * weight

        end do
      end do

    end do
    !$omp end parallel do

  end subroutine kernel_gaussian

end module footprint_tools


! ==============================================================
!  Plain subroutine wrappers — required for .Fortran() in R.
!  .Fortran() resolves by Fortran name-mangling in the global
!  symbol table; it cannot reach inside a Fortran module.
! ==============================================================

subroutine r_grid_simple(n_part, p_lat, p_lon, p_foot, &
                         nx, ny, lon_min, lat_min, res, &
                         n_threads, grid_out)
  use footprint_tools
  implicit none
  integer, intent(in)  :: n_part, nx, ny, n_threads
  real(8), intent(in)  :: p_lat(n_part), p_lon(n_part), p_foot(n_part)
  real(8), intent(in)  :: lon_min, lat_min, res
  real(8), intent(out) :: grid_out(nx, ny)

  call grid_simple(n_part, p_lat, p_lon, p_foot, &
                   nx, ny, lon_min, lat_min, res, &
                   n_threads, grid_out)
end subroutine r_grid_simple


subroutine r_kernel_gaussian(n_pts, lats, lons, foot,   &
                              n_lon, n_lat,               &
                              grid_lon, grid_lat,         &
                              lon_min, lat_min,           &
                              lon_res, lat_res,           &
                              bandwidth, use_haversine,   &
                              n_threads, grid_out)
  use footprint_tools
  implicit none
  integer, intent(in) :: n_pts, n_lon, n_lat, n_threads
  real(8), intent(in) :: lats(n_pts), lons(n_pts), foot(n_pts)
  real(8), intent(in) :: grid_lon(n_lon), grid_lat(n_lat)
  real(8), intent(in) :: lon_min, lat_min, lon_res, lat_res, bandwidth
  logical, intent(in) :: use_haversine
  real(8), intent(out):: grid_out(n_lon, n_lat)

  call kernel_gaussian(n_pts, lats, lons, foot,   &
                       n_lon, n_lat,               &
                       grid_lon, grid_lat,         &
                       lon_min, lat_min,           &
                       lon_res, lat_res,           &
                       bandwidth, use_haversine,   &
                       n_threads, grid_out)
end subroutine r_kernel_gaussian