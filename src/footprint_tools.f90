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
    real(8), intent(out) :: grid_out(ny, nx)

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
        grid_out(j, i) = grid_out(j, i) + p_foot(k)
      end if
    end do
    !$OMP END PARALLEL DO

  end subroutine grid_simple


  ! ----------------------------------------------------------
  !  grid_cube — 3D bin-and-sum for time-resolved footprints.
  !
  !  Bins particles into a longitude-latitude-time grid.
  !  Time is handled as "hours back" from p_time.
  !
  !  Parameters
  !  ----------
  !  n_part   : number of particles
  !  p_lat    : latitudes  (degrees)
  !  p_lon    : longitudes (degrees)
  !  p_time   : minutes (HYSPLIT relative time)
  !  p_foot   : foot values
  !  nx, ny, nt: grid dimensions (lon × lat × time)
  !  lon_min  : western edge (degrees)
  !  lat_min  : southern edge (degrees)
  !  res      : spatial cell size (degrees)
  !  t0       : start time (minutes, usually 0)
  !  dt       : time step (minutes, e.g. 60)
  !  n_threads: OMP thread count
  !  grid_out : [out] nx × ny × nt summed footprint
  ! ----------------------------------------------------------
  subroutine grid_cube(n_part, p_lat, p_lon, p_time, p_foot, &
                       nx, ny, nt, lon_min, lat_min, res, t0, dt, &
                       n_threads, grid_out)

    integer, intent(in)  :: n_part, nx, ny, nt, n_threads
    real(8), intent(in)  :: p_lat(n_part), p_lon(n_part), p_time(n_part), p_foot(n_part)
    real(8), intent(in)  :: lon_min, lat_min, res, t0, dt
    real(8), intent(out) :: grid_out(ny, nx, nt)

    integer :: k, i, j, l
    real(8) :: inv_res, inv_dt, btime

    grid_out = 0.0d0
    inv_res  = 1.0d0 / res
    inv_dt   = 1.0d0 / dt

    call omp_set_num_threads(n_threads)

    !$OMP PARALLEL DO PRIVATE(k, i, j, l, btime) &
    !$OMP SHARED(p_lon, p_lat, p_time, p_foot, lon_min, lat_min, inv_res, t0, inv_dt, nt) &
    !$OMP REDUCTION(+:grid_out)
    do k = 1, n_part
      ! Handle longitude and latitude
      i = floor((p_lon(k) - lon_min) * inv_res) + 1
      j = floor((p_lat(k) - lat_min) * inv_res) + 1
      
      ! btime is minutes back from t0 (usually 0 in HYSPLIT)
      btime = t0 - p_time(k)
      l = floor(btime * inv_dt) + 1

      if (i >= 1 .and. i <= nx .and. j >= 1 .and. j <= ny .and. l >= 1 .and. l <= nt) then
        grid_out(j, i, l) = grid_out(j, i, l) + p_foot(k)
      end if
    end do
    !$OMP END PARALLEL DO

  end subroutine grid_cube



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
    real(8), intent(out):: grid_out(n_lat, n_lon)

    integer :: i, j, k, i_min, i_max, j_min, j_max
    real(8) :: dist_sq, weight, norm_factor
    real(8) :: bw_sq, search_radius
    real(8) :: lat_range, lon_range, cos_lat, dx, dy
    real(8) :: deg2m, cell_area
    ! deg2m: factor to convert degree^2 to metres^2 at the equator
    ! (R * pi / 180)^2

    grid_out    = 0.0d0
    norm_factor = 1.0d0 / (2.0d0 * pi * bandwidth**2)
    bw_sq       = bandwidth**2
    search_radius = 3.0d0 * bandwidth    ! 3σ truncation
    deg2m       = (earth_radius * pi / 180.0d0)**2

    call omp_set_num_threads(n_threads)

    !$omp parallel do default(none) &
    !$omp private(i, j, k, i_min, i_max, j_min, j_max, &
    !$omp         lat_range, lon_range, cos_lat,         &
    !$omp         dx, dy, dist_sq, weight, cell_area)    &
    !$omp shared(n_pts, lats, lons, foot, n_lon, n_lat,  &
    !$omp        grid_lon, grid_lat, lon_min, lat_min,   &
    !$omp        lon_res, lat_res, bandwidth, bw_sq,     &
    !$omp        search_radius, norm_factor, deg2m,      &
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

      ! Use floor() to be consistent with grid_simple and safe for negative coords
      i_min = max(1,     floor((lons(i) - lon_range - lon_min) / lon_res) + 1)
      i_max = min(n_lon, floor((lons(i) + lon_range - lon_min) / lon_res) + 1)
      j_min = max(1,     floor((lats(i) - lat_range - lat_min) / lat_res) + 1)
      j_max = min(n_lat, floor((lats(i) + lat_range - lat_min) / lat_res) + 1)

      ! ---- Kernel accumulation over bounding box -------------
      do j = i_min, i_max
        do k = j_min, j_max

          if (use_haversine) then
            ! dist_sq in m^2
            dist_sq = haversine(lats(i), lons(i), grid_lat(k), grid_lon(j))**2
            if (dist_sq > search_radius**2) cycle
            ! area in m^2 = deg^2 * deg2m * cos(lat)
            cell_area = lon_res * lat_res * deg2m * cos(grid_lat(k) * pi / 180.0d0)
          else
            ! dist_sq in deg^2
            dx      = grid_lon(j) - lons(i)
            dy      = grid_lat(k) - lats(i)
            dist_sq = dx*dx + dy*dy
            if (dist_sq > search_radius**2) cycle
            ! area in deg^2
            cell_area = lon_res * lat_res
          end if

          ! Gaussian weight; multiply by cell area so that
          ! sum of weights over the window integrates to ~1,
          ! conserving total foot.
          weight = norm_factor * dexp(-0.5d0 * dist_sq / bw_sq) * cell_area

          !$omp atomic
          grid_out(k, j) = grid_out(k, j) + foot(i) * weight

        end do
      end do

    end do
    !$omp end parallel do

  end subroutine kernel_gaussian


  ! ----------------------------------------------------------
  !  grid_cube_kernel — 3D kernel-smoothed footprint gridding.
  !
  !  Combines the temporal binning of grid_cube with the spatial
  !  smoothing of kernel_gaussian. Each particle is binned to a
  !  time slice l, then its foot value is spread over a 2-D spatial
  !  Gaussian kernel within that slice.
  !
  !  Parameters
  !  ----------
  !  n_pts      : number of particles
  !  lats, lons : particle coordinates (degrees)
  !  p_time     : minutes (HYSPLIT relative time)
  !  foot       : particle foot values
  !  n_lon,n_lat,nt: grid dimensions
  !  grid_lon   : vector of grid cell-centre longitudes  (degrees)
  !  grid_lat   : vector of grid cell-centre latitudes   (degrees)
  !  lon_min    : western grid edge  (degrees)
  !  lat_min    : southern grid edge (degrees)
  !  lon_res    : cell width  in lon (degrees)
  !  lat_res    : cell height in lat (degrees)
  !  t0         : start time (minutes)
  !  dt         : time step per layer (minutes)
  !  bandwidth  : kernel sigma (degrees if Cartesian, metres if Haversine)
  !  use_haversine: logical
  !  n_threads  : OMP thread count
  !  grid_out   : [out] n_lat × n_lon × nt smoothed footprint
  ! ----------------------------------------------------------
  subroutine grid_cube_kernel(n_pts, lats, lons, p_time, foot, &
                              n_lon, n_lat, nt,               &
                              grid_lon, grid_lat,             &
                              lon_min, lat_min,               &
                              lon_res, lat_res,               &
                              t0, dt, bandwidth,              &
                              use_haversine,                  &
                              n_threads, grid_out)

    integer, intent(in) :: n_pts, n_lon, n_lat, nt, n_threads
    real(8), intent(in) :: lats(n_pts), lons(n_pts), p_time(n_pts), foot(n_pts)
    real(8), intent(in) :: grid_lon(n_lon), grid_lat(n_lat)
    real(8), intent(in) :: lon_min, lat_min, lon_res, lat_res
    real(8), intent(in) :: t0, dt, bandwidth
    logical, intent(in) :: use_haversine
    real(8), intent(out):: grid_out(n_lat, n_lon, nt)

    integer :: i, j, k, l, i_min, i_max, j_min, j_max
    real(8) :: dist_sq, weight, norm_factor
    real(8) :: bw_sq, search_radius
    real(8) :: lat_range, lon_range, cos_lat, dx, dy, btime
    real(8) :: deg2m, cell_area, inv_dt

    grid_out    = 0.0d0
    norm_factor = 1.0d0 / (2.0d0 * pi * bandwidth**2)
    bw_sq       = bandwidth**2
    search_radius = 3.0d0 * bandwidth
    deg2m       = (earth_radius * pi / 180.0d0)**2
    inv_dt      = 1.0d0 / dt

    call omp_set_num_threads(n_threads)

    !$omp parallel do default(none) &
    !$omp private(i, j, k, l, i_min, i_max, j_min, j_max, &
    !$omp         lat_range, lon_range, cos_lat,         &
    !$omp         dx, dy, dist_sq, weight, cell_area, btime) &
    !$omp shared(n_pts, lats, lons, p_time, foot, n_lon, n_lat, nt, &
    !$omp        grid_lon, grid_lat, lon_min, lat_min,   &
    !$omp        lon_res, lat_res, t0, inv_dt, bandwidth, bw_sq, &
    !$omp        search_radius, norm_factor, deg2m,      &
    !$omp        use_haversine, grid_out)
    do i = 1, n_pts
      if (foot(i) == 0.0d0) cycle

      ! ---- Temporal binning ----------------------------------
      btime = t0 - p_time(i)
      l = floor(btime * inv_dt) + 1
      if (l < 1 .or. l > nt) cycle

      ! ---- Bounding box in grid indices ----------------------
      if (use_haversine) then
        lat_range = search_radius / 111320.0d0
        cos_lat   = cos(lats(i) * pi / 180.0d0)
        lon_range = search_radius / (111320.0d0 * max(cos_lat, 0.01d0))
      else
        lat_range = search_radius
        lon_range = search_radius
      end if

      i_min = max(1,     floor((lons(i) - lon_range - lon_min) / lon_res) + 1)
      i_max = min(n_lon, floor((lons(i) + lon_range - lon_min) / lon_res) + 1)
      j_min = max(1,     floor((lats(i) - lat_range - lat_min) / lat_res) + 1)
      j_max = min(n_lat, floor((lats(i) + lat_range - lat_min) / lat_res) + 1)

      ! ---- Kernel accumulation over bounding box into slice l -------------
      do j = i_min, i_max
        do k = j_min, j_max

          if (use_haversine) then
            dist_sq = haversine(lats(i), lons(i), grid_lat(k), grid_lon(j))**2
            if (dist_sq > search_radius**2) cycle
            cell_area = lon_res * lat_res * deg2m * cos(grid_lat(k) * pi / 180.0d0)
          else
            dx      = grid_lon(j) - lons(i)
            dy      = grid_lat(k) - lats(i)
            dist_sq = dx*dx + dy*dy
            if (dist_sq > search_radius**2) cycle
            cell_area = lon_res * lat_res
          end if

          weight = norm_factor * dexp(-0.5d0 * dist_sq / bw_sq) * cell_area

          !$omp atomic
          grid_out(k, j, l) = grid_out(k, j, l) + foot(i) * weight

        end do
      end do

    end do
    !$omp end parallel do

  end subroutine grid_cube_kernel


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
  real(8), intent(out) :: grid_out(ny, nx)

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
  real(8), intent(out):: grid_out(n_lat, n_lon)

  call kernel_gaussian(n_pts, lats, lons, foot,   &
                       n_lon, n_lat,               &
                       grid_lon, grid_lat,         &
                       lon_min, lat_min,           &
                       lon_res, lat_res,           &
                       bandwidth, use_haversine,   &
                       n_threads, grid_out)
end subroutine r_kernel_gaussian


subroutine r_grid_cube(n_part, p_lat, p_lon, p_time, p_foot, &
                       nx, ny, nt, lon_min, lat_min, res, t0, dt, &
                       n_threads, grid_out)
  use footprint_tools
  implicit none
  integer, intent(in)  :: n_part, nx, ny, nt, n_threads
  real(8), intent(in)  :: p_lat(n_part), p_lon(n_part), p_time(n_part), p_foot(n_part)
  real(8), intent(in)  :: lon_min, lat_min, res, t0, dt
  real(8), intent(out) :: grid_out(ny, nx, nt)

  call grid_cube(n_part, p_lat, p_lon, p_time, p_foot, &
                 nx, ny, nt, lon_min, lat_min, res, t0, dt, &
                 n_threads, grid_out)
end subroutine r_grid_cube


subroutine r_grid_cube_kernel(n_pts, lats, lons, p_time, foot, &
                              n_lon, n_lat, nt,               &
                              grid_lon, grid_lat,             &
                              lon_min, lat_min,               &
                              lon_res, lat_res,               &
                              t0, dt, bandwidth,              &
                              use_haversine,                  &
                              n_threads, grid_out)
  use footprint_tools
  implicit none
  integer, intent(in) :: n_pts, n_lon, n_lat, nt, n_threads
  real(8), intent(in) :: lats(n_pts), lons(n_pts), p_time(n_pts), foot(n_pts)
  real(8), intent(in) :: grid_lon(n_lon), grid_lat(n_lat)
  real(8), intent(in) :: lon_min, lat_min, lon_res, lat_res, t0, dt, bandwidth
  logical, intent(in) :: use_haversine
  real(8), intent(out):: grid_out(n_lat, n_lon, nt)

  call grid_cube_kernel(n_pts, lats, lons, p_time, foot, &
                        n_lon, n_lat, nt,               &
                        grid_lon, grid_lat,             &
                        lon_min, lat_min,               &
                        lon_res, lat_res,               &
                        t0, dt, bandwidth,              &
                        use_haversine,                  &
                        n_threads, grid_out)
end subroutine r_grid_cube_kernel