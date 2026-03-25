#' Check the max number of threads
#'
#' @description \code{get_nt} check the number of threads in this machine
#'
#' @return Integer with the max number of threads
#'
#' @useDynLib rtorf
#' @import data.table
#' @export
#' @examples
#' {
#'   get_nt()
#' }
get_nt <- function() {
  .Fortran("ntf", nt = integer(1L))$nt
}

# ============================================================
#  obs_grid.R
#  R wrappers for the Fortran subroutines in footprint_tools.f90
#
#  Naming convention:  obs_{output}_{method}
#    obs_grid_simple — gridded footprint via bin-and-sum
#    obs_grid_kernel — gridded footprint via Gaussian kernel
#    obs_grid_check  — conservation sanity check
#
#  Package layout:
#    src/footprint_tools.f90   (Fortran, compiled by R CMD build)
#    R/obs_grid.R              (this file)
# ============================================================

# ------------------------------------------------------------
#  obs_grid_simple
# ------------------------------------------------------------

#' Bin-and-sum footprint gridding
#'
#' Bins STILT/HYSPLIT particles into a regular longitude/latitude
#' grid and sums the \code{foot} sensitivity values per cell.
#' This is the primary scientific output: no smoothing is applied,
#' no foot is redistributed spatially, and the grid total exactly
#' equals the particle total.
#'
#' @param x A \code{data.table} or \code{data.frame} with at least
#'   the columns \code{lat} (degrees), \code{lon} (degrees), and
#'   \code{foot} (sensitivity, ppm / (umol m-2 s-1)).
#' @param lon_min Western edge of the output grid (degrees).
#' @param lat_min Southern edge of the output grid (degrees).
#' @param lon_max Eastern edge of the output grid (degrees).
#' @param lat_max Northern edge of the output grid (degrees).
#' @param res Cell size in degrees. Both longitude and latitude use
#'   the same value (square cells). Default \code{0.1}.
#' @param n_threads Number of OpenMP threads passed to the Fortran
#'   routine. Default \code{1L}. Increase for large particle sets
#'   (> ~50 000 rows); for smaller sets the thread-management
#'   overhead outweighs the gain.
#' @param npar Number of particles used for normalization. The output
#'   grid is divided by this value to return the average footprint
#'   sensitivity. Default \code{1L}.
#'
#' @return A named list with three elements:
#' \describe{
#'   \item{\code{grid}}{Numeric matrix of dimension
#'     \code{[nlon x nlat]} containing the summed \code{foot}
#'     per cell, divided by \code{npar}. Row \code{i} corresponds
#'     to \code{lon[i]}, column \code{j} to \code{lat[j]}.}
#'   \item{\code{lon}}{Numeric vector of cell-centre longitudes
#'     (length \code{nlon}).}
#'   \item{\code{lat}}{Numeric vector of cell-centre latitudes
#'     (length \code{nlat}).}
#' }
#'
#' @note Particles whose coordinates fall outside the rectangle
#'   \code{[lon_min, lon_max) x [lat_min, lat_max)} are silently
#'   discarded. Add a small buffer to the grid extent if edge
#'   particles matter.
#'
#' @seealso \code{\link{obs_grid_kernel}} for a smoothed
#'   alternative, \code{\link{obs_grid_check}} to compare totals.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dat <- fread("PARTICLE.DAT")
#' d   <- dat[time %in% sort(unique(time))[1:4]]   # first hour
#'
#' bs <- obs_grid_simple(
#'   x         = d,
#'   lon_min   = floor(min(d$lon))   - 0.1,
#'   lat_min   = floor(min(d$lat))   - 0.1,
#'   lon_max   = ceiling(max(d$lon)) + 0.1,
#'   lat_max   = ceiling(max(d$lat)) + 0.1,
#'   res       = 0.1,
#'   n_threads = 4L
#' )
#' image(bs$lon, bs$lat, log1p(bs$grid), main = "Bin and sum")
#' }
#'
#' @export
obs_grid_simple <- function(
  x,
  lon_min,
  lat_min,
  lon_max,
  lat_max,
  res = 0.1,
  n_threads = 1L,
  npar = 1L
) {
  stopifnot(
    is.data.frame(x),
    all(c("lat", "lon", "foot") %in% names(x)),
    lon_max > lon_min,
    lat_max > lat_min,
    res > 0
  )

  nx <- as.integer(round((lon_max - lon_min) / res))
  ny <- as.integer(round((lat_max - lat_min) / res))

  out <- .Fortran(
    "r_grid_simple",
    n_part = as.integer(nrow(x)),
    p_lat = as.double(x$lat),
    p_lon = as.double(x$lon),
    p_foot = as.double(x$foot),
    nx = nx,
    ny = ny,
    lon_min = as.double(lon_min),
    lat_min = as.double(lat_min),
    res = as.double(res),
    n_threads = as.integer(n_threads),
    grid_out = double(nx * ny)
  )

  list(
    grid = matrix(out$grid_out, nrow = ny, ncol = nx, byrow = FALSE) / npar,
    lon = lon_min + (seq_len(nx) - 0.5) * res,
    lat = lat_min + (seq_len(ny) - 0.5) * res
  )
}


# ------------------------------------------------------------
#  obs_grid_cube
# ------------------------------------------------------------

#' 3D bin-and-sum footprint gridding (lon, lat, time)
#'
#' Bins particles into a 3D grid: longitude, latitude, and time.
#' Time is binned as "hours back" (or any other time unit) from a
#' reference time.
#'
#' @param x A \code{data.table} or \code{data.frame} with columns
#'   \code{lat}, \code{lon}, \code{time} (minutes), and \code{foot}.
#' @param lon_min,lat_min,lon_max,lat_max Grid extent (degrees).
#' @param res Spatial resolution (degrees).
#' @param nt Number of time layers in the output cube.
#' @param t0 Reference time for the first layer (minutes). Default \code{0}.
#'   In HYSPLIT backward runs, particles start at \code{time = 0}
#'   and go negative. \code{t0 = 0} means layer 1 is 0 to \code{dt}
#'   minutes back.
#' @param dt Time step per layer (minutes). Default \code{60} (1 hour).
#' @param n_threads Number of OpenMP threads.
#' @param npar Normalization factor (total particles released).
#'
#' @return A named list:
#' \describe{
#'   \item{\code{grid}}{Numeric 3D array of dimension \code{[nx x ny x nt]}.}
#'   \item{\code{lon, lat}}{Cell-centre coordinates.}
#'   \item{\code{time}}{Start time of each bin (minutes back from \code{t0}).}
#' }
#'
#' @export
obs_grid_cube <- function(
  x,
  lon_min,
  lat_min,
  lon_max,
  lat_max,
  res = 0.1,
  nt = 240L,
  t0 = 0,
  dt = 60,
  n_threads = 1L,
  npar = 1L
) {
  stopifnot(
    is.data.frame(x),
    all(c("lat", "lon", "time", "foot") %in% names(x)),
    lon_max > lon_min,
    lat_max > lat_min,
    res > 0,
    nt > 0
  )

  nx <- as.integer(round((lon_max - lon_min) / res))
  ny <- as.integer(round((lat_max - lat_min) / res))

  out <- .Fortran(
    "r_grid_cube",
    n_part = as.integer(nrow(x)),
    p_lat = as.double(x$lat),
    p_lon = as.double(x$lon),
    p_time = as.double(x$time),
    p_foot = as.double(x$foot),
    nx = nx,
    ny = ny,
    nt = as.integer(nt),
    lon_min = as.double(lon_min),
    lat_min = as.double(lat_min),
    res = as.double(res),
    t0 = as.double(t0),
    dt = as.double(dt),
    n_threads = as.integer(n_threads),
    grid_out = double(nx * ny * nt)
  )

  list(
    grid = array(out$grid_out, dim = c(ny, nx, nt)) / npar,
    lon = lon_min + (seq_len(nx) - 0.5) * res,
    lat = lat_min + (seq_len(ny) - 0.5) * res,
    time = t0 - (seq_len(nt) - 1) * dt
  )
}



# ------------------------------------------------------------
#  obs_grid_kernel
# ------------------------------------------------------------

#' Gaussian kernel footprint gridding
#'
#' Computes a gridded footprint by spreading each particle's
#' \code{foot} value over neighbouring cells using a 2-D isotropic
#' Gaussian kernel. The kernel integrates to 1 (cell area
#' \code{lon_res * lat_res} is included in the weight), so the
#' total \code{foot} is approximately conserved — the small
#' residual arises from truncation at ±3 sigma.
#'
#' Use this function when particle density is too low to fill every
#' grid cell, or for display purposes. For analyses that require
#' exact conservation of the total sensitivity, prefer
#' \code{\link{obs_grid_simple}}.
#'
#' @param x A \code{data.table} or \code{data.frame} with columns
#'   \code{lat}, \code{lon}, and \code{foot} (same units as
#'   \code{\link{obs_grid_simple}}).
#' @param lon_min Western edge of the output grid (degrees).
#' @param lat_min Southern edge of the output grid (degrees).
#' @param lon_max Eastern edge of the output grid (degrees).
#' @param lat_max Northern edge of the output grid (degrees).
#' @param lon_res Cell width in longitude (degrees). Default \code{0.1}.
#' @param lat_res Cell height in latitude (degrees). Default \code{0.1}.
#' @param bandwidth Kernel standard deviation. Units depend on
#'   \code{use_haversine}:
#'   \itemize{
#'     \item \code{use_haversine = FALSE} (default) — degrees.
#'       Recommended starting value: \code{lon_res} (one cell).
#'       Increase to \code{2 * lon_res} for sparser particle sets.
#'     \item \code{use_haversine = TRUE} — metres. A typical value
#'       for 0.1-degree grids is \code{11000} (approximately one
#'       degree at mid-latitudes).
#'   }
#'   Default \code{lon_res}.
#' @param use_haversine Logical. If \code{FALSE} (default), Cartesian
#'   distance in degrees is used — fast and adequate for domains
#'   smaller than ~1000 km and latitudes below ~60 degrees. If
#'   \code{TRUE}, great-circle distance via the Haversine formula is
#'   used — accurate at high latitudes and continental-scale domains;
#'   \code{bandwidth} must then be supplied in metres. A warning is
#'   issued when \code{use_haversine = TRUE} and
#'   \code{bandwidth < 100}, which suggests degrees were passed
#'   instead of metres.
#' @param n_threads Number of OpenMP threads. Default \code{1L}.
#' @param npar Number of particles for normalization. Default \code{1L}.
#'
#' @return A named list with the same structure as
#'   \code{\link{obs_grid_simple}}:
#' \describe{
#'   \item{\code{grid}}{Numeric matrix \code{[nlon x nlat]} of
#'     kernel-smoothed \code{foot}.}
#'   \item{\code{lon}}{Cell-centre longitude vector.}
#'   \item{\code{lat}}{Cell-centre latitude vector.}
#' }
#'
#' @note The Fortran routine skips particles with \code{foot == 0}
#'   before entering the kernel loop, so sparse footprints with many
#'   zero-weight particles incur no unnecessary computation.
#'
#' @seealso \code{\link{obs_grid_simple}} for the unsmoothed primary
#'   output, \code{\link{obs_grid_check}} to verify conservation
#'   between the two methods.
#'
#' @examples
#' \dontrun{
#' library(data.table)
#' dat <- fread("PARTICLE.DAT")
#' d   <- dat[time %in% sort(unique(time))[1:4]]
#'
#' # Cartesian mode (degrees, fast)
#' gk <- obs_grid_kernel(
#'   x             = d,
#'   lon_min       = floor(min(d$lon))   - 0.1,
#'   lat_min       = floor(min(d$lat))   - 0.1,
#'   lon_max       = ceiling(max(d$lon)) + 0.1,
#'   lat_max       = ceiling(max(d$lat)) + 0.1,
#'   lon_res       = 0.1,
#'   lat_res       = 0.1,
#'   bandwidth     = 0.2,          # 2-cell sigma in degrees
#'   use_haversine = FALSE,
#'   n_threads     = 4L
#' )
#' image(gk$lon, gk$lat, log1p(gk$grid), main = "Gaussian kernel")
#'
#' # Haversine mode (metres, accurate at high latitudes)
#' gk_hav <- obs_grid_kernel(
#'   x             = d,
#'   lon_min       = -130, lat_min = 55,
#'   lon_max       = -100, lat_max = 75,
#'   lon_res       = 0.1,  lat_res = 0.1,
#'   bandwidth     = 15000,        # 15 km
#'   use_haversine = TRUE,
#'   n_threads     = 4L
#' )
#' }
#'
#' @export
obs_grid_kernel <- function(
  x,
  lon_min,
  lat_min,
  lon_max,
  lat_max,
  lon_res = 0.1,
  lat_res = 0.1,
  bandwidth = lon_res,
  use_haversine = FALSE,
  n_threads = 1L,
  npar = 1L
) {
  stopifnot(
    is.data.frame(x),
    all(c("lat", "lon", "foot") %in% names(x)),
    lon_max > lon_min,
    lat_max > lat_min,
    lon_res > 0,
    lat_res > 0,
    bandwidth > 0
  )

  if (isTRUE(use_haversine) && bandwidth < 100) {
    warning(
      "use_haversine = TRUE but bandwidth = ",
      bandwidth,
      " which looks like degrees, not metres. ",
      "Did you mean use_haversine = FALSE?"
    )
  }

  n_lon <- as.integer(round((lon_max - lon_min) / lon_res))
  n_lat <- as.integer(round((lat_max - lat_min) / lat_res))
  grid_lon <- lon_min + (seq_len(n_lon) - 0.5) * lon_res
  grid_lat <- lat_min + (seq_len(n_lat) - 0.5) * lat_res

  out <- .Fortran(
    "r_kernel_gaussian",
    n_pts = as.integer(nrow(x)),
    lats = as.double(x$lat),
    lons = as.double(x$lon),
    foot = as.double(x$foot),
    n_lon = n_lon,
    n_lat = n_lat,
    grid_lon = as.double(grid_lon),
    grid_lat = as.double(grid_lat),
    lon_min = as.double(lon_min),
    lat_min = as.double(lat_min),
    lon_res = as.double(lon_res),
    lat_res = as.double(lat_res),
    bandwidth = as.double(bandwidth),
    use_haversine = as.logical(use_haversine),
    n_threads = as.integer(n_threads),
    grid_out = double(n_lon * n_lat)
  )

  list(
    grid = matrix(out$grid_out, nrow = n_lat, ncol = n_lon, byrow = FALSE) / npar,
    lon = grid_lon,
    lat = grid_lat
  )
}


# ------------------------------------------------------------
#  obs_grid_check
# ------------------------------------------------------------

#' Conservation check for gridded footprints
#'
#' Compares the total \code{foot} between the outputs of
#' \code{\link{obs_grid_simple}} and \code{\link{obs_grid_kernel}}
#' and prints a brief summary to the console. A relative difference
#' larger than 5\% triggers a \code{\link{warning}} with diagnostic
#' suggestions.
#'
#' The kernel method loses a small amount of \code{foot} at the ±3
#' sigma boundary of the truncated Gaussian. Larger differences
#' usually indicate a grid extent that is too tight (edge particles
#' are discarded) or a bandwidth that is large relative to the domain.
#'
#' @param simple Output list from \code{\link{obs_grid_simple}}.
#' @param kernel Output list from \code{\link{obs_grid_kernel}}.
#'
#' @return Invisibly returns a named numeric vector with three
#'   elements: \code{simple} (total foot from bin-and-sum),
#'   \code{kernel} (total foot from kernel method), and
#'   \code{relative_diff_pct} (absolute percentage difference
#'   relative to \code{simple}).
#'
#' @seealso \code{\link{obs_grid_simple}}, \code{\link{obs_grid_kernel}}
#'
#' @examples
#' \dontrun{
#' bs <- obs_grid_simple(d, lon_min, lat_min, lon_max, lat_max)
#' gk <- obs_grid_kernel(d, lon_min, lat_min, lon_max, lat_max,
#'                       bandwidth = 0.2)
#' chk <- obs_grid_check(bs, gk)
#' chk["relative_diff_pct"]
#' }
#'
#' @export
obs_grid_check <- function(simple, kernel) {
  s <- sum(simple$grid)
  k <- sum(kernel$grid)
  rel <- abs(s - k) / s * 100

  cat(sprintf("obs_grid_simple total foot : %.6f\n", s))
  cat(sprintf("obs_grid_kernel total foot : %.6f\n", k))
  cat(sprintf("Relative difference        : %.4f%%\n", rel))

  if (rel > 5) {
    warning(
      "Relative difference > 5%. Consider: ",
      "(1) increasing grid extent to capture edge particles, ",
      "(2) reducing bandwidth, ",
      "(3) checking that the same particle set was used for both."
    )
  }

  invisible(c(simple = s, kernel = k, relative_diff_pct = rel))
}
