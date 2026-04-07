# Tests for obs_nc() and obs_nc_get()
# Uses only base R + ncdf4 (already a package import) + testthat.

# ── Helpers ───────────────────────────────────────────────────────────────────

# Build a tiny lat/lon grid and matching array list for re-use across tests.
make_args <- function(nlat = 3L, nlon = 4L, ntime = 2L) {
  lat      <- seq(30, 32, length.out = nlat)
  lon      <- seq(-100, -97, length.out = nlon)
  time_nc  <- ISOdatetime(2020, 1, 1, 0, 0, 0, tz = "UTC") +
               (seq_len(ntime) - 1L) * 3600
  arr      <- array(seq_len(nlat * nlon * ntime), dim = c(nlon, nlat, ntime))
  list(lat = lat, lon = lon, time_nc = time_nc, arr = arr)
}

# ── obs_nc: input validation ───────────────────────────────────────────────────

test_that("obs_nc stops when vars_out and larrays have different lengths", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  expect_error(
    obs_nc(
      lat      = a$lat,
      lon      = a$lon,
      time_nc  = a$time_nc,
      vars_out = c("a", "b"),          # 2 names
      units_out = "ppm",
      nc_out   = tmp,
      larrays  = list(a = a$arr)       # 1 array  -> mismatch
    ),
    regexp = "Length of vars_out must be equal to length of larrays"
  )
})

test_that("obs_nc stops when lat is missing", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  expect_error(
    obs_nc(
      lon      = a$lon,
      time_nc  = a$time_nc,
      vars_out = "total",
      units_out = "ppm",
      nc_out   = tmp,
      larrays  = list(total = a$arr)
    ),
    regexp = "Missing lat"
  )
})

test_that("obs_nc stops when lon is missing", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  expect_error(
    obs_nc(
      lat      = a$lat,
      time_nc  = a$time_nc,
      vars_out = "total",
      units_out = "ppm",
      nc_out   = tmp,
      larrays  = list(total = a$arr)
    ),
    regexp = "Missing lon"
  )
})

test_that("obs_nc stops when time_nc is missing", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  expect_error(
    obs_nc(
      lat      = a$lat,
      lon      = a$lon,
      vars_out = "total",
      units_out = "ppm",
      nc_out   = tmp,
      larrays  = list(total = a$arr)
    ),
    regexp = "Missing time_nc"
  )
})

test_that("obs_nc stops when units_out is missing", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  expect_error(
    obs_nc(
      lat      = a$lat,
      lon      = a$lon,
      time_nc  = a$time_nc,
      vars_out = "total",
      nc_out   = tmp,
      larrays  = list(total = a$arr)
    ),
    regexp = "Missing units_out"
  )
})

# ── obs_nc: successful file creation ──────────────────────────────────────────

test_that("obs_nc creates a NetCDF file on disk", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0L)
})

test_that("obs_nc creates multiple variables in the NetCDF", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = c("bio", "fossil"),
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(bio = a$arr, fossil = a$arr * 2)
  )

  nc <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  expect_true("bio"    %in% names(nc$var))
  expect_true("fossil" %in% names(nc$var))
})

# ── obs_nc: dimension correctness ─────────────────────────────────────────────

test_that("obs_nc writes correct latitude dimension values", {
  a   <- make_args(nlat = 5L)
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc  <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  lat_out <- ncdf4::ncvar_get(nc, "latitude")

  expect_equal(length(lat_out), 5L)
  expect_equal(as.vector(lat_out), as.vector(a$lat), tolerance = 1e-6)
})

test_that("obs_nc writes correct longitude dimension values", {
  a   <- make_args(nlon = 6L)
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc      <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  lon_out <- ncdf4::ncvar_get(nc, "longitude")

  expect_equal(length(lon_out), 6L)
  expect_equal(as.vector(lon_out), as.vector(a$lon), tolerance = 1e-6)
})

test_that("obs_nc writes correct time dimension values", {
  a   <- make_args(ntime = 3L)
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc        <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  time_out  <- ncdf4::ncvar_get(nc, "Time")

  expect_equal(length(time_out), 3L)
  expect_equal(as.vector(time_out), as.vector(as.numeric(a$time_nc)), tolerance = 1e-3)
})

# ── obs_nc: variable data integrity ───────────────────────────────────────────

test_that("obs_nc stores array values that can be read back correctly", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc      <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  arr_out <- ncdf4::ncvar_get(nc, "total")

  expect_equal(dim(arr_out), dim(a$arr))
  expect_equal(arr_out, a$arr, tolerance = 1e-6)
})

test_that("obs_nc stores each variable independently", {
  a    <- make_args()
  tmp  <- tempfile(fileext = ".nc")
  arr2 <- a$arr * 10
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = c("bio", "fossil"),
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(bio = a$arr, fossil = arr2)
  )

  nc   <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  bio_out    <- ncdf4::ncvar_get(nc, "bio")
  fossil_out <- ncdf4::ncvar_get(nc, "fossil")

  expect_equal(bio_out,    a$arr, tolerance = 1e-6)
  expect_equal(fossil_out, arr2,  tolerance = 1e-6)
})

# ── obs_nc: global attributes ─────────────────────────────────────────────────

test_that("obs_nc writes the TITLE global attribute", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc    <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  title <- ncdf4::ncatt_get(nc, 0, "TITLE")$value

  expect_equal(title, "rtorf::obs_nc")
})

test_that("obs_nc writes the History global attribute", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc      <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  history <- ncdf4::ncatt_get(nc, 0, "History")$value

  expect_match(history, "^created on ")
})

test_that("obs_nc writes the Author global attribute containing rtorf version", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc     <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)
  author <- ncdf4::ncatt_get(nc, 0, "Author")$value

  expect_match(author, "rtorf")
  expect_match(author, "ncdf4")
})

# ── obs_nc: verbose flag ───────────────────────────────────────────────────────

test_that("obs_nc with verbose = TRUE prints the output path", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  output <- capture.output(
    obs_nc(
      lat      = a$lat,
      lon      = a$lon,
      time_nc  = a$time_nc,
      vars_out = "total",
      units_out = "ppm",
      nc_out   = tmp,
      larrays  = list(total = a$arr),
      verbose  = TRUE
    )
  )

  expect_true(any(grepl("Writting", output)))
  expect_true(any(grepl(basename(tmp), output)))
})

test_that("obs_nc with verbose = FALSE prints nothing", {
  a   <- make_args()
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  output <- capture.output(
    obs_nc(
      lat      = a$lat,
      lon      = a$lon,
      time_nc  = a$time_nc,
      vars_out = "total",
      units_out = "ppm",
      nc_out   = tmp,
      larrays  = list(total = a$arr),
      verbose  = FALSE
    )
  )

  expect_length(output, 0L)
})

# ── obs_nc_get: input validation ───────────────────────────────────────────────

test_that("obs_nc_get stops when nc_path has length > 1", {
  expect_error(
    obs_nc_get(nc_path = c("a.nc", "b.nc")),
    regexp = "nc_path must be a single string"
  )
})

test_that("obs_nc_get stops when nc_name has length > 1", {
  expect_error(
    obs_nc_get(nc_path = "dummy.nc", nc_name = c("foot1", "foot2")),
    regexp = "nc_name must be a single string"
  )
})

test_that("obs_nc_get stops when nc_lat has length > 1", {
  expect_error(
    obs_nc_get(nc_path = "dummy.nc", nc_lat = c("lat1", "lat2")),
    regexp = "nc_lat must be a single string"
  )
})

test_that("obs_nc_get stops when nc_lon has length > 1", {
  expect_error(
    obs_nc_get(nc_path = "dummy.nc", nc_lon = c("lon1", "lon2")),
    regexp = "nc_lon must be a single string"
  )
})

# ── obs_nc / obs_nc_get round-trip ────────────────────────────────────────────
# Build a NetCDF with obs_nc using default-compatible dimension names
# then read dimension values back via ncdf4 directly.

test_that("obs_nc round-trip: all dimension sizes match inputs", {
  nlat <- 4L; nlon <- 5L; ntime <- 3L
  a    <- make_args(nlat = nlat, nlon = nlon, ntime = ntime)
  tmp  <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc,
    vars_out = c("total", "bio"),
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr, bio = a$arr / 2)
  )

  nc <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  expect_equal(nc$dim$latitude$len,  nlat)
  expect_equal(nc$dim$longitude$len, nlon)
  expect_equal(nc$dim$Time$len,      ntime)
})

test_that("obs_nc round-trip: single time step is handled correctly", {
  a   <- make_args(ntime = 1L)
  tmp <- tempfile(fileext = ".nc")
  on.exit(unlink(tmp))

  obs_nc(
    lat      = a$lat,
    lon      = a$lon,
    time_nc  = a$time_nc[1L],   # scalar POSIXct
    vars_out = "total",
    units_out = "ppm",
    nc_out   = tmp,
    larrays  = list(total = a$arr)
  )

  nc <- ncdf4::nc_open(tmp)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  expect_equal(nc$dim$Time$len, 1L)
})
