pkgname <- "rtorf"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('rtorf')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("fex")
### * fex

flush(stderr()); flush(stdout())

### Name: fex
### Title: File extension
### Aliases: fex

### ** Examples

## Not run: 
##D # Do not run
## End(Not run)



cleanEx()
nameEx("get_nt")
### * get_nt

flush(stderr()); flush(stdout())

### Name: get_nt
### Title: Check the max number of threads
### Aliases: get_nt

### ** Examples

{
  get_nt()
}



cleanEx()
nameEx("obs_addltime")
### * obs_addltime

flush(stderr()); flush(stdout())

### Name: obs_addltime
### Title: add local time
### Aliases: obs_addltime

### ** Examples

## Not run: 
##D # Do not run
## End(Not run)



cleanEx()
nameEx("obs_addmtime")
### * obs_addmtime

flush(stderr()); flush(stdout())

### Name: obs_addmtime
### Title: Add matlab time
### Aliases: obs_addmtime

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <-  obs_summary(obs)
##D dt <- obs_read(index)
##D dt <- obs_addtime(dt)
## End(Not run)



cleanEx()
nameEx("obs_addstime")
### * obs_addstime

flush(stderr()); flush(stdout())

### Name: obs_addstime
### Title: Add solar time
### Aliases: obs_addstime

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <-  obs_summary(obs)
##D dt <- obs_read(index)
##D dt <- obs_addtime(dt)
## End(Not run)



cleanEx()
nameEx("obs_addtime")
### * obs_addtime

flush(stderr()); flush(stdout())

### Name: obs_addtime
### Title: Add times
### Aliases: obs_addtime

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <-  obs_summary(obs)
##D dt <- obs_read(index)
##D dt <- obs_addtime(dt)
## End(Not run)



cleanEx()
nameEx("obs_agg")
### * obs_agg

flush(stderr()); flush(stdout())

### Name: obs_agg
### Title: Aggregates Observations by time
### Aliases: obs_agg

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
## End(Not run)



cleanEx()
nameEx("obs_convolve")
### * obs_convolve

flush(stderr()); flush(stdout())

### Name: obs_convolve
### Title: obs_convolve
### Aliases: obs_convolve

### ** Examples

## Not run: 
##D # do not run
## End(Not run)



cleanEx()
nameEx("obs_find_receptors")
### * obs_find_receptors

flush(stderr()); flush(stdout())

### Name: obs_find_receptors
### Title: Compares expected receptors
### Aliases: obs_find_receptors

### ** Examples

## Not run: 
##D # do not run
##D p <- "/path/to/continuous/"
##D # here we have year/month/hysplit*.nc
##D x <- dt
##D dt <- obs_find_receptors(p, year, month....)
## End(Not run)



cleanEx()
nameEx("obs_foot")
### * obs_foot

flush(stderr()); flush(stdout())

### Name: obs_foot
### Title: obs_footprints
### Aliases: obs_foot

### ** Examples

## Not run: 
##D # do not run
## End(Not run)



cleanEx()
nameEx("obs_footname")
### * obs_footname

flush(stderr()); flush(stdout())

### Name: obs_footname
### Title: Expected footprint name
### Aliases: obs_footname

### ** Examples

## Not run: 
##D # Do not run
##D obs_footname(time = Sys.time(),
##D              second = data.table::second(Sys.Time()),
##D              lat = 0,
##D              lon = 0,
##D              alt = 0)
##D obs_footname(year = 2020,
##D              month = 12,
##D              day = 30,
##D              hour = 9,
##D              minute = 54,
##D              lat = 3.2133,
##D              lon = 30.9131,
##D              alt = 497,
##D              fullpath = TRUE)
##D obs_footname(year = 2020,
##D              month = 12,
##D              day = 30,
##D              hour = 9,
##D              minute = 54,
##D              lat = 1,
##D              lon = -130.9131,
##D              alt = 497,
##D              fullpath = TRUE)
## End(Not run)



cleanEx()
nameEx("obs_format")
### * obs_format

flush(stderr()); flush(stdout())

### Name: obs_format
### Title: Formatting data
### Aliases: obs_format

### ** Examples

## Not run: 
##D # do not run
## End(Not run)



cleanEx()
nameEx("obs_freq")
### * obs_freq

flush(stderr()); flush(stdout())

### Name: obs_freq
### Title: return numeric vector in intervals
### Aliases: obs_freq

### ** Examples

## Not run: 
##D # Do not run
## End(Not run)



cleanEx()
nameEx("obs_grid")
### * obs_grid

flush(stderr()); flush(stdout())

### Name: obs_grid
### Title: obs_grid
### Aliases: obs_grid

### ** Examples

obs_grid(1, 10, 1, 10, 100, 100, 1)



cleanEx()
nameEx("obs_grid_check")
### * obs_grid_check

flush(stderr()); flush(stdout())

### Name: obs_grid_check
### Title: Conservation check for gridded footprints
### Aliases: obs_grid_check

### ** Examples

## Not run: 
##D bs <- obs_grid_simple(d, lon_min, lat_min, lon_max, lat_max)
##D gk <- obs_grid_kernel(d, lon_min, lat_min, lon_max, lat_max,
##D                       bandwidth = 0.2)
##D chk <- obs_grid_check(bs, gk)
##D chk["relative_diff_pct"]
## End(Not run)




cleanEx()
nameEx("obs_grid_kernel")
### * obs_grid_kernel

flush(stderr()); flush(stdout())

### Name: obs_grid_kernel
### Title: Gaussian kernel footprint gridding
### Aliases: obs_grid_kernel

### ** Examples

## Not run: 
##D library(data.table)
##D dat <- fread("PARTICLE.DAT")
##D d   <- dat[time %in% sort(unique(time))[1:4]]
##D 
##D # Cartesian mode (degrees, fast)
##D gk <- obs_grid_kernel(
##D   x             = d,
##D   lon_min       = floor(min(d$lon))   - 0.1,
##D   lat_min       = floor(min(d$lat))   - 0.1,
##D   lon_max       = ceiling(max(d$lon)) + 0.1,
##D   lat_max       = ceiling(max(d$lat)) + 0.1,
##D   lon_res       = 0.1,
##D   lat_res       = 0.1,
##D   bandwidth     = 0.2,          # 2-cell sigma in degrees
##D   use_haversine = FALSE,
##D   n_threads     = 4L
##D )
##D image(gk$lon, gk$lat, log1p(gk$grid), main = "Gaussian kernel")
##D 
##D # Haversine mode (metres, accurate at high latitudes)
##D gk_hav <- obs_grid_kernel(
##D   x             = d,
##D   lon_min       = -130, lat_min = 55,
##D   lon_max       = -100, lat_max = 75,
##D   lon_res       = 0.1,  lat_res = 0.1,
##D   bandwidth     = 15000,        # 15 km
##D   use_haversine = TRUE,
##D   n_threads     = 4L
##D )
## End(Not run)




cleanEx()
nameEx("obs_grid_simple")
### * obs_grid_simple

flush(stderr()); flush(stdout())

### Name: obs_grid_simple
### Title: Bin-and-sum footprint gridding
### Aliases: obs_grid_simple

### ** Examples

## Not run: 
##D library(data.table)
##D dat <- fread("PARTICLE.DAT")
##D d   <- dat[time %in% sort(unique(time))[1:4]]   # first hour
##D 
##D bs <- obs_grid_simple(
##D   x         = d,
##D   lon_min   = floor(min(d$lon))   - 0.1,
##D   lat_min   = floor(min(d$lat))   - 0.1,
##D   lon_max   = ceiling(max(d$lon)) + 0.1,
##D   lat_max   = ceiling(max(d$lat)) + 0.1,
##D   res       = 0.1,
##D   n_threads = 4L
##D )
##D image(bs$lon, bs$lat, log1p(bs$grid), main = "Bin and sum")
## End(Not run)




cleanEx()
nameEx("obs_hysplit_ascdata")
### * obs_hysplit_ascdata

flush(stderr()); flush(stdout())

### Name: obs_hysplit_ascdata
### Title: obs_hysplit_ascdata
### Aliases: obs_hysplit_ascdata

### ** Examples

{
# Do not run
ascdata_file <- tempfile()
obs_hysplit_ascdata(ascdata = ascdata_file)
cat(readLines(ascdata_file), sep =  "\n")
}



cleanEx()
nameEx("obs_hysplit_control")
### * obs_hysplit_control

flush(stderr()); flush(stdout())

### Name: obs_hysplit_control
### Title: obs_hysplit_control
### Aliases: obs_hysplit_control

### ** Examples

{
# Do not run
obs <- system.file("data-raw", package = "rtorf")
index <- obs_summary(obs)
dt <- obs_read(index)
df <- dt[1]
control_file <- tempfile()
obs_hysplit_control(df, control = control_file)
ff <- readLines(control_file)

cat(ff, sep =  "\n")
}



cleanEx()
nameEx("obs_hysplit_control_read")
### * obs_hysplit_control_read

flush(stderr()); flush(stdout())

### Name: obs_hysplit_control_read
### Title: obs_hysplit_control_read
### Aliases: obs_hysplit_control_read

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
##D df <- dt[1]
##D control_file <- tempfile()
##D obs_hysplit_control(df, control = control_file)
##D ff <- readLines(control_file)
##D 
##D cat(ff, sep =  "\n")
##D obs_hysplit_control_read(control_file)
##D 
## End(Not run)



cleanEx()
nameEx("obs_hysplit_setup")
### * obs_hysplit_setup

flush(stderr()); flush(stdout())

### Name: obs_hysplit_setup
### Title: obs_hysplit_setup
### Aliases: obs_hysplit_setup

### ** Examples

{
# Do not run
# default
setup_file <- tempfile()
obs_hysplit_setup(setup = setup_file)
cat(readLines(setup_file),sep =  "\n")
# bypass
setup_file <- tempfile()
obs_hysplit_setup(bypass_params = c(lala = 1), setup = setup_file)
cat(readLines(setup_file),sep =  "\n")
}



cleanEx()
nameEx("obs_id2pos")
### * obs_id2pos

flush(stderr()); flush(stdout())

### Name: obs_id2pos
### Title: obs_id2pos
### Aliases: obs_id2pos

### ** Examples

## Not run: 
##D # Do not run
##D id <- '2002x08x03x10x45.00Nx090.00Ex00030'
##D (obs_id2pos(id, asdf = TRUE) -> dx)
##D id <- c('2002x08x03x10x00x45.000Nx090.000Ex00030',
##D         '2002x08x03x10x55x45.335Sx179.884Wx00030')
##D (obs_id2pos(id) -> dx)
##D (obs_id2pos(id, asdf = TRUE) -> dx)
##D (obs_id2pos(rep(id, 2)) -> dx)
##D (obs_id2pos(rep(id, 2), asdf = TRUE) -> dx)
## End(Not run)



cleanEx()
nameEx("obs_info2id")
### * obs_info2id

flush(stderr()); flush(stdout())

### Name: obs_info2id
### Title: obs_info2id
### Aliases: obs_info2id

### ** Examples

## Not run: 
##D # Do not run
##D obs_info2id(yr = 2002,
##D             mo = 8,
##D             dy = 3,
##D             hr = 10,
##D             mn = 0,
##D             lat = 42,
##D             lon = -90,
##D             alt = 1) [1]
## End(Not run)



cleanEx()
nameEx("obs_julian")
### * obs_julian

flush(stderr()); flush(stdout())

### Name: obs_julian
### Title: obs_julian
### Aliases: obs_julian

### ** Examples

## Not run: 
##D # Do not run
##D obs_julian(1, 2020, 1)
## End(Not run)
## Not run: 
##D # Do not run
##D obs_julian(y = 2002,
##D             m = 8,
##D             d = 3,
##D             legacy = TRUE)
##D obs_julian(y = 2002,
##D             m = 8,
##D             d = 3,
##D             legacy = FALSE)
## End(Not run)



cleanEx()
nameEx("obs_list.dt")
### * obs_list.dt

flush(stderr()); flush(stdout())

### Name: obs_list.dt
### Title: list.dt
### Aliases: obs_list.dt

### ** Examples

## Not run: 
##D # Do not run
## End(Not run)



cleanEx()
nameEx("obs_meta")
### * obs_meta

flush(stderr()); flush(stdout())

### Name: obs_meta
### Title: Read obspack metadata
### Aliases: obs_meta

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_meta(index)
## End(Not run)



cleanEx()
nameEx("obs_nc")
### * obs_nc

flush(stderr()); flush(stdout())

### Name: obs_nc
### Title: obs_nc
### Aliases: obs_nc

### ** Examples




cleanEx()
nameEx("obs_nc_get")
### * obs_nc_get

flush(stderr()); flush(stdout())

### Name: obs_nc_get
### Title: obs_nc_get
### Aliases: obs_nc_get

### ** Examples




cleanEx()
nameEx("obs_normalize_dmass")
### * obs_normalize_dmass

flush(stderr()); flush(stdout())

### Name: obs_normalize_dmass
### Title: obs_normalize_dmass
### Aliases: obs_normalize_dmass

### ** Examples

## Not run: 
##D # Do not run
## End(Not run)
#' # Do not run



cleanEx()
nameEx("obs_out")
### * obs_out

flush(stderr()); flush(stdout())

### Name: obs_out
### Title: outersect
### Aliases: obs_out

### ** Examples

## Not run: 
##D #do not run
## End(Not run)



cleanEx()
nameEx("obs_plot")
### * obs_plot

flush(stderr()); flush(stdout())

### Name: obs_plot
### Title: Read obspack metadata
### Aliases: obs_plot

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
##D obs_plot(dt, time = "time")
## End(Not run)



cleanEx()
nameEx("obs_rbind")
### * obs_rbind

flush(stderr()); flush(stdout())

### Name: obs_rbind
### Title: rbind obspack
### Aliases: obs_rbind

### ** Examples

## Not run: 
##D # Do not run
## End(Not run)



cleanEx()
nameEx("obs_read")
### * obs_read

flush(stderr()); flush(stdout())

### Name: obs_read
### Title: Read obspack (.txt)
### Aliases: obs_read

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
##D obs_read(index, expr = "altitude_final == '5800'")
## End(Not run)



cleanEx()
nameEx("obs_read_csvy")
### * obs_read_csvy

flush(stderr()); flush(stdout())

### Name: obs_read_csvy
### Title: reads CSVY
### Aliases: obs_read_csvy

### ** Examples

## Not run: 
##D # Do not run
##D df <- data.frame(a = rnorm(n = 10),
##D                  time = Sys.time() + 1:10)
##D 
##D f <- paste0(tempfile(), ".csvy")
##D notes <- c("notes",
##D            "more notes")
##D obs_write_csvy(dt = df, notes = notes, out = f)
##D s <- obs_read_csvy(f)
##D s
##D # or
##D readLines(f)
##D data.table::fread(f)
## End(Not run)



cleanEx()
nameEx("obs_read_nc")
### * obs_read_nc

flush(stderr()); flush(stdout())

### Name: obs_read_nc
### Title: Read obspack (.nc)
### Aliases: obs_read_nc

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
## End(Not run)



cleanEx()
nameEx("obs_read_nc_att")
### * obs_read_nc_att

flush(stderr()); flush(stdout())

### Name: obs_read_nc_att
### Title: Read obspack attributes (.nc)
### Aliases: obs_read_nc_att

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
## End(Not run)



cleanEx()
nameEx("obs_roundtime")
### * obs_roundtime

flush(stderr()); flush(stdout())

### Name: obs_roundtime
### Title: round seconds from "POSIXct" "POSIXt" classes
### Aliases: obs_roundtime

### ** Examples

## Not run: 
##D # Do not run
##D x <- Sys.time() + seq(1, 55, 1)
##D paste0(x,"  ",
##D        obs_roundtime(x), "  ",
##D        obs_freq(data.table::second(x),
##D                 seq(0, 55, 10)))
## End(Not run)



cleanEx()
nameEx("obs_select_sec")
### * obs_select_sec

flush(stderr()); flush(stdout())

### Name: obs_select_sec
### Title: Select Observations by closest time (seconds)
### Aliases: obs_select_sec

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
##D dx <- obs_read(index, expr = "altitude_final == '5800'")
##D dx <- obs_addtime(dx[, -"site_code"])
##D dy <- obs_select_sec(dx)
## End(Not run)



cleanEx()
nameEx("obs_summary")
### * obs_summary

flush(stderr()); flush(stdout())

### Name: obs_summary
### Title: Summary of the ObsPack files (.txt)
### Aliases: obs_summary obs_index

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
## End(Not run)
{
## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
## End(Not run)
}



cleanEx()
nameEx("obs_table")
### * obs_table

flush(stderr()); flush(stdout())

### Name: obs_table
### Title: Obspack Table Summary
### Aliases: obs_table

### ** Examples

## Not run: 
##D # Do not run
##D obs <- system.file("data-raw", package = "rtorf")
##D index <- obs_summary(obs)
##D dt <- obs_read(index)
##D dx <- obs_table(dt)
## End(Not run)



cleanEx()
nameEx("obs_traj_foot")
### * obs_traj_foot

flush(stderr()); flush(stdout())

### Name: obs_traj_foot
### Title: obs_traj_foot
### Aliases: obs_traj_foot

### ** Examples

## Not run: 
##D # Do not run
## End(Not run)



cleanEx()
nameEx("obs_trunc")
### * obs_trunc

flush(stderr()); flush(stdout())

### Name: obs_trunc
### Title: Trunc numbers with a desired number of decimals
### Aliases: obs_trunc

### ** Examples

## Not run: 
##D # Do not run
##D # in bash:
##D # printf "%07.4f" 72.05785
##D # results in 72.0578
##D # but:
##D formatC(72.05785, digits = 4, width = 8, format = "f", flag = "0")
##D # results in
##D "072.0579"
##D # the goal is to obtain the same trunc number as using bash, then:
##D formatC(obs_trunc(72.05785, 4),
##D         digits = 4,
##D         width = 8,
##D         format = "f",
##D         flag = "0")
## End(Not run)



cleanEx()
nameEx("obs_write_csvy")
### * obs_write_csvy

flush(stderr()); flush(stdout())

### Name: obs_write_csvy
### Title: Generates YAML and write data.frame
### Aliases: obs_write_csvy

### ** Examples

## Not run: 
##D # Do not run
##D df <- data.frame(a = rnorm(n = 10),
##D                  time = Sys.time() + 1:10)
##D 
##D f <- paste0(tempfile(), ".csvy")
##D notes <- c("notes",
##D            "more notes")
##D obs_write_csvy(dt = df, notes = notes, out = f)
##D readLines(f)
##D data.table::fread(f, h = TRUE)
## End(Not run)



cleanEx()
nameEx("rtorf-deprecated")
### * rtorf-deprecated

flush(stderr()); flush(stdout())

### Name: rtorf-deprecated
### Title: Deprecated functions in package 'rtorf'.
### Aliases: rtorf-deprecated obs_addzero
### Keywords: internal

### ** Examples

{## Not run: 
##D #do not run
## End(Not run)
}



cleanEx()
nameEx("sr")
### * sr

flush(stderr()); flush(stdout())

### Name: sr
### Title: Extacts n last characters
### Aliases: sr

### ** Examples

## Not run: 
##D # do not run
## End(Not run)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
