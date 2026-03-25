#' obs_hysplit_control
#'
#' This function creates a CONTROL file for HYSPLIT model.
#' It uses inputs from a data.frame with the receptor information.
#'
#' @param df data.frame of receptor information. Must include, "year",
#' "month", "day", "hour" (0:23), "minute", "latitude", "longitude", "altitude"
#' @param year year, if missing df.
#' @param month month, if missing df.
#' @param day day, if missing df.
#' @param hour hour, if missing df.
#' @param minute minute, if missing df.
#' @param lat latitude, if missing df.
#' @param lon longitude, if missing df.
#' @param alt altitude, if missing df.
#' @param start_time String to be added, default derived from df.
#' @param nlocations number of locations.
#' @param duration number of hours of release. (Negative is backwards in time).
#' @param vertical_motion Vertical motion option.  (0:data 1:isob 2:isen 3:dens
#'  4:sigma 5:diverg 6:msl2agl 7:average 8:damped).
#'  . The default "data" selection will use the meteorological model's
#'  vertical velocity fields; other options include isobaric, isentropic,
#'  constant density, constant internal sigma coordinate, computed from the
#'  velocity divergence, a special transformation to correct the vertical
#'  velocities when mapped from quasi-horizontal surfaces (such as relative
#'   to MSL) to HYSPLIT's internal terrain following sigma coordinate, and a
#'   special option (7) to spatially average the vertical velocity.
#'   The averaging distance is automatically computed from the ratio of
#'   the temporal frequency of the data to the horizontal grid resolution.
#'   Default 5
#' @param top_model_domain altitude above ground level (m).
#' @param met meteorological models to be used.
#' @param nmet Number of days for the meteorological files. Default is
#' number of days in duration plus two days.
#' nmet is the number of simultaneous input meteorological files. For instance,
#' 11 means that for each meteorological grid, 11 files are expected. Usually,
#' the files are daily. Note that the same number of files are required for
#' each grid in this approach. Hysplit expects something like 2 11,
#' which means 2 meteorological grids with 11 files each. The number
#' 2 comes from the length of met files.
#' @param metpath paths for each meteorological model output.
#' @param metformat format to applied to the meteorological daily file
#' @param ngases Default 1.
#' @param gas default "Foot".
#' @param emissions_rate Default 0
#' @param hour_emissions hour release, depend of type of release, instantaneous 0.01,
#' continuous 1.
#' @param release_start derived from df.
#' @param nsim_grids Number of simulated grids.
#' @param center_conc_grids center coordinates for conc grid.
#' @param grid_spacing grid spacing, default is 0.1 degree.
#' @param grid_span model extension in degrees by dimension.
#' @param nconc name of the concentration file, default is "cdump"
#' @param nvert_levels number of vertical levels
#' @param height_vert_levels hright vertical levels (50)
#' @param sampling_start_time 2 digits year, month, day, hour, minute
#' @param sampling_end_time  2 digits year, month, day, hour, minute
#' @param sampling_interval_type  type, hour, minure
#' @param npol_depositing number of pollutant depositing
#' @param particle_params Particle diameter, density, and shape
#' @param dmwsrdre DepVel, MW, SurfReRa, DifRa, EHenry
#' @param wrhcicbc Wet removal: HC, incloud, belowcloud
#' @param radiactive_decay days
#' @param pol_res Pollutant Resuspension (1/m)
#' @param control name of the file, default "CONTROL"
#' @param verbose logical, to show more information
#' @return A CONTROL file
#' @export
#' @examples {
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' df <- dt[1]
#' control_file <- tempfile()
#' obs_hysplit_control(df, control = control_file)
#' ff <- readLines(control_file)
#'
#' cat(ff, sep =  "\n")
#' }
#
obs_hysplit_control <- function(
  df,
  year,
  month,
  day,
  hour,
  minute,
  lat,
  lon,
  alt,
  start_time = NULL,
  nlocations = 1,
  duration = -240,
  vertical_motion = 5,
  top_model_domain = 10000,
  met = c("hrrr", "nams", "gfs0p25"),
  nmet = abs(duration / 24) + 1, # 10 days plus 1, defaultdays
  metpath = c(
    "/work/noaa/lpdm/metfiles/hrrr/",
    "/work/noaa/lpdm/metfiles/nams/",
    "/work/noaa/lpdm/metfiles/gfs0p25/"
  ),
  metformat = c(
    hrrr = "%Y%m%d_hrrr",
    nams = "%Y%m%d_hysplit.t00z.namsa",
    gfs0p25 = "%Y%m%d_gfs0p25",
    era5 = "ERA5_%Y%m%d.ARL"
  ),
  ngases = 1,
  gas = "Foot",
  emissions_rate = 0,
  hour_emissions = 0.01,
  release_start = NULL,
  nsim_grids = 1,
  center_conc_grids = c(0, 0),
  grid_spacing = c(0.1, 0.1),
  grid_span = c(30, 30),
  nconc = "cdump",
  nvert_levels = 1,
  height_vert_levels = 50,
  sampling_start_time = c(0, 0, 0, 0, 0),
  sampling_end_time = c(0, 0, 0, 0, 0),
  sampling_interval_type = c(0, abs(duration), 0),
  npol_depositing = 1,
  particle_params = c(0, 0, 0),
  dmwsrdre = c(0, 0, 0, 0, 0),
  wrhcicbc = c(0, 0, 0),
  radiactive_decay = 0,
  pol_res = 0,
  control = "CONTROL",
  verbose = FALSE
) {
  if (!missing(df)) {
    if (
      !"latitude" %in% names(df) ||
        !"longitude" %in% names(df) ||
        !"altitude" %in% names(df) ||
        !"year" %in% names(df) ||
        !"month" %in% names(df) ||
        !"day" %in% names(df) ||
        !"hour" %in% names(df) ||
        !"minute" %in% names(df)
    ) {
      stop(
        "Data frame must contain 'latitude', 'longitude', 'altitude',
           'year', 'month', 'day', 'hour', and 'minute' columns."
      )
    }
    # lat lon height
    lat <- df$latitude
    lon <- df$longitude
    agl <- df$altitude

    lat <- sprintf(lat, fmt = '%#.4f')

    lon <- sprintf(lon, fmt = '%#.4f')

    yr <- df$year
    mo <- df$month
    dy <- df$day
    ho <- df$hour
    mi <- df$minute
  } else {
    # lat lon height
    lat <- sprintf(lat, fmt = '%#.4f')

    lon <- sprintf(lon, fmt = '%#.4f')

    yr <- year
    mo <- month
    dy <- day
    ho <- hour
    mi <- minute
    agl <- alt
  }

  start_loc <- paste(lat, lon, sprintf("%#.1f", agl))

  if (nchar(yr) != 4) {
    stop("year must have for digits")
  }

  rel_start <- paste(
    substr(yr, 3, 4), #I need to confirm
    sprintf(mo, fmt = '%02d'),
    sprintf(dy, fmt = '%02d'),
    sprintf(ho, fmt = '%02d'),
    sprintf(mi, fmt = '%02d')
  )

  if (verbose) {
    print(paste0("start: ", rel_start))
  }

  if (missing(start_time)) {
    start_time <- rel_start
  }

  if (!is.character(start_time)) {
    stop("start_time neds to be character with format yy mm dd hh mm")
  }
  nmodels <- length(met)

  hydate <- as.Date(ISOdate(
    year = yr,
    month = mo,
    day = dy,
    hour = ho,
    min = mi,
    sec = 0,
    tz = "UTC"
  ))

  if (verbose) {
    print(paste0("hydate: ", hydate))
  }

  if (verbose) {
    print(paste("location:", lat, lon, sprintf("%#.1f", agl)))
  }

  hydate <- hydate + 1

  # magick ####

  sink(control)

  cat(start_time)
  cat("\n")

  cat(nlocations, sep = "\n")

  start_loc <- paste(lat, lon, sprintf("%#.1f", agl))

  cat(lat)
  cat(" ")

  cat(lon)
  cat(" ")

  cat(sprintf("%#.1f", agl))
  cat("\n")

  cat(duration)
  cat("\n")

  cat(vertical_motion)
  cat("\n")

  cat(sprintf("%#.1f", top_model_domain))
  cat("\n")

  if (length(met) > 1) {
    cat(paste(length(met), nmet))
  } else {
    cat(nmet)
  }

  cat("\n")

  if (is.null(names(metformat))) {
    stop("metformat must be a named vector")
  }

  for (j in seq_along(met)) {
    metx <- met[j]

    if (metx %in% c("nams", "gfs0p25", "hrrr", "era5")) {
      for (i in 1:nmet) {
        hyd <- as.Date(hydate - i)

        cat(
          paste0(metpath[j], data.table::year(hyd), "/")
        )
        cat("\n")

        cat(
          strftime(hyd, metformat[[metx]], tz = "UTC")
        )
        cat("\n")
      }
    } else {
      stop("met can be only nams, gfs0p25, hrrr, or era5")
    }
  }

  cat(ngases)
  cat("\n")

  cat(gas)
  cat("\n")

  cat(emissions_rate)
  cat("\n")

  cat(hour_emissions)
  cat("\n")

  cat(rel_start)
  cat("\n")

  cat(nsim_grids)
  cat("\n")

  cat(sprintf("%#.1f", center_conc_grids[1]))

  cat(" ")

  cat(sprintf("%#.1f", center_conc_grids[2]))

  cat("\n")

  cat(sprintf("%#.2f", grid_spacing[1]))

  cat(" ")

  cat(sprintf("%#.2f", grid_spacing[2]))

  cat("\n")

  cat(sprintf("%#.1f", grid_span[1]))

  cat(" ")

  cat(sprintf("%#.1f", grid_span[2]))

  cat("\n")

  cat("./\n")

  cat(nconc)
  cat("\n")

  cat(nvert_levels)
  cat("\n")

  cat(height_vert_levels)
  cat("\n")

  cat(sprintf(fmt = "%02d", sampling_start_time))
  cat("\n")

  cat(sprintf(fmt = "%02d", sampling_end_time))
  cat("\n")

  cat(sprintf(fmt = "%02d", sampling_interval_type))
  cat("\n")

  cat(npol_depositing)
  cat("\n")

  cat(sprintf(fmt = "%#.1f", particle_params))
  cat("\n")

  cat(sprintf(fmt = "%#.1f", dmwsrdre))
  cat("\n")

  cat(sprintf(fmt = "%#.1f", wrhcicbc))
  cat("\n")

  cat(sprintf(fmt = "%#.1f", radiactive_decay))
  cat("\n")

  cat(sprintf(fmt = "%#.1f", pol_res))
  cat("\n")

  sink()
}


#' obs_hysplit_setup
#'
#' This function creates a SETUP.CFG file for HYSPLIT model.
#'
#' @param idsp particle dispersion scheme 1:HYSPLIT 2:STILT
#' @param capemin -1 no convection; -2 Grell convection scheme;
#' -3 extreme convection; >0 enhanced vertical mixing when CAPE exceeds this value (J/kg)
#' @param vscales vertical Lagrangian time scale (sec) for stable PBL
#' @param kbls boundary layer stability derived from 1:fluxes 2:wind_temperature
#' @param kblt  boundary layer turbulence parameterizations 1:Beljaars 2:Kanthar
#'  3:TKE 4:Measured 5:Hanna
#' @param kmixd mixed layer obtained from 0:input 1:temperature 2:TKE 3:modified Richardson
#' @param initd initial distribution, particle, puff, or combination
#' @param veght Height below which particle's time is spent is tallied to
#' calculate footprint for PARTICLE_STILT.DAT less than or equal to 1.
#' 0: fraction of PBL height; greater than 1.0: heightAGL (m)
#' @param kmix0 minimum mixing depth
#' @param numpar number of puffs or particles to released per cycle
#' @param maxpar maximum number of particles carried in simulation
#' @param ichem chemistry conversion modules 0:none 1:matrix 2:convert 3:dust
#' 4: conc grid equal to met grid, 5: divide output mass by air density (kg/m3)
#' to sum as mixing ration, 7: transport deposited particles on the ocean surface,
#' 8: stilt mode mixing ratio and varying layer,
#' 9: set concentration layer one to a fraction of the boundary layer,
#' 10: restructure concentration grid into time-varying transfer matrix,
#' 11: enable daughter produyct calculation.
#' @param krand 0 method to calculate random number
#' 0=precompute if NUMPAR greater than 5000 or dynamic if NUMPAR less than or equal to 5000;
#' 1=precalculated; 2=calculated in pardsp;
#' 3=none; 4=random initial seed number and calculated in pardsp;
#' 10=same as 0 with random initial seed for non-dispersion applications;
#' 11=same as 1 with random initial seed for non-dispersion applications;
#' 12=same as 2 with random initial seed for non-dispersion applications;
#' 13=same as 3 with random initial seed for non-dispersion applications
#' @param ivmax 0 number of variables written to PARTICLE_STILT.DAT. Must
#' equal the number of variables listed for variable VARSIWANT
#' @param varsiwant ='TIME','INDX','LONG','LATI','ZAGL','SIGW','TLGR','ZSFC', TEMP',
#' 'SAMT','FOOT','SHTF','DMAS','DENS','RHFR','SPHU','DSWF','WOUT','MLHT','PRES'
#' variables written to PARTICLE_STILT.DAT.
#'
#' However, the default in this case is:
#' c('time','indx', 'lati','long','zagl', 'zsfc','foot','samt', 'temp',
#' 'dswf','mlht','dens','dmas','sigw','tlgr'
#' @param outdt  Default 15. defines the output frequency in minutes of the endpoint positions
#' in the PARTICLE.DAT file when the STILT emulation mode is configured.
#' The default value of 0 results in output each time step while the positive
#' value gives the output frequency in minutes. A negative value disables output. The
#' output frequency should be an even multiple of the time step and be evenly
#' divisible into 60. In STILT emulation mode, the time step is forced to one minute.
#' @param extra_params more parameters
#' @param bypass_params named vector of characters to bypass all other arguments. If this
#' list is available, only the content of this list will be used to
#' write SETUP.CFG file and not other arguments.
#' @param setup Default SETUP.CFG
#' @return A SETUP.CFG file
#' @note The var description comes from hysplit 5.3 manual page 214
#'  From the hysplit user guide:
#' STILT mode
#' The STILT model incorporates the variation of HYSPLIT developed by Lin et al.
#' (2003 - JGR, VOL. 108, NO. D16, 4493, doi:10.1029/2002JD003161) that can be used to
#' estimate upwind surface fluxes from atmospheric measurements.
#' Two changes are introduced; the mass summation is divided by air density
#' resulting in a mixing ratio output field (ICHEM=6) and the lowest
#' concentration summation layer (concentration layer top depth) is permitted
#' to vary with the mixed layer depth (ICHEM=9). The ICHEM=8 switch turns on
#' both density and varying layer depth. Two text files of particle position
#'  information (PARTICLE.DAT and PARTICLE_STILT.DAT) at each time step will
#'  also be created unless the namelist parameter OUTDT defining the output
#'  interval (min) is changed. PARTICLE_STILT.DAT follows the same format as STILT.
#'  The footprint output in PARTICLE.DAT represents particles that were below
#'  50% of the mixed layer height. The footprint in PARTICLE_STILT.DAT represent
#'   particles that were below a user defined height (VEGHT).
#' @export
#' @examples {
#' # Do not run
#' # default
#' setup_file <- tempfile()
#' obs_hysplit_setup(setup = setup_file)
#' cat(readLines(setup_file),sep =  "\n")
#' # bypass
#' setup_file <- tempfile()
#' obs_hysplit_setup(bypass_params = c(lala = 1), setup = setup_file)
#' cat(readLines(setup_file),sep =  "\n")
#' }
#
obs_hysplit_setup <- function(
  idsp = 2,
  capemin = 500,
  vscales = -1.0,
  kbls = 1,
  kblt = 5,
  kmixd = 0,
  initd = 0,
  veght = 0.5,
  kmix0 = 150,
  numpar = 500,
  maxpar = 500,
  ichem = 8,
  krand = 4,
  varsiwant = c(
    'time',
    'indx',
    'lati',
    'long',
    'zagl',
    'zsfc',
    'foot',
    'samt',
    'temp',
    'dswf',
    'mlht',
    'dens',
    'dmas',
    'sigw',
    'tlgr'
  ),
  ivmax = length(varsiwant),
  outdt = 15,
  extra_params,
  bypass_params,
  setup = "SETUP.CFG"
) {
  # start
  write("&SETUP", file = setup)

  if (!missing(bypass_params)) {
    for (i in seq_along(bypass_params)) {
      bp <- paste0(" ", names(bypass_params)[i], " = ", bypass_params[i], ",")

      write(bp, file = setup, append = TRUE)
    }
  } else {
    write(paste0(" idsp = ", idsp, ","), file = setup, append = TRUE)

    write(paste0(" capemin = ", capemin, ","), file = setup, append = TRUE)

    write(
      paste0(" vscales = ", sprintf("%#.1f", vscales), ","),
      file = setup,
      append = TRUE
    )

    write(paste0(" kbls = ", kbls, ","), file = setup, append = TRUE)

    write(paste0(" kblt = ", kblt, ","), file = setup, append = TRUE)

    write(paste0(" kmixd = ", kmixd, ","), file = setup, append = TRUE)

    write(paste0(" initd = ", initd, ","), file = setup, append = TRUE)

    write(paste0(" veght = ", veght, ","), file = setup, append = TRUE)

    write(paste0(" kmix0 = ", kmix0, ","), file = setup, append = TRUE)

    write(paste0(" numpar = ", numpar, ","), file = setup, append = TRUE)

    write(paste0(" maxpar = ", maxpar, ","), file = setup, append = TRUE)

    write(paste0(" ichem = ", ichem, ","), file = setup, append = TRUE)

    write(paste0(" krand = ", krand, ","), file = setup, append = TRUE)

    write(paste0(" ivmax = ", ivmax, ","), file = setup, append = TRUE)

    write(
      paste0(
        " varsiwant=",
        paste(sQuote(varsiwant, q = ""), collapse = ","),
        sep = ","
      ),
      file = setup,
      append = TRUE
    )

    write(paste0(" outdt = ", outdt, ","), file = setup, append = TRUE)

    if (!missing(extra_params)) {
      for (i in seq_along(extra_params)) {
        ep <- paste0(
          " ",
          eval(parse(text = extra_params[i])),
          " = ",
          extra_params[i],
          ","
        )

        write(ep, file = setup, append = TRUE)
      }
    }
  }

  # end
  write("/", file = setup, append = TRUE)

  # sink(setup)
  #
  # cat("&SETUP\n")
  #
  # cat(paste0(" idsp = ", idsp, ",\n"))
  #
  # cat(paste0(" capemin = ", capemin, ",\n"))
  #
  # cat(paste0(" vscales = ", sprintf("%#.1f", vscales), ",\n"))
  #
  # cat(paste0(" kbls = ", kbls, ",\n"))
  #
  # cat(paste0(" kblt = ", kblt, ",\n"))
  #
  # cat(paste0(" kmixd = ", kmixd, ",\n"))
  #
  # cat(paste0(" initd = ", initd, ",\n"))
  #
  # cat(paste0(" veght = ", veght, ",\n"))
  #
  # cat(paste0(" kmix0 = ", kmix0, ",\n"))
  #
  # cat(paste0(" numpar = ", numpar, ",\n"))
  #
  # cat(paste0(" maxpar = ", maxpar, ",\n"))
  #
  # cat(paste0(" ichem = ", ichem, ",\n"))
  #
  # cat(paste0(" krand = ", krand, ",\n"))
  #
  # cat(paste0(" ivmax = ", ivmax, ",\n"))
  #
  # cat(" varsiwant=")
  #
  # cat(sQuote(varsiwant, q = ""), sep = ",")
  #
  # cat(",\n")
  #
  # cat(paste0(" outdt = ", outdt, ",\n"))
  #
  # if(!missing(extra_params)) {
  #
  #   for(i in seq_along(extra_params)) {
  #     cat(paste0(" ",
  #         eval(parse(text = extra_params[i])),
  #         " = ",
  #         extra_params[i],
  #         ",\n"))
  #   }
  # }
  # cat(" /\n")
  #
  # sink()
  # }
}


#' obs_hysplit_ascdata
#'
#' This function creates a ASCDATA.CFG file for HYSPLIT model.
#'
#' @param llc Lower left corner, default c(-90.0, -180.0)
#' @param spacing spacing in degress, default c(1.0, 1.0)
#' @param n number of data points, default c(180, 360)
#' @param landusecat land use category, default 2
#' @param rough default roughness length (meters), default 0.2
#' @param bdyfiles  directory location of the data files,
#' default '../bdyfiles/'
#' @param ascdata file, default ASCDATA.CFG
#' @return A ASCDATA.CFG file
#' @export
#' @examples {
#' # Do not run
#' ascdata_file <- tempfile()
#' obs_hysplit_ascdata(ascdata = ascdata_file)
#' cat(readLines(ascdata_file), sep =  "\n")
#' }
#
obs_hysplit_ascdata <- function(
  llc = c(-90.0, -180.0),
  spacing = c(1.0, 1.0),
  n = c(180, 360),
  landusecat = 2,
  rough = 0.2,
  bdyfiles = '../bdyfiles/',
  ascdata = "ASCDATA.CFG"
) {
  sink(ascdata)

  cat(sprintf("%#.1f", llc[1]))

  cat("  ")

  cat(sprintf("%#.1f", llc[2]))

  cat("  lat/lon of lower left corner (last record in file)\n")

  cat(sprintf("%#.1f", spacing[1]))

  cat("     ")

  cat(sprintf("%#.1f", spacing[2]))

  cat("    lat/lon spacing in degrees between data points\n")

  cat(n, sep = "     ")

  cat("    lat/lon number of data points\n")

  cat(landusecat)

  cat("              default land use category\n")

  cat(rough)

  cat("            default roughness length (meters)\n")

  cat(sQuote(bdyfiles, q = ""))

  cat(" directory location of data files\n")

  sink()
}


#' obs_hysplit_control_read
#'
#' This function creates a CONTROL file for HYSPLIT model.
#' It uses inputs from a data.frame with the receptor information.
#'
#' @param control CONTROL text file
#' @return A list with CONTROL information
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' df <- dt[1]
#' control_file <- tempfile()
#' obs_hysplit_control(df, control = control_file)
#' ff <- readLines(control_file)
#'
#' cat(ff, sep =  "\n")
#' obs_hysplit_control_read(control_file)
#'
#' }
obs_hysplit_control_read <- function(
  control = "CONTROL"
) {
  # null arguments for check
  time <- dt <- lat <- lon <- alt <- ident <- id <- NULL

  x <- readLines(control)

  # time
  t1 <- unlist(strsplit(x[1], " "))

  t2 <- ISOdate(
    year = year(as.POSIXct(t1[1], "%y", tz = "UTC")),
    month = t1[2],
    day = t1[3],
    hour = t1[4],
    0,
    0,
    tz = "UTC"
  )

  dt_time <- data.table::data.table(
    control = x[1],
    time = t2,
    year = year(t2),
    month = t1[2],
    day = t1[3],
    hour = t1[4],
    minute = 0,
    second = 0,
    track_hours = x[4]
  )

  dt_time$recep1 <- list(as.numeric(c(
    dt_time$year,
    dt_time$month,
    dt_time$day,
    dt_time$hour
  )))

  # space
  sp <- unlist(strsplit(x[3], " "))

  dt_space <- data.table::data.table(
    lat = sp[1],
    lon = sp[2],
    alt = sp[3],
    nlocations = x[2],
    vertical_motion = x[5],
    top_model = x[6]
  )

  # met
  met <- unlist(strsplit(x[7], " "))

  lmet <- lapply(met, as.numeric)

  metfiles <- list(
    models = lmet[[1]],
    files = x[8:(7 + lmet[[1]] * lmet[[2]] * 2)]
  )

  # emis
  end_met <- (7 + lmet[[1]] * lmet[[2]] * 2)

  dt_emis <- data.table::data.table(
    ngases = x[end_met + 1],
    gas = x[end_met + 2],
    emissions_rate = x[end_met + 3],
    hours_emissions = x[end_met + 4]
  )

  rt <- unlist(strsplit(x[end_met + 5], " "))

  trt <- ISOdate(
    year = year(as.POSIXct(rt[1], "%y", tz = "UTC")),
    month = rt[2],
    day = rt[3],
    hour = rt[4],
    min = rt[5],
    0,
    tz = "UTC"
  )

  dt_time_release <- data.table::data.table(
    control = x[end_met + 5],
    time = trt,
    year = year(trt),
    month = rt[2],
    day = rt[3],
    hour = rt[4],
    min = rt[5],
    sec = 0
  )

  dtemis <- cbind(dt_emis, dt_time_release)

  dtemis$hyemit.start <- list(as.numeric(c(
    dt_time_release$year,
    dt_time_release$month,
    dt_time_release$day,
    dt_time_release$hour
  )))

  # grid

  ngrids <- x[end_met + 6]

  cg <- unlist(strsplit(x[end_met + 7], " "))
  cg_lat <- cg[1]
  cg_lon <- cg[2]

  cgspc <- unlist(strsplit(x[end_met + 8], " "))
  cgspc_y <- cgspc[1]
  cgspc_x <- cgspc[2]

  cgspan <- unlist(strsplit(x[end_met + 9], " "))
  cgs_y <- cgspan[1]
  cgs_x <- cgspan[2]

  cgdir <- x[end_met + 10]

  nconc <- x[end_met + 11]

  num_vert <- x[end_met + 12]

  height_vert_lev <- x[end_met + 13]

  sampling_start_time <- x[end_met + 14]

  sampling_end_time <- x[end_met + 15]

  sampling_interval <- x[end_met + 16]

  dt_grid <- data.table::data.table(
    ngrids,
    center_grid_lat = cg_lat,
    center_grid_lon = cg_lon,
    spacing_y = cgspc[1],
    spacing_x = cgspc[2],
    span_x = cgspan[1],
    span_y = cgspan[2],
    dir = cgdir,
    name_conc = nconc,
    num_vertical_vels = num_vert,
    height_vertical_levels = height_vert_lev,
    sampling_start_time,
    sampling_end_time,
    sampling_interval
  )

  # particle deposition

  num_pol_depositing = x[end_met + 17]

  pd <- unlist(strsplit(x[end_met + 18], " "))

  particle_char <- x[end_met + 18]

  particle_diamter_um <- pd[1]

  density_gcc <- pd[2]

  shape <- pd[3]

  dep <- unlist(strsplit(x[end_met + 19], " "))

  dep_velocity <- dep[1]

  molecular_weight <- dep[2]

  surface_reactivity_ratio <- dep[3]

  diffusitivity_ratio <- dep[4]

  henry_constant <- dep[5]

  dt_pd <- data.table::data.table(
    particle_diamter_um,
    density_gcc,
    shape,
    dep_velocity,
    molecular_weight,
    surface_reactivity_ratio,
    diffusitivity_ratio,
    henry_constant
  )

  wet_removal <- unlist(strsplit(x[end_met + 20], " "))

  actual_henry <- wet_removal[1]

  in_cloud <- wet_removal[2]

  below_cloud <- wet_removal[3]

  radioactive_decay_half_life <- x[end_met + 21]

  pollutant_resuspension <- x[end_met + 22]

  dt_wr <- data.table::data.table(
    actual_henry,
    in_cloud,
    below_cloud,
    radioactive_decay_half_life,
    pollutant_resuspension
  )

  dt_receptor <- cbind(dt_time, dt_space)

  dt_receptor[,
    id := obs_footname(
      time = time,
      lat = as.numeric(lat),
      lon = as.numeric(lon),
      alt = as.numeric(alt)
    )
  ]

  dtemis[,
    ident := obs_footname(
      time = time,
      lat = as.numeric(dt_receptor$lat),
      lon = as.numeric(dt_receptor$lon),
      alt = as.numeric(dt_receptor$alt)
    )
  ]

  l_control <- list(
    receptor = dt_receptor,
    met = metfiles,
    emi = dtemis,
    grid = dt_grid,
    deposition = dt_pd,
    wet_removal = dt_wr
  )
  return(l_control)
}
