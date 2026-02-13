#' obs_convolve
#'
#' This function returns a arrays (or list of arrays) of convolved footpritns with flux
#'
#' @param foot_path path for footprint (length 1).
#' @param name_foot name of the footprint variable in the NetCDF file.
#' @param flon name of lons
#' @param flat name of lats
#' @param time_foot Time of the footprints (in the name file) or third dimension
#' @param flux String, default NOAA "CTCO2". Implies bio, ocn, fossil, fire.
#' @param df_flux data.table with columns f (full path) and nf file name 'Ymd.nc'
#' @param flux_format string with date format 'Ymd.nc'
#' @param name_var_flux string or numeric of position for the name var in the flux format.
#' e.g.: If missing, asusmes date ("2020-01-01"), if string something like ("flux"),
#' if numeric, identify the var names and you select which of them (first, second, etc)
#' @param factor number to multiply fluxes.
#' @param fn string with function to aggregate convolved fluxes, e.g. `mean`, `sum`, `max`, etc.
#' @param flip Logical, to vertically flip emissions, or not
#' @param as_list Logical, to return list of arrays
#' @param verbose Logical, to display more information
#' @return An array of footprints and convolved footprints, or a list of footprints, convolved fooprints,
#' latitudes, longitudes and time.
#' @note main assumption is that fluxes have same spatial dimensions as footprints
#' when flux is "monthly", "daily" or "yearly", it assumes flux variable has the same
#' name as "%y-%m-01", "%Y-%m-%d" or "%Y-01-01" respectively.
#' @export
#' @import ncdf4 data.table
#' @examples \dontrun{
#' # do not run
#' }
obs_convolve <- function(
  foot_path = "AAA",
  name_foot = "foot1",
  flon = "foot1lon",
  flat = "foot1lat",
  time_foot,
  flux = "CTCO2",
  df_flux,
  flux_format = "%Y%m%d.nc",
  name_var_flux = 1,
  factor = 1e9,
  fn = NULL,
  flip = FALSE,
  as_list = FALSE,
  verbose = TRUE
) {
  # footprint information

  if (length(foot_path) != 1) {
    stop("foot_path must be a single string")
  }

  if (verbose) {
    if (missing(name_var_flux)) {
      cat("Expected var as date, e.g.: '2020-12-01'\n")
    } else if (is.character(name_var_flux)) {
      cat("Reading flux var: ", name_var_flux, "\n")
    } else {
      cat("Reading flux var by position ", name_var_flux, "\n")
    }
  }

  if (verbose) {
    cat(paste0("Reading ", foot_path, "\n"))
  }

  if (missing(df_flux)) {
    stop(
      "df_flux is missing, it must be a data.table with columns `f` and `nf`"
    )
  }

  if (!inherits(df_flux, "data.table")) {
    stop(
      "df_flux must be a data.table with columns `f` (full path) and `nf` (file name)"
    )
  }

  if (!any("f" %in% names(df_flux))) {
    stop(
      "df_flux must be a data.table with columns `f` (full path) and `nf` (file name)"
    )
  }

  if (!any("nf" %in% names(df_flux))) {
    stop(
      "df_flux must be a data.table with columns `f` (full path) and `nf` (file name)"
    )
  }

  nc <- ncdf4::nc_open(foot_path)

  if (verbose) {
    cf <- ncdf4::ncatt_get(nc, 0, "Conventions")$value
    if (any(grepl("CF", cf))) print(paste0("Footprint Conventions = ", cf))
  }

  if (any(name_foot %in% names(nc$var))) {
    if (verbose) {
      cat(paste0("Reading ", name_foot, "\n"))
    }
    foot <- ncdf4::ncvar_get(nc, name_foot)
  } else {
    stop(paste0("Variable ", name_foot, " not found among: ", names(nc$var)))
  }

  foot1lat <- ncdf4::ncvar_get(nc, flat)
  foot1lon <- ncdf4::ncvar_get(nc, flon)

  ncdf4::nc_close(nc)

  # footprint times

  df_times_foot <- data.table::data.table(
    time_start = rep(time_foot, dim(foot)[3]) # 240
  )

  seq_time_start <- time_start <- nf <- hr <- NULL
  df_times_foot[,
    seq_time_start := seq.POSIXt(
      time_start[1],
      length.out = dim(foot)[3],
      by = "-1 hour",
      tz = "UTC"
    )
  ]
  # first time is the footprint time, ;last time, 240 hours in the past.
  # df_times_foot has time_start seq_time_start
  # seq_time_start has the time to match emissions

  if (verbose) {
    cat(
      "footprints, from ",
      strftime(
        df_times_foot$seq_time_start[1],
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      " to ",
      strftime(
        df_times_foot$seq_time_start[dim(foot)[3]],
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      "\n"
    )
  }

  dim_names <- list(
    paste0("lon_", sprintf("%02d", 1:dim(foot)[1])),
    paste0("lat_", sprintf("%02d", 1:dim(foot)[2])),
    strftime(
      df_times_foot$seq_time_start,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ) # this  can vary, what if I have hourly fluxes?, or m
  )

  dimnames(foot) <- dim_names

  if (
    !flux %in% c('CTCO2', 'monthly', 'month', 'daily', 'day', 'yearly', 'year')
  ) {
    stop(
      "flux must be 'CTCO2', 'monthly', 'month', 'daily', 'day', 'yearly', 'year'"
    )
  }

  # flux information
  if (flux == "CTCO2") {
    # CTCO2 has three hour average file order from 1 to 8
    df_times_foot[,
      hr := data.table::fifelse(
        data.table::hour(seq_time_start) %in% 0:(0 + 2),
        1,
        data.table::fifelse(
          data.table::hour(seq_time_start) %in% 3:(3 + 2),
          2,
          data.table::fifelse(
            data.table::hour(seq_time_start) %in% 6:(6 + 2),
            3,
            data.table::fifelse(
              data.table::hour(seq_time_start) %in% 9:(9 + 2),
              4,
              data.table::fifelse(
                data.table::hour(seq_time_start) %in% 12:(12 + 2),
                5,
                data.table::fifelse(
                  data.table::hour(seq_time_start) %in% 15:(15 + 2),
                  6,
                  data.table::fifelse(
                    data.table::hour(seq_time_start) %in% 18:(18 + 2),
                    7,
                    data.table::fifelse(
                      data.table::hour(seq_time_start) %in% 21:(21 + 2),
                      8,
                      0
                    )
                  )
                )
              )
            )
          )
        )
      )
    ]

    # convolving matching times bio

    conv_bio <- foot # to preserve same dimensions and iterate in last index
    conv_ocn <- foot # to preserve same dimensions and iterate in last index
    conv_fossil <- foot # to preserve same dimensions and iterate in last index
    conv_fire <- foot # to preserve same dimensions and iterate in last index

    for (j in 1:dim(foot)[3]) {
      dx_f <- df_flux[
        nf ==
          strftime(
            df_times_foot$seq_time_start,
            format = flux_format,
            tz = "UTC"
          )[j]
      ]

      # flux
      f <- dx_f$f

      if (verbose) {
        cat(paste0("Reading ", f, "\n"))
      }

      nc_f <- ncdf4::nc_open(f)

      if (verbose) {
        cat("vars: \n")
        cat(names(nc_f$var), "\n")
      }

      # bio
      flux_bio <- ncdf4::ncvar_get(
        nc_f,
        paste0("bio_flux_opt_", df_times_foot$hr[j])
      ) *
        factor

      if (flip) {
        dims <- dim(flux_bio)
        conv_bio[,, j] <- foot[,, j] * flux_bio[, dims[2]:1]
      } else {
        conv_bio[,, j] <- foot[,, j] * flux_bio
      }

      # ocn
      flux_ocn <- ncdf4::ncvar_get(
        nc_f,
        paste0("ocn_flux_opt_", df_times_foot$hr[j])
      ) *
        factor

      if (flip) {
        dims <- dim(flux_ocn)
        conv_ocn[,, j] <- foot[,, j] * flux_ocn[, dims[2]:1]
      } else {
        conv_ocn[,, j] <- foot[,, j] * flux_ocn
      }

      # fossil
      flux_fossil <- ncdf4::ncvar_get(
        nc_f,
        paste0("fossil_flux_imp_", df_times_foot$hr[j])
      ) *
        factor

      if (flip) {
        dims <- dim(flux_fossil)
        conv_fossil[,, j] <- foot[,, j] * flux_fossil[, dims[2]:1]
      } else {
        conv_fossil[,, j] <- foot[,, j] * flux_fossil
      }

      # fire
      flux_fire <- ncdf4::ncvar_get(
        nc_f,
        paste0("fire_flux_imp_", df_times_foot$hr[j])
      ) *
        factor

      if (flip) {
        dims <- dim(flux_fire)
        conv_fire[,, j] <- foot[,, j] * flux_fire[, dims[2]:1]
      } else {
        conv_fire[,, j] <- foot[,, j] * flux_fire
      }

      ncdf4::nc_close(nc_f)

      # print(paste0(round(j/240*100, 2), " %"))
    }

    conv_total <- conv_bio +
      conv_ocn +
      conv_fossil +
      conv_fire

    if (!is.null(fn)) {
      conv_total <- apply(conv_total, c(1, 2), fn)
      conv_bio <- apply(conv_bio, c(1, 2), fn)
      conv_ocn <- apply(conv_ocn, c(1, 2), fn)
      conv_fossil <- apply(conv_fossil, c(1, 2), fn)
      conv_fire <- apply(conv_fire, c(1, 2), fn)
      foot <- apply(foot, c(1, 2), fn)
    }

    if (!as_list) {
      simplify2array(list(
        foot = foot,
        conv_total = conv_total,
        conv_bio = conv_bio,
        conv_ocn = conv_ocn,
        conv_fossil = conv_fossil,
        conv_fire = conv_fire
      )) -> out
      return(out)
    } else {
      return(list(
        foot = foot,
        conv_total = conv_total,
        conv_bio = conv_bio,
        conv_ocn = conv_ocn,
        conv_fossil = conv_fossil,
        conv_fire = conv_fire,
        lat = foot1lat,
        lon = foot1lon,
        time = if (!missing(fn)) time_foot else df_times_foot$seq_time_start
      ))
    }
  }
  yt <- NULL

  #name in flux nc is "%Y-%m-01"
  if (flux %in% c("monthly", "month")) {
    #

    df_times_foot[,
      yt := strftime(seq_time_start, format = "%Y-%m-01", tz = "UTC")
    ]
    #name in flux nc is "%Y-%m-%d"
  } else if (flux %in% c("daily", "day")) {
    df_times_foot[,
      yt := strftime(seq_time_start, format = "%Y-%m-%d", tz = "UTC")
    ]
  } else if (flux %in% c("yearly", "year")) {
    df_times_foot[,
      yt := strftime(seq_time_start, format = "%Y-01-01", tz = "UTC")
    ]
  }
  conv <- foot # to preserve same dimensions and iterate in last index

  for (j in 1:dim(foot)[3]) {
    if (verbose) {
      cat(
        "df_flux$nf is ",
        df_flux$nf[1],
        "\n",
        "But expected  is: ",
        strftime(
          df_times_foot$seq_time_start,
          format = flux_format,
          tz = "UTC"
        )[j],
        "\n"
      )
    }

    dx_f <- df_flux[
      nf ==
        strftime(
          df_times_foot$seq_time_start,
          format = flux_format,
          tz = "UTC"
        )[j]
    ]

    # flux
    f <- dx_f$f

    if (verbose) {
      cat(paste0("Reading ", f, "\n"))
    }

    nc_f <- ncdf4::nc_open(f)

    if (verbose) {
      cat("vars: \n")
      cat(names(nc_f$var), "\n")
    }

    if (missing(name_var_flux)) {
      flux <- ncdf4::ncvar_get(nc_f, df_times_foot$yt[j]) * factor
    } else if (is.character(name_var_flux)) {
      flux <- ncdf4::ncvar_get(nc_f, name_var_flux) * factor # here I'm assuming one date per NetCDF
    } else if (is.numeric(name_var_flux)) {
      flux <- ncdf4::ncvar_get(nc_f, names(nc_f$var)[name_var_flux]) * factor
    } else {
      stop(
        "name_var_flux can be missing assumed var name date, integer or character"
      )
    }

    if (flip) {
      dims <- dim(flux)
      conv[,, j] <- foot[,, j] * flux[, dims[2]:1]
    } else {
      conv[,, j] <- foot[,, j] * flux
    }

    ncdf4::nc_close(nc_f)

    # print(paste0(round(j/240*100, 2), " %"))
  }

  if (!is.null(fn)) {
    conv <- apply(conv, c(1, 2), fn, na.rm = T)
    foot <- apply(foot, c(1, 2), fn, na.rm = T)
  }

  if (!as_list) {
    simplify2array(list(
      foot = foot,
      conv = conv
    )) -> out
    return(out)
  } else {
    return(list(
      foot = foot,
      conv = conv,
      lat = foot1lat,
      lon = foot1lon,
      time = if (!missing(fn)) time_foot else df_times_foot$seq_time_start
    ))
  }
}


#' obs_footprints
#'
#' This function returns a arrays (or list of arrays) of convolved footpritns with flux
#'
#' @param foot_path path for footprint (length 1).
#' @param name_foot name of the footprint variable in the NetCDF file.
#' @param flon name of lons
#' @param flat name of lats
#' @param time_foot Time of the footprints (in the name file) or third dimension
#' @param fn string with function to aggregate convolved fluxes, e.g. `mean`, `sum`, `max`, etc.
#' @param as_list Logical, to return list of arrays
#' @param verbose Logical, to display more information
#' @return An array of footprints, or a list of footprints, latitudes, longitudes and time.
#' @export
#' @import ncdf4 data.table
#' @examples \dontrun{
#' # do not run
#' }
obs_foot <- function(
  foot_path = "AAA",
  name_foot = "foot1",
  flon = "foot1lon",
  flat = "foot1lat",
  time_foot,
  fn = NULL,
  as_list = FALSE,
  verbose = TRUE
) {
  # footprint information

  if (length(foot_path) != 1) {
    stop("foot_path must be a single string")
  }

  if (verbose) {
    cat(paste0("Reading ", foot_path, "\n"))
  }

  ncf <- ncdf4::nc_open(foot_path)

  if (verbose) {
    cf <- ncdf4::ncatt_get(ncf, 0, "Conventions")$value
    if (any(grepl("CF", cf))) print(paste0("Footprint Conventions = ", cf))
  }

  if (any(name_foot %in% names(ncf$var))) {
    if (verbose) {
      cat(paste0("Reading ", name_foot, "\n"))
    }
    foot <- ncdf4::ncvar_get(ncf, name_foot)
  } else {
    stop(paste0("Variable ", name_foot, " not found among: ", names(ncf$var)))
  }

  foot1lat <- ncdf4::ncvar_get(ncf, flat)
  foot1lon <- ncdf4::ncvar_get(ncf, flon)

  ncdf4::nc_close(ncf)

  # footprint times

  df_times_foot <- data.table::data.table(
    time_start = rep(time_foot, dim(foot)[3]) # 240
  )

  seq_time_start <- time_start <- nf <- hr <- NULL
  df_times_foot[,
    seq_time_start := seq.POSIXt(
      time_start[1],
      length.out = dim(foot)[3],
      by = "-1 hour",
      tz = "UTC"
    )
  ]
  # first time is the footprint time, ;last time, 240 hours in the past.
  # df_times_foot has time_start seq_time_start
  # seq_time_start has the time to match emissions

  if (verbose) {
    cat(
      "footprints, from ",
      strftime(
        df_times_foot$seq_time_start[1],
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      " to ",
      strftime(
        df_times_foot$seq_time_start[dim(foot)[3]],
        format = "%Y-%m-%d %H:%M:%S",
        tz = "UTC"
      ),
      "\n"
    )
  }

  dim_names <- list(
    paste0("lon_", sprintf("%02d", 1:dim(foot)[1])),
    paste0("lat_", sprintf("%02d", 1:dim(foot)[2])),
    strftime(
      df_times_foot$seq_time_start,
      format = "%Y-%m-%d %H:%M:%S",
      tz = "UTC"
    ) # this  can vary, what if I have hourly fluxes?, or m
  )

  dimnames(foot) <- dim_names

  if (!is.null(fn)) {
    foot <- apply(foot, c(1, 2), fn, na.rm = T)
  }

  if (!as_list) {
    simplify2array(list(
      foot = foot
    )) -> out
    return(out)
  } else {
    return(list(
      foot = foot,
      lat = foot1lat,
      lon = foot1lon,
      time = if (!missing(fn)) time_foot else df_times_foot$seq_time_start
    ))
  }
}
