#' @title Read obspack (.txt)
#'
#' @description
#' Each obspack file has a header with metadata and this
#' function reads selected fields from the metadata and add
#' them as columns. This new columns are used later to
#' be filtered
#'
#' @param index data.table
#' @param categories character; ONE category
#' : of c("aircraft-pfp", "aircraft-insitu",
#' "surface-insitu", "tower-insitu", "aircore", "surface-pfp", "shipboard-insitu",
#' "flask").
#' @param expr String expressions to filter data.table internally
#' @param verbose Logical to show more information
#' @param n_site_code number of characters extratced from metadata after search
#' @param n_site_latitude number of characters extracted from metadata after search
#' @param n_site_longitude number of characters extracted from metadata after search
#' @param n_site_name number of characters extracted from metadata after search
#' @param n_site_country number of characters extracted from metadata after search
#' @param n_dataset_project number of characters extracted from metadata after search
#' @param n_lab number of characters extracted from metadata after search
#' @param n_scales number of characters extracted from metadata after search
#' @param n_site_elevation number of characters extracted from metadata after search
#' @param n_altitude_comment number of characters extracted from metadata after search
#' @param n_utc number of characters extracted from metadata after search
#' @param fill_value fill value. Appeared in aoa_aircraft-flask_19_allvalid.txt
#' @param as_list Logical to return as list
#' @return A data.frame with with an index obspack.
#' @importFrom data.table fwrite ".N" ":=" rbindlist "%chin%"
#' @export
#' @note The identification of the altitude and type is critical.
#' The approach used here consists of:
#' 1. Identify agl from the name of the tile.
#' 2. If magl not present, search dill_values used in elevation and
#' transform them into NA (not available)
#' 3. If magl is not present, agl = altitude - elevation
#' 4. If there are some NA in elevation, will result some NA in agl
#' 5. A new column is added named `altitude_final` to store agl or asl
#' 6. Another column named `type_altitude` is added to identify "magl" or "masl"
#' 7. If there is any case NA in `altitude_final`,
#' `type_altitude` is "not available"
#'
#' Then, the relationship with hysplit is:
#' \tabular{ccc}{
#'   type_altitude \tab hysplit  \cr
#'   magl          \tab agl      \cr
#'   masl          \tab asl      \cr
#'   not available \tab f-PBL    \cr
#'}
#'
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' obs_read(index, expr = "altitude_final == '5800'")
#' }
obs_read <- function(
  index,
  categories = "flask",
  expr = NULL,
  verbose = TRUE,
  n_site_code = 15,
  n_site_latitude = 18,
  n_site_longitude = 19,
  n_site_name = 15,
  n_site_country = 18,
  n_dataset_project = 21,
  n_lab = 16,
  n_scales = 31,
  n_site_elevation = 20,
  n_altitude_comment = 22,
  n_utc = 18,
  fill_value = -1e+34,
  as_list = FALSE
) {
  if (nrow(index) == 0) {
    stop("empty index")
  }

  if (verbose) {
    cat(paste0("Searching ", categories, "...\n"))
  }
  sector <- NULL
  df <- index[sector %chin% categories]
  if (nrow(df) == 0) {
    stop(paste0("I did not find any ", categories))
  }

  x1 <- df$id

  lapply(seq_along(x1), function(i) {
    agl <- df$agl[i]
    n <- df$n

    if (verbose) {
      cat(paste0(i, ": ", df$name[i], "\n"))
    }

    nr <- data.table::fread(x1[i], nrows = 1)$V4

    att <- readLines(x1[i], n = nr)

    # site code ####
    pattern <- grep(pattern = "site_code", x = att, value = T)

    # everything after char 15
    site_code <- substr(x = pattern, start = n_site_code, stop = nchar(pattern))

    # site_name ####
    pattern <- grep(pattern = "site_name", x = att, value = T)

    site_name <- substr(x = pattern, start = n_site_name, stop = nchar(pattern))

    # site_latitude ####
    pattern <- grep(pattern = "site_latitude", x = att, value = T)[1]

    (site_latitude <- substr(
      x = pattern,
      start = n_site_latitude,
      stop = nchar(pattern)
    ))

    # site_longitude ####
    pattern <- grep(pattern = "site_longitude", x = att, value = T)[1]

    (site_longitude <- substr(
      x = pattern,
      start = n_site_longitude,
      stop = nchar(pattern)
    ))

    # site_country ####
    pattern <- grep(pattern = "site_country", x = att, value = T)[1]

    site_country <- substr(
      x = pattern,
      start = n_site_country,
      stop = nchar(pattern)
    )

    # dataset_project ####
    pattern <- grep(pattern = "dataset_project", x = att, value = T)[1]

    dataset_project <- substr(
      x = pattern,
      start = n_dataset_project,
      stop = nchar(pattern)
    )

    # lab_1_abbr ####
    pattern <- grep(pattern = "lab_1_abbr", x = att, value = T)[1]

    (lab_1_abbr <- substr(x = pattern, start = n_lab, stop = nchar(pattern)))

    # dataset_calibration_scale ####
    pattern <- grep(pattern = "dataset_calibration_scale", x = att, value = T)[
      1
    ]

    (dataset_calibration_scale <- substr(
      x = pattern,
      start = n_scales,
      stop = nchar(pattern)
    ))

    # elevation ####
    pattern <- grep(pattern = " site_elevation", x = att, value = T)[1]

    (site_elevation <- substr(
      x = pattern,
      start = n_site_elevation,
      stop = nchar(pattern)
    ))

    # altitude:comment ####
    pattern <- grep(pattern = " altitude:comment", x = att, value = T)

    # everything after char 16
    (altitude_comment <- substr(
      x = pattern,
      start = n_altitude_comment,
      stop = nchar(pattern)
    ))

    # # altitude:provider_comment ####
    # pattern <- grep(pattern = " altitude:provider_comment",
    #                 x = att,
    #                 value = T)
    #
    # # everything after char 16
    # (altitude_provider_comment <- substr(x = pattern,
    #                                      start = 31,
    #                                      stop = nchar(pattern)))

    # site_utc2lst
    (pattern <- grep(pattern = "site_utc2lst", x = att, value = T)[1])
    # print(pattern)

    # everything after char 16
    (site_utc2lst <- as.numeric(substr(
      x = pattern,
      start = n_utc,
      stop = nchar(pattern)
    )))

    names_df <- data.table::fread(x1[i], skip = nr - 1, nrows = 1, h = FALSE)

    err <- tryCatch(
      data.table::fread(x1[i], skip = nr - 1),
      error = function(e) e,
      warning = function(w) w
    )

    if (inherits(err, "warning")) {
      message("Found warnings")
      print(class(err))
      dt <- data.table::fread(x1[i], skip = nr - 1, header = FALSE)

      dif_names <- ncol(dt) - length(names_df)

      names(dt) <- c(unlist(names_df), paste0("newcol", 1:length(dif_names)))

      if (verbose) {
        message(paste0(
          "\nAdded ",
          length(dif_names),
          " columns on",
          x1[i],
          "\n"
        ))
      }
      print(warnings())
    } else {
      dt <- data.table::fread(x1[i], skip = nr - 1)
    }

    dt$name <- df$name[i]

    dt$sector <- categories
    # dt$id_by_site_code <- 1:nrow(dt)
    dt$site_code <- site_code
    dt$site_name <- site_name
    dt$site_country <- site_country
    dt$site_elevation <- site_elevation
    dt$site_latitude <- site_latitude
    dt$site_longitude <- site_longitude
    dt$dataset_project <- dataset_project
    dt$lab_1_abbr <- lab_1_abbr
    dt$dataset_calibration_scale <- dataset_calibration_scale
    dt$altitude_comment <- rep(altitude_comment, nrow(dt))
    dt$site_utc2lst <- site_utc2lst
    dt$agl <- agl
    df$n <- n
    #if agl is NA, agl = altitude - elevation
    #if elevation is fill_value, NA
    #if altitude - elevation = NA, use altitude => asl

    dt$site_elevation <- ifelse(
      dt$site_elevation == fill_value,
      NA,
      dt$site_elevation
    )

    agl <- NULL
    site_elevation <- NULL
    altitude <- NULL
    dt[is.na(agl), agl := altitude - as.numeric(site_elevation)]

    dt$type_altitude <- ifelse(
      is.na(dt$agl),
      1, # masl
      0
    ) # magl

    dt$altitude_final <- ifelse(is.na(dt$agl), dt$altitude, dt$agl)

    dt$type_altitude <- ifelse(
      is.na(dt$altitude_final),
      "not available",
      dt$type_altitude
    )
    dt$id <- i

    if (!is.null(expr)) {
      dt <- dt[eval(parse(text = expr))]
    }
    dt
  }) -> lx

  unames <- unique(unlist(sapply(lx, names)))

  #add names
  for (i in 1:length(lx)) {
    ly <- obs_out(names(lx[[i]]), unames)
    if (length(ly) > 0) {
      for (j in seq_along(ly)) {
        lx[[i]][[ly[j]]] <- NA
      }
    }
  }

  type_altitude <- NULL
  .N <- NULL
  if (as_list) {
    # for(i in seq_along(lx)) {
    #   print(lx[[i]][, .N, by = type_altitude])
    # }
    return(lx)
  } else {
    dt <- data.table::rbindlist(lx, use.names = T)
    # if(verbose) cat("Adding id\n")
    # dt$id <- 1:nrow(dt)
    # print(dt[, .N, by = type_altitude])
    return(dt)
  }
}

#' @title Read obspack (.nc)
#'
#' @description
#' Each obspack file has a header with metadata and this
#' function reads selected fields from the metadata and add
#' them as columns. This new columns are used later to
#' be filtered
#'
#' @param index data.table
#' @param categories character; ONE category
#' : of c("aircraft-pfp", "aircraft-insitu",
#' "surface-insitu", "tower-insitu", "aircore", "surface-pfp", "shipboard-insitu",
#' "flask").
#' @param att Logical, if global attributes should be added to data.table
#' @param expr String expressions to filter data.table internally
#' @param solar_time Logical, add solar time? Default:
#' if categories include aircraft, FALSE, otherwise, TRUE
#' @param as_list Logical to return as list
#' @param verbose Logical to show more information
#' @param warnings Logical to show warnings when reading NetCDF, especially
#' global attributes
#' @return A data.frame with with an index obspack.
#' @importFrom data.table fwrite ".N" ":=" rbindlist "%chin%"
#' @importFrom utils capture.output
#' @export
#'
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' }
obs_read_nc <- function(
  index,
  categories = "flask",
  att = FALSE,
  expr = NULL,
  solar_time = if (grepl("aircraft", categories)) FALSE else TRUE,
  as_list = FALSE,
  verbose = FALSE,
  warnings = FALSE
) {
  if (nrow(index) == 0) {
    stop("empty index")
  }

  if (verbose) {
    cat(paste0("Searching ", categories, "...\n"))
  }
  sector <- NULL
  df <- index[sector %chin% categories]
  if (nrow(df) == 0) {
    stop(paste0("I did not find any ", categories))
  }

  x1 <- df$id

  lapply(seq_along(x1), function(j) {
    agl <- df$agl[j]
    n <- df$n

    if (verbose) {
      cat(paste0(j, ": ", df$name[j], "\n"))
    }

    nc <- ncdf4::nc_open(x1[j])
    names(nc$var)

    na <- names(nc$var)
    na <- data.frame(vars = names(nc$var), stringsAsFactors = FALSE)

    la <- lapply(1:nrow(na), function(i) {
      suppressWarnings(
        unlist(ncdf4::ncatt_get(nc = nc, varid = na$vars[i]))
      )
    })
    names(la) <- na$vars

    lv <- lapply(1:nrow(na), function(i) {
      suppressWarnings(
        x <- ncdf4::ncvar_get(nc = nc, varid = na$vars[i])
      )
    })
    names(lv) <- na$vars

    d <- rbindlist(lapply(seq_along(lv), function(i) {
      data.table(length(dim(lv[[i]])))
    }))
    d$names <- names(lv)
    names(d)[1] <- "dim"

    x2 <- ncdf4::ncvar_get(nc = nc, varid = "time_components")

    dt <- as.data.table(t(x2))
    names(dt) <- c("year", "month", "day", "hour", "minute", "second")

    if (solar_time) {
      st <- ncdf4::ncvar_get(nc = nc, varid = "solartime_components")

      dtst <- as.data.table(t(st))
      names(dtst) <- paste0(names(dt), "_st")
    }

    xx <- d[dim == 1]

    for (i in 1:nrow(xx)) {
      # add if to add variables with same length as ntow(dt)
      if (length(lv[[xx$names[i]]]) == nrow(dt)) {
        dt[[xx$names[i]]] <- lv[[xx$names[i]]]
      }
    }

    if (solar_time) {
      for (i in 1:ncol(dtst)) {
        dt[[names(dtst)[i]]] <- dtst[[names(dtst)[i]]]
      }
    }

    dt$scale <- la$value[["scale_comment"]]

    log <- utils::capture.output(
      global <- ncdf4::ncatt_get(nc = nc, varid = 0, verbose = F)
    )
    if (warnings) {
      print(log)
    }

    #here we manually add intake and site unit
    suppressWarnings(
      dt$dataset_intake_ht_unit <- global[["dataset_intake_ht_unit"]]
    )
    suppressWarnings(dt$site_elevation_unit <- global[["site_elevation_unit"]])
    suppressWarnings(dt$dataset_project <- global[["dataset_project"]])
    suppressWarnings(
      dt$dataset_selection_tag <- global[["dataset_selection_tag"]]
    )

    # obs_table
    suppressWarnings(dt$site_name <- global[["site_name"]])
    suppressWarnings(dt$site_elevation <- global[["site_elevation"]])
    suppressWarnings(dt$site_latitude <- global[["site_latitude"]])
    suppressWarnings(dt$site_longitude <- global[["site_longitude"]])
    suppressWarnings(dt$site_country <- global[["site_country"]])
    suppressWarnings(dt$site_code <- global[["site_code"]])
    suppressWarnings(dt$site_utc2lst <- global[["site_utc2lst"]])

    # obs_agg
    suppressWarnings(dt$lab_1_abbr <- global[["lab_1_abbr"]])
    suppressWarnings(
      dt$dataset_calibration_scale <- global[["dataset_calibration_scale"]]
    )

    if (att) {
      x <- do.call("cbind", global)
      x <- as.data.frame(x)
      for (i in 1:ncol(x)) {
        dt[[names(x)[i]]] <- global[[names(x)[i]]]
      }

      # for(i in seq_along(global)) {
      #   # print(length(global[[i]]))
      #   print(global[[names(global)[i]]])
      # }
    }

    # dt$obspack_citation <- NULL
    ncdf4::nc_close(nc)

    # if exist, cool, if not, NA
    #aircore or aircraft
    if (any(grepl("airc", unique(dt$dataset_project)))) {
      dt$altitude_final <- dt$altitude
      dt$type_altitude <- 1
    } else {
      dt$altitude_final <- dt$intake_height
      dt$type_altitude <- ifelse(dt$dataset_intake_ht_unit == "magl", 0, 1)
    }

    # if(!is.null(cols)) {
    # dt <- dt[, cols, with = FALSE]
    # } else {
    if (!is.null(expr)) {
      dt <- dt[eval(parse(text = expr))]
    }
    dt
    # }
  }) -> lx

  unames <- unique(unlist(sapply(lx, names)))

  #add names
  for (i in 1:length(lx)) {
    ly <- obs_out(names(lx[[i]]), unames)
    if (length(ly) > 0) {
      for (j in seq_along(ly)) {
        lx[[i]][[ly[j]]] <- NA
      }
    }
  }

  type_altitude <- NULL
  .N <- NULL
  if (as_list) {
    # for(i in seq_along(lx)) {
    #   print(lx[[i]][, .N, by = type_altitude])
    # }
    return(lx)
  } else {
    dt <- data.table::rbindlist(lx, use.names = T)
    # if(verbose) cat("Adding id\n")
    # dt$id <- 1:nrow(dt)
    # print(dt[, .N, by = type_altitude])
    return(dt)
  }
}


#' @title Read obspack attributes (.nc)
#'
#' @description
#' Each obspack file has a header with metadata and this
#' function reads selected fields from the metadata and add
#' them as columns. This new columns are used later to
#' be filtered
#'
#' @param index data.table
#' @param as_list Logical to return as list
#' @param verbose Logical to show more information
#' @param warnings Logical to show warnings when reading NetCDF, especially
#' global attributes
#' @return A data.frame with with an index obspack.
#' @importFrom data.table fwrite ".N" ":=" rbindlist "%chin%"
#' @importFrom utils capture.output
#' @export
#'
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' }
obs_read_nc_att <- function(
  index,
  as_list = FALSE,
  verbose = FALSE,
  warnings = FALSE
) {
  if (nrow(index) == 0) {
    stop("empty index")
  }

  df <- index

  if (nrow(df) == 0) {
    stop("Empty index")
  }

  x1 <- df$id

  lapply(seq_along(x1), function(j) {
    n <- df$n

    if (verbose) {
      cat(paste0(j, ": ", df$name[j], "\n"))
    }

    nc <- ncdf4::nc_open(x1[j])
    names(nc$var)

    log <- utils::capture.output(
      global <- ncdf4::ncatt_get(nc = nc, varid = 0, verbose = F)
    )
    if (warnings) {
      print(log)
    }
    global <- as.data.frame(global)

    # dt$obspack_citation <- NULL
    ncdf4::nc_close(nc)

    global
    # }
  }) -> lx

  unames <- unique(unlist(sapply(lx, names)))

  #add names
  for (i in 1:length(lx)) {
    ly <- obs_out(names(lx[[i]]), unames)
    if (length(ly) > 0) {
      for (j in seq_along(ly)) {
        lx[[i]][[ly[j]]] <- NA
      }
    }
  }

  type_altitude <- NULL
  .N <- NULL
  if (as_list) {
    # for(i in seq_along(lx)) {
    #   print(lx[[i]][, .N, by = type_altitude])
    # }
    return(lx)
  } else {
    dt <- data.table::rbindlist(lx, use.names = T)
    # if(verbose) cat("Adding id\n")
    # dt$id <- 1:nrow(dt)
    # print(dt[, .N, by = type_altitude])
    return(dt)
  }
}
