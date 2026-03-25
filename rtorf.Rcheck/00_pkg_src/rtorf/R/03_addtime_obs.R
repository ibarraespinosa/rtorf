#' @title Add times
#'
#' @description
#' This function add timeUTC as POSIX class,
#' local time and ending sampling time
#'
#' @family time
#' @param dt data.table
#' @param verbose Logical, to show more info
#' @param tz Timezone, default "UTC"
#' @param timeonly return only timeUTC column
#' @return A data.frame with with an index obspack.
#' @importFrom data.table year month hour minute second setDT fifelse uniqueN
#' @note timeUTC is calculated based on the field column start_time,
#' the timeUTC_end is calculated using this approach:
#' 1. If the column time_interval is not found, proceed
#' with the calculation using midpoint_time
#' 2. Else, use column time_interval
#'
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <-  obs_summary(obs)
#' dt <- obs_read(index)
#' dt <- obs_addtime(dt)
#' }
obs_addtime <- function(dt, verbose = TRUE, tz = "UTC", timeonly = FALSE) {
  if (!inherits(dt, "data.table")) {
    data.table::setDT(dt)
  }

  if (verbose) {
    cat("Adding timeUTC\n")
  }
  # Time is already UTC
  dt$timeUTC <- as.POSIXct(dt$time, origin = "1970-01-01", tz = "UTC")

  if (verbose) {
    cat("Adding timeUTC_start\n")
  }
  # Time is already UTC
  dt$timeUTC_start <- as.POSIXct(dt$start_time, origin = "1970-01-01", tz = tz)

  if (verbose) {
    cat("Adding timeUTC_end\n")
  }
  # adding timeUTC_end
  midpoint_time <- NULL
  start_time <- NULL
  timeUTC <- NULL
  time_interval <- NULL
  timeUTC_start <- NULL
  if (any("time_interval" == names(dt))) {
    cat("Found time_interval\n")
    dt[,
      timeUTC_end := data.table::fifelse(
        is.na(time_interval),
        timeUTC_start + (midpoint_time - start_time) * 2,
        timeUTC_start + time_interval
      )
    ]
  } else {
    #if there is no column named time_interval
    time_inter <- (dt$midpoint_time - dt$start_time) * 2
    dt$timeUTC_end <- dt$timeUTC_start + time_inter
  }

  dt$dif_time <- dt$timeUTC_end - dt$timeUTC_start
  time_warning <- NULL
  timeUTC_end <- NULL
  dt[,
    time_warning := ifelse(
      timeUTC == timeUTC_end,
      "warning, start/end time equals",
      "all good"
    )
  ]

  dt$year_end <- data.table::year(dt$timeUTC_end)
  dt$month_end <- data.table::month(dt$timeUTC_end)
  dt$day_end <- as.numeric(strftime(dt$timeUTC_end, "%d"))
  dt$hour_end <- data.table::hour(dt$timeUTC_end)
  dt$minute_end <- data.table::minute(dt$timeUTC_end)
  dt$second_end <- data.table::second(dt$timeUTC_end)

  # new_second <- NULL
  #   dt[, new_second := cut(timeUTC, breaks = freq)]
  #
  # # dt$new_second <- (( second(dt$timeUTC) + 5/2) %/% 10)*10
  # dt$new_second <- ifelse(dt$new_second == 60,
  # 0,
  # dt$new_second)
  if (timeonly) return(dt$timeUTC) else return(dt)
}


#' @title add local time
#' @family time
#' @name obs_addltime
#' @description Calculate an approximation of local hour
#' @param dt data.table
#' @param timeUTC Character indicating the Time column as POSIXct
#' @param utc2lt Character indicating the integer column to convert to local time
#' if available
#' @param longitude Character indicating the column with the lingitude
#' @param tz Timezone, default "UTC"
#' @param timeonly return only local_time column
#' @importFrom data.table hour
#' @return data.table with local time columns
#' @note time depending n longitude is by John Miller (GML/NOAA)
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
obs_addltime <- function(
  dt,
  timeUTC = "timeUTC",
  utc2lt = "site_utc2lst",
  longitude = "longitude",
  tz = "UTC",
  timeonly = FALSE
) {
  # Priority site_utc2lst and if it is not available,
  if (any(utc2lt == names(dt))) {
    cat(paste("Found ", utc2lt, "\n"))
    dt$local_time <- dt[[timeUTC]] + as.numeric(dt[[utc2lt]]) * 60 * 60
    dt$local_time <- ifelse(
      is.na(dt$local_time),
      dt[[timeUTC]] + dt[[longitude]] / 15 * 60 * 60, #john miller approach
      dt$local_time
    )
  } else {
    dt$local_time <- dt[[timeUTC]] + dt[[longitude]] / 15 * 60 * 60 #john miller approach
  }

  if (inherits(dt$local_time, "numeric")) {
    dt$local_time <- as.POSIXct(dt$local_time, origin = "1970-01-01", tz = tz)
  }

  dt$lh <- data.table::hour(dt$local_time)

  if (timeonly) return(dt$local_time) else return(dt)
}

#' @title Add solar time
#'
#' @description
#' This function add timeUTC as POSIX class,
#' local time and ending sampling time
#'
#' @family time
#' @param dt obspack data.table
#' @param tz Timezone, default "UTC"
#' @return return the same data.frame adding solar time
#' @importFrom data.table setDT
#'
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <-  obs_summary(obs)
#' dt <- obs_read(index)
#' dt <- obs_addtime(dt)
#' }
obs_addstime <- function(dt, tz = "UTC") {
  if (!inherits(dt, "data.table")) {
    data.table::setDT(dt)
  }

  # Time is already UTC
  timeUTC_st <- ISOdatetime(
    dt$year_st,
    dt$month_st,
    dt$day_st,
    dt$hour_st,
    dt$minute_st,
    dt$second_st,
    tz = tz
  )

  return(timeUTC_st)
}


#' @title Add matlab time
#'
#' @description
#' This function add timeUTC as POSIX class,
#' local time and ending sampling time
#'
#' @family time
#' @param time numeric time from MATLAB
#' @param origin default "1970-01-01"
#' @param tz Timezone, default "UTC"
#' @return return the same data.frame adding solar time
#' @importFrom data.table setDT
#'
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <-  obs_summary(obs)
#' dt <- obs_read(index)
#' dt <- obs_addtime(dt)
#' }
obs_addmtime <- function(time, origin = "1970-01-01", tz = "UTC") {
  # The constant 719529 is the number of days between 0000-01-01 and 1970-01-01

  timeUTC <- as.POSIXct((time - 719529) * 86400, origin = origin, tz = tz)

  return(timeUTC)
}
