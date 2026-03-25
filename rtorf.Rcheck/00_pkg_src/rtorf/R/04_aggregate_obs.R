#' Aggregates Observations by time
#'
#' This function add aggregate cols by `key_time`
#'
#' @param dt  data.table
#' @param cols Character which defines columns to be aggregated
#' @param by String with columns to be aggregated
#' @param fn Function to be applied to the columns, default mean
#' @param verbose logical to show more information
#' @return A data.frame with with an index.
#' @importFrom data.table  second minute hour month year setDT setorderv mday .SD
#' @importFrom lubridate decimal_date
#' @note By default add column timeUTC based on the input column `key_time`
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' }
obs_agg <- function(
  dt,
  cols = c(
    "year",
    "month",
    "day",
    "hour",
    "minute",
    "second",
    "time",
    "time_decimal",
    "value",
    "latitude",
    "longitude",
    "altitude",
    "pressure",
    "u",
    "v",
    "temperature",
    "type_altitude"
  ),
  by = c(
    "key_time",
    "site_code",
    "altitude_final", # useful for towers. Name may change
    "type_altitude",
    "lab_1_abbr",
    "dataset_calibration_scale"
  ),
  fn = "mean",
  verbose = TRUE
) {
  if (!inherits(dt, "data.table")) {
    if (verbose) {
      cat("Transforming your data into data.table\n")
    }
    data.table::setDT(dt)
  }

  if (!any(names(dt) %in% "key_time")) {
    stop("missing `key_time`")
  }

  # Remember aircraft
  # Only aircraft need to be aggregated every 20 seconds
  # For hysplit, the ending times are not needed

  # Here we need daily information
  # as this function assumes that the input is already preselected data
  # for the evening time, I need to aggregate by
  # year, month day
  # here I have two options, by solar time (default)
  # or by local time
  # solar time names ending in _st
  # while local time, the POSIXct time vector `local_time`

  .SD <- NULL

  dt <- dt[,
    lapply(.SD, FUN = eval(parse(text = fn)), na.rm = T),
    .SDcols = cols,
    by = by
  ]

  if (verbose) {
    cat("Adding time\n")
  }

  dt$timeUTC <- as.POSIXct(dt$key_time, tz = "UTC")

  dt$year <- data.table::year(dt$timeUTC)
  dt$month <- data.table::month(dt$timeUTC)
  dt$day <- strftime(dt$timeUTC, "%d", tz = "UTC")
  dt$hour <- data.table::hour(dt$timeUTC)
  dt$minute <- data.table::minute(dt$timeUTC)
  dt$second <- data.table::second(dt$timeUTC)

  dt$time <- as.numeric(dt$timeUTC)
  dt$time_decimal <- lubridate::decimal_date(dt$timeUTC)

  #     if(any(cols %in% "dif_time")) {
  #       cat("Detecting dif_time. Adding ending times\n")
  #       dt$timeUTC_end <- dt$timeUTC + dt$dif_time
  #       dt$year_end <- data.table::year(dt$timeUTC_end)
  #       dt$month_end <- data.table::month(dt$timeUTC_end)
  #       dt$day_end <- data.table::mday(dt$timeUTC_end)
  #       dt$hour_end <- data.table::hour(dt$timeUTC_end)
  #       dt$minute_end <- data.table::minute(dt$timeUTC_end)
  #       dt$second_end <- data.table::second(dt$timeUTC_end)
  #
  #     }

  data.table::setorderv(x = dt, cols = c("timeUTC"), order = 1)

  # dt$timeUTC <- as.character(dt$timeUTC)

  return(dt)
}

#' Select Observations by closest time (seconds)
#'
#' This function select by closest time.
#' Good for aircrafts
#'
#' @param dt  data.table
#' @param origin a date-time object, default "1970-01-01"
#' @param tz time zone string
#' @param seconds seocnds target to select and filter data
#' @param verbose logical to show more information
#' @return A filtered data.frame.
#' @importFrom data.table  setDT ":="
#' @importFrom lubridate decimal_date
#' @note By default add column timeUTC based on the input column `key_time`
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' dx <- obs_read(index, expr = "altitude_final == '5800'")
#' dx <- obs_addtime(dx[, -"site_code"])
#' dy <- obs_select_sec(dx)
#' }
obs_select_sec <- function(
  dt,
  origin = "1970-01-01",
  tz = "UTC",
  seconds = 30,
  verbose = TRUE
) {
  if (!inherits(dt, "data.table")) {
    if (verbose) {
      cat("Transforming your data into data.table\n")
    }
    data.table::setDT(dt)
  }

  if (!any(names(dt) %in% "timeUTC")) {
    stop("missing `timeUTC`")
  }
  if (any(names(dt) %in% "site_code")) {
    stop("This function needs ot be run by group individually")
  }

  # 1. Calculate the 'target_time' (the nearest preceding 30-second mark)
  timeUTC <- target_time <- diff_to_target <- diff_to_next_target <- window_id <- sumx <- NULL
  dt[, target_time := floor(as.numeric(timeUTC) / seconds) * seconds]

  dt[,
    target_time := as.POSIXct(target_time, origin = origin, tz = tz)
  ]

  # 2. Calculate the difference from the 'target_time' and the *next* 30-second mark
  #    We want the difference to the closest target:
  dt[, diff_to_target := abs(as.numeric(timeUTC) - as.numeric(target_time))]

  dt[,
    diff_to_next_target := abs(
      as.numeric(timeUTC) - (as.numeric(target_time) + seconds)
    )
  ]

  # 3. Determine the 'target_window' and the time difference to its closest target
  #    The window is defined by the *closest* 30-second multiple.
  dt[, window_id := target_time] # Initial window based on preceding 30s mark

  # If the observation is closer to the *next* 30s mark, shift the window_id
  dt[diff_to_next_target < diff_to_target, window_id := target_time + seconds]

  # Now, we group by 'window_id' and select the row with the minimum difference to that target
  mindif <- NULL
  dt[, mindif := diff_to_target == min(diff_to_target), by = window_id]
  df <- dt[mindif == TRUE]

  return(df)
}
