#' Obspack Table Summary
#'
#' This function reads the obsPack directory
#' providing a summary for the columns: "value", "time",
#' "time_decimal", "latitude" and "longitude". These summary
#' are made by the columns "name", "sector", "site_name",
#' "site_country", "type_altitude", "lab_1_abbr" and
#' "site_utc2lst"
#'
#' @param df data.table
#' @param cols String of columns to be summarized.
#' @return A data.frame with with an index obspack.
#' @importFrom data.table fwrite ".N" ":=" rbindlist "%chin%"
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' dx <- obs_table(dt)
#' }
obs_table <- function(
  df,
  cols = c("value", "time", "time_decimal", "latitude", "longitude")
) {
  .SD <- . <- name <- site_name <- site_latitude <- NULL
  site_longitude <- site_country <- site_code <- NULL
  lab_1_abbr <- site_utc2lst <- NULL

  df[,
    lapply(.SD, summary, na.rm = TRUE),
    by = .(site_name, site_latitude, site_longitude, site_country, site_code),
    .SDcols = cols
  ] -> dxx

  dxx$stat <- rep(c("min", "q1", "median", "mean", "q3", "max"), nrow(dxx) / 6)

  dxx$time <- as.integer(dxx$time)

  dxx$timeUTC <- as.POSIXct(dxx$time, origin = "1970-01-01", tz = "UTC")

  return(dxx)
}
