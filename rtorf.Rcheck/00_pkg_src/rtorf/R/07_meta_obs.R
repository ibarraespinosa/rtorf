#' Read obspack metadata
#'
#' @param index data.table
#' @param verbose Logical to show more information
#' @param n_site_code number of characters extraced from metadata after search
#' @param n_site_name number of characters extraced from metadata after search
#' @param n_site_latitude number of characters extraced from metadata after search
#' @param n_site_longitude number of characters extraced from metadata after search
#' @param n_site_country number of characters extraced from metadata after search
#' @param n_dataset_project number of characters extraced from metadata after search
#' @param n_lab number of characters extraced from metadata after search
#' @param n_scales number of characters extraced from metadata after search
#' @param n_site_elevation number of characters extraced from metadata after search
#' @param n_altitude_comment number of characters extraced from metadata after search
#' @param n_utc number of characters extraced from metadata after search
#' @param fill_value fill value. Appeared in aoa_aircraft-flask_19_allvalid.txt
#' @param as_list Logical to return as list
#' @return A data.frame with with an index obspack.
#' @importFrom data.table fwrite ".N" ":=" rbindlist "%chin%"
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_meta(index)
#' }
obs_meta <- function(
  index,
  verbose = TRUE,
  n_site_code = 15,
  n_site_name = 15,
  n_site_latitude = 18,
  n_site_longitude = 19,
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
  df <- index
  if (nrow(df) == 0) {
    stop("empty index")
  }

  x1 <- df$id

  lapply(seq_along(x1), function(i) {
    agl <- df$agl[i]
    n <- df$n
    categories <- df$sector[i]

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

    # everything after char 15
    site_name <- substr(x = pattern, start = n_site_name, stop = nchar(pattern))

    # site_latitude ####
    pattern <- grep(pattern = "site_latitude", x = att, value = T)

    # everything after char 15
    (site_latitude <- substr(
      x = pattern,
      start = n_site_latitude,
      stop = nchar(pattern)
    ))

    # site_name ####
    pattern <- grep(pattern = "site_longitude", x = att, value = T)

    # everything after char 15
    (site_longitude <- substr(
      x = pattern,
      start = n_site_longitude,
      stop = nchar(pattern)
    ))

    # site_country ####
    pattern <- grep(pattern = "site_country", x = att, value = T)[1]

    # everything after char 18
    site_country <- substr(
      x = pattern,
      start = n_site_country,
      stop = nchar(pattern)
    )

    # dataset_project ####
    pattern <- grep(pattern = "dataset_project", x = att, value = T)[1]

    # everything after char 21
    dataset_project <- substr(
      x = pattern,
      start = n_dataset_project,
      stop = nchar(pattern)
    )

    # lab_1_abbr ####
    pattern <- grep(pattern = "lab_1_abbr", x = att, value = T)[1]

    # everything after char 16
    (lab_1_abbr <- substr(x = pattern, start = n_lab, stop = nchar(pattern)))

    # dataset_calibration_scale ####
    pattern <- grep(pattern = "dataset_calibration_scale", x = att, value = T)[
      1
    ]

    # everything after char 21
    (dataset_calibration_scale <- substr(
      x = pattern,
      start = n_scales,
      stop = nchar(pattern)
    ))

    # elevation ####
    pattern <- grep(pattern = " site_elevation", x = att, value = T)[1]

    # everything after char 16
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

    # site_utc2lst
    (pattern <- grep(pattern = "site_utc2lst", x = att, value = T)[1])

    # everything after char 16
    (site_utc2lst <- as.numeric(substr(
      x = pattern,
      start = n_utc,
      stop = nchar(pattern)
    )))

    # data.frame ####

    dt <- data.frame(name = df$name[i])

    dt$sector <- categories
    dt$site_code <- site_code
    dt$site_name <- site_name
    dt$site_latitude <- site_latitude
    dt$site_longitude <- site_longitude
    dt$site_elevation <- site_elevation

    dt$site_country <- site_country
    dt$dataset_project <- dataset_project
    dt$lab_1_abbr <- lab_1_abbr
    dt$dataset_calibration_scale <- dataset_calibration_scale
    dt$altitude_comment <- rep(altitude_comment, nrow(dt))
    dt$site_utc2lst <- site_utc2lst
    dt$agl <- agl
    dt$site_elevation <- ifelse(
      dt$site_elevation == fill_value,
      NA,
      dt$site_elevation
    )

    dt$id <- i

    dt
  }) -> lx

  if (as_list) {
    return(lx)
  } else {
    dt <- data.table::rbindlist(lx, use.names = T)
    return(dt)
  }
}
