#' Compares expected receptors
#'
#' This function creates a data.frame with the expected
#' footprints in .nc and compare with the actual footprints.
#' The idea is verify is there are missing footprints.
#'
#' @param path String with path hwere are stored the footprints
#' @param year numeric number
#' @param month numeric number
#' @param day numeric number
#' @param hour numeric number
#' @param minute numeric number
#' @param lat numeric number
#' @param lon numeric number
#' @param alt numeric number
#' @param out outfile path.
#' @param verbose logical to show more information
#' @return A data.frame with with expected footprints
#' @export
#' @examples \dontrun{
#' # do not run
#' p <- "/path/to/continuous/"
#' # here we have year/month/hysplit*.nc
#' x <- dt
#' dt <- obs_find_receptors(p, year, month....)
#' }
obs_find_receptors <- function(path,
                               year,
                               month,
                               day,
                               hour,
                               minute,
                               lat,
                               lon,
                               alt,
                               out = paste0(tempfile(), ".csvy"),
                               verbose = FALSE){


  dt <- data.table::data.table(
    expect = obs_footname(year = year,
                          month  = month,
                          day = day,
                          hour = hour,
                          minute = minute,
                          lat = lat,
                          lon = lon,
                          alt = alt
    )
  )


  ld <- list.files(path = path,
                   pattern = ".nc",
                   recursive = T,
                   full.names = T)

  ln <- list.files(path = path,
                   pattern = ".nc",
                   recursive = T,
                   full.names = F)

  if(verbose) print(ld)

  df <- data.table::data.table(file = ln,
                               expect = ln)

  dx <- merge(dt, df, by = "expect", all.x = T)

  # missing <- NULL
  dx[, missing := ifelse(is.na(file), T, F)]


  if(!missing(out)) {
    obs_write_csvy(dt = dx,
              notes = "expected files",
              out = out)
  }
  return(dx)

}
