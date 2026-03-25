#' Helpers
#'
#'
#' @title outersect
#' @family helpers
#' @name obs_out
#' @description Just the opposite of intersect
#' @param x vector
#' @param y vector
#' @return vector opposite of intersect
#' @export
#' @examples \dontrun{
#' #do not run
#' }
obs_out <- function(x, y) {
  sort(c(setdiff(x, y), setdiff(y, x)))
}


#' @title list.dt
#' @family helpers
#' @name obs_list.dt
#' @description Some treatments for list of data.frames
#' @param ldf list of data.frames
#' @param na common names in the final data.frame
#' @param verbose Logical to show more information
#' @return long data.table
#' @note 1. Filter out empty data.frames
#' 2. identify common names
#' 3. rbindlist and return data.table
#' @importFrom data.table rbindlist
#' @importFrom utils object.size
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
obs_list.dt <- function(ldf, na, verbose = TRUE) {
  ldf <- Filter(function(x) dim(x)[1] > 0, ldf)

  lm <- lapply(ldf, names)

  na <- Reduce(intersect, lapply(ldf, names))

  lxx <- function(x, names) {
    lapply(seq_along(x), function(i) {
      x[[i]][, names, with = FALSE]
    }) -> lx
    data.table::rbindlist(lx)
  }
  dt <- lxx(ldf, na)
  if (verbose) {
    print(format(utils::object.size(dt), units = "Mb"))
  }
  return(dt)
}


#' @title return numeric vector in intervals
#' @family helpers
#' @description return numeric vector in intervals
#' @param x numeric, longer than `freq`
#' @param freq numeric vector
#' @param ... findInterval arguments
#' @return numeric vector
#' @name obs_freq
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
obs_freq <- function(x, freq, ...) {
  if (freq[1] > (min(x, na.rm = TRUE))) {
    stop("first freq must be equal or lower than min(x)")
  }
  # add and feq
  return(freq[findInterval(
    x = x,
    vec = freq,
    rightmost.closed = T,
    left.open = T,
    ...
  )])
}

#' @title round seconds from "POSIXct" "POSIXt"  classes
#' @family helpers
#' @name obs_roundtime
#' @description return rounded seconds from time
#' @param x time as "POSIXct" "POSIXt"
#' @param n factor
#' @return numeric vector
#' @importFrom data.table second
#' @export
#' @examples \dontrun{
#' # Do not run
#' x <- Sys.time() + seq(1, 55, 1)
#' paste0(x,"  ",
#'        obs_roundtime(x), "  ",
#'        obs_freq(data.table::second(x),
#'                 seq(0, 55, 10)))
#' }
obs_roundtime <- function(x, n = 10) {
  if (!inherits(x, "POSIXct")) {
    stop("`x` must have classes `POSIXct` `POSIXt` ")
  }
  y <- ((data.table::second(x) + 5 / 2) %/% n) * n
  y <- ifelse(y == 60, 0, y)
  return(y)
}

#' @title rbind obspack
#' @family helpers
#' @name obs_rbind
#' @description return rbind obs data.tables
#' @param dt1 first data.table
#' @param dt2  second data.table
#' @param verbose logical to show more information
#' @return numeric vector
#' @importFrom data.table data.table
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
obs_rbind <- function(dt1, dt2, verbose = TRUE) {
  if (verbose) {
    cat("Identifying common names\n")
  }

  neco <- intersect(names(dt1), names(dt2))
  if (verbose) {
    print(neco)
  }

  dt1 <- dt1[,
    neco,
    with = F
  ]

  dt2 <- dt2[,
    neco,
    with = F
  ]

  cl1 <- (lapply(dt1, class))
  cl2 <- (lapply(dt2, class))

  dt <- data.table::data.table(names = neco, class1 = cl1, class2 = cl2)

  lx <- lapply(1:nrow(dt), function(i) {
    (cl1[[i]] == cl2[[i]])[1]
  })

  dt$equal <- lx

  if (verbose) {
    cat("Comparing classes by column\n")
  }

  equal <- NULL
  if (nrow(dt[equal == F]) > 0) {
    if (verbose) {
      cat("Found different classes:\n")
    }
    print(dt[equal == F])
    # stop()
  }
  dx <- rbind(dt1, dt2, use.names = TRUE)

  return(dx)
}

#' @title Generates YAML and write data.frame
#' @family helpers
#' @name obs_write_csvy
#' @description Add YAML header info and writes a the data.frame
#' into disk. The YAML section includes notes and str(dt).
#' @param dt data.table
#' @param notes notes.
#' @param out outfile path.
#' @param sep  The separator between columns. Default is ",".
#' @param nchar.max  Max nchar for str.
#' @param ... extra data.table arguments
#' @return a csvy file to disk
#' @importFrom utils str
#' @export
#' @examples \dontrun{
#' # Do not run
#' df <- data.frame(a = rnorm(n = 10),
#'                  time = Sys.time() + 1:10)
#'
#' f <- paste0(tempfile(), ".csvy")
#' notes <- c("notes",
#'            "more notes")
#' obs_write_csvy(dt = df, notes = notes, out = f)
#' readLines(f)
#' data.table::fread(f, h = TRUE)
#' }
obs_write_csvy <- function(
  dt,
  notes,
  out = paste0(tempfile(), ".csvy"),
  sep = ",",
  nchar.max = 80,
  ...
) {
  sink(file = out)
  cat("---\n")
  cat("name: Metadata \n")
  cat(notes, sep = "\n")
  cat("structure: \n")
  print(utils::str(
    object = dt,
    vec.len = 2,
    width = 60,
    strict.width = "cut",
    nchar.max = nchar.max
  ))
  cat("---\n")
  sink()
  data.table::fwrite(
    x = dt,
    file = out,
    sep = sep,
    append = TRUE,
    col.names = TRUE,
    ...
  )
}


#' @title reads CSVY
#' @family helpers
#' @name obs_read_csvy
#' @description Reads CSVY, print YAML and fread file.
#' @param f path to csvy file
#' @param n number of files to search for "---" yaml
#' @param ... extra data.table arguments
#' @return a data.table from a csvy file
#' @export
#' @examples \dontrun{
#' # Do not run
#' df <- data.frame(a = rnorm(n = 10),
#'                  time = Sys.time() + 1:10)
#'
#' f <- paste0(tempfile(), ".csvy")
#' notes <- c("notes",
#'            "more notes")
#' obs_write_csvy(dt = df, notes = notes, out = f)
#' s <- obs_read_csvy(f)
#' s
#' # or
#' readLines(f)
#' data.table::fread(f)
#' }
obs_read_csvy <- function(f, n = 100, ...) {
  nr <- readLines(f, n = 100)

  lis <- grep("---", nr)
  if (length(lis) == 1) {
    nr <- readLines(f, n = 200)
    lis <- grep("---", nr)
  }
  l1 <- lis[1]
  l2 <- lis[2]

  yaml <- readLines(f, n = l2)
  print(yaml)
  data.table::fread(f, h = TRUE, skip = l2, ...)
}

#' @title Trunc numbers with a desired number of decimals
#' @family helpers
#' @name obs_trunc
#' @description Trunc numbers with a specified number of decimals.
#' @param n Numeric  number
#' @param dec Integer, number of decimals
#' @return truncated number
#' @note source https://stackoverflow.com/a/47015304/2418532
#' @export
#' @examples \dontrun{
#' # Do not run
#' # in bash:
#' # printf "%07.4f" 72.05785
#' # results in 72.0578
#' # but:
#' formatC(72.05785, digits = 4, width = 8, format = "f", flag = "0")
#' # results in
#' "072.0579"
#' # the goal is to obtain the same trunc number as using bash, then:
#' formatC(obs_trunc(72.05785, 4),
#'         digits = 4,
#'         width = 8,
#'         format = "f",
#'         flag = "0")
#'}
obs_trunc <- function(n, dec) {
  n <- n + (10^-(dec + 5))
  f <- NULL
  splitNumber <- strsplit(
    x = format(n, digits = 20, format = f),
    split = "\\."
  )[[1]]
  decimalPartTrunc <- substr(x = splitNumber[2], start = 1, stop = dec)
  truncatedNumber <- as.numeric(paste0(splitNumber[1], ".", decimalPartTrunc))
  return(truncatedNumber)
}


#' @title Expected footprint name
#' @family helpers
#' @name obs_footname
#' @description return the expected name for the NetCDF footprint
#' @param time POSIXct time to extract time variblaes
#' @param year numeric number
#' @param month numeric number
#' @param day numeric number
#' @param hour numeric number
#' @param minute numeric number
#' @param second numeric number, optional
#' @param lat numeric number
#' @param lon numeric number
#' @param alt numeric number
#' @param fullpath Logical, to add or not YYYY/MO/hysplit to id
#' @param out outfile path.
#' @param ... data.table::fwrite arguments.
#' @return A concatenation of characters representing a spatio-temporal point.
#' @note source https://stackoverflow.com/a/47015304/2418532
#'
#' # IMPORTANT!!!
#' # This function will generate the expected NetCDF file name.
#' # It assumes that the name was generated under the following considerations:
#' # time variables (year, month, day, etc) have a format of two digits, eg "0.1"
#' # latitude and longitude have been round with 4 decimals
#' # The format for latitude is 2 integers, a point and 4 decimals
#' # The format for longitude is 3 integers, a point and 4 decimals
#'
#'
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs_footname(time = Sys.time(),
#'              second = data.table::second(Sys.Time()),
#'              lat = 0,
#'              lon = 0,
#'              alt = 0)
#' obs_footname(year = 2020,
#'              month = 12,
#'              day = 30,
#'              hour = 9,
#'              minute = 54,
#'              lat = 3.2133,
#'              lon = 30.9131,
#'              alt = 497,
#'              fullpath = TRUE)
#' obs_footname(year = 2020,
#'              month = 12,
#'              day = 30,
#'              hour = 9,
#'              minute = 54,
#'              lat = 1,
#'              lon = -130.9131,
#'              alt = 497,
#'              fullpath = TRUE)
#' }
obs_footname <- function(
  time = NULL,
  year,
  month,
  day,
  hour,
  minute,
  second,
  lat,
  lon,
  alt,
  fullpath = FALSE,
  out,
  ...
) {
  lats <- ifelse(lat > 0, "N", "S")
  lat <- abs(lat)

  lons <- ifelse(lon > 0, "E", "W")
  lon <- abs(lon)

  agl <- alt

  if (!is.null(time)) {
    dt1 <- paste0(
      sprintf(data.table::year(time), fmt = '%02d'),
      "/",
      sprintf(data.table::month(time), fmt = '%02d'),
      "/hysplit"
    )

    dt <- paste0(
      sprintf(data.table::year(time), fmt = '%02d'),
      "x",
      sprintf(data.table::month(time), fmt = '%02d'),
      "x",
      sprintf(as.numeric(strftime(time, "%d", tz = "UTC")), fmt = '%02d'),
      "x",
      sprintf(data.table::hour(time), fmt = '%02d'),
      "x",
      sprintf(data.table::minute(time), fmt = '%02d'),
      "x",
      if (!missing(second)) {
        paste0(sprintf(data.table::second(time), fmt = '%02d'), "x")
      } else {
        NULL
      },
      formatC(
        lat, # this approach works whendata is round(x, 4)
        digits = 4,
        width = 7,
        format = "f",
        flag = "0"
      ),
      lats,
      "x",
      # sprintf(round(lon, 4), fmt = '0%7.4f'),
      formatC(
        lon, # this approach works whendata is round(x, 4)
        digits = 4,
        width = 8,
        format = "f",
        flag = "0"
      ),
      lons,
      "x",
      sprintf(round(agl), fmt = '%05d')
    )
  } else {
    dt1 <- paste0(
      sprintf(year, fmt = '%02d'),
      "/",
      sprintf(month, fmt = '%02d'),
      "/hysplit"
    )

    dt <- paste0(
      sprintf(year, fmt = '%02d'),
      "x",
      sprintf(month, fmt = '%02d'),
      "x",
      sprintf(day, fmt = '%02d'),
      "x",
      sprintf(hour, fmt = '%02d'),
      "x",
      sprintf(minute, fmt = '%02d'),
      "x",
      if (!missing(second)) {
        paste0(sprintf(second, fmt = '%02d'), "x")
      } else {
        NULL
      },
      formatC(
        lat, # this approach works whendata is round(x, 4)
        digits = 4,
        width = 7,
        format = "f",
        flag = "0"
      ),
      lats,
      "x",
      # sprintf(round(lon, 4), fmt = '0%7.4f'),
      formatC(
        lon, # this approach works whendata is round(x, 4)
        digits = 4,
        width = 8,
        format = "f",
        flag = "0"
      ),
      lons,
      "x",
      sprintf(round(agl), fmt = '%05d')
    )
  }

  if (fullpath) {
    x <- paste0(dt1, dt, ".nc")
  } else {
    x <- dt
  }
  if (!missing(out)) {
    data.table::fwrite(x = data.table::as.data.table(x), x = out, ...)
  }
  return(x)
}


#' @title File extension
#' @family helpers
#' @name fex
#' @description file extension
#' @param x character vector giving file paths.
#' @return A character vector of the same length as \code{x} containing the file
#' extensions without the leading dot. Files without an extension return an
#' empty string ("").
#' @note source tools::file_ext
#' @export
#' @seealso \code{tools::file_ext()}
#' @examples \dontrun{
#' # Do not run
#' }
fex <- function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}


#' @title Extacts n last characters
#' @family helpers
#' @name sr
#' @description file extension
#' @param x a character vector.
#' @param n integer.
#' @return last n characters
#' @export
#' @examples \dontrun{
#' # do not run
#' }
sr <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
