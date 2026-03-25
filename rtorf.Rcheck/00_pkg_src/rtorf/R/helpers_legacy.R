#' Helpers Legacy
#'
#'
#' @title obs_id2pos
#' @family helpers legacy
#' @name obs_id2pos
#' @description return ndata.frame based on footprint/receptor id
#' @param id string
#' @param sep string
#' @param asdf Logical, to return as data.frame or not
#' @return data.frame with time and location based on input
#' @export
#' @examples \dontrun{
#' # Do not run
#' id <- '2002x08x03x10x45.00Nx090.00Ex00030'
#' (obs_id2pos(id, asdf = TRUE) -> dx)
#' id <- c('2002x08x03x10x00x45.000Nx090.000Ex00030',
#'         '2002x08x03x10x55x45.335Sx179.884Wx00030')
#' (obs_id2pos(id) -> dx)
#' (obs_id2pos(id, asdf = TRUE) -> dx)
#' (obs_id2pos(rep(id, 2)) -> dx)
#' (obs_id2pos(rep(id, 2), asdf = TRUE) -> dx)
#' }

obs_id2pos <- function(id, sep = "x", asdf = FALSE) {
  # revers of id2pos function to read identifying label for
  # multiple receptors (location&time) returns time as fractional
  # julian day since 1/1/1960 and alt as altitude above ground in
  # meters returns a 2d array for multiple receptors, a vector for
  # a single receptor examples:
  # id2pos('2002x08x03x10x45.00Nx090.00Ex00030') time lat lon alt
  # yr mon day hr min 15555.42 45.00 90.00 30.00 2002 08 03 10 00
  # id2pos(c('2002x08x03x10x00x45.000Nx090.000Ex00030','2002x08x03x10x55x45.335Sx179.884Wx00030'))
  # time lat lon alt yr mon day hr min 15555.42 45.000 90.000 30
  # 2002 08 03 10 00 15555.45 -45.335 -179.884 30 2002 08 03 10 55
  # $Id: id2pos.r,v 1.6 2012/09/24 19:34:37 mellis Exp $
  #---------------------------------------------------------------------------------------------------

  # get format using separator location (assumes same format is
  # used for all elements of id)
  cpos <- as.numeric(gregexpr("x", id[1], fixed = TRUE)[[1]])
  # Detect whether the date contains a minute string Note: this
  # will fail if the date does not include it, but there are
  # extraneous characters after the agl string that contain the
  # separator string

  encode.minutes <- length(cpos) > 6
  yr4 <- as.numeric(substring(id, 1, cpos[1] - 1))
  mon <- as.numeric(substring(id, cpos[1] + 1, cpos[2] - 1))
  day <- as.numeric(substring(id, cpos[2] + 1, cpos[3] - 1))
  hr <- as.numeric(substring(id, cpos[3] + 1, cpos[4] - 1))
  ipos <- 4
  if (encode.minutes) {
    min <- as.numeric(substring(id, cpos[4] + 1, cpos[5] - 1))
    frac.hr <- hr + min / 60
    ipos <- ipos + 1
  } else {
    frac.hr <- hr
    min <- 0
  }
  lat <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lat <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) == "S",
    -lat,
    lat
  )
  ipos <- ipos + 1
  lon <- as.numeric(substring(id, cpos[ipos] + 1, cpos[ipos + 1] - 2))
  lon <- ifelse(
    substring(id, cpos[ipos + 1] - 1, cpos[ipos + 1] - 1) == "W",
    -lon,
    lon
  )
  ipos <- ipos + 1
  # strip any alpha characters (usually trailing) from agl string:
  alt <- as.numeric(sub("[a-z]+", "", substring(id, cpos[ipos] + 1)))

  time <- obs_julian(mon, day, yr4) +
    frac.hr / 24

  pos <- cbind(
    time = time,
    lat = lat,
    lon = lon,
    alt = alt,
    yr = yr4,
    mon = mon,
    day = day,
    hr = hr,
    min = min
  )

  nx <- dimnames(pos)[[2]]

  xx <- pos[1:dim(pos)[1], , drop = TRUE]

  if (asdf) {
    m <- matrix(xx, nrow = length(xx) / length(nx))
    df <- data.frame(m)
    names(df) <- nx
    return(df)
  } else {
    return(xx)
  }
}

#' Helpers Legacy
#'
#'
#' @title obs_julian
#' @family helpers legacy
#' @name obs_julian
#' @description return numeric
#' @param m numeric
#' @param d numeric
#' @param y numeric
#' @param origin. string
#' @return returns day since 1/1/1960
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs_julian(1, 2020, 1)
#' }
obs_julian <- function(m, d, y, origin.) {
  # returns day since 1/1/1960
  #
  #  $Id: julian.r,v 1.2 2007/06/27 11:54:03 skoerner Exp $
  #---------------------------------------------------------------------------------------------------

  only.origin <- all(missing(m), missing(d), missing(y))
  if (only.origin) {
    m <- d <- y <- NULL
  }
  # return days since origin
  nonnumeric.p <- !c(is.numeric(m), is.numeric(d), is.numeric(y))
  if (any(nonnumeric.p) && !only.origin) {
    badarg <- paste(c("m", "d", "y")[nonnumeric.p], collapse = ", ")
    stop(paste("Arguments:", badarg, "are not numeric"))
  }
  if (missing(origin.)) {
    if (is.null(origin. <- .Options$chron.origin)) {
      origin. <- c(month = 1, day = 1, year = 1960)
    }
  }
  nms <- names(d)
  max.len <- max(length(m), length(d), length(y))
  #
  # prepend new origin value and rep out to common max. length:
  m <- c(origin.[1], rep(m, length = max.len))
  d <- c(origin.[2], rep(d, length = max.len))
  y <- c(origin.[3], rep(y, length = max.len))
  #
  # code from julian date in the S book (p.269)
  #
  y <- y + ifelse(m > 2, 0, -1)
  m <- m + ifelse(m > 2, -3, 9)
  c <- y %/% 100
  ya <- y - 100 * c
  out <- (146097 * c) %/%
    4 +
    (1461 * ya) %/% 4 +
    (153 * m + 2) %/% 5 +
    d +
    1721119
  #
  # now subtract the new origin from all dates
  #
  if (!only.origin) {
    if (all(origin. == 0)) {
      out <- out[-1]
    } else {
      out <- out[-1] - out[1]
    }
  }
  names(out) <- nms
  out
}


#' @title obs_info2id
#' @family helpers legacy
#' @name obs_info2id
#' @description return footprint/receptor id
#' @param yr  year
#' @param mo  month
#' @param dy day
#' @param hr hour
#' @param mn minute
#' @param lat latitude
#' @param lon longitude
#' @param alt altitude above ground level
#' @param sep character, default"x"
#' @param long Logical, to add minute, and rounded with 2 decimals, instead of 4. default TRUE
#' @return a string with the receptor id
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs_info2id(yr = 2002,
#'             mo = 8,
#'             dy = 3,
#'             hr = 10,
#'             mn = 0,
#'             lat = 42,
#'             lon = -90,
#'             alt = 1) [1]
#' }
obs_info2id <- function(
  yr,
  mo,
  dy,
  hr,
  mn = 0,
  lat,
  lon,
  alt,
  sep = "x",
  long = TRUE
) {
  # function to create identifying label for single receptor
  # (location&time) expects alt as altitude above ground in meters
  # example: info2id(2002,8,3,10,45,-90,0.03) [1]
  # '2002x08x03x10x+45.00x+090.00x00030' Caution: No rounding to
  # nearest hour when long=F Cation: Arguments must be specified
  # when long=F (cannot rely on order when mn is absent).
  #---------------------------------------------------------------------------------------------------

  # 10/21/2024, Sergio
  # just to check missing arguments
  if (missing(yr)) {
    stop("Add year")
  }
  if (missing(mo)) {
    stop("Add month")
  }
  if (missing(dy)) {
    stop("Add day")
  }
  if (missing(hr)) {
    stop("Add hour")
  }
  if (missing(lat)) {
    stop("Add lat")
  }
  if (missing(lon)) {
    stop("Add lon")
  }
  if (missing(alt)) {
    stop("Add alt")
  }

  # need leading zeros
  hr <- substring(100 + hr, 2) #2 digit hr
  yr4 <- substring(10000 + yr, 2) #4 digit year
  mon <- substring(100 + mo, 2) #2 digit mon
  day <- substring(100 + dy, 2) #2 digit day
  mn <- substring(100 + mn, 2) #2 digit day

  # location
  if (!long) {
    lat <- round(lat, 2) #1 km roundoff
    lon <- round(lon, 2) #1 km roundoff
  } else {
    lat <- round(lat, 4)
    lon <- round(lon, 4)
  }

  alt <- round(alt) #1 m roundoff
  # no below ground...
  alt[alt < 0] <- 0
  # need leading zeros and sign
  lats <- rep("S", length(lat))
  lons <- rep("W", length(lat))
  lats[lat >= 0] <- "N"
  lons[lon >= 0] <- "E"
  if (!long) {
    lat <- paste(substring(100.001 + abs(lat), 2, 6), lats, sep = "") #2 dig. before decimal + sign
    lon <- paste(substring(1000.001 + abs(lon), 2, 7), lons, sep = "") #3 dig. before decimal + sign
  } else {
    lat <- paste(substring(100.00001 + abs(lat), 2, 8), lats, sep = "") #2 dig. before decimal + sign
    lon <- paste(substring(1000.00001 + abs(lon), 2, 9), lons, sep = "") #3 dig. before decimal + sign
  }
  alt <- substring(1e+05 + alt, 2)
  out <- paste(yr4, mon, day, hr, lat, lon, alt, sep = sep) #put everything together
  if (long) {
    out <- paste(yr4, mon, day, hr, mn, lat, lon, alt, sep = sep)
  }
  return(out)
}


#' @title obs_julian
#' @family helpers legacy
#' @name obs_julian
#' @description return footprint/receptor id
#' @param y  year
#' @param m  month
#' @param d day
#' @param legacy logical, to use legacy code
#' @param verbose logical, to show more messages
#' @return a string with the receptor id
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs_julian(y = 2002,
#'             m = 8,
#'             d = 3,
#'             legacy = TRUE)
#' obs_julian(y = 2002,
#'             m = 8,
#'             d = 3,
#'             legacy = FALSE)
#' }
obs_julian <- function(y, m, d, origin., legacy = FALSE, verbose = TRUE) {
  if (legacy) {
    if (verbose) {
      cat("Using legacy code\n")
    }
    #returns day since 1/1/1960
    #
    #  $Id: julian.r,v 1.2 2007/06/27 11:54:03 skoerner Exp $
    #---------------------------------------------------------------------------------------------------

    only.origin <- all(missing(m), missing(d), missing(y))
    if (only.origin) {
      m <- d <- y <- NULL
    }
    # return days since origin
    nonnumeric.p <- !c(is.numeric(m), is.numeric(d), is.numeric(y))
    if (any(nonnumeric.p) && !only.origin) {
      badarg <- paste(c("m", "d", "y")[nonnumeric.p], collapse = ", ")
      stop(paste("Arguments:", badarg, "are not numeric"))
    }
    if (missing(origin.)) {
      if (is.null(origin. <- .Options$chron.origin)) {
        origin. <- c(month = 1, day = 1, year = 1960)
      }
    }
    nms <- names(d)
    max.len <- max(length(m), length(d), length(y))
    #
    # prepend new origin value and rep out to common max. length:
    m <- c(origin.[1], rep(m, length = max.len))
    d <- c(origin.[2], rep(d, length = max.len))
    y <- c(origin.[3], rep(y, length = max.len))
    #
    # code from julian date in the S book (p.269)
    #
    y <- y + ifelse(m > 2, 0, -1)
    m <- m + ifelse(m > 2, -3, 9)
    c <- y %/% 100
    ya <- y - 100 * c
    out <- (146097 * c) %/%
      4 +
      (1461 * ya) %/% 4 +
      (153 * m + 2) %/% 5 +
      d +
      1721119
    #
    # now subtract the new origin from all dates
    #
    if (!only.origin) {
      if (all(origin. == 0)) {
        out <- out[-1]
      } else {
        out <- out[-1] - out[1]
      }
    }
    names(out) <- nms
    return(out)
  } else {
    day <- ISOdate(
      year = y,
      month = m,
      day = d,
      hour = 0,
      min = 0,
      sec = 0,
      tz = "UTC"
    )

    y1960 <- ISOdate(
      year = 1960,
      month = 1,
      day = 1,
      hour = 0,
      min = 0,
      sec = 0,
      tz = "UTC"
    )
    out <- day - y1960
    return(out)
  }
}

#' @title obs_traj_foot
#' @family helpers legacy
#' @name obs_traj_foot
#' @description return trajectory
#' @param ident foot id
#' @param part data.table with PARTICLE.DAT information
#' @param timelabel timelabel
#' @param pathname pathname
#' @param foottimes vector of times between which footprint or influence will be integrated
#' @param zlim zlim
#' @param coarse default 1
#' @param dmassTF default TRUE weighting by accumulated mass due to violation of mass conservation in met fields
#' @param numpix.x number of pixels in x directions in grid
#' @param numpix.y number of pixels in y directions in grid
#' @param lon.ll lower left corner of grid (longitude of southwest corner of southwest corner gridcell)
#' @param lat.ll lower left corner of grid (latitude of southwest corner of southwest corner gridcell)
#' @param lon.ll lower left lon
#' @param lat.ll lower left lat
#' @param lon.res resolution
#' @param lat.res resolution
#' @param npar default 500, number of particles hysplit was run with (required in order to
#' account for those cases where a thinned particle table that may not contain all particle indices is used)
#' @param eps.global default 0.01
#' @return return footprint
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
obs_traj_foot <- function(
  ident,
  part = NULL,
  timelabel = NULL,
  pathname = "",
  foottimes = 0:240,
  zlim = c(0, 0),
  coarse = 1,
  dmassTF = TRUE,
  numpix.x = 70,
  numpix.y = 70,
  lon.ll = -145,
  lat.ll = 11,
  lon.res = 1,
  lat.res = 1,
  npar = 500,
  eps.global = 0.1
) {
  part <- data.table::as.data.table(part)
  rqdnames <- c("time", "lat", "lon", "agl", "zi", "index", "foot")
  if (any(!(rqdnames %in% names(part)))) {
    stop("not all columns available for this run")
  }

  if (is.null(npar)) {
    stop('Missing npar')
  }

  if (dmassTF) {
    if (any(names(part) == 'dmass')) {
      dmassname <- 'dmass'
    } else {
      if (any(names(part) == 'ndmass')) {
        dmassname <- 'ndmass'
      } else {
        stop("dmass or ndmass column is not available for this run")
      }
    }
  }

  # null args for checking
  foot <- index <- sumx <- NULL

  #get grid indices
  #For horizontal grids (lower left corner of south-west gridcell: 11N,145W; resolution: 1/4 lon, 1/6 lat, 376 (x) times 324 (y))
  lats <- seq(lat.ll, (lat.ll + (numpix.y - 1) * lat.res), lat.res)
  lons <- seq(lon.ll, (lon.ll + (numpix.x - 1) * lon.res), lon.res)

  if (any(lons > 180)) {
    #lon.ll is specified for Eastern Hemisphere and footprint
    #grid crosses dateline into Western Hemisphere
    partlons <- part$lon
    partlons[partlons < 0] <- partlons[partlons < 0] + 360
    gitx <- floor(1 / lon.res * (partlons - lon.ll) + 1)
  } else {
    #assumes westernmost gridcell is smallest longitude & easternmost
    #gridcell is largest longitude; does not work over dateline
    gitx <- floor(1 / lon.res * (part$lon - lon.ll) + 1)
  }

  # Check for global grid, adjust gitx accordingly: define nperiodic.x as: lon(i)=lon(i+nperiodic.nx)
  periodic.nx <- NULL
  #case 1: explicitly periodic, lon(1)=lon(numpix.x):
  if (abs(abs((numpix.x - 1) * lon.res) - 360) < abs(eps.global * lon.res)) {
    periodic.nx <- numpix.x - 1
  }
  #case 2: implicitly periodic, lon(1)=lon(numpix.x)+lon.res:
  if (abs(abs(numpix.x * lon.res) - 360) < abs(eps.global * lon.res)) {
    periodic.nx <- numpix.x
  }
  if (!is.null(periodic.nx)) {
    # For periodic grid in longitude: 1 <= gitx <= periodic.nx
    gitx[gitx < 1] <- gitx[gitx < 1] + periodic.nx
    gitx[gitx > periodic.nx] <- gitx[gitx > periodic.nx] - periodic.nx
  }

  #assumes southernmost gridcell is smallest latitude & northernmost gridcell
  #is largest latitude; does not work over poles
  global.lats <- abs(abs(lat.res * numpix.y) - 180) < abs(eps.global) * lat.res
  global.msg <- 'Using a grid that is '
  if (is.null(periodic.nx)) {
    global.msg <- c(global.msg, ' not global in lon')
  } else {
    global.msg <- c(global.msg, ' global in lon with periodic.nx=', periodic.nx)
  }
  global.msg <- c(global.msg, ' and global.lats=', global.lats)
  cat(global.msg, '\n', sep = '')
  gity <- floor(1 / lat.res * (part$lat - lat.ll) + 1)

  #edit lons over dateline (for use in foot dimnames)
  lons[lons > 180] <- lons[lons > 180] - 360

  part <- cbind(part, gitx, gity)

  if ("time" %in% names(part)) {
    time <- NULL
    part[, btime := abs(time) / 60]
  }

  # ordering to make sure
  data.table::setorderv(part, c("btime", "index"))

  if (dmassTF) {
    if ('ndmass' %in% names(part)) {
      cat('Using existing ndmass to remove particles with dmass violation\n')
    } else {
      cat('Calculating ndmass to remove particles with dmass violation\n')
      part <- obs_normalize_dmass(part = part)
    }

    #remove rows where ndmass =NA
    ndmass <- NULL
    part <- part[!is.na(ndmass)]
  }

  # ordering to make sure
  data.table::setorderv(part, c("btime", "index"))

  if (is.null(periodic.nx)) {
    # Set the 'gitx' and 'gity' columns to NA for particles that have a gitx < 1.
    # This marks them as being outside the valid grid area.

    part[floor(gitx) < 1, `:=`(gitx = NA, gity = NA)]

    # Calculate the cumulative sum of 'gitx' for each particle. The cumsum
    # will propagate NA values, marking all subsequent rows for any particle
    # that has entered the background area.
    part[,
      sumx := cumsum(gitx),
      by = index
    ]

    # Filter out all rows where the cumulative sum is NA. This removes the particle
    # from the dataset from the point it enters the background area onward.
    part <- part[!is.na(sumx)]

    # Clean up the temporary 'sumx' column for memory efficiency.
    part[, sumx := NULL]
  }

  #only keep points, when information is changed:
  if (is.null(periodic.nx)) {
    # For non-periodic longitude grids
    if (!global.lats) {
      # Non-periodic longitude, non-global latitude
      part <- part[
        floor(gitx) >= 1 &
          floor(gitx) <= numpix.x &
          floor(gity) >= 1 &
          floor(gity) <= numpix.y &
          foot > 0
      ]
    } else {
      # Non-periodic longitude, global latitude
      part <- part[
        floor(gitx) >= 1 &
          floor(gitx) <= numpix.x &
          foot > 0
      ]
    }
  } else {
    # For periodic longitude grids
    if (!global.lats) {
      # Periodic longitude, non-global latitude
      part <- part[
        floor(gity) >= 1 &
          floor(gity) <= numpix.y &
          foot > 0
      ]
    } else {
      # Periodic longitude, global latitude
      part <- part[foot > 0]
    }
  }

  if (zlim[2] > 0) {
    # "Volume" influence: calculate residence time and filter by altitude.
    btime <- agl <- NULL
    part[, foot := c(diff(btime), 0) * 60, by = index]
    part <- part[agl > zlim[1] & agl <= zlim[2]]
  }

  #move x and y position of final position to initialization area (gitx=1, gity= 1 to numpix.y), at least make sure they are not outside NGM
  part[gitx > numpix.x, gitx := numpix.x]
  part[gitx < 1, gitx := 1]

  part[gity > numpix.y, gity := numpix.y]
  part[gity < 1, gity := 1]

  part[, btime := round(btime, 2)]

  #create object for output: 3d array (lat-lon-time)
  foot.arr <- array(0, dim = c(numpix.y, numpix.x, length(foottimes) - 1))

  for (foottimespos in 1:(length(foottimes) - 1)) {
    #loop over time intervals

    # to iterate between 0 to 240 every hour
    subpart <- part[
      btime > foottimes[foottimespos] &
        btime <= foottimes[foottimespos + 1]
    ]

    if (length(unlist(subpart)) <= 21) {
      next
    }

    data.table::setorderv(subpart, c("btime", "index"))

    # get different resolutions for surface grids depending on range in x and y
    # and on particle number for each timestep
    # get selector for first and last row w/ a given btime

    selfirst <- c(T, diff(subpart$btime) > 0)

    selast <- c(diff(subpart$btime) > 0, T)

    max.x <- subpart[selast > 0]$gitx

    min.x <- subpart[selfirst > 0]$gitx

    max.y <- subpart[selast > 0]$gity

    min.y <- subpart[selfirst > 0]$gity

    btime <- subpart[selfirst > 0]$btime

    #now get information back in format for all timesteps and index-numbers
    minmax.yx <- data.table::data.table(btime, max.x, min.x, max.y, min.y)
    minmax.yx <- merge(subpart[, c("btime", "index")], minmax.yx, by = "btime")

    max.x <- minmax.yx$max.x
    min.x <- minmax.yx$min.x
    max.y <- minmax.yx$max.y
    min.y <- minmax.yx$min.y

    #Call '' to get correct emission grid--necessary b/c emission grid is too large,
    #  so divided into several diff objects
    #use getgridp.ssc function: don't allow the resolution to get
    # finer at earlier backtime; use cummax(ran.x)

    gridresult <- obs_grid(
      min.x,
      max.x,
      min.y,
      max.y,
      numpix.x,
      numpix.y,
      coarse.factor = coarse
    )
    emissname <- paste0(
      gridresult$xpart,
      gridresult$ypart,
      gridresult$gridname
    )

    #Extract appropriate emissions within each emission grid--do one grid at a time
    #  b/c reduces # of times grid has to be accessed
    coarsex <- c(1, 1, 2, 2, 2, 4, 4, 4, 8, 8, 8, 16, 16, 16, 32, 32) #factors by which grids have been made coarser
    coarsey <- c(1, 2, 1, 2, 4, 2, 4, 8, 4, 8, 16, 8, 16, 32, 16, 32) #e.g., '4' means grid is 4 times coarser
    #loop over different surface grids
    emissgrid.all <- matrix(0, nrow = numpix.y, ncol = numpix.x) #initialize fine grid with "0" everywhere

    for (name in unique(emissname)) {
      emissgrid <- matrix(0, nrow = numpix.y, ncol = numpix.x) #initialize fine grid with "0" everywhere

      sel <- emissname == name

      xpart <- gridresult[sel, unique(xpart)] #xpart can be 0~3

      ypart <- gridresult[sel, unique(ypart)] #ypart can be 0~3

      gridname <- gridresult[sel, unique(gridname)] #gridname can be 1~16, representing diff. resolutions of grid

      x <- subpart[sel]$gitx

      y <- subpart[sel]$gity

      #Convert mins & maxes from coordinate values to rows & columns
      shrink.x <- coarsex[gridname]

      shrink.y <- coarsey[gridname]

      if (
        gridname > 3 |
          xpart == 0 |
          ypart == 0
      ) {
        #grids have NOT been divided
        x <- ceiling(x / shrink.x)
        y <- ceiling(y / shrink.y)
      } else {
        #grids have been divided up
        x <- floor((x - floor((numpix.x / 4) * (xpart - 1))) / shrink.x)
        y <- floor((y - floor((numpix.y / 4) * (ypart - 1))) / shrink.y)
      }
      #get index pairs to extract surface pixels
      yxv <- y * 1000 + x #separate in order of magnitude

      #get index pairs to extract surface pixels, for extracting emission grid (e.g. CO)
      yx <- cbind(y, x)

      ##########BUDGET##########
      if (zlim[2] == 0) {
        #surface influence
        #Take emission values at particle positions; multiply by "foot", i.e. sensitivity of mixing ratio changes to fluxes,
        #in ppm/(micro-mol/m^2/s)
        influence <- subpart[sel]$foot
        #also multiplied by dmass (accumulated weight of particles due to mass violation, normalized by average dmass to conserve total mass over time)
        if (dmassTF) influence <- influence * subpart[sel]$ndmass # make it work wiht dmass also
      } else {
        #different "budget" for volume influence (i.e. zlim[2]>0)
        influence <- subpart[sel]$foot * 60 #res. time in seconds
        if (dmassTF) influence <- influence * subpart[sel]$ndmass
      }

      for (yxg in unique(yxv)) {
        #for each gridcell...
        #now need to vary each x and y between coarse grid cells to map it on fine grid cell
        selunix <- yxv == yxg
        xfine <- yxg - floor(yxg / 1000) * 1000
        yfine <- floor(yxg / 1000)
        if (gridname > 3 | xpart == 0 | ypart == 0) {
          #grids have NOT been divided
          xfine <- (xfine - 1) * shrink.x + 1
          yfine <- (yfine - 1) * shrink.y + 1
          xfine <- xfine:(xfine + shrink.x - 1)
          yfine <- yfine:(yfine + shrink.y - 1)
        } else {
          #grids have been divided up
          xfine <- (xfine - 1) * shrink.x + 1
          yfine <- (yfine - 1) * shrink.y + 1
          xfine <- xfine + floor((numpix.x / 4) * (xpart - 1)) #shift number to sub grid position
          yfine <- yfine + floor((numpix.y / 4) * (ypart - 1))
          xfine <- xfine:(xfine + shrink.x - 1)
          yfine <- yfine:(yfine + shrink.y - 1)
        }
        #cut out areas which are not in fine grid
        xfine <- xfine[xfine <= numpix.x]
        yfine <- yfine[yfine <= numpix.y]
        #get number of pixels (every combination if fine pixel indices)
        npixfx <- length(xfine)
        npixfy <- length(yfine)

        emissgrid[yfine, xfine] <- sum(influence[selunix]) / (npixfx * npixfy) #'dilute' influence over larger area of fine grid
      } # for each gridcell...

      emissgrid.all <- emissgrid.all + emissgrid / npar #uses number of particles used to determine footprint
    } #for different emissname

    #JCL(5/23/2004)------- not normalize by 'foottimes',b/c want TIME-INTEGRATED footprint----------------#
    #foot.arr[,,foottimespos]<-emissgrid.all/(foottimes[foottimespos+1]-foottimes[foottimespos])
    #JCL(5/23/2004)------- not normalize by 'foottimes',b/c want TIME-INTEGRATED footprint----------------#
    foot.arr[,, foottimespos] <- emissgrid.all
  } #loop over time intervals

  #For horizontal grids (lower left corner of south-west gridcell: 11N,145W; resolution: 1/4 lon, 1/6 lat, 376 (x) times 324 (y))
  dimnames(foot.arr) <- list(lats, lons, foottimes[1:(length(foottimes) - 1)])
  return(foot.arr)
}

#' @title obs_normalize_dmass
#' @family helpers legacy
#' @name obs_normalize_dmass
#' @description add columns of
#' @param part particle data.table
#' @return return footprint
#' @export
#' @examples \dontrun{
#' # Do not run
#' }
obs_normalize_dmass <- function(part = NULL) {
  # Convert part to a data.table if it isn't one already
  if (!inherits(part, "data.table")) {
    part <- data.table::as.data.table(part)
  }

  # null args for checking
  btime <- dmass <- index <- mean.dmass <- ndmass <- NULL

  # check if btime is not presnet and add it
  if (!"btime" %in% names(part)) {
    cat("Adding btime\n")
    time <- NULL
    part[, btime := abs(time) / 60]
  }

  # must be in this order (might not be...)
  # this is from lower time to bigger time by part index
  # index from first to last repeating btime
  data.table::setorderv(part, c("btime", "index")) #data.table

  # Identify and remove particles with too strong dmass violation
  # `[.data.table` can be used to subset efficiently

  ind <- part[
    dmass > 1E3 |
      dmass < 1 / 1E3,
    unique(index)
  ]
  #' #' # Do not run

  if (length(ind) >= length(unique(part[, index])) / 2) {
    stop("50% or more particles have mass defect")
  }

  part[index %in% ind, dmass := NA_real_]

  # Calculate the mean dmass for each btime using `by` argument
  part[, mean.dmass := mean(dmass, na.rm = TRUE), by = btime]

  # Normalize dmass by mean.dmass using `:=`
  part[, ndmass := dmass / mean.dmass]

  # Remove rows where normalized dmass is NA
  part <- part[!is.na(ndmass)]

  # Select and rename columns, and return
  return(part)
}
