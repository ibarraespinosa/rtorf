#' @title obs_grid
#' @family helpers legacy
#' @name obs_grid
#' @description add columns of
#' @param min.x grid indices
#' @param max.x grid indices
#' @param min.y grid indices
#' @param max.y grid indices
#' @param numpix.x number pixels x
#' @param numpix.y number pixels y
#' @param coarse.factor integer to coarse the grid resolution
#' @param adaptive logical, to use adaptive gridding
#' @return return footprint
#' @export
#' @examples
#' obs_grid(1, 10, 1, 10, 100, 100, 1)
obs_grid <- function(
  min.x,
  max.x,
  min.y,
  max.y,
  numpix.x,
  numpix.y,
  coarse.factor = 1,
  adaptive = TRUE
) {
  leng <- length(min.x)
  #number of elements, or timepoints
  #this will come in handy later
  ran.x <- max.x - min.x
  ran.y <- max.y - min.y
  ran.x <- cummax(ran.x) #don't allow resolution to get finer at earlier btimes
  ran.y <- cummax(ran.y)
  #adapt coarse.factor
  cf <- c(1, 2, 4, 8, 16, 32)
  mr <- c(0, 6, 12, 24, 48, 96)
  mr <- cbind(cf, mr)
  mr <- mr[mr[, "cf"] == coarse.factor, "mr"]
  ran.x[ran.x < mr] <- mr * rep(1, leng)[ran.x < mr]
  ran.y[ran.y < mr] <- mr * rep(1, leng)[ran.y < mr]
  #calculate ranges
  minranx <- c(0, 0, 6, 6, 6, 12, 12, 12, 24, 24, 24, 48, 48, 48, 96, 96)
  minrany <- c(0, 6, 0, 6, 12, 6, 12, 24, 12, 24, 48, 24, 48, 96, 48, 96)
  gridname <- rep(0, leng)
  # gridname represents different resolutions of emission grid--16 is coarsest & 1 is finest
  for (i in seq_along(minranx)) {
    minranxTF <- ran.x >= minranx[i]
    minranyTF <- ran.y >= minrany[i]
    gridname[minranxTF & minranyTF] <- i
  }

  if (coarse.factor == 0 || !adaptive) {
    gridname <- rep(1, leng)
  } #use high resolution for coarse.factor 0
  #xpart & ypart can range betw. 0~3.These tell you which piece of whole grid is needed.
  #If value is 0, then means that ENTIRE grid is used.
  #Note that only when resolution is fine (gridname<=3) is emission grid broken down.
  #Note also that broken down emission grids can OVERLAP.
  xpart <- rep(0, leng)
  ypart <- rep(0, leng)
  first.x <- (min.x >= 1) & (max.x <= 0.5 * numpix.x)
  second.x <- (min.x > 0.25 * numpix.x) & (max.x <= 0.75 * numpix.x)
  third.x <- (min.x > 0.5 * numpix.x) & (max.x <= 1 * numpix.x)
  first.y <- (min.y >= 1) & (max.y <= 0.5 * numpix.y)
  second.y <- (min.y > 0.25 * numpix.y) & (max.y <= 0.75 * numpix.y)
  third.y <- (min.y > 0.5 * numpix.y) & (max.y <= 1 * numpix.y)
  largedx <- !(first.x | second.x | third.x)
  largedy <- !(first.y | second.y | third.y)
  sel <- gridname == 1
  xpart[first.x & sel] <- 1
  xpart[second.x & sel] <- 2
  xpart[third.x & sel] <- 3
  ypart[first.y & sel] <- 1
  ypart[second.y & sel] <- 2
  ypart[third.y & sel] <- 3
  sel <- gridname == 2
  xpart[first.x & sel] <- 1
  xpart[second.x & sel] <- 2
  xpart[third.x & sel] <- 3
  ypart[(!largedx) & sel] <- 1
  #if deltax is small enough, then ypart must = 1
  sel <- gridname == 3
  ypart[first.y & sel] <- 1
  ypart[second.y & sel] <- 2
  ypart[third.y & sel] <- 3
  xpart[(!largedy) & sel] <- 1
  #if deltay is small enough, then xpart must = 1
  #deltax or deltay is too large, so have to use coarse grid
  xpart[largedx | largedy] <- 0
  ypart[largedx | largedy] <- 0
  result <- data.table::data.table(xpart, ypart, gridname)
  return(result)
}
