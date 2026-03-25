#' Read obspack metadata
#'
#' @param dt data.table
#' @param time x axis column (time)
#' @param y y axis column, default "value"
#' @param yfactor factor of y
#' @param colu column to plot by color, default site_code
#' @param type type of plot, default "p"
#' @param n Character indicating `colu` to subset, for instance,
#' if you want to plot "site_code", here include all the
#' site_code that you want to plot
#' @param pal Color palette name see \link[cptcity]{cpt},
#' default "cb_qual_Accent_08"
#' @param xlab Character, xlab
#' @param ylab Character, ylab
#' @param verbose Logical to show more information
#' @param ... plot arguments
#' @param xlim x limits
#' @param ylim y limits
#' @return Plot
#' @importFrom cptcity cpt find_cpt
#' @importFrom graphics par lines
#' @importFrom data.table as.data.table
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' dt <- obs_read(index)
#' obs_plot(dt, time = "time")
#' }
obs_plot <- function(dt,
                     time,
                     y = "value",
                     yfactor = 1,
                     colu = "site_code",
                     type = "p",
                     n = if(length(unique(dt[[colu]])) == 1) unique(dt[[colu]]) else unique(dt[[colu]])[1:2] ,
                     pal = cptcity::find_cpt("qual")[6],
                     verbose = TRUE,
                     xlab = "time",
                     ylab = "value",
                     xlim = range(dt[[time]], na.rm = TRUE),
                     ylim = range(dt[[y]], na.rm = TRUE),
                     ...){

  oldpar <- graphics::par(no.readonly = TRUE)       # code line i
  on.exit(graphics::par(oldpar))                    # code line i + 1

  dt <- data.table::as.data.table(dt)
  dt[[y]] <- dt[[y]]*yfactor
  value <- NULL
  nmiss <- nrow(dt[is.na(value)])

  if(nmiss > 0) if(verbose) cat("Found ", nmiss, "missing `value` on dt\n")

  dt <- dt[!is.na(value)]

  if(length(colu) > 1) stop("column must be unique name ")

  usi <- unique(dt[[colu]])
  if(verbose){
  cat("Found the following sites: \n")
  print(usi, quote = FALSE)
  }
  if(length(n) >= length(usi)){
   nn <- usi
  } else {
    nn <- n
  }

  if(verbose) {
  cat("Plotting the following sites: \n")
  print(n, quote = FALSE)
  }

  dt <- dt[dt[[colu]] %in% n]

  lx <- split(dt, dt[[colu]])

  colores <- cptcity::cpt(pal,
                          n = length(lx))

  par(mar = c(5, 4, 1.4, 0.2))

  plot(x = lx[[1]][[time]],
       y = lx[[1]][[y]],
       col = colores[1],
       type = type,
       pch = 16,
       xlab = xlab,
       ylab = ylab,
       xlim = xlim,
       ylim = ylim,
       ...)


  if(length(lx) > 1) {
    for(i in 2:length(lx)) {
      graphics::points(x = lx[[i]][[time]],
                       y = lx[[i]][[y]],
                       col = colores[i],
                       type = type,
                       pch = 16,
                       ...)

    }
  }

  add_legend <- function(...) {
    opar <- graphics::par(fig=c(0, 1, 0, 1),
                          oma=c(0, 0, 0, 0),
                          mar=c(0, 0, 0, 0),
                          new=TRUE)
    on.exit(graphics::par(opar))
    plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
    graphics::legend(...)
  }

  add_legend("topright",
             legend = names(lx),
             pch=20,
             col = colores[1:length(lx)],
             horiz=TRUE,
             bty='n',
             cex=1)

  add_legend("bottomleft",
             legend = min(dt[[time]], na.rm = TRUE),
             horiz=TRUE,
             bty='n',
             cex=0.8)
  add_legend("bottomright",
             legend = max(dt[[time]], na.rm = TRUE),
             horiz=TRUE,
             bty='n',
             cex=0.8)

}
