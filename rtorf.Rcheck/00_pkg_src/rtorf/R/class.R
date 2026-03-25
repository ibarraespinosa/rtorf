#' #' Methods for objects with class `invfile`
#' #'
#' #'
#' #' @return Objects of class "invfile"
#' #'
#' #' @param x Object with class "invfile"
#' #' @param ... plot arguments
#' #' @aliases invfile
#' #' @examples \dontrun{
#' #' #do not run
#' #' }
#'
#'
#' #' @rdname invfile
#' #' @method print invfile
#' #' @export
#' print.invfile <- function(x, ...) {
#'   cat("footprints_hera_hysplit: ",
#'       length(x$footprints_hera_hysplit),"Observations\n:")
#'   print(x$footprints_hera_hysplit[1:5])
#'   print("...")
#'
#'
#'   cat("obs_hysplit:",
#'       length(x$obs_hysplit),"Observations\n:")
#'   print(x$obs_hysplit[1:5])
#'   print("...")
#'
#'   cat("receptor_info_hysplit:",
#'       nrow(x$receptor_info_hysplit),"Observations\n:")
#'   print(str(x$receptor_info_hysplit))
#'
#' }
#'
#'
#'
#' #' @rdname invfile
#' #' @param object invfile
#' #' @method summary invfile
#' #' @export
#' summary.invfile <- function(object, ...) {
#'
#'   cat("footprints_hera_hysplit\n:")
#'   cat(length(object[[1]]), "files\n:")
#'   cat(object[[1]][1], "\n...\n", object[[1]][length(object[[1]])], "\n")
#'
#'
#'   cat("obs_hysplit\n:")
#'   print(summary(object[[2]]))
#'
#'   cat("receptor_info_hysplit\n:")
#'   print(lapply(object[[3]],  summary))
#'
#' }
#'
#' #' @rdname invfile
#' #' @param time x axis column (time)
#' #' @method plot invfile
#' #' @importFrom cptcity cpt find_cpt
#' #' @importFrom graphics par lines
#' #' @export
#' plot.invfile <- function(x,
#'                          time = "timeUTC",
#'                          ...) {
#'
#'   # oldpar <- graphics::par(no.readonly = TRUE)       # code line i
#'   # on.exit(graphics::par(oldpar))                    # code line i + 1
#'
#'   master <- x$master
#'
#'   if(is.numeric(x$obs_hysplit)) {
#'     value = x$obs_hysplit
#'   } else {
#'     value = x$obs_hysplit[[1]]
#'   }
#'
#'   obs_plot(dt = master,
#'            time = time,
#'            y = value,
#'            ...)
#'
#'   # value <- NULL
#'   # nmiss <- nrow(master[is.na(value)])
#'   #
#'   # if(nmiss > 0) cat("Found ", nmiss, "missing `value` on master\n")
#'   #
#'   # master <- master[!is.na(value)]
#'   #
#'   # lx <- split(master, master$site_code)
#'   #
#'   # cols <- cptcity::cpt(cptcity::find_cpt("qual")[6],
#'   #                      n = length(lx))
#'   #
#'   # par(mar = c(5, 4, 1.4, 0.2))
#'   #
#'   # plot(x = lx[[1]]$timeUTC,
#'   #      y = lx[[1]]$value,
#'   #      col = cols[1],
#'   #      xlim = range(master$timeUTC, na.rm = TRUE),
#'   #      ylim = range(master$value, na.rm = TRUE),
#'   #      pch = 16,
#'   #      xlab = "time",
#'   #      ylab = "value")
#'   #
#'   #
#'   # if(length(lx) > 1) {
#'   #
#'   #   for(i in 2:length(lx)) {
#'   #     graphics::points(x = lx[[i]]$timeUTC,
#'   #                      y = lx[[i]]$value,
#'   #                      col = cols[i],
#'   #                      pch = 16)
#'   #
#'   #   }
#'   # }
#'   #
#'   # add_legend <- function(...) {
#'   #   opar <- graphics::par(fig=c(0, 1, 0, 1),
#'   #                         oma=c(0, 0, 0, 0),
#'   #                         mar=c(0, 0, 0, 0),
#'   #                         new=TRUE)
#'   #   on.exit(graphics::par(opar))
#'   #   plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
#'   #   graphics::legend(...)
#'   # }
#'   #
#'   # add_legend("topright",
#'   #            legend = names(lx),
#'   #            pch=20,
#'   #            col = cols[1:length(lx)],
#'   #            horiz=TRUE,
#'   #            bty='n',
#'   #            cex=0.8)
#'   #
#'   # add_legend("bottomleft",
#'   #            legend = min(master$timeUTC, na.rm = TRUE),
#'   #            horiz=TRUE,
#'   #            bty='n',
#'   #            cex=0.8)
#'   # add_legend("bottomright",
#'   #            legend = max(master$timeUTC, na.rm = TRUE),
#'   #            horiz=TRUE,
#'   #            bty='n',
#'   #            cex=0.8)
#'   #
#'
#' }
