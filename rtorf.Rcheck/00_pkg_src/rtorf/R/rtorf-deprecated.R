## rtorf-deprecated.r
#' @title Deprecated functions in package \pkg{rtorf}.
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned. Help pages for deprecated functions are
#'   available at \code{help("-deprecated")}.
#' @name rtorf-deprecated
#' @keywords internal
NULL


#' @title Index of the ObsPack files (Deprecated)
#'
#' @description
#' This function returns a data.frame index of all files
#' available in obspack.
#'
#' @param obs Path to the Obspack GLOBALview txt data
#' @param categories character; default are c("aircraft-pfp", "aircraft-insitu",
#' "surface-insitu", "tower-insitu", "aircore", "surface-pfp", "shipboard-insitu",
#' "flask"). The ideia is that,as the file names include these words,
#' this function identifies which files has these words and add them as columns.
#' @param lnchar Integer; last nchar, default = 11.
#' @param out Path to the Obspack index output
#' @param verbose Logical to show more information
#' @return A data.frame with with an index of the obspack Globalview.
#' @rdname obs_summary
#' @importFrom data.table fwrite ".N" ":=" setDT data.table
#' @export
#' @examples {
#' \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' }
#' }
obs_index <- function(
  obs,
  categories = c(
    "aircraft-pfp",
    "aircraft-insitu",
    "surface-insitu",
    "tower-insitu",
    "aircore",
    "surface-pfp",
    "shipboard-insitu",
    "flask"
  ),
  lnchar = 11,
  out = paste0(tempfile(), "_index.csv"),
  verbose = TRUE
) {
  # cat("Please, use `obs_summary` instead")
  #
  #
  # x <- list.files(obs,
  #                 full.names = T)
  #
  # na <- list.files(obs,
  #                  full.names = F)
  #
  # # create index ####
  # # the idea
  # index <- data.table::data.table(id = x, name = na)
  # index$n <- 1:nrow(index)
  # sector <- NULL
  # for(i in seq_along(categories)) {
  #   index[grepl(pattern = categories[i], x = index$na),
  #         sector := categories[i]]
  # }
  #
  #
  # cat(paste0("\nNumber of files of index: ", nrow(index), "\n"))
  # xx <- index[, .N, by = sector]
  # dx <- data.table::data.table(sector = c("Total sectors"),
  #                              N = sum(xx$N))
  # if(verbose) print(rbind(xx, dx))
  #
  #
  # # identifying agl when reading the data instead of name file
  # # 2023/09/13
  # #
  # # function to extract last n characters
  # sr <- function(x, n){
  #   substr(x, nchar(x)-n+1, nchar(x))
  # }
  #
  # # add the last 11 characters of each name file'
  # agl <- NULL
  # id <- NULL
  # index[grepl(pattern = "magl", x = x),
  #       agl := sr(id, 11)]
  #
  # # This line replaces removes the characters magl.txt
  # # for instance, remove "magl.txt" from -11magl.txt
  # index$agl <- gsub("magl.txt", "", index$agl)
  #
  # # check
  # index
  # # Now we transform the column
  # # then get the absolute number and now we have magl
  # index$agl <- suppressWarnings(abs(as.numeric(index$agl)))
  #
  # if(!missing(out)) {
  #   data.table::fwrite(index, out)
  #
  #   if(verbose) cat("index in: ", out, "\n")
  #
  # }
  # # identifying agl when reading the data instead of name file
  # # 2023/09/13
  # if(verbose) {
  #   yai <- !is.na(index$agl)
  #   nai <- is.na(index$agl)
  #   cat(paste0("Detected ", sum(yai, na.rm = T), " files with agl\n"))
  #   cat(paste0("Detected ", sum(nai, na.rm = T), " files without agl\n"))
  # }
  #
  # return(index)
  .Deprecated("obs_summary")
  "obs_index"
}

#' @title Number to character
#' @family helpers
#' @description transform integer to character and add zero at left
#' @param x integer
#' @return character
#' @rdname rtorf-deprecated
#' @seealso \code{\link{rtorf-deprecated}}
#' @export
#' @examples {\dontrun{
#' #do not run
#' }
#' }
obs_addzero <- function(x, ...) {
  #
  #   # if(!inherits(x, "integer")) stop("x must be integer")
  #
  #   dd <- as.character(x)
  #   nch <- nchar(dd)
  #   nchmax <- max(nchar(dd))
  #
  #   difch <- nchmax - nch
  #
  #   if(length(unique(nch)) == 1 ) {
  #     if (unique(nch) == 1) {
  #       message("Character has same length. Adding one zero.")
  #       y <- paste0("0", x)
  #     } else {
  #       y <- x
  #     }
  #
  #   } else {
  #     unlist(lapply(seq_along(difch), function(i) {
  #       paste(rep("0", difch[i]), collapse = "")
  #     })) -> zero
  #
  #     y <- paste0(zero, x)
  #
  #   }
  #
  #
  #   if(!missing(char)) {
  #     y <- paste0(char, y)
  #   }
  #
  #   return(y)
  # }
  .Deprecated("sprintf")
  "obs_addzero"
}


#' @title Formatting data
#' @family helpers
#' @name obs_format
#' @export
#' @return formatted characters
#' @examples \dontrun{
#' # do not run
#' }
obs_format <- function() {
  #   dt,
  #   spf = c(
  #     "month",
  #     "day",
  #     "hour",
  #     "minute",
  #     "second",
  #     "month_end",
  #     "day_end",
  #     "hour_end",
  #     "minute_end",
  #     "second_end"
  #   ),
  #   spffmt = "%02d",
  #   rnd = c("latitude", "longitude"),
  #   rndn = 4,
  #   spfrnd = TRUE,
  #   out,
  #   ...
  # ) {
  #   for (i in seq_along(spf)) {
  #     dt[[spf[i]]] <- sprintf(fmt = spffmt, as.numeric(dt[[spf[i]]]))
  #   }
  #   for (i in seq_along(rnd)) {
  #     dt[[rnd[i]]] <- round(dt[[rnd[i]]], rndn)
  #   }
  #   if (spfrnd) {
  #     for (i in seq_along(rnd)) {
  #       dt[[rnd[i]]] <- sprintf(fmt = "%2.4f", dt[[rnd[i]]])
  #     }
  #   }
  #   if (!missing(out)) {
  #     data.table::fwrite(x = dt, x = out, ...)
  #   }
  #   return(dt)
  .Deprecated("obs_hysplit_control")
  "obs_format"
}
