#' @title Summary of the ObsPack files (.txt)
#'
#' @description
#' This function returns a data.frame index of all files
#' available in ObsPack.
#'
#' @family obs_summary
#' @param obs Path to the Obspack GLOBALview txt data
#' @param categories character; default are c("aircraft-pfp", "aircraft-insitu",
#' "surface-insitu", "tower-insitu", "aircore", "surface-pfp", "shipboard-insitu",
#' "flask"). The idea is that,as the file names include these words,
#' this function identifies which files has these words and add them as columns.
#' @param lnchar Integer; last nchar, default = 11.
#' @param out Path to the ObsPack index output
#' @param verbose Logical to show more information
#' @param aslist Logical to return list of index and summary
#' @return A data.frame with with an index of the obspack Globalview.
#' @importFrom data.table fwrite ".N" ":=" setDT data.table
#' @export
#' @examples \dontrun{
#' # Do not run
#' obs <- system.file("data-raw", package = "rtorf")
#' index <- obs_summary(obs)
#' }
obs_summary <- function(
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
  verbose = TRUE,
  aslist = FALSE
) {
  x <- list.files(obs, full.names = T)

  na <- list.files(obs, full.names = F)

  # create index ####
  # the idea
  index <- data.table::data.table(id = x, name = na)
  data.table::setorderv(index, "name")

  index$n <- 1:nrow(index)
  sector <- NULL
  for (i in seq_along(categories)) {
    index[grepl(pattern = categories[i], x = index$na), sector := categories[i]]
  }

  cat(paste0("Number of files of index: ", nrow(index), "\n"))
  xx <- index[, .N, by = sector]
  dx <- data.table::data.table(sector = c("Total sectors"), N = sum(xx$N))
  if (verbose) {
    print(rbind(xx, dx))
  }

  # detecting file extension
  # using tools::file_ext function instead of import
  fiex <- fex(index$id[1])

  idfiex <- ifelse(fiex == "nc", 10, 11)
  # add the last 11 characters of each name file'
  agl <- NULL
  id <- NULL
  index[grepl(pattern = "magl", x = x), agl := sr(id, idfiex)]

  # This line replaces removes the characters magl.txt
  # for instance, remove "magl.txt" from -11magl.txt
  index$agl <- gsub(paste0("magl.", fiex), "", index$agl)
  # assuming d-{number}magl.txt
  index$agl <- gsub("d", "", index$agl)

  # check
  index
  # Now we transform the column
  # then get the absolute number and now we have magl
  index$agl <- suppressWarnings(abs(as.numeric(index$agl)))

  if (!missing(out)) {
    data.table::fwrite(index, out)

    if (verbose) cat("index in: ", out, "\n")
  }
  # identifying agl when reading the data instead of name file
  # 2023/09/13
  if (verbose) {
    yai <- !is.na(index$agl)
    nai <- is.na(index$agl)
    cat(paste0("Detected ", sum(yai, na.rm = T), " files with agl\n"))
    cat(paste0("Detected ", sum(nai, na.rm = T), " files without agl\n"))
  }

  return(index)
}
