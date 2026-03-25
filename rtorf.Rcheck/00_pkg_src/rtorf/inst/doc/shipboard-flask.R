## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rtorf)
library(data.table)

## ----read1, eval = F----------------------------------------------------------
# 
# cate = c("aircraft-pfp",
#          "aircraft-insitu",
#          "aircraft-flask",
#          "surface-insitu",
#          "surface-flask",
#          "surface-pfp",
#          "tower-insitu",
#          "aircore",
#          "shipboard-insitu",
#          "shipboard-flask")
# 
# obs <- "Z:/obspack/obspack_ch4_1_GLOBALVIEWplus_v5.1_2023-03-08/data/nc/"
# index <- obs_summary(obs = obs,
#                      categories = cate)

## ----readnc, eval = F---------------------------------------------------------
# datasetid <- "surface-flask"
# df <- obs_read_nc(index = index,
#                   categories = datasetid,
#                   solar_time = TRUE,
#                   verbose = TRUE)
# 

## ----checkdf, eval = F--------------------------------------------------------
# df

## ----spatial, eval = F--------------------------------------------------------
# yy <- 2020
# evening <- 14
# 

## ----checkcols, eval = F------------------------------------------------------
# df[, c("altitude", "altitude_final", "intake_height", "elevation",
#        "dataset_selection_tag",
#               "site_name")]

## ----range_year, eval = F-----------------------------------------------------
# range(df$year)

## ----dim_df, eval = F---------------------------------------------------------
# dim(df)

## ----spatial_temporal_filter, eval = F----------------------------------------
# df <- df[year == yy]
# dim(df)

## ----eval = F-----------------------------------------------------------------
# dfa <- df[,
#           max(altitude_final),
#           by = site_code] |> unique()
# 
# names(dfa)[2] <- "max_altitude"
# dfa

## ----eval = F-----------------------------------------------------------------
# df2 <- obs_addtime(df)

## ----eval = F-----------------------------------------------------------------
# df2$solar_time <- obs_addstime(df2)
# 

## ----eval = F-----------------------------------------------------------------
# df2$solar_time_cut <- cut(x = df2$solar_time,
#                           breaks = "1 hour") |>
#   as.character()
# 

## ----eval = F-----------------------------------------------------------------
# df3 <- df2
# df3[, c("solar_time", "solar_time_cut")]
# 
# 

## ----eval = F-----------------------------------------------------------------
# df3$key_time <- df3$solar_time_cut

## ----eval = F-----------------------------------------------------------------
# df4 <- obs_agg(dt = df3,
#                cols = c("value",
#                         "latitude",
#                         "longitude",
#                         "site_utc2lst"),
#                verbose = T,
#                byalt = TRUE)

## ----eval = F-----------------------------------------------------------------
# df4[,
#     max_altitude := max(altitude_final),
#     by = site_code]
# df4[,
#     c("site_code",
#       "altitude_final",
#       "max_altitude")] |> unique()
# 

## ----rename, eval = F---------------------------------------------------------
# master <- df4
# 

## ----round, eval = F----------------------------------------------------------
# master$timeUTC <- as.character(master$timeUTC)
# master$local_time <- as.character(master$local_time)
# master$latitude <- round(master$latitude, 4)
# master$longitude <- round(master$longitude, 4)
# 

## ----outfile, eval = F--------------------------------------------------------
# out <- tempfile()
# 

## ----outtxt, eval = F---------------------------------------------------------
# message(paste0(out,"_", datasetid, ".txt\n"))
# fwrite(master,
#        paste0(out,"_", datasetid, ".txt"),
#        sep = " ")

## ----outcsv, eval = F---------------------------------------------------------
# message(paste0(out,"_", datasetid, ".csv\n"))
# fwrite(master,
#        paste0(out,"_", datasetid, ".csv"),
#        sep = ",")

## ----csvy, eval = F-----------------------------------------------------------
# cat("\nAdding notes in csvy:\n")
# notes <- c(paste0("sector: ", datasetid),
#            paste0("timespan: ", yy),
#            paste0("hours: ", evening),
#            "local_time: used solar_time")
# 
# cat(notes, sep = "\n")
# 
# message(paste0(out,"_", datasetid, ".csvy\n"))
# obs_write_csvy(dt = master,
#                notes = notes,
#                out = paste0(out,"_", datasetid, ".csvy"))

## ----readcsvy, eval = F-------------------------------------------------------
# obs_read_csvy(paste0(out,"_", datasetid, ".csvy"))

## ----receptors, eval = F------------------------------------------------------
# receptor <- master[, c("site_code",
#                      "year",
#                      "month",
#                      "day",
#                      "hour",
#                      "minute",
#                      "second",
#                      "latitude",
#                      "longitude",
#                      "altitude_final",
#                      "type_altitude",
#                      "time_decimal")]
# 

## ----round_alt, eval = F------------------------------------------------------
# receptor$altitude_final <- round(receptor$altitude_final)

## ----formatrec, eval = F------------------------------------------------------
# receptor <- obs_format(receptor,
#                         spf =  c("month", "day",
#                                  "hour", "minute", "second"))

## ----aslagl, eval = F---------------------------------------------------------
# receptor_agl <- receptor[type_altitude == 0]
# receptor_asl <- receptor[type_altitude == 1]

## ----save_receptors, eval = F-------------------------------------------------
# if(nrow(receptor_agl) > 0) {
#   message(paste0(out, "_", datasetid, "_receptor_AGL.txt"), "\n")
# 
#   fwrite(x = receptor_agl,
#          file = paste0(out, "_", datasetid, "_receptor_AGL.txt"),
#          sep = " ")
# }
# 
# if(nrow(receptor_asl) > 0) {
#   message(paste0(out, "_", datasetid, "_receptor_ASL.txt"), "\n")
# 
#   fwrite(x = receptor_asl,
#          file = paste0(out, "_", datasetid, "receptor_ASL.txt"),
#          sep = " ")
# 
# }

## ----obs_plotsave, fig.width=5, fig.height=3, eval = F, echo = F, message=F, warning=F----
# png("../man/figures/obsplot_shipboardflask.png", width = 1500, height = 1000, res = 200)
# obs_plot(df3, time = "timeUTC", yfactor = 1e9)
# dev.off()

## ----obs_plot, fig.width=5, fig.height=3, eval = F----------------------------
# obs_plot(df4, time = "timeUTC", yfactor = 1e9)

## ----savesf, fig.width=5, fig.height=3, eval = F, echo = F--------------------
# library(sf)
# dx <- df4[,
#     lapply(.SD, mean),
#     .SDcols = "value",
#     by = .(latitude, longitude)]
# x <- st_as_sf(dx, coords = c("longitude", "latitude"), crs = 4326)
# png("../man/figures/obsplot_shipboardflask_map.png", width = 1500, height = 1000, res = 200)
# plot(x["value"], axes = T, reset = F)
# maps::map(add = T)
# dev.off()

## ----sf, fig.width=5, fig.height=3, eval = F----------------------------------
# library(sf)
# dx <- df4[,
#     lapply(.SD, mean),
#     .SDcols = "value",
#     by = .(latitude, longitude)]
# x <- st_as_sf(dx, coords = c("longitude", "latitude"), crs = 4326)
# plot(x["value"], axes = T, reset = F)
# maps::map(add = T)

