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
# datasetid <- "aircraft-insitu"
# df <- obs_read_nc(index = index,
#                   categories = datasetid,
#                   solar_time = FALSE,
#                   verbose = T)
# 

## ----checkdf, eval = F--------------------------------------------------------
# df

## ----spatial, eval = F--------------------------------------------------------
# north <- 80
# south <- 10
# west <- -170
# east <- -50
# max_altitude <- 8000
# yy <- 2020
# 

## ----checkcols, eval = F------------------------------------------------------
# df[, c("altitude", "altitude_final", "intake_height", "elevation")]

## ----range_year, eval = F-----------------------------------------------------
# range(df$year)

## ----dim_df, eval = F---------------------------------------------------------
# dim(df)

## ----spatial_temporal_filter, eval = F----------------------------------------
# df <- df[year == yy]
# 
# df <- df[altitude_final < max_altitude &
#            latitude < north &
#            latitude > south &
#            longitude < east &
#            longitude > west]
# dim(df)

## ----add_time1, eval = F------------------------------------------------------
# df <- obs_addtime(df)
# df[, "timeUTC"]

## ----cut_sec, eval = F--------------------------------------------------------
# df$sec2 <- obs_freq(x = df$second,
#                      freq = seq(0, 59, 20))
# df[, c("second", "sec2")]

## ----key_time, eval = F-------------------------------------------------------
# df$key_time <- ISOdatetime(year = df$year,
#                            month = df$month,
#                            day = df$day,
#                            hour = df$hour,
#                            min = df$minute,
#                            sec = df$sec2,
#                            tz = "UTC")
# df[, c("timeUTC", "key_time")]
# 

## ----aggregatingdata, eval = F------------------------------------------------
# df2 <- obs_agg(df, cols =  c("year",
#                              "month",
#                              "day",
#                              "hour",
#                              "minute",
#                              "second",
#                              "time",
#                              "time_decimal",
#                              "value",
#                              "latitude",
#                              "longitude",
#                              "altitude_final",
#                              "elevation",
#                              "intake_height",
#                              "gps_altitude",
#                              "pressure",
#                              "pressure_altitude",
#                              "u", "v", "temperature",
#                              "type_altitude"))

## ----addlt, eval = F----------------------------------------------------------
# df3 <- obs_addltime(df2)
# setorderv(df3, cols = c("site_code", "timeUTC"),
#           order = c(-1, 1))
# df3
# 
# 

## ----rename, eval = F---------------------------------------------------------
# master <- df3
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
# 

## ----outcsv, eval = F---------------------------------------------------------
# message(paste0(out,"_", datasetid, ".csv\n"))
# fwrite(master,
#        paste0(out,"_", datasetid, ".csv"),
#        sep = ",")
# 

## ----csvy, eval = F-----------------------------------------------------------
# cat("\nAdding notes in csvy:\n")
# notes <- c(paste0("sector: ", datasetid),
#            paste0("timespan: ", yy),
#            paste0("spatial_limits: north = ", north, ", south = ", south, ", east = ", east, ", west = ", west),
#            "data: Data averaged every 20 seconds",
#            paste0("altitude: < ", max_altitude),
#            "hours: All",
#            "local_time: if var `site_utc2lst` is not available, calculated as",
#            "longitude/15*60*60 (John Miller)")
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
#                        "year",
#                        "month",
#                        "day",
#                        "hour",
#                        "minute",
#                        "second",
#                        "latitude",
#                        "longitude",
#                        "altitude_final",
#                        "type_altitude")]
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
# 
# 
# 

## ----obs_plotsave, fig.width=5, fig.height=3, eval = F, echo = F, message=F, warning=F----
# png("../man/figures/obsplot_aircraftinsitu.png", width = 1500, height = 1000, res = 200)
# obs_plot(df3, time = "timeUTC", yfactor = 1e9)
# dev.off()

## ----obs_plot, fig.width=5, fig.height=3, eval = F----------------------------
# obs_plot(df3, time = "timeUTC", yfactor = 1e9)

## ----savesf, fig.width=5, fig.height=3, eval = F, echo = F--------------------
# library(sf)
# x <- st_as_sf(df3, coords = c("longitude", "latitude"), crs = 4326)
# png("../man/figures/obsplot_aircraftinsitu_map.png", width = 1500, height = 1000, res = 200)
# plot(x["value"], axes = T, reset = F)
# maps::map(add = T)
# dev.off()

## ----sf, fig.width=5, fig.height=3, eval = F----------------------------------
# library(sf)
# x <- st_as_sf(df3, coords = c("longitude", "latitude"), crs = 4326)
# plot(x["value"], axes = T, reset = F)
# maps::map(add = T)

## ----vertplot, fig.width=5, fig.height=3, eval = F, echo = F, message=F, warning=F----
# x <- df3
# x$ch4 <- x$value*1e+9
# png("../man/figures/obsplot_aircraftinsitu_vert.png", width = 1500, height = 1000, res = 200)
# obs_plot(x,
#          time = "ch4",
#          y = "altitude_final",
#          colu = "month", n = c(1L, 3L, 6L, 8L, 9L, 11L, 12L),
#          type = "b",
#          xlab = expression(CH[4]~ppb),
#          ylab = "altitude (m)")
# dev.off()

## ----obs_plot_vertt, fig.width=7, fig.height=5, eval = F----------------------
# x <- df3
# x$ch4 <- x$value*1e+9
# obs_plot(x,
#          time = "ch4",
#          y = "altitude_final",
#          colu = "month", #n = c(1L, 3L, 6L, 8L, 9L, 11L, 12L),
#          type = "b",
#          xlab = expression(CH[4]~ppb),
#          ylab = "altitude (m)")

