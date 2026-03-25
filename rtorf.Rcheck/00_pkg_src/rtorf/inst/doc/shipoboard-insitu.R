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
# datasetid <- "shipboard-insitu"
# df <- obs_read_nc(index = index,
#                   categories = datasetid,
#                   solar_time = TRUE,
#                   verbose = TRUE)
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

## ----obs_plotsave, fig.width=5, fig.height=3, eval = F, echo = F, message=F, warning=F----
# png("../man/figures/obsplot_shipboardinsitu.png", width = 1500, height = 1000, res = 200)
# obs_plot(df, time = "timeUTC", yfactor = 1e9)
# dev.off()

## ----obs_plot, fig.width=5, fig.height=3, eval = F----------------------------
# obs_plot(df4, time = "timeUTC", yfactor = 1e9)

## ----savesf, fig.width=5, fig.height=3, eval = F, echo = F--------------------
# library(sf)
# dx <- df[,
#     lapply(.SD, mean),
#     .SDcols = "value",
#     by = .(latitude, longitude)]
# x <- st_as_sf(dx, coords = c("longitude", "latitude"), crs = 4326)
# png("../man/figures/obsplot_shipboardinsitu_map.png", width = 1500, height = 1000, res = 200)
# plot(x["value"], axes = T, reset = F)
# maps::map(add = T)
# dev.off()

## ----sf, fig.width=5, fig.height=3, eval = F----------------------------------
# library(sf)
# dx <- df[,
#     lapply(.SD, mean),
#     .SDcols = "value",
#     by = .(latitude, longitude)]
# x <- st_as_sf(dx, coords = c("longitude", "latitude"), crs = 4326)
# plot(x["value"], axes = T, reset = F)
# maps::map(add = T)

