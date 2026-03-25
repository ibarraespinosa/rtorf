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
# 

## ----check, eval = F----------------------------------------------------------
# index

