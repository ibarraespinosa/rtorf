## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(rtorf)
library(data.table)
library(rslurm)


## ----eval = F-----------------------------------------------------------------
# setwd("/path/to/myfootprints/")

## ----eval = F-----------------------------------------------------------------
# x <- fread("receptor.csv")
# 

## ----eval = F-----------------------------------------------------------------
# 
# x$altitude  <- x$altitude_final
# 
# x[, id := obs_footname(year = year,
#                        month = month,
#                        day = day,
#                        hour = hour,
#                        minute = minute,
#                        lat = latitude,
#                        lon = longitude,
#                        alt = altitude)]

## ----eval = F-----------------------------------------------------------------
# x$id[1]
# [1] "2020x12x30x09x54x03.2133Nx030.9131Ex00497"

## ----eval = F-----------------------------------------------------------------
# x[, dir := paste0("/Path/To/Footprints/",
#                   year,
#                   "/",
#                   sprintf("%02d",
#                           month),
#                   "/tmp_",
#                   id)]
# 

## ----eval = F-----------------------------------------------------------------
# x$dir[1]
# [1] "/Path/To/Footprints/2020/12/tmp_2020x12x30x09x54x03.2133Nx030.9131Ex00497"

## ----eval = F-----------------------------------------------------------------
# invisible(sapply(x$dir, dir.create, recursive = T))

## ----eval = F-----------------------------------------------------------------
# x[, idn := 1:.N]
# 

## ----eval = F-----------------------------------------------------------------
# for(i in seq_along(x$dir)) {
#   obs_hysplit_setup(setup = paste0(x$dir[i], "/SETUP.CFG"))
# }

## ----eval = F-----------------------------------------------------------------
# for(i in seq_along(x$dir)) {
#   obs_hysplit_ascdata(ascdata = paste0(x$dir[i], "/ASCDATA.CFG"))
# }

## ----eval = F-----------------------------------------------------------------
# for(i in seq_along(x$dir)) {
#   # print(paste0(x$dir[i], "/CONTROL"))
#   obs_hysplit_control(df = x[i],
#                       top_model_domain = 10000,
#                       met = "gfs0p25",
#                       metpath = "/Path/To/metfiles/gfs0p25/",
#                       emissions_rate = 0,
#                       hour_emissions = 0.01,
#                       center_conc_grids = c(5, 45),
#                       grid_spacing = c(1, 1),
#                       grid_span = c(69, 69),
#                       height_vert_levels = 50,
#                       sampling_interval_type = c(0, 1, 0),
#                       control = paste0(x$dir[i], "/CONTROL"))
# 
# }
# 

## ----eval = F-----------------------------------------------------------------
# setorderv(x, "idn")

## ----eval = F-----------------------------------------------------------------
# x[, nc := paste0(dir,  "/hysplit", id, ".nc")]
# 
# x[, nc_exists := file.exists(paste0(dir,  "/hysplit", id, ".nc"))]

## ----eval = F-----------------------------------------------------------------
# x$nc[1]
# [1] "/Path/To/Footprints/2020/12/tmp_2020x12x30x09x54x03.2133Nx030.9131Ex00497/hysplit2020x12x30x09x54x03.2133Nx030.9131Ex00497.nc"

## ----eval = F-----------------------------------------------------------------
# x[,.N, by = nc_exists]
# 
# x <- x[nc_exists == FALSE]
# 
# if(nrow(x) == 0) stop("ALL FOOTPRINTS GENERATED")
# 

## ----eval = F-----------------------------------------------------------------
# fx <- function(dir, idn){
# 
#   torf <- "
#           *@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@&
#       %@@@@@.                                    %@@@@@
#     @@@@                          @                  ,@@@
#    @@@                       *@@@@                     %@@&
#   @@@             @@@@@@.@@@@@@@@@@@#                   @@@
#   @@@          @@@@%@@* @@@@@@@@@            /@@@@@(    ,@@,
#   @@@        @@@@@ @@@@@@@@@@@#       ,@(           *   ,@@,
#   @@@      @@@@@@% (@@@@@@@@@                           ,@@,
#   @@@    &@@@@@@@@  @@@@@@@@@, @@*          (@@@@@@@#   ,@@,
#   @@@   @@@@@@@@@@   @@@@@@@@@@@(                       ,@@,
#   @@@     @@@@@@@@@  @@@@@@@@@@@@@@@&                   ,@@,
#   @@@  @.  .@@@@@@,  @@@@@@@.                           ,@@,
#   @@@  @@@   @@@@@  %@@ @@@                             ,@@,
#   @@@  @@@@.       &   /                                ,@@,
#   @@@                                                   ,@@,
#   @@@                                                   ,@@,
#   @@@   @@@@@@@@@* @@@@@@@@@@   @@@@@@@@@@  @@@@@@@@@@  ,@@,
#   @@@      @@@    @@@      @@@@ @@@    @@@  @@@         ,@@,
#   @@@      @@@   #@@@       @@@ @@@@@@@@@.  @@@@@@@.    ,@@,
#   @@@      @@@    @@@@*   @@@@  @@@  @@@@   @@@         @@@
#    @@@#    @@@      (@@@@@@@    @@@    @@@. @@@        @@@
#      @@@@                                           .@@@@
#        %@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# 
#   "
# 
# 
#   setwd(dir)
# 
#   system("/Path/To/hysplit/exec/hycs_std")
# 
#   sink("log.txt")
#   cat("")
# 
#   cat(torf)
#   cat("\n\n")
# 
#   utils::sessionInfo()
#   cat("\n\n")
# 
#   cat("Receptor:\n")
#   cat("receptor.csv\n\n")
# 
#   cat("logs:\n")
#   cat("_rslurm_rtorf_job/slurm_*.out\n\n")
# 
#   sink()
# 
# 
#   rdir="/Path/To/rscripts/"
# 
#   system(
#     paste0('Rscript ',
#            rdir,
#            '/hysplit_netcdf.r ',
#            '--rsource=',
#            rdir,
#            ' --gridspecs=',
#            rdir,
#            '/gridspecs_uganda.txt',
#            ' --plotfoot',
#            ' --footnearfield',
#            ' --thinpart',
#            ' --outpath=',
#            dir,
#            '/'))
# 
# }
# 
# 

## ----eval = F-----------------------------------------------------------------
# sjob <- slurm_apply(fx,
#                     x[, c("dir", "idn")],
#                     jobname = 'rtorf_job',
#                     nodes = 8,
#                     cpus_per_node = 4,
#                     submit = T)
# 

## ----eval = F-----------------------------------------------------------------
# file.edit("_rslurm_rtorf_job/submit.sh")
# #!/bin/bash
# #
# #SBATCH --array=0-7
# #SBATCH --cpus-per-task=4
# #SBATCH --job-name=rtorf_job
# #SBATCH --output=slurm_%a.out
# /Path/To/R/bin/Rscript --vanilla slurm_run.R

