# NEWS

### rtorf 3.3.0 (Release date: 2026-02-013)

Fix documentation for CRAN.
Add argument to flip vertically NetCDF in obs_convolve

### rtorf 3.2.4 (Release date: 2026-02-01)

Fix documentation for CRAN.

### rtorf 3.2.3 (Release date: 2026-01-16)

Prepare for CRAN.
Update DESCRIPTION

### rtorf 3.2.2 (Release date: 2026-01-02)

fix Removes default units from obs_nc to force user to add them  manually.

### rtorf 3.2.1 (Release date: 2025-12-11)

fix obs_select_sec

### rtorf 3.2.0 (Release date: 2025-12-01)

Add obs_foot to read, process footprints parallel

### rtorf 3.1.0 (Release date: 2025-11-11)

fix obs_hysplit_control. It was nor wokring for the last day of the month
Added verbose argument.

### rtorf 3.0.0 (Release date: 2025-11-03)

Add second argument in obs_footname. Deprecated obs_format, since it formatted in obs_hysplit_control Deleted a bunch of legacy code
Check code for green flag R CMD check

### rtorf 2.9.0 (Release date: 2025-11-03)

Add second argument in obs_footname. Deprecated obs_format, since it formatted in obs_hysplit_control Deleted a bunch of legacy code

### rtorf 2.8.2 (Release date: 2025-10-17)

Add message to show what I'm comparing in obs_convolve Add argument name_var_flux to obs_convolve

### rtorf 2.8.1 (Release date: 2025-10-15)

Remove need for columns site_code or altitude_final in obs_agg User may need to reorder data

### rtorf 2.8.0 (Release date: 2025-10-15)

add obs_select_sec to select based on closest time, rather than agg add obs_add_mtime to add matlab time

### rtorf 2.7.0 (Release date: 2025-09-25)

add normalize mass, grid and tracj

### rtorf 2.6.0 (Release date: 2025-09-25)

add obs_normalize_dmass remove make_nc, npot needed at the moment. Focus on tracj

### rtorf 2.5.0 (Release date: 2025-09-24)

Adding more legacy code. Add note about availability of files

### rtorf 2.4.0 (not released)

Adding more legacy code. Add note about availability of files

### rtorf 2.3.1 (Release date: 2025-09-12)

-   Expand obs_convolve and fix tz, now it is explicit tz = "UTC"

### rtorf 2.3.0 (Release date: 2025-09-09)

-   Add legacy time functions. Many of themm will be removed or replaced with lubridate. The idea is to preserve and improve only the functions to generate NetCDF from PARTICLE.DAT

### rtorf 2.2.0 (Release date: 2025-09-08)

-   Remove former classes. classes may be added in the futre for complex objects, such as data.tables from read from obspack or receptor-ready files.

### rtorf 2.1.0 (Release date: 2025-08-28)

-   Add legacy code obs_julian

### rtorf 2.0.1 (Release date: 2025-07-24)

-   obs_convolve also returns foot

### rtorf 2.0.0 (Release date: 2025-07-16)

-   Big release, add support to read, write NetCDF and convolve footprints with fluxes.

### rtorf 1.7.0 (Release date: 2025-07-15)

-   Add obs_nc to read and create NetCDF.
-   add text (not done yet): obs_convolve

### rtorf 1.6.1 (Release date: 2025-06-13)

-   metformat for obs_hysplit_control is argument.

### rtorf 1.6.0 (Release date: 2025-06-04)

-   Add support for High-Resolution Rapid Refresh (HRRR)daily files. ( all met files must be daily). changed expected ERA5 file: ERA5\_%Y%m%d.ARL

### rtorf 1.5.2 (Release date: 2025-06-02)

-   check for names in df for obs_hysplit_control
-   Add option to return time columns in obs_add_time and obs_add_ltime

### rtorf 1.5.1 (Release date: 2025-05-28)

-   fix obs_hysplit_control when use time params instead of df. I needed to declare agl

### rtorf 1.5.0 (Release date: 2025-05-20)

-   Change way obs_hysplit_setup writes. Now instead of cat, uses write. obs_hysplit_ascdata and obs_hysplit_control still use cat.

### rtorf 1.4.0 (Release date: 2025-04-28)

-   Adding era5 to obs_hysplit_control

### rtorf 1.3.1 (Release date: 2025-04-09)

-   Adding documentation for vertical_motion in obs_hysplit_control default 5

### rtorf 1.3.0 (Release date: 2025-04-01)

-   Adding capacity to read co2
-   Fix obs_read_nc now add variables with same length of dt

### rtorf 1.2.1 (Release date: 2025-03-21)

-   Update email

### rtorf 1.2.0 (Release date: 2025-03-12)

-   Add argument solar_time solar_time = if(grepl("aircraft", categories)) FALSE else TRUE
-   Update vignettes and add one aobut parallel hysplit
-   Change name Obspoack to Observations

### rtorf 1.1.0 (Release date: 2025-01-31)

Add functions to configure hysplit

-   obs_hysplit_control
-   obs_hysplit_setup
-   obs_hysplit_ascdata

### rtorf 1.0.3 (Release date: 2024-11-13)

-   Add obs_read_nc_att to read global attributes

### rtorf 1.0.2 (Release date: 2024-10-31)

-   Add stop when missing arguments in obs_info2id

### rtorf 1.0.1 (Release date: 2024-10-07)

-   Fix obs_footname when inout time components instead of POSICxt.

### rtorf 1.0.0 (Release date: 2024-09-23)

-   Started adding some legacy code. This means that rtorf can be upgraded to 1.0.0 and adding newer legacy code will be 1.0.x.

### rtorf 0.9.5 (Release date: 2024-08-26)

-   Add articles for each dataid, running examples in `examples/`, articles in `vignettes`

### rtorf 0.9.4 (Release date: 2024-08-19)

-   remove `cols` argument to obs_read_nc to avoid reading all columns by default
-   add `att` logical argument to add global attributes from NetCDF or not to data.table. Default is `F`
-   Started adding vignettes/articles by dataid

### rtorf 0.9.3 (Release date: 2024-07-09)

-   Add cols argument to obs_read_nc to avoid reading all columns by default

### rtorf 0.9.2 (Release date: 2024-04-23)

-   reformatted obs_table to read df from obs_read
-   obs_read now read intake_height, site_longitude and site_latitude

obs_addstime return a POSIXct vector, (fix #6 )

### rtorf 0.9.1 (Release date: 2024-03-05)

-   order summary index by name (obs_summary)

### rtorf 0.9.0 (Release date: 2024-03-05)

-   Rename robspack to rtorf.
-   Prepare to submit to JOSS and https://github.com/noaa-gml.

### robspack 0.8.1 (Release date: 2023-11-02)

-   Adding solar time in NetCDF.

### robspack 0.8.0 (Release date: 2023-09-13)

-   Adding support for NetCDF.
-   obs_index detect file extension

### robspack 0.7.0 (Release date: 2023-04-18)

-   rename package to `robspack`
-   rename obs_write to obs_write_csvy
-   add rscripts in GitHub repo

### robspackfilter 0.6.0 (Release date: 2023-04-04)

-   add obs_table, to summarize ObsPack

### robspackfilter 0.5.1 (Release date: 2023-03-22)

-   add obs_format to format data
-   add obs_plot
-   Change method to plot invfiles, based on obs_plot
-   Rename obs_index to obs_summary

### robspackfilter 0.4.1 (Release date: 2023-02-01)

-   Deprecate some functions

### robspackfilter 0.4.0 (Release date: 2023-01-19)

-   add obs_meta, which creates a data.table based on metadata

### robspackfilter 0.3.4 (Release date: 2022-10-03)

-   Reverse obs_footname, from obs_trunc to sprintf
-   add round (x, 4) into lat lon obs_footname to match receptors.
-   Adding institution and Scale into obs_invfiles

### robspackfilter 0.3.3 (Release date: 2022-08-29)

-   add by lab_1_abbr, dataset_calibration_scale into obs_add, assuming site_code matches scale and lab_1

### robspackfilter 0.3.3 (Release date: 2022-08-29)

-   add obs_footname
-   add obs_invfiles.
-   add dataset_calibration_scale into obs_read

### robspackfilter 0.3.1 (Release date: 2022-08-25)

-   Prepare submission to JOSS
-   obs_index now does not write temporal file for index, when out argument is missing.

### robspackfilter 0.3.0 (Release date: 2022-08-11)

-   add function obs_trunc and used inside obs_find_receptors

### robspackfilter 0.2.2 (Release date: 2022-08-01)

-   rename write_meta to obs_write
-   obs_find_receptors: change\
    sprintf(lon, fmt = '%7.4f') to sprintf(lon, fmt = '%07.4f') to formatC(lon, digits = 4, width = 8, format = "f", flag = "0")

### robspackfilter 0.2.1 (Release date: 2022-07-19)

-   Add obs_find_receptors

### robspackfilter 0.2.0 (Release date: 2022-07-14)

-   Add write_meta which generates a CSVY file

### robspackfilter 0.1.4 (Release date: 2022-07-12)

-   add obs_addltime to return a data.table with local time columns

### robspackfilter 0.1.3 (Release date: 2022-07-06)

-   changed all functions to obs\_\*
-   add obs_agg to aggregate columns by time interval
-   imported lubridate to calculate decimal date
-   obs_agg also add some character vars

### robspackfilter 0.1.2 (Release date: 2022-07-04)

-   add addtime_obs into read_obs
-   add option into addtime_obs to find intervals time data into desired frequency eg. 20, using coumn 20
-   Add name of file as variable in read_obs
-   add option in addtime_obs to aggregate numeric by mean

### robspackfilter 0.1.1 (Release date: 2022-06-29)

-   add variable n into index and read_obs
-   add verbose to avoid mistakes in read_obs
-   add add_zero, c(1, 10) -\> c("01", "10")
-   addtime_obs add column dif_time (end - start)

### robspackfilter 0.1.0 (Release date: 2022-06-24)

-   Create robspackfilter