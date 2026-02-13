R Tools for Observations, Receptors and Footprints (rtorf)
================

<img src="man/figures/logo.png" align="right" alt="" width="220" />

![GitHub commit
activity](https://img.shields.io/github/commit-activity/y/noaa-gml/rtorf)
[![R-CMD-check](https://github.com/noaa-gml/rtorf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/noaa-gml/rtorf/actions/workflows/R-CMD-check.yaml)
![GitHub Repo stars](https://img.shields.io/github/stars/noaa-gml/rtorf)

[NOAA Obspack](https://gml.noaa.gov/ccgg/obspack/) is a collection of
greenhouse gases observations

`rtorf` only depends on `data.table` and `ncdf4`, which is basically
parallel C, so it can be installed in any machine.

## Installation

Using git bash/powershell

``` bash
git clone https://github.com/noaa-gml/rtorf
R CMD INSTALL rtorf
```

Using remotes R package

``` r
remotes::install_github("noaa-gml/rtorf")
```

``` r
library(rtorf)
library(data.table)
utils::packageVersion("rtorf")
```

    ## [1] '3.3.0'

## ObsPack summary

The first step consists in constructing a summary for ObsPack (CH4, CO2
or other). This is required to read the data, but also, identify `agl`,
which is present in some of the file names. This function returns a
`data.frame`. Optionally, the user can indicate a path to store the
`data.frame`. `obs_summary` also prints a summary of the data. The
second argument is the categories, and by default includes the
categories shown below, to account for all the files. Then the summary
`data.frame` contains the columns `id` as the full path to each file,
`name` which is the name or relative path of the file, `n` just an id,
`sector` such as tower, and the column `agl` which indicates the `agl`
indicated in the name of the file if available. To read the
documentation of this function, the user must run `?obs_summary`.

> We first define the categories

``` r
cate = c(
  "aircraft-pfp",
  "aircraft-insitu",
  "aircraft-flask",
  "surface-insitu",
  "surface-flask",
  "surface-pfp",
  "tower-insitu",
  "aircore",
  "shipboard-insitu",
  "shipboard-flask"
)

obs <- "Z:/obspack/obspack_ch4_1_GLOBALVIEWplus_v5.1_2023-03-08/data/nc/"
index <- obs_summary(obs = obs, categories = cate)
```

    ## Number of files of index: 0
    ##           sector     N
    ##           <char> <int>
    ## 1: Total sectors     0
    ## Detected 0 files with agl
    ## Detected 0 files without agl

For each one of these dataset ids, check the articles in the
documentation

<https://noaa-gml.github.io/rtorf/>

e.g. for the `aircraft-pfp` we can read $CO_2$ and $CH_4$
[here](https://noaa-gml.github.io/rtorf/articles/aircraft-pfp.html)

## Implementation in python:

I’m currently implementing a version in python
[pytorf](https://github.com/noaa-gml/pytorf):

## Special thanks to all the

**contributors**

[![Contributors](https://contrib.rocks/image?repo=noaa-gml/rtorf)](https://github.com/noaa-gml/rtorf/graphs/contributors)

and

**Stargazers**

<p>

<a href="https://github.com/noaa-gml/rtorf/stargazers">
<img src="http://reporoster.com/stars/dark/noaa-gml/rtorf"/> </a>
</p>

### Note about legacy code.

I received the task of reformat legacy code used to read PARTICLE.DAT
and generate NetCDF. This code is really old and since STILT and other
have permissive licenses, it is here, however, some code is not exported
to NAMESPACE. A future version of rtorf, will have a new design for the
legacy code, a modern and efficient approach, keeping the essential
tasks.
