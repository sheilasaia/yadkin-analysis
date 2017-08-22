# r script for calculating county weights by swat subbasin

# ---- 1. set up ----

# clear workspace
rm(list = ls())

# load libraries
library(tidyverse) # data mgmt
library(raster) # raster analysis
#library(rgdal) # dgal analysis in r
#library(sp) # spatial data analysis
#library(sf)


# ---- 2. load in data ----

# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_analysis")

# subbasin bounds
subbasins_ras=raster("subbasins_ras.tif")
subbasins_ras

# county bounds
counties_ras=raster("counties_ras.tif")
counties_ras

# county bounds metadata
# i could not get 'feature to raster' tool to use GEOID
# as the value field when making new raster
counties_ras_meta=read_csv("counties_ras.txt",col_names=TRUE) # commas spaced
counties_ras_meta=dplyr::select(counties_ras_meta,VALUE:AREA_KM2)

# shape files
counties_shp=st_read("yadkin_counties.shp",quiet=TRUE)
subbasins_shp=st_read("yadkin_subs1_utm17N.shp",quiet=TRUE)

# ---- 3. intersect two raster files ----

# use python scrip: https://gis.stackexchange.com/questions/19928/batch-clipping-in-arcgis-desktop-using-arcpy
# use split in analysis tools (toolbox) but then have to merge for all counties
# do this in a for loop... use https://gis.stackexchange.com/questions/63577/joining-polygons-in-r
# raster data in r: http://neondataskills.org/R/Raster-Data-In-R/

# first delete fields from shape file
subbasins_shp_simp=dplyr::select(subbasins_shp,Subbasin)

test=subbasins_shp %>% filter(Subbasin==1)
glimpse(test)

library(utils)
remove.packages(tid)
