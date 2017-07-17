# r script for woods & pool data formatting/selection

# load libraries
library(dplyr)
library(readr)
library(stringr)

#---- WPCOMP data ----

# set directory for WPCOMP data
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/WPCOMP")

# download WPCOMP data
comp_data=read_csv("WPCOMP.csv",skip=3,col_names=TRUE,n_max=4246)

# rename column names
comp_cols_original=colnames(comp_data)
comp_cols_new=gsub('\\s+', '', comp_cols_original) %>%
  gsub(',','_',.) %>%
  gsub('\\+','',.)
colnames(comp_data)=comp_cols_new

# save general rows
comp_usa=comp_data %>% filter(NAME=="UNITED STATES")
comp_se=comp_data %>% filter(REGION==5)
comp_nc=comp_data %>% filter(NAME=="NORTH CAROLINA")
comp_nc_counties=comp_data %>% filter(COUNTY>37000&COUNTY<37200)

# fix name column of comp_nc_counties to remove comma
comp_nc_counties$NAME=gsub(', NC','',comp_nc_counties$NAME)

# select only counties in upper yadkin ws
my_counties=data.frame(NAME=c())
comp_nc_yadkin_data
