# r script for yale climate opinions data formatting

# ---- 1. set up ----

# clear workspace
rm(list = ls())

# load libraries
library(tidyverse)


# ---- 2. reformat climate opinions data for yadkin counties ----

# set directory for yale data
setwd("/Users/ssaia/Documents/sociohydro_project/data/yale_opinions_2016")

# load in data
opinions_data=read_csv("YCOM_2016_Data.01.csv")

# change GEOID to COUNTY and GeoName to NAME to be consistent with other df's
opinions_data=opinions_data %>% mutate(COUNTY=GEOID, NAME=toupper(GeoName))

# convert state names to all uppercase to be consistent with other df's
opinions_data$NAME=toupper(opinions_data$NAME)

# add in states that are from southeast
se_region=data.frame(NAME=c("ALABAMA","ARKANSAS","FLORIDA","GEORGIA","KENTUCKY",
                            "LOUISIANA","MISSISSIPPI","NORTH CAROLINA","SOUTH CAROLINA",
                            "TENNESSEE","VIRGINIA","WEST VIRGINIA"))
se_region$NAME=as.character(se_region$NAME) # set as character so don't get an error below

# save general rows
opinions_usa_data=opinions_data %>% filter(NAME=="US") # national
opinions_se_data=left_join(se_region,opinions_data,by="NAME")
opinions_nc_data=opinions_data %>% filter(NAME=="NORTH CAROLINA") # north carolina (statewide)
opinions_nc_county_data=opinions_data %>% filter(COUNTY>37000&COUNTY<37200) %>%
  select(-NAME) # north carolina (by county), drop NAME so can add short version below

# bring in info on yadkin
# set working directory to save data to
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_counties/")

# read in yadkin county names/numbers
yadkin_counties_sel=read_csv("yadkin_counties_sel.csv",col_names=TRUE)

# select only counties in yadkin ws
opinions_yadkin_data=left_join(yadkin_counties_sel,opinions_nc_county_data,by="COUNTY")

# set working directory to save data to
setwd("/Users/ssaia/Documents/sociohydro_project/data/yale_opinions_2016/reformatted_data")

# export data
write_csv(opinions_usa_data,"opinions_usa_data.csv")
write_csv(opinions_se_data,"opinions_se_data.csv")
write_csv(opinions_nc_data,"opinions_nc_data.csv")
write_csv(opinions_nc_county_data,"opinions_nc_county_data.csv")
write_csv(opinions_yadkin_data,"opinions_yadkin_data.csv")
