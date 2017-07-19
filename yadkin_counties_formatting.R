# r script for yadkin counties data formatting

# load libraries
library(dplyr)
library(readr)
library(stringr)

#---- WPCOMP data ----

# set directory yadkin counties
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_counties/")

# load in data
yadkin_county_data=read_csv("yadkin_counties.txt",col_names=TRUE)

# make new df with just county names
my_counties=yadkin_county_data %>% select(NAME)

# make all county entries uppercase
my_counties$NAME=toupper(my_counties$NAME)

# export my_counties
write_csv(my_counties,"yadkin_counties_sel.csv")
