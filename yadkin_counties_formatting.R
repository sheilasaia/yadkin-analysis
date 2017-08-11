# r script for yadkin counties data formatting

# ---- 1. set up ----

# load libraries
library(tidyverse)
library(readr)
library(stringr)

# ---- 2. reformat county data yadkin ----

# set directory yadkin counties
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_analysis/")

# load in data
yadkin_county_data=read_csv("yadkin_counties.txt",col_names=TRUE)

# make new df with just county names
my_counties=yadkin_county_data %>% select(NAME,GEOID) %>%
  mutate(COUNTY=GEOID) %>% select(-GEOID)

# make all county entries uppercase
my_counties$NAME=toupper(my_counties$NAME)

# export my_counties
write_csv(my_counties,"yadkin_counties_sel.csv")
