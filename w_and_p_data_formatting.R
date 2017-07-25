# r script for woods & pool data formatting/selection

# ---- 1. set up ----
# load libraries
library(tidyverse)
#library(devtools)
#devtools::install_github("tidyverse/ggplot2") #needed to load geom_sf b/c it's only in the devel. version
library(ggplot2)
library(sf)

# for help
# http://strimas.com/r/tidy-sf/
# https://cran.r-project.org/web/packages/sf/vignettes/sf2.html


# ---- 2. reformat WPCOMP data for nc ----

# set directory for WPCOMP data
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/WPCOMP")

# load in data
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


# ---- 3. load in yadkin counties ----

# set directory yadkin counties data
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_counties/")

# load data
yadkin_counties_names=read_csv("yadkin_counties_sel.csv",col_names=TRUE)


# ---- 4. select comp data from only yadkin counties ----

# select only counties in yadkin ws
comp_yadkin_counties=left_join(yadkin_counties_names,comp_nc_counties,by="NAME")

# select employed and unemployed (in thousands)
emp_yadkin_data=comp_yadkin_counties %>% select(NAME,EMP_2000:EMP_2009)
unemp_yadkin_data=comp_yadkin_counties %>% select(NAME,UE_2000:UE_2009)

# gather employment and unemployment data by year for each county
emp_yadkin_to_gather=emp_yadkin_data 
colnames(emp_yadkin_to_gather)=c("NAME",seq(2000,2009,1))
emp_yadkin_gather_data=emp_yadkin_to_gather %>% group_by(NAME) %>%
  gather(key=year,value=num_thous,-NAME)

unemp_yadkin_to_gather=unemp_yadkin_data
colnames(unemp_yadkin_to_gather)=c("NAME",seq(2000,2009,1))
unemp_yadkin_gather_data=unemp_yadkin_to_gather %>% group_by(NAME) %>%
  gather(key=year,value=num_thous,-NAME)

# plot
ggplot(emp_yadkin_gather_data,aes(x=year,y=num_thous,color=NAME)) +
  geom_point(size=2) +
  ylab("number of employed (in thousands)")

ggplot(unemp_yadkin_gather_data,aes(x=year,y=num_thous,color=NAME)) +
  geom_point(size=2) +
  ylab("number of unemployed (in thousands)")

# zoom plot
ggplot(emp_yadkin_gather_data,aes(x=year,y=num_thous,color=NAME)) +
  geom_point(size=2) +
  ylab("number of employed (in thousands), zoom") +
  ylim(0,100)

ggplot(unemp_yadkin_gather_data,aes(x=year,y=num_thous,color=NAME)) +
  geom_point(size=2) +
  ylab("number of unemployed (in thousands), zoom") +
  ylim(0,5)


# ---- 5. displaying change over 2000-2009 by county ----

# calculate percent change
emp_yadkin_data = emp_yadkin_data %>% 
  mutate(perc_change=((EMP_2000-EMP_2009)/EMP_2000)*100)
unemp_yadkin_data=unemp_yadkin_data %>%
  mutate(perc_change=((UE_2000-UE_2009)/UE_2000)*100)

# set directory yadkin counties data
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_counties/")

# load in file
yadkin_counties_shp=st_read("yadkin_counties.shp",quiet=TRUE)
#glimpse(yadkin_counties_shp$NAME)

# convert name to uppercase
yadkin_counties_shp$NAME=toupper(yadkin_counties_shp$NAME)

# add in percent change of employed and unemployed to df
yadkin_counties_shp$emp_perc_change=emp_yadkin_data$perc_change
yadkin_counties_shp$unemp_perc_change=unemp_yadkin_data$perc_change
#glimpse(yadkin_counties_shp)

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=emp_perc_change)) +
  scale_fill_gradient2()

ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=unemp_perc_change)) +
  scale_fill_gradient2()


#yadkin_counties_geom=st_geometry(yadkin_counties_shp)