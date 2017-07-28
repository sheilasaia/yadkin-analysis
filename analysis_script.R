# r script for demographic data analysis

# ---- 1. set up ----

# clear workspace
rm(list = ls())

# load libraries
library(tidyverse)
#library(devtools)
#devtools::install_github("tidyverse/ggplot2") #needed to load geom_sf b/c it's only in the devel. version
library(sf)

# for help
# http://strimas.com/r/tidy-sf/
# https://cran.r-project.org/web/packages/sf/vignettes/sf2.html

# ---- 2. load in data ----

# set directory and load yadkin counties data (list of names & ids)
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_counties/")
yadkin_counties_names=read_csv("yadkin_counties_sel.csv",col_names=TRUE)

# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/sociohydro_project/swat_yadkin_counties/")
yadkin_counties_shp=st_read("yadkin_counties.shp",quiet=TRUE)
#glimpse(yadkin_counties_shp$NAME)

# convert county name to uppercase
yadkin_counties_shp$NAME=toupper(yadkin_counties_shp$NAME)
# set working directory and load WPGEO data (economic and labor data) **used this one**
setwd("/Users/ssaia/Documents/sociohydro_project/data/woods_pool_v2011/reformatted_data")
geo_yadkin_data=read_csv("geo_yadkin_data.csv",col_names=TRUE)

# load WPCOMP data (economic and labor data)
comp_yadkin_data=read_csv("comp_yadkin_data.csv",col_names=TRUE)

# set working and load yale opinions data
setwd("/Users/ssaia/Documents/sociohydro_project/data/yale_opinions_2016/reformatted_data")
opinions_yadkin_data=read_csv("opinions_yadkin_data.csv",col_names=TRUE)


# ---- 3. dataset comparision ----

# look at data from alexander county (# ppl employed)
comp_alexander_data=comp_yadkin_data %>% filter(NAME=="ALEXANDER") %>%
  select(NAME,EMP_2000:EMP_2009)
geo_alexander_data=geo_yadkin_data %>% filter(var_name=="emp_total_thous" & NAME=="ALEXANDER")

# combine into one df for easy viewing
alexander_comparison=data.frame(year=seq(2000,2009,1),comp_data=as.numeric(comp_alexander_data[1,2:11]),
                            geo_data=as.numeric(geo_alexander_data[1,4:13]),
                            diff=as.numeric(comp_alexander_data[1,2:11])-as.numeric(geo_alexander_data[1,4:13]))

# why are their differences between these two files? maybe i should just use the geo file?
# the geo files has most of the things i need...


# ---- 4. select emp & unemp data from only yadkin counties (comp_yadkin_data) ----

# select employed and unemployed (in thousands)
emp_yadkin_data=comp_yadkin_data %>% select(NAME,EMP_2000:EMP_2009)
unemp_yadkin_data=comp_yadkin_data %>% select(NAME,UE_2000:UE_2009)

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


# ---- 5. displaying emp & unemp change over 2000-2009 by county (comp_yadkin_data) ----

# calculate percent change
comp_employ_change_data=comp_yadkin_data %>%
  mutate(comp_emp_perc_change=((EMP_2009-EMP_2000)/EMP_2000)*100, comp_unemp_perc_change=((UE_2009-UE_2000)/UE_2000)*100) %>%
  select(NAME,comp_emp_perc_change,comp_unemp_perc_change)

# add in percent change of employed and unemployed to shape file
yadkin_counties_shp=left_join(yadkin_counties_shp,comp_employ_change_data,by="NAME")
#glimpse(yadkin_counties_shp)

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=comp_emp_perc_change)) +
  scale_fill_gradient2(name="% Change # Emp. (WPCOMP)")

ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=comp_unemp_perc_change)) +
  scale_fill_gradient2(name="% Change # Unemp. (WPCOMP)")

#yadkin_counties_geom=st_geometry(yadkin_counties_shp)


# ---- 6. displaying emp change over 2000-2009 by county (geo_yadkin_data) ----

# calculate percent change
geo_employ_change_data=geo_yadkin_data %>% filter(var_name=="emp_total_thous") %>%
  mutate(geo_emp_perc_change=((yr_2009-yr_2000)/yr_2000)*100) %>%
  select(NAME,geo_emp_perc_change)

# add in percent change of employed to shape file
yadkin_counties_shp=left_join(yadkin_counties_shp,geo_employ_change_data,by="NAME")
#glimpse(yadkin_counties_shp)

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=geo_emp_perc_change)) +
  scale_fill_gradient2(name="% Change # Emp. (WPGEO)")


# ---- 7. displaying population change over 2000-2009 by county (geo_yadkin_data) ----

# calculate percent change
geo_pop_change_data=geo_yadkin_data %>% filter(var_name=="total_pop_thous") %>%
  mutate(geo_pop_perc_change=((yr_2009-yr_2000)/yr_2000)*100) %>%
  select(NAME,geo_pop_perc_change)

# add in percent change of population to shape file
yadkin_counties_shp=left_join(yadkin_counties_shp,geo_pop_change_data,by="NAME")
#glimpse(yadkin_counties_shp)

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=geo_pop_perc_change)) +
  scale_fill_gradient2(name="% Change Population (WPGEO)",limits=c(-20,60))


# ---- 8. display max empolyment sector for 2009 (geo_yadkin_data) ----

# select all employment sector data
geo_jobs_2009_data=geo_yadkin_data %>% arrange(var_name) %>% 
  slice(93:644) %>%
  select(var_name,NAME,COUNTY,yr_2009)

# gather data
geo_jobs_2009_spread=spread(geo_jobs_2009_data,var_name,yr_2009) %>%
  select(NAME:emp_statelocalgovt_thous,emp_trade_thous:emp_util_thous,emp_total_thous)

# shorten column names
geo_jobs_2009_spread_cols_oringinal=colnames(geo_jobs_2009_spread)
geo_jobs_2009_spread_cols_new=gsub('emp_','', geo_jobs_2009_spread_cols_oringinal)
colnames(geo_jobs_2009_spread)=geo_jobs_2009_spread_cols_new

# calculate percent of total jobs
geo_jobs_2009_perc_calc=geo_jobs_2009_spread %>% group_by(NAME) %>%
  mutate_at(vars(accom_thous:util_thous),funs(perc=./total_thous*100)) %>%
  select(NAME,accom_thous_perc:util_thous_perc)

# shorten column names
geo_jobs_2009_perc_cols_original=colnames(geo_jobs_2009_perc_calc)
geo_jobs_2009_perc_cols_new=gsub('_thous_perc','',geo_jobs_2009_perc_cols_original)
colnames(geo_jobs_2009_perc_calc)=geo_jobs_2009_perc_cols_new

# gather percentage results
geo_jobs_2009_perc=geo_jobs_2009_perc_calc %>% gather(job,perc,-NAME)

# plot as stacked bar chart
ggplot(geo_jobs_2009_perc,aes(x=NAME,y=perc,fill=job)) +
  geom_col(position="stack") +
  theme(axis.text.x=element_text(angle=90,hjust=1))

# look at just farming jobs
geo_jobs_2009_perc_farming=geo_jobs_2009_perc %>% filter(job=="farm")

# plot just farming jobs
ggplot(geo_jobs_2009_perc_farming,aes(x=NAME,y=perc)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90,hjust=1))

# look at just max sectors
geo_jobs_2009_perc_max=geo_jobs_2009_perc %>% filter(perc==max(perc)) %>%
  mutate(max_job_2009=job) %>% select(NAME,max_job_2009,perc)

# plot
ggplot(geo_jobs_2009_perc_max,aes(x=NAME,y=perc,fill=max_job_2009)) +
  geom_col() +
  theme(axis.text.x=element_text(angle=90,hjust=1))

# add to shp file
geo_jobs_2009_perc_max_sel=geo_jobs_2009_perc_max %>% select(NAME,max_job_2009)
yadkin_counties_shp=left_join(yadkin_counties_shp,geo_jobs_2009_perc_max_sel,by="NAME")
#glimpse(yadkin_counties_shp)

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=max_job_2009)) +
  scale_fill_discrete(name="Primary Job Sector 2009")


# ---- 9. display % change in income per capita from 2000 to 2009 ----

# calculate percent change (using 2011 USD)
geo_ipc_change_data=geo_yadkin_data %>% filter(var_name=="inc_per_capita_2011USD") %>%
  mutate(geo_ipc_change_perc=((yr_2009-yr_2000)/yr_2000)*100) %>%
  select(NAME,geo_ipc_change_perc)

# add in percent change of population to shape file
yadkin_counties_shp=left_join(yadkin_counties_shp,geo_ipc_change_data,by="NAME")
#glimpse(yadkin_counties_shp)

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=geo_ipc_change_perc)) +
  scale_fill_gradient2(name="% Change Inc./Capita (2011USDs)",limits=c(0,50))


# ---- 10. display yale climate opinions data ----

# select data to add to shp file
opinions_yadkin_data_sel=opinions_yadkin_data %>% select(NAME,worried:personalOppose)

# add to shp file
yadkin_counties_shp=left_join(yadkin_counties_shp,opinions_yadkin_data_sel,by="NAME")
#glimpse(yadkin_counties_shp)

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=worried)) +
  scale_fill_gradientn(name="% Worried about C.C.",colours=rev(heat.colors(10)),
                     breaks=seq(0,100,20),labels=seq(0,100,20),limits=c(0,100))

# plot
ggplot(yadkin_counties_shp) +
  geom_sf(aes(fill=personal)) +
  scale_fill_gradientn(name="% Personally Impacted by C.C.",colours=rev(heat.colors(10)),
                       breaks=seq(0,100,20),labels=seq(0,100,20),limits=c(0,100))



