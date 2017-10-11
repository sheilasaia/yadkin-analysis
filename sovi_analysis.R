# yadkin sovi analysis

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
#devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
#library(ggplot2)
library(sf)

# set directory and load data
# sovi data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
us_sovi_data=read_csv("sovi_data.csv",col_names=TRUE)

# counties list
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers")
yadkin_counties_raw=read_csv("yadkin_clip_counties_albers.txt",col_names=TRUE)

# gis data
# county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)
#glimpse(yadkin_subs_shp)

# looking at gis data
yadkin_subs_shp_albers_geom=st_geometry(yadkin_subs_shp_albers)
#attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
#attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2 reformat data ----

# set common names between us_sovi_data columns and yadkin_counties
#names(us_sovi_data)
#names(yadkin_counties_raw)
yadkin_counties=yadkin_counties_raw %>% transmute(fip_code=as.numeric(GEOID),
                                                  state_fip=as.numeric(STATEFP),
                                                  county_fip=as.numeric(COUNTYFP),
                                                  county_name_gis=NAME)
# select only necessary columns
yadkin_counties_sel=yadkin_counties %>% select(fip_code,county_name_gis)

# join with sovi data
yadkin_sovi_data=left_join(yadkin_counties_sel,us_sovi_data,by="fip_code") %>%
  mutate(dataset="yadkin") %>% select(-county_name_gis)

# add dataset column to us sovi data
us_sovi_data=us_sovi_data %>% mutate(dataset="national")

# bind all together for plotting
us_yadkin_sovi_data=bind_rows(yadkin_sovi_data,us_sovi_data)


# gis data
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin) # add SUB column to .shp file
#glimpse(yadkin_subs_shp)


# ---- 3 data exploration ----

# plot hist of us sovi
ggplot(us_sovi_data,aes(county_sovi)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey75") +
  xlab("US SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  theme_bw()

# plot hist of yadkin sovi
ggplot(yadkin_sovi_data,aes(county_sovi)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  xlab("Yadkin SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  theme_bw()

# plot together
ggplot(us_yadkin_sovi_data,aes(county_sovi,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  scale_fill_manual(values=c("grey75","white")) +
  scale_color_manual(values=c("black","black")) +
  theme_bw()

# zoom
ggplot(us_yadkin_sovi_data,aes(county_sovi,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count (zoom)") +
  xlim(-10,16) +
  scale_fill_manual(values=c("grey75","white")) +
  scale_color_manual(values=c("black","black")) +
  coord_cartesian(ylim=c(0,20)) +
  theme_bw()


# ---- 4.1 scale county-level data to subbasin ----
