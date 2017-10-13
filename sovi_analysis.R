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

# county scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/county_scaling_calculations")
county_scaling_raw=read_csv("county_scaling_allsubs.csv",col_names=TRUE)

# flood frequency results
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
flood_10yr_data=read_csv("flood_10yr_perc_change.csv",col_names=TRUE)
flood_100yr_data=read_csv("flood_10yr_perc_change.csv",col_names=TRUE)
#lowflow_10yr_data=read_csv("lowflow_10yr_perc_change.csv",col_names=TRUE)
#lowflow_100yr_data=read_csv("lowflow_100yr_perc_change.csv",col_names=TRUE)

# gis data
# county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)
yadkin_counties_shp_albers=read_sf("yadkin_clip_counties_albers.shp",quiet=TRUE)
yadkin_counties_shp=read_sf("yadkin_clip_counties_utm17N.shp",quiet=TRUE)
#glimpse(yadkin_subs_shp)
#glimpse(yadkin_counties_shp_albers)

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

# select NC data
nc_sovi_data=us_sovi_data %>% filter(state_fip==37) %>%
  mutate(dataset="n_carolina")

# bind all together for plotting
us_nc_yadkin_sovi_data=bind_rows(yadkin_sovi_data,nc_sovi_data,us_sovi_data) %>%
  filter(dataset!="national" | state_fip!=37)

# reformat county scaling so matches other columns in other datasets
county_scaling_data=county_scaling_raw %>% select(SUB,fip_code=GEOID,area_perc=AREA_PERC)

# select data for yadkin sovi plot
yadkin_sovi_data_sel=yadkin_sovi_data %>% select(fip_code,county_sovi,percentile)
yadkin_counties_shp_sovi=yadkin_counties_shp %>% left_join(yadkin_sovi_data_sel,by="fip_code")

# gis data
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin) # add SUB column to .shp file
#glimpse(yadkin_subs_shp)
yadkin_counties_shp=yadkin_counties_shp %>% mutate(fip_code=as.numeric(GEOID))
#glimpse(yadkin_counties_shp)


# ---- 3 data exploration ----

# plot hist of us sovi
ggplot(us_sovi_data,aes(county_sovi)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey30") +
  xlab("US SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  theme_bw()

# plot hist of north carolina sovi
ggplot(nc_sovi_data,aes(county_sovi)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey75") +
  xlab("NC SoVI") +
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
us_nc_yadkin_sovi_data$dataset=factor(us_nc_yadkin_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_sovi_data,aes(county_sovi,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw()

# zoom
ggplot(us_nc_yadkin_sovi_data,aes(county_sovi,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count (zoom)") +
  xlim(-10,16) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,20)) +
  theme_bw()

#setwd("/Users/ssaia/Desktop")
#cairo_pdf("yadkin_sovi_by_county.pdf",width=11,height=8.5)
ggplot(yadkin_counties_shp_sovi,aes(fill=county_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_counties_shp_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",low="blue",high="red") +
  theme_bw()
#dev.off()


# ---- 4.1 scale county data to subbasin ----

# join county scaling data to yadkin sovi select data
wtd_calcs_data=left_join(yadkin_sovi_data_sel,county_scaling_data,by="fip_code") %>%
  mutate(area_wtd_sovi_indiv=county_sovi*area_perc,area_wtd_percentile_indiv=percentile*area_perc) %>%
  group_by(SUB) %>% summarise(area_wtd_sovi=sum(area_wtd_sovi_indiv),area_wtd_percentile=sum(area_wtd_percentile_indiv))

# add to shp file
yadkin_subs_shp_sovi=yadkin_subs_shp %>% left_join(wtd_calcs_data,by="SUB")
#glimpse(yadkin_subs_shp_sovi)

# question: does it make sense to area wt sovi and sovi percentiles?


# ---- 4.2 plot county sovi data in space ----

#setwd("/Users/ssaia/Desktop")
#cairo_pdf("yadkin_sovi_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_sovi,aes(fill=area_wtd_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Weighted SoVI",low="blue",high="red") +
  theme_bw()
#dev.off()

#setwd("/Users/ssaia/Desktop")
#cairo_pdf("yadkin_sovi_percentile_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_sovi,aes(fill=area_wtd_percentile)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Weighted SoVI Percentile",limits=c(0,100)) +
  theme_bw()
#dev.off()


# ---- 5.1 compare flood and lowflow freq results to scaled sovi data ----

# join flood and sovi data
flood_10yr_sovi_data=left_join(flood_10yr_data,wtd_calcs_data,by="SUB") 

# use scale() to get z score


# ---- 5.2 plot flood and lowflow freq results to scaled sovi data ----

flood_10yr_sovi_data$dataset=factor(flood_10yr_sovi_data$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
ggplot(flood_10yr_sovi_data,(aes(x=area_wtd_sovi,y=perc_change,color=dataset))) +
  geom_point(size=2) +
  scale_x_reverse() +
  xlab("Area Weighted SoVI") +
  ylab("10yr Flood Frequency % Change (from Baseline)") +
  theme_bw()
