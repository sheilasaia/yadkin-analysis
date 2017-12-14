# yadkin sovi analysis

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
#devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
#library(ggplot2)
library(sf)

# load home-made functions
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path,"risk_vuln_reclass.R")) # reclassify sovi analysis results
source(paste0(functions_path,"rescale_minus1to1.R")) # rescale from -1 to 1

# set directory and load data
# usc sovi data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
us_usc_sovi_data=read_csv("usc_sovi_data.csv",col_names=TRUE)

# atsdr sovi data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
us_atsdr_sovi_data=read_csv("atsdr_sovi_data.csv",col_names=TRUE)

# counties list
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers")
yadkin_counties_raw=read_csv("yadkin_clip_counties_albers.txt",col_names=TRUE)

# county scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/county_scaling_calculations")
county_scaling_raw=read_csv("county_scaling_allsubs.csv",col_names=TRUE)

# atsdr sovi tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_raw=read_csv("sovibd_scaling_allsubs.csv",col_names=TRUE)

# flood frequency results
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
flood_10yr_data=read_csv("flood_10yr_perc_change.csv",col_names=TRUE)
flood_100yr_data=read_csv("flood_10yr_perc_change.csv",col_names=TRUE)
#lowflow_10yr_data=read_csv("lowflow_10yr_perc_change.csv",col_names=TRUE)
#lowflow_100yr_data=read_csv("lowflow_100yr_perc_change.csv",col_names=TRUE)

# gis data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)
yadkin_counties_shp_albers=read_sf("yadkin_clip_counties_albers.shp",quiet=TRUE)
yadkin_counties_shp=read_sf("yadkin_clip_counties_utm17N.shp",quiet=TRUE)
#glimpse(yadkin_subs_shp)
#glimpse(yadkin_counties_shp_albers)
nc_counties_shp=read_sf("nc_counties_utm17N.shp",quiet=TRUE)
#glimpse(nc_counties_shp)

# looking at gis data
yadkin_subs_shp_albers_geom=st_geometry(yadkin_subs_shp_albers)
#attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
#attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2 reformat data ----

# set common names between us_usc_sovi_data & us_atsdr_sovi_data columns and yadkin_counties
#names(us_usc_sovi_data)
#names(us_atsdr_sovi_data)
#names(yadkin_counties_raw)
yadkin_counties=yadkin_counties_raw %>% transmute(fip_code=as.numeric(GEOID),
                                                  state_fip=as.numeric(STATEFP),
                                                  county_fip=as.numeric(COUNTYFP),
                                                  county_name_gis=NAME)

# select only necessary columns
yadkin_counties_sel=yadkin_counties %>% select(fip_code,county_name_gis)

# add dataset id (and rescaled usc data)
# usc
us_usc_sovi_data=us_usc_sovi_data %>% mutate(dataset="national") %>%
  mutate(county_sovi_rescaled=rescale_minus1to1(county_sovi))

# atsrd
us_atsdr_sovi_data=us_atsdr_sovi_data %>% mutate(dataset="national")

# add state abbreviation to usc data
state_id_list=us_atsdr_sovi_data %>% select(state_abbrev,state_fip) %>%
  distinct()
us_usc_sovi_data=us_usc_sovi_data %>% left_join(state_id_list,by="state_fip")

# use join to only select yadkin data
# usc
yadkin_usc_sovi_data=left_join(yadkin_counties_sel,us_usc_sovi_data,by="fip_code") %>%
  mutate(dataset="yadkin") %>% select(-county_name_gis)

# atsdr
yadkin_atsdr_sovi_data=left_join(yadkin_counties_sel,us_atsdr_sovi_data,by="fip_code") %>%
  mutate(dataset="yadkin") %>% select(-county_name_gis)

# select nc data
# usc
nc_usc_sovi_data=us_usc_sovi_data %>% filter(state_fip==37) %>%
  mutate(dataset="n_carolina")

# atsdr
nc_atsdr_sovi_data=us_atsdr_sovi_data %>% filter(state_fip==37) %>%
  mutate(dataset="n_carolina")

# bind all together for plotting
# usc
us_nc_yadkin_usc_sovi_data=bind_rows(yadkin_usc_sovi_data,nc_usc_sovi_data,us_usc_sovi_data)

# atsdr
us_nc_yadkin_atsdr_sovi_data=bind_rows(yadkin_atsdr_sovi_data,nc_atsdr_sovi_data,us_atsdr_sovi_data)

# reformat county scaling so matches other columns in other datasets
county_scaling_data=county_scaling_raw %>% select(SUB,fip_code=GEOID,area_perc=AREA_PERC)

# reformat sovi boundary scaling so it matches other columns in other datasets
sovibd_scaling_data=

# gis data
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin) # add SUB column to .shp file
#glimpse(yadkin_subs_shp)
yadkin_counties_shp=yadkin_counties_shp %>% mutate(fip_code=as.numeric(GEOID))
#glimpse(yadkin_counties_shp)
nc_counties_shp=nc_counties_shp %>% mutate(fip_code=as.numeric(GEOID))
#glimpse(nc_counties_shp)

# select data for yadkin sovi plot
yadkin_usc_sovi_data_sel=yadkin_usc_sovi_data %>% select(fip_code,county_sovi,percentile,county_sovi_rescaled)
yadkin_counties_shp_sovi=yadkin_counties_shp %>% left_join(yadkin_usc_sovi_data_sel,by="fip_code")

# select data for nc sovi plot
nc_usc_sovi_data_sel=nc_usc_sovi_data %>% select(fip_code:percentile,county_sovi_rescaled)
nc_counties_shp_usc_sovi=nc_counties_shp %>% left_join(nc_usc_sovi_data_sel,by="fip_code")


# ---- 3.1 usc data exploration ----

# plot hist of us usc sovi
ggplot(us_usc_sovi_data,aes(county_sovi)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey30") +
  xlab("US SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  theme_bw()

# plot hist of north carolina usc sovi
ggplot(nc_usc_sovi_data,aes(county_sovi)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey75") +
  xlab("NC SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  theme_bw()

# plot hist of yadkin usc sovi
ggplot(yadkin_usc_sovi_data,aes(county_sovi)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  xlab("Yadkin SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  theme_bw()

# plot together
setwd("/Users/ssaia/Desktop")
cairo_pdf("usc_sovi_hist.pdf",width=11,height=8.5)
us_nc_yadkin_usc_sovi_data$dataset=factor(us_nc_yadkin_usc_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_usc_sovi_data,aes(county_sovi,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count") +
  xlim(-10,16) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("sovi_usc_hist_zoom.pdf",width=11,height=8.5)
ggplot(us_nc_yadkin_usc_sovi_data,aes(county_sovi,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count (zoom)") +
  xlim(-10,16) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,20)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# plot hist of us usc sovi (rescaled)
ggplot(us_usc_sovi_data,aes(county_sovi_rescaled)) +
  geom_histogram(binwidth=0.05, colour="black", fill="grey30") +
  xlab("US SoVI Rescaled") +
  ylab("Count") +
  xlim(-1,1) +
  theme_bw()

# plot hist of north carolina usc sovi (rescaled)
ggplot(nc_usc_sovi_data,aes(county_sovi_rescaled)) +
  geom_histogram(binwidth=.05, colour="black", fill="grey75") +
  xlab("NC SoVI Rescaled") +
  ylab("Count") +
  xlim(-1,1) +
  theme_bw()

# plot hist of yadkin usc sovi (rescaled)
ggplot(yadkin_usc_sovi_data,aes(county_sovi_rescaled)) +
  geom_histogram(binwidth=.05, colour="black", fill="white") +
  xlab("Yadkin SoVI Rescaled") +
  ylab("Count") +
  xlim(-1,1) +
  theme_bw()

# plot together (rescaled)
setwd("/Users/ssaia/Desktop")
cairo_pdf("usc_sovi_hist_rescaled_.pdf",width=11,height=8.5)
us_nc_yadkin_usc_sovi_data$dataset=factor(us_nc_yadkin_usc_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_usc_sovi_data,aes(county_sovi_rescaled,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.05, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count") +
  xlim(-1,1) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("sovi_usc_hist_rescaled_zoom.pdf",width=11,height=8.5)
ggplot(us_nc_yadkin_usc_sovi_data,aes(county_sovi_rescaled,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.05, alpha=0.5) +
  xlab("SoVI Rescaled") +
  ylab("Count (zoom)") +
  xlim(-1,1) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,20)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# yadkin usc sovi (spatial)
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_usc_sovi_by_county.pdf",width=11,height=8.5)
ggplot(yadkin_counties_shp_sovi,aes(fill=county_sovi_rescaled)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_counties_shp_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Rescaled SoVI",low="blue",high="red",limits=c(-1,1)) +
  theme_bw()
dev.off()

# nc usc sovi (spatial)
setwd("/Users/ssaia/Desktop")
cairo_pdf("nc_usc_sovi_by_county.pdf",width=11,height=8.5)
ggplot(nc_counties_shp_usc_sovi,aes(fill=county_sovi_rescaled)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # nc_counties_shp_usc_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Rescaled SoVI",low="blue",high="red",limits=c(-1,1)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# ---- 3.2 atsdr data exploration ----

# plot hist of us atsdr sovi
ggplot(us_atsdr_sovi_data,aes(sovi_total)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey30") +
  xlab("US SoVI") +
  ylab("Count") +
  xlim(0,15) +
  theme_bw()

# plot hist of north carolina atsdr sovi
ggplot(nc_atsdr_sovi_data,aes(sovi_total)) +
  geom_histogram(binwidth=.5, colour="black", fill="grey75") +
  xlab("NC SoVI") +
  ylab("Count") +
  xlim(0,15) +
  theme_bw()

# plot hist of yadkin atsdr sovi
ggplot(yadkin_atsdr_sovi_data,aes(sovi_total)) +
  geom_histogram(binwidth=.5, colour="black", fill="white") +
  xlab("Yadkin SoVI") +
  ylab("Count") +
  xlim(0,15) +
  theme_bw()

# plot together (all themes)
setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi_allthemes_hist.pdf",width=11,height=8.5)
us_nc_yadkin_atsdr_sovi_data$dataset=factor(us_nc_yadkin_atsdr_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_total,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count") +
  xlim(0,15) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("sovi_atsdr_allthemes_hist_zoom.pdf",width=11,height=8.5)
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_total,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI") +
  ylab("Count (zoom)") +
  xlim(0,15) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,250)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# plot together (theme 1)
setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi_theme1_hist.pdf",width=11,height=8.5)
us_nc_yadkin_atsdr_sovi_data$dataset=factor(us_nc_yadkin_atsdr_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme1,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI Theme 1") +
  ylab("Count") +
  xlim(0,5) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("sovi_atsdr_theme1_hist_zoom.pdf",width=11,height=8.5)
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme1,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI Theme 1") +
  ylab("Count (zoom)") +
  xlim(0,5) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,1500)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# plot together (theme 2)
setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi_theme2_hist.pdf",width=11,height=8.5)
us_nc_yadkin_atsdr_sovi_data$dataset=factor(us_nc_yadkin_atsdr_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme2,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI Theme 2") +
  ylab("Count") +
  xlim(0,5) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("sovi_atsdr_theme2_hist_zoom.pdf",width=11,height=8.5)
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme2,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI Theme 2") +
  ylab("Count (zoom)") +
  xlim(0,5) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,2000)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# plot together (theme 3)
setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi_theme3_hist.pdf",width=11,height=8.5)
us_nc_yadkin_atsdr_sovi_data$dataset=factor(us_nc_yadkin_atsdr_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme3,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.25, alpha=0.5) +
  xlab("SoVI Theme 3") +
  ylab("Count") +
  xlim(0,3) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("sovi_atsdr_theme3_hist_zoom.pdf",width=11,height=8.5)
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme3,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.25, alpha=0.5) +
  xlab("SoVI Theme 3") +
  ylab("Count (zoom)") +
  xlim(0,3) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,1000)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# plot together (theme 4)
setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi_theme4_hist.pdf",width=11,height=8.5)
us_nc_yadkin_atsdr_sovi_data$dataset=factor(us_nc_yadkin_atsdr_sovi_data$dataset,levels=c("national","n_carolina","yadkin"))
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme4,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI Theme 4") +
  ylab("Count") +
  xlim(0,5) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("sovi_atsdr_theme4_hist_zoom.pdf",width=11,height=8.5)
ggplot(us_nc_yadkin_atsdr_sovi_data,aes(sovi_theme4,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI Theme 4") +
  ylab("Count (zoom)") +
  xlim(0,5) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  coord_cartesian(ylim=c(0,1000)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()


# yadkin sovi (spatial)
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_by_county.pdf",width=11,height=8.5)
ggplot(yadkin_counties_shp_sovi,aes(fill=county_sovi_rescaled)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_counties_shp_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Rescaled SoVI",low="blue",high="red",limits=c(-1,1)) +
  theme_bw()
dev.off()

# nc sovi (spatial)
setwd("/Users/ssaia/Desktop")
cairo_pdf("nc_sovi_by_county.pdf",width=11,height=8.5)
ggplot(nc_counties_shp_sovi,aes(fill=county_sovi_rescaled)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # nc_counties_shp_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Rescaled SoVI",low="blue",high="red",limits=c(-1,1)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()




# ---- 4.1 scale county data to subbasin ----

# join county scaling data to yadkin sovi select data
wtd_calcs_data=left_join(yadkin_usc_sovi_data_sel,county_scaling_data,by="fip_code") %>%
  mutate(area_wtd_sovi_indiv=county_sovi*area_perc,
         area_wtd_percentile_indiv=percentile*area_perc,
         area_wtd_sovi_rescaled_indiv=county_sovi_rescaled*area_perc) %>%
  group_by(SUB) %>% summarise(area_wtd_sovi=sum(area_wtd_sovi_indiv),
                              area_wtd_percentile=sum(area_wtd_percentile_indiv),
                              area_wtd_sovi_rescaled=sum(area_wtd_sovi_rescaled_indiv))

# add to shp file
yadkin_subs_shp_sovi=yadkin_subs_shp %>% left_join(wtd_calcs_data,by="SUB")
#glimpse(yadkin_subs_shp_sovi)

# question: does it make sense to area wt sovi and sovi percentiles?


# ---- 4.2 plot county sovi data in space ----

setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_sovi,aes(fill=area_wtd_sovi_rescaled)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_sovi is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Weighted SoVI",low="blue",high="red",limits=c(-1,1)) +
  theme_bw()
dev.off()

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

# ---- 5.2 plot flood and lowflow freq results to scaled sovi data ----

# by dataset
flood_10yr_sovi_data$dataset=factor(flood_10yr_sovi_data$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
ggplot(flood_10yr_sovi_data,(aes(x=area_wtd_sovi_rescaled,y=perc_change,color=dataset))) +
  geom_point(size=2) +
  xlab("Area Weighted SoVI (Rescaled to National Data)") +
  ylab("10yr Flood Frequency % Change (from Baseline)") +
  xlim(-1,1) +
  theme_bw()

# by subbasin
flood_10yr_sovi_data$dataset=factor(flood_10yr_sovi_data$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
ggplot(flood_10yr_sovi_data,(aes(x=area_wtd_sovi_rescaled,y=perc_change,color=SUB))) +
  geom_point(size=2) +
  xlab("Area Weighted SoVI (Rescaled to National Data)") +
  ylab("10yr Flood Frequency % Change (from Baseline)") +
  xlim(-1,1) +
  theme_bw()


# ---- 5.3 reclass risk/vulnerability results ----

flood_10yr_risk_vuln_data=risk_vuln_reclass(flood_10yr_sovi_data)

# ---- 5.4 plot reclassified results (points and spatially) ----

# check output
# by dataset
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_risk_vs_sovi_reclass.pdf",width=11,height=8.5)
ggplot(flood_10yr_risk_vuln_data,(aes(x=area_wtd_sovi_rescaled,y=perc_change,color=sovi_class))) +
  geom_point(size=5) +
  geom_point(shape=1,size=5,color="black") +
  xlab("Area Weighted SoVI Re-scaled") +
  ylab("10yr Flood Frequency % Change") +
  xlim(-1,1) +
  scale_color_manual(values=c("red","orange","orange","yellow")) +
  scale_fill_manual(values=rep("black",4)) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()

# plot spatially
flood_10yr_risk_vuln_data_sel=flood_10yr_risk_vuln_data %>% select(SUB,dataset,sovi_class)
# add to shp file
yadkin_subs_shp_sovi_reclass=yadkin_subs_shp %>% left_join(flood_10yr_risk_vuln_data_sel,by="SUB")
#glimpse(yadkin_subs_shp_sovi_reclass)

setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_reclass_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_sovi_reclass,aes(fill=sovi_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_sovi_reclass is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values=c("red","orange","orange","yellow")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20))
dev.off()
