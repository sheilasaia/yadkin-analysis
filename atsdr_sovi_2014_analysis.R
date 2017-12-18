# yadkin sovi analysis (atsdr 2010-2014 tract level data)
# last updated 20171215

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
#devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
#library(ggplot2)
library(sf)

# atsdr sovi 2010-2014 tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_raw=read_csv("sovibd_2014_scaling_allsubs.csv",col_names=TRUE)
#don't need tract column?

# atsdr sovi 2010-2014 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
us_sovi_data=read_csv("atsdr_us_sovi_2014_data.csv",col_names=TRUE)

# gis data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_sub_shp_raw=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)
yadkin_tract_shp_raw=read_sf("yadkin_sovi2014_utm17N.shp",quiet=TRUE)
# use Albers projection for calcs in ArcGIS but UTM 17N
# here for plotting because can sf() recognizes UTM and
# then can convert sf() is not recognizing Albers projection

# looking at gis data
#yadkin_sub_shp_raw_geom=st_geometry(yadkin_sub_shp_raw)
#attributes(yadkin_sub_shp_raw) # this has an epsg code!


# ---- 2 reformat data -----

# remove X1 column in scaling data
sovidb_scaling_data=sovibd_scaling_raw %>% select(-X1)

# make sure fips columns are same name and are as.numeric()
yadkin_tract_shp=yadkin_tract_shp_raw %>% 
  mutate(fips=as.numeric(FIPS)) %>% select(-FIPS)

# copy census tract data to new variable
yadkin_census_tracts_data=yadkin_tract_shp %>% 
  select(fips,E_TOTPOP:E_DAYPOP)  %>% # select only columns you need 
  st_set_geometry(NULL) # set geometry as null to get df

# change subbasin id column to match other files
yadkin_sub_shp=yadkin_sub_shp_raw %>% 
  mutate(SUB=Subbasin) %>% 
  select(-Subbasin)

# select atsdr data for yadkin
yadkin_sovi_data=left_join(sovidb_scaling_data,us_sovi_data,by="fips")


# ---- extract tract data to plot all themes together on one plot ----

# select out theme 1
yadkin_sovi_theme1_tract_data=yadkin_census_tracts_data %>%
  select(fips,sovi=SPL_THEME1) %>%
  mutate(theme="theme1_socio_econ_demog")

# select out theme 2
yadkin_sovi_theme2_tract_data=yadkin_census_tracts_data %>%
  select(fips,sovi=SPL_THEME2) %>%
  mutate(theme="theme2_household_comp_ability_demog")

# select out theme 1
yadkin_sovi_theme3_tract_data=yadkin_census_tracts_data %>%
  select(fips,sovi=SPL_THEME3) %>%
  mutate(theme="theme3_minority_esl_demog")

# select out theme 1
yadkin_sovi_theme4_tract_data=yadkin_census_tracts_data %>%
  select(fips,sovi=SPL_THEME4) %>%
  mutate(theme="theme4_housing_transportation_demog")

# bind
yadkin_sovi_themes_tract_data=bind_rows(yadkin_sovi_theme1_tract_data,
                                        yadkin_sovi_theme2_tract_data,
                                        yadkin_sovi_theme3_tract_data,
                                        yadkin_sovi_theme4_tract_data)

yadkin_tract_shp_sovi_theme1to4=left_join(yadkin_tract_shp,yadkin_sovi_themes_tract_data,by="fips")
#glimpse(yadkin_tract_shp_sovi_theme1to4)

# ---- plot sovi data by tract ----

# total sovi by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_total_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp,aes(fill=SPL_THEMES)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_tract_shp is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Total SoVI",na.value="grey75",high="darkred",low="white",limits=c(2,13)) +
  theme_bw() #+
  #theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
  #      text=element_text(size=16))
dev.off()

# sovi theme 1 (socioeconomic status) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme1_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp,aes(fill=SPL_THEME1)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 1",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#        text=element_text(size=16))
dev.off()

# sovi theme 2 (household make-up & disability) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme2_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp,aes(fill=SPL_THEME2)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_tract_shp is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 2",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#        text=element_text(size=16))
dev.off()

# sovi theme 3 (minority status & knowledge of english) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme3_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp,aes(fill=SPL_THEME3)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_tract_shp is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 3",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#        text=element_text(size=16))
dev.off()

# sovi theme 4 (housing and transportation) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme4_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp,aes(fill=SPL_THEME4)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_tract_shp is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 4",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#        text=element_text(size=16))
dev.off()

# all four themes together in one plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme1to4_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp_sovi_theme1to4,aes(fill=sovi)) +
  geom_sf() +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",na.value="grey75",high="darkred",low="white",limit=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()


# ---- scale sovi data to subbasin ----

# scale to subbasin
# total sovi
yadkin_sovi_total_sub_data=yadkin_sovi_data %>%
  select(SUB,fips,tract_perc,sub_perc,sovi_total) %>%
  mutate(wt_sovi_total=round(sovi_total*sub_perc,3)) %>%
  group_by(SUB) %>% summarize(area_wt_sovi=sum(wt_sovi_total))

# sovi theme 1
yadkin_sovi_theme1_sub_data=yadkin_sovi_data %>%
  select(SUB,fips,tract_perc,sub_perc,sovi_theme1) %>%
  mutate(wt_sovi_theme1=round(sovi_theme1*sub_perc,3)) %>%
  group_by(SUB) %>% summarize(area_wt_sovi=sum(wt_sovi_theme1)) %>%
  mutate(theme="theme1_socio_econ_demog")

# sovi theme 2
yadkin_sovi_theme2_sub_data=yadkin_sovi_data %>%
  select(SUB,fips,tract_perc,sub_perc,sovi_theme2) %>%
  mutate(wt_sovi_theme2=round(sovi_theme2*sub_perc,3)) %>%
  group_by(SUB) %>% summarize(area_wt_sovi=sum(wt_sovi_theme2)) %>%
  mutate(theme="theme2_household_ability_demog")

# sovi theme 3
yadkin_sovi_theme3_sub_data=yadkin_sovi_data %>%
  select(SUB,fips,tract_perc,sub_perc,sovi_theme3) %>%
  mutate(wt_sovi_theme3=round(sovi_theme3*sub_perc,3)) %>%
  group_by(SUB) %>% summarize(area_wt_sovi=sum(wt_sovi_theme3)) %>%
  mutate(theme="theme3_minority_esl_demog")

# sovi theme 4
yadkin_sovi_theme4_sub_data=yadkin_sovi_data %>%
  select(SUB,fips,tract_perc,sub_perc,sovi_theme4) %>%
  mutate(wt_sovi_theme4=round(sovi_theme4*sub_perc,3)) %>%
  group_by(SUB) %>% summarize(area_wt_sovi=sum(wt_sovi_theme4)) %>%
  mutate(theme="theme4_housing_transportation_demog")

# themes 1 to 4 together
yadkin_sovi_themes_sub_data=bind_rows(yadkin_sovi_theme1_sub_data,
                                      yadkin_sovi_theme2_sub_data,
                                      yadkin_sovi_theme3_sub_data,
                                      yadkin_sovi_theme4_sub_data)

# join with gis data
yadkin_sub_shp_sovi_total=left_join(yadkin_sub_shp,yadkin_sovi_total_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme1=left_join(yadkin_sub_shp,yadkin_sovi_theme1_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme2=left_join(yadkin_sub_shp,yadkin_sovi_theme2_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme3=left_join(yadkin_sub_shp,yadkin_sovi_theme3_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme4=left_join(yadkin_sub_shp,yadkin_sovi_theme4_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme1to4=left_join(yadkin_sub_shp,yadkin_sovi_themes_sub_data,by="SUB")
#glimpse(yadkin_sub_shp_sovi_total)
#glimpse(yadkin_sub_shp_sovi_theme1)
#glimpse(yadkin_sub_shp_sovi_theme2)
#glimpse(yadkin_sub_shp_sovi_theme3)
#glimpse(yadkin_sub_shp_sovi_theme4)
#glimpse(yadkin_sub_shp_sovi_theme1to4)


# ---- plot subbasin scaled sovi data ----

# total sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_total_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_total,aes(fill=area_wt_sovi_total)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_total is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd Total SoVI",na.value="grey75",high="darkred",low="white",limits=c(2,13)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#        text=element_text(size=16))
dev.off()

# theme 1 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme1_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme1,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme1 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 1",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
#theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#      text=element_text(size=16))
dev.off()

# theme 2 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme2_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme2,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme2 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 2",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()

# theme 3 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme3_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme3,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme3 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 3",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()

# theme 4 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme4_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme4,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 3",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()

# all four themes together in one plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme1to4_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme1to4,aes(fill=area_wt_sovi)) +
  geom_sf() +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI",na.value="grey75",high="darkred",low="white",limit=c(0,4)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()