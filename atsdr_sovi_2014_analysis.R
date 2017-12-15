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

# load home-made functions
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
#source(paste0(functions_path,"risk_vuln_reclass.R")) # reclassify sovi analysis results
#source(paste0(functions_path,"rescale_minus1to1.R")) # rescale from -1 to 1

# atsdr sovi 2010-2014 tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_raw=read_csv("sovibd_2014_scaling_allsubs.csv",col_names=TRUE)
#don't need tract column?

# atsdr sovi 2010-2014 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
us_sovi_data=read_csv("atsdr_us_sovi_2014_data.csv",col_names=TRUE)

# gis data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_raw=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)
yadkin_tracts_shp_raw=read_sf("yadkin_sovi2014_utm17N.shp",quiet=TRUE)
# use Albers projection for calcs in ArcGIS but UTM 17N
# here for plotting because can sf() recognizes UTM and
# then can convert sf() is not recognizing Albers projection

# looking at gis data
#yadkin_subs_shp_raw_geom=st_geometry(yadkin_subs_shp_raw)
#attributes(yadkin_subs_shp_raw) # this has an epsg code!


# ---- 2 reformat data -----

# remove X1 column in scaling data
sovidb_scaling_data=sovibd_scaling_raw %>% select(-X1)

# make sure fips columns are same name and are as.numeric()
yadkin_tracts_shp=yadkin_tracts_shp_raw %>% 
  mutate(fips=as.numeric(FIPS)) %>% select(-FIPS)

# change subbasin id column to match other files
yadkin_subs_shp=yadkin_subs_shp_raw %>% 
  mutate(SUB=Subbasin) %>% 
  select(-Subbasin)

# select atsdr data for yadkin
yadkin_sovi_data=left_join(sovidb_scaling_data,us_sovi_data,by="fips")

# ---- extract 2010-2014 acs/census data from tract shp file ----

# copy data to new variable
yadkin_census_data=yadkin_tracts_shp %>% 
  select(fips,E_TOTPOP:E_DAYPOP)  %>% # select only columns you need 
  st_set_geometry(NULL) # set geometry as null to get df

# select only estimate columns
yadkin_census_est_data=
  


# ---- plot sovi data by tract ----

# total sovi by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_total_sovi_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tracts_shp,aes(fill=SPL_THEMES)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Total SoVI",na.value="grey75",high="darkred",low="white",limits=c(2,13)) +
  theme_bw() #+
  #theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
  #      text=element_text(size=16))
dev.off()

# sovi theme 1 (socioeconomic status) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme1_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tracts_shp,aes(fill=SPL_THEME1)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 1",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
  #theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
  #      text=element_text(size=16))
dev.off()

# sovi theme 2 (household make-up & disability) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme2_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tracts_shp,aes(fill=SPL_THEME2)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 2",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
  #theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
  #      text=element_text(size=16))
dev.off()

# sovi theme 3 (minority status & knowledge of english) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme3_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tracts_shp,aes(fill=SPL_THEME3)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 3",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
  #theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
  #      text=element_text(size=16))
dev.off()

# sovi theme 4 (housing and transportation) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi_theme4_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tracts_shp,aes(fill=SPL_THEME4)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI Theme 4",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
  #theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
  #      text=element_text(size=16))
dev.off()

# ---- scale sovi data to subbasin ----

# scale to subbasin
# total sovi
yadkin_sovi_total_scaled_data=yadkin_sovi_data %>%
  select(SUB,fips,tract_perc,sub_perc,sovi_total) %>%
  mutate(wt_sovi_total=round(sovi_total*sub_perc,3)) %>%
  group_by(SUB) %>% summarize(area_wt_sovi_total=sum(wt_sovi_total))

# join with gis data
yadkin_subs_shp_sovi_total=left_join(yadkin_subs_shp,yadkin_sovi_total_scaled_data,by="SUB")
#glimpse(yadkin_tracts_shp_sovi)

# total sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_total_sovi_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_sovi_total,aes(fill=area_wt_sovi_total)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd Total SoVI",na.value="grey75",high="darkred",low="white",limits=c(5,10)) +
  theme_bw() #+
  #theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
  #      text=element_text(size=16))
dev.off()



