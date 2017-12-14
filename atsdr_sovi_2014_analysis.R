# yadkin sovi analysis (atsdr 2010-2014 tract level data)

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

# atsdr sovi tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_raw=read_csv("sovibd_2014_scaling_allsubs.csv",col_names=TRUE)
#don't need tract column?

# atsdr sovi data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
us_atsdr_sovi_data=read_csv("atsdr_us_sovi_data.csv",col_names=TRUE)

# gis data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
#yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)
#yadkin_tracts_shp_albers=read_sf("yadkin_sovi2014_albers.shp",quiet=TRUE)
yadkin_tracts_shp=read_sf("yadkin_sovi2014_utm17N.shp",quiet=TRUE)
#yadkin_counties_shp_albers=read_sf("yadkin_clip_counties_albers.shp",quiet=TRUE)
#yadkin_counties_shp=read_sf("yadkin_clip_counties_utm17N.shp",quiet=TRUE)
#glimpse(yadkin_counties_shp_albers)
#glimpse(yadkin_subs_shp)
#glimpse(yadkin_counties_shp_albers)
#glimpse(yadkin_tracts_shp_albers)
#nc_counties_shp=read_sf("nc_counties_utm17N.shp",quiet=TRUE)
#glimpse(nc_counties_shp)

# looking at gis data
#yadkin_subs_shp_albers_geom=st_geometry(yadkin_subs_shp_albers)
#attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
#yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
#attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2 reformat data -----

# remove X1 column
sovidb_scaling_data=sovibd_scaling_raw %>% select(-X1)

# make sure fips columns are same name and are as.numeric()
yadkin_tracts_shp=yadkin_tracts_shp %>% mutate(fips=as.numeric(FIPS)) %>% select(-FIPS)
yadkin_subs_shp=yadkin_subs_shp %>% 
  mutate(SUB=Subbasin) %>% 
  select(-Subbasin)

# select atsdr data for yadkin
yadkin_atsdr_sovi_data=left_join(sovidb_scaling_data,us_atsdr_sovi_data,by="fips")


# ---- plot sovi data by tract ----

# total sovi by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_total_sovi_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tracts_shp,aes(fill=SPL_THEMES)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Total SoVI",na.value="grey75",high="darkred",low="white",limits=c(2,13)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()



# ---- scale sovi data to subbasin ----

# scale to subbasin
# total sovi
yadkin_atsdr_sovi_total_scaled_data=yadkin_atsdr_sovi_data %>%
  select(SUB,fips,tract_perc,sub_perc,sovi_total) %>%
  mutate(wt_sovi_total=round(sovi_total*sub_perc,3)) %>%
  group_by(SUB) %>% summarize(area_wt_sovi_total=sum(wt_sovi_total))

# join with gis data
yadkin_subs_shp_sovi_total=left_join(yadkin_subs_shp,yadkin_atsdr_sovi_total_scaled_data,by="SUB")
#glimpse(yadkin_tracts_shp_sovi)

# total sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_total_sovi_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_sovi_total,aes(fill=area_wt_sovi_total)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd Total SoVI",na.value="grey75",high="darkred",low="white",limits=c(5,10)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()




# ---- extra ----







# map plot template
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("no_flow_change_using_baseline_bc_up.pdf",width=11,height=8.5)
ggplot(yadkin_atsdr_sovi_data,aes(fill=sovi_total)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",na.value="grey75",high="darkred",low="darkblue") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
#dev.off()

