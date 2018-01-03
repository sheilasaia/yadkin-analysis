# yadkin sovi analysis (atsdr 2010-2014 tract level data)
# last updated 20180103

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


# ---- 3.1 combine us, nc, and yadkin tract sovi data for histogram ----

# find unique fips id's for yadkin
yadkin_unique_fips=sovidb_scaling_data %>% select(fips) %>% distinct()

# find unique fips id's for nc
nc_unique_fips=us_sovi_data %>% filter(state_abbrev=="NC") %>% select(fips) %>% distinct()

# select sovi data for yadkin using unique fips id's
yadkin_sovi_hist=us_sovi_data %>% select(fips,sovi_total) %>% 
  right_join(yadkin_unique_fips,by="fips") %>% mutate(dataset="UYPD")

# select sovi data for nc using unique fips id's
nc_sovi_hist=us_sovi_data %>% select(fips,sovi_total) %>% 
  right_join(nc_unique_fips,by="fips") %>% mutate(dataset="NC")

# select sovi data for us using unique fips id's
us_sovi_hist=us_sovi_data %>% select(fips,sovi_total) %>% mutate(dataset="US")

# bind all together
sovi_hist=bind_rows(yadkin_sovi_hist,nc_sovi_hist,us_sovi_hist)

# ---- 3.2 plot us, nc, yadkin tract sovi data as histogram ----

setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi2014_hist.pdf",width=11,height=8.5)
sovi_hist$dataset=factor(sovi_hist$dataset,levels=c("US","NC","UYPD"))
ggplot(sovi_hist,aes(sovi_total,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI (Census Tract Scale)") +
  ylab("Count") +
  xlim(0,15) +
  coord_cartesian(ylim=c(0,8000)) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi2014_hist_zoom.pdf",width=11,height=8.5)
sovi_hist$dataset=factor(sovi_hist$dataset,levels=c("US","NC","UYPD"))
ggplot(sovi_hist,aes(sovi_total,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI (Census Tract Scale)") +
  ylab("Count") +
  xlim(0,15) +
  coord_cartesian(ylim=c(0,250)) + # allows you to zoom without cutting out data
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())
dev.off()


# ---- 4.1 extract tract data to plot all themes together on one plot ----

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

# ---- 4.2 plot sovi data by tract ----

# total sovi by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_total_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp,aes(fill=SPL_THEMES)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_tract_shp is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Total SoVI (2010-2014)",na.value="grey75",high="darkred",low="white",limits=c(2,13)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#         text=element_text(size=16))
dev.off()

# sovi theme 1 (socioeconomic status) by tract
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_theme1_by_tract.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme2_by_tract.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme3_by_tract.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme4_by_tract.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme1to4_by_tract.pdf",width=11,height=8.5)
ggplot(yadkin_tract_shp_sovi_theme1to4,aes(fill=sovi)) +
  geom_sf() +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI (2010-2014)",na.value="grey75",high="darkred",low="white",limit=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()


# ---- 5.1 scale sovi data to subbasin ----

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


# ---- 5.2 plot subbasin scaled sovi data ----

# total sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_total_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_total,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_total is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd Total SoVI (2010-2014)",na.value="grey75",high="darkred",low="white",limits=c(2,13)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#        text=element_text(size=16))
dev.off()

# theme 1 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_theme1_by_sub.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme2_by_sub.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme3_by_sub.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme4_by_sub.pdf",width=11,height=8.5)
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
cairo_pdf("yadkin_sovi2014_theme1to4_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme1to4,aes(fill=area_wt_sovi)) +
  geom_sf() +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI",na.value="grey75",high="darkred",low="white",limit=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()


# ---- 6.1 import high flow and low flow outlier percent change data ----

# set working direcorty and input data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
hiflow_change_data=read_csv("hiflow_outlier_perc_change_data.csv",col_names=TRUE)
lowflow_change_data=read_csv("lowflow_outlier_perc_change_data.csv",col_names=TRUE)


# ---- 6.2 reclass flow and sovi data for plotting ----

# calculate mean sovi for us
mean_us_sovi=mean(us_sovi_hist$sovi_total)
sd_us_sovi=sd(us_sovi_hist$sovi_total)
min_us_sovi=min(us_sovi_hist$sovi_total)
max_us_sovi=max(us_sovi_hist$sovi_total)

# define number of years
num_yrs=21 # baseline and projection are both 21 years

# high flow data
hiflow_change_data_sel=hiflow_change_data %>%
  select(SUB:minor_outlier_perc_change) %>% # select minor outliers from flow data 
  mutate(minor_outlier_perc_change_per_yr=minor_outlier_perc_change/num_yrs) %>% # per year output
  left_join(yadkin_sovi_total_sub_data,by="SUB") %>% # join area weighted sovi
  mutate(vuln_class=ifelse(area_wt_sovi>mean_us_sovi,"high","low")) %>% # make new class variable based on us sovi mean
  mutate(risk_class=ifelse(minor_outlier_perc_change_per_yr>0,"high","low")) %>% # make new class variable based on outlier flows
  mutate(risk_vuln_class=ifelse(vuln_class=="high"&risk_class=="high","high",
                                ifelse(vuln_class=="low"&risk_class=="low","low",
                                       ifelse(vuln_class=="low"&risk_class=="high","med",
                                              ifelse(vuln_class=="high"&risk_class=="low","med","NA"))))) # combine risk and vulnerability
# low flow data
lowflow_change_data_sel=lowflow_change_data %>%
  select(SUB:minor_outlier_perc_change) %>% # select minor outliers from flow data 
  mutate(minor_outlier_perc_change_per_yr=minor_outlier_perc_change/num_yrs) %>% # per year output
  left_join(yadkin_sovi_total_sub_data,by="SUB") %>% # join area weighted sovi
  mutate(vuln_class=ifelse(area_wt_sovi>mean_us_sovi,"high","low")) %>% # make new class variable based on us sovi mean
  mutate(risk_class=ifelse(minor_outlier_perc_change_per_yr>0,"high","low")) %>% # make new class variable based on outlier flows
  mutate(risk_vuln_class=ifelse(vuln_class=="high"&risk_class=="high","high",
                                ifelse(vuln_class=="low"&risk_class=="low","low",
                                       ifelse(vuln_class=="low"&risk_class=="high","med",
                                              ifelse(vuln_class=="high"&risk_class=="low","med","NA"))))) # combine risk and vulnerability

# ---- 6.3 point cloud of reclassified data ----

# omit na's for plotting
hiflow_change_data_sel_naomit=hiflow_change_data_sel %>% na.omit()
lowflow_change_data_sel_naomit=lowflow_change_data_sel %>% na.omit()

# define factor levels
hiflow_change_data_sel_naomit$risk_vuln_class=factor(hiflow_change_data_sel_naomit$risk_vuln_class,levels=c("high","med","low"))
hiflow_change_data_sel_naomit$dataset=factor(hiflow_change_data_sel_naomit$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
lowflow_change_data_sel_naomit$risk_vuln_class=factor(lowflow_change_data_sel_naomit$risk_vuln_class,levels=c("high","med","low"))
lowflow_change_data_sel_naomit$dataset=factor(lowflow_change_data_sel_naomit$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# high flow data
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_risk_vuln_per_yr_pointplot.pdf",width=11,height=8.5)
ggplot(data=hiflow_change_data_sel_naomit,
       mapping=aes(x=area_wt_sovi,y=minor_outlier_perc_change_per_yr,color=risk_vuln_class)) +
  geom_point(aes(shape=dataset),size=4,alpha=0.75) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=mean_us_sovi) +
  labs(x="Subbasin SoVI",y="% Change in Number of Minor HOFs/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-10,60) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("black","grey50","grey75")) +
  #scale_fill_manual(values=rep("black",4)) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=16))
dev.off()

# low flow data
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_risk_vuln_per_yr_pointplot.pdf",width=11,height=8.5)
ggplot(data=lowflow_change_data_sel_naomit,
       mapping=aes(x=area_wt_sovi,y=minor_outlier_perc_change_per_yr,color=risk_vuln_class,shape=dataset)) +
  geom_point(size=4,alpha=0.75) +
  #geom_point(shape=1,size=3,color="black") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=mean_us_sovi) +
  labs(x="Subbasin SoVI",y="% Change in Number of Minor LOFs/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-10,220) +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("black","grey50","grey75")) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=16))
dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_risk_vuln_per_yr_pointplot_zoom.pdf",width=11,height=8.5)
ggplot(data=lowflow_change_data_sel_naomit,
       mapping=aes(x=area_wt_sovi,y=minor_outlier_perc_change_per_yr,color=risk_vuln_class,shape=dataset)) +
  geom_point(size=4,alpha=0.75) +
  #geom_point(shape=1,size=3,color="black") +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=mean_us_sovi) +
  labs(x="Subbasin SoVI",y="% Change in Number of Minor LOFs/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-10,100) +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("black","grey50","grey75")) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=16))
dev.off()


# ---- 6.4 spatial distribution of reclassified data ----

# add to shp file
yadkin_sub_shp_risk_vuln_hiflow=left_join(yadkin_sub_shp,hiflow_change_data_sel,by="SUB")
#glimpse(yadkin_sub_shp_risk_vuln_hiflow)
yadkin_sub_shp_risk_vuln_lowflow=left_join(yadkin_sub_shp,lowflow_change_data_sel,by="SUB")
#glimpse(yadkin_sub_shp_risk_vuln_lowflow)

# define factor levels
yadkin_sub_shp_risk_vuln_hiflow$risk_vuln_class=factor(yadkin_sub_shp_risk_vuln_hiflow$risk_vuln_class,levels=c("high","med","low"))
yadkin_sub_shp_risk_vuln_hiflow$dataset=factor(yadkin_sub_shp_risk_vuln_hiflow$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
yadkin_sub_shp_risk_vuln_lowflow$risk_vuln_class=factor(yadkin_sub_shp_risk_vuln_lowflow$risk_vuln_class,levels=c("high","med","low"))
yadkin_sub_shp_risk_vuln_lowflow$dataset=factor(yadkin_sub_shp_risk_vuln_lowflow$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# high flow data
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_risk_vuln_per_yr_map.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_risk_vuln_hiflow,aes(fill=risk_vuln_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_risk_vuln_hiflow is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values=c("red","orange","yellow"),na.value="grey75") +
  theme_bw()
dev.off()

# low flow data
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_risk_vuln_per_yr_map.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_risk_vuln_lowflow,aes(fill=risk_vuln_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_risk_vuln_lowflow is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values=c("red","orange","yellow"),na.value="grey75") +
  theme_bw()
dev.off()

