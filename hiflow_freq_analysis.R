# yadkin high flow frequency analysis

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(smwrBase)
library(tidyverse)
#devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
#library(ggplot2)
library(sf)

# load home-made functions 
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path,"reformat_rch_file.R")) # reformat SWAT .rch file
source(paste0(functions_path,"logpearson3_factor_calc.R")) # calculate log-Pearson III frequency factors
source(paste0(functions_path,"remove_outliers.R")) # removes low and high flows deemed as outliers
source(paste0(functions_path,"obs_hiflow_freq_calcs_one_rch.R")) # select observations for one reach
source(paste0(functions_path,"obs_freq_calcs_all_rchs.R")) # selects observations for all reaches
source(paste0(functions_path,"model_hiflow_freq_calcs_one_rch.R")) # determines high flow model for one reach
source(paste0(functions_path,"model_freq_calcs_all_rchs.R")) # determines flow model for all reaches
source(paste0(functions_path,"flow_change.R")) # determines % change in flows for a given return period
source(paste0(functions_path,"count_hiflow_outliers.R")) # counts number of minor and major outliers for risk analysis
source(paste0(functions_path,"count_hiflow_outliers_using_baseline.R")) # counts number of minor and major outliers for risk analysis based on baseline cutoffs
source(paste0(functions_path,"outlier_change.R")) # determines % change in minor and major outliers

# download kn_table for outlier analysis
setwd("/Users/ssaia/Documents/GitHub/yadkin-analysis/")
kn_table=read_csv("kn_table_appendix4_usgsbulletin17b.csv",col_names=TRUE)

# set directory and load data
# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
#baseline_sub_raw_data=read_table2("output.sub",col_names=FALSE,skip=9) # baseline .sub file from SWAT
baseline_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#yadkin_net_raw_data=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# miroc rcp 8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/C_CSIRO45")
csiro4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# hadley rcp 4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/D_Hadley45")
hadley4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)

# looking at gis data
yadkin_subs_shp_albers_geom=st_geometry(yadkin_subs_shp_albers)
#attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
#attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2 reformat data ----

# reach file (.rch) 
# baseline data
baseline_rch_data=reformat_rch_file(baseline_rch_raw_data)

# mirco 8.5 data
miroc8_5_rch_data=reformat_rch_file(miroc8_5_rch_raw_data)

# csiro 8.5 data
csiro8_5_rch_data=reformat_rch_file(csiro8_5_rch_raw_data)

# csiro 4.5 data
csiro4_5_rch_data=reformat_rch_file(csiro4_5_rch_raw_data)

# hadley 4.5 data
hadley4_5_rch_data=reformat_rch_file(hadley4_5_rch_raw_data)


# shape file (.shp)
# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)

# join areas for yadkin_net_data (=weights)
#yadkin_sub_areas=baseline_sub_data %>% select(SUB,AREAkm2) %>% distinct()
#yadkin_net_data=left_join(yadkin_net_data_sel,yadkin_sub_areas,by="SUB") %>%
#  select(FROM_NODE,TO_NODE,AREAkm2) # remove SUB b/c FROM_NODE=SUB


# ---- 3.1 calculate obs and model ouptuts for each subbasin ----

baseline_obs_calcs=obs_freq_calcs_all_rchs(baseline_rch_data,1,"hiflow")
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_calcs=model_freq_calcs_all_rchs(baseline_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

miroc8_5_obs_calcs=obs_freq_calcs_all_rchs(miroc8_5_rch_data,1,"hiflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
miroc8_5_model_calcs=model_freq_calcs_all_rchs(miroc8_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

csiro8_5_obs_calcs=obs_freq_calcs_all_rchs(csiro8_5_rch_data,1,"hiflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro8_5_model_calcs=model_freq_calcs_all_rchs(csiro8_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

csiro4_5_obs_calcs=obs_freq_calcs_all_rchs(csiro4_5_rch_data,1,"hiflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro4_5_model_calcs=model_freq_calcs_all_rchs(csiro4_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

hadley4_5_obs_calcs=obs_freq_calcs_all_rchs(hadley4_5_rch_data,1,"hiflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
hadley4_5_model_calcs=model_freq_calcs_all_rchs(hadley4_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")


# ---- 3.2 plot results for each subbasin ----

# plot observations and models together
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_max_flow_cms_adj),baseline_obs_calcs,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_calcs,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_calcs,color="green") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_calcs,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_calcs,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_calcs,color="blue") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  theme_bw()


# ---- 4.1 calculate % change in flows ----

# miroc 8.5
miroc8_5_10yr_flow=flow_change(10,baseline_model_calcs,miroc8_5_model_calcs)
miroc8_5_100yr_flow=flow_change(100,baseline_model_calcs,miroc8_5_model_calcs)

# csiro 8.5
csiro8_5_10yr_flow=flow_change(10,baseline_model_calcs,csiro8_5_model_calcs)
csiro8_5_100yr_flow=flow_change(100,baseline_model_calcs,csiro8_5_model_calcs)

# csiro 4.5
csiro4_5_10yr_flow=flow_change(10,baseline_model_calcs,csiro4_5_model_calcs)
csiro4_5_100yr_flow=flow_change(100,baseline_model_calcs,csiro4_5_model_calcs)

# hadley 4.5
hadley4_5_10yr_flow=flow_change(10,baseline_model_calcs,hadley4_5_model_calcs)
hadley4_5_100yr_flow=flow_change(100,baseline_model_calcs,hadley4_5_model_calcs)


# ---- 4.2 reformat calculations for plots ----

# 10 yr flow
# select data to add to shp file
miroc8_5_10yr_flow_sel=miroc8_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="miroc8_5",perc_change=flow_change_perc)
csiro8_5_10yr_flow_sel=csiro8_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro8_5",perc_change=flow_change_perc)
csiro4_5_10yr_flow_sel=csiro4_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro4_5",perc_change=flow_change_perc)
hadley4_5_10yr_flow_sel=hadley4_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="hadley4_5",perc_change=flow_change_perc)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# gather projections
hiflow_10yr_projections=bind_rows(miroc8_5_10yr_flow_sel,
                                 csiro8_5_10yr_flow_sel,
                                 csiro4_5_10yr_flow_sel,
                                 hadley4_5_10yr_flow_sel)

# add to shp file
yadkin_subs_shp_hiflow_10yr=left_join(yadkin_subs_shp,hiflow_10yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_10yr)


# 100 yr flow
# select data to add to shp file
miroc8_5_100yr_flow_sel=miroc8_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="miroc8_5",perc_change=flow_change_perc)
csiro8_5_100yr_flow_sel=csiro8_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro8_5",perc_change=flow_change_perc)
csiro4_5_100yr_flow_sel=csiro4_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro4_5",perc_change=flow_change_perc)
hadley4_5_100yr_flow_sel=hadley4_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="hadley4_5",perc_change=flow_change_perc)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# gather projections
hiflow_100yr_projections=bind_rows(miroc8_5_100yr_flow_sel,
                                  csiro8_5_100yr_flow_sel,
                                  csiro4_5_100yr_flow_sel,
                                  hadley4_5_100yr_flow_sel)

# add to shp file
yadkin_subs_shp_hiflow_100yr=left_join(yadkin_subs_shp,hiflow_100yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_100yr)

# ---- 4.3 plot % change in flows on map ----

# 10 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_10yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_10yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_10yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 10yr Flow",na.value="grey75",limits=c(-60,60)) +
  theme_bw() #+
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  #theme(text = element_text(size = 20))
dev.off()

# 100 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_100yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_100yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_100yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 100yr Flow",na.value="grey75") +
  theme_bw()
dev.off()


# ---- 4.4 export results for sovi analysis ----

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(hiflow_10yr_projections,"hiflow_10yr_perc_change.csv")
#write_csv(hiflow_10yr_projections,"hiflow_100yr_perc_change.csv")


# ---- 5.1 calculate outlier cutoffs and number of outlier high flows (not using and using basesline) ----

# baseline
baseline_outlier_calcs=count_hiflow_outliers(baseline_rch_data)
baseline_outlier_counts=baseline_outlier_calcs[[1]]
baseline_outlier_cutoffs=baseline_outlier_calcs[[2]]

# baseline (only 21 most recent years)
baseline_rch_data_21yrs=baseline_rch_data %>% filter(YR>1987)
baseline_outlier_calcs_21yrs=count_hiflow_outliers(baseline_rch_data_21yrs)
baseline_outlier_counts_21yrs=baseline_outlier_calcs_21yrs[[1]]
baseline_outlier_cutoffs_21yrs=baseline_outlier_calcs_21yrs[[2]]

# miroc 8.5
miroc8_5_outlier_calcs=count_hiflow_outliers(miroc8_5_rch_data) # find outliers using cutoff from data itself
miroc8_5_outlier_counts=miroc8_5_outlier_calcs[[1]]
miroc8_5_outlier_cutoffs=miroc8_5_outlier_calcs[[2]]
miroc8_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(baseline_outlier_cutoffs_21yrs,miroc8_5_rch_data) # find outliers using baseline cutoff
miroc8_5_outlier_counts_using_baseline=miroc8_5_outlier_calcs_using_baseline[[1]]
miroc8_5_outlier_cutoffs_using_baseline=miroc8_5_outlier_calcs_using_baseline[[2]]

# csiro 8.5
csiro8_5_outlier_calcs=count_hiflow_outliers(csiro8_5_rch_data)
csiro8_5_outlier_counts=csiro8_5_outlier_calcs[[1]]
csiro8_5_outlier_cutoffs=csiro8_5_outlier_calcs[[2]]
csiro8_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(baseline_outlier_cutoffs_21yrs,csiro8_5_rch_data) # find outliers using baseline cutoff
csiro8_5_outlier_counts_using_baseline=csiro8_5_outlier_calcs_using_baseline[[1]]
csiro8_5_outlier_cutoffs_using_baseline=csiro8_5_outlier_calcs_using_baseline[[2]]

# csiro 4.5
csiro4_5_outlier_calcs=count_hiflow_outliers(csiro4_5_rch_data)
csiro4_5_outlier_counts=csiro4_5_outlier_calcs[[1]]
csiro4_5_outlier_cutoffs=csiro4_5_outlier_calcs[[2]]
csiro4_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(baseline_outlier_cutoffs_21yrs,csiro4_5_rch_data) # find outliers using baseline cutoff
csiro4_5_outlier_counts_using_baseline=csiro4_5_outlier_calcs_using_baseline[[1]]
csiro4_5_outlier_cutoffs_using_baseline=csiro4_5_outlier_calcs_using_baseline[[2]]

# hadley 4.5
hadley4_5_outlier_calcs=count_hiflow_outliers(hadley4_5_rch_data)
hadley4_5_outlier_counts=hadley4_5_outlier_calcs[[1]]
hadley4_5_outlier_cutoffs=hadley4_5_outlier_calcs[[2]]
hadley4_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(baseline_outlier_cutoffs_21yrs,hadley4_5_rch_data) # find outliers using baseline cutoff
hadley4_5_outlier_counts_using_baseline=hadley4_5_outlier_calcs_using_baseline[[1]]
hadley4_5_outlier_cutoffs_using_baseline=hadley4_5_outlier_calcs_using_baseline[[2]]


# ---- 5.2 calculate % change in outlier high flows (not using baseline) ----

# sum outlier counts data by subbasin
baseline_outlier_counts_sum=baseline_outlier_counts %>% filter(YR>1987) %>% 
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="baseline")
# baseline has to be cut down to most recent time period (1988-2008) to timeframe compares to projections
miroc8_5_outlier_counts_sum=miroc8_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="miroc8_5")
csiro8_5_outlier_counts_sum=csiro8_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro8_5")
csiro4_5_outlier_counts_sum=csiro4_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro4_5")
hadley4_5_outlier_counts_sum=hadley4_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="hadley4_5")

# calculate % change 
miroc8_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,miroc8_5_outlier_counts_sum)
csiro8_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,csiro8_5_outlier_counts_sum)
csiro4_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,csiro4_5_outlier_counts_sum)
hadley4_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,hadley4_5_outlier_counts_sum)

# bind rows
hiflow_outlier_change_projections=bind_rows(miroc8_5_hiflow_outlier_change,
                                           csiro8_5_hiflow_outlier_change,
                                           csiro4_5_hiflow_outlier_change,
                                           hadley4_5_hiflow_outlier_change) %>% mutate(SUB=RCH) %>% select(-RCH)

# add to shp file
yadkin_subs_shp_hiflow_outliers=left_join(yadkin_subs_shp,hiflow_outlier_change_projections,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_outliers)


# ---- 5.3 calculate % change in outlier high flows (using baseline) ----

# sum outlier counts data by subbasin
baseline_outlier_counts_sum_2=baseline_outlier_counts_21yrs %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="baseline")
# baseline has to be cut down to most recent time period (1988-2008) to timeframe compares to projections
miroc8_5_outlier_counts_using_baseline_sum=miroc8_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="miroc8_5")
csiro8_5_outlier_counts_using_baseline_sum=csiro8_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro8_5")
csiro4_5_outlier_counts_using_baseline_sum=csiro4_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro4_5")
hadley4_5_outlier_counts_using_baseline_sum=hadley4_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="hadley4_5")

# calculate % change 
miroc8_5_hiflow_outlier_change_using_baseline=outlier_change(baseline_outlier_counts_sum_2,miroc8_5_outlier_counts_using_baseline_sum)
csiro8_5_hiflow_outlier_change_using_baseline=outlier_change(baseline_outlier_counts_sum_2,csiro8_5_outlier_counts_using_baseline_sum)
csiro4_5_hiflow_outlier_change_using_baseline=outlier_change(baseline_outlier_counts_sum_2,csiro4_5_outlier_counts_using_baseline_sum)
hadley4_5_hiflow_outlier_change_using_baseline=outlier_change(baseline_outlier_counts_sum_2,hadley4_5_outlier_counts_using_baseline_sum)

# bind rows
hiflow_outlier_change_using_baseline_projections=bind_rows(miroc8_5_hiflow_outlier_change_using_baseline,
                                                           csiro8_5_hiflow_outlier_change_using_baseline,
                                                           csiro4_5_hiflow_outlier_change_using_baseline,
                                                           hadley4_5_hiflow_outlier_change_using_baseline) %>% mutate(SUB=RCH) %>% select(-RCH)

# add to shp file
yadkin_subs_shp_hiflow_outliers_using_baseline=left_join(yadkin_subs_shp,hiflow_outlier_change_using_baseline_projections,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_outliers_using_baseline)



# ---- 5.4 plot % change in outlier high flows on map (not using baseline) ----

# minor outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_minor_outlier_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers,aes(fill=minor_outlier_perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Minor High Flow Outliers",na.value="grey75",limits=c(-10,120)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# major outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_major_outlier_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers,aes(fill=major_outlier_perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Major High Flow Outliers",na.value="grey75",limits=c(-20,300)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()


# ---- 5.5 plot % change in outlier high flows on map (using baseline) ----

# minor outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_minor_outlier_change_using_baseline.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers_using_baseline,aes(fill=minor_outlier_perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers_using_baseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Minor High Flow Outliers",na.value="grey75") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# major outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_major_outlier_change_using_baseline.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers_using_baseline,aes(fill=major_outlier_perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers_using_baseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Major High Flow Outliers",na.value="grey75") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()


# ---- 5.x plot flow distrubutions and cutoffs for outlet ----




blah$dataset=factor(blah$dataset,levels=c("major_outlier","minor_outlier","all_data"))
ggplot(blah,aes(x=FLOW_OUTcms,fill=dataset)) +
  geom_density(alpha=0.75) + #joyplot
  xlab("Flow Out (cms)") +
  ylab("Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# i don't think this density overlay is accurate b/c i want it to be on the same density scale
ggplot(blah,aes(x=FLOW_OUTcms)) +
  geom_density(alpha=0.75,fill="grey75") +
  #geom_vline(xintercept=hibound_minor_outlier,color="black",linetype=2) +
  #geom_vline(xintercept=hibound_major_outlier,color="black",linetype=1) +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") +
  ylab("Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(baseline_outlet_outlier_summary,aes(x=YR,y=n_minor_hiflow)) +
  geom_bar(stat="identity") +
  #facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Year") +
  ylab("Number of Minor Outlier High Flows (1.5xIQR)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# ---- 5.x plot distributions of outflow for each subbasin by month and by year (Joyplot) ----




# select outlet data
baseline_outlet_rch_data=baseline_rch_data %>% filter(RCH==28)

library(ggridges) # https://cran.rstudio.com/web/packages/ggjoy/vignettes/introduction.html
#library(ggbeeswarm)

# by month (all subbasins)
ggplot(baseline_rch_data,aes(x=FLOW_OUTcms,y=as.factor(MO))) +
  geom_density_ridges2() + #joyplot
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# by month (outlet)
ggplot(baseline_outlet_rch_data,aes(x=FLOW_OUTcms,y=as.factor(MO))) +
  geom_density_ridges2() + #joyplot
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# by year (all subbasins)
ggplot(baseline_rch_data,aes(x=FLOW_OUTcms,y=as.factor(YR))) +
  geom_density_ridges2() + #joyplot
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# by year (outlet)
ggplot(baseline_outlet_rch_data,aes(x=FLOW_OUTcms,y=as.factor(YR))) +
  geom_density_ridges2() + #joyplot
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# how does variance change from year to year
baseline_outlet_variance_by_year=baseline_outlet_rch_data %>% group_by(YR) %>% summarise(variance=sd(FLOW_OUTcms))
ggplot(baseline_outlet_variance_by_year,aes(x=as.factor(YR),y=variance)) + 
  geom_point(size=3) + 
  geom_smooth(method="lm") + # this function adds the fitted line (w/ confidence interval)
  theme(axis.text.x=element_text(angle=90))
# no trend so no lm added?
  
# determine outliers for each subbasin: https://www.wikihow.com/Calculate-Outliers
# determine quartiles
# but then this still doesn't have any meaning in the real world
# maybe add in month vs flow for climate data sets and see how they compare? (as differently colored distributions)

# join baseline and projection data for overlapping joyplots
baseline_rch_data_sel=baseline_rch_data %>% select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="baseline")
csiro4_5_rch_data_sel=csiro4_5_rch_data %>% select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="csiro4_5")
csiro8_5_rch_data_sel=csiro8_5_rch_data %>% select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="csiro8_5")
hadley4_5_rch_data_sel=hadley4_5_rch_data %>% select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="hadley4_5")
miroc8_5_rch_data_sel=miroc8_5_rch_data %>% select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="miroc8_5")
all_rch_data=bind_rows(baseline_rch_data_sel,csiro4_5_rch_data_sel,csiro8_5_rch_data_sel,hadley4_5_rch_data_sel,miroc8_5_rch_data_sel)
all_rch_data$dataset=factor(all_rch_data$dataset,levels=c("baseline","miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
ggplot(all_rch_data,aes(x=FLOW_OUTcms,y=dataset,fill=dataset)) +
  geom_density_ridges2(alpha=0.5) + #joyplot
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") + 
  ylab("Dataset") +
  xlim(0,1000) +
  theme_bw()


#
all_outlet_rch_data=all_rch_data %>% filter(RCH==28)
ggplot(all_outlet_rch_data,aes(x=FLOW_OUTcms,y=as.factor(MO),fill=dataset)) +
  geom_density_ridges2(alpha=0.5) + #joyplot
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  xlim(0,1000) +
  theme_bw()

# ---- x. extra ----

sub_area=baseline_sub_data_raw %>% select(SUB,AREAkm2) %>% 
  transmute(SUB=SUB,sub_AREAkm2=round(AREAkm2,0)) %>% distinct()
rch_area=baseline_rch_data_raw %>% select(RCH,AREAkm2) %>% 
  transmute(RCH=RCH,rch_AREAkm2=round(AREAkm2,0)) %>% distinct()

subs_equal_rch=bind_cols(sub_area,rch_area) %>% filter(sub_AREAkm2==rch_AREAkm2)

test=bind_cols(sub_area,rch_area)


# network analysis help: http://www.shizukalab.com/toolkits/sna/plotting-directed-networks
# use network analysis graph to automate subbasin contributions of runoff?


# ---- x. function: find projection return period for baseline flow of a specified return period ----

# define function
return_period_diff=function(return_period,num_decimal_places,baseline_model_calcs,projection_model_calcs) {
  # return_period must be an entry in the modeled data
  
  # select only data for return period of interest
  baseline_return_period_sel=baseline_model_calcs %>% filter(model_return_period==return_period)
  projection_return_period_sel=projection_model_calcs %>% filter(model_return_period==return_period)
  
  # define variables and output dataframe
  return_period_range=seq(floor(min(baseline_model_calcs$model_return_period)),
                          floor(max(baseline_model_calcs$model_return_period)),1)
  num_subs=length(unique(baseline_model_calcs$SUB))
  diff_df=data.frame(SUB=as.integer(),
                     baseline_return_period=as.numeric(),
                     baseline_model_flow_cms=as.numeric(),
                     projection_return_period=as.numeric(),
                     projection_model_flow_cms=as.numeric(),
                     base_minus_proj_return_period=as.numeric(),
                     note=as.character())
  
  # for loop for each subbasin
  for (i in 1:num_subs) {
    # use spline rather than analytical solution to approx. result
    baseline_funct_temp=as.data.frame(spline(baseline_model_calcs$model_return_period[baseline_model_calcs$SUB==i],
                                             baseline_model_calcs$model_flow_cms[baseline_model_calcs$SUB==i],
                                             n=length(return_period_range)*(10^num_decimal_places), method="natural"))
    projection_funct_temp=as.data.frame(spline(projection_model_calcs$model_return_period[projection_model_calcs$SUB==i],
                                               projection_model_calcs$model_flow_cms[projection_model_calcs$SUB==i],
                                               n=length(return_period_range)*(10^num_decimal_places), method="natural"))
    
    # baseline data for specified return period and subbasin
    baseline_sub_temp=baseline_return_period_sel %>% filter(SUB==i)
    baseline_sub_flow_temp=baseline_sub_temp$model_flow_cms
    
    # find projection return period for corresponding baseline flow
    diff_temp=abs(baseline_sub_flow_temp-projection_funct_temp$y)
    projection_return_period_temp=round(projection_funct_temp$x[match(min(diff_temp),diff_temp)],num_decimal_places)
    projection_flow_temp=round(projection_funct_temp$y[match(min(diff_temp),diff_temp)],num_decimal_places)
    
    # note potential error and calculate difference for those without error
    if (projection_return_period_temp>=return_period) {
      base_minus_proj_return_period_temp=NA
      note_temp="check model"
    } else {
      base_minus_proj_return_period_temp=return_period-projection_return_period_temp
      note_temp=""
    }
    
    # save results to data frame
    diff_df_temp=data.frame(SUB=i,baseline_return_period=return_period,
                            baseline_model_flow_cms=baseline_sub_flow_temp,
                            projection_return_period=projection_return_period_temp,
                            projection_model_flow_cms=projection_flow_temp,
                            base_minus_proj_return_period=base_minus_proj_return_period_temp,
                            note=note_temp)
    
    # bind results to diff_df
    diff_df=bind_rows(diff_df,diff_df_temp)
  }
  
  # return output
  return(diff_df)
}


# ---- x. calculate return period difference ----

# csiro 4.5
csiro4_5_10years=return_period_diff(10,2,baseline_model_calcs,csiro4_5_model_calcs)
csiro4_5_100years=return_period_diff(100,2,baseline_model_calcs,csiro4_5_model_calcs)

# csiro 8.5
csiro8_5_10years=return_period_diff(10,2,baseline_model_calcs,csiro8_5_model_calcs)
csiro8_5_100years=return_period_diff(100,2,baseline_model_calcs,csiro8_5_model_calcs)

# hadley 4.5
hadley4_5_10years=return_period_diff(10,2,baseline_model_calcs,hadley4_5_model_calcs)
hadley4_5_100years=return_period_diff(100,2,baseline_model_calcs,hadley4_5_model_calcs)

# miroc 8.5
miroc8_5_10years=return_period_diff(10,2,baseline_model_calcs,miroc8_5_model_calcs)
miroc8_5_100years=return_period_diff(100,2,baseline_model_calcs,miroc8_5_model_calcs)

