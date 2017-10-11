# yadkin low flow frequency analysis

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
source(paste0(functions_path,"obs_lowflow_freq_calcs_one_rch.R")) # select observations for one reach
source(paste0(functions_path,"obs_freq_calcs_all_rchs.R")) # selects observations for all reaches
source(paste0(functions_path,"model_lowflow_freq_calcs_one_rch.R")) # determines lowflow model for one reach
source(paste0(functions_path,"model_freq_calcs_all_rchs.R")) # determines flow model for all reaches
source(paste0(functions_path,"flow_change.R")) # determines % change in flows for a given return period

# download kn_table for outlier analysis
setwd("/Users/ssaia/Documents/GitHub/yadkin-analysis/")
kn_table=read_csv("kn_table_appendix4_usgsbulletin17b.csv",col_names=TRUE)

# set directory and load data
# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
baseline_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#yadkin_net_data_raw=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# miroc rcp 8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 8.5 data
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
attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2 reformat data ----

# baseline data
baseline_rch_data=reformat_rch_file(baseline_rch_raw_data)

# miroc 8.5 data
miroc8_5_rch_data=reformat_rch_file(miroc8_5_rch_raw_data)

# csiro 8.5 data
csiro8_5_rch_data=reformat_rch_file(csiro8_5_rch_raw_data)

# csiro 4.5 data
csiro4_5_rch_data=reformat_rch_file(csiro4_5_rch_raw_data)

# hadley 4.5 data
hadley4_5_rch_data=reformat_rch_file(hadley4_5_rch_raw_data)

# gis data
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin) # add SUB column to .shp file
#glimpse(yadkin_subs_shp)


# ---- 3.1 calculate obs and model ouptuts for each subbasin ----

#baseline_rch_data_test=baseline_rch_data %>% filter(RCH<11)
#baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(baseline_rch_data_test,span_days=1,flow_option="lowflow")
baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(baseline_rch_data,span_days=1,flow_option="lowflow")
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_lowflow_calcs=model_freq_calcs_all_rchs(baseline_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

miroc8_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(miroc8_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
miroc8_5_model_lowflow_calcs=model_freq_calcs_all_rchs(miroc8_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

csiro8_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(csiro8_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro8_5_model_lowflow_calcs=model_freq_calcs_all_rchs(csiro8_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

csiro4_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(csiro4_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro4_5_model_lowflow_calcs=model_freq_calcs_all_rchs(csiro4_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

hadley4_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(hadley4_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
hadley4_5_model_lowflow_calcs=model_freq_calcs_all_rchs(hadley4_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")


# ---- 3.2 plot results for each subbasin ----

# omit zero values from observations
baseline_obs_lowflow_calcs_nozeros=baseline_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
miroc8_5_obs_lowflow_calcs_nozeros=miroc8_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
csiro8_5_obs_lowflow_calcs_nozeros=csiro8_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
csiro4_5_obs_lowflow_calcs_nozeros=csiro4_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
hadley4_5_obs_lowflow_calcs_nozeros=hadley4_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)

# omit NAs from model outputs
baseline_model_lowflow_calcs_noNAa=baseline_model_lowflow_calcs %>% na.omit()
miroc8_5_model_lowflow_calcs_noNAa=miroc8_5_model_lowflow_calcs %>% na.omit()
csiro8_5_model_lowflow_calcs_noNAa=csiro8_5_model_lowflow_calcs %>% na.omit()
csiro4_5_model_lowflow_calcs_noNAa=csiro4_5_model_lowflow_calcs %>% na.omit()
hadley4_5_model_lowflow_calcs_noNAa=hadley4_5_model_lowflow_calcs %>% na.omit()

# plot observations and models together (not log transformed)
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_models.pdf",width=11,height=8.5)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_lowflow_calcs_noNAa,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_lowflow_calcs_noNAa,color="green") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_lowflow_calcs_noNAa,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_lowflow_calcs_noNAa,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_lowflow_calcs_noNAa,color="blue") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  ylim(0,max(baseline_obs_lowflow_calcs$obs_min_flow_cms_adj)+25) +
  xlim(0,105) +
  theme_bw()
#dev.off()

# plot observations and models together (log transformed)
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_models_log.pdf",width=11,height=8.5)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_log_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),baseline_model_lowflow_calcs_noNAa,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),miroc8_5_model_lowflow_calcs_noNAa,color="green") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),csiro8_5_model_lowflow_calcs_noNAa,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),csiro4_5_model_lowflow_calcs_noNAa,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),hadley4_5_model_lowflow_calcs_noNAa,color="blue") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("return Period (yr)") + 
  ylab("Flow Out (log cms)") +
  ylim((min(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)-2),(max(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)+2)) +
  xlim(0,105) +
  theme_bw()
#dev.off()


# ---- 4.1 calculate % change in lowflows ----

# miroc 8.5
miroc8_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)
miroc8_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)

# csiro 8.5
csiro8_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)
csiro8_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)

# csiro 4.5
csiro4_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)
csiro4_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)

# hadley 4.5
hadley4_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)
hadley4_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)


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
lowflow_10yr_projections=bind_rows(miroc8_5_10yr_flow_sel,
                                   csiro8_5_10yr_flow_sel,
                                   csiro4_5_10yr_flow_sel,
                                   hadley4_5_10yr_flow_sel)

# add to shp file
yadkin_subs_shp_lowflow_10yr=left_join(yadkin_subs_shp,lowflow_10yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_10yr)


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
lowflow_100yr_projections=bind_rows(miroc8_5_100yr_flow_sel,
                                    csiro8_5_100yr_flow_sel,
                                    csiro4_5_100yr_flow_sel,
                                    hadley4_5_100yr_flow_sel)

# add to shp file
yadkin_subs_shp_lowflow_100yr=left_join(yadkin_subs_shp,lowflow_100yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_100yr)


# ---- 4.3 plot % change in lowflows on map ----

# 10 yr
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_10yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_10yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_10yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 10yr Lowflow",na.value="grey75") +
  theme_bw()
#dev.off()

# 100 yr
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_100yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_100yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_100yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 100yr Lowflow",na.value="grey75") +
  theme_bw()
#dev.off()


# ---- 5.1 count number of low flow frequency obs = zero ----

# baseline
baseline_num_yrs=length(unique(baseline_rch_data$YR))
baseline_obs_zero_counts=baseline_obs_lowflow_calcs %>%
  group_by(RCH) %>% summarise(cum_num_zero_entries=sum(obs_min_flow_cms_adj==0)) %>%
  mutate(num_zero_entries_per_yr=cum_num_zero_entries/baseline_num_yrs,
         dataset="baseline")

# miroc 8.5
miroc8_5_num_yrs=length(unique(miroc8_5_rch_data$YR))
miroc8_5_obs_zero_counts=miroc8_5_obs_lowflow_calcs %>% 
  group_by(RCH) %>% summarize(cum_num_zero_entries=sum(obs_min_flow_cms_adj==0)) %>%
  mutate(num_zero_entries_per_yr=cum_num_zero_entries/baseline_num_yrs,
         dataset="miroc_8_5")

# csiro 8.5
csiro8_5_num_yrs=length(unique(csiro8_5_rch_data$YR))
csiro8_5_obs_zero_counts=csiro8_5_obs_lowflow_calcs %>%
  group_by(RCH) %>% summarise(cum_num_zero_entries=sum(obs_min_flow_cms_adj==0)) %>%
  mutate(num_zero_entries_per_yr=cum_num_zero_entries/baseline_num_yrs,
         dataset="csiro_8_5")

# csiro 4.5
csiro4_5_num_yrs=length(unique(csiro4_5_rch_data$YR))
csiro4_5_obs_zero_counts=csiro4_5_obs_lowflow_calcs %>%
  group_by(RCH) %>% summarise(cum_num_zero_entries=sum(obs_min_flow_cms_adj==0)) %>%
  mutate(num_zero_entries_per_yr=cum_num_zero_entries/baseline_num_yrs,
         dataset="csiro_4_5")

# hadley 4.5
hadley4_5_num_yrs=length(unique(hadley4_5_rch_data$YR))
hadley4_5_obs_zero_counts=hadley4_5_obs_lowflow_calcs %>%
  group_by(RCH) %>% summarise(cum_num_zero_entries=sum(obs_min_flow_cms_adj==0)) %>%
  mutate(num_zero_entries_per_yr=cum_num_zero_entries/baseline_num_yrs,
         dataset="hadley_4_5")

# bind all together
all_models_zero_counts=bind_rows(baseline_obs_zero_counts,
                                 miroc8_5_obs_zero_counts,
                                 csiro8_5_obs_zero_counts,
                                 csiro4_5_obs_zero_counts,
                                 hadley4_5_obs_zero_counts)


# ---- 5.2 plot number of low flow frequency obs = zero ----

ggplot(all_models_zero_counts,aes(x=dataset,y=cum_num_zero_entries,fill=dataset)) +
  geom_col() +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("") +
  ylab("Cumulative Number of Zero Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))


# ---- 5.3 export results ----

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(all_models_zero_counts,"zero_flow_counts.csv")


# ---- 6.1 calculate lowflow counts below threshold ----

# find days where flow_out equals lowflow
my_threshold=0.01 # in cms

# create data frame to pad lowflow data
num_subs=28
num_baseline_yrs=27
my_yr_list=as.numeric()
str=1
for (i in 1982:2008) {
  my_yr_list[str:(str+num_subs-1)]=rep(i,num_subs)
  str=str+num_subs
}
baseline_pad_ids=data.frame(RCH=rep(seq(1:num_subs),num_baseline_yrs),
                             YR=my_yr_list) %>% 
  mutate(id=paste0(RCH,"_",YR)) %>% select(id)

# baseline data
baseline_lowflow_data=baseline_rch_data %>% filter(FLOW_OUTcms<my_threshold)
baseline_lowflow_thresh_counts=baseline_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="baseline")

# miroc 8.5
miroc8_5_lowflow_data=miroc8_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
miroc8_5_lowflow_thresh_counts=miroc8_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="miroc_8_5")

# csiro 8.5
csiro8_5_lowflow_data=csiro8_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
csiro8_5_lowflow_thresh_counts=csiro8_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="csiro_8_5")

# csiro 4.5
csiro4_5_lowflow_data=csiro4_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
csiro4_5_lowflow_thresh_counts=csiro4_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="csiro_4_5")

# hadley 4.5 select
hadley4_5_lowflow_data=hadley4_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
hadley4_5_lowflow_thresh_counts=hadley4_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="hadley_4_5")

# combine all
all_models_lowflow_thresh_counts=bind_rows(baseline_lowflow_thresh_counts,
                                       miroc8_5_lowflow_thresh_counts,
                                       csiro8_5_lowflow_thresh_counts,
                                       csiro4_5_lowflow_thresh_counts,
                                       hadley4_5_lowflow_thresh_counts)


# ---- 6.2 plot lowflow counts vs time (bar plot) ----

# baseline
ggplot(baseline_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
         geom_col() +
         facet_wrap(~RCH) +
         xlab("Year") +
         ylab("Baseline Number of Low Flow Events") +
         theme_bw()


# miroc 8.5
ggplot(miroc8_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("MIROC 8.5 Number of Low Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))

# csiro 8.5
ggplot(csiro8_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("CSIRO 8.5 Number of Low Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))

# csiro 4.5
ggplot(csiro4_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("CSIRO 4.5 Number of Low Flow Events") +
  theme_bw()

# hadley 4.5
ggplot(hadley4_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("Hadley 4.5 Number of Low Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))


# ---- 6.3 plot lowflow counts in space (map) ----

# select only necessary down data
all_models_lowflow_thresh_counts_sum=all_models_lowflow_thresh_counts %>% 
  select(RCH,YR,num_lowflow_days,dataset) %>%
  group_by(RCH,dataset) %>% 
  summarize(num_lowflow_days_per_yr=sum(num_lowflow_days)/20) %>%
  select(SUB=RCH,num_lowflow_days_per_yr=num_lowflow_days_per_yr,dataset=dataset)

# select only baseline
baseline_lowflow_thresh_counts_sum=all_models_lowflow_thresh_counts_sum %>%
  filter(dataset=="baseline")

# select only projections
all_projs_lowflow_thresh_counts_sum=all_models_lowflow_thresh_counts_sum %>% 
  filter(dataset!="baseline")

# add to shp file
yadkin_subs_shp_lowflow_base=left_join(yadkin_subs_shp,baseline_lowflow_thresh_counts_sum,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_base)
yadkin_subs_shp_lowflow_proj=left_join(yadkin_subs_shp,all_projs_lowflow_thresh_counts_sum,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_proj)

# plot baseline
setwd("/Users/ssaia/Desktop")
cairo_pdf("baseline_lowflow_count.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_base,aes(fill=num_lowflow_days_per_yr)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_base is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Avg # Lowflow Days/Yr",na.value="grey75",limits=c(0,10)) +
  theme_bw()
dev.off()

# plot projections (this isn't working!)
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("projections_lowflow_count.pdf",width=11,height=8.5)
#ggplot(yadkin_subs_shp_lowflow_proj,aes(fill=num_lowflow_days_per_yr)) +
#  facet_wrap(~dataset) +
#  geom_sf() +
#  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_projs is base utm 17N so convert to Albers for CONUS
#  scale_fill_gradient2("Avg # Lowflow Days/Yr",na.value="grey75") +
#  theme_bw()
#dev.off()

