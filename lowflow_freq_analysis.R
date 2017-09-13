# yadkin low flow frequency analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
#library(stringr)
#library(sf)
#library(smwrBase)

# load home-made functions 
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path,"reformat_rch_file.R")) # reformat SWAT .rch file
source(paste0(functions_path,"logpearson3_factor_calc.R")) # calculate log-Pearson III frequency factors
source(paste0(functions_path,"remove_outliers.R")) # removes low and high flows deemed as outliers
source(paste0(functions_path,"obs_lowflow_freq_calcs_one_rch.R")) # select observations for one reach
#source(paste0(functions_path,"obs_flood_freq_calcs_one_rch.R")) # select observations for one reach
source(paste0(functions_path,"obs_freq_calcs_all_rchs.R")) # selects observations for all reaches
source(paste0(functions_path,"model_lowflow_freq_calcs_one_rch.R")) # determines low-flow model for one reach
#source(paste0(functions_path,"model_flood_freq_calcs_one_rch.R")) # determines low-flow model for one reach
source(paste0(functions_path,"model_freq_calcs_all_rchs.R")) # determines low-flow model for all reaches
source(paste0(functions_path,"movingAve.R")) # from smwrBase with edits (add here for now)

# download kn_table for outlier analysis
setwd("/Users/ssaia/Documents/GitHub/yadkin-analysis/")
kn_table=read_csv("kn_table_appendix4_usgsbulletin17b.csv",col_names=TRUE)

# set directory and load data
# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
baseline_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#yadkin_net_data_raw=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# CSIRO RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/C_CSIRO45")
csiro4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# CSIRO RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# Hadley RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/D_Hadley45")
hadley4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# MIROC RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/gis_data")
yadkin_subs_shp=st_read("subs1.shp",quiet=TRUE)


# ---- 2. reformat data ----

# baseline data
baseline_rch_data=reformat_rch_file(baseline_rch_raw_data)

# CSIRO 4.5 data
csiro4_5_rch_data=reformat_rch_file(csiro4_5_rch_raw_data)

# CSIRO 8.5 data
csiro8_5_rch_data=reformat_rch_file(csiro8_5_rch_raw_data)

# HADLEY 4.5 data
hadley4_5_rch_data=reformat_rch_file(hadley4_5_rch_raw_data)

# MIROC 8.5 data
miroc8_5_rch_data=reformat_rch_file(miroc8_5_rch_raw_data)

# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)


# ---- 3. calculate obs and model ouptuts for each subbasin ----

baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(baseline_rch_data,span_days=1,flow_option="lowflow")
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_lowflow_calcs=model_rch_lowflow_freq_calcs_all_subs(baseline_obs_lowflow_calcs,my_model_p_list,0.4)

csiro4_5_obs_lowflow_calcs=obs_rch_lowflow_freq_calcs_all_subs(csiro4_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro4_5_model_lowflow_calcs=model_rch_lowflow_freq_calcs_all_subs(csiro4_5_obs_lowflow_calcs,my_model_p_list,0.4)

csiro8_5_obs_lowflow_calcs=obs_rch_lowflow_freq_calcs_all_subs(csiro8_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro8_5_model_lowflow_calcs=model_rch_lowflow_freq_calcs_all_subs(csiro8_5_obs_lowflow_calcs,my_model_p_list,0.4)

hadley4_5_obs_lowflow_calcs=obs_rch_lowflow_freq_calcs_all_subs(hadley4_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
hadley4_5_model_lowflow_calcs=model_rch_lowflow_freq_calcs_all_subs(hadley4_5_obs_lowflow_calcs,my_model_p_list,0.4)

miroc8_5_obs_lowflow_calcs=obs_rch_lowflow_freq_calcs_all_subs(miroc8_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
miroc8_5_model_lowflow_calcs=model_rch_lowflow_freq_calcs_all_subs(miroc8_5_obs_lowflow_calcs,my_model_p_list,0.4)


# ---- 7. plot results for each subbasin ----

# omit zero values from observations
baseline_obs_lowflow_calcs_nozeros=baseline_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj<0)
#
#
#
#

# omit NAs from model outputs
baseline_model_lowflow_calcs_noNAa=baseline_model_lowflow_calcs %>% na.omit()
csiro4_5_model_lowflow_calcs_noNAa=csiro4_5_model_lowflow_calcs %>% na.omit()
csiro8_5_model_lowflow_calcs_noNAa=csiro8_5_model_lowflow_calcs %>% na.omit()
hadley4_5_model_lowflow_calcs_noNAa=hadley4_5_model_lowflow_calcs %>% na.omit()
miroc8_5_model_lowflow_calcs_noNAa=miroc8_5_model_lowflow_calcs %>% na.omit()

# plot observations and models together (not log transformed)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_lowflow_calcs_noNAa,color="black") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_lowflow_calcs_noNAa,color="orange") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_lowflow_calcs_noNAa,color="red") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_lowflow_calcs_noNAa,color="blue") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_lowflow_calcs_noNAa,color="green") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (cms)") +
  ylim(0,max(baseline_obs_lowflow_calcs$obs_min_flow_cms_adj)+50) +
  theme_bw()

# plot observations and models together (log transformed)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_log_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_lowflow_calcs_noNAa,color="black") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_lowflow_calcs_noNAa,color="orange") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_lowflow_calcs_noNAa,color="red") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_lowflow_calcs_noNAa,color="blue") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_lowflow_calcs_noNAa,color="green") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (log cms)") +
  ylim((min(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)-2),(max(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)+2)) +
  xlim(0,105) +
  theme_bw()

# ---- X. one subbasin test ----

# run for one reach
baseline_rch_data_sel=baseline_rch_data %>% filter(RCH==8)
baseline_obs_lowflow_calcs_one_rch=obs_lowflow_freq_calcs_one_rch(baseline_rch_data_sel,1)
baseline_obs_lowflow_calcs_one_rch_nozeros=baseline_obs_lowflow_calcs_one_rch %>% filter(obs_min_flow_cms_adj>0)
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_lowflow_calcs_one_rch=model_lowflow_freq_calcs_one_rch(baseline_obs_lowflow_calcs_one_rch,kn_table,my_model_p_list,0.4)

# plotting
plot(obs_min_flow_cms_adj~obs_return_period_yr,data=baseline_obs_lowflow_calcs_one_rch_nozeros,pch=16,xlim=c(0,105))
lines(baseline_model_lowflow_calcs_one_rch$model_return_period_yr,baseline_model_lowflow_calcs_one_rch$model_flow_cms,col="green")


# ---- X. count number of low flow frequency obs = zero ----

num_yrs=length(unique(baseline_rch_data$YR))
baseline_obs_lowflow_calcs_zero_count=baseline_obs_lowflow_calcs %>% 
  filter(obs_min_flow_cms_adj==0) %>%
  group_by(SUB) %>% summarize(num_zero_entries=n()) %>%
  mutate(percent_zero=num_zero_entries/num_yrs)


# ---- 6. calculate lowflow counts ----

# find days where flow_out equals lowflow
my_lowflow=0.01 # in cms
baseline_lowflow_data=baseline_rch_data %>% filter(FLOW_OUTcms<my_lowflow)

# count number of days for each basin (for each year) where flow equals zero
baseline_lowflow_tally=baseline_lowflow_data %>% group_by(SUB,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,SUB)


# ---- 7. plot lowflow counts (scatter plot and on map) ----

# scatter plot vs time
ggplot(baseline_lowflow_tally,aes(x=YR,y=num_lowflow_days,color=as.factor(SUB))) +
  geom_point(size=3) +
  xlab("year") + 
  ylab(paste("num days w/ outflow <", my_lowflow,"cms")) +
  theme_bw()

# spatial distrubution
# select only necessary down data
baseline_lowflow_tally_sel=baseline_lowflow_tally %>% select(SUB,num_lowflow_days) %>%
  group_by(SUB) %>% summarize(num_lowflow_days=sum(num_lowflow_days))

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,baseline_lowflow_tally_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=num_lowflow_days)) +
  scale_fill_gradient(name=paste("total # days <",my_lowflow,"cms \n from 1982-2008"),limits=c(100,200),
                      low="white",high="red",na.value="grey75")