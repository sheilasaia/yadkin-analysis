# yadkin drought frequency analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(stringr)
library(sf)

# set directory and load data

# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/baseline82-08_daily")
baseline_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#yadkin_net_data_raw=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# CSIRO RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/C_CSIRO45")
csiro4_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# CSIRO RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# Hadley RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/D_Hadley45")
hadley4_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# MIROC RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/gis_data")
yadkin_subs_shp=st_read("subs1.shp",quiet=TRUE)

# ---- 2. reformat data ----

# column names
rch_col_names=c("FILE","RCH","GIS","MO","DA","YR","AREAkm2","FLOW_INcms","FLOW_OUTcms","EVAPcms",
                "TLOSScms","SED_INtons","SED_OUTtons","SEDCONCmg_kg","ORGN_INkg","ORGN_OUTkg",
                "ORGP_INkg", "ORGP_OUTkg","NO3_INkg","NO3_OUTkg","NH4_INkg","NH4_OUTkg",
                "NO2_INkg", "NO2_OUTkg","MINP_INkg","MINP_OUTkg","CHLA_INkg","CHLA_OUTkg",
                "CBOD_INkg","CBOD_OUTkg","DISOX_INkg","DISOX_OUTkg","SOLPST_INmg",
                "SOLPST_OUTmg","SORPST_INmg","SORPST_OUTmg","REACTPSTmg","VOLPSTmg",
                "SETTLPSTmg","RESUSP_PSTmg","DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg",
                "BED_PSTmg","BACTP_OUTct","BACTLP_OUTct","CMETAL_1kg","CMETAL_2kg","CMETAL_3kg",
                "TOTNkg","TOTPkg","NO3_mg_l","WTMPdegc")

# reassign column names
colnames(baseline_rch_data_raw)=rch_col_names
colnames(csiro4_5_rch_data_raw)=rch_col_names
colnames(csiro8_5_rch_data_raw)=rch_col_names
colnames(hadley4_5_rch_data_raw)=rch_col_names
colnames(miroc8_5_rch_data_raw)=rch_col_names

# remove unnecessary columns
baseline_rch_data=baseline_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH) # add column so can join later if needed


# ---- 3. function: observation 1-day, low-flow freq analysis (one subbasin) ----

# define function
obs_rch_lowflow_freq_calcs=function(rch_data) { 
  # rch_data is df with all reach data for ONLY 1 subbasin
  
  # calculate number of years
  num_yrs=length(unique(rch_data$YR))
  
  # find max, sort descending, and adjust
  obs_df_temp=rch_data %>% 
    group_by(SUB,YR) %>% 
    summarise(obs_min_flow_cms=min(FLOW_OUTcms)) %>%
    arrange(SUB,obs_min_flow_cms) %>%
    mutate(obs_min_flow_cms_adj=obs_min_flow_cms) %>% # adjust using standard window shift
    mutate(obs_min_flow_cms_adj_log=log(obs_min_flow_cms_adj))
  
  # rank data
  obs_df_temp$obs_rank_num=seq(1,num_yrs,1)
  
  # define return period
  obs_df_temp$obs_return_period=(num_yrs+1)/obs_df_temp$obs_rank_num
  
  # select only necessary fields
  obs_df=obs_df_temp %>% select(SUB,obs_return_period,obs_min_flow_cms_adj) %>%
    mutate(data_type=rep("obs",num_yrs))
  
  # return
  return(obs_df)
}

baseline_rch_data_sel=baseline_rch_data %>% filter(RCH==1)
baseline_obs_calcs=obs_rch_lowflow_freq_calcs(baseline_rch_data_sel)





# ---- 3. zero stream flow ----

#see methods here: https://pubs.usgs.gov/sir/2008/5126/section3.html
#see riggs 1972: https://pubs.usgs.gov/twri/twri4b1/pdf/twri_4-B1_a.pdf

# find days where flow_out equals zero
zero_flow=baseline_rch_data %>% filter(FLOW_OUTcms<0.01)

# count number of days for each basin (for each year) where flow equals zero
zero_flow_tally=zero_flow %>% group_by(SUB,YR) %>% 
  summarize(num_zero_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,SUB)



ggplot(zero_flow_tally,aes(x=YR,y=num_zero_days,color=as.factor(SUB))) +
  geom_point(size=3) +
  #xlab("return period") + 
  #ylab("flow out (cms)") +
  theme_bw()