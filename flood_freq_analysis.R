# yadkin flood frequency analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(stringr)
library(sf)

# set directory and load data

# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
#baseline_sub_data_raw=read_table2("output.sub",col_names=FALSE,skip=9) # baseline .sub file from SWAT
baseline_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#yadkin_net_data_raw=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# CSIRO RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/C_CSIRO45")
csiro4_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# CSIRO RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# Hadley RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/D_Hadley45")
hadley4_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# MIROC RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_data_raw=read_table2("output.rch",col_names=FALSE,skip=9)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/gis_data")
yadkin_subs_shp=st_read("subs1.shp",quiet=TRUE)

# ---- 2. reformat data ----

# column names
sub_col_names=c("FILE","SUB","GIS","MO","DA","YR","AREAkm2","PRECIPmm","SNOMELTmm",
               "PETmm","ETmm","SWmm","PERCmm","SURQmm","GWQmm","WYLDmm","SYLDt_ha",
               "ORGNkg_ha","ORGPkg_ha","NSURQkg_ha","SOLPkg_ha","SEDPkg_ha","LATQmm",
               "LATNO3kg_ha,GWNO3kg_ha","CHOLAmic_l","CBODUmg_l","DOXQmg_l","TNO3kg_ha",
               "UNKNOWN","SUB_DUPLICATE")
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
colnames(baseline_sub_data_raw)=sub_col_names
colnames(baseline_rch_data_raw)=rch_col_names
colnames(csiro4_5_rch_data_raw)=rch_col_names
colnames(csiro8_5_rch_data_raw)=rch_col_names
colnames(hadley4_5_rch_data_raw)=rch_col_names
colnames(miroc8_5_rch_data_raw)=rch_col_names

# remove unnecessary columns
baseline_sub_data=baseline_sub_data_raw %>% select(SUB,MO:WYLDmm)
baseline_rch_data=baseline_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH) # add column so can join later if needed
#yadkin_net_data_sel=yadkin_net_data_raw %>% mutate(SUB=Subbasin) %>% select(SUB,FROM_NODE,TO_NODE)
csiro4_5_rch_data=csiro4_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)
csiro8_5_rch_data=csiro8_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)
hadley4_5_rch_data=hadley4_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)
miroc8_5_rch_data=miroc8_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)

# join areas for yadkin_net_data (=weights)
#yadkin_sub_areas=baseline_sub_data %>% select(SUB,AREAkm2) %>% distinct()
#yadkin_net_data=left_join(yadkin_net_data_sel,yadkin_sub_areas,by="SUB") %>%
#  select(FROM_NODE,TO_NODE,AREAkm2) # remove SUB b/c FROM_NODE=SUB

# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)


# ---- 3. function: observation flood freq analysis (one subbasin) ----

# define function
obs_rch_freq_calcs=function(rch_data) { 
  # rch_data is df with all reach data for ONLY 1 subbasin
  
  # calculate number of years
  num_yrs=length(unique(rch_data$YR))
  
  # find max, sort descending, and adjust
  obs_df_temp=rch_data %>% 
    group_by(SUB,YR) %>% 
    summarise(obs_max_flow_cms=max(FLOW_OUTcms)) %>%
    arrange(SUB,desc(obs_max_flow_cms)) %>%
    mutate(obs_max_flow_cms_adj=obs_max_flow_cms*1.13) %>% # adjust using standard window shift
    mutate(obs_max_flow_log_cms_adj=log(obs_max_flow_cms_adj)) # take log
  
  # rank data
  obs_df_temp$obs_rank_num=seq(1,num_yrs,1)
  
  # define return period
  obs_df_temp$obs_return_period_yr=(num_yrs+1)/obs_df_temp$obs_rank_num
  
  # select only necessary fields
  obs_df=obs_df_temp %>% select(SUB,
                                obs_return_period_yr,
                                obs_max_flow_cms_adj,
                                obs_max_flow_log_cms_adj) %>%
    mutate(data_type=rep("obs",num_yrs))
  
  # return
  return(obs_df)
}


# ---- 4. function: model flood freq analysis (one subbasin) ----

# define function for log-Pearson tyoe III test
model_rch_freq_calcs=function(obs_rch_freq_calcs_df,model_p_list) {
  # obs_rch_freq_calcs_df is the output dataframe from running obs_rch_freq_calcs function
  
  # save some of input data as temporary variable
  obs_flow_unlog=obs_rch_freq_calcs_df$obs_max_flow_cms_adj
  obs_flow_log=obs_rch_freq_calcs_df$obs_max_flow_log_cms_adj
  obs_sub=unique(obs_rch_freq_calcs_df$SUB)
  num_yrs=dim(obs_rch_freq_calcs_df)[1]
  
  # calculate mean
  obs_mean=mean(obs_flow_log)
    
  # calculate difference from mean
  obs_mean_diff=obs_flow_log-obs_mean
  obs_mean_diff_sqrd=obs_mean_diff^2
  obs_mean_diff_sqrd_sum=sum(obs_mean_diff_sqrd)
  obs_mean_diff_cubed=obs_mean_diff^3
  obs_mean_diff_cubed_sum=sum(obs_mean_diff_cubed)
  
  # calculate coefficient of skew (cskew)
  # source: http://streamflow.engr.oregonstate.edu/analysis/floodfreq/meandaily_tutorial.htm
  obs_variance=(1/(num_yrs-1))*obs_mean_diff_sqrd_sum
  obs_stdev=sqrt(obs_variance)
  obs_cskew=(num_yrs*obs_mean_diff_cubed_sum)/((num_yrs-1)*(num_yrs-2)*(obs_stdev^3))

  # find return period
  num_p=length(model_p_list)
  model_rank_num=seq(1,num_p,1)
  model_return_period=1/model_p_list
  
  # calculate frequency factors
  # source: BEE 4730 Watershed Engineering: Hydrological Risk Analysis PDF
  # w
  model_w_term=1/(model_p_list^2)
  model_w=sqrt(log(model_w_term))
  
  # z statistic
  model_z_term2=2.515517+0.802853*model_w+0.010328*(model_w^2)
  model_z_term3=1+1.432788*model_w+0.189269*(model_w^2)+0.001308*(model_w^3)
  model_z=model_w-(model_z_term2/model_z_term3)
  
  # log-Person type III frequency factor (kt)
  k_term=obs_cskew/6
  model_kt_term2=((model_z^2)-1)*k_term
  model_kt_term3=(1/3)*((model_z^3)-6*model_z)*(k_term^2)
  model_kt_term4=((model_z^2)-1)*(k_term^3)
  model_kt_term5=model_z*(k_term^4)
  model_kt_term6=(1/3)*(k_term^5)
  model_kt=model_z+model_kt_term2+model_kt_term3-model_kt_term4+model_kt_term5+model_kt_term6
  
  # calculate final modeled flow values
  model_flow_log=obs_mean+obs_stdev*model_kt
  model_flow_unlog=exp(obs_mean+obs_stdev*model_kt)
  
  # make output dataframe with results
  model_df=data.frame(SUB=rep(obs_sub,num_p),
                      model_return_period_yr=model_return_period, 
                      model_flow_cms=model_flow_unlog,
                      model_flow_log_cms=model_flow_log,
                      data_type=rep("model",num_p))
  
  # return output
  return(model_df)
}


# ---- 5. function: flood freq analysis by subbasin ----

# observation for all subbasins in .rch file (uses obs_rch_freq_calcs function) 
obs_rch_freq_calcs_all_subs=function(rch_data) {
  
  # calculate number of subbasins for for loop
  num_subs=length(unique(rch_data$SUB))
  
  # make dataframe for all outputs
  obs_df_all_subs=data.frame(SUB=as.integer(),
                             obs_return_period_yr=as.numeric(),
                             obs_max_flow_cms_adj=as.numeric(),
                             obs_max_flow_log_cms_adj=as.numeric(),
                             data_type=as.character())
  
  for (i in 1:num_subs) {
    sel_rch_data=rch_data %>% filter(SUB==i)
    obs_df_all_temp=obs_rch_freq_calcs(sel_rch_data)
    obs_df_all_subs=bind_rows(obs_df_all_subs,obs_df_all_temp)
  }
  
  return(obs_df_all_subs)
}


# models for all subbasins in .rch file (uses model_rch_freq_calcs function) 
model_rch_freq_calcs_all_subs=function(obs_rch_freq_calcs_all_subs_df,model_p_list) {
  
  # calculate number of subbasins for for loop
  num_subs=length(unique(obs_rch_freq_calcs_all_subs_df$SUB))
  
  # make dataframe for all outputs
  model_df_all_subs=data.frame(SUB=as.integer(),
                               model_return_period=as.numeric(),
                               model_flow_cms=as.numeric(),
                               model_flow_cms_log=as.numeric(),
                               data_type=as.character())
  
  for (i in 1:num_subs) {
    sel_rch_data=obs_rch_freq_calcs_all_subs_df %>% filter(SUB==i)
    model_df_all_temp=model_rch_freq_calcs(sel_rch_data,model_p_list)
    model_df_all_subs=bind_rows(model_df_all_subs,model_df_all_temp)
  }
  
  return(model_df_all_subs)
}


# ---- 6. calculate obs and model ouptuts for each subbasin ----

baseline_obs_calcs=obs_rch_freq_calcs_all_subs(baseline_rch_data)
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_calcs=model_rch_freq_calcs_all_subs(baseline_obs_calcs,my_model_p_list)

csiro4_5_obs_calcs=obs_rch_freq_calcs_all_subs(csiro4_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro4_5_model_calcs=model_rch_freq_calcs_all_subs(csiro4_5_obs_calcs,my_model_p_list)

csiro8_5_obs_calcs=obs_rch_freq_calcs_all_subs(csiro8_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro8_5_model_calcs=model_rch_freq_calcs_all_subs(csiro8_5_obs_calcs,my_model_p_list)

hadley4_5_obs_calcs=obs_rch_freq_calcs_all_subs(hadley4_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
hadley4_5_model_calcs=model_rch_freq_calcs_all_subs(hadley4_5_obs_calcs,my_model_p_list)

miroc8_5_obs_calcs=obs_rch_freq_calcs_all_subs(miroc8_5_rch_data)
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
miroc8_5_model_calcs=model_rch_freq_calcs_all_subs(miroc8_5_obs_calcs,my_model_p_list)


# ---- 7. plot results for each subbasin ----

# plot observations and models together
ggplot() +
  geom_point(aes(x=obs_return_period,y=obs_max_flow_cms_adj),baseline_obs_calcs,size=1) +
  geom_line(aes(x=model_return_period,y=model_flow_cms),baseline_model_calcs,color="black") +
  geom_line(aes(x=model_return_period,y=model_flow_cms),csiro4_5_model_calcs,color="orange") +
  geom_line(aes(x=model_return_period,y=model_flow_cms),csiro8_5_model_calcs,color="red") +
  geom_line(aes(x=model_return_period,y=model_flow_cms),hadley4_5_model_calcs,color="blue") +
  geom_line(aes(x=model_return_period,y=model_flow_cms),miroc8_5_model_calcs,color="green") +
  facet_wrap(~SUB,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (cms)") +
  theme_bw()


# ---- x8. function: find projection return period for baseline flow of a specified return period ----

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


# ---- x9. calculate return period difference ----

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


# ---- 10. function: find baseline and projection flows for same return period ----

flow_diff=function(return_period,baseline_model_calcs,projection_model_calcs) {
  # return_period must be an entry in the modeled data
  
  # select only data for return period of interest
  baseline_return_period_sel=baseline_model_calcs %>% filter(model_return_period==return_period)
  projection_return_period_sel=projection_model_calcs %>% filter(model_return_period==return_period)
  
  # define variables and output dataframe
  num_subs=length(unique(baseline_model_calcs$SUB))
  diff_df=data.frame(SUB=as.integer(),return_period=as.numeric(),
                     baseline_model_flow_cms=as.numeric(),
                     projection_model_flow_cms=as.numeric(),
                     proj_minus_base_flow_cms=as.numeric(),
                     proj_minus_base_flow_percchange=as.numeric())
  
  # for loop for each subbasin
  for (i in 1:num_subs) {
    
    # baseline data for specified return period and subbasin
    baseline_sub_temp=baseline_return_period_sel %>% filter(SUB==i)
    baseline_sub_flow_temp=baseline_sub_temp$model_flow_cms
    
    # projection data for specified return period and subbasin
    projection_sub_temp=projection_return_period_sel %>% filter(SUB==i)
    projection_sub_flow_temp=projection_sub_temp$model_flow_cms
    
    # find difference
    proj_minus_base_flow_cms_temp=projection_sub_flow_temp-baseline_sub_flow_temp
    proj_minus_base_flow_percchange_temp=(proj_minus_base_flow_cms_temp/baseline_sub_flow_temp)*100
    
    # save results to data frame
    diff_df_temp=data.frame(SUB=i,return_period=return_period,
                            baseline_model_flow_cms=baseline_sub_flow_temp,
                            projection_model_flow_cms=projection_sub_flow_temp,
                            proj_minus_base_flow_cms=proj_minus_base_flow_cms_temp,
                            proj_minus_base_flow_percchange=proj_minus_base_flow_percchange_temp)
    
    # bind results to diff_df
    diff_df=bind_rows(diff_df,diff_df_temp)
  }
 
  # return output
  return(diff_df) 
}


# ---- 11. calculate flow difference ----

# csiro 4.5
csiro4_5_10yr_flow=flow_diff(10,baseline_model_calcs,csiro4_5_model_calcs)
csiro4_5_100yr_flow=flow_diff(100,baseline_model_calcs,csiro4_5_model_calcs)

# csiro 8.5
csiro8_5_10yr_flow=flow_diff(10,baseline_model_calcs,csiro8_5_model_calcs)
csiro8_5_100yr_flow=flow_diff(100,baseline_model_calcs,csiro8_5_model_calcs)

# hadley 4.5
hadley4_5_10yr_flow=flow_diff(10,baseline_model_calcs,hadley4_5_model_calcs)
hadley4_5_100yr_flow=flow_diff(100,baseline_model_calcs,hadley4_5_model_calcs)

# miroc 8.5
miroc8_5_10yr_flow=flow_diff(10,baseline_model_calcs,miroc8_5_model_calcs)
miroc8_5_100yr_flow=flow_diff(100,baseline_model_calcs,miroc8_5_model_calcs)


# ---- 12. plot flow differences on map ----

# csiro 4.5 vs baseline 10 yr flow
# select only necessary down data
csiro4_5_10yr_flow_sel=csiro4_5_10yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, csiro4_5_10yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro4_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p1=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro4_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60))


# csiro 4.5 vs baseline 100 yr flow
# select only necessary down data
csiro4_5_100yr_flow_sel=csiro4_5_100yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, csiro4_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro4_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p5=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro4_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315))


# csiro 8.5 vs baseline 10 yr flow
# select only necessary down data
csiro8_5_10yr_flow_sel=csiro8_5_10yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, csiro8_5_10yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p2=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro8_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60))


# csiro 8.5 vs baseline 100 yr flow
# select only necessary down data
csiro8_5_100yr_flow_sel=csiro8_5_100yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, csiro8_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p6=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro8_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315))


# hadley 4.5 vs baseline 10 yr flow
# select only necessary down data
hadley4_5_10yr_flow_sel=hadley4_5_10yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, hadley4_5_10yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p3=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60))


# hadley 4.5 vs baseline 100 yr flow
# select only necessary down data
hadley4_5_100yr_flow_sel=hadley4_5_100yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, hadley4_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p7=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315))


# miroc 8.5 vs baseline 10 yr flow
# select only necessary down data
miroc8_5_10yr_flow_sel=miroc8_5_10yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, miroc8_5_10yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p4=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=miroc8_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60))


# miroc 8.5 vs baseline 100 yr flow
# select only necessary down data
miroc8_5_100yr_flow_sel=miroc8_5_100yr_flow %>% select(SUB,proj_minus_base_flow_percchange) %>%
  transmute(SUB=SUB, miroc8_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p8=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=miroc8_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315))


# plot 10yr figures
setwd("/Users/ssaia/Desktop")
pdf("test.pdf",width=11,height=8.5)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

# plot 100yr figures
setwd("/Users/ssaia/Desktop")
pdf("test2.pdf",width=11,height=8.5)
multiplot(p5, p6, p7, p8, cols=2)
dev.off()


# ---- 13. function: multiplot ----
# from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



# ---- X. Extra ----

sub_area=baseline_sub_data_raw %>% select(SUB,AREAkm2) %>% 
  transmute(SUB=SUB,sub_AREAkm2=round(AREAkm2,0)) %>% distinct()
rch_area=baseline_rch_data_raw %>% select(RCH,AREAkm2) %>% 
  transmute(RCH=RCH,rch_AREAkm2=round(AREAkm2,0)) %>% distinct()

subs_equal_rch=bind_cols(sub_area,rch_area) %>% filter(sub_AREAkm2==rch_AREAkm2)

test=bind_cols(sub_area,rch_area)


# network analysis help: http://www.shizukalab.com/toolkits/sna/plotting-directed-networks
# use network analysis graph to automate subbasin contributions of runoff?

