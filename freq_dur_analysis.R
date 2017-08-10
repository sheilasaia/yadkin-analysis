# yadkin frequency-duration analysis

# ---- 1. set up -----

# clear ws


# load libraries
library(tidyverse)
library(stringr)

# set directory
setwd("/Users/ssaia/Documents/sociohydro_project/raw_data/kelly_results/baseline82-08_daily")

# load data
baseline_data_raw=read_table("output.sub",col_names=FALSE,skip=9)


# ---- 2. reformat data ----

# column names
my_col_names=c("NAME","SUB","GIS","MO","DA","YR","AREAkm2","PRECIPmm","SNOMELTmm",
               "PETmm","ETmm","SWmm","PERCmm","SURQmm","GWQmm","WYLDmm","SYLDt_ha",
               "ORGNkg_ha","ORGPkg_ha","NSURQkg_ha","SOLPkg_ha","SEDPkg_ha","LATQmm",
               "LATNO3kg_ha,GWNO3kg_ha","CHOLAmic_l","CBODUmg_l","DOXQmg_l","TNO3kg_ha",
               "UNKNOWN","SUB_DUPLICATE")

# reassign column names
colnames(baseline_data_raw)=my_col_names

# remove unnecessary columns
baseline_data=baseline_data_raw %>% select(SUB,MO:TNO3kg_ha)


# ---- 3. observation function ----

# define function
obs_freq_calcs=function(baseline_data) { 
  # baseline data is df with all flow data for 1 subbasin for period of study
  # includes fields: SUB, MO, DA, YR, AREAkm2, PRECIPmm, SNOMELTmm,
  # PETmm, ETmm, SWmm, PERCmm, SURQmm, GWQmm, WYLDmm, SYLDt_ha,
  # ORGNkg_ha, ORGPkg_ha, NSURQkg_ha, SOLPkg_ha, SEDPkg_ha, LATQmm,
  # LATNO3kg_ha, GWNO3kg_ha, CHOLAmic_l, CBODUmg_l, DOXQmg_l, TNO3kg_ha,
  
  # calculate number of years
  num_yrs=length(unique(baseline_data$YR))
  
  # find max, sort descending, and adjust
  obs_df_temp=baseline_data %>% 
    group_by(SUB,YR) %>% 
    summarise(max_surfq=max(SURQmm)) %>%
    arrange(SUB,desc(max_surfq)) %>%
    mutate(max_surfq_adj=max_surfq*1.13) %>% # adjust using standard window shift
    mutate(max_surfq_adj_log=log(max_surfq_adj)) # take log
  
  # rank data
  obs_df_temp$obs_rank_num=seq(1,num_yrs,1)
  
  # define return period
  obs_df_temp$obs_return_period=(num_yrs+1)/obs_df_temp$obs_rank_num
  
  # select only necessary fields
  obs_df=obs_df_temp %>% select(SUB,obs_return_period,max_surfq_adj,max_surfq_adj_log)
  
  # return
  return(obs_df)
}


# ---- 4. model function ----

# define function
model_freq_calcs=function(obs_freq_calcs_surfq_adj_log,num_yrs,model_p_list) {
  
  # basic stats
  my_mean=mean(obs_freq_calcs_surfq_adj_log)
  my_stdev=sd(obs_freq_calcs_surfq_adj_log)
  my_cv=my_stdev/my_mean
  
  # calculate difference from mean
  my_mean_diff=obs_freq_calcs_surfq_adj_log-my_mean
  my_mean_diff_sqrd=my_mean_diff^2
  my_mean_diff_sqrd_sum=sum(my_mean_diff_sqrd)
  my_mean_diff_cubed=my_mean_diff^3
  my_mean_diff_cubed_sum=sum(my_mean_diff_cubed)
  
  # calculate coefficient of skew (cskew)
  s_term=(1/(num_yrs-1)*my_mean_diff_sqrd_sum
  s_term_cubed=s_term^3
  cskew=(num_yrs*my_mean_diff_cubed_sum)/((num_yrs-1)*(num_yrs-2)*s_term_cubed)
  k_term=cskew/6
  
  # find return period
  num_p=length(model_p_list)
  model_rank_num=seq(1,num_p,1)
  model_return_period=1/model_p_list
  
  # calculate frequency factors
  # w
  model_w_term=1/(model_p_list^2)
  model_w=log(model_w_term)^(1/2)
  
  # z
  model_z_term2=2.515517+0.802853*model_w+0.010328*(model_w^2)
  model_z_term3=1+1.432788*model_w+0.189269*(model_w^2)+0.001308*(model_w^3)
  model_z=model_w-(model_z_term2/model_z_term3)
  
  # kt
  model_kt_term2=((model_z^2)-1)*k_term
  model_kt_term3=(1/3)*((model_z^3)-6*model_z)*(k_term^2)
  model_kt_term4=((model_z^2)-1)*(k_term^3)
  model_kt_term5=model_z*(k_term^4)
  model_kt_term6=(1/3)*(k_term^5)
  model_kt=model_z+model_kt_term2+model_kt_term3-model_kt_term4+model_kt_term5+model_kt_term6
  
  # calculate final modeled surfq values
  model_surfq_unlog=exp(my_mean+my_stdev*model_kt)
  
  # make output dataframe with results
  model_df=data.frame(model_return_period=model_return_period,
                      model_surfq_unlog=model_surfq_unlog)
  
  # return output
  return(model_df)
}


# ---- 5. calculate obs and model ouptuts for each subbasin ----

# observation calcs
my_sub_baseline_data=baseline_data %>% filter(SUB==1)
my_obs_freq_calcs=obs_freq_calcs(my_sub_baseline_data)

# model calcs
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
my_num_yrs=length(unique(my_sub_baseline_data$YR))
my_model_freq_calcs=model_freq_calcs(my_obs_freq_calcs$max_surfq_adj_log,my_num_yrs,my_model_p_list)

# plot
plot(my_obs_freq_calcs$obs_return_period,my_obs_freq_calcs$max_surfq_adj,
     pch=16,xlim=c(0,100),ylim=c(0,500))
points(my_model_freq_calcs$model_return_period,my_model_freq_calcs$model_surfq_unlog)



#


# ---- reformat data ----

# reformat using function swat_outputsub_function
baseline_data=swat_outputsub_reformat(baseline_data_raw)

# write output to folder
write_csv(baseline_data,"baseline_data.csv")
