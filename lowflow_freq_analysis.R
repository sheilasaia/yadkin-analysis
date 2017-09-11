# yadkin low flow frequency analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(stringr)
library(sf)
library(smwrBase)

# set directory and load data

# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
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
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/gis_data")
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
csiro4_5_rch_data=csiro4_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)
csiro8_5_rch_data=csiro8_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)
hadley4_5_rch_data=hadley4_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)
miroc8_5_rch_data=miroc8_5_rch_data_raw %>% select(RCH,MO:FLOW_OUTcms) %>%
  mutate(SUB=RCH)

# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)

# ---- 3. function: observation 1-day, low flow freq analysis (one subbasin) ----

# define function
obs_rch_lowflow_freq_calcs=function(rch_data) { 
  # rch_data is df with all reach data for ONLY 1 subbasin
  
  # calculate number of years
  num_yrs=length(unique(rch_data$YR))
  
  # find min, sort descending, and adjust
  obs_df_temp=rch_data %>% 
    group_by(SUB,YR) %>% 
    summarise(obs_min_flow_cms=min(FLOW_OUTcms)) %>%
    arrange(SUB,obs_min_flow_cms) %>%
    mutate(obs_min_flow_cms_adj=obs_min_flow_cms) %>% # adjust using standard window shift
    mutate(obs_min_flow_log_cms_adj=log(obs_min_flow_cms_adj))
  
  # rank data
  obs_df_temp$obs_rank_num=seq(1,num_yrs,1)
  
  # define return period
  obs_df_temp$obs_return_period_yr=(num_yrs+1)/obs_df_temp$obs_rank_num
  
  # select only necessary fields
  obs_df=obs_df_temp %>% select(SUB,
                                obs_rank_num,
                                obs_return_period_yr,
                                obs_min_flow_cms_adj,
                                obs_min_flow_log_cms_adj) %>%
    mutate(data_type=rep("obs",num_yrs))
  
  # return
  return(obs_df)
}


# ---- 4. function: model 1-day, low flow freq analysis (one subbasin) ----

model_rch_lowflow_freq_calcs=function(obs_rch_lowflow_freq_calcs_df,model_p_list,general_cskew) {
  # obs_rch_lowflow_freq_calcs_df is the output dataframe from running obs_rch_lowflow_freq_calcs function
  
  # references:
  # http://streamflow.engr.oregonstate.edu/analysis/floodfreq/meandaily_tutorial.htm
  # http://www.hydrology.bee.cornell.edu/BEE473Homework_files/RiskAnalysis.pdf
  # USGS Riggs, 1972
  # USGS Bulletin 17B, 1982
  # https://water.usgs.gov/osw/pubs/TM_4-B4/
  # pdf: https://water.usgs.gov/osw/TRB/Bulletin17B_Computations_TRB_010909.pdf
  
  # save temporary variables
  current_sub=unique(obs_rch_lowflow_freq_calcs_df$SUB)
  num_p=length(model_p_list)
  num_yrs=dim(obs_rch_lowflow_freq_calcs_df)[1]
  obs_rank_num=obs_rch_lowflow_freq_calcs_df$obs_rank_num
  obs_return_period=obs_rch_lowflow_freq_calcs_df$obs_return_period_yr
  obs_flow_unlog=obs_rch_lowflow_freq_calcs_df$obs_min_flow_cms_adj
  
  # just data > zero
  obs_overzero_rank_num=obs_rank_num[obs_flow_unlog>0] # select only data > zero
  obs_overzero_return_period=obs_return_period[obs_flow_unlog>0]
  obs_overzero_flow_unlog=obs_flow_unlog[obs_flow_unlog>0]
  num_overzero_yrs=length(obs_overzero_flow_unlog)
  
  # percent of data = zero
  num_zero_yrs=num_yrs-num_overzero_yrs
  perc_zeros_check=num_zero_yrs/num_yrs # only proceed if 25% or less
  
  # make output dataframe to hold results
  model_df=data.frame(SUB=as.integer(),
                      model_return_period_yr=as.numeric(),
                      model_flow_cms=as.numeric(),
                      model_flow_log_cms=as.numeric(),
                      data_type=as.character())
  
  if (perc_zeros_check<=0.25) {
    if (perc_zeros_check==0) {
      # low flow analysis without conditional probability adjustment
      
      # log inputs and find mean
      obs_flow_log=log(obs_flow_unlog)
      obs_mean=mean(obs_flow_log)
      # calculate difference from mean
      obs_mean_diff=obs_flow_log-obs_mean
      obs_mean_diff_sqrd=obs_mean_diff^2
      obs_mean_diff_sqrd_sum=sum(obs_mean_diff_sqrd)
      obs_mean_diff_cubed=obs_mean_diff^3
      obs_mean_diff_cubed_sum=sum(obs_mean_diff_cubed)
      
      # calculate coefficient of skew (cskew)
      obs_variance=(1/(num_yrs-1))*obs_mean_diff_sqrd_sum
      obs_stdev=sqrt(obs_variance)
      obs_cskew=(num_yrs*obs_mean_diff_cubed_sum)/((num_yrs-1)*(num_yrs-2)*(obs_stdev^3))
      
      # calculate frequency factors
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
      
      # final output dataframe
      model_df=data.frame(SUB=rep(current_sub,num_p),
                          model_return_period_yr=1/(1-(model_p_list)),
                          model_flow_log_cms=model_flow_log,
                          model_flow_cms=model_flow_unlog,
                          data_type=rep("model",num_p))
      
      # model_rank_num=seq(1,num_p,1)
      # note only difference here between low flow and flood frequency analysis is
      # return period is based on 1-p (i.e., T=1/(1-p))
    }
    else {
      
      # if there are any flows=0 then do conditional probability adjustment
      
      # log inputs > zero and calculate mean
      obs_overzero_flow_log=log(obs_overzero_flow_unlog)
      obs_overzero_mean=mean(obs_overzero_flow_log)
      
      # calculate standard deviation and coefficient of skew (cskew)
      obs_overzero_mean_diff=obs_overzero_flow_log-obs_overzero_mean
      obs_overzero_mean_diff_sqrd=obs_overzero_mean_diff^2
      obs_overzero_mean_diff_sqrd_sum=sum(obs_overzero_mean_diff_sqrd)
      obs_overzero_mean_diff_cubed=obs_overzero_mean_diff^3
      obs_overzero_mean_diff_cubed_sum=sum(obs_overzero_mean_diff_cubed)
      obs_overzero_variance=(1/(num_yrs-1))*obs_overzero_mean_diff_sqrd_sum
      obs_overzero_stdev=sqrt(obs_overzero_variance)
      obs_overzero_cskew=(num_yrs*obs_overzero_mean_diff_cubed_sum)/((num_yrs-1)*(num_yrs-2)*(obs_overzero_stdev^3))
      
      # calculate flow outliers...threshold pg 136/142
      
      # adj probability
      obs_overzero_prob=1/obs_overzero_return_period
      obs_overzero_prob_adj=1-(num_overzero_yrs/num_yrs)*(1-obs_overzero_prob)
      obs_overzero_return_perdo_adj_yr=1/obs_overzero_prob_adj
      
      # caculate log-Person type III frequency factor (kt) for conditional probability adjustment
      # w term
      con_w_term=1/((model_p_list)^2)
      con_w=sqrt(log(con_w_term))
      
      # standard normal z statistic
      con_z_term2=2.515517+0.802853*con_w+0.010328*(con_w^2)
      con_z_term3=1+1.432788*con_w+0.189269*(con_w^2)+0.001308*(con_w^3)
      con_z=con_w-(con_z_term2/con_z_term3)
      
      # kt
      con_k_term=obs_overzero_cskew/6
      con_kt_term2=((con_z^2)-1)*con_k_term
      con_kt_term3=(1/3)*((con_z^3)-6*con_z)*(con_k_term^2)
      con_kt_term4=((con_z^2)-1)*(con_k_term^3)
      con_kt_term5=con_z*(con_k_term^4)
      con_kt_term6=(1/3)*(con_k_term^5)
      con_kt=con_z+con_kt_term2+con_kt_term3-con_kt_term4+con_kt_term5+con_kt_term6
      
      # other conditional probability adjustment calculations
      con_flow_log=obs_overzero_mean+obs_overzero_stdev*con_kt
      con_flow_unlog=exp(obs_overzero_mean+obs_overzero_stdev*con_kt)
      con_prob_adj=1-perc_zeros_check
      con_p_list=model_p_list*con_prob_adj
      con_return_period_no_adj=1/(1-model_p_list)
      con_return_period_adj=1/(1-con_p_list)
      
      # fit smooth and predict
      con_smooth=loess(con_flow_log~con_p_list)
      con_predict=predict(con_smooth,c(0.01,0.1,0.5)) # predictions for p=0.01, p=0.1, p=0.5
      
      # caculate log-Person type III frequency factor (kt) for synthetic curve
      # coefficient of skew
      syn_cskew=-2.50+3.12*((con_predict[1]-con_predict[2])/(con_predict[2]-con_predict[3]))
      # only ok for obs_overzero_cskew between +2.5 and -2.0
      
      # w term
      syn_p_list=c(0.01,0.5) # only need for these probabilities
      syn_w_term=1/((syn_p_list)^2)
      syn_w=sqrt(log(syn_w_term))
      
      # standard normal z statistic
      syn_z_term2=2.515517+0.802853*syn_w+0.010328*(syn_w^2)
      syn_z_term3=1+1.432788*syn_w+0.189269*(syn_w^2)+0.001308*(syn_w^3)
      syn_z=syn_w-(syn_z_term2/syn_z_term3)
      
      # kt
      syn_k_term=syn_cskew/6
      syn_kt_term2=((syn_z^2)-1)*syn_k_term
      syn_kt_term3=(1/3)*((syn_z^3)-6*syn_z)*(syn_k_term^2)
      syn_kt_term4=((syn_z^2)-1)*(syn_k_term^3)
      syn_kt_term5=syn_z*(syn_k_term^4)
      syn_kt_term6=(1/3)*(syn_k_term^5)
      syn_kt=syn_z+syn_kt_term2+syn_kt_term3-syn_kt_term4+syn_kt_term5+syn_kt_term6
      
      # standard deviation and mean
      syn_stdev=(con_predict[1]-con_predict[3])/(syn_kt[1]-syn_kt[2])
      syn_mean=(con_predict[3])-syn_kt[2]*syn_stdev
      
      # calculate weighted coefficient of skew
      if (abs(syn_cskew)<=0.9) {
        mse_syn_cskew_A=-0.33+0.08*abs(syn_cskew)
      } else {
        mse_syn_cskew_A=-0.52+0.30*abs(syn_cskew)
      }
      
      if (abs(syn_cskew)<=1.5) {
        mse_syn_cskew_B=0.94-0.26*abs(syn_cskew)
      } else {
        mse_syn_cskew_B=0.55
      }
      
      mse_syn_cskew=mse_syn_cskew_A-(mse_syn_cskew_B*log(num_yrs/10))
      mse_general_cskew=0.302 # by definition for Plate I in USGS bulletin 17B (pg 184)
      #general_cskew=0.4 # for Yadkin-Pee Dee Watershed, NC region
      wtd_cskew=(mse_general_cskew*syn_cskew+mse_syn_cskew*general_cskew)/(mse_general_cskew+mse_syn_cskew)
      
      # caculate log-Person type III frequency factor (kt) for final curve
      # w term
      fin_w_term=1/((model_p_list)^2)
      fin_w=sqrt(log(fin_w_term))
      
      # standard normal z statistic
      fin_z_term2=2.515517+0.802853*fin_w+0.010328*(fin_w^2)
      fin_z_term3=1+1.432788*fin_w+0.189269*(fin_w^2)+0.001308*(fin_w^3)
      fin_z=fin_w-(fin_z_term2/fin_z_term3)
      
      # kt
      fin_k_term=wtd_cskew/6
      fin_kt_term2=((fin_z^2)-1)*fin_k_term
      fin_kt_term3=(1/3)*((fin_z^3)-6*fin_z)*(fin_k_term^2)
      fin_kt_term4=((fin_z^2)-1)*(fin_k_term^3)
      fin_kt_term5=fin_z*(fin_k_term^4)
      fin_kt_term6=(1/3)*(fin_k_term^5)
      fin_kt=fin_z+fin_kt_term2+fin_kt_term3-fin_kt_term4+fin_kt_term5+fin_kt_term6
      
      # final output dataframe
      model_df=data.frame(SUB=rep(current_sub,num_p),
                          model_return_period_yr=1/(1-model_p_list),
                          model_flow_log_cms=syn_mean+syn_stdev*fin_kt,#syn_stdev*fin_kt,
                          model_flow_cms=exp(syn_mean+syn_stdev*fin_kt),#=exp(syn_mean+syn_stdev*fin_kt),
                          data_type=rep("model",num_p))
      
      # plotting (just to check)
      #plot(obs_flow_unlog~obs_return_period,pch=16)
      #lines(con_flow_unlog~con_return_period_no_adj,col="black")
      #lines(con_flow_unlog~con_return_period_adj,col="red")
      #lines(model_flow_cms~model_return_period_yr,col="green")
      
      return(model_df)
    }
  } 
  else {
    
    # return df without completing any calculations
    # low flow analysis cannot be done when zeros make up >25% of data
    
    model_df=data.frame(SUB=rep(current_sub,num_p),
                        model_return_period_yr=1/(1-(model_p_list)), 
                        model_flow_log_cms=rep(as.numeric("NA"),num_p),
                        model_flow_cms=rep(as.numeric("NA"),num_p),
                        data_type=rep("model",num_p))
    return(model_df)
  }
}


# ---- 5. function: low flow freq analysis by subbasin ----

# observation for all subbasins in .rch file (uses obs_rch_freq_calcs function) 
obs_rch_lowflow_freq_calcs_all_subs=function(rch_data) {
  
  # calculate number of subbasins for for loop
  num_subs=length(unique(rch_data$SUB))
  
  # make dataframe for all outputs
  obs_df_all_subs=data.frame(SUB=as.integer(),
                             obs_return_period_yr=as.numeric(),
                             obs_min_flow_cms_adj=as.numeric(),
                             obs_min_flow_log_cms_adj=as.numeric(),
                             data_type=as.character())
  
  for (i in 1:num_subs) {
    sel_rch_data=rch_data %>% filter(SUB==i)
    obs_df_all_temp=obs_rch_lowflow_freq_calcs(sel_rch_data)
    obs_df_all_subs=bind_rows(obs_df_all_subs,obs_df_all_temp)
  }
  
  return(obs_df_all_subs)
}


# models for all subbasins in .rch file (uses model_rch_lowflow_freq_calcs function) 
model_rch_lowflow_freq_calcs_all_subs=function(obs_rch_lowflow_freq_calcs_all_subs_df,model_p_list,general_cskew) {

  # calculate number of subbasins for for loop
  num_subs=length(unique(obs_rch_lowflow_freq_calcs_all_subs_df$SUB))
  
  # make dataframe for all outputs
  model_df_all_subs=data.frame(SUB=as.integer(),
                      model_return_period_yr=as.numeric(),
                      model_flow_cms=as.numeric(),
                      model_flow_log_cms=as.numeric(),
                      data_type=as.character())
  
  for (i in 1:num_subs) {
    sel_rch_data=obs_rch_lowflow_freq_calcs_all_subs_df %>% filter(SUB==i)
    model_df_all_temp=model_rch_lowflow_freq_calcs(sel_rch_data,model_p_list,general_cskew)
    model_df_all_subs=bind_rows(model_df_all_subs,model_df_all_temp)
  }
  
  return(model_df_all_subs)
  }

# ---- 6. calculate obs and model ouptuts for each subbasin ----

baseline_obs_lowflow_calcs=obs_rch_lowflow_freq_calcs_all_subs(baseline_rch_data)
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

# omit NAs
baseline_model_lowflow_calcs_noNAa=baseline_model_lowflow_calcs %>% na.omit()
csiro4_5_model_lowflow_calcs_noNAa=csiro4_5_model_lowflow_calcs %>% na.omit()
csiro8_5_model_lowflow_calcs_noNAa=csiro8_5_model_lowflow_calcs %>% na.omit()
hadley4_5_model_lowflow_calcs_noNAa=hadley4_5_model_lowflow_calcs %>% na.omit()
miroc8_5_model_lowflow_calcs_noNAa=miroc8_5_model_lowflow_calcs %>% na.omit()

# plot observations and models together
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_cms_adj),baseline_obs_lowflow_calcs,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_lowflow_calcs_noNAa,color="black") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_lowflow_calcs_noNAa,color="orange") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_lowflow_calcs_noNAa,color="red") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_lowflow_calcs_noNAa,color="blue") +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_lowflow_calcs_noNAa,color="green") +
  facet_wrap(~SUB,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (cms)") +
  ylim(0,max(baseline_obs_lowflow_calcs$obs_min_flow_cms_adj)+50) +
  theme_bw()


# ---- X. one subbasin test ----

baseline_rch_data_sel=baseline_rch_data %>% filter(RCH==8)
baseline_obs_calcs=obs_rch_lowflow_freq_calcs(baseline_rch_data_sel)
#obs_rch_lowflow_freq_calcs_df=baseline_obs_calcs
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_calcs=model_rch_lowflow_freq_calcs(baseline_obs_calcs,my_model_p_list,0.4)

# plotting
plot(obs_min_flow_cms_adj~obs_return_period_yr,data=obs_rch_lowflow_freq_calcs_df,pch=16)

# use moving average
baseline_rch_data_sel_blah=baseline_rch_data_sel %>% 
  mutate(blah=movingAve(baseline_rch_data_sel$FLOW_OUTcms,span=7,pos="end")) %>%
  na.omit()
baseline_obs_calcs_blah=obs_rch_lowflow_freq_calcs(baseline_rch_data_sel_blah)


ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_cms_adj),baseline_obs_calcs,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_calcs,color="black") +
  #geom_line(aes(x=model_return_period,y=model_flow_cms),csiro4_5_model_calcs,color="orange") +
  #geom_line(aes(x=model_return_period,y=model_flow_cms),csiro8_5_model_calcs,color="red") +
  #geom_line(aes(x=model_return_period,y=model_flow_cms),hadley4_5_model_calcs,color="blue") +
  #geom_line(aes(x=model_return_period,y=model_flow_cms),miroc8_5_model_calcs,color="green") +
  facet_wrap(~SUB,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (cms)") +
  theme_bw()


# ---- 8. ----


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








movingAve <- function(x, span=3, order=0, pos="center") {
  ## Coding history:
  ##    2009Aug17 DLLorenz Original Coding
  ##    2012May24 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##
  ## get the correct position
  pos <- match.arg(pos, c("center", "begin", "end", "leading", "trailing"))
  if(pos == "leading")
    pos <- "begin"
  else if(pos == "trailing")
    pos <- "end"
  if(order >= span)
    stop("the value for order must be less than the value for span")
  ## Construct the filter matrix
  ## Note that for order greater than 0, the construction of the matrix is
  ##  based on linear model theory
  if(order > 0) {
    X <- cbind(1, poly(seq(span), order))
    filMat <- X %*% solve(crossprod(X)) %*% t(X)
  }
  else
    filMat <- matrix(1/span, ncol=span, nrow=span)
  if(span > length(x)) # need to protect against failure in filter
    retval <- rep(NA_real_, length(x))
  else if(pos == "center")
    retval <- stats::filter(x, filMat[span + 1 - trunc((span + 1)/2),])
  else if(pos == "begin")
    retval <- rev(stats::filter(rev(x), filMat[1L,], sides=1))
  else # Must be end
    retval <- stats::filter(x, filMat[1L,], sides=1)
  return(as.vector(retval))
}
