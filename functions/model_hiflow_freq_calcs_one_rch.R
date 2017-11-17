# high flow log-Pearson type III models (one reach)

# purpose: generate model curve for high flow frequency analysis
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

# define function
model_hiflow_freq_calcs_one_rch=function(obs_hiflow_freq_calcs_one_rch_df,kn_table,model_p_list) {
  # obs_lowflow_freq_calcs_df is the output dataframe from obs_lowflow_freq_calcs_one_rch()
  # kn value comes from appendix 4 of the USGS bulletin 17b
  # model_p_list is a list of desired probabilities of exceedance
  
  # references:
  # http://streamflow.engr.oregonstate.edu/analysis/floodfreq/meandaily_tutorial.htm
  # http://www.hydrology.bee.cornell.edu/BEE473Homework_files/RiskAnalysis.pdf
  # USGS Riggs, 1972
  # USGS Bulletin 17B, 1982
  
  # save temporary variables
  current_rch=unique(obs_hiflow_freq_calcs_one_rch_df$RCH)
  num_p=length(model_p_list)
  num_yrs=dim(obs_hiflow_freq_calcs_one_rch_df)[1]
  flow_option="hiflow"
  
  # make output dataframe to hold results
  model_df=data.frame(RCH=as.integer(),
                      model_return_period_yr=as.numeric(),
                      model_flow_cms=as.numeric(),
                      model_flow_log_cms=as.numeric(),
                      data_type=as.character())
  
  # remove outliers
  obs_temp_data=remove_outliers(obs_hiflow_freq_calcs_one_rch_df,kn_table,flow_option)
  
  # log inputs and find mean
  obs_flow_log=obs_temp_data$obs_max_flow_log_cms_adj
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
  
  # calculate model kt
  model_kt=logpearson3_factor_calc(model_p_list,obs_cskew)
  
  # calculate final modeled flow values
  model_flow_log=obs_mean+obs_stdev*model_kt
  model_flow_unlog=exp(obs_mean+obs_stdev*model_kt)
  
  # final output dataframe
  model_df=data.frame(RCH=rep(current_rch,num_p),
                      model_return_period_yr=1/model_p_list,
                      model_flow_log_cms=model_flow_log,
                      model_flow_cms=model_flow_unlog,
                      data_type=rep("model",num_p))
  
  return(model_df)
}
