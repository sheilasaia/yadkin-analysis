# flow_change function

# purpose: calculate % change in flow btwn. baseline and projection models for given return period
# last updated: 20171006
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

flow_change=function(return_period,baseline_model_calcs,projection_model_calcs) {
  # required to run model_freq_calcs_all_rchs() for baseline and projection datasets for inputs here
  # return_period must correspond to a return_period_yr entry in both model_freq_calcs_all_rchs() outputs
  
  # select only data for return period of interest
  baseline_return_period_sel=baseline_model_calcs %>% filter(round(model_return_period_yr,4)==return_period)
  projection_return_period_sel=projection_model_calcs %>% filter(round(model_return_period_yr,4)==return_period)
  
  # define variables and output dataframe
  num_rchs=length(unique(baseline_model_calcs$RCH))
  change_df=data.frame(RCH=as.integer(),
                       return_period_yr=as.numeric(),
                       baseline_model_flow_cms=as.numeric(),
                       projection_model_flow_cms=as.numeric(),
                       flow_change_cms=as.numeric(), # via subtraction
                       flow_change_perc=as.numeric())
  
  # for loop for each subbasin
  for (i in 1:num_rchs) {
    
    # baseline data for specified return period and subbasin
    baseline_rch_temp=baseline_return_period_sel %>% filter(RCH==i)
    baseline_rch_flow_temp=baseline_rch_temp$model_flow_cms
    
    # projection data for specified return period and subbasin
    projection_rch_temp=projection_return_period_sel %>% filter(RCH==i)
    projection_rch_flow_temp=projection_rch_temp$model_flow_cms
    
    # find percent change
    flow_change_cms_temp=projection_rch_flow_temp-baseline_rch_flow_temp
    flow_change_perc_temp=(flow_change_cms_temp/baseline_rch_flow_temp)*100
    
    # save results to data frame
    change_df_temp=data.frame(RCH=i,
                              return_period_yr=return_period,
                              baseline_model_flow_cms=baseline_rch_flow_temp,
                              projection_model_flow_cms=projection_rch_flow_temp,
                              flow_change_cms=flow_change_cms_temp,
                              flow_change_perc=flow_change_perc_temp)
    
    # bind results to change_df
    change_df=bind_rows(change_df,change_df_temp)
  }
  
  # return output
  return(change_df) 
}