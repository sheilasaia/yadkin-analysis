# select observations for low-flow frequench analysis (all reaches)

# purpose: prepare SWAT .rch file (observations) for low-flow frequency analysis
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

# observation for all reaches in .rch file (uses obs_lowflow_freq_calcs_one_rch function) 
obs_freq_calcs_all_rchs=function(rch_data,span_days,flow_option) {
  # rch_data is formatted using reformat_rch_file()
  # span_days is equal to the desired number of days being averaged for frequency analysis (e.g., 7 for 7-day flow analysis)
  # flow_option equal to "flood" or "lowflow" depending on which analysis type
  
  # load libraries
  library(smwrBase) # for movingAve()
  library(tidyverse) # data management
  
  # calculate number of reaches for for loop
  num_rchs=length(unique(rch_data$RCH))
  
  for (i in 1:num_rch) {
    sel_rch_data=rch_data %>% filter(RCH==i)
    
    if (flow_option=="flood") {
      # make data frame for all outputs
      obs_df_all_rchs=data.frame(RCH=as.integer(),
                                 YR=as.integer(),
                                 obs_return_period_yr=as.numeric(),
                                 obs_max_flow_cms_adj=as.numeric(),
                                 obs_max_flow_log_cms_adj=as.numeric(),
                                 data_type=as.character())
      
      # fill data frame
      obs_df_all_temp=obs_flood_freq_calcs_one_rch(sel_rch_data,span)
      
    } else (flow_option=="lowflow") {
      # make data frame for all outputs
      obs_df_all_rchs=data.frame(RCH=as.integer(),
                                 YR=as.integer(),
                                 obs_return_period_yr=as.numeric(),
                                 obs_min_flow_cms_adj=as.numeric(),
                                 obs_min_flow_log_cms_adj=as.numeric(),
                                 data_type=as.character())
      
      # fill data frame
      obs_df_all_temp=obs_lowflow_freq_calcs_one_rch(sel_rch_data,span)
    }
    
    obs_df_all_rchs=bind_rows(obs_df_all_rchs,obs_df_all_temp)
  }
  
  return(obs_df_all_rchs)
}