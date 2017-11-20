# select observations for low flow frequench analysis (all reaches)

# purpose: prepare SWAT .rch file (observations) for low-flow frequency analysis
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

# observation for all reaches in .rch file (uses obs_lowflow_freq_calcs_one_rch function) 
obs_freq_calcs_all_rchs=function(rch_data,span_days,flow_option) {
    # rch_data is formatted using reformat_rch_file()
    # span_days is equal to the desired number of days being averaged for frequency analysis (e.g., 7 for 7-day flow analysis)
    # flow_option equal to "hiflow" or "lowflow" depending on which analysis type
    
    # load libraries
    library(tidyverse) # data management
    
    # calculate number of reaches for for loop
    num_rchs=length(unique(rch_data$RCH))
    
    if (flow_option=="hiflow") {
      # make data frame for all outputs
      obs_df_all_rchs=data.frame(RCH=as.integer(),
                                 YR=as.integer(),
                                 obs_return_period_yr=as.numeric(),
                                 obs_max_flow_cms_adj=as.numeric(),
                                 obs_max_flow_log_cms_adj=as.numeric(),
                                 data_type=as.character())
      for (i in 1:num_rchs) {
        # select only one reach
        sel_rch_data=rch_data %>% filter(RCH==i)
        
        # fill data frame
        obs_df_all_temp=obs_hiflow_freq_calcs_one_rch(sel_rch_data,span_days)
        
        # append to final output
        obs_df_all_rchs=bind_rows(obs_df_all_rchs,obs_df_all_temp)
      }
      
    } else if (flow_option=="lowflow") {
      # make data frame for all outputs
      obs_df_all_rchs=data.frame(RCH=as.integer(),
                                 YR=as.integer(),
                                 obs_return_period_yr=as.numeric(),
                                 obs_min_flow_cms_adj=as.numeric(),
                                 obs_min_flow_log_cms_adj=as.numeric(),
                                 data_type=as.character())
      
      for (i in 1:num_rchs) {
        # select only one reach
        sel_rch_data=rch_data %>% filter(RCH==i)
        
        # fill data frame
        obs_df_all_temp=obs_lowflow_freq_calcs_one_rch(sel_rch_data,span_days)
        
        # append to final output
        obs_df_all_rchs=bind_rows(obs_df_all_rchs,obs_df_all_temp)
      }
    } else {
      print("The flow_otion used was not valid.")
    }
  
  return(obs_df_all_rchs)
}