# low-flow log-Pearson type III models (all reaches)

# purpose: generate model curve for low-flow frequency analysis
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

model_freq_calcs_all_rchs=function(obs_freq_calcs_all_rchs_df,model_p_list,general_cskew,flow_option) {
  # obs_freq_calcs_all_rchs_df is formatted using obs_freq_calcs_all_rchs()
  # model_p_list is a list of desired probabilities of exceedance or non-exceedance (for flooding and low-flow analysis, respectively)
  # flow_option equal to "flood" or "lowflow" depending on which analysis type
  
  # calculate number of RCHbasins for for loop
  num_subs=length(unique(obs_rch_lowflow_freq_calcs_all_subs_df$RCH))
  
  # make dataframe for all outputs
  model_df_all_subs=data.frame(RCH=as.integer(),
                               model_return_period_yr=as.numeric(),
                               model_flow_cms=as.numeric(),
                               model_flow_log_cms=as.numeric(),
                               data_type=as.character())
  
  for (i in 1:num_rchs) {
    sel_rch_data=obs_rch_lowflow_freq_calcs_all_rchs_df %>% filter(RCH==i)
    
    if (flow_option=="flood") {
      # fill data frame
      model_df_all_temp=model_rch_flood_freq_calcs_one_rch(sel_rch_data,model_p_list,general_cskew)
      
      } else { # must be "lowflow"
      # fill data frame
      model_df_all_temp=model_rch_lowflow_freq_calcs_one_rch(sel_rch_data,model_p_list,general_cskew)
      }
    
    model_df_all_rchs=bind_rows(model_df_all_rchs,model_df_all_temp)
    }
  return(model_df_all_rchs)
}

