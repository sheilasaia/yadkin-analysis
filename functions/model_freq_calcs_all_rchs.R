# high flow and low flow log-Pearson type III models (all reaches)

# purpose: generate model curve for low-flow frequency analysis
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

model_freq_calcs_all_rchs=function(obs_freq_calcs_all_rchs_df,kn_table,model_p_list,general_cskew,flow_option) {
  # obs_freq_calcs_all_rchs_df is formatted using obs_freq_calcs_all_rchs()
  # model_p_list is a list of desired probabilities of exceedance or non-exceedance (for high flow and low flow analysis, respectively)
  # flow_option equal to "hiflow" or "lowflow" depending on which analysis type
  
  # calculate number of subbasins for for loop
  num_rchs=length(unique(obs_freq_calcs_all_rchs_df$RCH))
  
  # make dataframe for all outputs
  model_df_all_rchs=data.frame(RCH=as.integer(),
                               model_return_period_yr=as.numeric(),
                               model_flow_cms=as.numeric(),
                               model_flow_log_cms=as.numeric(),
                               data_type=as.character())
  
  for (i in 1:num_rchs) {
    sel_rch_data=obs_freq_calcs_all_rchs_df %>% filter(RCH==i)
    
    if (flow_option=="hiflow") {
      # fill data frame
      model_df_all_temp=model_hiflow_freq_calcs_one_rch(sel_rch_data,kn_table,model_p_list)
    }
    
    else if (flow_option=="lowflow") {
        
      # fill data frame
      model_df_all_temp=model_lowflow_freq_calcs_one_rch(sel_rch_data,kn_table,model_p_list,general_cskew)
    }
    
    else {
      print("The flow_otion used was not valid.")
    }
    
    model_df_all_rchs=bind_rows(model_df_all_rchs,model_df_all_temp)
    }
  return(model_df_all_rchs)
}

