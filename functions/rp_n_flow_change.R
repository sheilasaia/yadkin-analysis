# rp_n_flow_change function

# purpose: determine percent change in number of flows greater than or equal to a specified return period
# last updated: 20180216
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

rp_n_flow_change=function(return_period, baseline_model_calcs, baseline_rch_data, projection_rch_data, flow_option) {
  # get baseline_model_calcs from model_freq_calcs_all_rchs()
  # get baseline_rch_data and projection_rch_data from reformat_rch_file()
  # flow_option must be either 'lowflow' or 'hiflow'
  
  # load libraries
  library(tidyverse) # data management
  
  # define variables
  num_sim_yrs = length(unique(baseline_rch_data$YR))
  num_rchs = length(unique(baseline_rch_data$RCH))
  model_calcs_return_period_sel = baseline_model_calcs %>%
    filter(model_return_period_yr == return_period)
  output_df = data.frame(RCH = as.numeric(),
                         cutoff_flow_cms = as.numeric(),
                         n_base_flows = as.numeric(), 
                         n_proj_flows = as.numeric(),
                         perc_change_per_yr = as.numeric())
  
  for (i in 1:num_rchs) {
    # grab cutoff flow from baseline model calcs
    cutoff_flow_sel_temp = model_calcs_return_period_sel$model_flow_cms[model_calcs_return_period_sel$RCH == i]
    
    # select baseline and projected rch data for subbasin of interest
    baseline_rch_data_temp = baseline_rch_data %>% 
      filter(RCH == i)
    projected_rch_data_temp = projection_rch_data %>%
      filter(RCH == i)
    
    if (flow_option == 'hiflow') {
      # tally instances where flow is greater than or equal to cutoff
      baseline_sel_flows = baseline_rch_data_temp %>%
        mutate(my_tally = if_else(
          FLOW_OUTcms >= cutoff_flow_sel_temp, 1, 0))
      projected_sel_flow = projected_rch_data_temp %>%
        mutate(my_tally = if_else(
          FLOW_OUTcms >= cutoff_flow_sel_temp, 1, 0))
    }
    else if (flow_option == 'lowflow') {
      # tally instances where flow is less than or equal to cutoff
      baseline_sel_flows = baseline_rch_data_temp %>%
        mutate(my_tally = if_else(
          FLOW_OUTcms <= cutoff_flow_sel_temp, 1, 0))
      projected_sel_flow = projected_rch_data_temp %>%
        mutate(my_tally = if_else(
          FLOW_OUTcms <= cutoff_flow_sel_temp, 1, 0))
    }
    else {
      print('User specified flow_option is not valid.')
    }
    
    # summarize tally of instances
    baseline_counts_temp = baseline_sel_flows %>%
      group_by(RCH) %>% 
      summarize(n_base_flows = sum(my_tally))
    projection_counts_temp = projected_sel_flow %>%
      group_by(RCH) %>%
      summarize(n_proj_flows = sum(my_tally))
    
    # combine ouputs into one data frame
    output_df_temp = left_join(baseline_counts_temp,projection_counts_temp, by = "RCH") %>%
      mutate(perc_change_per_yr = (((n_proj_flows - n_base_flows)/n_base_flows) * 100)/num_sim_yrs,
             cutoff_flow_cms = cutoff_flow_sel_temp) %>%
      select(RCH, cutoff_flow_cms, n_base_flows:perc_change_per_yr) # include flow and rearrange

    # fix divide by zero error
    output_df_temp$perc_change_per_yr[output_df_temp$perc_change_per_yr == Inf | output_df_temp$perc_change_per_yr == "NaN"] = NA
    
    # append to output_df
    output_df = bind_rows(output_df,output_df_temp)

  }
  
  return(output_df)
}