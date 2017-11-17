# identify and remove high and low outliers

# purpose: remove high and low outliers then give new df
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

# references: USGS bulletin 17b

remove_outliers=function(obs_flow_input_df,kn_table,flow_option) {
  # example obs_flow_input_df is the output from obs_lowflow_freq_calcs_one_rch()
  # kn value comes from appendix 4 of the USGS bulletin 17b
  # flow_option equals either "hiflow" or "lowflow" depending on which analysis
  
  if (flow_option=="hiflow") {
    # log inputs and find mean
    input_flow_log=obs_flow_input_df$obs_max_flow_log_cms_adj
    input_mean=mean(input_flow_log)
    
    # calculate difference from mean
    input_mean_diff=input_flow_log-input_mean
    input_mean_diff_sqrd=input_mean_diff^2
    input_mean_diff_sqrd_sum=sum(input_mean_diff_sqrd)
    input_mean_diff_cubed=input_mean_diff^3
    input_mean_diff_cubed_sum=sum(input_mean_diff_cubed)
    
    # calculate coefficient of skew (cskew)
    num_yrs=dim(obs_flow_input_df)[1]
    input_variance=(1/(num_yrs-1))*input_mean_diff_sqrd_sum
    input_stdev=sqrt(input_variance)
    input_cskew=(num_yrs*input_mean_diff_cubed_sum)/((num_yrs-1)*(num_yrs-2)*(input_stdev^3))
    
    if (input_cskew>=-0.4 & input_cskew<=0.4) {
      
      # if skew is between -0.4 and +0.4 then no need to remove outliers, otherwise, need to
      return(obs_flow_input_df)
      
    } 
    else {
      if (input_cskew<-0.4) {
        
        # look up kn value (only defined for 10-140 samples)
        kn_value=kn_table$kn_value[kn_table$sample_size==num_yrs]
        
        # calculate kn value
        input_lowflow_limit=input_mean-kn_value*input_stdev
        
        #output new df
        obs_output_df=obs_flow_input_df %>% filter(obs_max_flow_log_cms_adj>input_lowflow_limit)
        
        return(obs_output_df)
        
      }
      else {
        # case when (input_cskew>0.4)
        # look up kn value (only defined for 10-140 samples)
        kn_value=kn_table$kn_value[kn_table$sample_size==num_yrs]
        
        input_highflow_limit=input_mean+kn_value*input_stdev
        #output new df w/in limits
        
        #output new df
        obs_output_df=obs_flow_input_df %>% filter(obs_max_flow_log_cms_adj<input_highflow_limit)
        
        return(obs_output_df)
      }
    }
  }
  else { # must be flow_option="lowflow"
    # log inputs and find mean
    input_flow_log=obs_flow_input_df$obs_min_flow_log_cms_adj
    input_mean=mean(input_flow_log)
    
    # calculate difference from mean
    input_mean_diff=input_flow_log-input_mean
    input_mean_diff_sqrd=input_mean_diff^2
    input_mean_diff_sqrd_sum=sum(input_mean_diff_sqrd)
    input_mean_diff_cubed=input_mean_diff^3
    input_mean_diff_cubed_sum=sum(input_mean_diff_cubed)
    
    # calculate coefficient of skew (cskew)
    num_yrs=dim(obs_flow_input_df)[1]
    input_variance=(1/(num_yrs-1))*input_mean_diff_sqrd_sum
    input_stdev=sqrt(input_variance)
    input_cskew=(num_yrs*input_mean_diff_cubed_sum)/((num_yrs-1)*(num_yrs-2)*(input_stdev^3))
    
    if (input_cskew>=-0.4 & input_cskew<=0.4) {
      
      # if skew is between -0.4 and +0.4 then no need to remove outliers, otherwise, need to
      return(obs_flow_input_df)
      
    } 
    else {
      if (input_cskew<-0.4) {
        
        # look up kn value (only defined for 10-140 samples)
        kn_value=kn_table$kn_value[kn_table$sample_size==num_yrs]
        
        # calculate kn value
        input_lowflow_limit=input_mean-kn_value*input_stdev
        
        #output new df
        obs_output_df=obs_flow_input_df %>% filter(obs_min_flow_log_cms_adj>input_lowflow_limit)
        
        return(obs_output_df)
        
      }
      else {
        # case when (input_cskew>0.4)
        # look up kn value (only defined for 10-140 samples)
        kn_value=kn_table$kn_value[kn_table$sample_size==num_yrs]
        
        input_highflow_limit=input_mean+kn_value*input_stdev
        #output new df w/in limits
        
        #output new df
        obs_output_df=obs_flow_input_df %>% filter(obs_min_flow_log_cms_adj<input_highflow_limit)
        
        return(obs_output_df)
      }
    }
  }
}

