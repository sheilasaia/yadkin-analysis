# identify and remove high and low outliers

# purpose: remove high and low outliers then give new df
# last updated: 20170912
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

# references: USGS bulletin 17b

remove_outliers=function(obs_input_df,kn_table) {
  # example obs_input_df is the output from obs_lowflow_freq_calcs_one_rch()
  
  # log inputs and find mean
  input_flow_unlog=obs_input_df$obs_min_flow
  temp_flow_log=log(input_flow_unlog)
  temp_mean=mean(temp_flow_log)
  
  # calculate difference from mean
  temp_mean_diff=temp_flow_log-temp_mean
  temp_mean_diff_sqrd=temp_mean_diff^2
  temp_mean_diff_sqrd_sum=sum(temp_mean_diff_sqrd)
  temp_mean_diff_cubed=temp_mean_diff^3
  temp_mean_diff_cubed_sum=sum(temp_mean_diff_cubed)
  
  # calculate coefficient of skew (cskew)
  num_yrs=length(input_flow_unlog)
  temp_variance=(1/(num_yrs-1))*temp_mean_diff_sqrd_sum
  temp_stdev=sqrt(temp_variance)
  temp_cskew=(num_yrs*temp_mean_diff_cubed_sum)/((num_yrs-1)*(num_yrs-2)*(temp_stdev^3))
  
  # if skew is between -0.4 and +0.4 then no need to remove outliers, otherwise, need to
  if (temp_cskew<-0.4) {
    
    if (num_yrs<10 | num_yrs>140) {
      print("Sample size is not between 10 and 140. Cannot run outlier analysis.")
    } else {
      # look up kn value
      kn_value=kn_table$kn_value[kn_table$sample_size==num_yrs]
      
      temp_flow_low_limit=temp_mean-kn_value*temp_stdev
      #output new df w/in limits
    }
  }
  
  if (temp_overzero_cskew>0.4) {
    
    # look up kn value
    kn_value=kn_table$kn_value[kn_table$sample_size==num_yrs]
    
    temp_flow_low_limit=temp_overzero_mean+kn_value*temp_overzero_stdev
    #output new df w/in limits
  }
  
}

