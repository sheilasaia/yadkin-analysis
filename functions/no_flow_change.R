# no_flow_change function

# purpose: calculate % change in number of days with no flow btwn. baseline and projection data
# last updated: 20171208
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

no_flow_change=function(baseline_no_flow_counts,projection_no_flow_counts,num_yrs) {
  # assumes number of years of baseline dataset is same as number of years of projection (num_yrs)
  
  # load libraries
  library(tidyverse)
  
  # define variables and output dataframe
  num_rchs=length(unique(baseline_no_flow_counts$RCH))
  dataset_temp=unique(projection_no_flow_counts$dataset)
  change_df=data.frame(RCH=as.integer(),
                       n_base_flows=as.numeric(),
                       n_proj_flows=as.numeric(),
                       perc_change_per_yr=as.numeric(),
                       dataset=as.character())
  
  # for loop for each subbasin
  for (i in 1:num_rchs) {

    # baseline data for specified subbasin
    baseline_no_flow_counts_temp=baseline_no_flow_counts %>% filter(RCH==i)
    n_base_flows_temp=baseline_no_flow_counts_temp$sum_n_no_flow_entries
    
    # projection data for specified subbasin
    projection_no_flow_counts_temp=projection_no_flow_counts %>% filter(RCH==i)
    n_proj_flows_temp=projection_no_flow_counts_temp$sum_n_no_flow_entries
    
    # find percent change
    if (n_base_flows_temp==0) { # if starting point is zero then can't calculate percent change
      perc_change_temp=as.numeric("NA")
    }
    else {
      perc_change_temp=((n_proj_flows_temp-n_base_flows_temp)/n_base_flows_temp)*100
    }
    
    # save results to data frame
    change_df_temp=data.frame(RCH=i,
                              n_base_flows=n_base_flows_temp,
                              n_proj_flows=n_proj_flows_temp,
                              perc_change_per_yr=perc_change_temp/num_yrs,
                              dataset=dataset_temp)
    
    # bind results to change_df
    change_df=bind_rows(change_df,change_df_temp)
  }
  
  # return output
  return(change_df) 
}
