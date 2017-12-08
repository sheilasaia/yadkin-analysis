# zero_flow_change function

# purpose: calculate % change in number of zero flows btwn. baseline and projection data
# last updated: 20171208
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

zero_flow_change=function(baseline_zero_counts,projection_zero_counts) {
  
  # load libraries
  library(tidyverse)
  
  # define variables and output dataframe
  num_rchs=length(unique(baseline_zero_counts$RCH))
  dataset_temp=unique(projection_zero_counts$dataset)
  change_df=data.frame(RCH=as.integer(),
                       baseline_sum_n_zero_entries=as.numeric(),
                       projection_sum_n_zero_entries=as.numeric(),
                       zero_flow_perc_change=as.numeric(),
                       dataset=as.character())
  
  # for loop for each subbasin
  for (i in 1:num_rchs) {

    # baseline data for specified subbasin
    baseline_zero_counts_temp=baseline_zero_counts %>% filter(RCH==i)
    baseline_sum_n_zero_entries_temp=baseline_zero_counts_temp$sum_n_zero_entries
    
    # projection data for specified subbasin
    projection_zero_counts_temp=projection_zero_counts %>% filter(RCH==i)
    projection_sum_n_zero_entries_temp=projection_zero_counts_temp$sum_n_zero_entries
    
    # find percent change
    if (baseline_sum_n_zero_entries_temp==0) { # if starting point is zero then can't calculate percent change
      zero_flow_perc_change_temp=as.numeric("NA")
    }
    else {
      zero_flow_perc_change_temp=((projection_sum_n_zero_entries_temp-baseline_sum_n_zero_entries_temp)/baseline_sum_n_zero_entries_temp)*100
    }
    
    # save results to data frame
    change_df_temp=data.frame(RCH=i,
                              baseline_sum_n_zero_entries=baseline_sum_n_zero_entries_temp,
                              projection_sum_n_zero_entries=projection_sum_n_zero_entries_temp,
                              zero_flow_perc_change=zero_flow_perc_change_temp,
                              dataset=dataset_temp)
    
    # bind results to change_df
    change_df=bind_rows(change_df,change_df_temp)
  }
  
  # return output
  return(change_df) 
}
