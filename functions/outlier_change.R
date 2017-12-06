# outlier_change function

# purpose: calculate % change in number of minor and major outliers btwn. baseline and projection data
# last updated: 20171120
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

outlier_change=function(baseline_outlier_counts_sum,projection_outlier_counts_sum,flow_option) {
  # required to run count_hiflow_outliers() or count_lowlfow_outliers(), 
  # save first list output (=counts), and sum counts by RCH
  # possible flow_option calls: "hiflow" or "lowflow" 
  
  # load libraries
  library(tidyverse)
  
  # define variables and output dataframe
  num_rchs=length(unique(baseline_outlier_counts_sum$RCH))
  dataset_temp=unique(projection_outlier_counts_sum$dataset)
  change_df=data.frame(RCH=as.integer(),
                       minor_outlier_perc_change=as.numeric(),
                       major_outlier_perc_change=as.numeric(),
                       dataset=as.character())
  
  # for loop for each subbasin
  for (i in 1:num_rchs) {
    
    if (flow_option=="hiflow") {
      # baseline data for specified subbasin
      baseline_rch_temp=baseline_outlier_counts_sum %>% filter(RCH==i)
      baseline_minor_temp=baseline_rch_temp$sum_minor_hiflow
      baseline_major_temp=baseline_rch_temp$sum_major_hiflow
      
      # projection data for specified subbasin
      projection_rch_temp=projection_outlier_counts_sum %>% filter(RCH==i)
      projection_minor_temp=projection_rch_temp$sum_minor_hiflow
      projection_major_temp=projection_rch_temp$sum_major_hiflow
      
      # find percent change
      minor_outlier_perc_change_temp=((projection_minor_temp-baseline_minor_temp)/baseline_minor_temp)*100
      major_outlier_perc_change_temp=((projection_major_temp-baseline_major_temp)/baseline_major_temp)*100
      
      # save results to data frame
      change_df_temp=data.frame(RCH=i,
                                minor_outlier_perc_change=minor_outlier_perc_change_temp,
                                major_outlier_perc_change=major_outlier_perc_change_temp,
                                dataset=dataset_temp)
      
      # bind results to change_df
      change_df=bind_rows(change_df,change_df_temp)
    }
    
    else if (flow_option=="lowflow") {
      # baseline data for specified subbasin
      baseline_rch_temp=baseline_outlier_counts_sum %>% filter(RCH==i)
      baseline_minor_temp=baseline_rch_temp$sum_minor_lowflow
      baseline_major_temp=baseline_rch_temp$sum_major_lowflow
      
      # projection data for specified subbasin
      projection_rch_temp=projection_outlier_counts_sum %>% filter(RCH==i)
      projection_minor_temp=projection_rch_temp$sum_minor_lowflow
      projection_major_temp=projection_rch_temp$sum_major_lowflow
      
      # find percent change
      if (baseline_minor_temp==0) { # if starting point is zero then can't calculate percent change
        minor_outlier_perc_change_temp=as.numeric("NA")
        }
      else {
        minor_outlier_perc_change_temp=((projection_minor_temp-baseline_minor_temp)/baseline_minor_temp)*100
      }
      
      if (baseline_major_temp==0) { # if starting point is zero then can't calculate percent change
        major_outlier_perc_change_temp=as.numeric("NA")
      }
      else {
        major_outlier_perc_change_temp=((projection_major_temp-baseline_major_temp)/baseline_major_temp)*100
      }
      
      # save results to data frame
      change_df_temp=data.frame(RCH=i,
                                minor_outlier_perc_change=minor_outlier_perc_change_temp,
                                major_outlier_perc_change=major_outlier_perc_change_temp,
                                dataset=dataset_temp)
      
      # bind results to change_df
      change_df=bind_rows(change_df,change_df_temp)
    }
    
    else {
      print("The flow_otion used was not valid.")
    }
  }
  
  # return output
  return(change_df) 
}
