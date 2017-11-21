# count_hiflow_outliers_using_baseline function

# purpose: find outliers in SWAT .rch file for high flow risk analysis
# last updated: 20171120
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

count_hiflow_outliers_using_baseline=function(baseline_outlier_cutoffs,projection_rch_data) {
  # baseline_outlier_cutoffs comes from count_hiflow_outliers()[[2]] (second list output)
  # projection_data is formatted using reformat_rch_file()
  # uses baseline data to calculate minor and major outlier cutoffs and then applies this
  # to select projection data
  
  # load libraries
  library(tidyverse) # data management
  
  # select necessary info
  projection_data_sel=projection_rch_data %>% select(RCH,MO,YR,FLOW_OUTcms)
  
  # define output data frames
  output_counts_df=data.frame(RCH=as.integer(),
                              YR=as.integer(),
                              n_minor_hiflow=as.integer(),
                              n_major_hiflow=as.integer())
  output_bounds_df=data.frame(RCH=as.integer(),
                              minor_outlier_cutoff=as.numeric(),
                              major_outlier_cutoff=as.numeric())
  
  # calculate number of subbasins for for loop
  num_rchs=length(unique(projection_rch_data$RCH))
  
  # find minor and major outliers for each subbasin
  for (i in 1:num_rchs) {
    
    # select one subbasin
    baseline_cutoff_df_temp=baseline_outlier_cutoffs %>% filter(RCH==i)
    projection_df_temp=projection_data_sel %>% filter(RCH==i) %>% mutate(dataset="all_data")
    
    # baseline minor outlier cutoff
    hibound_minor_outlier=baseline_cutoff_df_temp$minor_outlier_cutoff
    
    # baseline major outlier cutoff
    hibound_major_outlier=baseline_cutoff_df_temp$major_outlier_cutoff
    
    # save projection outlier data
    minor_hiflow_df=projection_df_temp %>% 
      filter(FLOW_OUTcms>hibound_minor_outlier) %>%
      mutate(dataset="minor_outlier")
    major_hiflow_df=projection_df_temp %>% 
      filter(FLOW_OUTcms>hibound_major_outlier) %>%
      mutate(dataset="major_outlier")
    
    # count outliers
    output_counts_df_temp=bind_rows(projection_df_temp,minor_hiflow_df,major_hiflow_df) %>% # bind baseline_df_temp too so make sure to get all years
      group_by(RCH,YR) %>% 
      summarize(n_minor_hiflow=sum(dataset=="minor_outlier"),
                n_major_hiflow=sum(dataset=="major_outlier"))
    
    # format bounds information (this should be the same as for baseline_outlier_cutoffs)
    output_bounds_df_temp=data.frame(RCH=i,
                                     minor_outlier_cutoff=hibound_minor_outlier,
                                     major_outlier_cutoff=hibound_major_outlier)
    
    # append temp output
    output_counts_df=bind_rows(output_counts_df,output_counts_df_temp)
    output_bounds_df=bind_rows(output_bounds_df,output_bounds_df_temp)
  }
  
  return(list(output_counts_df,output_bounds_df)) # returns list element with both dataframes
}
