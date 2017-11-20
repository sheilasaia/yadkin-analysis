# find outliers in SWAT .rch file for high flow risk analysis

# purpose: find outliers for high flow risk analysis
# last updated: 20171120
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

count_hiflow_outliers=function(rch_data) {
  # rch_data is formatted using reformat_rch_file()
  
  # load libraries
  library(tidyverse) # data management
  
  # select necessary info
  rch_data_sel=rch_data %>% select(RCH,MO,YR,FLOW_OUTcms)
  
  # define output data frames
  output_counts_df=data.frame(RCH=as.integer(),
                              YR=as.integer(),
                              mean_daily_flow_cms=as.numeric(),
                              median_daily_flow_cms=as.numeric(),
                              n_minor_hiflow=as.integer(),
                              n_major_hiflow=as.integer())
  output_bounds_df=data.frame(RCH=as.integer(),
                              minor_outlier_cutoff=as.numeric(),
                              major_outlier_cutoff=as.numeric())
  
  # calculate number of subbasins for for loop
  num_rchs=length(unique(rch_data_sel$RCH))
  
  # find minor and major outliers for each subbasin
  for (i in 1:num_rchs) {
    
    # select one subbasin
    temp_df=rch_data_sel %>% filter(RCH==i) %>%
      mutate(dataset="all_data")
    
    # calculate q's and interquartile range
    q1=as.numeric(quantile(temp_df$FLOW_OUTcms)[2])
    q2=as.numeric(quantile(temp_df$FLOW_OUTcms)[3]) # = median
    q3=as.numeric(quantile(temp_df$FLOW_OUTcms)[4])
    interquartile_range=q3-q1
    
    # minor outlier cutoff
    inner_fence_coeff=1.5 # within +3 standard deviations
    hibound_minor_outlier=q3+qrange*inner_fence_coeff
    
    # major outlier cutoff
    outter_fence_coeff=3 # within +6 stanadard deviations
    hibound_major_outlier=q3+qrange*outter_fence_coeff
    
    # save outlier data
    minor_hiflow_df=temp_df %>% 
      filter(FLOW_OUTcms>hibound_minor_outlier) %>%
      mutate(dataset="minor_outlier")
    major_hiflow_df=temp_df %>% 
      filter(FLOW_OUTcms>hibound_major_outlier) %>%
      mutate(dataset="major_outlier")
    
    # count outliers
    output_counts_temp_df=bind_rows(temp_df,minor_hiflow_df,major_hiflow_df) %>% # bind temp_df too so make sure to get all years
      group_by(RCH,YR) %>% 
      summarize(n_minor_hiflow=sum(dataset=="minor_outlier"),
                                   n_major_hiflow=sum(dataset=="major_outlier"))
    
    # format bounds information
    output_bounds_temp_df=data.frame(RCH=i,
                                     mean_daily_flow_cms=mean(temp_df$FLOW_OUTcms),
                                     median_daily_flow_cms=q2,
                                     minor_outlier_cutoff=hibound_minor_outlier,
                                     major_outlier_cutoff=hibound_major_outlier)
    
    # append temp output
    output_counts_df=bind_rows(output_counts_df,output_counts_temp_df)
    output_bounds_df=bind_rows(output_bounds_df,output_bounds_temp_df)
  }
  
  return(list(output_counts_df,output_bounds_df)) # returns list element with both dataframes
}
