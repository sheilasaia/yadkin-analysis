# count_hiflow_outliers function

# purpose: find outliers in SWAT .rch file for high flow risk analysis
# last updated: 20171120
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

count_hiflow_outliers=function(rch_data) {
  # rch_data is formatted using reformat_rch_file()
  
  # load libraries
  library(tidyverse) # data management
  
  # select necessary info
  rch_data_sel=rch_data %>% 
    select(RCH,MO,YR,FLOW_OUTcms)
  
  # define output data frames
  output_counts_df=data.frame(RCH=as.integer(),
                              YR=as.integer(),
                              n_minor_hiflow=as.integer(),
                              n_major_hiflow=as.integer())
  output_bounds_df=data.frame(RCH=as.integer(),
                              mean_daily_flow_cms_no_zeros=as.numeric(),
                              median_daily_flow_cms_no_zeros=as.numeric(),
                              minor_outlier_cutoff=as.numeric(),
                              major_outlier_cutoff=as.numeric())
  
  # calculate number of subbasins for for loop
  num_rchs=length(unique(rch_data_sel$RCH))
  
  # find minor and major outliers for each subbasin
  for (i in 1:num_rchs) {
    
    # select one subbasin
    temp_df=rch_data_sel %>% 
      filter(RCH==i) %>%
      mutate(dataset="all_data")
    
    temp_log_df=temp_df %>%
      filter(FLOW_OUTcms!=0) %>% # select only non-zero values only for quartile cacls
      mutate(log_FLOW_OUTcms=log(FLOW_OUTcms)) # take log (to ensure normality)
    
    # calculate q's and interquartile range
    q1=as.numeric(quantile(temp_log_df$log_FLOW_OUTcms)[2])
    q2=as.numeric(quantile(temp_log_df$log_FLOW_OUTcms)[3]) # = log of median
    q3=as.numeric(quantile(temp_log_df$log_FLOW_OUTcms)[4])
    interquartile_range=q3-q1
    
    # minor outlier cutoff
    inner_fence_coeff=1.5 # within +3 standard deviations
    hibound_minor_outlier=q3+interquartile_range*inner_fence_coeff
    
    # major outlier cutoff
    outter_fence_coeff=3 # within +6 stanadard deviations
    hibound_major_outlier=q3+interquartile_range*outter_fence_coeff
    
    # save outlier data
    minor_hiflow_df=temp_df %>% 
      filter(FLOW_OUTcms>exp(hibound_minor_outlier)) %>%
      mutate(dataset="minor_outlier")
    major_hiflow_df=temp_df %>% 
      filter(FLOW_OUTcms>exp(hibound_major_outlier)) %>%
      mutate(dataset="major_outlier")
    
    # count outliers
    output_counts_temp_df=bind_rows(temp_df,minor_hiflow_df,major_hiflow_df) %>% # bind temp_df too so make sure to get all years
      group_by(RCH,YR) %>% 
      summarize(n_minor_hiflow=sum(dataset=="minor_outlier"),
                n_major_hiflow=sum(dataset=="major_outlier"))
    
    # format bounds information
    output_bounds_temp_df=data.frame(RCH=i,
                                     mean_daily_flow_cms_no_zeros=exp(mean(temp_log_df$log_FLOW_OUTcms)),
                                     median_daily_flow_cms_no_zeros=exp(q2),
                                     minor_outlier_cutoff=exp(hibound_minor_outlier),
                                     major_outlier_cutoff=exp(hibound_major_outlier))
    
    # append temp output
    output_counts_df=bind_rows(output_counts_df,output_counts_temp_df)
    output_bounds_df=bind_rows(output_bounds_df,output_bounds_temp_df)
  }
  
  return(list(output_counts_df,output_bounds_df)) # returns list element with both dataframes
}
