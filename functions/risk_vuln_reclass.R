# reclassify risk and vulnerability from scaled sovi analysis

# purpose: reclassify risk and vulnerability from scaled sovi analysis
# last updated: 20171016
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

# define function
risk_vuln_reclass=function(flood_freq_sovi_data) {
  
  # run for each dataset
  # case 1: if flood freq is above 0 & sovi rescaled is equal to/below 0 (high risk/low vulnerability)
  # case 2: if flood freq is equal to/below 0 & sovi rescaled is equal to/below 0 (low risk/low vulnerability)
  # case 3: if flood freq is above 0 & sovi rescaled is above 0 (high risk/high vulnerability)
  # case 4: if flood freq is equal to/below 0 & sovi rescaled is above 0 (low risk/high vulnerability)
  
  # make output dataframe to hold results
  output_df=data.frame(SUB=as.numeric(),
                       dataset=as.character(),
                       perc_change=as.numeric(),
                       area_wtd_sovi=as.numeric(),
                       area_wtd_percentile=as.numeric(),
                       area_wtd_sovi_rescaled=as.numeric(),
                       sovi_class=as.character())
  num_datasets=length(unique(flood_freq_sovi_data$dataset))
  
  # iterate through data
  for (i in 1:num_datasets) {
    
    # select one dataset for analysis
    temp_dataset_name=as.character(unique(flood_freq_sovi_data$dataset)[i])
    temp_df=flood_freq_sovi_data %>% filter(dataset==temp_dataset_name)
    temp_sovi_class=as.character()
    
    # classify according to cases 1-4 above
    for (j in 1:dim(temp_df)[1]) {
      temp_perc_change=temp_df$perc_change[j]
      temp_area_wtd_sovi_rescaled=temp_df$area_wtd_sovi_rescaled[j]
      if (temp_perc_change>0 && temp_area_wtd_sovi_rescaled<=0) { #case 1
        temp_sovi_class[j]="hi_risk_lo_vuln"
      }
      else if (temp_perc_change<=0 & temp_area_wtd_sovi_rescaled<=0) { #case 2
        temp_sovi_class[j]="lo_risk_lo_vuln"
      }
      else if (temp_perc_change>0 & temp_area_wtd_sovi_rescaled>0) { #case 3
        temp_sovi_class[j]="hi_risk_hi_vuln"
      }
      else if (temp_perc_change<=0 & temp_area_wtd_sovi_rescaled>0) { #case 4
        temp_sovi_class[j]="lo_risk_hi_vuln"
      }
    }
    
    # save new column to temp_df
    temp_df$sovi_class=temp_sovi_class
    
    # record results in output_df
    output_df=bind_rows(output_df,temp_df)
  }
  return(output_df)
}
