# flow_cutoff_to_rp function

# purpose: convert outlier flow cutoff to a return period
# last updated: 20180215
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

outlier_flow_cutoff_to_rp=function(baseline_model_calcs, baseline_outlier_cutoffs) {
  # get baseline_model_calcs from model_freq_calcs_all_rchs() and get
  # baseline_outlier_cutoffs from second list element of 
  # count_hiflow_outliers() or count_lowflow_outliers()
  
  # load libraries
  library(tidyverse) # data management
  
  # set up output dataframe
  output_df = data.frame(RCH = as.numeric(),
                         minor_outlier_cutoff_cms = as.numeric(),
                         minor_rp_years = as.numeric(),
                         minor_prob_exced = as.numeric(),
                         major_outlier_cutoff_cms = as.numeric(),
                         major_rp_years = as.numeric(),
                         major_prob_exced = as.numeric())
  
  # number of rchs
  num_rchs = length(unique(baseline_model_calcs$RCH))
  
  for (i in 1:num_rchs) {
    temp_model_sel = baseline_model_calcs %>%
      filter(RCH==i)
    temp_rp_fit=lm(log(model_return_period_yr) ~ model_flow_cms, data = temp_model_sel)
    temp_intercept = summary(temp_rp_fit)$coef[1]
    temp_slope = summary(temp_rp_fit)$coef[2]
    
    # select minor cutoffs
    temp_minor_cutoff_sel = baseline_outlier_cutoffs$minor_outlier_cutoff[baseline_outlier_cutoffs$RCH==i]
    temp_major_cutoff_sel = baseline_outlier_cutoffs$major_outlier_cutoff[baseline_outlier_cutoffs$RCH==i]
    
    # calculate return period and excedence probability
    temp_minor_rp_predict = exp(sub_slope*temp_minor_cutoff_sel + sub_intercept)
    temp_minor_prob_predict = 1/temp_minor_rp_predict
    temp_major_rp_predict = exp(sub_slope*temp_major_cutoff_sel + sub_intercept)
    temp_major_prob_predict = 1/temp_major_rp_predict
    
    # save results to data frame
    output_df_temp = data.frame(RCH = i,
                                minor_outlier_cutoff_cms = temp_minor_cutoff_sel,
                                minor_rp_years = temp_minor_rp_predict,
                                minor_prob_exced = temp_minor_prob_predict,
                                major_outlier_cutoff_cms = temp_major_cutoff_sel,
                                major_rp_years = temp_major_rp_predict,
                                major_prob_exced = temp_major_prob_predict)
    
    # bind results to output_df
    output_df=bind_rows(output_df,output_df_temp)
    
  }

  return(output_df)
}