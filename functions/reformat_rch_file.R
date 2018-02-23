# reformat_rch_file function

# purpose: prepare SWAT .rch file for analysis
# last updated: 2018218
# author: sheila saia
# contact: ssaia [at] ncsu [dot] edu

# reformat SWAT, raw .rch file for other analyses
reformat_rch_file=function(raw_rch_data) {
  # import raw_rch_data into R session using: 
  # raw_rch_data_raw=read_table2("output.rch", col_names=FALSE, skip=9)
  
  # load libraries
  library(tidyverse) # data management
  
  # column names
  rch_col_names=c("FILE","RCH","GIS","MO","DA","YR","AREAkm2","FLOW_INcms","FLOW_OUTcms","EVAPcms",
                  "TLOSScms","SED_INtons","SED_OUTtons","SEDCONCmg_kg","ORGN_INkg","ORGN_OUTkg",
                  "ORGP_INkg", "ORGP_OUTkg","NO3_INkg","NO3_OUTkg","NH4_INkg","NH4_OUTkg",
                  "NO2_INkg", "NO2_OUTkg","MINP_INkg","MINP_OUTkg","CHLA_INkg","CHLA_OUTkg",
                  "CBOD_INkg","CBOD_OUTkg","DISOX_INkg","DISOX_OUTkg","SOLPST_INmg",
                  "SOLPST_OUTmg","SORPST_INmg","SORPST_OUTmg","REACTPSTmg","VOLPSTmg",
                  "SETTLPSTmg","RESUSP_PSTmg","DIFFUSEPSTmg","REACBEDPSTmg","BURYPSTmg",
                  "BED_PSTmg","BACTP_OUTct","BACTLP_OUTct","CMETAL_1kg","CMETAL_2kg","CMETAL_3kg",
                  "TOTNkg","TOTPkg","NO3_mg_l","WTMPdegc")
  
  # reassign column names
  colnames(raw_rch_data) = rch_col_names
  
  # remove unnecessary columns
  rch_data = raw_rch_data %>% 
  select(RCH, MO:FLOW_OUTcms)
  
  return(rch_data)
}