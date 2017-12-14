# reformat atsdr sovi data

# ---- 1 set up ----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)

# load pdf
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers")
sovi_raw=read_csv("us_sovi2014_albers.txt",col_names=TRUE,na=c("-999"))

# ---- 2 reformat data ----

# select only columns you need
sovi_data=sovi_raw %>%
  select(fips=FIPS,state_abbrev=ST_ABBR,county_name=COUNTY,
         sovi_theme1=SPL_THEME1,sovi_theme1_percentile=RPL_THEME1,
         sovi_theme2=SPL_THEME2,sovi_theme2_percentile=RPL_THEME2,
         sovi_theme3=SPL_THEME3,sovi_theme3_percentile=RPL_THEME3,
         sovi_theme4=SPL_THEME4,sovi_theme4_percentile=RPL_THEME4,
         sovi_total=SPL_THEMES,sovi_total_percentile=RPL_THEMES) %>%
  # remove -999 vaules (=NA)
  filter(sovi_theme1>=0 & sovi_theme1_percentile>=0 &
           sovi_theme2>=0 & sovi_theme2_percentile>=0 &
           sovi_theme3>=0 & sovi_theme3_percentile>=0 &
           sovi_theme4>=0 & sovi_theme4_percentile>=0 &
           sovi_total>=0 & sovi_total_percentile>=0)


# ---- 3 export data ----

# export reformatted data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
write_csv(sovi_data,"atsdr_us_sovi_data.csv")
