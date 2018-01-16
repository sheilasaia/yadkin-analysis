# yadkin climate change cost analysis
# last updated 20180116

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)

# ---- 2.1 import data ----

# set working direcorty and import high flow and low flow outlier percent change data 
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
hiflow_change_data <- read_csv("hiflow_outlier_perc_change_data.csv", col_names = TRUE)
lowflow_change_data <- read_csv("lowflow_outlier_perc_change_data.csv", col_names = TRUE)

# set working direcorty and import Hsiang et al. 2017 damages data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/climate_damage_cost_2012/reformatted_data")
damage_data_raw <- read_csv("county_damages_by_sector_reformatted.csv", col_names = TRUE)

# tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_raw <- read_csv("sovibd_2014_scaling_allsubs.csv", col_names = TRUE)


# ---- 2.2 reformat data ----

# select only tract scaling data that is needed
yadkin_fips_data <- sovibd_scaling_raw %>%
  select(SUB, fips) %>%
  mutate(county_fips = as.numeric(str_sub(as.character(fips), 1, 5))) %>%
  select(-fips) %>%
  distinct()

# select only damage data that is needed
damage_data <- damage_data_raw %>%
  select(county_name:avg_county_income_2012, total_damages_perc_county_income)

# use join to select out yadkin damage data
yadkin_damage_data <- left_join(yadkin_fips_data, damage_data,by = "county_fips")


# ---- 3.1 plot data ----

setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_projected_damages.pdf", width = 11, height = 8.5)
ggplot(data=yadkin_damage_data) +
  geom_point(aes(x = avg_county_income_2012, y = total_damages_perc_county_income, 
                 size = county_pop_2012, color = SUB), alpha = 0.75) +
  labs(x = "Per Person Income",y = "Total Damages (% County Income)") +
  ylim(-5,15) +
  scale_color_continuous("Subbasin ID", low = "grey75", high = "black") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=16))
dev.off()
