# combined analysis on high, low, and no flows

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)

# set working directory
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")

# import high, low, and no flow results
n_hiflow_10yr_change_data = read_csv("num_hiflow_change_10yr_calcs.csv",col_names=TRUE)
n_lowflow_10yr_change_data = read_csv("num_lowflow_change_10yr_calcs.csv",col_names=TRUE)
n_no_flow_change_data = read_csv("num_no_flow_change_calcs.csv",col_names=TRUE)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)

# ---- 2 rank percent change for each analysis by climate scenario ----

# rank percent changes

# high flow
hiflow_rank_data = n_hiflow_10yr_change_data %>%
  group_by(dataset) %>%
  mutate(hiflow_rank = percent_rank(perc_change_per_yr)) %>%
  mutate(analysis = "hiflow") %>%
  mutate(analysis_id = paste0(dataset,"_",SUB))

# low flow
lowflow_rank_data = n_lowflow_10yr_change_data %>%
  group_by(dataset) %>%
  mutate(lowflow_rank = percent_rank(perc_change_per_yr)) %>%
  mutate(analysis = "lowflow") %>%
  mutate(analysis_id = paste0(dataset,"_",SUB))

# low flow select
lowflow_rank_data_sel = lowflow_rank_data %>%
  select(analysis_id, lowflow_rank) # select only needed columns for later join

# no flow
no_flow_rank_data = n_no_flow_change_data %>%
  group_by(dataset) %>%
  mutate(no_flow_rank = percent_rank(perc_change_per_yr)) %>%
  mutate(no_flow_rank_fix = as.numeric(no_flow_rank)) %>%
  mutate(analysis = "noflow") %>%
  mutate(analysis_id = paste0(dataset, "_", SUB))

# no flow select
no_flow_rank_data_sel = no_flow_rank_data %>%
  select(analysis_id, no_flow_rank) # select only needed columns for later join

# combine into one dataframe
rank_data = hiflow_rank_data %>%
  left_join(lowflow_rank_data_sel, by = "analysis_id") %>%
  left_join(no_flow_rank_data_sel, by = "analysis_id") %>%
  mutate(rank_sum = as.numeric(hiflow_rank) + as.numeric(lowflow_rank) + as.numeric(no_flow_rank)) %>%
  select(SUB, dataset, dataset.x, rank_sum)

# add to map
