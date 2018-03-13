# combined analysis on high, low, and no flows

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(sf)

# set working directory
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")

# import high, low, and no flow results
n_hiflow_10yr_hydro_change_data = read_csv("num_hiflow_change_10yr_calcs.csv",col_names=TRUE)
n_lowflow_10yr_hydro_change_data = read_csv("num_lowflow_change_10yr_calcs.csv",col_names=TRUE)
n_no_flow_hydro_change_data = read_csv("num_no_flow_change_calcs.csv",col_names=TRUE)

# import high, low, and no flow results with sovi
n_hiflow_10yr_hydro_sovi_change_data = read_csv("num_hiflow_change_10yr_with_sovi_calcs.csv",col_names=TRUE) %>%
  select(SUB:area_wt_sovi)
n_lowflow_10yr_hydro_sovi_change_data = read_csv("num_lowflow_change_10yr_with_sovi_calcs.csv",col_names=TRUE) %>%
  select(SUB:area_wt_sovi)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)

# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% 
  mutate(SUB=Subbasin)

# ---- 2.1 rank hydrology analysis ----

# rank percent changes

# high flow
hiflow_hydro_rank_data = n_hiflow_10yr_hydro_change_data %>%
  group_by(dataset) %>%
  mutate(class = ifelse(perc_change_per_yr <= 25, "lower",
                        ifelse(perc_change_per_yr > 25 & perc_change_per_yr <= 50, "moderate", "higher")))
  #mutate(hiflow_rank = percent_rank(perc_change_per_yr)) %>%
  #mutate(analysis = "hiflow") %>%
  #mutate(analysis_id = paste0(dataset,"_",SUB))

# low flow
lowflow_hydro_rank_data = n_lowflow_10yr_hydro_change_data %>%
  group_by(dataset) %>%
  mutate(lowflow_rank = percent_rank(perc_change_per_yr)) %>%
  mutate(analysis = "lowflow") %>%
  mutate(analysis_id = paste0(dataset,"_",SUB))

# low flow select
lowflow_hydro_rank_data_sel = lowflow_hydro_rank_data %>%
  select(analysis_id, dataset_lowflow = dataset, lowflow_rank) # select only needed columns for later join

# no flow
no_flow_hydro_rank_data = n_no_flow_hydro_change_data %>%
  group_by(dataset) %>%
  mutate(no_flow_rank = percent_rank(perc_change_per_yr)) %>%
  mutate(no_flow_rank_fix = as.numeric(no_flow_rank)) %>%
  mutate(analysis = "noflow") %>%
  mutate(analysis_id = paste0(dataset, "_", SUB))

# no flow select
no_flow_hydro_rank_data_sel = no_flow_hydro_rank_data %>%
  select(analysis_id, dataset_noflow = dataset, no_flow_rank) # select only needed columns for later join

# combine into one dataframe
hydro_rank_data = hiflow_hydro_rank_data %>%
  left_join(lowflow_hydro_rank_data_sel, by = "analysis_id") %>%
  #left_join(no_flow_hydro_rank_data_sel, by = "analysis_id") %>%
  mutate(rank_sum = as.numeric(hiflow_rank) + as.numeric(lowflow_rank)) %>% # + as.numeric(no_flow_rank)) %>%
  select(SUB, dataset, rank_sum) %>%
  mutate(rank_sum_scaled = rank_sum/max(rank_sum, na.rm = TRUE)) %>%
  select(-rank_sum)

# add to shp file
yadkin_subs_shp_hydro_rank_data = left_join(yadkin_subs_shp, hydro_rank_data, by = "SUB")
#glimpse(yadkin_subs_shp_hydro_rank_data)

# adjust levels
yadkin_subs_shp_hydro_rank_data$dataset = factor(yadkin_subs_shp_hydro_rank_data$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
yadkin_subs_shp_hydro_rank_data$class = factor(yadkin_subs_shp_hydro_rank_data$class, levels = c("higher", "moderate", "lower"))


# ---- 2.2 plot on map ----

setwd("/Users/ssaia/Desktop")
cairo_pdf("hydro_analysis_total_rank_map.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hydro_rank_data,aes(fill=class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs = st_crs(102003)) + # yadkin_sub_shp_impact_vuln_hiflow_10yr_2 is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values = c("darkblue", "steelblue3", "lightblue"), na.value = "grey75") +
  theme_bw()
dev.off()


# ---- 3.1 rank hydrology plus sovi analysis ----

# high flow
hiflow_hydro_sovi_rank_data = n_hiflow_10yr_hydro_sovi_change_data %>%
  group_by(dataset) %>%
  mutate(hiflow_rank = percent_rank(perc_change_per_yr)) %>%
  mutate(sovi_rank = percent_rank(area_wt_sovi)) %>%
  mutate(hiflow_combo_rank = as.numeric(hiflow_rank) + as.numeric(sovi_rank)) %>%
  mutate(analysis = "hiflow") %>%
  mutate(analysis_id = paste0(dataset,"_",SUB))

# low flow
lowflow_hydro_sovi_rank_data = n_lowflow_10yr_hydro_sovi_change_data %>%
  group_by(dataset) %>%
  mutate(lowflow_rank = percent_rank(perc_change_per_yr)) %>%
  mutate(sovi_rank = percent_rank(area_wt_sovi)) %>%
  mutate(lowflow_combo_rank = as.numeric(lowflow_rank) + as.numeric(sovi_rank)) %>%
  mutate(analysis = "lowflow") %>%
  mutate(analysis_id = paste0(dataset,"_",SUB))

# low flow select
lowflow_hydro_sovi_rank_data_sel = lowflow_hydro_sovi_rank_data %>%
  select(analysis_id, dataset_lowflow = dataset, lowflow_combo_rank) # select only needed columns for later join

# combine into one dataframe
hydro_sovi_rank_data = hiflow_hydro_sovi_rank_data %>%
  left_join(lowflow_hydro_sovi_rank_data_sel, by = "analysis_id") %>%
  mutate(rank_sum = as.numeric(hiflow_combo_rank) + as.numeric(lowflow_combo_rank)) %>%
  select(SUB, dataset, rank_sum) %>%
  mutate(rank_sum_scaled = rank_sum/max(rank_sum, na.rm = TRUE)) %>%
  select(-rank_sum)

# add to shp file
yadkin_subs_shp_hydro_sovi_rank_data = left_join(yadkin_subs_shp, hydro_sovi_rank_data, by = "SUB")
#glimpse(yadkin_subs_shp_hydro_rank_data)

# adjust levels
yadkin_subs_shp_hydro_sovi_rank_data$dataset = factor(yadkin_subs_shp_hydro_sovi_rank_data$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))


# ---- 3.2 plot on map ----



setwd("/Users/ssaia/Desktop")
cairo_pdf("hydro_sovi_analysis_total_rank_map.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hydro_sovi_rank_data,aes(fill=rank_sum_scaled)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hydro_sovi_rank_data is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Combined hydrology and SoVI analysis impact", na.value = "grey75", limits = c(0,1), low = "white", high = "darkred") +
  theme_bw()
dev.off()