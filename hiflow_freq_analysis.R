# yadkin high flow frequency analysis

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(smwrBase)
library(tidyverse)
#devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
#library(ggplot2)
library(sf)
library(ggridges) # joyplots - https://cran.rstudio.com/web/packages/ggjoy/vignettes/introduction.html

# load home-made functions 
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path,"reformat_rch_file.R")) # reformat SWAT .rch file
source(paste0(functions_path,"logpearson3_factor_calc.R")) # calculate log-Pearson III frequency factors
source(paste0(functions_path,"remove_outliers.R")) # removes low and high flows deemed as outliers
source(paste0(functions_path,"obs_hiflow_freq_calcs_one_rch.R")) # select observations for one reach
source(paste0(functions_path,"obs_freq_calcs_all_rchs.R")) # selects observations for all reaches
source(paste0(functions_path,"model_hiflow_freq_calcs_one_rch.R")) # determines high flow model for one reach
source(paste0(functions_path,"model_freq_calcs_all_rchs.R")) # determines flow model for all reaches
source(paste0(functions_path,"flow_change.R")) # determines % change in flows for a given return period
source(paste0(functions_path,"count_hiflow_outliers.R")) # counts number of minor and major outliers for risk analysis
source(paste0(functions_path,"count_hiflow_outliers_using_baseline.R")) # counts number of minor and major outliers for risk analysis based on baseline cutoffs
source(paste0(functions_path,"outlier_change.R")) # determines % change in minor and major outliers
source(paste0(functions_path,"outlier_flow_cutoff_to_rp.R")) # determines return period for outlier cutoff flow
source(paste0(functions_path,"rp_n_flow_change.R")) # determines percent change in flows greater to or equal to a specied return period
source(paste0(functions_path,"multiplot.R")) # for creating plots with multiple figures

# download kn_table for outlier analysis
setwd("/Users/ssaia/Documents/GitHub/yadkin-analysis/")
kn_table=read_csv("kn_table_appendix4_usgsbulletin17b.csv",col_names=TRUE)

# set directory and load data
# baseline climate+landuse change data (not backcasted) & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
baseline_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#baseline_sub_raw_data=read_table2("output.sub",col_names=FALSE,skip=9) # baseline .sub file from SWAT
#yadkin_net_raw_data=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# baseline climate+landuse change data (backcasted)
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/backcast_results")
miroc_baseline_rch_raw_data=read_table2("MIROC-output.rch",col_names=FALSE,skip=9) # baseline backcast .rch file from SWAT
csiro_baseline_rch_raw_data=read_table2("CSIRO-output.rch",col_names=FALSE,skip=9) # baseline backcast .rch file from SWAT
hadley_baseline_rch_raw_data=read_table2("Hadley-output.rch",col_names=FALSE,skip=9) # baseline backcast .rch file from SWAT

# miroc rcp 8.5 climate+landuse change data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 8.5 climate+landuse change data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 4.5 climate+landuse change data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/C_CSIRO45")
csiro4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# hadley rcp 4.5 climate+landuse change data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/D_Hadley45")
hadley4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)

# looking at gis data
#yadkin_subs_shp_albers_geom=st_geometry(yadkin_subs_shp_albers)
#attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
#yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
#attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2 reformat data ----

# reach file (.rch) 
# baseline data
baseline_rch_data=reformat_rch_file(baseline_rch_raw_data) %>% 
  filter(YR<2003) # not backcast
# take only 21 most recent years (1982-2002) so there's same data record length as projection
miroc_baseline_rch_data=reformat_rch_file(miroc_baseline_rch_raw_data) # backcast
csiro_baseline_rch_data=reformat_rch_file(csiro_baseline_rch_raw_data) # backcast
hadley_baseline_rch_data=reformat_rch_file(hadley_baseline_rch_raw_data) # backcast
# all climate model baseline backcasts are from 1982-2002

# mirco 8.5 data
miroc8_5_rch_data=reformat_rch_file(miroc8_5_rch_raw_data)

# csiro 8.5 data
csiro8_5_rch_data=reformat_rch_file(csiro8_5_rch_raw_data)

# csiro 4.5 data
csiro4_5_rch_data=reformat_rch_file(csiro4_5_rch_raw_data)

# hadley 4.5 data
hadley4_5_rch_data=reformat_rch_file(hadley4_5_rch_raw_data)


# shape file (.shp)
# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% 
  mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)

# join areas for yadkin_net_data (=weights)
#yadkin_sub_areas=baseline_sub_data %>% select(SUB,AREAkm2) %>% distinct()
#yadkin_net_data=left_join(yadkin_net_data_sel,yadkin_sub_areas,by="SUB") %>%
#  select(FROM_NODE,TO_NODE,AREAkm2) # remove SUB b/c FROM_NODE=SUB


# ---- 3.1 calculate obs and model ouptuts for each subbasin ----

# probability list
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)

# baseline (not backcast)
#baseline_obs_calcs=obs_freq_calcs_all_rchs(baseline_rch_data,1,"hiflow")
#baseline_model_calcs=model_freq_calcs_all_rchs(baseline_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

# miroc baseline backcast
miroc_baseline_obs_calcs=obs_freq_calcs_all_rchs(miroc_baseline_rch_data,1,"hiflow")
miroc_baseline_model_calcs=model_freq_calcs_all_rchs(miroc_baseline_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

# mirco 8.5 projection
miroc8_5_obs_calcs=obs_freq_calcs_all_rchs(miroc8_5_rch_data,1,"hiflow")
miroc8_5_model_calcs=model_freq_calcs_all_rchs(miroc8_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

# csiro baseline backcast (for comparison with csiro 8.5 and 4.5 projections)
csiro_baseline_obs_calcs=obs_freq_calcs_all_rchs(csiro_baseline_rch_data,1,"hiflow")
csiro_baseline_model_calcs=model_freq_calcs_all_rchs(csiro_baseline_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

# csiro 8.5 projection
csiro8_5_obs_calcs=obs_freq_calcs_all_rchs(csiro8_5_rch_data,1,"hiflow")
csiro8_5_model_calcs=model_freq_calcs_all_rchs(csiro8_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

# cisro 4.5 projection
csiro4_5_obs_calcs=obs_freq_calcs_all_rchs(csiro4_5_rch_data,1,"hiflow")
csiro4_5_model_calcs=model_freq_calcs_all_rchs(csiro4_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

# hadley baseline backcast
hadley_baseline_obs_calcs=obs_freq_calcs_all_rchs(hadley_baseline_rch_data,1,"hiflow")
hadley_baseline_model_calcs=model_freq_calcs_all_rchs(hadley_baseline_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")

# hadley 4.5 projection
hadley4_5_obs_calcs=obs_freq_calcs_all_rchs(hadley4_5_rch_data,1,"hiflow")
hadley4_5_model_calcs=model_freq_calcs_all_rchs(hadley4_5_obs_calcs,kn_table,my_model_p_list,0.4,"hiflow")


# ---- 3.2 plot high flow freq. baselines for each subbasin (no backcast and backcast comparison) ----

# plot observations and baseline models together
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_max_flow_cms_adj),baseline_obs_calcs,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_calcs,color="black",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc_baseline_model_calcs,color="green",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro_baseline_model_calcs,color="red",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley_baseline_model_calcs,color="blue",linetype=1) +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      panel.background = element_blank())


# ---- 3.3 plot high flow freq. results for each subbasin (backcast) ----

# plot observations and models together
ggplot() +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc_baseline_model_calcs,color="green",linetype=2) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_calcs,color="green",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro_baseline_model_calcs,color="red",linetype=2,size=0.75) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_calcs,color="red",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro_baseline_model_calcs,color="orange",linetype=2) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_calcs,color="orange",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley_baseline_model_calcs,color="blue",linetype=2) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_calcs,color="blue",linetype=1) +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


# ---- 4.1 calculate % change in MAGNITUDE OF FLOWS for given return period (backcast) ----

# using baseline backcast for each projection rather than true baseline

# miroc 8.5
miroc8_5_10yr_flow_bc=flow_change(10,miroc_baseline_model_calcs,miroc8_5_model_calcs)
miroc8_5_100yr_flow_bc=flow_change(100,miroc_baseline_model_calcs,miroc8_5_model_calcs)

# csiro 8.5
csiro8_5_10yr_flow_bc=flow_change(10,csiro_baseline_model_calcs,csiro8_5_model_calcs)
csiro8_5_100yr_flow_bc=flow_change(100,csiro_baseline_model_calcs,csiro8_5_model_calcs)

# csiro 4.5
csiro4_5_10yr_flow_bc=flow_change(10,csiro_baseline_model_calcs,csiro4_5_model_calcs)
csiro4_5_100yr_flow_bc=flow_change(100,csiro_baseline_model_calcs,csiro4_5_model_calcs)

# hadley 4.5
hadley4_5_10yr_flow_bc=flow_change(10,hadley_baseline_model_calcs,hadley4_5_model_calcs)
hadley4_5_100yr_flow_bc=flow_change(100,hadley_baseline_model_calcs,hadley4_5_model_calcs)


# ---- 4.2 reformat calculations for plots (backcast) ----

# using baseline backcast for each projection rather than true baseline

# 10 yr
# select data to add to shp file
miroc8_5_10yr_flow_bc_sel=miroc8_5_10yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="miroc8_5",perc_change=flow_change_perc)
csiro8_5_10yr_flow_bc_sel=csiro8_5_10yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro8_5",perc_change=flow_change_perc)
csiro4_5_10yr_flow_bc_sel=csiro4_5_10yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro4_5",perc_change=flow_change_perc)
hadley4_5_10yr_flow_bc_sel=hadley4_5_10yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="hadley4_5",perc_change=flow_change_perc)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# gather projections
hiflow_10yr_projections_bc=bind_rows(miroc8_5_10yr_flow_bc_sel,
                                     csiro8_5_10yr_flow_bc_sel,
                                     csiro4_5_10yr_flow_bc_sel,
                                     hadley4_5_10yr_flow_bc_sel)

# add to shp file
yadkin_subs_shp_hiflow_10yr_bc=left_join(yadkin_subs_shp,hiflow_10yr_projections_bc,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_10yr_bc)


# 100 yr flow
# select data to add to shp file
miroc8_5_100yr_flow_bc_sel=miroc8_5_100yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="miroc8_5",perc_change=flow_change_perc)
csiro8_5_100yr_flow_bc_sel=csiro8_5_100yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro8_5",perc_change=flow_change_perc)
csiro4_5_100yr_flow_bc_sel=csiro4_5_100yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro4_5",perc_change=flow_change_perc)
hadley4_5_100yr_flow_bc_sel=hadley4_5_100yr_flow_bc %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="hadley4_5",perc_change=flow_change_perc)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# gather projections
hiflow_100yr_projections_bc=bind_rows(miroc8_5_100yr_flow_bc_sel,
                                      csiro8_5_100yr_flow_bc_sel,
                                      csiro4_5_100yr_flow_bc_sel,
                                      hadley4_5_100yr_flow_bc_sel)

# add to shp file
yadkin_subs_shp_hiflow_100yr_bc=left_join(yadkin_subs_shp,hiflow_100yr_projections_bc,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_100yr_bc)

# ---- 4.3 plot on map (backcast) ----

# 10 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_10yr_change_bc.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_10yr_bc,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_10yr_bc is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 10yr Flow",na.value="grey75",limits=c(-50,250)) +
  theme_bw() #+
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  #theme(text = element_text(size = 20))
dev.off()

# 100 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_100yr_change_bc.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_100yr_bc,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_100yr_bc is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 100yr Flow",na.value="grey75",limits=c(-55,505)) +
  theme_bw()
dev.off()


# ---- 4.4 export results (backcast) ----

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(hiflow_10yr_projections_bc,"hiflow_10yr_perc_change_bc.csv")
#write_csv(hiflow_10yr_projections_bc,"hiflow_100yr_perc_change_bc.csv")

# ---- 5.1 calculate % change in NUMBER OF FLOWS at/above a given return period (backcast) ----

# 10 yr
miroc8_5_10yr_n_flow_change = rp_n_flow_change(10, miroc_baseline_model_calcs, miroc_baseline_rch_data, miroc8_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "miroc8_5")
csiro4_5_10yr_n_flow_change = rp_n_flow_change(10, csiro_baseline_model_calcs, csiro_baseline_rch_data, csiro4_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "csiro4_5")
csiro8_5_10yr_n_flow_change = rp_n_flow_change(10, csiro_baseline_model_calcs, csiro_baseline_rch_data, csiro8_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "csiro8_5")
hadley4_5_10yr_n_flow_change = rp_n_flow_change(10, hadley_baseline_model_calcs, hadley_baseline_rch_data, hadley4_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "hadley4_5")

# 25 yr
miroc8_5_25yr_n_flow_change = rp_n_flow_change(25, miroc_baseline_model_calcs, miroc_baseline_rch_data, miroc8_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "miroc8_5")
csiro4_5_25yr_n_flow_change = rp_n_flow_change(25, csiro_baseline_model_calcs, csiro_baseline_rch_data, csiro4_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "csiro4_5")
csiro8_5_25yr_n_flow_change = rp_n_flow_change(25, csiro_baseline_model_calcs, csiro_baseline_rch_data, csiro8_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "csiro8_5")
hadley4_5_25yr_n_flow_change = rp_n_flow_change(25, hadley_baseline_model_calcs, hadley_baseline_rch_data, hadley4_5_rch_data, flow_option = 'hiflow') %>%
  mutate(dataset = "hadley4_5")


# ---- 5.2 reformat calcs for plots (backcast) ----

# combine data for plotting
n_flow_change_10yr = rbind(miroc8_5_10yr_n_flow_change,
                           csiro4_5_10yr_n_flow_change,
                           csiro8_5_10yr_n_flow_change,
                           hadley4_5_10yr_n_flow_change) %>%
  mutate(SUB = RCH)

n_flow_change_25yr = rbind(miroc8_5_25yr_n_flow_change,
                           csiro4_5_25yr_n_flow_change,
                           csiro8_5_25yr_n_flow_change,
                           hadley4_5_25yr_n_flow_change) %>%
  mutate(SUB = RCH)

# add to shp file
yadkin_subs_shp_n_flow_change_10yr=left_join(yadkin_subs_shp,n_flow_change_10yr,by="SUB")
yadkin_subs_shp_n_flow_change_25yr=left_join(yadkin_subs_shp,n_flow_change_25yr,by="SUB")


# ---- 5.3 plot on map (backcast) ----

# 10 yr
yadkin_subs_shp_n_flow_change_10yr$dataset=factor(yadkin_subs_shp_n_flow_change_10yr$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
setwd("/Users/ssaia/Desktop")
cairo_pdf("change_num_hiflows_10yr_map.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_n_flow_change_10yr,aes(fill=perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_n_flow_change_10yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Flows >= 10yr Flow",na.value="grey75", limits = c(-5, 150), high="darkblue",low="darkred") +
  theme_bw()
dev.off()

# 25 yr
yadkin_subs_shp_n_flow_change_25yr$dataset=factor(yadkin_subs_shp_n_flow_change_25yr$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
setwd("/Users/ssaia/Desktop")
cairo_pdf("change_num_hiflows_25yr_map.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_n_flow_change_25yr,aes(fill=perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_n_flow_change_25yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Flows >= 25yr Flow",na.value="grey75", limits = c(-5,90), high="darkblue",low="darkred") +
  theme_bw()
dev.off()


# ---- 5.3 calculate variation (backcast) ----

# make dataframe with contributing errors to can use to plot
contributing_areas = baseline_rch_data %>% 
  select(RCH, AREAkm2) %>%
  distinct() %>% 
  mutate(SUB = RCH) %>% 
  select(-RCH)

# 10yr flows
# join areas
n_flow_change_10yr_area = n_flow_change_10yr %>%
  left_join(contributing_areas, by = 'SUB')

# select only backcast baseline results (and recode them for plotting)
baseline_num_yrs = length(unique(miroc_baseline_rch_data$YR))
n_flow_change_10yr_baseline=n_flow_change_10yr_area %>%
  select(SUB,AREAkm2,n_base_flows,dataset) %>%
  mutate(baseline_n_flows_per_yr=n_base_flows/baseline_num_yrs) %>%
  filter(dataset!="csiro4_5") # don't need both CSIRO datasets b/c backcast baselines are the same for both
n_flow_change_10yr_baseline$dataset=recode(n_flow_change_10yr_baseline$dataset,"miroc8_5"="miroc","csiro8_5"="csiro","hadley4_5"="hadley")

# backcast baseline ordered by subbasin area
n_flow_change_10yr_baseline$SUB=factor(n_flow_change_10yr_baseline$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
n_flow_change_10yr_baseline$dataset=factor(n_flow_change_10yr_baseline$dataset,levels=c("miroc","csiro","hadley"))

# backcast baseline summary for pointrange plot
n_flow_change_10yr_baseline_summary=n_flow_change_10yr_baseline %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_flows_per_yr=min(baseline_n_flows_per_yr),
            max_n_flows_per_yr=max(baseline_n_flows_per_yr),
            mean_n_flows_per_yr=mean(baseline_n_flows_per_yr))

# select only projection results (and recode them for plotting)
projection_num_yrs = length(unique(miroc8_5_rch_data$YR))
n_flow_change_10yr_projection=n_flow_change_10yr_area %>%
  select(SUB,AREAkm2,n_proj_flows,dataset) %>%
  mutate(projection_n_flows_per_yr=n_proj_flows/projection_num_yrs)

# projection ordered by subbasin area
n_flow_change_10yr_projection$SUB=factor(n_flow_change_10yr_projection$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
n_flow_change_10yr_projection$dataset=factor(n_flow_change_10yr_projection$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# projection summary for pointrange plot
n_flow_change_10yr_projection_summary=n_flow_change_10yr_projection %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_flows_per_yr=min(projection_n_flows_per_yr),
            max_n_flows_per_yr=max(projection_n_flows_per_yr),
            mean_n_flows_per_yr=mean(projection_n_flows_per_yr)) # all are cumulative for length of projection


# 25yr flows
# join areas
n_flow_change_25yr_area=n_flow_change_25yr %>%
  left_join(contributing_areas,by='SUB')

# select only backcast baseline results (and recode them for plotting)
baseline_num_yrs = length(unique(miroc_baseline_rch_data$YR))
n_flow_change_25yr_baseline=n_flow_change_25yr_area %>%
  select(SUB,AREAkm2,n_base_flows,dataset) %>%
  mutate(baseline_n_flows_per_yr=n_base_flows/baseline_num_yrs) %>%
  filter(dataset!="csiro4_5") # don't need both CSIRO datasets b/c backcast baselines are the same for both
n_flow_change_25yr_baseline$dataset=recode(n_flow_change_25yr_baseline$dataset,"miroc8_5"="miroc","csiro8_5"="csiro","hadley4_5"="hadley")

# backcast baseline ordered by subbasin area
n_flow_change_25yr_baseline$SUB=factor(n_flow_change_25yr_baseline$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
n_flow_change_25yr_baseline$dataset=factor(n_flow_change_25yr_baseline$dataset,levels=c("miroc","csiro","hadley"))

# backcast baseline summary for pointrange plot
n_flow_change_25yr_baseline_summary=n_flow_change_25yr_baseline %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_flows_per_yr=min(baseline_n_flows_per_yr),
            max_n_flows_per_yr=max(baseline_n_flows_per_yr),
            mean_n_flows_per_yr=mean(baseline_n_flows_per_yr))

# select only projection results (and recode them for plotting)
projection_num_yrs = length(unique(miroc8_5_rch_data$YR))
n_flow_change_25yr_projection=n_flow_change_25yr_area %>%
  select(SUB,AREAkm2,n_proj_flows,dataset) %>%
  mutate(projection_n_flows_per_yr=n_proj_flows/projection_num_yrs)

# projection ordered by subbasin area
n_flow_change_25yr_projection$SUB=factor(n_flow_change_25yr_projection$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
n_flow_change_25yr_projection$dataset=factor(n_flow_change_25yr_projection$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# projection summary for pointrange plot
n_flow_change_25yr_projection_summary=n_flow_change_25yr_projection %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_flows_per_yr=min(projection_n_flows_per_yr),
            max_n_flows_per_yr=max(projection_n_flows_per_yr),
            mean_n_flows_per_yr=mean(projection_n_flows_per_yr)) # all are cumulative for length of projection


# ---- 5.5 plot variation (backcast) ----

# 10yr flow
# make a list to hold plots
my_10yr_plots = list()

# backcast baselines variation plot
my_10yr_plots[[1]] = ggplot() +
  geom_pointrange(data=n_flow_change_10yr_baseline_summary,
                  aes(x=SUB,y=mean_n_flows_per_yr,ymin=min_n_flows_per_yr,ymax=max_n_flows_per_yr),shape=32) +
  geom_point(data=n_flow_change_10yr_baseline,aes(x=SUB,y=baseline_n_flows_per_yr,color=dataset),
             shape=17,size=5,alpha=0.75, position=position_jitter(height=0.005,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Flows >= 10 yr Flow/Year") +
  scale_color_manual(values=c("grey75","grey50","black")) +
  ylim(-0.25,3) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18),
        legend.position = c(0.2, 0.8))

# projection variation plot
my_10yr_plots[[2]] = ggplot() +
  geom_pointrange(data=n_flow_change_10yr_projection_summary,
                  aes(x=SUB,y=mean_n_flows_per_yr,ymin=min_n_flows_per_yr,ymax=max_n_flows_per_yr),shape=32) +
  geom_point(data=n_flow_change_10yr_projection,aes(x=SUB,y=projection_n_flows_per_yr,color=dataset),
             shape=16,size=5,alpha=0.75, position=position_jitter(height=0.005,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Flows >= 10 yr Flow/Year") +
  scale_color_manual(values=c("grey75","grey50","grey25","black")) +
  ylim(-0.25,3) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18),
        legend.position = c(0.2, 0.8))

# save plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("num_10yr_hiflows_variation.pdf", width = 15, height = 8.5, pointsize = 18)
multiplot(plotlist = my_10yr_plots, cols = 2)
dev.off()

# 25yr flow
# make a list to hold plots
my_25yr_plots = list()

# backcast baselines variation plot
my_25yr_plots[[1]] = ggplot() +
  geom_pointrange(data=n_flow_change_25yr_baseline_summary,
                  aes(x=SUB,y=mean_n_flows_per_yr,ymin=min_n_flows_per_yr,ymax=max_n_flows_per_yr),shape=32) +
  geom_point(data=n_flow_change_25yr_baseline,aes(x=SUB,y=baseline_n_flows_per_yr,color=dataset),
             shape=17,size=5,alpha=0.75, position=position_jitter(height=0.01,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Flows >= 25 yr Flow/Year") +
  scale_color_manual(values=c("grey75","grey50","black")) +
  ylim(-0.25,2) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18),
        legend.position = c(0.8, 0.8))

# projection variation plot
my_25yr_plots[[2]] = ggplot() +
  geom_pointrange(data=n_flow_change_25yr_projection_summary,
                  aes(x=SUB,y=mean_n_flows_per_yr,ymin=min_n_flows_per_yr,ymax=max_n_flows_per_yr),shape=32) +
  geom_point(data=n_flow_change_25yr_projection,aes(x=SUB,y=projection_n_flows_per_yr,color=dataset),
             shape=16,size=5,alpha=0.75, position=position_jitter(height=0.01,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Flows >= 25 yr Flow/Year") +
  scale_color_manual(values=c("grey75","grey50","grey25","black")) +
  ylim(-0.25,2) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18),
        legend.position = c(0.8, 0.8))

# save plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("num_25yr_hiflows_variation.pdf",width=15,height=8.5,pointsize=18)
multiplot(plotlist=my_25yr_plots,cols=2)
dev.off()


# ---- 5.6 export results (backcast) ----

# export results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(n_flow_change_10yr_area,"num_hiflow_change_10yr_calcs.csv")
#write_csv(n_flow_change_25yr_area,"num_hiflow_change_25yr_calcs.csv")

# ---- 6.1 check normality of data (backcast) ----

# make new data frame without zero values (b/c just looking at actual flows)
# miroc baseline (backcast)
#miroc_baseline_rch_data_log_no_zeros=miroc_baseline_rch_data %>%
#  mutate(FLOW_OUTcms_no_zeros=replace(FLOW_OUTcms,FLOW_OUTcms==0,as.numeric("NA")),
#         log_FLOW_OUTcms=log(FLOW_OUTcms),
#         log_FLOW_OUTcms_no_zeros=replace(log(FLOW_OUTcms),log(FLOW_OUTcms)=="-Inf",as.numeric("NA"))) %>% na.omit()
miroc_baseline_rch_data_log_no_zeros=miroc_baseline_rch_data %>%
  filter(FLOW_OUTcms!=0) %>%
  mutate(log_FLOW_OUTcms=log(FLOW_OUTcms))

# csiro baseline (backcast)
csiro_baseline_rch_data_log_no_zeros=csiro_baseline_rch_data %>%
  filter(FLOW_OUTcms!=0) %>%
  mutate(log_FLOW_OUTcms=log(FLOW_OUTcms))

# hadley baseline (backcast)
hadley_baseline_rch_data_log_no_zeros=hadley_baseline_rch_data %>%
  filter(FLOW_OUTcms!=0) %>%
  mutate(log_FLOW_OUTcms=log(FLOW_OUTcms))

# miroc 8.5
miroc8_5_rch_data_log_no_zeros=miroc8_5_rch_data %>%
  filter(FLOW_OUTcms!=0) %>%
  mutate(log_FLOW_OUTcms=log(FLOW_OUTcms))

# csiro 8.5
csiro8_5_rch_data_log_no_zeros=csiro8_5_rch_data %>%
  filter(FLOW_OUTcms!=0) %>%
  mutate(log_FLOW_OUTcms=log(FLOW_OUTcms))

# csiro 4.5
csiro4_5_rch_data_log_no_zeros=csiro4_5_rch_data %>%
  filter(FLOW_OUTcms!=0) %>%
  mutate(log_FLOW_OUTcms=log(FLOW_OUTcms))

# hadley 4.5
hadley4_5_rch_data_log_no_zeros=hadley4_5_rch_data %>%
  filter(FLOW_OUTcms!=0) %>%
  mutate(log_FLOW_OUTcms=log(FLOW_OUTcms))


# plot unlogged data
# miroc baseline (backcast)
ggplot(miroc_baseline_rch_data_log_no_zeros,aes(sample=FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(miroc_baseline_rch_data_log_no_zeros,aes(x=FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# qqplot tails are off line, hist looks non-normal

# csiro baseline (backcast)
ggplot(csiro_baseline_rch_data_log_no_zeros,aes(sample=FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(csiro_baseline_rch_data_log_no_zeros,aes(x=FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# qqplot tails are off line, hist looks non-normal

# csiro baseline (backcast)
ggplot(hadley_baseline_rch_data_log_no_zeros,aes(sample=FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(hadley_baseline_rch_data_log_no_zeros,aes(x=FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# qqplot tails are off line, hist looks non-normal

# miroc 8.5
ggplot(miroc8_5_rch_data_log_no_zeros,aes(sample=FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(miroc8_5_rch_data_log_no_zeros,aes(x=FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# qqplot tails are off line, hist looks non-normal

# csiro 8.5
ggplot(csiro8_5_rch_data_log_no_zeros,aes(sample=FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(csiro8_5_rch_data_log_no_zeros,aes(x=FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# qqplot tails are off line, hist looks non-normal

# csiro 4.5
ggplot(csiro4_5_rch_data_log_no_zeros,aes(sample=FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(csiro4_5_rch_data_log_no_zeros,aes(x=FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# qqplot tails are off line, hist looks non-normal

# hadley 4.5
ggplot(hadley4_5_rch_data_log_no_zeros,aes(sample=FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(hadley4_5_rch_data_log_no_zeros,aes(x=FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# qqplot tails are off line, hist looks non-normal


# plot logged data
# miroc baseline (backcast)
ggplot(miroc_baseline_rch_data_log_no_zeros,aes(sample=log_FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(miroc_baseline_rch_data_log_no_zeros,aes(x=log_FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# looks normal in qqplot and hist

# csiro baseline (backcast)
ggplot(csiro_baseline_rch_data_log_no_zeros,aes(sample=log_FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(csiro_baseline_rch_data_log_no_zeros,aes(x=log_FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# looks normal in qqplot and hist

# hadley baseline (backcast)
ggplot(hadley_baseline_rch_data_log_no_zeros,aes(sample=log_FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(hadley_baseline_rch_data_log_no_zeros,aes(x=log_FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# looks normal in qqplot and hist

# miroc 8.5
ggplot(miroc8_5_rch_data_log_no_zeros,aes(sample=log_FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(miroc8_5_rch_data_log_no_zeros,aes(x=log_FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# looks normal in qqplot and hist

# csiro 8.5
ggplot(csiro8_5_rch_data_log_no_zeros,aes(sample=log_FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(csiro8_5_rch_data_log_no_zeros,aes(x=log_FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# looks normal in qqplot and hist

# csiro 4.5
ggplot(csiro4_5_rch_data_log_no_zeros,aes(sample=log_FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(csiro4_5_rch_data_log_no_zeros,aes(x=log_FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# looks normal in qqplot and hist

# hadley 4.5
ggplot(hadley4_5_rch_data_log_no_zeros,aes(sample=log_FLOW_OUTcms)) +
  geom_qq(size=1) +
  geom_qq_line() +
  facet_wrap(~RCH,ncol=7,nrow=4)
ggplot(hadley4_5_rch_data_log_no_zeros,aes(x=log_FLOW_OUTcms)) +
  geom_histogram() +
  facet_wrap(~RCH,ncol=7,nrow=4)
# looks normal in qqplot and hist

# in conclusion...need to log transform FLOW_OUTcms data for outlier calcs


# ---- 6.2 calculate outlier cutoffs and number of outlier high flows (no backcast and backcast) ----

# baseline (not backcast)
#baseline_outlier_calcs=count_hiflow_outliers(baseline_rch_data)
#baseline_outlier_counts=baseline_outlier_calcs[[1]]
#baseline_outlier_cutoffs=baseline_outlier_calcs[[2]]

# miroc baseline (backcast)
miroc_baseline_outlier_calcs=count_hiflow_outliers(miroc_baseline_rch_data)
miroc_baseline_outlier_counts=miroc_baseline_outlier_calcs[[1]]
miroc_baseline_outlier_cutoffs=miroc_baseline_outlier_calcs[[2]]

# csiro baseline (backcast)
csiro_baseline_outlier_calcs=count_hiflow_outliers(csiro_baseline_rch_data)
csiro_baseline_outlier_counts=csiro_baseline_outlier_calcs[[1]]
csiro_baseline_outlier_cutoffs=csiro_baseline_outlier_calcs[[2]]

# hadley baseline (backcast)
hadley_baseline_outlier_calcs=count_hiflow_outliers(hadley_baseline_rch_data)
hadley_baseline_outlier_counts=hadley_baseline_outlier_calcs[[1]]
hadley_baseline_outlier_cutoffs=hadley_baseline_outlier_calcs[[2]]

# miroc 8.5
#miroc8_5_outlier_calcs=count_hiflow_outliers(miroc8_5_rch_data) # find outliers using cutoff from data itself
#miroc8_5_outlier_counts=miroc8_5_outlier_calcs[[1]]
#miroc8_5_outlier_cutoffs=miroc8_5_outlier_calcs[[2]]
miroc8_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(miroc_baseline_outlier_cutoffs,miroc8_5_rch_data) # find outliers using backcast baseline cutoff
miroc8_5_outlier_counts_using_baseline=miroc8_5_outlier_calcs_using_baseline[[1]]
miroc8_5_outlier_cutoffs_using_baseline=miroc8_5_outlier_calcs_using_baseline[[2]]

# csiro 8.5
#csiro8_5_outlier_calcs=count_hiflow_outliers(csiro8_5_rch_data)
#csiro8_5_outlier_counts=csiro8_5_outlier_calcs[[1]]
#csiro8_5_outlier_cutoffs=csiro8_5_outlier_calcs[[2]]
csiro8_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(csiro_baseline_outlier_cutoffs,csiro8_5_rch_data) # find outliers using backcast baseline cutoff
csiro8_5_outlier_counts_using_baseline=csiro8_5_outlier_calcs_using_baseline[[1]]
csiro8_5_outlier_cutoffs_using_baseline=csiro8_5_outlier_calcs_using_baseline[[2]]

# csiro 4.5
#csiro4_5_outlier_calcs=count_hiflow_outliers(csiro4_5_rch_data)
#csiro4_5_outlier_counts=csiro4_5_outlier_calcs[[1]]
#csiro4_5_outlier_cutoffs=csiro4_5_outlier_calcs[[2]]
csiro4_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(csiro_baseline_outlier_cutoffs,csiro4_5_rch_data) # find outliers using backcast baseline cutoff
csiro4_5_outlier_counts_using_baseline=csiro4_5_outlier_calcs_using_baseline[[1]]
csiro4_5_outlier_cutoffs_using_baseline=csiro4_5_outlier_calcs_using_baseline[[2]]

# hadley 4.5
#hadley4_5_outlier_calcs=count_hiflow_outliers(hadley4_5_rch_data)
#hadley4_5_outlier_counts=hadley4_5_outlier_calcs[[1]]
#hadley4_5_outlier_cutoffs=hadley4_5_outlier_calcs[[2]]
hadley4_5_outlier_calcs_using_baseline=count_hiflow_outliers_using_baseline(hadley_baseline_outlier_cutoffs,hadley4_5_rch_data) # find outliers using backcast baseline cutoff
hadley4_5_outlier_counts_using_baseline=hadley4_5_outlier_calcs_using_baseline[[1]]
hadley4_5_outlier_cutoffs_using_baseline=hadley4_5_outlier_calcs_using_baseline[[2]]


# sum outlier counts data by subbasin
# backcast baselines
miroc_baseline_outlier_counts_sum=miroc_baseline_outlier_counts %>%
  group_by(RCH) %>% 
  summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="miroc_baseline",datatype="baseline")
csiro_baseline_outlier_counts_sum=csiro_baseline_outlier_counts %>%
  group_by(RCH) %>% 
  summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro_baseline",datatype="baseline")
hadley_baseline_outlier_counts_sum=hadley_baseline_outlier_counts %>%
  group_by(RCH) %>% 
  summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="hadley_baseline",datatype="baseline")

# projections
miroc8_5_outlier_counts_using_baseline_sum=miroc8_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="miroc8_5",datatype="projection")
csiro8_5_outlier_counts_using_baseline_sum=csiro8_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro8_5",datatype="projection")
csiro4_5_outlier_counts_using_baseline_sum=csiro4_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro4_5",datatype="projection")
hadley4_5_outlier_counts_using_baseline_sum=hadley4_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="hadley4_5",datatype="projection")

# combine data
all_models_hiflow_outlier_counts=bind_rows(miroc_baseline_outlier_counts_sum,
                                           csiro_baseline_outlier_counts_sum,
                                           hadley_baseline_outlier_counts_sum,
                                           miroc8_5_outlier_counts_using_baseline_sum,
                                           csiro8_5_outlier_counts_using_baseline_sum,
                                           csiro4_5_outlier_counts_using_baseline_sum,
                                           hadley4_5_outlier_counts_using_baseline_sum)


# ---- 6.3 calculate % change in NUMBER OF MINOR OUTLIER high flows (backcast) ----

# specify number of years of baseline/projection
# for this script to work these have to be equal
baseline_num_yrs=length(unique(baseline_rch_data$YR))

# calculate % change 
miroc8_5_hiflow_outlier_change_using_baseline=outlier_change(miroc_baseline_outlier_counts_sum,miroc8_5_outlier_counts_using_baseline_sum,flow_option="hiflow",baseline_num_yrs)
csiro8_5_hiflow_outlier_change_using_baseline=outlier_change(csiro_baseline_outlier_counts_sum,csiro8_5_outlier_counts_using_baseline_sum,flow_option="hiflow",baseline_num_yrs)
csiro4_5_hiflow_outlier_change_using_baseline=outlier_change(csiro_baseline_outlier_counts_sum,csiro4_5_outlier_counts_using_baseline_sum,flow_option="hiflow",baseline_num_yrs)
hadley4_5_hiflow_outlier_change_using_baseline=outlier_change(hadley_baseline_outlier_counts_sum,hadley4_5_outlier_counts_using_baseline_sum,flow_option="hiflow",baseline_num_yrs)

# bind rows
all_models_hiflow_outlier_change=bind_rows(miroc8_5_hiflow_outlier_change_using_baseline,
                                           csiro8_5_hiflow_outlier_change_using_baseline,
                                           csiro4_5_hiflow_outlier_change_using_baseline,
                                           hadley4_5_hiflow_outlier_change_using_baseline) %>% 
  mutate(SUB=RCH) %>% 
  select(-RCH)

# add to shp file
yadkin_subs_shp_hiflow_outliers_using_baseline=left_join(yadkin_subs_shp,all_models_hiflow_outlier_change,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_outliers_using_baseline)

# adjust levels
yadkin_subs_shp_hiflow_outliers_using_baseline$dataset=factor(yadkin_subs_shp_hiflow_outliers_using_baseline$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))


# ---- 6.4 plot on map (backcast) ----

# minor outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_minor_outlier_change_using_baseline.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers_using_baseline,aes(fill=minor_outlier_perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers_using_baseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Minor High Flow Outliers/yr",na.value="grey75",limits=c(-20,60),high="darkblue",low="darkred") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# major outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_major_outlier_change_using_baseline.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers_using_baseline,aes(fill=major_outlier_perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers_using_baseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Major High Flow Outliers/yr",na.value="grey75",limits=c(-10,20)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()


# ---- 6.5 calculate variation (backcast) ----

# make dataframe with contributing errors to can use to plot
contributing_areas=baseline_rch_data %>% select(RCH,AREAkm2) %>%
  distinct() %>% 
  mutate(SUB=RCH) %>% 
  select(-RCH)

# join areas
all_models_hiflow_change_area=all_models_hiflow_outlier_change %>%
  left_join(contributing_areas,by='SUB')

# backcast baselines (and recode them for plotting)
baseline_num_yrs=length(unique(baseline_rch_data$YR))
hiflow_change_baseline=all_models_hiflow_change_area %>%
  select(SUB,AREAkm2,baseline_sum_n_minor_outliers,dataset) %>%
  mutate(baseline_sum_n_minor_outliers_per_yr=baseline_sum_n_minor_outliers/baseline_num_yrs) %>%
  filter(dataset!="csiro4_5") # don't need both CSIRO datasets b/c backcast baselines are the same for both
hiflow_change_baseline$dataset=recode(hiflow_change_baseline$dataset,"miroc8_5"="MIROC","csiro8_5"="CSIRO","hadley4_5"="Hadley")

# backcast baseline ordered by subbasin area
hiflow_change_baseline$SUB=factor(hiflow_change_baseline$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
hiflow_change_baseline$dataset=factor(hiflow_change_baseline$dataset,levels=c("MIROC","CSIRO","Hadley"))

# backcast baseline summary for pointrange plot
hiflow_change_baseline_summary=hiflow_change_baseline %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_minor_outliers_per_yr=min(baseline_sum_n_minor_outliers_per_yr),
            max_n_minor_outliers_per_yr=max(baseline_sum_n_minor_outliers_per_yr),
            mean_n_minor_outliers_per_yr=mean(baseline_sum_n_minor_outliers_per_yr))

# projections
miroc8_5_num_yrs=length(unique(miroc8_5_rch_data$YR)) # all are equal to 21 but use miroc8_5_num_yrs for simplicity
hiflow_change_projection=all_models_hiflow_change_area %>%
  select(SUB,AREAkm2,projection_sum_n_minor_outliers,dataset) %>%
  mutate(projection_sum_n_minor_outliers_per_yr=projection_sum_n_minor_outliers/miroc8_5_num_yrs) # all are equal to 21 but use miroc8_5_num_yrs for simplicity 

# projections ordered by subbasin area
hiflow_change_projection$SUB=factor(hiflow_change_projection$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
hiflow_change_projection$dataset=factor(hiflow_change_projection$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# projections summary for pointrange plot
hiflow_change_projection_summary=hiflow_change_projection %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_minor_outliers_per_yr=min(projection_sum_n_minor_outliers_per_yr),
            max_n_minor_outliers_per_yr=max(projection_sum_n_minor_outliers_per_yr),
            mean_n_minor_outliers_per_yr=mean(projection_sum_n_minor_outliers_per_yr))


# ---- 6.6 plot variation (backcast) ----

# make a list to hold plots
my_outlier_plots = list()

# backcast baselines plot
my_outlier_plots[[1]] = ggplot() +
  geom_pointrange(data=hiflow_change_baseline_summary,
                  aes(x=SUB,y=mean_n_minor_outliers_per_yr,ymin=min_n_minor_outliers_per_yr,ymax=max_n_minor_outliers_per_yr),shape=32) +
  geom_point(data=hiflow_change_baseline,aes(x=SUB,y=baseline_sum_n_minor_outliers_per_yr,color=dataset),
             shape=17,size=5,alpha=0.75, position=position_jitter(height=0.075,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Minor HOFs/yr") +
  scale_color_manual(values=c("grey75","grey50","black")) +
  ylim(-1,20) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18))

# projections plot
my_outlier_plots[[2]] = ggplot() +
  geom_pointrange(data=hiflow_change_projection_summary,
                  aes(x=SUB,y=mean_n_minor_outliers_per_yr,ymin=min_n_minor_outliers_per_yr,ymax=max_n_minor_outliers_per_yr),
                  shape=32) +
  geom_point(data=hiflow_change_projection,aes(x=SUB,y=projection_sum_n_minor_outliers_per_yr,color=dataset),
             size=5,alpha=0.75, position=position_jitter(height=0.1,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Minor HOFs/yr") +
  scale_color_manual(values=c("grey80","grey60","grey40","black")) +
  ylim(-1,20) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18))

# save plots
setwd("/Users/ssaia/Desktop")
cairo_pdf("num_outlier_hiflows_variation.pdf",width=15,height=8.5,pointsize=18)
multiplot(plotlist=my_outlier_plots,cols=2)
dev.off()

# ---- 6.7 export results ----

# just export percent change
all_models_hiflow_outlier_change_sel=all_models_hiflow_outlier_change %>%
  select(SUB,dataset,minor_outlier_perc_change,minor_outlier_perc_change_per_yr,
         major_outlier_perc_change,major_outlier_perc_change_per_yr)

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(all_models_hiflow_outlier_change_sel,"hiflow_outlier_perc_change_data.csv")


# ---- 7.1 calculate return period for outlier cutoffs ----

# calculate 
miroc_baseline_outlier_cutoff_rp = outlier_flow_cutoff_to_rp(miroc_baseline_model_calcs,miroc_baseline_outlier_cutoffs)
csiro_baseline_outlier_cutoff_rp = outlier_flow_cutoff_to_rp(csiro_baseline_model_calcs,csiro_baseline_outlier_cutoffs)
hadley_baseline_outlier_cutoff_rp = outlier_flow_cutoff_to_rp(hadley_baseline_model_calcs,hadley_baseline_outlier_cutoffs)

# make dataframe with contributing errors to can use to plot
contributing_areas=baseline_rch_data %>% select(RCH,AREAkm2) %>%
  distinct() %>% 
  mutate(SUB=RCH) #%>% 
  #select(-RCH)

# join areas
blah=miroc_baseline_outlier_cutoff_rp %>%
  left_join(contributing_areas,by='RCH')

# ---- 7.2 plot excedence probability of outlier cutoffs for subbasins ---- 

plot(minor_prob_exced~AREAkm2, data = blah,pch=16)



# ---- 8.1 plot flow distributions ----

# join backcast baseline and projection data for overlapping joyplots
baseline_rch_data_sel=baseline_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="true_baseline") %>% 
  mutate(datatype="true_baseline")
miroc_baseline_rch_data_sel=miroc_baseline_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="miroc_baseline") %>% 
  mutate(datatype="backcast_baseline")
csiro_baseline_rch_data_sel=csiro_baseline_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="csiro_baseline") %>% 
  mutate(datatype="backcast_baseline")
hadley_baseline_rch_data_sel=hadley_baseline_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="hadley_baseline") %>% 
  mutate(datatype="backcast_baseline")
miroc8_5_rch_data_sel=miroc8_5_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="miroc8_5") %>% 
  mutate(datatype="projection")
csiro8_5_rch_data_sel=csiro8_5_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="csiro8_5") %>% 
  mutate(datatype="projection")
csiro4_5_rch_data_sel=csiro4_5_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="csiro4_5") %>% 
  mutate(datatype="projection")
hadley4_5_rch_data_sel=hadley4_5_rch_data %>% 
  select(RCH,MO,YR,FLOW_OUTcms) %>%
  mutate(dataset="hadley4_5") %>% 
  mutate(datatype="projection")


all_rch_data_sel=bind_rows(baseline_rch_data_sel,miroc_baseline_rch_data_sel,
                           csiro_baseline_rch_data_sel,hadley_baseline_rch_data_sel,
                           miroc8_5_rch_data_sel,csiro8_5_rch_data_sel,
                           csiro4_5_rch_data_sel,hadley4_5_rch_data_sel)
all_rch_data_sel$dataset=factor(all_rch_data_sel$dataset,levels=rev(c("true_baseline","miroc_baseline","miroc8_5","csiro_baseline","csiro8_5","csiro4_5","hadley_baseline","hadley4_5")))
all_rch_data_sel$datatype=factor(all_rch_data_sel$datatype,levels=c("true_baseline","backcast_baseline","projection"))

# all datasets all subs
ggplot(all_rch_data_sel,aes(x=FLOW_OUTcms,y=dataset,fill=dataset)) +
  geom_density_ridges2() +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") + 
  ylab("Dataset") +
  xlim(0,1000) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# all datasets one sub
my_sub=28
my_sub_baseline_to_projections=all_rch_data_sel %>% filter(RCH==my_sub) %>% filter(datatype!="true_baseline")
ggplot(my_sub_baseline_to_projections,aes(x=FLOW_OUTcms,y=dataset,fill=datatype)) +
  geom_density_ridges2() +
  xlab("Flow Out (cms)") + 
  ylab("Dataset") +
  xlim(0,1000) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())

# one dataset one sub (by month)
blah=my_sub_baseline_to_projections %>% filter(dataset=="miroc_baseline")
ggplot(blah,aes(x=FLOW_OUTcms,y=as.factor(MO))) +
  geom_density_ridges2() +
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  xlim(0,1000) +
  theme_bw()
blah2=my_sub_baseline_to_projections %>% filter(dataset=="miroc8_5")
ggplot(blah2,aes(x=FLOW_OUTcms,y=as.factor(MO))) +
  geom_density_ridges2() +
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  xlim(0,1000) +
  theme_bw()

# all datasets one sub (just true and backcast baselines)
my_sub_true_baseline_to_bcbaseline=all_rch_data_sel %>% filter(RCH==my_sub) %>% filter(datatype!="projection")
ggplot(my_sub_true_baseline_to_bcbaseline,aes(x=FLOW_OUTcms,y=dataset,fill=datatype)) +
  geom_density_ridges2() +
  xlab("Flow Out (cms)") + 
  ylab("Dataset") +
  xlim(0,1000) +
  theme_bw()


# ---- 8.x plot flow distrubutions and cutoffs for outlet ----

blah$dataset=factor(blah$dataset,levels=c("major_outlier","minor_outlier","all_data"))
ggplot(blah,aes(x=FLOW_OUTcms,fill=dataset)) +
  geom_density(alpha=0.75) + #joyplot
  xlab("Flow Out (cms)") +
  ylab("Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# i don't think this density overlay is accurate b/c i want it to be on the same density scale
ggplot(blah,aes(x=FLOW_OUTcms)) +
  geom_density(alpha=0.75,fill="grey75") +
  #geom_vline(xintercept=hibound_minor_outlier,color="black",linetype=2) +
  #geom_vline(xintercept=hibound_major_outlier,color="black",linetype=1) +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") +
  ylab("Density") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))

ggplot(baseline_outlet_outlier_summary,aes(x=YR,y=n_minor_hiflow)) +
  geom_bar(stat="identity") +
  #facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Year") +
  ylab("Number of Minor Outlier High Flows (1.5xIQR)") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))


# ---- 8.x plot distributions of outflow for each subbasin by month and by year (Joyplot) ----

# select outlet data
baseline_outlet_rch_data=baseline_rch_data %>% filter(RCH==28)

#library(ggbeeswarm)

# by month (all subbasins)
ggplot(baseline_rch_data,aes(x=FLOW_OUTcms,y=as.factor(MO))) +
  geom_density_ridges2() + #joyplot
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# by month (outlet)
ggplot(baseline_outlet_rch_data,aes(x=FLOW_OUTcms,y=as.factor(MO))) +
  geom_density_ridges2() + #joyplot
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# by year (all subbasins)
ggplot(baseline_rch_data,aes(x=FLOW_OUTcms,y=as.factor(YR))) +
  geom_density_ridges2() + #joyplot
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# by year (outlet)
ggplot(baseline_outlet_rch_data,aes(x=FLOW_OUTcms,y=as.factor(YR))) +
  geom_density_ridges2() + #joyplot
  xlab("Flow Out (cms)") + 
  ylab("Month") +
  theme_bw()

# how does variance change from year to year
baseline_outlet_variance_by_yr=baseline_outlet_rch_data %>% group_by(YR) %>% summarise(variance=sd(FLOW_OUTcms))
ggplot(baseline_outlet_variance_by_yr,aes(x=as.factor(YR),y=variance)) + 
  geom_point(size=3) + 
  geom_smooth(method="lm") + # this function adds the fitted line (w/ confidence interval)
  theme(axis.text.x=element_text(angle=90))
# no trend so no lm added?
  
# determine outliers for each subbasin: https://www.wikihow.com/Calculate-Outliers
# determine quartiles
# but then this still doesn't have any meaning in the real world
# maybe add in month vs flow for climate data sets and see how they compare? (as differently colored distributions)


# ---- x. extra ----

sub_area=baseline_sub_data_raw %>% select(SUB,AREAkm2) %>% 
  transmute(SUB=SUB,sub_AREAkm2=round(AREAkm2,0)) %>% distinct()
rch_area=baseline_rch_data_raw %>% select(RCH,AREAkm2) %>% 
  transmute(RCH=RCH,rch_AREAkm2=round(AREAkm2,0)) %>% distinct()

subs_equal_rch=bind_cols(sub_area,rch_area) %>% filter(sub_AREAkm2==rch_AREAkm2)

test=bind_cols(sub_area,rch_area)


# network analysis help: http://www.shizukalab.com/toolkits/sna/plotting-directed-networks
# use network analysis graph to automate subbasin contributions of runoff?


# ---- x. function: find projection return period for baseline flow of a specified return period ----

# define function
return_period_diff=function(return_period,num_decimal_places,baseline_model_calcs,projection_model_calcs) {
  # return_period must be an entry in the modeled data
  
  # select only data for return period of interest
  baseline_return_period_sel=baseline_model_calcs %>% filter(model_return_period==return_period)
  projection_return_period_sel=projection_model_calcs %>% filter(model_return_period==return_period)
  
  # define variables and output dataframe
  return_period_range=seq(floor(min(baseline_model_calcs$model_return_period)),
                          floor(max(baseline_model_calcs$model_return_period)),1)
  num_subs=length(unique(baseline_model_calcs$SUB))
  diff_df=data.frame(SUB=as.integer(),
                     baseline_return_period=as.numeric(),
                     baseline_model_flow_cms=as.numeric(),
                     projection_return_period=as.numeric(),
                     projection_model_flow_cms=as.numeric(),
                     base_minus_proj_return_period=as.numeric(),
                     note=as.character())
  
  # for loop for each subbasin
  for (i in 1:num_subs) {
    # use spline rather than analytical solution to approx. result
    baseline_funct_temp=as.data.frame(spline(baseline_model_calcs$model_return_period[baseline_model_calcs$SUB==i],
                                             baseline_model_calcs$model_flow_cms[baseline_model_calcs$SUB==i],
                                             n=length(return_period_range)*(10^num_decimal_places), method="natural"))
    projection_funct_temp=as.data.frame(spline(projection_model_calcs$model_return_period[projection_model_calcs$SUB==i],
                                               projection_model_calcs$model_flow_cms[projection_model_calcs$SUB==i],
                                               n=length(return_period_range)*(10^num_decimal_places), method="natural"))
    
    # baseline data for specified return period and subbasin
    baseline_sub_temp=baseline_return_period_sel %>% filter(SUB==i)
    baseline_sub_flow_temp=baseline_sub_temp$model_flow_cms
    
    # find projection return period for corresponding baseline flow
    diff_temp=abs(baseline_sub_flow_temp-projection_funct_temp$y)
    projection_return_period_temp=round(projection_funct_temp$x[match(min(diff_temp),diff_temp)],num_decimal_places)
    projection_flow_temp=round(projection_funct_temp$y[match(min(diff_temp),diff_temp)],num_decimal_places)
    
    # note potential error and calculate difference for those without error
    if (projection_return_period_temp>=return_period) {
      base_minus_proj_return_period_temp=NA
      note_temp="check model"
    } else {
      base_minus_proj_return_period_temp=return_period-projection_return_period_temp
      note_temp=""
    }
    
    # save results to data frame
    diff_df_temp=data.frame(SUB=i,baseline_return_period=return_period,
                            baseline_model_flow_cms=baseline_sub_flow_temp,
                            projection_return_period=projection_return_period_temp,
                            projection_model_flow_cms=projection_flow_temp,
                            base_minus_proj_return_period=base_minus_proj_return_period_temp,
                            note=note_temp)
    
    # bind results to diff_df
    diff_df=bind_rows(diff_df,diff_df_temp)
  }
  
  # return output
  return(diff_df)
}


# ---- x. calculate return period difference ----

# csiro 4.5
csiro4_5_10years=return_period_diff(10,2,baseline_model_calcs,csiro4_5_model_calcs)
csiro4_5_100years=return_period_diff(100,2,baseline_model_calcs,csiro4_5_model_calcs)

# csiro 8.5
csiro8_5_10years=return_period_diff(10,2,baseline_model_calcs,csiro8_5_model_calcs)
csiro8_5_100years=return_period_diff(100,2,baseline_model_calcs,csiro8_5_model_calcs)

# hadley 4.5
hadley4_5_10years=return_period_diff(10,2,baseline_model_calcs,hadley4_5_model_calcs)
hadley4_5_100years=return_period_diff(100,2,baseline_model_calcs,hadley4_5_model_calcs)

# miroc 8.5
miroc8_5_10years=return_period_diff(10,2,baseline_model_calcs,miroc8_5_model_calcs)
miroc8_5_100years=return_period_diff(100,2,baseline_model_calcs,miroc8_5_model_calcs)


# ---- 3.x plot high flow freq. results for each subbasin (no backcast) ----

# plot observations and models together
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_max_flow_cms_adj),baseline_obs_calcs,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_calcs,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_calcs,color="green") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_calcs,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_calcs,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_calcs,color="blue") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())



# ---- 4.x calculate % change in flows for given return period (no backcast) ----

# miroc 8.5
miroc8_5_10yr_flow=flow_change(10,baseline_model_calcs,miroc8_5_model_calcs)
miroc8_5_100yr_flow=flow_change(100,baseline_model_calcs,miroc8_5_model_calcs)

# csiro 8.5
csiro8_5_10yr_flow=flow_change(10,baseline_model_calcs,csiro8_5_model_calcs)
csiro8_5_100yr_flow=flow_change(100,baseline_model_calcs,csiro8_5_model_calcs)

# csiro 4.5
csiro4_5_10yr_flow=flow_change(10,baseline_model_calcs,csiro4_5_model_calcs)
csiro4_5_100yr_flow=flow_change(100,baseline_model_calcs,csiro4_5_model_calcs)

# hadley 4.5
hadley4_5_10yr_flow=flow_change(10,baseline_model_calcs,hadley4_5_model_calcs)
hadley4_5_100yr_flow=flow_change(100,baseline_model_calcs,hadley4_5_model_calcs)


# ---- 4.x reformat calculations for plots (no backcast) ----

# 10 yr flow
# select data to add to shp file
miroc8_5_10yr_flow_sel=miroc8_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="miroc8_5",perc_change=flow_change_perc)
csiro8_5_10yr_flow_sel=csiro8_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro8_5",perc_change=flow_change_perc)
csiro4_5_10yr_flow_sel=csiro4_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro4_5",perc_change=flow_change_perc)
hadley4_5_10yr_flow_sel=hadley4_5_10yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="hadley4_5",perc_change=flow_change_perc)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# gather projections
hiflow_10yr_projections=bind_rows(miroc8_5_10yr_flow_sel,
                                  csiro8_5_10yr_flow_sel,
                                  csiro4_5_10yr_flow_sel,
                                  hadley4_5_10yr_flow_sel)

# add to shp file
yadkin_subs_shp_hiflow_10yr=left_join(yadkin_subs_shp,hiflow_10yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_10yr)


# 100 yr flow
# select data to add to shp file
miroc8_5_100yr_flow_sel=miroc8_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="miroc8_5",perc_change=flow_change_perc)
csiro8_5_100yr_flow_sel=csiro8_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro8_5",perc_change=flow_change_perc)
csiro4_5_100yr_flow_sel=csiro4_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="csiro4_5",perc_change=flow_change_perc)
hadley4_5_100yr_flow_sel=hadley4_5_100yr_flow %>% select(RCH,flow_change_perc) %>%
  transmute(SUB=RCH,dataset="hadley4_5",perc_change=flow_change_perc)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# gather projections
hiflow_100yr_projections=bind_rows(miroc8_5_100yr_flow_sel,
                                   csiro8_5_100yr_flow_sel,
                                   csiro4_5_100yr_flow_sel,
                                   hadley4_5_100yr_flow_sel)

# add to shp file
yadkin_subs_shp_hiflow_100yr=left_join(yadkin_subs_shp,hiflow_100yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_100yr)

# ---- 4.x plot % change in flows on map (no backcast) ----

# 10 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_10yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_10yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_10yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 10yr Flow",na.value="grey75",limits=c(-60,60)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# 100 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_100yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_100yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_100yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 100yr Flow",na.value="grey75") +
  theme_bw()
dev.off()


# ---- 4.x export results for sovi analysis (no backcast) ----

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(hiflow_10yr_projections,"hiflow_10yr_perc_change.csv")
#write_csv(hiflow_10yr_projections,"hiflow_100yr_perc_change.csv")



# ---- 6.x calculate % change in NUMBER OF MINOR OUTLIER high flows (no backcast) ----

# sum outlier counts data by subbasin
baseline_outlier_counts_sum=baseline_outlier_counts %>% filter(YR>1987) %>% 
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="baseline")
# baseline has to be cut down to most recent time period (1988-2008) to timeframe compares to projections
miroc8_5_outlier_counts_sum=miroc8_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="miroc8_5")
csiro8_5_outlier_counts_sum=csiro8_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro8_5")
csiro4_5_outlier_counts_sum=csiro4_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="csiro4_5")
hadley4_5_outlier_counts_sum=hadley4_5_outlier_counts %>%
  group_by(RCH) %>% summarize(sum_minor_hiflow=sum(n_minor_hiflow),sum_major_hiflow=sum(n_major_hiflow)) %>%
  mutate(dataset="hadley4_5")

# calculate % change 
miroc8_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,miroc8_5_outlier_counts_sum,flow_option="hiflow")
csiro8_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,csiro8_5_outlier_counts_sum,flow_option="hiflow")
csiro4_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,csiro4_5_outlier_counts_sum,flow_option="hiflow")
hadley4_5_hiflow_outlier_change=outlier_change(baseline_outlier_counts_sum,hadley4_5_outlier_counts_sum,flow_option="hiflow")

# bind rows
hiflow_outlier_change_no_bcbaseline_projections=bind_rows(miroc8_5_hiflow_outlier_change,
                                                          csiro8_5_hiflow_outlier_change,
                                                          csiro4_5_hiflow_outlier_change,
                                                          hadley4_5_hiflow_outlier_change) %>% mutate(SUB=RCH) %>% select(-RCH)

# add to shp file
yadkin_subs_shp_hiflow_outliers=left_join(yadkin_subs_shp,hiflow_outlier_change_no_bcbaseline_projections,by="SUB")
#glimpse(yadkin_subs_shp_hiflow_outliers)



# ---- 6.x plot % change in outlier high flows on map (no backcast) ----

# minor outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_minor_outlier_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers,aes(fill=minor_outlier_perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Minor High Flow Outliers",na.value="grey75",limits=c(-10,120)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# major outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_major_outlier_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_hiflow_outliers,aes(fill=major_outlier_perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_hiflow_outliers is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Major High Flow Outliers",na.value="grey75",limits=c(-20,300)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

