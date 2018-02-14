# yadkin low flow frequency analysis

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
source(paste0(functions_path,"obs_lowflow_freq_calcs_one_rch.R")) # select observations for one reach
source(paste0(functions_path,"obs_freq_calcs_all_rchs.R")) # selects observations for all reaches
source(paste0(functions_path,"model_lowflow_freq_calcs_one_rch.R")) # determines lowflow model for one reach
source(paste0(functions_path,"model_freq_calcs_all_rchs.R")) # determines flow model for all reaches
source(paste0(functions_path,"flow_change.R")) # determines % change in flows for a given return period
source(paste0(functions_path,"count_lowflow_outliers.R")) # counts number of minor and major outliers for risk analysis
source(paste0(functions_path,"count_lowflow_outliers_using_baseline.R")) # counts number of minor and major outliers for risk analysis based on baseline cutoffs
source(paste0(functions_path,"outlier_change.R")) # determines % change in minor and major outliers
source(paste0(functions_path,"no_flow_change.R")) # determines % change in number of days with no flow

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
csiro_baseline_rch_raw_data=read_table2("CSIRO-output.rch",col_names=FALSE,skip=9) # baseline backcast .rch file from SWAT
miroc_baseline_rch_raw_data=read_table2("MIROC-output.rch",col_names=FALSE,skip=9) # baseline backcast .rch file from SWAT
hadley_baseline_rch_raw_data=read_table2("Hadley-output.rch",col_names=FALSE,skip=9) # baseline backcast .rch file from SWAT

# miroc rcp 8.5 climate+landuse change data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 8.5 climate+landuse change data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 8.5 climate+landuse change data
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

# miroc 8.5 data
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


# ---- 3.1 calculate obs and model ouptuts for each subbasin ----

# probability list
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)

# baseline (not backcast)
baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(baseline_rch_data,1,"lowflow")
baseline_model_lowflow_calcs=model_freq_calcs_all_rchs(baseline_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")

# miroc baseline backcast
miroc_baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(miroc_baseline_rch_data,1,"lowflow")
miroc_baseline_model_lowflow_calcs=model_freq_calcs_all_rchs(miroc_baseline_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")

# mirco 8.5 projection
miroc8_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(miroc8_5_rch_data,1,"lowflow")
miroc8_5_model_lowflow_calcs=model_freq_calcs_all_rchs(miroc8_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")

# csiro baseline backcast (for comparison with csiro 8.5 and 4.5 projections)
csiro_baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(csiro_baseline_rch_data,1,"lowflow")
csiro_baseline_model_lowflow_calcs=model_freq_calcs_all_rchs(csiro_baseline_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")

# csiro 8.5 projection
csiro8_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(csiro8_5_rch_data,1,"lowflow")
csiro8_5_model_lowflow_calcs=model_freq_calcs_all_rchs(csiro8_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")

# cisro 4.5 projection
csiro4_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(csiro4_5_rch_data,1,"lowflow")
csiro4_5_model_lowflow_calcs=model_freq_calcs_all_rchs(csiro4_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")

# hadley baseline backcast
hadley_baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(hadley_baseline_rch_data,1,"lowflow")
hadley_baseline_model_lowflow_calcs=model_freq_calcs_all_rchs(hadley_baseline_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")

# hadley 4.5 projection
hadley4_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(hadley4_5_rch_data,1,"lowflow")
hadley4_5_model_lowflow_calcs=model_freq_calcs_all_rchs(hadley4_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,"lowflow")


# ---- 3.2 plot low flow freq. baselines for each subbasin (no backcast and backcast comparison) ----

# omit zero values from observations
baseline_obs_lowflow_calcs_nozeros=baseline_obs_lowflow_calcs %>% 
  filter(obs_min_flow_cms_adj>0)
miroc_baseline_obs_lowflow_calcs_nozeros=miroc_baseline_obs_lowflow_calcs %>% 
  filter(obs_min_flow_log_cms_adj>0)
miroc8_5_obs_lowflow_calcs_nozeros=miroc8_5_obs_lowflow_calcs %>% 
  filter(obs_min_flow_cms_adj>0)
csiro_baseline_obs_lowflow_calcs_nozeros=csiro_baseline_obs_lowflow_calcs %>% 
  filter(obs_min_flow_log_cms_adj>0)
csiro8_5_obs_lowflow_calcs_nozeros=csiro8_5_obs_lowflow_calcs %>% 
  filter(obs_min_flow_cms_adj>0)
csiro4_5_obs_lowflow_calcs_nozeros=csiro4_5_obs_lowflow_calcs %>% 
  filter(obs_min_flow_cms_adj>0)
hadley_baseline_obs_lowflow_calcs_nozeros=hadley_baseline_obs_lowflow_calcs %>% 
  filter(obs_min_flow_log_cms_adj>0)
hadley4_5_obs_lowflow_calcs_nozeros=hadley4_5_obs_lowflow_calcs %>% 
  filter(obs_min_flow_cms_adj>0)

# omit NAs from model outputs
baseline_model_lowflow_calcs_noNA=baseline_model_lowflow_calcs %>% na.omit()
miroc_baseline_model_lowflow_calcs_noNA=miroc_baseline_model_lowflow_calcs %>% na.omit()
miroc8_5_model_lowflow_calcs_noNA=miroc8_5_model_lowflow_calcs %>% na.omit()
csiro_baseline_model_lowflow_calcs_noNA=csiro_baseline_model_lowflow_calcs %>% na.omit()
csiro8_5_model_lowflow_calcs_noNA=csiro8_5_model_lowflow_calcs %>% na.omit()
csiro4_5_model_lowflow_calcs_noNA=csiro4_5_model_lowflow_calcs %>% na.omit()
hadley_baseline_model_lowflow_calcs_noNA=hadley_baseline_model_lowflow_calcs %>% na.omit()
hadley4_5_model_lowflow_calcs_noNA=hadley4_5_model_lowflow_calcs %>% na.omit()

# plot observations and baseline models together
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_lowflow_calcs_noNA,color="black",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc_baseline_model_lowflow_calcs_noNA,color="green",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro_baseline_model_lowflow_calcs_noNA,color="red",linetype=1) +
  #geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley_baseline_model_lowflow_calcs_noNA,color="blue",linetype=1) +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


# ---- 3.3 plot low flow freq. results for each subbasin (no backcast) ----

# plot observations and models together (not log transformed)
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_models.pdf",width=11,height=8.5)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_lowflow_calcs_noNA,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_lowflow_calcs_noNA,color="green") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_lowflow_calcs_noNA,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_lowflow_calcs_noNA,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_lowflow_calcs_noNA,color="blue") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  ylim(0,max(baseline_obs_lowflow_calcs$obs_min_flow_cms_adj)+25) +
  xlim(0,105) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#dev.off()

# plot observations and models together (log transformed)
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_models_log.pdf",width=11,height=8.5)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_log_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),baseline_model_lowflow_calcs_noNA,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),miroc8_5_model_lowflow_calcs_noNA,color="green") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),csiro8_5_model_lowflow_calcs_noNA,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),csiro4_5_model_lowflow_calcs_noNA,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),hadley4_5_model_lowflow_calcs_noNA,color="blue") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("return Period (yr)") + 
  ylab("Flow Out (log cms)") +
  ylim((min(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)-2),(max(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)+2)) +
  xlim(0,105) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
#dev.off()


# ---- 3.4 plot low flow freq. results for each subbasin (backcast) ----

# plot observations and models together (not log transformed)
ggplot() +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc_baseline_model_lowflow_calcs,color="green",linetype=2) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_lowflow_calcs,color="green",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro_baseline_model_lowflow_calcs,color="red",linetype=2,size=0.75) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_lowflow_calcs,color="red",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro_baseline_model_lowflow_calcs,color="orange",linetype=2) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_lowflow_calcs,color="orange",linetype=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley_baseline_model_lowflow_calcs,color="blue",linetype=2) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_lowflow_calcs,color="blue",linetype=1) +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("Return Period (yr)") + 
  ylab("Flow Out (cms)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())


# ---- 4.1 calculate % change in flows for given return period (no backcast) ----

# miroc 8.5
miroc8_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)
miroc8_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)

# csiro 8.5
csiro8_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)
csiro8_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)

# csiro 4.5
csiro4_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)
csiro4_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)

# hadley 4.5
hadley4_5_10yr_flow=flow_change(10,baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)
hadley4_5_100yr_flow=flow_change(100,baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)


# ---- 4.2 reformat calculations for plots (no backcast) ----

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
lowflow_10yr_projections=bind_rows(miroc8_5_10yr_flow_sel,
                                   csiro8_5_10yr_flow_sel,
                                   csiro4_5_10yr_flow_sel,
                                   hadley4_5_10yr_flow_sel)

# add to shp file
yadkin_subs_shp_lowflow_10yr=left_join(yadkin_subs_shp,lowflow_10yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_10yr)


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
lowflow_100yr_projections=bind_rows(miroc8_5_100yr_flow_sel,
                                    csiro8_5_100yr_flow_sel,
                                    csiro4_5_100yr_flow_sel,
                                    hadley4_5_100yr_flow_sel)

# add to shp file
yadkin_subs_shp_lowflow_100yr=left_join(yadkin_subs_shp,lowflow_100yr_projections,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_100yr)


# ---- 4.3 plot % change in lowflows on map (no backcast) ----

# 10 yr
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_10yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_10yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_10yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 10yr Lowflow",na.value="grey75") +
  theme_bw()
#dev.off()

# 100 yr
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("lowflow_100yr_change.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_100yr,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_100yr is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 100yr Lowflow",na.value="grey75") +
  theme_bw()
#dev.off()


# ---- 4.4 export results for sovi analysis (no backcast) ----

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(lowflow_10yr_projections,"lowflow_10yr_perc_change.csv")
#write_csv(lowflow_10yr_projections,"lowflow_100yr_perc_change.csv")


# ---- 4.5 calculate % change in flows for given return period (backcast) ----

# using baseline backcast for each projection rather than true baseline

# miroc 8.5
miroc8_5_10yr_flow_bc=flow_change(10,miroc_baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)
miroc8_5_100yr_flow_bc=flow_change(100,miroc_baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)

# csiro 8.5
csiro8_5_10yr_flow_bc=flow_change(10,csiro_baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)
csiro8_5_100yr_flow_bc=flow_change(100,csiro_baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)

# csiro 4.5
csiro4_5_10yr_flow_bc=flow_change(10,csiro_baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)
csiro4_5_100yr_flow_bc=flow_change(100,csiro_baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)

# hadley 4.5
hadley4_5_10yr_flow_bc=flow_change(10,hadley_baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)
hadley4_5_100yr_flow_bc=flow_change(100,hadley_baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)


# ---- 4.6 reformat calculations for plots (backcast) ----

# using baseline backcast for each projection rather than true baseline

# 10 yr flow
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
lowflow_10yr_projections_bc=bind_rows(miroc8_5_10yr_flow_bc_sel,
                                      csiro8_5_10yr_flow_bc_sel,
                                      csiro4_5_10yr_flow_bc_sel,
                                      hadley4_5_10yr_flow_bc_sel)

# add to shp file
yadkin_subs_shp_lowflow_10yr_bc=left_join(yadkin_subs_shp,lowflow_10yr_projections_bc,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_10yr_bc)


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
lowflow_100yr_projections_bc=bind_rows(miroc8_5_100yr_flow_bc_sel,
                                      csiro8_5_100yr_flow_bc_sel,
                                      csiro4_5_100yr_flow_bc_sel,
                                      hadley4_5_100yr_flow_bc_sel)

# add to shp file
yadkin_subs_shp_lowflow_100yr_bc=left_join(yadkin_subs_shp,lowflow_100yr_projections_bc,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_100yr_bc)

# ---- 4.7 plot % change in flows on map (backcast) ----

# 10 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_10yr_change_bc.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_10yr_bc,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_10yr_bc is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 10yr Flow",na.value="grey75",limits=c(-50,250)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# 100 yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_100yr_change_bc.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_100yr_bc,aes(fill=perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_100yr_bc is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change 100yr Flow",na.value="grey75",limits=c(-55,505)) +
  theme_bw()
dev.off()


# ---- 4.8 export results for sovi analysis (backcast) ----

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(lowflow_10yr_projections_bc,"lowflow_10yr_perc_change_bc.csv")
#write_csv(lowflow_10yr_projections_bc,"lowflow_100yr_perc_change_bc.csv")


# ---- 5.1 count number of days with no flow ----

# baseline (not backcast)
baseline_num_yrs=length(unique(baseline_rch_data$YR))
baseline_obs_no_flow_counts=baseline_rch_data %>%
  group_by(RCH) %>% summarise(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/baseline_num_yrs,
         dataset="true_baseline",datatype="baseline")

# miroc baseline backcast
miroc_baseline_num_yrs=length(unique(miroc_baseline_rch_data$YR))
miroc_baseline_obs_no_flow_counts=miroc_baseline_rch_data %>%
  group_by(RCH) %>% summarise(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/miroc_baseline_num_yrs,
         dataset="miroc_bcbaseline",datatype="baseline")

# miroc 8.5
miroc8_5_num_yrs=length(unique(miroc8_5_rch_data$YR))
miroc8_5_obs_no_flow_counts=miroc8_5_rch_data %>% 
  group_by(RCH) %>% summarize(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/miroc8_5_num_yrs,
         dataset="miroc8_5",datatype="projection")

# csiro baseline backcast (for comparison with csiro 8.5 and 4.5 projections)
csiro_baseline_num_yrs=length(unique(csiro_baseline_rch_data$YR))
csiro_baseline_obs_no_flow_counts=csiro_baseline_rch_data %>%
  group_by(RCH) %>% 
  summarise(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/csiro_baseline_num_yrs,
         dataset="csiro_bcbaseline",datatype="baseline")

# csiro 8.5
csiro8_5_num_yrs=length(unique(csiro8_5_rch_data$YR))
csiro8_5_obs_no_flow_counts=csiro8_5_rch_data %>%
  group_by(RCH) %>% 
  summarise(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/csiro8_5_num_yrs,
         dataset="csiro8_5",datatype="projection")

# csiro 4.5
csiro4_5_num_yrs=length(unique(csiro4_5_rch_data$YR))
csiro4_5_obs_no_flow_counts=csiro4_5_rch_data %>%
  group_by(RCH) %>% 
  summarise(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/csiro4_5_num_yrs,
         dataset="csiro4_5",datatype="projection")

# hadley baseline backcast
hadley_baseline_num_yrs=length(unique(hadley_baseline_rch_data$YR))
hadley_baseline_obs_no_flow_counts=hadley_baseline_rch_data %>%
  group_by(RCH) %>% 
  summarise(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/hadley_baseline_num_yrs,
         dataset="hadley_bcbaseline",datatype="baseline")

# hadley 4.5
hadley4_5_num_yrs=length(unique(hadley4_5_rch_data$YR))
hadley4_5_obs_no_flow_counts=hadley4_5_rch_data %>%
  group_by(RCH) %>% 
  summarise(sum_n_no_flow_entries=sum(FLOW_OUTcms==0)) %>%
  mutate(n_no_flow_entries_per_yr=sum_n_no_flow_entries/hadley4_5_num_yrs,
         dataset="hadley4_5",datatype="projection")

# bind all together for each subbasin
all_models_no_flow_counts_by_sub=bind_rows(baseline_obs_no_flow_counts,
                                        miroc_baseline_obs_no_flow_counts,
                                        miroc8_5_obs_no_flow_counts,
                                        csiro_baseline_obs_no_flow_counts,
                                        csiro8_5_obs_no_flow_counts,
                                        csiro4_5_obs_no_flow_counts,
                                        hadley_baseline_obs_no_flow_counts,
                                        hadley4_5_obs_no_flow_counts)

# summarize for each dataset (sum of subbasins)
num_subs=length(unique(baseline_rch_data$RCH))
all_models_no_flow_counts=all_models_no_flow_counts_by_sub %>% 
  group_by(dataset,datatype) %>%
  summarize(sum_n_no_flow_entries_by_dataset=sum(sum_n_no_flow_entries),
            sum_n_no_flow_entries_by_dataset_per_year_per_subbasin=sum(sum_n_no_flow_entries)/baseline_num_yrs/num_subs)


# ---- 5.2 calculate % change in number days with no flow (backcast) ----

# number of years in simulation
# for this script to work we must assume basleine and projection are equal in simulation length
baseline_num_yrs=length(unique(hadley_baseline_rch_data$YR))

# calculate % change 
miroc8_5_no_flow_change_using_bcbaseline=no_flow_change(miroc_baseline_obs_no_flow_counts,miroc8_5_obs_no_flow_counts,baseline_num_yrs)
csiro8_5_no_flow_change_using_bcbaseline=no_flow_change(csiro_baseline_obs_no_flow_counts,csiro8_5_obs_no_flow_counts,baseline_num_yrs)
csiro4_5_no_flow_change_using_bcbaseline=no_flow_change(csiro_baseline_obs_no_flow_counts,csiro4_5_obs_no_flow_counts,baseline_num_yrs)
hadley4_5_no_flow_change_using_bcbaseline=no_flow_change(hadley_baseline_obs_no_flow_counts,hadley4_5_obs_no_flow_counts,baseline_num_yrs)

# bind rows
all_models_no_flow_change=bind_rows(miroc8_5_no_flow_change_using_bcbaseline,
                                    csiro8_5_no_flow_change_using_bcbaseline,
                                    csiro4_5_no_flow_change_using_bcbaseline,
                                    hadley4_5_no_flow_change_using_bcbaseline) %>%
  mutate(SUB=RCH) %>% select(-RCH)

# add to shp file
yadkin_subs_shp_no_flow_using_bcbaseline=left_join(yadkin_subs_shp,all_models_no_flow_change,by="SUB")
#glimpse(yadkin_subs_shp_no_flow_using_bcbaseline)

# adjust levels
all_models_no_flow_change$dataset=factor(all_models_no_flow_change$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
yadkin_subs_shp_no_flow_using_bcbaseline$dataset=factor(yadkin_subs_shp_no_flow_using_bcbaseline$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))


# ---- 5.3 plot number days with no flow by subbasin per year (no backcast and backcast) ----

setwd("/Users/ssaia/Desktop")
cairo_pdf("no_flow_counts_by_sub.pdf",width=11,height=8.5)
all_models_no_flow_counts_by_sub$dataset=factor(all_models_no_flow_counts_by_sub$dataset,levels=c("true_baseline","miroc_bcbaseline","miroc8_5","csiro_bcbaseline","csiro8_5","csiro4_5","hadley_bcbaseline","hadley4_5"))
all_models_no_flow_counts_by_sub$datatype=factor(all_models_no_flow_counts_by_sub$datatype,levels=c("baseline","projection"))
ggplot(all_models_no_flow_counts_by_sub,aes(x=dataset,y=n_no_flow_entries_per_yr,fill=datatype)) +
  geom_col() +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("") +
  ylab("Number of Days with No Flow/Year") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()


# ---- 5.4 plot total number days with no flow by dataset (sum of subbasins, no backcast and backcast) ----

setwd("/Users/ssaia/Desktop")
cairo_pdf("no_flow_counts_by_dataset.pdf",width=11,height=8.5)
all_models_no_flow_counts$dataset=factor(all_models_no_flow_counts$dataset,levels=c("true_baseline","miroc_bcbaseline","miroc8_5","csiro_bcbaseline","csiro8_5","csiro4_5","hadley_bcbaseline","hadley4_5"))
all_models_no_flow_counts$datatype=factor(all_models_no_flow_counts$datatype,levels=c("baseline","projection"))
ggplot(all_models_no_flow_counts,aes(x=dataset,y=sum_n_no_flow_entries_by_dataset_per_year_per_subbasin,fill=datatype)) +
  geom_col() +
  xlab("") +
  ylab("Number Days with No Flow/Year/Subbasin") +
  scale_fill_manual(values=c("grey75","black")) +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank())
dev.off()


# ---- 5.5 plot % change in days with no flow on map (backcast) ----

# results below 40 % change
setwd("/Users/ssaia/Desktop")
cairo_pdf("no_flow_change_using_bcbaseline_low.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_no_flow_using_bcbaseline,aes(fill=no_flow_perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Days with Flow = 0/yr",na.value="grey75",limits=c(-10,40),high="darkred",low="darkblue") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# results above 40 % change
setwd("/Users/ssaia/Desktop")
cairo_pdf("no_flow_change_using_bcbaseline_up.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_no_flow_using_bcbaseline,aes(fill=no_flow_perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change # Days with Flow = 0/yr",na.value="grey75",limits=c(40,205),high="darkred",low="white") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()


# ---- 5.6 plot variation in days with no flow (backcast) ----

# make dataframe with contributing errors to can use to plot
contributing_areas=baseline_rch_data %>% select(RCH,AREAkm2) %>%
  distinct() %>% 
  mutate(SUB=RCH) %>% 
  select(-RCH)

# join areas
all_models_no_flow_change_area=all_models_no_flow_change %>%
  left_join(contributing_areas,by='SUB')

# backcast baseline (and recode them for plotting)
no_flow_change_baseline=all_models_no_flow_change_area %>%
  select(SUB,AREAkm2,baseline_sum_n_no_flow_entries,dataset) %>%
  mutate(baseline_sum_n_no_flow_entries_per_year=baseline_sum_n_no_flow_entries/baseline_num_yrs) %>%
  filter(dataset!="csiro4_5") # don't need both CSIRO datasets b/c backcast baselines are the same for both
no_flow_change_baseline$dataset=recode(no_flow_change_baseline$dataset,"miroc8_5"="MIROC","csiro8_5"="CSIRO","hadley4_5"="Hadley")

# backcast baseline ordered by subbasin area
no_flow_change_baseline$SUB=factor(no_flow_change_baseline$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
no_flow_change_baseline$dataset=factor(no_flow_change_baseline$dataset,levels=c("MIROC","CSIRO","Hadley"))

# backcast baseline summary for pointrange plot
no_flow_change_baseline_summary=no_flow_change_baseline %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_no_flow_entries_per_year=min(baseline_sum_n_no_flow_entries_per_year),
            max_n_no_flow_entries_per_year=max(baseline_sum_n_no_flow_entries_per_year),
            mean_n_no_flow_entries_per_year=mean(baseline_sum_n_no_flow_entries_per_year))

# backcast baseline plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("num_no_flow_baseline.pdf",width=11,height=8.5,pointsize=18)
ggplot() +
  geom_pointrange(data=no_flow_change_baseline_summary,
                  aes(x=SUB,y=mean_n_no_flow_entries_per_year,ymin=min_n_no_flow_entries_per_year,ymax=max_n_no_flow_entries_per_year),shape=32) +
  geom_point(data=no_flow_change_baseline,aes(x=SUB,y=baseline_sum_n_no_flow_entries_per_year,color=dataset),
             shape=17,size=5,alpha=0.75, position=position_jitter(height=0.1,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Days with No Flow/yr") +
  scale_color_manual(values=c("grey75","grey50","black")) +
  ylim(-1,25) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18))
dev.off()


# projections
no_flow_change_projection=all_models_no_flow_change_area %>%
  select(SUB,AREAkm2,projection_sum_n_no_flow_entries,dataset) %>%
  mutate(projection_sum_n_no_flow_entries_per_year=projection_sum_n_no_flow_entries/miroc8_5_num_yrs) # all are equal to 21 but use miroc8_5_num_yrs for simplicity 

# projections ordered by subbasin area
no_flow_change_projection$SUB=factor(no_flow_change_projection$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
no_flow_change_projection$dataset=factor(no_flow_change_projection$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# projections summary for pointrange plot
no_flow_change_projection_summary=no_flow_change_projection %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_no_flow_entries_per_year=min(projection_sum_n_no_flow_entries_per_year),
            max_n_no_flow_entries_per_year=max(projection_sum_n_no_flow_entries_per_year),
            mean_n_no_flow_entries_per_year=mean(projection_sum_n_no_flow_entries_per_year))

# projections plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("num_no_flow_projection.pdf",width=11,height=8.5,pointsize=18)
ggplot() +
  geom_pointrange(data=no_flow_change_projection_summary,
                  aes(x=SUB,y=mean_n_no_flow_entries_per_year,ymin=min_n_no_flow_entries_per_year,ymax=max_n_no_flow_entries_per_year),
                  shape=32) +
  geom_point(data=no_flow_change_projection,aes(x=SUB,y=projection_sum_n_no_flow_entries_per_year,color=dataset),
             size=5,alpha=0.75, position=position_jitter(height=0.1,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Days with No Flow/yr") +
  scale_color_manual(values=c("grey80","grey60","grey40","black")) +
  ylim(-1,25) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18))
dev.off()


# ---- 5.7 calculate number of consecutive days with no flow (backcast) ----

consec_no_flow_df=miroc_baseline_rch_data %>%
  mutate(date_ymd=ymd(sprintf('%04d%02d%02d', YR, MO, DA))) %>% select(-DA) %>%
  filter(FLOW_OUTcms==0) 

sub_list=sort(unique(consec_no_flow_df$RCH))
num_subs=length(sub_list)

# make data frames to hold results
num_no_flow_df=data.frame(SUB=as.numeric(),
                          num_no_flow_events=as.numeric())
length_no_flow_df=data.frame(SUB=as.numeric(),
                             no_flow_event_strt_month=as.numeric(),
                             no_flow_event_strt_year=as.numeric(),
                             no_flow_event_length_days=as.numeric())


for (i in 1:num_subs) {
  # select data from one sub, arrange by date, and find moving difference
  consec_no_flow_df_temp=consec_no_flow_df %>% filter(RCH==sub_list[i]) %>%
    arrange(date_ymd) %>%
    mutate(moving_diff=movingDiff(as.numeric(consec_no_flow_df_temp$date_ymd),span=1))
  
  # whenever movingDiff>1 here it's the start of a no flow event
  num_no_flow_events_temp=dim(consec_no_flow_df_temp %>% filter(moving_diff>1))[1]
  
  # initialize variables
  no_flow_event_length_days=rep(0,num_no_flow_events_temp)
  no_flow_event_strt_month=rep(0,num_no_flow_events_temp)
  no_flow_event_strt_year=rep(0,num_no_flow_events_temp)
  str=1
  tally=1
  
  # calculate length and timing of no flow period
  for (j in 2:95){#dim(consec_no_flow_df_temp)[1]) {
    current_day_diff=consec_no_flow_df_temp$moving_diff[j]
    future_day_diff=consec_no_flow_df_temp$moving_diff[j+1]
    if (current_day_diff>1 & future_day_diff==1) { # start of event
      tally=tally+1
      no_flow_event_strt_month[str]=consec_no_flow_df_temp$MO[j]
      no_flow_event_strt_year[str]=consec_no_flow_df_temp$YR[j]
    }
    
    else if (current_day_diff==1 & future_day_diff==1) { # during event
      tally=tally+1
    }
    
    else if (current_day_diff==1 & future_day_diff>1) { # end of event
      no_flow_event_length_days[str]=tally
      str=str+1 # move to next entry in no_flow_event_length_days
      tally=1 # restart tally
    }
    
    else if (current_day_diff>1 & future_day_diff>1) { # single day event
      tally=1 # restart tally
      no_flow_event_length_days[str]=tally
      no_flow_event_strt_month[str]=consec_no_flow_df_temp$MO[j]
      no_flow_event_strt_year[str]=consec_no_flow_df_temp$YR[j]
      str=str+1 # move to next entry in no_flow_event_length_days
    }
    
    else if (current_day_diff==1 & is.na(future_day_diff)) { # end of data
      
    }
  }
  return()
}



# ---- 5.8 plot number of consecutive days with no flow (backcast) ----

# use rle()! and tibble time?

# ---- 5.9 export results ----

# just export percent change
all_models_no_flow_change_sel=all_models_no_flow_change %>%
  select(SUB,dataset,no_flow_perc_change,no_flow_perc_change_per_yr)

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(all_models_no_flow_change_sel,"no_flow_perc_change_data.csv")
#write_csv(contributing_areas,"subbasin_contributing_areas_data.csv")


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


# ---- 6.2 calculate outlier cutoffs and number of outlier low flows (backcast) ----

# baseline (not backcast)
#baseline_outlier_calcs=count_lowflow_outliers(baseline_rch_data)
#baseline_outlier_counts=baseline_outlier_calcs[[1]]
#baseline_outlier_cutoffs=baseline_outlier_calcs[[2]]

# miroc baseline (backcast)
miroc_baseline_outlier_calcs=count_lowflow_outliers(miroc_baseline_rch_data)
miroc_baseline_outlier_counts=miroc_baseline_outlier_calcs[[1]]
miroc_baseline_outlier_cutoffs=miroc_baseline_outlier_calcs[[2]]

# csiro baseline (backcast)
csiro_baseline_outlier_calcs=count_lowflow_outliers(csiro_baseline_rch_data)
csiro_baseline_outlier_counts=csiro_baseline_outlier_calcs[[1]]
csiro_baseline_outlier_cutoffs=csiro_baseline_outlier_calcs[[2]]

# hadley baseline (backcast)
hadley_baseline_outlier_calcs=count_lowflow_outliers(hadley_baseline_rch_data)
hadley_baseline_outlier_counts=hadley_baseline_outlier_calcs[[1]]
hadley_baseline_outlier_cutoffs=hadley_baseline_outlier_calcs[[2]]

# miroc 8.5
#miroc8_5_outlier_calcs=count_lowflow_outliers(miroc8_5_rch_data) # find outliers using cutoff from data itself
#miroc8_5_outlier_counts=miroc8_5_outlier_calcs[[1]]
#miroc8_5_outlier_cutoffs=miroc8_5_outlier_calcs[[2]]
miroc8_5_outlier_calcs_using_baseline=count_lowflow_outliers_using_baseline(miroc_baseline_outlier_cutoffs,miroc8_5_rch_data) # find outliers using backcast baseline cutoff
miroc8_5_outlier_counts_using_baseline=miroc8_5_outlier_calcs_using_baseline[[1]]
miroc8_5_outlier_cutoffs_using_baseline=miroc8_5_outlier_calcs_using_baseline[[2]]

# csiro 8.5
#csiro8_5_outlier_calcs=count_lowflow_outliers(csiro8_5_rch_data)
#csiro8_5_outlier_counts=csiro8_5_outlier_calcs[[1]]
#csiro8_5_outlier_cutoffs=csiro8_5_outlier_calcs[[2]]
csiro8_5_outlier_calcs_using_baseline=count_lowflow_outliers_using_baseline(csiro_baseline_outlier_cutoffs,csiro8_5_rch_data) # find outliers using backcast baseline cutoff
csiro8_5_outlier_counts_using_baseline=csiro8_5_outlier_calcs_using_baseline[[1]]
csiro8_5_outlier_cutoffs_using_baseline=csiro8_5_outlier_calcs_using_baseline[[2]]

# csiro 4.5
#csiro4_5_outlier_calcs=count_lowflow_outliers(csiro4_5_rch_data)
#csiro4_5_outlier_counts=csiro4_5_outlier_calcs[[1]]
#csiro4_5_outlier_cutoffs=csiro4_5_outlier_calcs[[2]]
csiro4_5_outlier_calcs_using_baseline=count_lowflow_outliers_using_baseline(csiro_baseline_outlier_cutoffs,csiro4_5_rch_data) # find outliers using backcast baseline cutoff
csiro4_5_outlier_counts_using_baseline=csiro4_5_outlier_calcs_using_baseline[[1]]
csiro4_5_outlier_cutoffs_using_baseline=csiro4_5_outlier_calcs_using_baseline[[2]]

# hadley 4.5
#hadley4_5_outlier_calcs=count_lowflow_outliers(hadley4_5_rch_data)
#hadley4_5_outlier_counts=hadley4_5_outlier_calcs[[1]]
#hadley4_5_outlier_cutoffs=hadley4_5_outlier_calcs[[2]]
hadley4_5_outlier_calcs_using_baseline=count_lowflow_outliers_using_baseline(hadley_baseline_outlier_cutoffs,hadley4_5_rch_data) # find outliers using backcast baseline cutoff
hadley4_5_outlier_counts_using_baseline=hadley4_5_outlier_calcs_using_baseline[[1]]
hadley4_5_outlier_cutoffs_using_baseline=hadley4_5_outlier_calcs_using_baseline[[2]]


# sum outlier counts data by subbasin
# backcast baselines
miroc_baseline_outlier_counts_sum=miroc_baseline_outlier_counts %>%
  group_by(RCH) %>% 
  summarize(sum_minor_lowflow=sum(n_minor_lowflow),sum_major_lowflow=sum(n_major_lowflow)) %>%
  mutate(dataset="miroc_baseline",datatype="baseline")
csiro_baseline_outlier_counts_sum=csiro_baseline_outlier_counts %>%
  group_by(RCH) %>% 
  summarize(sum_minor_lowflow=sum(n_minor_lowflow),sum_major_lowflow=sum(n_major_lowflow)) %>%
  mutate(dataset="csiro_baseline",datatype="baseline")
hadley_baseline_outlier_counts_sum=hadley_baseline_outlier_counts %>%
  group_by(RCH) %>% 
  summarize(sum_minor_lowflow=sum(n_minor_lowflow),sum_major_lowflow=sum(n_major_lowflow)) %>%
  mutate(dataset="hadley_baseline",datatype="baseline")

# projections
miroc8_5_outlier_counts_using_baseline_sum=miroc8_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_lowflow=sum(n_minor_lowflow),sum_major_lowflow=sum(n_major_lowflow)) %>%
  mutate(dataset="miroc8_5",datatype="projection")
csiro8_5_outlier_counts_using_baseline_sum=csiro8_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_lowflow=sum(n_minor_lowflow),sum_major_lowflow=sum(n_major_lowflow)) %>%
  mutate(dataset="csiro8_5",datatype="projection")
csiro4_5_outlier_counts_using_baseline_sum=csiro4_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_lowflow=sum(n_minor_lowflow),sum_major_lowflow=sum(n_major_lowflow)) %>%
  mutate(dataset="csiro4_5",datatype="projection")
hadley4_5_outlier_counts_using_baseline_sum=hadley4_5_outlier_counts_using_baseline %>%
  group_by(RCH) %>% 
  summarize(sum_minor_lowflow=sum(n_minor_lowflow),sum_major_lowflow=sum(n_major_lowflow)) %>%
  mutate(dataset="hadley4_5",datatype="projection")

all_models_lowflow_outlier_counts=bind_rows(miroc_baseline_outlier_counts_sum,
                                            csiro_baseline_outlier_counts_sum,
                                            hadley_baseline_outlier_counts_sum,
                                            miroc8_5_outlier_counts_using_baseline_sum,
                                            csiro8_5_outlier_counts_using_baseline_sum,
                                            csiro4_5_outlier_counts_using_baseline_sum,
                                            hadley4_5_outlier_counts_using_baseline_sum)

# ---- 6.3 calculate % change in outlier low flows (backcast) ----

# specify number of years of baseline/projection
# for this script to work these have to be equal
baseline_num_yrs=length(unique(baseline_rch_data$YR))

# calculate % change 
miroc8_5_lowflow_outlier_change_using_bcbaseline=outlier_change(miroc_baseline_outlier_counts_sum,miroc8_5_outlier_counts_using_baseline_sum,flow_option="lowflow",baseline_num_yrs)
csiro8_5_lowflow_outlier_change_using_bcbaseline=outlier_change(csiro_baseline_outlier_counts_sum,csiro8_5_outlier_counts_using_baseline_sum,flow_option="lowflow",baseline_num_yrs)
csiro4_5_lowflow_outlier_change_using_bcbaseline=outlier_change(csiro_baseline_outlier_counts_sum,csiro4_5_outlier_counts_using_baseline_sum,flow_option="lowflow",baseline_num_yrs)
hadley4_5_lowflow_outlier_change_using_bcbaseline=outlier_change(hadley_baseline_outlier_counts_sum,hadley4_5_outlier_counts_using_baseline_sum,flow_option="lowflow",baseline_num_yrs)

# bind rows
all_models_lowflow_outlier_change=bind_rows(miroc8_5_lowflow_outlier_change_using_bcbaseline,
                                            csiro8_5_lowflow_outlier_change_using_bcbaseline,
                                            csiro4_5_lowflow_outlier_change_using_bcbaseline,
                                            hadley4_5_lowflow_outlier_change_using_bcbaseline) %>% 
  mutate(SUB=RCH) %>% 
  select(-RCH)

# add to shp file
yadkin_subs_shp_lowflow_outliers_using_bcbaseline=left_join(yadkin_subs_shp,all_models_lowflow_outlier_change,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_outliers_using_bcbaseline)

# adjust levels
yadkin_subs_shp_lowflow_outliers_using_bcbaseline$dataset=factor(yadkin_subs_shp_lowflow_outliers_using_bcbaseline$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))


# ---- 6.4 plot variation in days with low flow (backcast) ----

# make dataframe with contributing errors to can use to plot
contributing_areas=baseline_rch_data %>% select(RCH,AREAkm2) %>%
  distinct() %>% 
  mutate(SUB=RCH) %>% 
  select(-RCH)

# join areas
all_models_lowflow_change_area=all_models_lowflow_outlier_change %>%
  left_join(contributing_areas,by='SUB')

# backcast baselines (and recode them for plotting)
baseline_num_yrs=length(unique(baseline_rch_data$YR))
lowflow_change_baseline=all_models_lowflow_change_area %>%
  select(SUB,AREAkm2,baseline_sum_n_minor_outliers,dataset) %>%
  mutate(baseline_sum_n_minor_outliers_per_year=baseline_sum_n_minor_outliers/baseline_num_yrs) %>%
  filter(dataset!="csiro4_5") # don't need both CSIRO datasets b/c backcast baselines are the same for both
lowflow_change_baseline$dataset=recode(lowflow_change_baseline$dataset,"miroc8_5"="MIROC","csiro8_5"="CSIRO","hadley4_5"="Hadley")

# backcast baseline ordered by subbasin area
lowflow_change_baseline$SUB=factor(lowflow_change_baseline$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
lowflow_change_baseline$dataset=factor(lowflow_change_baseline$dataset,levels=c("MIROC","CSIRO","Hadley"))

# backcast baseline summary for pointrange plot
lowflow_change_baseline_summary=lowflow_change_baseline %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_minor_outliers_per_year=min(baseline_sum_n_minor_outliers_per_year),
            max_n_minor_outliers_per_year=max(baseline_sum_n_minor_outliers_per_year),
            mean_n_minor_outliers_per_year=mean(baseline_sum_n_minor_outliers_per_year))

# backcast baselines plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("num_lowflow_baseline.pdf",width=11,height=8.5,pointsize=18)
ggplot() +
  geom_pointrange(data=lowflow_change_baseline_summary,
                  aes(x=SUB,y=mean_n_minor_outliers_per_year,ymin=min_n_minor_outliers_per_year,ymax=max_n_minor_outliers_per_year),shape=32) +
  geom_point(data=lowflow_change_baseline,aes(x=SUB,y=baseline_sum_n_minor_outliers_per_year,color=dataset),
             shape=17,size=5,alpha=0.75, position=position_jitter(height=0.1,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Minor LOFs/yr") +
  scale_color_manual(values=c("grey75","grey50","black")) +
  ylim(-1,30) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18))
dev.off()


# projections
miroc8_5_num_yrs=length(unique(miroc8_5_rch_data$YR)) # all are equal to 21 but use miroc8_5_num_yrs for simplicity
lowflow_change_projection=all_models_lowflow_change_area %>%
  select(SUB,AREAkm2,projection_sum_n_minor_outliers,dataset) %>%
  mutate(projection_sum_n_minor_outliers_per_year=projection_sum_n_minor_outliers/miroc8_5_num_yrs) # all are equal to 21 but use miroc8_5_num_yrs for simplicity 

# projections ordered by subbasin area
lowflow_change_projection$SUB=factor(lowflow_change_projection$SUB,levels=contributing_areas$SUB[order(contributing_areas$AREAkm2)])
lowflow_change_projection$dataset=factor(lowflow_change_projection$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# projections summary for pointrange plot
lowflow_change_projection_summary=lowflow_change_projection %>%
  group_by(SUB,AREAkm2) %>%
  summarize(min_n_minor_outliers_per_year=min(projection_sum_n_minor_outliers_per_year),
            max_n_minor_outliers_per_year=max(projection_sum_n_minor_outliers_per_year),
            mean_n_minor_outliers_per_year=mean(projection_sum_n_minor_outliers_per_year))

# projections plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("num_lowflow_projection.pdf",width=11,height=8.5,pointsize=18)
ggplot() +
  geom_pointrange(data=lowflow_change_projection_summary,
                  aes(x=SUB,y=mean_n_minor_outliers_per_year,ymin=min_n_minor_outliers_per_year,ymax=max_n_minor_outliers_per_year),
                  shape=32) +
  geom_point(data=lowflow_change_projection,aes(x=SUB,y=projection_sum_n_minor_outliers_per_year,color=dataset),
             size=5,alpha=0.75, position=position_jitter(height=0.1,width=0)) +
  #geom_smooth(method='loess',formula=y~x) +
  xlab("SWAT Subbasin Number (by Increasing Conbributing Area)") +
  ylab("Number of Minor LOFs/yr") +
  scale_color_manual(values=c("grey80","grey60","grey40","black")) +
  ylim(-1,30) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),
        axis.text.x=element_text(angle=90,hjust=1,vjust=0.5),
        text=element_text(size=18))
dev.off()


# ---- 6.5 plot boxplots for low flow outlier context (backcast) ----

ggplot(all_models_lowflow_outlier_change) +
  geom_point(aes(x=dataset,y=minor_outlier_perc_change))

# ---- 6.6 plot % change in outlier low flows on map (backcast) ----

# minor outliers (up to 100% change)
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_minor_outlier_change_using_bcbaseline_low.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_outliers_using_bcbaseline,aes(fill=minor_outlier_perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change Number of Minor LOFs/yr",limits=c(-25,100),na.value="grey75",high="darkred",low="darkblue") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# minor outliers (beyond 100% change)
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_minor_outlier_change_using_bcbaseline_up.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_outliers_using_bcbaseline,aes(fill=minor_outlier_perc_change_per_yr)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change in Number of Minor LOFs",na.value="grey75",limits=c(100,220),high="darkred",low="white") +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# major outliers
setwd("/Users/ssaia/Desktop")
cairo_pdf("lowflow_major_outlier_change_using_baseline_bc.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_outliers_using_bcbaseline,aes(fill=major_outlier_perc_change)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_outliers_using_bcbaseline is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change Number of Major LOFs",na.value="grey75",limits=c(-10,160)) +
  theme_bw() #+
#theme(axis.text = element_text(size = 20)) +
#theme(axis.title = element_text(size = 20)) +
#theme(text = element_text(size = 20))
dev.off()

# ---- 6.7 export results from outlier analysis ----

# just export percent change
all_models_lowflow_outlier_change_sel=all_models_lowflow_outlier_change %>%
  select(SUB,dataset,minor_outlier_perc_change,minor_outlier_perc_change_per_yr,
         major_outlier_perc_change,major_outlier_perc_change_per_yr)

# export to results
#setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
#write_csv(all_models_lowflow_outlier_change_sel,"lowflow_outlier_perc_change_data.csv")


# ---- x.1 calculate lowflow counts below threshold ----

# find days where flow_out equals lowflow
my_threshold=0.01 # in cms

# create data frame to pad lowflow data
num_subs=28
num_baseline_yrs=27
my_yr_list=as.numeric()
str=1
for (i in 1982:2008) {
  my_yr_list[str:(str+num_subs-1)]=rep(i,num_subs)
  str=str+num_subs
}
baseline_pad_ids=data.frame(RCH=rep(seq(1:num_subs),num_baseline_yrs),
                             YR=my_yr_list) %>% 
  mutate(id=paste0(RCH,"_",YR)) %>% select(id)

# baseline data
baseline_lowflow_data=baseline_rch_data %>% filter(FLOW_OUTcms<my_threshold)
baseline_lowflow_thresh_counts=baseline_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="baseline")

# miroc 8.5
miroc8_5_lowflow_data=miroc8_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
miroc8_5_lowflow_thresh_counts=miroc8_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="miroc8_5")

# csiro 8.5
csiro8_5_lowflow_data=csiro8_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
csiro8_5_lowflow_thresh_counts=csiro8_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="csiro8_5")

# csiro 4.5
csiro4_5_lowflow_data=csiro4_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
csiro4_5_lowflow_thresh_counts=csiro4_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="csiro4_5")

# hadley 4.5 select
hadley4_5_lowflow_data=hadley4_5_rch_data %>% filter(FLOW_OUTcms<my_threshold)
hadley4_5_lowflow_thresh_counts=hadley4_5_lowflow_data %>% group_by(RCH,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,RCH) %>% mutate(dataset="hadley4_5")

# combine all
all_models_lowflow_thresh_counts=bind_rows(baseline_lowflow_thresh_counts,
                                           miroc8_5_lowflow_thresh_counts,
                                           csiro8_5_lowflow_thresh_counts,
                                           csiro4_5_lowflow_thresh_counts,
                                           hadley4_5_lowflow_thresh_counts)


# ---- x.2 plot lowflow counts vs time (bar plot) ----

# baseline
ggplot(baseline_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
         geom_col() +
         facet_wrap(~RCH) +
         xlab("Year") +
         ylab("Baseline Number of Low Flow Events") +
         theme_bw()


# miroc 8.5
ggplot(miroc8_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("MIROC 8.5 Number of Low Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))

# csiro 8.5
ggplot(csiro8_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("CSIRO 8.5 Number of Low Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))

# csiro 4.5
ggplot(csiro4_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("CSIRO 4.5 Number of Low Flow Events") +
  theme_bw()

# hadley 4.5
ggplot(hadley4_5_lowflow_thresh_counts,aes(x=YR,y=num_lowflow_days)) +
  geom_col() +
  facet_wrap(~RCH) +
  xlab("Year") +
  ylab("Hadley 4.5 Number of Low Flow Events") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))


# ---- x.3 plot lowflow counts in space (map) ----

# select only necessary down data
all_models_lowflow_thresh_counts_sum=all_models_lowflow_thresh_counts %>% 
  select(RCH,YR,num_lowflow_days,dataset) %>%
  group_by(RCH,dataset) %>% 
  summarize(num_lowflow_days_per_yr=sum(num_lowflow_days)/20) %>%
  select(SUB=RCH,num_lowflow_days_per_yr=num_lowflow_days_per_yr,dataset=dataset)

# select only baseline
baseline_lowflow_thresh_counts_sum=all_models_lowflow_thresh_counts_sum %>%
  filter(dataset=="baseline")

# select only projections
all_projs_lowflow_thresh_counts_sum=all_models_lowflow_thresh_counts_sum %>% 
  filter(dataset!="baseline")

# add to shp file
yadkin_subs_shp_lowflow_base=left_join(yadkin_subs_shp,baseline_lowflow_thresh_counts_sum,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_base)
yadkin_subs_shp_lowflow_proj=left_join(yadkin_subs_shp,all_projs_lowflow_thresh_counts_sum,by="SUB")
#glimpse(yadkin_subs_shp_lowflow_proj)

# plot baseline
setwd("/Users/ssaia/Desktop")
cairo_pdf("baseline_lowflow_count.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_lowflow_base,aes(fill=num_lowflow_days_per_yr)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_base is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Avg # Lowflow Days/Yr",na.value="grey75",limits=c(0,10)) +
  theme_bw()
dev.off()

# plot projections (this isn't working!)
#setwd("/Users/ssaia/Desktop")
#cairo_pdf("projections_lowflow_count.pdf",width=11,height=8.5)
#ggplot(yadkin_subs_shp_lowflow_proj,aes(fill=num_lowflow_days_per_yr)) +
#  facet_wrap(~dataset) +
#  geom_sf() +
#  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_lowflow_projs is base utm 17N so convert to Albers for CONUS
#  scale_fill_gradient2("Avg # Lowflow Days/Yr",na.value="grey75") +
#  theme_bw()
#dev.off()

