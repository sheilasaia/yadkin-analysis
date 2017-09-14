# yadkin low flow frequency analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(sf)
#library(smwrBase)
#devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
library(ggplot2)

# load home-made functions 
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path,"reformat_rch_file.R")) # reformat SWAT .rch file
source(paste0(functions_path,"logpearson3_factor_calc.R")) # calculate log-Pearson III frequency factors
source(paste0(functions_path,"remove_outliers.R")) # removes low and high flows deemed as outliers
source(paste0(functions_path,"obs_lowflow_freq_calcs_one_rch.R")) # select observations for one reach
#source(paste0(functions_path,"obs_flood_freq_calcs_one_rch.R")) # select observations for one reach
source(paste0(functions_path,"obs_freq_calcs_all_rchs.R")) # selects observations for all reaches
source(paste0(functions_path,"model_lowflow_freq_calcs_one_rch.R")) # determines low-flow model for one reach
#source(paste0(functions_path,"model_flood_freq_calcs_one_rch.R")) # determines low-flow model for one reach
source(paste0(functions_path,"model_freq_calcs_all_rchs.R")) # determines low-flow model for all reaches
source(paste0(functions_path,"movingAve.R")) # from smwrBase with edits (add here for now)

# download kn_table for outlier analysis
setwd("/Users/ssaia/Documents/GitHub/yadkin-analysis/")
kn_table=read_csv("kn_table_appendix4_usgsbulletin17b.csv",col_names=TRUE)

# set directory and load data
# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
baseline_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#yadkin_net_data_raw=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# CSIRO RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/C_CSIRO45")
csiro4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# CSIRO RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# Hadley RCP4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/D_Hadley45")
hadley4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# MIROC RCP8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/gis_data")
yadkin_subs_shp=st_read("subs1.shp",quiet=TRUE)


# ---- 2. reformat data ----

# baseline data
baseline_rch_data=reformat_rch_file(baseline_rch_raw_data)

# CSIRO 4.5 data
csiro4_5_rch_data=reformat_rch_file(csiro4_5_rch_raw_data)

# CSIRO 8.5 data
csiro8_5_rch_data=reformat_rch_file(csiro8_5_rch_raw_data)

# HADLEY 4.5 data
hadley4_5_rch_data=reformat_rch_file(hadley4_5_rch_raw_data)

# MIROC 8.5 data
miroc8_5_rch_data=reformat_rch_file(miroc8_5_rch_raw_data)

# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)


# ---- 3. calculate obs and model ouptuts for each subbasin ----

#baseline_rch_data_test=baseline_rch_data %>% filter(RCH<11)
#baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(baseline_rch_data_test,span_days=1,flow_option="lowflow")
baseline_obs_lowflow_calcs=obs_freq_calcs_all_rchs(baseline_rch_data,span_days=1,flow_option="lowflow")
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_lowflow_calcs=model_freq_calcs_all_rchs(baseline_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

csiro4_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(csiro4_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro4_5_model_lowflow_calcs=model_freq_calcs_all_rchs(csiro4_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

csiro8_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(csiro8_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro8_5_model_lowflow_calcs=model_freq_calcs_all_rchs(csiro8_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

hadley4_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(hadley4_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
hadley4_5_model_lowflow_calcs=model_freq_calcs_all_rchs(hadley4_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")

miroc8_5_obs_lowflow_calcs=obs_freq_calcs_all_rchs(miroc8_5_rch_data,span_days=1,flow_option="lowflow")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
miroc8_5_model_lowflow_calcs=model_freq_calcs_all_rchs(miroc8_5_obs_lowflow_calcs,kn_table,my_model_p_list,0.4,flow_option="lowflow")


# ---- 4. plot results for each subbasin ----

# omit zero values from observations
baseline_obs_lowflow_calcs_nozeros=baseline_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
csiro4_5_obs_lowflow_calcs_nozeros=csiro4_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
csiro8_5_obs_lowflow_calcs_nozeros=csiro8_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
hadley4_5_obs_lowflow_calcs_nozeros=hadley4_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)
miroc8_5_obs_lowflow_calcs_nozeros=miroc8_5_obs_lowflow_calcs %>% filter(obs_min_flow_cms_adj>0)

# omit NAs from model outputs
baseline_model_lowflow_calcs_noNAa=baseline_model_lowflow_calcs %>% na.omit()
csiro4_5_model_lowflow_calcs_noNAa=csiro4_5_model_lowflow_calcs %>% na.omit()
csiro8_5_model_lowflow_calcs_noNAa=csiro8_5_model_lowflow_calcs %>% na.omit()
hadley4_5_model_lowflow_calcs_noNAa=hadley4_5_model_lowflow_calcs %>% na.omit()
miroc8_5_model_lowflow_calcs_noNAa=miroc8_5_model_lowflow_calcs %>% na.omit()

# plot observations and models together (not log transformed)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_lowflow_calcs_noNAa,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_lowflow_calcs_noNAa,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_lowflow_calcs_noNAa,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_lowflow_calcs_noNAa,color="blue") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_lowflow_calcs_noNAa,color="green") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (cms)") +
  ylim(0,max(baseline_obs_lowflow_calcs$obs_min_flow_cms_adj)+25) +
  theme_bw()

# plot observations and models together (log transformed)
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_min_flow_log_cms_adj),baseline_obs_lowflow_calcs_nozeros,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),baseline_model_lowflow_calcs_noNAa,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),csiro4_5_model_lowflow_calcs_noNAa,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),csiro8_5_model_lowflow_calcs_noNAa,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),hadley4_5_model_lowflow_calcs_noNAa,color="blue") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_log_cms),miroc8_5_model_lowflow_calcs_noNAa,color="green") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (log cms)") +
  ylim((min(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)-2),(max(baseline_obs_lowflow_calcs_nozeros$obs_min_flow_log_cms_adj)+2)) +
  xlim(0,105) +
  theme_bw()


# ---- 10. function: find baseline and projection flows for same return period ----

flow_diff=function(return_period,baseline_model_calcs,projection_model_calcs) {
  # return_period must be an entry in the modeled data
  
  # select only data for return period of interest
  baseline_return_period_sel=baseline_model_calcs %>% filter(round(model_return_period_yr,4)==return_period)
  projection_return_period_sel=projection_model_calcs %>% filter(round(model_return_period_yr,4)==return_period)
  
  # define variables and output dataframe
  num_rchs=length(unique(baseline_model_calcs$RCH))
  diff_df=data.frame(RCH=as.integer(),
                     return_period_yr=as.numeric(),
                     baseline_model_flow_cms=as.numeric(),
                     projection_model_flow_cms=as.numeric(),
                     proj_minus_base_flow_cms=as.numeric(),
                     proj_minus_base_flow_percchange=as.numeric())
  
  # for loop for each subbasin
  for (i in 1:num_rchs) {
    
    # baseline data for specified return period and subbasin
    baseline_rch_temp=baseline_return_period_sel %>% filter(RCH==i)
    baseline_rch_flow_temp=baseline_rch_temp$model_flow_cms
    
    # projection data for specified return period and subbasin
    projection_rch_temp=projection_return_period_sel %>% filter(RCH==i)
    projection_rch_flow_temp=projection_rch_temp$model_flow_cms
    
    # find difference
    proj_minus_base_flow_cms_temp=projection_rch_flow_temp-baseline_rch_flow_temp
    proj_minus_base_flow_percchange_temp=(proj_minus_base_flow_cms_temp/baseline_rch_flow_temp)*100
    
    # save results to data frame
    diff_df_temp=data.frame(RCH=i,
                            return_period=return_period,
                            baseline_model_flow_cms=baseline_rch_flow_temp,
                            projection_model_flow_cms=projection_rch_flow_temp,
                            proj_minus_base_flow_cms=proj_minus_base_flow_cms_temp,
                            proj_minus_base_flow_percchange=proj_minus_base_flow_percchange_temp)
    
    # bind results to diff_df
    diff_df=bind_rows(diff_df,diff_df_temp)
  }
  
  # return output
  return(diff_df) 
}


# ---- 11. calculate flow difference ----

# csiro 4.5
csiro4_5_10yr_flow=flow_diff(10,baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)
csiro4_5_100yr_flow=flow_diff(100,baseline_model_lowflow_calcs,csiro4_5_model_lowflow_calcs)

# csiro 8.5
csiro8_5_10yr_flow=flow_diff(10,baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)
csiro8_5_100yr_flow=flow_diff(100,baseline_model_lowflow_calcs,csiro8_5_model_lowflow_calcs)

# hadley 4.5
hadley4_5_10yr_flow=flow_diff(10,baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)
hadley4_5_100yr_flow=flow_diff(100,baseline_model_lowflow_calcs,hadley4_5_model_lowflow_calcs)

# miroc 8.5
miroc8_5_10yr_flow=flow_diff(10,baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)
miroc8_5_100yr_flow=flow_diff(100,baseline_model_lowflow_calcs,miroc8_5_model_lowflow_calcs)


# ---- 12. plot flow differences on map ----

# csiro 4.5 vs baseline 10 yr flow
# select only necessary down data
csiro4_5_10yr_flow_sel=csiro4_5_10yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, csiro4_5_10yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro4_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p1=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro4_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-100,100),na.value="grey75")


# csiro 4.5 vs baseline 100 yr flow
# select only necessary down data
csiro4_5_100yr_flow_sel=csiro4_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, csiro4_5_100yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro4_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p5=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro4_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow")#,limits=c(-100,315),na.value="grey75")


# csiro 8.5 vs baseline 10 yr flow
# select only necessary down data
csiro8_5_10yr_flow_sel=csiro8_5_10yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, csiro8_5_10yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p2=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro8_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-100,100),na.value="grey75")


# csiro 8.5 vs baseline 100 yr flow
# select only necessary down data
csiro8_5_100yr_flow_sel=csiro8_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, csiro8_5_100yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p6=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro8_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow")#,limits=c(-100,315),na.value="grey75")


# hadley 4.5 vs baseline 10 yr flow
# select only necessary down data
hadley4_5_10yr_flow_sel=hadley4_5_10yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, hadley4_5_10yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p3=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-100,100),na.value="grey75")


# hadley 4.5 vs baseline 100 yr flow
# select only necessary down data
hadley4_5_100yr_flow_sel=hadley4_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, hadley4_5_100yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p7=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow")#,limits=c(-100,315),na.value="grey75")


# miroc 8.5 vs baseline 10 yr flow
# select only necessary down data
miroc8_5_10yr_flow_sel=miroc8_5_10yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, miroc8_5_10yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p4=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=miroc8_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-100,100),na.value="grey75")


# miroc 8.5 vs baseline 100 yr flow
# select only necessary down data
miroc8_5_100yr_flow_sel=miroc8_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, miroc8_5_100yr_flow_perc=proj_minus_base_flow_percchange)
# RCH is generally equal to SUB and need SUB column for joining to .shp file

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p8=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=miroc8_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow")#,limits=c(-100,315),na.value="grey75")


# plot 10yr figures
setwd("/Users/ssaia/Desktop")
pdf("test.pdf",width=11,height=8.5)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

# plot 100yr figures
setwd("/Users/ssaia/Desktop")
pdf("test2.pdf",width=11,height=8.5)
multiplot(p5, p6, p7, p8, cols=2)
dev.off()


# ---- 13. function: multiplot ----
# from: http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


# ---- X. count number of low flow frequency obs = zero ----

num_yrs=length(unique(baseline_rch_data$YR))
baseline_obs_lowflow_calcs_zero_count=baseline_obs_lowflow_calcs %>% 
  filter(obs_min_flow_cms_adj==0) %>%
  group_by(SUB) %>% summarize(num_zero_entries=n()) %>%
  mutate(percent_zero=num_zero_entries/num_yrs)


# ---- X. calculate lowflow counts ----

# find days where flow_out equals lowflow
my_lowflow=0.01 # in cms
baseline_lowflow_data=baseline_rch_data %>% filter(FLOW_OUTcms<my_lowflow)

# count number of days for each basin (for each year) where flow equals zero
baseline_lowflow_tally=baseline_lowflow_data %>% group_by(SUB,YR) %>% 
  summarize(num_lowflow_days=n(),
            avg_FLOW_INcms=mean(FLOW_INcms),
            avg_FLOW_OUTcms=mean(FLOW_OUTcms)) %>%
  arrange(YR,SUB)


# ---- X. plot lowflow counts (scatter plot and on map) ----

# scatter plot vs time
ggplot(baseline_lowflow_tally,aes(x=YR,y=num_lowflow_days,color=as.factor(SUB))) +
  geom_point(size=3) +
  xlab("year") + 
  ylab(paste("num days w/ outflow <", my_lowflow,"cms")) +
  theme_bw()

# spatial distrubution
# select only necessary down data
baseline_lowflow_tally_sel=baseline_lowflow_tally %>% select(SUB,num_lowflow_days) %>%
  group_by(SUB) %>% summarize(num_lowflow_days=sum(num_lowflow_days))

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,baseline_lowflow_tally_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=num_lowflow_days)) +
  scale_fill_gradient(name=paste("total # days <",my_lowflow,"cms \n from 1982-2008"),limits=c(100,200),
                      low="white",high="red",na.value="grey75")