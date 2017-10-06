# yadkin flood frequency analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(smwrBase)
library(tidyverse)
#devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
#library(ggplot2)
library(sf)

# load home-made functions 
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path,"reformat_rch_file.R")) # reformat SWAT .rch file
source(paste0(functions_path,"logpearson3_factor_calc.R")) # calculate log-Pearson III frequency factors
source(paste0(functions_path,"remove_outliers.R")) # removes low and high flows deemed as outliers
source(paste0(functions_path,"obs_flood_freq_calcs_one_rch.R")) # select observations for one reach
source(paste0(functions_path,"obs_freq_calcs_all_rchs.R")) # selects observations for all reaches
source(paste0(functions_path,"model_flood_freq_calcs_one_rch.R")) # determines flood flow model for one reach
source(paste0(functions_path,"model_freq_calcs_all_rchs.R")) # determines flow model for all reaches

# download kn_table for outlier analysis
setwd("/Users/ssaia/Documents/GitHub/yadkin-analysis/")
kn_table=read_csv("kn_table_appendix4_usgsbulletin17b.csv",col_names=TRUE)

# set directory and load data
# baseline data & river network
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
#baseline_sub_raw_data=read_table2("output.sub",col_names=FALSE,skip=9) # baseline .sub file from SWAT
baseline_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT
#yadkin_net_raw_data=read_csv("rch_table.txt",col_names=TRUE) # wdreach.shp attribute table from ArcSWAT

# miroc rcp 8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/A_MIROC8.5")
miroc8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 8.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/B_CSIRO85")
csiro8_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# csiro rcp 4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/C_CSIRO45")
csiro4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)

# hadley rcp 4.5 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/D_Hadley45")
hadley4_5_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9)



# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)

# looking at gis data
yadkin_subs_shp_albers_geom=st_geometry(yadkin_subs_shp_albers)
attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2. reformat data ----

# reach file (.rch) 
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

# shape file (.shp)
# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp_utm %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)

# join areas for yadkin_net_data (=weights)
#yadkin_sub_areas=baseline_sub_data %>% select(SUB,AREAkm2) %>% distinct()
#yadkin_net_data=left_join(yadkin_net_data_sel,yadkin_sub_areas,by="SUB") %>%
#  select(FROM_NODE,TO_NODE,AREAkm2) # remove SUB b/c FROM_NODE=SUB


# ---- 3.1. calculate obs and model ouptuts for each subbasin ----

baseline_obs_calcs=obs_freq_calcs_all_rchs(baseline_rch_data,1,"flood")
my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
baseline_model_calcs=model_freq_calcs_all_rchs(baseline_obs_calcs,kn_table,my_model_p_list,0.4,"flood")

csiro4_5_obs_calcs=obs_freq_calcs_all_rchs(csiro4_5_rch_data,1,"flood")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro4_5_model_calcs=model_freq_calcs_all_rchs(csiro4_5_obs_calcs,kn_table,my_model_p_list,0.4,"flood")

csiro8_5_obs_calcs=obs_freq_calcs_all_rchs(csiro8_5_rch_data,1,"flood")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
csiro8_5_model_calcs=model_freq_calcs_all_rchs(csiro8_5_obs_calcs,kn_table,my_model_p_list,0.4,"flood")

hadley4_5_obs_calcs=obs_freq_calcs_all_rchs(hadley4_5_rch_data,1,"flood")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
hadley4_5_model_calcs=model_freq_calcs_all_rchs(hadley4_5_obs_calcs,kn_table,my_model_p_list,0.4,"flood")

miroc8_5_obs_calcs=obs_freq_calcs_all_rchs(miroc8_5_rch_data,1,"flood")
#my_model_p_list=c(0.99,0.95,0.9,0.8,0.7,0.6,0.5,0.4,0.2,0.1,0.08,0.06,0.04,0.03,0.02,0.01)
miroc8_5_model_calcs=model_freq_calcs_all_rchs(miroc8_5_obs_calcs,kn_table,my_model_p_list,0.4,"flood")


# ---- 3.2. plot results for each subbasin ----

# plot observations and models together
ggplot() +
  geom_point(aes(x=obs_return_period_yr,y=obs_max_flow_cms_adj),baseline_obs_calcs,size=1) +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),baseline_model_calcs,color="black") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro4_5_model_calcs,color="orange") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),csiro8_5_model_calcs,color="red") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),hadley4_5_model_calcs,color="blue") +
  geom_line(aes(x=model_return_period_yr,y=model_flow_cms),miroc8_5_model_calcs,color="green") +
  facet_wrap(~RCH,ncol=7,nrow=4) +
  xlab("return period") + 
  ylab("flow out (cms)") +
  theme_bw()


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


# ---- 4.1. function: find baseline and projection flows for same return period ----

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


# ---- 4.2. calculate flow difference ----

# csiro 4.5
csiro4_5_10yr_flow=flow_diff(10,baseline_model_calcs,csiro4_5_model_calcs)
csiro4_5_100yr_flow=flow_diff(100,baseline_model_calcs,csiro4_5_model_calcs)

# csiro 8.5
csiro8_5_10yr_flow=flow_diff(10,baseline_model_calcs,csiro8_5_model_calcs)
csiro8_5_100yr_flow=flow_diff(100,baseline_model_calcs,csiro8_5_model_calcs)

# hadley 4.5
hadley4_5_10yr_flow=flow_diff(10,baseline_model_calcs,hadley4_5_model_calcs)
hadley4_5_100yr_flow=flow_diff(100,baseline_model_calcs,hadley4_5_model_calcs)

# miroc 8.5
miroc8_5_10yr_flow=flow_diff(10,baseline_model_calcs,miroc8_5_model_calcs)
miroc8_5_100yr_flow=flow_diff(100,baseline_model_calcs,miroc8_5_model_calcs)


# ---- 4.3. plot flow differences on map ----

# need to add column with name of dataset so can use facet


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
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60),na.value="grey75")


# csiro 4.5 vs baseline 100 yr flow
# select only necessary down data
csiro4_5_100yr_flow_sel=csiro4_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, csiro4_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro4_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p5=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro4_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315),na.value="grey75")


# csiro 8.5 vs baseline 10 yr flow
# select only necessary down data
csiro8_5_10yr_flow_sel=csiro8_5_10yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, csiro8_5_10yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p2=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro8_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60),na.value="grey75")


# csiro 8.5 vs baseline 100 yr flow
# select only necessary down data
csiro8_5_100yr_flow_sel=csiro8_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, csiro8_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p6=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=csiro8_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315),na.value="grey75")


# hadley 4.5 vs baseline 10 yr flow
# select only necessary down data
hadley4_5_10yr_flow_sel=hadley4_5_10yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, hadley4_5_10yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p3=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60),na.value="grey75")


# hadley 4.5 vs baseline 100 yr flow
# select only necessary down data
hadley4_5_100yr_flow_sel=hadley4_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, hadley4_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p7=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315),na.value="grey75")


# miroc 8.5 vs baseline 10 yr flow
# select only necessary down data
miroc8_5_10yr_flow_sel=miroc8_5_10yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, miroc8_5_10yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_10yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p4=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=miroc8_5_10yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 10yr Flow",limits=c(-60,60),na.value="grey75")


# miroc 8.5 vs baseline 100 yr flow
# select only necessary down data
miroc8_5_100yr_flow_sel=miroc8_5_100yr_flow %>% select(RCH,proj_minus_base_flow_percchange) %>%
  transmute(SUB=RCH, miroc8_5_100yr_flow_perc=proj_minus_base_flow_percchange)

# add to shp file
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_100yr_flow_sel,by="SUB")
#glimpse(yadkin_subs_shp)

# plot
p8=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=miroc8_5_100yr_flow_perc)) +
  scale_fill_gradient2(name="% Change 100yr Flow",limits=c(-100,315),na.value="grey75")


# plot 10yr figures
setwd("/Users/ssaia/Desktop")
pdf("test.pdf",width=11,height=8.5)
multiplot(p1, p2, p3, p4, cols=2) # this is taking forever to load...
#library(gridExtra)
#grid.arrange(p1,p2,nrow=2)
#library(tmap)
#library(ggpubr)
#ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
dev.off()

# plot 100yr figures
setwd("/Users/ssaia/Desktop")
pdf("test2.pdf",width=11,height=8.5)
multiplot(p5, p6, p7, p8, cols=2)
dev.off()


# ---- 4.4. function: multiplot ----
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



# ---- 4.5. export results for kelly ----

# kelly's watersheds (8,10,18,28) and return periods (10,100)

# baseline
baseline_model_flood_freq_sel=baseline_model_calcs %>% 
  filter(RCH==8 | RCH==10 | RCH==18 | RCH==28) %>%
  filter(round(model_return_period_yr,4)==10 | round(model_return_period_yr,4)==100) %>%
  mutate(dataset="baseline")

# csiro 4.5
csiro4_5_model_flood_freq_sel=csiro4_5_model_calcs %>% 
  filter(RCH==8 | RCH==10 | RCH==18 | RCH==28) %>%
  filter(round(model_return_period_yr,4)==10 | round(model_return_period_yr,4)==100) %>%
  mutate(dataset="csiro_4_5")

# csiro 8.5
csiro8_5_model_flood_freq_sel=csiro8_5_model_calcs %>% 
  filter(RCH==8 | RCH==10 | RCH==18 | RCH==28) %>%
  filter(round(model_return_period_yr,4)==10 | round(model_return_period_yr,4)==100) %>%
  mutate(dataset="csiro_8_5")

# hadley 4.5
hadley4_5_model_flood_freq_sel=hadley4_5_model_calcs %>% 
  filter(RCH==8 | RCH==10 | RCH==18 | RCH==28) %>%
  filter(round(model_return_period_yr,4)==10 | round(model_return_period_yr,4)==100) %>%
  mutate(dataset="hadley_4_5")

# miroc 8.5
miroc8_5_model_flood_freq_sel=miroc8_5_model_calcs %>% 
  filter(RCH==8 | RCH==10 | RCH==18 | RCH==28) %>%
  filter(round(model_return_period_yr,4)==10 | round(model_return_period_yr,4)==100) %>%
  mutate(dataset="miroc_8_5")

# bind together
all_flood_freq_models_sel=bind_rows(baseline_model_flood_freq_sel,
                                csiro4_5_model_flood_freq_sel,
                                csiro8_5_model_flood_freq_sel,
                                hadley4_5_model_flood_freq_sel,
                                miroc8_5_model_flood_freq_sel)

# export to results
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
write_csv(all_flood_freq_models_sel,"model_flood_frequency_results_for_kelly.csv")





# ---- x. extra ----

sub_area=baseline_sub_data_raw %>% select(SUB,AREAkm2) %>% 
  transmute(SUB=SUB,sub_AREAkm2=round(AREAkm2,0)) %>% distinct()
rch_area=baseline_rch_data_raw %>% select(RCH,AREAkm2) %>% 
  transmute(RCH=RCH,rch_AREAkm2=round(AREAkm2,0)) %>% distinct()

subs_equal_rch=bind_cols(sub_area,rch_area) %>% filter(sub_AREAkm2==rch_AREAkm2)

test=bind_cols(sub_area,rch_area)


# network analysis help: http://www.shizukalab.com/toolkits/sna/plotting-directed-networks
# use network analysis graph to automate subbasin contributions of runoff?

