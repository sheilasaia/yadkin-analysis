# yadkin land use analysis

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(sf)
#library(ggpubr)
#library(gridExtra)
#library(tmap)
#vignette("tmap-nutshell")

# load home-made functions 
functions_path="/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path,"reformat_rch_file.R")) # reformat SWAT .rch file

# set directory and load watershed wide percent use
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers")

# baseline (1992)
baseline_lu=read_csv("yadlurec_1992.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% 
  mutate(dataset="baseline")
  
# miroc 8.5 (2060)
miroc8_5_lu=read_csv("yadluA_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% 
  mutate(dataset="miroc8_5")

# csiro 8.5 (2060)
csiro8_5_lu=read_csv("yadluB_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% 
  mutate(dataset="csiro8_5")

# csiro 4.5 (2060)
csiro4_5_lu=read_csv("yadluC_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% 
  mutate(dataset="csiro4_5")

# hadley 4.5 (2060)
hadley4_5_lu=read_csv("yadluD_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% 
  mutate(dataset="hadley4_5")


# set directory and load subbasin percent use
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/lu_area_calculations")

# baseline (1992)
baseline_lu_sub=read_csv("lu1992_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% 
  mutate(sub_id=paste0("subid_",SUB,"_",VALUE),dataset="baseline")

# miroc 8.5 (2060)
miroc8_5_lu_temp=read_csv("luA2060_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% 
  mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

# csiro 8.5 (2060)
csiro8_5_lu_temp=read_csv("luB2060_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% 
  mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

# csiro 4.5 (2060)
csiro4_5_lu_temp=read_csv("luC2060_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% 
  mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

# hadley 4.5 (2060)
hadley4_5_lu_temp=read_csv("luD2060_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% 
  mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

# contributing area data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/kelly_results/baseline82-08_daily")
baseline_rch_raw_data=read_table2("output.rch",col_names=FALSE,skip=9) # basline .rch file from SWAT

# join areas
blah=miroc_baseline_outlier_cutoff_rp %>%
  left_join(contributing_areas,by='RCH')

# gis data
# set directory and load county bounds (.shp file)
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_subs_shp_albers=read_sf("yadkin_subs_albers.shp",quiet=TRUE)
yadkin_subs_shp=read_sf("yadkin_subs_utm17N.shp",quiet=TRUE)

# just looking at gis data
yadkin_subs_shp_albers_geom=st_geometry(yadkin_subs_shp_albers)
attributes(yadkin_subs_shp_albers_geom) # there is no epsg code for this projection so maybe this is why it's plotting so slow?
yadkin_subs_shp_geom=st_geometry(yadkin_subs_shp)
attributes(yadkin_subs_shp_geom) # this has an epsg code!


# ---- 2 reformat data ----

# combine all watershed wide data
yadlu_data=bind_rows(baseline_lu,miroc8_5_lu,csiro8_5_lu,csiro4_5_lu,hadley4_5_lu)

# save description
num_cats=length(unique(baseline_lu_sub$VALUE))
num_subs=length(unique(baseline_lu_sub$SUB))
num_cats*num_subs # expected number of entries
yadlu_descriptions=yadlu_data[1:num_cats,] %>% select(VALUE,DESCRIPTION)

# save indexing b/c need to pad empty category area values with zeros
baseline_lu_sub_for_index=baseline_lu_sub %>% select(sub_id,SUB,VALUE)

# 2060 are missing some landuse classes so need to pad with zeros

# miroc 8.5 (2060)
miroc8_5_lu_sub=left_join(baseline_lu_sub_for_index,miroc8_5_lu_temp,by="sub_id") %>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(dataset="miroc8_5")

# csiro 8.5 (2060)
csiro8_5_lu_sub=left_join(baseline_lu_sub_for_index,csiro8_5_lu_temp,by="sub_id") %>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(dataset="csiro8_5")

# csiro 4.5 (2060)
csiro4_5_lu_sub=left_join(baseline_lu_sub_for_index,csiro4_5_lu_temp,by="sub_id") %>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(dataset="csiro4_5")

# hadley 4.5 (2060)
hadley4_5_lu_sub=left_join(baseline_lu_sub_for_index,hadley4_5_lu_temp,by="sub_id") %>%
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%
  mutate(dataset="hadley4_5")

# combine all subbasin data
sublu_data=bind_rows(baseline_lu_sub,
                     miroc8_5_lu_sub,
                     csiro8_5_lu_sub,
                     csiro4_5_lu_sub,
                     hadley4_5_lu_sub) %>%
  left_join(yadlu_descriptions,by="VALUE") # add description in

# contributing area data
baseline_rch_data=reformat_rch_file(baseline_rch_raw_data) %>% 
  filter(YR<2003)

# make dataframe with contributing errors to can use to plot
contributing_areas=baseline_rch_data %>% select(RCH,AREAkm2) %>%
  distinct() %>% 
  mutate(SUB=RCH) %>%
  select(-RCH)

# shape file (.shp)
# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)


# ---- 3.1 plot watershed wide data (all categories) ----

ggplot(yadlu_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  xlab("") +
  ylab("Area (%)") +
  theme_bw()


# ---- 3.2 plot subbasin data (all categories) ----

ggplot(sublu_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  facet_wrap(~SUB,ncol=7,nrow=4) +
  xlab("") +
  ylab("Area (%)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))


# ---- 4.1 condense categories and reset sub_id ----

# reclassify to simple categories
yadlu_reclass_data=yadlu_data
yadlu_reclass_data$DESCRIPTION[yadlu_reclass_data$DESCRIPTION=="lowland hardwood"]="forested"
yadlu_reclass_data$DESCRIPTION[yadlu_reclass_data$DESCRIPTION=="upland hardwood"]="forested"
yadlu_reclass_data$DESCRIPTION[yadlu_reclass_data$DESCRIPTION=="mixed forest"]="forested"
yadlu_reclass_data$DESCRIPTION[yadlu_reclass_data$DESCRIPTION=="pine"]="forested"
yadlu_reclass_data$DESCRIPTION[yadlu_reclass_data$DESCRIPTION=="nonstocked"]="grassland"
yadlu_reclass_data$DESCRIPTION[yadlu_reclass_data$DESCRIPTION=="wetland"]="wetlands_and_water"
yadlu_reclass_data$DESCRIPTION[yadlu_reclass_data$DESCRIPTION=="water"]="wetlands_and_water"

sublu_reclass_data=sublu_data
sublu_reclass_data$DESCRIPTION[sublu_reclass_data$DESCRIPTION=="lowland hardwood"]="forested"
sublu_reclass_data$DESCRIPTION[sublu_reclass_data$DESCRIPTION=="upland hardwood"]="forested"
sublu_reclass_data$DESCRIPTION[sublu_reclass_data$DESCRIPTION=="mixed forest"]="forested"
sublu_reclass_data$DESCRIPTION[sublu_reclass_data$DESCRIPTION=="pine"]="forested"
sublu_reclass_data$DESCRIPTION[sublu_reclass_data$DESCRIPTION=="nonstocked"]="grassland"
sublu_reclass_data$DESCRIPTION[sublu_reclass_data$DESCRIPTION=="wetland"]="wetlands_and_water"
sublu_reclass_data$DESCRIPTION[sublu_reclass_data$DESCRIPTION=="water"]="wetlands_and_water"

# reset sub_id in sublu_reclass_data (for % diff calc)
sublu_reclass_data=sublu_reclass_data %>% 
  select(SUB,AREA_PERC,DESCRIPTION,dataset) %>%
  mutate(sub_id=paste0("subid_",SUB,"_",DESCRIPTION)) %>%
  group_by(SUB,DESCRIPTION,dataset,sub_id) %>%
  summarize(AREA_PERC=sum(AREA_PERC))

# summarize baseline % landuse
yadlu_reclass_baseline_summary = yadlu_reclass_data %>% 
  filter(dataset == "baseline") %>% 
  group_by(DESCRIPTION) %>%
  summarize(sum_area_perc = sum(AREA_PERC))

# summarize baseline % landuse for each subbasin
sublu_reclass_baseline_summary = sublu_reclass_data %>% 
  filter(dataset == "baseline") %>%
  mutate(baseline_perc = AREA_PERC) %>%
  ungroup() %>%
  select(-AREA_PERC, -dataset)

# summarize projections % landuse for each subbasin
sublu_reclass_projection_summary = sublu_reclass_data %>% 
  filter(dataset != "baseline") %>% 
  left_join(contributing_areas, by = "SUB") %>%
  group_by(DESCRIPTION, SUB, AREAkm2) %>%
  summarize(projection_perc = round(mean(AREA_PERC), 3)) %>%
  ungroup() %>%
  mutate(sub_id = paste0("subid_", SUB, "_", DESCRIPTION)) %>%
  select(-DESCRIPTION, -SUB)
  

# ---- 4.2 plot watershed wide data (reclassified categories) ----

setwd("/Users/ssaia/Desktop")
cairo_pdf("lu_diff_watershedwide.pdf",width=11,height=8.5, pointsize=20)
ggplot(yadlu_reclass_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  xlab("") +
  ylab("Area (%)") +
  scale_fill_manual(values=c("orange", "grey75", "forestgreen","green","blue")) +
  theme_bw() +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  theme(text = element_text(size = 20)) +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))
dev.off()


# ---- 4.3 plot subbasin data (reclassified categories) ----

ggplot(sublu_reclass_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  facet_wrap(~SUB,ncol=7,nrow=4) +
  xlab("") +
  ylab("Area (%)") +
  theme_bw() +
  scale_fill_manual(values=c("orange", "grey75", "forestgreen","green","blue")) +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))



# ---- 4.4 barplot subbasin data by subbasin id for major landuses (summary) ----

blah = left_join(sublu_reclass_baseline_summary, sublu_reclass_projection_summary, by = "sub_id") %>%
  select(SUB, sub_id, AREAkm2, DESCRIPTION, baseline_perc, projection_perc) %>%
  gather(key = dataset, value = perc, baseline_perc:projection_perc) %>%
  filter(DESCRIPTION != "wetlands_and_water") %>% # some small changes but basically the same
  arrange(AREAkm2)

blah$SUB = factor(blah$SUB, levels = contributing_areas$SUB[order(contributing_areas$AREAkm2)])

setwd("/Users/ssaia/Desktop")
cairo_pdf("sublu_comparision.pdf",width=11,height=8.5, pointsize=20)
ggplot(blah,aes(x=as.factor(SUB),y=perc, fill = dataset)) +
  geom_col(position = "dodge") +
  facet_wrap(~DESCRIPTION) +
  xlab("Subbasin") +
  ylab("Area (%)") +
  ylim(0, 100) +
  scale_fill_manual(values=c("grey75", "black")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 14),
        axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))
dev.off()


# ---- 5.1 function: find % change in landuse betwn. baseline and projection ----

lu_diff=function(input_sublu_data) {
  
  # initialize empty data frame
  output_df=data.frame(SUB=as.integer(),
                       DESCRIPTION=as.character(),
                       perc_diff=as.numeric(),
                       dataset=as.character())
  
  # unique id's for for loop and other constants
  id_list=unique(input_sublu_data$sub_id)
  num_models=4
  
  for (i in 1:length(id_list)) {
    
    # select data
    sub_id_temp=input_sublu_data %>% filter(sub_id==id_list[i])
    
    # save percent area as temp variable
    baseline_temp=sub_id_temp$AREA_PERC[sub_id_temp$dataset=="baseline"]
    miroc8_5_temp=sub_id_temp$AREA_PERC[sub_id_temp$dataset=="miroc8_5"]
    csiro8_5_temp=sub_id_temp$AREA_PERC[sub_id_temp$dataset=="csiro8_5"]
    csiro4_5_temp=sub_id_temp$AREA_PERC[sub_id_temp$dataset=="csiro4_5"]
    hadley4_5_temp=sub_id_temp$AREA_PERC[sub_id_temp$dataset=="hadley4_5"]
    
    # calculate percent difference
    miroc8_5_diff=((miroc8_5_temp-baseline_temp)/baseline_temp)*100
    csiro8_5_diff=((csiro8_5_temp-baseline_temp)/baseline_temp)*100
    csiro4_5_diff=((csiro4_5_temp-baseline_temp)/baseline_temp)*100
    hadley4_5_diff=((hadley4_5_temp-baseline_temp)/baseline_temp)*100
    
    # save output
    output_temp=data.frame(SUB=rep(unique(sub_id_temp$SUB),num_models),
                           DESCRIPTION=rep(unique(sub_id_temp$DESCRIPTION),num_models),
                           perc_diff=c(miroc8_5_diff,csiro8_5_diff,csiro4_5_diff,hadley4_5_diff),
                           dataset=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
    
    # append to output
    output_df=bind_rows(output_df,output_temp)
  }
  return(output_df)
}


# ---- 5.2 calculate % change in landuse and reformat data ----

# calculate % difference between 1992 baseline and 2060 projections
all_models_lu_diff=lu_diff(sublu_reclass_data)

# forested
# select data to add to shp file
csiro4_5_forest_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="forested") %>% filter(dataset=="csiro4_5") %>% 
  select(SUB,dataset,perc_diff) 
csiro8_5_forest_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="forested") %>% filter(dataset=="csiro8_5") %>% 
  select(SUB,dataset,perc_diff) 
miroc8_5_forest_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="forested") %>% filter(dataset=="miroc8_5") %>% 
  select(SUB,dataset,perc_diff) 
hadley4_5_forest_lu_diff=all_models_lu_diff %>% 
  filter(DESCRIPTION=="forested") %>% filter(dataset=="hadley4_5") %>% 
  select(SUB,dataset,perc_diff)

# gather projections
forest_projections=bind_rows(csiro4_5_forest_lu_diff,
                             csiro8_5_forest_lu_diff,
                             miroc8_5_forest_lu_diff,
                             hadley4_5_forest_lu_diff)

# add to shp file
yadkin_subs_shp_forest=left_join(yadkin_subs_shp,forest_projections,by="SUB")
#glimpse(yadkin_subs_shp_forest)


# ag
# select data to add to shp file
csiro4_5_ag_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="csiro4_5") %>% 
  select(SUB,dataset,perc_diff)
csiro8_5_ag_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="csiro8_5") %>% 
  select(SUB,dataset,perc_diff)
miroc8_5_ag_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="miroc8_5") %>% 
  select(SUB,dataset,perc_diff)
hadley4_5_ag_lu_diff=all_models_lu_diff %>% 
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="hadley4_5") %>% 
  select(SUB,dataset,perc_diff)

# gather projections
ag_projections=bind_rows(csiro4_5_ag_lu_diff,
                         csiro8_5_ag_lu_diff,
                         miroc8_5_ag_lu_diff,
                         hadley4_5_ag_lu_diff)

# add to shp file
yadkin_subs_shp_ag=left_join(yadkin_subs_shp,ag_projections,by="SUB")
#glimpse(yadkin_subs_shp_ag)


# developed
csiro4_5_dev_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="developed") %>% filter(dataset=="csiro4_5") %>% 
  select(SUB,dataset,perc_diff)
csiro8_5_dev_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="developed") %>% filter(dataset=="csiro8_5") %>% 
  select(SUB,dataset,perc_diff)
miroc8_5_dev_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="developed") %>% filter(dataset=="miroc8_5") %>% 
  select(SUB,dataset,perc_diff)
hadley4_5_dev_lu_diff=all_models_lu_diff %>% 
  filter(DESCRIPTION=="developed") %>% filter(dataset=="hadley4_5") %>% 
  select(SUB,dataset,perc_diff)

# gather projections
dev_projections=bind_rows(csiro4_5_dev_lu_diff,
                          csiro8_5_dev_lu_diff,
                          miroc8_5_dev_lu_diff,
                          hadley4_5_dev_lu_diff)

# add to shp file
yadkin_subs_shp_dev=left_join(yadkin_subs_shp,dev_projections,by="SUB")
#glimpse(yadkin_subs_shp_dev)


# ---- 5.3 plot landuse differences on map ----

# plot and save figures
# forest
setwd("/Users/ssaia/Desktop")
cairo_pdf("lu_diff_forest.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_forest,aes(fill=perc_diff)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_forest is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change Forest") +
  theme_bw()
dev.off()

# ag
setwd("/Users/ssaia/Desktop")
cairo_pdf("lu_diff_ag.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_ag,aes(fill=perc_diff)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_ag is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change Ag") +
  theme_bw()
dev.off()

# developed
# replace high values with NA for plotting
hist(yadkin_subs_shp_dev$perc_diff)
cutoff_dev=2500
yadkin_subs_shp_dev=yadkin_subs_shp_dev %>% 
  mutate(perc_diff_na=replace(perc_diff,perc_diff>=cutoff_dev,NA))
hist(yadkin_subs_shp_dev$perc_diff_na)
#glimpse(yadkin_subs_shp_dev)

# plot
setwd("/Users/ssaia/Desktop")
cairo_pdf("lu_diff_dev.pdf",width=11,height=8.5)
ggplot(yadkin_subs_shp_dev,aes(fill=perc_diff_na)) +
  facet_wrap(~dataset) +
  geom_sf() +
  #lims(0,cutoff_dev+100)
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_dev is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change Developed",na.value="darkblue") +
  theme_bw()
dev.off()


# ---- 5.4 save plot and datatable (for jenni) ----

setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/write_up/files_for_jenni/lc_change")
st_write(yadkin_subs_shp_forest, "yadkin_forest_perc_change.shp")
st_write(yadkin_subs_shp_dev, "yadkin_developed_perc_change.shp")
write_csv(forest_projections, "yadkin_forest_perc_change.csv")
write_csv(dev_projections, "yadkin_developed_perc_change.csv")
write_csv(sublu_reclass_data, "yadkin_landcover_data.csv")

