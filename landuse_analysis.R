# yadkin land use analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(sf)
#library(ggpubr)
#library(gridExtra)
#library(tmap)
#vignette("tmap-nutshell")

# set directory and load watershed wide percent use
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers")

# baseline (1992)
baseline_lu=read_csv("yadlurec_1992.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% mutate(dataset="baseline")
  
# miroc 8.5 (2060)
miroc8_5_lu=read_csv("yadluA_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% mutate(dataset="miroc8_5")

# csiro 8.5 (2060)
csiro8_5_lu=read_csv("yadluB_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% mutate(dataset="csiro8_5")

# csiro 4.5 (2060)
csiro4_5_lu=read_csv("yadluC_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% mutate(dataset="csiro4_5")

# hadley 4.5 (2060)
hadley4_5_lu=read_csv("yadluD_2060.txt",col_names=TRUE) %>% 
  select(VALUE:AREA_PERC) %>%
  arrange(VALUE) %>% mutate(dataset="hadley4_5")


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
  arrange(SUB,VALUE) %>% mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

# csiro 8.5 (2060)
csiro8_5_lu_temp=read_csv("luB2060_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

# csiro 4.5 (2060)
csiro4_5_lu_temp=read_csv("luC2060_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

# hadley 4.5 (2060)
hadley4_5_lu_temp=read_csv("luD2060_allsubs.csv",col_names=TRUE) %>% 
  select(SUB:AREA_PERC) %>%
  arrange(SUB,VALUE) %>% mutate(sub_id=paste0("subid_",SUB,"_",VALUE)) %>%
  select(sub_id,AREA_PERC)

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


# ---- 2. reformatting data ----

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

# shape file (.shp)
# add SUB column to .shp file
yadkin_subs_shp=yadkin_subs_shp %>% mutate(SUB=Subbasin)
#glimpse(yadkin_subs_shp)


# ---- 3.1 plotting watershed wide data (all categories) ----

ggplot(yadlu_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  xlab("") +
  ylab("Area (%)") +
  theme_bw()


# ---- 3.2 plotting subbasin data (all categories) ----

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
sublu_reclass_data=sublu_reclass_data %>% select(SUB,AREA_PERC,DESCRIPTION,dataset) %>%
  mutate(sub_id=paste0("subid_",SUB,"_",DESCRIPTION)) %>%
  group_by(SUB,DESCRIPTION,dataset,sub_id) %>%
  summarize(AREA_PERC=sum(AREA_PERC))

# ---- 4.2 plotting watershed wide data (reclassified categories) ----

ggplot(yadlu_reclass_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  xlab("") +
  ylab("Area (%)") +
  scale_fill_manual(values=c("orange", "grey75", "forestgreen","green","blue")) +
  theme_bw()


# ---- 4.3 plotting subbasin data (reclassified categories) ----

ggplot(sublu_reclass_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  facet_wrap(~SUB,ncol=7,nrow=4) +
  xlab("") +
  ylab("Area (%)") +
  theme_bw() +
  scale_fill_manual(values=c("orange", "grey75", "forestgreen","green","blue")) +
  theme(axis.text.x=element_text(angle=90, hjust=1,vjust=0.5))


# ----- 5.1 function: find % diff betwn. baseline and projection landuses ----

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


# ----- 5.2 calculate % diff betw. baseline and projection landuses ----

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

# ag
csiro4_5_ag_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="csiro4_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, csiro4_5_ag_perc=perc_diff)
csiro8_5_ag_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="csiro8_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, csiro8_5_ag_perc=perc_diff)
miroc8_5_ag_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="miroc8_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, miroc8_5_ag_perc=perc_diff)
hadley4_5_ag_lu_diff=all_models_lu_diff %>% 
  filter(DESCRIPTION=="agriculture") %>% filter(dataset=="hadley4_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, hadley4_5_ag_perc=perc_diff)

# developed
csiro4_5_dev_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="developed") %>% filter(dataset=="csiro4_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, csiro4_5_dev_perc=perc_diff)
csiro8_5_dev_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="developed") %>% filter(dataset=="csiro8_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, csiro8_5_dev_perc=perc_diff)
miroc8_5_dev_lu_diff=all_models_lu_diff %>%
  filter(DESCRIPTION=="developed") %>% filter(dataset=="miroc8_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, miroc8_5_dev_perc=perc_diff)
hadley4_5_dev_lu_diff=all_models_lu_diff %>% 
  filter(DESCRIPTION=="developed") %>% filter(dataset=="hadley4_5") %>% 
  select(SUB,perc_diff) %>% transmute(SUB=SUB, hadley4_5_dev_perc=perc_diff)


# ---- 5.3 plot landuse differences on map ----

# add to shp file

# forest
yadkin_subs_shp_forest=left_join(yadkin_subs_shp,forest_projections,by="SUB")
#glimpse(yadkin_subs_shp_forest)

# ag
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro4_5_ag_lu_diff,by="SUB")
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_ag_lu_diff,by="SUB")
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_ag_lu_diff,by="SUB")
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_ag_lu_diff,by="SUB")

# developed
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro4_5_dev_lu_diff,by="SUB")
yadkin_subs_shp=left_join(yadkin_subs_shp,csiro8_5_dev_lu_diff,by="SUB")
yadkin_subs_shp=left_join(yadkin_subs_shp,miroc8_5_dev_lu_diff,by="SUB")
yadkin_subs_shp=left_join(yadkin_subs_shp,hadley4_5_dev_lu_diff,by="SUB")


# forest plot
ggplot(yadkin_subs_shp_forest,aes(fill=perc_diff)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_subs_shp_forest is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("% Change Forest") +
  theme_bw()


# ag plots
p5=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_ag_perc)) +
  scale_fill_gradient2(name="% Change Ag")


# developed plots
hist(yadkin_subs_shp$hadley4_5_devel_perc) # just to see
p9=ggplot(yadkin_subs_shp) +
  geom_sf(aes(fill=hadley4_5_devel_perc)) +
  scale_fill_gradient2(name="% Change Developed",limit=c(0,2000),na.value="darkblue")

# looks weird for developed...
blah=sublu_reclass_data %>% filter(DESCRIPTION=="developed") %>% filter(dataset=="hadley4_5" | dataset=="baseline")


# plot forest figures
setwd("/Users/ssaia/Desktop")
pdf("forest_lu_diff.pdf",width=11,height=8.5)
multiplot(p1, p2, p3, p4, cols=2)
dev.off()

# plot ag figures
setwd("/Users/ssaia/Desktop")
pdf("ag_lu_diff.pdf",width=11,height=8.5)

multiplot(p5, p6, p7, p8, cols=2)
dev.off()

# plot developed figures
setwd("/Users/ssaia/Desktop")
pdf("developed_lu_diff.pdf",width=11,height=8.5)
multiplot(p9, p10, p11, p12, cols=2)
dev.off()



# ---- x. function: multiplot ----
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



