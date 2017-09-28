# yadkin land cover analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(sf)

# set directory and load watershed wide percent cover
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


# set directory and load subbasin percent cover
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

# 2060 are missing some landcover classes so need to pad with zeros

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


# ---- plotting watershed wide data ----

ggplot(yadlu_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  xlab("") +
  ylab("Area (%)") +
  theme_bw()


# ---- plotting subbasin data

ggplot(sublu_data,aes(x=dataset,y=AREA_PERC,fill=DESCRIPTION)) +
  geom_col() +
  facet_wrap(~SUB,ncol=7,nrow=4) +
  xlab("") +
  ylab("Area (%)") +
  theme_bw() +
  theme(axis.text.x=element_text(angle=90, hjust=1))



