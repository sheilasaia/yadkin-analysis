# yadkin sovi analysis (atsdr 2010-2014 tract level data)

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
# devtools::install_github("tidyverse/ggplot2") # sf requires newest ggplot2 version
# library(ggplot2)
library(sf)

# load home-made functions 
functions_path = "/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path, "multiplot.R")) # for creating plots with multiple figures

# atsdr sovi 2010-2014 tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_raw = read_csv("sovibd_2014_scaling_allsubs.csv", col_names = TRUE)
# don't need tract column?

# atsdr sovi 2010-2014 data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
us_sovi_data = read_csv("atsdr_us_sovi_2014_data.csv", col_names = TRUE)

# gis data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/")
yadkin_sub_shp_raw = read_sf("yadkin_subs_utm17N.shp", quiet = TRUE)
yadkin_tract_shp_raw = read_sf("yadkin_sovi2014_utm17N.shp", quiet = TRUE)
yadkin_unclip_tract_shp_raw = read_sf("yadkin_counties_sovi2014_utm17N.shp", quiet = TRUE)
yadkin_river_shp = read_sf("yadkin_majortribs_utm17N.shp", quiet = TRUE)
# use Albers projection for calcs in ArcGIS but UTM 17N
# here for plotting because can sf() recognizes UTM and
# then can convert sf() is not recognizing Albers projection

# looking at gis data
# yadkin_sub_shp_raw_geom = st_geometry(yadkin_sub_shp_raw)
# attributes(yadkin_sub_shp_raw) # this has an epsg code!


# ---- 2 reformat data -----

# remove X1 column in scaling data
sovidb_scaling_data = sovibd_scaling_raw %>% 
  select(-X1)

# make sure fips columns are same name and are as.numeric()
yadkin_tract_shp = yadkin_tract_shp_raw %>% 
  mutate(fips = as.numeric(FIPS)) %>% 
  select(-FIPS)

# census tracts in yadkin counties (not clipped to watershed bounds)
yadkin_unclip_tract_shp = yadkin_unclip_tract_shp_raw %>%
  mutate(fips = as.numeric(FIPS)) %>% 
  select(-FIPS)

# copy census tract data to new variable
yadkin_census_tract_data = yadkin_tract_shp %>% 
  select(fips, E_TOTPOP:E_DAYPOP)  %>% # select only columns you need 
  st_set_geometry(NULL) # set geometry as null to get df

# change subbasin id column to match other files
yadkin_sub_shp = yadkin_sub_shp_raw %>% 
  mutate(SUB = Subbasin) %>% 
  select(-Subbasin)

# select atsdr data for yadkin
yadkin_sovi_data = left_join(sovidb_scaling_data, us_sovi_data, by = "fips")


# ---- 3.1 combine us, nc, and yadkin tract sovi data for histogram ----

# find unique fips id's for yadkin
yadkin_unique_fips = sovidb_scaling_data %>% 
  select(fips) %>% distinct()

# find unique fips id's for nc
nc_unique_fips = us_sovi_data %>% 
  filter(state_abbrev == "NC") %>% 
  select(fips) %>% distinct()

# select sovi data for yadkin using unique fips id's
yadkin_sovi_hist = us_sovi_data %>% 
  select(fips, sovi_total) %>% 
  right_join(yadkin_unique_fips, by = "fips") %>% 
  mutate(dataset = "UYPD")

# select sovi data for nc using unique fips id's
nc_sovi_hist = us_sovi_data %>% select(fips, sovi_total) %>% 
  right_join(nc_unique_fips, by = "fips") %>% 
  mutate(dataset = "NC")

# select sovi data for us using unique fips id's
us_sovi_hist = us_sovi_data %>% select(fips, sovi_total) %>% 
  mutate(dataset = "US")

# calculate mean sovi for us and yadkin
mean_us_sovi = mean(us_sovi_hist$sovi_total)
sd_us_sovi = sd(us_sovi_hist$sovi_total)
mean_yadkin_sovi=mean(yadkin_sovi_hist$sovi_total)
sd_yadkin_sovi=sd(yadkin_sovi_hist$sovi_total)
 
# bind yadkin, nc, and us together
sovi_hist = bind_rows(yadkin_sovi_hist, nc_sovi_hist, us_sovi_hist)


# ---- 3.2 plot us, nc, yadkin tract sovi data as histogram ----

# setwd("/Users/ssaia/Desktop")
# cairo_pdf("atsdr_sovi2014_hist.pdf", width = 11, height = 8.5)
sovi_hist$dataset = factor(sovi_hist$dataset, levels = c("US", "NC", "UYPD"))
ggplot(sovi_hist, aes(sovi_total, fill = dataset, color = dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI (Census Tract Scale)") +
  ylab("Count") +
  xlim(0,15) +
  coord_cartesian(ylim=c(0,8000)) +
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())
# dev.off()

# zoom
setwd("/Users/ssaia/Desktop")
cairo_pdf("atsdr_sovi2014_hist_zoom.pdf",width=11,height=8.5)
sovi_hist$dataset=factor(sovi_hist$dataset,levels=c("US","NC","UYPD"))
ggplot(sovi_hist,aes(sovi_total,fill=dataset,color=dataset)) +
  geom_histogram(binwidth=.5, alpha=0.5) +
  xlab("SoVI (Census Tract Scale)") +
  ylab("Count") +
  xlim(0,15) +
  coord_cartesian(ylim=c(0,250)) + # allows you to zoom without cutting out data
  scale_fill_manual(values=c("grey30","grey75","white")) +
  scale_color_manual(values=c("black","black","black")) +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())
dev.off()


# ---- 3.3 plot yadkin total sovi by tract on map ----

# make a list to hold plots
my_total_sovi_plots = list()

# total sovi by tract
# setwd("/Users/ssaia/Desktop")
# cairo_pdf("yadkin_sovi2014_total_by_tract.pdf", width = 11, height = 8.5, pointsize = 18)
my_total_sovi_plots[[1]] = ggplot(yadkin_tract_shp, aes(fill = SPL_THEMES)) +
  geom_sf(color = "black") +
  coord_sf(crs = st_crs(102003)) + # yadkin_tract_shp is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Total SoVI", high = "darkred", low = "white", limits = c(0, 15)) +
  theme_bw()
# dev.off()


# ---- 4.1 gather four sovi themes together ----

yadkin_sovi_themes_tract_data = yadkin_census_tract_data %>%
  select(fips, SPL_THEME1, SPL_THEME2, SPL_THEME3, SPL_THEME4) %>%
  gather(key = "theme", value = "sovi", SPL_THEME1:SPL_THEME4)

# theme1 - socio-economic vulnerability
# theme2 - household composition and diability vulnerability
# theme3 - minority status and esl speakers vulnerability
# theme4 - housing and transportation vulnerability

yadkin_tract_shp_sovi_theme1to4=left_join(yadkin_tract_shp,yadkin_sovi_themes_tract_data,by="fips")
# glimpse(yadkin_tract_shp_sovi_theme1to4)


# ---- 4.2 plot four sovi themes by tract on map ----

# all four themes together in one plot
setwd("/Users/ssaia/Desktop")
#cairo_pdf("yadkin_sovi2014_theme1to4_by_tract.pdf",width=11,height=8.5,pointsize=18)
cairo_pdf("fig_s3.pdf",width=11,height=8.5,pointsize=18)
ggplot(yadkin_tract_shp_sovi_theme1to4,aes(fill=sovi)) +
  geom_sf(color = "grey10") +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred",low="white",limit=c(0,5)) +
  theme_bw()
dev.off()


# ---- 5.1 scale sovi data to subbasin ----

# scale to subbasin
# total sovi
yadkin_sovi_total_sub_data = yadkin_sovi_data %>%
  select(SUB, fips, tract_perc, sub_perc, sovi_total) %>%
  mutate(wt_sovi_total = round(sovi_total * sub_perc, 3)) %>%
  group_by(SUB) %>% 
  summarize(area_wt_sovi = sum(wt_sovi_total),
            median_sovi = median(sovi_total),
            min_sovi = min(sovi_total),
            max_sovi = max(sovi_total))

# sovi theme 1
yadkin_sovi_theme1_sub_data = yadkin_sovi_data %>%
  select(SUB, fips, tract_perc, sub_perc, sovi_theme1) %>%
  mutate(wt_sovi_theme1 = round(sovi_theme1 * sub_perc, 3)) %>%
  group_by(SUB) %>% 
  summarize(area_wt_sovi = sum(wt_sovi_theme1)) %>%
  mutate(theme = "theme1_socio_econ_demog")

# sovi theme 2
yadkin_sovi_theme2_sub_data = yadkin_sovi_data %>%
  select(SUB, fips, tract_perc, sub_perc, sovi_theme2) %>%
  mutate(wt_sovi_theme2 = round(sovi_theme2 * sub_perc, 3)) %>%
  group_by(SUB) %>% 
  summarize(area_wt_sovi = sum(wt_sovi_theme2)) %>%
  mutate(theme = "theme2_household_ability_demog")

# sovi theme 3
yadkin_sovi_theme3_sub_data = yadkin_sovi_data %>%
  select(SUB, fips, tract_perc, sub_perc, sovi_theme3) %>%
  mutate(wt_sovi_theme3 = round(sovi_theme3 * sub_perc, 3)) %>%
  group_by(SUB) %>% 
  summarize(area_wt_sovi = sum(wt_sovi_theme3)) %>%
  mutate(theme = "theme3_minority_esl_demog")

# sovi theme 4
yadkin_sovi_theme4_sub_data = yadkin_sovi_data %>%
  select(SUB, fips, tract_perc, sub_perc, sovi_theme4) %>%
  mutate(wt_sovi_theme4 = round(sovi_theme4 * sub_perc, 3)) %>%
  group_by(SUB) %>% 
  summarize(area_wt_sovi = sum(wt_sovi_theme4)) %>%
  mutate(theme = "theme4_housing_transportation_demog")

# themes 1 to 4 together
yadkin_sovi_themes_sub_data = bind_rows(yadkin_sovi_theme1_sub_data,
                                        yadkin_sovi_theme2_sub_data,
                                        yadkin_sovi_theme3_sub_data,
                                        yadkin_sovi_theme4_sub_data)

# join with gis data
yadkin_sub_shp_sovi_total=left_join(yadkin_sub_shp,yadkin_sovi_total_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme1=left_join(yadkin_sub_shp,yadkin_sovi_theme1_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme2=left_join(yadkin_sub_shp,yadkin_sovi_theme2_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme3=left_join(yadkin_sub_shp,yadkin_sovi_theme3_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme4=left_join(yadkin_sub_shp,yadkin_sovi_theme4_sub_data,by="SUB")
yadkin_sub_shp_sovi_theme1to4=left_join(yadkin_sub_shp,yadkin_sovi_themes_sub_data,by="SUB")
# glimpse(yadkin_sub_shp_sovi_total)
# glimpse(yadkin_sub_shp_sovi_theme1)
# glimpse(yadkin_sub_shp_sovi_theme2)
# glimpse(yadkin_sub_shp_sovi_theme3)
# glimpse(yadkin_sub_shp_sovi_theme4)
# glimpse(yadkin_sub_shp_sovi_theme1to4)


# ---- 5.2 plot subbasin scaled sovi data ----

# total sovi by sub
# setwd("/Users/ssaia/Desktop")
# cairo_pdf("yadkin_sovi2014_total_by_sub.pdf",width=11,height=8.5,pointsize=18)
my_total_sovi_plots[[2]] = ggplot(yadkin_sub_shp_sovi_total,aes(fill=area_wt_sovi)) +
  geom_sf(color = "black") +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_total is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Total SoVI",high="darkred",low="white",limits=c(0,15)) +
  theme_bw() #+
#  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#        text=element_text(size=16))
# dev.off()

# plot tract and subbasin scaled together
setwd("/Users/ssaia/Desktop")
cairo_pdf("fig_4.pdf", width = 16, height = 8, pointsize = 18)
multiplot(plotlist = my_total_sovi_plots, cols = 2)
dev.off()

# theme 1 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_theme1_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme1,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme1 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 1",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
#theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#      text=element_text(size=16))
dev.off()

# theme 2 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_theme2_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme2,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme2 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 2",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()

# theme 3 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_theme3_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme3,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme3 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 3",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()

# theme 4 sovi by sub
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sovi2014_theme4_by_sub.pdf",width=11,height=8.5)
ggplot(yadkin_sub_shp_sovi_theme4,aes(fill=area_wt_sovi)) +
  geom_sf() +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("Area Wtd SoVI Theme 3",na.value="grey75",high="darkred",low="white",limits=c(0,5)) +
  theme_bw() #+
# theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
#       text=element_text(size=16))
dev.off()

# all four themes together in one plot
setwd("/Users/ssaia/Desktop")
#cairo_pdf("yadkin_sovi2014_theme1to4_by_sub.pdf",width=11,height=8.5,pointsize=18)
cairo_pdf("fig_s3.pdf",width=11,height=8.5,pointsize=18)
ggplot(yadkin_sub_shp_sovi_theme1to4,aes(fill=area_wt_sovi)) +
  geom_sf(color = "black") +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_sovi_theme4 is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred",low="white",limit=c(0,5)) +
  theme_bw()
dev.off()


# ---- 6.1 import % change in NUMBER OF OUTLIER FLOWS (high and low) data ----

# set working directory and import data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
hiflow_outlier_change_data = read_csv("hiflow_outlier_perc_change_data.csv", col_names = TRUE)
# lowflow_outlier_change_data = read_csv("lowflow_outlier_perc_change_data.csv", col_names = TRUE)


# ---- 6.2 reclass hydrology and sovi data for plotting ----

# calculate mean sovi for us
mean_us_sovi = mean(us_sovi_hist$sovi_total)
median_us_sovi = median(us_sovi_hist$sovi_total)
sd_us_sovi = sd(us_sovi_hist$sovi_total)

mean_yadkin_sovi = mean(yadkin_sovi_hist$sovi_total)
median_yadkin_sovi = median(yadkin_sovi_hist$sovi_total)
sd_yadkin_sovi = sd(yadkin_sovi_hist$sovi_total)

# high flow data (hydro plus demographics)
hiflow_outlier_reclass_hydrodemo = hiflow_outlier_change_data %>%
  select(SUB, minor_outlier_perc_change_per_yr, dataset) %>%
  left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
  mutate(vuln_class = ifelse(area_wt_sovi <= mean_us_sovi + sd_us_sovi, 1, 
                             ifelse(area_wt_sovi > mean_us_sovi + sd_us_sovi & area_wt_sovi <= mean_us_sovi + 2 * sd_us_sovi, 2, 3))) %>%
  mutate(impact_class = ifelse(minor_outlier_perc_change_per_yr <= 25, 1, 
                               ifelse(minor_outlier_perc_change_per_yr > 25 & minor_outlier_perc_change_per_yr <= 50, 2, 3))) %>%
  mutate(impact_vuln_class_num = impact_class + vuln_class) %>%
  mutate(impact_vuln_class = ifelse(impact_vuln_class_num <= 2, "lower",
                                    ifelse(impact_vuln_class_num == 3 , "moderate", "higher")))

# high flow data and max sovi (hydro plus demographics)
hiflow_outlier_reclass_hydrodemo_max = hiflow_outlier_change_data %>%
  select(SUB, minor_outlier_perc_change_per_yr, dataset) %>%
  left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
  mutate(vuln_class = ifelse(max_sovi <= mean_us_sovi + sd_us_sovi, 1, 
                             ifelse(max_sovi > mean_us_sovi + sd_us_sovi & max_sovi <= mean_us_sovi + 2 * sd_us_sovi, 2, 3))) %>%
  mutate(impact_class = ifelse(minor_outlier_perc_change_per_yr <= 25, 1, 
                               ifelse(minor_outlier_perc_change_per_yr > 25 & minor_outlier_perc_change_per_yr <= 50, 2, 3))) %>%
  mutate(impact_vuln_class_num = impact_class + vuln_class) %>%
  mutate(impact_vuln_class = ifelse(impact_vuln_class_num <= 2, "lower",
                                    ifelse(impact_vuln_class_num == 3 , "moderate", "higher")))


# high flow data (hyrology only)
hiflow_outlier_reclass_hydro = hiflow_outlier_change_data %>%
  select(SUB, minor_outlier_perc_change_per_yr, dataset) %>%
  left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
  mutate(impact_class = ifelse(minor_outlier_perc_change_per_yr <= 25, "lower", 
                               ifelse(minor_outlier_perc_change_per_yr > 25 & minor_outlier_perc_change_per_yr <= 50, "moderate", "higher")))

# low flow data
# lowflow_outlier_change_data_sel = lowflow_outlier_change_data %>%
#   select(SUB:minor_outlier_perc_change_per_yr) %>% # select minor outliers from flow data 
#   left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
#   mutate(vuln_class = ifelse(area_wt_sovi > mean_us_sovi, "higher", "lower")) %>% # make new class variable based on us sovi mean
#   mutate(impact_class = ifelse(minor_outlier_perc_change_per_yr > 0, "higher", "lower")) %>% # make new class variable based on outlier flows
#   mutate(impact_vuln_class = ifelse(vuln_class == "higher" & impact_class == "higher", "higher",
#                                 ifelse(vuln_class == "lower" & impact_class == "lower", "lower",
#                                        ifelse(vuln_class == "lower" & impact_class == "higher", "moderate",
#                                               ifelse(vuln_class == "higher" & impact_class == "lower", "moderate", "NA"))))) # combine impact and vulnerability


# omit na's for plotting
hiflow_outlier_reclass_hydrodemo_naomit = hiflow_outlier_reclass_hydrodemo %>% na.omit()
hiflow_outlier_reclass_hydrodemo_max_naomit = hiflow_outlier_reclass_hydrodemo_max %>% na.omit()
hiflow_outlier_reclass_hydro_naomit = hiflow_outlier_reclass_hydro %>% na.omit()
# lowflow_outlier_change_data_sel_naomit = lowflow_outlier_change_data_sel %>% na.omit()

# define factor levels
hiflow_outlier_reclass_hydrodemo_naomit$impact_vuln_class = factor(hiflow_outlier_reclass_hydrodemo_naomit$impact_vuln_class, levels = c("higher", "moderate", "lower"))
hiflow_outlier_reclass_hydrodemo_naomit$dataset = factor(hiflow_outlier_reclass_hydrodemo_naomit$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
hiflow_outlier_reclass_hydrodemo_max_naomit$impact_vuln_class = factor(hiflow_outlier_reclass_hydrodemo_max_naomit$impact_vuln_class, levels = c("higher", "moderate", "lower"))
hiflow_outlier_reclass_hydrodemo_max_naomit$dataset = factor(hiflow_outlier_reclass_hydrodemo_max_naomit$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
hiflow_outlier_reclass_hydro_naomit$impact_class = factor(hiflow_outlier_reclass_hydro_naomit$impact_class, levels = c("higher", "moderate", "lower"))
hiflow_outlier_reclass_hydro_naomit$dataset = factor(hiflow_outlier_reclass_hydro_naomit$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
# lowflow_outlier_change_data_sel_naomit$impact_vuln_class = factor(lowflow_outlier_change_data_sel_naomit$impact_vuln_class, levels = c("higher", "moderate", "lower"))
# lowflow_outlier_change_data_sel_naomit$dataset = factor(lowflow_outlier_change_data_sel_naomit$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))


# ---- 6.3 plot on matrix ----

# make a list to hold plots
my_outlier_point_plots = list()

# high flow data (hydrology plus demographics)
my_matrix_plots[[2]] = ggplot(data = hiflow_outlier_reclass_hydrodemo_naomit,
       mapping = aes(x = area_wt_sovi, y = minor_outlier_perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  # annotate("text", x = 4, y = 55, label = "Hydrology+Demographics") +
  labs(x="Subbasin SoVI",y="% change/yr",
       color="Class",shape="Dataset") +
  xlim(0,15) +
  ylim(-10,150) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("darkblue", "steelblue3", "lightblue")) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))

# high flow data and max sovi (hydrology plus demographics)
my_outlier_point_plots[[2]] = ggplot(data = hiflow_outlier_reclass_hydrodemo_max_naomit,
                                     mapping = aes(x = max_sovi, y = minor_outlier_perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  #geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  annotate("text", x = 4, y = 55, label = "Hydrology+MaxSoVI") +
  labs(x="Max Census Tract SoVI in a Subbasin",y="% change/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-5,60) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("darkblue", "steelblue3", "lightblue")) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))


# high flow data (hydrology only)
my_outlier_point_plots[[3]] = ggplot(data = hiflow_outlier_reclass_hydro_naomit,
       mapping = aes(x = area_wt_sovi, y = minor_outlier_perc_change_per_yr, color = impact_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  #geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  annotate("text", x = 4, y = 55, label = "Hydrology") +  
  labs(x="Subbasin SoVI",y="% change/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-5,60) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("darkblue", "steelblue3", "lightblue")) +
  #scale_fill_manual(values=rep("black",4)) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))

# high flow data (comparison)
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_outlier_pointplot.pdf", width = 8.5, height = 15, pointsize = 18)
multiplot(plotlist = my_outlier_point_plots, cols = 1)
dev.off()

# # low flow data
# setwd("/Users/ssaia/Desktop")
# cairo_pdf("lowflow_impact_vuln_per_yr_pointplot.pdf",width=11,height=8.5,pointsize=18)
# ggplot(data=lowflow_outlier_change_data_sel_naomit,
#        mapping=aes(x=area_wt_sovi,y=minor_outlier_perc_change_per_yr,color=impact_vuln_class,shape=dataset)) +
#   geom_point(size=5,alpha=0.75) +
#   #geom_point(shape=1,size=3,color="black") +
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=mean_us_sovi) +
#   labs(x="Subbasin SoVI",y="% Change in Number of Minor LOFs/yr",
#        color="Class",shape="Dataset") +
#   xlim(0,14) +
#   ylim(-10,220) +
#   scale_shape_manual(values=c(15,16,17,18)) +
#   scale_color_manual(values=c("red","orange","gold")) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         panel.background=element_blank(),text=element_text(size=18))
# dev.off()
# 
# # zoom
# setwd("/Users/ssaia/Desktop")
# cairo_pdf("lowflow_impact_vuln_per_yr_pointplot_zoom.pdf",width=11,height=8.5)
# ggplot(data=lowflow_outlier_change_data_sel_naomit,
#        mapping=aes(x=area_wt_sovi,y=minor_outlier_perc_change_per_yr,color=impact_vuln_class,shape=dataset)) +
#   geom_point(size=4,alpha=0.75) +
#   #geom_point(shape=1,size=3,color="black") +
#   geom_hline(yintercept=0) +
#   geom_vline(xintercept=mean_us_sovi) +
#   labs(x="Subbasin SoVI",y="% Change in Number of Minor LOFs/yr",
#        color="Class",shape="Dataset") +
#   xlim(0,14) +
#   ylim(-10,100) +
#   scale_shape_manual(values=c(15,16,17,18)) +
#   scale_color_manual(values=c("black","grey50","grey75")) +
#   theme_bw() +
#   theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         panel.background=element_blank(),text=element_text(size=16))
# dev.off()


# ---- 6.4 plot on matrix with error bars ----


# make a list to hold plots
my_outlier_range_point_plots = list()

# high flow data (hydrology plus demographics, with min/max sovi for subbasin)
my_matrix_plots[[4]] = ggplot(data = hiflow_outlier_reclass_hydrodemo_naomit,
                                     mapping = aes(x = area_wt_sovi, y = minor_outlier_perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_errorbarh(aes(xmax = max_sovi, xmin = min_sovi, height = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  # annotate("text", x = 4, y = 55, label = "Hydrology+Demographics") +
  labs(x="Subbasin SoVI",y="% change/yr",
       color="Class",shape="Dataset") +
  xlim(0,15) +
  ylim(-10,150) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("darkblue", "steelblue3", "lightblue")) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))

# high flow data (hydrology plus demographics, with median sovi)
my_outlier_range_point_plots[[2]] = ggplot(data = hiflow_outlier_reclass_hydrodemo_naomit,
                                           mapping = aes(x = median_sovi, y = minor_outlier_perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  annotate("text", x = 4, y = 55, label = "Hydrology+Demographics") +
  labs(x="Median Subbasin SoVI",y="% change/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-5,60) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("darkblue", "steelblue3", "lightblue")) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))

setwd("/Users/ssaia/Desktop")
cairo_pdf("fig_s4.pdf", width = 18, height = 16, pointsize = 18)
multiplot(plotlist = my_matrix_plots, cols = 2)
dev.off()


# ---- 6.5 plot on map ----

# add to shp file
yadkin_sub_shp_hiflow_outlier_hydrodemo = left_join(yadkin_sub_shp, hiflow_outlier_reclass_hydrodemo, by = "SUB")
yadkin_sub_shp_hiflow_outlier_hydrodemo_max = left_join(yadkin_sub_shp, hiflow_outlier_reclass_hydrodemo_max, by = "SUB")
yadkin_sub_shp_hiflow_outlier_hydro = left_join(yadkin_sub_shp, hiflow_outlier_reclass_hydro, by = "SUB")
# yadkin_sub_shp_impact_vuln_lowflow = left_join(yadkin_sub_shp,lowflow_outlier_change_data_sel, by = "SUB")

# define factor levels
yadkin_sub_shp_hiflow_outlier_hydrodemo$impact_vuln_class = factor(yadkin_sub_shp_hiflow_outlier_hydrodemo$impact_vuln_class, levels = c("higher", "moderate", "lower"))
yadkin_sub_shp_hiflow_outlier_hydrodemo$dataset = factor(yadkin_sub_shp_hiflow_outlier_hydrodemo$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
yadkin_sub_shp_hiflow_outlier_hydrodemo_max$impact_vuln_class = factor(yadkin_sub_shp_hiflow_outlier_hydrodemo_max$impact_vuln_class, levels = c("higher", "moderate", "lower"))
yadkin_sub_shp_hiflow_outlier_hydrodemo_max$dataset = factor(yadkin_sub_shp_hiflow_outlier_hydrodemo_max$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
yadkin_sub_shp_hiflow_outlier_hydro$impact_class = factor(yadkin_sub_shp_hiflow_outlier_hydro$impact_class, levels = c("higher", "moderate", "lower"))
yadkin_sub_shp_hiflow_outlier_hydro$dataset = factor(yadkin_sub_shp_hiflow_outlier_hydro$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
# yadkin_sub_shp_impact_vuln_lowflow$impact_vuln_class=factor(yadkin_sub_shp_impact_vuln_lowflow$impact_vuln_class,levels=c("higher","moderate","lower"))
# yadkin_sub_shp_impact_vuln_lowflow$dataset=factor(yadkin_sub_shp_impact_vuln_lowflow$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# high flow data (hydrology plus demographics)
setwd("/Users/ssaia/Desktop")
cairo_pdf("fig_s5.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(yadkin_sub_shp_hiflow_outlier_hydrodemo, aes(fill = impact_vuln_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs = st_crs(102003)) + # yadkin_sub_shp_hiflow_outlier_hydrodemo is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values = c("darkblue", "steelblue3", "lightblue"), na.value = "grey75") +
  theme_bw()
dev.off()

# high flow data and max sovi (hydrology plus demographics)
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_outlier_impact_hydrodemo_max_map.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(yadkin_sub_shp_hiflow_outlier_hydrodemo_max, aes(fill = impact_vuln_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs = st_crs(102003)) + # yadkin_sub_shp_hiflow_outlier_hydrodemo is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values = c("darkblue", "steelblue3", "lightblue"), na.value = "grey75") +
  theme_bw()
dev.off()

# high flow data (hydrology)
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_outlier_impact_hydro_map.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(yadkin_sub_shp_hiflow_outlier_hydro, aes(fill = impact_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs = st_crs(102003)) + # yadkin_sub_shp_hiflow_outlier_hydro is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values = c("darkblue", "steelblue3", "lightblue"), na.value = "grey75") +
  theme_bw()
dev.off()

# low flow data
# setwd("/Users/ssaia/Desktop")
# cairo_pdf("lowflow_impact_vuln_per_yr_map.pdf",width=11,height=8.5,pointsize=18)
# ggplot(yadkin_sub_shp_impact_vuln_lowflow,aes(fill=impact_vuln_class)) +
#   facet_wrap(~dataset) +
#   geom_sf() +
#   coord_sf(crs=st_crs(102003)) + # yadkin_sub_shp_impact_vuln_lowflow is base utm 17N so convert to Albers for CONUS
#   scale_fill_manual(values=c("red","orange","gold"),na.value="grey75") +
#   theme_bw()
# dev.off()


# ---- 6.6 select subbasins ----

# select subbasins where max census tract scale sovi is greater than mean+2sd
hiflow_outlier_reclass_hydrodemo_sel = hiflow_outlier_reclass_hydrodemo %>%
  filter(max_sovi >= mean_us_sovi + 2 * sd_us_sovi & impact_vuln_class == "lower") %>%
  na.omit()

hiflow_outlier_reclass_hydrodemo_sel$impact_vuln_class = factor(hiflow_outlier_reclass_hydrodemo_sel$impact_vuln_class, levels = c("higher", "moderate", "lower"))
hiflow_outlier_reclass_hydrodemo_sel$dataset = factor(hiflow_outlier_reclass_hydrodemo_sel$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))

# plot as before with error bars
ggplot(data = hiflow_outlier_reclass_hydrodemo_sel,
       mapping = aes(x = area_wt_sovi, y = minor_outlier_perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_errorbarh(aes(xmax = max_sovi, xmin = min_sovi, height = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  annotate("text", x = 4, y = 55, label = "Hydrology+Demographics") +
  labs(x="Subbasin SoVI",y="% change/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-5,60) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("lightblue")) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))

# subbasins 8, 11, 14, 18 (just one model sim), and 20 all have vulnerable communities but are indicated
# in the "lower" class


# ---- 7.1 import % change in NUMBER OF FLOWS at/above a given return period data ----

# set working directory and import data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
hiflow_10yr_change_data = read_csv("num_hiflow_change_10yr_calcs.csv", col_names = TRUE)
# hiflow_25yr_change_data = read_csv("num_hiflow_change_25yr_calcs.csv", col_names = TRUE)
# lowflow_10yr_change_data = read_csv("num_lowflow_change_10yr_calcs.csv", col_names = TRUE)
# lowflow_25yr_change_data = read_csv("num_lowflow_change_25yr_calcs.csv", col_names = TRUE)

# ---- 7.2 reclass data for plotting ----

# 10yr hiflow data (hydrology + demographics)
hiflow_10yr_reclass_hydrodemo = hiflow_10yr_change_data %>%
  select(SUB, perc_change_per_yr, dataset) %>%
  left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
  mutate(vuln_class = ifelse(area_wt_sovi <= mean_us_sovi + sd_us_sovi, 1, 
                             ifelse(area_wt_sovi > mean_us_sovi + sd_us_sovi & area_wt_sovi <= mean_us_sovi + 2 * sd_us_sovi, 2, 3))) %>%
  mutate(impact_class = ifelse(perc_change_per_yr <= 25, 1, 
                               ifelse(perc_change_per_yr > 25 & perc_change_per_yr <= 50, 2, 3))) %>%
  mutate(impact_vuln_class_num = impact_class + vuln_class) %>%
  mutate(impact_vuln_class = ifelse(impact_vuln_class_num <= 2, "lower",
                                    ifelse(impact_vuln_class_num == 3 , "moderate", "higher")))

# 10yr high flow data (hydrology only)
hiflow_10yr_reclass_hydro = hiflow_10yr_change_data %>%
  select(SUB, perc_change_per_yr, dataset) %>%
  left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
  mutate(impact_class = ifelse(perc_change_per_yr <= 25, "lower", 
                               ifelse(perc_change_per_yr > 25 & perc_change_per_yr <= 50, "moderate", "higher")))

# 10yr lowflow data (hydro + demographics)
# lowflow_10yr_change_data_sel_2 = lowflow_10yr_change_data %>%
#   select(SUB, perc_change_per_yr, dataset) %>% 
#   left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
#   mutate(vuln_class = ifelse(area_wt_sovi <= mean_us_sovi + sd_us_sovi, 1, 
#                              ifelse(area_wt_sovi > mean_us_sovi & area_wt_sovi <= mean_us_sovi + 2 * sd_us_sovi, 2, 3))) %>%
#   mutate(impact_class = ifelse(perc_change_per_yr <= 25, 1, 
#                                ifelse(perc_change_per_yr > 25 & perc_change_per_yr <= 50, 2, 3))) %>%
#   mutate(impact_vuln_class_num = impact_class + vuln_class) %>%
#   mutate(impact_vuln_class = ifelse(impact_vuln_class_num <= 2, "lower",
#                                     ifelse(impact_vuln_class_num == 3 , "moderate", "higher")))

# omit na's for plotting
hiflow_10yr_reclass_hydrodemo_naomit = hiflow_10yr_reclass_hydrodemo %>% na.omit()
hiflow_10yr_reclass_hydro_naomit = hiflow_10yr_reclass_hydro %>% na.omit()
# lowflow_10yr_change_data_sel_2_naomit = lowflow_10yr_change_data_sel_2 %>% na.omit()

# define factor levels
hiflow_10yr_reclass_hydrodemo_naomit$impact_vuln_class=factor(hiflow_10yr_reclass_hydrodemo_naomit$impact_vuln_class,levels=c("higher","moderate","lower"))
hiflow_10yr_reclass_hydrodemo_naomit$dataset=factor(hiflow_10yr_reclass_hydrodemo_naomit$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
hiflow_10yr_reclass_hydro_naomit$impact_class=factor(hiflow_10yr_reclass_hydro_naomit$impact_class,levels=c("higher","moderate","lower"))
hiflow_10yr_reclass_hydro_naomit$dataset=factor(hiflow_10yr_reclass_hydro_naomit$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
# lowflow_10yr_change_data_sel_2_naomit$impact_vuln_class=factor(lowflow_10yr_change_data_sel_2_naomit$impact_vuln_class,levels=c("higher","moderate","lower"))
# lowflow_10yr_change_data_sel_2_naomit$dataset=factor(lowflow_10yr_change_data_sel_2_naomit$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# ---- 7.3 plot on matrix ----

# make a list to hold plots
my_matrix_plots = list()

# 10yr high flow data (hydrology plus demographics)
my_matrix_plots[[1]] = ggplot(data = hiflow_10yr_reclass_hydrodemo_naomit,
       mapping = aes(x = area_wt_sovi, y = perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  # annotate("text", x = 2, y = 150, label = "Hydrology+Demographics") +  
  labs(x = "Subbasin SoVI", y = "% change/yr", 
       color = "Class", shape = "Dataset") +
  xlim(0, 15) +
  ylim(-10, 150) +
  theme_bw() +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  # scale_color_manual(values = c("black", "grey50", "grey75")) +
  scale_color_manual(values = c("darkblue", "steelblue3", "lightblue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 18))

# 10yr high flow data (hydrology)
my_10yr_point_plots[[2]] = ggplot(data = hiflow_10yr_reclass_hydro_naomit,
                             mapping = aes(x = area_wt_sovi, y = perc_change_per_yr, color = impact_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  # geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  # geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  annotate("text", x = 4, y = 150, label = "Hydrology") + 
  labs(x = "Subbasin SoVI", y = "% change/yr", 
       color = "Class", shape = "Dataset") +
  xlim(0, 14) +
  ylim(-10, 150) +
  theme_bw() +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  # scale_color_manual(values = c("black", "grey50", "grey75")) +
  scale_color_manual(values = c("darkblue", "steelblue3", "lightblue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 18))

# 10yr lowflow data (more gradation)
# my_10yr_point_plots[[2]] = ggplot(data = lowflow_10yr_change_data_sel_2_naomit,
#        mapping = aes(x = area_wt_sovi, y = perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
#   geom_point(size = 5, alpha = 0.75) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_hline(yintercept = 25, linetype = "dashed") +
#   geom_hline(yintercept = 50, linetype = "dashed") +
#   geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
#   geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
#   geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
#   labs(x = "Subbasin SoVI", y = "% change in number of days/yr with flows <= 10-yr flow", 
#        color = "Class", shape = "Dataset") +
#   xlim(0, 14) +
#   ylim(-10, 150) +
#   theme_bw() +
#   scale_shape_manual(values = c(15, 16, 17, 18)) +
#   #scale_color_manual(values = c("black", "grey50", "grey75")) +
#   scale_color_manual(values = c("darkorange1","gold")) +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
#         panel.background = element_blank(), text = element_text(size = 18))

# plot together
setwd("/Users/ssaia/Desktop")
cairo_pdf("impact_vuln_per_yr_pointplot_10yr_more_gradation.pdf", width = 8.5, height = 15, pointsize = 18)
multiplot(plotlist = my_10yr_point_plots, cols = 1)
dev.off()


# ---- 7.4 plot on matrix with error bars ----

# make a list to hold plots
my_10yr_range_point_plots = list()

# 10yr high flow data (hydrology plus demographics)
my_matrix_plots[[3]] = ggplot(data = hiflow_10yr_reclass_hydrodemo_naomit,
                                  mapping = aes(x = area_wt_sovi, y = perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_errorbarh(aes(xmax = max_sovi, xmin = min_sovi, height = 0)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  # annotate("text", x = 2, y = 150, label = "Hydrology+Demographics") +  
  labs(x = "Subbasin SoVI", y = "% change/yr", 
       color = "Class", shape = "Dataset") +
  xlim(0, 15) +
  ylim(-10, 150) +
  theme_bw() +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  #scale_color_manual(values = c("black", "grey50", "grey75")) +
  scale_color_manual(values = c("darkblue", "steelblue3", "lightblue")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 18))

# ---- 7.5 plot on map ----

# add to shp file
yadkin_sub_shp_hiflow_10yr_hydrodemo = left_join(yadkin_sub_shp, hiflow_10yr_reclass_hydrodemo, by = "SUB")
yadkin_sub_shp_hiflow_10yr_hydro = left_join(yadkin_sub_shp, hiflow_10yr_reclass_hydro, by = "SUB")
# yadkin_sub_shp_impact_vuln_lowflow_10yr_2 = left_join(yadkin_sub_shp, lowflow_10yr_change_data_sel_2, by = "SUB")

# define factor levels
yadkin_sub_shp_hiflow_10yr_hydrodemo$impact_vuln_class = factor(yadkin_sub_shp_hiflow_10yr_hydrodemo$impact_vuln_class, levels = c("higher", "moderate", "lower"))
yadkin_sub_shp_hiflow_10yr_hydrodemo$dataset = factor(yadkin_sub_shp_hiflow_10yr_hydrodemo$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
yadkin_sub_shp_hiflow_10yr_hydro$impact_class = factor(yadkin_sub_shp_hiflow_10yr_hydro$impact_class, levels = c("higher", "moderate", "lower"))
yadkin_sub_shp_hiflow_10yr_hydro$dataset = factor(yadkin_sub_shp_hiflow_10yr_hydro$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))
# yadkin_sub_shp_impact_vuln_lowflow_10yr_2$impact_vuln_class = factor(yadkin_sub_shp_impact_vuln_lowflow_10yr_2$impact_vuln_class, levels = c("higher", "moderate", "lower"))
# yadkin_sub_shp_impact_vuln_lowflow_10yr_2$dataset = factor(yadkin_sub_shp_impact_vuln_lowflow_10yr_2$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))

# 10yr high flow data (hydrology plus demographics)
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_10yr_impact_hydrodemo_map.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(yadkin_sub_shp_hiflow_10yr_hydrodemo, aes(fill = impact_vuln_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs = st_crs(102003)) + # yadkin_sub_shp_hiflow_outlier_hydrodemo is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values = c("darkblue", "steelblue3", "lightblue"), na.value = "grey75") +
  theme_bw()
dev.off()

# 10yr high flow data (hydrology)
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_10yr_impact_hydro_map.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(yadkin_sub_shp_hiflow_10yr_hydro, aes(fill = impact_class)) +
  facet_wrap(~dataset) +
  geom_sf() +
  coord_sf(crs = st_crs(102003)) + # yadkin_sub_shp_hiflow_outlier_hydro is base utm 17N so convert to Albers for CONUS
  scale_fill_manual(values = c("darkblue", "steelblue3", "lightblue"), na.value = "grey75") +
  theme_bw()
dev.off()

# 10yr lowflow data (more gradation)
# setwd("/Users/ssaia/Desktop")
# cairo_pdf("impact_vuln_lowflow_10yr_per_yr_map_more_gradation.pdf", width = 11, height = 8.5, pointsize = 18)
# ggplot(yadkin_sub_shp_impact_vuln_lowflow_10yr_2, aes(fill = impact_vuln_class)) +
#   facet_wrap(~dataset) +
#   geom_sf() +
#   coord_sf(crs = st_crs(102003)) + # yadkin_sub_shp_impact_vuln_lowflow_10yr_2 is base utm 17N so convert to Albers for CONUS
#   scale_fill_manual(values = c("darkorange1","gold"), na.value = "grey75") +
#   theme_bw()
# dev.off()


# ---- 7.6 select subbasins ----

# select subbasins where max census tract scale sovi is greater than mean+2sd
hiflow_10yr_reclass_hydrodemo_sel = hiflow_10yr_reclass_hydrodemo %>%
  filter(max_sovi >= mean_us_sovi + 2 * sd_us_sovi & impact_vuln_class == "lower") %>%
  na.omit()

hiflow_10yr_reclass_hydrodemo_sel$impact_vuln_class = factor(hiflow_10yr_reclass_hydrodemo_sel$impact_vuln_class, levels = c("higher", "moderate", "lower"))
hiflow_10yr_reclass_hydrodemo_sel$dataset = factor(hiflow_10yr_reclass_hydrodemo_sel$dataset, levels = c("miroc8_5", "csiro8_5", "csiro4_5", "hadley4_5"))

# plot as before with error bars
ggplot(data = hiflow_10yr_reclass_hydrodemo_sel,
       mapping = aes(x = area_wt_sovi, y = perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_errorbarh(aes(xmax = max_sovi, xmin = min_sovi, height = 0)) +
  #geom_hline(yintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 50, linetype = "dashed") +
  geom_hline(yintercept = 25, linetype = "dashed") +
  #geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  annotate("text", x = 4, y = 150, label = "Hydrology+Demographics") +
  labs(x="Subbasin SoVI",y="% change/yr",
       color="Class",shape="Dataset") +
  xlim(0,14) +
  ylim(-5,150) +
  theme_bw() +
  scale_shape_manual(values=c(15,16,17,18)) +
  scale_color_manual(values=c("lightblue")) +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))

# subbasins 8, 11, 14, 18, and 20 all have vulnerable communities but are rated as being in the "lower" class




# ---- 7.7 export results ----

# export results
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
write_csv(hiflow_10yr_reclass_hydrodemo,"num_hiflow_change_10yr_with_sovi_calcs.csv")
write_csv(lowflow_10yr_change_data_sel_2,"num_lowflow_change_10yr_with_sovi_calcs.csv")

# ---- 8.1 reformat census data for pca analysis ----

# select only estimate columns
yadkin_tract_est_data = yadkin_census_tract_data %>% 
  select(fips, contains("E_")) %>%
  rename_all(tolower)

# take z-score of 15 variables
yadkin_tract_est_data_zscore = yadkin_tract_est_data %>%
  select(e_pov:e_groupq) %>%
  map(function(x) scale(x)) %>%
  as.data.frame()


# ---- 8.2 principle component analysis ----

# calculate principle components
sovi_pca=prcomp(yadkin_tract_est_data_zscore,scale=FALSE) #already scaled it
print(sovi_pca)
plot(sovi_pca, type = "l")
summary(sovi_pca)
# first two components only account for 57% of variance (to get over 95% we have to go to PC 11!)
sovi_pca_results=data.frame(var=names(yadkin_tract_est_data_zscore),pc1_loadings=sovi_pca$rotation[1:15],pc2_loadings=sovi_pca$rotation[16:30])
biplot(sovi_pca)

# take out e_pci and redo pca
yadkin_tract_est_data_zscore_no_income = yadkin_tract_est_data_zscore %>%
  select(-e_pci)

# calculate principle components
sovi_pca_no_income=prcomp(yadkin_tract_est_data_zscore_no_income,scale=FALSE) #already scaled it
print(sovi_pca_no_income)
plot(sovi_pca_no_income, type = "l")
summary(sovi_pca_no_income)
# first two components only account for 58% of variance (to get over 95% we have to go to PC 10!)
# this is slightly higher than the pca with income included but not much more
sovi_pca_results_no_income=data.frame(var=names(yadkin_tract_est_data_zscore_no_income),pc1_loadings=sovi_pca_no_income$rotation[1:14],pc2_loadings=sovi_pca_no_income$rotation[15:28])
biplot(sovi_pca_no_income)
# ---- 9.1 zoom in subbasin theme analysis ----

# reformat unclipped data
yadkin_unclip_tract_shp_sel = yadkin_unclip_tract_shp %>%
  select(fips, County = COUNTY, ST_ABBR, SPL_THEME1, SPL_THEME2, SPL_THEME3, SPL_THEME4, geometry) %>%
  gather.sf(key = "theme", value = "sovi", SPL_THEME1:SPL_THEME4)

# select 8
# select subbasin of interest
my_sub8 = 8
yadkin_sub8_shp = yadkin_sub_shp %>%
  filter(SUB == my_sub8)

# select tract sovi theme data for subbasin of interest
my_glimpse_sub8 = yadkin_sub8_shp %>%
  st_join(yadkin_unclip_tract_shp_sel)

# select river for area of interest
yadkin_river_sub8_shp = yadkin_sub8_shp %>%
  select(geometry) %>%
  st_intersection(yadkin_river_shp)

# look at counties that are included
unique(my_glimpse_sub8$County)
min(my_glimpse_sub8$sovi)

yadkin_tract_sub8_sovi_themes = yadkin_unclip_tract_shp_sel %>%
  filter(County == "Davidson" |
           County == "Forsyth" |
           County == "Stokes")

yadkin_sub8_shp_for_plot = yadkin_sub8_shp %>%
  mutate(SPL_THEME1 = 1, SPL_THEME2 = 1, SPL_THEME3 = 1, SPL_THEME4 = 1) %>%
  gather.sf(key = "theme", value = "sovi", SPL_THEME1:SPL_THEME4)

yadkin_river_sub8_shp_for_plot = yadkin_river_sub8_shp %>%
  mutate(SPL_THEME1 = 1, SPL_THEME2 = 1, SPL_THEME3 = 1, SPL_THEME4 = 1) %>%
  gather.sf(key = "theme", value = "sovi", SPL_THEME1:SPL_THEME4)
  
# plot 8
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sub8.pdf",width=10,height=10,pointsize=18)
ggplot() + 
  geom_sf(data = yadkin_tract_sub8_sovi_themes, aes(fill = sovi, color = County)) + 
  geom_sf(data = yadkin_river_sub8_shp, color = "blue", alpha = 0, size = 1) +
  geom_sf(data = yadkin_sub8_shp_for_plot, color = "black", alpha = 0, size = 1) +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred", low="white", limits = c(0,5)) +
  scale_color_manual(values=c("Davidson" = "#fc8d62", 
                              "Forsyth" = "#8da0cb", 
                              "Stokes" = "#66c2a5")) +   theme_bw()
dev.off()


# select 10 and 11
# select subbasin of interest
my_sub10and11 = 11
yadkin_sub10and11_shp = yadkin_sub_shp %>%
  filter(SUB == my_sub10and11 | SUB == 10)

# select tract sovi theme data for subbasin of interest
my_glimpse_sub10and11 = yadkin_sub10and11_shp %>%
  st_join(yadkin_unclip_tract_shp_sel)

# look at counties that are included
unique(my_glimpse_sub10and11$County)
min(my_glimpse_sub10and11$sovi)

# select river for area of interest
yadkin_river_sub10and11_shp = yadkin_sub10and11_shp %>%
  select(geometry) %>%
  st_intersection(yadkin_river_shp)

yadkin_tract_sub10and11_sovi_themes = yadkin_unclip_tract_shp_sel %>%
  filter(County == "Alexander" |
           County == "Davidson" |
           County == "Davie" |
           County == "Iredell" |
           County == "Rowan" |
           County == "Wilkes" |
           County == "Yadkin")

# plot 10 and 11
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sub10and11.pdf",width=10,height=10,pointsize=18)
ggplot() + 
  geom_sf(data = yadkin_tract_sub10and11_sovi_themes, aes(fill = sovi, color = County)) + 
  geom_sf(data = yadkin_river_sub10and11_shp, color = "blue", alpha = 0, size = 1) +
  geom_sf(data = yadkin_sub10and11_shp, color = "black", alpha = 0, size = 1) +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # yadkin_tract_sel_sovi_themes is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred", low="white", limits = c(0,5)) +
  scale_color_manual(values=c("Alexander" = "#66c2a5", 
                              "Davidson" = "#fc8d62", 
                              "Davie" = "#8da0cb",
                              "Iredell" = "#e78ac3",
                              "Rowan" = "#a6d854",
                              "Wilkes" = "#ffd92f",
                              "Yadkin" = "#e5c494")) +
    theme_bw()
dev.off()


# select 14
# select subbasin of interest
my_sub14 = 14
yadkin_sub14_shp = yadkin_sub_shp %>%
  filter(SUB == my_sub14)

# select tract sovi theme data for subbasin of interest
my_glimpse_sub14 = yadkin_sub14_shp %>%
  st_join(yadkin_unclip_tract_shp_sel)

# look at counties that are included
unique(my_glimpse_sub14$County)
min(my_glimpse_sub14$sovi)

# select river for area of interest
yadkin_river_sub14_shp = yadkin_sub14_shp %>%
  select(geometry) %>%
  st_intersection(yadkin_river_shp)

yadkin_tract_sub14_sovi_themes = yadkin_unclip_tract_shp_sel %>%
  filter(County == "Cabarrus" |
           County == "Davidson" |
           County == "Forsyth" |
           County == "Guilford" |
           County == "Randolph" |
           County == "Rowan")

# plot 14
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sub14.pdf",width=10,height=10,pointsize=18)
ggplot() + 
  geom_sf(data = yadkin_tract_sub14_sovi_themes, aes(fill = sovi, color = County)) + 
  geom_sf(data = yadkin_river_sub14_shp, color = "blue", alpha = 0, size = 1) +
  geom_sf(data = yadkin_sub14_shp, color = "black", alpha = 0, size = 1) +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred", low="white", limits = c(0,5)) +
  scale_color_manual(values=c("Cabarrus" = "#66c2a5", 
                              "Davidson" = "#fc8d62", 
                              "Forsyth" = "#8da0cb",
                              "Guilford" = "#e78ac3",
                              "Randolph" = "#ffd92f",
                              "Rowan" = "#a6d854")) +   theme_bw()
dev.off()


# select 18
# select subbasin of interest
my_sub18 = 18
yadkin_sub18_shp = yadkin_sub_shp %>%
  filter(SUB == my_sub18)

# select tract sovi theme data for subbasin of interest
my_glimpse_sub18 = yadkin_sub18_shp %>%
  st_join(yadkin_unclip_tract_shp_sel)

# look at counties that are included
unique(my_glimpse_sub18$County)
min(my_glimpse_sub18$sovi)

# select river for area of interest
yadkin_river_sub18_shp = yadkin_sub18_shp %>%
  select(geometry) %>%
  st_intersection(yadkin_river_shp)

yadkin_tract_sub18_sovi_themes = yadkin_unclip_tract_shp_sel %>%
  filter(County == "Davidson" |
           County == "Guilford" |
           County == "Montgomery" |
           County == "Randolph")

# plot 18
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sub18.pdf",width=10,height=10,pointsize=18)
ggplot() + 
  geom_sf(data = yadkin_tract_sub18_sovi_themes, aes(fill = sovi, color = County)) + 
  geom_sf(data = yadkin_river_sub18_shp, color = "blue", alpha = 0, size = 1) +
  geom_sf(data = yadkin_sub18_shp, color = "black", alpha = 0, size = 1) +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred", low="white", limits = c(0,5)) +
  scale_color_manual(values=c("Davidson" = "#fc8d62",
                              "Guilford" = "#e78ac3",
                              "Montgomery" = "#8da0cb", 
                              "Randolph" = "#ffd92f")) +
  theme_bw()
dev.off()


# select 20
# select subbasin of interest
my_sub20 = 20
yadkin_sub20_shp = yadkin_sub_shp %>%
  filter(SUB == my_sub20)

# select tract sovi theme data for subbasin of interest
my_glimpse_sub20 = yadkin_sub20_shp %>%
  st_join(yadkin_unclip_tract_shp_sel)

# look at counties that are included
unique(my_glimpse_sub20$County)
min(my_glimpse_sub20$sovi)

# select river for area of interest
yadkin_river_sub20_shp = yadkin_sub20_shp %>%
  select(geometry) %>%
  st_intersection(yadkin_river_shp)

yadkin_tract_sub20_sovi_themes = yadkin_unclip_tract_shp_sel %>%
  filter(County == "Anson" |
           County == "Cabarrus" |
           County == "Iredell" |
           County == "Mecklenburg" |
           County == "Rowan" |
           County == "Stanly" |
           County == "Union")

# plot 20
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sub20.pdf",width=10,height=10,pointsize=20)
ggplot() + 
  geom_sf(data = yadkin_tract_sub20_sovi_themes, aes(fill = sovi, color = County)) + 
  geom_sf(data = yadkin_river_sub20_shp, color = "blue", alpha = 0, size = 1) +
  geom_sf(data = yadkin_sub20_shp, color = "black", alpha = 0, size = 1) +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred", low="white", limits = c(0,5)) +
  scale_color_manual(values=c("Anson" = "#fc8d62",
                              "Cabarrus" = "#66c2a5",
                              "Iredell" = "#e78ac3",
                              "Mecklenburg" = "#8da0cb",
                              "Rowan" = "#a6d854",
                              "Stanly" = "#ffd92f",
                              "Union" = "#e5c494")) +
  theme_bw()
dev.off()


# select 25
# select subbasin of interest
my_sub25 = 25
yadkin_sub25_shp = yadkin_sub_shp %>%
  filter(SUB == my_sub25)

# select tract sovi theme data for subbasin of interest
my_glimpse_sub25 = yadkin_sub25_shp %>%
  st_join(yadkin_unclip_tract_shp_sel)

# look at counties that are included
unique(my_glimpse_sub25$County)
min(my_glimpse_sub25$sovi)

# select river for area of interest
yadkin_river_sub25_shp = yadkin_sub25_shp %>%
  select(geometry) %>%
  st_intersection(yadkin_river_shp)

yadkin_tract_sub25_sovi_themes = yadkin_unclip_tract_shp_sel %>%
  filter(County == "Montgomery" |
           County == "Randolph" |
           County == "Richmond")

# plot 25
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_sub25.pdf",width=10,height=10,pointsize=25)
ggplot() + 
  geom_sf(data = yadkin_tract_sub25_sovi_themes, aes(fill = sovi, color = County)) + 
  geom_sf(data = yadkin_river_sub25_shp, color = "blue", alpha = 0, size = 1) +
  geom_sf(data = yadkin_sub25_shp, color = "black", alpha = 0, size = 1) +
  facet_wrap(~theme) +
  coord_sf(crs=st_crs(102003)) + # is base utm 17N so convert to Albers for CONUS
  scale_fill_gradient2("SoVI",high="darkred", low="white", limits = c(0,5)) +
  scale_color_manual(values=c("Montgomery" = "#8da0cb",
                              "Randolph" = "#ffd92f",
                              "Richmond" = "#e78ac3")) +
  theme_bw()
dev.off()

#fc8d62
#8da0cb
#66c2a5
#e78ac3
#a6d854
#ffd92f
#e5c494


# colorblind friendly: http://colorbrewer2.org/#type=qualitative&scheme=Set2&n=3
# colorblind frieldly: http://bconnelly.net/2013/10/creating-colorblind-friendly-figures/



# ---- 9.2 cdf's of sovi by subbasin ----

# one sub, total sovi
cdf_calcs_total_sovi_subsel = yadkin_sovi_data %>% 
  filter(SUB == 1) %>%
  select(SUB, fips, sub_perc, sovi_total) %>%
  arrange(sovi_total) %>%
  mutate(sovi_total_wtd = sub_perc * sovi_total,
         cumul_sum_sovi_total = cumsum(sovi_total_wtd) / sum(sovi_total_wtd))

# plot
ggplot(data = cdf_calcs_total_sovi_subsel) +
  geom_point(aes(x = sovi_total, y = cumul_sum_sovi_total))

# all subs, total sovi
cdf_calcs_total_sovi = yadkin_sovi_data %>%
  select(SUB, fips, sub_perc, sovi_total) %>%
  arrange(SUB, sovi_total) %>%
  group_by(SUB) %>%
  mutate(sovi_total_wtd = sub_perc * sovi_total,
         cumul_sum_sovi_total = cumsum(sovi_total_wtd) / sum(sovi_total_wtd))

# plot
ggplot(data = cdf_calcs_total_sovi, aes(x = sovi_total, y = cumul_sum_sovi_total)) +
  #geom_smooth(se = FALSE) +
  geom_point() +
  facet_wrap(~SUB, ncol = 7) +
  xlab("SoVI") +
  ylab("CDF (Weighted)") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 10))

# all subs, sovi themes
cdf_calcs_sovi_themes = yadkin_sovi_data %>%
  select(SUB, fips, sub_perc, sovi_theme1, sovi_theme2, sovi_theme3, sovi_theme4) %>%
  group_by(SUB) %>%
  arrange(SUB, sovi_theme1) %>%
  mutate(sovi_theme1_wtd = sub_perc * sovi_theme1,
         cumul_sum_sovi_theme1 = cumsum(sovi_theme1_wtd) / sum(sovi_theme1_wtd)) %>%
  arrange(SUB, sovi_theme2) %>%
  mutate(sovi_theme2_wtd = sub_perc * sovi_theme2,
         cumul_sum_sovi_theme2 = cumsum(sovi_theme2_wtd) / sum(sovi_theme2_wtd)) %>%
  arrange(SUB, sovi_theme3) %>%
  mutate(sovi_theme3_wtd = sub_perc * sovi_theme3,
         cumul_sum_sovi_theme3 = cumsum(sovi_theme3_wtd) / sum(sovi_theme3_wtd)) %>%
  arrange(SUB, sovi_theme4) %>%
  mutate(sovi_theme4_wtd = sub_perc * sovi_theme4,
         cumul_sum_sovi_theme4 = cumsum(sovi_theme4_wtd) / sum(sovi_theme4_wtd)) %>%
  ungroup() %>%
  select(SUB, sovi_theme1:sovi_theme4, cumul_sum_sovi_theme1, cumul_sum_sovi_theme2, cumul_sum_sovi_theme3, cumul_sum_sovi_theme4) # %>%
  #gather(key = "key", value = "value", sovi_theme1:cumul_sum_sovi_theme4)
  
ggplot(data = cdf_calcs_sovi_themes) +
  geom_line(aes(x = sovi_theme1, y = cumul_sum_sovi_theme1), color = "green") +
  geom_line(aes(x = sovi_theme2, y = cumul_sum_sovi_theme2), color = "black") +
  geom_line(aes(x = sovi_theme3, y = cumul_sum_sovi_theme3), color = "blue") +
  geom_line(aes(x = sovi_theme4, y = cumul_sum_sovi_theme4), color = "red") +
  facet_wrap(~SUB, ncol = 7) +
  xlab("SoVI") +
  ylab("CDF (Weighted)") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 10))


blah2_sel = blah2 %>%
  filter(SUB == 8 | SUB == 11 | SUB ==14 | SUB == 18 |
           SUB == 20 | SUB == 25)
ggplot(data = blah2_sel) +
  geom_point(aes(x = sovi_total, y = cumul_sum_sovi_total)) +
  facet_wrap(~SUB) +
  xlab("SoVI") +
  ylab("CDF (Weighted)") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + sd_us_sovi, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi + 2 * sd_us_sovi, linetype = "dashed") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 10))
  
# do the same thing for the different themes
# do the same thing for subbasins (1) always in implement and (2) in watch but high (mean + 2sd) sovi


# ---- 7.x basic gradiation ----

# 10yr hiflow data (basic gradation)
hiflow_10yr_change_data_sel = hiflow_10yr_change_data %>%
  select(SUB, perc_change_per_yr, dataset) %>%  
  left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
  mutate(vuln_class = ifelse(area_wt_sovi > mean_us_sovi, "higher", "lower")) %>% # make new class variable based on us sovi mean
  mutate(impact_class = ifelse(perc_change_per_yr > 0, "higher", "lower")) %>% # make new class variable based on outlier flows
  mutate(impact_vuln_class = ifelse(vuln_class == "higher" & impact_class == "higher", "higher",
                                    ifelse(vuln_class == "lower" & impact_class == "lower", "lower",
                                           ifelse(vuln_class == "lower" & impact_class == "higher", "moderate",
                                                  ifelse(vuln_class == "higher" & impact_class == "lower", "moderate", "NA"))))) # combine impact and vulnerability
# 25yr hiflow data (basic)
hiflow_25yr_change_data_sel_2 = hiflow_25yr_change_data %>%
  select(SUB, perc_change_per_yr, dataset) %>% 
  left_join(yadkin_sovi_total_sub_data, by = "SUB") %>% # join area weighted sovi
  mutate(vuln_class = ifelse(area_wt_sovi > mean_us_sovi, "higher", "lower")) %>% # make new class variable based on us sovi mean
  mutate(impact_class = ifelse(perc_change_per_yr > 0, "higher", "lower")) %>% # make new class variable based on outlier flows
  mutate(impact_vuln_class = ifelse(vuln_class == "higher" & impact_class == "higher", "higher",
                                    ifelse(vuln_class == "lower" & impact_class == "lower", "lower",
                                           ifelse(vuln_class == "lower" & impact_class == "higher", "moderate",
                                                  ifelse(vuln_class == "higher" & impact_class == "lower", "moderate", "NA"))))) # combine impact and vulnerability


# ---- 7.x plot basic gradiation ----

# omit na's for plotting
hiflow_10yr_change_data_sel_naomit = hiflow_10yr_change_data_sel %>% na.omit()
hiflow_25yr_change_data_sel_naomit = hiflow_25yr_change_data_sel %>% na.omit()

# define factor levels
hiflow_10yr_change_data_sel_naomit$impact_vuln_class=factor(hiflow_10yr_change_data_sel_naomit$impact_vuln_class,levels=c("higher","moderate","lower"))
hiflow_10yr_change_data_sel_naomit$dataset=factor(hiflow_10yr_change_data_sel_naomit$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))
hiflow_25yr_change_data_sel_naomit$impact_vuln_class=factor(hiflow_25yr_change_data_sel_naomit$impact_vuln_class,levels=c("higher","moderate","lower"))
hiflow_25yr_change_data_sel_naomit$dataset=factor(hiflow_25yr_change_data_sel_naomit$dataset,levels=c("miroc8_5","csiro8_5","csiro4_5","hadley4_5"))

# 10yr hiflow data
setwd("/Users/ssaia/Desktop")
cairo_pdf("hiflow_impact_vuln_per_yr_pointplot_10yr.pdf",width=11,height=8.5,pointsize=18)
ggplot(data = hiflow_10yr_change_data_sel_naomit,
       mapping = aes(x = area_wt_sovi, y = perc_change_per_yr, color = impact_vuln_class, shape = dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  labs(x = "Subbasin SoVI", y = "% Change in Number of Flows/Yr >= 10yr Flow", 
       color = "Class", shape = "Dataset") +
  xlim(0, 14) +
  ylim(-10, 150) +
  theme_bw() +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  #scale_color_manual(values = c("black", "grey50", "grey75")) +
  scale_color_manual(values = c("red", "orange", "gold")) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 18))
dev.off()

# 25yr hiflow data
ggplot(data = hiflow_25yr_change_data_sel_naomit,
       mapping = aes(x = area_wt_sovi, y = perc_change_per_yr,color=impact_vuln_class,shape=dataset)) +
  geom_point(size = 5, alpha = 0.75) +
  #geom_point(shape=1,size=3,color="black") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = mean_us_sovi, linetype = "dashed") +
  labs(x = "Subbasin SoVI", y = "% Change in Number of Flows/Yr >= 25yr Flow",
       color = "Class", shape = "Dataset") +
  xlim(0, 14) +
  ylim(-10, 100) +
  scale_shape_manual(values = c(15, 16, 17, 18)) +
  #scale_color_manual(values = c("black", "grey50", "grey75")) +
  scale_color_manual(values = c("red", "orange", "gold")) +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), text = element_text(size = 18))


# ---- 7.x ellipse reclassify ----

# reclassify using an ellipse
blah = hiflow_outlier_change_data %>%
  select(SUB, dataset, minor_outlier_perc_change_per_yr) %>% # select minor outliers from flow data 
  left_join(yadkin_sovi_total_sub_data,by="SUB")

a = max(na.omit(blah$minor_outlier_perc_change_per_yr)) # only care about above zero
b = 15 # max sovi possible
# a has to be greater than b for vertical ellipse (test for this)
ymax = a
ymin = min(na.omit(blah$minor_outlier_perc_change_per_yr))
y2 = ymax/2
y1 = y2/2
y3 = y1 + y2
aseq = c(abs(ymin), y1 + abs(ymin), y2 + abs(ymin), y3 + abs(ymin), ymax + abs(ymin))
xmin = 0
x1 = mean_us_sovi + sd_us_sovi
x2 = mean_us_sovi + 2 * sd_us_sovi
x3 = mean_us_sovi + 3 * sd_us_sovi
xmax = b
bseq = c(mean_us_sovi, x1, x2, x3, xmax)

# five vertical ellipses
for (i in 1:5) {
  
  
  
}

testx = seq(0, bseq[1], length.out = 100)
testy = sqrt(aseq[1]^2 * (1 - (testx^2 / bseq[1]^2))) + ymin

testx2 = seq(0, bseq[2], length.out = 100)
testy2 = sqrt(aseq[2]^2 * (1 - (testx2^2 / bseq[2]^2))) + ymin

testx3 = seq(0, bseq[3], length.out = 100)
testy3 = sqrt(aseq[3]^2 * (1 - (testx3^2 / bseq[3]^2))) + ymin

testx4 = seq(0, bseq[4], length.out = 100)
testy4 = sqrt(aseq[4]^2 * (1 - (testx4^2 / bseq[4]^2))) + ymin

testx5 = seq(0, bseq[5], length.out = 100)
testy5 = sqrt(aseq[5]^2 * (1 - (testx5^2 / bseq[5]^2))) + ymin

plot(minor_outlier_perc_change_per_yr ~ area_wt_sovi, data = blah, pch = 16, xlim = c(0,15))
lines(testx, testy)
lines(testx2, testy2)
lines(testx3, testy3)
lines(testx4, testy4)
lines(testx5, testy5)


# ---- x.x extra ----

# high flow data
hiflow_outlier_change_data_sel=hiflow_outlier_change_data %>%
  select(SUB:minor_outlier_perc_change_per_yr) %>% # select minor outliers from flow data 
  left_join(yadkin_sovi_total_sub_data,by="SUB") %>% # join area weighted sovi
  mutate(vuln_class=ifelse(area_wt_sovi>mean_us_sovi,"higher","lower")) %>% # make new class variable based on us sovi mean
  mutate(impact_class=ifelse(minor_outlier_perc_change_per_yr>0,"higher","lower")) %>% # make new class variable based on outlier flows
  mutate(impact_vuln_class=ifelse(vuln_class=="higher"&impact_class=="higher","higher",
                                  ifelse(vuln_class=="lower"&impact_class=="lower","lower",
                                         ifelse(vuln_class=="lower"&impact_class=="higher","moderate",
                                                ifelse(vuln_class=="higher"&impact_class=="lower","moderate","NA"))))) # combine impact and vulnerability

# ---- 9.x subbasin sovi distributions ----

blah = yadkin_sovi_data %>%
  select(SUB, fips, tract_perc, sub_perc, sovi_total) %>%
  mutate(sovi_total_int = round(sovi_total,0))

ggplot(data = blah) +
  geom_bar(aes(x = as.factor(sovi_total_int), y = sub_perc), stat = "identity") +
  facet_wrap(~SUB, ncol = 7, nrow = 4) +
  xlab("SoVI Bin") +
  ylab("Percent of Subbasin") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), text = element_text(size = 12))