# yadkin climate change cost analysis
# last updated 20180116

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)

# ---- 2.1 import data ----

# set working directory and import high flow and low flow outlier percent change data 
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
hiflow_change_data <- read_csv("hiflow_outlier_perc_change_data.csv", col_names = TRUE)
lowflow_change_data <- read_csv("lowflow_outlier_perc_change_data.csv", col_names = TRUE)
hiflow_10yr_change_data = read_csv("num_hiflow_change_10yr_calcs.csv", col_names = TRUE)

# set working directory and import Hsiang et al. 2017 damages data
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/raw_data/climate_damage_cost_2012/reformatted_data")
damage_data_raw <- read_csv("county_damages_by_sector_reformatted.csv", col_names = TRUE)

# set working directory and import tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_raw <- read_csv("sovibd_2014_scaling_allsubs.csv", col_names = TRUE)

# set working directory and import baseline data (for subbasin area calcs)
setwd("/Users/ssaia/Documents/sociohydro_project/analysis/results/r_outputs")
contributing_areas <- read_csv("subbasin_contributing_areas_data.csv")

# ---- 2.3 reformat data ----

# select only tract scaling data that is needed
yadkin_fips_data <- sovibd_scaling_raw %>%
  select(SUB, fips, sub_perc) %>%
  mutate(county_fips = as.numeric(str_sub(as.character(fips), 1, 5))) %>%
  select(-fips) %>%
  distinct()

# summarize high flow change data
hiflow_change_data_summary <- hiflow_change_data %>%
  select(SUB, dataset, minor_outlier_perc_change_per_yr) %>%
  na.omit() %>%
  group_by(SUB) %>%
  summarise(avg_minor_hiflow_outlier_perc_change_per_yr = mean(minor_outlier_perc_change_per_yr))

# summarize 10yr high flow change data
hiflow_10yr_change_data_summary <- hiflow_10yr_change_data %>%
  select(SUB, dataset, perc_change_per_yr) %>%
  na.omit() %>%
  group_by(SUB) %>%
  summarise(avg_perc_change_per_yr = mean(perc_change_per_yr))

# summarize low flow change data
lowflow_change_data_summary <- lowflow_change_data %>%
  select(SUB, dataset, minor_outlier_perc_change_per_yr) %>%
  na.omit() %>%
  group_by(SUB) %>%
  summarise(avg_minor_lowflow_outlier_perc_change_per_yr = mean(minor_outlier_perc_change_per_yr))

# summarize scalling data (one value per subbasin per county)
yadkin_fips_summary_data <- yadkin_fips_data %>%
  group_by(SUB, county_fips) %>%
  summarize(sum_sub_perc = round(sum(sub_perc), 3))

# select only damage data that is needed
damage_data <- damage_data_raw %>%
  select(county_name:avg_county_income_2012, total_damages_perc_county_income)

# use join to select out yadkin damage data and add high flow data
yadkin_damage_data_hiflows <- left_join(yadkin_fips_summary_data, damage_data,by = "county_fips") %>%
  mutate(wt_county_pop_2012 = county_pop_2012 * sum_sub_perc,
         wt_avg_county_income_2012 = avg_county_income_2012 * sum_sub_perc,
         wt_total_damages_perc_county_income = total_damages_perc_county_income * sum_sub_perc) %>%
  select(SUB, wt_county_pop_2012:wt_total_damages_perc_county_income) %>%
  group_by(SUB) %>%
  summarize(sub_scaled_county_pop_2012 = sum(wt_county_pop_2012),
            sub_scaled_avg_county_income_2012 = sum(wt_avg_county_income_2012),
            sub_scaled_total_damages_perc_county_income = sum(wt_total_damages_perc_county_income)) %>%
  left_join(contributing_areas, by = "SUB") %>%
  left_join(hiflow_change_data_summary, by = "SUB")

# use join to select out yadkin damage data and add 10yr high flow data
yadkin_damage_data_hiflows_10yr <- left_join(yadkin_fips_summary_data, damage_data,by = "county_fips") %>%
  mutate(wt_county_pop_2012 = county_pop_2012 * sum_sub_perc,
         wt_avg_county_income_2012 = avg_county_income_2012 * sum_sub_perc,
         wt_total_damages_perc_county_income = total_damages_perc_county_income * sum_sub_perc) %>%
  select(SUB, wt_county_pop_2012:wt_total_damages_perc_county_income) %>%
  group_by(SUB) %>%
  summarize(sub_scaled_county_pop_2012 = sum(wt_county_pop_2012),
            sub_scaled_avg_county_income_2012 = sum(wt_avg_county_income_2012),
            sub_scaled_total_damages_perc_county_income = sum(wt_total_damages_perc_county_income)) %>%
  left_join(contributing_areas, by = "SUB") %>%
  left_join(hiflow_10yr_change_data_summary, by = "SUB")

# use join to select out yadkin damage data and add low flow data
yadkin_damage_data_lowflows <- left_join(yadkin_fips_summary_data, damage_data,by = "county_fips") %>%
  mutate(wt_county_pop_2012 = county_pop_2012 * sum_sub_perc,
         wt_avg_county_income_2012 = avg_county_income_2012 * sum_sub_perc,
         wt_total_damages_perc_county_income = total_damages_perc_county_income * sum_sub_perc) %>%
  select(SUB, wt_county_pop_2012:wt_total_damages_perc_county_income) %>%
  group_by(SUB) %>%
  summarize(sub_scaled_county_pop_2012 = sum(wt_county_pop_2012),
            sub_scaled_avg_county_income_2012 = sum(wt_avg_county_income_2012),
            sub_scaled_total_damages_perc_county_income = sum(wt_total_damages_perc_county_income)) %>%
  left_join(contributing_areas, by = "SUB") %>%
  left_join(lowflow_change_data_summary, by = "SUB") %>%
  na.omit()


# ---- 3.1 compare damage data for US and Yadkin ----


# us data
mean(damage_data_raw$total_damages_perc_county_income)
sd(damage_data_raw$total_damages_perc_county_income)

# select yadkin data
yadkin_counties <- sovibd_scaling_raw %>%
  select(fips) %>%
  mutate(county_fips = as.integer(str_sub(fips, start = 1, end = 5))) %>%
  select(county_fips)

damage_data_yadkin <- damage_data_raw %>%
  right_join(yadkin_counties, by = "county_fips")

mean(damage_data_yadkin$total_damages_perc_county_income)
sd(damage_data_yadkin$total_damages_perc_county_income)

# ---- 3.1 check for statisical relationship ----

# check for normality in the response variable
hist(yadkin_damage_data_hiflows$sub_scaled_total_damages_perc_county_income)
qqnorm(yadkin_damage_data_hiflows$sub_scaled_total_damages_perc_county_income)
qqline(yadkin_damage_data_hiflows$sub_scaled_total_damages_perc_county_income)
# not too bad but not great

# linear model
damages_lm <- lm(sub_scaled_total_damages_perc_county_income ~ sub_scaled_avg_county_income_2012, data = yadkin_damage_data_hiflows)
summary(damages_lm)
# slope is sign. p = 0.00487 and R sqrd = 0.2388 (not well explained)

# plot to check
plot(sub_scaled_total_damages_perc_county_income ~ sub_scaled_avg_county_income_2012, 
     data = yadkin_damage_data_hiflows,
     pch = 16, xlim = c(28000, 42000), ylim = c(2.5, 10.5), 
     xlab = "Subbasin Scaled Total Damages (% County Income)", ylab = "Subbasin Scaled Per Person Income (2012)")
abline(a = 14.8693069 , b = -0.0002684, col = "red", lwd = 3)
mtext(expression('p-value = 0.004,  R'^2*' = 0.24'), side = 3)

# ---- 3.2 plot data ----

# high flows
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_projected_damages_hiflows.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(data=yadkin_damage_data_hiflows) +
  geom_point(aes(x = sub_scaled_avg_county_income_2012, y = sub_scaled_total_damages_perc_county_income, 
                 size = AREAkm2, color = avg_minor_hiflow_outlier_perc_change_per_yr), alpha = 0.80) +
  labs(x = "Subbasin Scaled Per Person Income", y = "Subbasin Scaled Total Damages (% County Income)",
       color = "Avg % Change in HOFs/yr", size = "Contributing Area (sq km)") +
  xlim(28000, 42000) +
  ylim(2.5, 10.5) +
  scale_color_continuous(low = "grey75", high = "black", limits = c(-2, 60)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))
dev.off()

# high flows 10yr
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_projected_damages_hiflows.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(data=yadkin_damage_data_hiflows_10yr) +
  geom_point(aes(x = sub_scaled_avg_county_income_2012, y = sub_scaled_total_damages_perc_county_income, 
                 size = AREAkm2, color = avg_perc_change_per_yr), alpha = 0.80) +
  labs(x = "Subbasin Scaled Per Person Income", y = "Subbasin Scaled Total Damages (% County Income)",
       color = "Avg % change # of 10yr flow days/yr", size = "Contributing Area (sq km)") +
  xlim(28000, 42000) +
  ylim(2.5, 10.5) +
  scale_color_continuous(low = "grey80", high = "black", limits = c(-2, 80)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))
dev.off()

# low flows
setwd("/Users/ssaia/Desktop")
cairo_pdf("yadkin_projected_damages_lowflows.pdf", width = 11, height = 8.5, pointsize = 18)
ggplot(data=yadkin_damage_data_lowflows) +
  geom_point(aes(x = sub_scaled_avg_county_income_2012, y = sub_scaled_total_damages_perc_county_income, 
                 size = AREAkm2, color = avg_minor_lowflow_outlier_perc_change_per_yr), alpha = 0.80) +
  labs(x = "Subbasin Scaled Per Person Income", y = "Subbasin Scaled Total Damages (% County Income)",
       color = "Avg % Change in LOFs/yr", size = "Contributing Area (sq km)") +
  xlim(28000, 42000) +
  ylim(2.5, 10.5) +
  scale_color_continuous(low = "grey75", high = "black", limits = c(-2, 60)) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18))
dev.off()
