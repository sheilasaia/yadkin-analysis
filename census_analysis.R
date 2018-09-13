# yadkin census tract analysis

# ---- 1. set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(tidycensus)

# load home-made functions 
functions_path = "/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path, "multiplot.R")) # for creating plots with multiple figures

# atsdr sovi 2010-2014 tract scaling data
setwd("/Users/ssaia/Documents/ArcGIS/yadkin_arcgis_analysis_albers/sovibd_scaling_calculations")
sovibd_scaling_data = read_csv("sovibd_2014_scaling_allsubs.csv", col_names = TRUE) %>%
  mutate(fips = as.character(fips)) %>%
  select(SUB, fips, tract_perc, sub_perc)

# unique census tracts
yadkin_tracts <- data.frame(fips = unique(sovibd_scaling_raw$fips))

# load tidycensus
#census_api_key("YOUR API KEY GOES HERE")

# load lookup table
acs_variables <- load_variables(2014, "acs5", cache = TRUE)

# download
acs_end_years <- seq(2009, 2014, 1)
#purrr::map()?
nc_pop_2005to2009 <- get_acs(geography = "tract", year = 2009, survey = "acs5", variables = "B01003_001", state = "NC") %>%
  mutate(acs5_end_year = "2009") # this isn't as long as the others...
nc_pop_2006to2010 <- get_acs(geography = "tract", year = 2010, survey = "acs5", variables = "B01003_001", state = "NC") %>%
  mutate(acs5_end_year = "2010")
nc_pop_2007to2011 <- get_acs(geography = "tract", year = 2011, survey = "acs5", variables = "B01003_001", state = "NC") %>%
  mutate(acs5_end_year = "2011")
nc_pop_2008to2012 <- get_acs(geography = "tract", year = 2012, survey = "acs5", variables = "B01003_001", state = "NC") %>%
  mutate(acs5_end_year = "2012")
nc_pop_2009to2013 <- get_acs(geography = "tract", year = 2013, survey = "acs5", variables = "B01003_001", state = "NC") %>%
  mutate(acs5_end_year = "2013")
nc_pop_2010to2014 <- get_acs(geography = "tract", year = 2014, survey = "acs5", variables = "B01003_001", state = "NC") %>%
  mutate(acs5_end_year = "2014")
#wv_pop_2010to2014 <- get_acs(geography = "tract", year = 2014, survey = "acs5", variables = "B01003_001", state = "WV")
#sc_pop_2010to2014 <- get_acs(geography = "tract", year = 2014, survey = "acs5", variables = "B01003_001", state = "SC")

# merge and reformat
nc_pop <- rbind(nc_pop_2006to2010, # leave out 2009 b/c it wasn't missing data
                nc_pop_2007to2011,
                nc_pop_2008to2012,
                nc_pop_2009to2013,
                nc_pop_2010to2014) %>%
  mutate(county_fips = str_sub(GEOID, 1, -5)) %>%
  select(fips = GEOID, county_fips, totpop_est = estimate, totpop_moe = moe, acs5_end_year, description = NAME)

yadkin_nc_pop <- nc_pop %>%
  left_join(sovibd_scaling_data, by = "fips") %>%
  na.omit()

# source for aggregating moe using sqrt(sum(totpop_moe^2)))
# https://www.census.gov/content/dam/Census/library/publications/2018/acs/acs_general_handbook_2018_ch08.pdf
# also acs handbook says moe represents 90% CI

# ---- 2. forsyth county/subbasin 8 ----

# make a list to hold plots
my_pop_plots = list()

# select sub 8 (for winston-salem)
sub8_pop <- yadkin_nc_pop %>%
  filter(SUB == 8)

# select forsyth county (for winston-salem)
forsyth_pop <- nc_pop %>%
  filter(county_fips == "3706700")
  
# sum for each year
forsyth_pop_summary <- forsyth_pop %>%
  group_by(acs5_end_year) %>%
  summarize(agg_totpop_thous_est = sum(totpop_est)/1000,
            agg_totpop_thous_moe = sqrt(sum((totpop_moe/1000)^2))) %>%
  mutate(lower_thous_90ci = agg_totpop_thous_est - agg_totpop_thous_moe,
         upper_thous_90ci = agg_totpop_thous_est + agg_totpop_thous_moe,
         county_name = "forsyth") %>%
  ungroup()

# plot
my_pop_plots[[3]] = ggplot(data = forsyth_pop_summary, aes(x = acs5_end_year, y = agg_totpop_thous_est)) +
  geom_errorbar(aes(ymin = lower_thous_90ci, ymax = upper_thous_90ci), width = 0.2, color = "grey60") +
  geom_point(size = 3) +
  xlab("5-year ACS End Year") + 
  ylab("Population (thousands)") +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())


# ---- 3. mecklenburg County/subbasin 20 ----

# select sub 20 (for winston-salem)
sub20_pop <- yadkin_nc_pop %>%
  filter(SUB == 20)

# select mecklenburg county (for charolette)
meck_pop <- nc_pop %>%
  filter(county_fips == "3711900")

# sum for each year
meck_pop_summary <- meck_pop %>%
  group_by(acs5_end_year) %>%
  summarize(agg_totpop_thous_est = sum(totpop_est)/1000,
            agg_totpop_thous_moe = sqrt(sum((totpop_moe/1000)^2))) %>%
  mutate(lower_thous_90ci = agg_totpop_thous_est - agg_totpop_thous_moe,
         upper_thous_90ci = agg_totpop_thous_est + agg_totpop_thous_moe,
         county_name = "mecklenburg") %>%
  ungroup()

# plot
my_pop_plots[[1]] = ggplot(data = meck_pop_summary, aes(x = acs5_end_year, y = agg_totpop_thous_est)) +
  geom_errorbar(aes(ymin = lower_thous_90ci, ymax = upper_thous_90ci), width = 0.2, color = "grey60") +
  geom_point(size = 3) +
  xlab("5-year ACS End Year") + 
  ylab("Population (thousands)") +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())


# ---- 4. richmond county/sub 27 ----

# select sub 27 (for winston-salem)
sub27_pop <- yadkin_nc_pop %>%
  filter(SUB == 27)

# select richmond county (for elerbee and norman)
richmond_pop <- nc_pop %>%
  filter(county_fips == "3715397")

# sum for each year
richmond_pop_summary <- richmond_pop %>%
  group_by(acs5_end_year) %>%
  summarize(agg_totpop_thous_est = sum(totpop_est)/1000,
            agg_totpop_thous_moe = sqrt(sum((totpop_moe/1000)^2))) %>%
  mutate(lower_thous_90ci = agg_totpop_thous_est - agg_totpop_thous_moe,
         upper_thous_90ci = agg_totpop_thous_est + agg_totpop_thous_moe,
         county_name = "richmond") %>%
  ungroup()

# plot
my_pop_plots[[2]] = ggplot(data = richmond_pop_summary, aes(x = acs5_end_year, y = agg_totpop_thous_est)) +
  geom_errorbar(aes(ymin = lower_thous_90ci, ymax = upper_thous_90ci), width = 0.2, color = "grey60") +
  geom_point(size = 3) +
  xlab("5-year ACS End Year") + 
  ylab("Population (thousands)") +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())


# ---- 5. union county/sub 24 ----

# select sub 27 (for winston-salem)
sub24_pop <- yadkin_nc_pop %>%
  filter(SUB == 24)

# select richmond county (for monroe)
union_pop <- nc_pop %>%
  filter(county_fips == "3717902")

# sum for each year
union_pop_summary <- union_pop %>%
  group_by(acs5_end_year) %>%
  summarize(agg_totpop_thous_est = sum(totpop_est)/1000,
            agg_totpop_thous_moe = sqrt(sum((totpop_moe/1000)^2))) %>%
  mutate(lower_thous_90ci = agg_totpop_thous_est - agg_totpop_thous_moe,
         upper_thous_90ci = agg_totpop_thous_est + agg_totpop_thous_moe,
         county_name = "union") %>%
  ungroup()

# plot
my_pop_plots[[4]] = ggplot(data = union_pop_summary, aes(x = acs5_end_year, y = agg_totpop_thous_est)) +
  geom_errorbar(aes(ymin = lower_thous_90ci, ymax = upper_thous_90ci), width = 0.2, color = "grey60") +
  geom_point(size = 3) +
  xlab("5-year ACS End Year") + 
  ylab("Population (thousands)") +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())


# ---- 6. plot together ----

# plot tract and subbasin scaled together
setwd("/Users/ssaia/Desktop")
cairo_pdf("fig_pop.pdf", width = 10, height = 10, pointsize = 16)
multiplot(plotlist = my_pop_plots, cols = 2)
dev.off()

# merge all
pop_summary <- rbind(forsyth_pop_summary, meck_pop_summary, richmond_pop_summary, union_pop_summary)

# plot merged pop summary
ggplot(data = pop_summary, aes(x = acs5_end_year, y = agg_totpop_thous_est, color = county_name)) +
  geom_errorbar(aes(ymin = lower_thous_90ci, ymax = upper_thous_90ci), width = 0.2, color = "grey60") +
  geom_point() +
  xlab("5-year ACS End Year") + 
  ylab("Population (thousands)") +
  theme_bw() +
  theme(axis.text=element_text(size=16),axis.title=element_text(size=16),
        text=element_text(size=16),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank())



# ----7. calculate slope of pop trends ----

forsyth_lm <- lm(agg_totpop_thous_est~as.numeric(acs5_end_year), data = forsyth_pop_summary)
summary(forsyth_lm)
# sign. increase of 37000 ppl per period

meck_lm <- lm(agg_totpop_thous_est~as.numeric(acs5_end_year), data = meck_pop_summary)
summary(meck_lm)
# sign. increase of 21000 ppl per period

richmond_lm <- lm(agg_totpop_thous_est~as.numeric(acs5_end_year), data = richmond_pop_summary)
summary(richmond_lm)
# not sign.

union_lm <- lm(agg_totpop_thous_est~as.numeric(acs5_end_year), data = union_pop_summary)
summary(union_lm)
# sign. incrase of 4700 ppl per period
