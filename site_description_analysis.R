# yadkin site description analysis
# last updated 20181019

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(lubridate)
library(readxl)

# load home-made functions 
functions_path = "/Users/ssaia/Documents/GitHub/yadkin-analysis/functions/"
source(paste0(functions_path, "multiplot.R")) # for creating plots with multiple figures


# ---- 2 import baseline weather data ----

# weather stations in common
weather_stations <- c("351-800", "354-806", "358-800", "361-816", "364-809")

# make list of days
baseline_start_date <- date("1979-01-01")
baseline_end_date <- date("2008-12-31")

# path to baseline data
baseline_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/observed_data/weather/'
precip_baseline_text_files <- paste0(baseline_data_path, "p", weather_stations, ".txt")
temp_baseline_text_files <- paste0(baseline_data_path, "t", weather_stations, ".txt")

modified_precip_read_csv <- function(file_path, start_date, end_date){
  weather_station <- str_sub(file_path, -11, -5)
  date_seq <- seq(start_date, end_date, "day")
  data <- read_csv(file_path, col_names = FALSE, skip = 1) %>%
    mutate(date = date_seq, gage_id = weather_station) %>%
    select(date, precip_mm = X1, gage_id) %>%
    mutate(precip_mm = ifelse(precip_mm == -99, NA_real_, precip_mm))
  return(data)
}

modified_temp_read_csv <- function(file_path, start_date, end_date){
  weather_station <- str_sub(file_path, -11, -5)
  date_seq <- seq(start_date, end_date, "day")
  data <- read_csv(file_path, col_names = FALSE, skip = 1, na = c("", -99)) %>%
    mutate(date = date_seq, gage_id = weather_station) %>%
    select(date, temp_c = X1, gage_id) %>%
    mutate(temp_c = ifelse(temp_c == -99, NA_real_, temp_c))
  return(data)
}

# test_precip_data <- modified_precip_read_csv(precip_baseline_text_files[1], baseline_start_date, baseline_end_date)

# read each csv file into a dataframe
precip_baseline_data <- purrr::map_dfr(precip_baseline_text_files, modified_precip_read_csv, start_date = baseline_start_date, end_date = baseline_end_date) %>%
  arrange(date, gage_id)
temp_baseline_data <- purrr::map_dfr(temp_baseline_text_files, modified_temp_read_csv, start_date = baseline_start_date, end_date = baseline_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)


# ---- import backcast baseline weather data ----

# make list of days
bc_baseline_start_date <- date("1979-01-01")
bc_baseline_end_date <- date("2002-12-31")

# path to miroc backcast baseline data
miroc_bc_baseline_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/simulated_data/climate/backcast_climate_1982-2002/miroc/'
precip_miroc_bc_baseline_text_files <- paste0(miroc_bc_baseline_data_path, "p", weather_stations, ".txt")
temp_miroc_bc_baseline_text_files <- paste0(miroc_bc_baseline_data_path, "t", weather_stations, ".txt")

# read each csv file into dataframe for backcast baseline data
precip_miroc_bc_baseline_data <- purrr::map_dfr(precip_miroc_bc_baseline_text_files, modified_precip_read_csv, start_date = bc_baseline_start_date, end_date = bc_baseline_end_date) %>%
  arrange(date, gage_id)
temp_miroc_bc_baseline_data <- purrr::map_dfr(temp_miroc_bc_baseline_text_files, modified_temp_read_csv, start_date = bc_baseline_start_date, end_date = bc_baseline_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)

# path to csiro backcast baseline data
csiro_bc_baseline_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/simulated_data/climate/backcast_climate_1982-2002/csiro/'
precip_csiro_bc_baseline_text_files <- paste0(csiro_bc_baseline_data_path, "p", weather_stations, ".txt")
temp_csiro_bc_baseline_text_files <- paste0(csiro_bc_baseline_data_path, "t", weather_stations, ".txt")

# read each csv file into dataframe for backcast baseline data
precip_csiro_bc_baseline_data <- purrr::map_dfr(precip_csiro_bc_baseline_text_files, modified_precip_read_csv, start_date = bc_baseline_start_date, end_date = bc_baseline_end_date) %>%
  arrange(date, gage_id)
temp_csiro_bc_baseline_data <- purrr::map_dfr(temp_csiro_bc_baseline_text_files, modified_temp_read_csv, start_date = bc_baseline_start_date, end_date = bc_baseline_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)

# path to hadley backcast baseline data
hadley_bc_baseline_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/simulated_data/climate/backcast_climate_1982-2002/hadley/'
precip_hadley_bc_baseline_text_files <- paste0(hadley_bc_baseline_data_path, "p", weather_stations, ".txt")
temp_hadley_bc_baseline_text_files <- paste0(hadley_bc_baseline_data_path, "t", weather_stations, ".txt")

# read each csv file into dataframe for backcast baseline data
precip_hadley_bc_baseline_data <- purrr::map_dfr(precip_hadley_bc_baseline_text_files, modified_precip_read_csv, start_date = bc_baseline_start_date, end_date = bc_baseline_end_date) %>%
  arrange(date, gage_id)
temp_hadley_bc_baseline_data <- purrr::map_dfr(temp_hadley_bc_baseline_text_files, modified_temp_read_csv, start_date = bc_baseline_start_date, end_date = bc_baseline_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)


# ---- 3 import projection weather data ----

# make list of days
project_start_date <- date("2047-01-01")
project_end_date <- date("2070-12-31")

# path to miroc 8.5
miroc8_5_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/simulated_data/climate/climate_2050-2070/miroc_8.5/'
precip_miroc8_5_text_files <- paste0(miroc8_5_data_path, "p", weather_stations, ".txt")
temp_miroc8_5_text_files <- paste0(miroc8_5_data_path, "t", weather_stations, ".txt")

# path to csiro 4.5
csiro4_5_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/simulated_data/climate/climate_2050-2070/csiro_4.5/'
precip_csiro4_5_text_files <- paste0(csiro4_5_data_path, "p", weather_stations, ".txt")
temp_csiro4_5_text_files <- paste0(csiro4_5_data_path, "t", weather_stations, ".txt")

# path to csiro 8.5
csiro8_5_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/simulated_data/climate/climate_2050-2070/csiro_8.5/'
precip_csiro8_5_text_files <- paste0(csiro8_5_data_path, "p", weather_stations, ".txt")
temp_csiro8_5_text_files <- paste0(csiro8_5_data_path, "t", weather_stations, ".txt")

# path to hadley 4.5
hadley4_5_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/simulated_data/climate/climate_2050-2070/hadley_4.5/'
precip_hadley4_5_text_files <- paste0(hadley4_5_data_path, "p", weather_stations, ".txt")
temp_hadley4_5_text_files <- paste0(hadley4_5_data_path, "t", weather_stations, ".txt")

# read in miroc 8.5 data
precip_miroc8_5_data <- purrr::map_dfr(precip_miroc8_5_text_files, modified_precip_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id)
temp_miroc8_5_data <- purrr::map_dfr(temp_miroc8_5_text_files, modified_temp_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)

# read in csiro 4.5 data
precip_csiro4_5_data <- purrr::map_dfr(precip_csiro4_5_text_files, modified_precip_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id)
temp_csiro4_5_data <- purrr::map_dfr(temp_csiro4_5_text_files, modified_temp_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)

# read in csiro 4.5 data
precip_csiro8_5_data <- purrr::map_dfr(precip_csiro8_5_text_files, modified_precip_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id)
temp_csiro8_5_data <- purrr::map_dfr(temp_csiro8_5_text_files, modified_temp_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)

# read in csiro 4.5 data
precip_hadley4_5_data <- purrr::map_dfr(precip_hadley4_5_text_files, modified_precip_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id)
temp_hadley4_5_data <- purrr::map_dfr(temp_hadley4_5_text_files, modified_temp_read_csv, start_date = project_start_date, end_date = project_end_date) %>%
  arrange(date, gage_id) %>%
  select(temp_c)


# ---- 3 import baseline streamflow data ----

baseline_streamflow_data_path <- '/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/observed_data/streamflow/cms_conversions/USGS_02129000_pee_dee.xlsx'

baseline_streamflow_data_raw <- read_excel(baseline_streamflow_data_path, skip = 29, col_names = FALSE)
colnames(baseline_streamflow_data_raw) <- c("agency", "gage_id", "date", "flow_max_cfs", "flow_max_code", "flow_min_cfs", "flow_min_code", "flow_mean_cfs", "flow_mean_code", "notes", "flow_cms")


# ---- 4 reformat data ----

# bind precip and temp
baseline_data <- cbind(precip_baseline_data, temp_baseline_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "1982-01-01" & date <= "2002-12-31") %>%
  mutate(data_type = "Baseline")

miroc_bc_baseline_data <- cbind(precip_miroc_bc_baseline_data, temp_miroc_bc_baseline_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "1982-01-01" & date <= "2002-12-31") %>%
  mutate(data_type = "MIROC Baseline")

csiro_bc_baseline_data <- cbind(precip_csiro_bc_baseline_data, temp_csiro_bc_baseline_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "1982-01-01" & date <= "2002-12-31") %>%
  mutate(data_type = "CSIRO Baseline")

hadley_bc_baseline_data <- cbind(precip_hadley_bc_baseline_data, temp_hadley_bc_baseline_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "1982-01-01" & date <= "2002-12-31") %>%
  mutate(data_type = "Hadley Baseline")

miroc8_5_data <- cbind(precip_miroc8_5_data, temp_miroc8_5_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "2050-01-01" & date <= "2070-12-31") %>%
  mutate(data_type = "MIROC 8.5")

csiro4_5_data <- cbind(precip_csiro4_5_data, temp_csiro4_5_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "2050-01-01" & date <= "2070-12-31") %>%
  mutate(data_type = "CSIRO 4.5")

csiro8_5_data <- cbind(precip_csiro8_5_data, temp_csiro8_5_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "2050-01-01" & date <= "2070-12-31") %>%
  mutate(data_type = "CSIRO 8.5")

hadley4_5_data <- cbind(precip_hadley4_5_data, temp_hadley4_5_data) %>%
  select(date, gage_id, precip_mm, temp_c) %>%
  filter(date >= "2050-01-01" & date <= "2070-12-31") %>%
  mutate(data_type = "Hadley 4.5")

# select only necessary baseline streamflow data
baseline_streamflow_data <- baseline_streamflow_data_raw %>%
  mutate(month = month(date), year = year(date)) %>%
  select(date, month, year, flow_cms) %>%
  filter(date <= "2002-12-31")
  


# ---- 5 calculate daily weather summaries ----

# calcuate average daily precip and temp for all 5 gages
baseline_avg_daily_data <- baseline_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

miroc_bc_baseline_avg_daily_data <- miroc_bc_baseline_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

csiro_bc_baseline_avg_daily_data <- csiro_bc_baseline_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

hadley_bc_baseline_avg_daily_data <- hadley_bc_baseline_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

miroc8_5_avg_daily_data <- miroc8_5_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

csiro4_5_avg_daily_data <- csiro4_5_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

csiro8_5_avg_daily_data <- csiro8_5_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

hadley4_5_avg_daily_data <- hadley4_5_data %>%
  group_by(date, data_type) %>%
  summarise(daily_avg_precip_mm = mean(precip_mm), daily_avg_temp_c = mean(temp_c)) %>%
  na.omit()

# bind together
climate_avg_daily_data <- rbind(baseline_avg_daily_data, miroc_bc_baseline_avg_daily_data, csiro_bc_baseline_avg_daily_data, hadley_bc_baseline_avg_daily_data, miroc8_5_avg_daily_data, csiro4_5_avg_daily_data, csiro8_5_avg_daily_data, hadley4_5_avg_daily_data)


# ---- 6 calculate annual weather summaries ----

# calcuate cummulative precip and average temp by year for all 5 gages
climate_annual_data <- climate_avg_daily_data %>%
  mutate(year = year(date)) %>%
  group_by(year, data_type) %>%
  summarize(annual_sum_precip_mm = sum(daily_avg_precip_mm), annual_avg_temp_c = mean(daily_avg_temp_c)) %>%
  ungroup() #%>%
  #mutate(period = "Baseline", simulation_year = seq(1:168))

# reorder factor
#climate_annual_data$data_type <- factor(climate_annual_data$data_type, levels = c("Baseline", "Baseline MIROC", "Baseline CSIRO", "Baseline Hadley", "MIROC 8.5", "CSIRO 4.5", "CSIRO 8.5", "Hadley 4.5"))


# ---- 7 plot ----

# make a list to hold plots
my_site_desc_plots = list()

my_site_desc_plots[[1]] = ggplot(data = climate_annual_data) +
  geom_boxplot(aes(x = data_type, y = annual_sum_precip_mm, color = period)) +
  geom_point(aes(x = data_type, y = annual_sum_precip_mm, color = period), alpha = 0.25, size = 3) +
  xlab("") +
  ylab("Cummulative Precipitation (mm/yr)") +
  scale_color_manual(values = c("black", "grey60")) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18),
        axis.text.x = element_text(angle = 90, hjust = 1))

my_site_desc_plots[[2]] = ggplot(data = climate_annual_data) +
  geom_boxplot(aes(x = data_type, y = annual_avg_temp_c, color = period)) +
  geom_point(aes(x = data_type, y = annual_avg_temp_c, color = period), alpha = 0.25, size = 3) +
  xlab("") +
  ylab("Average Annual Temperature (C)") +
  scale_color_manual(values = c("black", "grey60")) +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18),
        axis.text.x = element_text(angle = 90, hjust = 1))

# plot tract and subbasin scaled together
setwd("/Users/ssaia/Desktop")
cairo_pdf("fig_x.pdf", width = 20, height = 10, pointsize = 18)
multiplot(plotlist = my_site_desc_plots, cols = 2)
dev.off()

ggplot(data = baseline_streamflow_data) +
  geom_boxplot(aes(x = factor(month), y = flow_cms)) +
  #geom_point(aes(x = factor(month), y = flow_cms), alpha = 0.25, size = 3) +
  xlab("Month") +
  ylab("Daily Streamflow (cms)") +
  theme_bw() +
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
        panel.background=element_blank(),text=element_text(size=18),
        axis.text.x = element_text(angle = 90, hjust = 1))


# ---- 8 calculate annual summaries for table ----

# precipitation
annual_precip_data <- climate_annual_data %>%
  group_by(data_type) %>%
  summarize(annual_sum_avg_precip_mm = mean(annual_sum_precip_mm),
            annual_sum_sd_precip_mm = sd(annual_sum_precip_mm))

# temperature
annual_temp_data <- climate_avg_daily_data %>%
  group_by(data_type) %>%
  summarize(annual_avg_temp_c = mean(daily_avg_temp_c),
            annual_sum_sd_temp_c = sd(daily_avg_temp_c))

# streamflow
mean(baseline_streamflow_data$flow_cms)
sd(baseline_streamflow_data$flow_cms)
