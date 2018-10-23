# yadkin site description analysis
# last updated 20181019

# ---- 1 set up -----

# clear ws
rm(list = ls())

# load libraries
library(tidyverse)
library(lubridate)


# ---- 2.1 import data ----

# weather stations in common
weather_stations <- c("351-800", "354-806", "358-800", "361-816", "364-809")

# make list of days
baseline_start_date <- date("1979-01-01")
baseline_end_date <- date("2008-12-31")
baseline_date_seq <- seq(baseline_start_date, baseline_end_date, "day")

# download baseline precip & temp
setwd('/Users/ssaia/Google Drive/STOTEN/paper-yadkin-swat-study-repo/observed_data/weather')
p1 <- read_csv(paste0("p", weather_stations[1], ".txt"), col_names = FALSE, skip = 1) %>%
  mutate(date = baseline_date_seq, gage_id = weather_stations[1]) %>%
  select(date, precip_mm = X1, gage_id)
p2 <- read_csv(paste0("p", weather_stations[2], ".txt"), col_names = FALSE, skip = 1) %>%
  mutate(date = baseline_date_seq, gage_id = weather_stations[2]) %>%
  select(date, precip_mm = X1, gage_id)
p3 <- read_csv(paste0("p", weather_stations[3], ".txt"), col_names = FALSE, skip = 1) %>%
  mutate(date = baseline_date_seq, gage_id = weather_stations[3]) %>%
  select(date, precip_mm = X1, gage_id)
p4 <- read_csv(paste0("p", weather_stations[4], ".txt"), col_names = FALSE, skip = 1) %>%
  mutate(date = baseline_date_seq, gage_id = weather_stations[4]) %>%
  select(date, precip_mm = X1, gage_id)
p5 <- read_csv(paste0("p", weather_stations[5], ".txt"), col_names = FALSE, skip = 1) %>%
  mutate(date = baseline_date_seq, gage_id = weather_stations[5]) %>%
  select(date, precip_mm = X1, gage_id)
precip_data <- rbind(p1, p2, p3, p4, p5)




# ---- 2.2 reformat data ----