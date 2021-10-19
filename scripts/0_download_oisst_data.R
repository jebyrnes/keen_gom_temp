#------------------------------------
#' Use site info to harvest buoy
#' temperature data from NOAA OISST
#------------------------------------
library(tidyverse)
library(readr)
library(oisstr) 
library(lubridate)
library(sf)
setwd(here::here())

# what are our site lat/longs
# load sites file
# and get site-level info summarized
sites <- read_csv("raw_data/keen_sites.csv")

sites_summary <- sites %>%
  group_by(PI, SITE, YEAR) %>%
  summarize(LAT = mean(START_LATITUDE, na.rm=T), LONG = mean(START_LONGITUDE, na.rm=T)) %>%
  group_by(SITE) %>%
  mutate(LAT = mean(LAT), LONG = mean(LONG)) %>%
  ungroup() %>%
  st_as_sf(crs = 4326, coords = c("LONG", "LAT"), 
           remove = FALSE)

# get oisst data from 2014-2021

#this is big
oisst_dat <- OISST_sub_dl(
  start = as.Date("2014-01-01"),
  end = as.Date("2021-09-30"),
  latitude_ext = st_bbox(sites_summary)[c(2,4)],
  longitude_ext = st_bbox(sites_summary)[c(1,3)]
)

# filter down to what points we need

#add indices for oisst_dat
idx_frame <- oisst_dat %>%
  select(lat, lon) %>%
  group_by(lat, lon) %>%
  slice(1L) %>%
  ungroup() %>%
  mutate(oisst_id = 1:n()) %>%
  st_as_sf(crs = 4326, coords = c("lon", "lat"), 
           remove = FALSE)

#get nearest points
nearest_index <- st_nearest_feature(sites_summary, idx_frame)

bio_data <- sites_summary %>%
  mutate(oisst_id = idx_frame$oisst_id[nearest_index])

# merge data
spatial_filtered_oisst <- oisst_dat %>%
  left_join(idx_frame %>% select(lon, lat, oisst_id))

saveRDS(spatial_filtered_oisst, "derived_data/oisst_temp_data.rds")
saveRDS(bio_data, "derived_data/sites_summary_oisst_id.rds")
