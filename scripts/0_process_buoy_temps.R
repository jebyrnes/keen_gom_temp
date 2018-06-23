#------------------------------------
#' Use site info to harvest buoy
#' temperature data from NOAA
#------------------------------------
library(tidyverse)
library(readr)
library(rnoaa) #make sure it is from github
library(lubridate)

#for viz checking
library(ggplot2)
library(ggmap)

# load sites file
# and get site-level info summarized
sites <- read_csv("../raw_data/keen_sites.csv") %>% filter(YEAR<2017)

sites_summary <- sites %>%
  group_by(PI, SITE, YEAR) %>%
  summarize(LAT = mean(START_LATITUDE, na.rm=T), LONG = mean(START_LONGITUDE, na.rm=T)) %>%
  group_by(SITE) %>%
  mutate(LAT = mean(LAT), LONG = mean(LONG)) %>%
  ungroup()

#----
#Load buoys, filter to which are close to sites
#and  which have relevant info, then join to sites_summary
#----

library(rgeos)
library(sp)


allbuoys <- buoy_stations() %>%
  mutate(station = str_to_lower(station))  %>%
  filter(station != "44092") %>% #only deployed for 1 year
  filter(station != "44028") %>% #only out until 1997
  filter(station != "44070")  #only out until 2010

# get_buoy <- function(x){
#   ret <- grep(str_to_upper(x), allbuoys$DC.description)
#   if(length(ret)==0) return(NA)
#   ret[1]
# }

#filter to stdmet buoys
met <- buoys(dataset="stdmet") %>%
  rename(station = id)

# buoyfilter <- map_int(met$station, get_buoy)
# na_filter <- which(is.na(buoyfilter))
# uibs <- met$station[-na_filter]
# buoyfilter <- buoyfilter[-which(is.na(buoyfilter))]

allbuoys_filtered <- left_join(met, allbuoys)%>%
  filter(!is.na(lat)) #allbuoys[buoyfilter,]
# allbuoys_filtered$station <- uibs

#some more filtering
allbuoys_filtered <- allbuoys_filtered %>%
  filter(station != "bhbm3") %>% #harbor, not mouth
  filter(station != "iosn3") %>% #no sst
  filter(station != "welm1") %>% #salt marsh
  filter(station != "bgxn3") %>% #inland bay
  filter(station != "nwpr1") %>% # in a bay
  filter(station != "qptr1") %>% # as are all
  filter(station != "prur1")%>%  #the next few
  filter(station != "naxr1")%>%  #and this leads
  filter(station != "ptcr1")%>%  #to very divergent
  filter(station != "cptr1")%>%  #temps
  filter(station != "frxm3")%>%
  filter(station != "bltm3")%>%
  filter(station != "frvm3")%>%
  filter(station != "pvdr1")%>%
  filter(station != "foxr1")%>%
  filter(station != "buzm3")  #no seawater temps


#get closest buoys
set1sp <- SpatialPoints(cbind(allbuoys_filtered$lon, allbuoys_filtered$lat))
set2sp <- SpatialPoints(cbind(sites_summary$LONG, sites_summary$LAT))

sites_summary <- sites_summary %>%
  mutate(station = allbuoys_filtered$station[apply(gDistance(set1sp, set2sp, byid=TRUE), 1, which.min)]) %>%
  left_join(allbuoys_filtered) %>%
  mutate(STATION_LONG = lon, STATION_LAT = lat, STATION= station) %>%
  dplyr::select(PI, SITE, YEAR, LAT, LONG, STATION, STATION_LONG, STATION_LAT)


#visualize to check
keen_map <- get_map(c(min(sites_summary$LONG), 
                      min(sites_summary$LAT)),
                    zoom=7)
ggmap(keen_map) +
  geom_point(data = sites_summary, mapping = aes(x=LONG, y = LAT), size=3) +
  geom_point(data = sites_summary, 
             mapping = aes(x = STATION_LONG, y = STATION_LAT), color="red", shape=1, size=1.5)+
  xlim(c(-72, -69)) + ylim(c(40,43.9)) +
  xlab("") + ylab("")


#-------
#for each site/year, get buoys data, downsample to daily
#--------

#first, get unique buoy/year combos
unique_buoy_by_year <- sites_summary %>%
  group_by(YEAR, STATION) %>%
  slice(1L) %>%
  ungroup() %>%
  dplyr::select(STATION, YEAR)

#meh, let's go with all years for each buoy
unique_buoy_by_year <- unique_buoy_by_year %>%
  complete(YEAR, STATION)

#function to get data for one station for one or more years

get_bdata <- function(buoyid, years){
  print(str_c(buoyid, years, sep=" "))
  bdata <- map_df(years, ~buoy(dataset = "stdmet", buoyid = buoyid, year=.)$data)
  
  bdata <- as.tibble(bind_rows(bdata)) %>%
    dplyr:: select(time, sea_surface_temperature) %>%
    mutate(station = buoyid,
           time = parse_date_time(time, orders="ymdHMS"),
           year = year(time),
           quarter = quarter(time),
           month = month(time)) 
  
  bdata 
  
}

#summarizing pipiline
#for summer mean, max, winter mean, min, max
temp_summary <- . %>% 
  filter(row_number()>1) %>% #first row is from previous year
  #make quarter names make sense
  mutate(quarter = case_when(
    quarter == 1 ~ "winter",
    quarter == 2 ~ "spring",
    quarter == 3 ~ "summer",
    TRUE ~ "fall"
  )) %>%

  #get averages
  group_by(station, year, quarter) %>%
  summarize(mean_sea_surface_temperature = mean(sea_surface_temperature, na.rm=T),
            min_sea_surface_temperature = min(sea_surface_temperature, na.rm=T),
            max_sea_surface_temperature = max(sea_surface_temperature, na.rm=T)
  ) %>%
  
  #reshape to a wide format
  gather(measurement, value, 
         mean_sea_surface_temperature, min_sea_surface_temperature,
         max_sea_surface_temperature) %>%
  mutate(measurement = str_c(quarter, measurement, sep="_")) %>% 
  dplyr::select(-quarter) %>%
  spread(measurement, value)


#OK, map the preceeding function and data manipulation workflow
#onto each row of unique_buoy_by_year
buoy_temps <- map_df(transpose(unique_buoy_by_year), 
                     ~get_bdata(.$STATION, .$YEAR) %>% temp_summary) %>%
  ungroup()

# debug
onebuoy <- map_df(transpose(unique_buoy_by_year %>% filter(STATION=="44030")), 
                     ~get_bdata(.$STATION, .$YEAR)) %>%
  ungroup()

qplot(data = onebuoy, x = time, y = sea_surface_temperature, geom="line")

#semantic matching
names(buoy_temps) <- str_to_upper(names(buoy_temps))

#test
# buoy_temps %>% ungroup() %>%
#   gather(measurement, value, -STATION, -YEAR) %>%
#   separate(measurement, c("SEASON", "MEASUREMENT"), sep = "_", extra = "merge") %>%
#   ggplot(aes(x=YEAR, y = value, color=factor(STATION))) +
#   geom_line() + 
#   facet_grid(SEASON~MEASUREMENT, scale="free_y")
  
#----------
# calculate centered winter mins
#----------

buoy_temps <- buoy_temps  %>%
  group_by(STATION) %>%
  mutate(group_temp = mean(WINTER_MIN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         temp_group_cent = WINTER_MIN_SEA_SURFACE_TEMPERATURE - group_temp)  %>%
  ungroup() %>%
  group_by(YEAR) %>%
  mutate(year_temp = mean(WINTER_MIN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         temp_year_cent = WINTER_MIN_SEA_SURFACE_TEMPERATURE - group_temp) %>% ungroup %>%
  mutate(resid_temp = WINTER_MIN_SEA_SURFACE_TEMPERATURE - group_temp - year_temp)

#-------
# merge back with KEEN site data, and write out results
#--------
write_csv(buoy_temps, path="../derived_data/buoy_temps.csv")

write_csv(left_join(sites_summary, buoy_temps), path="../derived_data/keen_sites_with_temps.csv")
