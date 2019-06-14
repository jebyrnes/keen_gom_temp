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
sites <- read_csv("../raw_data/keen_sites.csv")

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
keen_map <- get_stamenmap(c(min(sites_summary$LONG), min(sites_summary$LAT), max(sites_summary$LONG), 
                      max(sites_summary$LAT)),
                    zoom=9)
ggmap(keen_map) +
 # geom_point(data = sites_summary, mapping = aes(x=LONG, y = LAT), size=3) +
  geom_point(data = sites_summary, 
             mapping = aes(x = STATION_LONG, y = STATION_LAT), color="red", shape=1, size=1.5)+
  #xlim(c(-72, -69)) +
  #ylim(c(40,43.9)) +
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
  
  bdata <- as_tibble(bind_rows(bdata)) %>%
    dplyr:: select(time, sea_surface_temperature) %>%
    mutate(station = buoyid,
           time = parse_date_time(time, orders="ymdHMS"),
           year = year(time),
           quarter = quarter(time),
           month = month(time)) 
  
  bdata 
  
}


#unique_buoy_by_year <- crossing(YEAR = 2014:2018, STATION = unique(unique_buoy_by_year$STATION))

#OK, map the preceeding function and data manipulation workflow
#onto each row of unique_buoy_by_year
raw_buoy_temps <- map(transpose(unique_buoy_by_year), 
                     ~get_bdata(.$STATION, .$YEAR))

#write out raw data
write_csv(bind_rows(raw_buoy_temps), "../derived_data/raw_buoy_temps.csv")
write_csv(sites_summary, path="../derived_data/keen_sites_summary.csv")

