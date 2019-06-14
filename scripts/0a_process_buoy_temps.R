#------------------------------------
#' Use site info to harvest buoy
#' temperature data from NOAA
#------------------------------------
library(tidyverse)
library(readr)
library(lubridate)

#for viz checking
library(ggplot2)

# load site-level info summarized
sites_summary <- read_csv("../derived_data/keen_sites_summary.csv")

#----
#Load raw buoy data
#----

raw_buoy_temps <- read_csv("../derived_data/raw_buoy_temps.csv",
                           col_types = "Tdciii")



#summarizing pipiline


#summarizing pipiline
#for summer mean, max, winter mean, min, max
temp_summary_daily <- . %>% 
  #filter(row_number()>1) %>% #first row is from previous year
  #make quarter names make sense
  mutate(quarter = case_when(
    quarter == 1 ~ "winter",
    quarter == 2 ~ "spring",
    quarter == 3 ~ "summer",
    TRUE ~ "fall"
  )) %>%
  group_by(station, year, month, day(time), quarter) %>%
  summarize(time = time[1],
            mean_sea_surface_temperature = 
              mean(sea_surface_temperature, na.rm=T),
            min_sea_surface_temperature = 
              min(sea_surface_temperature, na.rm=T),
            max_sea_surface_temperature = 
              max(sea_surface_temperature, na.rm=T)  
  ) %>%
  filter(is.finite(min_sea_surface_temperature))%>%
  ungroup() #some odd days threw errors
  
#for summer mean, max, winter mean, min, max
temp_summary_quarterly <- . %>% 
  #get averages
  group_by(station, year, quarter) %>%
  summarize(degree_days_14 = sum(min_sea_surface_temperature>14),
            degree_days_15 = sum(min_sea_surface_temperature>15),
            degree_days_17 = sum(min_sea_surface_temperature>17),
            degree_days_19 = sum(min_sea_surface_temperature>19),
            degree_days_20 = sum(min_sea_surface_temperature>20),
            degree_days_22 = sum(min_sea_surface_temperature>22),
            mean_sea_surface_temperature = mean(mean_sea_surface_temperature),
            min_sea_surface_temperature = min(min_sea_surface_temperature),
            max_sea_surface_temperature = max(max_sea_surface_temperature)  
            
  ) %>%
    ungroup()

temps_dd_total <- . %>%
  group_by(station, year) %>%
  filter(str_detect(measurement, "degree_days")) %>%
  mutate(temp = str_remove(measurement, "[a-z,_]*")) %>%
  group_by(station, year, temp) %>%
  summarize(value = sum(value, na.rm=T)) %>%
  mutate(measurement = str_c("total_degree_days_", temp)) %>%
  select(-temp) %>%
  ungroup()

temps_long <- . %>%
  #reshape to a wide format
  gather(measurement, value, 
         degree_days_14:max_sea_surface_temperature) %>%
  mutate(measurement = str_c(quarter, measurement, sep="_"))
  
temps_wide_with_total_dd <- . %>%
  dplyr::select(-quarter) %>%
  bind_rows((.) %>% temps_dd_total) %>% #check out that (.)!
  spread(measurement, value) 
  

#Do the work of getting 

buoy_temps_daily <- raw_buoy_temps %>%
  temp_summary_daily

buoy_temps_quarterly <- buoy_temps_daily %>%
  temp_summary_quarterly

buoy_temps <- buoy_temps_quarterly %>%
  temps_long %>%
  temps_wide_with_total_dd


#checking with viz


ggplot(buoy_temps_daily, aes(x = time, y = min_sea_surface_temperature, color = station)) +
  geom_line() +
  geom_hline(yintercept = 15, lty = 2)+
  geom_hline(yintercept = 17, lty = 2)+
  geom_hline(yintercept = 19, lty = 2)+
  geom_hline(yintercept = 20, lty = 2)+
  geom_hline(yintercept = 22, lty = 2)


ggplot(buoy_temps_quarterly,aes (x = year, y = degree_days_14, color = station)) +
  geom_line() +
  facet_wrap(~quarter)

ggplot(buoy_temps_quarterly,aes (x = year, y = degree_days_14, color = station)) +
  geom_line() +
  facet_wrap(~quarter)

ggplot(buoy_temps,aes (x = year, y = total_degree_days_20, color = station)) +
  geom_line() 
  
ggplot(buoy_temps_quarterly,aes (x = year, y = min_sea_surface_temperature, color = station)) +
  geom_line() +
  facet_wrap(~quarter)
  
  
ggplot(buoy_temps %>%
         gather(measurement, value, -station, -year) %>%
         filter(str_detect(measurement, "total")),
         aes (x = year, y = value, color = measurement)) +
    geom_line() +
  facet_wrap(~station)


#############

#----------
# calculate centered winter envt vars
#----------

names(buoy_temps) <- str_to_upper(names(buoy_temps))


#do all of the group mean centering
buoy_temps_centered <- buoy_temps  %>%
  gather(measurement, value, -STATION, -YEAR) %>%
  group_by(STATION, measurement) %>%
  mutate(STATION_MEAN = mean(value)) %>%
  ungroup() %>%
  mutate(STATION_CENT = value - STATION_MEAN) %>%
  group_by(YEAR) %>%
  mutate(ANNUAL_MEAN = mean(value)) %>%
  ungroup() %>%
  mutate(ANNUAL_CENT = value - ANNUAL_MEAN,
         RESID_FROM_CENT = value - STATION_MEAN - ANNUAL_MEAN) %>%
  pivot_wider(names_from = measurement, 
              values_from = c(STATION_MEAN, STATION_CENT,
                            ANNUAL_MEAN, ANNUAL_CENT,
                            RESID_FROM_CENT))
  


#-------
# merge back with KEEN site data, and write out results
#--------
write_csv(buoy_temps_centered, path="../derived_data/buoy_temps.csv")

write_csv(left_join(sites_summary, buoy_temps_centered), path="../derived_data/keen_sites_with_temps.csv")
