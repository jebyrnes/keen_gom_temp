#------------------------------------
#' Merge KEEN and temperature data
#------------------------------------

library(tidyverse)
library(readr)

community <- read_csv("../derived_data/keen_merged_data.csv")
temps <- read_csv("../derived_data/keen_sites_with_temps.csv")

all <- left_join(community, temps)

visdat::vis_dat(all)
visdat::vis_miss(all)


write_csv(all, "../derived_data/keen_temp_community_merged.csv", na="")
