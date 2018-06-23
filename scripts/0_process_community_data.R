#------------------------------------
#' Merge KEEN community data into a standardized
#' single file format for analysis
#------------------------------------

library(tidyverse)
library(readr)
library(lubridate)

#for viz checking
library(ggplot2)
library(ggmap)

# load sites file
# and get site-level info summarized
sites <- read_csv("../raw_data/keen_sites.csv") %>% filter(YEAR<2017) %>%
  select(PI, YEAR, SITE, TRANSECT, START_LATITUDE, START_LONGITUDE, START_DEPTH_M) 


quads <-read_csv("../raw_data/keen_quads.csv") %>% 
  filter(SP_CODE %in% c("SL", "AGCL", "LADI")) %>%
  group_by(PI, YEAR, SITE, TRANSECT, SP_CODE) %>%
  summarize(NUM_SQ_M = sum(COUNT, na.rm=T)/6) %>%
  spread(SP_CODE, NUM_SQ_M, fill=0) %>%
  mutate(TOTAL_KELP_SQ_M = SL + LADI + AGCL ) %>%
  rename(S_LATISSIMA_SQ_M = SL) %>%
  select(PI, YEAR, SITE, TRANSECT, S_LATISSIMA_SQ_M, TOTAL_KELP_SQ_M) 


#cover for habitat
cover <-read_csv("../raw_data/keen_cover.csv") %>%
  filter(GROUP == "Substrate") %>%
  spread(SP_CODE, PERCENT_COVER, fill=0) %>%
  mutate(PERCENT_ROCK=B+BL+BM+BS+C) %>%
  rename(PERCENT_COBBLE = C,
         PERCENT_SAND = S) %>%
  select(PI, YEAR, SITE, TRANSECT, PERCENT_ROCK, PERCENT_COBBLE) 


#cover for habitat
kelpcover <-read_csv("../raw_data/keen_cover.csv") %>%
  filter(SP_CODE %in% c("SL", "AGCL", "LADI")) %>%
  spread(SP_CODE, PERCENT_COVER, fill=0) %>%
  mutate(PERCENT_KELP = SL + LADI + AGCL) %>%
  rename(PERCENT_S_LATISSIMA = SL) %>%
  select(PI, YEAR, SITE, TRANSECT, PERCENT_S_LATISSIMA, PERCENT_KELP) 


#swath for crustaceans
swath <-read_csv("../raw_data/keen_swath.csv") %>%
  filter(SP_CODE %in% c("CABO", "CAIR", "HOAM")) %>%
  group_by(PI, YEAR, SITE, TRANSECT, SP_CODE) %>%
  summarize(NUM_SQ_M = sum(COUNT)/80) %>%
  spread(SP_CODE, NUM_SQ_M, fill=0) %>%
  rename(LOBSTERS_SQ_M = HOAM, 
         C_BOREALIS_SQ_M = CABO,
         C_IRRORATUS_SQ_M = CAIR)

#fish for...fish
fish <-read_csv("../raw_data/keen_fish.csv") %>%
  filter(SP_CODE %in% c("TAAD", "POVI")) %>%
  group_by(PI, YEAR, SITE, TRANSECT, SP_CODE) %>%
  summarize(COUNT = sum(COUNT, na.rm=T)) %>%
  spread(SP_CODE, COUNT, fill=0) %>%
  rename(CUNNER_PER_TRANSECT = TAAD,
         POLLACK_PER_TRANSECT = POVI) %>%
  select(PI, YEAR, SITE, TRANSECT, CUNNER_PER_TRANSECT, POLLACK_PER_TRANSECT) 



#What I want
#site, year, transect, latlon, perc rock, perc cobble, adult SL, total adult kelp, crabs, lobster, cunner, pollack,

all_data <- Reduce(left_join, list(sites, cover, kelpcover, quads, swath, fish))

visdat::vis_dat(all_data)

#get species richness
get_sp <- function(filename){
  print(filename)
  sampling <- read_csv(filename) %>%
    filter(SP_CODE !="NO_FISH")
  
 sampling %>%
    group_by(NETWORK, PI, YEAR, SITE, TRANSECT,
             COMMON.DIVISION.NAME, GROUP, COMMON.NAME, SIZE, GENUS,
             SPECIES) %>%
   summarize(PHYLUM = first(PHYLUM)) %>%
   ungroup() %>%
   dplyr::select(-PHYLUM)
} 
 

#what files contain biological information?
files <- str_c("../raw_data/", list.files("../raw_data/"))
files <- files[-grep("sites", files)]

#get unique species (filter to remove size classes)
common_rich <- map_df(files, get_sp)  %>%
  filter(COMMON.DIVISION.NAME != "Substrate") %>%
  group_by(NETWORK, PI, YEAR, SITE, TRANSECT,
           COMMON.DIVISION.NAME, GROUP, COMMON.NAME, GENUS,
           SPECIES) %>%
  slice(1L) %>% #gets 1 species per protocol
  ungroup() %>%
  group_by(NETWORK, PI, YEAR, SITE, TRANSECT, GROUP, COMMON.DIVISION.NAME) %>%
  summarize(RICHNESS = n()) %>%
  ungroup() 


group_rich <- common_rich %>%
  group_by(NETWORK, PI, YEAR, SITE, TRANSECT, GROUP) %>%
  filter(GROUP != "Fish") %>% #taken care of already
  summarize(RICHNESS = sum(RICHNESS)) %>%
  mutate(COMMON.DIVISION.NAME = GROUP) %>%
  ungroup() %>% 
  dplyr::select(-GROUP)

all_rich <- common_rich %>%
  group_by(NETWORK, PI, YEAR, SITE, TRANSECT) %>%
  summarize(RICHNESS = sum(RICHNESS)) %>%
  mutate(COMMON.DIVISION.NAME = "TOTAL") %>%
  ungroup()

#make one big wide frame using filled in common division name as the key
rich_tibble <- bind_rows(common_rich %>% dplyr::select(-GROUP),
                         group_rich,
                         all_rich) %>%
  spread(COMMON.DIVISION.NAME, RICHNESS, fill=0)%>%
  rename_at(vars(Algae:Urchins), 
            funs(str_c(., "_RICHNESS")))

all_data <- left_join(all_data, rich_tibble)

#Write data out
visdat::vis_dat(all_data)

write_csv(all_data, "../derived_data/keen_merged_data.csv", na="")
