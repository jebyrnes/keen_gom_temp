#------------------------------------
#' Analysis of temperature effect on Saccharina
#' in KEEN ONE data
#------------------------------------
library(lme4)
library(lmerTest)
library(tidyverse)
library(readr)
library(ggplot2)
library(merTools)
library(modelr)
library(broom.mixed)

keen_one <- read_csv("../derived_data/keen_temp_community_merged.csv") %>%
  filter(SITE != "SW Appledore")

#look at relationship between kelp and each group
keen_one_long_rich  <- keen_one %>%
  gather(GROUP, RICHNESS, Algae_RICHNESS:Urchins_RICHNESS)

makectab <- function(mermod){
  summary(mermod)$coefficients %>%
    as_tibble(rownames = "Term")
}

rich_analysis <- keen_one_long_rich %>%
  group_by(GROUP) %>%
  nest() %>%
  mutate(mod_fits = map(data, ~glmer(RICHNESS ~ S_LATISSIMA_SQ_M + 
                                   PERCENT_ROCK + (1|SITE), data = .,
                                   family=poisson(link="log"),
                                   nAGQ=0))) %>%
  mutate(mod_coefs = map(mod_fits, makectab)) %>%
  unnest(mod_coefs) %>%
  filter(Term != "(Intercept)") %>%
  filter(Term != "PERCENT_ROCK")
  
rich_analysis %>% filter( `Pr(>|z|)` <= 0.05)
         