#------------------------------------
#' SEM of temperature effect on Saccharina and richness
#' in KEEN ONE data
#------------------------------------
library(lme4)
library(blme)
library(pbkrtest)
library(tidyverse)
library(readr)
library(ggplot2)
library(merTools)
library(piecewiseSEM) 
library(glmmTMB)

keen_one <- read_csv("../derived_data/keen_temp_community_merged.csv") %>%
  filter(SITE != "SW Appledore") #?

#all data
ggplot(keen_one %>% group_by(SITE, YEAR) %>% summarize(kelp = mean(S_LATISSIMA_SQ_M, na.rm=T), kelp_sd = sd(S_LATISSIMA_SQ_M, na.rm=T)), 
       aes(x = YEAR, y = kelp, color = SITE, ymin = kelp-kelp_sd, ymax = kelp+kelp_sd)) +
  geom_point() +
  geom_linerange() +
  geom_line()

##

## Richness analysis
keen_one_glm <- keen_one %>%
  mutate(SL = S_LATISSIMA_SQ_M+1e-5,
         SLP = (PERCENT_S_LATISSIMA + 1e-5)/100,
         SLB = quadrat_median_wet_weight/1000+1e-5,
         R = scale(PERCENT_ROCK)) %>%
  group_by(SITE) %>%
  mutate(group_temp = mean(WINTER_MEAN_SEA_SURFACE_TEMPERATURE, na.rm=T),
         temp_group_cent = WINTER_MEAN_SEA_SURFACE_TEMPERATURE-group_temp)

keen_sl_mod <-  glmer(SL ~ group_temp + temp_group_cent + 
                       R + (1|SITE),
                     data = keen_one_glm,
                     family=Gamma(link="log"),
                     control = glmerControl(optimizer = "bobyqa"))

summary(keen_sl_mod)

keen_slp_mod_l <-  glmer(SLP ~ SL + group_temp + temp_group_cent + 
                        R + (1|SITE),
                      data = keen_one_glm,
                      family=Gamma(link="log"),
                      control = glmerControl(optimizer = "bobyqa"))

keen_rich_mod <-  glmer(TOTAL_RICHNESS ~ group_temp + 
                         temp_group_cent + SL + SLP+
                         R + (1|SITE),
                       data = keen_one_glm,
                       family=poisson(link="log"),
                       control = glmerControl(optimizer = "bobyqa"))

keen_sem <- psem(keen_sl_mod, 
                 keen_slp_mod,
                 keen_rich_mod,
                data =  keen_one_glm)

#difference data
coefs(keen_sem)

## Differencing model with Byrnes data only
byrnes_difference <- keen_one %>%
  filter(PI == "Byrnes") %>%
  #first, summarize by site*year, as transects are not permanent
  #so we lose individual level variation
  group_by(SITE, YEAR) %>%
  summarize(SITE_S_LATISSIMA_SQ_M = mean(S_LATISSIMA_SQ_M, na.rm=T),
            SITE_PERCENT_S_LATISSIMA = mean(PERCENT_S_LATISSIMA, na.rm=T),
            WINTER_MIN_SEA_SURFACE_TEMPERATURE = mean(WINTER_MIN_SEA_SURFACE_TEMPERATURE, na.rm=T),
            SITE_PERCENT_ROCK = mean(PERCENT_ROCK, na.rm=T),
            SITE_RICHNESS = mean(TOTAL_RICHNESS, na.rm=T)
  ) %>% 
  arrange(SITE, YEAR) %>%
  #then, difference by year
  group_by(SITE) %>%
  mutate(SL_DIFF = SITE_S_LATISSIMA_SQ_M - lag(SITE_S_LATISSIMA_SQ_M),
         SL_PERC_DIFF = 100*SL_DIFF/lag(SITE_S_LATISSIMA_SQ_M),
         SL_COVER_DIFF = SITE_PERCENT_S_LATISSIMA - lag(SITE_PERCENT_S_LATISSIMA),
         TEMP_DIFF = WINTER_MIN_SEA_SURFACE_TEMPERATURE - lag(WINTER_MIN_SEA_SURFACE_TEMPERATURE),
         RICH_DIFF = SITE_RICHNESS - lag(SITE_RICHNESS),
         RICH_PERC_DIFF = 100*RICH_DIFF/lag(SITE_RICHNESS)) %>%
  ungroup()



diff_mod <- lmer(SL_DIFF ~ TEMP_DIFF + SITE_PERCENT_ROCK +(1|SITE), data = byrnes_difference)

rich_diff_mod <- lmer(RICH_DIFF ~ TEMP_DIFF+SL_DIFF  +(1|SITE), data = byrnes_difference)


diff_sem <- psem(diff_mod, 
                 rich_diff_mod, 
                 data = byrnes_difference)

summary(diff_mod)$coefficients
summary(rich_diff_mod)$coefficients


rich_diff_perc_mod <- lmer(RICH_PERC_DIFF ~ TEMP_DIFF+SL_DIFF  +(1|SITE), data = byrnes_difference)


diff_perc_mod <-lmer(SL_PERC_DIFF ~ TEMP_DIFF + SITE_PERCENT_ROCK +(1|SITE), data = byrnes_difference)


summary(diff_perc_mod)$coefficients
summary(rich_diff_perc_mod)$coefficients
