#------------------------------------
#' Analysis of temperature effect on Saccharina
#' in KEEN ONE data
#------------------------------------
library(lme4)
library(lmerTest)
library(tidyverse)
library(readr)
library(ggplot2)
theme_set(theme_bw(base_size=17))
library(merTools)
library(modelr)

keen_one <- read_csv("../derived_data/keen_temp_community_merged.csv") %>%
  filter(SITE != "SW Appledore")

byrnes <- keen_one %>%
  filter(PI == "Byrnes")

#for funsies

qplot(WINTER_MIN_SEA_SURFACE_TEMPERATURE, S_LATISSIMA_SQ_M+1e-5, data = keen_one, color=SITE) +
  stat_smooth(method="glm",  mapping = aes(group=1),
              method.args = list(family=Gamma(link="log")) )


qplot(WINTER_MIN_SEA_SURFACE_TEMPERATURE, TOTAL_RICHNESS, data = keen_one, color=SITE) +
  stat_smooth(method="glm",  mapping = aes(group=1),
              method.args = list(family=poisson(link="log")) )



## Group means centered model with Byrnes data only

#Data transformation
byrnes <- byrnes %>%
  group_by(SITE) %>%
  mutate(group_temp = mean(WINTER_MIN_SEA_SURFACE_TEMPERATURE, na.rm=TRUE),
         temp_group_cent = WINTER_MIN_SEA_SURFACE_TEMPERATURE - group_temp)  %>%
  ungroup()

#The models
byrnes_meancent_mod <- lmer(log(S_LATISSIMA_SQ_M+1) ~ group_temp + temp_group_cent + 
                              PERCENT_ROCK + (1|SITE),
                            data = byrnes)

byrnes_meancent_mod_cover <- lmer(log(PERCENT_S_LATISSIMA+1) ~ group_temp + temp_group_cent + 
                              PERCENT_ROCK + (1|SITE),
                            data = byrnes)


byrnes_sl_mod_decompose <-  lmer(log(S_LATISSIMA_SQ_M+1) ~ group_temp + year_temp + resid_temp + 
                                 PERCENT_ROCK + (1|SITE) + (1|YEAR),
                               data = byrnes)

#quickie viz
qplot(WINTER_MIN_SEA_SURFACE_TEMPERATURE, S_LATISSIMA_SQ_M+1, data = byrnes, color=SITE) +
  stat_smooth(method="glm",  mapping = aes(group=1),
              method.args = list(family=Gamma(link="log")) )

qplot(WINTER_MIN_SEA_SURFACE_TEMPERATURE, PERCENT_S_LATISSIMA+1, data = byrnes, color=SITE) +
  stat_smooth(method="glm",  mapping = aes(group=1),
              method.args = list(family=Gamma(link="log")) )


## Differencing model with Byrnes data only
byrnes_difference <- byrnes %>%
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
         RICH_PERC_DIFF = RICH_DIFF/lag(SITE_RICHNESS)) %>%
  ungroup()

#model
diff_mod <- lmer(SL_DIFF ~ TEMP_DIFF + SITE_PERCENT_ROCK +(1|SITE), data = byrnes_difference)

diff_perc_mod <- lmer(SL_PERC_DIFF ~ TEMP_DIFF + SITE_PERCENT_ROCK +(1|SITE), data = byrnes_difference)



rich_diff_mod <- lmer(RICH_DIFF ~ TEMP_DIFF+SL_DIFF+ SITE_PERCENT_ROCK  +(1|SITE), data = byrnes_difference)

summary(diff_mod)
summary(rich_diff_mod)


qplot(TEMP_DIFF, SL_PERC_DIFF, color=SITE, data = byrnes_difference) +
  stat_smooth(method = "lm", mapping=aes(group = 1)) +
  xlab("Change in winter minimum temperature (C)") + ylab("Percent Change in kelp")


qplot(TEMP_DIFF, SL_DIFF, color=SITE, data = byrnes_difference) +
  stat_smooth(method = "lm", mapping=aes(group = 1)) +
  xlab("Change in winter min temperature") + ylab("Change in kelp")

qplot(SL_DIFF, RICH_DIFF, color=SITE, data = byrnes_difference) +
  stat_smooth(method = "lm", mapping=aes(group = 1)) +
  xlab("Change in kelp") + ylab("Change in species richness")


## Raw measurement model with all sites
keen_sl_mod_raw <- lmer(log(S_LATISSIMA_SQ_M+1) ~ WINTER_MIN_SEA_SURFACE_TEMPERATURE +  PERCENT_ROCK + (1|SITE),
                            data = keen_one)

keen_sl_mod <-  lmer(log(S_LATISSIMA_SQ_M+1) ~ group_temp + temp_group_cent + 
                       PERCENT_ROCK + (1|SITE),
                     data = keen_one)

keen_sl_cover_mod <-  lmer(log(PERCENT_S_LATISSIMA+1) ~ group_temp + temp_group_cent + 
                       PERCENT_ROCK + (1|SITE),
                     data = keen_one)

keen_sl_mod_decompose <-  lmer(log(S_LATISSIMA_SQ_M+1) ~ group_temp + year_temp + resid_temp + 
                                 PERCENT_ROCK + (1|SITE) + (1|YEAR),
                               data = keen_one)

summary(keen_sl_mod)
summary(keen_sl_mod_decompose)


## visualize raw measurement model
predict_data_group <- modelr::data_grid(keen_one,
                                  group_temp = round(seq_range(group_temp, 4),2),
                                  temp_group_cent = seq_range(temp_group_cent, 100),
                                  PERCENT_ROCK = mean(PERCENT_ROCK),
                                  SITE = "NE Appledore")

keen_sl_predict <- predictInterval(keen_sl_mod, predict_data_group,
                                   which = "fixed",
                                   include.resid.var=FALSE) %>% cbind(predict_data_group) %>%
  mutate(WINTER_MIN_SEA_SURFACE_TEMPERATURE = group_temp + temp_group_cent)


p2 <- keen_one %>% mutate(SITE = SITE[1],
                          PERCENT_ROCK = mean(PERCENT_ROCK))
keen_sl_fix_fit <- predictInterval(keen_sl_mod, 
                                   p2,
                                   which = "fixed",
                                   include.resid.var=FALSE)


keen_sl_fix_fit <- cbind(keen_one, keen_sl_fix_fit)

ggplot(keen_one, 
       aes(x = temp_group_cent, y=S_LATISSIMA_SQ_M)) +
  geom_jitter(mapping = aes(color = SITE)) +
  geom_line(data = keen_sl_predict,
            mapping = aes(y = exp(fit)-1, lty = factor(group_temp)), lwd=1) +
  geom_text(data = keen_sl_predict %>% 
              group_by(group_temp) %>% 
              slice(1L) %>% mutate(fit = exp(fit)+1,
                                   temp_group_cent = -1.35),
            aes(y=fit, label=group_temp)) +
  ylab("Sugar Kelp per Square Meter") +
  xlab("Site-Level Temperature Anomaly (C)") +
  scale_linetype(guide = guide_legend("Average Winter\nMinimum Temperature"))+
  scale_color_discrete(guide = guide_legend("Site"))



ggplot(keen_one, 
       aes(x = WINTER_MIN_SEA_SURFACE_TEMPERATURE, y=S_LATISSIMA_SQ_M)) +
  geom_jitter(mapping = aes(color = SITE)) +
  scale_color_discrete(guide = guide_legend("Site"))+
  ylab("Sugar Kelp per Square Meter") +
  xlab("Winter Minimum Sea Surface Temperature (C)") +
  stat_smooth(method="glm", formula = y+1e-9 ~ x,
              method.args=list(family=Gamma(link="log")),
              color="black")

