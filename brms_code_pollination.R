##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      BRMS pollination, Bayesian approach 
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
rm(list = ls())
gc()

library(ggplot2)
library(brms)
library(xlsx)
library(nlme)

## Note: inorder to run the models you need the parallel package, since I use the detectCores function

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##      read data
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dia_dat <- read.csv("https://raw.githubusercontent.com/Martin19910130/Bayesian_pollination/main/Dia_seeds_per_capsule.csv")

ggplot(dia_dat, aes(x = climate, y = Seeds_capsule, fill = treatment)) + geom_boxplot() + 
  facet_wrap(~season) + theme_bw()

dia_mod <- brm(Seeds_capsule ~ climate * treatment * season + 
                               (1|individual_id/plot_id/climate) + 
                               (1|plot_id/climate) +
                               ar(gr = individual_id), 
               cores = parallel::detectCores() - 1, 
               iter = 4000, 
               data = dia_dat,
               )

sca_dat <- read.csv("https://raw.githubusercontent.com/Martin19910130/Bayesian_pollination/main/Sca_seeds_per_flowerhead.csv")

ggplot(sca_dat, aes(x = climate, y = seeds_flowerhead, fill = treatment)) + geom_boxplot() + 
  facet_wrap(~ season) + theme_bw()


sca_mod <-  brm(seeds_flowerhead ~ climate * treatment * season + 
                  (1|plot_id/climate) + 
                  (1|individual_id/plot_id/climate) + 
                  ar(gr = individual_id), 
                cores = parallel::detectCores() - 1, 
                iter = 4000, 
                data = sca_dat)

## use this to reorder panel of seasons 
sca_dat$season <- factor(sca_dat$season, levels = c("summer", "fall"))
dia_dat$season <- factor(dia_dat$season, levels = c("summer", "fall"))

ggplot(sca_dat, aes(y = seeds_flowerhead, x = climate, fill = treatment)) + geom_boxplot() + facet_wrap(~ season)+ 
  geom_point(dia_dat, mapping = aes(y = Seeds_capsule, x = climate, shape = treatment), 
             position = position_dodge(width = .75)) + ggtitle("Sca och")

ggplot(dia_dat, aes(y = Seeds_capsule, x = climate, fill = treatment)) + geom_boxplot() + facet_wrap(~ season) + 
  geom_point(dia_dat, mapping = aes(y = Seeds_capsule, x = climate, shape = treatment), 
             position = position_dodge(width = .75)) + ggtitle("Dia car")


# NLME -----------------------------------------------------------

# I suggest removing correlation structure
# This does not fit this model, but rather defaults to corAR1(form=~1)
# So, this will not be helpful (correlation need be among individual measurements)
dia_lme <- lme(Seeds_capsule ~ climate * treatment * season,
               random = ~ 1 | individual_id/plot_id/climate,
               correlation = corAR1(form=~1|individual_id/plot_id/climate),
               data = dia_dat
               )

# scabiosa
sca_mod <-  lme(seeds_flowerhead ~ climate * treatment * season,
                random = ~ 1 |individual_id/plot_id/climate,
                correlation = corAR1(form=~1|individual_id/plot_id/climate),
                data = sca_dat)
