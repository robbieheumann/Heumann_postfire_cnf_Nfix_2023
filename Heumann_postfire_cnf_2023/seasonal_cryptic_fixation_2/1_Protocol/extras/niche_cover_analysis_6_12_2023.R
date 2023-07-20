### cryptic N fix niche cover analysis:

library(tidyverse)
library(readr)
cover <- read_csv("seasonal_cryptic_fixation_2/0_Data/external/Plot_level_cover_litter_CWD/14Plots_S22_CrustSpeciesCover.csv")

cover %>% group_by(Species) %>%
  summarize(mean_cov = mean(mean.cover), 
            sd_cov = sd(mean.cover))
hist(cover$mean.cover)
summary(lm(log(mean.cover)~Species, cover))
plot(lm(log(mean.cover)~Species, cover))

fluxes <- read_csv("seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")
fluxes %>% group_by(species) %>%
  summarize(mean_flux = mean(flux_nmol_C2H4_g_h), 
                             median = median(flux_nmol_C2H4_g_h),
                             count = length(flux_nmol_C2H4_g_h))

fluxes$flux_nmol_C2H4_g_h[fluxes$flux_nmol_C2H4_g_h < 0] <- 0 
species_aov <- aov(log(flux_nmol_C2H4_g_h+0.001)~species, fluxes)
summary(species_aov)
TukeyHSD(species_aov)

undercover <- read_csv("seasonal_cryptic_fixation_2/0_Data/external/Plot_level_cover_litter_CWD/14Plots_S22_UnderstoryAllCover.csv")

undercover_sum <- undercover %>% filter(Species == "LUAR") %>% group_by(Site) %>% summarize(mean_cover = mean(mean.cover))
