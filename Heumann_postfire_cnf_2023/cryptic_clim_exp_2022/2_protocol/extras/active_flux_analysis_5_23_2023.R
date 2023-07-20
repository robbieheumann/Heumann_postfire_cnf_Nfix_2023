### modeling zero inflation in the data - probability of N fix occuring and if so how much? 
# import combined flux data from experiment
comb_flux <- read_csv("cryptic_clim_exp_2022/1_incremental/combined_fluxes_5_23_2023.csv")

# load tidyverse
library(tidyverse)

## how many fluxes are not zero?
comb_flux
comb_flux %>%
  group_by(niche) %>%
  summarise(count = sum(flux_nmol_C2H4_g_hr > 0),
            total = length(flux_nmol_C2H4_g_hr),
            proportion = count/total*100)

### model active fluxes
active_flux <- comb_flux %>% filter(flux_nmol_C2H4_g_hr>0)

active_flux %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(flux_nmol_C2H4_g_hr+0.0001)~actual_moist, data=.x)))
active_flux %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(flux_nmol_C2H4_g_hr+0.0001)~temp, data=.x)))
active_flux %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(flux_nmol_C2H4_g_hr+0.0001)~temp*actual_moist, data=.x)))
active_flux %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(flux_nmol_C2H4_g_hr+0.0001)~temp*actual_moist, data=.x)))

## active fluxes from seasonal measurements
seasonal_rates <- read_csv("seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")
seasonal_rates %>%
  group_by(niche, season) %>%
  summarise(count = sum(flux_nmol_C2H4_g_h > 0, na.rm=T),
            total = length(flux_nmol_C2H4_g_h),
            proportion = count/total*100)
active_field_rates
