#### climate experiment vs seasonal field measurement results
library(tidyverse)
### import clim_exp data SLW august experiment data and June clim data

library(readxl)
## import raw_june_clim_exp ppm data
ML_fluxes <- read_csv("C:/Users/rh176228/Desktop/cryptic_fixation/cryptic_clim_exp_2022/1_incremental/ML_fluxes_updated.csv")


### import august exp SLW data
library(readr)
slw_combined_fluxes_03_13_2023 <- read_csv("cryptic_clim_exp_2022/1_incremental/slw_combined_fluxes_03_13_2023.csv")


### import seasonal data 
final_2022_season_fluxes <- read_csv("seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")


## reduce ML fluxes and SLW fluxes to just ethylene fluxes and moist percent and niche and compare to experimental data

ml_flux_reduced <- ML_fluxes %>% select(c(niche, temp, actual_moist_perc, flux_nmol_C2H4_g_hr))
slw_flux_reduced <- slw_combined_fluxes_03_13_2023 %>% select(c(niche, temp_C, actual_moist, flux_nmol_c2h4_g_hr_med_corr))

season_flux_reduced <- final_2022_season_fluxes %>% select(c(niche, site, season, moist, flux_nmol_C2H4_g_h))

## rename columns so they all have the same variable names
ml_flux_reduced <- ml_flux_reduced %>% rename("temp_C" = "temp", "actual_moist" = "actual_moist_perc")
slw_flux_reduced <- slw_flux_reduced %>% rename("flux_nmol_C2H4_g_hr" ="flux_nmol_c2h4_g_hr_med_corr")
season_flux_reduced <- season_flux_reduced %>% rename("actual_moist" = "moist", "flux_nmol_C2H4_g_hr" = "flux_nmol_C2H4_g_h")
season_flux_reduced$flux_nmol_C2H4_g_hr[season_flux_reduced$flux_nmol_C2H4_g_hr < 0] <- 0

## rbind ML and SLW then create two different categories field vs experimental to rbind all of them with
exp_flux <- rbind(ml_flux_reduced, slw_flux_reduced)
exp_flux <- exp_flux %>% mutate(type = "experimental", season = "na", site = "na")
season_flux_reduced <- season_flux_reduced %>% mutate(type = "field")
## zero negative moisture contents
season_flux_reduced$actual_moist[season_flux_reduced$actual_moist < 0] <- 0

### remove lupine
season_flux_reduced <- season_flux_reduced %>% filter(niche != "lupine")
season_flux_reduced <- season_flux_reduced %>% filter(niche != "rhizo")
season_flux_reduced <- season_flux_reduced %>% filter(niche != "endo")

## put in incubation temp for seasonal calculating means of incubation temps if incubation included multiple days
season_flux_reduced <- season_flux_reduced %>% mutate(temp_C = ifelse(season == "spring", 4.6, 
                                               ifelse(season=="summer", 18.06, 
                                                      ifelse(season=="summer_wet", 13.1, 8.615))))
glimpse(season_flux_reduced)
flux_combined <- rbind(season_flux_reduced, exp_flux)

### plot flux by moisture with type as color
ggplot(flux_combined, aes(x = actual_moist, y = flux_nmol_C2H4_g_hr, color = type)) + 
  geom_point() + 
  facet_wrap(~niche, scales = 'free')

## create model to predict
## change temp_C in exp fluxes to temp
L_flux <- exp_flux %>% filter(niche == "Lichen")
M_flux <- exp_flux %>% filter(niche == "Moss")
W_flux <- exp_flux %>% filter(niche == "Wood")
Lt_flux <- exp_flux %>% filter(niche == "Litter")
S_flux <- exp_flux %>% filter(niche == "Soil")

my_Lt_polynomial_model <- lm(log(flux_nmol_C2H4_g_hr+0.0001) ~ poly(temp_C,2)*actual_moist, data  = Lt_flux)
my_W_polynomial_model <- lm(log(flux_nmol_C2H4_g_hr+0.0001) ~ poly(temp_C,2)*actual_moist, data  = W_flux)
my_S_polynomial_model <- lm(log(flux_nmol_C2H4_g_hr+0.0001) ~ poly(temp_C,2)*actual_moist, data  = S_flux)
lichen_log_model <- lm(log(flux_nmol_C2H4_g_hr+0.0001) ~ log(temp_C)*actual_moist, data  = L_flux)
moss_log_model <- lm(log(flux_nmol_C2H4_g_hr+0.0001) ~ log(temp_C)*actual_moist, data  = M_flux)

### predict fluxes based on season moist contents and temp
season_flux_reduced <- season_flux_reduced %>% mutate(predicted_flux = ifelse(niche == "Lichen", predict(lichen_log_model),
                                                                              ifelse(niche == "Moss", predict(moss_log_model),
                                                                                     ifelse(niche == "Litter", predict(my_Lt_polynomial_model),
                                                                                            ifelse(niche == "Wood", predict(my_W_polynomial_model), 
                                                                                                   predict(my_S_polynomial_model))))))

### plot
ggplot(season_flux_reduced, aes(x = exp(predicted_flux), y = flux_nmol_C2H4_g_hr)) +
         geom_point() + 
  facet_wrap(~niche, scales = "free")

