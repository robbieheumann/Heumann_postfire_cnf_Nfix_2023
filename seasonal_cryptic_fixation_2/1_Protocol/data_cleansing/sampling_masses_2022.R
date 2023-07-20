### sample collection report
## load tidyverse
library(tidyverse)
library(readxl)
### Import cumulative flux calcs from 2022 - "for R" sheet
clim_exp_data <- read_csv("C:/Users/rh176228/Desktop/cryptic_fixation/seasonal_cryptic_fixation_2/0_Data/final_clim_exp_fluxes.csv")
seasonals <- read_csv("C:/Users/rh176228/Desktop/cryptic_fixation/seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")

### seasonal sampling dry mass by niche
seasonal_sample_masses <- seasonals %>% group_by(niche) %>% summarize(total_mass_collected = sum(dry_mass)) 

climate_sampling_masses <- clim_exp_data %>% group_by(niche) %>% summarize(total_mass_collected = sum(dry_mass))
