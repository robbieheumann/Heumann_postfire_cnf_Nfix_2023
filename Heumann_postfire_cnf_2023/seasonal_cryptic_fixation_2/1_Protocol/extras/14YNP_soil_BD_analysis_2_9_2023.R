library(tidyverse)
library(readr)
final_2022_season_fluxes <- read_csv("C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")


## filter out soils and just select drymass and site
soils <- final_2022_season_fluxes %>% filter(niche =="Soil") %>% select(c(site,dry_mass))
## calculate bulk density (sample volume = 4.909cm2*2cm)
soils_bd <- soils %>% mutate(bulk_dens_g_cm3 = dry_mass/(4.909*2))
write.csv(soils_bd,file = '2_Incremental/soil_bulk_density_raw_2022.csv')


## plot bulk density variability by plot
ggplot(soils_bd, aes(x = site, y = bulk_dens_g_cm3)) + geom_boxplot(varwidth = 0.5) +
  theme_classic() + geom_point() + 
  theme(text = element_text(size=18))

## summarize bulk density by plot
soil_bd_sum <- soils_bd %>% group_by(site) %>% summarize(mean_bd = mean(bulk_dens_g_cm3),
                                                         sd_bd = sd(bulk_dens_g_cm3))
## export this
write.csv(soil_bd_sum,file = '2_Incremental/soil_bulk_density_summary_2022.csv')


## import sensitivity analysis
Robbie_Soil_BD_sensitivity_analysis_02_07_2023 <- read_excel("C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Soil N calcs/Robbie_Soil_BD_sensitivity_analysis_02_07_2023.xlsx", 
                                                             +     sheet = "Sheet2")
soil_bd_sens <- Robbie_Soil_BD_sensitivity_analysis_02_07_2023  

ggplot(soil_bd_sens, aes(x = BD_g_cm3, y = N_kg_ha, shape = factor(N_perc))) + geom_point(size=6) + 
  labs(shape = "%N") + xlab(label = "soil bulk density (g/cm3)") + 
  ylab(label = "kg N per hectare") + 
  theme(text = element_text(size=18))
