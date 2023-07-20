#### Seasonal Cryptic N fix Flux Analysis cleaned and finalized 2022

#### Outline ####
#### import FINAL_Cumulative Flux Calcs ####
library(tidyverse)
library(readr)
getwd()
final_2022_season_fluxes <- read_csv("0_Data/final_2022_season_fluxes.csv")
## rename this df "rates" 
rates <- final_2022_season_fluxes
## remove summer BiscBas soil NW which for some reason never had dry mass recorded,
## as well as the extra flux calc at the bottom of the doc that was just from dragging an equation one cell too far in excel...
rates <- rates %>% filter(flux_nmol_C2H4_g_h != 'NA')

## ****NOTE*** "summer" and "summer_wet" season are both 24hr incubation measurements, but
##             "summer" is from late July following 3 weeks of no rain
##             "summer_wet" is from early august the morning after a full day of rain

## create data frame of just cryptic niches (no rhizo, endo or lupine rates)
cryptic_rates <- rates %>% filter(niche !='endo') 
cryptic_rates <- cryptic_rates %>% filter(niche !='rhizo')
cryptic_rates <- cryptic_rates %>% filter(niche !='lupine')

## convert every rate less than 0 to zero. negative rates we are assuming are meaningless in this case
cryptic_rates$flux_nmol_C2H4_g_h[cryptic_rates$flux_nmol_C2H4_g_h < 0] <- 0
cryptic_rates$flux_gramN_g_h[cryptic_rates$flux_gramN_g_h < 0] <- 0
cryptic_rates$flux_gN_cm2_hr[cryptic_rates$flux_gN_cm2_hr < 0] <- 0

## now plot histogram of AR rates
ggplot(cryptic_rates, aes(x = flux_nmol_C2H4_g_h))+geom_histogram()+facet_wrap(~niche, scales = "free")
## log transform the above rates
ggplot(cryptic_rates, aes(x = log(flux_nmol_C2H4_g_h)))+geom_histogram()+facet_wrap(~niche)
### trasnforms to a normal distribution. So fluxes will be log trasnformed for future analyses


## scatter plot colored by niche with moist_perc (percent water content) on x
ggplot(cryptic_rates, aes(x = moist, y = flux_nmol_C2H4_g_h)) + geom_point()

## convert outlier moisture contents (FounEast Moss NE pre and BiscBas Moss NW wet) to NA (these are 
## missing or have incorrect dry mass data for unknown reason)
## convert remaining moisture contents >-4 % moist to 0 (these are just errors associated with 
## our balance that then got multiplied by 100 to convert to %)
cryptic_rates$moist[cryptic_rates$moist < -70] <- NA
cryptic_rates$moist[cryptic_rates$moist < 0] <- 0

## now repeat scatter plot from above
ggplot(cryptic_rates, aes(x = moist, y = flux_nmol_C2H4_g_h)) + geom_point()

## Group data by season, site and by niche to compare spring, summer (dry and wet) and fall rates across niches. 
## Ultimately I want to create a bar graph of average rates colored and separated by niche, for each season. 
## so first, create summary table of niche averages and standard deviations, sample_counts, average moisture content
## and moisture varince across each season. 
## Call this table "season_rate_sum". This will be used to scale up to kg/ha/season fluxes
season_rate_sum <- cryptic_rates %>% group_by(season, site, niche) %>% 
  summarize(mean_moist = mean(moist, na.rm=T), ## some moisture contents are missing so ignore these NAs
            variance_moist = var(moist, na.rm=T),
            mean_gN_cm2_hr = mean(flux_gN_cm2_hr),
            median_gN_cm2_hr = median(flux_gN_cm2_hr),
            st_dev_cm2 = sd(flux_gN_cm2_hr),
            variance_cm2 = var(flux_gN_cm2_hr),
            mean_rate_gN_g_h = mean(flux_gramN_g_h), 
            median_rate_gN_g_h = median(flux_gramN_g_h),
            st_dev_g = sd(flux_gramN_g_h),
            variance_g = var(flux_gramN_g_h),
            sample_n = length(flux_gramN_g_h))

## export this data sheet to .csv in "incrementals" folder in "Seasonal_cryptic_fixation_2022" -> "data" subfolder
getwd()
setwd('C:/Users/rh176228/Desktop/cryptic_fixation/seasonal_cryptic_fixation_2')
write.table(season_rate_sum, file = '2_Incremental/season_rate_sum_1_25.csv', col.names = TRUE, row.names = FALSE, sep = ",")

###########################################Scaling up to fluxes#####################################################################################################
## Scale up sample N rates to plot level and per hectare level fluxes using percent cover,
## CWD and litter density data from the plots. 

## 1. Calculate mean n fix rates and standard deviations for each niche in each plot during each season. Create new data
# frame for this table. "seasonal_plot_fluxes"
## 2. merge plot level percent cover and CWD data with that "seasonal_plot_fluxes" data frame. 
## 3.  
##  A. Moss/lichen % cover: calculate the moss % cover for each plot by the area of the plot (2500 m2 = 25,000,000 cm2).
##      This gives you the area of moss per plot. 
##      - Now multiply that area by the plot average rate (N per gram per hour) and this will give the plot level hourly flux of N. 
##      - Now calculate the amount of hours in the 2 months of that season:
##      Spring: May and June,  Summer: July and August,  Fall: September and October
##      - Multiply the hourly plot flux for moss and lichen by the amount of hours in that season and that is the seasonal
##      plot level flux of N from those niches

##  B. CWD: Convert the CWD rates per plot to grams of wood.
##      - Multiply the per gram average rate of fixation in wood for each plot by that CWD biomass amount in grams
##      - Multiply that hourly plot-level rate by the number of hours in that season and this equals your seasonal
##      plot-level flux of N from CWD fixation. 

##. C: Litter: Convert the average litter mass per plot to grams and follow same steps as above using litter mass instead of CWD

##. D: Soil: calculate the mass of soil in each plot by multiply cm2 area (25000000) by 10cm depth = 250000000 cm3
##      - multiply the volume (250000000) by bulk density average of soil samples from each plot 
##      (dry mass/9.818cm3 - 2cm sampling depth by area of tube)
##      - this results in the mass of soil per plot which is then multiplied by the plot average N fix rate to produce
##      the seasonal plot-level flux. 

##### ******* IMPORTANT ******* ##### : In order to sum standard deviations, calculate variances of each flux measurement across groups
#####                           ##### : then sum the variances and take the square root for the final standard deviation of mean fluxes 
##### ******* IMPORTANT ******* ##### : across plots. 


######  Moss and Lichen flux calcs  ########
# import moss and lichen percent cover data
X14Plots_S22_UnderstoryMossLichen <- read_csv("0_Data/external/Plot_level_cover_litter_CWD/14Plots_S22_UnderstoryMossLichen.csv")
cover <- X14Plots_S22_UnderstoryMossLichen ## change name of moss and lichen cover data to "cover"
ggplot(cover, aes(x = Site, y = mean.cover.percent, fill = Functional.Group)) + 
  geom_col( position = 'dodge') ## quick glance at what that data looks like


# filter out our five high density sites for which I have N fix measurements
fiveplot_cover <- cover %>% filter(Site %in% c('BiscBas', 'FirLopSo', 'FounEast', 'GibFalls', 'GravPit'))
ggplot(fiveplot_cover, aes(x = Site, y = mean.cover.percent, fill = Functional.Group)) + geom_col( position = 'dodge')
## These cover measurements include vertical layering in the ground cover but I'm still 
## skeptical of this moss and lichen cover in FounEast. I feel like there is way more than ~1% cover. 

## convert percent cover to area of moss and lichen for each plot
## plot dimensions = 50x50m (0.25ha)
fiveplot_cover <- fiveplot_cover %>% mutate(total_coverage_cm2 = (mean.cover.percent/100) * (50*100)^2) #area of plot in cm2

## rename Site and Functional.group to site and niche in fiveplot cover then merge with seasonal rate sum
fiveplot_cover <- fiveplot_cover %>% rename("site" = "Site",
                                            "niche" = "Functional.Group")
moss_lichen_fluxes <- merge(season_rate_sum, fiveplot_cover)

## calculate total area of each niche in each plot to then scale up rates using mean_gN_cm2_hr flux (based off sample tube area)

moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(flux_kgN_.25ha_h = (total_coverage_cm2*mean_gN_cm2_hr)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(median_flux_kgN_.25ha_h = (total_coverage_cm2*median_gN_cm2_hr)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(sd_kgN_.25ha_h = (total_coverage_cm2*st_dev_cm2)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(var_kgN_.25ha_h = (total_coverage_cm2*variance_cm2)/1000)
## dont need this yet but saving it in case: moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(SE_kgN_.25ha_h = (total_coverage_cm2*st_err_cm2)/1000)

## multiply by 4 for per hectare fluxes, then multiply by total hours in that season (2 months = 1460 hrs)

moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(median_flux_kgN_ha_season = median_flux_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(sd_kgN_ha_season = sd_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(var_kgN_ha_season = var_kgN_.25ha_h*4*1460)
## same here... moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(SE_kgN_ha_season = SE_kgN_.25ha_h*4*1460)

## export this data.table
write_excel_csv(moss_lichen_fluxes, file = '2_Incremental/moss_lichen_fluxes_1_25_2023.csv')


##### Wood flux calcs ######
## import wood biomass data
X14Plots_S22_CoarseWoodyDebrisBiomass <- read_csv("0_Data/external/Plot_level_cover_litter_CWD/14Plots_S22_CoarseWoodyDebrisBiomass.csv")
CWD <- X14Plots_S22_CoarseWoodyDebrisBiomass ## rename to "CWD"

# filter out our five high density sites for which I have N fix measurements
fiveplot_CWD <- CWD %>% filter(Site %in% c('BiscBas', 'FirLopSo', 'FounEast', 'GibFalls', 'GravPit'))
ggplot(fiveplot_CWD, aes(x = Site, y = bm_1000h_Class45_Mg_ha)) + geom_col()

## convert Mg/ha to g/ha (1 Mg = 1000000g)
fiveplot_CWD <- fiveplot_CWD %>% mutate(g_biomass_4.5.CWD = bm_1000h_Class45_Mg_ha*1000000)

## rename Site column and add niche column, then remove class 3 bm biomass column
fiveplot_CWD <- fiveplot_CWD %>% rename("site" = "Site")
fiveplot_CWD <- fiveplot_CWD %>% mutate(niche = "Wood")
fiveplot_CWD <- fiveplot_CWD %>% select(-bm_1000h_Class3_Mg_ha)

## merge with seasonal rates
wood_fluxes <- merge(season_rate_sum, fiveplot_CWD)

## scale to plot level using grams biomass
wood_fluxes <- wood_fluxes %>% mutate(flux_kgN_ha_h = ((g_biomass_4.5.CWD*mean_rate_gN_g_h)/1000)*4)
wood_fluxes <- wood_fluxes %>% mutate(median_flux_kgN_ha_h = ((g_biomass_4.5.CWD*median_rate_gN_g_h)/1000)*4)
wood_fluxes <- wood_fluxes %>% mutate(sd_kgN_ha_h = ((g_biomass_4.5.CWD*st_dev_g)/1000)*4)
wood_fluxes <- wood_fluxes %>% mutate(var_kgN_ha_h = ((g_biomass_4.5.CWD*variance_g)/1000)*4)
## wood_fluxes <- wood_fluxes %>% mutate(SE_kgN_ha_h = ((g_biomass_4.5.CWD*st_err_g)/1000)*4)

wood_fluxes <- wood_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_ha_h*1460)
wood_fluxes <- wood_fluxes %>% mutate(median_flux_kgN_ha_season = median_flux_kgN_ha_h*1460)
wood_fluxes <- wood_fluxes %>% mutate(var_kgN_ha_season = var_kgN_ha_h*1460)
wood_fluxes <- wood_fluxes %>% mutate(sd_kgN_ha_season = sd_kgN_ha_h*1460)
## wood_fluxes <- wood_fluxes %>% mutate(SE_kgN_ha_season = SE_kgN_ha_h*1460)

## export this data.table
write_excel_csv(wood_fluxes, file = '2_Incremental/wood_fluxes_1_25_2023.csv')

##### Soil Flux Calcs ######

## calculate average bulk density for each plot (column of soil sample  = 4.909tube area*2cm depth)
soil_fluxes <- cryptic_rates %>% filter(niche == "Soil") %>% filter(moist<700)
soil_fluxes <- soil_fluxes %>% group_by(season, site) %>% 
  summarize(mean_rate = mean(flux_gramN_g_h),
            median_rate = median(flux_gramN_g_h),
            stdev = sd(flux_gramN_g_h),
            variance_g = var(flux_gramN_g_h),
            mean_BD = mean(dry_mass/(4.909*2)),
            st_err_g = sd(flux_gramN_g_h/sqrt(length(flux_gramN_g_h))),
            sample_n = length(flux_gramN_g_h),
            mean_moist =mean(moist))

### just interested to see bulk density distribution
ggplot(soil_fluxes, aes(x = site, y = mean_BD)) + geom_boxplot() + 
  ylab(label = "mean bulk density (g/cm3)") +
  theme(text = element_text(size=20))
## add plot soil volume (to a depth of 10cm - because we are assuming most N fix activity is
##                                            within the upper 10cm of soil - Cory pilot data 2017)
soil_fluxes <- soil_fluxes %>% mutate(plot_soil_vol = 25000000*10) # cm2 of plot * depth

## calculate mass of soil per plot
soil_fluxes <- soil_fluxes %>% mutate(plot_soil_mass_g = plot_soil_vol*mean_BD)

## now multiply that by the mean rate per dry gram soil, and scale up sd, var, and SE (st_err)
soil_fluxes <- soil_fluxes %>% 
  mutate(flux_kgN_plot_hr = (plot_soil_mass_g*mean_rate)/1000, 
         median_flux_kgN_plot_hr = (plot_soil_mass_g*median_rate)/1000, 
         var_kgN_plot_hr = plot_soil_mass_g*variance_g/1000,
         st_dev_kg = stdev*plot_soil_mass_g/1000, 
         st_err_kg = plot_soil_mass_g*st_err_g/1000)

## scale to per ha and seasonal flux
soil_fluxes <- soil_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_plot_hr*4*1460)
soil_fluxes <- soil_fluxes %>% mutate(median_flux_kgN_ha_season = median_flux_kgN_plot_hr*4*1460)
soil_fluxes <- soil_fluxes %>% mutate(var_kgN_ha_season = var_kgN_plot_hr*4*1460)
soil_fluxes <- soil_fluxes %>% mutate(sd_kgN_ha_season = st_dev_kg*4*1460)
soil_fluxes <- soil_fluxes %>% mutate(st_err_kgN_ha_season = st_err_kg*4*1460)

## export this data.table
write_excel_csv(soil_fluxes, file = '2_Incremental/soil_fluxes_1_25_2023.csv')

#### Litter Flux Calcs ####
library(readxl)
Litter_mass <- read_excel("0_Data/external/Plot_level_cover_litter_CWD/GYE 2022 Litter Mass (Fertilization and 14 Plots).xls", sheet = "14 Plots")
summary(Litter_mass)

## filter out five target plots
litter <- Litter_mass %>% filter(Plot %in% c('BiscBas', 'FirLopSo', 'FounEast', 'GibFalls', 'GravPit'))

## litter quadrat dimensions = 27.5x27.5cm
## calculate average litter mass per quadrat for each plot
litter_mass_sum <- litter %>% group_by(Plot) %>% summarize(mean_mass_g = mean(`Mass (g)`),
                                                           sd_mass_g = sd(`Mass (g)`))
## scale average mass to plot area (.25ha)
# one quadrat = 0.0756m2 so 0.0756*13.223 = g/m2 * 100m*100m (1ha) = g/ha
litter_mass_sum <- litter_mass_sum %>% mutate(litter_g_ha = (mean_mass_g*13.223*10000),
                           sd_litter_g_ha = sd_mass_g*13.223*10000)


## merge plot litter mass with per plot average litter N fix
#3 change litter_mass_sum "Site" to "Plot"
litter_mass_sum <- rename(litter_mass_sum, "site" = "Plot")
litter_flux_sum <- season_rate_sum %>% filter(niche == "Litter")
litter_flux_sum <- merge(litter_flux_sum, litter_mass_sum)

## calculate N flux by multiplying mean_rate_gN_g_hr * litter_g_ha
litter_flux_sum <- litter_flux_sum %>% mutate(N_flux_gN_ha_hr = mean_rate_gN_g_h*litter_g_ha,
                                              median_N_flux_gN_ha_hr = median_rate_gN_g_h*litter_g_ha,
                                              sd_gN_ha_hr = st_dev_g*litter_g_ha)
## scale plot level flux to per ha per season (*4*1460) 
litter_flux_sum <- litter_flux_sum %>% mutate(flux_kgN_ha_season = (N_flux_gN_ha_hr*1460)/1000,
                                              median_flux_kgN_ha_season = (median_N_flux_gN_ha_hr*1460)/1000,
                                              sd_kgN_ha_season = (sd_gN_ha_hr*1460)/1000)

write_excel_csv(litter_flux_sum, file = '2_Incremental/litter_fluxes_2_3_2023.csv')
##### Season and Site Summaries #####
# Soil summaries
soil_site_sum <- soil_fluxes %>% select(c("season", "site", "sample_n", "mean_moist", "flux_kgN_ha_season","median_flux_kgN_ha_season", "sd_kgN_ha_season"))
soil_site_sum <- soil_site_sum %>% mutate(niche = "Soil")

# Wood Summaries
wood_site_sum <- wood_fluxes %>% select(c("season", "niche", "site", "sample_n", "mean_moist", "flux_kgN_ha_season", "median_flux_kgN_ha_season","sd_kgN_ha_season"))

## Moss and Lichen Summaries
ML_site_sum <- moss_lichen_fluxes %>% select(c("niche","season", "site", "sample_n", "mean_moist", "flux_kgN_ha_season", "median_flux_kgN_ha_season", "sd_kgN_ha_season"))

litter_site_sum <- litter_flux_sum %>% select(c("niche","season", "site", "sample_n", "mean_moist", "flux_kgN_ha_season", "median_flux_kgN_ha_season", "sd_kgN_ha_season"))
#### merge all the niche summaries ####
fluxes_combined_4season <- rbind(wood_site_sum, soil_site_sum, ML_site_sum, litter_site_sum)
write_excel_csv(fluxes_combined_4season, file = '2_Incremental/combined_4seas_fluxes_2_6_2023.csv')

## average the two summer measurements for 3 season analysis
fluxes_combined_4season$season[fluxes_combined_4season$season == 'summer_wet'] <- 'summer'

fluxes_combined_3season <- fluxes_combined_4season %>% 
  group_by(season, niche, site) %>% 
  summarise(sample_n = sum(sample_n), 
            flux_kgN_ha_season = mean(flux_kgN_ha_season), 
            median_season_flux = median(median_flux_kgN_ha_season),
            mean_moist = mean(mean_moist),
            sd_moist = sd(mean_moist),
            sd_kgN_ha_season = sum(sd_kgN_ha_season))

fluxes_combined_4season <- rbind(wood_site_sum, soil_site_sum, ML_site_sum) ### retain the 4 season df for pre vs post rain
write_excel_csv(fluxes_combined_3season, file = '2_Incremental/combined_3seas_fluxes_2_6_2023.csv')

### summarize the average across plots for season average by niche, but keep SE on the combined plot inputs level for plotting
seasonal_fluxes <- fluxes_combined_3season %>% group_by(season, niche) %>%
  summarise(combined_mean_flux = mean(flux_kgN_ha_season), se_flux = sd(flux_kgN_ha_season/sqrt(length(flux_kgN_ha_season)))) %>%
  group_by(season) %>%
  mutate(se = sum(se_flux), total_flux = sum(combined_mean_flux)) %>% ## 5 plots = n for SE calc
  ungroup()
