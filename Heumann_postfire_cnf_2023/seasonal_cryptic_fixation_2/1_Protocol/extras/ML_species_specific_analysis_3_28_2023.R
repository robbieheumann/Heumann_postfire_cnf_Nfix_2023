#### Moss and Lichen species specific cluxes
library(tidyverse)
Flux_Calcs_YNP_2022_seasonals <- read_csv("C:/Users/rh176228/Desktop/cryptic_fixation/seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")
cryptic_rates <- Flux_Calcs_YNP_2022_seasonals
cryptic_rates <- rates %>% filter(flux_nmol_C2H4_g_h != 'NA')
## convert every 'flux_nmol_C2H4_g_h' rate less than 0 to zero
cryptic_rates$flux_nmol_C2H4_g_h[cryptic_rates$flux_nmol_C2H4_g_h < 0] <- 0
cryptic_rates$flux_gramN_g_h[cryptic_rates$flux_gramN_g_h < 0] <- 0
cryptic_rates$flux_gN_cm2_hr[cryptic_rates$flux_gN_cm2_hr < 0] <- 0

## create data frame of just cryptic niches (no rhizo, endo or lupine rates)

cryptic_rates <- rates %>% filter(niche !='endo') 
cryptic_rates <- cryptic_rates %>% filter(niche !='rhizo')
cryptic_rates <- cryptic_rates %>% filter(niche !='lupine')

## group with species
season_rate_sum_siteless <- cryptic_rates %>% group_by(season, niche, species) %>% 
  summarize(mean_moist = mean(moist, na.rm=T), 
            variance_moist = var(moist, na.rm=T),
            mean_gN_cm2_hr = mean(flux_gN_cm2_hr),
            st_dev_cm2 = sd(flux_gN_cm2_hr),
            variance_cm2 = var(flux_gN_cm2_hr),
            se_cm2 = st_dev_cm2/sqrt(length(flux_gramN_g_h)),
            mean_rate_gN_g_h = mean(flux_gramN_g_h), 
            st_dev_g = sd(flux_gramN_g_h),
            variance_g = var(flux_gramN_g_h), 
            se_g = st_dev_g/sqrt(length(flux_gramN_g_h)),
            sample_n = length(flux_gramN_g_h))

######  Moss and Lichen flux calcs  ########
# import moss and lichen percent cover data
X14Plots_S22_UnderstoryMossLichen <- read_csv("C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Plot_level_cover_litter_CWD/14Plots_S22_UnderstoryMossLichen.csv")
cover <- X14Plots_S22_UnderstoryMossLichen
ggplot(cover, aes(x = Site, y = mean.cover.percent, fill = Functional.Group)) + geom_col( position = 'dodge')


# filter out our five high density sites
fiveplot_cover <- cover %>% filter(Site %in% c('BiscBas', 'FirLopSo', 'FounEast', 'GibFalls', 'GravPit'))
ggplot(fiveplot_cover, aes(x = Site, y = mean.cover.percent, fill = Functional.Group)) + geom_col( position = 'dodge')
## These cover measurements include vertical layering in the ground cover but I'm still 
## skeptical of this moss and lichen cover in FounEast. I feel like there is way more than ~1% cover. 

## convert percent cover to area of moss and lichen for each plot
fiveplot_cover <- fiveplot_cover %>% mutate(total_coverage_cm2 = (mean.cover.percent/100) * (50*100)^2) #area of plot in cm2

## rename Site and Functional.group to site and niche in fiveplot cover then merge with seasonal rate sum
fiveplot_cover <- fiveplot_cover %>% rename("site" = "Site",
                                            "niche" = "Functional.Group")
moss_lichen_fluxes <- merge(season_rate_sum_siteless, fiveplot_cover)

## calculate total area of each niche in each plot to then scale up rates 

moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(flux_kgN_.25ha_h = (total_coverage_cm2*mean_gN_cm2_hr)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(sd_kgN_.25ha_h = (total_coverage_cm2*st_dev_cm2)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(var_kgN_.25ha_h = (total_coverage_cm2*variance_cm2)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(SE_kgN_0.25ha_h = (total_coverage_cm2*se_cm2)/1000)

## multiply by 4 for per hectare fluxes, then multiply by total hours in that season (2 months = 1460 hrs)

moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(sd_kgN_ha_season = sd_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(var_kgN_ha_season = var_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(SE_kgN_ha_season = SE_kgN_0.25ha_h*4*1460)


# average across sites
ML_flux_sum <- moss_lichen_fluxes %>% group_by(niche, season) %>% 
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season), var_sum = sum(var_kgN_ha_season), st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

## what does that look like? 
ggplot(ML_flux_sum, aes(x = mean_flux_kgN_ha_season)) + geom_histogram()
season_order <- c("spring", "summer", "summer_wet", "fall")
ggplot(ML_flux_sum, aes(x = factor(season, level = season_order), y = mean_flux_kgN_ha_season, fill = niche)) + 
  geom_col(position = 'dodge') 

## we do see a huge post-rain flux, but this is still a tiny flux (not even 1 kg) relative to the mystery amount

#remove summer_wet and combine into a cumulative growing season flux
ML_summer_flux <- moss_lichen_fluxes %>% filter(season %in% c("summer","summer_wet"))
ML_summer_flux <- ML_summer_flux %>% group_by(niche) %>%
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season),
            var_sum = sum(var_kgN_ha_season),
            st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

ML_spring_fall_flux <- moss_lichen_fluxes %>% filter(season %in% c("fall", "spring")) %>% 
  group_by(season, niche) %>%
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season),
            var_sum = sum(var_kgN_ha_season),
            st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

ML_summer_flux <- ML_summer_flux %>% mutate(season = "summer")

ML_seasonal_flux_final<- rbind(ML_summer_flux, ML_spring_fall_flux)

sum(moss_lichen_fluxes$sample_n)
annual_ML_flux <- ML_seasonal_flux_final %>% group_by(niche) %>% 
  summarize(grow_seas_flux_kg_ha_yr = sum(mean_flux_kgN_ha_season), 
            sd = sqrt(sum(var_sum)))
dodge <- position_dodge(width=0.9)
ggplot(ML_seasonal_flux_final, aes(x = season, y = mean_flux_kgN_ha_season, fill = niche)) + geom_col(position='dodge') + 
  geom_errorbar(aes(ymin = mean_flux_kgN_ha_season-st_err, ymax = mean_flux_kgN_ha_season+st_err), 
                width = 0.25, position = dodge)

## at least this emphasizes the role of lichen and their elevated ability to fix N relative to other cryptics

# site summary
ML_site_sum <- moss_lichen_fluxes %>% select(c("niche","season", "site", "sample_n", "mean_moist", "flux_kgN_ha_season", "SE_kgN_ha_season"))


##### Species specific coverage + updated flux calcs #####
##species specific:
X14Plots_S22_CrustSpeciesCover <- read_csv("C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Plot_level_cover_litter_CWD/14Plots_S22_CrustSpeciesCover.csv")
spec_cover <- X14Plots_S22_CrustSpeciesCover
ggplot(spec_cover, aes(x = Site, y = mean.cover, fill = Species)) + geom_col( position = 'dodge')
colnames(spec_cover)[1] = "site"
colnames(spec_cover)[2] = "species"
colnames(spec_cover)[4] = "niche"

### merge with fluxes
ML_season_rate_sum_siteless <- merge(season_rate_sum_siteless, spec_cover)

### scale cover percent to plot area ocverage
ML_spec_fluxes <- ML_season_rate_sum_siteless %>% mutate(total_coverage_cm2 = (mean.cover/100) * (50*100)^2) #area of plot in cm2


## scale up to plot level flux
ML_spec_fluxes <- ML_spec_fluxes %>% mutate(flux_kgN_.25ha_h = (total_coverage_cm2*mean_gN_cm2_hr)/1000)
ML_spec_fluxes <- ML_spec_fluxes %>% mutate(sd_kgN_.25ha_h = (total_coverage_cm2*st_dev_cm2)/1000)
ML_spec_fluxes <- ML_spec_fluxes %>% mutate(var_kgN_.25ha_h = (total_coverage_cm2*variance_cm2)/1000)

## multiply by 4 for per hectare fluxes, then multiply by total hours in that season (2 months = 1460 hrs)

ML_spec_fluxes <- ML_spec_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_.25ha_h*4*1460)
ML_spec_fluxes <- ML_spec_fluxes %>% mutate(sd_kgN_ha_season = sd_kgN_.25ha_h*4*1460)
ML_spec_fluxes <- ML_spec_fluxes %>% mutate(var_kgN_ha_season = var_kgN_.25ha_h*4*1460)

## average for each season and species
ML_spec_flux_sum <- ML_spec_fluxes %>% group_by(species, season) %>% 
  summarize(mean_moist = mean(mean_moist),
            st_dev_moist = sqrt(variance_moist),
            mean_rate = mean(mean_rate_gN_g_h), 
            rate_st_dev = sqrt(variance_g), 
            mean_flux = mean(flux_kgN_ha_season), 
            st_dev = sqrt(var_kgN_ha_season))

###### SPECIES SPECIFIC WITH MOISTURE 
ML_spec_fluxes_b <- merge(ML_season_rate_sum_siteless, spec_cover, na.rm=T)

### scale cover percent to plot area ocverage
ML_spec_fluxes_b <- ML_spec_fluxes_b %>% mutate(total_coverage_cm2 = (mean.cover/100) * (50*100)^2) #area of plot in cm2


## scale up to plot level flux
ML_spec_fluxes_b <- ML_spec_fluxes_b %>% mutate(flux_kgN_.25ha_h = (total_coverage_cm2*mean_gN_cm2_hr)/1000)
ML_spec_fluxes_b <- ML_spec_fluxes_b %>% mutate(sd_kgN_.25ha_h = (total_coverage_cm2*st_dev_cm2)/1000)
ML_spec_fluxes_b <- ML_spec_fluxes_b %>% mutate(var_kgN_.25ha_h = (total_coverage_cm2*variance_cm2)/1000)

## multiply by 4 for per hectare fluxes, then multiply by total hours in that season (2 months = 1460 hrs)

ML_spec_fluxes_b <- ML_spec_fluxes_b %>% mutate(flux_kgN_ha_season = flux_kgN_.25ha_h*4*1460)
ML_spec_fluxes_b <- ML_spec_fluxes_b %>% mutate(sd_kgN_ha_season = sd_kgN_.25ha_h*4*1460)
ML_spec_fluxes_b <- ML_spec_fluxes_b %>% mutate(var_kgN_ha_season = var_kgN_.25ha_h*4*1460)


## average for B
ML_flux_sum_b <- ML_spec_fluxes %>% group_by(species, season) %>% 
  summarize(mean_moist = mean(mean_moist, na.rm=T),
            mean_rate = mean(mean_rate_gN_g_h,na.rm=T), 
            mean_flux = mean(flux_kgN_ha_season,na.rm=T),
            st_dev = sqrt(sum(var_kgN_ha_season, na.rm=T)))


#### Moss and lichen plots
## graphs
ggplot(ML_spec_flux_sum, aes(x = mean_flux)) + geom_histogram()

ggplot(ML_spec_flux_sum, aes(x = factor(season, level = season_order), y = mean_flux, fill = species)) + 
  geom_col(position = 'dodge') +
  geom_errorbar(aes(ymin = mean_flux-st_dev, ymax = mean_flux+st_dev), width = 0.25, position = dodge) +
  labs(title = "Species-specific N flux from fixation across seasons (spring species missing)") +
  ylab(label = "N flux (kg N ha-1 season-1)") +
  xlab(label = "Season") +
  theme(text = element_text(size=20))+
  theme(legend.position = c(0.9, 0.6))
### can I add moisture to this? 
##### SEE BELOW 

ggplot(ML_flux_sum_b, aes(x = factor(season, level = season_order), y = mean_flux, fill = species)) + 
  geom_col(position = 'dodge') +
  labs(title = "Species-specific N flux from fixation across seasons (spring species missing)") +
  ylab(label = expression(paste("Cryptic N fixation flux (kg N ha"^{-1}, "season"^{-1},")"))) +
  xlab(label = "Season") +
  theme(text = element_text(size=20))+
  theme(legend.position = c(0.9, 0.6))

#### with moisture on X axis 
ggplot(ML_flux_sum_b, aes(x = mean_moist, y = mean_flux, color = species)) +
  geom_point(size=4) +
  geom_errorbar(aes(ymin = mean_flux-st_dev, ymax = mean_flux+st_dev), width = 0.25, position = dodge) +
  labs(title = "Species-specific N flux from fixation across seasons (spring species missing)") +
  ylab(label = "N flux (kg N ha-1 season-1)") +
  xlab(label = "Moisture content") +
  theme(text = element_text(size=20))+
  theme(legend.position = c(0.15, 0.6))

