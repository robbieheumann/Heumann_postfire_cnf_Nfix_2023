######### Initial Data analysis for cryptic fixer seasonal fluxes ######
## load tidyverse
library(tidyverse)
library(readxl)
### Import cumulative flux calcs from 2022 - "for R" sheet
Flux_Calcs_YNP_2022_seasonals <- read_csv("C:/Users/rh176228/Desktop/cryptic_fixation/seasonal_cryptic_fixation_2/0_Data/final_2022_season_fluxes.csv")
rates <- Flux_Calcs_YNP_2022_seasonals
rates <- rates %>% filter(flux_nmol_C2H4_g_h != 'NA')

## create data frame of just cryptic niches (no rhizo, endo or lupine rates)

cryptic_rates <- rates %>% filter(niche !='endo') 
cryptic_rates <- cryptic_rates %>% filter(niche !='rhizo')
cryptic_rates <- cryptic_rates %>% filter(niche !='lupine')

## initial analysis: graph distributions of rates across seasons and niches in faceted histogram by niche
ggplot(cryptic_rates, aes(x = flux_nmol_C2H4_g_h))+geom_histogram()+facet_wrap(~niche, scales="free")
## super right skewed, will log transform all rates plot distribution again. This creates lots of NA because of negative values bu
ggplot(cryptic_rates, aes(x = log(flux_nmol_C2H4_g_h)))+geom_histogram()+facet_wrap(~niche, scales="free")
## there may be reason to re-check blanks and eliminate some questionable blank averages
## sample only and bag blanks are not unreasonably high between 1.5 and 4 ppm and pretty tightly constrained
## around those values so the negative value variability most likely is differences in natural ethylene production
## in the samples or really tiny rates of acetylene conversion. Therefore anything less than 0 is mostly likely a 0
## N fix rate which makes sense cause these are mostly in the drier sample periods and in the less active niches

## convert every 'flux_nmol_C2H4_g_h' rate less than 0 to zero
cryptic_rates$flux_nmol_C2H4_g_h[cryptic_rates$flux_nmol_C2H4_g_h < 0] <- 0
cryptic_rates$flux_gramN_g_h[cryptic_rates$flux_gramN_g_h < 0] <- 0
cryptic_rates$flux_gN_cm2_hr[cryptic_rates$flux_gN_cm2_hr < 0] <- 0
## now plot histogram
ggplot(cryptic_rates, aes(x = flux_nmol_C2H4_g_h))+geom_histogram()+facet_wrap(~niche, scales = "free")
## log transform again
ggplot(cryptic_rates, aes(x = log(flux_nmol_C2H4_g_h)))+geom_histogram()+facet_wrap(~niche)

## boxplot by niche
ggplot(cryptic_rates, aes(x = niche, y = flux_nmol_C2H4_g_h)) + geom_boxplot() + facet_wrap(~season, scales = "free")

## logged
ggplot(cryptic_rates, aes(x = niche, y = log(flux_nmol_C2H4_g_h))) + geom_boxplot() + facet_wrap(~season, scales = "free")

## distribution of moisture percent
ggplot(cryptic_rates, aes(x = moist)) + geom_histogram()

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

## facet wrap by niche
ggplot(cryptic_rates, aes(x = moist, y = flux_nmol_C2H4_g_h)) + geom_point() + 
  facet_wrap(~niche, scales = "free_y") +
  geom_smooth(method = 'lm')

## log rates then plot again
ggplot(cryptic_rates, aes(x = moist, y = log(flux_nmol_C2H4_g_h))) + geom_point() + 
  facet_wrap(~niche, scales = "free_y") +
  geom_smooth(method = 'lm')

## log rates and moisture then plot again
ggplot(cryptic_rates, aes(x = log(moist), y = log(flux_nmol_C2H4_g_h))) + geom_point() + 
  facet_wrap(~niche, scales = "free") +
  geom_smooth(method = 'lm')

## there is so much variability, so many zeros... relationships are hard to discern. 

## curious as to what the lichen data looks like across lichen species, same for moss
lichen_rates <- cryptic_rates %>% filter(niche == "Lichen")
ggplot(lichen_rates, aes(x = moist, y = flux_nmol_C2H4_g_h, color = species)) + geom_point(size = 5)

## just cladonias: 
cladonia_rates <- cryptic_rates %>% filter(species %in% c("clafri", "clacar", "peca/clafri"))
cladonia_rates$flux_nmol_C2H4_g_h[cladonia_rates$flux_nmol_C2H4_g_h < 0] <- 0

ggplot(cladonia_rates, aes(x = moist, y = flux_nmol_C2H4_g_h, color = species)) + 
  geom_point(size = 5)+
  ylab(label = expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab(label = "Moisture Content (% w/w)") +
  theme(text = element_text(size=18))
## five sample shave non negative rates... none of them are peca so there is some evidence that these things fix
## all of those rates are in the summer, both wet and dry though which is interesting

## just peltigera
pelti_rates <- cryptic_rates %>% filter(species %in% c("peap", "peca"))
pelti_rates$flux_nmol_C2H4_g_h[pelti_rates$flux_nmol_C2H4_g_h < 0] <- 0
ggplot(pelti_rates, aes(x = moist, y = log(flux_nmol_C2H4_g_h+0.0001), color = species)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm')

## lm model for above
ggplot(pelti_rates, aes(x = flux_nmol_C2H4_g_h))  +geom_histogram() ## skewed

pelti_rates %>%
  group_by(species) %>%
  group_modify(~broom::glance(lm(log(flux_nmol_C2H4_g_h+0.0001)~moist, data=.)))

## active pelti rates
active_pelti <- pelti_rates %>% filter(flux_nmol_C2H4_g_h>0)
ggplot(active_pelti, aes(x = moist, y = log(flux_nmol_C2H4_g_h), color = species)) + geom_point(size = 5) + 
  geom_smooth(method = 'lm') +
  ylab(label = expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab(label = "Moisture Content (% w/w)") +
  theme(text = element_text(size=18))

active_pelti %>%
  group_modify(~broom::glance(lm(log(flux_nmol_C2H4_g_h)~moist, data=.)))


### how about moss?
moss_rates <- cryptic_rates %>% filter(niche == "Moss")
active_moss <- moss_rates %>% filter(flux_nmol_C2H4_g_h>0)
ggplot(moss_rates, aes(x = moist, y = flux_nmol_C2H4_g_h, color = species)) + geom_point(size = 5)
ggplot(active_moss, aes(x = moist, y = log(flux_nmol_C2H4_g_h+1), color = species)) + 
  geom_point(size = 5) +
  geom_smooth(method='lm', se=F, linetype=2)+
  ylab(label = expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab(label = "Moisture Content (% w/w)") +
  theme(text = element_text(size=18))

active_moss %>%
  group_by(species) %>%
  group_modify(~broom::glance(lm(log(flux_nmol_C2H4_g_h)~moist, data=.)))

moss_rates_known <- moss_rates %>% filter(species !="NA")
ggplot(moss_rates_known, aes(x = moist, y = flux_nmol_C2H4_g_h, color = species)) + geom_point(size = 5)

## Group data by season to compare spring, summer (dry and wet) and fall rates across niches. 
## Ultimately I want to create a bar graph of average rates colored and separated by niche, for each season. 
## so first, create summary table of niche averages and standard deviations across each season. 
## for the graph. Call this table "season_rate_sum"

season_rate_sum <- cryptic_rates %>% group_by(season, site, niche) %>% 
  summarize(mean_moist = mean(moist, na.rm=T), 
            variance_moist = var(moist, na.rm=T),
            mean_gN_cm2_hr = mean(flux_gN_cm2_hr),
            st_dev_cm2 = sd(flux_gN_cm2_hr),
            variance_cm2 = var(flux_gN_cm2_hr),
            mean_rate_gN_g_h = mean(flux_gramN_g_h), 
            st_dev_g = sd(flux_gramN_g_h),
            variance_g = var(flux_gramN_g_h),
            sample_n = length(flux_gramN_g_h))

season_rate_sum <- season_rate_sum %>% mutate(st_err_cm2 = st_dev_cm2/sqrt(sample_n), 
                                              st_err_g = st_dev_g/sqrt(sample_n))

## group with species
season_rate_sum_siteless <- cryptic_rates %>% group_by(season, niche, species) %>% 
  summarize(mean_moist = mean(moist, na.rm=T), 
            variance_moist = var(moist, na.rm=T),
            mean_gN_cm2_hr = mean(flux_gN_cm2_hr),
            st_dev_cm2 = sd(flux_gN_cm2_hr),
            variance_cm2 = var(flux_gN_cm2_hr),
            mean_rate_gN_g_h = mean(flux_gramN_g_h), 
            st_dev_g = sd(flux_gramN_g_h),
            variance_g = var(flux_gramN_g_h), 
            sample_n = length(flux_gramN_g_h))

ML_season_rate_sum_siteless <- season_rate_sum_siteless %>% filter(niche %in% c("Moss", "Lichen"))

dodge <- position_dodge(width=0.9)

ggplot(season_rate_sum, aes(x = season, y = mean_rate_gN_g_h, fill = niche)) + geom_col(position = dodge) 

## filter out lupine, rhizo and endo data into their own tables
lupine_rates <- rates %>% filter(niche == "lupine")
rhizo_endo_rates <- rates %>% filter(niche %in% c("endo", "rhizo"))
season_rate_sum <- season_rate_sum %>% filter(niche != "lupine")
season_rate_sum <- season_rate_sum %>% filter(niche != "rhizo")
season_rate_sum <- season_rate_sum %>% filter(niche != "endo")

## create column graph of rates across niches with season on x axis and nmol ethylene rate on the y
ggplot(season_rate_sum, aes(x = season, y = mean_rate_gN_g_h, fill = niche)) + geom_col( position = "dodge")

# re-order x axis (spring - summer - fall)
season_order <- c('spring', 'summer', 'summer_wet', 'fall')
ggplot(season_rate_sum, aes(x = factor(season, level = season_order), y = mean_rate_gN_g_h, fill = niche)) +
  geom_col(position = "dodge")

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
fiveplot_cover <- fiveplot_cover %>% rename("site" = "site",
                          "niche" = "Functional.Group")
moss_lichen_fluxes <- merge(season_rate_sum, fiveplot_cover)

## calculate total area of each niche in each plot to then scale up rates 

moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(flux_kgN_.25ha_h = (total_coverage_cm2*mean_gN_cm2_hr)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(sd_kgN_.25ha_h = (total_coverage_cm2*st_dev_cm2)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(var_kgN_.25ha_h = (total_coverage_cm2*variance_cm2)/1000)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(SE_kgN_.25ha_h = (total_coverage_cm2*st_err_cm2)/1000)

## multiply by 4 for per hectare fluxes, then multiply by total hours in that season (2 months = 1460 hrs)

moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(sd_kgN_ha_season = sd_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(var_kgN_ha_season = var_kgN_.25ha_h*4*1460)
moss_lichen_fluxes <- moss_lichen_fluxes %>% mutate(SE_kgN_ha_season = SE_kgN_.25ha_h*4*1460)


# average across sites
ML_flux_sum <- moss_lichen_fluxes %>% group_by(niche, season) %>% 
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season), var_sum = sum(var_kgN_ha_season), st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

## what does that look like? 
ggplot(ML_flux_sum, aes(x = mean_flux_kgN_ha_season)) + geom_histogram()

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
ML_spec_fluxes <- merge(season_rate_sum, spec_cover)

### scale cover percent to plot area ocverage
ML_spec_fluxes <- ML_spec_fluxes %>% mutate(total_coverage_cm2 = (mean.cover/100) * (50*100)^2) #area of plot in cm2


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
            rate_st_dev = sqrt(mean(variance_g)), 
            mean_flux = mean(flux_kgN_ha_season), 
            st_dev = sqrt(mean(var_kgN_ha_season)))

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
  geom_errorbar(aes(ymin = mean_flux-st_dev, ymax = mean_flux+st_dev), width = 0.25, position = dodge) +
  labs(title = "Species-specific N flux from fixation across seasons (spring species missing)") +
  ylab(label = "N flux (kg N ha-1 season-1)") +
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

##### Wood flux calcs ######
## import wood biomass data
X14Plots_S22_CoarseWoodyDebrisBiomass <- read_csv("C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Plot_level_cover_litter_CWD/14Plots_S22_CoarseWoodyDebrisBiomass.csv")
CWD <- X14Plots_S22_CoarseWoodyDebrisBiomass

## filter out five high density focal sites
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
wood_fluxes <- wood_fluxes %>% mutate(sd_kgN_ha_h = ((g_biomass_4.5.CWD*st_dev_g)/1000)*4)
wood_fluxes <- wood_fluxes %>% mutate(var_kgN_ha_h = ((g_biomass_4.5.CWD*variance_g)/1000)*4)
wood_fluxes <- wood_fluxes %>% mutate(SE_kgN_ha_h = ((g_biomass_4.5.CWD*st_err_g)/1000)*4)

wood_fluxes <- wood_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_ha_h*1460)
wood_fluxes <- wood_fluxes %>% mutate(var_kgN_ha_season = var_kgN_ha_h*1460)
wood_fluxes <- wood_fluxes %>% mutate(SE_kgN_ha_season = SE_kgN_ha_h*1460)

## what does that look like
mean_wood_fluxes <- wood_fluxes %>% group_by(season) %>% 
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season), st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))
ggplot(mean_wood_fluxes, aes(x = factor(season,levels=season_order), y = mean_flux_kgN_ha_season)) + geom_col()

mean_wood_fluxes_withdeadwood <- wood_fluxes %>% group_by(season) %>% 
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season), variance = var(flux_kgN_ha_season), Mg_deadwood_ha = bm_1000h_Class45_Mg_ha)

## combine summer measurements
wood_summer_flux <- wood_fluxes %>% filter(season %in% c("summer","summer_wet"))
wood_summer_flux <- wood_summer_flux %>% group_by(niche) %>%
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season),
            var_sum = sum(var_kgN_ha_season),
            st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

wood_spring_fall_flux <- wood_fluxes %>% filter(season %in% c("fall", "spring")) %>% 
  group_by(season, niche) %>%
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season),
            var_sum = sum(var_kgN_ha_season),
            st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

wood_summer_flux <- wood_summer_flux %>% mutate(season = "summer")

wood_seasonal_flux_final<- rbind(wood_summer_flux, wood_spring_fall_flux)
ggplot(wood_seasonal_flux_final, aes(x = season, y = mean_flux_kgN_ha_season)) + geom_col(position='dodge') + 
  geom_errorbar(aes(ymin = mean_flux_kgN_ha_season-st_err, ymax = mean_flux_kgN_ha_season+st_err), 
                width = 0.25, position = dodge)

## site summary
wood_site_sum <- wood_fluxes %>% select(c("season", "niche", "site", "sample_n", "mean_moist", "flux_kgN_ha_season", "SE_kgN_ha_season"))


## average across sites
mean_season_wood_flux <- wood_site_sum %>% group_by(season) %>% summarize(mean_flux = mean(flux_kgN_ha_season))

## what does that look like? 
ggplot(mean_season_wood_flux, aes(x = factor(season, level = season_order), y = mean_flux)) + geom_col()

##### Soil Flux Calcs ######

## calculate average bulk density for each plot (column of soil sample  = 4.909*2)
soil_fluxes <- cryptic_rates %>% filter(niche == "Soil")
soil_fluxes <- soil_fluxes %>% group_by(season, site) %>% 
  summarize(mean_rate = mean(flux_gramN_g_h),
            stdev = sd(flux_gramN_g_h),
            variance_g = var(flux_gramN_g_h),
            mean_BD = mean(dry_mass/(4.909*2)),
            st_err = sd(flux_gramN_g_h/sqrt(length(flux_gramN_g_h))),
            sample_n = length(flux_gramN_g_h),
            mean_moist =mean(moist))


## add plot soil volume (to a depth of 10cm)
soil_fluxes <- soil_fluxes %>% mutate(plot_soil_vol = 25000000*10)

## calculate mass of soil per plot
soil_fluxes <- soil_fluxes %>% mutate(plot_soil_mass_g = plot_soil_vol*mean_BD)

## now multiply that by the mean rate
soil_fluxes <- soil_fluxes %>% 
  mutate(flux_kgN_plot_hr = (plot_soil_mass_g*mean_rate)/1000, var_kgN_plot_hr = plot_soil_mass_g*variance_g/1000,
         st_err = plot_soil_mass_g*st_err/1000)

## scale to per ha and seasonal flux
soil_fluxes <- soil_fluxes %>% mutate(flux_kgN_ha_season = flux_kgN_plot_hr*4*1460)
soil_fluxes <- soil_fluxes %>% mutate(var_kgN_ha_season = var_kgN_plot_hr*4*1460)
soil_fluxes <- soil_fluxes %>% mutate(st_err_kgN_ha_season = st_err*4*1460)

## plot
ggplot(soil_fluxes, aes(x = site, y = flux_kgN_ha_season, fill = season)) + geom_col(position = 'dodge')

# site summaries
soil_site_sum <- soil_fluxes %>% select(c("season", "site", "sample_n", "mean_moist", "flux_kgN_ha_season", "st_err_kgN_ha_season"))
soil_site_sum <- rename(soil_site_sum, SE_kgN_ha_season = st_err_kgN_ha_season)
soil_site_sum <- soil_site_sum %>% mutate(niche = "Soil")
## combine summer measurements
soil_summer_flux <- soil_fluxes %>% filter(season %in% c("summer","summer_wet"))
soil_summer_flux <- soil_summer_flux %>% ungroup() %>%
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season),
            var_sum = sum(var_kgN_ha_season),
            st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

soil_spring_fall_flux <- soil_fluxes %>% filter(season %in% c("fall", "spring")) %>% 
  group_by(season) %>%
  summarize(mean_flux_kgN_ha_season = mean(flux_kgN_ha_season),
            var_sum = sum(var_kgN_ha_season),
            st_err = sd(flux_kgN_ha_season)/sqrt(sum(sample_n)))

soil_summer_flux <- soil_summer_flux %>% mutate(season = "summer")

soil_seasonal_flux_final<- rbind(soil_summer_flux, soil_spring_fall_flux)
soil_seasonal_flux_final <- soil_seasonal_flux_final %>% mutate(niche = "Soil")
###### Litter TBD WAITING ON MASS DATA ######

###########################Combined Flux Analyses and Plots############################################################################################################


###### Combine all fluxes ####### make a consistent data sheet of just season, niche, kgN_ha_season and st_dev_kgN_ha_season
fluxes_combined_4season <- rbind(wood_site_sum, soil_site_sum, ML_site_sum)
### average the two summer measurements
fluxes_combined_4season$season[fluxes_combined_4season$season == 'summer_wet'] <- 'summer'
##### NEED TO FIX STANDARD ERROR BELOW HERE 
fluxes_combined_3season <- fluxes_combined_4season %>% group_by(season, niche, site) %>% summarise(sample_n = sum(sample_n), 
                                                          flux_kgN_ha_season = mean(flux_kgN_ha_season),
                                                          SE_kgN_ha_season = mean(SE_kgN_ha_season),
                                                          mean_moist = mean(mean_moist))
fluxes_combined_4season <- rbind(wood_site_sum, soil_site_sum, ML_site_sum)


flux_sums_combined <- rbind(soil_seasonal_flux_final, ML_seasonal_flux_final, wood_seasonal_flux_final)

## what does that look like? 
ggplot(flux_sums_combined, aes(x = factor(season, levels=season_order), y = mean_flux_kgN_ha_season, fill = niche)) + 
  geom_col(position = dodge) +
  geom_errorbar(aes(ymin = mean_flux_kgN_ha_season-st_err, ymax = mean_flux_kgN_ha_season+st_err), 
                width = 0.25, position = dodge) + 
  labs(title = "N fluxes from cryptic fixation across seasons") +
  ylab(label = "Cryptic N fixation flux (kg N ha-1 season-1)") + 
  xlab(label = "Season") +
  theme(text = element_text(size=20))

### this is kind of cool actually, because this doesnt include litter, litter may add up to another 0.05 Kg/ha to these measurements,
### but even without litter, this adds up to about 0.18 kg of N per hectare per year, almost entirely coming from the spring, however an additional 0.05 kg
### could accumulate during summer if every day in the summer experienced the same condition we did that one day post-rain. And that rain wasnt that big of a rain
### plus we were a little late, by the time we got to the last plot, much of it had dried out so had we sampled in the rain the day before fluxes might be 
### even higher


##### RHIZO AND ENDO DATA ######
ggplot(rhizo_endo_rates, aes(x = niche, y = flux_gramN_g_h)) + geom_boxplot()

## No nitrogenase activity detected!

ggplot(lupine_rates, aes(x = site, y = flux_gramN_g_h)) + geom_boxplot()

ggplot(lupine_rates, aes(x = dry_mass, y = flux_gramN_g_h, color = site)) + geom_point(size = 6) + 
  theme(text = element_text(size = 20))

ggplot(lupine_rates, aes(x = moist, y = flux_gramN_g_h, color = site)) + geom_point(size = 4)


#### total plot combined niche across season excluding summer_wet
## cumulative inputs
cryptic_fluxes <- flux_sums_combined %>% group_by(season) %>% 
  summarize(mean_cryptic_flux = sum(mean_flux_kgN_ha_season), st_dev = sqrt(sum(variance)))

ggplot(cryptic_fluxes, aes(x = factor(season, levels = season_order), y = mean_cryptic_flux)) + geom_col(position = dodge) +
  geom_errorbar(aes(ymin = mean_cryptic_flux-st_dev, ymax = mean_cryptic_flux+st_dev), 
                width = 0.25, position = dodge)

## take out summer wet
cryptic_fluxes_3 <- cryptic_fluxes %>% filter(season != "summer_wet")
ggplot(cryptic_fluxes_3, aes(x = factor(season, levels = season_order), y = mean_cryptic_flux)) + geom_col(position = dodge) +
  geom_errorbar(aes(ymin = mean_cryptic_flux-st_dev, ymax = mean_cryptic_flux+st_dev), 
                width = 0.25, position = dodge) +
  labs(title = "Total N fluxes from combined cryptic fixation across seasons") +
  ylab(label = "Cryptic N fixation flux (kg N ha-1 season-1)") +
  xlab(label = "Season") +
  theme(text = element_text(size=20))

## dry vs wet day comparison of hourly fluxes logged
dry_v_wet <- cryptic_rates %>% filter(season %in% c("summer_wet", "summer"))
dry_v_wet$season[dry_v_wet$season == "summer"] <- "before rain"
dry_v_wet$season[dry_v_wet$season == "summer_wet"] <- "after rain event"

dry_v_wet <- dry_v_wet %>% mutate(log.flux_N = log(flux_gramN_g_h))

ggplot(dry_v_wet, aes(x = flux_gramN_g_h)) + geom_histogram() + facet_wrap(~season)
ggplot(dry_v_wet, aes(x = log.flux_N)) + geom_histogram() + facet_wrap(~season)
ggplot(dry_v_wet, aes(x = niche, y = log.flux_N)) + geom_boxplot() + facet_wrap(~season)

dry_v_wet_sum <- dry_v_wet %>% group_by(season, niche) %>%
  summarize(mean_hourly_flux = mean(flux_gramN_g_h), sd_N = sd(flux_gramN_g_h), 
            log_mean_hourly_flux_N = mean(log.flux_N), log_sd = sd(log.flux_N), 
            mean_flux_C2H4 = mean(flux_nmol_C2H4_g_h),
            sd_C2H4 = sd(flux_nmol_C2H4_g_h))
ggplot(dry_v_wet_sum, aes(x = season, y = mean_hourly_flux, fill = niche)) + geom_col(position = dodge) +
  geom_errorbar(aes(ymin = mean_hourly_flux-sd_N, ymax = mean_hourly_flux+sd_N), 
                width = 0.25, position = dodge)

### N fix rates across seasons in relation to sample moisture content
cryptic_rates_red <- cryptic_rates %>% filter(moist < 700)
ggplot(cryptic_rates_red, aes(x = moist, y  = flux_nmol_C2H4_g_h, color = niche)) + geom_point()+
  facet_wrap(~season, scales = "free") + 
  geom_smooth(method = 'loess', se = F)

## separate out heterotrophs and autorophs
hetero_rates <- cryptic_rates_red %>% filter(niche %in% c("Litter", "Soil", "Wood"))
auto_rates <- cryptic_rates_red %>% filter(niche %in% c("Lichen", "Moss"))

## heterotrophs
ggplot(hetero_rates, aes(x = moist, y  = flux_nmol_C2H4_g_h, color = niche)) + geom_point()+
  facet_wrap(~season, scales = "free") + 
  geom_smooth(method = 'lm', se = F) +
  labs(title = "Rates of C2H4 production in cryptic fixers in relation to moisture content") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Moisture Content (%w/w)") +
  theme(text = element_text(size=20))

## autotrophs
ggplot(auto_rates, aes(x = moist, y  = flux_nmol_C2H4_g_h, color = niche)) + geom_point()+
  facet_wrap(~season, scales = "free") + 
  geom_smooth(method = 'lm', se = F) +
  labs(title = "Rates of C2H4 production in cryptic fixers in relation to moisture content") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Moisture Content (%w/w)") +
  theme(text = element_text(size=20))

## ethylene comparisons: SEPARATE BETWEEN HETEROTROPHS AND AUTOTROPHS
ML_dry_v_wet <- dry_v_wet_sum %>% filter(niche %in% c("Lichen", "Moss"))
hetero_dry_v_wet <- dry_v_wet_sum %>% filter(niche %in% c("Litter", "Soil", "Wood"))
rain_order <- c("before rain", "after rain event")
ggplot(dry_v_wet_sum, aes(x = factor(season, levels = rain_order), y = mean_flux_C2H4, fill = niche)) + geom_col(position = dodge)+
  geom_errorbar(aes(ymin = mean_flux_C2H4-sd_C2H4, ymax = mean_flux_C2H4+sd_C2H4), 
                width = 0.25, position = dodge)+
  labs(title = "Rates of C2H4 production in cryptic fixers after a summer rain") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  theme(text = element_text(size=20), 
        axis.title.x = element_blank())

##Autotrophs
ggplot(ML_dry_v_wet, aes(x = factor(season, levels = rain_order), y = mean_flux_C2H4, fill = niche)) + geom_col(position = dodge)+
  geom_errorbar(aes(ymin = mean_flux_C2H4-sd_C2H4, ymax = mean_flux_C2H4+sd_C2H4), 
                width = 0.25, position = dodge)+
  labs(title = "Rates of C2H4 production in cryptic fixers after a summer rain") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  theme(text = element_text(size=20), 
        axis.title.x = element_blank())

##Heterotrophs
ggplot(hetero_dry_v_wet, aes(x = factor(season, levels = rain_order), y = mean_flux_C2H4, fill = niche)) + geom_col(position = dodge)+
  geom_errorbar(aes(ymin = mean_flux_C2H4-sd_C2H4, ymax = mean_flux_C2H4+sd_C2H4), 
                width = 0.25, position = dodge)+
  labs(title = "Rates of C2H4 production in cryptic fixers after a summer rain") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  theme(text = element_text(size=20), 
        axis.title.x = element_blank())
ggplot(dry_v_wet, aes(x = niche, y = log(1+flux_nmol_C2H4_g_h))) + geom_boxplot() + facet_wrap(~season, scales = 'free_y')

## compare niche fluxes vs niche biomass across plots
## combine wood, litter, percent cover moss and lichen into one data sheet with mean fluxes 
## sum inputs then plot against biomass
plot_level_wood_inputs <- wood_fluxes %>% group_by(site) %>% 
  summarize(mean_annual_input = sum(flux_kgN_ha_season), deadwood_biomass_Mg_ha = bm_1000h_Class45_Mg_ha)
plot_level_wood_inputs <- plot_level_wood_inputs %>% group_by(site) %>% 
  summarize(mean_annual_input = mean(mean_annual_input), deadwood_biomass_Mg_ha = deadwood_biomass_Mg_ha)


## plot wood
ggplot(plot_level_wood_inputs, aes(x=deadwood_biomass_Mg_ha, y = mean_annual_input,)) + geom_point(size = 4) + 
  geom_text(aes(label=site), nudge_x =0, nudge_y =0.01)  +geom_smooth(method = 'lm', se = T)


### Moss and Lichen fixation rate by coverage
ML_flux_cover_sum <- moss_lichen_fluxes %>% group_by(niche, site) %>% 
  summarize(mean.cover.perc = mean(mean.cover.percent), mean_flux_kgN_ha_yr = sum(flux_kgN_ha_season), st_dev = sum(sd_kgN_ha_season))
ggplot(ML_flux_cover_sum, aes(x = mean.cover.perc, y = mean_flux_kgN_ha_yr, color = niche, label= site)) + geom_point(size= 4) + 
  geom_smooth(method = 'lm', se = T) + 
  geom_text(aes(label=ifelse(mean_flux_kgN_ha_yr >0.1,as.character(site), ''), hjust=1.25, vjust=1))



### just curious to see what the relationship between soil moisture and litter moisture is
wet_soilandlitter <- cryptic_rates %>% filter(season %in% c("spring","summer_wet")) %>% filter(niche %in% c("Soil", "Litter"))
wet_soil <- wet_soilandlitter %>% filter(niche == "Soil") %>% select(c(moist, site, quad))
wet_litter <- wet_soilandlitter %>% filter(niche == "Litter") %>% select(moist, site, quad)

colnames(wet_soil)[1] = "soil_m"
colnames(wet_litter)[1] = "litter_m"
wet_sl <- merge(wet_litter, wet_soil)
wet_sl <- wet_sl %>% filter(soil_m < 700)
ggplot(wet_sl, aes(x = soil_m, y = litter_m)) + geom_point() +
  geom_smooth(method = 'lm')
wet_sl_lm <- lm(soil_m~litter_m, wet_sl)
wet_sl_lm
summary(wet_sl_lm)


## just spring
spring_SL <- cryptic_rates %>% filter(season %in% c("spring")) %>% filter(niche %in% c("Soil", "Litter"))
spring_soil <- spring_SL %>% filter(niche == "Soil") %>% select(c(moist, site, quad))
spring_litter <- spring_SL %>% filter(niche == "Litter") %>% select(moist, site, quad)

colnames(spring_soil)[1] = "soil_m"
colnames(spring_litter)[1] = "litter_m"
spring_sl <- merge(spring_litter, spring_soil)
spring_sl <- spring_sl %>% filter(soil_m < 700)
ggplot(spring_sl, aes(x = soil_m, y = litter_m)) + geom_point() +
  geom_smooth(method = 'lm')
spring_sl_lm <- lm(soil_m~litter_m, spring_sl)
spring_lm
summary(spring_sl_lm)

## just summer wet up
rain_SL <- cryptic_rates %>% filter(season %in% c("summer_wet")) %>% filter(niche %in% c("Soil", "Litter", "Wood"))
rain_soil <- rain_SL %>% filter(niche == "Soil") %>% select(c(moist, site, quad))
rain_litter <- rain_SL %>% filter(niche == "Litter") %>% select(moist, site, quad)
rain_wood <- rain_SL %>% filter(niche == "Wood") %>% select(moist, site, quad)

colnames(rain_soil)[1] = "soil_m"
colnames(rain_litter)[1] = "litter_m"
colnames(rain_wood)[1] = "wood_m"
rain_sl <- merge(rain_litter, rain_soil)
rain_sl <- merge(rain_sl, rain_wood)
rain_sl <- rain_sl %>% filter(soil_m < 700)
ggplot(rain_sl, aes(x = soil_m, y = litter_m)) + geom_point() +
  geom_smooth(method = 'lm')+
  labs(title = "Litter moisture in relation to soil mositure following rain event") +
  ylab(label = "Litter mositure (% w/w)") +
  xlab(label = "Soil Moisture (% w/w)")+
  theme(text = element_text(size=20))

rain_sl_lm <- lm(soil_m~litter_m, rain_sl)
rain_sl_lm
summary(rain_sl_lm)

## soil x wood
ggplot(rain_sl, aes(x = soil_m, y = wood_m)) + geom_point() +
  geom_smooth(method = 'lm')+
  labs(title = "Deadwood moisture in relation to soil mositure following rain event") +
  ylab(label = "Deadwood mositure (% w/w)") +
  xlab(label = "Soil Moisture (% w/w)")+
  theme(text = element_text(size=20))

rain_sw_lm <- lm(soil_m~wood_m, rain_sl)
plot(rain_sw_lm)
summary(rain_sw_lm)

################################ Models ########################################################################################################################################

library(broom)
summary(fluxes_combined)

###### Start Trash #####
### one way anovas across seasons for each niche
one.way.niches <- fluxes_combined %>% 
  group_by(niche) %>% 
  group_modify(~broom::tidy(TukeyHSD(aov(flux_kgN_ha_season~season, data = .x))))

one.way.niches.table <- one.way.niches %>% group_by(niche) %>% mutate(tukey = ifelse(adj.p.value<0.05, "a", ifelse(adj.p.value < 0.1, "b","")))

## think I have to do them individually
## wood season one way anova
wood_season_aov <- aov(flux_kgN_ha_season ~ season, data = wood_site_sum)
summary(wood_season_aov)
tuk_wood <- TukeyHSD(wood_season_aov)
CLD_season <- glht(wood_season_aov, linfct = mcp(adj.p.value = "Tukey"))


two.way.flux_season_niche <- aov(flux_kgN_ha_season~season+niche, data = fluxes_combined)
summary(two.way.flux_season_niche)

install.packages("AICcmodavg")
library(AICcmodavg)

plot(two.way.flux_season_niche)

tukey.two.way<-TukeyHSD(two.way.flux_season_niche)

tukey.two.way

## add these results to the season flux graph
install.packages("multcomp")
library(multcomp)
season_niche.tukey <- glht(two.way.flux_season_niche, linfct=mcp(niche="Tukey"))
cld(season_niche.tukey)


ggplot(flux_sums_combined, aes(x = factor(season, levels=season_order), y = mean_flux_kgN_ha_season, fill = niche)) + 
  geom_col(position = dodge) +
  geom_errorbar(aes(ymin = mean_flux_kgN_ha_season-st_err, ymax = mean_flux_kgN_ha_season+st_err), 
                width = 0.25, position = dodge) + 
  labs(title = "N fluxes from cryptic fixation across seasons") +
  ylab(label = "Cryptic N fixation flux (kg N ha-1 season-1)") + 
  xlab(label = "Season") +
  theme(text = element_text(size=20)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
#### HOW THE FUCK DO I SHOW THE TUKEY LETTERS???? #### ^^ this is all trash
##### End Trash #####


#### Seasonal GLS analysis ####
## import nlme
library(nlme)
library(lme4)
## flux data for analyses are fluxes_combined_3season and ...4_season
## ideally a biomass component is added to the above data sheets ^ for both the niches and tree
## biomass and or soil chemistry data as well. 

#### 3 season gls ####
ggplot(fluxes_combined_3season, aes(x = flux_kgN_ha_season)) + geom_histogram()
ggplot(fluxes_combined_3season, aes(x = log(flux_kgN_ha_season))) + geom_histogram()
### log transform inputs for gls

#### add 0.000001 to each flux to eliminate 0s for log transform
fluxes_combined_3season <- fluxes_combined_3season %>% mutate(log.fluxes = log(flux_kgN_ha_season+0.000001))

three_seas.gls <- gls(log.fluxes ~ season + niche, data=fluxes_combined_3season, method = "REML")
anova(three_seas.gls, type = c("marginal"))
summary(three_seas.gls)

three_seas_int.gls <- gls(log.fluxes ~ season*niche, data=fluxes_combined_3season, method = "REML")
summary(three_seas_int.gls)
anova(three_seas_int.gls, type = c("marginal"))
qqnorm(three_seas_int.gls)
qqline(three_seas_int.gls)

anova(three_seas.gls, three_seas_int.gls)
### interaction is better

## plot the three season fluxes
ggplot(fluxes_combined_3season, aes(x = site, y = flux_kgN_ha_season, fill = niche)) + 
  facet_wrap(~factor(season, levels = c("spring", "summer", "fall"))) + 
  geom_col(position = dodge) +
  geom_errorbar(aes(ymin = flux_kgN_ha_season-SE_kgN_ha_season, ymax = flux_kgN_ha_season+SE_kgN_ha_season), 
                width = 0.25, position = dodge) + 
  labs(title = "N fluxes from cryptic fixation across seasons") +
  ylab(label = "Cryptic N fixation flux (kg N ha-1 season-1)") + 
  xlab(label = "Season") +
  theme(text = element_text(size=15)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  theme(legend.position = c(0.15, 0.7))

### combine the plots


#### 4 season gls ####

ggplot(fluxes_combined_4season, aes(x = flux_kgN_ha_season)) + geom_histogram()
ggplot(fluxes_combined_4season, aes(x = log(flux_kgN_ha_season))) + geom_histogram()
### log transform inputs for gls

#### add 0.000001 to each flux to eliminate 0s for log transform
fluxes_combined_4season <- fluxes_combined_4season %>% mutate(log.fluxes = log(flux_kgN_ha_season+0.000001))

four_seas.gls <- gls(log.fluxes ~ season + niche, data=fluxes_combined_4season, method = "REML")
anova(four_seas.gls, type = c("marginal"))
summary(four_seas.gls)
qqnorm(four_seas.gls)

four_seas_int.gls <- gls(log.fluxes ~ season*niche, data=fluxes_combined_4season, method = "REML")
summary(four_seas_int.gls)
anova(four_seas_int.gls, type = c("marginal"))
qqnorm(four_seas_int.gls)
qqline(four_seas_int.gls)

anova(four_seas.gls, four_seas_int.gls)


## Lower AICs for both season models that include niche*season interaction indicate
## these models are stronger, interesting the 3 season interaction model had the lowest AIC

# four season critical F 
qf(0.05, 80, 73)



