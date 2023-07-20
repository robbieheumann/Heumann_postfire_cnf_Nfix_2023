## August climate experiment data QC and flux calcs

library(tidyverse)
library(readxl)
setwd("C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Cryptic_fixation_backup/July_August Actuals and Climate Experiment Samples")
# import august climate experiment "cleaned" and "blanks" and "Bag Blanks" sheets

ppm <- read_excel("August_Climate_exp/August Climate Experiment GC data.xlsx",     sheet = "cleaned")

### sample only blank analysis
blanks <- read_excel("August_Climate_exp/August Climate Experiment GC data.xlsx",     sheet = "blanks")

order <- c("5 C", "15 C", "25 C", "35 C")
hist(blanks$ppm)
ggplot(blanks, aes(x = temp, y = ppm, color = moist_level)) + geom_boxplot()

ggplot(blanks, aes(x = niche, y = ppm)) + geom_boxplot() + geom_point()

ggplot(blanks, aes(x = ppm)) + geom_histogram() + facet_wrap(~niche) 

### just group by niche, 
blanks %>% group_by(niche) %>% summarize(mean_ppm = mean(ppm), median_ppm = median(ppm), iqr = IQR(ppm), sd = sd(ppm))

ggplot(blanks, aes(x = factor(temp, levels = order), y = ppm, color = moist_level)) + geom_boxplot() + 
  facet_wrap(~niche)  + geom_point(size=3) + geom_smooth(method='lm', se = TRUE)

blank_lm_sum <- blanks %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(lm(ppm~temp*moist_level, data = blanks)))

## doesn't appear to be much significance in the temp and moist effects within niche

summary(lm(ppm~temp*moist_level, blanks))
plot(lm(ppm~temp*moist_level, blanks))
### seems like maybe there was just an error in or methods 
####  (maybe we mistakenly added c2h2 to some tubes in litter 35)

## therefore just group by niche and use median blank
blank_avg <- blanks %>% group_by(niche) %>% summarize(avg_blank = mean(ppm), median_blank = median(ppm), geomean_blank = exp(mean(log(ppm))))

bagblanks <- read_excel("August_Climate_exp/August Climate Experiment GC data.xlsx", 
                        sheet = "Bag Blanks")

bag_avg <- bagblanks %>% group_by(Bag_ID) %>% summarize(bag_avg = mean(ppm))


ppm_corrected <- left_join(ppm, bag_avg, by = "Bag_ID")
ppm_corrected <- left_join(ppm_corrected, blank_avg, by = c("niche"))

ppm_corrected_update<- ppm_corrected %>% mutate(ppm_corrected_median = ppm-(median_blank+bag_avg), ppm_corrected_geomean = ppm-(geomean_blank+bag_avg), moist = moist_level*100)

write.csv(ppm_corrected_update, "August_Climate_exp/ppm_corrected_update_3_15_2023.csv")
#### copy correct incubation hours into incubation duration column, 
#### CHANGE temp to temp_C,get rid of C in cell values, make sure columns "niche" and "moist" (moist_level*100) are correct
#### in excel then re-import
ppm_corrected_median <- read_csv("August_Climate_exp/ppm_corrected_median.csv")


##import dry mass and clim_exp2_ppm data
clim_drymass <- August_Climate_Experiment_drymasses
clim_ppm <- clim_exp2_ppm
## merge by sample ID
clim_flux_calcs <- merge(clim_ppm, clim_drymass, by = c("niche", "sample_no", "temp_C", "moist"))

### using median corrected
clim_flux_calcs_2 <- merge(ppm_corrected_median, clim_drymass, by = c("niche", "sample_no", "temp_C", "moist"))


## actual moist_percent calcs

clim_flux_calcs <- clim_flux_calcs %>% mutate(drymass = tin_dry-tin_mass, actual_moist = (((tin_wet-tin_mass)-drymass)/drymass)*100)
clim_flux_calcs_2 <- clim_flux_calcs_2 %>% mutate(drymass = tin_dry-tin_mass, actual_moist = (((tin_wet-tin_mass)-drymass)/drymass)*100)
clim_flux_calcs_2 <- clim_flux_calcs_2 %>% select(-...1)
## export this combined data
write.csv(clim_flux_calcs, "C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/July Samples/August_Climate_exp/clim_fluxes.csv")
write.csv(clim_flux_calcs_2, "August_Climate_exp/clim_flux_calcs_2.csv")


######## blank corrections on temp and moisture level ##########
blank_avg_2 <- blanks %>% group_by(niche, temp, moist_level) %>% summarize(avg_blank = mean(ppm))
#### some of those sample only blanks are unbelievably high
#### its possible some of those were Blanks that were initially injected with C2H2 by mistake then let to air out
#### the bag blanks are a bit more realistic, Im debating whether or not I should just remove sample only blanks 
#### it is possible that these blanks are so high due to the microbial production of ethylene during anaerobic 
#### decomposition, all these blanks with super high ppms are in the higher temps in higher moist contents
#### see plot below

ggplot(blank_avg_2, aes(x = factor(moist_level), y  = avg_blank, color = niche)) + geom_point(size=4) + facet_wrap(~temp, )

bag_avg <- bagblanks %>% group_by(Bag_ID) %>% summarize(bag_avg = mean(ppm))

## just bag blank corrected
ppm_corrected_2 <- left_join(ppm, bag_avg, by = "Bag_ID")
ppm_corrected_2<- ppm_corrected_2 %>% mutate(ppm_corrected = ppm-bag_avg)

##import dry mass and clim_exp2_ppm data

clim_drymass <- August_Climate_Experiment_drymasses
clim_ppm <- clim_exp2_ppm
## merge by sample ID
clim_flux_calcs <- merge(clim_ppm, clim_drymass, by = c("niche", "sample_no", "temp_C", "moist"))

### merge ppm_corrected_2 with clim drymass
clim_flux_calcs_v2 <- merge(clim_ppm, clim_drymass, by = c("niche", "sample_no", "temp_C", "moist"))


## actual moist_percent calcs

clim_flux_calcs <- clim_flux_calcs %>% mutate(drymass = tin_dry-tin_mass, actual_moist = (((tin_wet-tin_mass)-drymass)/drymass)*100)

## export this combined data
write.csv(clim_flux_calcs, "C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/July Samples/August_Climate_exp/clim_fluxes.csv")

