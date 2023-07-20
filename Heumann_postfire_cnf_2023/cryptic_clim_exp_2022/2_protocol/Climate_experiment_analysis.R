##### Climate Experiment #####
#### Initial Data Analysis #####

library(tidyverse)
library(readxl)
getwd()
#### Import "final_clim_exp_fluxes" from incremental folder in git
Combined_clim_exp_flux_calcs <- read_csv("C:/Users/rh176228/Desktop/cryptic_fixation/cryptic_clim_exp_2022/1_incremental/final_clim_exp_fluxes.csv")

fluxes <- Combined_clim_exp_flux_calcs

round1 <- fluxes %>% filter(round == "1")
round2 <- fluxes %>% filter(round == "2")

### initial analysis: 
## plot distributions of fluxes across niche in both rounds

ggplot(round1, aes(x = flux_nmol_C2H4_g_hr)) + geom_histogram() + facet_wrap(~niche, scales = "free")
## pretty much always zero

ggplot(round1, aes(x = flux_gN_g_h)) + geom_histogram() + facet_wrap(~niche, scales = "free")

## logged...
ggplot(round1, aes(x = log(flux_nmol_C2H4_g_hr))) + geom_histogram() + facet_wrap(~niche)

## make negative fluxes zero
round1$flux_nmol_C2H4_g_hr[round1$flux_nmol_C2H4_g_hr < 0] <- 0

## plot again
ggplot(round1, aes(x = log(flux_nmol_C2H4_g_hr))) + geom_histogram(na.rm = TRUE) + 
  facet_wrap(~niche)

## boxplot
ggplot(round1, aes(x = factor(temp), y = flux_nmol_C2H4_g_hr)) + geom_boxplot() + 
  facet_wrap(~niche, scales = "free")


## round2
ggplot(round2, aes(x = flux_nmol_C2H4_g_hr)) + geom_histogram() + facet_wrap(~niche)
## good normality (if negative values are kept**) save soil, but that's what I expected. Really wondering if I should analyze
## without changing negative values to 0

ggplot(round2, aes(x = flux_gN_g_h)) + geom_histogram() + facet_wrap(~niche)
qqnorm(round2$flux_nmol_C2H4_g_hr)
qqline(round2$flux_nmol_C2H4_g_hr)

#make negative values zero
round2$flux_nmol_C2H4_g_hr[round2$flux_nmol_C2H4_g_hr < 0] <- 0
ggplot(round2, aes(x = flux_nmol_C2H4_g_hr)) + geom_histogram() + facet_wrap(~niche, scales = "free")

## logged
ggplot(round2, aes(x = log(flux_nmol_C2H4_g_hr))) + geom_histogram() + facet_wrap(~niche)

### boxplot
ggplot(round2, aes(x = factor(moist_level), y = flux_nmol_C2H4_g_hr)) + 
  geom_boxplot(na.rm = TRUE) + 
  facet_wrap(~niche, scales = "free")
## despite the negative fluxes, there does seem to be a response here, positive, to higher moisture

##### Normality assumptions #####
# According to Webster and Oliver (2007) if the skewness is below 0.5, 
# we can consider the deviation from normality not big enough to transform the data. 
# Moreover, according to Witte and Witte (2009) if we have more than 10 samples per group we 
# should not worry too much about violating the assumption of normality or equality of variances.


##boxplot
## all samples
ggplot(fluxes, aes(x = factor(moist_level), y = flux_nmol_C2H4_g_hr, fill = factor(temp))) + 
  geom_boxplot(na.rm = TRUE) + 
  facet_wrap(~niche, scales = "free")

## round 2
ggplot(round2, aes(x = factor(temp), y = flux_nmol_C2H4_g_hr, fill = factor(moist_level))) + 
  geom_boxplot(na.rm = TRUE) + 
  facet_wrap(~niche, scales = "free")

#logged
ggplot(round2, aes(x = factor(moist_level), y = log(flux_nmol_C2H4_g_hr))) + 
  geom_boxplot(na.rm = TRUE) + facet_wrap(~niche)
ggplot(round2, aes(x = factor(temp), y = log(flux_nmol_C2H4_g_hr))) +
  geom_boxplot(na.rm = TRUE) + facet_wrap(~niche)
# these are more normal but I don't think they tell an accurate story

##plot just mositure percent by moisture level
ggplot(fluxes, aes(x = factor(moist_level), y = actual_moist_perc)) + geom_boxplot() + 
  facet_wrap(~niche, scales = "free") +
  labs(title = "Moisture content in relation to target moisture level") +
  ylab(label = "actual moisture level (% w/w)") +
  xlab(label = "Moisture level target (%w/w)") +
  theme(text = element_text(size=20))


## no log, just gN_g_h
ggplot(round2, aes(x = factor(temp), y = flux_gN_g_h, color = actual_moist_perc)) +
  geom_point(size = 4, na.rm = TRUE) + facet_wrap(~niche)

ggplot(round2, aes(x = factor(temp), y = flux_gN_g_h, color = factor(moist_level))) +
  geom_point(size = 4, na.rm = TRUE) + facet_wrap(~niche)

### cumulative soil wood and litter fluxes
SLW_fluxes_combined <- fluxes %>% filter(niche %in% c("Wood", "Litter", "Soil"))

#### SLW distributions ####
ggplot(SLW_fluxes_combined, aes(x = flux_gN_g_h)) + geom_histogram() + facet_wrap(~niche)
SLW_fluxes_combined %>% group_by(niche, moist_level, temp) %>% summarize(n = length(flux_nmol_C2H4_g_hr))
ggplot(SLW_fluxes_combined, aes(x = factor(temp), y = flux_gN_g_h)) + 
  geom_boxplot() + facet_wrap(~niche)

ggplot(SLW_fluxes_combined, aes(x = factor(temp), y = flux_gN_g_h, color = moist_level)) + 
  geom_point(size = 4) + facet_wrap(~niche)


### what does it look like if we remove zeroes - the nature of the problem is that not every 
### little sample of wood, litter or soil has n fixing bacteria in it. Hot Spots and Hot Moments!!!
### so if we remov the samples that had absolutely nothing going on, then what does the process look like? 

active_fluxes <- fluxes %>% filter(flux_nmol_C2H4_g_hr > 0)

ggplot(active_fluxes, aes(x = temp, y = flux_nmol_C2H4_g_hr, color = actual_moist_perc)) + 
  geom_point(size = 4) + facet_wrap(~niche, scales = "free_y") + geom_smooth(method = 'lm') +
  labs(title = "Active fluxes across moisture levels and temperatures") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  scale_x_discrete(name = "Temperature (C)", limits = c(5,15,25,35)) +
  theme(text = element_text(size=20))+
  theme(legend.position = c(0.8, 0.2))


# seems as though I did not quite achieve the moisture gradient I wanted, however the opposite
# may be true in that I created too wide of a gradient that smaller differences aren't visible on 
# an inclusive scale like this
# going to bin the moisture percentages

binned_SLW_fluxes <- SLW_fluxes_combined %>% group_by(niche, temp) %>% 
  mutate(moist_bin = cut(actual_moist_perc, breaks=4))

active_SLW_fluxes <- binned_SLW_fluxes %>% filter(flux_nmol_C2H4_g_hr > 0)

ggplot(active_SLW_fluxes, aes(x = factor(temp), y = flux_nmol_C2H4_g_hr, color = moist_bin)) + 
  geom_point(size = 4) + facet_wrap(~niche, scales = "free")

active_SLW_fluxes %>% group_by(niche, temp, moist_bin) %>% summarize(n = length(flux_nmol_C2H4_g_hr))

# what if I make moist percent continuous along x axis
cols <- c("35" = "brown", "25" = "red", "15" = "orange", "5" = "pink")

ggplot(active_SLW_fluxes, aes(x = actual_moist_perc, y = flux_nmol_C2H4_g_hr, color = factor(temp))) + 
  geom_point(size = 4) + facet_wrap(~niche, scales = "free") + 
  scale_color_manual(values= cols)

## remove mositure percent outliers
active_SLW_reduced <- active_SLW_fluxes %>% filter(actual_moist_perc <400)


ggplot(active_SLW_reduced, aes(x = actual_moist_perc, y = flux_nmol_C2H4_g_hr, color = factor(temp))) + 
  geom_point(size = 4) + facet_wrap(~niche, scales = "free_x") + 
  scale_color_manual(values= cols) + geom_smooth(method = "lm", na.rm = T, se = F)


### repeat above graph just with all fluxes including negatives but make negatives zeroes
SLW_fluxes_combined$flux_nmol_C2H4_g_hr[SLW_fluxes_combined$flux_nmol_C2H4_g_hr < 0] <- 0

SLW_reduced <- SLW_fluxes_combined %>% filter(actual_moist_perc <= 350)

ggplot(SLW_reduced, aes(x = actual_moist_perc, y = flux_nmol_C2H4_g_hr, color = factor(temp))) + 
  geom_point(size = 3) + facet_wrap(~niche, scales = "free_x") + 
  scale_color_manual(values= cols)

## by temp
ggplot(SLW_reduced, aes(x = temp, y = flux_nmol_C2H4_g_hr, color = factor(moist_level))) + 
  geom_point(size = 4) + facet_wrap(~niche, scales = "free_x") + 
  scale_color_manual(values = c("grey", "light blue", "sky blue", "blue", "navy blue")) + 
  geom_smooth(method = "lm", se = FALSE)

## zero negative fluxes and calculate average and SE for each treatment flux and moisture percent
fluxes$flux_nmol_C2H4_g_hr[fluxes$flux_nmol_C2H4_g_hr < 0] <- 0

exp_flux_sum <- fluxes %>% group_by(niche, temp, moist_level) %>%
  summarize(mean_flux_C2H4 = mean(flux_nmol_C2H4_g_hr), 
            se_flux = (sd(flux_nmol_C2H4_g_hr)/sqrt(length(flux_nmol_C2H4_g_hr))),
            mean_moist_perc = mean(actual_moist_perc),
            se_moist = (sd(actual_moist_perc)/sqrt(length(actual_moist_perc))),
  )


### plot the summary
ggplot(exp_flux_sum, aes(x = temp, y = mean_flux_C2H4)) +
  geom_point(size=4) + facet_wrap(~niche, scales = "free") + 
  geom_smooth(method = "lm", se = FALSE)

#zero the negative averages
exp_flux_sum$mean_flux_C2H4[exp_flux_sum$mean_flux_C2H4 < 0] <- 0
ggplot(exp_flux_sum, aes(x = temp, y = mean_flux_C2H4)) +
  geom_point(size=4) + facet_wrap(~niche, scales = "free_y") + 
  geom_smooth(method = "lm", se = FALSE)
# factor in moist level
ggplot(exp_flux_sum, aes(x = temp, y = mean_flux_C2H4, color = factor(moist_level))) +
  geom_point(size=4) + facet_wrap(~niche, scales = "free_y") + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(name = "Moisture Level (% w/w)", values = c("grey", "light blue", "royalblue1", "blue", "navy blue")) +
  scale_x_discrete(name = "Temperature (C)", limits = c(5,15,25,35)) + 
  geom_errorbar(aes(ymin = mean_flux_C2H4 - se_flux, ymax = mean_flux_C2H4 + se_flux), width = 2) +
  labs(title = "AR rate across moisture levels and temperatures") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Temperature (C))")  +
  theme(text = element_text(size=20)) +
  theme(legend.position = c(0.8, 0.2))

## filter out heterotrophs
SLW_sum <- exp_flux_sum %>% filter(niche %in% c("Wood", "Litter", "Soil"))
ggplot(SLW_sum, aes(x = temp, y = mean_flux_C2H4, color = factor(moist_level))) +
  geom_point(size=4) + facet_wrap(~niche) + 
  geom_smooth(method = "lm", se = FALSE) + 
  scale_color_manual(values = c("grey", "light blue", "royalblue1", "blue", "navy blue")) +
  geom_errorbar(aes(ymin = mean_flux_C2H4 - se_flux, ymax = mean_flux_C2H4 + se_flux), width = 2) + 
  scale_x_discrete(name = "Temperature (C)", limits = c(5,15,25,35)) + 
  labs(color = "Moisture (%w/w)") +
  labs(title = "Heterotrophic AR rate across moisture levels and temperatures") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Temperature (C))")  +
  theme(text = element_text(size=20)) 


## what does soil look like without the outlier? 

################# Moisture Level Organization ######################################################
## how can I organize these moisture levels better?

active_fluxes$actual_moist_perc[active_fluxes$actual_moist_perc < 0] <- 0
active_fluxes_reduced <- active_fluxes %>% filter(actual_moist_perc <400)
ggplot(active_fluxes_reduced, aes(x = actual_moist_perc, y = flux_nmol_C2H4_g_hr)) + geom_point() + 
  facet_wrap(~niche, scales = "free") + geom_smooth(method = "lm")

#652/831 = 78% of samples were not active (negative fluxes) 

#### take SLW samples and slice them up evenly based on actual moisture 
SLW_fluxes_combined.no_outlier <- SLW_fluxes_combined %>% filter(dry_mass != 0.692)
SLW_fluxes_combined.no_outlier$actual_moist_group <- cut_number(SLW_fluxes_combined.no_outlier$actual_moist_perc, 4)

SLW_rate_sum <- SLW_fluxes_combined.no_outlier %>% group_by(niche, temp, actual_moist_group) %>%
  summarize(mean_rate_nmol_C2H4_g_h = mean(flux_nmol_C2H4_g_hr), sd = sd(flux_nmol_C2H4_g_hr), n = length(flux_nmol_C2H4_g_hr), se = sd/sqrt(n))

ggplot(SLW_rate_sum, aes(x = mean_rate_nmol_C2H4_g_h)) + geom_histogram() + facet_wrap(~niche)
ggplot(SLW_rate_sum, aes(x = mean_rate_nmol_C2H4_g_h, fill=niche)) + geom_histogram() + facet_wrap(~temp)

## log for normality
ggplot(SLW_rate_sum, aes(x = log(mean_rate_nmol_C2H4_g_h), fill=niche)) + geom_histogram() + facet_wrap(~temp)

log.SLW_rate_sum <- SLW_rate_sum %>% mutate(log.mean.rate_nmol_C2H4_g_h = log(mean_rate_nmol_C2H4_g_h), log.se = log(se))

###plot
ggplot(log.SLW_rate_sum, aes(x = actual_moist_group, y = mean_rate_nmol_C2H4_g_h, color = factor(temp))) + 
  geom_point(size=3) + facet_wrap(~niche) + scale_color_manual(values= cols) +
  geom_errorbar(aes(ymin = mean_rate_nmol_C2H4_g_h - se, ymax = mean_rate_nmol_C2H4_g_h + se), width = 0.25) + 
  geom_smooth(method = 'lm', se=FALSE)

log.SLW_rate_sum <- log.SLW_rate_sum %>% mutate(act_moist_level = as.numeric(actual_moist_group))
type.convert(log.SLW_rate_sum$actual_moist_group, as.is = TRUE)

## moist group 1
(11.9+39)/2
25.45+13.5

## group 2
(39+69.2)/2
54.1-39

## group 3
(69.2+148)/2
108.6+39.4

## group 4: 
(148+774)/2

## moist by temp plot with changed moist levels
log.SLW_rate_sum$act_moist_level[log.SLW_rate_sum$act_moist_level == 1] <- 25
log.SLW_rate_sum$act_moist_level[log.SLW_rate_sum$act_moist_level == 2] <- 54
log.SLW_rate_sum$act_moist_level[log.SLW_rate_sum$act_moist_level == 3] <- 108
log.SLW_rate_sum$act_moist_level[log.SLW_rate_sum$act_moist_level == 4] <- 461

library(scales)
ggplot(log.SLW_rate_sum, aes(x = act_moist_level,
  y = mean_rate_nmol_C2H4_g_h, color = factor(temp))) + 
  geom_point(size=3) + facet_wrap(~niche, scales = "free_x") + scale_color_manual(values= cols, name = "Temperature (C)") +
  geom_errorbar(aes(ymin = mean_rate_nmol_C2H4_g_h - se, ymax = mean_rate_nmol_C2H4_g_h + se), width = 3) + 
  geom_smooth(method = 'lm', se=FALSE, span = 1) +
  scale_x_continuous(breaks=c(10, 50, 100, 200, 300, 400, 500)) +
  labs(title = "AR rate across moisture levels and temperatures") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Average moisture content (%w/w))") +
  theme(text = element_text(size=20)) +
  theme(legend.position = c(0.15, 0.7)) 

### switch ^ around so that temp is on the x axis
ggplot(log.SLW_rate_sum, aes(x = temp, y = mean_rate_nmol_C2H4_g_h, color = factor(act_moist_level))) + 
  geom_point(size=3) + facet_wrap(~niche, scales = "free_x") +
  geom_errorbar(aes(ymin = mean_rate_nmol_C2H4_g_h - se, ymax = mean_rate_nmol_C2H4_g_h + se), width = 3) + 
  geom_smooth(method = 'lm', se=FALSE)  +
  scale_color_manual(name = "Moisture Level (% w/w)", values = c("grey", "light blue", "royalblue1", "blue", "navy blue")) + 
  labs(title = "AR rate across moisture levels and temperatures") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Temperature (C)") +
  theme(text = element_text(size=20)) +
  theme(legend.position = c(0.15, 0.7)) 

class(log.SLW_rate_sum$act_moist_level)

ggplot(log.SLW_rate_sum, aes(x = act_moist_level,
                             y = mean_rate_nmol_C2H4_g_h, color = factor(temp))) + 
  geom_point(size=3) + facet_wrap(~niche) + scale_color_manual(values= cols, name = "Temperature (C)") +
  geom_errorbar(aes(ymin = mean_rate_nmol_C2H4_g_h - se, ymax = mean_rate_nmol_C2H4_g_h + se), width = 0.25) + 
  geom_smooth(method = 'loess', se=FALSE, span = 1.5) +   
  labs(title = "AR rate across moisture levels and temperatures") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Average moisture content (%w/w))") +
  theme(text = element_text(size=20)) +
  theme(legend.position = c(0.15, 0.7)) 

###### Re-do of moisture groupings on an individual niche basis
SLW_fluxes_combined.no_outlier <- SLW_fluxes_combined %>% filter(dry_mass != 0.692)


SLW_fluxes_combined.no_outlier <- SLW_fluxes_combined.no_outlier %>% group_by(niche) %>% mutate(actual_moist_group = cut_number(actual_moist_perc, 4))

SLW_rate_sum.2 <- SLW_fluxes_combined.no_outlier %>% group_by(niche, temp, actual_moist_group) %>%
  summarize(mean_rate_nmol_C2H4_g_h = mean(flux_nmol_C2H4_g_hr), sd = sd(flux_nmol_C2H4_g_hr), 
            n = length(flux_nmol_C2H4_g_hr), se = sd/sqrt(n),
            mean_moist = mean(actual_moist_perc))

ggplot(SLW_rate_sum.2, aes(x = mean_rate_nmol_C2H4_g_h)) + geom_histogram() + facet_wrap(~niche)
ggplot(SLW_rate_sum.2, aes(x = mean_rate_nmol_C2H4_g_h, fill=niche)) + geom_histogram() + facet_wrap(~temp)

## log for normality
ggplot(SLW_rate_sum.2, aes(x = log(mean_rate_nmol_C2H4_g_h), fill=niche)) + geom_histogram() + facet_wrap(~temp)

log.SLW_rate_sum.2 <- SLW_rate_sum.2 %>% mutate(log.mean.rate_nmol_C2H4_g_h = log(mean_rate_nmol_C2H4_g_h), log.se = log(se))

###plot
log.SLW_rate_sum.2 <- log.SLW_rate_sum.2 %>% mutate(moist_group = as.numeric(actual_moist_group))
unique(log.SLW_rate_sum.2$actual_moist_group)
log.SLW_rate_sum.2 <- log.SLW_rate_sum.2 %>% mutate(act_moist_level = as.numeric(actual_moist_group))

log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 1] <- "11.9-49.5"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 2] <- "49.5-70.1"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 3] <- "70.1-143"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 4] <- "143-287"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 5] <- "13.9-22.5"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 6] <- "22.5-31.9"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 7] <- "31.9-54.6"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 8] <- "54.6-115"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 9] <- "18.4-83.9"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level ==10] <- "83.9-159"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 11] <- "159-222"
log.SLW_rate_sum.2$act_moist_level[log.SLW_rate_sum.2$act_moist_level == 12] <- "222-774"

ggplot(log.SLW_rate_sum.2, aes(x = act_moist_level, y = mean_rate_nmol_C2H4_g_h, color = factor(temp))) + 
  geom_point(size=3) + facet_wrap(~niche, scales = 'free_x') + scale_color_manual(values= cols, name = "Temperature (C)") +
  geom_errorbar(aes(ymin = mean_rate_nmol_C2H4_g_h - se, ymax = mean_rate_nmol_C2H4_g_h + se), width = 0.25) + 
  geom_smooth(method = 'loess', se=FALSE, span = 1.5)  +
  labs(title = "AR rate across moisture levels and temperatures") +
  ylab(label = "AR rate (nmol C2H4 g-1 hr-1)") +
  xlab(label = "Average moisture content (%w/w))") +
  theme(text = element_text(size=20)) +
  theme(legend.position = c(0.15, 0.7)) 

  
  ##### MAYBE  separate each niche out, cut their moisture groups differently and add an extra group 
  ### if needed for higher moisture ranges 
  


  