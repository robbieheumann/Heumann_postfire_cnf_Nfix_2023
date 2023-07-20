### climate experiment analysis script for collab data analysis class spring 2023

## import tidyverse
library(tidyverse)
library(stats)
library(MASS)
cbbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#D55E00", "#000000")
## import clim_flux_calcs_2
slw_fluxes <- read_csv("cryptic_clim_exp_2022/1_incremental/clim_flux_calcs_3_FINAL.csv")

## what we working with?
head(slw_fluxes)

## remove the NA that I dropped and make sure flux columns are numeric
slw_fluxes <- slw_fluxes %>% filter(drymass >0)
glimpse(slw_fluxes)
slw_fluxes$flux_nmol_c2h4_g_hr_geo_corr <- as.numeric(slw_fluxes$flux_nmol_c2h4_g_hr_geo_corr)
slw_fluxes$grams_N_g_hr_geo_corr <- as.numeric(slw_fluxes$grams_N_g_hr_geo_corr)
slw_fluxes$flux_nmol_c2h4_g_hr_med_corr <- as.numeric(slw_fluxes$flux_nmol_c2h4_g_hr_med_corr)
slw_fluxes$grams_N_g_hr_med_corr <- as.numeric(slw_fluxes$grams_N_g_hr_med_corr)
glimpse(slw_fluxes)

## how weird is this data?
summary(slw_fluxes)
ggplot(slw_fluxes, aes(x = flux_nmol_c2h4_g_hr_med_corr)) + geom_histogram() + 
  facet_wrap(~niche)


## unfortunately, for this analysis, I don't care about negative values
slw_fluxes$flux_nmol_c2h4_g_hr_med_corr[slw_fluxes$flux_nmol_c2h4_g_hr_med_corr < 0] <- 0

## distribution
ggplot(slw_fluxes, aes(x = flux_nmol_c2h4_g_hr_med_corr)) + geom_histogram() + 
  facet_wrap(~niche)

## logged?
ggplot(slw_fluxes, aes(x = log(flux_nmol_c2h4_g_hr_med_corr+0.0001))) + geom_histogram() + 
  facet_wrap(~niche)

## box cox analysis
boxcox(flux_nmol_c2h4_g_hr_med_corr+0.0001 ~ actual_moist*temp_C, data = slw_fluxes, 
       lambda = seq(-1, 1, length = 15)) 

## negative lambda thus log? not exactly sure what a negative lambda means...
## will log fluxes for future analyses

## but first, who do the initial raw trends look like?

## spread of actual moisture contents? 
ggplot(slw_fluxes, aes(x = actual_moist)) + geom_histogram() + 
  facet_wrap(~niche)

## how far off was I from target moisture levels? 
ggplot(slw_fluxes, aes(x = factor(moist_level), y = actual_moist, color = niche)) + 
  geom_boxplot(width = 0.5, ) + 
  facet_wrap(~temp_C, scales = "free")
## ... well I was pretty fucking far off for soil - how do I group those moisture levels now? 
## 26 outliers overall

## bin each niche by 4 equally replicated moisture groups
bin_slw_fluxes <- slw_fluxes %>% group_by(niche, temp_C) %>%
  mutate(moist_bin = cut(actual_moist, 4))

bin_slw_fluxes %>% group_by(niche, temp_C, moist_bin) %>% 
  summarize(mean_moist = mean(actual_moist), sample_n = length(actual_moist))

## if I use these bins I have an extremely unbalanced dataset, so what the heck do I do...?
## remove moisture content outliers in wood, defined as those more than 1.5*IQR for sake  including more outliers
## standard outlying point computation from McGill 1985, in geom_boxplot function too 

# Compute the upper and lower whiskers using 1.5 * IQR
slw_moist_outlier_summary <- slw_fluxes %>% 
  group_by(niche, temp_C, moist_level) %>%
  summarize(
    lower_whisker = min(actual_moist[actual_moist > quantile(actual_moist, 0.25) - 1.5 * IQR(actual_moist)]),
    upper_whisker = max(actual_moist[actual_moist < quantile(actual_moist, 0.75) + 1.5 * IQR(actual_moist)])
  )
### filter out outliers for each niche, in each moist level, by calculating z scores for each data point
### then removing z scores greater than or 3 or less than -3

slw_fluxes_z <- slw_fluxes %>%
  group_by(niche, moist_level) %>%
  mutate(group_mean =  mean(actual_moist),
         group_sd = sd(actual_moist)) %>%
  ungroup()

slw_fluxes_z <- slw_fluxes_z %>%
  mutate(moist_z = (actual_moist - group_mean)/group_sd)

## remove outliers greater than 3 sd from mean
slw_fluxes_no_moist_outlier <- slw_fluxes_z %>% filter(moist_z < 3)
slw_fluxes_no_moist_outlier <- slw_fluxes_no_moist_outlier %>% filter(moist_z > -3)
## plot again
ggplot(slw_fluxes_no_moist_outlier, aes(x = factor(moist_level), y = actual_moist, color = niche)) + 
  geom_boxplot(width = 0.5, ) + 
  facet_wrap(~temp_C, scales = "free")

slw_fluxes_no_moist_outlier$actual_moist_group <- cut_number(slw_fluxes_no_moist_outlier$actual_moist, 4)

## what does this look like with c2h4 fluxes on y
ggplot(slw_fluxes_no_moist_outlier, aes(x = factor(temp_C), y = flux_nmol_c2h4_g_hr_med_corr, color = actual_moist_group)) + 
  geom_boxplot() + 
  facet_wrap(~niche, scales = "free")

## DO NOT REMOVE FLUX OUTLIERS (SOPER ET AL 2021), just calculate geomeans***

ggplot(slw_fluxes_no_moist_outlier, aes(x = factor(temp_C), y = flux_nmol_c2h4_g_hr_med_corr, color = actual_moist_group)) + 
  geom_point(size=2) + 
  facet_wrap(~niche, scales = "free")


ggplot(slw_fluxes_no_moist_outlier, aes(x = temp_C, y = flux_nmol_c2h4_g_hr_med_corr, color = actual_moist_group)) + 
  geom_jitter(size=3) + facet_wrap(~ niche, scales = "free")

## summary
slw_sum <- slw_fluxes_no_moist_outlier %>% group_by(niche, temp_C, actual_moist_group) %>%
  summarize(geomean_nmol_c2h4_g_hr = exp(mean(log(flux_nmol_c2h4_g_hr_med_corr+0.00001))), ## geomean + 0.00001 to make non-zeroes
            mean_nmol_c2h4_g_hr = mean(flux_nmol_c2h4_g_hr_med_corr),
            sd = sd(flux_nmol_c2h4_g_hr_med_corr), 
            n = length(flux_nmol_c2h4_g_hr_med_corr))

## plot summary
ggplot(slw_sum, aes(x = temp_C, y = geomean_nmol_c2h4_g_hr, color = actual_moist_group)) + 
  geom_point(size=3) + facet_wrap(~niche, scales = "free")

## group each niche in different moisture groups
soil <- slw_fluxes_no_moist_outlier %>% filter(niche == "Soil")
litter <- slw_fluxes_no_moist_outlier %>% filter(niche == "Litter")
wood <- slw_fluxes_no_moist_outlier %>% filter(niche == "Wood")

soil$actual_moist_group <- cut_number(soil$actual_moist, 4)
litter$actual_moist_group <- cut_number(litter$actual_moist, 4)
wood$actual_moist_group <- cut_number(wood$actual_moist, 4)

## combine these back into one df
slw_combined <- rbind(soil, litter, wood)

slw_combined %>% group_by(niche, temp_C, actual_moist_group) %>%
  summarize(geomean_nmol_c2h4_g_hr = exp(mean(log(flux_nmol_c2h4_g_hr_med_corr+0.00001))), ## geomean + 0.00001 to make non-zeroes
            mean_nmol_c2h4_g_hr = mean(flux_nmol_c2h4_g_hr_med_corr),
            sd = sd(flux_nmol_c2h4_g_hr_med_corr), 
            n = length(flux_nmol_c2h4_g_hr_med_corr))


## summarize based on niche, temp and actual moist group
slw_comb_sum <- slw_combined %>% group_by(niche, temp_C, actual_moist_group) %>%
  summarize(mean_nmol_c2h4_g_hr = mean(flux_nmol_c2h4_g_hr_med_corr),
            log_mean_c2h4 = mean(log(flux_nmol_c2h4_g_hr_med_corr+0.00001)),
            geomean_nmol_c2h4_g_hr = exp(mean(log(flux_nmol_c2h4_g_hr_med_corr+0.00001))),
            sd = sd(flux_nmol_c2h4_g_hr_med_corr), 
            n = length(flux_nmol_c2h4_g_hr_med_corr),
            mean_moist = mean(actual_moist),
            sd_moist = sd(actual_moist))

## some of the wood samples are somewhat unbalanced but overall between 4-11 reps per treatment per niche

slw_comb_sum

## geo mean plot
ggplot(slw_comb_sum, aes(x = temp_C, y = geomean_nmol_c2h4_g_hr, color = actual_moist_group)) + 
  geom_point(size =3) + 
  facet_wrap(~niche, scales = "free") + 
  geom_smooth(method = 'loess', se = FALSE, linetype = 2)

## arithmetic mean plot
ggplot(slw_comb_sum, aes(x = temp_C, y = mean_nmol_c2h4_g_hr, color = actual_moist_group)) + 
  geom_point(size =3) + 
  facet_wrap(~niche, scales = "free") + 
  geom_smooth(method = 'loess', span = 4, se = FALSE, linetype = 2)


#### ANALYSIS ####
## how????
# first - residual distributions
plot(lm(flux_nmol_c2h4_g_hr_med_corr ~ temp_C*actual_moist_group, soil))
plot(lm(log(flux_nmol_c2h4_g_hr_med_corr+0.0001) ~ temp*actual_moist_group, soil))

plot(lm(flux_nmol_c2h4_g_hr_med_corr ~ temp_C*actual_moist_group, litter))
plot(lm(log(flux_nmol_c2h4_g_hr_med_corr+0.0001) ~ temp*actual_moist_group, litter))

plot(lm(flux_nmol_c2h4_g_hr_med_corr ~ temp_C*actual_moist_group, wood))
plot(lm(log(flux_nmol_c2h4_g_hr_med_corr+0.0001) ~ temp*actual_moist_group, wood))

# These data don't meet OLS assumptions... variation increases with increasing x
## check zero inflation
slw_combined %>% filter(slw_combined$flux_nmol_c2h4_g_hr_med_corr >0) %>% summarize(length_non_zero = length(flux_nmol_c2h4_g_hr_med_corr))
## 78 non-zero fluxes from that whole fucking experiment 

## I have zero inflated (censored. - truncated), non normal (heteroscedastic), continuous data


## the plots I want with the two data sets to use for analysis:

write_excel_csv(slw_combined, 'C:/Users/rh176228/Desktop/cryptic_fixation/cryptic_clim_exp_2022/1_incremental/slw_combined_fluxes_03_13_2023.csv')
write_excel_csv(slw_comb_sum, 'C:/Users/rh176228/Desktop/cryptic_fixation/cryptic_clim_exp_2022/1_incremental/slw_combined_flux_summary_03_13_2023.csv')



##### SCRAP #####
## raw ppm data on y
ggplot(slw_combined, aes(x = temp_C, y = ppm, color = factor(moist_level))) + 
  geom_jitter(size=2) + 
  facet_wrap(~niche, scales = 'free')+
  geom_smooth(method = 'glm', se = FALSE,)

## median corrected ppm data
ggplot(slw_combined, aes(x = temp_C, y = ppm_corrected, color = factor(moist_level))) + 
  geom_jitter(size=2) + 
  facet_wrap(~niche, scales = 'free')

### make new df with just bag blank corrections
slw_combined<- slw_combined %>% mutate(bag_blank_correction_ppm = ppm-bag_avg)

ggplot(slw_combined, aes(x = temp_C, y = bag_blank_correction_ppm, color = factor(moist_level))) + 
  geom_jitter(size=2) + 
  facet_wrap(~niche, scales = 'free')


## polynomial regression for both temp and moist? 
## second order quadratic
slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(geomean_nmol_c2h4_g_hr~poly(temp_C,2)+poly(mean_moist,2), data=.x)))

## logged
slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(geomean_nmol_c2h4_g_hr)~poly(temp_C,2)+poly(mean_moist,2), data=.x)))

slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(plot(lm(geomean_nmol_c2h4_g_hr~poly(temp_C,2)+poly(mean_moist,2), data=.x))))

## log plots
slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(plot(lm(log(geomean_nmol_c2h4_g_hr)~poly(temp_C,2)+poly(mean_moist,2), data=.x))))
### doesnt really help with heteroscedasticity

## third order (cubic) 
slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(geomean_nmol_c2h4_g_hr~poly(temp_C,3)+poly(mean_moist,3), data=.x)))

slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(geomean_nmol_c2h4_g_hr)~poly(temp_C,3)+poly(mean_moist,3), data=.x)))

slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(plot(lm(log(geomean_nmol_c2h4_g_hr)~poly(temp_C,3)+poly(mean_moist,3), data=.x))))

### pseudoreplicates
head(slw_combined)
litter_cubic_intlm <- slw_combined %>% filter(niche == 'Litter') %>%
  lm(log(flux_nmol_c2h4_g_hr_med_corr+0.00001)~poly(temp_C,3)*actual_moist, data=.)

summary(litter_cubic_intlm)
anova(litter_cubic_intlm)
### moisture significant on its own, however this model is pseudo replicated 

soil_cubic_intlm <- slw_combined %>% filter(niche == 'Soil') %>%
  lm(log(flux_nmol_c2h4_g_hr_med_corr+0.00001)~poly(temp_C,3)*actual_moist, data=.)

summary(soil_cubic_intlm)
anova(soil_cubic_intlm)
### neither temp nor moisture are significant here

wood_cubic_intlm <- slw_combined %>% filter(niche == 'Wood') %>%
  lm(log(flux_nmol_c2h4_g_hr_med_corr+0.00001)~poly(temp_C,3)*actual_moist, data=.)

summary(wood_cubic_intlm)
anova(wood_cubic_intlm)
## both the cubic temp term, actual moist, and the interaction of the two were all significant <0.05
plot(wood_cubic_intlm)

## again this is not using means, this is pseudo replicated 

## quantile regression
install.packages("quantreg")
library(quantreg)
example(rq)

head(slw_combined)

ggplot(slw_combined, aes(x = actual_moist, y = flux_nmol_c2h4_g_hr_med_corr)) + 
  geom_point() +
  facet_wrap(~niche, scales = "free_x")

lm_fit1 <- lm(flux_nmol_c2h4_g_hr_med_corr ~ actual_moist * temp_C, data = slw_combined)
plm_fit1 <- lm(flux_nmol_c2h4_g_hr_med_corr ~ actual_moist * poly(temp_C,2), data = slw_combined)
rqm_fit <- rq(flux_nmol_c2h4_g_hr_med_corr ~ actual_moist, data = slw_combined, tau = seq(0,1, by = 0.1))


# plotting different quantiles
colors <- c("#ffe6e6", "#ffcccc", "#ff9999", "#ff6666", "#ff3333",
            "#ff0000", "#cc0000", "#b30000", "#800000", "#4d0000", "#000000")
BluRed <- c("navy", "skyblue2", "orange", "red4")
ggplot(slw_combined, aes(x = actual_moist, y = flux_nmol_c2h4_g_hr_med_corr, color = factor(temp_C, levels = c('5', '15', '25', '35')))) + 
  geom_point(size=3) +
  scale_color_manual(values = BluRed, name = "Temperature (°C)") + 
  facet_wrap(~niche, scales = "free_x") + 
  geom_quantile(formula = y~poly(x,2), linetype)


