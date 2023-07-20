library(tidyverse)
cbbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#D55E00", "#000000")
## import raw_june_clim_exp ppm data
library(readxl)
Combined_clim_exp_flux_calcs <- read_excel("cryptic_clim_exp_2022/1_incremental/clim_flux_calcs_3_FINAL.xlsx", sheet = "for R")
### convert actual moist to percent
Combined_clim_exp_flux_calcs <- Combined_clim_exp_flux_calcs %>% mutate(actual_moist_perc = actual_moist*100)


### filter out mosses and lichens
ML_fluxes <- Combined_clim_exp_flux_calcs %>% filter(niche %in% c("Moss", "Lichen"))

write.csv(ML_fluxes)
### constrain fluxes to 0
ML_fluxes$flux_nmol_C2H4_g_hr[ML_fluxes$flux_nmol_C2H4_g_hr < 0] <- 0


ggplot(ML_fluxes, aes(x = actual_moist_perc, y = flux_nmol_C2H4_g_hr, color = temp)) + 
  geom_point()

## spread of moisture contents
ggplot(ML_fluxes,aes(x = factor(moist_level), y = actual_moist_perc, color = niche)) + 
  geom_boxplot(width = 0.5, ) + 
  facet_wrap(~temp, scales = "free")

## pretty far off as well
## identify and remove outliers, group equally based on actual moisture percents
Ml_flux_moist_outlier_sum <- ML_fluxes %>% 
  group_by(niche, temp, moist_level) %>%
  summarize(
    lower_whisker = min(actual_moist_perc[actual_moist_perc > quantile(actual_moist_perc, 0.25) - 1.5 * IQR(actual_moist_perc)]),
    upper_whisker = max(actual_moist_perc[actual_moist_perc < quantile(actual_moist_perc, 0.75) + 1.5 * IQR(actual_moist_perc)])
  )
### filter out outliers for each niche, in each moist level, by calculating z scores for each data point
### then removing z scores greater than or 3 or less than -3

ML_fluxes_z <- ML_fluxes %>%
  group_by(niche, moist_level) %>%
  mutate(group_mean =  mean(actual_moist_perc),
         group_sd = sd(actual_moist_perc)) %>%
  ungroup()

ML_fluxes_z <- ML_fluxes_z %>%
  mutate(moist_z = (actual_moist_perc - group_mean)/group_sd)

## remove outliers greater than 1 sd from mean
ML_fluxes_no_moist_outlier <- ML_fluxes_z %>% filter(moist_z < 3)
ML_fluxes_no_moist_outlier <- ML_fluxes_no_moist_outlier %>% filter(moist_z > -2)
## remove negative 44 moisture content lichen sample
ML_fluxes_no_moist_outlier <- ML_fluxes_no_moist_outlier %>% filter(actual_moist_perc>0)
## plot again
ggplot(ML_fluxes_no_moist_outlier, aes(x = factor(moist_level), y = actual_moist_perc, color = niche)) + 
  geom_boxplot(width = 0.5, ) + 
  facet_wrap(~temp, scales = "free")

## cut up based on roughly equal groups of moisture contents
M_fluxes_nooutlier <- ML_fluxes_no_moist_outlier %>% filter(niche == "Moss")
M_fluxes_nooutlier$actual_moist_group <- cut_number(M_fluxes_nooutlier$actual_moist_perc, 4)
L_fluxes_nooutlier <- ML_fluxes_no_moist_outlier %>% filter(niche == "Lichen")
L_fluxes_nooutlier$actual_moist_group <- cut_number(L_fluxes_nooutlier$actual_moist_perc, 4)

ML_fluxes_no_moist_outlier <- rbind(L_fluxes_nooutlier, M_fluxes_nooutlier)
ML_sum <- ML_fluxes_no_moist_outlier %>% group_by(niche, temp, actual_moist_group) %>%
  summarize(geomean_nmol_c2h4_g_hr = exp(mean(log(flux_nmol_C2H4_g_hr+0.00001))), ## geomean + 0.00001 to make non-zeroes
            mean_nmol_c2h4_g_hr = mean(flux_nmol_C2H4_g_hr),
            sd = sd(flux_nmol_C2H4_g_hr), 
            n = length(flux_nmol_C2H4_g_hr),
            mean_moist = mean(actual_moist_perc),
            sd_moist = sd(actual_moist_perc))
write.csv(ML_sum)

ggplot(ML_fluxes_no_moist_outlier, aes(x = factor(temp), y = flux_nmol_C2H4_g_hr, color = actual_moist_group)) + 
  geom_boxplot() + 
  facet_wrap(~niche, scales = "free")
write_csv(ML_fluxes_no_moist_outlier, "cryptic_clim_exp_2022/1_incremental/ML_fluxes_updated.csv")
ML_sum <- ML_sum %>% mutate(sem = sd/sqrt(n))
write_csv(ML_sum, "cryptic_clim_exp_2022/1_incremental/ML_sum.csv")
### plot fluxes
BluRed <- c("navy", "skyblue2", "orange", "red4")
ML_sum <- ML_sum %>% mutate(er_width = ifelse(niche =="Lichen", 1,0.03))
library(RColorBrewer)
ML_plot <- ggplot(ML_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr, color = factor(temp, levels = c('5', '15', '25', '35')))) + 
  geom_point(size=4) + 
  geom_errorbar(aes(ymin = mean_nmol_c2h4_g_hr-sem, ymax=mean_nmol_c2h4_g_hr+sem, width=8))+
  geom_errorbarh(aes(xmax = mean_moist+sd_moist, xmin = mean_moist-sd_moist, height=er_width)) +
  facet_wrap(~niche, scales = "free") + 
  scale_color_discrete(name = "Temperature (C)") +
  theme(legend.title = element_text(size=18)) +
  scale_color_manual(values = BluRed, name = "Temperature (°C)")+
  geom_smooth(method = 'lm', formula = y~log(x), se=F, linetype=2) +
  theme(legend.position = "bottom") +
  theme_classic()+
  theme(strip.text.x = element_text(size = 18, face ='bold'),
        text = element_text(size=18,face ='bold')) +
  xlab("Mean moisture content (% w/w)")+
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1})))+
  theme(legend.position = "bottom")

## ^^ model for this graph
ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(mean_moist,2)*poly(temp,2), data=.x)))
ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(mean_moist)*temp, data=.x)))### better model
ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(mean_moist)*temp, data=.x)))
### nothing significant here

ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(mean_moist)+temp, data=.x)))
ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(mean_moist)+temp, data=.x)))

ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp), data=.x)))

ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)+mean_moist, data=.x)))

ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)*mean_moist, data=.x)))## best model

ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)*mean_moist, data=.x)))## best model

ML_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(anova(lm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(temp,2)*mean_moist, data=.x))))

### temp is significant, moisture is not, nor is the interaction, thus temperature seems to be the dominant
## variable here


L_fit1 <- ML_sum %>%
  filter(niche == "Lichen") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)*mean_moist, data=.)
L_fit2 <- ML_sum %>%
  filter(niche == "Lichen") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp), data=.)
L_fit3 <- ML_sum %>%
  filter(niche == "Lichen") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)+mean_moist, data=.)
L_fit4 <- ML_sum %>%
  filter(niche == "Lichen") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)*log(mean_moist), data=.)
L_fit5 <- ML_sum %>%
  filter(niche == "Lichen") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(temp,2), data=.)
L_fit6 <- ML_sum %>%
  filter(niche == "Lichen") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(temp,2)+poly(mean_moist,2), data=.)

anova1 <- anova(L_fit1, L_fit2, L_fit3, L_fit4, L_fit5, L_fit6)

anova(L_fit1)
anova(L_fit2)
anova(L_fit3)
anova(L_fit4)
anova(L_fit5)
anova(L_fit6)

M_fit1 <- ML_sum %>%
  filter(niche == "Moss") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)*mean_moist, data=.)
M_fit2 <- ML_sum %>%
  filter(niche == "Moss") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp), data=.)
M_fit3 <- ML_sum %>%
  filter(niche == "Moss") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)+mean_moist, data=.)
M_fit4 <- ML_sum %>%
  filter(niche == "Moss") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp)*log(mean_moist), data=.)
M_fit5 <- ML_sum %>%
  filter(niche == "Moss") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(temp,2), data=.)
M_fit6 <- ML_sum %>%
  filter(niche == "Moss") %>%
  lm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(temp,2)+poly(mean_moist,2), data=.)

anova(M_fit1, M_fit2, M_fit3, M_fit4, M_fit5, M_fit6)

anova(M_fit1)
anova(M_fit2)
anova(M_fit3)
anova(M_fit4)
anova(M_fit5)
anova(M_fit6)
