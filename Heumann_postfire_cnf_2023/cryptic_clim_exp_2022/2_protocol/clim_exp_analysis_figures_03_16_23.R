library(tidyverse)
### import slw_combined and slw_comb_sum
slw_combined <- read_csv("cryptic_clim_exp_2022/1_incremental/slw_combined_fluxes_03_13_2023.csv")
slw_comb_sum <- read_csv("cryptic_clim_exp_2022/1_incremental/slw_combined_flux_summary_03_13_2023.csv")

ggplot(slw_combined, aes(x = flux_nmol_c2h4_g_hr_med_corr)) + geom_histogram()
ggplot(slw_combined, aes(x = log(flux_nmol_c2h4_g_hr_med_corr+0.00001))) + geom_histogram()

ggplot(slw_comb_sum, aes(x = mean_nmol_c2h4_g_hr)) + geom_histogram() +
  facet_wrap(~niche)

## moisture on x 
ggplot(slw_comb_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr, color = factor(temp_C, levels = c('5', '15', '25', '35')))) + 
  geom_point(size=4) +
  facet_wrap(~niche, scales = 'free') + 
  geom_smooth(method = 'lm', formula = y~poly(x,2), aes(color=factor(temp_C)), linetype = 2, se=F) +
  scale_color_discrete(name = "Temperature (C)") +
  theme(legend.title = element_text(size=10))

ggplot(slw_comb_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr, color = factor(temp_C, levels = c('5', '15', '25', '35')))) + 
  geom_point(size=4) +
  facet_wrap(~niche, scales = 'free') + 
  geom_smooth(method = 'lm', se=F, linetype =2) +
  scale_color_discrete(name = "Temperature (C)") +
  theme(legend.title = element_text(size=18)) 

## create standard error
slw_comb_sum <- slw_comb_sum %>% mutate(sem=sd/sqrt(n))

BluRed <- c("navy", "skyblue2", "orange", "red4")


#### FINAL GRAPH ####
library(RColorBrewer)
BluRed <- c("navy", "skyblue2", "orange", "red4")
slw_comb_sum <- slw_comb_sum %>% mutate(sem=sd/sqrt(n))
SLW_plot <- ggplot(slw_comb_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr, 
                         color = factor(temp_C, levels = c('5', '15', '25', '35')),
)) + 
  geom_point(size=4) +
  geom_errorbar(aes(ymin = mean_nmol_c2h4_g_hr-sem, ymax=mean_nmol_c2h4_g_hr+sem, width=10))+
  geom_errorbarh(aes(xmax = mean_moist+sd_moist, xmin = mean_moist-sd_moist, height=.03))+
  facet_wrap(~niche, scales = 'free') + 
  geom_smooth(method = 'lm', formula = y~poly(x,2), se=F, linetype =2) +
  scale_color_discrete(name = "Temperature (°C)") +
  theme(legend.title = element_text(size=18)) +
  scale_color_manual(values = BluRed, name = "Temperature (°C)") + 
  scale_shape_discrete(name =element_blank()) +
  ylim(0,0.69) + 
  theme_classic()+
  theme(strip.text.x = element_text(size = 18, face ='bold'),
        text = element_text(size=18,face ='bold')) + 
  theme(legend.position = "none") +
  xlab(element_blank())+
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1})))

cor(slw_combined$temp_C, slw_combined$actual_moist)
write_csv(slw_comb_sum, "cryptic_clim_exp_2022/1_incremental/SLW_sum.csv")
### FINAL MODEL ###
slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log_mean_c2h4~poly(temp_C,2)*mean_moist, data=.x)))
slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(lm(log_mean_c2h4~poly(temp_C,2)*mean_moist, data=.x)))
slw_comb_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(glm(log_mean_c2h4~poly(temp_C,2)*mean_moist, family=gaussian(), data=.x)))

##########################################################################
##### combined SLW and ML #####
ML_sum <- read_csv("cryptic_clim_exp_2022/1_incremental/ML_sum.csv")
ML_fluxes <- read_csv("cryptic_clim_exp_2022/1_incremental/ML_fluxes_updated.csv")
### just temp or moisture analysis
ggplot(slw_combined, aes(x = temp_C, y = flux_nmol_c2h4_g_hr_med_corr)) + geom_point()
ggplot(ML_fluxes, aes(x = temp, y = flux_nmol_C2H4_g_hr)) + geom_point()
ML_red <- ML_fluxes %>% select(c("niche", "temp", "flux_nmol_C2H4_g_hr", "actual_moist"))
## get rid of negative mositure contents
ML_red <- ML_red %>% filter(actual_moist>0)
slw_comb_red <- slw_combined %>% select(c("niche", "temp_C", "flux_nmol_c2h4_g_hr_med_corr", "actual_moist"))
slw_comb_red <- rename(slw_comb_red, temp = temp_C, flux_nmol_C2H4_g_hr = flux_nmol_c2h4_g_hr_med_corr)

combined_fluxes <- rbind(slw_comb_red, ML_red)
ggplot(combined_fluxes, aes(x = log(flux_nmol_C2H4_g_hr+0.001), fill = niche)) + geom_histogram() + 
 facet_wrap(~niche, scales = 'free')
write.csv(combined_fluxes, "cryptic_clim_exp_2022/1_incremental/combined_fluxes_5_23_2023.csv")
hist(sqrt(combined_fluxes$actual_moist+0.01))

####### Perform Rank ANCOVA ######
library(rstatix)
## summarize by niche and temperature
ML_sum_red <- ML_sum %>% select(c("niche", "temp", "actual_moist_group", "mean_nmol_c2h4_g_hr",
                                  "sd", "n", "mean_moist", "sd_moist", "sem"))
ML_sum_red <- rename(ML_sum_red, temp_C = temp)
slw_comb_sum_red <- slw_comb_sum %>% select(c("niche", "temp_C", "actual_moist_group", "mean_nmol_c2h4_g_hr",
                                              "sd", "n", "mean_moist", "sd_moist", "sem"))
combined_flux_sum <- rbind(slw_comb_sum_red, ML_sum_red)
combined_flux_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(aov(rank(mean_nmol_c2h4_g_hr) ~ temp_C * sqrt(mean_moist+0.01), data = .x)))

combined_flux_sum %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(aov(glm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(temp_C,2)*mean_moist, 
                                      family=gaussian(), data=.x))))
combined_fluxes %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(anova(lm(log(flux_nmol_C2H4_g_hr+0.0001)~poly(temp,2)*actual_moist, data=.x))))
glm_full<- combined_fluxes %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(glm(flux_nmol_C2H4_g_hr~log(temp)*actual_moist, family = gaussian(), data=.x)))

ggplot(combined_fluxes, aes(x = temp, y = flux_nmol_C2H4_g_hr)) + geom_point(size=3) + 
  facet_wrap(~niche, scales = "free") + geom_smooth(method = "lm", se = F) +
  scale_x_continuous(breaks = c(5, 15, 25, 35)) +
  theme_bw(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  xlab("Temperature (°C)")+
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1})))

ggplot(combined_fluxes, aes(x = actual_moist, y = flux_nmol_C2H4_g_hr)) + geom_point(size=3) + 
  facet_wrap(~niche, scales = "free") + geom_smooth(method = "lm", se = F) +
  theme_bw(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

##### moisture only
ggplot(combined_flux_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr)) + geom_point(size=3)+ geom_smooth(method = "lm", se = F) +
  theme_classic(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+  
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Average moisture content (% w/w)")
## model for above graph
combined_flux_sum %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~mean_moist, data=.x)))

ggplot(combined_flux_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr)) + geom_point(size=3)+ 
  facet_wrap(~niche, scales = "free") + 
  geom_smooth(method = "lm",formula = y~poly(x,2), se = F,linetype = 2, linewidth = 1.25) +
  theme_bw(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Average moisture content (% w/w)") +
  geom_errorbar(aes(ymin = mean_nmol_c2h4_g_hr-sem, ymax=mean_nmol_c2h4_g_hr+sem, width=12))+
  geom_errorbarh(aes(xmax = mean_moist+sd_moist, xmin = mean_moist-sd_moist, height=.03))

## model for above graph
combined_flux_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~poly(mean_moist,2), data=.x)))
combined_flux_sum %>%
  group_by(niche) %>%
  group_modify(~broom::glance(kruskal.test(mean_nmol_c2h4_g_hr~mean_moist, data=.x)))

##### temperature only
ggplot(combined_flux_sum, aes(x = temp_C, y = mean_nmol_c2h4_g_hr)) + geom_point(size=3)+ geom_smooth(method = "lm", se = F) +
  theme_classic(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  xlab("Temperature (°C)") +
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1})))


## model for above graph
combined_flux_sum %>%
  group_modify(~broom::glance(lm(log(mean_nmol_c2h4_g_hr+0.0001)~temp_C, data=.x)))

ggplot(combined_flux_sum, aes(x = temp_C, y = mean_nmol_c2h4_g_hr)) + geom_point(size=3)+ 
  facet_wrap(~niche, scales = "free") + 
  geom_smooth(method = "lm",formula = y~poly(x,2), se = F, linetype = 2, linewidth = 1.25) +
  geom_errorbar(aes(ymin = mean_nmol_c2h4_g_hr-sem, ymax=mean_nmol_c2h4_g_hr+sem, width=4))+
  theme_bw(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  xlab("Temperature (°C)") +
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1})))




### heterotrophs
hetero_flux_sum <- combined_flux_sum %>% filter(niche %in% c("Soil", "Litter", "Wood"))
ggplot(hetero_flux_sum, aes(x = mean_moist, y = rank(mean_nmol_c2h4_g_hr))) + geom_point(size=3)+ geom_smooth(method = "lm", se = F) +
  theme_bw(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Average moisture content (% w/w)")
## model for above graph
summary(lm(mean_nmol_c2h4_g_hr~mean_moist, data=hetero_flux_sum))
summary(lm(rank(mean_nmol_c2h4_g_hr)~mean_moist, data=hetero_flux_sum))
kruskal.test(mean_nmol_c2h4_g_hr~mean_moist, data=hetero_flux_sum)
hetero_rancova <- hetero_flux_sum %>%
  group_modify(~broom::tidy(aov(rank(mean_nmol_c2h4_g_hr) ~ temp_C * sqrt(mean_moist+0.01), data = .x)))

ggplot(hetero_flux_sum, aes(x = temp_C, y = mean_nmol_c2h4_g_hr)) + 
  geom_point(size=4) + 
  geom_smooth(method = "lm", formula = y~poly(x,2),se = F, linetype = 2, linewidth=2) +
  scale_x_continuous(breaks = c(5, 15, 25, 35))+
  theme_bw(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='blank')) +
  theme_classic()+
  theme(text = element_text(size = 20, color = "black"), axis.text = element_text(color = "black"))+
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Temperature (°C)")
## model for above graph
ggplot(hetero_flux_sum, aes(x = log(mean_nmol_c2h4_g_hr))) + geom_histogram()
shapiro.test(log(hetero_flux_sum$mean_nmol_c2h4_g_hr+0.0001))
hetero_flux_sum %>% levene_test(mean_nmol_c2h4_g_hr~factor(temp_C))
summary(lm(mean_nmol_c2h4_g_hr~temp_C, data=hetero_flux_sum))
cor(hetero_flux_sum$mean_nmol_c2h4_g_hr, hetero_flux_sum$temp_C)
kruskal.test(mean_nmol_c2h4_g_hr~temp_C, data=hetero_flux_sum)

hfs<- hetero_flux_sum %>%
  group_by(temp_C, actual_moist_group) %>%
  summarize(n_sum = sum(n))


#### autotrophs ####
auto_flux_sum <- combined_flux_sum %>% filter(niche %in% c("Moss", "Lichen"))
ggplot(auto_flux_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr)) + geom_point(size=3)+ geom_smooth(method = "lm", se = F) +
  theme_bw(base_size = 18) +
  theme(legend.position = c(.85,.25)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))+
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) +
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Average moisture content (% w/w)")
summary(lm(mean_nmol_c2h4_g_hr~mean_moist, data=auto_flux_sum))
kruskal.test(mean_nmol_c2h4_g_hr~mean_moist, data=auto_flux_sum)
### ANCOVA FOR ABOVE
auto_flux_sum %>%
  group_modify(~broom::tidy(aov(lm(log(mean_nmol_c2h4_g_hr+0.0001)~log(temp_C)*mean_moist, data=.x))))
auto_flux_sum %>%
  group_modify(~broom::tidy(aov(rank(mean_nmol_c2h4_g_hr) ~ temp_C * sqrt(mean_moist+0.01), data = .x)))

ggplot(auto_flux_sum, aes(x = temp_C, y = mean_nmol_c2h4_g_hr)) + geom_point(size=3)+ 
  geom_smooth(method = "lm", formula = y~x, se = F, linetype = 2, linewidth=2 ) +
  theme_bw(base_size = 18) +
  theme_classic()+
  theme(text = element_text(size = 20, color = "black"), axis.text = element_text(color = "black"))+
  theme(legend.position = c(.85,.25)) +
  scale_x_continuous(breaks = c(5, 15, 25, 35)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='blank'))+
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='blank')) +
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Temperature (°C)")
summary(lm(log(mean_nmol_c2h4_g_hr+0.0001)~temp_C, data=auto_flux_sum))

## interaction
ggplot(hetero_flux_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr, color = factor(temp_C))) + 
  geom_point(size=4)+ 
  facet_wrap(~niche, scales = "free") + 
  geom_errorbar(aes(ymin = mean_nmol_c2h4_g_hr-sem, ymax=mean_nmol_c2h4_g_hr+sem, width=8))+
  geom_errorbarh(aes(xmax = mean_moist+sd_moist, xmin = mean_moist-sd_moist, height=.02))+
  geom_smooth(method = "lm", formula = y~poly(x,2), se = F, linetype = 2, linewidth=1.5 ) +
  scale_color_manual(values = BluRed, name = "Temperature (°C)") +
  theme_classic(base_size = 18) +
  theme(text = element_text(size = 28, colour = "black"), axis.text = element_text(colour = "black"))+
  theme(legend.position = "none") +
  ylim(0,0.69) + 
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Average moisture content (% w/w)")

ggplot(auto_flux_sum, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr, color = factor(temp_C))) + 
  geom_point(size=4)+ 
  facet_wrap(~niche, scales = "free") + 
  geom_errorbar(aes(ymin = mean_nmol_c2h4_g_hr-sem, ymax=mean_nmol_c2h4_g_hr+sem, width=8))+
  geom_errorbarh(aes(xmax = mean_moist+sd_moist, xmin = mean_moist-sd_moist, height= (mean_nmol_c2h4_g_hr*0.1)))+
  geom_smooth(method = "lm", formula = y~log(x), se = F, linetype = 2, linewidth=1.5 ) +
  scale_color_manual(values = BluRed, name = "Temperature (°C)") +
  theme_classic(base_size = 18) +
  theme(text = element_text(size = 28, colour = "black"), axis.text = element_text(colour = "black"))+
  theme(legend.position = "bottom") + 
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='blank')) +
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1}))) +
  xlab("Moisture content (% w/w)") 

          