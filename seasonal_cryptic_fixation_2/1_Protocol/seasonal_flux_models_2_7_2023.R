#### seasonal flux modeling ####
library(tidyverse)
library(ggpubr)
library(multcompView)
library(RColorBrewer)
library(esquisse)
library(rstatix)
dodge <- position_dodge(width=0.9)
# re-order x axis (spring - summer - fall)
season_order <- c('spring', 'summer', 'summer_wet', 'fall', 'annual')
### model 1: ANOVA of combined N inputs from CNF across seasons using fluxes_combined_3season

### import "combined_3seas_fluxes_2_6_2023
combined_3seas_fluxes_2_6_2023 <- read_csv("2_Incremental/combined_3seas_fluxes_2_6_2023.csv")

### normality tests
ggplot(combined_3seas_fluxes_2_6_2023, aes(x =  log(flux_kgN_ha_season+0.0001))) + geom_histogram()
shapiro.test(log(combined_3seas_fluxes_2_6_2023$flux_kgN_ha_season+0.0001))

### normality assumptions not met, non-parametric tests used from here on out (kruskal.test)
## across season anova using plots as replicates
season.k <- kruskal.test(flux_kgN_ha_season ~ season, combined_3seas_fluxes_2_6_2023)
season.k
dunn_test(combined_3seas_fluxes_2_6_2023, flux_kgN_ha_season ~ season)

plot.k <- kruskal.test(flux_kgN_ha_season ~ site, combined_3seas_fluxes_2_6_2023)
niche.k <- kruskal.test(flux_kgN_ha_season ~ niche, combined_3seas_fluxes_2_6_2023)
moist.k <- kruskal.test(flux_kgN_ha_season ~ mean_moist, combined_3seas_fluxes_2_6_2023)

combined_3seas_fluxes_2_6_2023 %>%
  group_map(~broom::tidy(aov(log(flux_kgN_ha_season+0.0001) ~ site + niche+ season+mean_moist, data = .x)))
combined_3seas_fluxes_2_6_2023 %>%
  group_map(~broom::tidy(kruskal.test(log(flux_kgN_ha_season+0.0001) ~ niche, data = .x)))
## none of these are significant factors affecting N fix except for season when KW is used, 
## but they all are except site when ANOVA is used,

## using GLMs?
combined_3seas_fluxes_2_6_2023 %>%
  group_map(~broom::tidy(glm(flux_kgN_ha_season ~ site + niche+ season+mean_moist, family = gaussian(), data = .x)))

# calculate season averages for figure display
seasonal_fluxes <- combined_3seas_fluxes_2_6_2023 %>% group_by(season, niche) %>%
  summarise(combined_mean_flux = mean(flux_kgN_ha_season), se_flux = sd(flux_kgN_ha_season/sqrt(length(flux_kgN_ha_season)))) %>%
  group_by(season) %>%
  mutate(se = sum(se_flux), total_flux = sum(combined_mean_flux)) %>% ## 5 plots = n for SE calc
  ungroup()

## make a annual combined table to join with season fluxes so plot can display annual flux next two seasonal fluxes
annual_combined_flux <- seasonal_fluxes %>% group_by(niche) %>%
  summarize(combined_mean_flux = sum(combined_mean_flux),
            se_flux = sum(se_flux))
annual_combined_flux <- annual_combined_flux %>% mutate(season = "annual", se = sum(se_flux), total_flux = sum(combined_mean_flux))
seasonal_annual_fluxes <- rbind(annual_combined_flux, seasonal_fluxes)

median_fluxes <- combined_4seas_fluxes_2_6_2023 %>% group_by(season, niche) %>%
  summarize(median_flux = median(flux_kgN_ha_season))

### remove niche grouping
seasonal_fluxes_noniche <- seasonal_fluxes %>% group_by(season) %>%
  summarize(mean_flux = mean(total_flux),
            se_flux = mean(se)) %>%
  arrange(desc(mean_flux))

## merge cld letters for graph visualization
cld <- list("a","a","b")
seasonal_fluxes_noniche$cld <- cld

## add annual inputs row
annual_only <- seasonal_annual_fluxes %>% filter(season == "annual") %>% group_by(season) %>% 
  summarize(mean_flux = mean(total_flux), se_flux = mean(se), cld = " ")
seasonal_fluxes_noniche <- rbind(seasonal_fluxes_noniche, annual_only)
cbbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#D55E00", "#000000")
### plot total fluxes, with individual niches
ggplot(seasonal_fluxes_noniche,
       aes(x = factor(season,levels=season_order), y = mean_flux)) +
     geom_col(data = seasonal_annual_fluxes, aes(y = combined_mean_flux, fill = niche)) + 
  scale_fill_manual(values = cbbPalette, name = "Niche")+
  geom_errorbar(data = seasonal_fluxes_noniche, aes(x = season, 
                                                    ymax = mean_flux + se_flux,
                                                    ymin = mean_flux-se_flux), width=0.2) + 
  geom_text(data = seasonal_fluxes_noniche, aes(x = season, y = mean_flux, label = cld), 
            size = 5, vjust=-1, hjust =-1) +
    labs(title = "N fluxes from cryptic fixation across seasons")+
  ylab(label = expression(paste("N fixation flux (kg N ha"^{-1},")"))) + 
  xlab(label = "Season") + 
  theme_classic(base_size = 18) +
  theme(legend.position = c(.5,.75), legend.text = element_text(size=18)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))
  
### individual anova across seasons and niches
combined_3seas_fluxes_2_6_2023 %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(kruskal.test(flux_kgN_ha_season~season, data=.)))
combined_3seas_fluxes_2_6_2023 %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(kruskal.test(flux_kgN_ha_season~mean_moist, data=.)))
combined_3seas_fluxes_2_6_2023 %>%
  group_by(niche) %>%
  group_modify(~broom::tidy(kruskal.test(flux_kgN_ha_season~site, data=.)))


##### same as above but with 4 seasons ######3
combined_4seas_fluxes_2_6_2023 <- read_csv("2_Incremental/combined_4seas_fluxes_2_6_2023.csv")
seasonal_fluxes_4seas <- combined_4seas_fluxes_2_6_2023 %>% group_by(season, niche) %>%
  summarise(combined_mean_flux = mean(flux_kgN_ha_season), se_flux = sd(flux_kgN_ha_season/sqrt(length(flux_kgN_ha_season)))) %>%
  group_by(season) %>%
  mutate(se = sum(se_flux), total_flux = sum(combined_mean_flux)) %>% ## 5 plots = n for SE calc
  ungroup()



## make a annual combined table to join with season fluxes so plot can display annual flux next two seasonal fluxes
annual_combined_flux_4seas <- seasonal_fluxes_4seas %>% group_by(niche) %>%
  summarize(combined_mean_flux = sum(combined_mean_flux),
            se_flux = sum(se_flux))
annual_combined_flux_4seas <- annual_combined_flux_4seas %>% mutate(season = "annual", se = sum(se_flux), total_flux = sum(combined_mean_flux))
seasonal_annual_fluxes_4seas <- rbind(annual_combined_flux_4seas, seasonal_fluxes_4seas)


### remove niche grouping
seasonal_fluxes_noniche_4seas <- seasonal_fluxes_4seas %>% group_by(season) %>%
  summarize(mean_flux = mean(total_flux),
            se_flux = mean(se)) %>%
  arrange(desc(mean_flux))

## add annual inputs row
annual_only_2 <- seasonal_annual_fluxes_4seas %>% filter(season == "annual") %>% group_by(season) %>% 
  summarize(mean_flux = mean(total_flux),
            se_flux = mean(se))
seasonal_fluxes_noniche_4seas <- rbind(seasonal_fluxes_noniche_4seas, annual_only_2)
cbbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#D55E00", "#000000")
### plot total fluxes, with individual niches
ggplot(seasonal_fluxes_noniche_4seas,
       aes(x = factor(season,levels=season_order), y = mean_flux)) + 
  geom_errorbar(data = seasonal_fluxes_noniche_4seas, aes(x = season, 
                                                    ymax = mean_flux + se_flux,
                                                    ymin = mean_flux-0), width=0.2)  +
  geom_col(data = seasonal_annual_fluxes_4seas, aes(y = combined_mean_flux, fill = niche)) + 
  scale_fill_manual(values = cbbPalette)+
  labs(title = "N fluxes from cryptic fixation across seasons") +
  ylab(label = expression(paste("N fixation flux (kg N ha"^{-1},")"))) + 
  xlab(label = "Season") + 
  theme_bw(base_size = 18) +
  theme(legend.position = c(.5,.75)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))


seas4_kw_results <- combined_4seas_fluxes_2_6_2023 %>%
  group_modify(~broom::tidy(kruskal.test(flux_kgN_ha_season~season, data=.)))

nichedunn <- dunn_test(combined_4seas_fluxes_2_6_2023 %>% group_by(niche), flux_kgN_ha_season ~ season)

seas4_aov_TukHSD_results <- combined_4seas_fluxes_2_6_2023 %>%
  group_modify(~broom::tidy(TukeyHSD(aov(flux_kgN_ha_season~season, data=.))))


#### Before vs after rain plot using fluxes_combined_4season

rain_fluxes <- combined_4seas_fluxes_2_6_2023 %>%
  filter(season %in% c("summer", "summer_wet"))
rain_fluxes$season[rain_fluxes$season == "summer"] <- "before"
rain_fluxes$season[rain_fluxes$season == "summer_wet"] <- "after"
rain_flux_sum <- rain_fluxes %>% group_by(season, niche) %>%
  summarize(mean_flux = mean(flux_kgN_ha_season),
            sample_n = 5,
            SE_flux = sd(flux_kgN_ha_season)/sqrt(sample_n),
            mean_moist = mean(mean_moist))
## filter out just summer wet (after rain) and summer (after 3 weeks of hot and dry conditions)
plot(lm(log(flux_kgN_ha_season+0.0001)~season, rain_fluxes))
### not normal
ggplot(rain_fluxes,aes(x=log(flux_kgN_ha_season+0.0001))) + geom_histogram()
## non-parametric kruskal_wallis
## divide mean flux and se_flux by 60.83 to get one day of flux 
## 1460 hours for 2 months (1 season) / 24 hours in 1 day = ~60.83 days so
## 1460/60.83 = ~ 24 hours
rain_flux_sum <- rain_flux_sum %>% mutate(daily_flux_g = (mean_flux/60.833)*1000,
                         daily_flux_se_g = (SE_flux/60.833)*1000)


## model = simple Kruskal wallis with N fixation flux as response to before or after rain across niches
rain_fluxes %>% group_by(niche) %>%
  group_modify(~ broom::tidy(kruskal.test(flux_kgN_ha_season ~ season, data=.x)))
### wood is significant
### plot with p-values

rain_plot<- ggplot(rain_flux_sum, aes(x = factor(season, levels = c("before", "after")), y =daily_flux_g, fill = niche)) + 
  geom_col(position = dodge) + facet_wrap(~niche, scales = "free") +
  geom_errorbar(aes(ymin = daily_flux_g - daily_flux_se_g, ymax = daily_flux_g + daily_flux_se_g), width = 0.25, position = dodge)+
  labs(title = "Cryptic fixation N inputs before and after rain") +
  ylab(label = "N fixation flux (g N ha-1 day-1)") + 
  xlab(label = element_blank()) +
  theme(text = element_text(size=18), legend.position = "none", plot.background = element_rect(colour = 'black', fill = 'white', linetype='solid')) 

## or
my_comparisons <- list(c("before", "after"))
rp_2 <- ggboxplot(rain_fluxes, x = "season", y = "flux_kgN_ha_season", 
          color = "niche",
          short.panel.labs = FALSE, size = 1, 
          order = c("before", "after"))
## now facet with free scales and kruskal-wallis results
library(rstatix)
stat_test <- rain_fluxes %>%
  group_by(niche) %>%
  t_test(flux_kgN_ha_season~season) %>% 
  add_significance()
stat_test <- stat_test %>% add_xy_position(x="season")
## for the sake of this graph im using t-test although that is not the actual test I use, 
## p-values differ but none the less the significance levels are the same. 

facet(rp_2, facet.by = "niche", scales = "free", panel.labs.font = list(size = 18, face ='bold'), 
      panel.labs.background =list(fill = "white"))+
  stat_pvalue_manual(stat_test, label.size = 6, y.position = 0.14,hide.ns = T)+
  geom_point(data = rain_fluxes, aes(x = season, y = flux_kgN_ha_season, color = niche), size=4) + 
  ylab(label = expression(paste("N fixation flux (g N g"^{-1}, "day"^{-1},")"))) +
  scale_color_manual(values = cbbPalette)+
  xlab(label = "Season") + theme(text = element_text(size = 18))


### differences in rain sample moisture contents
rain_fluxes %>% group_by(season, niche) %>%
  summarize(mean = mean(mean_moist), sd_moist = sd(mean_moist))


shapiro.test(log(rain_fluxes$mean_moist+0.001))
hist(rain_fluxes$mean_moist)
# unequal variance, non normal
rain_moist.kw<-kruskal.test(mean_moist ~ season, rain_fluxes)
nichedunn_moist <- dunn_test(combined_4seas_fluxes_2_6_2023 %>% group_by(niche), mean_moist ~ season)

summary(rain_moist.kw)
59/1.07
42.7/1.68
## table for paper
seasonal_fluxes_4seas <- combined_4seas_fluxes_2_6_2023 %>% group_by(season, niche) %>%
  summarise(combined_mean_flux = mean(flux_kgN_ha_season), median_flux = median(flux_kgN_ha_season), 
            sd_flux = sd(flux_kgN_ha_season), 
            se_flux = sd(flux_kgN_ha_season/sqrt(length(flux_kgN_ha_season))),
            mean_moist = mean(mean_moist)) %>%
  group_by(season) %>%
  mutate(se_total = sum(se_flux), sd_total=sum(sd_flux),total_flux = sum(combined_mean_flux)) %>% ## 5 plots = n for SE calc
  ungroup()
#write.csv(seasonal_fluxes_4seas, "seasonal_cryptic_fixation_2/3_output/seasonal_fluxes.csv")

seasonal_fluxes_4seas %>% group_by(season, niche) %>% summarize(moist = mean(mean_moist))


## annual inputs by niche
seasonal_fluxes_3seas_niche <- combined_3seas_fluxes_2_6_2023 %>% group_by(season, niche) %>%
  summarise(combined_mean_flux = mean(flux_kgN_ha_season), median_flux = median(flux_kgN_ha_season), 
            sd_flux = sd(flux_kgN_ha_season), 
            se_flux = sd(flux_kgN_ha_season/sqrt(length(flux_kgN_ha_season)))) %>%
  group_by(season) %>%
  mutate(se_total = sum(se_flux), sd_total=sum(sd_flux),total_flux = sum(combined_mean_flux)) %>% ## 5 plots = n for SE calc
  ungroup()

## no niche
seasonal_fluxes_3seas_noniche <- combined_3seas_fluxes_2_6_2023 %>% group_by(season) %>%
  summarise(combined_mean_flux = mean(flux_kgN_ha_season), median_flux = median(flux_kgN_ha_season), 
            sd_flux = sd(flux_kgN_ha_season), 
            se_flux = sd(flux_kgN_ha_season/sqrt(length(flux_kgN_ha_season)))) %>%
  group_by(season) %>%
  mutate(se_total = sum(se_flux), sd_total=sum(sd_flux),total_flux = sum(combined_mean_flux)) %>% ## 5 plots = n for SE calc
  ungroup()


sum(seasonal_fluxes_3seas_niche$combined_mean_flux)

annual_fluxes <- seasonal_fluxes_3seas_niche %>% group_by(niche) %>%
  summarize(annual_flux = sum(combined_mean_flux), se_flux = sum(se_flux))

ggplot(annual_fluxes) +
  aes(x = niche, y = annual_flux, fill = niche) + 
  geom_col() + 
  geom_errorbar(aes(ymax = annual_flux + se_flux,
                    ymin = annual_flux-se_flux), width=0.2)+
  scale_fill_manual(values = cbbPalette) +
  labs(title = "N fluxes from cryptic fixation throughout growing season") +
  ylab(label = expression(paste("N fixation flux (kg N ha"^{-1}, "yr"^{-1},")"))) + 
  xlab(label = "") + 
  theme_bw(base_size = 18) +
  theme(legend.position = "none") +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))




