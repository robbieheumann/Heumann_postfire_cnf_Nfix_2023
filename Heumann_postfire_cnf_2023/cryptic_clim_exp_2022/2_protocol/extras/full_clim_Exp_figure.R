library(tidyverse)
ML_sum <- read.csv("cryptic_clim_exp_2022/1_incremental/ML_sum.csv")
SLW_sum <- read.csv("cryptic_clim_exp_2022/1_incremental/SLW_sum.csv")


head(ML_sum)
head(SLW_sum)

tidy_ML_sum <- ML_sum %>% select(c("niche", "temp", "mean_nmol_c2h4_g_hr", "sem","mean_moist", "sd_moist", "n"))
tidy_SLW_sum <- SLW_sum %>% select(c("niche", "temp_C", "mean_nmol_c2h4_g_hr", "sem","mean_moist", "sd_moist", "n"))
tidy_SLW_sum <- tidy_SLW_sum %>% rename("temp" = temp_C)
clim <- rbind(tidy_SLW_sum, tidy_ML_sum)
clim <- clim %>% mutate(ylim = ifelse(niche == "Lichen", 30, 81))
as.numeric(clim$ylim)
### figure
BluRed <- c("navy", "skyblue2", "orange", "red4")

ggplot(clim, aes(x = mean_moist, y = mean_nmol_c2h4_g_hr, 
                         color = factor(temp, levels = c('5', '15', '25', '35')),
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
  theme_classic() +
  theme(strip.text.x = element_text(size = 18, face ='bold'),
        text = element_text(size=18,face ='bold')) + 
  theme(legend.position = "bottom") +
  xlab("Mean moisture content (%w/w)")+
  ylab(expression(paste("nmol C"[2],"H"[4]," g"^{-1}," hr"^{-1})))

library(gridExtra)
grid.arrange(SLW_plot, ML_plot, nrow=2)

