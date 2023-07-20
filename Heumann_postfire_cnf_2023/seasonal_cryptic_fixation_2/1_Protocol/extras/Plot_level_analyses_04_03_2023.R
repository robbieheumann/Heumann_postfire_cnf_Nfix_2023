### plot level analyses
library(readr)
library(tidyverse)
library(esquisse)
library(patchwork)
combined_4seas_fluxes_2_6_2023 <- read_csv("seasonal_cryptic_fixation_2/2_Incremental/combined_4seas_fluxes_2_6_2023.csv")

season_order <- c('spring', 'summer', 'summer_wet', 'fall', 'annual')
dodge <- position_dodge(width=0.9)
cbbPalette <- c("#56B4E9", "#E69F00", "#009E73", "#D55E00", "#000000")
### plot inputs by niche by plot by season
ggplot(combined_4seas_fluxes_2_6_2023) +
  aes(y = flux_kgN_ha_season, 
      fill = niche, x = site) +
  geom_errorbar(aes(ymax = flux_kgN_ha_season + (sd_kgN_ha_season/sqrt(sample_n)), 
                    ymin = flux_kgN_ha_season - (sd_kgN_ha_season/sqrt(sample_n)), 
                    width=0.6), stat = "identity", position = dodge)+
  scale_fill_manual(values = cbbPalette) + geom_col(position = dodge) +
  theme_minimal() +
  theme(legend.position = "bottom") + 
  facet_grid(~factor(season, levels = season_order)) +
  theme_classic()+
  ylab(label = expression(paste("Cryptic N fixation flux (kg N ha"^{-1},")"))) + 
  xlab(label = "Site") +
  theme(strip.text.x = element_text(size = 15),
        text = element_text(size=13, face = "bold", color = 'black')) +
  theme(legend.position = c(.9,.75)) +
  theme(legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'))

seas_flux <- combined_4seas_fluxes_2_6_2023[1:9]

seas_flux %>% pivot_wider(names_from = site, values_from=c(flux_kgN_ha_season, median_flux_kgN_ha_season)
)
                          