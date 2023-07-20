### Median corrections
library(tidyverse)
library(MASS)

ppm_corrected_median <- read_csv("cryptic_clim_exp_2022/1_incremental/ppm_corrected_median.csv")

ppm <- rename(ppm_corrected_median, "ppm_median_corrected" = ppm_orreted)

ppm <- ppm %>% mutate(ppm_mean_corrected = ppm - (avg_blank + bag_avg))

## now send back to excel to finihs flux calcs, which are in clim_flux_calcs_2

