### Median corrections
library(tidyverse)
library(MASS)

ppm_corrected_median <- read_csv("~/Desktop/ppm_corrected_median.csv", col_types = cols(X1 = col_skip()))

ppm <- rename(ppm_corrected_median, "ppm_median_corrected" = ppm_orreted)

ppm <- ppm %>% mutate(ppm_mean_corrected = ppm - (avg_blank + bag_avg))

