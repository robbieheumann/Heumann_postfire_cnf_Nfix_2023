### June climate experiment blank corrections update
### import sample only blanks and compiled GC data from june raw data folder
library(tidyverse)
library(readxl)
june_clim_exp_blanks <- read_excel("cryptic_clim_exp_2022/0_data/June_2022_ClimExp_rawdata/june_clim_exp_blanks.xlsx", sheet = "Sheet1")
blanks <- june_clim_exp_blanks %>% select(-"...7")
library(readr)
june_clim_exp_compiled_GC <- read_csv("cryptic_clim_exp_2022/0_data/June_2022_ClimExp_rawdata/june_clim_exp_compiled_GC.csv")


## group by blanks by sample and calculate mean and median blank then merge with GC data
blank_sum <- blanks %>% group_by(sample) %>%
  summarize(mean_blank = mean(ppm), median_blank = median(ppm), n = length(ppm))
## merge
ppm <- june_clim_exp_compiled_GC

ppm_merge <- merge(ppm, blank_sum, by = "sample")
ppm_merge <- ppm_merge %>% mutate(Bag_ID = ifelse(temp == 5, "A", ifelse(temp== 15, "B", ifelse(temp==25, "C", "D"))))
bag_blank_sum <- blank_sum %>% filter(sample %in% c("Bag_A", "Bag_B", "Bag_C", "Bag_D")) %>%
  mutate(Bag_ID = c("A", "B", "C", "D"))
ppm_merge <- merge(ppm_merge, bag_blank_sum, by= "Bag_ID")

### calculate ppm corrections
ppm_merge <- ppm_merge %>% mutate(ppm_corrected = ppm-(median_blank.x+median_blank.y))
### get rid of sample only blank rows AKA sample no = 6
ppm_merge <- ppm_merge %>% filter(sample_no != 6)
ppm_merge <- ppm_merge %>% filter(sample_no != 12)
ppm_merge <- ppm_merge %>% filter(sample_no != 18)
ppm_merge <- ppm_merge %>% filter(sample_no != 24)

write.csv(ppm_merge, "cryptic_clim_exp_2022/0_data/June_2022_ClimExp_rawdata/june_clim_exp_updated_blank_corrections.csv")
