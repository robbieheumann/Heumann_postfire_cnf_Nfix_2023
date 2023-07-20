## cryptic seasonal data QC and tidying
library(tidyverse)
library(generics)
# import "copy of July samples" 
#examine drymasses for duplicates and QC for all samples

drymass <- Copy_of_July_Samples
distinct(drymass)
sum(duplicated(drymass))
unique(drymass)
cleaned_drymass <- distinct(drymass)

tallies <- cleaned_drymass %>% group_by(sample_name, Treatment, niche) %>%
  summarize(no_samples = length(tin_dry_mass))

# export cleaned to excel for further QC
write.csv(cleaned_drymass, "C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Cumulative Flux Calcs/Julydrymasses.csv")


## import GC data and drymass_dirty to combine ppm data with masses
GC_data <- Julydrymasses_dirty
mass <- Julydrymasses_dirty

unique(mass$sample_ID)
unique(GC_data$sample_ID)
unique(mass$sample_name)
unique(GC_data$sample_name)

match(GC_data$sample_ID, mass$sample_ID)


ppm_mass_merge <- merge(mass, GC_data, by = c("sample_name", "sample_ID", "niche", "Treatment"))

write.csv(ppm_mass_merge, "C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Cumulative Flux Calcs/ppm_mass_merge.csv")


#join rhizo and endos
rhizo_mass <- Julydrymasses_dirty
rhizo_merge <- merge(rhizo_mass, rhizo_ppm, by=c("niche", "sample_name", "sample_ID"))

glimpse(rhizo_mass)
glimpse(rhizo_ppm)

write.csv(rhizo_merge, "C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Cumulative Flux Calcs/rhizo_merge.csv")


## July actual blank corrections: ******C = blank******
#import ppm_mass_merge page and blanks pages from ppm_mass_merge
bag_blanks <- ppm_mass_merge_blanks
blank_sum <- blanks %>% group_by(niche, Treatment) %>% summarize(avg_blank = mean(ppm))
bag_blank_sum <- bag_blanks %>% group_by(Bag_ID) %>% summarize(avg_bag_blank = mean(ppm))

July_ppm_corrected <- merge(ppm_mass_merge_blanks, blank_sum, by = c("niche", "Treatment"))
July_ppm_corrected <- merge(July_ppm_corrected, bag_blank_sum, by = c("Bag_ID"))
July_ppm_corrected <- July_ppm_corrected %>% select(-corrected_ppm)
July_ppm_corrected <- July_ppm_corrected %>% mutate(corrected_ppm = ppm - (avg_blank + avg_bag_blank))

write.csv(July_ppm_corrected, "C:/Users/rh176228/Box/GYE Nitrogen Fixation Folder/Robbie Data/Summer 2022/Cumulative Flux Calcs/ppm_corrected.csv")


##### CLIMATE EXPERIMENT DATA CLEANING AND MERGING #####

