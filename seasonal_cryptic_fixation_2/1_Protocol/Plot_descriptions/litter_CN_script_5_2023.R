### Litter table script fo rcryptic seasons paper
### import Turner C:N data from 0_data external Plotcover_litter_CWD folder "results tab" 
Litter_chem <- read_excel("seasonal_cryptic_fixation_2/0_Data/external/Plot_level_cover_litter_CWD/Copy of Turner Litter CN_013123_031523.xlsx", 
                            sheet = "Results_013123_031523")

## summarize by means of each plot 
Litter_chem_cryptics <- Litter_chem %>% filter(Site %in% c("BiscBas", "GibFalls", "GravPit", "FounEast", "FirLopSo"))

Litter_chem_cryptics$`%N` <- as.numeric(Litter_chem_cryptics$`%N`)
Litter_chem_cryptics <- rename(Litter_chem_cryptics, perc_N = `%N`)
Litter_chem_cryptics <- rename(Litter_chem_cryptics, perc_C = `%C`)
Litter_chem_cryptics %>% group_by(Site) %>% summarize(mean_N = mean(perc_N), sd_N = sd(perc_N),
                                                      mean_C = mean(perc_C), sd_C = sd(perc_C))
