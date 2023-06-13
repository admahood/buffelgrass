#buffelgrass herbaceous biomass carbon
#November 4, 2020
#Dr. R. Chelsea Nagy and Dr. Emily Fusco

library(tidyverse)
setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")


herbsamp <- as.data.frame(read_csv("DataPrep/AGB_herb/buffelgrass_herbaceous_samples.csv"))

#add samples from multiple bags/sample together
combining <- herbsamp %>%
  group_by(sample_number) %>%
  mutate(sample_biomass_g = sum(biomass_g))

#remove duplicate rows now that bags / sample have been added together

herbsamps <-combining[!duplicated(combining$sample_number), ]

#clipping frame was 2 m x 0.1 m =  0.2 m2

#convert biomass/sample to biomass/area by dividing by 0.2

herbsamps <- herbsamps %>%
  mutate(sample_biomass_g_m2 = sample_biomass_g / 0.2)

#bring in carbon data for each sample

herbcarb <- as.data.frame(read_csv("DataPrep/AGB_herb/buffelgrass_herbaceous_carbon_data.csv"))

herbcarb2 <- separate(data = herbcarb, col = Sample, into = c("sample_number", "plot"), sep = "\\ ")
is.numeric(herbcarb2$sample_number)

herbcarb2$sample_number <- as.numeric(herbcarb2$sample_number)
is.numeric(herbcarb2$sample_number)

#join dataframes together
herbcarb3 <- full_join(herbsamps, herbcarb2, by = c("sample_number", "plot"))

herbcarb4 <- herbcarb3 %>%
  mutate (herbcarb_g_m2 = sample_biomass_g_m2 * percC/100) %>%
  mutate (herbnit_g_m2 = sample_biomass_g_m2 * percN/100) 

#cleaner dataframe with the most useful variables
herbcarbclean <- herbcarb4 %>%
  select(sample_number, plot, sub_plot, sample_biomass_g_m2, percC, percN, herbcarb_g_m2, herbnit_g_m2)

#change Lola to LOLA

herbcarbclean<- herbcarbclean %>% 
  separate(plot, c("site", "plot"))

herbcarbclean<- herbcarbclean %>%
  mutate (site= 
            ifelse (((site=="Lola")), "LOLA", site))

herbcarbclean <- herbcarbclean %>% 
  mutate(site_plot = paste(site, plot, sep = "_"))


# add AGB to percent PECI cover data
cover<-read.csv("DataPrep/AGB_herb/cover.csv")

herbcarbclean<-cover%>%
  left_join(herbcarbclean,  by = c('site_plot'='site_plot'), na.rm=FALSE)

herbcarbclean2<-herbcarbclean%>%
  select(-site.y, -plot.y)%>%
  rename(site = site.x,plot = plot.x)

#send this cleaned data to the output folder and to the herb carb analysis folder

write.csv(herbcarbclean2, file="DataPrep/AGB_herb/output/herb_carb_clean.csv", row.names = FALSE)
write.csv(herbcarbclean2, file="Analysis/AGB_herb_a/herb_carb_clean.csv", row.names = FALSE)

