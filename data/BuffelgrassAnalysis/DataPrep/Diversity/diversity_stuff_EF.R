# do diversity stuff
library(vegan)
source("div_data_cleanup.R")
library(tidyverse)
library(dplyr)

setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis/Analysis/Diversity_a")


div_input<- read.csv("diversity_cleaned_EF.csv")



div_wide <- div_input %>%
  dplyr::select(plot, subplot, scientific_name_clean, percent_cover) %>%
  group_by(plot, scientific_name_clean) %>%
  summarise(cover = sum(percent_cover, na.rm=T)/9) %>%
  ungroup() %>%
  pivot_wider(id_cols = plot, names_from = scientific_name_clean, 
              values_from = cover, values_fill =list(cover=0)) %>%
  tibble::column_to_rownames("plot")

#would you divide the sum cover by 8 and then add the trace amounts from the walk-about?

# div indexes
diversity(div_wide, index = c("shannon"))
diversity(div_wide, index = c("simpson"))


#make shannon and simpson their own objects
  
shannon<-diversity(div_wide, index = c("shannon"))
simpson<-diversity(div_wide, index = c("simpson"))  

#Now make them into dataframes
shannon<-as.data.frame(shannon)
shannon<-tibble::rownames_to_column(shannon, "site_plot")

simpson<-as.data.frame(simpson)
simpson<-tibble::rownames_to_column(simpson, "site_plot")

# add diversity index to percent PECI cover data
cover<-read.csv("cover.csv")

PECI<-cover%>%
  left_join(shannon,  by = c('site_plot'), na.rm=FALSE)

PECI<-PECI%>%
  left_join(simpson,  by = c('site_plot'), na.rm=FALSE)


#calculate richness and evenness and add them to PECI data too

richness<-div_input%>%
  group_by(plot)%>%
  summarize(richness=n_distinct(scientific_name_clean, na.rm= TRUE))

PECI<-PECI%>%
  left_join(richness,  by = c('site_plot'= 'plot'), na.rm=FALSE)

PECI<- PECI %>%
  mutate(evenness= shannon/(log(richness)))

write_csv(PECI, "Biodiversity.csv")

#ggplot of cover by diversity
ggplot(PECI, aes(x=PECI_cover, y=shannon, color=site, size=1)) +
  geom_point()+
  theme_bw()

#ggplot of cover by richness
ggplot(PECI, aes(x=PECI_cover, y=richness, color=site, size=1)) +
  geom_point()+
  theme_bw()

#ggplot of cover by evenness
ggplot(PECI, aes(x=PECI_cover, y=evenness, color=site, size=1)) +
  geom_point()+
  theme_bw()

#lets look at PECI cover by all cover

ggplot(PECI, aes(x=PECI_cover, y=ALL_cover, color=site, size=1)) +
  geom_point()+
  theme_bw()

