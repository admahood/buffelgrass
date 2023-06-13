#buffelgrass soil bulk density
#November 4, 2020
#Dr. R. Chelsea Nagy and Dr. Emily Fusco

library(tidyverse)
require(ggplot2)

setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")

soilbd <- read.csv("DataPrep/Soil/bulk_density_drying_data.csv")

soilbd <- soilbd %>%
  select(-notes, -additional_weight)

adding <- soilbd %>%
  group_by(plot_id, cover_type) %>%
  mutate(sample_mass_g = sum(net_dry_weight)) %>%
  mutate(sn = paste(plot_id, cover_type, sep = "_"))

#remove duplicate rows now that bags / sample have been added together

soilbds <-adding[!duplicated(adding$sn), ]

soilbds <- soilbds %>% 
  rename(plot = plot_id)

soilbds$plot <- gsub('rr', 'RR', soilbds$plot)
soilbds$plot <- gsub('lola', 'Lola', soilbds$plot)

soilbds$sn <- gsub('lola', 'Lola', soilbds$sn)
soilbds$sn <- gsub('rr', 'RR', soilbds$sn)

#add in volume here (cm3)...for now using 1000 which isn't the real number
#Chelsea, confirm with Emily that pits were 10 cm x 10 cm x 10 cm#

cm3 <- 1000
soilbds$BD_g_cm3 <- soilbds$sample_mass_g / cm3


#look at these to make sure they seem reasonable
#use this one in carbon content calculations; plot-cover 
means1 <- soilbds %>%
  group_by(plot, cover_type) %>%
  summarise(meanBD = mean(BD_g_cm3, na.rm = TRUE)) %>%
  rename(BD_g_cm3 = meanBD)

ggplot(means1, aes(x=cover_type, y=BD_g_cm3))+
  geom_boxplot()


#there is a lot of variation in these; low numbers could be do to pits with rocks (rocks are not accounted for here)
#could throw out the pits that had large rocks?

#also calculate means based on site and cover

means2<- soilbds%>%
  separate(plot, c("site", "plot2"), remove=FALSE)
  
means2 <- means2 %>%
  group_by(site, cover_type) %>%
  summarise(meanBD = mean(BD_g_cm3, na.rm = TRUE)) %>%
  rename(BD_g_cm3 = meanBD)

median1<- soilbds%>%
  separate(plot, c("site", "plot2"), remove=FALSE)
median1 <- median1 %>%
  group_by(site, cover_type) %>%
  summarise(medBD = median(BD_g_cm3, na.rm = TRUE)) %>%
  rename(BD_g_cm3 = medBD)


#put into output folder for data prep
write.csv(x = means1, file = "DataPrep/Soil/output/plotcoverBD.csv", row.names = FALSE)
write.csv(x = means2, file = "DataPrep/Soil/output/plotcoverBDsitemeans.csv", row.names = FALSE)
write.csv(x = median1, file = "DataPrep/Soil/output/plotcoverBDsitemedians.csv", row.names = FALSE)
#put into input folder for analysis
write.csv(x = means1, file = "Analysis/Soil_a/plotcoverBD.csv", row.names = FALSE)
write.csv(x = means2, file = "Analysis/Soil_a/plotcoverBDsitemeans.csv", row.names = FALSE)
write.csv(x = median1, file = "Analysis/Soil_a/plotcoverBDsitemedians.csv", row.names = FALSE)


#some exploratory plots
plotcoverBD <- means1

#use this one for any missing plots by cover type
means2 <- soilbds %>%
  group_by(cover_type) %>%
  summarise(meanBD = mean(BD_g_cm3, na.rm = TRUE))

#Lola_04 looks a bit funky...consider throwing this one out
means3 <- soilbds %>%
  group_by(plot) %>%
  summarise(meanBD = mean(BD_g_cm3, na.rm = TRUE))


ggplot(subset(means1, cover_type=="bare"), aes(x=BD_g_cm3))+
  geom_histogram(bins=10)+xlab("Bare BD_g_cm3")

ggplot(subset(means1, cover_type=="buffel"), aes(x=BD_g_cm3))+
  geom_histogram(bins=10)+xlab("Buffel BD_g_cm3")

ggplot(subset(means1, cover_type=="other"), aes(x=BD_g_cm3))+
  geom_histogram(bins=10)+xlab("Other BD_g_cm3")
