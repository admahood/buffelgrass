setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")

AGB_input<- read.csv("DataPrep/AGB_woody/output/AGB_cleaned_EF.csv")

require(tidyverse)

# add AGB to percent PECI cover data
cover<-read.csv("DataPrep/AGB_woody/cover.csv")

PECI_AGB<-cover%>%
  left_join(AGB_input,  by = c('site_plot'='plot'), na.rm=FALSE)


################################################
##VOLUME CALCULATIONS##
################################################

#change whole plot so it is one word
AGB_input<- AGB_input %>%
  mutate (subplot= 
            ifelse (((subplot=="whole plot")), "wholeplot", subplot))

#we'll need to deal with two species later
AGB_inputsub<- AGB_input %>%
  filter(scientific_name_clean!= c("Opuntia engelmannii"))

AGB_inputsub<- AGB_inputsub %>%
  filter(scientific_name_clean!= c("Carnegiea gigantea"))

#now onto the species we can calculate now!

#create a unique ID so that we can pivot wider based on this ID
AGB_inputsub <- AGB_inputsub %>% 
  mutate(id = paste(plot, subplot, individual_number, datasheet_number, scientific_name_clean, sep = "_"))

#pivot wider based on new ID
AGB_wide <- AGB_inputsub %>%
  pivot_wider(id_cols = id, names_from = measurement_variable_clean, 
              values_from = measurement)

#need to make sure all measurements needed to calculate cylinder volume are there

#check the heights
heightcheck<- AGB_wide%>%
  filter(Height== c("NULL")) #Missing 2 lycium species. interpolate?

#check diameters
diametercheck<- AGB_wide%>%
  filter(maxD== c("NULL"))   
#Gotta fix f. splendens. we can use the base D measure

#check diameters
diametercheck90<- AGB_wide%>% 
  filter(max90D == c("NULL"))
#Missing values, but it's because the plant was circular, so just copy the maxD measurement
#also gotta fix f. splendens, can use the base90D measure

#we'll need to add back in species names and number of individuals per plot
#i think we can do this with a bind

#first filter the input dataset for the columns we want

AGBinputextra<-AGB_inputsub %>%
  select(id, total_minishrub, scientific_name_clean)

#now bind them to the wider dataset based on id

AGB_wide<- AGBinputextra%>% 
  left_join(AGB_wide, by= "id")%>%
  distinct

AGB_wide<- AGB_wide %>%
  mutate (maxD= 
            ifelse (((scientific_name_clean=="Fouquieria splendens")), maxDbase, maxD))
  
AGB_wide<- AGB_wide %>%
  mutate (max90D= 
            ifelse (((scientific_name_clean=="Fouquieria splendens")), max90Dbase, max90D))

#interpolate 2 lycium heights based on average
#first isolate the Lycium
Lyciumsp<- AGB_wide %>%
  filter (grepl("Lycium sp. (2)", id, fixed= TRUE))

#now find a mean value and exclude null values
Lyciumsp2<-Lyciumsp %>%
  filter(Height != c("NULL"))
mean(as.numeric(Lyciumsp2$Height))
median(as.numeric(Lyciumsp2$Height))
range(as.numeric(Lyciumsp2$Height))
#mean value is 168.6 cm

#now lets add it to the dataset
#we can add it straight to the dataset because lycium is the only species with height nulls so they both get the same value
AGB_wide<- AGB_wide %>% 
  mutate (Height= 
            ifelse (((Height=="NULL")), 168.6, Height))

#now for all species without a diameter90, repeat the diameter measurement
#this works because they were circular, and the measure should be the same
AGB_wide<- AGB_wide %>% 
  mutate (max90D= 
            ifelse (((max90D=="NULL")), maxD, max90D))

#lastly we need to fix the mamillaria and barrel cactus that had a "maxDbase" measurement
AGB_wide<- AGB_wide %>% 
  mutate (maxD= 
            ifelse (((maxD=="NULL")), maxDbase, maxD))

AGB_wide<- AGB_wide %>% 
  mutate (max90D= 
            ifelse (((max90D=="NULL")), maxD, max90D))

#lets subset the data so we can just keep the columns we want
#maybe this will let us make it into a dataframe

AGB_wide<-AGB_wide %>%
  select(id, Height, maxD, max90D, total_minishrub, scientific_name_clean)

#now lets make it a data frame

AGB_widedf<-as.data.frame(AGB_wide)
glimpse(AGB_widedf)

#make the lists numeric
AGB_widedf<- AGB_widedf %>%
  mutate (HeightN=as.numeric(unlist(AGB_widedf$Height)))

AGB_widedf<- AGB_widedf %>%
  mutate (maxDN=as.numeric(unlist(AGB_widedf$maxD)))

AGB_widedf<- AGB_widedf %>%
  mutate (max90DN=as.numeric(unlist(AGB_widedf$max90D)))


#clean it up so we only have the columns we want
AGB_widedf<-AGB_widedf %>%
  select(id, HeightN, maxDN, max90DN, total_minishrub, scientific_name_clean)

#now calculate volume!

AGB_widedf<- AGB_widedf %>%
  mutate (volume= pi*(.5*maxDN)*(.5*max90DN)*HeightN)

#split the columns back apart
AGB_widedf<- AGB_widedf %>% 
  separate(id, c("site", "plot", "subplot", "individual_number", 
                 "datasheet_number", "Genus", "Species"))
#this warning is okay- just saying that it lost some of the species names... 
#like lycium sp (2), the last part (2) is lost

#make a new csv
write_csv(AGB_widedf, "DataPrep/AGB_woody/output/AGB_mostVOLUMES_EF.csv")
write_csv(AGB_widedf, "Analysis/AGB_woody_a/AGB_mostVOLUMES_EF.csv")


###
###

#Now lets calculate volume for saguaro#

###
###

AGB_saguaro<- AGB_input %>%
  filter(scientific_name_clean== c("Carnegiea gigantea"))

#remove the arms for now, we can calculate those separately later or something?

#make a unique wider ID
AGB_saguaro <- AGB_saguaro %>% 
  mutate(id = paste(plot, subplot, individual_number, subplot_size, datasheet_number, scientific_name_clean, sep = "_"))

#pivot wider based on new ID
AGB_wide_saguaro <- AGB_saguaro %>%
  pivot_wider(id_cols = id, names_from = measurement_variable_clean, 
              values_from = measurement)

#now calculate volume!

#calculate for main stem
AGB_wide_saguaro<- AGB_wide_saguaro %>%
  mutate (main_volume= (pi*((.5*ddh)^2))*Height)

#calculate for arms using half main stem diameter

AGB_wide_saguaro<- AGB_wide_saguaro %>%
  mutate (armvolume1= (pi*((.25*ddh)^2))*ArmLength1)%>%
  mutate (armvolume2= (pi*((.25*ddh)^2))*ArmLength2)%>%
  mutate (armvolume3= (pi*((.25*ddh)^2))*ArmLength3)%>%
  mutate (armvolume4= (pi*((.25*ddh)^2))*ArmLength4)

#calculate total volume for each cactus
AGB_wide_saguaro<- AGB_wide_saguaro %>%
  rowwise() %>% 
  mutate (indivCAGIvolume= sum(main_volume,armvolume1,armvolume2,
                               armvolume3,armvolume4, na.rm=T))


#now calculate total cagi volume per plot
AGB_wide_saguaro_sep<- AGB_wide_saguaro %>% 
  separate(id, c("site", "plot", "subplot", "individual_number", 
                 "subplot_size", "datasheet_number", "Genus", "Species"))

#sum the total for each plot

#first put the site plot back together
AGB_wide_saguaro_sep <- AGB_wide_saguaro_sep %>% 
  mutate(site_plot = paste(site, plot, sep = "_"))

#now calculate for only the large cacti that were in the "whole plot" measurements
#filter for correct individuals
AGB_saguaro_Large<-AGB_wide_saguaro_sep%>%
  filter(datasheet_number == 1)
#add the individuals together based on plot
AGB_saguaro_Large<-AGB_saguaro_Large%>%
  group_by(site_plot)%>%
  summarize(CAGI_large_volume=sum(indivCAGIvolume, na.rm= TRUE))

#now calculate for the medium sized saguaro which are on datasheets 3 and 4
AGB_saguaro_mid<-AGB_wide_saguaro_sep%>%
  filter(datasheet_number > 1)

#add the individuals together based on plot
AGB_saguaro_mid<-AGB_saguaro_mid%>%
  group_by(site_plot)%>%
  summarize(CAGIprebiomass=sum(indivCAGIvolume, na.rm= TRUE))


#Add in the subplot multiplier for the mid size

plotmultiply<- read.csv("DataPrep/AGB_Woody/Plot_Multipliers.csv")

AGB_saguaro_mid<-plotmultiply%>%
  left_join(AGB_saguaro_mid,  by = c('site_plot'='site_plot'), na.rm=TRUE)

#now create a new column based on the multiplier

AGB_saguaro_mid<-AGB_saguaro_mid%>%
  mutate(cagi_med_volume= CAGIprebiomass*midsize)

#Complete the multiplier for the rest of the mid size veg

AGB_widedf<- read.csv("DataPrep/AGB_Woody/output/AGB_mostVOLUMES_EF.csv")

#filter for only midsize veg (datasheets 3, 4, and 4B)

AGB_midsize_most<-AGB_widedf%>%
  filter(datasheet_number == "4B" | datasheet_number == "4" | datasheet_number == "3")

#create a siteplot variable. need to add the 0 in for the siteplot
AGB_midsize_most <- AGB_midsize_most %>% 
  mutate(site_plot = paste(site, plot, sep = "_0"))

AGB_midsize_most<-AGB_midsize_most%>%
  group_by(site_plot)%>%
  summarize(Midsizeprebiomass=sum(volume, na.rm= TRUE))


#add the plot multiplier
plotmultiply<- read.csv("DataPrep/AGB_Woody/Plot_Multipliers.csv")

AGB_midsize_most<-plotmultiply%>%
  left_join(AGB_midsize_most,  by = c('site_plot'='site_plot'), na.rm=TRUE)

#now create a new column based on the multiplier

AGB_midsize_most<-AGB_midsize_most%>%
  mutate(midsizemost_volume= Midsizeprebiomass*midsize)




#Complete the multiplier for the minishrubs

AGB_widedf<- read.csv("DataPrep/AGB_Woody/output/AGB_mostVOLUMES_EF.csv")

#filter for only midsize veg (datasheets 3, 4, and 4B)

AGB_minishrubs<-AGB_widedf%>%
  filter(datasheet_number == "7")

#create site plot variable
AGB_minishrubs <- AGB_minishrubs %>% 
  mutate(site_plot = paste(site, plot, sep = "_0"))

AGB_minishrubs<-AGB_minishrubs%>%
  group_by(site_plot)%>%
  summarize(miniprebiomass=mean(volume, na.rm= TRUE))

#add the plot multiplier
plotmultiply<- read.csv("DataPrep/AGB_Woody/Plot_Multipliers.csv")

AGB_minishrubs<-plotmultiply%>%
  left_join(AGB_minishrubs,  by = c('site_plot'='site_plot'), na.rm=TRUE)

#now create a new column based on the multiplier

AGB_minishrubs<-AGB_minishrubs%>%
  mutate(minishrub_volume= miniprebiomass*minishrub)


###

#add all the woody biomass parts together to get total m^3/m^2


###

AGB_woodycacti<-AGB_minishrubs%>%
  left_join(AGB_midsize_most,  by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(AGB_saguaro_mid,  by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(AGB_saguaro_Large,  by = c('site_plot'='site_plot'), na.rm=FALSE)


#add all of the volumes together except for OPEN
AGB_woodycacti<- AGB_woodycacti%>%
  rowwise() %>% 
  mutate(woodyvolume_cm3_noOPEN= sum(minishrub_volume, midsizemost_volume+ 
                              cagi_med_volume+ CAGI_large_volume, na.rm=TRUE))

#convert volume from cm3 to m3
AGB_woodycacti<- AGB_woodycacti%>%
  mutate(woodyvolume_m3_noOPEN= woodyvolume_cm3_noOPEN/1000000)

#convert volume to m3/m2
AGB_woodycacti<- AGB_woodycacti%>%
  mutate(woodyvolume_m3bym2_noopen= woodyvolume_m3_noOPEN/400)

#Use Arroyos power equation from Burquez et al. 2010
#Y= aX^b
#a= 356.983 (188.003e525.962)
#b= 1.416 (1.226e1.606)
AGB_woodycacti<- AGB_woodycacti%>%
  mutate(arroyos_biom_gperm2= 356.983*woodyvolume_m3bym2_noopen^1.416)%>%
  mutate(arroyos_biomC_gperm2=0.5*arroyos_biom_gperm2)


###CALCULATE OPEN SEPARATELY USING CURT ET AL###
#calculate the volume of OPEN in each plot
#we will need to OPEN from AGB_input and the opuntia pad measurements
AGB_OPEN<- AGB_input %>%
  filter(scientific_name_clean== c("Opuntia engelmannii"))

#make a unique wider ID
AGB_OPEN <- AGB_OPEN %>% 
  mutate(id = paste(plot, subplot, individual_number, subplot_size, datasheet_number, scientific_name_clean, sep = "_"))

#pivot wider based on new ID
AGB_wide_OPEN <- AGB_OPEN %>%
  pivot_wider(id_cols = id, names_from = measurement_variable_clean, 
              values_from = measurement)

AGB_wide_OPEN<- AGB_wide_OPEN %>%
  rowwise()%>%
  mutate(totalpad= sum(OldPad, OldPad2, na.rm=TRUE))%>%
  select("id", "totalpad")

AGB_wide_OPEN_sep<- AGB_wide_OPEN %>% 
  separate(id, c("site", "plot", "subplot", "individual_number", 
                 "subplot_size", "datasheet_number", "Genus", "Species"))%>% 
  mutate(site_plot = paste(site, plot, sep = "_"))


#now is a good time to calculate the OPEN pad volume per pad

pads<-read.csv("DataPrep/AGB_Woody/opuntia_pads.csv")

pads<- pads%>%
  rowwise()%>%
  mutate(pad_vol= length*width*neckwidth)

pads<- pads%>%
  select("site", "pad_vol")
 
#look at pads by site
ggplot(pads, aes(x=site, y=pad_vol)) + 
  geom_violin()+
  geom_jitter(shape=16, position=position_jitter(0.2))

#Supplemental Figure Pads below

ggplot(pads, aes(x=site, y=pad_vol, color= site)) + 
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_boxplot()+
  geom_jitter(shape=16, position=position_jitter(0.2), color= "dark grey")+
  theme_bw()+
  theme(axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),  
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20))+
  theme(legend.position="none")+
  xlab("Site")+ ylab("Opuntia Pad Volume (cm3)")


#There are a couple of very high values. May be better to use median
#check mean and median
pads %>% group_by(site) %>% summarize(Avg = mean(pad_vol))
# site    Avg
# 1 LOLA   509.
# 2 RR     694.
pads %>% group_by(site) %>% summarize(Med = median(pad_vol))
# site    Med
# 1 LOLA   456 
# 2 RR     532

#mean and median are quite different. Should use median
#make median into a dataframe
pad_med<-pads %>% 
  group_by(site) %>% summarize(Med = median(pad_vol))

#add the pad volume multiplier to the pad count sheet
AGB_wide_OPEN_sep<-pad_med%>%
  left_join(AGB_wide_OPEN_sep,  by = c('site'='site'), na.rm=FALSE)

#multiply it out so each plant gets a total pad volume

AGB_wide_OPEN_sep<-AGB_wide_OPEN_sep%>%
  mutate(plant_volume_cm3=Med*totalpad)

#calculate g per plant using Curt et al. 2011
#y=a*x
#a=0.0345

AGB_wide_OPEN_sep<-AGB_wide_OPEN_sep%>%
  mutate(plant_g=plant_volume_cm3*0.0345)

#summarize g per plot
OPEN_AGB<-AGB_wide_OPEN_sep%>%
  group_by(site_plot)%>%
  summarize(pads_g=sum(plant_g, na.rm= TRUE))

#add the plot multiplier
plotmultiply<- read.csv("DataPrep/AGB_Woody/Plot_Multipliers.csv")

OPEN_AGB<-plotmultiply%>%
  left_join(OPEN_AGB,  by = c('site_plot'='site_plot'), na.rm=TRUE)

#now create a new column based on the multiplier

OPEN_AGB<-OPEN_AGB%>%
  mutate(pads_total_g= midsize*pads_g)

#calculate g/m2
OPEN_AGB<-OPEN_AGB%>%
  mutate(pads_total_gperm2=pads_total_g/400)%>%
  mutate(pads_total_gperm2C=pads_total_gperm2/2)

#add OPEN volume to the rest of plot volume
AGB_totals<-AGB_woodycacti%>%
  left_join(OPEN_AGB,  by = c('site_plot'='site_plot'), na.rm=TRUE)

AGB_totals<-AGB_totals%>%
  rowwise()%>%
  mutate(TotalC_g_m2= sum(pads_total_gperm2C, arroyos_biomC_gperm2, na.rm=TRUE))
  
AGB_totals<-AGB_totals%>%
  select(site_plot, TotalC_g_m2)
#note that this does not include herb cover.
#this is the dataframe to export. 

PECI_AGB<-cover%>%
  left_join(AGB_totals,  by = c('site_plot'='site_plot'), na.rm=FALSE)

ggplot(PECI_AGB, aes(x = PECI_cover, y = TotalC_g_m2, color = site) ) +
  geom_text(aes(label=ALL_cover),hjust=1, vjust=1)+
  geom_point(size=4) 

ggplot(PECI_AGB, aes(x = PECI_cover , y = ALL_cover, color = TotalC_g_m2) ) +
  geom_point(size=4) 

#export this df as a new sheet for analysis
write_csv(PECI_AGB, "DataPrep/AGB_woody/output/AGB_woody_biomass.csv")
write_csv(PECI_AGB, "Analysis/AGB_woody_a/AGB_woody_biomass.csv")
