#buffelgrass soil carbon
#November 4, 2020
#Dr. R. Chelsea Nagy and Dr. Emily Fusco

library(tidyverse)

setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")

soilcarb <- read.csv("DataPrep/Soil/soil_carbon.csv")

soilcarb <- soilcarb %>% 
  rename(perctotC = C) %>%
  rename(perctotN = N) %>%
  rename(percSOC = TOC) %>%
  select(-Lab)

soilcarb2 <- separate(data = soilcarb, col = Sample, into = c("sample_number", "plot"), sep = "\\ ")

soilcarb2$sample_number <- as.numeric(soilcarb2$sample_number)

soilcarb2$plot <- gsub('rr', 'RR', soilcarb2$plot)
soilcarb2$plot <- gsub('lola', 'Lola', soilcarb2$plot)


#bring in table that relates sample number, cover, and rep
coverrep <- read.csv("DataPrep/Soil/Nagy_soil_carbon_nitrogen.csv")

coverrep <- coverrep %>%
  rename(plot = plot_name)

coverrep$plot <- gsub('rr', 'RR', coverrep$plot)
coverrep$plot <- gsub('lola', 'Lola', coverrep$plot)

#join dataframes together
soilcarb3 <- full_join(coverrep, soilcarb2, by = c("sample_number", "plot"))

#bring in table that relates plot, cover, and BD
plotcoverBD <- read.csv("DataPrep/Soil/output/plotcoverBD.csv")

#join dataframes together
soilcarb4 <- left_join(soilcarb3, plotcoverBD, by = c("plot", "cover_type"))

#missing BD data from Lola_01 bare
#mean BD from bare = 1.3912
soilcarb4 <- soilcarb4 %>% 
  mutate(BD_g_cm3 = replace_na(BD_g_cm3, 1.3912))
  

#add option for using mean values
plotcoverBDmeans<- read.csv("DataPrep/Soil/output/plotcoverBDsitemeans.csv")

plotcoverBDmeans<- plotcoverBDmeans%>%
  mutate(site_cover = paste(site, cover_type, sep = "_"))%>%
  rename(BD_g_cm3_means = BD_g_cm3)%>%
  select(-site, -cover_type)

soilcarb4<- soilcarb4 %>%
  rename(site_plot = plot)%>%
  separate(site_plot, c("site", "plot"), remove= FALSE)%>%
  mutate(site_cover = paste(site, cover_type, sep = "_"))

#join dataframes together
soilcarb4 <- left_join(soilcarb4, plotcoverBDmeans, by = c("site_cover"))




#soil carbon content calculations#######################################

###SOC###
#%C (gC / g soil)* BD (g soil / cm3) * depth (cm) * conversion factor (10000 cm2 / 1 m2)
soilcarb4 <- soilcarb4 %>%
  mutate(SOCcontent_gC_m2 = (percSOC/100)*BD_g_cm3*10*10000)%>%
  mutate(SOCcontent_gC_m2_means = (percSOC/100)*BD_g_cm3_means*10*10000)

SOC_plotBD <- soilcarb4 %>%
  group_by(cover_type, site) %>%
  summarise(meanSOC = mean(SOCcontent_gC_m2, na.rm = TRUE))

SOC_meanBD<- soilcarb4 %>%
  group_by(cover_type, site) %>%
  summarise(meanSOC = mean(SOCcontent_gC_m2_means, na.rm = TRUE))

#whoa, this is interesting!!

#make a barplot

#plot with CIs

SOC_plotBD2 <- SOC_plotBD %>%
  group_by(cover_type) %>%
  summarise( 
    n=n(),
    mean=mean(meanSOC),
    sd=sd(meanSOC)
  ) %>%
  mutate( se=sd/sqrt(n))  %>%
  mutate( ci=se * qt((1-0.05)/2 + .5, n-1))

ggplot(data=SOC_plotBD, aes(x=site, y=meanSOC, fill=cover_type)) +
  geom_bar(stat="identity",  position=position_dodge())+ 
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))

ggplot(data=SOC_meanBD, aes(x=site, y=meanSOC, fill=cover_type)) +
  geom_bar(stat="identity",  position=position_dodge())+ 
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))

#add in peci cover
# 
# means4<- means4 %>% 
#   separate(plot, c("site", "plot"))
# 
# means4<- means4 %>%
#   mutate (site= 
#             ifelse (((site=="Lola")), "LOLA", site))
# 
# means4 <- means4 %>% 
#   mutate(site_plot = paste(site, plot, sep = "_"))
# 
# cover<-read.csv("cover.csv")
# 
# means4<-cover%>%
#   left_join(means4,  by = c('site_plot'='site_plot'), na.rm=FALSE)

#first prelim plot!
# 
# ggplot(means4, aes(x = PECI_cover, y = meanSOC, color = site.x, shape=cover_type) ) +
#   geom_point(size=4) 



###Total soil carbon###
soilcarb4 <- soilcarb4 %>%
  mutate(totsoilCcontent_gC_m2 = (perctotC/100)*BD_g_cm3*10*10000)%>%
  mutate(totsoilCcontent_gC_m2_means = (perctotC/100)*BD_g_cm3_means*10*10000)

TSC_plotBD <- soilcarb4 %>%
  group_by(cover_type, site) %>%
  summarise(meanTSC = mean(totsoilCcontent_gC_m2, na.rm = TRUE))

TSC_meanBD<-soilcarb4 %>%
  group_by(cover_type, site) %>%
  summarise(meanTSC = mean(totsoilCcontent_gC_m2_means, na.rm = TRUE))

#whoa, this is interesting!!

ggplot(data=TSC_plotBD, aes(x=site, y=meanTSC, fill=cover_type)) +
  geom_bar(stat="identity",  position=position_dodge())+ 
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))

ggplot(data=TSC_meanBD, aes(x=site, y=meanTSC, fill=cover_type)) +
  geom_bar(stat="identity",  position=position_dodge())+ 
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))


# means5<- means5 %>% 
#   separate(plot, c("site", "plot"))
# 
# means5<- means5 %>%
#   mutate (site= 
#             ifelse (((site=="Lola")), "LOLA", site))
# 
# means5 <- means5 %>% 
#   mutate(site_plot = paste(site, plot, sep = "_"))
# 
# means5<-cover%>%
#   left_join(means5,  by = c('site_plot'='site_plot'), na.rm=FALSE)
# 
# #prelim plot!
# 
# ggplot(means5, aes(x = PECI_cover, y = meanTSC, color = site.x, shape=cover_type) ) +
#   geom_point(size=4) 



###Total soil nitrogen###
soilcarb4 <- soilcarb4 %>%
  mutate(totsoilNcontent_gC_m2 = (perctotN/100)*BD_g_cm3*10*10000)%>%
  mutate(totsoilNcontent_gC_m2_means = (perctotN/100)*BD_g_cm3_means*10*10000)

N_plotBD<- soilcarb4 %>%
  group_by(cover_type, site) %>%
  summarise(meanTSN = mean(totsoilNcontent_gC_m2, na.rm = TRUE))

N_meanBD<- soilcarb4 %>%
  group_by(cover_type, site) %>%
  summarise(meanTSN = mean(totsoilNcontent_gC_m2_means, na.rm = TRUE))

#same pattern as for total soil C and SOC

ggplot(data=N_plotBD, aes(x=site, y=meanTSN, fill=cover_type)) +
  geom_bar(stat="identity",  position=position_dodge())+ 
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))

ggplot(data=N_meanBD, aes(x=site, y=meanTSN, fill=cover_type)) +
  geom_bar(stat="identity",  position=position_dodge())+ 
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))



# means6<- means6 %>% 
#   separate(plot, c("site", "plot"))
# 
# means6<- means6 %>%
#   mutate (site= 
#             ifelse (((site=="Lola")), "LOLA", site))
# 
# means6 <- means6 %>% 
#   mutate(site_plot = paste(site, plot, sep = "_"))
# 
# means6<-cover%>%
#   left_join(means6,  by = c('site_plot'='site_plot'), na.rm=FALSE)
# 
# #prelim plot!
# 
# ggplot(means6, aes(x = PECI_cover, y = meanTSN, color = site.x, shape=cover_type) ) +
#   geom_point(size=4) 

#Create a NEW csv that has the full soils dataset based on finished soilcarb4
SoilData<-soilcarb4%>%
  select(site_plot, site, plot, cover_type, rep, SOCcontent_gC_m2_means,totsoilCcontent_gC_m2_means, totsoilNcontent_gC_m2_means)

write_csv(SoilData, "DataPrep/Soil/output/SoilData.csv")
write_csv(SoilData, "Analysis/Soil_a/SoilData.csv")
#Now calculate per plot g/m2 based on cover

cover<-read.csv("DataPrep/Soil/cover.csv")

cover<- cover%>%
  rename(other_cover= ALL_cover)%>%
  rename(buffel_cover=PECI_cover)

cover<- cover%>%
  mutate(bare_cover=100-other_cover-buffel_cover)

###CALCULATE FOR SOIL N###

N_plot_totals<- soilcarb4 %>%
  group_by(cover_type, site_plot) %>%
  summarise(meanTSN = mean(totsoilNcontent_gC_m2_means, na.rm = TRUE))

#change Lola to LOLA
N_plot_totals<- N_plot_totals %>% 
  separate(site_plot, c("site", "plot"))

N_plot_totals<- N_plot_totals %>%
  mutate (site= 
            ifelse (((site=="Lola")), "LOLA", site))

N_plot_totals <- N_plot_totals %>% 
  mutate(site_plot = paste(site, plot, sep = "_"))

N_plot_totals <- N_plot_totals %>% 
  select(-site, -plot)

#now add in cover 
N_plot_totals<- N_plot_totals%>%
  left_join(cover, by = c('site_plot'='site_plot'), na.rm=FALSE)


#calculate total g/m2 for each cover type

N_plot_totals<- N_plot_totals %>%
  mutate (N_bare= 
            ifelse (((cover_type=="bare")), (bare_cover/100)*meanTSN, 0))
N_plot_totals<- N_plot_totals %>%
  mutate (N_buffel= 
            ifelse (((cover_type=="buffel")), (buffel_cover/100)*meanTSN, 0))
N_plot_totals<- N_plot_totals %>%
  mutate (N_other= 
            ifelse (((cover_type=="other")), (other_cover/100)*meanTSN, 0))

#collapse by site_plot

N_bare<- N_plot_totals %>%
  group_by(site_plot) %>%
  summarise(N_bare = max(N_bare, na.rm = TRUE))

N_buffel<- N_plot_totals %>%
  group_by(site_plot) %>%
  summarise(N_buffel = max(N_buffel, na.rm = TRUE))

N_other<- N_plot_totals %>%
  group_by(site_plot) %>%
  summarise(N_other = max(N_other, na.rm = TRUE))

#add them back together

N_plot_totals_added<- N_bare%>%
  left_join(N_buffel,by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(N_other, by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(cover, by = c('site_plot'='site_plot'), na.rm=FALSE)
  
N_plot_totals_added<-N_plot_totals_added%>%
rowwise()%>%
  mutate(TSN_g_m2= sum(N_bare, N_buffel, N_other, na.rm=TRUE))

ggplot(N_plot_totals_added, aes(x = buffel_cover, y = TSN_g_m2, color=site) ) +
  geom_point(size=4) 

ggplot(N_plot_totals_added, aes(x = other_cover, y = TSN_g_m2, color=site) ) +
  geom_point(size=4)


###CALCULATE FOR SOC###

SOC_plot_totals<- soilcarb4 %>%
  group_by(cover_type, site_plot) %>%
  summarise(meanSOC = mean(SOCcontent_gC_m2_means, na.rm = TRUE))

#change Lola to LOLA
SOC_plot_totals<- SOC_plot_totals %>% 
  separate(site_plot, c("site", "plot"))

SOC_plot_totals<- SOC_plot_totals %>%
  mutate (site= 
            ifelse (((site=="Lola")), "LOLA", site))

SOC_plot_totals <- SOC_plot_totals %>% 
  mutate(site_plot = paste(site, plot, sep = "_"))

SOC_plot_totals <- SOC_plot_totals %>% 
  select(-site, -plot)

#now add in cover 
SOC_plot_totals<- SOC_plot_totals%>%
  left_join(cover, by = c('site_plot'='site_plot'), na.rm=FALSE)


#calculate total g/m2 for each cover type

SOC_plot_totals<- SOC_plot_totals %>%
  mutate (SOC_bare= 
            ifelse (((cover_type=="bare")), (bare_cover/100)*meanSOC, 0))
SOC_plot_totals<- SOC_plot_totals %>%
  mutate (SOC_buffel= 
            ifelse (((cover_type=="buffel")), (buffel_cover/100)*meanSOC, 0))
SOC_plot_totals<- SOC_plot_totals %>%
  mutate (SOC_other= 
            ifelse (((cover_type=="other")), (other_cover/100)*meanSOC, 0))

#collapse by site_plot

SOC_bare<- SOC_plot_totals %>%
  group_by(site_plot) %>%
  summarise(SOC_bare = max(SOC_bare, na.rm = TRUE))

SOC_buffel<- SOC_plot_totals %>%
  group_by(site_plot) %>%
  summarise(SOC_buffel = max(SOC_buffel, na.rm = TRUE))

SOC_other<- SOC_plot_totals %>%
  group_by(site_plot) %>%
  summarise(SOC_other = max(SOC_other, na.rm = TRUE))

#add them back together

SOC_plot_totals_added<- SOC_bare%>%
  left_join(SOC_buffel,by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(SOC_other, by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(cover, by = c('site_plot'='site_plot'), na.rm=FALSE)

SOC_plot_totals_added<-SOC_plot_totals_added%>%
  rowwise()%>%
  mutate(SOC_g_m2= sum(SOC_bare, SOC_buffel, SOC_other, na.rm=TRUE))


ggplot(SOC_plot_totals_added, aes(x = buffel_cover, y = SOC_g_m2, color=site) ) +
  geom_point(size=4)

ggplot(SOC_plot_totals_added, aes(x = other_cover, y = SOC_g_m2, color=site) ) +
  geom_point(size=4) 


###CALCULATE FOR TSC###

TSC_plot_totals<- soilcarb4 %>%
  group_by(cover_type, site_plot) %>%
  summarise(meanTSC = mean(totsoilCcontent_gC_m2_means, na.rm = TRUE))

#change Lola to LOLA
TSC_plot_totals<- TSC_plot_totals %>% 
  separate(site_plot, c("site", "plot"))

TSC_plot_totals<- TSC_plot_totals %>%
  mutate (site= 
            ifelse (((site=="Lola")), "LOLA", site))

TSC_plot_totals <- TSC_plot_totals %>% 
  mutate(site_plot = paste(site, plot, sep = "_"))

TSC_plot_totals <- TSC_plot_totals %>% 
  select(-site, -plot)

#now add in cover 
TSC_plot_totals<- TSC_plot_totals%>%
  left_join(cover, by = c('site_plot'='site_plot'), na.rm=FALSE)


#calculate total g/m2 for each cover type

TSC_plot_totals<- TSC_plot_totals %>%
  mutate (TSC_bare= 
            ifelse (((cover_type=="bare")), (bare_cover/100)*meanTSC, 0))
TSC_plot_totals<- TSC_plot_totals %>%
  mutate (TSC_buffel= 
            ifelse (((cover_type=="buffel")), (buffel_cover/100)*meanTSC, 0))
TSC_plot_totals<- TSC_plot_totals %>%
  mutate (TSC_other= 
            ifelse (((cover_type=="other")), (other_cover/100)*meanTSC, 0))

#collapse by site_plot

TSC_bare<- TSC_plot_totals %>%
  group_by(site_plot) %>%
  summarise(TSC_bare = max(TSC_bare, na.rm = TRUE))

TSC_buffel<- TSC_plot_totals %>%
  group_by(site_plot) %>%
  summarise(TSC_buffel = max(TSC_buffel, na.rm = TRUE))

TSC_other<- TSC_plot_totals %>%
  group_by(site_plot) %>%
  summarise(TSC_other = max(TSC_other, na.rm = TRUE))

#add them back together

TSC_plot_totals_added<- TSC_bare%>%
  left_join(TSC_buffel,by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(TSC_other, by = c('site_plot'='site_plot'), na.rm=FALSE)%>%
  left_join(cover, by = c('site_plot'='site_plot'), na.rm=FALSE)

TSC_plot_totals_added<-TSC_plot_totals_added%>%
  rowwise()%>%
  mutate(TSC_g_m2= sum(TSC_bare, TSC_buffel, TSC_other, na.rm=TRUE))

ggplot(TSC_plot_totals_added, aes(x = buffel_cover, y = TSC_g_m2, color=site) ) +
  geom_point(size=4) 

ggplot(TSC_plot_totals_added, aes(x = other_cover, y = TSC_g_m2, color=site) ) +
  geom_point(size=4) 



#####
#ANOVAs to check for differences
#####

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "FSA", "lme4", "rstatix","car", "ggpubr")
lapply(x, library, character.only = TRUE, verbose = FALSE)


SOC_meanBD_plots<- soilcarb4 %>%
  select(cover_type,site_plot,SOCcontent_gC_m2_means,site)%>%
  group_by(cover_type, site_plot) %>%
  summarise(meanSOC = mean(SOCcontent_gC_m2_means, na.rm = TRUE))

SOC_meanBD_plots<- SOC_meanBD_plots%>%
  separate(site_plot, c("site", "plot"), remove= FALSE)

##
#data viz##
##

ggplot(SOC_meanBD_plots, aes(x=site, y=meanSOC, fill=cover_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))

ggplot(SOC_meanBD_plots, aes(x=cover_type, y=meanSOC)) +
  geom_boxplot()

SOC_meanBD_plots %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC)
#LOLA_04 other and RR_07 other are outliers
#will need to run analysis with and without outliers

#check to see if it is balanced

table(SOC_meanBD_plots$site, SOC_meanBD_plots$cover_type)
#        bare  buffel other
# Lola    6      5     6
# RR      7      5     7
#it's not balanced but it is close... not sure if thats okay


#Create anova
#try block design where we account for site
model.anova=aov(meanSOC~as.factor(cover_type)*site, data = SOC_meanBD_plots)
summary(model.anova)

#                             Df  Sum Sq    Mean Sq   F value   Pr(>F)  
# as.factor(cover_type)       2   1654631   827316    4.839     0.0151 *
# site                        1   739994    739994    4.329     0.0461 *
# as.factor(cover_type):site  2   151156    75578     0.442     0.6468  
# Residuals                  30   5128558   170952         

#create linear model to test normality assumption
test.lm.block = lm(meanSOC ~ cover_type*site, data = SOC_meanBD_plots)
plot(test.lm.block)
#QQ a little funky
shapiro.test(residuals(test.lm.block))
#p=0.0001277, so not normal

#check normality by groups
SOC_meanBD_plots %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC)
#LOLA and RR other sites are not normal

#try a log transformation
SOC_meanBD_plots<- SOC_meanBD_plots%>%
  mutate(meanSOC_log= log(meanSOC))

#check using log transformed data
#check normality by groups
SOC_meanBD_plots %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC_log)
#LOLA other is still bad

#check for homogeneity of variance
leveneTest(meanSOC ~ cover_type*site, data = SOC_meanBD_plots)
#p=0.8415 so no violation

#create a new anova with transformed variables
block.fit.tran=aov(meanSOC_log~as.factor(cover_type)*site, data = SOC_meanBD_plots)
summary(block.fit.tran)
plot(block.fit.tran)
#I think these look okay

#check for homogeneity of variance again with log transformed data
leveneTest(meanSOC_log ~ cover_type*site, data = SOC_meanBD_plots)
#p=0.3785 so no violation

#check for outliers again using log transformed data
SOC_meanBD_plots %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_log)
#LOLA_04 other and RR_07 other are still outliers

#try a square root transformation
SOC_meanBD_plots<- SOC_meanBD_plots%>%
  mutate(meanSOC_sqrt= sqrt(meanSOC))

SOC_meanBD_plots %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_sqrt)
#yes, still outliers

#check using sqrt transformed data
#check normality by groups
SOC_meanBD_plots %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC_sqrt)
#still issues with "other" rr and lola plots

#check for homogeneity of variance again with sqrt transformed data
leveneTest(meanSOC_sqrt ~ cover_type*site, data = SOC_meanBD_plots)
#p=0.7333 so no violation

#try running anova with log data with no outliers
SOC_meanBD_plots_no_outliers<-SOC_meanBD_plots%>%
  mutate(site_plot_cover= paste(site_plot, cover_type, sep="_"))

SOC_meanBD_plots_no_outliers<-SOC_meanBD_plots_no_outliers%>%
  filter(site_plot_cover!="Lola_04_other")%>%
  filter(site_plot_cover!="RR_07_other")

#check normality by groups
SOC_meanBD_plots_no_outliers %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC_log)
#LOLA and RR other sites are not normal
#everything works now

#check for outliers again using log transformed data
SOC_meanBD_plots_no_outliers %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_log)
#no outliers now which makes sense

#check for homogeneity of variance again with sqrt transformed data
leveneTest(meanSOC_log ~ cover_type*site, data = SOC_meanBD_plots_no_outliers)
#p=0.08414 so no violation

#create a new anova with transformed variables and no outliers
block.fit.tran.no.outliers=aov(meanSOC_log~as.factor(cover_type)+site, data = SOC_meanBD_plots_no_outliers)
summary(block.fit.tran.no.outliers)
plot(block.fit.tran.no.outliers)

#                        Df   Sum Sq  Mean Sq   F value  Pr(>F)   
# as.factor(cover_type)  2    0.5795  0.2898    3.125    0.05852 . 
# site                   1    1.2116  1.2116    13.066   0.00109 **
# Residuals             30    2.7818  0.0927

#ack I dont know. 

#try testing for log and sqrt transformed outliers
SOC_meanBD_plots<- SOC_meanBD_plots%>%
  mutate(meanSOC_log_sqrt= sqrt(meanSOC_log))

SOC_meanBD_plots %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_log_sqrt)
#doesn't help



#run 

#check assumptions
plot(fit)
plot(block.fit)

#test for normality
test.lm = lm(meanSOC ~ cover_type, data = SOC_meanBD_plots)
shapiro.test(residuals(test.lm))
#p-value = 0.01547; not normal



#make it a random effect model
mixed.lmer <- lmer(meanSOC ~ cover_type + (1|site), data = SOC_meanBD_plots)
summary(mixed.lmer)
plot(mixed.lmer)
#but you cant have a random effect with only 2 groups?



#try to transform
test.lm1a <- lm(log(meanSOC) ~ cover_type, data = SOC_meanBD_plots)
shapiro.test(residuals(test.lm1a))
#p-value = 0.5267; it's normal now

test.lm1a.block <- lm(log(meanSOC) ~ cover_type+site, data = SOC_meanBD_plots)
shapiro.test(residuals(test.lm1a.block))
#p-value = 0.3689; it's normal now
plot(test.lm1a.block)
#residuals are probably fine but should check with someone
summary(test.lm1a.block)

#                   Estimate Std.   Error   t value  Pr(>|t|)    
# (Intercept)       7.00854        0.11529   60.790  < 2e-16 ***
# cover_typebuffel -0.05308        0.14677  -0.362   0.71996    
# cover_typeother   0.37713        0.13680   2.757   0.00956 ** 
# siteRR           -0.35183        0.11650  -3.020   0.00494 ** 




#test for additivity
with(SOC_meanBD_plots,
     interaction.plot(cover_type,site,meanSOC, col=1:4))
#meets assumption, lines are parallel using log or not

#Finally build the anova https://www.datanovia.com/en/lessons/anova-in-r/
#http://www.sthda.com/english/wiki/two-way-anova-test-in-r
?anova_test
anova_test(meanSOC_log ~ as.factor(cover_type) * site, data= SOC_meanBD_plots)
model.3<-anova(model.2)
summary(model.2)
summary(model.3)
glimpse(SOC_meanBD_plots)


#info from eve from her biotic invasions paper
# require(lsmeans)
# pairwise <- lsmeans(model, pairwise ~ ln_N_richness_Z*(Ecoregion+Community), adjust="tukey", type="response")

#check this site too 
#https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/


#package was changed to emmeans
require(emmeans)
emmeans(test.lm.block, list(pairwise ~ Group), adjust = "tukey")
#look here too

emmSOC = emmeans(test.lm1a.block, specs = pairwise ~ cover_type, type="response")
emmSOC

# cover_type    response    SE    df    lower.CL  upper.CL
# bare            928       89.8  32    762       1130
# buffel          880       97.0  32    703       1101
# other          1353       131.0 32    1110      1647

# contrast       estimate    SE df t.ratio p.value
# bare - buffel    0.0531 0.147 32  0.362  0.9306 
# bare - other    -0.3771 0.137 32 -2.757  0.0252 
# buffel - other  -0.4302 0.147 32 -2.931  0.0166

emmSOCanova.tran = emmeans(block.fit.tran, specs = pairwise ~ cover_type, type="response")
emmSOCanova.tran
#significant difference in other plots

emmSOCanova.nooutliers = emmeans(block.fit.tran.no.outliers, specs = pairwise ~ cover_type, type="response")
emmSOCanova.nooutliers
#not significant

SOCother<-SOC_meanBD_plots%>%
  filter(cover_type=="other")

hist(SOCother$meanSOC_log, breaks=10)
hist(SOCother$meanSOC, breaks=10)
###############################
#Modeling by buffelgrass cover
###############################
###NONE OF THIS WORKS YET 12/9/2020###


m0 = lm(SOC_g_m2 ~buffel_cover*site, data=SOC_plot_totals_added)
summary(m0)
plot(m0)
drop1(m0) # Its better to drop it

# Try quadratic
m1 = lm(SOC_g_m2 ~ poly(buffel_cover, 2)*site, data=SOC_plot_totals_added)
summary(m1)
plot(m1)
drop1(m1) # Its better to drop it

m2 = lm(SOC_g_m2 ~buffel_cover, data=SOC_plot_totals_added)
summary(m2)
plot(m2)
drop1(m2) #Its better to keep it

AICctab(m0, m1, m2, weights=T, base=T)
anova(m0,m2)
# The linear model (m2) with no interaction is the best

# Calculate fitteds & resids for the model with no interaction
F2 = fitted(m2)
E2 = resid(m2)


SOC_plot_totals_added2 = SOC_plot_totals_added %>% mutate(F2 = F2, E2 = E2)

ggplot(PECI, aes(x=PECI_cover)) +
  geom_point(aes(y=richness), size=5) +
  geom_point(aes(y=F2), size=5, shape=2) +
  geom_line(aes(y=F2)) +
  ggtitle("Richness: 
          Observed (circles) & Expected (m2, triangles)")

ggplot(PECI, aes(x=F2, y=E2)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")


# Calculate fitteds & resids for the model with an interaction
F0 = fitted(m0)
E0 = resid(m0)

SOC_plot_totals_addedm0 = SOC_plot_totals_added %>% mutate(F0 = F0, E0 = E0)

ggplot(PECI, aes(x=PECI_cover, color=site)) +
  geom_point(aes(y=richness), size=5) +
  geom_point(aes(y=F0), size=5, shape=2) +
  geom_line(aes(y=F0)) +
  ggtitle("Richness: 
          Observed (circles) & Expected (m0, triangles)")


#Friedman Test for blocked design
# SOC_meanBD_plots$cover_type<-factor(SOC_meanBD_plots$cover_type)
# SOC_meanBD_plots$site<-factor(SOC_meanBD_plots$site)
# 
# SOC_meanBD_plots_ma<-as.matrix(SOC_meanBD_plots)
# SOC_meanBD_plots<-SOC_meanBD_plots%>%
#   select(site, cover_type, meanSOC)
# friedman.test(meanSOC ~ cover_type | site,
#               data = SOC_meanBD_plots, na.action=)
# 
# ?friedman.test

#conduct a two way anova without interaction?