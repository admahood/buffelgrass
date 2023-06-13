setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")


#load multiple libraries 
# ALM: Added performance
x <- c("tidyverse", "sf", "see", "ggplot2", "doBy", "FSA", "lme4", "rstatix","car", 
       "ggpubr","performance", "lmerTest", "qqplotr", "sjPlot", "sjmisc", "bbmle")
lapply(x, library, character.only = TRUE, verbose = FALSE)


soils <- read_csv("Analysis/Soil_a/SoilData.csv")

soils_SOC<- soils %>%
  select(cover_type,site_plot,SOCcontent_gC_m2_means,site)%>%
  group_by(cover_type, site_plot) %>%
  summarise(meanSOC = mean(SOCcontent_gC_m2_means, na.rm = TRUE))

soils_SOC<- soils_SOC%>%
  separate(site_plot, c("site", "plot"), remove= FALSE)

means <- soils_SOC %>%
  group_by(cover_type) %>%
  summarise(meanSOC = mean(meanSOC, na.rm = TRUE)) 

##
#data viz##
##

ggplot(soils_SOC, aes(x=site, y=meanSOC, fill=cover_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))

#check for outliers
soils_SOC %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC)
#LOLA_04 other and RR_07 other are outliers
#they are correct though

table(soils_SOC$site, soils_SOC$cover_type)
#        bare  buffel other
# Lola    6      5     6
# RR      7      5     7

#Create anova
#try block design where we account for site
model.anova=aov(meanSOC~as.factor(cover_type)*site, data = soils_SOC)
summary(model.anova)

#                             Df  Sum Sq    Mean Sq   F value   Pr(>F)  
# as.factor(cover_type)       2   1654631   827316    4.839     0.0151 *
# site                        1   739994    739994    4.329     0.0461 *
# as.factor(cover_type):site  2   151156    75578     0.442     0.6468  
# Residuals                  30   5128558   170952   

#check to see if the design is balanced
Anova(model.anova, type = "III")
#the output is different than above, so design is not balanced

#shapiro suggested non-normal
#try a log transformation
soils_SOC<- soils_SOC%>%
  mutate(meanSOC_log= log(meanSOC))

#create a new anova with transformed variables
model.anova.log=aov(meanSOC_log~as.factor(cover_type)*site, data = soils_SOC)
summary(model.anova.log)
plot(model.anova.log)

#check for homogeneity of variance again with log transformed data
leveneTest(meanSOC_log ~ cover_type*site, data = soils_SOC)
#p=0.3785 so no violation

#check for outliers again using log transformed data
soils_SOC %>%
  select(site, cover_type, meanSOC_log)%>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_log)
#LOLA_04 other and RR_07 other are still extreme outliers

#check for normality
soils_SOC %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC_log)
#Lola other still doesn't pass


#was thinking of something like this for a post hoc comparison
require(emmeans)

model.anova.log=aov(meanSOC_log~as.factor(cover_type)*site, data = soils_SOC)
summary(model.anova)
Anova(model.anova.log, type = "III")

emmSOC = emmeans(model.anova.log, specs = pairwise ~ cover_type, type="response")
emmSOC


# adam section ============
ggplot(soils, aes(x=SOCcontent_gC_m2_means)) +
  geom_histogram() +
  facet_grid(cover_type~site)

# looks like logging leads to good model residuals
mod<-lmer(log(meanSOC + 1) ~ cover_type + (1|site), data=soils_SOC)
performance::check_model(mod)
plot(mod)
summary(mod)
Anova(mod, type = "III")

# not a big difference to add in plot but theoretically better since it includes 
# the sampling structure but the first mod has better residuals so maybe stick with that
lmer(log(SOCcontent_gC_m2_means + 1) ~ cover_type + (1|site/site_plot), data=soils)->mod1
performance::check_model(mod1)
summary(mod1)
plot(mod1)
Anova(mod1, type="III")

emmeans::emmeans(mod, pairwise~cover_type, type="response")
emmeans::emmeans(mod1, pairwise~cover_type, type="response", adjust = "Tukey")

mod2<-lm(log(meanSOC + 1) ~ cover_type*site, data=soils_SOC)
performance::check_model(mod2)
summary(mod2)
Anova(mod2, type = "III")

#look at the interaction before determining if we can check main effects
plot_model(mod2, type = "int", colors = c("#F8766D", "#00BFC4"))
#lines are pretty parallel, so main effects should be reasonable
#also, the interaction is not significant in the model

#test estimated marginal means
emmeans::emmeans(mod2, pairwise~cover_type, adjust = "sidak", type="response")
#estimating main effects is okay because interaction is not significant

#plot it within emmeans
# plot(mod2CI, comparisons=TRUE)+
#   scale_color_manual(values=cols)

#try modeling without interaction to see if it makes it much better
mod3<-lm(log(meanSOC + 1) ~ cover_type+site, data=soils_SOC)
performance::check_model(mod3)
summary(mod3)
plot(check_normality(mod3), type = "qq")
Anova(mod3, type = "III")
emmeans::emmeans(mod3, pairwise~cover_type, adjust = "sidak", type="response")

require(bbmle)
AICtab(mod2, mod3)

#mod2 and 3 are about the same and are the best
#use mod2



##SOIL TSC##

soils_TSC<- soils %>%
  select(cover_type,site_plot,totsoilCcontent_gC_m2_means,site)%>%
  group_by(cover_type, site_plot) %>%
  summarise(meanTSC = mean(totsoilCcontent_gC_m2_means, na.rm = TRUE))

soils_TSC<- soils_TSC%>%
  separate(site_plot, c("site", "plot"), remove= FALSE)

mod1_TSC<-lm(log(meanTSC + 1) ~ cover_type*site, data=soils_TSC)
performance::check_model(mod1_TSC)
plot(check_normality(mod1_TSC), type = "qq")
summary(mod1_TSC)
plot(mod1_TSC)
Anova(mod1_TSC, type="III")

#look at the interaction before determining if we can check main effects
plot_model(mod1_TSC, type = "int", colors = c("#F8766D", "#00BFC4"))
#lines are pretty parallel, so main effects should be reasonable

emmeans::emmeans(mod1_TSC, pairwise~cover_type, adjust = "sidak", type="response")





#make it a dataframe

#SOC

SOC_mod2<-emmeans::emmeans(mod2, pairwise~cover_type, adjust = "sidak", type="response")
emmSOCdf = as.data.frame(SOC_mod2)

meansSOC <- soils_SOC %>%
  group_by(cover_type) %>%
  summarise(meanSOC = mean(meanSOC, na.rm = TRUE))

emmSOCdf_CI<-emmSOCdf%>%
  filter(cover_type!=".")

emmSOCdf_CI<-emmSOCdf_CI%>%
  left_join(meansSOC, by="cover_type")

emmSOCdf_CI<-emmSOCdf_CI%>%
  mutate(sig=c("a","a","b"))

#TSC

TSC_mod1<-emmeans::emmeans(mod1_TSC, pairwise~cover_type, adjust = "sidak", type="response")
emmTSCdf = as.data.frame(TSC_mod1)

meansTSC <- soils_TSC %>%
  group_by(cover_type) %>%
  summarise(meanTSC = mean(meanTSC, na.rm = TRUE))

emmTSCdf_CI<-emmTSCdf%>%
  filter(cover_type!=".")

emmTSCdf_CI<-emmTSCdf_CI%>%
  left_join(meansTSC, by="cover_type")

emmTSCdf_CI<-emmTSCdf_CI%>%
  mutate(sig=c("a","a","b"))



#plot them

pSOC<-ggplot(emmSOCdf_CI) +
  geom_bar( aes(x=cover_type, y=meanSOC, fill=cover_type), stat="identity")+
  scale_fill_manual(values=c('#A2A475','#D8B70A', '#02401B'))+
  geom_errorbar( aes(x=cover_type, ymin=lower.CL, ymax=upper.CL), 
                 width=0.3, colour="black", alpha=0.9, size=1)+
  geom_text(data = emmSOCdf_CI, aes(x = cover_type, y = meanSOC, label = sig),vjust=-3.5,hjust=+.5)+
  xlab("Cover Type")+ ylab("Soil Organic Carbon g/m2")+theme_bw()+
  ylim(0,2300)+
  theme(legend.position="none")

pTSC<-ggplot(emmTSCdf_CI) +
  geom_bar( aes(x=cover_type, y=meanTSC, fill=cover_type), stat="identity")+
  scale_fill_manual(values=c('#A2A475','#D8B70A', '#02401B'))+
  geom_errorbar( aes(x=cover_type, ymin=lower.CL, ymax=upper.CL), 
                 width=0.3, colour="black", alpha=0.9, size=1)+
  geom_text(data = emmTSCdf_CI, aes(x = cover_type, y = meanTSC, label = sig),vjust=-3.5,hjust=+.5)+
  xlab("Cover Type")+ ylab("Total Soil Carbon g/m2")+theme_bw()+
  ylim(0,2300)+
  theme(legend.position="none")

require(cowplot)
cowplot::plot_grid(pTSC, pSOC, labels = "AUTO", ncol=2)


#export csvs for tables

write.csv(x = emmSOCdf_CI, file = "Analysis/Soil_a/output/SOCresults.csv", row.names = FALSE)
write.csv(x = emmTSCdf_CI, file = "Analysis/Soil_a/output/TSCresults.csv", row.names = FALSE)





#################
##SOIL TSN##

soils_TSN<- soils %>%
  select(cover_type,site_plot,totsoilNcontent_gC_m2_means,site)%>%
  group_by(cover_type, site_plot) %>%
  summarise(meanTSN = mean(totsoilNcontent_gC_m2_means, na.rm = TRUE))

soils_TSN<- soils_TSN%>%
  separate(site_plot, c("site", "plot"), remove= FALSE)

lm(log2(meanTSN) ~ cover_type*site, data=soils_TSN)-> mod1_TSN
performance::check_model(mod1_TSN)
plot(check_normality(mod1_TSN), type = "qq")
summary(mod1_TSN)
plot(mod1_TSN)
Anova(mod1_TSN, type="III")

#issues with normality here.

emmeans::emmeans(mod1_TSN, pairwise~cover_type, adjust = "sidak", type="response")


