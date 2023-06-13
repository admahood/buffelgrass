setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")
soils <- read.csv("Analysis/Soil_a/SoilData.csv")

#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "FSA", "lme4", "rstatix","car", "ggpubr")
lapply(x, library, character.only = TRUE, verbose = FALSE)

soils_SOC<- soils %>%
  select(cover_type,site_plot,SOCcontent_gC_m2_means,site)%>%
  group_by(cover_type, site_plot) %>%
  summarise(meanSOC = mean(SOCcontent_gC_m2_means, na.rm = TRUE))

soils_SOC<- soils_SOC%>%
  separate(site_plot, c("site", "plot"), remove= FALSE)

##
#data viz##
##

ggplot(soils_SOC, aes(x=site, y=meanSOC, fill=cover_type)) +
  geom_boxplot()+
  scale_fill_manual(values=c('#CC9966','#FFFF33', '#669900'))

soils_SOC %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC)
#LOLA_04 other and RR_07 other are outliers
#will need to run analysis with and without outliers

#check to see if it is balanced

table(soils_SOC$site, soils_SOC$cover_type)
#        bare  buffel other
# Lola    6      5     6
# RR      7      5     7
#it's not balanced but it is close... not sure if thats okay


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
#Type III is recommended for unbalanced designs


#create linear model to test normality assumption

test.lm.block = lm(meanSOC ~ cover_type*site, data = soils_SOC)
plot(test.lm.block)
#QQ a little funky
shapiro.test(residuals(test.lm.block))
#p=0.0001277, so not normal

#check normality by groups
soils_SOC %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC)
#LOLA and RR other sites are not normal

#try a log transformation
soils_SOC<- soils_SOC%>%
  mutate(meanSOC_log= log(meanSOC))

#check using log transformed data
#check normality by groups
soils_SOC %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC_log)
#LOLA other is still bad

#check for homogeneity of variance
leveneTest(meanSOC ~ cover_type*site, data = soils_SOC)
#p=0.8415 so no violation

#create a new anova with transformed variables
block.fit.tran=aov(meanSOC_log~as.factor(cover_type)*site, data = soils_SOC)
summary(block.fit.tran)
plot(block.fit.tran)
#I think these look okay

#check for homogeneity of variance again with log transformed data
leveneTest(meanSOC_log ~ cover_type*site, data = soils_SOC)
#p=0.3785 so no violation

#check for outliers again using log transformed data
soils_SOC %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_log)
#LOLA_04 other and RR_07 other are still extreme outliers

#try a square root transformation
soils_SOC<- soils_SOC%>%
  mutate(meanSOC_sqrt= sqrt(meanSOC))

soils_SOC %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_sqrt)
#yes, still outliers

#check using sqrt transformed data
#check normality by groups
soils_SOC %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC_sqrt)
#still issues with "other" rr and lola plots

#check for homogeneity of variance again with sqrt transformed data
leveneTest(meanSOC_sqrt ~ cover_type*site, data = soils_SOC)
#p=0.7333 so no violation

#try running anova with log data with no outliers
soils_SOC_no_outliers<-soils_SOC%>%
  mutate(site_plot_cover= paste(site_plot, cover_type, sep="_"))

soils_SOC_no_outliers<-soils_SOC_no_outliers%>%
  filter(site_plot_cover!="Lola_04_other")%>%
  filter(site_plot_cover!="RR_07_other")

#check normality by groups
soils_SOC_no_outliers %>%
  group_by(cover_type, site) %>%
  shapiro_test(meanSOC_log)
#LOLA and RR other sites are not normal
#everything works now

#check for outliers again using log transformed data
soils_SOC_no_outliers %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_log)
#no outliers now which makes sense

#check for homogeneity of variance again with sqrt transformed data
leveneTest(meanSOC_log ~ cover_type*site, data = soils_SOC_no_outliers)
#p=0.08414 so no violation

#create a new anova with transformed variables and no outliers
block.fit.tran.no.outliers=aov(meanSOC_log~as.factor(cover_type)+site, data = soils_SOC_no_outliers)
summary(block.fit.tran.no.outliers)
plot(block.fit.tran.no.outliers)

#                        Df   Sum Sq  Mean Sq   F value  Pr(>F)   
# as.factor(cover_type)  2    0.5795  0.2898    3.125    0.05852 . 
# site                   1    1.2116  1.2116    13.066   0.00109 **
# Residuals             30    2.7818  0.0927

#ack I dont know. 

#try testing for log and sqrt transformed outliers
soils_SOC<- soils_SOC%>%
  mutate(meanSOC_log_sqrt= sqrt(meanSOC_log))

soils_SOC %>%
  group_by(site, cover_type) %>%
  identify_outliers(meanSOC_log_sqrt)
#doesn't help



#run 

#check assumptions
plot(fit)
plot(block.fit)

#test for normality
test.lm = lm(meanSOC ~ cover_type, data = soils_SOC)
shapiro.test(residuals(test.lm))
#p-value = 0.01547; not normal



#make it a random effect model
# mixed.lmer <- lmer(meanSOC ~ cover_type + (1|site), data = soils_SOC)
# summary(mixed.lmer)
# plot(mixed.lmer)
#but you cant have a random effect with only 2 groups?



#try to transform
test.lm1a <- lm(log(meanSOC) ~ cover_type, data = soils_SOC)
shapiro.test(residuals(test.lm1a))
#p-value = 0.5267; it's normal now

test.lm1a.block <- lm(log(meanSOC) ~ cover_type+site, data = soils_SOC)
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
with(soils_SOC,
     interaction.plot(cover_type,site,meanSOC, col=1:4))
#meets assumption, lines are parallel using log or not

#Finally build the anova https://www.datanovia.com/en/lessons/anova-in-r/
#http://www.sthda.com/english/wiki/two-way-anova-test-in-r
?anova_test
summary(soils_SOC)
anova_test(meanSOC_log ~ as.factor(cover_type) * site, data= soils_SOC)
model.3<-anova(model.2)
summary(model.2)
summary(model.3)
glimpse(soils_SOC)

model.anova.log=aov(meanSOC_log~as.factor(cover_type)*site, data = soils_SOC)
summary(model.anova)
Anova(model.anova.log, type = "III")
require(emmeans)
emmSOC = emmeans(model.anova.log, specs = pairwise ~ cover_type, type="response")
emmSOC
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

SOCother<-soils_SOC%>%
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