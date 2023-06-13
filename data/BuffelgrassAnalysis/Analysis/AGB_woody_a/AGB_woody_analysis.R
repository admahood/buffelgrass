########################
####Data Exploration####
########################

setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")

library(tidyverse)
library(ggplot2)
library(lme4)
library(nlme)
library(MASS)
library(car)
library(bbmle)

#Lets see what the breakdown by species is in each plot
AGB_input<- read.csv("DataPrep/AGB_woody/output/AGB_cleaned_EF.csv")

AGB_input<- AGB_input %>%
  mutate (subplot= 
            ifelse (((subplot=="whole plot")), "wholeplot", subplot))

AGB_species<- AGB_input%>%
  mutate(id = paste(plot, subplot, individual_number, datasheet_number, scientific_name_clean, sep = "_"))

#pivot wider based on new ID
AGB_species <- AGB_species %>%
  pivot_wider(id_cols = id, names_from = measurement_variable_clean, 
              values_from = measurement)
#this warning is okay since we aren't using the measurements
AGB_species <- AGB_species %>%
  select(id)

#now separate out ID column
AGB_species<- AGB_species %>% 
  separate(id, c("site", "plot", "subplot", "individual_number", 
                 "datasheet_number", "Genus", "Species"))
#this warning is play because we don't need those extra numbers

#make a site plot variable and a species variable
AGB_species <- AGB_species %>% 
  mutate(site_plot = paste(site, plot, sep = "_"))%>% 
  mutate(scientific_name = paste(Genus, Species, sep= " "))

#multiply out the total number of individuals based on the subplot sampling scheme

plotmultiply<- read.csv("Analysis/AGB_woody_a/Plot_Multipliers.csv")

AGB_species<-plotmultiply%>%
  left_join(AGB_species,  by = c('site_plot'='site_plot'), na.rm=TRUE)

AGB_species<- AGB_species%>%
  mutate(nindiv= 
           ifelse (((datasheet_number=="3" | datasheet_number=="4")), midsize, 1))

#how many unique species?
unique(AGB_species$scientific_name)
#17 unique species

#filter out the small sized species
AGB_species_nosmall<- AGB_species%>%
  filter(datasheet_number!=7)

unique(AGB_species_nosmall$scientific_name)
#11 unique species

#count number of individuals for each species on each plot
AGB_species_count <- AGB_species_nosmall %>% 
  group_by(site_plot, scientific_name)%>%
  summarize(totalindiv=sum(nindiv))

#need something outside default to see these well
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Set1"))(nb.cols)

ggplot(AGB_species_count, aes(site_plot, totalindiv )) +   
  geom_bar(aes(fill = scientific_name), position = "dodge", stat="identity")+ 
  scale_fill_manual(values= mycolors)
#the numbers of Opuntia and Prosopsis seem high for RR_01, but they are correct
#for both species, there were a couple of large individuals, and then lots of small individuals
#the biomass number for this plot also seems reasonable.
#tried looking at number of each species by cover, and nothing interesting there

#########
##Data Analysis
#########

#bring in the data
AGB_woodyC<- read.csv("Analysis/AGB_woody_a/AGB_woody_biomass.csv")

#plot volume by PECI cover

ggplot(AGB_woodyC, aes(x = PECI_cover, y = TotalC_g_m2, color = site) ) +
  geom_text(aes(label=ALL_cover),hjust=1, vjust=1)+
  geom_smooth()+
  geom_point(size=4) 

#appears to be quadratic where C increases initially, then decreases

#look at it compared to all cover
ggplot(AGB_woodyC, aes(x = ALL_cover, y = TotalC_g_m2, color = site) ) +
  geom_text(aes(label=PECI_cover),hjust=1, vjust=1)+
  geom_point(size=4) 

#looks like a weak linear relationship which would be expected. Interesting!


m0 = lm(TotalC_g_m2 ~PECI_cover*site, data=AGB_woodyC)
summary(m0)
plot(m0)
#definite wedge shape
drop1(m0)

# Try quadratic
m1 = lm(TotalC_g_m2 ~ poly(PECI_cover, 2)*site, data=AGB_woodyC)
summary(m1)
plot(m1)
#wedge shape again. will need to account for this.
#but seems site is not important


m2 = lm(TotalC_g_m2 ~ poly(PECI_cover, 2)*site, data=AGB_woodyC)

summary(m2)
check_model(m2)
check_normality(m2)
plot(m2)

AICctab(m0, m1, m2, weights=T, base=T)
#model 2 is by far the best, but it has some issues that need to be fixed

F2 = fitted(m2)
E2 = resid(m2)

AGB_woodyC = AGB_woodyC %>% mutate(F2 = F2, E2 = E2)

ggplot(AGB_woodyC, aes(x=PECI_cover, color=site)) +
  geom_point(aes(y=TotalC_g_m2), size=5) +
  geom_point(aes(y=F2), size=5, shape=2) +
  geom_line(aes(y=F2)) +
  ggtitle("C_g_m2: 
          Observed (circles) & Expected (m2, triangles)")

ggplot(AGB_woodyC, aes(x=F2, y=E2, color=site)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")

# The residuals do not look okay for either site
# show variance increasing with F2!
# This is a violation of the homogeneity of variance assumption!

#time to z transform some variables and make site a factor
AGB_woodyC = AGB_woodyC %>% mutate(site = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

# nlme has various variance structures that we can use
# need to calculate a complement of peci to use in these variance structures

AGB_woodyC = AGB_woodyC %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))

# for comparison purposes we have to redo m2 using gls 
# also use z transformed to make easier to fit

m2.lm <- gls(TotalC_g_m2 ~ poly(PECI_coverZ, 2), data=AGB_woodyC)
summary(m2.lm) # Correlations between coefficients are a little high
plot(m2.lm)


#see how residuals change with the variables

plot(AGB_woodyC$site, resid(m2.lm), xlab = "site",
       ylab = "Residuals")
plot(AGB_woodyC$PECI_coverZ, resid(m2.lm), xlab = "PECI cover",
       ylab = "Residuals")

#it seems like variance is different between sites
#but it is pretty equal across PECI cover


vf1Fixed <- varFixed(~CompPCov)
m2.gls.1 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
              weights = vf1Fixed, data=AGB_woodyC)
plot(m2.gls.1)
summary(m2.gls.1)
anova(m2.lm, m2.gls.1)

AICtab(m2.lm, m2.gls.1, base=T, weights=T)
#they are about the same

#check to see if this fixes it
E <- resid(m2.gls.1, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

F2.gls.1 = fitted(m2.gls.1)
E2.gls.1 = resid(m2.gls.1)

AGB_woodyC = AGB_woodyC %>% mutate(F2.gls.1 = F2.gls.1, E2.gls.1 = E2.gls.1)

ggplot(AGB_woodyC, aes(x=F2.gls.1, y=E2.gls.1)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")

#try another var function

vf3 <- varPower(form =~ CompPCovZ)

m2.gls.2 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
                weights = vf3, data=AGB_woodyC)

#check to see if this fixes it
E <- resid(m2.gls.2, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

F2.gls.2 = fitted(m2.gls.2)
E2.gls.2 = resid(m2.gls.2)

AGB_woodyC = AGB_woodyC %>% mutate(F2.gls.2 = F2.gls.2, E2.gls.2 = E2.gls.2)

ggplot(AGB_woodyC, aes(x=F2.gls.2, y=E2.gls.2)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")

#seems to have helped a little but variance shoots up at the end!

vf5 <- varExp(form =~ CompPCovZ)

m2.gls.3 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
                weights = vf5, data=AGB_woodyC)

#check to see if this fixes it
E <- resid(m2.gls.3, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

F2.gls.3 = fitted(m2.gls.3)
E2.gls.3 = resid(m2.gls.3)

AGB_woodyC = AGB_woodyC %>% mutate(F2.gls.3 = F2.gls.3, E2.gls.3 = E2.gls.3)

ggplot(AGB_woodyC, aes(x=F2.gls.3, y=E2.gls.3)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")

#keep testing some

vf6 <- varConstPower(form =~ CompPCovZ)

m2.gls.4 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
                weights = vf6, data=AGB_woodyC)

#check to see if this fixes it
E <- resid(m2.gls.4, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

F2.gls.4 = fitted(m2.gls.4)
E2.gls.4 = resid(m2.gls.4)

AGB_woodyC = AGB_woodyC %>% mutate(F2.gls.4 = F2.gls.4, E2.gls.4 = E2.gls.4)

ggplot(AGB_woodyC, aes(x=F2.gls.4, y=E2.gls.4)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")

#try a combo structure

vf8 <- varComb(varPower(form =~ CompPCovZ),
               varExp(form =~ CompPCovZ))

m2.gls.5 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
                weights = vf8, data=AGB_woodyC)

#check to see if this fixes it
E <- resid(m2.gls.5, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

F2.gls.5 = fitted(m2.gls.5)
E2.gls.5 = resid(m2.gls.5)

AGB_woodyC = AGB_woodyC %>% mutate(F2.gls.5 = F2.gls.5, E2.gls.5 = E2.gls.5)

ggplot(AGB_woodyC, aes(x=F2.gls.5, y=E2.gls.5)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")

#try including site in the variance structure?

vf2 <- varIdent(form= ~ 1 | site)

m2.gls.6 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
                weights = vf2, data=AGB_woodyC)

#check to see if this fixes it
E <- resid(m2.gls.6, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

summary(m2.gls.6)


#the residuals will still show heterogeneity, but this is okay
#instead we want to plot the normalized residuals. these should not show heterogeneity

E <- resid(m2.gls.6, type="normalized")
coplot(E ~ PECI_coverZ | site, data = AGB_woodyC)


F2.gls.6 = fitted(m2.gls.6)
E2.gls.6 = resid(m2.gls.6, type="normalized")


#try one of the combos

vf9 <- varComb(varIdent(form =~ 1 | site),
               varExp(form =~ CompPCovZ))

m2.gls.7 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
                weights = vf9, data=AGB_woodyC)

#check to see if this fixes it
E <- resid(m2.gls.7, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

F2.gls.7 = fitted(m2.gls.7)
E2.gls.7 = resid(m2.gls.7)

AGB_woodyC = AGB_woodyC %>% mutate(F2.gls.7 = F2.gls.7, E2.gls.7 = E2.gls.7)

ggplot(AGB_woodyC, aes(x=F2.gls.7, y=E2.gls.7)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m2)")

#test all the options we created
AICctab(m2.lm, m2.gls.1, m2.gls.2, m2.gls.3, m2.gls.4, 
        m2.gls.5, m2.gls.6, m2.gls.7, weights=T, base=T)

#m2.gls.1 was best. try it with an interaction

vf1Fixed <- varFixed(~CompPCov)
m2.gls.10 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)*site, 
                weights = vf1Fixed, data=AGB_woodyC)

#test all the options we created
AICctab(m2.lm, m2.gls.1, m2.gls.2, m2.gls.3, m2.gls.4, 
        m2.gls.5, m2.gls.6, m2.gls.7, m2.gls.10, weights=T, base=T)

#now m2.gls.10 is the best
E <- resid(m2.gls.10, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)

#try another var function 

vf9 <- varComb(varIdent(form =~ 1 | site),
               varExp(form =~ CompPCovZ))

m2.gls.11 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)*site, 
                 weights = vf9, data=AGB_woodyC)

AICctab(m2.lm, m2.gls.1, m2.gls.2, m2.gls.3, m2.gls.4, 
        m2.gls.5, m2.gls.6, m2.gls.7, m2.gls.10,
        m2.gls.11, weights=T, base=T)

#model m2.gls.10 is still the best


vf11 <- varExp(form =~ CompPCovZ | site)

m2.gls.12 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)*site, 
                 weights = vf11, data=AGB_woodyC)

AICctab(m2.lm, m2.gls.1, m2.gls.2, m2.gls.3, m2.gls.4, 
        m2.gls.5, m2.gls.6, m2.gls.7, m2.gls.10,
        m2.gls.11,m2.gls.12, weights=T, base=T)


vf12 <- varConstPower(form = ~CompPCovZ|site)

m2.gls.13 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)*site, 
                 weights = vf12, data=AGB_woodyC)

AICctab(m2.lm, m2.gls.1, m2.gls.2, m2.gls.3, m2.gls.4, 
        m2.gls.5, m2.gls.6, m2.gls.7, m2.gls.10,
        m2.gls.11,m2.gls.12,m2.gls.13, weights=T, base=T)


summary(m2.gls.10)

#check the coplot

E <- resid(m2.gls.10, type= "normalized")
coplot(E ~ CompPCovZ|site, data = AGB_woodyC)
#these residuals look good to me!
#a little funky on RR side, but nothing concerning


#find the significance of peci cover
#https://stats.stackexchange.com/questions/13859/finding-overall-p-value-for-gls-model


vf1Fixed <- varFixed(~CompPCov)
m2.gls.10 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)*site, 
                 weights = vf1Fixed, data=AGB_woodyC, method="REML")
summary(m2.gls.10)
anova(m2.gls.10)


#remodel using ML so we can compare the models

m2.gls.10.ML <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)*site, 
                                weights = vf1Fixed, data=AGB_woodyC, method= "ML")

anova(m2.gls.10.ML)
#anova here is using single term deletions
#so we can only trust pval for interaction term
#interaction is not significant p=0.1617

#create a model with no interaction
m2.gls.10.ML.noint <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)+site, 
                    weights = vf1Fixed, data=AGB_woodyC, method= "ML")

anova(m2.gls.10.ML, m2.gls.10.ML.noint)
#using this method, the interaction is significant (p=0.0339)

#create a model without using site
m2.gls.10.ML.nosite <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2), 
                    weights = vf1Fixed, data=AGB_woodyC, method= "ML")

anova(m2.gls.10.ML, m2.gls.10.ML.nosite)
#site is marginally significant (p=0.0588)
#wouldn't drop it anyway because the interaction is significant

#create a model without using PECI
m2.gls.10.ML.nopeci <- gls(TotalC_g_m2 ~site, 
                           weights = vf1Fixed, data=AGB_woodyC, method= "ML")

anova(m2.gls.10.ML, m2.gls.10.ML.nopeci)
#Peci cover is highly significant (p=0.0009)

summary(m2.gls.10.ML)
summary(m2.gls.10.ML.noint)
summary(m2.gls.10.ML.nosite)
summary(m2.gls.10.ML.nopeci)


AICtab(m2.gls.10.ML, m2.gls.10.ML.noint, m2.gls.10.ML.nosite, m2.gls.10.ML.nopeci, weights=TRUE)

#                       dAIC  df  weight
# m2.gls.10.ML          0.0   7   0.5750
# m2.gls.10.ML.nosite   1.5   4   0.2783
# m2.gls.10.ML.noint    2.8   5   0.1441
# m2.gls.10.ML.nopeci   10.8  3   0.0026

#full model with interaction is the best.
#anova tests that drop site and interaction show these are not significant
#BUT aic has us choose full model and residuals when the other are dropped look bad

#compare full model with a null model to determine pval for peci
m2.gls.10.ML.null <- gls(TotalC_g_m2 ~ 1+site, 
                           weights = vf1Fixed, data=AGB_woodyC, method= "ML")

summary(m2.gls.10.ML.null)

#anova to compare
anova(m2.gls.10.ML,m2.gls.10.ML.null) 

#                         Model df      AIC       BIC       logLik      Test    L.Ratio   p-value
# m2.gls.10.ML            1     7       137.8148  141.7694  -61.90738                        
# m2.gls.10.ML.nopeci     2     3       148.5927  150.2875  -71.29633   1 vs 2  18.77789  9e-04

# peci cover was highly significant
#this is the same as the no peci model whichi s good.

#check the final model

#see Zuur Ch4
 E <- resid(m2.gls.10, type = "normalized")
 Fit <- fitted(m2.gls.10)
 op <- par(mfrow = c(1, 2))
 plot(x = Fit, y = E, 
       xlab = "Fitted values", ylab = "Residuals",
       main = "Residuals versus fitted values")
 #identify(Fit, E) this is making R want to crash
 hist(E, nclass = 5)
 par(op)
#these residuals look good!
 
 #plot it so that we can see by site
 F.gls.10 = fitted(m2.gls.10)
 E.gls.10 = resid(m2.gls.10, type = "normalized")
 
 AGB_woodyC = AGB_woodyC %>% mutate(F.gls.10 = F.gls.10, E.gls.10 = E.gls.10)
 
 ggplot(AGB_woodyC, aes(x=F.gls.10, y=E.gls.10, color=site)) +
   geom_point(size=2) +
   geom_smooth(method=lm, se=TRUE)+
   ggtitle("Residuals vs. Expected (m2.gls.10)")

#the tiniest increase with LOLA, but I think these residuals look okay

#look at the final model
summary(m2.gls.10)


#lets draw the model 
#using method below from Bolker
#https://stackoverflow.com/questions/14033551/r-plotting-confidence-bands-with-ggplot

vf1Fixed <- varFixed(~CompPCov)
m2.gls.10 <- gls(TotalC_g_m2 ~poly(PECI_coverZ, 2)*site, 
                 weights = vf1Fixed, data=AGB_woodyC)

fit <- predict(m2.gls.10)
summary(fit)
V <- vcov(m2.gls.10)
X <- model.matrix(~poly(PECI_coverZ, 2)*site, 
                  weights = vf1Fixed, data=AGB_woodyC)
se.fit <- sqrt(diag(X %*% V %*% t(X)))
predframe <- with(AGB_woodyC,data.frame(PECI_coverZ, site,PECI_cover,
                                TotalC_g_m2=fit,lwr=fit-1.96*se.fit,upr=fit+1.96*se.fit))


#plot the model with confidence intervals


ggplot(AGB_woodyC, aes(PECI_cover, TotalC_g_m2, color = site))+
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_line(data=predframe, size=1)+
  geom_ribbon(data=predframe,
              aes(ymin=lwr,ymax=upr, fill = site, color = NULL),alpha=0.2)+
  xlab("Buffelgrass Cover (%)")+ ylab("Woody Carbon (g/m2)")+
  theme_bw()+
  ylim(-100,250)+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4))












head( seq(min(AGB_woodyC$PECI_coverZ), max(AGB_woodyC$PECI_coverZ), by = .01) )

newPECI = expand.grid(PECI_coverZ = seq(min(AGB_woodyC$PECI_coverZ), max(AGB_woodyC$PECI_coverZ), by = .01),
                      site = unique(AGB_woodyC$site) )

fit = predict(m2.gls.10, newdata = newPECI)
summary(fit)
V <- vcov(m2.gls.10)
X <- model.matrix(~poly(PECI_coverZ, 2)*site, 
                  weights = vf1Fixed, data=newPECI)

se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )
newPECI$fit   = with(newPECI, fit)

F0 = fitted(m2.gls.10)
E0 = resid(m2.gls.10)

AGB_woodyC = AGB_woodyC %>% mutate(F0 = F0, E0 = E0)

newPECI$PECI_cover= sd(PECI$PECI_cover)*newPECI$PECI_coverZ+mean(PECI$PECI_cover)

#plot the data with confidence intervals
#Figure
ggplot(AGB_woodyC, aes(x = PECI_cover, y = TotalC_g_m2, color=site)) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill=site), alpha = .15) +
  geom_line(aes(y=fit), data=newPECI)+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Aboveground Woody Carbon (g/m2)")+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4))
