#buffelgrass herbaceous biomass carbon
#March 23, 2021
#Dr. Emily Fusco

wes_palette("Cavalcanti1")
?wes_palette

#hex codes 
# yellow-"#D8B70A" green-"#02401B" gray-"#A2A475" blue-"#81A88D" red-"#972D15"

setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")

require(tidyverse)
require(wesanderson)

herb <- as.data.frame(read_csv("Analysis/AGB_herb_a/herb_carb_clean.csv"))
cover <- as.data.frame(read_csv("Analysis/AGB_herb_a/cover.csv"))

#first prelim plot!

ggplot(herb, aes(x = PECI_cover, y = herbcarb_g_m2, color = site) ) +
  geom_point(size=4) 

#take the average herbcarb

herbcarbsubplotn<-herb%>%
  group_by(site_plot)%>%
  count(plot)
#not all of the plots had 8 samples, but they all need to be divided by 8
#this accounts for bare ground where no sample was taken

herbcarbavg<- herb%>%
  group_by(site_plot)%>%
  summarize(totalherb= sum(herbcarb_g_m2))%>%
  mutate(avgbio=totalherb/8)

herbcarbavg<-cover%>%
  left_join(herbcarbavg,  by = c('site_plot'='site_plot'), na.rm=FALSE)

ggplot(herbcarbavg, aes(x = PECI_cover, y = avgbio, color = site) ) +
  geom_point(size=4)+
  ylab("Average Carbon g/m^2")

range(herbcarbavg$avgbio)
#################################################################################
#################################################################################

#Time to fit some models

#################################################################################
#################################################################################


library(lme4)
library(nlme)
library(MASS)
library(car)
library(ggplot2)
library(tidyverse)
library(bbmle)
library(performance)

#make site a factor

herbcarbavg<- herbcarbavg%>%
  mutate(site=as.factor(site))

m0 = lm(avgbio ~PECI_cover*site, data=herbcarbavg)
check_model(m0)

summary(m0)
#plot(m0)
drop1(m0) 
#                   Estimate    Std. Error  t value   Pr(>|t|)    
# (Intercept)        22.1031    15.3455     1.440     0.184    
# PECI_cover          5.3136    0.4167      12.753    4.58e-07 ***
# siteRR             25.1406    19.9017     1.263     0.238    
# PECI_cover:siteRR  -1.2398    0.5979      -2.074    0.068 .

# The interaction is weakly important. Will probably keep it for consistency

# Calculate fitteds & resids
# F0 = fitted(m0)
# E0 = resid(m0)
# 
# herbcarbavg = herbcarbavg %>% mutate(F0 = F0, E0 = E0)

ggplot(herbcarbavg, aes(x=F0, y=E0, color=site)) +
  geom_point(size=5) +
  geom_smooth(se=F) +
  ggtitle("Residuals vs. Expected (m0)")
#I think the residuals look okay

# Breusch-Pagan test
lmtest::bptest(m0) 
#absence of heteroscedasticity, p=0.3177

shapiro.test(residuals(m0))
#passes shapiro test p=0.4933

library(rstatix)
identify_outliers(as.data.frame(herbcarbavg$avgbio))
#I guess those are technically outliers but they are correct. 
#they just occur at the really high cover levels

#I think the original model is fine then?

#try without site
m1 = lm(avgbio ~PECI_cover, data=herbcarbavg)
check_model(m1)
check_normality(m1)
check_outliers(m1)
summary(m1)
plot(m1)

#             Estimate    Std. Error  t value   Pr(>|t|)    
#(Intercept)  36.0289     10.7729     3.344     0.00654 ** 
#PECI_cover    4.7293     0.3254      14.535    1.59e-08 ***


# Breusch-Pagan test
lmtest::bptest(m1) 
#absence of heteroscedasticity, p=0.4089

shapiro.test(residuals(m1))
#passes shapiro test p=0.1451

bbmle::AICctab(m0,m1, weights=TRUE)
#     dAICc   df  weight
# m1  0.0     3   0.913 
# m0  4.7     5   0.087

check_model(m1)

#try logging the response
#try without site
m1log = lm(log(avgbio+1) ~PECI_cover, data=herbcarbavg)
summary(m1log)
check_model(m1log)
check_outliers(m1log)
check_normality(m1log)

summary(m1log)

m1log.1 <- lm(log(avgbio+1) ~PECI_cover*site, data=herbcarbavg)
check_model(m1log.1)
check_normality(m1log.1)
check_outliers(m1log.1)
check_collinearity(m1log.1)
check_heteroscedasticity(m1log.1)
shapiro.test(residuals(m1log.1))
summary(m1log.1)


m1sqrt<-lm(sqrt(avgbio) ~PECI_cover, data=herbcarbavg)
check_model(m1sqrt)
check_outliers(m1sqrt)
check_normality(m1sqrt)
summary(m1sqrt)
#m1sqrt is the best model
#residuals look the best here

m1sqrt.2<-lm(sqrt(avgbio) ~PECI_cover+site, data=herbcarbavg)
check_model(m1sqrt.2)
check_outliers(m1sqrt.2)
check_normality(m1sqrt.2)
summary(m1sqrt.2)
#violates outliers

m1sqrt.1<-lm(sqrt(avgbio) ~PECI_cover*site, data=herbcarbavg)
check_model(m1sqrt.1)
check_outliers(m1sqrt.1)
check_normality(m1sqrt.1)
summary(m1sqrt.1)
#this model is not good. violates outliers and normality

AICtab(m1, m1log, m1log.1, m1sqrt, m1sqrt.1,m1sqrt.2, weights=TRUE)
AICtab(m1,m1sqrt, m1sqrt.1,m1sqrt.2, weights=TRUE)

#m1log.1 is the best model
#it meets all assumptions and has the best AIC

summary(m1log.1)
#   Call:
#   lm(formula = log(avgbio + 1) ~ PECI_cover * site, data = herbcarbavg)
# 
# Residuals:
#   Min         1Q        Median      3Q        Max 
#   -0.55824    -0.14718  -0.00683    0.08943   0.66838 
# 
# Coefficients:
#                      Estimate   Std. Error  t value   Pr(>|t|)    
#   (Intercept)        3.289111   0.206331    15.941    6.64e-08 ***
#   PECI_cover         0.044589   0.005602    7.959     2.31e-05 ***
#   siteRR             0.977562   0.267592    3.653     0.00529 ** 
#   PECI_cover:siteRR -0.023437   0.008040    -2.915    0.01717 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Residual standard error: 0.3262 on 9 degrees of freedom
# Multiple R-squared:  0.896,	Adjusted R-squared:  0.8613 
# F-statistic: 25.84 on 3 and 9 DF,  p-value: 9.34e-05

anova(m1log.1)

#time to plot it with confidence intervals
#plot on raw data scale

m1log.1 <- lm(log(avgbio+1) ~PECI_cover*site, data=herbcarbavg)

head( seq(min(herbcarbavg$PECI_cover), max(herbcarbavg$PECI_cover), by = .01) )

newPECI = expand.grid(PECI_cover = seq(min(herbcarbavg$PECI_cover), max(herbcarbavg$PECI_cover), by = .01),
                      site = unique(herbcarbavg$site) )

fit = predict(m1log.1, newdata = newPECI)

V <- vcov(m1log.1)
X <- model.matrix(~ PECI_cover*site, data=newPECI)

se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )
newPECI$fit   = with(newPECI, fit)

F0 = fitted(m1log.1)
E0 = resid(m1log.1)

herbcarbavg = herbcarbavg %>% mutate(F0 = F0, E0 = E0)

#plot the data with confidence intervals
#Figure
ggplot(herbcarbavg, aes(x = PECI_cover, y = avgbio, color=site)) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = exp(lower)-1, ymax = exp(upper)-1, 
                  color = NULL, fill=site), alpha = .15) +
  geom_line(data=newPECI,
            aes(y=exp(fit)-1))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Herbaceous Carbon g/m2")+
  guides(fill=FALSE)+
  theme(
    legend.position = c(.01, .99),
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4))


























#make the same plot with the log data

m1log.1 <- lm(log(avgbio+1) ~PECI_cover*site, data=herbcarbavg)

head( seq(min(herbcarbavg$PECI_cover), max(herbcarbavg$PECI_cover), by = .01) )

newPECI = expand.grid(PECI_cover = seq(min(herbcarbavg$PECI_cover), max(herbcarbavg$PECI_cover), by = .01),
                      site = unique(herbcarbavg$site) )

fit = predict(m1log.1, newdata = newPECI)

V <- vcov(m1log.1)
X <- model.matrix(~ PECI_cover*site, data=newPECI)

se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )
newPECI$fit   = with(newPECI, fit)

F0 = fitted(m1log.1)
E0 = resid(m1log.1)

herbcarbavg = herbcarbavg %>% mutate(F0 = F0, E0 = E0)
?scale_y_continuous
#plot the data with confidence intervals
#Figure
ggplot(herbcarbavg, aes(x = PECI_cover, y = log(avgbio+1), color=site)) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill=site), alpha = .15) +
  geom_line(aes(y=F0))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Log Transformed Herbaceous Carbon g/m2")+
  guides(fill=FALSE)+
  theme(
    legend.position = c(.99, .3),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4))
