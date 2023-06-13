#################################################################################
#################################################################################

#Time to fit some models

#################################################################################
#################################################################################

setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis/Analysis/Diversity_a")

library(lme4)
library(nlme)
library(MASS)
library(car)
library(ggplot2)
library(tidyverse)
library(bbmle)

PECI<- read.csv("Biodiversity.csv")


# ## You can't have a random effect with only 2 groups!
# m0 = lm(shannon ~PECI_cover*site, data=PECI)
# summary(m0)
# plot(m0)
# drop1(m0) # The interaction is important! Can't drop it.
# 
# # Summary:
# #   Intercept:            2.1125
# #   PECI_Cover slope:    -0.0259
# #   RR Intercept:        -0.1657
# #   RR/PECI_Cover slope:  0.0215
# #   
# # Rearrange to get intercepts and slopes for each site
# #   LOLA Intercept:         2.1125
# #   LOLA PECI_Cover slope: -0.0259
# #   RR Intercept:           1.9468
# #   RR PECI_Cover slope:   -0.004452
# 
# # Try quadratic
# m1 = lm(shannon ~ poly(PECI_cover, 2)*site, data=PECI)
# summary(m1)
# plot(m1)
# 
# drop1(m1) # The interaction is important! Can't drop it.
# 
# AICctab(m0, m1, weights=T, base=T)
# # The linear model (m0) is MUCH better than the quadratic (m1)!
# 
# # Calculate fitteds & resids
# F0 = fitted(m0)
# E0 = resid(m0)
# 
# PECI = PECI %>% mutate(F0 = F0, E0 = E0)
# 
# ggplot(PECI, aes(x=PECI_cover, color=site)) +
#   geom_point(aes(y=shannon), size=5) +
#   geom_point(aes(y=F0), size=5, shape=2) +
#   geom_line(aes(y=F0)) +
#   ggtitle("Shannon Diversity: 
#           Observed (circles) & Expected (m0, triangles)")
# 
# ggplot(PECI, aes(x=F0, y=E0, color=site)) +
#   geom_point(size=5) +
#   geom_smooth(se=F) +
#   ggtitle("Residuals vs. Expected (m0)")

# The residuals look OK for the LOLA site, but the
#    show variance increasing with F0!
# This is a violation of the homogeneity of variance assumption!
# Actually, Lola shows increasing variance with F0, just not as extreme.

# Fix some variables
PECI = PECI %>% mutate(fSite = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

# We need a variance covariate! Complement of Peci_cover?

# nlme has various variance structures that we can use
#  for comparison purposes we have to redo m0 using gls
# 
# m0.lm <- gls(shannon ~PECI_cover*site, data=PECI)
# summary(m0.lm) # Correlations between coefficients are a little high

PECI = PECI %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))

#first model dealing with variance
vf1Fixed <- varFixed(~CompPCov)
M.gls1 <- gls(shannon ~PECI_cover*site, 
              weights = vf1Fixed, data=PECI)
anova(m0.lm, M.gls1)

# Use transformed x variables
m1.lm <- gls(shannon ~PECI_coverZ*fSite, data=PECI)
summary(m1.lm) # Correlations among coefs are lower

vf1Fixed <- varFixed(~CompPCovZ)
M1.gls1 <- gls(shannon ~PECI_coverZ*fSite, 
               weights = vf1Fixed, data=PECI)
summary(M1.gls1)
anova(m1.lm, M1.gls1)

# M.gls1 better than m0.lm. Let's check AICc.
AICtab(m1.lm, M1.gls1, base=T, weights=T)
# AIC agrees that m1.gls1 is the better model
# We'll use the transformed predictors to avoid high coef correlations

# Need to account for difference in variance between sites
vf2 <- varIdent(form= ~ 1 | fSite)
M.gls2 <- gls(shannon ~PECI_coverZ*fSite,
              weights = vf2, data=PECI)
#anova(m1.lm, M1.gls1, M.gls2)

# M.gls2 is doing slightly better than M1.gls1 (but not significanntly)
# We need to combine these two variance structures!

# First, let's look at the way the errors are distributed in m1.lm
# E <- resid(m1.lm)
# coplot(E ~ CompPCovZ | fSite, data = PECI)
# 
# E1 = resid(M1.gls1)
# coplot(E1~ CompPCovZ | fSite, data = PECI)
# 
# E2 = resid(M.gls2)
# coplot(E2 ~ CompPCovZ | fSite, data = PECI)

# These errors only only slightly better so far.

# Let's try other variance structures
vf3 <- varPower(form =~ CompPCovZ)
M.gls3 <- gls(shannon ~PECI_coverZ*fSite,
              weights = vf3, data=PECI)
# anova(m1.lm, M1.gls1, M.gls2, M.gls3)
# # M.gls3 is worse than m.gls2, about the same as M1.gls1
# 
vf4 <- varPower(form =~ CompPCovZ | fSite)
M.gls4 <- gls(shannon ~PECI_coverZ*fSite,
              weights = vf4, data=PECI)
# anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4)
# # M.gls4 is better than all except M.gls2
# summary(M.gls4)
# # You can see the power parameter estimates are different for each site.
# #   RR has the higher one!

vf5 <- varExp(form =~ CompPCovZ)
M.gls5 <- gls(shannon ~PECI_coverZ*fSite,
              weights = vf5, data=PECI)
#anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5)
# M.gls5 is the best so far!

vf5.1 <- varExp(form =~ CompPCovZ|fSite)
M.gls5.1 <- gls(shannon ~PECI_coverZ*fSite,
                weights = vf5.1, data=PECI)
#anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1)
# M.gls5.1 is the best so far!

vf6 <- varConstPower(form =~ CompPCovZ)
M.gls6 <- gls(shannon ~PECI_coverZ*fSite,
              weights = vf6, data=PECI)
#anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,M.gls6)
# M.gls5.1 is still the best!

vf7 <- varConstPower(form = ~CompPCovZ|fSite)
M.gls7 <- gls(shannon ~PECI_coverZ*fSite,
              weights = vf7, data=PECI)
#anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1, M.gls6, M.gls7)

# So far the VarExp structure given is the best (vf5.1)
# Let's combine it with the vaeident structure using varComb

vf8 <- varComb(varIdent(form =~ 1 | fSite),
               varExp(form =~ CompPCovZ | fSite) )
M.gls8 <- gls(shannon ~ PECI_coverZ*fSite,
              weights = vf8, data=PECI)
#anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,M.gls6, M.gls7, M.gls8)
# M.gls5.1 is the best so far!

vf9 <- varComb(varIdent(form =~ 1 | fSite),
               varExp(form =~ CompPCovZ) )
M.gls9 <- gls(shannon ~ PECI_coverZ*fSite,
              weights = vf9, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6, M.gls7, M.gls8, M.gls9)
#M.gls5.1 has lowest AIC!
anova(M.gls5.1,M.gls8)


#use a more intuitive variable to see if model 5.1 still works
#If we use PECI_coverZ instead of CompPCovZ do we get the same answer
PECI = PECI %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))
PECI = PECI %>% mutate(fSite = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

vf5.1b <- varExp(form =~ PECI_coverZ|fSite)
M.gls5.1b <- gls(shannon ~PECI_coverZ*fSite,
                 weights = vf5.1b, data=PECI)

# Compare models with AICc
AICtab(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1, M.gls5.1b,
       M.gls6, M.gls7, M.gls8, M.gls9, base=T, weights=T)

summary(M.gls5.1b)
summary(M.gls5.1)
# Yes, they are identical!
#5.1 and 5.1b are the same

# Make coplot for best 5 models to see how we did
E5 = resid(M.gls5)
coplot(E5 ~ CompPCovZ | fSite, data = PECI)

E5.1 = resid(M.gls5.1)
coplot(E5.1 ~ CompPCovZ | fSite, data = PECI)

E8 = resid(M.gls8)
coplot(E8 ~ CompPCovZ | fSite, data = PECI)

E9 = resid(M.gls9)
coplot(E9 ~ CompPCovZ | fSite, data = PECI)

E5.1b = resid(M.gls5.1b, type="normalized")
coplot(E5.1b ~ CompPCovZ | fSite, data = PECI)

E5.1n = resid(M.gls5.1n, type= "normalized")
coplot(E5.1n ~ CompPCovZ | fSite, data = PECI)

# There are no obvious winners in the coplots among the top 4 models.
# These were all whithin 2 AIC units of th ebest model (M.gls5.1).

# You could do some more exploring, but we have basically fixed the problem by 
# changing the assumption from equal variance everywhere, to variance that
# varies by site and CompPCovZ



# Plot the model
E5.1b = resid(M.gls5.1b)
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(M.gls5.1b))
ggplot(PECI, aes(x=F5.1b, y=E5.1b, color=fSite)) +
  geom_point(size=5) + geom_smooth(se=F) +
  ggtitle("Residuals vs. Fitteds for Model M.gls5.1b")

ggplot(PECI, aes(x=PECI_cover, color=site)) +
  geom_point(aes(y=shannon), size=5) +
  geom_point(aes(y=F5.1b), size=5, shape=2) +
  geom_line(aes(y=F5.1b)) +
  ggtitle("Shannon Diversity: 
          Observed (circles) & Expected (M.gls5.1b, triangles)")

summary(M.gls5.1b)
plot(M.gls5.1b)


#PECI_cover is highly significant (p = 0.00)
#site is significant
#the interaction between site and cover is highly significant.

#final model check
E <- resid(M.gls5.1b, type = "normalized")
Fit <- fitted(M.gls5.1b)
op <- par(mfrow = c(1, 2))
plot(x = Fit, y = E, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals versus fitted values")
#identify(Fit, E) this is making R want to crash
hist(E, nclass = 3)
par(op)
#looks okay

# We could figure out how to draw the uncertainty envelope around the 
# regression lines, and we probably should. 
# Remember that variance is now a function of PECI_cover and site

#now we will try to draw the envelope!

head( seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .001) )

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(M.gls5.1b, newdata = newPECI)


V <- vcov(M.gls5.1b)
X <- model.matrix(~PECI_coverZ*fSite,
                  weights = vf5.1b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 2*sqrt(se.fit) )
newPECI$upper = with(newPECI, fit + 2*sqrt(se.fit) )

#back transform peci cover

mean(PECI$PECI_cover)
sd(PECI$PECI_cover)

newPECI$PECI_cover= sd(PECI$PECI_cover)*newPECI$PECI_coverZ+mean(PECI$PECI_cover)


#plot the data with confidence intervals
pShannon<-ggplot(PECI, aes(x = PECI_cover, y = shannon, color = site) ) +
  geom_point(size=4) +
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F5.1b))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Shannon Diversity")+
  guides(fill=FALSE)+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4))



# #model summary
# summary(M.gls5.1b)
# 
# 
# #see if the variance covariance matrix accounts for var structure
# 
# M.gls5.test<- gls(shannon ~PECI_coverZ*fSite,data=PECI)
# 
# AICtab(M.gls5.1b, M.gls5.test)
# summary(M.gls5.test)
# 
# 
# head( seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01) )
# 
# newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
#                       fSite = unique(PECI$fSite) )
# 
# fit2 = predict(M.gls5.test, newdata = newPECI)
# 
# 
# V <- vcov(M.gls5.test)
# X <- model.matrix(~PECI_coverZ*fSite,data=newPECI)
# se.fit <- sqrt(diag(X %*% V %*% t(X)))
# 
# newPECI$lower = with(newPECI, fit2 - 2*sqrt(se.fit) )
# newPECI$upper = with(newPECI, fit2 + 2*sqrt(se.fit) )
# 
# #back transform peci cover
# 
# mean(PECI$PECI_cover)
# sd(PECI$PECI_cover)
# 
# newPECI$PECI_cover= sd(PECI$PECI_cover)*newPECI$PECI_coverZ+mean(PECI$PECI_cover)
# 
# E5.test = resid(M.gls5.test)
# PECI = PECI %>% mutate(E5.test = E5.test,
#                        F5.test = fitted(M.gls5.test))
# 
# #plot the data with confidence intervals
# ggplot(PECI, aes(x = PECI_cover, y = shannon, color = fSite) ) +
#   geom_point(size=4) +
#   geom_ribbon(data = newPECI, 
#               aes(y = NULL, ymin = lower, ymax = upper, 
#                   color = NULL, fill = fSite),alpha = .2) +
#   geom_line(aes(y=F5.test))+theme_bw()
# 
# #this plot looks very different than the first which I think means that the 
# #variance structure WAS taken into account to create the first var cov matrix









#############################################################################
#Try the same for richness#
#############################################################################




PECI<- read_csv("Biodiversity.csv")



# m0 = lm(richness ~PECI_cover*site, data=PECI)
# summary(m0)
# plot(m0)
# drop1(m0) # Its better to drop it

# Try quadratic
# m1 = lm(richness ~ poly(PECI_cover, 2)*site, data=PECI)
# summary(m1)
# plot(m1)
# drop1(m1) # Its better to drop it

# m2 = lm(richness ~PECI_cover, data=PECI)
# summary(m2)
# plot(m2)
# drop1(m2) #Its better to keep it
# 
# AICctab(m0, m1, m2, weights=T, base=T)
# anova(m0,m2)
# The linear model (m2) with no interaction is the best

# Calculate fitteds & resids for the model with no interaction
# F2 = fitted(m2)
# E2 = resid(m2)
# 
# 
# PECI = PECI %>% mutate(F2 = F2, E2 = E2)
# 
# ggplot(PECI, aes(x=PECI_cover)) +
#   geom_point(aes(y=richness), size=5) +
#   geom_point(aes(y=F2), size=5, shape=2) +
#   geom_line(aes(y=F2)) +
#   ggtitle("Richness: 
#           Observed (circles) & Expected (m2, triangles)")
# 
# ggplot(PECI, aes(x=F2, y=E2)) +
#   geom_point(size=5) +
#   geom_smooth(se=F) +
#   ggtitle("Residuals vs. Expected (m2)")
# 
# 
# # Calculate fitteds & resids for the model with an interaction
# F0 = fitted(m0)
# E0 = resid(m0)
# 
# PECI = PECI %>% mutate(F0 = F0, E0 = E0)
# 
# ggplot(PECI, aes(x=PECI_cover, color=site)) +
#   geom_point(aes(y=richness), size=5) +
#   geom_point(aes(y=F0), size=5, shape=2) +
#   geom_line(aes(y=F0)) +
#   ggtitle("Richness: 
#           Observed (circles) & Expected (m0, triangles)")
# 
# ggplot(PECI, aes(x=F0, y=E0, color=site)) +
#   geom_point(size=5) +
#   geom_smooth(se=F) +
#   ggtitle("Residuals vs. Expected (m0)")



###There doesnt seem to an issue with heterogeneity in the model without an interaction
###but there is a violation in the model with an interaction
###so i will try dealing with that to see if it makes a better model overall


# Fix some variables
PECI = PECI %>% mutate(fSite = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

# We need a variance covariate! Complement of Peci_cover?

# nlme has various variance structures that we can use
#  for comparison purposes we have to redo m0 using gls
m0.lm <- gls(richness ~PECI_cover*site, data=PECI)
summary(m0.lm) # Correlations between coefficients are a little high
PECI = PECI %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))
vf1Fixed <- varFixed(~CompPCov)
M.gls1 <- gls(richness ~PECI_cover*site, 
              weights = vf1Fixed, data=PECI)
anova(m0.lm, M.gls1)

# Use transformed x variables
m1.lm <- gls(richness ~PECI_coverZ*fSite, data=PECI)
summary(m1.lm) # Correlations among coefs are lower

vf1Fixed <- varFixed(~CompPCovZ)
M1.gls1 <- gls(richness ~PECI_coverZ*fSite, 
               weights = vf1Fixed, data=PECI)
summary(M1.gls1)
anova(m1.lm, M1.gls1)

#  m1.lm better than M.gls1. Let's check AICc.
AICtab(m1.lm, M1.gls1, base=T, weights=T)
# AIC agrees that m1.lm is the better model
# We'll use the transformed predictors to avoid high coef correlations

# Need to account for difference in variance between sites
vf2 <- varIdent(form= ~ 1 | fSite)
M.gls2 <- gls(richness ~PECI_coverZ*fSite,
              weights = vf2, data=PECI)
anova(m1.lm, M.gls2, M1.gls1)

# M.gls2 is doing significantly better than the others
# We need to combine these two variance structures!

# First, let's look at the way the errors are distributed in m1.lm
# E <- resid(m1.lm)
# coplot(E ~ CompPCovZ | fSite, data = PECI)
# 
# E1 = resid(M1.gls1)
# coplot(E1~ CompPCovZ | fSite, data = PECI)
# 
# E2 = resid(M.gls2)
# coplot(E2 ~ CompPCovZ | fSite, data = PECI)

# These errors only only slightly better so far.

# Let's try other variance structures
vf3 <- varPower(form =~ CompPCovZ)
M.gls3 <- gls(richness ~PECI_coverZ*fSite,
              weights = vf3, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3)
# M.gls3 is worse than m.gls2, about the same as M1.gls1

vf4 <- varPower(form =~ CompPCovZ | fSite)
M.gls4 <- gls(richness ~PECI_coverZ*fSite,
              weights = vf4, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4)
# M.gls4 is better than all except M.gls2
summary(M.gls4)
# You can see the power parameter estimates are different for each site.
#   LOLA has the higher one!

vf5 <- varExp(form =~ CompPCovZ)
M.gls5 <- gls(richness ~PECI_coverZ*fSite,
              weights = vf5, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5)
# M.gls5 is the best so far!

vf5.1 <- varExp(form =~ CompPCovZ|fSite)
M.gls5.1 <- gls(richness ~PECI_coverZ*fSite,
                weights = vf5.1, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1)
# M.gls5 is the best so far!

vf6 <- varConstPower(form =~ CompPCovZ)
M.gls6 <- gls(richness ~PECI_coverZ*fSite,
              weights = vf6, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6)
# M.gls5 is still the best!

vf7 <- varConstPower(form = ~CompPCovZ|fSite)
M.gls7 <- gls(richness ~PECI_coverZ*fSite,
              weights = vf7, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6, M.gls7)

# So far the VarExp structure given is the best (vf5)
# Let's combine it with the vaeident structure using varComb

vf8 <- varComb(varIdent(form =~ 1 | fSite),
               varExp(form =~ CompPCovZ))
M.gls8 <- gls(richness ~ PECI_coverZ*fSite,
              weights = vf8, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6, M.gls7, M.gls8)
# M.gls8 is the best so far!

vf9 <- varComb(varIdent(form =~ 1 | fSite),
               varExp(form =~ CompPCovZ|fSite) )
M.gls9 <- gls(richness ~ PECI_coverZ*fSite,
              weights = vf9, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6, M.gls7, M.gls8, M.gls9)
# M.gls8 is still the best, but M.gls5.1 has lowest AIC!
anova(M.gls5.1,M.gls8)

# Compare models with AICc
AICtab(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
       M.gls6, M.gls7, M.gls8, M.gls9, base=T, weights=T)


# Make coplot for best 4 models to see how we did
E8 = resid(M.gls8, type="normalized")
coplot(E8 ~ CompPCovZ | fSite, data = PECI)

E9 = resid(M.gls9, type="normalized")
coplot(E9 ~ CompPCovZ | fSite, data = PECI)

E5 = resid(M.gls5, type="normalized")
coplot(E5 ~ CompPCovZ | fSite, data = PECI)

E2 = resid(M.gls2, type="normalized")
coplot(E2 ~ CompPCovZ | fSite, data = PECI)

#8 and 9 look much better than 5 and 2


#lets also redo m2 using gls so we can compare

m2.lm <- gls(richness ~PECI_cover, data=PECI)

# Compare models with AICc
AICtab(m2.lm, M.gls8, M.gls9, base=T, weights=T)


#I believe the best model is M.gls8

summary(M.gls8)

#now lets see what happens if we use PECI cover instead of the compliment

vf8b <- varComb(varIdent(form =~ 1 | fSite),
                varExp(form =~ PECI_coverZ))
M.gls8b <- gls(richness ~ PECI_coverZ*fSite,
               weights = vf8b, data=PECI)

#yes they are still the same

# Plot the model
E8b = resid(M.gls8b, type="normalized")
PECI = PECI %>% mutate(E8b = E8b,
                       F8b = fitted(M.gls8b))
ggplot(PECI, aes(x=F8b, y=E8b, color=fSite)) +
  geom_point(size=5) + geom_smooth(se=F) +
  ggtitle("Residuals vs. Fitteds for Model M.gls8")

ggplot(PECI, aes(x=PECI_cover, color=site)) +
  geom_point(aes(y=richness), size=5) +
  geom_point(aes(y=F8b), size=5, shape=2) +
  geom_line(aes(y=F8b)) +
  ggtitle("Richness: 
          Observed (circles) & Expected (M.gls8, triangles)")

summary(M.gls8b)

#final model check
E <- resid(M.gls8b, type = "normalized")
Fit <- fitted(M.gls8b)
op <- par(mfrow = c(1, 2))
plot(x = Fit, y = E, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals versus fitted values")
#identify(Fit, E) this is making R want to crash
hist(E, nclass = 3)
par(op)

#looks good to me

#now lets plot confidence intervals for richness

head( seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01) )

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(M.gls8b, newdata = newPECI)


V <- vcov(M.gls8b)
X <- model.matrix(~ PECI_coverZ*fSite,
                  weights = vf8b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 2*sqrt(se.fit) )
newPECI$upper = with(newPECI, fit + 2*sqrt(se.fit) )

#back transform peci cover

mean(PECI$PECI_cover)
sd(PECI$PECI_cover)

newPECI$PECI_cover= sd(PECI$PECI_cover)*newPECI$PECI_coverZ+mean(PECI$PECI_cover)


#plot the data with confidence intervals
pRichness<-ggplot(PECI, aes(x = PECI_cover, y = richness, color = fSite) ) +
  geom_point(size=4) +
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F8b))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Richness")+
  theme(legend.position="none")









#############################################################
#Now for evenness#
#############################################################





PECI<- read_csv("Biodiversity.csv")

## You can't have a random effect with only 2 groups!
# m0 = lm(evenness ~PECI_cover*site, data=PECI)
# summary(m0)
# plot(m0)
# drop1(m0) # Def keep cause -68 is lower than -54
# 
# # Try quadratic
# m1 = lm(evenness ~ poly(PECI_cover, 2)*site, data=PECI)
# summary(m1)
# plot(m1)
# drop1(m1) # Def keep cause -64 is lower than -52
# 
# m2 = lm(evenness ~PECI_cover, data=PECI)
# summary(m2)
# plot(m2)
# drop1(m2) #Def keep cause -55 is lower than -48
# 
# AICctab(m0, m1, m2, weights=T, base=T)
# anova(m0,m2)
# # The best model is m0 by far
# 
# # Calculate fitteds & resids for the model with no interaction
# F0 = fitted(m0)
# E0 = resid(m0)
# 
# 
# PECI = PECI %>% mutate(F0 = F0, E0 = E0)
# 
# ggplot(PECI, aes(x=PECI_cover, color=site)) +
#   geom_point(aes(y=evenness), size=5) +
#   geom_point(aes(y=F0), size=5, shape=2) +
#   geom_line(aes(y=F0)) +
#   ggtitle("Evenness: 
#           Observed (circles) & Expected (m0, triangles)")
# 
# 
# ggplot(PECI, aes(x=F0, y=E0, color=site)) +
#   geom_point(size=5) +
#   geom_smooth(se=F) +
#   ggtitle("Residuals vs. Expected (m0)")
# 
# 
# # Calculate fitteds & resids for the model with an interaction
# F0 = fitted(m0)
# E0 = resid(m0)
# 
# 
# PECI = PECI %>% mutate(F0 = F0, E0 = E0)
# 
# ggplot(PECI, aes(x=PECI_cover, color=site)) +
#   geom_point(aes(y=evenness), size=5) +
#   geom_point(aes(y=F0), size=5, shape=2) +
#   geom_line(aes(y=F0)) +
#   ggtitle("Evenness: 
#           Observed (circles) & Expected (m0, triangles)")
# 
# ggplot(PECI, aes(x=F0, y=E0, color=site)) +
#   geom_point(size=5) +
#   geom_smooth(se=F) +
#   ggtitle("Residuals vs. Expected (m0)")




###There is def a difference in variation between the two sites
###Something very strange happening with RR, so probably needs to be delt with
###LOLA mostly looks okay.




# Fix some variables
PECI = PECI %>% mutate(fSite = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

# We need a variance covariate! Complement of Peci_cover?

# nlme has various variance structures that we can use
#  for comparison purposes we have to redo m0 using gls
m0.lm <- gls(evenness ~PECI_cover*site, data=PECI)
summary(m0.lm) # Correlations between coefficients are a little high
PECI = PECI %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))
vf1Fixed <- varFixed(~CompPCov)
M.gls1 <- gls(evenness ~PECI_cover*site, 
              weights = vf1Fixed, data=PECI)
anova(m0.lm, M.gls1)

# Use transformed x variables
m1.lm <- gls(evenness ~PECI_coverZ*fSite, data=PECI)
summary(m1.lm) # Correlations among coefs are lower

vf1Fixed <- varFixed(~CompPCovZ)
M1.gls1 <- gls(evenness ~PECI_coverZ*fSite, 
               weights = vf1Fixed, data=PECI)
summary(M1.gls1)
anova(m1.lm, M1.gls1)

# M.gls1 is better than m1.lm . Let's check AICc.
AICtab(m1.lm, M1.gls1, base=T, weights=T)
# AIC agrees that M.gls1 is the better model
# We'll use the transformed predictors to avoid high coef correlations

# Need to account for difference in variance between sites
vf2 <- varIdent(form= ~ 1 | fSite)
M.gls2 <- gls(evenness ~PECI_coverZ*fSite,
              weights = vf2, data=PECI)
anova(m1.lm, M.gls2, M1.gls1)

# M.gls2 is doing significantly better than the others
# We need to combine these two variance structures!

# First, let's look at the way the errors are distributed in m1.lm
E <- resid(m1.lm, type="normalized")
coplot(E ~ CompPCovZ | fSite, data = PECI)

E1 = resid(M1.gls1, type="normalized")
coplot(E1~ CompPCovZ | fSite, data = PECI)

E2 = resid(M.gls2, type="normalized")
coplot(E2 ~ CompPCovZ | fSite, data = PECI)

# I think M.gls2 is doing very well.

# Let's try other variance structures
vf3 <- varPower(form =~ CompPCovZ)
M.gls3 <- gls(evenness ~PECI_coverZ*fSite,
              weights = vf3, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3)
# still not as good as 2

vf4 <- varPower(form =~ CompPCovZ | fSite)
M.gls4 <- gls(evenness ~PECI_coverZ*fSite,
              weights = vf4, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4)
# still not as good as 2


vf5 <- varExp(form =~ CompPCovZ)
M.gls5 <- gls(evenness ~PECI_coverZ*fSite,
              weights = vf5, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5)
# M.gls5 is the best so far!

vf5.1 <- varExp(form =~ CompPCovZ|fSite)
M.gls5.1 <- gls(evenness ~PECI_coverZ*fSite,
                weights = vf5.1, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1)
# M.gls5.1 is the best so far!

vf6 <- varConstPower(form =~ CompPCovZ)
M.gls6 <- gls(evenness ~PECI_coverZ*fSite,
              weights = vf6, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6)
# M.gls5.1 is still the best!

vf7 <- varConstPower(form = ~CompPCovZ|fSite)
M.gls7 <- gls(evenness ~PECI_coverZ*fSite,
              weights = vf7, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6, M.gls7)

# So far the VarExp structure given is the best (vf5)
# Let's combine it with the vaeident structure using varComb

vf8 <- varComb(varIdent(form =~ 1 | fSite),
               varExp(form =~ CompPCovZ|fSite))
M.gls8 <- gls(evenness ~ PECI_coverZ*fSite,
              weights = vf8, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6, M.gls7, M.gls8)
# 5.1 is the best so far!

vf9 <- varComb(varIdent(form =~ 1 | fSite),
               varExp(form =~ CompPCovZ) )
M.gls9 <- gls(evenness ~ PECI_coverZ*fSite,
              weights = vf9, data=PECI)
anova(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
      M.gls6, M.gls7, M.gls8, M.gls9)
# M.gls5.1 is the best- it has the lowest AIC

# Compare models with AICc
AICtab(m1.lm, M1.gls1, M.gls2, M.gls3, M.gls4, M.gls5, M.gls5.1,
       M.gls6, M.gls7, M.gls8, M.gls9, base=T, weights=T)


# Make coplot for best 4 models to see how we did
E5.1 = resid(M.gls5.1, type="normalized")
coplot(E5.1 ~ CompPCovZ | fSite, data = PECI)

E5 = resid(M.gls5, type="normalized")
coplot(E5 ~ CompPCovZ | fSite, data = PECI)

E9 = resid(M.gls9, type="normalized")
coplot(E9 ~ CompPCovZ | fSite, data = PECI)

E8 = resid(M.gls8, type="normalized")
coplot(E8 ~ CompPCovZ | fSite, data = PECI)

#they all look okay


#lets also redo m0 using gls so we can compare

m2.lm <- gls(evenness ~PECI_cover, data=PECI)

# Compare models with AICc
AICtab(M.gls5.1b, m2.lm, base=T, weights=T)

AICtab(m0.lm, M.gls8, M.gls5, M.gls5.1, M.gls9, base=T, weights=T)
#I believe the best model is M.gls5.1

summary(M.gls5.1)

#now lets see what happens if we use PECI cover instead of the compliment

vf5.1b <- varComb(varIdent(form =~ 1 | fSite),
                  varExp(form =~ PECI_coverZ))
M.gls5.1b <- gls(evenness ~ PECI_coverZ*fSite,
                 weights = vf5.1b, data=PECI)


summary(M.gls5.1b)



# Plot the model
E5.1b = resid(M.gls5.1b, type="normalized")
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(M.gls5.1b))
ggplot(PECI, aes(x=F5.1b, y=E5.1b, color=fSite)) +
  geom_point(size=5) + geom_smooth(se=F) +
  ggtitle("Residuals vs. Fitteds for Model M.gls5.1b")

ggplot(PECI, aes(x=PECI_cover, color=site)) +
  geom_point(aes(y=evenness), size=5) +
  geom_point(aes(y=F5.1b), size=5, shape=2) +
  geom_line(aes(y=F5.1b)) +
  ggtitle("Evenness: 
          Observed (circles) & Expected (M.gls5.1b, triangles)")

summary(M.gls5.1b)

#final model check
E <- resid(M.gls5.1b, type = "normalized")
Fit <- fitted(M.gls5.1b)
op <- par(mfrow = c(1, 2))
plot(x = Fit, y = E, 
     xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals versus fitted values")
#identify(Fit, E) this is making R want to crash
hist(E, nclass = 3)
par(op)


#now lets plot confidence intervals for richness

head( seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01) )

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(M.gls5.1b, newdata = newPECI)


V <- vcov(M.gls5.1b)
X <- model.matrix(~ PECI_coverZ*fSite,
                  weights = vf5.1b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )

#back transform peci cover

mean(PECI$PECI_cover)
sd(PECI$PECI_cover)

newPECI$PECI_cover= sd(PECI$PECI_cover)*newPECI$PECI_coverZ+mean(PECI$PECI_cover)


#plot the data with confidence intervals
pEvenness<-ggplot(PECI, aes(x = PECI_cover, y = evenness, color = site) ) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F5.1b))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Evenness")+
  theme(legend.position= "none")


#Make the figure!#
require(cowplot)
cowplot::plot_grid(pRichness, pEvenness, pShannon, labels = "AUTO", ncol=3)




# #############
# #what about overall cover and diversity?
# ###########################################
# 
# ggplot(PECI, aes(y=PECI_cover, x=ALL_cover, color=site))+
#   geom_point(size=4)+
#   geom_smooth(method=lm, se=FALSE)+
#   theme_bw()
# 
# 
# #stop here
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# #pre Jack work
# 
# 
# #linear model
# shannonmodel<-lm(shannon~PECI_cover, data=PECI)
# summary(shannonmodel)
# plot(shannonmodel)
# 
# #linear mixed model
# library(lme4)
# library(nlme)
# library(MASS)
# library(car)
# library(ggplot2)
# library(pbkrtest)
# library(RLRsim)
# 
# hist(PECI$shannon)
# #pretty normal
# help(hist)
# 
# hist(PECI$simpson)
# #kinda left skewed
# 
# #make a mixed model using lme
# shannonmixm <- lme(shannon ~PECI_cover,
#                    random = ~1|site, data=PECI)
# summary(shannonmixm)
# plot(shannonmixm)
# 
# 
# 
# #make the same mixed model using lmer 
# shannonmixm2 <- lmer(shannon ~PECI_cover +(1|site), PECI)
# summary(shannonmixm2)
# plot(shannonmixm)
# 
# deviance(shannonmixm2, REML=FALSE)
# REMLcrit(shannonmixm2)
# 
# #create diagnostic plots for the lmer model
# plot(shannonmixm2, type = c("p", "smooth"))
# plot(shannonmixm2, sqrt(abs(resid(.))) ~ fitted(.),
#      type = c("p", "smooth"))
# qqmath(shannonmixm2, id = 0.05)
# 
# shannon.res = resid(shannonmixm2)
# plot(PECI$PECI_cover, shannon.res)
# abline(0, 0) 
# 
# hist(shannon.res)
# 
# iqrvec <- sapply(simulate(shannonmixm2, 1000), IQR)
# obsval <- IQR(PECI$shannon)
# post.pred.p <- mean(obsval >= c(obsval, iqrvec))
# 
# 
# #plot the mixed effects model
# 
# #add model fits and CI to dataframe
# library(merTools)
# 
# #PECI_new <- predictInterval(shannonmixm2)
# #PECI_new$fit2<- predict(shannonmixm2)
# 
# PECIshan <- cbind(PECI, predictInterval(shannonmixm2, PECI))
# 
# confint(shannonmixm2, character, level = 0.95,
#         method = c("profile", "Wald", "boot"), zeta,
#         nsim = 500,
#         boot.type = c("perc","basic","norm"),
#         FUN = NULL, quiet = FALSE,
#         oldNames = TRUE)
# 
# #add CI to to dataframe
# 
# 
# 
# ggplot(PECIshan) + 
#   geom_line(aes(PECI_cover, fit, color = site)) +
#   geom_ribbon(aes(PECI_cover, ymin = lwr, ymax = upr, fill = site), alpha = .2) +
#   geom_point(aes(PECI_cover, y = shannon, color = site, size=1))+
#   ylab("shannon diversity")+ xlab("Percent Buffelgrass Cover")
# 
# 
# 
# #Now look at simpson
# 
# 
# simpsonmodel<-lm(simpson~PECI_cover, data=PECI)
# summary(simpsonmodel)
# plot(simpsonmodel)
# 
# 
# #make a mixed model using lme
# simpsonmixm <- lme(simpson ~PECI_cover,
#                    random = ~1|site, data=PECI)
# summary(simpsonmixm)
# plot(simpsonmixm)
# 
# 
# 
# #make the same mixed model using lmer 
# simpsonmixm2 <- lmer(simpson ~PECI_cover +(1|site), PECI)
# summary(simpsonmixm2)
# plot(simpsonmixm2)
# 
# deviance(simpsonmixm2, REML=FALSE)
# REMLcrit(simpsonmixm2)
# 
# #create diagnostic plots for the lmer model
# plot(simpsonmixm2, type = c("p", "smooth"))
# plot(simpsonmixm2, sqrt(abs(resid(.))) ~ fitted(.),
#      type = c("p", "smooth"))
# qqmath(simpsonmixm2, id = 0.05)
# 
# simpson.res = resid(simpsonmixm2)
# plot(PECI$PECI_cover, simpson.res)
# abline(0, 0) 
# hist(simpson.res)
# 
# 
# iqrvec <- sapply(simulate(simpsonmixm2, 1000), IQR)
# obsval <- IQR(PECI$simpson)
# post.pred.p <- mean(obsval >= c(obsval, iqrvec))
# 
# #try it as a glmm and compare two different distributions
# simpsonmixm3 <- glmer(simpson ~PECI_cover +(1|site), family=Gamma, PECI)
# summary(simpsonmixm3)
# plot(simpsonmixm3)
# 
# simpsonmixm4 <- glmer(simpson ~PECI_cover +(1|site), family=gaussian, PECI)
# summary(simpsonmixm4)
# plot(simpsonmixm4)
# 
# AIC(simpsonmixm3,simpsonmixm4)
# 
# #plot the mixed effects model
# 
# #add model fits and CI to dataframe
# library(merTools)
# 
# #PECI_new <- predictInterval(simpsonmixm2)
# #PECI_new$fit2<- predict(simpsonmixm2)
# 
# PECIsimp <- cbind(PECI, predictInterval(simpsonmixm2, PECI))
# 
# confint(shannonmixm2, character, level = 0.95,
#         method = c("profile", "Wald", "boot"), zeta,
#         nsim = 500,
#         boot.type = c("perc","basic","norm"),
#         FUN = NULL, quiet = FALSE,
#         oldNames = TRUE)
# 
# #add CI to to dataframe
# 
# 
# 
# ggplot(PECIsimp) + 
#   geom_line(aes(PECI_cover, fit, color = site)) +
#   geom_ribbon(aes(PECI_cover, ymin = lwr, ymax = upr, fill = site), alpha = .2) +
#   geom_point(aes(PECI_cover, y = simpson, color = site, size=1))+
#   ylab("simpson diversity")+ xlab("Percent Buffelgrass Cover")
# 
# 
# 
# cor(PECI$simpson, PECI$shannon,  method = "pearson", use = "complete.obs")
# 
# 
# 
# 
# #ggplot of cover by diversity
# ggplot(PECI, aes(x=PECI_cover, y=simpson, color=site, size=1)) +
#   geom_point()+
#   theme_bw()
# 
# 
# # ggplot of a nmds
# comm <- decostand(div_wide, method = "total")
# comm.k.mds <- metaMDS(comm, distance = "kul", trace = 0)
# scores <- as.data.frame(scores(comm.k.mds))
# scores$site <- rownames(scores)
# scores$block <- rownames(scores) %>%str_sub(1,2)
# 
# ggplot(data=scores) +
#   stat_ellipse(aes(x=NMDS1, y=NMDS2, color = block),type = "t", level = 0.95) +
#   geom_point(aes(x=NMDS1, y=NMDS2, color=block),size= 4) +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
#   coord_fixed() +
#   theme(legend.justification=c(0,0), legend.position=c(0,0),
#         legend.background = element_rect(fill = 'transparent'))
