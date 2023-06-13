#Final Diversity Models
#Creating Plots and Finding Pvalues

library(lme4)
library(nlme)
library(MASS)
library(car)
library(ggplot2)
library(tidyverse)
library(bbmle)



###
#SHANNON DIVERSITY
###

#get dataset ready
PECI<- read.csv("~/projects/buffelgrass/data/BuffelgrassAnalysis/Analysis/Diversity_a/Biodiversity.csv")

PECI = PECI %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))
PECI = PECI %>% mutate(fSite = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

#make the model
vf5.1b <- varExp(form =~ PECI_coverZ|fSite)
M.gls5.1b <- gls(shannon ~PECI_coverZ*fSite,
                 weights = vf5.1b, data=PECI)

#find variable significance
summary(M.gls5.1b)


# Plot the model
E5.1b = resid(M.gls5.1b)
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(M.gls5.1b))

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




###
#RICHNESS
###

#get dataset ready
PECI<- read.csv("Biodiversity.csv")

PECI = PECI %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))
PECI = PECI %>% mutate(fSite = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

#make the model
vf8b <- varComb(varIdent(form =~ 1 | fSite),
                varExp(form =~ PECI_coverZ))
M.gls8b <- gls(richness ~ PECI_coverZ*fSite,
               weights = vf8b, data=PECI)


summary(M.gls8b)

# Plot the model
E8b = resid(M.gls8b, type="normalized")
PECI = PECI %>% mutate(E8b = E8b,
                       F8b = fitted(M.gls8b))

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






###
#Evenness
###


#get dataset ready
PECI<- read.csv("Biodiversity.csv")

PECI = PECI %>% mutate(CompPCov = 100-PECI_cover,
                       CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov))
PECI = PECI %>% mutate(fSite = factor(site),
                       PECI_coverZ = (PECI_cover-mean(PECI_cover))/sd(PECI_cover))

#make the model
vf5.1b <- varComb(varIdent(form =~ 1 | fSite),
                  varExp(form =~ PECI_coverZ))
M.gls5.1b <- gls(evenness ~ PECI_coverZ*fSite,
                 weights = vf5.1b, data=PECI)

summary(M.gls5.1b)

#plot the model
E5.1b = resid(M.gls5.1b, type="normalized")
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(M.gls5.1b))

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




###
#Make the figure!#
###


require(cowplot)
cowplot::plot_grid(pRichness, pEvenness, pShannon, labels = "AUTO", ncol=3)


################################################################################


