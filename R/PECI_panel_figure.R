# making all the x ~ PECI_cover  figures in the paper
source("R/data_prep.R")

library(performance)
library(nlme)
library(cowplot)
library(broom)
library(mgcv)
library(broom.mixed)
library(ggeffects)
library(ggtext)

# herbaceous carbon ============================================================

mod_herbc <- lm(log(herb_C+1) ~PECI_cv*site, data=d)


newPECI = expand.grid(PECI_cv = seq(min(d$PECI_cv), max(d$PECI_cv), by = .01),
                      site = unique(d$site) )

fit = predict(mod_herbc, newdata = newPECI)

V <- vcov(mod_herbc)
X <- model.matrix(~ PECI_cv*site, data=newPECI)

se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )
newPECI$fit   = with(newPECI, fit)

# Figure herb
p_herb_c <- ggplot(d, aes(x = PECI_cv, y = herb_C, color=site)) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = exp(lower)-1, ymax = exp(upper)-1, 
                  color = NULL, fill=site), alpha = .15) +
  geom_line(data=newPECI,
            aes(y=exp(fit)-1))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Herbaceous Carbon g/m2")+
  guides(fill="none")+
  theme(
    # legend.position = c(.01, .99),
    legend.position = "none",
    legend.justification = c("left", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4))




# diversity models =============================================================

###
#SHANNON DIVERSITY
###

#get dataset ready

PECI = d %>% mutate(CompPCov = 100-PECI_cv,
                    CompPCovZ = (CompPCov-mean(CompPCov))/sd(CompPCov),
                    fSite = factor(site),
                    PECI_coverZ = (PECI_cv-mean(PECI_cv))/sd(PECI_cv))

#make the model
vf5.1b <- varExp(form =~ PECI_coverZ|fSite)
mod_shannon <- gls(shannon ~PECI_coverZ*fSite,
                 weights = vf5.1b, data=PECI)

# ggeffects::ggpredict(lm(shannon~PECI_coverZ*fSite + slope_aspect, data=PECI)) %>% plot(facets=TRUE)

# Plot the model
E5.1b = resid(mod_shannon)
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(mod_shannon))

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), 
                                        max(PECI$PECI_coverZ), by = .01),
                      # slope_aspect = seq(min(PECI$slope_aspect), 
                      #                   max(PECI$slope_aspect), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(mod_shannon, newdata = newPECI)
V <- vcov(mod_shannon)
X <- model.matrix(~PECI_coverZ*fSite,
                  weights = vf5.1b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 2*sqrt(se.fit) )
newPECI$upper = with(newPECI, fit + 2*sqrt(se.fit) )

newPECI$PECI_cv= sd(PECI$PECI_cv)*newPECI$PECI_coverZ+mean(PECI$PECI_cv)

#plot the data with confidence intervals
pShannon<-ggplot(PECI, aes(x = PECI_cv, y = shannon, color = site) ) +
  geom_point(size=4) +
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F5.1b))+
  theme_bw()+
  xlab("Buffelgrass Cover (%)")+ 
  ylab("Shannon Diversity")+
  guides(fill="none")+
  theme(
    legend.position = c(.99, .99),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4))


#RICHNESS ======================================================================

#make the model
vf8b <- varComb(varIdent(form =~ 1 | fSite),
                varExp(form =~ PECI_coverZ))
mod_rich <- gls(richness ~ PECI_coverZ*fSite,
               weights = vf8b, data=PECI)
# Plot the model
E8b = resid(mod_rich, type="normalized")
PECI = PECI %>% mutate(E8b = E8b,
                       F8b = fitted(mod_rich))

#now lets plot confidence intervals for richness

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(mod_rich, newdata = newPECI)


V <- vcov(mod_rich)
X <- model.matrix(~ PECI_coverZ*fSite,
                  weights = vf8b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 2*sqrt(se.fit) )
newPECI$upper = with(newPECI, fit + 2*sqrt(se.fit) )

newPECI$PECI_cv= sd(PECI$PECI_cv)*newPECI$PECI_coverZ+mean(PECI$PECI_cv)


#plot the data with confidence intervals
pRichness<-ggplot(PECI, aes(x = PECI_cv, y = richness, color = fSite) ) +
  geom_point(size=4) +
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F8b))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Richness")+
  theme(legend.position="none")


#Evenness ======================================================================

#make the model
vf5.1b <- varComb(varIdent(form =~ 1 | fSite),
                  varExp(form =~ PECI_coverZ))
mod_even <- gls(evenness ~ PECI_coverZ*fSite,
                 weights = vf5.1b, data=PECI)

#plot the model
E5.1b = resid(mod_even, type="normalized")
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(mod_even))

# head( seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01) )

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(mod_even, newdata = newPECI)


V <- vcov(mod_even)
X <- model.matrix(~ PECI_coverZ*fSite,
                  weights = vf5.1b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )

#back transform peci cover

newPECI$PECI_cv= sd(PECI$PECI_cv)*newPECI$PECI_coverZ+mean(PECI$PECI_cv)


#plot the data with confidence intervals
pEvenness<-ggplot(PECI, aes(x = PECI_cv, y = evenness, color = site) ) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F5.1b))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Evenness")+
  theme(legend.position= "none")


# ecosystem weighted C ===================

#make the model
vf5.1b <- varComb(varIdent(form =~ 1 | fSite),
                  varExp(form =~ PECI_coverZ))
mod_eco_c <- gls(ecosystem_gC_m2 ~ PECI_coverZ*fSite,
                weights = vf5.1b, data=PECI)

#plot the model
E5.1b = resid(mod_eco_c, type="normalized")
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(mod_eco_c))

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(mod_eco_c, newdata = newPECI)


V <- vcov(mod_eco_c)
X <- model.matrix(~ PECI_coverZ*fSite,
                  weights = vf5.1b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )

#back transform peci cover

newPECI$PECI_cv= sd(PECI$PECI_cv)*newPECI$PECI_coverZ+mean(PECI$PECI_cv)


#plot the data with confidence intervals
p_eco_c <-ggplot(PECI, aes(x = PECI_cv, y = ecosystem_gC_m2, color = site) ) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F5.1b))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Ecosystem C (g/m2)")+
  theme(legend.position= "none")

# soil total C ===================

#make the model
vf5.1b <- varComb(varIdent(form =~ 1 | fSite),
                  varExp(form =~ PECI_coverZ))
mod_soil_c <- gls(soil_TC ~ PECI_coverZ*fSite,
                 weights = vf5.1b, data=PECI)

#plot the model
E5.1b = resid(mod_soil_c, type="normalized")
PECI = PECI %>% mutate(E5.1b = E5.1b,
                       F5.1b = fitted(mod_soil_c))

newPECI = expand.grid(PECI_coverZ = seq(min(PECI$PECI_coverZ), max(PECI$PECI_coverZ), by = .01),
                      fSite = unique(PECI$fSite) )

fit = predict(mod_soil_c, newdata = newPECI)


V <- vcov(mod_soil_c)
X <- model.matrix(~ PECI_coverZ*fSite,
                  weights = vf5.1b, data=newPECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))

newPECI$lower = with(newPECI, fit - 1.96*(se.fit) )
newPECI$upper = with(newPECI, fit + 1.96*(se.fit) )

#back transform peci cover

newPECI$PECI_cv= sd(PECI$PECI_cv)*newPECI$PECI_coverZ+mean(PECI$PECI_cv)


#plot the data with confidence intervals
p_soil_c <-ggplot(PECI, aes(x = PECI_cv, y = soil_TC, color = site) ) +
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_ribbon(data = newPECI, 
              aes(y = NULL, ymin = lower, ymax = upper, 
                  color = NULL, fill = fSite),alpha = .2) +
  geom_line(aes(y=F5.1b))+theme_bw()+
  xlab("Buffelgrass Cover (%)")+ ylab("Soil Total C (g/m2)")+
  theme(legend.position= "none")
# woody c ======================================================================

vf1Fixed <- varFixed(~CompPCov)
mod_woody_C <- gls(woody_C ~poly(PECI_coverZ, 2)*site, 
                 weights = vf1Fixed, 
                 data=PECI)

fit <- predict(mod_woody_C)

V <- vcov(mod_woody_C)
X <- model.matrix(~poly(PECI_coverZ, 2)*site, 
                  weights = vf1Fixed, data=PECI)
se.fit <- sqrt(diag(X %*% V %*% t(X)))
predframe <- with(PECI, data.frame(PECI_coverZ, site,PECI_cv,
                                        woody_C=fit,lwr=fit-1.96*se.fit,upr=fit+1.96*se.fit))


#plot the model with confidence intervals


p_woody_c<-ggplot(PECI, aes(PECI_cv, woody_C, color = site))+
  geom_point(size=4)+
  scale_color_manual(values=c("#972D15","#81A88D"))+
  geom_line(data=predframe, size=1)+
  geom_smooth(method="lm",se=F, lty=2, data =PECI %>% filter(PECI_cv < 40) )+
  geom_ribbon(data=predframe,
              aes(ymin=lwr,ymax=upr, fill = site, color = NULL),alpha=0.2)+
  xlab("Buffelgrass Cover (%)")+ ylab("Woody Carbon (g/m2)")+
  theme_bw()+
  ylim(NA,250)+
  theme(legend.position = "none",
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(4, 4, 4, 4));p_woody_c

# p_woody_c<-ggplot(PECI %>% filter(PECI_cv < 40), aes(PECI_cv, woody_C, color = site))+
#   geom_point(size=4)+
#   scale_color_manual(values=c("#972D15","#81A88D"))+
#   xlab("Buffelgrass Cover (%)")+ ylab("Woody Carbon (g/m2)")+
#   theme_bw()+
#   theme(legend.position = "none",
#         legend.justification = c("right", "top"),
#         legend.box.just = "right",
#         legend.margin = margin(4, 4, 4, 4));p_woody_c

# Make the figures! =============================================================


ggsave(plot = cowplot::plot_grid(pRichness, pEvenness, pShannon,
                                 labels = "AUTO", ncol=3, nrow=1),
       filename = "figs/multipanel_peci_cv_div.png", height = 4, width=11,
       bg="white")

ggsave(plot = cowplot::plot_grid(p_herb_c,  p_woody_c,p_soil_c, p_eco_c,
                                 labels = "AUTO", ncol=4, nrow=1),
       filename = "figs/multipanel_peci_cv_ecostuff.png", height = 4, width=13,
       bg="white")
# table
bind_rows(
broom::tidy(mod_even) %>% mutate(response = "evennness"),
broom::tidy(mod_rich) %>% mutate(response = "richness"),
broom::tidy(mod_shannon) %>% mutate(response = "shannon"),
broom::tidy(mod_herbc) %>% mutate(response = "log(herbaceous_C + 1)"),
broom::tidy(mod_eco_c) %>% mutate(response = "ecosystem_c"),
broom::tidy(mod_soil_c) %>% mutate(response = "soil_c"),
broom::tidy(mod_woody_C) %>% mutate(response = "woody_c")
) %>%
  mutate_if(is.numeric, round, 3) %>%
  write_csv("data/model_coefficients.csv")







