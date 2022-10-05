#load multiple libraries 
x <- c("tidyverse", "sf", "ggplot2", "doBy", "FSA", "lme4", "rstatix","car", "ggpubr")
lapply(x, library, character.only = TRUE, verbose = FALSE)

soils <- read_csv("data/weighted_C/SoilData.csv")
herb <- read_csv("data/weighted_C/herbdata.csv")
woody <- read_csv("data/weighted_C/AGB_woody_biomass.csv")
cover <- read_csv("data/weighted_C/Cover.csv") %>% 
  rename("other_cover" = "ALL_cover", "buffel_cover" = "PECI_cover")  %>%
  mutate(bare_cover = 100 - (buffel_cover + other_cover))


#combine herb and woody by site_plot
veg <- left_join(woody, herb, by = c("site_plot", "site", "plot", "PECI_cover", "ALL_cover"))

#rename columns in joint dataset, and extra columns
veg <- veg %>% 
  rename("woody_gC_m2" = "TotalC_g_m2", "herb_gC_m2" = "avgbio_C_g_m2") %>%
  select (-totalherb, -PECI_cover, -ALL_cover)

#split into 3 separate datasets for merging with soils
bare_cover <- cover %>%
  select (site_plot, bare_cover)

buffel_cover <- cover %>%
  select (site_plot, buffel_cover)

other_cover <- cover %>%
  select (site_plot, other_cover)


##############################################
#weight total soil C by % cover

#calculate mean total soil C by cover type and site_plot
soils_TC<- soils %>%
  select(cover_type,site_plot,totsoilCcontent_gC_m2_means,site)%>%
  group_by(cover_type, site_plot) %>%
  summarise(meanTC = mean(totsoilCcontent_gC_m2_means, na.rm = TRUE)) %>% # sure we don't want an ungroup after this?
  mutate(site_plot = str_replace(site_plot, "Lola", "LOLA"))


#split soils into 3 datasets
bare_soil <- soils_TC %>%
  filter(cover_type == "bare")

buffel_soil <- soils_TC %>%
  filter(cover_type == "buffel")

other_soil <- soils_TC %>%
  filter(cover_type == "other")




#append total soil C data with cover data
bare_soil_cover <- bare_soil %>%
  left_join(bare_cover) %>%
  mutate(weighted_bare = meanTC * bare_cover/100) %>% 
  ungroup() %>%
  select(-meanTC, -cover_type) 

buffel_soil_cover <- buffel_soil %>%
  left_join(buffel_cover)%>%
  mutate(weighted_buffel = meanTC * buffel_cover/100) %>% 
  ungroup() %>%
  select(-meanTC, -cover_type)

other_soil_cover <- other_soil %>%
  left_join(other_cover) %>%
  mutate(weighted_other = meanTC * other_cover/100) %>% 
  ungroup() %>%
  select(-meanTC, -cover_type)

#join soils across cover types again
soil_cover1 <- bare_soil_cover %>%
  left_join(buffel_soil_cover)

#created total weighted total soil C
soil_cover <- soil_cover1 %>%
  left_join(other_soil_cover) %>%
  replace_na(list(buffel_cover = 0, weighted_buffel = 0)) %>%
  mutate(weighted_TC_gC_m2 = weighted_bare + weighted_buffel + weighted_other)

#add weighted total soil C + woody + herb
ecosystem <- soil_cover %>%
  left_join(veg) %>%
  mutate(ecosystem_gC_m2 = weighted_TC_gC_m2 + woody_gC_m2 + herb_gC_m2)

write_csv(ecosystem, "data/weighted_carbon.csv")


#########################################################
#plot ecosystem carbon as a function of buffelgrass cover
ggplot(ecosystem, aes(x = buffel_cover, y = ecosystem_gC_m2)) +
  geom_point(size = 2) +
  geom_smooth(method=lm)
#increases, because these areas haven't burned????

#with other cover
ggplot(ecosystem, aes(x = other_cover, y = ecosystem_gC_m2)) +
  geom_point(size = 2) +
  geom_smooth(method=lm)
#no idea why this would go down

#with bare cover
ggplot(ecosystem, aes(x = bare_cover, y = ecosystem_gC_m2)) +
  geom_point(size = 2) +
  geom_smooth(method=lm)
#this one makes sense


# do a model
library(lme4)
library(ggResidpanel)
library(ggeffects)

ecosystem_d <- left_join(ecosystem %>%
                           dplyr::select(-site, -plot, -bare_cover),
                         d, by = "site_plot") %>%
  mutate(slope_aspect = fa * slope)



mod_e <- lmer(ecosystem_gC_m2 ~ buffel_cover + other_cover +
                slope_aspect + elevation + (1|site),
            data = ecosystem_d)
summary(mod_e)

ggpredict(mod_e) %>% plot(add.data=T, facets=T)
ggResidpanel::resid_panel(mod_e)

mod_s <- lmer(weighted_TC_gC_m2 ~ buffel_cover + other_cover  +
                slope_aspect + elevation + (1|site),
            data = ecosystem_d)
summary(mod_s)

ggpredict(mod_s) %>% plot(add.data=T, facets=T)
ggResidpanel::resid_panel(mod_s)

# do a simple model

mod_simple <- lm(ecosystem_gC_m2~buffel_cover + slope_aspect + elevation, data = ecosystem_d)
summary(mod_simple)

mod_simple <- lm(weighted_TC_gC_m2~buffel_cover + slope_aspect + elevation, data = ecosystem_d)
summary(mod_simple)

# do a bayesian model
library(brms)

bmod_s <- brms::brm(weighted_TC_gC_m2 ~ 
                      buffel_cover +
                      slope_aspect + 
                      elevation + 
                      (1|site.x),
              data = ecosystem_d)
summary(bmod_s)
conditional_effects(bmod_s,facets=T)



ggpredict(bmod_s) %>% plot(add.data=T, facets=T)
rstantools::posterior_predict(bmod_s)
ggResidpanel::resid_panel(bmod_s)