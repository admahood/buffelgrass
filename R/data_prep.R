# sem script
library(tidyverse)
library(lavaan)
library(lme4)
library(lmerTest)
library(mgcv)
source("R/ggplot_sem.R")

get_folded_aspect<-function(aspect){
  180 - abs(aspect-180)
}

d <- read_csv("data/AGB_woody_biomass.csv") %>%
  left_join(read_csv("data/Biodiversity.csv")) %>%
  left_join(read_csv("data/cover.csv")) %>%
  left_join(read_csv("data/herb_carbon_output.csv")) %>%
  left_join(read_csv("data/site_info.csv")[,1:8], by = c("site"="Site", "plot"="Plot"))  %>%
  dplyr::rename(aspect = `mean aspect`, elevation = `mean elevation (m)`, slope=`mean slope`) %>%
  left_join(read_csv("data/soils_SOC_output.csv")[,2:6]%>% mutate(plot = as.numeric(plot))%>%
              pivot_wider(names_from=cover_type, values_from=meanSOC_gC_m2, names_glue = "{cover_type}_SOC_g_m2")%>%
              mutate(site_plot = str_to_upper(site_plot),site = str_to_upper(site))) %>%
  left_join(read_csv("data/soils_TSC_output.csv")[,2:6]%>% mutate(plot = as.numeric(plot))%>%
              pivot_wider(names_from=cover_type, values_from=meanTSC_g_m2, names_glue = "{cover_type}_TSC_g_m2")%>%
              mutate(site_plot = str_to_upper(site_plot),site = str_to_upper(site))) %>%
  dplyr::select(-ALL_cover, -`Buffelgrass cover (%)`) %>%
  dplyr::rename(other_cover = `Other cover (%)`,
                bare_cover = `Bare ground (%)`,
                woody_c_g_m2 = TotalC_g_m2) %>%
  mutate(total_cover = other_cover + PECI_cover,
         fa = get_folded_aspect(aspect),
         plot = as.factor(plot)) %>%
  dplyr::rename(woody_C = woody_c_g_m2,
                other_cv = other_cover,
                PECI_cv = PECI_cover,
                herb_C = avgherbcarb_g_m2,
                bare_SOC = bare_SOC_g_m2,
                buffel_SOC = buffel_SOC_g_m2,
                other_SOC = other_SOC_g_m2) 

write_csv(d, "data/env_data_cleanish.csv")

d <- d %>% mutate_if(is.numeric, scale)

# univariate models ============================================================
mod_simpson <- lmer(simpson ~ PECI_cv + 
                      herb_C + 
                      fa+(1|site), d, REML=TRUE);summary(mod_simpson);car::Anova(mod_simpson)

# mod_shannon <- lmer(shannon ~ PECI_cv + fa+(1|site), d, REML=F);summary(mod_shannon)
# mod_richness <- glmer(richness ~ PECI_cv+ fa+(1|site), d, family="poisson");summary(mod_richness)
# mod_evenness <- lmer(evenness ~ PECI_cv+ fa+(1|site), d, REML=F);summary(mod_evenness)
mod_tc <- gamm(woody_C ~ s(PECI_cv),data= d);summary(mod_tc$gam);summary(mod_tc$lme)
mod_oc <- lmer(other_cv ~ PECI_cv+fa+(1|site),d);summary(mod_oc)
# mod_herb <- lmer(herb_C ~ PECI_cv + (1|site),d);summary(mod_herb)
mod_herb1 <- glmer(herb_C ~ PECI_cv+ total_cover+simpson + (1|site),d);summary(mod_herb1);car::Anova(mod_herb1)

mod_peci<- lmer(PECI_cv ~herb_C +
                  # woody_C+ 
                  fa+
                  # buffel_SOC+
                  simpson+
                  # richness+
                  # evenness+
                  (1|site), d, REML=T); summary(mod_peci); car::Anova(mod_peci)

# forgot to set REML=F when doing model selection oops
m_oc1 <- lmer(buffel_SOC ~ 
                woody_C+
                other_TSC_g_m2+
                herb_C+
                buffel_TSC_g_m2+
                (1|site),d);summary(m_oc1);car::Anova(m_oc1)
m_oc2 <- lmer(bare_SOC ~  
                woody_C+
                bare_TSC_g_m2+
                PECI_cv+
                richness+
                bare_cover+
                buffel_SOC+
                buffel_TSC_g_m2+
                (1|site),d);summary(m_oc2);car::Anova(m_oc2)
m_oc3 <- lmer(other_SOC ~ 
                other_TSC_g_m2+
                herb_C+(1|site),d);summary(m_oc3) 

# Path Models ==================================================================
si_l <-'herb_C ~ PECI_cv+simpson+fa+woody_C+site
        bare_SOC~ herb_C+ richness+PECI_cv+other_cv+site
        simpson ~ PECI_cv +site
        # evenness ~  fa + simpson+richness#+herb_C+site
        richness ~ PECI_cv+herb_C+simpson+site#+woody_C+site#+other_SOC
        PECI_cv ~ other_cv +fa+ woody_C+site+ richness#+herb_C#+ bare_SOC+other_SOC
' %>%
  lavaan::sem(data=d)

# summary(si_l)
si_l

# looking for paths to add
modificationindices(si_l, sort. = TRUE)[1:10,]
# looking for paths to subtract
parameterestimates(si_l) %>%
  filter(op=="~")%>%
  group_by(lhs, op, rhs) %>%
  dplyr::summarise(max = max(pvalue, na.rm=T),
                   min=min(pvalue, na.rm=T),
                   mean=mean(pvalue, na.rm=T)) %>%
  ungroup()%>%
  arrange(desc(mean)) %>%
  filter(max >0.05, min >0.05)
resid(si_l, "cor")$cov
set.seed(100)
paths <- ggsem(fit = si_l, filename = "figs/peci_sem.png", exclude = "site", title = "Path Model", 
      layout = "auto", alpha = 0.05);paths

ggsave(paths, filename = "figs/pathmod_jan31.png",bg='white',height =7, width =9)
