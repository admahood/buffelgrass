# soil figures and tables

source("R/data_prep.R")
library(lmerTest)
library(emmeans)
library(ggtext)

# SOIL C Figures

d %>%
  dplyr::select(bare_SOC, buffel_SOC, other_SOC, ends_with("TSC_g_m2")) %>%
  pivot_longer(cols = names(.)) %>%
  tidyr::separate(name, c("cover_type", "variable"), "_") %>%
  mutate(cover_type = ifelse(cover_type == "other", "native", cover_type),
         variable = ifelse(variable == "SOC", "Soil Organic Carbon", "Total Soil Carbon")) %>%
  ggplot(aes(x=cover_type, y=value, fill=cover_type)) +
  geom_violin(draw_quantiles = .5,trim = F) +
  facet_wrap(~variable) +
  theme_classic() +
  ylab("g m<sup>2</sup> C") +
  xlab("Cover Type") +
  scale_fill_manual(values = c("burlywood4", "gold", "darkgreen")) +
  theme(axis.title.y = element_markdown(),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_blank(),
        legend.title = element_blank(),
        panel.border = element_rect(color = "black", fill=NA))

ggsave(filename = "figs/soil_c_violin.png", width = 5.5, height=3.5)

# soil c means

dsc<- d %>%
  dplyr::select(bare_SOC, buffel_SOC, other_SOC, ends_with("TSC_g_m2"),site_plot) %>%
  pivot_longer(cols = names(.)[1:6]) %>%
  tidyr::separate(name, c("cover_type", "variable"), "_") %>%
  tidyr::separate(site_plot, c("site", "plot"), "_") %>%
  mutate(cover_type = ifelse(cover_type == "other", "native", cover_type))


m_soc<-lm(log(value) ~ cover_type + site, data = dsc %>% filter(variable == "SOC"))
m_tsc<-lm(log(value) ~ cover_type + site, data = dsc %>% filter(variable == "TSC"))

summary(m_soc); r2glmm::r2beta(m_soc)
summary(m_tsc); r2glmm::r2beta(m_tsc)
performance::check_model(m_soc)
performance::check_model(m_tsc)

car::Anova(m_tsc)
car::Anova(m_soc)

rg_soc <- ref_grid(m_soc)
rg_tsc <- ref_grid(m_tsc)


c_soc<- emmeans(m_soc, ~cover_type, adjust = "sidak") %>% contrast(type = "response")
c_tsc<- emmeans(m_tsc, ~cover_type, adjust = "sidak") %>% contrast(type = "response")
p_soc<- emmeans(m_soc, ~cover_type, adjust = "sidak") %>% pairs(type = "response")
p_tsc<- emmeans(m_tsc, ~cover_type, adjust = "sidak") %>% pairs(type = "response")

bind_rows(data.frame(p_soc) %>% mutate(var = "SOC"), 
          data.frame(p_tsc) %>% mutate(var = "TSC"))  %>%
  mutate_if(is.numeric, function(x) round(x, 3)) %>%
  write_csv("figs/soil_c_pairs.csv")

bind_rows(
  emmeans(m_tsc, ~cover_type, adjust = "sidak", type ="response") %>% as.data.frame()%>% mutate(var = "TSC")
  ,emmeans(m_soc, ~cover_type, adjust = "sidak", type ="response") %>% as.data.frame()%>% mutate(var="SOC")
) %>%
  mutate_if(is.numeric, function(x) round(x, 1)) %>%
  write_csv("figs/soil_c_table.csv")
