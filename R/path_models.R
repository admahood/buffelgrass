# path model
source("R/data_prep.R")
source("R/ggplot_sem.R")

# node names

nnn <- c("slope_aspect" = "Slope X Aspect",
         "herb_C" = "Herbaceous\nCarbon",
         "shannon" = "Alpha\nDiversity",
         "PECI_cv" = "Buffelgrass\nCover",
         "richness" = "Species\nRichness",
         "soil_TC" = "Soil\nTotal Carbon",
         "woody_C" = "Woody\nCarbon",
         "other_cv" = "Native\nCover",
         "site" = "Site")

# Path Models ==================================================================
# si_l <-'herb_C ~ PECI_cv+shannon+fa+woody_C+site
#         # bare_SOC~ herb_C+ richness+PECI_cv+other_cv+site +woody_C+fa+shannon
#         # other_SOC~ richness + PECI_cv + other_cv + site + woody_C + fa
#         # buffel_SOC~ herb_C+ richness+PECI_cv+other_cv+site +woody_C+fa+shannon
#         shannon ~ PECI_cv +site + fa + herb_C + richness
#         # evenness ~  fa + shannon+richness#+herb_C+site
#         richness ~ PECI_cv+herb_C+shannon+site#+woody_C+site#+other_SOC
#         PECI_cv ~ other_cv +fa+ woody_C+site+ richness#+herb_C#+ bare_SOC+other_SOC
#        ' %>%
#   lavaan::sem(data=d %>% mutate_if(is.numeric, scale))
# 
# 
# 
# # summary(si_l)
# si_l
# 
# # looking for paths to add
# modificationindices(si_l, sort. = TRUE)[1:10,]
# # looking for paths to subtract
# parameterestimates(si_l) %>%
#   filter(op=="~")%>%
#   group_by(lhs, op, rhs) %>%
#   dplyr::summarise(max = max(pvalue, na.rm=T),
#                    min=min(pvalue, na.rm=T),
#                    mean=mean(pvalue, na.rm=T)) %>%
#   ungroup()%>%
#   arrange(desc(mean)) %>%
#   filter(max >0.05, min >0.05)
# resid(si_l, "cor")$cov
# set.seed(100)
# 
# 
# layout_df <-  random_layout(si_l) %>%
#   mutate(x=replace(x, metric=="fa", -1),
#          y=replace(y, metric=="fa", 1),
#          x=replace(x, metric=="woody_C", -1),
#          y=replace(y, metric=="woody_C", 0),
#          x=replace(x, metric=="richness", 1.3),
#          y=replace(y, metric=="richness", -.33),
#          x=replace(x, metric=="other_cv", -1),
#          y=replace(y, metric=="other_cv", -1),
#          x=replace(x, metric=="PECI_cv", 0),
#          y=replace(y, metric=="PECI_cv", 0),
#          x=replace(x, metric=="bare_SOC", 1.3),
#          y=replace(y, metric=="bare_SOC", .33),
#          x=replace(x, metric=="herb_C", 0),
#          y=replace(y, metric=="herb_C", 1),
#          x=replace(x, metric=="shannon", 1),
#          y=replace(y, metric=="shannon", -1),
#          x=replace(x, metric=="other_SOC", 1),
#          y=replace(y, metric=="other_SOC", 1))
# 
# 
# paths <- ggsem(fit = si_l, filename = "figs/peci_sem.png", exclude = "site", title = "Path Model", 
#                layout = "manual",layout_df = layout_df, alpha = 0.05);paths
# 
# ggsave(paths, filename = "figs/pathmod_july18_nosoil.png",bg='white',height =7, width =9)

# path model with weighted C estimates =========================================

si_t <-'herb_C ~ bhc*PECI_cv + shannon + slope_aspect + woody_C + site
        soil_TC ~ btc*PECI_cv + hctc*herb_C + slope_aspect + richness  + site
        shannon ~ PECI_cv + site + slope_aspect + herb_C + richness
        richness ~ br*PECI_cv + hcr*herb_C + shr*shannon + site + tcr*soil_TC
        PECI_cv ~ other_cv + slope_aspect + woody_C + site + rb*richness + soil_TC
        
        # indirect effects
        i_bhc_hcr := bhc*hcr
        T_bhc_sr := bhc*hcr*rb + br
       ' %>%
  lavaan::sem(data=d %>% mutate_if(is.numeric, scale))

summary(si_t)
si_t

# looking for paths to add
modificationindices(si_t, sort. = TRUE)[1:10,]
# looking for paths to subtract
parameterestimates(si_t) %>%
  filter(op=="~")%>%
  group_by(lhs, op, rhs) %>%
  dplyr::summarise(max = max(pvalue, na.rm=T),
                   min=min(pvalue, na.rm=T),
                   mean=mean(pvalue, na.rm=T)) %>%
  ungroup()%>%
  arrange(desc(mean)) %>%
  filter(max >0.05, min >0.05)
resid(si_t, "cor")$cov %>%
  as_tibble(rownames = "variable") %>%
  mutate_if(is.numeric, round, 3) %>%
  write_csv("data/covariance_sem.csv")
set.seed(100)

layout_df <-  random_layout(si_t) %>%
  mutate(x=replace(x, metric=="slope_aspect", -1),
         y=replace(y, metric=="slope_aspect", 1),
         x=replace(x, metric=="woody_C", -1),
         y=replace(y, metric=="woody_C", 0),
         x=replace(x, metric=="richness", 1),
         y=replace(y, metric=="richness", 0),
         x=replace(x, metric=="other_cv", -1),
         y=replace(y, metric=="other_cv", -1),
         x=replace(x, metric=="PECI_cv", 0),
         y=replace(y, metric=="PECI_cv", 0),
         x=replace(x, metric=="site", 0),
         y=replace(y, metric=="site", -1),
         x=replace(x, metric=="bare_SOC", 1.3),
         y=replace(y, metric=="bare_SOC", .33),
         x=replace(x, metric=="herb_C", 0),
         y=replace(y, metric=="herb_C", 1),
         x=replace(x, metric=="shannon", 1),
         y=replace(y, metric=="shannon", -1),
         x=replace(x, metric=="soil_TC", 1),
         y=replace(y, metric=="soil_TC", 1))

paths <- ggsem1(fit = si_t, filename = "figs/peci_semt.png", exclude = "site",
                title = "Path Model", rename_nodes = T, new_node_names = nnn,
               layout = "manual",layout_df = layout_df, alpha = 0.05);paths

legend <- make_legend()

ggsave(ggarrange(paths, legend, widths = c(2,.5)), 
       filename = "figs/pathmod_ecosystem_weighted_c.png",
       bg='white',height =5, width =10)

sem_df <- tibble(measure = c("df", "p-value", "Chi-sq", "cfi", "tli", "rmsea", "srmr"),
                 value = c(
si_t@Fit@test[[1]]$df,
round(si_t@Fit@test[[1]]$pvalue,2),
round(si_t@Fit@test[[1]]$stat,2),
round(fitmeasures(si_t, "cfi") %>% as.numeric,2),
round(fitmeasures(si_t, "tli") %>% as.numeric,2),
round(fitmeasures(si_t, "rmsea") %>% as.numeric,2),
round(fitmeasures(si_t, "srmr") %>% as.numeric,2)))
  
write_csv(sem_df, "data/sem_df.csv")
  
### plot indirect effects ======================================================
  
params1 <- lavaan::standardizedSolution(si_t) %>%
  filter(lhs != "exclude",
         rhs != "exclude") %>%
  filter(lhs == "richness" | lhs == "herb_C" | lhs == "PECI_cv") %>%
  filter(rhs %in% c("PECI_cv", "richness", "herb_C")) %>%
  filter(label != "br", label != "rb")
  
indirect1 <- ggsem_filt(params = params1, filename = "figs/peci_sem_ie1.png", exclude = "site",
                title = "Indirect Pathway", rename_nodes = T, new_node_names = nnn,
                layout = "manual",layout_df = layout_df, alpha = 0.05) +
  scale_edge_color_manual(values = "#377EB8");indirect1

params1 <- lavaan::standardizedSolution(si_t) %>%
  filter(lhs != "exclude",
         rhs != "exclude") %>%
  filter(lhs == "richness" | lhs == "herb_C" | lhs == "PECI_cv") %>%
  filter(rhs %in% c("PECI_cv", "richness", "herb_C")) %>%
  filter(label != "br", label != "rb")

indirect1 <- ggsem_filt(params = params1, filename = "figs/peci_sem_ie1.png", exclude = "site",
                        title =c("Indirect Effect on Species \nRichness through Herbaceous Carbon",
                                 "Standardized Effect = 1.589"),
                        rename_nodes = T, new_node_names = nnn,
                        layout = "manual",layout_df = layout_df, alpha = 0.05) +
  scale_edge_color_manual(values = "#377EB8");indirect1

params2 <- lavaan::standardizedSolution(si_t) %>%
  filter(lhs != "exclude",
         rhs != "exclude") %>%
  filter(lhs == "richness" | lhs == "herb_C" | lhs == "PECI_cv") %>%
  filter(rhs %in% c("PECI_cv", "richness", "herb_C"))

indirect2 <- ggsem_filt(params = params2, filename = "figs/peci_sem_ie1.png", exclude = "site",
                        title = c("Total effect on Species Richness","Standardized Effect = -2.505"),
                        rename_nodes = T, new_node_names = nnn,
                        layout = "manual",layout_df = layout_df, alpha = 0.05) ;indirect2

ggarrange(indirect1, indirect2) %>%
  ggsave(plot=., filename = "figs/indirect_effects.png", width=12, height = 5,
         bg="white")


ggarrange(
  ggarrange(paths, legend, widths = c(2,.5), nrow=1),
  ggarrange(indirect1, indirect2, nrow = 1, labels = c('', 'c')),
  nrow = 2, labels = 'auto'
)
ggsave(filename = 'figs/multipanel_sem.png', width = 12, height =10, bg='white')
