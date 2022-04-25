library(tidyverse)
library(Hmsc)
require(snow)
library(ggpubr)
library(ggthemes)
library(ggtext)

source("R/data_prep.R")

# basic data wrangling =========================================================
# div_long <- read_csv("data/diversity_cleaned_EF.csv") %>%
#   dplyr::select(plot, subplot, species = scientific_name_clean, percent_cover) %>%
#   mutate(species = str_replace_all(species, " ", "_") %>%
#            str_replace_all("\\.","") %>%
#            str_replace_all("\\(","") %>%
#            str_replace_all("\\)","") %>%
#            str_replace_all("bigelovlii", "bigelovii") %>%
#            str_replace_all("curdipendula", "curtipendula") %>%
#            str_replace_all("Carnegia_gigantea", "Carnegiea_gigantea")%>%
#            str_replace_all("Sphaerelcea_ambigua", "Sphaeralcea_ambigua")%>%
#            str_replace_all("cf_Oroborche_cooperi", "cf_Orobanche_cooperi")%>%
#            str_replace_all("Jatropha_caridophylla", "Jatropha_cardiophylla")%>%
#            str_replace_all("Lycium_porishii", "Lycium_parishii")%>%
#            str_replace_all("Mammilaria_microcarpa", "Mammillaria_microcarpa")%>%
#            str_replace_all("Pennelia_longifolia", "Pennellia_longifolia")%>%
#            str_replace_all("Resurrection_fern", "Selaginella_pilifera")%>%
#            str_replace_all("Tridens_mutica", "Tridens_muticus")%>%
#            str_replace_all("Xanthisima_gracile", "Xanthisma_gracile")) %>%
#   group_by(plot, species) %>%
#   summarise(cover = sum(percent_cover)/9) %>%
#   ungroup() %>%
#   arrange(species)
# 
# div_wide <- div_long %>%
#   pivot_wider(id_cols="plot", 
#               names_from = "species", 
#               values_from = "cover", 
#               values_fill = 0)
# 
# write_csv(div_long, "data/species_cover_long.csv")
# write_csv(div_long, "data/species_cover_wide.csv")

env_data <- read_csv("data/env_data_cleanish.csv") %>%
  arrange(site_plot)

# subplot level =========================================================
div_long_subplot <- read_csv("data/diversity_cleaned_EF.csv") %>%
  dplyr::select(plot, subplot, species = scientific_name_clean, cover=percent_cover) %>%
  mutate(species = str_replace_all(species, " ", "_") %>%
           str_replace_all("\\.","") %>%
           str_replace_all("\\(","") %>%
           str_replace_all("\\)","") %>%
           str_replace_all("bigelovlii", "bigelovii") %>%
           str_replace_all("curdipendula", "curtipendula") %>%
           str_replace_all("Carnegia_gigantea", "Carnegiea_gigantea")%>%
           str_replace_all("Sphaerelcea_ambigua", "Sphaeralcea_ambigua")%>%
           str_replace_all("cf_Oroborche_cooperi", "cf_Orobanche_cooperi")%>%
           str_replace_all("Jatropha_caridophylla", "Jatropha_cardiophylla")%>%
           str_replace_all("Lycium_porishii", "Lycium_parishii")%>%
           str_replace_all("Mammilaria_microcarpa", "Mammillaria_microcarpa")%>%
           str_replace_all("Pennelia_longifolia", "Pennellia_longifolia")%>%
           str_replace_all("Resurrection_fern", "Selaginella_pilifera")%>%
           str_replace_all("Tridens_mutica", "Tridens_muticus")%>%
           str_replace_all("Xanthisima_gracile", "Xanthisma_gracile")) %>%
  filter(subplot != "plant_walk")%>%
  group_by(plot, subplot, species) %>%
  summarise(cover = sum(cover)) %>%
  ungroup()  %>%
  arrange(species) 

subplots_with_nothing <- tibble(plot = c("LOLA_02_1"),
                                species = "Carnegiea_gigantea",
                                cover = 0)

peci_cv_sub <- div_long_subplot %>%
  filter(species == "Pennisetum_ciliare")  %>%
  mutate(plot = str_c(plot,"_", subplot)) %>%
  dplyr::select(-subplot, -species) %>%
  dplyr::rename(peci_sub_cv = cover)

div_wide_subplot <- div_long_subplot %>%
  mutate(plot = str_c(plot,"_", subplot)) %>%
  dplyr::select(-subplot) %>%
  rbind(subplots_with_nothing) %>%
  pivot_wider(id_cols="plot", 
              names_from = "species", 
              values_from = "cover", 
              values_fill = 0) 

write_csv(div_long_subplot, "data/species_cover_long_subplot.csv")
write_csv(div_wide_subplot, "data/species_cover_wide_subplot.csv")

env_data_prep <- read_csv("data/env_data_cleanish.csv") %>%
  arrange(site_plot) %>%
  expand_grid(df=., subplot=1:8,.name_repair = "check_unique")

env_data_subplot <- pluck(env_data_prep,1) %>%
  mutate(subplot = pluck(env_data_prep, 2)) %>%
  mutate(plot = str_c(site_plot,"_", subplot)) %>%
  dplyr::select(-subplot) %>%
  left_join(peci_cv_sub) %>%
  replace_na(list(peci_sub_cv = 0))
  
  
spp_list_new <- div_long %>%
  pull(species) %>%
  unique() %>%
  as_tibble() %>%
  rename(species = value)
# 
# write_csv(spp_list , "data/spp_list_raw.csv")

spp_list <- read_csv("data/spp_list.csv") %>%
  arrange(species) %>%
  filter(species %in% spp_list_new$species)

# Hmsc-specific data wrangling =================================================

Y <- div_wide_subplot %>%
  arrange(plot) %>%
  tibble::column_to_rownames("plot") %>%
  as.matrix
Y[Y>0] <-1

prevalence<- colSums(Y) %>%
  as_tibble(rownames = "Species") %>%
  dplyr::rename(prevalence = value) %>%
  arrange(desc(prevalence))

XData <- env_data_subplot %>%
  arrange(plot) %>%
  dplyr::select(-starts_with("buffel"))%>%
  mutate(site = as.factor(site), site_plot = as.factor(site_plot))%>%
  tibble::column_to_rownames("plot") %>%
  as.data.frame()

XFormula <- ~elevation+
  fa+
  peci_sub_cv+
  woody_C+
  totalherb_raw+
  slope

XFormula1 <- ~elevation+
  fa+
  woody_C+
  totalherb_raw+
  slope


traits <- spp_list %>%
  dplyr::select(-new_name) %>%
  transmute_all(as.factor)%>%
  left_join(prevalence, by =c("species"="Species")) %>%
  arrange("species") %>%
  tibble::column_to_rownames("species") %>%
  na.omit()

t_formula <- ~origin+duration+phylogeny+woody+succulent

studyDesign <- data.frame(site = as.factor(env_data_subplot$site),
                          plot = as.factor(env_data_subplot$site_plot))
rL <- HmscRandomLevel(units = levels(studyDesign$site))
rL1 <- HmscRandomLevel(units = levels(studyDesign$site_plot))

# The models ===================================================================

mod = Hmsc(Y = Y, XData = XData, XFormula = XFormula, distr="probit",
           TrData = traits,
           TrFormula = t_formula,
           studyDesign = studyDesign,
           ranLevels = list("plot" = rL1))

mod_no_peci = Hmsc(Y = Y, XData = XData, XFormula = XFormula1, distr="probit",
           TrData = traits,
           TrFormula = t_formula,
           studyDesign = studyDesign,
           ranLevels = list("plot" = rL1))

nChains = 4
test.run = TRUE
if (test.run){
  #with this option, the vignette evaluates in ca. 1 minute in adam's laptop
  thin = 1
  samples = 100
  transient = ceiling(thin*samples*.5)
}else{
  # with a spatial random effect, evaluates in --- 2 hours
  # looks like a compute-optimized aws instance is called for, very little ram usage
  thin = 100
  samples = 100
  transient = ceiling(thin*samples*.5)
}
t0 <- Sys.time()
hmsc_file <- "data/hmsc/hmsc_probit_subplot_test.Rda"
hmsc_file_no_peci <- "data/hmsc/hmsc_probit_test_no_peci.Rda"

dir.create("data/hmsc")
if(!file.exists(hmsc_file)){
  m = sampleMcmc(mod, thin = thin,
                 samples = samples,
                 transient = transient,
                 adaptNf = rep(ceiling(0.4*samples*thin),1),
                 nChains = nChains,
                 nParallel = nChains)
  print(Sys.time()-t0)
  save(m, file=hmsc_file)
  
  m1 = sampleMcmc(mod_no_peci, thin = thin,
                 samples = samples,
                 transient = transient,
                 adaptNf = rep(ceiling(0.4*samples*thin),1),
                 nChains = nChains,
                 nParallel = nChains)
  print(Sys.time()-t0)
  save(m1, file=hmsc_file_no_peci)
}else{load(hmsc_file)}

mpost <- convertToCodaObject(m)
preds = computePredictedValues(m)
MF = evaluateModelFit(hM=m, predY=preds)
VP <- computeVariancePartitioning(m)

# model convergence, diagnostics ===============================================

psrf.V = gelman.diag(mpost$V,multivariate=FALSE)$psrf%>%
  as_tibble() %>% dplyr::rename(psrf_v = `Point est.`)


ess.beta <- effectiveSize(mpost$Beta) %>%
  as_tibble() %>% dplyr::rename(ess_beta = value)

ess.v <- effectiveSize(mpost$V)%>%
  as_tibble() %>% dplyr::rename(ess_v = value)
psrf.beta <- gelman.diag(mpost$Beta, multivariate=FALSE)$psrf%>%
  as_tibble() %>% dplyr::rename(psrf_beta = `Point est.`)

diag_all <- ggarrange(ggplot(ess.beta, aes(x=ess_beta)) + 
                        geom_histogram()+
                        xlab("Effective Sample Size"),
                      ggplot(psrf.beta, aes(x=psrf_beta)) +
                        geom_histogram()+
                        xlab("Gelman Diagnostic"),
                      align = "v") +ggtitle("All Plots")

ggsave(diag_all,filename = "figs/geldman_ess.pdf", width = 5.5, height=3.5, bg="white")
MF$TjurR2%>% mean(na.rm=T)
# explanatory power

ggarrange(
  ggplot(as.data.frame(MF),aes(x=(RMSE))) + geom_histogram(),
  ggplot(as.data.frame(MF),aes(x=(TjurR2))) + geom_histogram(),
  ggplot(as.data.frame(MF),aes(x=(AUC))) + geom_histogram())

# plot the variance partitioning ===============================================

mf_df <- data.frame(Species = colnames(m$Y),
                    R2 = MF$TjurR2,
                    AUC = MF$AUC,
                    RMSE = MF$RMSE) %>%
  left_join(prevalence)
mean(mf_df%>% filter(prevalence>7) %>% pull(R2), na.rm=T)
ggplot(mf_df, aes(x=prevalence, y=R2)) +
  geom_point()

sbquants <- summary(mpost$Beta)$quantiles %>%
  as_tibble(rownames = "variable") %>% 
  mutate(sign = `2.5%` * `97.5%`) %>%
  filter(sign>0) %>%
  separate(variable,
           into = c("variable", "species"),
           sep = ",") %>%
  mutate(variable = str_sub(variable, 3,nchar(variable)-5),
         species = str_sub(species, 2,nchar(species)-6) %>% trimws) %>%
  filter(variable!= "(Intercept)") %>%
  dplyr::select(variable,species,`2.5%`,`50%`,`97.5%`) %>%
  arrange(variable)


vp_df <- VP$vals%>%
  as_tibble(rownames = "variable") %>%
  pivot_longer(cols=names(.)[2:ncol(.)], 
               names_to = "Species", 
               values_to = "value") %>%
  left_join(prevalence) %>%
  na.omit()

vp_summary <- vp_df %>%
  group_by(variable) %>%
  summarise(value = mean(value)) %>%
  ungroup() 

vp_order <- vp_df %>%
  filter(variable == "peci_sub_cv") %>%
  arrange(value) %>%
  mutate(Species_f = factor(Species, levels = .$Species)) %>%
  dplyr::select(Species, Species_f) 

# 
# vp_order <- vp_df %>% filter(variable == "Random: sample") %>%
#   filter(origin=="I") %>%
#   left_join(prevalence) %>%
#   arrange(prevalence, origin) %>%
#   mutate(Species_f = factor(Species, levels = .$Species)) %>%
#   dplyr::select(Species, Species_f, origin) %>%
#   rbind(vp_order_n)# %>%
#   #left_join(mf_df)



vp <- left_join(vp_df, vp_order) %>% 
  mutate(variable = factor(variable, c("Random: plot","woody_C",
                                           "totalherb_raw", "slope","elevation",  "fa","peci_sub_cv"
                                            )))%>%
  ggplot(aes(x=value,y=Species_f, fill = variable)) +
  geom_bar(stat="identity")+
  theme_classic() +
  ylab("Species") +
  xlab("Proportion of Variance Explained") +
  scale_fill_brewer(palette = "Dark2")+
  theme(legend.position = "right",
        legend.text = element_markdown(),
        legend.title = element_blank(),
        # legend.justification = c(1,0),
        legend.background = element_rect(color="black")) +
  ggtitle("Variance Partitioning, Occurrence Model")

ggsave(vp, filename="figs/variance_partitioning_occurrence.png", height = 11.5, width = 9)

# Environmental filters ==================================================

postBeta <- getPostEstimate(m, parName = "Beta")

means <- postBeta$mean %>%
  as_tibble() %>%
  rowid_to_column("env_var") %>%
  mutate(env_var = c("intercept",VP$groupnames)) %>%
  pivot_longer(cols=names(.)[2:ncol(.)], names_to = "Species", values_to = "Mean")

supported <- postBeta$support %>% 
  as_tibble() %>%
  rowid_to_column("env_var") %>%
  mutate(env_var = c("intercept",VP$groupnames)) %>%
  pivot_longer(cols=names(.)[2:ncol(.)], 
               names_to = "Species", 
               values_to = "Support") %>%
  filter(Support >0.89|Support<0.11,
         env_var != "intercept") %>%
  left_join(means, by = c("env_var", "Species"))%>%
  mutate(sign = ifelse(Mean>0, "+", "-"))%>%
  left_join(vp_order)#

p_beta<-ggplot(supported, aes(x=env_var,y=reorder(Species_f,Species), fill = Mean, color = sign)) +
  geom_tile(lwd=.5) +
  theme_clean()+
  scale_fill_steps2() +
  scale_color_manual(values = c(("red"), ("blue"))) +
  guides(color = "none")+
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust = 1),
        axis.title = element_blank())
ggsave("figs/betas_binomial_subplot.png", bg="white", width=6, height=8)



plotBeta(m, post = postBeta, param = "Support",
         supportLevel = 0.95, split=.4, spNamesNumbers = c(T,F))

# species associations =========================================================
OmegaCor = computeAssociations(m)
supportLevel = 0.89
toPlot = ((OmegaCor[[1]]$support>supportLevel) + 
            (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean

toPlot_p = ((OmegaCor[[1]]$support>supportLevel) + 
            (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$support

filtered<- toPlot[colSums(toPlot) != 1,rowSums(toPlot) != 1]
filteredp<- toPlot_p[colSums(toPlot) != 1,rowSums(toPlot) != 1]

hmdf_mean <- OmegaCor[[1]]$mean %>%
  as.matrix
hmdf_support <- OmegaCor[[1]]$support %>%
  as.matrix

# avg association strengths
OmegaCor[[1]]$mean %>%
  abs() %>%
  rowSums() %>%
  as_tibble(rownames = "Species") %>%
  arrange(desc(value)) %>%
  left_join(prevalence) %>%
  print(n=20)



ggsave(ggcorrplot::ggcorrplot(filtered, type = "lower",
                              hc.order = TRUE,
                              title = "Occurrence, p < 0.11"),
       filename="figs/species_associations_filtered.png",
       bg="white", width=18, height = 18)
ggsave(ggcorrplot::ggcorrplot(toPlot, type = "lower",hc.order = TRUE, title = "Occurrence"),
       filename="figs/species_associations_suported.png",
       bg="white", width=18, height = 18)
ggsave(ggcorrplot::ggcorrplot(hmdf_mean,type = "lower",hc.order = TRUE, title = "Occurrence"),
       filename="figs/species_associations.png",
       bg="white", width=18, height = 18)


# gradients ===================

peci_gradient = constructGradient(m, focalVariable = "peci_sub_cv")


predY_peci = predict(m, XData=peci_gradient$XDataNew, 
                     studyDesign=peci_gradient$studyDesignNew, 
                     ranLevels=peci_gradient$rLNew, expected=TRUE)

plotGradient(m, peci_gradient, pred=predY_peci, measure="S")

n_runs <- nChains*samples

pred_df_grazing <- do.call("rbind", predY_peci) %>%
  as_tibble() %>%
  mutate(peci_cover = rep(peci_gradient$XDataNew$peci_sub_cv,
                                 n_runs),
         run = rep(1:n_runs,each=20)) %>%
  pivot_longer(values_to = "cover", names_to = "Species", -c(peci_cover,run)) %>%
  left_join(prevalence) %>%
  filter(prevalence > 1) %>%
  left_join(spp_list, by = c("Species" = "species")) %>%
  arrange(habit, desc(prevalence)) %>%
  mutate(Species_f = factor(Species, levels = unique(.$Species))) 

grazing_native <- pred_df_grazing  %>%
  ggplot(aes(x=peci_cover, y=cover, color = habit)) +
  geom_line(alpha = 0.03, aes(group=run), key_glyph="rect")+
  facet_wrap(~Species_f, ncol=7)+
  xlab("PECI Cover") +
  ylab("Probability of Occurrence") +
  guides(color=guide_legend(override.aes = list(alpha=1)))+
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = c(.9,0.05),
        legend.direction = "horizontal",
        strip.text = element_markdown(),
        panel.border = element_rect(fill=NA, size=0.75),
        legend.justification = c(1,0),
        legend.title = element_blank())

ggsave(grazing_native, filename = "figs/gradient_peci_cover_subplot.png", width = 20, height = 15)
