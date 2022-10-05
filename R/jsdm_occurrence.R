# buffelgrass jsdm

library(tidyverse)
library(Hmsc)
require(snow)
library(ggpubr)
library(ggthemes)
library(ggtext)


# grouping not super prevalent stuff together
# rerun some stuff

# source("R/data_prep.R")

# look up tables ===============================================================

lut_sp_groups <- c("Abutilon_palmeri" = "shrub",
                   "Abutilon_abutiloides" = "perennial_forb",
                   "Acacia_greggii" = "leguminous_tree",
                   "Acacia_sp_unarmed" = "leguminous_tree",
                   "Acourtia_wrightii" = "perennial_forb", 
                   "Agave_schottii" = "woody_succulent",
                   "Allionia_incarnata"  = "perennial_forb"        ,
                   "Allium_sp" = "perennial_forb",
                   "Aloysia_wrightii" = "shrub",
                   "Amaranthus_palmeri" = "annual_forb",
                   "Ambrosia_ambrosioides" = "annual_forb",
                   "Ambrosia_deltoidea"    = "shrub",
                   "Ambrosia_psilostachya" = "annual_forb",
                   "Arceuthobium_sp" = "mistletoe",
                   "Artemisia_ludoviciana" = "perennial_forb", 
                   "Asteraceae_sp" = "perennial_forb",
                   "Asteraceae_sp_shrub" = "shrub", 
                   "Ayenia_filiformis" = "shrub",
                   "Bothriochloa_barbinodis" = "perennial_graminoid", 
                   "Bouteloua_curtipendula" = "perennial_graminoid",
                   "Bouteloua_repens" = "perennial_graminoid", 
                   "Brassica_nigra" = "annual_forb"               ,
                   "Brassicaceae_sp"  = "perennial_forb"           , 
                   "Brickellia_sp" = "shrub",
                   "Calliandra_eriophylla" = "leguminous_shrub"       ,
                   "Carnegiea_gigantea"  = "Carnegiea_gigantea"         ,
                   "Centaurea_melitensis" = "annual_forb",
                   "cf_Bebbia_juncea" = "shrub",
                   "cf_Boechera_sp" = "perennial_forb",
                   "cf_Bromus_sp" = "annual_graminoid",
                   "cf_Chenopodiaceae_sp" = "annual_forb"       , 
                   "cf_Descurainia_sophia" = "annual_forb",
                   "cf_Orobanche_cooperi" = "annual_forb"       , 
                   "cf_Phacelia_sp" = "perennial_forb"               ,
                   "Cirsium_sp" = "annual_forb"                  , 
                   "Cottsia_gracilis" = "Janusia gracilis",
                   "Cryptantha_sp"  = "Cryptantha_spp",
                   "Cryptantha_sp_Unk_2" = "Cryptantha_spp",
                   "Cylindropuntia_bigelovii" ="Cylindropuntia_spp"   ,
                   "Cylindropuntia_versicolor" =    "Cylindropuntia_spp",
                   "Dalea_albiflora"  = "leguminous_shrub"           , 
                   "Dasylirion_wheeleri"  = "woody_succulent"        ,
                   "Echinocereus_triglochidiatus" ="Echinocereus_triglochidiatus", 
                   "Encelia_farinosa"="Encelia_farinosa",
                   "Ephedra_trifurca" = "shrub", 
                   "Eragrostis_lehmanniana" = "Eragrostis_lehmanniana"     ,
                   "Erigeron_divergens" = "perennial_forb"         , 
                   "Erigeron_sp"   = "perennial_forb" ,
                   "Eriogonum_sp" = "perennial_forb"                , 
                   "Eriogonum_wrightii"      = "perennial_forb"      ,
                   "Erodium_cicutarium"  = "annual_forb"         ,
                   "Euphorbiaceae_sp"  = "perennial_forb" ,
                   "Evolvulus_arizonicus"   = "perennial_forb"      ,
                   "Fabaceae_sp"   = "leguminous_forb"                ,
                   "Fern_sp" ="moss_fern"                    , 
                   "Fern_sp_2"  ="moss_fern",
                   "Fern_sp_3"  ="moss_fern"                 ,
                   "Ferocactus_wislizeni"    =  "Ferocactus_wislizeni"    ,
                   "Fouquieria_splendens" = "Fouquieria_splendens"       , 
                   "Gutierrezia_sarothrae" = "shrub",
                   "Hesperostipa_neomexicana"  = "perennial_graminoid"  ,
                   "Ipomoea_hederacea" = "annual_forb"           ,
                   "Ipomoea_tenuiloba"  = "perennial_forb"         , 
                   "Jacquemontia_pringlei"   = "perennial_forb",
                   "Jatropha_cardiophylla"  = "shrub"     , 
                   "Justicia_longii"=  "shrub"            ,
                   "Krameria_erecta"  = "shrub"           , 
                   "Lepidium_densiflorum"     = "Lepidium_densiflorum",
                   "Lycium_parishii" = "Lycium_spp",
                   "Lycium_sp" = "Lycium_spp",
                   "Lycium_sp_2" = "Lycium_spp"                 ,
                   "Machaeranthera_bigelovii"  ="perennial_forb",
                   "Mammillaria_grahamii"  = "Mammillaria_grahamii"      , 
                   "Mammillaria_microcarpa" = "Mammillaria_microcarpa",
                   "Marina_parryi"  = "shrub"             ,
                   "Metastelma_arizonicum"  = "perennial_forb",
                   "Mimosa_aculetecorpa" = "Mimosa_aculetecorpa"        ,
                   "Moss_sp" = "moss_fern"                     ,
                   "Muhlenbergia_porteri"  = "perennial_graminoid"      , 
                   "Opuntia_engelmannii"      =  "Opuntia_engelmannii", 
                   "Parkinsonia_microphylla" = "leguminous_tree"     ,
                   "Pectocarya_sp"  = "annual_forb"              ,
                   "Pennellia_longifolia"   = "perennial_forb"     , 
                   "Pennisetum_ciliare"= "Pennisetum_ciliare",
                   "Phemeranthus_aurantiacus" = "perennial_forb"   , 
                   "Plantago_purshii"     = "Plantago_purshii"        ,
                   "Poa_sp_perennial"   = "perennial_graminoid"         ,
                   "Poaceae_sp_Unk_21" = "perennial_graminoid", 
                   "Porophyllum_gracile"  = "shrub"       , 
                   "Prosopsis_velutina" = "leguminous_tree"          ,
                   "Schrophulariaceae_sp" = "perennial_forb"       , 
                   "Selaginella_pilifera" = "moss_fern",
                   "Sida_abutifolia"      = "perennial_forb"       , 
                   "Sphaeralcea_ambigua"  = "perennial_forb"       , 
                   "Tridens_muticus"      = "perennial_graminoid"       , 
                   "Trixis_californica" = "shrub", 
                   "Unknown_forb"             = "perennial_forb"   , 
                   "Unknown_forb_perennial"    ="perennial_forb",  
                   "Vulpia_bromoides"           ="annual_graminoid" , 
                   "Xanthisma_gracile" = "perennial_forb")         


# lut_sp_groups[div_long$species %>% head()]
# basic data wrangling =========================================================
div_long <- read_csv("data/diversity_cleaned_EF.csv") %>%
  dplyr::select(plot, subplot, species = scientific_name_clean, percent_cover) %>%
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
           str_replace_all("Abutiton_abutiloides", "Abutilon_abutiloides")%>%
           str_replace_all("Mammilaria_microcarpa", "Mammillaria_microcarpa")%>%
           str_replace_all("Pennelia_longifolia", "Pennellia_longifolia")%>%
           str_replace_all("Resurrection_fern", "Selaginella_pilifera")%>%
           str_replace_all("Tridens_mutica", "Tridens_muticus")%>%
           str_replace_all("Xanthisima_gracile", "Xanthisma_gracile")) %>%
  group_by(plot, species) %>%
  summarise(cover = sum(percent_cover)/9) %>%
  ungroup() %>%
  arrange(species)
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

# subplot level ================================================================

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
           str_replace_all("Abutiton_abutiloides", "Abutilon_abutiloides")%>%
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

groups_long_subplot <- read_csv("data/diversity_cleaned_EF.csv") %>%
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
           str_replace_all("Abutiton_abutiloides", "Abutilon_abutiloides")%>%
           str_replace_all("Jatropha_caridophylla", "Jatropha_cardiophylla")%>%
           str_replace_all("Lycium_porishii", "Lycium_parishii")%>%
           str_replace_all("Mammilaria_microcarpa", "Mammillaria_microcarpa")%>%
           str_replace_all("Pennelia_longifolia", "Pennellia_longifolia")%>%
           str_replace_all("Resurrection_fern", "Selaginella_pilifera")%>%
           str_replace_all("Tridens_mutica", "Tridens_muticus")%>%
           str_replace_all("Xanthisima_gracile", "Xanthisma_gracile")) %>%
  filter(subplot != "plant_walk") %>%
  mutate(species = lut_sp_groups[species]) %>%
  group_by(plot, subplot, species) %>%
  summarise(cover = n()) %>%
  ungroup() %>%
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

groups_wide_subplot <- groups_long_subplot %>%
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
  
  
spp_list_new <- div_long_subplot %>%
  pull(species) %>%
  unique() %>%
  as_tibble() %>%
  rename(species = value) 

prev0 <- div_wide_subplot %>%
  arrange(plot) %>%
  tibble::column_to_rownames("plot") %>%
  as.matrix
prev0[prev0>0] <-1

prev <- prev0 %>%
  colSums() %>%
  as_tibble(rownames = "species") %>%
  dplyr::rename(prevalence = value) %>%
  arrange(desc(prevalence))

spp_list_tab1<- spp_list_new%>%
  mutate(group = lut_sp_groups[species]) %>%
  left_join(prev)

  
write_csv(spp_list_tab1, "table_1_species_list.csv")

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

Y <- groups_wide_subplot %>%
  arrange(plot) %>%
  tibble::column_to_rownames("plot") %>%
  as.matrix
Y[Y>0] <-1

prevalence<- colSums(Y) %>%
  as_tibble(rownames = "Species") %>%
  dplyr::rename(prevalence = value) %>%
  arrange(desc(prevalence))#%>%
 # mutate(group = lut_sp_groups[Species])



write_csv(spp_list_tab1, "table_1_species_list.csv")

# group_prevalence<- colSums(Y) %>%
#   as_tibble(rownames = "Species") %>%
#   dplyr::rename(prevalence = value) %>%
#   arrange(desc(prevalence)) %>%
#   mutate(group = lut_sp_groups[Species]) %>%
#   group_by(group) %>%
#   summarise(prevalence = sum(prevalence)) %>%
#   ungroup() %>%
#   arrange(desc(prevalence))

# plotting prevalence for grouping purposes

# ggplot(prevalence, aes(x=prevalence, y=Species)) +
#   geom_bar(stat = "identity")

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
  # totalherb_raw+
  slope

XFormula1 <- ~elevation+
  fa+
  woody_C+
  # totalherb_raw+
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
           # TrData = traits,
           # TrFormula = t_formula,
           studyDesign = studyDesign,
           ranLevels = list("plot" = rL1))

mod_no_peci = Hmsc(Y = Y, XData = XData, XFormula = XFormula1, distr="probit",
           # TrData = traits,
           # TrFormula = t_formula,
           studyDesign = studyDesign,
           ranLevels = list("plot" = rL1))

nChains = 4
test.run = FALSE
if (test.run){
  #with this option, the vignette evaluates in ca. 1 minute in adam's laptop
  thin = 1
  samples = 100
  transient = ceiling(thin*samples*.5)
  hmsc_file <- "data/hmsc/hmsc_probit_subplot_test.Rda"
  hmsc_file_no_peci <- "data/hmsc/hmsc_probit_test_no_peci.Rda"
}else{
  # with a spatial random effect, evaluates in --- 2 hours
  # looks like a compute-optimized aws instance is called for, very little ram usage
  thin = 100
  samples = 1000
  transient = ceiling(thin*samples*.5)
  hmsc_file <- "data/hmsc/hmsc_probit_subplot_group.Rda"
  hmsc_file_no_peci <- "data/hmsc/hmsc_probit_no_peci_group.Rda"
}
t0 <- Sys.time()


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
}else{load(hmsc_file);load(hmsc_file_no_peci)}

# getting the posteriors =======================================================
which_model <- "no_peci"
# which_model <- "w_peci"
if(which_model == "no_peci"){
  load(hmsc_file);load(hmsc_file_no_peci)
  m <-m1
}else{load(hmsc_file)}

mpost <- convertToCodaObject(m)
preds = computePredictedValues(m)
MF = evaluateModelFit(hM=m, predY=preds)
VP <- computeVariancePartitioning(m)

# model convergence, diagnostics ===============================================

# psrf.V = gelman.diag(mpost$V,multivariate=FALSE)$psrf%>%
#   as_tibble() %>% dplyr::rename(psrf_v = `Point est.`)
# 
# ess.v <- effectiveSize(mpost$V)%>%
#   as_tibble() %>% dplyr::rename(ess_v = value)

ess.beta <- effectiveSize(mpost$Beta) %>%
  as_tibble() %>% 
  dplyr::mutate(variable = "Effective Sample Size")
psrf.beta <- gelman.diag(mpost$Beta, multivariate=FALSE)$psrf%>%
  as_tibble() %>% 
  dplyr::rename(value = `Point est.`) %>%
  dplyr::mutate(variable = "Gelman Diagnostic")


diag_all<-bind_rows(ess.beta, psrf.beta) %>%
  ggplot(aes(x=value)) +
  geom_histogram() +
  theme_classic() +
  facet_wrap(~variable, scales="free") +
  ggtitle("Convergence Diagnostics")+
  theme(plot.background = element_rect( color ="black"),
        axis.title.x = element_blank(),
        plot.title = element_text(hjust = 1, face = "bold"))



ggsave(diag_all,filename = paste0("figs/geldman_ess_",which_model,".pdf"), 
       width = 5.5, height=3.5, bg="white")
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



# 
# vp_order <- vp_df %>% filter(variable == "Random: sample") %>%
#   filter(origin=="I") %>%
#   left_join(prevalence) %>%
#   arrange(prevalence, origin) %>%
#   mutate(Species_f = factor(Species, levels = .$Species)) %>%
#   dplyr::select(Species, Species_f, origin) %>%
#   rbind(vp_order_n)# %>%
#   #left_join(mf_df)


if(which_model == "w_peci"){
  vp_order <- vp_df %>%
    filter(variable == "peci_sub_cv") %>%
    arrange(value) %>%
    mutate(Species_f = factor(Species, levels = .$Species)) %>%
    dplyr::select(Species, Species_f) 
  
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
  ggtitle("Variance Partitioning")
}else{
  vp_order <- vp_df %>%
    filter(variable == "fa") %>%
    arrange(value) %>%
    mutate(Species_f = factor(Species, levels = .$Species)) %>%
    dplyr::select(Species, Species_f) 
  
  vp <- left_join(vp_df, vp_order) %>% 
    mutate(variable = replace(variable, variable == "fa", "aspect")) %>%
    mutate(variable = factor(variable, c("Random: plot","woody_C",
                                         "totalherb_raw", "slope","elevation",  "aspect"
    )))%>%
    ggplot(aes(x=value,y=Species_f, fill = variable)) +
    geom_bar(stat="identity")+
    theme_classic() +
    ylab("Species") +
    # xlab("") +
    scale_fill_brewer(palette = "Dark2")+
    theme(legend.text = element_markdown(),
          legend.title = element_blank(),
          legend.position = "left",
          axis.title.x = element_blank(),
          legend.background = element_rect(color="black"),
          plot.title = element_text(hjust = 1, face = "bold")) +
    ggtitle("Proportion of Variance Explained")
  }

ggsave(vp, filename=paste0("figs/variance_partitioning_occurrence_group_",which_model,".png"),
       height = 11.5, width = 9)

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

p_beta<-supported %>%
  mutate(env_var = replace(env_var, env_var == "fa", "aspect")) %>%
  ggplot(aes(x=env_var,y=reorder(Species_f,Species), fill = Mean, color = sign)) +
  geom_tile(lwd=.5) +
  theme_classic()+
  scale_fill_steps2() +
  scale_color_manual(values = c(("red"), ("blue"))) +
  guides(color = "none")+
  theme(axis.text.x = element_text(angle=45, vjust=1,hjust = 1),
        axis.title = element_blank(),
        legend.position = "left",
        plot.background = element_rect(color="black"),
        plot.title = element_text(hjust = 1, face = "bold")) +
  ggtitle("Environmental Filters")

ggsave(paste0("figs/betas_binomial_subplot_group_",which_model,".png"), 
       bg="white", width=6, height=8)




plotBeta(m, post = postBeta, param = "Support",
         supportLevel = 0.95, split=.4, spNamesNumbers = c(T,F))

# species associations =========================================================
OmegaCor = computeAssociations(m)
supportLevel = 0.5
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
  print(n=20) %>%
  write_csv("figs/residual_correlation_abs.csv")


# switch colors around
pcor<-ggcorrplot::ggcorrplot(filtered, type = "lower",
                       hc.order = TRUE)

pcor1<- ggcorrplot::ggcorrplot(hmdf_mean,type = "lower",
                               hc.order = TRUE,
                               colors = c("red", "white", "blue"),
                               title = "Species Associations") +
  theme(plot.background = element_rect(color="black"),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_rect(color="black"),
        plot.title = element_text(hjust = 1, face = "bold"))
ggsave(pcor,
       filename=paste0("figs/species_associations_filtered_",which_model,".png"),
       bg="white", width=18, height = 18)
ggsave(ggcorrplot::ggcorrplot(toPlot, type = "lower",hc.order = TRUE, title = "Occurrence"),
       filename=paste0("figs/species_associations_suported_",which_model,".png"),
       bg="white", width=18, height = 18)
ggsave(pcor1,
       filename=paste0("figs/species_associations_",which_model,".png"),
       bg="white", width=10, height = 10)

# big multipanel ==================

ggarrange(p_beta, vp +
            theme(plot.background = element_rect(color="black")) ,
          diag_all, nrow=3, ncol=1, labels = c("(a)","(b)","(c)")) %>%
  ggarrange(pcor1, widths = c(1,2.5), labels = c("", "(d)")) %>%
  ggsave(., filename = "figs/multipanel_jsdm.png", height=10, width=15.45, bg="white")

ggarrange(p_beta, vp +
            theme(plot.background = element_rect(color="black")) ,
          diag_all, 
          widths = c(1.5,1.5,1),
          nrow=1, ncol=3, labels = c("(a)","(b)","(c)")) %>%
  ggarrange(pcor1, heights = c(1,2), labels = c("", "(d)"),nrow=2) %>%
  ggsave(., filename = "figs/multipanel_jsdm_vertical.png", height=16.45, width=11, bg="white")



#smaller mulipanel

ggarrange(p_beta, 
          diag_all, 
          nrow=2, ncol=1, labels = c("(a)","(b)")) %>%
  ggarrange(vp + theme(plot.background = element_rect(color="black")),
            nrow=1, ncol=2, widths = c(1,1.5),labels = c("", "(c)")) %>%
  ggsave(., filename = "figs/multipanel_jsdm_nocor.png", height=7, width=12, bg="white")

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
