# data prep script
library(tidyverse)
write_to_csv <- FALSE
# folded aspect function
get_folded_aspect<-function(aspect){
  180 - abs(aspect-180)
}

# read in weighted C data frame
weighted_c <- read_csv("data/weighted_carbon.csv")

# read in and join several data frames, renaming as necessary or appropriate
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
  # creating total cover and folded aspect columns, making plot a factor
  mutate(total_cover = other_cover + PECI_cover,
         fa = get_folded_aspect(aspect),
         plot = as.factor(plot)) %>%
  dplyr::rename(woody_C = woody_c_g_m2,
                other_cv = other_cover,
                PECI_cv = PECI_cover,
                herb_C = avgherbcarb_g_m2,
                bare_SOC = bare_SOC_g_m2,
                buffel_SOC = buffel_SOC_g_m2,
                other_SOC = other_SOC_g_m2) %>%
  left_join(x=weighted_c %>% dplyr::select(-site, -plot, -bare_cover),
            y=., by = "site_plot") %>%
  # creating a slope * folded aspect column
  mutate(slope_aspect = fa * slope) %>% 
  dplyr::rename(soil_TC = weighted_TC_gC_m2)

# decide whether to write the data frame to a csv  
# write_to_csv <- askYesNo("Do you want to write the data frame to a csv?")  
if (write_to_csv) {  
 write_csv(d, "data/env_data_cleanish.csv")
}

