setwd("C:/Users/Emily/Desktop/BuffelgrassAnalysis")

# cleaning AGB data


library(tidyverse)

lut_species <- c("Carnegiea gigantea"= "CAGI",
                 "Lycium sp. (2)" = "Lycium sp 2",
                 "Encelia farinosa" = "ENFO",
                 "Encelia farinosa" = "Encilia farinosa",
                 "Encelia farinosa" = "ENFA",
                 "Encelia farinosa" = "ENFI",
                 "Ferocactus wislizeni"= "FEWI", 
                 "Cylindropuntia versicolor" = "CYVE",
                 "Parkinsonia microphylla" = "PAMI",
                 "Fouquieria splendens" = "FOSP",
                 "Cylindropuntia bigelovii" = "CYBI",
                 "Acacia greggii" = "ACGR",
                 "Acacia greggii" = "AGCR",
                 "Cylindropuntia versicolor" = "Cylindropuntia versicolor",
                 "Dalea albiflora" = "DAAL",
                 "Dasylirion wheeleri" = "DAWH",
                 "Carnegiea gigantea"= "GACI",
                 "Jatropha cardiophylla" = "JACA",
                 "Lycium sp. (2)" = "Lycium sp 2 (inverted cone)",
                 "Lycium sp. (2)" = "Lycium",
                 "Lycium sp. (2)" = "Lycium spp",
                 "Lycium sp. (2)" = "Lycium sp 2",
                 "Mammillaria grahamii" = "MAGR", 
                 "Mammillaria grahamii" = "Mammillari arahami", 
                 "Mimosa aculetecorpa"= "MIAC",
                 "Opuntia engelmannii" = "Opuntia englemani",
                 "Opuntia engelmannii" = "Opuntia englemannii",
                 "Opuntia engelmannii" = "OPEN",
                 "Parkinsonia microphylla" = "PAMI",
                 "Prosopis velutina" = "PRVE",
                 "Prosopis velutina" = "Prosopsis velutina",
                 "Prosopis velutina" = "Prosopis velutina",
                 "Jatropha cardiophylla" = "SACA",
                 "Acacia sp. (unarmed)" = "Thornless Acacia")

# lut is backwards!
lut_fixed <- names(lut_species)
names(lut_fixed) <- lut_species %>% as.character

AGB_input <- read_csv("DataPrep/AGB_woody/AGB_data - Data.csv") %>%
  replace_na(list(subplot = "whole plot")) %>%
  mutate(unknown_status = NA,
         scientific_name_clean = lut_fixed[species_raw])

unique(AGB_input$scientific_name_clean)
 
unique(AGB_input$measurement_variable)

lut_measure <- c("AlphaUp"= "AlphaUp",
                 "AlphaDown" = "AlphaDown",
                 "ArmLength1" = "ArmLength1",
                 "ArmLength2" = "ArmLength2",
                 "ArmLength3" = "ArmLength3",
                 "ArmLength4" = "ArmLength4",
                 "Distance"= "Distance",
                 "ddh" = "ddh",
                 "Height"= "Height",
                 "maxD" = "crownD",
                 "max90D"= "crown90D",
                 "maxD"= "MaxDiameter",
                 "max90D" = "PerpendiculartoMax",
                 "max90D"= "90maxD",
                 "max90D" = "max90D",
                 "max90D" = "PerpendiculartoMax",
                 "maxD"= "maxc",
                 "max90D" = "90maxc",
                 "max90Dbase" = "ninetyBaseCrownDiameter",
                 "maxDbase" = "maxBaseCrownDiameter",
                 "dbh" = "dbh",
                 "basecrownheight"= "basecrownheight",
                 "Distance"= "distance",
                 "OldPad" = "OP",
                 "MeasurementHeight"= "MeasurementHeight",
                 "OldPad"= "OldPad",
                 "OldPad2" = "New Pad",
                 "OldPad"= "Old Pad ",
                 "Height" = "MaxHeight",
                 "OldPad"= "OP",
                 "EmergentStems" = "EmergentStems",
                 "OldPad" = "Old Pad")

#note that all of the maxD and 90maxD can refer to crown or diameter for ferocactus etc
#but it will be easier to have them all called the same thing for volume measure.
#also includes base measurements of f. splendens.

# lut is backwards!
lut_measuref <- names(lut_measure)
names(lut_measuref) <- lut_measure %>% as.character

AGB_input <- AGB_input%>%
  mutate(unknown_status = NA,
         measurement_variable_clean = lut_measuref[measurement_variable])
  
write_csv(AGB_input, "DataPrep/AGB_woody/output/AGB_cleaned_EF.csv")        
 

                 
                 