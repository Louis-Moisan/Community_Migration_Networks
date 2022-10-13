#Title: Determining the efficacity of filters to reduce the number of biogeographic region (ecoregion. province, realm) of non-breeding range for each taxonomic group
#Author: Louis Moisan
#Date: July 19, 2021


#-------------------#
##### Librairies ####
#-------------------#
library(tidyverse)
library(sf)
source("scripts/functions/1_functions_data_manip.R")


#------------------#
#### Meata data ####
#------------------#
#---Read the species meta data file
species_meta <- read.csv("data/metadata/species_colors.csv")

#Create a data frame with only long distance migrant
long_dist_migrant <- species_meta %>% 
  #Keep only long distance migrants
  dplyr::filter(non_breeding_strategy == "Migrant")

#Read ecoregion type associated to each species
sp_eco_type <-  read.csv("data/metadata/TableS3.SpeciesHabitat.csv")
sp_eco_type_v <- strsplit(sp_eco_type$Habitat.type, split = "\n")
sp_eco_type <-  data.frame(Species = rep(sp_eco_type$Species, sapply(sp_eco_type_v, length)), Habitat.type = unlist(sp_eco_type_v)) %>% 
  plyr::rename( c("Species"= "species", "Habitat.type" = "eco_type")) %>% 
  dplyr::mutate(eco_type= tolower(eco_type)) %>% 
  dplyr::mutate_all(na_if,"") %>% 
  na.omit() %>% 
  dplyr::arrange(species, eco_type)

#read flyway meta-data
#Read the database
sp_fly <-  read.csv("data/metadata/TableS2.SpeciesFlyway.csv")
sp_fly_v <- strsplit(sp_fly$Flyway, split = "\n")
sp_fly <-  data.frame(Species = rep(sp_fly$Species, sapply(sp_fly_v, length)), Flyway = unlist(sp_fly_v))%>% 
  plyr::rename( c("Species"= "species", "Flyway" = "flyway")) %>% 
  dplyr::arrange(species, flyway)


#------ Ebird 
#Ecoregion
ebird_eco_list <- extract_nb_region_data(data= "ebird",
                       scale= "ecoregion",
                       scale_abbrev = "eco")

#----- Birdlife
#Ecoregion
birdlife_eco_list <- extract_nb_region_data(data= "birdlife",
                                         scale= "ecoregion",
                                         scale_abbrev = "eco")
#------ Ebird-Birdlife
#Ecoregion
ebird_birdlife_eco_list <- extract_nb_region_data(data= "ebird_birdlife",
                                            scale= "ecoregion",
                                            scale_abbrev = "eco")

#---- Combine all number of ecoregion filtered by species in a single data frame
list_eco <- c(ebird_eco_list, birdlife_eco_list, ebird_birdlife_eco_list)
ecoregion <- data.frame(species= character(), ecoregion= numeric(), data= character(), filter= character()) #Create empty data frame

for(df in names(list_eco)){
 ecoregion <-  rbind(ecoregion, list_eco[[df]])
}

#------------------------#
#### Tracking devices ####
#------------------------#
sp_track <-  sf::st_read("data/shapefiles/overlap_range_ecoregions/sp_track_eco_fly.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(species,ecoregion, eco_type, flyway) %>% 
  unique() %>% 
dplyr::mutate(data="tracking") #add data type

#--- Raw
sp_track_raw <- sp_track %>% 
  dplyr::select(species, data, ecoregion, eco_type) %>% 
  unique() %>% 
  dplyr::mutate(filter= "raw")
#ecoregion
sp_track_eco_raw <- sp_track_raw %>%
  dplyr::group_by(species, data, filter) %>%
  dplyr::summarise(ecoregion = length(unique(ecoregion)))

#--- Habitat filter
sp_track_hab <- sp_track_raw %>%
  dplyr::semi_join(sp_eco_type, by= c("species","eco_type")) %>% 
  dplyr::mutate(filter= "habitat")#apply filter
#ecoregion
sp_track_eco_hab <- sp_track_hab %>%
  dplyr::group_by(species, data, filter) %>%
  dplyr::summarise(ecoregion = length(unique(ecoregion)))

#--- flyway filter
sp_track_fly <- sp_track %>% 
  dplyr::semi_join(sp_fly, by= "flyway") %>% 
  dplyr::mutate(filter = "flyway") %>% #apply flyway filter
  dplyr::select(species, data, filter, ecoregion)#remove flyway column
#ecoregion
sp_track_eco_fly <- sp_track_fly %>%
  dplyr::group_by(species, data, filter) %>%
  dplyr::summarise(ecoregion = length(unique(ecoregion)))

#---habitat and flyway filter
sp_track_hab_fly <- sp_track %>% 
  dplyr::semi_join(sp_fly, by= "flyway") %>% #apply flyway filter
  dplyr::semi_join(sp_eco_type, by= c("species","eco_type")) %>% #apply habitat filter
  dplyr::mutate(filter= "flyway and habitat") %>% 
  dplyr::select(species, data, filter, ecoregion)#remove flyway column
#ecoregion
sp_track_eco_hab_fly <- sp_track_hab_fly %>%
  dplyr::group_by(species, data, filter) %>%
  dplyr::summarise(ecoregion = length(unique(ecoregion)))

#--------------------------------#
#### Combine all data sources ####
#--------------------------------#
ecoregion <- rbind.data.frame(ecoregion,
                              sp_track_eco_raw,
                              sp_track_eco_hab,
                              sp_track_eco_fly,
                              sp_track_eco_hab_fly)
#Export as .csv
write_csv(ecoregion, "data/species_region_filter.csv")
