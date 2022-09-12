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
  filter(non_breeding_strategy == "Migrant")



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
#---- Combine Ecoregions data frames
list_eco <- c(ebird_eco_list, birdlife_eco_list, ebird_birdlife_eco_list)
ecoregion <- data.frame(species= character(), ecoregion= numeric(), data= character(), filter= character())
for(df in names(list_eco)){
 ecoregion <-  rbind(ecoregion, list_eco[[df]])
}
#Remove resident species
ecoregion<- ecoregion[!(ecoregion$species %in% species_meta[species_meta$non_breeding_strategy == "Resident",]$species), ]
#Remove partial migrants from flyway filter
ecoregion<- ecoregion %>% filter(!(filter %in% c("flyway", "flyway and habitat") & species %in% species_meta[species_meta$non_breeding_strategy == "Partial migrant",]$species))


#--- Habitat filter
#filter by habitat
#Read ecoregion type associated to each species
sp_eco_type <-  read.csv("data/metadata/TableS3.SpeciesHabitat.csv")
sp_eco_type_v <- strsplit(sp_eco_type$Habitat.type, split = "\n")
sp_eco_type <-  data.frame(Species = rep(sp_eco_type$Species, sapply(sp_eco_type_v, length)), Habitat.type = unlist(sp_eco_type_v)) %>% 
  plyr::rename( c("Species"= "species", "Habitat.type" = "eco_type")) %>% 
  dplyr::mutate(eco_type= tolower(eco_type)) %>% 
  dplyr::mutate_all(na_if,"") %>% 
  na.omit() %>% 
  dplyr::arrange(species, eco_type)


#------------------------#
#### Tracking devices ####
#------------------------#
#---- snowy owls
snow_eco_fly <-  sf::st_read("data/shapefiles/overlap_range_ecoregions/snow_winter_bylot_eco_fly.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(species,ecoregion, eco_type, flyway) %>% 
  unique()

#---- Common-Ringed plover
crpl_eco_fly <-  sf::st_read("data/shapefiles/overlap_range_ecoregions/crpl_winter_bylot_eco_fly.shp") %>%
  sf::st_drop_geometry() %>% 
  dplyr::select(species, ecoregion, eco_type, flyway) %>% 
  unique()

#---- American-golden Plover
amgp_eco_fly <-  sf::st_read("data/shapefiles/overlap_range_ecoregions/amgp_winter_bylot_eco_fly.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(species, ecoregion, eco_type, flyway) %>% 
  unique()

#--- Long-tailed jaeger
ltja_eco_fly <-  sf::st_read("data/shapefiles/overlap_range_ecoregions/ltja_winter_bylot_eco_fly.shp")%>% #marine
  sf::st_drop_geometry()%>% 
  dplyr::select(species, ecoregion, eco_type, flyway) %>% 
  unique()

#---Combine all species
sp_track <- rbind.data.frame(snow_eco_fly, crpl_eco_fly, amgp_eco_fly, ltja_eco_fly)
#add data type
sp_track$data <- "tracking"

#--- Raw
sp_track_raw <- sp_track %>% 
  dplyr::select(species, data, ecoregion, eco_type) %>% 
  unique()
sp_track_raw$filter <- "raw"
#ecoregion
sp_track_eco_raw <- sp_track_raw %>%
  dplyr::group_by(species, data, filter) %>%
  dplyr::summarise(ecoregion = length(unique(ecoregion)))


#--- Habitat filter
sp_track_hab <- sp_track_raw
#apply filter
sp_track_hab <- semi_join(sp_track_hab, sp_eco_type, by= c("species","eco_type"))
sp_track_hab$filter <- "habitat"
#ecoregion
sp_track_eco_hab <- sp_track_hab %>% dplyr::group_by(species, data, filter) %>% dplyr::summarise(ecoregion = length(unique(ecoregion)))

#--- flyway filter
#read flyway meta-data
#Read the database
sp_fly <-  read.csv("data/metadata/TableS2.SpeciesFlyway.csv")
sp_fly_v <- strsplit(sp_fly$Flyway, split = "\n")
sp_fly <-  data.frame(Species = rep(sp_fly$Species, sapply(sp_fly_v, length)), Flyway = unlist(sp_fly_v))%>% 
  plyr::rename( c("Species"= "species", "Flyway" = "flyway")) %>% 
  dplyr::arrange(species, flyway)

sp_track_fly <- sp_track
#apply flyway filter
sp_track_fly <- semi_join(sp_track_fly, sp_fly, by= "flyway")
sp_track_fly$filter <- "flyway"
#remove flyway column
sp_track_fly <- sp_track_fly %>% 
  dplyr::select(species, data, filter, ecoregion)
#ecoregion
sp_track_eco_fly <- sp_track_fly %>% dplyr::group_by(species, data, filter) %>% dplyr::summarise(ecoregion = length(unique(ecoregion)))


#---habitat and flyway filter
sp_track_hab_fly <- sp_track
#apply flyway filter
sp_track_hab_fly <- semi_join(sp_track_hab_fly, sp_fly, by= "flyway")
#apply habitat filter
sp_track_hab_fly <- semi_join(sp_track_hab_fly, sp_eco_type, by= c("species","eco_type"))
sp_track_hab_fly$filter <- "flyway and habitat"
#remove flyway column
sp_track_hab_fly <- sp_track_hab_fly %>% 
  dplyr::select(species, data, filter, ecoregion)
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
