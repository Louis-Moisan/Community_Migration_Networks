#Title: Extract adjency matrix of ecoregions used by each migratory species of Bylot Island during wintering
#Author: Louis Moisan
#Creation Date: January 27 2021


###!!! Message " although coordinates are longitude/latitude, st_intersection assumes that they are planar"
#   https://gis.stackexchange.com/questions/381446/choosing-projection-for-running-polygon-intersections-at-global-scale-i-e-geod

#------------------#
#### Librairies ####
#------------------#
library(dplyr)
library(sf)
sf::sf_use_s2(FALSE) # for the moment using s2 cause invalid geometries, but we the further developpment of the s2 package, considering turning s2 on to more accurate geocomputation
library(plyr)
source("scripts/functions/1_functions_data_manip.R")

#----------------------------------------#
#### Import ecoregion type by species ####
#----------------------------------------#
#Read ecoregion type associated to each species
sp_eco_type <-  read.csv("data/metadata/TableS3.SpeciesHabitat.csv")
sp_eco_type_v <- strsplit(sp_eco_type$Habitat.type, split = "\n")
sp_eco_type <-  data.frame(Species = rep(sp_eco_type$Species, sapply(sp_eco_type_v, length)), Habitat.type = unlist(sp_eco_type_v)) %>% 
  plyr::rename( c("Species"= "species", "Habitat.type" = "eco_type")) %>% 
  dplyr::mutate(eco_type= tolower(eco_type)) %>% 
  dplyr::mutate_all(na_if,"") %>% 
  na.omit() %>% 
  dplyr::arrange(species, eco_type)

eco_type <- read.csv("data/metadata/ecoregion_type.csv")

#--------------------------------#
#### Import species meta-data ####
#--------------------------------#
sp_meta_data <- read.csv("data/metadata/species_colors.csv")


#------------------------------------------------#
#### Import flyway associated to each species ####
#------------------------------------------------#
sp_fly <-  read.csv("data/metadata/TableS2.SpeciesFlyway.csv")
sp_fly_v <- strsplit(sp_fly$Flyway, split = "\n")
sp_fly <-  data.frame(Species = rep(sp_fly$Species, sapply(sp_fly_v, length)), Flyway = unlist(sp_fly_v))%>% 
  plyr::rename( c("Species"= "species", "Flyway" = "flyway")) %>% 
  dplyr::arrange(species, flyway)

#-------------------------------#
#### Partial migrant species ####
#-------------------------------#
partial_migrant_eco <- sf::st_read("data/shapefiles/overlap_range_ecoregions/partial_migrant_eco.shp") %>%
  sf::st_drop_geometry() %>%
  dplyr::select(species, ecoregion, eco_type) %>% 
  unique()
#export for sensibility analysis
write.csv(partial_migrant_eco, "data/adjency_matrix/partial_migrant.raw.csv")

#--- resident species
resident_eco <- sp_meta_data %>% 
  dplyr::filter(non_breeding_strategy =="Resident") %>% 
  dplyr::filter(trophic_level > 0) %>% 
  dplyr::filter(func_group != "Arthropods") %>% 
  dplyr::select(species) %>% 
  dplyr::mutate(ecoregion = NA, eco_type =NA) %>% 
  unique()

#--- Combine all short distance migrant with resident_sp under a single data frame
bylot_sp <- rbind(resident_eco, partial_migrant_eco)
bylot_sp$flyway <- NA

  
#----------------------------------#
#### Extract incidency matrices ####
#----------------------------------#
#Ebird
extract_adjency_m(shp= "data/shapefiles/overlap_range_ecoregions/ebird_eco_fly.shp",
                  shp_track = "data/shapefiles/ebird_track.shp",
                  sp_list= sp_meta_data,
                  resident_sp = bylot_sp,
                  sp_habitat= sp_eco_type,
                  sp_flyway= sp_fly,
                  data= "ebird",
                  output_path= "data/adjency_matrix/")


#Birdlife
extract_adjency_m(shp= "data/shapefiles/overlap_range_ecoregions/birdlife_eco_fly.shp",
                  shp_track = "data/shapefiles/birdlife_track.shp",
                  sp_list= sp_meta_data,
                  resident_sp = bylot_sp,
                  sp_habitat= sp_eco_type,
                  sp_flyway= sp_fly,
                  data= "birdlife",
                  output_path= "data/adjency_matrix/")
#Ebird-Birdife
extract_adjency_m(shp= "data/shapefiles/overlap_range_ecoregions/ebird_birdlife_eco_fly.shp",
                  shp_track = "data/shapefiles/ebird_birdlife_track.shp",
                  sp_list= sp_meta_data,
                  resident_sp = bylot_sp,
                  sp_habitat= sp_eco_type,
                  sp_flyway= sp_fly,
                  data= "ebird_birdlife",
                  output_path= "data/adjency_matrix/")

#Absence of Long-tailed Jaeger, Common-ringed plover, Red Phalarope from Ebird shapefiles
#Absence of King Eider because birds from Bylot used the East Atlantic Flyway to reach Greenland instead of the non breeding area define on the Atlantic Coast by the Ebird non breeding range
#Absence of Parasitic Jaeger beacause ebird range is only a small part in pacific south america not corresponding to flyway


#--------------------------------------------------#
#### Optimal network  ecoregions adjency matrix ####
#--------------------------------------------------#
sp_range <-  read.csv("data/metadata/TableS1.SpeciesRange.csv") %>% 
  dplyr::filter(Non.breeding..strategy %in% c("Partial migrant", "Migrant")) %>% 
  dplyr::select(Species, Range.selected) %>% 
  dplyr::mutate(Species= gsub("\n.*","",Species)) %>% 
  plyr::rename(c("Species"= "species", "Range.selected"= "data")) %>% 
  dplyr::mutate(data= tolower(gsub("-", "_", data))) %>% 
  dplyr::mutate(data= gsub(".*km buffer", "buffer zone", data)) %>% 
  dplyr::mutate(data= gsub("tracking and ", "", data)) %>% 
  dplyr::arrange(species)


#---- Birdlife filter by habitat and flyway ecoregions data frame
birdlife_eco_hab_fly <- sf::st_read("data/shapefiles/overlap_range_ecoregions/birdlife_eco_fly.shp") %>% 
  dplyr::semi_join(sp_fly, by= c("species","flyway")) %>% #flyway filter
  dplyr::semi_join(sp_eco_type, by= c("species","eco_type")) %>% 
  dplyr::select(species, ecoregion, province, realm, eco_type, geometry) %>% 
  dplyr::group_by(species, realm, province, ecoregion, eco_type) %>% 
  dplyr::summarize() #habitat filter


#---- eBird filter by habitat and flyway ecoregions data frame
ebird_eco_hab_fly <- sf::st_read("data/shapefiles/overlap_range_ecoregions/ebird_eco_fly.shp") %>% 
  dplyr::semi_join(sp_fly, by= c("species","flyway")) %>% #flyway filter
  dplyr::semi_join(sp_eco_type, by= c("species","eco_type")) %>% 
  dplyr::select(species, ecoregion, province, realm, eco_type, geometry) %>% 
  dplyr::group_by(species, realm, province, ecoregion, eco_type) %>% 
  dplyr::summarize() #habitat filter

#---- eBird-Birdlife filter by habitat and flyway ecoregions data frame
ebird_birdlife_eco_hab_fly <- sf::st_read("data/shapefiles/overlap_range_ecoregions/ebird_birdlife_eco_fly.shp") %>% 
  dplyr::semi_join(sp_fly, by= c("species","flyway")) %>% #flyway filter
  dplyr::semi_join(sp_eco_type, by= c("species","eco_type")) %>% 
  dplyr::select(species, ecoregion, province, realm, eco_type, geometry) %>% 
  dplyr::group_by(species, realm, province, ecoregion, eco_type) %>% 
  dplyr::summarize() #habitat filter

#----- Selecting species using Ebird-Flyway ecoregions
optimal_ebird <-  ebird_eco_hab_fly %>% 
  dplyr::filter(species %in% sp_range[sp_range$data == "ebird",]$species) %>% 
  dplyr::group_by(species, realm, province, ecoregion, eco_type) %>% 
  dplyr::summarize()

#---- Selecting species using Birdlife-Flyway ecoregions
optimal_birdlife <-  birdlife_eco_hab_fly %>% 
  dplyr::filter(species %in% sp_range[sp_range$data == "birdlife",]$species) %>% 
  dplyr::group_by(species, realm, province, ecoregion, eco_type) %>% 
  dplyr::summarize()

#----- Selecting species using Ebird-Birdlife-Flyway ecoregions
optimal_ebird_birdlife <-  ebird_birdlife_eco_hab_fly %>% 
  dplyr::filter(species %in% sp_range[sp_range$data == "ebird_birdlife",]$species) %>% 
  dplyr::group_by(species, realm, province, ecoregion, eco_type) %>% 
  dplyr::summarize()

#---- Tracked species
optimal_tracked_species <- sf::st_read("data/shapefiles/overlap_range_ecoregions/sp_track_eco_fly.shp") %>% #habitat filter
  dplyr::semi_join(sp_eco_type, by= c("species","eco_type")) %>% 
  dplyr::group_by(species, realm, province, ecoregion, eco_type) %>% 
  dplyr::summarize()

#---- Combine all data source together
partial_migrant_eco <- sf::st_read("data/shapefiles/overlap_range_ecoregions/partial_migrant_eco.shp")
optimal_eco <- rbind.data.frame(optimal_ebird,
                                optimal_birdlife,
                                optimal_ebird_birdlife,
                                optimal_tracked_species,
                                partial_migrant_eco) 
#Export the ecoregions inside each migratory and partially migratory species from Bylot
sf::st_write(optimal_eco,"data/shapefiles/overlap_range_ecoregions/bylot_non_breeding_range_eco.shp")

optimal_eco_df <- optimal_eco %>% 
  sf::st_drop_geometry() %>% 
  dplyr::group_by(species, ecoregion, eco_type) %>% 
  unique() %>% 
  rbind(resident_eco) %>% 
  dplyr::select(species, ecoregion)
  
#---Matrix
optimal_eco <- table(unique(optimal_eco_df[, c("species", "ecoregion")]))
#----- Export as .csv
write.csv(optimal_eco, "data/adjency_matrix/optimal_eco.csv")
