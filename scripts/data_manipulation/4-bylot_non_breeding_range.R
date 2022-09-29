#Title: Mapping the distribution of the non-breeding range centroids of migratory species from Bylot Island
#Author: Louis Moisan
#Creation Date: June 5 2021
#Review date: September 2 2022

sf_use_s2(FALSE)
###!!! Message " although coordinates are longitude/latitude, st_intersection assumes that they are planar"
#   https://gis.stackexchange.com/questions/381446/choosing-projection-for-running-polygon-intersections-at-global-scale-i-e-geod

##### INPUT FILES
# - data/shapefiles/overlap_range_ecoregions/ebird_eco_fly.shp
# - data/shapefiles/overlap_range_ecoregions/birdlife_eco_fly.shp
# - data/shapefiles/overlap_range_ecoregions/ltja_winter_bylot_eco.shp
# - data/shapefiles/overlap_range_ecoregions/amgp_winter_bylot_eco.shp
# - data/shapefiles/overlap_range_ecoregions/crpl_winter_bylot_eco.shp
# - data/shapefiles/overlap_range_ecoregions/snow_winter_bylot_eco.shp
# - data/shapefiles/overlap_range_ecoregions/sngo_winter_bylot_eco.shp
# - data/metadata/TableS2.SpeciesFlyway.csv
# - data/metadata/TableS3.SpeciesHabitat.csv
# - data/metadata/TableS1.SpeciesRange.csv

##### OUTPUT FILES
# - data/shapefiles/bylot_non_breeding_range.shp



#------------------#
#### Librairies ####
#------------------#
library(dplyr)
library(sf) 

#-------------------#
#### Import data ####
#-------------------#
#Species non-breeding distributions
ebird <- sf::st_read("data/shapefiles/overlap_range_ecoregions/ebird_eco_fly.shp")

birdlife <- sf::st_read("data/shapefiles/overlap_range_ecoregions/birdlife_eco_fly.shp")

ebird_birdlife <- sf::st_read("data/shapefiles/overlap_range_ecoregions/ebird_birdlife_eco_fly.shp")

species_track_code <- data.frame(species= c("Common-ringed Plover", "Snowy Owl", "Long-tailed Jaeger", "American Golden-Plover", "Snow Goose"), code= c("crpl", "snow", "ltja", "amgp", "sngo"))

species_tracked <- c()

for (i in species_track_code$code){
  sp <- sf::st_read(paste("data/shapefiles/overlap_range_ecoregions/",i
                          ,"_winter_bylot_eco.shp", sep = "")) %>%
                      dplyr::select(species, geometry)%>% 
                      dplyr::group_by(species) %>% 
                      dplyr::summarise()
  
  species_tracked[[i]] <- sp
}


#--- Filters general species range by flyway
#Read species-flyway database
sp_fly <-  read.csv("data/metadata/TableS2.SpeciesFlyway.csv")
sp_fly_v <- strsplit(sp_fly$Flyway, split = "\n")
sp_fly <-  data.frame(Species = rep(sp_fly$Species, sapply(sp_fly_v, length)), Flyway = unlist(sp_fly_v))%>% 
  plyr::rename( c("Species"= "species", "Flyway" = "flyway")) %>% 
  dplyr::arrange(species, flyway)
#Filter general range by species and flyway
ebird <- dplyr::semi_join(ebird, sp_fly, by = c("species", "flyway"))
birdlife <- dplyr::semi_join(birdlife, sp_fly, by = c("species", "flyway"))
ebird_birdlife <- dplyr::semi_join(ebird_birdlife, sp_fly, by = c("species", "flyway"))


#--- Filters general species range by habitat type
#Read ecoregion type associated to each species
sp_eco_type <-  read.csv("data/metadata/TableS3.SpeciesHabitat.csv")
sp_eco_type_v <- strsplit(sp_eco_type$Habitat.type, split = "\n")
sp_eco_type <-  data.frame(Species = rep(sp_eco_type$Species, sapply(sp_eco_type_v, length)), Habitat.type = unlist(sp_eco_type_v)) %>% 
  plyr::rename( c("Species"= "species", "Habitat.type" = "eco_type")) %>% 
  dplyr::mutate(eco_type= tolower(eco_type)) %>% 
  dplyr::mutate_all(na_if,"") %>% 
  na.omit() %>% 
  dplyr::arrange(species, eco_type)

#Filter general range by species and habitat type
ebird <- dplyr::semi_join(ebird, sp_eco_type, by = c("species", "eco_type"))
birdlife <- dplyr::semi_join(birdlife, sp_eco_type, by = c("species", "eco_type"))
ebird_birdlife <- dplyr::semi_join(ebird_birdlife, sp_eco_type, by = c("species", "eco_type"))


#Select only most representative non-breeding range for each species
sp_range <-  read.csv("data/metadata/TableS1.SpeciesRange.csv") %>% 
  dplyr::filter(Non.breeding..strategy %in% c("Partial migrant", "Migrant")) %>% 
  dplyr::select(Species, Range.selected) %>% 
  dplyr::mutate(Species= gsub("\n.*","",Species)) %>% 
  plyr::rename(c("Species"= "species", "Range.selected"= "data")) %>% 
  dplyr::mutate(data= tolower(gsub("-", "_", data))) %>% 
  dplyr::mutate(data= gsub(".*km buffer", "buffer zone", data)) %>% 
  dplyr::mutate(data= gsub("tracking and ", "", data)) %>% 
  dplyr::arrange(species)



#----- eBird
ebird <-  ebird %>% 
  dplyr::filter(species %in% sp_range[sp_range$data == "ebird",]$species) %>% 
  dplyr::select(species, geometry)%>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise()

#----- Birdlife
birdlife <- birdlife %>% 
  dplyr::filter(species %in% sp_range[sp_range$data == "birdlife",]$species) %>% 
  dplyr::select(species, geometry)%>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise()

#----- eBird-Birdlife
ebird_birdlife <- ebird_birdlife %>% 
  dplyr::filter(species %in% sp_range[sp_range$data == "ebird_birdlife",]$species)%>% 
  dplyr::select(species, geometry)%>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise()

#Combine all shapefiles in one
non_breeding_range <- rbind(ebird, birdlife, ebird_birdlife, bind_rows(species_tracked))%>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise()

sf::write_sf(non_breeding_range, "data/shapefiles/bylot_non_breeding_range.shp")
