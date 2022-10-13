#Title: Realized the Geoprocessing of the ecoregions and species stationnary non-breeding range shapefiles
#Author: Louis Moisan
#Creation date: May 2021
#Review date: September 1st 2022



##### INPUT FILES
# - data/shapefiles/raw/flyways.shp
# - data/shapefiles/raw/range_maps/species_range_maps/ebird.shp
# - data/shapefiles/raw/range_maps/species_range_maps/birdlife.shp
# - data/shapefiles/ecoregions/ecoregions.shp
# - data/shapefiles/raw/range_maps/tracking/crpl_winter_bylot.shp
# - data/shapefiles/raw/range_maps/tracking/amgp_winter_bylot.shp
# - data/shapefiles/raw/range_maps/tracking/ltja_winter_bylot.shp
# - data/shapefiles/raw/range_maps/tracking/snow_winter_bylot.shp
# - data/shapefiles/raw/range_maps/tracking/sngo_winter_bylot.shp

##### OUTPUT FILES
# - Multiple files

#------------------#
#### Librairies ####
#------------------#
library(sf)
sf::sf_use_s2(FALSE) # for the moment using s2 cause invalid geometries, but we the further developpment of the s2 package, considering turning s2 on to more accurate geocomputation
library(dplyr)
library(plyr)

### Message " although coordinates are longitude/latitude, st_intersection assumes that they are planar"
#   https://gis.stackexchange.com/questions/381446/choosing-projection-for-running-polygon-intersections-at-global-scale-i-e-geod

#---------------------------#
#### Read the Shapefiles ####
#---------------------------#
#-----------------Flyway
flyway <- sf::st_read("data/shapefiles/raw/flyways.shp") %>% 
  dplyr::mutate(flyway = plyr::revalue(layer, c("atlantic_americas"= "Atlantic Americas", "blacksea_mediterranean"= "Black Sea-Mediterranean", "central_asia"= "Central Asia", "central_pacific"= "Central Pacific", "east_asian_australasian"= "East Asian-Australian", "east_atlantic"= "East Atlantic", "mississippi_americas"= "Central Americas", "pacific_americas"= "Pacific Americas", "west_asian_east_african"= "West Asian-East African"))) %>% #Change the id of each flyway by his common name
  dplyr::select(flyway)#Removed unused columns

####---------- Ecoregions classifications
ecoregion <- sf::st_read("data/shapefiles/ecoregions/ecoregions.shp")#The classification of ecoregions was performed in the script 1-geoprocessing_ecoregions.R


  #-------------eBird
ebird <- sf::st_read("data/shapefiles/raw/range_maps/species_range_maps/ebird.shp") %>% 
  dplyr::filter(season_nam == "nonbreeding") %>%
  dplyr::mutate(species = common_nam) %>% #Change the species column name
  dplyr::filter(season_nam == "nonbreeding") %>%  #Select only non-breeding range
  dplyr::mutate(species = plyr::revalue(species, c("Baird's Sandpiper"= "Bairds Sandpiper"))) %>% #Rename Baird's Sandpiper to Bairds Sandpiper
  dplyr::rename( season= season_nam) %>% #Rename season_nam to season
  dplyr::select(species, scientific) %>% #Remove unsuded column
  dplyr::mutate(season= "non-breeding")#Add column season

#-------------Birdlife
birdlife <- sf::st_read("data/shapefiles/raw/range_maps/species_range_maps/birdlife.shp")%>%
  dplyr::filter(SEASONAL == "3") %>% #Select only non-breeding range with Birdlife
  dplyr::rename(scientific= SCINAME) %>% #Rename SCINAME to scientific
  dplyr::mutate(species = plyr::revalue(scientific, c("Anser caerulescens"= "Snow Goose", "Anthus rubescens"= "American Pipit", "Antigone canadensis" = "Sandhill Crane", "Arenaria interpres" = "Ruddy Turnstone", "Branta hutchinsii"= "Cackling Goose", "Bubo scandiacus"= "Snowy Owl","Buteo lagopus" = "Rough-legged Hawk", "Calcarius lapponicus"= "Lapland Longspur", "Calidris bairdii" = "Bairds Sandpiper",  "Calidris canutus"= "Red Knot",  "Calidris fuscicollis" = "White-rumped Sandpiper",  "Calidris melanotos"= "Pectoral Sandpiper", "Calidris subruficollis"= "Buff-breasted Sandpiper","Charadrius hiaticula"= "Common-ringed Plover" ,"Clangula hyemalis"= "Long-tailed Duck", "Cygnus columbianus"= "Tundra Swan", "Eremophila alpestris"= "Horned Lark", "Falco peregrinus"= "Peregrine Falcon", "Gavia pacifica"= "Pacific Loon", "Gavia stellata" = "Red-throated Loon", "Larus hyperboreus"= "Glaucous Gull","Phalaropus fulicarius"= "Red Phalarope" ,"Plectrophenax nivalis"= "Snow Bunting", "Pluvialis dominica"= "American Golden-Plover", "Pluvialis squatarola"="Black-bellied Plover", "Somateria spectabilis"= "King Eider","Stercorarius longicaudus"= "Long-tailed Jaeger" ,"Stercorarius parasiticus"= "Parasitic Jaeger"))) %>% #Change the species column name
  dplyr::select(species, scientific) %>% #removed unsuded colummn
  dplyr::mutate(season ="non-breeding") #Add a season column

#--------- Overlap between eBird-Birdlife
#eBird and Birdlife non-breeding range
ebird_birdlife <- sf::st_intersection(ebird, birdlife)
#Keep only polygon corresponding of the same species w. range overlap
ebird_birdlife<- ebird_birdlife[as.character(ebird_birdlife$scientific) == as.character(ebird_birdlife$scientific.1),]
#removed unused columns
ebird_birdlife <- ebird_birdlife %>% 
  dplyr::select(species, scientific, season)
#export as .shp
sf::st_write(ebird_birdlife, "data/shapefiles/ebird_birdlife.shp")

#--------------------------------------------------#
#### Overlap between non-breeding range maps, ecoregions and flyways ####
#-------------------------------------------------#
#---- eBird
#Ecoregions
ebird_eco <- sf::st_intersection(ebird, ecoregion)
sf::st_write(ebird_eco, "data/shapefiles/overlap_range_ecoregions/ebird_eco.shp")
#Ecoregions and flyways
ebird_eco_fly <- sf::st_intersection(ebird_eco, flyway)
sf::st_write(ebird_eco_fly, "data/shapefiles/overlap_range_ecoregions/ebird_eco_fly.shp")

#---- Birdlife
#Ecoregions
birdlife_eco <- sf::st_intersection(birdlife, ecoregion)
sf::st_write(birdlife_eco, "data/shapefiles/overlap_range_ecoregions/birdlife_eco.shp")
#Ecoregions and flyways
birdlife_eco_fly <- sf::st_intersection(birdlife_eco, flyway)
sf::st_write(birdlife_eco_fly, "data/shapefiles/overlap_range_ecoregions/birdlife_eco_fly.shp")


#Ecoregions
ebird_birdlife_eco <- sf::st_intersection(ebird_birdlife, ecoregion)
sf::st_write(ebird_birdlife_eco, "data/shapefiles/overlap_range_ecoregions/ebird_birdlife_eco.shp")
#Ecoregions and flyways
ebird_birdlife_eco_fly <- sf::st_intersection(ebird_birdlife_eco, flyway)
sf::st_write(ebird_birdlife_eco_fly, "data/shapefiles/overlap_range_ecoregions/ebird_birdlife_eco_fly.shp")


#-----------------------#
#### Tracked species ####
#-----------------------#
#
#Read ecoregion type associated to each species
sp_eco_type <-  read.csv("data/metadata/TableS3.SpeciesHabitat.csv")
sp_eco_type_v <- strsplit(sp_eco_type$Habitat.type, split = "\n")
sp_eco_type <-  data.frame(Species = rep(sp_eco_type$Species, sapply(sp_eco_type_v, length)), Habitat.type = unlist(sp_eco_type_v)) %>% 
  plyr::rename( c("Species"= "species", "Habitat.type" = "eco_type")) %>% 
  dplyr::mutate(eco_type= tolower(eco_type)) %>% 
  dplyr::mutate_all(na_if,"") %>% 
  na.omit() %>% 
  dplyr::arrange(species, eco_type)

#Species tracked directly from Bylot
species_track_code <- data.frame(species= c("Common-ringed Plover", "Snowy Owl", "Long-tailed Jaeger", "American Golden-Plover", "Snow Goose"), code= c("crpl", "snow", "ltja", "amgp", "sngo"))
species_track_range <- c() 

for (i in species_track_code$code){
  range <- sf::st_read(paste("data/shapefiles/raw/range_maps/tracking/",i,"_winter_bylot.shp", sep = "")) %>%  
  sf::st_make_valid() %>% 
    dplyr::mutate(species = species_track_code[which(species_track_code$code == i), ]$species)
  
  range_eco <- sf::st_intersection(range, ecoregion)%>% 
    dplyr::semi_join(sp_eco_type) %>% 
  sf::st_collection_extract("POLYGON")
  sf::st_write(range_eco, paste("data/shapefiles/overlap_range_ecoregions/",i,"_winter_bylot_eco.shp", sep = ""))
  
  range_eco_fly <- sf::st_intersection(range_eco, flyway)%>% 
  dplyr::select(species, ecoregion, province, realm, eco_type, flyway)
  sf::st_write(range_eco_fly, paste("data/shapefiles/overlap_range_ecoregions/",i,"_winter_bylot_eco_fly.shp", sep = ""))
  
  species_track_range[[i]] <- range_eco_fly
}

#Add King Eider tracked from East Bay
kiei_range <- sf::st_read("data/shapefiles/raw/range_maps/tracking/kiei_winter_east_bay.shp") %>%  
  sf::st_make_valid() %>% 
  dplyr::mutate(species = "King Eider")

kiei_range_eco <- sf::st_intersection(kiei_range, ecoregion)%>%
  dplyr::semi_join(sp_eco_type) %>% 
  sf::st_collection_extract("POLYGON")
sf::st_write(kiei_range_eco, "data/shapefiles/overlap_range_ecoregions/kiei_winter_east_bay_eco.shp")

kiei_range_eco_fly <- sf::st_intersection(kiei_range_eco, flyway)%>% 
  dplyr::select(species, ecoregion, province, realm, eco_type, flyway)
sf::st_write(kiei_range_eco_fly, "data/shapefiles/overlap_range_ecoregions/kiei_winter_east_bay_eco_fly.shp")

species_track_range[["kiei"]] <- kiei_range_eco_fly


#--- Combine tracked species into a single shapefile
sp_track_eco_fly <- rbind(species_track_range$crpl, species_track_range$snow, species_track_range$ltja, species_track_range$amgp, species_track_range$sngo, species_track_range$kiei)
sf::st_write(sp_track_eco_fly, "data/shapefiles/overlap_range_ecoregions/sp_track_eco_fly.shp")

#----------------------------------------#
#### Define Partially migratory range ####
#----------------------------------------#
#Define the Eckert IV equal area CRS
crs <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

bylot <- sf::st_point(c(-80.0000, 73.000)) %>%
  sf::st_sfc(crs = 4326) %>%
  sf::st_transform(crs)

#--- Arctic fox buffer of 500 km (Lai et al., 2016)
fox_buffer <- sf::st_buffer(bylot, 500000) %>% 
  sf::st_transform(4326)
fox_eco <- sf::st_intersection(ecoregion, fox_buffer)#Ecoregion
fox_eco$species <- "Arctic Fox"

#--- Common raven buffer of 100 km
raven_buffer <- sf::st_buffer(bylot, 100000) %>% 
  sf::st_transform(4326)
raven_eco <- sf::st_intersection(ecoregion, raven_buffer)#Ecoregion
raven_eco$species <- "Common Raven"

#---Combine all partial migrants
partial_migrant_eco <- rbind(fox_eco, raven_eco)
#sf::st_write(partial_migrant_eco, "data/shapefiles/overlap_range_ecoregions/partial_migrant_eco.shp")



#--------------------------------------------------------------#
#### Overlap between tracked species and eBird and Birdlife ####
#--------------------------------------------------------------#
#----- eBird
ebird_track <- sf::st_intersection(ebird, sp_track_eco_fly)
#Keep only polygon corresponding of the same species w. range overlap
ebird_track<- ebird_track[as.character(ebird_track$species) == as.character(ebird_track$species.1),] %>% 
  dplyr::select(species, ecoregion, province, realm, eco_type, flyway)
#export as .shp
sf::st_write(ebird_track, "data/shapefiles/ebird_track.shp")

#----- Birdlife
birdlife_track <- sf::st_intersection(birdlife, sp_track_eco_fly)
#Keep only polygon corresponding of the same species w. range overlap
birdlife_track<- birdlife_track[as.character(birdlife_track$species) == as.character(birdlife_track$species.1),] %>%
  dplyr::select(species, ecoregion, province, realm, eco_type, flyway)
#export as .shp
sf::st_write(birdlife_track, "data/shapefiles/birdlife_track.shp")

#----- eBird-Birdlife
ebird_birdlife <- sf::read_sf("data/shapefiles/ebird_birdlife.shp")
ebird_birdlife_track <- sf::st_intersection(ebird_birdlife, sp_track_eco_fly)
#Keep only polygon corresponding of the same species w. range overlap
ebird_birdlife_track<- ebird_birdlife_track[as.character(ebird_birdlife_track$species) == as.character(ebird_birdlife_track$species.1),] %>%
  dplyr::select(species, ecoregion, province, realm, eco_type, flyway)
#export as .shp
sf::st_write(ebird_birdlife_track, "data/shapefiles/ebird_birdlife_track.shp")

