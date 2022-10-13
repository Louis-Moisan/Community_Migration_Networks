#Title: Prepare  and uniformize the terrestrial, marine and coastal ecoergions classifications
#Author: Louis Moisan
#Creation date: May 2021
#Review date: September 1 2022


##### INPUT FILES
# - data/shapefiles/raw/ecoregions/wwf_terr_ecos.shp
# - data/shapefiles/raw/ecoregions/Freshwater_Ecoregions.shp
# - data/shapefiles/ecoregions/coastal_ecoregions_pre_process.shp
# - data/shapefiles/ecoregions/marine_ecoregions_pre_process.shp

##### OUTPUT FILES
# - data/shapefiles/ecoregions/terrestrial_ecoregions.shp
# - data/shapefiles/ecoregions/coastal_ecoregions.shp
# - data/shapefiles/ecoregions/marine_ecoregions.shp
# - data/shapefiles/ecoregions/ecoregions.shp
# - data/metadata/ecoregion_type.csv


#------------------#
#### Librairies ####
#------------------#
library(sf)
sf::sf_use_s2(FALSE) # for the moment using s2 cause invalid geometries, but with the further development of the s2 package, considering turning s2 on to more accurate geocomputation
library(dplyr)
library(plyr)


#-------------------------------------------#
#### Terrestrial Ecoregions of the World ####
#-------------------------------------------#
#Download Terrestrial Ecoregions: https://www.worldwildlife.org/publications/terrestrial-ecoregions-of-the-world
#Import raw data
terrestrial <- sf::st_read("data/shapefiles/raw/ecoregions/wwf_terr_ecos.shp") %>% 
  sf::st_make_valid() %>%  # Make sure there is no invalid geometries
  dplyr::mutate_at(c("ECO_NAME", "REALM", "BIOME"), as.factor) #Change some variables to factor

# ! The Lake ecoregions are not defined as specific ecoregions, so we will use the freshwater ecoregions of the globe to associated specific ecoregions to large lakes !

#Download Freshwater Ecoregions :https://geospatial.tnc.org/datasets/38da4656e8074e1c820c42cc21cd76cd_0/explore?location=-0.867642%2C0.000000%2C2.20&showTable=true
freshwater <- sf::st_read("data/shapefiles/raw/ecoregions/Freshwater_Ecoregions.shp") %>% 
  sf::st_transform(4326) %>% 
  sf::st_make_valid() %>% 
  dplyr::mutate_at(c("ECOREGION", "MHT_TXT"), as.factor) %>% 
  plyr::rename( c("MHT_TXT" = "biome", "ECOREGION"= "ecoregion")) #Change the name to combine with terrestrial
  #the Major Habitat Type (MHT) are similar to biome, but not reviewed entirely for the moment

#Associate freshwater ecoregions to lake
lake <- terrestrial %>% 
  dplyr::filter(ECO_NAME == c("Lake")) %>% 
  sf::st_intersection(freshwater)

#Remove Rock and Ice and lake since they are not considered as ecoregions in the terrestrial classification
terrestrial <- terrestrial %>% 
  dplyr::filter(ECO_NAME != c("Lake", "Rock and Ice"))

#Add lakes ecoregions
terrestrial <-  dplyr::full_join(as_tibble(terrestrial), dplyr::as_tibble(lake), by = "geometry") %>% 
  sf::st_as_sf() %>%
  dplyr::mutate(ecoregion = dplyr::coalesce(ECO_NAME.x,ecoregion))
#Combine the lake ecoregions and terrestrial ecoregions

#Set the biome name
terrestrial$BIOME.x<- plyr::revalue(terrestrial$BIOME.x, c("1"= "Tropical and Subtropical Moist Broadleaf Forests", "2"= "Tropical and Subtropical Dry Broadleaf Forests", "3"= "Tropical and Subtropical Coniferous Forests", "4"= "Temperate Broadleaf & Mixed Forests", "5"= "Temperate Conifer Forests", "6"="Boreal Forests/Taiga", "7"= "Tropical and Subtropical Grasslands, Savannas and Shrublands", "8"= "Temperate Grasslands, Savannas and Shrublands","9"= "Flooded Grasslands and Savannas", "10"= "Montane Grasslands and Shrublands", "11"= "Tundra", "12"= "Mediterranean Forests, Woodlands and Scrub", "13"= "Deserts and Xeric Shrublands", "14"="Mangroves", "98"= "Lake", "99"= "Rock and Ice")) 

#Combine the lake biome and terrestrial biomes
terrestrial <- terrestrial %>%
  dplyr::mutate(biome = dplyr::coalesce(BIOME.x,biome))

#Set the realm name
terrestrial$realm <- plyr::revalue(terrestrial$REALM.x, c("NA"= "Nearctic", "NT"= "Neotropical", "AA"= "Australian", "AN"= "Antarctic", "AT"= "Afrotropical", "IM"= "Indomalayan", "OC"= "Oceanian", "PA"= "Palearctic"))

#Create a separate file for mangroves that will be add to coastal ecoregions
mangroves <- terrestrial %>% 
  dplyr::filter(biome == "Mangroves")

#Remove Mangroves from terrestrial ecoregions (it will be associated with coastal ecoregions instead)
terrestrial <- terrestrial %>% 
  dplyr::filter(biome != "Mangroves")

#Create a biogeographic province column using a combination of biomes and realms
terrestrial$province <- paste(terrestrial$realm, terrestrial$biome)

#Add a column for the ecoregion type
terrestrial$eco_type <- "terrestrial"

#Removed unecessary columns
terrestrial <- terrestrial %>% 
  dplyr::select(ecoregion, province, realm, eco_type)

#Export  shapefile
sf::write_sf(terrestrial, "data/shapefiles/ecoregions/terrestrial_ecoregions.shp")

#----------------------------------------#
#### Coastal Ecoregions of the World #####
#----------------------------------------#
#!!! Steps precedently done in QGIS because computer performance is not enough with R !!!
# 1.Dissolve terrestrial ecoregions to resolved boundaries (Tool: Dissolve)
# 2.Group into a single object (QGIS tool: Multipart to singlepart)
# 3.Extract costaline of the world (QGIS tool: Polygons to lines)
# 4.Reproject layer with a world projection with meters unit (QGIS tool: Reproject layer (with espg 3857))
# 5.Define a coastal region of 3 km of both side of coastline (QGIS tool: Buffer (with 3 km, boundaries dissolved))
# 7.Manually correct geoprocessing error, remove large parrallel stripes (QGIS tool: Toggle layer)
# 8.Reproject layer in WGS 84 (QGIS tool: Reproject layer (with espg 4326))
# 9.Assign marine ecoregion boundaries (QGIS tool: Intersection (with raw marine ecoregions))
# 10.Remove overlap with mangroves (QGIS tool: Difference (with terrestrial biomes mangroves))
# 11.Add mangroves to coastal ecoregions (QGIS tool: Merge vector layer (with terrestrial biomes mangroves))

#Import the coastal ecoregion
coastal <- sf::st_read("data/shapefiles/ecoregions/coastal_ecoregions_pre_process.shp", stringsAsFactors=FALSE)

#Set the mangrove ecoregion name in the same column as marine ecoregion
coastal[coastal$layer == "Intersection", ]$ECOREGION <- coastal[coastal$layer == "Intersection", ]$ECO_NAME 

#Add coastal to each marine ecoregion name
coastal[coastal$layer == "Difference",]$ECOREGION <- paste("Coastal", coastal[coastal$layer == "Difference",]$ECOREGION)

#Add a column for the ecoregion type
coastal$eco_type <- "coastal"

#Add coastal to each realm (The column "REALM_2" represent the good one)
coastal$REALM <- paste("Coastal", coastal$REALM_2)
coastal$PROVINCE <- paste("Coastal", coastal$PROVINCE)
coastal <- plyr::rename(coastal,c("REALM" = "realm", "ECOREGION" = "ecoregion", "PROVINCE"= "province"))

#Removed unused columns
coastal <- coastal %>% 
  dplyr::select(ecoregion, province, realm, eco_type)

#Export  shapefile
sf::write_sf(coastal, "data/shapefiles/ecoregions/coastal_ecoregions.shp")




#----------- Marine Ecoregions of the Wolrd
#Download link: https://www.worldwildlife.org/publications/marine-ecoregions-of-the-world-a-bioregionalization-of-coastal-and-shelf-areas#:~:text=Marine%20Ecoregions%20of%20the%20World%20(MEOW)%20is%20a%20biogeographic%20classification,link%20to%20existing%20regional%20systems.
#--- Import data
#marine <- sf::st_read("data/shapefiles/raw/ecoregions/meow_ecos.shp") %>% 
 # sf::st_make_valid() %>%  #Correct invalid geometries 
  #dplyr::mutate_at(c("ECOREGION", "REALM", "PROVINCE"), as.factor) #Change some variable to factor

#Change column names to eventually combine with marine and coastal ecoregions
#marine <- plyr::rename(marine,c("REALM" = "realm", "ECOREGION" = "ecoregion", "PROVINCE"= "province"))

#Add the eco_type to each marine ecoregion
#marine$eco_type <- "marine"

#Remove the unused columns
#marine <-  marine %>%
 # dplyr::select(ecoregion, province, realm, eco_type)

#Cut the terrestrial part
#marine <- sf::st_difference(marine, terrestrial)

#Cut the coastal part
#marine <- sf::st_difference(marine, coastal)

#--- Geoprocessing in QGIS
#!!! I define the alternative option, because my computer can't run all the geoprocessing step in R !!!
#1. Remove terrestrial region of marine ecoregion (QGIS tool: Difference (with terrestrial ecoregions boundaries))
#2. Remove coastal region of marine ecoregion (QGIS tool: Difference (with coastal ecoregions boundaries))
#3. Correct invalid geometry (Fix Geometry)
marine <- sf::st_read("data/shapefiles/ecoregions/marine_ecoregions_pre_process.shp") %>% 
  plyr::rename(c("REALM" = "realm", "ECOREGION" = "ecoregion", "PROVINCE"= "province")) %>% 
dplyr::mutate(eco_type = "marine") %>% 
  dplyr::select(ecoregion, province, realm, eco_type)
  

#Export  shapefile
sf::write_sf(marine, "data/shapefiles/ecoregions/marine_ecoregions.shp")


#---------- Combine all types of ecoregions together
ecoregion <- rbind.data.frame( 
  terrestrial,
  marine,
  coastal
)

#--- Export as shapefile
sf::write_sf(ecoregion, "data/shapefiles/ecoregions/ecoregions.shp")


#-----------------------------------------------#
#### Data frame with ecoregion name and type ####
#-----------------------------------------------#
eco_type <- ecoregion %>% 
  sf::st_drop_geometry() %>% 
  unique()

#Export as a data frame so it can be used in other script
write.csv(eco_type, "data/metadata/ecoregion_type.csv")
