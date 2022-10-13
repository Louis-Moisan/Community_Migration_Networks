#Title:Delimiting wintering area of snow goose banded from Bylot Island
#Author: Louis Moisan
#Date: March 2021


#------------------#
#### Librairies ####
#------------------#
library(sf)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(lubridate)
library(adehabitatHR)

#---------------------------------------------------------#
##### Retrive location of birds band breeding on Bylot ####
#---------------------------------------------------------#
band_enc_nunavut <- sf::st_read("data/shapefiles/raw/range_maps/tracking/banding_encounter_NU.shp") %>% 
  dplyr::filter(sp_code == "SNGO")

#Create a point of reference for the study site, set the Geographic Coordinates System (GCS) and then project coordinates in UTM to get a scale with units in meters in a way to create a buffer using km units.
bylot <- sf::st_point(c(-80.0000, 73.000)) %>%
  sf::st_sfc(crs = 4326) %>%
  sf::st_transform(32617)

#Assume that species located in the buffer region use the same migratory route as birds of the study site.
#Create a buffer of 30 km radius around the study site for precise banding data and then reproject coordinates in lat/long
bylot_100km <- sf::st_buffer(bylot, 100000) %>%
  sf::st_transform(4326)

#Get the band and encounter location falling inside the 30 km buffer representing the bylot region
band_enc_bylot_100km <- sf::st_intersection(band_enc_nunavut, bylot_100km)


#Match the band_id find inside the zone with band_id find outside the zone to find where birds find in the bylot region came from or went to
band_enc_bylot_100km <-  band_enc_nunavut[band_enc_nunavut$band_id %in% band_enc_bylot_100km$band_id, ]

#Remove unused objects created in the process of selecting the desire band and encounter locations
rm(bylot, bylot_100km, band_enc_nunavut)


#-----------------------------#
#### Define wintering area ####
#-----------------------------#
#keep locations from january and february to exclude migration area by late and early migrants
#Extract date from time
sngo_band_winter <- band_enc_bylot_100km %>% 
  dplyr::mutate(month= as.numeric(month)) %>% 
  dplyr::filter(month <= 2)

#Transform data frame into spatial points
sngo_band_winter_sf <- sf::st_as_sf(x = sngo_band_winter, 
                        coords = c("lon", "lat"),
                        crs = 4326)

#---------------------------#
#### Visualize locations ####
#---------------------------#
#get the world map layer
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot(data = world) + #world map
  geom_sf(fill= "grey40", color= "grey45") + # world map color
  geom_sf(data= sngo_band_winter_sf, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  coord_sf(xlim = c(-140, -40), ylim = c(40, 85))+ # set the map lat/lon limits
  xlab("Longitude") + 
  ylab("Latitude") 

#------------------------------#
#### Kernel density polygon ####
#------------------------------#
#Create a sp spatial point object
coords.xy <- sngo_band_winter_sf %>%
  sf::st_coordinates()
coords.spatialpoints <- SpatialPoints(coords.xy)

#Calculate the point density
#using the ad hoc 
polygon.href <- kernelUD(coords.spatialpoints, h = "href", grid = 500) %>% 
 getverticeshr( percent= 50)


#Set the coordinate reference system of the polygon
proj4string(polygon.href) = CRS("+init=epsg:4326")

#Transform back into sf object
winter.area.href <- st_as_sf(polygon.href)

ggplot2::ggplot(data = world) + #world map
  geom_sf(fill= "grey40", color= "grey45") + # world map color
geom_sf(data= sngo_band_winter_sf, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.0001)+
  geom_sf(data= winter.area.href, fill= "grey15", color= "steelblue4", alpha= 0.5, cex= 0.35)+
  coord_sf(xlim = c(-135, -40), ylim = c(30, 50))+ # set the map lat/lon limits
  xlab("Longitude") + 
  ylab("Latitude") 

#Export as shapefile
sf::st_write(winter.area.href, "data/shapefiles/raw/range_maps/tracking/sngo_winter_bylot_banding.shp")
# Smoothing: Fixed  
# Band width selection: Automated selection with ad hoc method
# Kernel distribution: Bivariate normal kernel
# Grid resoltion: Some test and the polygon stay the same size and location from 500


#--------------------------------------------------------------#
#### Compare the number of ecoregions with tracking devices ####
#--------------------------------------------------------------#
ecoregion <- sf::st_read("data/shapefiles/ecoregions/ecoregions.shp") #Read ecoregions file

sngo_tracking_winter_eco <- sf::st_read("data/shapefiles/overlap_range_ecoregions/sngo_winter_bylot_eco.shp") %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(eco_type != "marine")%>% 
  dplyr::select(ecoregion) %>% 
  unique()%>% 
  arrange(ecoregion)
nrow(sngo_tracking_winter_eco)

sf::sf_use_s2(FALSE) # for the moment using s2 cause invalid geometries, but we the further developpment of the s2 package, considering turning s2 on to more accurate geocomputation
sngo_banding_winter_eco <-  sf::st_intersection(winter.area.href, ecoregion) %>% 
  sf::st_drop_geometry() %>% 
  dplyr::filter(eco_type != "marine")%>% 
  dplyr::select(ecoregion) %>% 
  unique() %>% 
  arrange(ecoregion)
nrow(sngo_banding_winter_eco)

sngo_banding_winter_eco$ecoregion
sngo_tracking_winter_eco$ecoregion  

sngo_banding_winter_eco$ecoregion %in%
sngo_tracking_winter_eco$ecoregion

sngo_tracking_winter_eco$ecoregion%in%
  sngo_banding_winter_eco$ecoregion 
  

