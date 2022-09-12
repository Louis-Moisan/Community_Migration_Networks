#Title:Delimiting wintering area of satellite tracked snowy goose from Bylot Island
#Author: Louis Moisan
#Date: March 2021

##### INPUT FILES
# - data/tracking/snow_goose/sngo_bylot_PTT_TAG_2021.csv

##### OUTPUT FILES
# - data/shapefiles/raw/range_maps/tracking/sngo_winter_bylot.shp

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


#---------------------------------------------------#
#### Import the shapefile of snow goose locations ####
#---------------------------------------------------#
sngo_df <- read.csv("data/tracking/snow_goose/sngo_bylot_PTT_TAG_2021.csv")


#Extract date from time
sngo_df$date <- lubridate::date(sngo_df$timestamp)
sngo_df$month <- lubridate::month(sngo_df$timestamp)
sngo_df$year <- lubridate::year(sngo_df$date)

sngo_df <-  sngo_df %>% 
  dplyr::mutate(timestamp= strptime(x = as.character(timestamp),format = "%Y-%m-%d %H:%M:%S"))

head(sngo_df, 5)

#-------------------------------------#
#### Select Bylot individuals only ####
#-------------------------------------#
#Transform data frame into spatial points
sngo_sf <- sf::st_as_sf(x = sngo_df, 
                        coords = c("location.long", "location.lat"),
                        crs = 4326) 

#Make a bylot spatial point
bylot <- sf::st_point(c(-80.0000, 73.000)) %>%
  sf::st_sfc(crs = 4326) %>%
  sf::st_transform(32617)

#Make a buffer around the point to represent study site
bylot_50km <- sf::st_buffer(bylot, 50000) %>%
  sf::st_transform(4326)

#Get the band and encounter location falling inside the 30 km buffer representing the bylot region
geese_bylot_id <- sf::st_intersection(sngo_sf, bylot_50km)

head(sngo_sf)

#Filter to retain only Geese that breed on Bylot
sngo_bylot <- sngo_sf[sngo_sf$tag.local.identifier %in% geese_bylot_id$tag.local.identifier, ]


#-------------------------------#
#### Filter winter locations ####
#-------------------------------#
#keep locations from january and february to exclude migration area by late and early migrants
sngo_bylot_winter <- dplyr::filter(sngo_bylot,  month <= 2)

#---------------------------#
#### Visualize locations ####
#---------------------------#
#get the world map layer
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

sngo_bylot_winter_sf <- sf::st_as_sf(x = sngo_bylot_winter, 
             coords = c("location.long", "location.lat"),
             crs = 4326)


ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= sngo_bylot_winter_sf, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-140, -40), ylim = c(40, 85))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 


#------------------------------#
#### Kernel density polygon ####
#------------------------------#

#Create a sp spatial point object
coords.xy <- sngo_bylot_winter_sf %>%
  sf::st_coordinates()

coords.spatialpoints <- sp::SpatialPoints(coords.xy)

#Calculate the point density
#using the ad hoc 
kernel.href <- adehabitatHR::kernelUD(coords.spatialpoints, h = "href")

#Create polygon encompassing 95 % of the points
polygon.href <- adehabitatHR::getverticeshr(kernel.href, percent= 95)


#Set the coordinate reference system of the polygon
sp::proj4string(polygon.href) = CRS("+init=epsg:4326")

#Transform back into sf object
winter.area.href <- sf::st_as_sf(polygon.href)

ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= sngo_bylot_winter_sf, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::geom_sf(data= winter.area.href, fill= "grey15", color= "steelblue4", alpha= 0.5, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-135, -40), ylim = c(30, 50))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 

#Export as shapefile
sf::st_write(winter.area.href, "data/shapefiles/raw/range_maps/tracking/sngo_winter_bylot.shp")
# Smoothing: Fixed  
# Band width selection: Automated selection with ad hoc method
# Kernel distribution: Bivariate normal kernel
# Grid resoltion: Software default, automated