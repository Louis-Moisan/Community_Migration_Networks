#Title:Delimiting wintering area of satellite tracked snowy owl from Bylot Island
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


##### INPUT FILES
# - data/tracking/snowy_owl/snow_north_america.shp
# - data/snowy_owl/snowy_bylot_wintering_dates.csv

##### OUTPUT FILES
# - data/shapefiles/raw/range_maps/tracking/snow_winter_area_bylot.shp

#---------------------------------------------------#
#### Import the shapefile of snowy owl locations ####
#---------------------------------------------------#
snow_north_america <- sf::st_read("data/tracking/snowy_owl/snow_north_america.shp") %>%
  dplyr::mutate(timestamp= strptime(x = as.character(timestamp),format = "%Y-%m-%d %H:%M:%S"))
#Extract date from time
snow_north_america$date <- lubridate::date(snow_north_america$timestamp)
snow_north_america$year <- lubridate::year(snow_north_america$date)



#---------------------------------------------#
#### Filter to keep only bylot individuals ####
#---------------------------------------------#
bylot_id <- c("39097", "39100", "38596", "38610", "38602", "39078", "39103", "48837", "148839", "106398", "106399", "106400", "106401", "106402", "106403", "134872", "134873")

snow_bylot <- snow_north_america[snow_north_america$tag_ident %in% bylot_id,]



#---------------------------#
#### Visualize locations ####
#---------------------------#
#get the world map layer
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= snow_bylot, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-140, -40), ylim = c(40, 85))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 


#---------------------------------------------------#
#### Filter data to get only wintering locations ####
#---------------------------------------------------#
#Read the arrival and departure date of each bylot snowy owl
snow_wintering_date <- read.csv("data/snowy_owl/snowy_bylot_wintering_dates.csv") %>%
  dplyr::mutate_at(vars(Arrival_w1, Departure_w1, Arrival_w2, Departure_w2, Arrival_w3, Departure_w3), funs(as.Date(., "%Y-%m-%d")))


#Locations for first winter
snow_bylot_filter_w1 <-snow_bylot[FALSE,]

for (i in levels(snow_bylot$tag_ident)){
  if (i %in% snow_wintering_date$tag_id) {
    p <- snow_bylot[snow_bylot$tag_ident == i, ] %>%
      dplyr::filter(date >= snow_wintering_date[snow_wintering_date$tag_id == i, ]$Arrival_w1 & date <= snow_wintering_date[snow_wintering_date$tag_id == i, ]$Departure_w1)
    snow_bylot_filter_w1 <-  rbind(snow_bylot_filter_w1, p) }
  else {next}
}

#Number of snowy per year
snow_bylot_filter_w1 %>% 
  dplyr::group_by(tag_ident, ind_ident) %>%
  dplyr::summarize(deploy= min(timestamp))

#Locations for second winter
snow_bylot_filter_w2 <-snow_bylot[FALSE,]

for (i in levels(snow_bylot$tag_ident)){
  if (i %in% snow_wintering_date$tag_id) {
    p <- snow_bylot[snow_bylot$tag_ident == i, ] %>%
      dplyr::filter(date >= snow_wintering_date[snow_wintering_date$tag_id == i, ]$Arrival_w2 & date <= snow_wintering_date[snow_wintering_date$tag_id == i, ]$Departure_w2)
    snow_bylot_filter_w2 <-  rbind(snow_bylot_filter_w2, p) }
  else {next}
}

#Locations for third winter
snow_bylot_filter_w3 <-snow_bylot[FALSE,]

for (i in levels(snow_bylot$tag_ident)){
  if (i %in% snow_wintering_date$tag_id) {
    p <- snow_bylot[snow_bylot$tag_ident == i, ] %>%
      dplyr::filter(date >= snow_wintering_date[snow_wintering_date$tag_id == i, ]$Arrival_w3 & date <= snow_wintering_date[snow_wintering_date$tag_id == i, ]$Departure_w3)
    snow_bylot_filter_w3 <-  rbind(snow_bylot_filter_w3, p) }
  else {next}
}


snow_bylot_filter <- rbind(snow_bylot_filter_w1, snow_bylot_filter_w2) %>%
  rbind(snow_bylot_filter_w3)

#Number of individual per winter
nlevels(droplevels(snow_bylot_filter_w1$tag_ident))
nlevels(droplevels(snow_bylot_filter_w2$tag_ident))
nlevels(droplevels(snow_bylot_filter_w3$tag_ident))
#Number total of individual
nlevels(droplevels(snow_bylot_filter$tag_ident))

#------------------------------------------------------#
#### Visualization of snowy owl wintering locations ####
#------------------------------------------------------#

ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= snow_bylot_filter, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-140, -40), ylim = c(40, 85))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 


#------------------------------#
#### Kernel density polygon ####
#------------------------------#
#Create a sp spatial point object
coords.xy <- data.frame(x= snow_bylot_filter$long, y= snow_bylot_filter$lat)

coords.spatialpoints <- sp::SpatialPoints(coords.xy)

#Calculate the point density
#using the ad hoc 
kernel.href <- adehabitatHR::kernelUD(coords.spatialpoints, h = "href")
#Algorithm did not converge with the Least square cross-validation


#Create polygon encompassing 95 % of the points
polygon.href <- adehabitatHR::getverticeshr(kernel.href, percent= 95)


#Set the coordinate reference system of the polygon
sp::proj4string(polygon.href) = CRS("+init=epsg:4326")

#Transform back into sf object
winter.area.href <- sf::st_as_sf(polygon.href)

ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= snow_bylot_filter, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::geom_sf(data= winter.area.href, fill= "grey15", color= "steelblue4", alpha= 0.3, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-140, -40), ylim = c(40, 85))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 

#Export as shapefile
sf::st_write(winter.area.href, "data/shapefiles/raw/range_maps/tracking/snow_winter_area_bylot.shp")
# Smoothing: Fixed  
# Band width selection: Automated selection with ad hoc method
#Convergence not found with the least square cross-validation algorithm
# Kernel distribution: Bivariate normal kernel
# Grid resoltion: Software default, automated
# Scaling or standardization: 