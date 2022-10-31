#Title:Delimiting wintering area of geolocalisator tracked common-ringed plover from Bylot Island
#Authors:  Louis Moisan
#Date: March 2021

##### INPUT FILES
# - data/tracking/ringed_plover/Bylot_CRPL.shp
# - data/ringed_plover/crpl_winter_dates.csv
# - data/ringed_plover/Bylot_CRPL_Metadata.csv
# - data/shapefiles/marine_ecoregions.shp

##### OUTPUT FILES
# - data/shapefiles/raw/range_maps/tracking/crpl_winter_bylot.shp

#------------------#
#### Librairies ####
#------------------#
library(sf)
library(sp)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggplot2)
library(lubridate)
library(adehabitatHR)
library(raster)

#--------------------------------------------------------------#
#### Import the shapefile of common ringeg plover locations ####
#--------------------------------------------------------------#
crpl_bylot <- sf::st_read("data/tracking/ringed_plover/Bylot_CRPL.shp") %>%  mutate(timestamp= strptime(x = as.character(timestamp), format = "%Y-%m-%d %H:%M:%S")) %>% 
  dplyr::mutate(date= lubridate::date(timestamp)) %>% 
  dplyr::mutate(ind_ident= as.factor(ind_ident)) %>% 
  dplyr::mutate(tag_ident = as.factor(tag_ident))


#---------------------------#
#### Visualize locations ####
#---------------------------#
#get the world map layer
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= crpl_bylot, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-25, 10), ylim = c(0, 40))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 


#----------------------------------------------------#
#### Retrieve Wintering dates for each individual ####
#(Supplementary material Léandri-Breton et al., 2019)# 
#----------------------------------------------------#
#Get the wintering arrival and departure dates of each individual
winter_dates <- read.csv("data/tracking/ringed_plover/crpl_winter_dates.csv")
#Change string to date format
winter_dates$Winter_arrival <-  as.Date(winter_dates$Winter_arrival,format="%Y/%d/%m")
winter_dates$Winter_departure <-  as.Date(winter_dates$Winter_departure,format="%Y/%d/%m")

#Match individual id to id number
metadata <- read.csv("data/tracking/ringed_plover/Bylot_CRPL_Metadata.csv") %>% dplyr::mutate(animal.number= factor(animal.number))

winter_dates <- dplyr::left_join(winter_dates, metadata, by= c("Individual"= "animal.number")) %>%  stats::na.omit()


#---------------------------------------------------------------------------#
#### Filter locations of each ind. based on winter arrival and departure ####
#---------------------------------------------------------------------------#
#Filter locations based on the specific data of winter arrival and departure
crpl_bylot_filter <-crpl_bylot[FALSE,]



for (i in levels(crpl_bylot$ind_ident)){
  if (i %in% winter_dates$animal.id) {
p <- crpl_bylot[crpl_bylot$ind_ident == i, ] %>% dplyr::filter(date >= winter_dates[winter_dates$animal.id == i, ]$Winter_arrival & date <= winter_dates[winter_dates$animal.id == i, ]$Winter_departure)
crpl_bylot_filter <-  rbind(crpl_bylot_filter, p) }
  else {next}
}

#Confirm that the wintering dates are correct
crpl_bylot_filter %>% 
  dplyr::group_by(ind_ident, tag_ident) %>%
  dplyr::summarize(winter_arrival = min(date), Winter_departure= max(date))
#Number of individual included
nlevels(droplevels(crpl_bylot_filter$ind_ident))
#Confirm absence of pseudo-replicate
nlevels(droplevels(crpl_bylot_filter$ind_ident)) == nlevels(droplevels(crpl_bylot_filter$tag_ident))

ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= crpl_bylot_filter, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-35, 20), ylim = c(-15, 55))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 

#-----------------------------------------------------#
#### Create a wintering polygon for the population ####
#-----------------------------------------------------#
#https://jamesepaterson.github.io/jamespatersonblog/04_trackingworkshop_kernels

#Create a sp spatial point object
coords.xy <- data.frame(x= crpl_bylot_filter$long, y= crpl_bylot_filter$lat)
coords.spatialpoints <- sp::SpatialPoints(coords.xy)

#Calculate the point density
#using the ad hoc 
kernel.href <- adehabitatHR::kernelUD(coords.spatialpoints, h = "href")
kernel.lscv <- adehabitatHR::kernelUD(coords.spatialpoints, h = "LSCV")

#Create polygon encompassing 95 % of the points
polygon.href <- adehabitatHR::getverticeshr(kernel.href, percent= 95)
polygon.lscv <- adehabitatHR::getverticeshr(kernel.lscv, percent= 95)


#The minimum h is represented in the scanned range (prensence of convergence)
adehabitatHR::plotLSCV(kernel.lscv)

#Set the coordinate reference system of the polygon
sp::proj4string(polygon.href) = CRS("+init=epsg:4326")
sp::proj4string(polygon.lscv) = CRS("+init=epsg:4326")

#Transform back into sf object
winter.area.href <- sf::st_as_sf(polygon.href)
winter.area.lscv <- sf::st_as_sf(polygon.lscv)

#--------------------------------#
#### Visualize polygon on map ####
#--------------------------------#
ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2:: geom_sf(data= crpl_bylot_filter, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::geom_sf(data= winter.area.lscv, fill= "steelblue4", color= "black", alpha= 0.4)+
  ggplot2::coord_sf(xlim = c(-35, 20), ylim = c(-15, 55))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 


#---Remove marine part of the wintering range due to uncertainty of geoloc
sf::sf_use_s2(FALSE) # for the moment using s2 cause invalid geometries, but we the further developpment of the s2 package, considering turning s2 on to more accurate geocomputation
#Read marine ecoregions shapefile
marine <- sf::st_read("data/shapefiles/ecoregions/marine_ecoregions.shp") %>% sf::st_union()
#Crop
winter.area.lscv.crop <- sf::st_difference(winter.area.lscv, marine)


#Visualize the cropped polygon
ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= winter.area.lscv.crop, fill= "steelblue4", color= "black", alpha= 0.4)+
  ggplot2::coord_sf(xlim = c(-35, 20), ylim = c(-15, 55))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 

sf::st_write(winter.area.lscv.crop, "data/shapefiles/raw/range_maps/tracking/crpl_winter_bylot.shp")
# Smoothing: Fixed  
# Band width selection: Automated selection with least-square cross validation method (LSCV)
    #Convergence confirmed
# Kernel distribution: Bivariate normal kernel
# Grid resoltion: Software default, automated
# Scaling or standardization: 

#------------------------------------------#
##### Export a raster of kernel density ####
#------------------------------------------#
# Extract the UD values and coordinates into a data frame
kernel.lscv.df <-data.frame("value" = kernel.lscv$ud, "lon" = kernel.lscv@coords[,1], "lat" = kernel.lscv@coords[,2])
sp::coordinates(kernel.lscv.df)<-~lon+lat
# coerce to SpatialPixelsDataFrame
sp::gridded(kernel.lscv.df) <- TRUE
# coerce to raster
kernel.raster <- raster::raster(kernel.lscv.df)
#Export raster of kernel
writeRaster(kernel.raster, filename = "data/tracking/ringed_plover/crpl_non_breeding_kernel.tif")
