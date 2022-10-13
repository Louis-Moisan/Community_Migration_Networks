#Title:Delimiting stationary non-breeding area area of King Eider tracked from East Bay Eastern Arctic Breeding site
#Authors:  Louis Moisan
#Date: September 2022

##### INPUT FILES
# data/non-breeding_range/tracking/king_eider/Common_King Eiders; East Bay Island, Nunavut; Gilchrist_Mosbech_Sonne 2001 and 2003.csv
# data/shapefiles/ecoregions.shp

##### OUTPUT FILES
# data/shapefiles/raw/range_maps/tracking/king_eider_winter_east_bay.shp
# data/tracking/king_eider/kiei_non_breeding_kernel.tif

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

#---------------------------------------------------------------------------#
#### Import the locations of King and Common Eiders tracked from East Bay####
#---------------------------------------------------------------------------#
kiei_df <- read.csv("data/tracking/king_eider/Common_King Eiders; East Bay Island, Nunavut; Gilchrist_Mosbech_Sonne 2001 and 2003.csv") %>% 
#Extract date from time
dplyr::mutate(date= lubridate::date(timestamp)) %>% 
dplyr::mutate(month= lubridate::month(timestamp)) %>% 
dplyr::mutate(year= lubridate::year(date)) %>% 
dplyr::mutate(timestamp= strptime(x = as.character(timestamp),format = "%Y-%m-%d %H:%M:%S")) %>% #Select only locations from King Eider
  dplyr::filter(individual.taxon.canonical.name == "Somateria spectabilis")

#Transform data frame into spatial points
kiei_sf <- sf::st_as_sf(x = kiei_df, 
                        coords = c("location.long", "location.lat"),
                        crs = 4326) 

#---------------------------#
#### Visualize locations ####
#---------------------------#
#get the world map layer
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

ggplot2::ggplot(data = world) + #world map
  ggplot2::geom_sf(fill= "grey40", color= "grey45") + # world map color
  ggplot2::geom_sf(data= kiei_sf, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-100, -50), ylim = c(55, 75))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 


#-------------------------------#
#### Filter winter locations ####
#-------------------------------#
#keep locations from november to march
#Those months were selected to represent the stationary non-breeding range, based on visual inspection of the largest time period during for which indivduals stays in the same area
kiei_winter <- kiei_sf %>% 
  dplyr::filter(month <= 3 | month>=11 )
#number of individuals
length(unique(kiei_winter$tag.local.identifier))

#------------------------------#
#### Kernel density polygon ####
#------------------------------#
#Create a sp spatial point object
coords.xy <- kiei_winter %>%
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
  ggplot2::geom_sf(data= kiei_winter, fill= "grey15", color= "orange", alpha= 0.8, cex= 0.35)+
  ggplot2::geom_sf(data= winter.area.href, fill= "grey15", color= "steelblue4", alpha= 0.5, cex= 0.35)+
  ggplot2::coord_sf(xlim = c(-100, -50), ylim = c(55, 75))+ # set the map lat/lon limits
  ggplot2::xlab("Longitude") + 
  ggplot2::ylab("Latitude") 

#Remove the continental part of the range due to range estimation error
sf::sf_use_s2(FALSE) # for the moment using s2 cause invalid geometries, but we the further developpment of the s2 package, considering turning s2 on to more accurate geocomputation
terrestrial <- sf::st_read("data/shapefiles/ecoregions.shp") %>% 
  dplyr::filter(eco_type =="terrestrial") %>% 
  sf::st_union()

#Crop
winter.area.href <- sf::st_difference(winter.area.href, terrestrial)

#Export as shapefile
sf::st_write(winter.area.href, "data/shapefiles/raw/range_maps/tracking/kiei_winter_east_bay.shp")
# Smoothing: Fixed  
# Band width selection: Automated selection with ad hoc method
# Kernel distribution: Bivariate normal kernel
# Grid resoltion: Software default, automated

#------------------------------------------#
##### Export a raster of kernel density ####
#------------------------------------------#
# Extract the UD values and coordinates into a data frame
kernel.href.df <-data.frame("value" = kernel.href$ud, "lon" = kernel.href@coords[,1], "lat" = kernel.href@coords[,2])
sp::coordinates(kernel.href.df)<-~lon+lat
# coerce to SpatialPixelsDataFrame
sp::gridded(kernel.href.df) <- TRUE
# coerce to raster
kernel.raster <- raster::raster(kernel.href.df)
#Export raster of kernel
writeRaster(kernel.raster, filename = "data/tracking/king_eider/kiei_non_breeding_kernel.tif", overwrite= T)
