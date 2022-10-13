#Title: Distribution map of the Bylot island species non-breeding distribution
#Author: Louis Moisan
#Creation Date: June 5 2021


#------------------#
#### Librairies ####
#------------------#
library(dplyr)
library(sf)
sf::sf_use_s2(FALSE)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
source("scripts/functions/1_functions_data_manip.R")


#-------------------#
#### Import data ####
#-------------------#
#---- Shapefiles
#Ecoregions of the World
ecoregion <- sf::read_sf("data/shapefiles/ecoregions/ecoregions.shp")
#Land basemap
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

#Incidence matrix Species-Ecoregions
optimal_eco <- csv_to_eco_sp_matrix("data/adjency_matrix/optimal_eco.csv") %>%
  as.matrix()
#Snow Goose Ecoregions
sngo_eco <- which(optimal_eco[row.names(optimal_eco) == "Snow Goose",] ==1) %>%
  as.data.frame() %>%
  row.names()

sngo_eco <- ecoregion[which(ecoregion$ecoregion %in% sngo_eco),] %>% 
  dplyr::group_by(ecoregion) %>% 
  dplyr::summarise()

#Coastal Shorebirds
coastal_shorebirds_eco <-  optimal_eco[row.names(optimal_eco) %in% c("Black-bellied Plover", "Red Knot", "Ruddy Turnstone"),]   
coastal_shorebirds_eco <- which(colSums(coastal_shorebirds_eco) >0) %>% 
  as.data.frame() %>%
  row.names()

coastal_shorebirds_eco <- ecoregion %>% 
  dplyr::filter(ecoregion %in% coastal_shorebirds_eco) %>% 
  dplyr::group_by(ecoregion) %>% 
  dplyr::summarise()

#Terrestrial Shorebirds
terrestrial_shorebirds_eco <-  optimal_eco[row.names(optimal_eco) %in% c("American Golden-Plover", "Bairds Sandpiper", "Buff-breasted Sandpiper", "Pectoral Sandpiper", "White-rumped Sandpiper"),]   
terrestrial_shorebirds_eco <- which(colSums(terrestrial_shorebirds_eco) >0) %>% 
  as.data.frame() %>%
  row.names()

terrestrial_shorebirds_eco <- ecoregion[which(ecoregion$ecoregion %in% terrestrial_shorebirds_eco),] %>% 
  dplyr::group_by(ecoregion) %>% 
  dplyr::summarise()

#Common-ringed Plover 
crpl_eco <- which(optimal_eco[row.names(optimal_eco) == "Common-ringed Plover",] ==1) %>%
  as.data.frame() %>%
  row.names()

crpl_eco <- ecoregion[which(ecoregion$ecoregion %in% crpl_eco),] %>% 
  dplyr::group_by(ecoregion) %>% 
  dplyr::summarise()

#Red Phalarope
reph_eco <- which(optimal_eco[row.names(optimal_eco) == "Red Phalarope",] ==1) %>%
  as.data.frame() %>%
  row.names()

reph_eco <- ecoregion[which(ecoregion$ecoregion %in% reph_eco),] %>% 
  dplyr::group_by(ecoregion) %>% 
  dplyr::summarise()

#------------------------------------#
##### Figure modules distribution ####
#------------------------------------#
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

#map
svg("figures/Figure_7-Shorebirds_geese_modules/sngo_eco.svg", #file name
    bg = "transparent") 

ggplot() + 
  #Land filling color
  geom_sf(data= world[world$sovereignt == "United States of America",],
          fill= "#000000", 
          cex= 0,
          alpha= 1) + 
  #Snow Goose ecoregions
  geom_sf(data= sngo_eco,
          alpha=1,
          fill= "#668247ff",
          color= "#343a40",
          cex= 0.5) +
  #Esthetic options
  theme(panel.grid.major = element_blank(), #Theme settings
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  #Crop map
  coord_sf( xlim= c(-130, -55),ylim = c(25, 50))
dev.off()


#----Red Phalarope
svg("figures/Figure_7-Shorebirds_geese_modules/reph_eco.svg", #file name
    bg = "transparent") 
ggplot() + 
  #Land filling color
  geom_sf(data= world,
          fill= "#000000", 
          cex= 0,
          alpha= 1) + 
  #Red Phalarope ecoregions
  geom_sf(data= reph_eco,
          alpha=1,
          fill= "#fa8535ff",
          color= "#343a40",
          cex= 0.5) +
  #Esthetic options
  theme(panel.grid.major = element_blank(), #Theme settings
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  #Crop map
  coord_sf( xlim= c(-31, 50),ylim = c(-35, 40))
dev.off()

#----Common-ringed plover
svg("figures/Figure_7-Shorebirds_geese_modules/crpl_eco.svg", #file name
    bg = "transparent") 
ggplot() + 
  #Land filling color
  geom_sf(data= world,
          fill= "#000000", 
          cex= 0,
          alpha= 1) + 
  #Red Phalarope ecoregions
  geom_sf(data= crpl_eco,
          alpha=1,
          fill= "#fa8535ff",
          color= "#343a40",
          cex= 0.5) +
  #Esthetic options
  theme(panel.grid.major = element_blank(), #Theme settings
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_blank(),
        panel.background = element_rect(fill = "transparent"))+
  #Crop map
  coord_sf( xlim= c(-31, 50),ylim = c(-35, 40))
dev.off()
  
  #----Terrestrial Shorebird
  svg("figures/Figure_7-Shorebirds_geese_modules/terrestrial_shorebirds.svg", #file name
      bg = "transparent") 
  ggplot() + 
    #Land filling color
    geom_sf(data= world,
            fill= "#000000", 
            cex= 0,
            alpha= 1) + 
    #terrestial Non-breeding ecoregions
    geom_sf(data= terrestrial_shorebirds_eco,
            alpha=1,
            fill= "#fa8535ff",
            color= "#343a40",
            cex= 0.5) +
    #Esthetic options
    theme(panel.grid.major = element_blank(), #Theme settings
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 22, face = "bold"),
          legend.title=element_blank(),
          panel.background = element_rect(fill = "transparent"))+
    #Crop map
    coord_sf( xlim= c(-100, -35),ylim = c(10, -55))
  dev.off()
  
#----Coastal Shorebird
  svg("figures/Figure_7-Shorebirds_geese_modules/coastal_shorebirds.svg", #file name
      bg = "transparent")
  ggplot() + 
    #Land filling color
    geom_sf(data= world,
            fill= "#000000", 
            cex= 0,
            alpha= 1) + 
    #Coastal shorebirds Non-breeding range polygons
    geom_sf(data= coastal_shorebirds_eco,
            alpha=1,
            fill= "#fa8535ff",
            color= "#fa8535ff",
            cex= 0.9)+
    #Esthetic options
    theme(panel.grid.major = element_blank(), #Theme settings
          panel.grid.minor = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size = 22, face = "bold"),
          legend.title=element_blank(),
          panel.background = element_rect(fill = "transparent"))+
    #Crop map
    coord_sf( xlim= c(-110, 40),ylim = c(65, -54))
  dev.off()
