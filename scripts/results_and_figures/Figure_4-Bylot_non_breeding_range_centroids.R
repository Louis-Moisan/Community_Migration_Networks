#Title: Mapping the distribution of the non-breeding range centroids of migratory species from Bylot Island
#Author: Louis Moisan
#Creation Date: June 5 2021
#Review date: September 2 2022
  
#------------------#
#### Librairies ####
#------------------#
      library(dplyr)
      library(sf) 
      library(ggplot2)
      library(rnaturalearth)
      library(rnaturalearthdata)
      
        
#-------------------#
#### Import data ####
#-------------------#
  #The refined non-breeding range for each migratory species from Bylot
    non_breeding_range <- sf::st_read("data/shapefiles/bylot_non_breeding_range.shp")
  
  #General map background
    world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
    ocean <- sf::st_read("data/shapefiles/raw/oceans.shp")
  
  #--------------------------------------#
  ##### Non-breeding range centroids #####
  #--------------------------------------#
  #----- Extract Centroid of non-breeding area of each species
  non_breeding_range_centro <- sf::st_centroid(non_breeding_range)
    
  #Bylot island
  bylot <- sf::st_point(c(-79.589966, 72.912929)) %>%
    sf::st_sfc(crs = 4326)
  
  bylot_df <- data.frame(to= "Bylot Island", Xend= -79.589966, Yend= 72.912929)
  
  #------ Assign colors to each species
  #Read the data base with the Hex code for each species
  sp_col <- read.csv("data/metadata/species_colors.csv") %>% 
  dplyr::mutate(func_group_col= as.character(func_group_col)) #Change the color factor as character so it works well
  
  #----- Assign colors to centroids
  non_breeding_range_centro <- dplyr::left_join(non_breeding_range_centro, sp_col[c("species", "func_group","func_group_col","trophic_level", "sub_order")], by= "species") %>%
    dplyr::mutate(type = "single")%>%
    dplyr::arrange(trophic_level, sub_order)  
  
#!!! QGIS ALERT !!! The centroids of species with divided non-breeding range (Long-tailed Jaeger, Red Knot, Parasitic Jaeger, Ruddy Turnstone and Black-bellied plover) were set by selecting each part of the divided range and using the tool "Centroids". It is important to note that the location of some centroids were slightly relocated to better represent the habitat type (e.g., moving the centroids of Red Knot was inland, but the species remain on coast during winter)
  #Adding the centroids defined in QGIS
  non_breeding_range_centro_double <- sf::st_read("data/shapefiles/non_breeding_range_centro_double_modified.shp") %>%
    dplyr::left_join( sp_col[c("species", "func_group","func_group_col","trophic_level", "sub_order")], by= "species") %>%
    dplyr::mutate(type = "double") %>% 
    dplyr::arrange(trophic_level, sub_order)
  
  #Remove those species from the single centroid list
  non_breeding_range_centro <- non_breeding_range_centro[!(non_breeding_range_centro$species %in% unique(non_breeding_range_centro_double$species)),]
  
  #Combine rows of the two data frame
  non_breeding_range_centro <- rbind(
   subset(non_breeding_range_centro, select = intersect(colnames(non_breeding_range_centro), colnames(non_breeding_range_centro_double))), 
  subset(non_breeding_range_centro_double, select = intersect(colnames(non_breeding_range_centro), colnames(non_breeding_range_centro_double))))
  
  #Reorder factor level single centroids per species
  non_breeding_range_centro$func_group <- factor(non_breeding_range_centro$func_group,  levels = unique(non_breeding_range_centro$func_group))
  
  non_breeding_range_centro$species <- factor(non_breeding_range_centro$species, levels =  unique(non_breeding_range_centro$species))
  
  #----- Assign colors to non breeding range
  non_breeding_range <- dplyr::left_join(non_breeding_range, sp_col[c("species","func_group","func_group_col","trophic_level", "sub_order")], by= "species") %>%
    dplyr::arrange(trophic_level, sub_order)
  
  #Reorder factor level for plot legend
  non_breeding_range$func_group <- factor(non_breeding_range$func_group,  levels = unique(non_breeding_range_centro$func_group))
  
  non_breeding_range$species <- factor(non_breeding_range$species,  levels = unique(non_breeding_range$species))
  
  #Extent of the range
  sf::st_bbox(non_breeding_range)
  
  sf::write_sf(non_breeding_range_centro,"data/shapefiles/non_breeding_centro.shp")
  sf::write_sf(non_breeding_range,"data/shapefiles/non_breeding_range.shp")
  
#----------------------------------------------#
#### Global map connectivity with centroids ####
#----------------------------------------------#
 
  
svg("figures/Figure_4-Bylot_non_breeding_range_centroids/NonBreedingRange.svg", #file name
   bg = "transparent") 

#General world map
ggplot() + 
  #Ocean filling colors
  geom_sf(data= ocean,
          fill= "#ffffff",
          color= "#264653",
          alpha=1,
          cex= 0.0000000000000000001)+
  #Land filling color
  geom_sf(data= world,
          fill= "#393939", 
          cex= 0.0000000000000000001,
          alpha= 1) + 
  #Colors of edges
  scale_color_manual(values= unique(non_breeding_range$func_group_col))+
  #centroids
  geom_sf(data= non_breeding_range_centro, #centroid 
          aes(fill= func_group, shape= type),
          color= "black",
          stroke =0.7,
          size= 2.5,
          alpha=1) +
  #Colors of non-breeding range polygons
  scale_fill_manual(values= unique(non_breeding_range_centro$func_group_col))+
  scale_shape_manual(values=c(23, 21))+
  #Bylot Island
  geom_sf(data= bylot,
          fill= "red",
          color= "white",
          size= 5,
          shape= 24,
          alpha= 1)+
  #Esthetic options
  theme(panel.grid.major = element_blank(), #Theme settings 
        panel.grid.minor = element_blank(),
        plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_blank(),
        legend.key=element_blank(),
        panel.background = element_rect(fill = "white"))+
  guides(shape = "none")+ 
  guides(color= "none")+
  guides(fill= "none")+
  guides(fill=guide_legend(override.aes=list(colour=unique(non_breeding_range_centro$func_group_col))))+
  #Map Projection orthographic
  coord_sf(crs = "+proj=laea +lat_0=0 +lon_0=-59 +ellps=GRS80 +units=m +no_defs ")

    dev.off()
    