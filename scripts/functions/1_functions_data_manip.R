#Title: Functions to manipulate, create and visualize migration network with trophic interactions
#Author: Louis Moisan
#Date: March 2021

#------------------#
#### Librairies ####
#------------------#
library(igraph)
library(dplyr)
library(sf)
#------------------------------------------------#
#### Extract adjency matrices from shapefiles ####
#------------------------------------------------#
extract_adjency_m <- function(shp,
                              shp_track,
                              sp_list,
                              resident_sp,
                              sp_habitat,
                              sp_flyway,
                              data,
                              output_path){

  eco <- sf::st_read(shp)%>%
    sf::st_drop_geometry() %>% 
    dplyr::select(species, ecoregion, eco_type, flyway) %>% 
    unique()
  #----- Extract a data frame with all the ecoregions connected to each species
  eco <- rbind.data.frame(eco, resident_sp)
  
  #----- Raw- Without filter
  eco.raw <- eco %>% 
    dplyr::select(species, ecoregion, eco_type) %>% 
    unique()
  
  #----- Filter by ecoregion type specific to each species
  eco.f_hab <- dplyr::semi_join(eco, sp_habitat, by= c("species","eco_type")) %>% 
    dplyr::select(species, ecoregion, eco_type) %>% 
    unique()
  #----- Filter by flyway type specific to each species
  eco.f_fly <- dplyr::semi_join(eco, sp_fly, by= c("species","flyway")) %>% 
    dplyr::select(species, ecoregion, eco_type) %>% 
    unique()
  #----- Filter by flyway  and habitat type specific to each species
  eco.f_fly_hab <- dplyr::semi_join(eco.f_fly, sp_habitat, by= c("species","eco_type")) %>% 
    dplyr::select(species, ecoregion, eco_type) %>% 
    unique()
  #--- Filter by tracking devices
  df_track <- sf::st_read(shp_track) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::select(species, ecoregion) %>% 
    unique()


#----- Transform into adjency matrix
#Raw
eco.raw <- table(unique(eco.raw[, c("species", "ecoregion")]))
#With habitat type filter
eco.f_hab <-table(unique(eco.f_hab[, c("species", "ecoregion")]))
#With flyway type filter
eco.f_fly <-table(unique(eco.f_fly[, c("species", "ecoregion")]))
#With habitat type and flyway filter
eco.f_fly_hab <-table(unique(eco.f_fly_hab[, c("species", "ecoregion")]))
#wWith Tracking device filter
eco.track <- table(unique(df_track[, c("species", "ecoregion")]))


#----- Export as .csv
#Raw - unweight
write.csv(eco.raw, paste(output_path, data, "_eco.raw.csv", sep= ""))
#Filter by habitat type 
write.csv(eco.f_hab, paste(output_path, data, "_eco.f_hab.csv", sep= ""))
#Filter by flyway
write.csv(eco.f_fly, paste(output_path, data, "_eco.f_fly.csv", sep= ""))
#Filter by flyway and habitat - unweight
write.csv(eco.f_fly_hab, paste(output_path, data, "_eco.f_fly_hab.csv", sep= ""))
#Tracking filter
write.csv(eco.track, paste(output_path, data, "_eco.track.csv", sep= ""))
}

#---Example
#extract_adjency_m(shp= "data/shapefiles/ebird_eco_fly.shp",
    #              sp_list= sp_meta_data,
    #              resident_sp = bylot_sp,
    #              sp_habitat= sp_eco_type,
    #              sp_flyway= sp_fly,
    #              data= "ebird",
    #              output_path= "data/adjency_matrix/"

#-------------------------------------------------------------------------------------#
#### Manipulate a .csv matrix of each ecoregion by species into an adjency matrix  ####
#-------------------------------------------------------------------------------------#
csv_to_eco_sp_matrix <- function(x) {
 i <-  read.csv(x, check.names = FALSE)#Read .csv ecoregions used by each species
 colnames(i)[1] <- "species" #Set a column name for the species factor
 rownames(i) <- i$species #Change the rownames for the species name
 i <- i[-1]  #Remove the column used to stored the species name
 i <- i[ order(row.names(i)), ]#Order the rows to get the same as the trophic web
 
}
#Example
# matrix_eco_birdlife_fly <- csv_to_eco_sp_matrix("data/adjency_matrix/birdlife_flyway_eco.csv")




#------------------------------------------------------------------#
#### Extract a data frame with the number of region per species ####
#------------------------------------------------------------------#
#--- With Ecoregion
numb_region_sp <- function(adjency_matrix, filter, data, sp_list, scale){
#Read adjency matrix
data_region <- csv_to_eco_sp_matrix(adjency_matrix) 
#Remove weight
if (scale %in% c("province", "realm")){
  data_region[data_region> 0] <- 1
}
#Get the number of Ecoregion by species
data_region <- data.frame(species= rownames(data_region), region= rowSums(data_region))
colnames(data_region)[2] <- scale
data_region$data <- data
data_region$filter <- filter
#Keep only species present in the species list
data_region <- data_region %>%
  dplyr::filter(species %in% sp_list)
return(data_region)
}



#---------------------------------------------------------------#
####  Extract  number of region per species per data, filter ####
#---------------------------------------------------------------#
extract_nb_region_data <- function(data,
                                   scale,
                                   scale_abbrev){
#---- Raw
data_eco <- numb_region_sp(paste("data/adjency_matrix/", data,"_",scale_abbrev,".raw.csv", sep = ""),
                            scale= scale,
                            filter= "raw",
                            data= data, 
                            sp_list = long_dist_migrant$species)
#---- Habitat filter
data_hab_eco <- numb_region_sp(paste("data/adjency_matrix/", data,"_",scale_abbrev, ".f_hab.csv", sep = ""),
                                scale= scale,
                                filter= "habitat",
                                data= data, 
                                sp_list = long_dist_migrant$species)
#---- Flyway filter
data_fly_eco <- numb_region_sp(paste("data/adjency_matrix/", data, "_",scale_abbrev,".f_fly.csv", sep = ""),
                                scale= scale,
                                filter= "flyway",
                                data= data, 
                                sp_list = long_dist_migrant$species)
#----- Flyway and Habitat filter
data_fly_hab_eco <- numb_region_sp(paste("data/adjency_matrix/", data, "_",scale_abbrev,".f_fly_hab.csv", sep = ""),
                                    scale= scale,
                                    filter= "flyway and habitat", 
                                    data= data, 
                                    sp_list = long_dist_migrant$species)
#---- Tracking filter
data_track_eco <- numb_region_sp(paste("data/adjency_matrix/", data,"_",scale_abbrev,".track.csv", sep = ""),
                               scale= scale,
                               filter= "tracking", 
                               data= data, 
                               sp_list = long_dist_migrant$species)
#Create a list of data frame
list <- list(data_eco, data_hab_eco, data_fly_eco, data_fly_hab_eco, data_track_eco)

#Set correspondent name to each data frame
names(list) <- c(paste(data, "_", scale_abbrev, sep= ""),
                    paste(data, "_hab", "_", scale_abbrev, sep= ""),
                    paste(data, "_fly", "_", scale_abbrev, sep= ""),
                    paste(data, "_fly_hab", "_", scale_abbrev, sep= ""),
                    paste(data, "_track", "_", scale_abbrev, sep= ""))
return(list)
}
