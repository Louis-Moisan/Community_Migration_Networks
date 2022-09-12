

#-------------------#
##### Librairies ####
#-------------------#
library(dplyr) #General data manipulation
library(igraph) #Creating, visualizing and analysisng networks objects
library(ggplot2) #General plot visualization
library(graphlayouts) #Multilevel layout for graph
library(ggraph) #Similar to ggplot for graph
library(circlize) # Allow Chord Diagram visualization
library(RColorBrewer) #Create color palette
source("scripts/functions/2_functions_net_viz.R") #functions for network visualization
source("scripts/functions/1_functions_data_manip.R") #functions for data manipulation

#------------------#
#### Data files ####
#------------------#
#---- Ecoregion type for each ecoregion and realms
eco_type <- read.csv("data/metadata/ecoregion_type.csv")
sp_col <- read.csv("data/metadata/species_colors.csv") %>%
  mutate(func_group_col = as.character(func_group_col))

resident_sp <- sp_col[sp_col$non_breeding_strategy == "Resident",]


#-------------------------#
#### Data manipulation ####
#-------------------------#
adjency_matrix.csv = "data/adjency_matrix/optimal_eco.csv"
remove_resident =FALSE
vertebrates_only= TRUE
biogeo_scale= "ecoregion"
taxo_scale = "species"
species_colors = "func_group_col"
unweight = TRUE
export.pdf= "figures/Figure_5-Chord_diagram/Figure5.svg"
width= 200
heigh= 200
labels= FALSE
label_size= 0
transparency= 0.2
small.gap = 0.6
start.degree = 50
gap.degree = 6
edge.border.size = 0.01
  
#Read indicence matrix
adjency_matrix <- csv_to_eco_sp_matrix("data/adjency_matrix/optimal_eco.csv")

#Remove non vertebrate species or groups
  adjency_matrix <- adjency_matrix[! (row.names(adjency_matrix) %in% (resident_sp$vertebrate == "N")),]

#Create igraph bipartite object
bipart_net <- graph.incidence(adjency_matrix)

#--- Assign an biogeoregion type (coastal, terrestrial or marine) to each region
bipart_net_type <- data.frame(a = colnames(adjency_matrix))
names(bipart_net_type) <- biogeo_scale
bipart_net_type <- left_join(bipart_net_type, eco_type[, c(biogeo_scale, "eco_type")], by = biogeo_scale) %>% unique()

#--- Assign eco_type and species label to igraph object
V(bipart_net)$type <- c(rep(taxo_scale, times= nrow(adjency_matrix)), as.character(bipart_net_type$eco_type))

#----- Extract edge list as a data frame
bipart_net_list <- igraph::get.data.frame(bipart_net)

#--------------------------#
#Define node colors #
#terrestrial realms color list
terrestrial_col <- rep("#008000",times= nrow(bipart_net_type[bipart_net_type$eco_type == "terrestrial",]))
#coastal realms color list
coastal_col <- rep("#5a189a",times =nrow(bipart_net_type[bipart_net_type$eco_type == "coastal",]))
#marine realms color list
marine_col <- rep("#023e8a", times=nrow(bipart_net_type[bipart_net_type$eco_type == "marine",]))

#species colors list
sp_col <- left_join(data.frame(species= rownames(adjency_matrix)), 
                    sp_col, by = taxo_scale)

#----- Assign a color for each node
grid.col = NULL
#terrestrial ecoregions colors
grid.col[names(V(bipart_net)[type == "terrestrial"])] <- terrestrial_col
#coastal ecoregions colors
grid.col[names(V(bipart_net)[type == "coastal"])] <- coastal_col
#marine ecoregions colors
grid.col[names(V(bipart_net)[type == "marine"])] <- marine_col
#species colors
grid.col[names(V(bipart_net)[type == taxo_scale])] <- sp_col[,species_colors]

#----- Define edges colors
col.df <- as.data.frame(grid.col, stringsAsFactors = FALSE)
col.df$node <- rownames(col.df)
#Define colors for edeges based on color of the ecoregion
col_mat <- left_join(bipart_net_list["to"],col.df, by= c("to"= "node"))
col_mat <- as.matrix(col_mat[2]) 

#Set the sector order
species_order <- data.frame(node= row.names(adjency_matrix)) %>%  #species name
  left_join(sp_col, by = c("node"= taxo_scale)) %>% arrange(non_breeding_strategy,trophic_level, sub_order)

region_order <- data.frame(node= colnames(adjency_matrix)) %>% left_join(unique(eco_type[c(biogeo_scale, "eco_type")]), by = c("node"= biogeo_scale)) %>% arrange(eco_type) #realm name

node_order <- as.matrix(rbind(species_order["node"], region_order["node"]))

#Define grouping structure
  group <-  structure(c(bipart_net_type$eco_type,
                        rep("Migrant", times= sum(species_order$non_breeding_strategy == "Migrant")),
                        rep("Partial migrant", times= sum(species_order$non_breeding_strategy == "Partial migrant")),
                        rep("Resident", times= sum(species_order$non_breeding_strategy == "Resident"))),
                      names =c(bipart_net_type$ecoregion, species_order$node))


####PLOT####
chordDiagram_Network(export.pdf, #file_name
                     adjency_list = bipart_net_list, #adjency list
                     node_color= grid.col, #node color
                     edge_color= col_mat, #edge color
                     start.degree= start.degree,
                     group= group, 
                     labels= labels,
                     order= node_order, #node order
                     width= width, #width in millimiter
                     heigh= heigh,
                     label_size= label_size,
                     transparency= transparency,
                     gap.degree= gap.degree,
                     small.gap,
                     edge.border.size)
