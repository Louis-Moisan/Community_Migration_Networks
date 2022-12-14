#Title: Figure 6.  Partition of the Bylot Island community migration network with the highest modularity 
#Author: Louis Moisan
#Date: November 9 2021
#Review: September 2 2022


#------------------#
#### Librairies 
library(infomapecology) #Run modularity analysis based on flow
#devtools::install_github('Ecological-Complexity-Lab/infomap_ecology_package', force=T)
library(dplyr) #Data manipulation
library(bipartite) #Network analysis
library(igraph) #Extract adjency list
source("scripts/functions/1_functions_data_manip.R") #function of data manipulation
source("scripts/functions/2_functions_net_viz.R") #function of data visualization

#### Import data 
#Ecoregions
optimal_eco <- csv_to_eco_sp_matrix("data/adjency_matrix/optimal_eco.csv") %>%
  as.matrix()
#Set species name to lower cases
row.names(optimal_eco) <- tolower(row.names(optimal_eco))

#### Meta-data
sp_colors <- read.csv("data/metadata/species_colors.csv") %>% 
  dplyr::mutate(species = tolower(species))

#Create the network object
optimal_eco_net <- infomapecology::create_monolayer_object(optimal_eco, bipartite = T, directed = F, group_names = c('Migratory species','Ecoregions'))
#Extract flow values for each node
optimal_eco_modules <- infomapecology::run_infomap_monolayer(optimal_eco_net, infomap_executable='Infomap',
                                                             flow_model = 'undirected',
                                                             silent=T, two_level=T, 
                                                             signif = F)
optimal_eco_modules$L



x= optimal_eco_modules
axes_titles=c('Species', 'Ecoregions')
outside_module_col='#f4d7d7ff'


# Add module affiliations to the edge list, module 1 is the affiliation of the node from Set1; module2 is the affiliation of the node from Set2
M_set1 <- M_set2 <- x$edge_list[1:3]

names(M_set1) <- names(M_set2) <- names(x$edge_list)[1:3] <- c('Set1','Set2','w')

M_set1 %<>%
  dplyr::left_join(x$modules, by=c('Set1'='node_name')) %>%
  dplyr::rename(module1=module_level1)

M_set2 %<>%
  dplyr::left_join(x$modules, by=c('Set2'='node_name')) %>%
  dplyr::rename(module2=module_level1)

# Join into a single tibble
  M <- dplyr::full_join(M_set1, M_set2, by = c("Set1", "Set2", "w")) %>% 
    dplyr::select(Set1, Set2, w, module1, module2)

# Order by modules
Set1_modules <- unique(M_set1[,c('Set1','module1')])
Set1_modules <- with(Set1_modules, Set1_modules[order(module1,Set1),])


Set2_modules <- unique(M_set2[,c('Set2','module2')])
Set2_modules <- with(Set2_modules, Set2_modules[order(module2,Set2),]) %>% 
  as.data.frame() %>% 
  dplyr::left_join(sp_colors, by= c("Set2" = "species")) %>% #Order species 
  dplyr::arrange(desc(non_breeding_strategy),module2,trophic_level, sub_order) %>% dplyr::select(Set2, module2)

M %<>% 
  dplyr::mutate(edge_in_out=ifelse(module1==module2,'in','out')) %>% # Determine if an interaction falls inside or outside a module
  dplyr::mutate(value_mod=ifelse(edge_in_out=='in',module1,0)) %>% # Assign a module value of 0 if interaction falls outside the modules
  dplyr::mutate(Set1=factor(Set1, levels=Set1_modules$Set1), Set2=factor(Set2, levels=rev(Set2_modules$Set2)))





# Join the module colors to the edge list
# If there are no interactions outside the module then do not need the gray
# color. Otherwise, it will plot the first module in gray.
a <- Set2_modules %>%
  dplyr::group_by(module2) %>% 
  dplyr::summarise(n=n())


# Define module colors
colors <-c("#000000", #ruby-red 
           "#660708", #Peregine Falcon
           "#cc3f0c", #Shorebirds coastal
           "#ff6d00", #Shorebird terrestrial
           "#002855", #Jaeger/Phalarope
           "#5890ff", #King eider
           "#fcbf49", #Common-ringed Plover
           "#8ecae6", #Pacific Loon
           "#495057", #partial migrant
           "#adb5bd" #resident species
)
module_colors <- tibble(module1=sort(unique(M$module1)), col=colors[1:length(unique(M$module1))]) 


M %<>%
  dplyr::left_join(module_colors) %>%
  dplyr::mutate(col=ifelse(edge_in_out=='in',col,outside_module_col)) 

#Add resident species
resident_sp <- data_frame(Set1 = rep(NA, times= 5), Set2= c("rock ptarmigan", "ermine", "collared lemming", "brown lemming", "arctic hare"), w= rep(0, times= 5), module1= rep(NA, times= 5), module2= rep(NA, times= 5), edge_in_out=rep(NA, times= 5), value_mod=rep(NA, times= 5), col= rep(NA, times= 5))

M <- rbind.data.frame(M, resident_sp)

#Colors for species label
y.lab.col <-  dplyr::left_join(data.frame(species= rev(levels(M$Set2))), sp_colors, by= "species") %>% 
  dplyr::filter(vertebrate == "Y") %>% 
  dplyr::select(func_group_col) %>% 
  dplyr::mutate(func_group_col= as.character(func_group_col))

#---Define bounding boxes for each modules
# y min and max values for each boxe
y_coords_boxes <-  Set2_modules %>%
  #Set partial migrant module to 0 so it appears first on the axis
  dplyr::mutate(module2 = replace(module2, module2 == 6, 0)) %>% 
  dplyr::arrange(desc(module2)) %>% 
  dplyr::mutate(y_coords= 1:length(module2)) %>% 
  dplyr::group_by(module2) %>% 
  dplyr::summarise(min_y= min(y_coords), max_y= max(y_coords))

x_coords_boxes <-  Set1_modules %>% 
  dplyr::mutate(module1 = replace(module1, module1 == 6, 0)) %>% 
  dplyr::mutate(x_coords= 1:length(module1)) %>% 
  dplyr::group_by(module1) %>% 
  dplyr::summarise(min_x= min(x_coords), max_x= max(x_coords))

boxes_coords <- dplyr::left_join(x_coords_boxes, y_coords_boxes, by= c("module1"= "module2")) %>% 
  dplyr::rename(module= module1)


#Plot incidence matrix as heat map with species and rows order by modules
svg("figures/Figure_6-Bylot_modularity/Figure6.svg", #file name
    width = (180/25.4), #enter in mm, 25.4 to convert from mmm to inch
    heigh= (120/25.4),
    bg = "white") #background color) 


# Generate a plot of a modular matrix
ggplot()+
    geom_tile(data=M, aes(Set1, Set2, fill="black"))+
  geom_tile(data=M %>% filter(w==0), aes(Set1, Set2, fill="white"))+# Interactions  whitin modules
  labs(x=axes_titles[2], y=axes_titles[1]) +
  geom_tile(data= M %>% filter(edge_in_out == "out"),aes(Set1, Set2, fill="#e8c9ab"))+
  scale_fill_identity()+
  ggplot2::theme_bw() +  
  ggplot2::scale_x_discrete(drop = FALSE) +
  ggplot2::scale_y_discrete(drop = FALSE) +
  geom_rect(aes(xmin = boxes_coords$min_x -1, xmax = boxes_coords$max_x+1, ymin = boxes_coords$min_y-0.4, ymax = boxes_coords$max_y+0.4),
            fill = "transparent", color = "red", size = 1.5)+
  theme_classic()+
  theme(legend.position='none',
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(hjust = 1, colour = rev(y.lab.col$func_group_col), face= "bold"),
        axis.title = element_text(face= "bold", size= 14))

dev.off()
