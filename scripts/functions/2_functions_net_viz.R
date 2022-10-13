#Title: Functions to analyse migration network with trophic interactions
#Author: Louis Moisan
#Date: June 2021

#------------------#
#### Librairies ####
#------------------#
library(igraph)
library(dplyr)
library(circlize)
library(ggplot2)
library(RColorBrewer) #Create color palette
library(paletteer)
library(ggsci)
source("scripts/functions/1_functions_data_manip.R") #functions for data manipulatio

#--------------------------#
#### Plot Chord Diagram ####
#--------------------------#
chordDiagram_Network <- function(file_name,
                                 adjency_list,
                                 node_color,
                                 edge_color,
                                 labels,
                                 order,
                                 start.degree,
                                 group,
                                 width,
                                 heigh,
                                 label_size,
                                 transparency,
                                 gap.degree,
                                 small.gap,
                                 edge.border.size) {
  #Define name and size of the .pdf file to export
  svg(file_name, #file name
      width = (width/25.4), #enter in mm, 25.4 to convert from mmm to inch
      heigh= (heigh/25.4),
      bg= "transparent")
  
  circlize::circos.clear() 
  
  #Plot
  circlize::circos.par(start.degree = start.degree)
  chordDiagram(adjency_list, #edge list
               grid.col = node_color, #node color
               transparency = transparency,
               col= edge_color, #edge color
               annotationTrack = "grid",
               big.gap = gap.degree,
               group = group,
               link.lwd = edge.border.size,
               small.gap = small.gap,
               link.border = "grey50",
               preAllocateTracks = 
                 list(track.height = max(strwidth(unlist(dimnames(adjency_list))))),
               order= order) #node order 
  if (labels == TRUE) {
    #Labels
    circlize::circos.track(track.index = 1, panel.fun = function(x, y) {
      circlize::circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, 
                  facing = "clockwise",
                  niceFacing = TRUE,
                  cex= label_size,
                  adj = c(0, 0.5))},
      bg.border = NA)  }
  
  dev.off()
}



#### Plot modular matrix ####
plot_modular_matrix_modif <- function(x, fix_coordinates=T, axes_titles=c('Set 1', 'Set 2'), transpose=F, outside_module_col='gray'){
  
  # Add module affiliations to the edge list, module 1 is the affiliation of the node from Set1; module2 is the affiliation of the node from Set2
  M_set1 <- M_set2 <- x$edge_list[1:3]
  names(M_set1) <- names(M_set2) <- names(x$edge_list)[1:3] <- c('Set1','Set2','w')
  suppressMessages(suppressWarnings(M_set1 %<>% left_join(x$modules, by=c('Set1'='node_name')) %>% rename(module1=module_level1)))
  suppressMessages(suppressWarnings(M_set2 %<>% left_join(x$modules, by=c('Set2'='node_name')) %>% rename(module2=module_level1)))
  # Join into a single tibble
  suppressMessages(suppressWarnings(
    M <- full_join(M_set1, M_set2, by = c("Set1", "Set2", "w")) %>% 
      dplyr::select(Set1, Set2, w, module1, module2)
  ))
  # Order by modules
  Set1_modules <- unique(M_set1[,c('Set1','module1')])
  Set1_modules <- with(Set1_modules, Set1_modules[order(module1,Set1),])
  Set2_modules <- unique(M_set2[,c('Set2','module2')])
  Set2_modules <- with(Set2_modules, Set2_modules[order(module2,Set2),])
  M %<>% 
    mutate(edge_in_out=ifelse(module1==module2,'in','out')) %>% # Determine if an interaction falls inside or outside a module
    mutate(value_mod=ifelse(edge_in_out=='in',module1,0)) %>% # Assign a module value of 0 if interaction falls outside the modules
    mutate(Set1=factor(Set1, levels=Set1_modules$Set1), Set2=factor(Set2, levels=Set2_modules$Set2))
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
  
  
  a <- Set2_modules %>% group_by(module2) %>% summarise(n=n())
  a$colors <- colors[1: nrow(a)]
  # Join the module colors to the edge list
  # If there are no interactions outside the module then do not need the gray
  # color. Otherwise, it will plot the first module in gray.
  
  suppressMessages(M %<>% left_join(module_colors) %>%
                     mutate(col=ifelse(edge_in_out=='in',col,outside_module_col)))
  
  # Generate a plot of a modular matrix
  if (transpose){
    p <- ggplot()+
      geom_tile(data=M %>% filter(w!=0), aes(Set2, Set1, fill=col)) # Interactions within modules
  } else {
    p <- ggplot()+
      geom_tile(data=M %>% filter(w!=0), aes(Set1, Set2, fill=col)) # Interactions within modules
    # geom_tile(data=M %>% filter(w==0), aes(Set1, Set2), fill='white') + # Add nodes with no interactions, if they exist
  }
  p <- p +
    labs(x=axes_titles[2], y=axes_titles[1]) +
    scale_fill_identity()+
    ggplot2::theme_bw() +  
    ggplot2::scale_x_discrete(drop = FALSE) +
    ggplot2::scale_y_discrete(drop = FALSE) +
    theme(legend.position='none',
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(hjust = 1, colour = c(rep(a$colors, times= a$n)), face= "bold"),
          axis.title = element_text(face= "bold", size= 14))
  if (fix_coordinates){
    p <- p+coord_fixed()
  }
  return(p)
}

