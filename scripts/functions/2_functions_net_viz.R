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
