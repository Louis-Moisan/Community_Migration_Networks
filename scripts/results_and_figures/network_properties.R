#Title: Analysing the node and network level properties of Bylot Island Community migration network
#Author: Louis Moisan
#Date: November 9 2021
#Review: September 2 2022


#------------------#
#### Librairies ####
#------------------#
library(infomapecology) #Run modularity analysis based on flow
library(econullnetr) #Allow comparaison to null models for ecological networks
library(dplyr) #Data manipulation
library(bipartite) #Network analysis
library(igraph) #Extract adjency list
source("scripts/functions/1_functions_data_manip.R") #function of data manipulation
source("scripts/functions/2_functions_net_viz.R") #function of data visualization


#-------------------#
#### Import data ####
#-------------------#
#Ecoregions
optimal_eco <- csv_to_eco_sp_matrix("data/adjency_matrix/optimal_eco.csv") %>%
  as.matrix()

optimal_eco_no_falcon <- optimal_eco[ -which(names(optimal_eco[,1]) == "Peregrine Falcon"),] #Remove Peregrine Falcon to test if nestedness change

#-----------------#
#### Meta-data ####
#-----------------#
#Species attributes and colors
sp_col <- read.csv("data/metadata/species_colors.csv") %>%
  dplyr::mutate(func_group_col = as.character(func_group_col))

sp_order <- sp_col$species


#Ecoregion type
eco_type <- read.csv("data/metadata/ecoregion_type.csv") %>% 
  dplyr::arrange(eco_type, realm, province)%>%
  dplyr::select(ecoregion, eco_type)
eco_type <-  eco_type[eco_type$ecoregion %in% colnames(optimal_eco),] %>%
  unique() %>%
  dplyr::arrange(eco_type)
eco_order <- eco_type[eco_type$ecoregion %in% colnames(optimal_eco),] %>% unique() %>% 
  dplyr::arrange(eco_type) %>% 
  dplyr::select(ecoregion) 

#-----------------------------#
#### Node level properties ####
#-----------------------------#
species_degree <- rowSums(optimal_eco)
ecoregion_degree <- colSums(optimal_eco)
node_degree <- c(species_degree, ecoregion_degree)

#Number of species
nb_sp <- nrow(optimal_eco)
#Number of eco
nb_eco <- ncol(optimal_eco)
#Maximum degree ecoregion 
max(ecoregion_degree)
#Maximum degree species
max(species_degree)
#Minimum degree ecoregion 
min(ecoregion_degree)
#Minimum degree species 
min(species_degree)
#Mean degree ecoregions
mean(ecoregion_degree)
sd(ecoregion_degree)
#Mean degree species
mean(species_degree)
sd(species_degree)
#Mean degree network
mean(node_degree)
sd(node_degree)

#---------------------------------#
##### Network level properties ####
#---------------------------------#
#----- Connectance
sum(optimal_eco) / (nb_sp*nb_eco)

#----- Asymmetry
(nb_sp - nb_eco)/ (nb_sp + nb_eco)

#----- Modularity
#Create the network object
optimal_eco_net <- infomapecology::create_monolayer_object(optimal_eco, bipartite = T, directed = F, group_names = c('Migratory species','Ecoregions'))
#Extract flow values for each node
optimal_eco_modules <- infomapecology::run_infomap_monolayer(optimal_eco_net, infomap_executable='Infomap',
                                        flow_model = 'undirected',
                                        silent=T, two_level=T, 
                                        signif = F)
optimal_eco_modules$L


#Extract p value based on fixed null models (quasiswap = shuffle.web from bipartite)
optimal_eco_modules_null <- run_infomap_monolayer(optimal_eco_net, infomap_executable='Infomap',
                                             flow_model = 'undirected',
                                              silent=T,
                                             two_level=T, 
                                             signif = T,
                                             shuff_method = "quasiswap",
                                             nsim=1000, 
                                             verbose = TRUE)

#Extract list of Ecoregions and modules
eco_module_list <- optimal_eco_modules$modules %>% 
  dplyr::select(node_name, module_level1) %>%
  dplyr::left_join(eco_type, by = c("node_name" = "ecoregion")) %>% 
  na.omit() %>% 
  dplyr::mutate(module= replace(module_level1, module_level1 ==6, 0)) %>% 
  dplyr::mutate(module = factor(module, labels = c(1:length(unique(module)))))%>% 
  dplyr::mutate(ecoregion = node_name) %>% 
  dplyr::left_join(eco_type) %>% 
  dplyr::select(ecoregion, eco_type, module) %>% 
  dplyr::arrange(module, eco_type)  
write.csv(eco_module_list, "data/modules/EcoregionsModulesList.csv")

#Number of ecoregions in each modules
optimal_eco_modules$modules %>%
  dplyr::filter(node_name %in% eco_type$ecoregion) %>%
  dplyr::group_by(module_level1) %>%
  dplyr::summarise(nb_eco= n())
#Number of species in each modules
 optimal_eco_modules$modules %>%
   dplyr::filter(node_name %in% sp_col$species) %>%
   na.omit() %>%
   dplyr::group_by(module_level1) %>%
   dplyr::summarise(nb_eco= n())


# Plot histograms
plots <- plot_signif(optimal_eco_modules_null, plotit = T)
plot_grid(
  plots$L_plot+
    theme_bw()+
    theme(legend.position='none', 
          axis.text = element_text(size=20), 
          axis.title = element_text(size=20)),
  plots$m_plot+
    theme_bw()+
    theme(legend.position='none', 
          axis.text = element_text(size=20), 
          axis.title = element_text(size=20))
)



#------ Nestedness 
  #Nestedness temperature observe
  Tobs_eco <- bipartite::networklevel(optimal_eco, weighted = FALSE, index = 'nestedness') 
  #Generate probabilistic 
  nulls_eco <- bipartite::nullmodel(optimal_eco, method= 4, N= 1000)
  #Calcul nestedness of null models
  Tnulls_eco <- sapply(nulls_eco, function(x) nestedtemp(x, weighted= F)$statistic[[1]])


  #Plot 
  plot(density(Tnulls_eco), xlim=c(0, 40), lwd=2, main="NODF")+
    abline(v=Tobs_eco, col="red", lwd=2)
  #p value
  sum(Tnulls_eco< Tobs_eco)/1000
  
  
  #-----Ecoregions without Peregrine Falcon
  #Nestedness temperature observe
  Tobs_eco_no_falcon <- bipartite::networklevel(optimal_eco_no_falcon, weighted = FALSE, index = 'nestedness') 
  #Generate probabilistic 
  nulls_eco_no_falcon <- bipartite::nullmodel(optimal_eco_no_falcon, method= 4, N= 1000)
  #Calcul nestedness of null models
  Tnulls_eco_no_falcon <- sapply(nulls_eco_no_falcon, function(x) nestedtemp(x, weighted= F)$statistic[[1]])
  #Plot 
  plot(density(Tnulls_eco_no_falcon), xlim=c(0, 40), lwd=2, main="NODF")+
    abline(v=Tobs_eco_no_falcon, col="red", lwd=2)
  #p value
  sum(Tnulls_eco_no_falcon< Tobs_eco_no_falcon)/1000
  
  
  
  #----------------------------------------#
  #### Histogram of degree distribution ####
  #----------------------------------------#
  #-----Ecoregions
 svg("figures/Supplementary/Supplementary_Figure_4-Degree_Distribution/Degree_Distribution.svg", #file name
      bg = "transparent" #background color
  ) 
  
  
  
  eco_degree <- ggplot(aes(x=degree), data= data.frame(degree= as.numeric(ecoregion_degree))) +
    geom_density(fill="#6c757d", color="#000000", alpha=0.4)+
    scale_x_continuous(expand = c(0,0), limits=c(0, 15)) +
    scale_y_continuous(expand = c(0, 0), n.breaks = 6)+ 
    xlab("Degree of ecoregions")+
    ylab("Density")+
    theme_classic()+
    theme(axis.text.x = element_text( color="#343a40", 
                                      size=12),
          axis.text.y = element_text( color="#343a40", 
                                      size=12),
          axis.title.y = element_text( color="#212529", 
                                       size=12),
          axis.title.x = element_text( color="#212529", 
                                       size=12))
  #Histogram of species degree
  sp_degree <- ggplot(aes(x=degree), data= data.frame(degree= as.numeric(species_degree))) +
    geom_density(fill="#343a40", color="#000000", alpha=0.8)+
    scale_x_continuous(expand = c(0,0), limits=c(0, 300)) +
    scale_y_continuous(expand = c(0, 0), limits=c(0, 0.025))+ 
    xlab("Degree of species")+
    ylab("")+
    theme_classic()+
    theme(axis.text.x = element_text(color="#343a40", 
                                     size=12),
          axis.text.y = element_text( color="#343a40", 
                                      size=12),
          axis.title.y = element_text(color="#212529", 
                                      size=12),
          axis.title.x = element_text(color="#212529", 
                                      size=12))
  
  cowplot::plot_grid(eco_degree, sp_degree, labels = "AUTO")    
  
  dev.off()
