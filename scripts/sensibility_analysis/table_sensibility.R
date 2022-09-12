#Title: Determining the efficacity of filters to reduce the number of biogeographic region (ecoregion. province, realm) of non-breeding range for each taxonomic group
#Author: Louis Moisan
#Date: July 19, 2021

#-------------------#
##### Librairies ####
#-------------------#
library(tidyverse)

#-------------------#
#### Import data ####
#-------------------#
#----- Ecoregion
species_eco <- read.csv("data/species_region_filter.csv") %>%
  dplyr::select(species, data, filter, ecoregion) %>% 
  arrange(species, data, desc(ecoregion))


#Set empty column to store proportion of reduction in number of region
species_eco$prop_eco <- as.numeric(NA)
#Define proportion of reduction in the  number of region by each filter
for (species in unique(species_eco$species)){ #For each species
  print(species)
  a <-species_eco[species_eco$species == species,]
    for (data in unique(a$data)){ #For each data type
      print(data)
      b <- a[a$data ==  data,]
        for (filter in unique(b$filter)){ #For each filter type
      species_eco[(species_eco$species == species & species_eco$data == data & species_eco$filter == filter),]$prop_eco   <- (1- (b[b$filter == filter,]$ecoregion / b[b$filter == "raw",]$ecoregion))*100 #Get the proportion of reduction in the number of ecoregion with raw data
    }
  }
}
#Create table
table_species_eco <- species_eco %>% filter(!(filter== "raw"))

#Export as .csv
write.csv(table_species_eco, "data/table_filter_region_raw.csv")

#-----------------------------------------------------------#
#### Test effect of each filter on the non-breeding area ####
#-----------------------------------------------------------#
#ecoregion 
eco_aov <- aov(prop_eco ~ filter + data+ data:filter, data = species_eco)
summary(eco_aov)

species_eco_summary <- species_eco %>% 
  group_by(data, filter) %>% 
  dplyr::summarise(mean= mean(prop_eco), sd= sd(prop_eco), min= min(prop_eco), max= max(prop_eco), median= median(prop_eco), n= n()) %>% 
  filter(!(filter == "raw")) %>% 
  mutate(filter= factor(filter, levels= c("flyway and habitat", "flyway", "habitat", "tracking")))

#Remove buffer zone as data source
species_eco_summary <- species_eco_summary %>% 
  filter(! data == "buffer zone") %>% 
  droplevels()

#add sample size in character
species_eco_summary$label <- paste("n= ", species_eco_summary$n, sep= "")


pdf("article/figures/FilterEfficiencyEcoregion.pdf", #file name
    bg = "white", #background color
    colormodel = "cmyk",
    width= 7,
    height = 3.3,
    paper= "special") 


ggplot(data=species_eco_summary, aes(x=filter, y=mean,fill=data)) + 
  geom_bar(stat="identity",
           position=position_dodge(0.9,preserve='single'),
           colour="black",
           lwd=0.25) + 
  geom_errorbar(stat='identity',
                aes(ymin=mean, ymax=mean+sd),
                position=position_dodge(0.9, preserve = "single"),
                width=0.5, 
                size=0.5,
                color= "grey35") +
  geom_text(aes(label = label), 
            position = position_dodge(0.9),
            vjust= +3.5,
            size=2.5)+
  scale_fill_manual("Data", values = c("birdlife" = "#EC7E70", "ebird" = "#7BD7A4", "ebird_birdlife" = "#E5D580", "tracking" = "#264653"),labels= c("Birdlife", "eBird", "eBird-Birdlife", "Tracking"))+
  scale_x_discrete("Filters", labels = c("flyway and habitat" = "Flyway and Habitat", "flyway"= "Flyway", "habitat"= "Habitat", "tracking"="Tracking"))+
  labs(x= "Filter applied", y= "Proportion of ecoregion filtered (%)")+
  scale_y_continuous(expand = c(0, 0))+
  theme_classic()

dev.off()
