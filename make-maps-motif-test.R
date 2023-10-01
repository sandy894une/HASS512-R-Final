# title: make-maps-facet-by-motif.R

# description: use ggplot2 to plot Jaccard similarity matrix
# author: 'Sandy Pullen'
# date: '2023-09-21'

library(sf)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(tidyr)


iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-motifs-subset.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

fileSuffix = "by-motif"

plotTitle <- "Motif Locations "

readBinaryFile %>% select(2,5:28)

longer_data <- readBinaryFile %>% select(1:4,5:28) %>% 
  pivot_longer(., cols = c(5:28), names_to = "motiftype", values_to = "present") %>%
  subset(present==1)



    
      #create the plots

      y_limits <- c(33, 50)
      p<-ggplot(iranSF)+
        labs(title=plotTitle, subtitle="") +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=longer_data, aes( x=Long, y=Lat),size = 1, color = "darkblue",alpha=.8) +
        geom_text_repel(data=longer_data, aes( x=Long, y=Lat, label=Site.Name),
                        size=2.0,
                        force_pull   = 0, # do not pull toward data points
                        nudge_y      = 0.05,
                        direction    = "x",
                        angle        = 90,
                        hjust        = 0,
                        segment.size = 0.2,
                        segment.color= "grey",
                        ylim  = y_limits,
                        max.overlaps = Inf)+
        
        coord_sf() +   
        theme_light() + 

        xlim(NA, 65) +
        ylim(NA, 40) +
        labs(color = "Jaccard Similarity Score") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.direction="horizontal",
              strip.text = element_text(size = 8))
      
      p+ facet_wrap(vars(motiftype), ncol = 4)
        
      saveplot=paste0('maps/motifs/motifs-test.png')
      ggsave(saveplot, bg="white",width = 30, height = 30, units = "cm")

 
