# title: make-maps-motif-by location.R

# description: use ggplot2 to plot motif by site
# author: 'Sandy Pullen'
# date: '2023-09-21'

library(sf)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(tidyr)


iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-motifs-v3-subset.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

fileSuffix = "by-motif"

plotTitle <- "Motif Locations "


longer_data <- readBinaryFile %>% select(1:4,5:23) %>% 
  pivot_longer(., cols = c(5:23), names_to = "motiftype", values_to = "present") %>%
  subset(present==1)



    
      #create the plots

      #y_limits <- c(33, 50)
      y_limits <- c(30.5, 50)
      p<-ggplot(iranSF)+
        labs(title=plotTitle, subtitle="") +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=longer_data, aes( x=Long, y=Lat),size = 1, color = "darkblue",alpha=.8) +
        geom_text_repel(data=longer_data, aes( x=Long, y=Lat, label=Sites),
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

        #xlim(NA, 65) +
        #ylim(NA, 40) +
        xlim(51, 54.5) +
        ylim(28.5, 31.5) +
        labs(color = "Jaccard Similarity Score") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.direction="horizontal",
              strip.text = element_text(size = 8))
      
      p+ facet_wrap(vars(motiftype), ncol = 4)
        
      saveplot=paste0('maps/motifs/motifs-by-location.png')
      ggsave(saveplot, bg="white",width = 30, height = 30, units = "cm")

 
