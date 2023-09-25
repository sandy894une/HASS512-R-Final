# title: make-maps-facet-by-motif.R

# description: use ggplot2 to plot Jaccard similarity matrix
# author: 'Sandy Pullen'
# date: '2023-09-21'

library(sf)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(dplyr)


iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-motifs-subset.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

fileSuffix = "by-motif"

plotTitle <- "Motif Locations "

  
  for (i in 5:19) {   
    print(i)
    
    thisMotif <- readBinaryFile[,c(1:4,i)]
    thisMotif <-subset(thisMotif, thisMotif[5] == 1)
    plotsubTitle <-names(thisMotif[5])
    
      #create the plots
      #y_limits <- c(31, 50)
      y_limits <- c(41, 50)
      p<-ggplot(iranSF)+
        labs(title=plotTitle, subtitle=plotsubTitle) +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=thisMotif, aes( x=Long, y=Lat),size = 2, color = "darkblue",alpha=.8,stat = "unique" ) +
        geom_text_repel(data=thisMotif, aes( x=Long, y=Lat, label=Sites),stat = "unique",
                        size=4.0,
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
       
        #xlim(51, 54.5) +
        #ylim(28.5, 31.5) +
        xlim(NA, 65) +
        ylim(NA, 43) +
        labs(color = "Jaccard Similarity Score") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.direction="horizontal",
              strip.text = element_text(size = 8))
      

      saveplot=paste0('maps/motifs/motifs-',i,'.png')
      ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")

  }   
