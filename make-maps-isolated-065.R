# title: make-maps-function.R
# description: use ggplot2 to plot just the nodes isolated from the soft ware horizonfrom the
# author: 'Sandy Pullen'
# date: '2023-07-04'


# sources
# https://stackoverflow.com/questions/69986414/add-label-to-provinces-to-iran-map-using-plot-geo-in-r
# https://ggplot2.tidyverse.org/reference/ggsf.html
# https://stackoverflow.com/questions/70481220/how-show-some-points-on-a-map-in-r
# https://stackoverflow.com/questions/46158896/r-ggplot-connecting-one-point-on-a-map-with-multiple-points-on-the-same-map?rq=3
# https://ggrepel.slowkow.com/articles/examples.html#align-labels-on-the-top-or-bottom-edge

library(sf)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(dplyr)
#set parameters - only need to run this section once

iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-all.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

segmentColours<-c("lightgreen", "forestgreen","gold", "orange","royalblue","purple","red", "black")

sites <- as.data.frame(unique(readBinaryFile$Site.Code))
#------------------------------------------------------------------------------
# all data - maps with different thresholds
inputFile= 'data/Iran-065-isolated.csv'
readSimFile <- read.csv(inputFile, header=TRUE)



thisSubtitle = "Isolated Nodes at 0.65"
fileSuffix = "-isolated-065"




  
  plotTitle <- "Network Plot - Isolated Nodes"

    S1SimData <- readSimFile %>% rename("fromCode" = "S1Code",
                                      "fromWare" = "S1Ware",
                                      "fromLat" = "S1lat",
                                      "fromLong" = "S1long",
                                      "toCode" = "S2Code",
                                      "toWare" = "S2Ware",
                                      "toLat" = "S2lat",
                                      "toLong" ="S2long")

    S2SimData <- readSimFile %>% rename("fromCode" = "S2Code",
                                      "fromWare" = "S2Ware",
                                      "fromLat" = "S2lat",
                                      "fromLong" = "S2long",
                                      "toCode" = "S1Code",
                                      "toWare" = "S1Ware",
                                      "toLat" = "S1lat",
                                      "toLong" ="S1long")
    
    allsimdata<-rbind(S1SimData,S2SimData)

      #create the plots
      y_limits <- c(41, 50)
      p<-ggplot(iranSF)+
        labs(title=plotTitle, subtitle=thisSubtitle) +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=allsimdata, aes( x=fromLong, y=fromLat),size = 1, color = "darkblue",alpha=.8 ,stat = "unique") +
        geom_point(data=allsimdata, aes( x=toLong, y=toLat),size = 0.5, color = "darkblue", alpha=.3, stat = "unique") +
        geom_text_repel(data=allsimdata, aes( x=toLong, y=toLat, label=toWare),stat = "unique",
                        size=3.0,
                        force_pull   = 0, # do not pull toward data points
                        nudge_y      = 0.05,
                        direction    = "x",
                        angle        = 90,
                        hjust        = 0,
                        segment.size = 0.2,
                        segment.color= "grey",
                        ylim  = y_limits,
                        max.overlaps = Inf
        ) +
        geom_segment(data=allsimdata,aes(x=fromLong, y=fromLat, xend=toLong,yend=toLat, color=as.factor(JacSim)),
                     inherit.aes = FALSE, size=0.5)+
        
        scale_color_manual(values=segmentColours, drop=F) +
        coord_sf() +   
        theme_light() + 
        scale_size_identity() +
        xlim(NA, 65) +
        ylim(NA, 45) +
        labs(color = "Node collections") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.direction="horizontal",
              strip.text = element_text(size = 8))
      

      
      saveplot=paste0('maps/',fileSuffix,'.png')
      ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")

    
 