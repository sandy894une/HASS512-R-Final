# title: make-maps-threshold.R

# description: use ggplot2 to plot Jaccard similarity matrix - plots all occurences on one map
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


#-------------------------------------------------------------------------------
#the function
make_maps <- function(theInputSimFile, thisSubtitle,fileSuffix,thisSize){
  

 
    S1SimData <- theInputSimFile %>% rename("fromCode" = "S1Code",
                                      "fromWare" = "S1Ware",
                                      "fromLat" = "S1lat",
                                      "fromLong" = "S1long",
                                      "toCode" = "S2Code",
                                      "toWare" = "S2Ware",
                                      "toLat" = "S2lat",
                                      "toLong" ="S2long")
    

    S2SimData <- theInputSimFile %>% rename("fromCode" = "S2Code",
                                      "fromWare" = "S2Ware",
                                      "fromLat" = "S2lat",
                                      "fromLong" = "S2long",
                                      "toCode" = "S1Code",
                                      "toWare" = "S1Ware",
                                      "toLat" = "S1lat",
                                      "toLong" ="S1long")
    
    allsimdata<-rbind(S1SimData,S2SimData)
    

      
      allsimdata$JacSimCut <- cut(allsimdata$JacSim, breaks=c(0,0.299, 0.6499, 0.799, 0.999, 1.0), 
        labels=c("0-0.3","0.3-0.65","0.65-0.8", "0.8-0.9","0.9-1"))
      
      plotTitle ="Ceramic Attribute Similarity"

      
      #create the plot
      y_limits <- c(41, 50)
      ggplot(iranSF)+
        labs(title=plotTitle, subtitle=thisSubtitle) +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=allsimdata, aes( x=fromLong, y=fromLat),size = 2, color = "darkblue",alpha=.8 ,stat = "unique") +
        geom_point(data=allsimdata, aes( x=toLong, y=toLat),size = 0.5, color = "darkblue", alpha=.3, stat = "unique") +

        geom_segment(data=allsimdata,aes(x=fromLong, y=fromLat, xend=toLong,yend=toLat, color=JacSimCut),
                     inherit.aes = FALSE, size=0.5)+
        
        scale_color_manual(values=segmentColours, drop=F) +
        coord_sf() +   
        theme_light() + 
        scale_size_identity() +
        xlim(NA, 65) +
        ylim(NA, 40) +
        labs(color = "Jaccard Similarity Score") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.direction="horizontal",
              strip.text = element_text(size = 8))
      

      saveplot=paste0('maps/similarity-',fileSuffix,'.png')
      ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")
    }
  
#end function
#-------------------------------------------------------------------------------

#set parameters - only need to run this section once

iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-all.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

segmentColours<-c("lightgreen", "forestgreen","gold", "orange","royalblue")

sites <- as.data.frame(unique(readBinaryFile$Site.Code))
#------------------------------------------------------------------------------
# all data - maps with different thresholds
inputFile= 'data/Iran-compiled-all.csv'
readSimFile <- read.csv(inputFile, header=TRUE)

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]

#------------------------------------------------------------------------------
#run groups for all wares

#1 - similarity =1.0
print(1)
run1SimFile <- readSimFile[readSimFile$JacSim == 1.0, ]
run1Subtitle = "Similarity = 1.0"
run1FileSuffix = "-eq-1"
run1Size = 2.0
make_maps(run1SimFile, run1Subtitle, run1FileSuffix, run1Size)

