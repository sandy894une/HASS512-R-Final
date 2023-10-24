# title: make-maps-ceramics-motifs-no-facet.R

# description: use ggplot2 to plot Jaccard similarity matrix
# ceramic attributes subset - no facet, so all on one map
# author: 'Sandy Pullen'
# date: '2023-09-21'


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
inputFile= 'data/Iran-binary-ceramic-motif-subset.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

#segmentColours<-c("lightblue","lightgreen", "forestgreen","royalblue","gold", "orange","black")
segmentColours<-c("lightgreen","forestgreen","royalblue", "orange","black")

#sites <- as.data.frame(unique(readBinaryFile$Site.Code))
#------------------------------------------------------------------------------
# all data - maps with different thresholds
inputFile= 'data/Iran-compiled-ceramic-motif-subset.csv'
readSimFile <- read.csv(inputFile, header=TRUE)

theInputSimFile <- readSimFile[readSimFile$JacSim == 1, ]
thisSubtitle = "Similarity = 1.0"
fileSuffix = "eq1"

theInputSimFile <- readSimFile[readSimFile$JacSim >= 0.8, ]
thisSubtitle = "Similarity >= 0.8"
fileSuffix = "gt08"

theInputSimFile <- readSimFile[readSimFile$JacSim >= 0.65, ]
thisSubtitle = "Similarity >= 0.65"
fileSuffix = "gt065"
#-------------------------------------------------------------------------------


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
  #get the site names

  thisSiteCodes <-readBinaryFile
  for (i in 1:nrow(allsimdata)) {
    allsimdata$SiteName[i] <- thisSiteCodes[which(allsimdata$fromCode[i] == thisSiteCodes$Site.Code ), 1]
  }

  

  allsimdata$JacSimCut <- cut(allsimdata$JacSim, breaks=c(0,0.299, 0.6499, 0.799, 0.999, 1.0), 
                              labels=c("0-0.3","0.3-0.65","0.65-0.8", "0.8-0.9","0.9-1"))
      
  plotTitle <- "Ceramic Subset Jaccard Similarity "
      
      #create the plots
      y_limits <- c(31, 50)
      p<-ggplot(iranSF)+
        labs(title=plotTitle, subtitle=thisSubtitle) +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=allsimdata, aes( x=fromLong, y=fromLat),size = 2, color = "darkblue",alpha=.8, stat = "unique" ) +
        geom_point(data=allsimdata, aes( x=toLong, y=toLat),size = 0.5, color = "darkblue", alpha=.8, stat = "unique") +
 
        geom_segment(data=allsimdata,aes(x=fromLong, y=fromLat, xend=toLong,yend=toLat, color=JacSimCut),
                     inherit.aes = FALSE, size=0.5)+
        geom_text_repel(data=allsimdata, aes( x=fromLong, y=fromLat, label=SiteName),stat = "unique",
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
        scale_color_manual(values=segmentColours, drop=F) +
        coord_sf() +   
        theme_light() + 
        scale_size_identity() +
        xlim(51, 54.5) +
        ylim(28.5, 31.5) +
        labs(color = "Jaccard Similarity Score") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.direction="horizontal",
              strip.text = element_text(size = 8))
      

      saveplot=paste0('maps/motifs/ceramic attribute subset/ceramic-subset-nofacet-',fileSuffix,'.png')
      ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
      
 