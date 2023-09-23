# title: make-maps-motifs.R

# description: use ggplot2 to plot Jaccard similarity matrix
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
inputFile= 'data/Iran-binary-motifs-subset.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

segmentColours<-c("lightblue","lightgreen", "forestgreen","royalblue","gold", "orange","black")

sites <- as.data.frame(unique(readBinaryFile$Site.Code))
#------------------------------------------------------------------------------
# all data - maps with different thresholds
inputFile= 'data/Iran-compiled-motifs-subset.csv'
readSimFile <- read.csv(inputFile, header=TRUE)

#-------------------------------------------------------------------------------
#the function
motifs_make_maps <- function(theInputSimFile, thisSubtitle,fileSuffix){
  

  S1SimData <- theInputSimFile %>% rename("fromCode" = "S1Code",
                                  
                                    "fromLat" = "S1lat",
                                    "fromLong" = "S1long",
                                    "toCode" = "S2Code",
                                   
                                    "toLat" = "S2lat",
                                    "toLong" ="S2long")
  

  S2SimData <- theInputSimFile %>% rename("fromCode" = "S2Code",
                                    
                                    "fromLat" = "S2lat",
                                    "fromLong" = "S2long",
                                    "toCode" = "S1Code",
                                   
                                    "toLat" = "S1lat",
                                    "toLong" ="S1long")
  
  allsimdata<-rbind(S1SimData,S2SimData)
      #get the site name and ware names
  thisSiteCodes <-readBinaryFile
  facetLabels <- cbind(thisSiteCodes[2], thisSiteCodes[1])
  to_string <- as_labeller(facetLabels)
  
  allsimdata$JacSimCut <- cut(allsimdata$JacSim, breaks=c(0,0.2999,0.3999,0.5999,0.6499, 0.999, 1.0), 
                                       labels=c("0-0.3","0.3-0.4","0.4-0.5","0.5-0.65", "0.65-0.9","0.9-1"))
      
  plotTitle <- "Motif Jaccard Similarity"
      

      
      
      #create the plots
      y_limits <- c(31, 50)
      p<-ggplot(iranSF)+
        labs(title=plotTitle, subtitle=thisSubtitle) +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=allsimdata, aes( x=fromLong, y=fromLat),size = 2, color = "darkblue",alpha=.8,stat = "unique" ) +
        geom_point(data=allsimdata, aes( x=toLong, y=toLat),size = 0.5, color = "darkblue", alpha=.8,stat = "unique") +
 
        geom_segment(data=allsimdata,aes(x=fromLong, y=fromLat, xend=toLong,yend=toLat, color=JacSimCut),
                     inherit.aes = FALSE, size=0.5)+
        geom_text_repel(data=allsimdata, aes( x=toLong, y=toLat, label=toCode),stat = "unique",
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
      
      
      #add the ware description on the next line
      appender <- function(string, suffix = facetLabels[which(facetLabels$Site.Code %in% string), 2]) paste0(string, '\n', suffix)
      p+facet_wrap(~fromCode,labeller = as_labeller(appender))
     # p+facet_wrap(~fromWare,labeller = as_labeller(appender))
      
      saveplot=paste0('maps/motifs/',fileSuffix,'.png')
      ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
      
    }

#end function
#-------------------------------------------------------------------------------



#1 - similarity >= 0.1
print(1)
run1SimFile <- readSimFile[readSimFile$JacSim >= 0.1, ]
run1Subtitle = "Similarity >= 0.1"
run1FileSuffix = "-gt0-1"

motifs_make_maps(run1SimFile, run1Subtitle,run1FileSuffix)

#2 - similarity >= 0.3
print(2)
run2SimFile <- readSimFile[readSimFile$JacSim >= 0.3, ]
run2Subtitle = "Similarity >= 0.3"
run2FileSuffix = "-gt0-3"

motifs_make_maps(run2SimFile, run2Subtitle,run2FileSuffix)

#3 - similarity >= 0.65
print(3)
run3SimFile <- readSimFile[readSimFile$JacSim >= 0.65, ]
run3Subtitle = "Similarity >= 0.65"
run3FileSuffix = "-gt0-65"

motifs_make_maps(run3SimFile, run3Subtitle,run3FileSuffix)
#------------------------------------------------------------------------------

