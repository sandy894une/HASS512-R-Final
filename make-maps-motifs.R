# title: make-maps.R
# description: use ggplot2 to plot Jaccard similarity matrix
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

iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")


#run groups
#-------------------------------------------------------------------------------
#1 - all values of similarity - Ware Codes
#note - in general this is too crowded with text labels to be of use
inputFile= 'Iran-compiled-all-Jul18-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "All Values of Similarity - showing Ware Codes"
fileSuffix = "-all-ware-code"
thisSize = 1.0
#also change geom_text_repel ...label=toWare
#-------------------------------------------------------------------------------
#2 - all values of similarity > 0 - Site Codes
inputFile= 'Iran-compiled-all-Jul18-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")
readSimFile <- readSimFile[readSimFile$JacSim > 0, ]

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "Similarity > 0 - showing Site Codes"
fileSuffix = "-all-gt0-site-code"
thisSize = 1.5
#also change geom_text_repel ...label=toCode
#-------------------------------------------------------------------------------
#3 - similarity >= 0.8
inputFile= 'Iran-compiled-all-Jul20-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")
readSimFile <- readSimFile[readSimFile$JacSim >= 0.8, ]

#remove same site similarity when necessary
#readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "Similarity >= 0.8"
fileSuffix = "-gt0-8"
thisSize = 1.5
#also change geom_text_repel ...label=toWare
#-------------------------------------------------------------------------------
#3 - similarity >= 0.65
inputFile= 'Iran-compiled-all-Jul20-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")
readSimFile <- readSimFile[readSimFile$JacSim >= 0.65, ]

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "Similarity >= 0.65"
fileSuffix = "-gt0-65"
thisSize = 1.5
#also change geom_text_repel ...label=toWare
#-------------------------------------------------------------------------------
#4 - similarity =1
inputFile= 'Iran-compiled-all-Jul20-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")
readSimFile <- readSimFile[readSimFile$JacSim == 1.0, ]

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "Similarity =1.0"
fileSuffix = "-eq1"
thisSize = 1.5
#also change geom_text_repel ...label=toWare
#-------------------------------------------------------------------------------
#5 - Decorated Only
inputFile= 'Iran-decorated-sim-Jul18-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")
readSimFile <- readSimFile[readSimFile$JacSim > 0, ]

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "Decorated Wares Only, Similarity > 0 "
fileSuffix = "-painted"
thisSize = 1.0
#also change geom_text_repel ...label=toWare

#-------------------------------------------------------------------------------
#6 - Decorated Only, Similarity >=0.8
inputFile= 'Iran-decorated-sim-Jul20-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")
readSimFile <- readSimFile[readSimFile$JacSim >= 0.8, ]

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "Decorated Wares Only, Similarity >=0.8"
fileSuffix = "-painted-gt0-8"
thisSize = 1.5
#also change geom_text_repel ...label=toWare
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#6 - Decorated Only, Similarity >=0.65
inputFile= 'Iran-decorated-sim-Jul20-UTF.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")
readSimFile <- readSimFile[readSimFile$JacSim >= 0.65, ]

#remove same site similarity when necessary
readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
thisSubtitle = "Decorated Wares Only, Similarity >=0.65"
fileSuffix = "-painted-gt0-65"
thisSize = 1.5
#also change geom_text_repel ...label=toWare
#-------------------------------------------------------------------------------



#segmentColours <-c("#4ee07f", "#41b67c", "#1e4573", "#0c0c6f")
#segmentColours <-c("#52ee80","#4ee07f", "#4ad27e", "#45c47d", "#41b67c", "#2f7d78", "#1e4573", "#0c0c6f")
#segmentColours <-c("#e684ee", "#b066ce", "#7948af", "#432a8f", "#0c0c6f")
segmentColours<-c( "forestgreen","gold", "orange","royalblue")

sites <- as.data.frame(unique(readFile$ï..Site.Code))

#loop through each site code

for (i in 1:nrow(sites)) {

  #get the sim data for the site
  
  
  S1SimData<- readSimFile[readSimFile$S1Code %in% sites[[1]][[i]], ]
  S1SimData <- S1SimData %>% rename("fromCode" = "S1Code",
                                    "fromWare" = "S1Ware",
                                    "fromLat" = "S1lat",
                                    "fromLong" = "S1long",
                                    "toCode" = "S2Code",
                                    "toWare" = "S2Ware",
                                    "toLat" = "S2lat",
                                    "toLong" ="S2long")
  
  S2SimData <- readSimFile[readSimFile$S2Code %in% sites[[1]][[i]], ]
  S2SimData <- S2SimData %>% rename("fromCode" = "S2Code",
                                    "fromWare" = "S2Ware",
                                    "fromLat" = "S2lat",
                                    "fromLong" = "S2long",
                                    "toCode" = "S1Code",
                                    "toWare" = "S1Ware",
                                    "toLat" = "S1lat",
                                    "toLong" ="S1long")
  
  allsimdata<-rbind(S1SimData,S2SimData)
  
  print(sites[[1]][[i]])
  if (nrow(allsimdata)>0) {
    
    #get the site name and ware names
    thisSiteCodes <-readFile[readFile$ï..Site.Code %in% sites[[1]][[i]], ] 
    
    allsimdata$JacSimCut <- cut(allsimdata$JacSim, breaks=c(0,0.6499, 0.799, 0.999, 1.0), labels=c("0-0.65","0.65-0.8", "0.8-0.9","0.9-1"))
    
    plotTitle <- thisSiteCodes[[2]][[1]]
    facetLabels <- cbind(thisSiteCodes[3], thisSiteCodes[4])
    to_string <- as_labeller(facetLabels)
   
    
    #create the plots
    y_limits <- c(41, 50)
    p<-ggplot(iranSF)+
      labs(title=plotTitle, subtitle=thisSubtitle) +
      geom_sf(fill="NA", color="darkgrey", size=0.2) +
      geom_point(data=allsimdata, aes( x=fromLong, y=fromLat),size = 2, color = "darkblue",alpha=.8 ,stat = "unique") +
      geom_point(data=allsimdata, aes( x=toLong, y=toLat),size = 0.5, color = "darkblue", alpha=.3, stat = "unique") +
      geom_text_repel(data=allsimdata, aes( x=toLong, y=toLat, label=toWare),stat = "unique",
                      size=thisSize,
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
      geom_segment(data=allsimdata,aes(x=fromLong, y=fromLat, xend=toLong,yend=toLat, color=JacSimCut),
                    inherit.aes = FALSE, size=0.5)+

      scale_color_manual(values=segmentColours, drop=F) +
      coord_sf() +   
      theme_light() + 
      scale_size_identity() +
      xlim(NA, 65) +
      ylim(NA, 45) +
      labs(color = "Jaccard Similarity Score") +
      theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
            legend.position = "bottom", legend.direction="horizontal",
            strip.text = element_text(size = 8))
   
    
   #add the ware description on the next line
    appender <- function(string, suffix = facetLabels[which(facetLabels$Ware.Code %in% string), 2]) paste0(string, '\n', suffix)
    
    p+facet_wrap(~fromWare,labeller = as_labeller(appender))
    
    saveplot=paste0('maps/', sites[[1]][[i]],fileSuffix,'.png')
    ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")
    
  }
}