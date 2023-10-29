# title: make-maps-function.R

# MAIN MAPS SCRIPT

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


#-------------------------------------------------------------------------------
#the function
make_maps <- function(theInputSimFile, thisSubtitle,fileSuffix,thisSize){
  

  
  #loop through each site code
  
  for (i in 1:nrow(sites)) {
    
    #get the sim data for the site

    
    S1SimData<- theInputSimFile[theInputSimFile$S1Code %in% sites[[1]][[i]], ]
    S1SimData <- S1SimData %>% rename("fromCode" = "S1Code",
                                      "fromWare" = "S1Ware",
                                      "fromLat" = "S1lat",
                                      "fromLong" = "S1long",
                                      "toCode" = "S2Code",
                                      "toWare" = "S2Ware",
                                      "toLat" = "S2lat",
                                      "toLong" ="S2long")
    
    S2SimData <- theInputSimFile[theInputSimFile$S2Code %in% sites[[1]][[i]], ]
    S2SimData <- S2SimData %>% rename("fromCode" = "S2Code",
                                      "fromWare" = "S2Ware",
                                      "fromLat" = "S2lat",
                                      "fromLong" = "S2long",
                                      "toCode" = "S1Code",
                                      "toWare" = "S1Ware",
                                      "toLat" = "S1lat",
                                      "toLong" ="S1long")
    
    allsimdata<-rbind(S1SimData,S2SimData)
    
    #print(sites[[1]][[i]])
    if (nrow(allsimdata)>0) {
      
      #get the site name and ware names
      thisSiteCodes <-readBinaryFile[readBinaryFile$Site.Code %in% sites[[1]][[i]], ] 
      
      allsimdata$JacSimCut <- cut(allsimdata$JacSim, breaks=c(0,0.299, 0.6499, 0.799, 0.999, 1.0), 
        labels=c("0-0.3","0.3-0.65","0.65-0.8", "0.8-0.9","0.9-1"))
      
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
      
      p+facet_wrap(~fromWare, ncol = 2, labeller = as_labeller(appender))
      
      saveplot=paste0('maps/2col/', sites[[1]][[i]],fileSuffix,'.png')
      ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")
      
      p+facet_wrap(~fromWare, ncol = 4, labeller = as_labeller(appender))
      
      saveplot=paste0('maps/4col/', sites[[1]][[i]],fileSuffix,'.png')
      ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")
    }
  }
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
#readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]

#------------------------------------------------------------------------------
#run groups for all wares

#1 - similarity =1.0
print(1)
run1SimFile <- readSimFile[readSimFile$JacSim == 1.0, ]
run1Subtitle = "Similarity = 1.0"
run1FileSuffix = "-eq-1"
run1Size = 2.0
make_maps(run1SimFile, run1Subtitle, run1FileSuffix, run1Size)


#3 - similarity >= 0.8
print(3)
run3SimFile <- readSimFile[readSimFile$JacSim >= 0.8, ]
run3Subtitle = "Similarity >= 0.8"
run3FileSuffix = "-gt0-8"
run3Size = 3.0
make_maps(run3SimFile, run3Subtitle,run3FileSuffix,run3Size)


#5 - similarity >= 0.65
print(5)
run5SimFile <- readSimFile[readSimFile$JacSim >= 0.65, ]
run5Subtitle = "Similarity >= 0.65"
run5FileSuffix = "-gt0-65"
run5Size = 1.5
make_maps(run5SimFile, run5Subtitle,run5FileSuffix,run5Size)


#------------------------------------------------------------------------------
# decorated only - maps with different thresholds
print("decorated")
inputFile= 'data/Iran-compiled-decorated.csv'
readSimFile <- read.csv(inputFile, header=TRUE,encoding ="UTF-8")

#remove same site similarity when necessary
#readSimFile <- readSimFile[readSimFile$S1Code != readSimFile$S2Code, ]
#------------------------------------------------------------------------------
#run groups for decorated only

#1 - similarity =1.0
print(1)
run1SimFile <- readSimFile[readSimFile$JacSim == 1.0, ]
run1Subtitle = "Decorated only - Similarity = 1.0"
run1FileSuffix = "-dec-eq-1"
run1Size = 1.5
make_maps(run1SimFile, run1Subtitle, run1FileSuffix, run1Size)

#2 - similarity >= 0.85
print(2)
run2SimFile <- readSimFile[readSimFile$JacSim >= 0.85, ]
run2Subtitle = "Decorated only - Similarity >= 0.85"
run2FileSuffix = "-dec-gt0-85"
run2Size = 1.5
make_maps(run2SimFile, run2Subtitle,run2FileSuffix,run2Size)

#3 - similarity >= 0.8
print(3)
run3SimFile <- readSimFile[readSimFile$JacSim >= 0.8, ]
run3Subtitle = "Decorated only - Similarity >= 0.8"
run3FileSuffix = "-dec-gt0-8"
run3Size = 3.0
make_maps(run3SimFile, run3Subtitle,run3FileSuffix,run3Size)

#4 - similarity >= 0.75
print(4)
run4SimFile <- readSimFile[readSimFile$JacSim >= 0.75, ]
run4Subtitle = "Decorated only - Similarity >= 0.75"
run4FileSuffix = "-dec-gt0-75"
run4Size = 1.5
make_maps(run4SimFile, run4Subtitle,run4FileSuffix,run4Size)

#5 - similarity >= 0.65
print(5)
run5SimFile <- readSimFile[readSimFile$JacSim >= 0.65, ]
run5Subtitle = "Decorated only - Similarity >= 0.65"
run5FileSuffix = "-dec-gt0-65"
run5Size = 1.5
make_maps(run5SimFile, run5Subtitle,run5FileSuffix,run5Size)
#-------------------------------------------------------------------------------
#6 - similarity >= 0.1
print(6)
run6SimFile <- readSimFile[readSimFile$JacSim >= 0.1, ]
run6Subtitle = "Decorated only - Similarity >= 0.1"
run6FileSuffix = "-dec-gt0-1"
run6Size = 1.5
make_maps(run6SimFile, run6Subtitle,run6FileSuffix,run6Size)

