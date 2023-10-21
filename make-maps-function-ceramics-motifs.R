# title: make-maps-function-ceramics-motifs.R

# description: use ggplot2 to plot Jaccard similarity matrix
# x and y limits altered to show a smaller area of Iran
#uses the ceramics subset for motifs sites
# author: 'Sandy Pullen'
# date: '2023-09-21'


library(sf)
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(dplyr)
#set parameters - only need to run this section once

iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-ceramic-motif-subset.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

#segmentColours<-c("gold","forestgreen","royalblue", "orange","black")
segmentColours<-c("lightgreen", "forestgreen","gold", "orange","royalblue")
#segmentColours<-c("lightblue","forestgreen","royalblue","gold", "orange","black")

sites <- as.data.frame(unique(readBinaryFile$Site.Code))
#------------------------------------------------------------------------------
# all data - maps with different thresholds
inputFile= 'data/Iran-compiled-ceramic-motif-subset.csv'
readSimFile <- read.csv(inputFile, header=TRUE)


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
      #y_limits <- c(41, 50)
      y_limits <- c(30.5, 50)
      p<-ggplot(iranSF)+
        labs(title=plotTitle, subtitle=thisSubtitle) +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=allsimdata, aes( x=fromLong, y=fromLat),size = 2, color = "darkblue",alpha=.8 ,stat = "unique") +
        geom_point(data=allsimdata, aes( x=toLong, y=toLat),size = 0.5, color = "darkblue", alpha=.3, stat = "unique") +
        geom_text_repel(data=allsimdata, aes( x=toLong, y=toLat, label=toWare),stat = "unique",
                        size=2.0,
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
        #xlim(NA, 65) +
        #ylim(NA, 45) +
        xlim(51, 54.5) +
        ylim(28.5, 31.5) +
        labs(color = "Jaccard Similarity Score") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
              legend.position = "bottom", legend.direction="horizontal",
              strip.text = element_text(size = 8))
      
      
      #add the ware description on the next line
      appender <- function(string, suffix = facetLabels[which(facetLabels$Ware.Code %in% string), 2]) paste0(string, '\n', suffix)
      
     # p+facet_wrap(~fromWare, ncol = 2, labeller = as_labeller(appender))
      
    #  saveplot=paste0('maps/motifs/2col/', sites[[1]][[i]],fileSuffix,'.png')
     # ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")
      
      p+facet_wrap(~fromWare, ncol = 4, labeller = as_labeller(appender))
      
      saveplot=paste0('maps/motifs/4col/', sites[[1]][[i]],fileSuffix,'.png')
      ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")
    }
  }
}

#end function
#-------------------------------------------------------------------------------



#1 - similarity = 1.0
print(1)
run1SimFile <- readSimFile[readSimFile$JacSim == 1.0, ]
run1Subtitle = "Similarity = 1.0"
run1FileSuffix = "-zoom-ceramic-motifs-eq0-1"

make_maps(run1SimFile, run1Subtitle,run1FileSuffix)

#2 - similarity >= 0.3
print(2)
run2SimFile <- readSimFile[readSimFile$JacSim >= 0.3, ]
run2Subtitle = "Similarity >= 0.3"
run2FileSuffix = "-zoom-ceramic-motifs-gt0-3"

make_maps(run2SimFile, run2Subtitle,run2FileSuffix)

#3 - similarity >= 0.65
print(3)
run3SimFile <- readSimFile[readSimFile$JacSim >= 0.65, ]
run3Subtitle = "Similarity >= 0.65"
run3FileSuffix = "-zoom-ceramic-motifs-gt0-65"

make_maps(run3SimFile, run3Subtitle,run3FileSuffix)
#------------------------------------------------------------------------------

#4 - similarity >= 0.8
print(4)
run4SimFile <- readSimFile[readSimFile$JacSim >= 0.8, ]
run4Subtitle = "Similarity >= 0.8"
run4FileSuffix = "-zoom-ceramic-motifs-gt0-8"

make_maps(run4SimFile, run4Subtitle,run4FileSuffix)
#------------------------------------------------------------------------------

#5 - similarity >= 0.5
print(5)
run5SimFile <- readSimFile[readSimFile$JacSim >= 0.5, ]
run5Subtitle = "Similarity >= 0.5"
run5FileSuffix = "-zoom-ceramic-motifs-gt0-5"

make_maps(run5SimFile, run5Subtitle,run5FileSuffix)