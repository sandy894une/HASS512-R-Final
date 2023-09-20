# title: make-maps-summary-stats.R
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


iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-all.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)

readBinaryFile['Decorated'] <- readBinaryFile['Decorated'] * 10
readBinaryFile['Burnished'] <- readBinaryFile['Burnished'] * 100
readBinaryFile['mapvalue'] <- readBinaryFile['Decorated'] + readBinaryFile['Burnished']

segmentColours<-c( "forestgreen","gold", "orange","royalblue")



      
      
      #create the plots
      y_limits <- c(41, 50)
      p<-ggplot(iranSF)+
        #labs(title=plotTitle, subtitle=thisSubtitle) +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=readBinaryFile, aes( x=LongDD, y=LatDD, color=as.factor(mapvalue)),size = 3) +

        coord_sf() +   
        theme_light() + 
        #scale_size_identity() +
        xlim(NA, 65) +
        ylim(NA, 40) +
        labs(color = "Burnished/Decorated") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text = element_text(size = 8))
      
      p+scale_colour_manual(values = segmentColours, labels=c("Undecorated + Unburnished", "Decorated + Unburnished", "Undecorated + Burnished", "Decorated + Burnished"))
      
      saveplot=paste0('maps/summary-stats.png')
      ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")



