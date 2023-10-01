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
library(dplyr)

iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
inputFile= 'data/Iran-binary-all.csv'
readBinaryFile <- read.csv(inputFile, header=TRUE)




statsFile <-readBinaryFile


statsFile <- statsFile %>%
  mutate(SurfaceVeryLightOnly = if_else(SurfaceVeryLight==1 & SurfaceLight==0 & SurfaceMod==0 & SurfaceDark==0, 1, 0)) %>%
  mutate(SurfaceVeryLightLight = if_else(SurfaceVeryLight==1 & SurfaceLight==1 & SurfaceMod==0 & SurfaceDark==0, 1, 0)) %>%
  mutate(SurfaceLightOnly = if_else(SurfaceVeryLight==0 & SurfaceLight==1 & SurfaceMod==0 & SurfaceDark==0, 1, 0)) %>%
  mutate(SurfaceLightMod = if_else(SurfaceLight==1 & SurfaceMod==1 & SurfaceVeryLight==0 & SurfaceDark==0, 1, 0))%>%
  mutate(SurfaceModDark = if_else(SurfaceMod==1 & SurfaceDark==1 & SurfaceVeryLight==0 & SurfaceLight==0, 1, 0))%>%
  mutate(SurfaceModOnly = if_else(SurfaceMod==1 & SurfaceDark==0 & SurfaceVeryLight==0 & SurfaceLight==0, 1, 0))%>%
  mutate(SurfaceDarkOnly = if_else(SurfaceMod==0 & SurfaceDark==1 & SurfaceVeryLight==0 & SurfaceLight==0, 1, 0))%>%
  mutate(SlipVeryLightLight = if_else(SlipVeryLight==1 & SlipLight==1 & SlipMod==0 & SlipDark==0, 1, 0)) %>%
  mutate(SlipVeryLightOnly = if_else(SlipVeryLight==1 & SlipLight==0 & SlipMod==0 & SlipDark==0, 1, 0)) %>%
  mutate(SlipLightMod = if_else(SlipLight==1 & SlipMod==1 & SlipVeryLight==0 & SlipDark==0, 1, 0))%>%
  mutate(SlipLightOnly = if_else(SlipVeryLight==0 & SlipLight==1 & SlipMod==0 & SlipDark==0, 1, 0)) %>%
  mutate(SlipModDark = if_else(SlipMod==1 & SlipDark==1 & SlipVeryLight==0 & SlipLight==0, 1, 0))%>%
  mutate(SlipModOnly = if_else(SlipVeryLight==0 & SlipLight==0 & SlipMod==1 & SlipDark==0, 1, 0)) %>%
  mutate(SlipDarkOnly = if_else(SlipVeryLight==0 & SlipLight==0 & SlipMod==0 & SlipDark==1, 1, 0)) %>%
  mutate(PaintVeryLightLight = if_else(PaintVeryLight==1 & PaintLight==1 & PaintMod==0 & PaintDark==0, 1, 0)) %>%
  mutate(PaintVeryLightOnly = if_else(PaintVeryLight==1 & PaintLight==0 & PaintMod==0 & PaintDark==0, 1, 0)) %>%
  mutate(PaintLightMod = if_else(PaintLight==1 & PaintMod==1 & PaintVeryLight==0 & PaintDark==0, 1, 0))%>%
  mutate(PaintModDark = if_else(PaintMod==1 & PaintDark==1 & PaintVeryLight==0 & PaintLight==0, 1, 0))%>%
  mutate(PaintLightOnly = if_else(PaintVeryLight==0 & PaintLight==1 & PaintMod==0 & PaintDark==0, 1, 0)) %>%
  mutate(PaintModOnly = if_else(PaintVeryLight==0 & PaintLight==0 & PaintMod==1 & PaintDark==0, 1, 0)) %>%
  mutate(PaintDarkOnly = if_else(PaintVeryLight==0 & PaintLight==0 & PaintMod==0 & PaintDark==1, 1, 0))

sums<-colSums(Filter(is.numeric, statsFile))
print(sums[21:41])

#run once
statsFile['Decorated'] <- readBinaryFile['Decorated'] * 10
statsFile['Burnished'] <- readBinaryFile['Burnished'] * 100
statsFile['mapvalue'] <- statsFile['Decorated'] + statsFile['Burnished']

table(statsFile$mapvalue)
ggplot(statsFile)+
  geom_bar(data=statsFile, aes(mapvalue), stat="count")


segmentColours<-c( "lightgreen","forestgreen", "lightblue","royalblue")


      #create the plots
      y_limits <- c(41, 50)
      

      
      # New facet label names for mapvalue variable
      mapvalue.labs <- c("Undecorated + Unburnished", "Decorated + Unburnished", "Undecorated + Burnished", "Decorated + Burnished")
      names(mapvalue.labs) <- c(0, 10, 100, 110)

      p<-ggplot(iranSF)+
        labs(title="Summary Statistics") +
        geom_sf(fill="NA", color="darkgrey", size=0.2) +
        geom_point(data=statsFile, aes( x=LongDD, y=LatDD, color=as.factor(mapvalue)),size = 2) +

        coord_sf() +   
        theme_light() + 
     
        xlim(NA, 65) +
        ylim(NA, 40) +
        labs(color = "Burnished/Decorated") +
        theme(axis.title.x=element_blank(), axis.title.y=element_blank(), strip.text = element_text(size = 8))+
        facet_wrap(~mapvalue, labeller=labeller(mapvalue = mapvalue.labs))+
        scale_colour_manual(values = segmentColours, labels=mapvalue.labs)
      
      saveplot=paste0('maps/summary-stats.png')
      ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")



