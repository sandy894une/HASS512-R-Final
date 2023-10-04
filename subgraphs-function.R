# title: subgraphs-function.R
# description: maps subgroups on Iran map with site code
# must run networks.R first
# author: 'Sandy Pullen'
# date: '2023-07-20'


library(igraph)
library(ggmap)
library(sf)

library(ggplot2)
library(ggrepel)
library(gridExtra)
library(dplyr)
library(ggpmisc)

#-------------------------------------------------------------------------------
#the function
make_subgroup_graphs <- function(theSimnet, thisSubtitle, fileSuffix){

  theSimnet<-  simnet_999
  # use a previously created simnet (networks.R)
  g<-as_tbl_graph(theSimnet) %E>%
    filter() %N>%
    filter(!node_is_isolated())
  
  # Extract edge list from network object - set min to 2 or 3
  mc <- max_cliques(g, min=2)
  
    for (j in 1:length(mc)) {  
      
      
        thesesites <-cbind(as_ids( mc[[j]]))
        sgsites <- as.data.frame(matrix(NA, length(mc[[j]]), 3))
        # Iterate across each edge and assign lat and long values to
        # X1, Y1
        for (i in 1:nrow(sgsites)) {
          
          sgsites[i, 1] <- nodes[which(nodes$Ware.Code == thesesites[i, 1]), 3]
          sgsites[i, 2] <- as.numeric(nodes[which(nodes$Ware.Code == thesesites[i, 1]), 5])
          sgsites[i, 3] <- as.numeric(nodes[which(nodes$Ware.Code == thesesites[i, 1]), 6])            
          sgsites[i, 4] <- nodes[which(nodes$Ware.Code == thesesites[i, 1]), 2]
          sgsites[i, 5] <- nodes[which(nodes$Ware.Code == thesesites[i, 1]), 4]
        }
        
        colnames(sgsites) <- c("Code","Long","Lat","Site","Ware")
        
 # collect attribute data to annotate the plot 
        attribute_data <-readbinFile[readbinFile$Ware.Code == sgsites[1,1],]
        attribute_data <- attribute_data %>% select(7:24) %>% 
          pivot_longer(., cols = c(1:18), names_to = "Attributes", values_to = "Present") %>%
          subset(Present==1)
        
        #create the plots
        y_limits <- c(41, 50)
  
        thisTitle=paste0("Network Subgroup - ", j)
  
        ggplot(iranSF)+
          labs(title=thisTitle, subtitle=thisSubtitle) +
          geom_sf(fill="NA", color="darkgrey", size=0.2) +
          geom_point(data=sgsites, aes( x=Long, y=Lat),size = 2, color = "darkblue",alpha=.8) +
          geom_text_repel(data=sgsites, aes( x=Long, y=Lat, label=Code) ,
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
                          )+
          coord_sf() +   
          theme_light() + 
          xlim(NA, 82) +
          ylim(NA, 45) +
          annotate(geom = "table",size=2, x = 80,y = 45,label = list(cbind(sgsites[, c(1,4,5)]))) +
          annotate(geom = "table",size=2, x = 80,y = 25,label = list(cbind(attribute_data[, c(1)]))) +
          theme(axis.title.x=element_blank(), axis.title.y=element_blank()) 
  
        
        saveplot=paste0('networks/subgraph-', fileSuffix, '-', j, '.png')
        ggsave(saveplot, bg="white",width = 20, height = 15, units = "cm")
        
  }
}
#end function
#------------------------------------------------------



#set parameters 

iranSF <-read_sf("irn_adm_unhcr_20190514_shp/irn_admbnda_adm0_unhcr_20190514.shp")
# Read in node location data


inputFile= 'data/Iran-binary-all.csv'
readbinFile <- read.csv(inputFile, header=TRUE)
# trim the file
nodes <- readbinFile[,1:6]

#------------------------------------------------------------------------------

#1 - similarity > 0.999
print(1)
run1Simnet <- simnet_999
run1Subtitle = "Cliques - threshold 0.999"
run1FileSuffix = "999"
make_subgroup_graphs(run1Simnet, run1Subtitle, run1FileSuffix)
