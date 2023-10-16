# title: networks.R
# description: create network diagram of the similarity tables
# separate into two groups based on decorated 1 or 0
# author: 'Sandy Pullen'
# date: '2023-07-12'

library(vegan)
library(dplyr)

inputFile= 'data/Iran-binary-all.csv'
readFile <- read.csv(inputFile, header=TRUE)

# run twice using paramaters below:
#run 1
#readFile <- readFile[readFile$Decorated == 1, ] #decorated
#inputType<-"dec"
#thisTitle = "Network - Decorated Only"

#run 2
readFile <- readFile[readFile$Decorated == 0, ] #undecorated
inputType<-"undec"
thisTitle = "Network - Undecorated Only"

# shift the first 6 columns into a sitelist file
# only the presence/absence data into simdata
sitelist <- readFile[,1:6]
simdata  <- readFile[,7:24 ]

result <- vegdist(simdata, method = 'jaccard', binary = TRUE) 
resultm<- as.matrix(result)
results <- as.matrix(1-resultm)

# Define our binary network object from BR similarity
# https://book.archnetworks.net/visualization#networkpackage

library(statnet)
library(ggraph)
library(igraph) 
library(tidyverse)
library(tidygraph)
library(ggmap)
library(ggpubr)


#------------------------------------------------------
fileSuffix = "-all-999"
thisSubtitle = "Threshold 0.999"

simnet_999 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.9999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_999 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_999 <-as_tbl_graph(simnet_999)  %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-95"
thisSubtitle = "Threshold 0.95"

simnet_95 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.9499),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_95 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_95 <-as_tbl_graph(simnet_95)  %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-90"
thisSubtitle = "Threshold 0.90"

simnet_90 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.8999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_90 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_90 <-as_tbl_graph(simnet_90)  %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-85"
thisSubtitle = "Threshold 0.85"

simnet_85 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.8499),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_85 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_85 <-as_tbl_graph(simnet_85)  %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-80"
thisSubtitle = "Threshold 0.80"

simnet_80 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.7999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_80 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_80 <-as_tbl_graph(simnet_80)  %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-75"
thisSubtitle = "Threshold 0.75 "

simnet_75 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.7499),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_75 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_75 <-as_tbl_graph(simnet_75)  %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
#------------------------------------------------------
fileSuffix = "-all-65"
thisSubtitle = "Threshold 0.65"

simnet_65 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.6499),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_65 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_65 <-as_tbl_graph(simnet_65)  %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")

#------------------------------------------------------
fileSuffix = "-all-0"
thisSubtitle = "Threshold 0"

simnet_0 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.01),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_0 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0 <-as_tbl_graph(simnet_0) %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------

#------------------------------------------------------

#------------------------------------------------------
fileSuffix = "-all-30"
thisSubtitle = "Threshold 0.30"

simnet_30 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.2999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_30 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_30all <-as_tbl_graph(simnet_30) %>%
  ggraph( layout = 'fr') +
  labs(title=thisTitle, subtitle=thisSubtitle) +
  geom_edge_link() + 
  geom_node_point() + 
  geom_node_text(aes(label = name),                
                 check_overlap = TRUE,
                 repel = TRUE,
                 nudge_x = 0.1,
                 nudge_y = 0.1,
                 max.overlaps=Inf) +
  theme_light() +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank())+
  theme(legend.position = "none") 
saveplot=paste0('networks/network', '-',inputType, fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")

#------------------------------------------------------

ggarrange(g0_999, g0_90, g0_85, g0_80, g0_75,g0_65, nrow = 2, ncol = 3)

saveplot=paste0('networks/network-',inputType, '-arranged.png')
ggsave(saveplot, bg="white",width = 75, height = 50, units = "cm")


#------------------------------------------------------