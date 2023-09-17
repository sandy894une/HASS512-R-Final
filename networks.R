# title: networks.R
# description: create network diagram of the similarity tables
# author: 'Sandy Pullen'
# date: '2023-07-12'

library(vegan)
library(dplyr)

inputFile= 'data/Iran-binary-all.csv'
readFile <- read.csv(inputFile, header=TRUE)
#readFile <- readFile[readFile$Decorated == 1, ] #decorated or undecorated?

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
fileSuffix = "-all-1-no-isolates"
thisSubtitle = "All - Threshold 0.999 - Isolates removed"

simnet_999 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_999 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_999 <-as_tbl_graph(simnet_999) %E>%
  filter() %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = 'fr') +
  labs(title="Network ", subtitle=thisSubtitle) +
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
saveplot=paste0('networks/network',fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-95-no-isolates"
thisSubtitle = "All - Threshold 0.95 - Isolates removed"

simnet_95 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.95),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_95 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_95 <-as_tbl_graph(simnet_95) %E>%
  filter() %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = 'fr') +
  labs(title="Network ", subtitle=thisSubtitle) +
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
saveplot=paste0('networks/network',fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-90-no-isolates"
thisSubtitle = "All - Threshold 0.90 - Isolates removed"

simnet_90 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.90),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_90 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_90 <-as_tbl_graph(simnet_90) %E>%
  filter() %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = 'fr') +
  labs(title="Network ", subtitle=thisSubtitle) +
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
saveplot=paste0('networks/network',fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-85-no-isolates"
thisSubtitle = "All - Threshold 0.85 - Isolates removed"

simnet_85 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.85),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_85 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_85 <-as_tbl_graph(simnet_85) %E>%
  filter() %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = 'fr') +
  labs(title="Network ", subtitle=thisSubtitle) +
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
saveplot=paste0('networks/network',fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-80-no-isolates"
thisSubtitle = "All - Threshold 0.80 - Isolates removed"

simnet_80 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.80),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_80 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_80 <-as_tbl_graph(simnet_80) %E>%
  filter() %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = 'fr') +
  labs(title="Network ", subtitle=thisSubtitle) +
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
saveplot=paste0('networks/network',fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------
fileSuffix = "-all-75-no-isolates"
thisSubtitle = "All - Threshold 0.75 - Isolates removed"

simnet_75 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.75),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_75 %v% "vertex.names" <- readFile$Ware.Code

# graph the results
set.seed(1234)

g0_75 <-as_tbl_graph(simnet_75) %E>%
  filter() %N>%
  filter(!node_is_isolated()) %>%
  ggraph( layout = 'fr') +
  labs(title="Network ", subtitle=thisSubtitle) +
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
saveplot=paste0('networks/network',fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 50, height = 50, units = "cm")
#------------------------------------------------------

ggarrange(g0_999, g0_95, g0_90, g0_85, g0_80, g0_75, nrow = 2, ncol = 3)

saveplot=paste0('networks/network-arranged.png')
ggsave(saveplot, bg="white",width = 75, height = 50, units = "cm")


#------------------------------------------------------