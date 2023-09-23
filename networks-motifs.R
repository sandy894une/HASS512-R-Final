# title: networks-motifs.R
# description: create network diagram of the similarity matrix for motifs
# author: 'Sandy Pullen'
# date: '2023-09-21'

library(vegan)
library(dplyr)

inputFile= 'data/Iran-binary-motifs-subset.csv'
readFile <- read.csv(inputFile, header=TRUE)

readbinFile <- read.csv(inputFile, header=TRUE)

# shift the first 4 columns into a sitelist file
# only the presence/absence data into simdata
sitelist <- readbinFile[,1:4]
simdata  <- readbinFile[,5:28 ]

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
fileSuffix = "-motifs-999-no-isolates"
thisSubtitle = "Motifs - Threshold 0.999"

motif_simnet999 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.9999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
motif_simnet999 %v% "vertex.names" <- readFile$Site.Code

# graph the results
set.seed(1234)

motifs_g0_999 <-as_tbl_graph(motif_simnet999) %E>%
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
fileSuffix = "-motifs-80-no-isolates"
thisSubtitle = "Motifs - Threshold 0.80"

motif_simnet80 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.7999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
motif_simnet80 %v% "vertex.names" <- readFile$Site.Code

# graph the results
set.seed(1234)

motifs_g0_80 <-as_tbl_graph(motif_simnet80) %E>%
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
fileSuffix = "-motifs-65-no-isolates"
thisSubtitle = "Motifs - Threshold 0.65"

motif_simnet65 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.6499),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
motif_simnet65 %v% "vertex.names" <- readFile$Site.Code

# graph the results
set.seed(1234)

motifs_g0_65 <-as_tbl_graph(motif_simnet65) %E>%
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
fileSuffix = "-motifs-50-no-isolates"
thisSubtitle = "Motifs - Threshold 0.50"

motif_simnet50 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.4999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
motif_simnet50 %v% "vertex.names" <- readFile$Site.Code

# graph the results
set.seed(1234)

motifs_g0_50 <-as_tbl_graph(motif_simnet50) %E>%
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
fileSuffix = "-motifs-0-3"
thisSubtitle = "Motifs - Threshold 0.3"

motif_simnet30 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.03),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
motif_simnet30 %v% "vertex.names" <- readFile$Site.Code

# graph the results
set.seed(1234)

motifs_g0_30 <-as_tbl_graph(motif_simnet30) %>%
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
fileSuffix = "-motifs-0"
thisSubtitle = "Motifs - Threshold 0"

motif_simnet0 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.01),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
motif_simnet0 %v% "vertex.names" <- readFile$Site.Code

# graph the results
set.seed(1234)

g0 <-as_tbl_graph(motif_simnet0) %>%
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
#------------------------------------------------------

ggarrange(motifs_g0_999, motifs_g0_80,motifs_g0_65,motifs_g0_50,motifs_g0_30, g0, nrow = 2, ncol = 3)

saveplot=paste0('networks/network-motifs-arranged.png')
ggsave(saveplot, bg="white",width = 75, height = 50, units = "cm")


#------------------------------------------------------
