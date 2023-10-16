# title: networks-metrics.R
# description: measure degree centrality and eigenvector centrality of networks
# author: 'Sandy Pullen'
# date: '2023-10-08'

#Definition: Like degree centrality, EigenCentrality measures a node’s influence based on the number of links it has to other nodes in the network. EigenCentrality then goes a step further by also taking into account how well connected a node is, and how many links their connections have, and so on through the network.

#What it tells us: By calculating the extended connections of a node, EigenCentrality can identify nodes with influence over the whole network, not just those directly connected to it.

#When to use it: EigenCentrality is a good ‘all-round’ SNA score, handy for understanding human social networks, but also for understanding networks like malware propagation.


library(vegan)
library(dplyr)
library(igraph)
library(statnet)

#uses results matrix - recalculated here

inputFile= 'data/Iran-binary-all.csv'
readFile <- read.csv(inputFile, header=TRUE)

# shift the first 6 columns into a sitelist file
# only the presence/absence data into simdata
sitelist <- readFile[,1:6]
simdata  <- readFile[,7:24 ]

result <- vegdist(simdata, method = 'jaccard', binary = TRUE) 
resultm<- as.matrix(result)
results <- as.matrix(1-resultm)


simnet_999 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.9999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_999 %v% "vertex.names" <- readFile$Ware.Code

simnet_80 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.7999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_80 %v% "vertex.names" <- readFile$Ware.Code


#degree centrality
deg_999 <- degree(simnet_999,  ignore.eval==TRUE)  %>% cbind(readFile[, c(3,5,6)]) #if plotting
sorted_deg_999 <-deg_999[order(-deg_999[1]),]
print(sorted_deg_999[1:20,1:2])

deg_80 <- sna::degree(simnet_80,  ignore.eval==TRUE)   %>% cbind(readFile[, c(3,5,6)]) #if plotting
sorted_deg_80 <-deg_80[order(-deg_80[1]),]
print(sorted_deg_80[1:20,1:2])

#eigenvector centrality
eig_80<- evcent(simnet_80)  %>% cbind(readFile[, c(3,5,6)]) #if plotting
eig_999<- evcent(simnet_999)  %>% cbind(readFile[, c(3,5,6)]) #if plotting

#make eigenvector values more readable
ex_80 = round(1000*as.numeric(unlist(eig_80[1])), 2)
ex_999 = round(1000*as.numeric(unlist(eig_999[1])), 2)

eig_80<- evcent(simnet_80) %>% cbind(readFile[, c(3,5,6)]) %>% cbind(ex_80)
sorted_eig_80 <-eig_80[order(-eig_80[1]),]
print(sorted_eig_80)
print(sorted_eig_80[1:20,c(2, 5)])

eig_999<- evcent(simnet_999) %>% cbind(readFile[, c(3,5,6)]) %>% cbind(ex_999)
sorted_eig_999 <-eig_999[order(-eig_999[1]),]
print(sorted_eig_999[1:20,c(2, 5)])

#motifs


inputFile= 'data/Iran-binary-motifs-v3-subset.csv'
readFile <- read.csv(inputFile, header=TRUE)

# shift the first 4 columns into a sitelist file
# only the presence/absence data into simdata
sitelist <- readFile[,1:4]
simdata  <- readFile[,5:23 ]

result <- vegdist(simdata, method = 'jaccard', binary = TRUE) 

resultm<- as.matrix(result)
results <- as.matrix(1-resultm)

simnet_999 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.9999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_999 %v% "vertex.names" <- readFile$Site.Code

simnet_80 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.7999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_80 %v% "vertex.names" <- readFile$Site.Code

simnet_50 <-
  network(event2dichot(results,
                       method = "absolute",
                       thresh = 0.4999),
          directed = FALSE)
# add names for the nodes based on the row names of original matrix
simnet_50 %v% "vertex.names" <- readFile$Site.Code

#degree centrality
deg_999 <- degree(simnet_999,  ignore.eval==TRUE)  %>% cbind(readFile[, c(1,2)]) #if plotting add long and lat
sorted_deg_999 <-deg_999[order(-deg_999[1]),]
print(sorted_deg_999[1:20,1:2])

deg_80 <- sna::degree(simnet_80,  ignore.eval==TRUE)   %>% cbind(readFile[,  c(1,2)]) #if plotting add long and lat
sorted_deg_80 <-deg_80[order(-deg_80[1]),]
print(sorted_deg_80[1:20,1:2])

deg_50 <- sna::degree(simnet_50,  ignore.eval==TRUE)   %>% cbind(readFile[,  c(1,2)]) #if plotting add long and lat
sorted_deg_50 <-deg_50[order(-deg_50[1]),]
print(sorted_deg_50[1:20,1:2])

#eigenvector centrality
eig_50<- evcent(simnet_50)  %>% cbind(readFile[,  c(1,2)]) #if plotting add long and lat
eig_80<- evcent(simnet_80)  %>% cbind(readFile[,  c(1,2)]) #if plotting add long and lat
eig_999<- evcent(simnet_999)  %>% cbind(readFile[,  c(1,2)]) #if plotting add long and lat

#make eigenvector values more readable
ex_50 = round(1000*as.numeric(unlist(eig_50[1])), 2)
ex_80 = round(1000*as.numeric(unlist(eig_80[1])), 2)
ex_999 = round(1000*as.numeric(unlist(eig_999[1])), 2)

eig_50<- evcent(simnet_50) %>% cbind(readFile[, c(1,2)]) %>% cbind(ex_50)
sorted_eig_50 <-eig_50[order(-eig_50[1]),]
#print(sorted_eig_50)
print(sorted_eig_50[1:20,c(2, 4)])

eig_80<- evcent(simnet_80) %>% cbind(readFile[, c(1,2)]) %>% cbind(ex_80)
sorted_eig_80 <-eig_80[order(-eig_80[1]),]
#print(sorted_eig_80)
print(sorted_eig_80[1:20,c(2, 4)])

eig_999<- evcent(simnet_999) %>% cbind(readFile[, c(1,2)]) %>% cbind(ex_999)
sorted_eig_999 <-eig_999[order(-eig_999[1]),]
print(sorted_eig_999[1:20,c(2, 4)])



