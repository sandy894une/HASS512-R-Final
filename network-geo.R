library(igraph)
library(ggmap)
library(sf)

# Read in edge list and node location data and covert to network object
#edges1 <- read.csv("Hispania_roads.csv", header = TRUE)
#nodes <- read.csv("Hispania_nodes.csv", header = TRUE)
#road_net <-
#  graph_from_edgelist(as.matrix(edges1[, 1:2]), directed = FALSE)

# Convert attribute location data to sf coordinates
locations_sf <-
  st_as_sf(sitelist, coords = c("LongDD", "LatDD"), crs = 4326)
# We also create a simple set of xy coordinates as this is used
# by the geom_point function
xy <- data.frame(x = sitelist$LongDD, y = sitelist$LatDD)

# Extract edge list from network object

e<- as.edgelist(simnet_999)

# Create data frame of beginning and ending points of edges
edges <- as.data.frame(matrix(NA, nrow(e), 4))
colnames(edges) <- c("X1", "Y1", "X2", "Y2")
# Iterate across each edge and assign lat and long values to
# X1, Y1, X2, and Y2
for (i in seq_len(nrow(e))) {
  edges[i, ] <- c(sitelist[e[i, 1], 5],
                  sitelist[e[i, 1], 6],
                  sitelist[e[i, 2], 5],
                  sitelist[e[i, 2], 6])
}

# Download stamenmap background data.
#my_map <- get_stamenmap(bbox = c(24, 36, 3, 43.8),
#                        maptype = "watercolor",
#                        zoom = 6)

# Produce map starting with background
ggplot(iranSF)+
  # geom_segment plots lines by the beginning and ending
  # coordinates like the edges object we created above
  geom_segment(
    data = edges,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2
    ),
    col = "black",
    size = 1
  ) +
  # plot site node locations
  geom_point(
    data = xy,
    aes(x, y),
    alpha = 0.8,
    col = "black",
    fill = "white",
    shape = 21,
    size = 2,
    show.legend = FALSE
  ) +
  theme_void()

segmentColours<-c("lightgreen", "forestgreen","gold", "orange","royalblue")
p<-ggplot(iranSF)+
  labs(title="Network - Ceramic Attributes", subtitle="Threshold >= 0.999") +
  geom_sf(fill="NA", color="darkgrey", size=0.2) +
  geom_segment(
    data = edges,
    aes(
      x = X1,
      y = Y1,
      xend = X2,
      yend = Y2
    ),
    col = "grey",
    size = 0.5
  ) +
  # plot site node locations
  geom_point(
    data = xy,
    aes(x, y),
    alpha = 0.8,
    col = "black",
    fill = "orange",
    shape = 21,
    size = 2,
    show.legend = FALSE
  ) +
  
  scale_color_manual(values=segmentColours, drop=F) +
  coord_sf() +   
  theme_light() + 
  scale_size_identity() +
  xlim(NA, 65) +
  ylim(NA, 40) +
  labs(color = "Jaccard Similarity Score") +
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),
        legend.position = "bottom", legend.direction="horizontal",
        strip.text = element_text(size = 8))
saveplot=paste0('maps/network-gt999',fileSuffix,'.png')
ggsave(saveplot, bg="white",width = 20, height = 20, units = "cm")