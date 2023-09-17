# cluster-colours.R
# cluster colours in usable groups script
# Sandy Pullen
# 27 June 2023
# Takes a csv file of Munsell  and RGB colours and creates a  
# csv file of clustered colours for varying number of center values 4,6,8,10,12.

# Source: https://search4fan.github.io/post/r_AI_cluster_by_k-means.html

# Loading required libraries
library(ggplot2)

#define files
inputFile= "data/munsell-RGB-LAB-Rlist3.csv"
outputFileBegin= "data/outputClusters-soils-Rlist3-"

#set centre values for clustering (different options)
centervalues <- c(4,6,8,10,12)

#read input file
colors <- read.csv(inputFile, header=TRUE)

#create a dataframe of vectors of the RGB co-ordinates
imgRGB <- data.frame(
  R = as.vector(colors[3]),
  G = as.vector(colors[4]),
  B = as.vector(colors[5])
)

#loop for all the required centre values for clustering
#set the seed for reproducible results

for (i in centervalues){
  set.seed(i)
  kmeans_result <- kmeans(imgRGB, centers = i)
  imgRGB$cluster <- as.factor(kmeans_result$cluster)

  # plot the clustering if needed to visually check
  # this can be commented out as not really required
  ggplot(imgRGB, aes(R, G, B, color = cluster)) + geom_point(size = 2) +
    theme_minimal() + labs(title = "K-means Clustering of Colours")
  
  #construct a results dataframe to write to a csv file
  imgRGB2 <- data.frame(
      Munsell= as.vector(colors[1]),
      ISCCNBS= as.vector(colors[2]),
      imgRGB$R,
      imgRGB$G,
      imgRGB$B,
      imgRGB$cluster
  )
  
 #create data-frames at each iteration to use for verification process in next step 
  assign(paste0("DF", i), data.frame(imgRGB2))
 

    
# different output file for each value of centers  
  outputFile <- paste0(outputFileBegin, i, '.csv')
  write.csv(imgRGB2, outputFile, row.names=FALSE)
}


#check clustering - Potts
PottsColourGroups <- c("Buff","Greenish-Buff", "Light Brown", "Brown", "Dark Brown", "Light Orange", "Orange", "Dark Orange", "Red", "Light Grey", "Grey", "Dark Grey")

list.dfs <- list(DF4, DF6, DF8, DF10,DF12)

thisCluster=4
for (i in list.dfs){
  
  print(thisCluster)

 buff <- i[imgRGB2$Munsell %in% c('5Y 8/1','5Y 8/2','2.5Y 8/1','2.5Y 8/2'),]
  print(buff)

  gbuff <- i[imgRGB2$Munsell %in% c('5GY 8/1','5G 8/1'),]
  print(gbuff)
  lb <- i[imgRGB2$Munsell %in% c('2.5Y 7/2', '2.5Y 7/4', '2.5Y 6/2', '2.5Y 6/4'),]
  print(lb)
  
    b <- i[imgRGB2$Munsell %in% c('10YR 5/3', '10YR 5/4', '10YR 5/6', '10YR 5/8'),]
  print(b)
  db <- i[imgRGB2$Munsell %in% c('10YR 3/3', '10YR 3/4', '10YR 3/5', '10YR 3/6', '10YR 4/3', '10YR 4/4', '10YR 4/5', '10YR 4/6'),]
    print(db)
  
    lo <- i[imgRGB2$Munsell %in% c('7.5YR 7/6', '7.5YR 7/8'),]
    print(lo)
  
    o <- i[imgRGB2$Munsell %in% c('2.5YR 6/8', '2.5YR 7/8'),]
    print(o)
  
    do <- i[imgRGB2$Munsell %in% c('10R 5/8', '10R 6/8'),]
    print(do)
  
    r <- i[imgRGB2$Munsell %in% c('10R 4/8', '10R 3/6'),]
    print(r)
  
    lg <- i[imgRGB2$Munsell %in% c('5Y 7/1', '5Y 7/2', '5B 7/1','5PB 7/1'),]
    print(lg)
  
    g <- i[imgRGB2$Munsell %in% c('5Y 7/1', '5B 6/1','5PB 6/1'),]
    print(g)
  
    dg <- i[imgRGB2$Munsell %in% c('2.5Y 4/1', '5B 4/1','5PB 4/1'),]
    print(dg)
  
  thisCluster = thisCluster+2
}





#check clustering - Nieuwenhuyse
imgRGB2[imgRGB2$Munsell %in% c('10R 5/6','2.5YR 5/6'),]
imgRGB2[imgRGB2$Munsell %in% c('2.5YR 5/3', '2.5YR 5/4', '5YR 5/3', '5YR 5/4', '7.5YR 4/3', '7.5YR 5/3', '7.5YR 5/4'),]
imgRGB2[imgRGB2$Munsell %in% c('2.5YR 6/4', '5YR 6/3', '5YR 6/4'),]
imgRGB2[imgRGB2$Munsell %in% c('2.5YR 6/6', '2.5YR 6/8'),]
imgRGB2[imgRGB2$Munsell %in% c('7.5YR 6/3', '7.5YR 6/4'),]
imgRGB2[imgRGB2$Munsell %in% c('5YR 6/6', '5YR 7/6'),]
imgRGB2[imgRGB2$Munsell %in% c('7.5YR 7/3', '7.5YR 7/4'),]

#retrieve cluster groups from Df4
#match description to the clusters in the spreadsheet
#1=pale
#2=dark
#3=moderate
#4=light

cluster <- c(1,2,3,4)
description <- c("pale", "dark", "moderate", "light")
colorsGroups <-data.frame(cluster, description)
#remove unwanted columns
df <- DF4[c(1,6)]

#find these Munsell codes and matching cluster in the dataframe
# repeat for each site

buff <- df[imgRGB2$Munsell %in% c('5Y 8/1','5Y 8/2','2.5Y 8/1','2.5Y 8/2'),]
buff$desc <- colorsGroups[buff[[2]][[1]],2]
print(buff)

