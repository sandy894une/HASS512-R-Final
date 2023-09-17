# title: jaccard.R
# description: create a similarity matrix 
# using jaccard calculations from either vegdist/vegan or distance/ecodist
# author: 'Sandy Pullen'
# date: '2023-06-04'

library(vegan)
#library(ecodist)
library(dplyr)


# declaring an empty data frame, name the columns, give datatypes
compileddf = data.frame(
  S1Code = character(),S1Ware = character(), S1long = character(),S1lat = character(),S2Code = character(),S2Ware = character(), S2long = character(),S2lat = character(),JacDissim = numeric(),JacSim = numeric(), stringsAsFactors = FALSE)

# specify the CSV file to be read in
# and name of the output file containing similarity matrix
# note: specify the path relative to the cwd

inputFile= 'data/Iran-binary-all.csv'
#inputFile= 'test-data.csv'

outputsimFile = 'data/Iran-compiled-all.csv'

readbinFile <- read.csv(inputFile, header=TRUE)

# shift the first 6 columns into a sitelist file
# only the presence/absence data into simdata
sitelist <- readbinFile[,1:6]
simdata  <- readbinFile[,7:24 ]

#use this line if using 'vegan'
result <- vegdist(simdata, method = 'jaccard', binary = TRUE) 

#use this line if using 'ecodist'
#result <- distance(simdata, method = "jaccard", sprange=NULL, spweight=NULL, inverted = FALSE)


resultm<- as.matrix(result)
#loop through the result dataframe to create a compiled df with site 1 code and ware and its lat and long,
#site 2 code and ware with lat and long, followed by the dissimilarity and similarity score between the two sites
iloop = nrow(sitelist)
jloop = 1
count=0
for (i in 2:iloop) {
  for (j in 1:jloop) {
    count = count+1
    # creating a vector to append to data frame
    vec <- c(sitelist[i,1],sitelist[i,3],sitelist[i,5],sitelist[i,6],sitelist[j,1],sitelist[j,3],sitelist[j,5],sitelist[j,6],resultm[i,j], 1-resultm[i,j])
    compileddf[count, ] <- vec 
  }
  jloop=jloop+1
}
# create a csv file with  all compiled results
write.csv(compileddf, outputsimFile, row.names=FALSE)