# title: jaccard-subsets.R
# description: jaccard calculations using vegdist
# author: 'Sandy Pullen'
# date: '2023-06-04'

library(vegan)
library(dplyr)


# declaring an empty data frame, name the columns, give datatypes
compileddf = data.frame(
  S1Code = character(),S1Ware = character(), S1lat = character(),S1long = character(),S2Code = character(),S2Ware = character(), S2lat = character(),S2long = character(),JacDissim = numeric(),JacSim = numeric(), stringsAsFactors = FALSE)

# specify the CSV file to be read in
# note: specify the path relative to the cwd
inputFile= 'Iran-binary-all-Jul12-UTF.csv'

readFile <- read.csv(inputFile, header=TRUE)

# subset the input file based on the interval group in intervalFile
# use this if splitting to regions or periods or decorated/undecorated

#subsetFile <- readFile[readFile$SiteCode %in% locationsSites$Code, ]
#outputsimFile = 'Iran-interval-description-sim-Jul11-UTF.csv'

#subsetFile <- readFile[readFile$Decorated==0, ] #undecorated
#outputsimFile = 'Iran-undecorated-sim-Jul11-UTF.csv'

subsetFile <- readFile[readFile$Decorated == 1, ] #decorated
outputsimFile = 'Iran-decorated-sim-Jul12-UTF.csv'


# shift the first 3 columns into a sitelist file
# put the presence/absence data into simdata
sitelist <- subsetFile[,1:6]
simdata  <- subsetFile[,7:21 ]

result <- vegdist(simdata, method = 'jaccard', binary = TRUE) 
#print(result)


#loop through the result dataframe to create a compiled df with site 1 code and ware and its lat and long,
#site 2 code and ware with lat and long, followed by the dissimilarity and similarity score between the two sites
jloop = nrow(sitelist)-1
iloop = nrow(sitelist)-1
count=0
for (i in 1:iloop) {
  for (j in i+1:jloop) {
    count = count+1
    
    # creating a vector to append to
    # data frame
    vec <- c(sitelist[i,1],sitelist[i,3],sitelist[i,5],sitelist[i,6],sitelist[j,1],sitelist[j,3],sitelist[j,5],sitelist[j,6],result[count], 1-result[count]) 
    compileddf[count, ] <- vec 
    
  }
  jloop=jloop-1
}

# create a csv file with  all compiled results

#print (compileddf)
write.csv(compileddf, outputsimFile, row.names=FALSE)

