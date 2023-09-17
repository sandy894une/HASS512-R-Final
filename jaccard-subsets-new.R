# title: jaccard-subsets.R
# description: jaccard calculations using vegdist
# author: 'Sandy Pullen'
# date: '2023-06-04'

library(vegan)
library(dplyr)


# declaring an empty data frame, name the columns, give datatypes
subsetcompileddf = data.frame(
  S1Code = character(),S1Ware = character(), S1long = character(),S1lat = character(),S2Code = character(),S2Ware = character(), S2long = character(),S2lat = character(),JacDissim = numeric(),JacSim = numeric(), stringsAsFactors = FALSE)

# specify the CSV file to be read in
# note: specify the path relative to the cwd
inputFile= 'Iran-binary-all-Sep12-UTF.csv'

readFile <- read.csv(inputFile, header=TRUE)

# subset the input file based on the interval group in intervalFile
# use this if splitting to regions or periods or decorated/undecorated

#subsetFile <- readFile[readFile$SiteCode %in% locationsSites$Code, ]
#outputsimFile = 'Iran-interval-description-sim-Jul11-UTF.csv'

#subsetFile <- readFile[readFile$Decorated==0, ] #undecorated
#outputsimFile = 'Iran-undecorated-sim-Jul11-UTF.csv'

subsetFile <- readFile[readFile$Decorated == 1, ] #decorated
outputFile = 'Iran-decorated-sim-Sep12-UTF.csv'


# shift the first 3 columns into a sitelist file
# put the presence/absence data into simdata
subsetsitelist <- subsetFile[,1:6]
subsetsimdata  <- subsetFile[,7:24 ]

subsetresult <- vegdist(subsetsimdata, method = 'jaccard', binary = TRUE) 
#print(result)
subsetresultm<- as.matrix(subsetresult)

#loop through the result dataframe to create a compiled df with site 1 code and ware and its lat and long,
#site 2 code and ware with lat and long, followed by the dissimilarity and similarity score between the two sites
iloop = nrow(subsetsitelist)
jloop = 1
count=0
for (i in 2:iloop) {
  for (j in 1:jloop) {
    count = count+1
    
    # creating a vector to append to
    # data frame
    vec <- c(subsetsitelist[i,1],subsetsitelist[i,3],subsetsitelist[i,5],subsetsitelist[i,6],subsetsitelist[j,1],subsetsitelist[j,3],subsetsitelist[j,5],subsetsitelist[j,6],subsetresultm[i,j], 1-subsetresultm[i,j])
    
    subsetcompileddf[count, ] <- vec 
    
  }
  jloop=jloop+1
}

# create a csv file with  all compiled results

#print (subsetcompileddf)
write.csv(subsetcompileddf, outputFile, row.names=FALSE)

