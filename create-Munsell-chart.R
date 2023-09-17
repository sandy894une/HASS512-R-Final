# create-Munsell-chart.R
# Sandy Pullen
# 27 June 2023
# Takes a csv file of Munsell colours and creates a table of 
# related ISCC colour block names, RGB colour identifiers, and CIE Lab co-ords

# Loading required libraries
library(munsellinterpol)

# File Input and Output
inputFile= "munsell-soil-Rlist.csv"
outputFile= "munsell-RGB-LAB-Rlist3.csv"

#read the file
readFile <- read.csv(inputFile, header=FALSE)

#Loop through the input file and convert the Munsell code to ISCC-NBS colour name
# RGB colours and CIE-Lab co-ordinates
for (i in 1:nrow(readFile)){
  ISCC <- ColorBlockFromMunsell(readFile[i,1])
  RGB <- MunsellToRGB(readFile[i,1])
  LAB <- MunsellToLab(readFile[i,1])

  # creating a vector to append to the file
  vec <- c(readFile[i,1],ISCC[1,3],RGB[[3]][[1]],RGB[[3]][[2]],RGB[[3]][[3]], LAB[1],LAB[2],LAB[3]) 
  
  #write the vector to an output file
  write.table(rbind(vec), file = outputFile, row.names =FALSE, col.names = FALSE,sep = ",", append = TRUE)
}

