# create-Munsell-chart.R
# used to create the range of Munsel colours in the soil charts
# Sandy Pullen
# 27 June 2023



chroma <- c("1","2","3","4","6","8")
value <- c("2.5","3","4","5","6","7","8")
hue <- c("10R", "10YR", "2.5YR", "2.5Y", "5Y", "5YR", "7.5YR", "10Y", "5GY", "5R", "7.5R")



# File Input and Output

outputFile= "munsell-soil-Rlist.csv"

for (h in hue){
  
  for (v in value) {
    
    for (c in chroma){
      
      munsellColor <- paste0(h, " ", v, "/" , c)
      write.table(rbind(munsellColor), file = outputFile, row.names =FALSE, col.names = FALSE,sep = ",", append = TRUE)

    }
  }
}


