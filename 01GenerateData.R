library(foreach)
library(doParallel)
library(data.table)

<<<<<<< HEAD
all.data <- read.csv("./DataSets/NBBuildingsWGS84.csv", header=TRUE, stringsAsFactors=FALSE)
=======
file <- "./DataSets/NBBuildingsWGS84.csv"
# Loads data
all.data <- data.table(fread(file))
>>>>>>> DataTable

# The following are used to manipulate various data sets
# colnames(all.data) <- c("Name", "Mass", "Latitude", "Longitude") # Meteorites
# all.data$X <- as.numeric(all.data$X)
# all.data$Y <- as.numeric(all.data$Y)
# all.data$Mass <- as.numeric(all.data$Mass)

# Time the code
start <- proc.time()

startEnd <- function(lats, lngs) {
  # Find the "upper left" (NW) and "bottom right" (SE) coordinates of a set of data.
  #
  # Args:
  #  lats: A list of latitude coordinates
  #  lngs: A list of longitude coordinates
  #
  # Returns: 
  #   A list of values corresponding to the northwest-most and southeast-most coordinates
  
  # Convert to real number and remove NA values
  lats <- na.omit(as.numeric(lats))
  lngs <- na.omit(as.numeric(lngs))
  
  topLat <- max(lats)
  topLng <- min(lngs)
  botLat <- min(lats)
  botLng <- max(lngs)
    
  return(c(topLat, topLng, botLat, botLng))
}

startEndVals <- startEnd(all.data$Y, all.data$X)
remove(startEnd)

startLat <- startEndVals[1]
endLat <- startEndVals[3]
startLng <- startEndVals[2]
endLng <- startEndVals[4]
remove(startEndVals)

num_intervals = 200.0
interval <- (startLat - endLat) / num_intervals
remove(num_intervals)

lat.list <- seq(startLat, endLat + interval, -1*interval)

#testLng <- -66.66152983 # Fredericton
#testLat <- 45.96538183 # Fredericton

# Prepare the data to be sent in

# If you have a value you want to sum, use this
<<<<<<< HEAD
# data <- all.data[,c("Y", "X", "DN")]

# If you want to perform a count, use this
data <- all.data[,c("X", "Y")]
data["Value"] <- 1
=======
# data <- all.data[,c("X", "Y", "DN")]

# If you want to perform a count, use this
data <- all.data[,list(X, Y)]
data[["Value"]] <- 1
setkey(data, X, Y)
>>>>>>> DataTable

sumInsideSquare <- function(pointLat, pointLng, interval, data) {
  # Sum all the values that fall within a square on a map given a point,
  # an interval of the map, and data that contains lat, lng and the values
  # of interest
<<<<<<< HEAD
  
  colnames(data) <- c("lng", "lat", "value")
=======
  setnames(data, c("lng", "lat", "value"))
>>>>>>> DataTable
  
  # Data inside boundaries
  data <- na.omit(data[data$lng >= pointLng & data$lng < pointLng + interval & data$lat >= pointLat - interval & data$lat < pointLat])
  return(sum(data$value))
}

# Debugging
# squareSumTemp <- sumInsideSquare(testLat, testLng, interval, data)

# Given a start longitude and an end longitude, calculate an array of values
# corresponding to the sums for that latitude

calcSumLat <- function(startLng, endLng, lat, interval, data) {
  row <- c()
  lng <- startLng
  while (lng < endLng) {
    row <- c(row, sumInsideSquare(lat, lng, interval, data))
    lng <- lng + interval
  }
  
  return(row)
}

# Debugging
# rowTemp <- calcSumLat(startLng, endLng, testLat, interval, data)
# write.csv(rowTemp, file = "Temp.csv", row.names = FALSE)

# Set up parallel computing with the number of cores you have
cl <- makeCluster(detectCores(), outfile = "./Progress.txt")
registerDoParallel(cl)

all.sums <- foreach(lat=lat.list, .packages=c("data.table")) %dopar% {
  
  lat.data <- calcSumLat(startLng, endLng, lat, interval, data)
  
  # Progress indicator that works on Mac/Windows
  print((startLat - lat)/(startLat - endLat)*100) # Prints to Progress.txt
  
  lat.data

}

stopCluster(cl = NULL)

# Convert to data frame
all.sums.table <- as.data.table(all.sums)

# Save to disk so I don't have to run it again
write.csv(all.sums.table, file = "./GeneratedData/LevyTable.csv", row.names = FALSE)

# End timer
totalTime <- proc.time() - start
print(totalTime)

# remove(cl, endLat, endLng, startLat, startLng, lat.list, start, calcSumLat, sumInsideSquare, interval)

