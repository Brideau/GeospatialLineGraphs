library(foreach)
library(doParallel)
library(data.table)
library(raster)

# Time the code
start <- proc.time()

if (!file.exists("./DataSets")) {
  dir.create("./DataSets")
}

# Data Source:
# http://sedac.ciesin.columbia.edu/data/set/gpw-v3-population-count/data-download
# Format: .ascii, 1/2 degree, 2000

population.file <- "./Canada/VoteDensityRaster64Bit.tif"
# Load the raster file
population.raster <- raster(population.file)
# Convert the raster file to a points file
population.points <- rasterToPoints(population.raster)
all.data <- as.data.table(population.points)
setnames(all.data, c("x", "y", "population"))

# If you have your data in a CSV file, use this instead
# file <- "./DataSets/NBBuildingsWGS84.csv"
# all.data <- data.table(fread(file))


# The following are used to manipulate various data sets
# colnames(all.data) <- c("Name", "Mass", "Latitude", "Longitude") # Meteorites
# all.data$X <- as.numeric(all.data$X)
# all.data$Y <- as.numeric(all.data$Y)
# all.data$Mass <- as.numeric(all.data$Mass)

startEnd <- function(lats, lngs) {
  # Find the "upper left" (NW) and "bottom right" (SE) coordinates 
  # of a set of data.
  #
  # Args:
  #  lats: A list of latitude coordinates
  #  lngs: A list of longitude coordinates
  #
  # Returns: 
  #   A list of values corresponding to the northwest-most and 
  # southeast-most coordinates
  
  # Convert to real number and remove NA values
  lats <- na.omit(as.numeric(lats))
  lngs <- na.omit(as.numeric(lngs))
  
  topLat <- max(lats)
  topLng <- min(lngs)
  botLat <- min(lats)
  botLng <- max(lngs)
  
  return(c(topLat, topLng, botLat, botLng))
}

startEndVals <- startEnd(all.data$y, all.data$x)
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

# testLng <- -66.66152983 # Fredericton
# testLat <- 45.96538183 # Fredericton

# Prepare the data to be sent in
# If you have a value you want to sum, use this
data <- all.data[,list(x, y, population)]

# If you want to perform a count, use this
# data <- all.data[,list(x, y)]
# data[,Value:=1]

sumInsideSquare <- function(pointLat, pointLng, interval, data) {
  # Sum all the values that fall within a square on a map given a point,
  # an interval of the map, and data that contains lat, lng and the values
  # of interest
  
  setnames(data, c("lng", "lat", "value"))
  
  # Get data inside lat/lon boundaries
  lng.interval <- c(pointLng, pointLng + interval)
  lat.interval <- c(pointLat - interval, pointLat)
  data <- data[lng %between% lng.interval][lat %between% lat.interval]
  
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

stopCluster(cl = cl)

# Convert to data frame
all.sums.table <- as.data.table(all.sums)


# Save to disk so I don't have to run it again
if (!file.exists("./GeneratedData")) {
  dir.create("./GeneratedData")
}
output.file <- "./GeneratedData/VoteDensity01.csv"
write.csv(all.sums.table, file = output.file, row.names = FALSE)

# End timer
totalTime <- proc.time() - start
print(totalTime)

# remove(cl, endLat, endLng, startLat, startLng, lat.list, start, calcSumLat, sumInsideSquare, interval)

