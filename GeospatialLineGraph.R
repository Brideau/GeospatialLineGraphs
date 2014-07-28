all.data <- read.csv("FrederictonPropertyTaxDiffCleanedv3.csv", header=TRUE, stringsAsFactors=FALSE)
all.data$X <- as.numeric(all.data$X)
all.data$Y <- as.numeric(all.data$Y)

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
startLat <- startEndVals[1]
endLat <- startEndVals[3]
startLng <- startEndVals[2]
endLng <- startEndVals[4]

num_intervals = 100.0
interval <- (startEndVals[1] - startEndVals[3]) / num_intervals

# testLng <- -66.6462379307115
# testLat <- 45.9581234392

# Prepare the data to be sent in
data <- all.data[,c("Y", "X", "levy2014_ha")]

sumInsideSquare <- function(pointLat, pointLng, interval, data) {
  # Sum all the values that fall within a square on a map given a point,
  # an interval of the map, and data that contains lat, lng and the values
  # of interest
  
  colnames(data) <- c("lat", "lng", "value")
  
  # Data east of point
  data <- data[data$lng > pointLng,] 
  # Data west of point + interval
  data <- data[data$lng < pointLng + interval,] 
  # Data north of point + interval (down)
  data <- data[data$lat > pointLat - interval,]
  # Data south of point
  data <- data[data$lat < pointLat, ]
  
  # Clean remaining data
  data <- na.omit(data)
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

# Get each line of data to plot
lat <- startLat
rowCount <- 1
all.sums <- list()
while (lat > endLat) {
  col <- calcSumLat(startLng, endLng, lat, interval, data)
  all.sums[[as.character(rowCount)]] <- col
  lat <- lat - interval
  rowCount <- rowCount + 1
}

# Convert to data frame
all.sums.frame <- data.frame(all.sums)

# Save to disk so I don't have to run it again
write.csv(all.sums.frame, file = "Levy2014Sums100", row.names = FALSE)












