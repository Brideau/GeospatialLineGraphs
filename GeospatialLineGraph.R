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

num_intervals = 100.0
interval <- (startEndVals[1] - startEndVals[3]) / num_intervals

# Provided a point, an interval, and a set of data with lat, lng, and a value of interest, return the sum
# of all the values that are within a square bounded by point + interval along lat and lng.

# testLng <- -66.6462379307115
# testLat <- 45.9581234392

# Prepare the data to be sent in
data <- all.data[,c("Y", "X", "levy2014_ha")]

sumInsideSquare <- function(point_lat, point_lng, interval, data) {
  
  colnames(data) <- c("lat", "lng", "value")
  
  # Data east of point
  data <- data[data$lng > point_lng,] 
  # Data west of point + interval
  data <- data[data$lng < point_lng + interval,] 
  # Data north of point + interval (down)
  data <- data[data$lat > point_lat - interval,]
  # Data south of point
  data <- data[data$lat < point_lat, ]
  
  # Clean remaining data
  data <- na.omit(data)
  return(sum(data$value))
}

squareSumTemp <- sumInsideSquare(testLat, testLng, interval, data)



















