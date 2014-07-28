plot.data <- read.csv("Levy2014Sums100.csv", header=TRUE, stringsAsFactors=FALSE)

max <- max(plot.data)

plot(0, 0, xlim=c(0, length(plot.data[[1]])), ylim=c(0,1000), type="n", las=1, xlab="Longitude", ylab="Latitude", bty="n")

for (i in 1:length(plot.data)) {
  lines(1:length(plot.data[[1]]), plot.data[[i]]/max*400, col="#000000")
  print(i)
}
