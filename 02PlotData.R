plot.data <- read.csv("Levy2014Sums100.csv", header=TRUE, stringsAsFactors=FALSE)

max <- max(plot.data)

plot(0, 0, xlim=c(0, length(plot.data[[1]])), ylim=c(0,1300), type="n", las=1, xlab=NA, ylab=NA, bty="n")

plottingHeight <- 1000
scaleFactor <- 400

for (i in 1:length(plot.data)) {
  # lines(1:length(plot.data[[1]]), plot.data[[i]]/max*400, col="#000000")
  
  row <- as.vector(plot.data[[i]]/max*scaleFactor + plottingHeight)
  
  xes <- c(0, 0:(length(plot.data[[1]]) - 1), length(plot.data[[1]]))
  ys <- c(plottingHeight, row, plottingHeight)
  
  polygon(x = xes, y = ys, col = "#EDEDED50")
  
  plottingHeight <- plottingHeight - 10
}

