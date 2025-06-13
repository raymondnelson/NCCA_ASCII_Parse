# lowPass.2()
# low pass filter to improve the diagnostic coeficient of the Auto EDA data
# will add the AutoEDA column to the data frame
# first order Butterworth filter
####


lowPass.2 <- function(x = chartData$EDA1, GAIN = 4.873950141e+01, zplane = 0.9589655220) {
  #         EDA <- EDA
  #         GAIN <- GAIN
  #         zplane <- zplane
  xv1 <- x[1]
  yv1 <- 0
  output <- NULL
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output <- c(output, yv1)
  }
  return(output)
} # end lowPass.2 function

# chartData$AutoEDA <- lowPass.2(chartData$EDA1, GAIN = 4.873950141e+01, zplane = 0.9589655220)
# plot.ts(chartData$AutoEDA)



