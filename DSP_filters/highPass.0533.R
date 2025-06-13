# highPass.0533()
# high pass filter to auto center the Auto EDA
# to improve the visual appearance and diagnostic coeficient
# first order Butterworth filter
####


highPass.0533 <- function(x, GAIN = 1.005585112e+00, zplane = 0.9888918171) {
  xv1 <- x[1]
  yv1 <- 0
  output <- NULL
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 - xv0) + (zplane * yv0)
    output <- c(output, yv1)
  }
  return(output)
} # end highPass.0533 function

# chartData$AutoEDA <- highPass.0533(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



