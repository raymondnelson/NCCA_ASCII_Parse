# highPass.1()
# high pass filter to auto center the Auto EDA
# to improve the visual appearance and diagnostic coeficient
# first order Butterworth filter
####


highPass.1 <- function(x, GAIN = 1.010472358e+00, zplane = 0.9792723507) {
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
} # end highPass.1 function

# chartData$AutoEDA <- highPass.1(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



