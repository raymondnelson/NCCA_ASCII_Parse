# lowPass.4()
# low pass filter to improve the diagnostic coeficient of the Auto EDA data
# will add the AutoEDA column to the data frame
# first order Butterworth filter
####


lowPass.4 <- function(x, GAIN = 2.485927720e+01, zplane = 0.9195471379) {
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
} # end lowPass.4 function

# chartData$AutoEDA <- lowPass.4(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



