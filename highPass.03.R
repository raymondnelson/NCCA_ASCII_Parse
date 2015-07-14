# highPass.03()
# high pass filter to auto center the Auto EDA
# to improve the visual appearance and diagnostic coeficient
# first order Butterworth filter
####


highPass.03 <- function(data =chartData$AutoEDA, GAIN = 1.003141603e+00, zplane = 0.9937364715) {
  #         EDA <- EDA
  #         GAIN <- GAIN
  #         zplane <- zplane
  xv1 <- data[1]
  yv1 <- 0
  output <- NULL
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 - xv0) + (zplane * yv0)
    output <- c(output, yv1)
  }
  return(output)
} # end highPass.03 function

# chartData$AutoEDA <- highPass.03(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



