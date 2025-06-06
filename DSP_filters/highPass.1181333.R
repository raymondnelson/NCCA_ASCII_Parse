# highPass.338628()
# high pass filter to constrain the PLE data to a stable baseline
#
# first order Butterworth filter
#
####


highPass.338628 <- function(data=chartDF$c_PL, GAIN = 1.052407779e+00, zplane = 0.9004040443) {
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
} # end highPass.338628() function

# chartData$c_PL <- highPass.338628(data=stimSegmentDF$c_PL)
# plot.ts(highPass.338628(data=stimSegmentDF$c_PL))
# plot.ts(stimSegmentDF$c_PL)


# #define GAIN   1.012371521e+00
# 
# static float xv[NZEROS+1], yv[NPOLES+1];
# 
# static void filterloop()
# { for (;;)
#   { xv[0] = xv[1]; 
#     xv[1] = next input value / GAIN;
#     yv[0] = yv[1]; 
#     yv[1] =   (xv[1] - xv[0]) + (  0.9755593256 * yv[0]);
#     next output value = yv[1];
#   }
# }