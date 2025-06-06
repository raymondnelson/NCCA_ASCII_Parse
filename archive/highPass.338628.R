# highPass.338628()
# high pass filter to constrain the PLE data to a stable baseline
#
# first order Butterworth filter
#
####


highPass.338628 <- function(data=chartDF$c_PL, GAIN = 1.035475913e+00, zplane = 0.9314790191) {
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

# 2nd order filter specification
# #define GAIN   1.051429057e+00
# 
# static float xv[NZEROS+1], yv[NPOLES+1];
# 
# static void filterloop()
# { for (;;)
# { xv[0] = xv[1]; xv[1] = xv[2]; 
# xv[2] = next input value / GAIN;
# yv[0] = yv[1]; yv[1] = yv[2]; 
# yv[2] =   (xv[0] + xv[2]) - 2 * xv[1]
# + ( -0.9045669959 * yv[0]) + (  1.8997790727 * yv[1]);
# next output value = yv[2];
# }
# }