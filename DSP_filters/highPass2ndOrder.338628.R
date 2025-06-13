# highPass2ndOrder.338628()
# high pass filter to constrain the PLE data to a stable baseline
#
# first order Butterworth filter
#
####

highPass.338628 <- function(data=chartDF$c_PL, GAIN = 1.051429057e+00, zplane1 = -0.9045669959, zplane2 = 1.8997790727) {
  xv1 <- data[1]
  xv2 <- data[2]
  yv1 <- 0
  yv2 <- 0
  output <- NULL
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- (xv0 + xv2) - (2 * xv1)  + (zplane1 * yv0) + (zplane2 * yv1)
    output <- c(output, yv2)
  }
  return(output)
} # end highPass.338628() function

# chartData$c_PL <- highPass.338628(data=stimSegmentDF$c_PL)
# plot.ts(highPass.338628(data=stimSegmentDF$c_PL))
# plot.ts(stimSegmentDF$c_PL)



# #define GAIN   1.051429057e+00
# 
# static float xv[NZEROS+1], yv[NPOLES+1];
# 
# static void filterloop()
# { for (;;)
#   { xv[0] = xv[1]; 
#     xv[1] = xv[2]; 
#     xv[2] = next input value / GAIN;
#     yv[0] = yv[1]; 
#     yv[1] = yv[2]; 
#     yv[2] =   (xv[0] + xv[2]) - 2 * xv[1] + ( -0.9045669959 * yv[0]) + (  1.8997790727 * yv[1]);
# next output value = yv[2];
# }
# }