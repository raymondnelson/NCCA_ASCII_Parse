# highPass3rdOrder()
# high pass filter to constrain the PLE data to a stable baseline
#
# first order Butterworth filter
#
####

highPass.338628 <- function(data=chartDF$c_PL, 
                            GAIN = 1.053761204e+00, 
                            zplane1 = 0.9005660890, 
                            zplane2 = -2.7959945843, 
                            zplane3 = 2.8952921779) {
  xv1 <- data[1]
  xv2 <- data[2]
  xv3 <- data[3]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  output <- NULL
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- (xv3 - xv0) + 3 * (xv1 - xv2) + (zplane1 * yv0) + (zplane2 * yv1) + (zplane3 * yv2)
    output <- c(output, yv2)
  }
  return(output)
} # end highPass3rdOrder.338628() function



# chartData$c_PL <- highPass.338628(data=stimSegmentDF$c_PL)
# plot.ts(highPass.338628(data=stimSegmentDF$c_PL))
# plot.ts(stimSegmentDF$c_PL)



# #define GAIN   1.073513554e+00
# 
# static float xv[NZEROS+1], yv[NPOLES+1];
# 
# static void filterloop()
# { for (;;)
#   { xv[0] = xv[1]; 
#     xv[1] = xv[2]; 
#     xv[2] = xv[3]; 
#     xv[3] = next input value / GAIN;
#     yv[0] = yv[1]; yv[1] = yv[2]; 
#     yv[2] = yv[3]; 
#     yv[3] =   (xv[3] - xv[0]) + 3 * (xv[1] - xv[2]) + (  0.8677306442 * yv[0]) + ( -2.7262487996 * yv[1]) + (  2.8581854321 * yv[2]);
#     next output value = yv[3];
#   }
# } 


