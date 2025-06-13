# highPass4thOrder.338628()
# high pass filter to constrain the PLE data to a stable baseline
#
# first order Butterworth filter
#
####

highPass <- function(data=chartDF$c_PL, 
                            GAIN = 1.097118207e+00, 
                            zplane1 = -0.8307936245, 
                            zplane2 = 3.4771284678, 
                            zplane3 = -5.4610484201,
                            zplane4 = 3.8146904774) {
  xv1 <- data[1]
  xv2 <- data[2]
  xv3 <- data[3]
  xv4 <- data[4]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  output <- NULL
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- (xv4 + xv0) - 4 * (xv1 + xv3) + 6 * xv2 + (zplane1 * yv0) + (zplane2 * yv1) + (zplane3 * yv2) + (zplane4 * yv3)
    output <- c(output, yv3)
  }
  return(output)
} # end highPass3rdOrder.338628() function



# chartData$c_PL <- highPass.338628(data=stimSegmentDF$c_PL)
# plot.ts(highPass.338628(data=stimSegmentDF$c_PL))
# plot.ts(stimSegmentDF$c_PL)



#define GAIN   1.097118207e+00

# static float xv[NZEROS+1], yv[NPOLES+1];
# 
# static void filterloop()
# { for (;;)
# { xv[0] = xv[1]; 
# xv[1] = xv[2]; 
# xv[2] = xv[3]; 
# xv[3] = xv[4]; 
# xv[4] = next input value / GAIN;
# yv[0] = yv[1]; 
# yv[1] = yv[2]; 
# yv[2] = yv[3]; 
# yv[3] = yv[4]; 
# yv[4] =   (xv[0] + xv[4]) - 4 * (xv[1] + xv[3]) + 6 * xv[2] + ( -0.8307936245 * yv[0]) + (  3.4771284678 * yv[1]) + ( -5.4610484201 * yv[2]) + (  3.8146904774 * yv[3]);
# next output value = yv[4];
# }
# }
# 
# 
# 
