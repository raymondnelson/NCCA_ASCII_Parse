# lowPass.443() 
# 4rd order Butterworth Filter
# designed using the bilinear transform method
# corner frequency is the frequency at which the magnitude of the response is -3 dB
# 
# http://www-users.cs.york.ac.uk/~fisher/mkfilter
#
####


# corner frequency of a moving average filter
# (.443/number-of-samples)*Fc 
# Fc is the sampling rate

###

lowPass.443.4th <- function(x=myCardioData[,7]) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  output <- rep(NA, length(x))
  GAIN <- 2.430487254e+05
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- (xv0 + xv4) + 
      4 * (xv1 + xv3) + 
      6 * xv2 +
      (-0.7846227535 * yv0) + 
      (3.3286900196 * yv1) + 
      (-5.3017248756 * yv2) +
      (3.7575917791 * yv3)
    output[i] <- yv4
  }
  return(output)
} # end lowpass filter 3rd Order Butterworth .443

cardioLowPass.443 <- lowPass.443.4th(myCardioData[,7])
plot.ts(cardioLowPass.443[1:1000], ylim=c(-3, 10))

myCardioData2$CardioLowPass.443.4 <- smoothedCardio


# Recurrence relation:
#   y[n] = (  1 * x[n- 4])
# + (  4 * x[n- 3])
# + (  6 * x[n- 2])
# + (  4 * x[n- 1])
# + (  1 * x[n- 0])
# 
# + ( -0.7846227535 * y[n- 4])
# + (  3.3286900196 * y[n- 3])
# + ( -5.3017248756 * y[n- 2])
# + (  3.7575917791 * y[n- 1])




# /* Digital filter designed by mkfilter/mkshape/gencode   A.J. Fisher
# Command line: /www/usr/fisher/helpers/mkfilter -Bu -Lp -o 4 -a 1.4766666667e-02 0.0000000000e+00 -l */
#   
#   #define NZEROS 4
#   #define NPOLES 4
#   #define GAIN   2.430487254e+05
#   
#   static float xv[NZEROS+1], yv[NPOLES+1];
# 
# static void filterloop()
# { for (;;)
# { xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; xv[3] = xv[4]; 
#   xv[4] = `next input value' / GAIN;
#         yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; yv[3] = yv[4]; 
#         yv[4] =   (xv[0] + xv[4]) + 4 * (xv[1] + xv[3]) + 6 * xv[2]
#                      + ( -0.7846227535 * yv[0]) + (  3.3286900196 * yv[1])
#                      + ( -5.3017248756 * yv[2]) + (  3.7575917791 * yv[3]);
#         `next output value' = yv[4];
# }
# }
