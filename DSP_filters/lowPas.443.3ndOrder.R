# lowPass.443() 
# 3rd order Butterworth Filter
# designed by the bilinear transform method
# corner frequency is the frequency at which the magnitude of the response is -3 dB
# 
# http://www-users.cs.york.ac.uk/~fisher/mkfilter
#
####


# corner frequency of a moving average filter
# (.443/number-of-samples)*Fc 
# Fc is the sampling rate


# # define the low pass .443 function
# lowPass.443.3rd <- function(x, GAIN = 1.096672395e+04, zplane = 0.8297443748) {
# 
#   xv1 <- x[1]
#   yv1 <- 0
#   output <- rep(NA, length(x))
#   # output <- NULL
#   for (i in 1:length(x)) {
#     xv0 <- xv1
#     xv1 <- x[i] / GAIN
#     yv0 <- yv1
#     yv1 <- (xv1 + xv0) + (zplane * yv0)
#     output[i] <- yv1
#     # output <- c(output, yv1)
#   }
#   return(output)
# } # end lowpass .886 function


lowPass.443.3rd <- function(x) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  yv3 <- 0
  yv2 <- 0
  yv1 <- 0
  yv0 <- 0
  output <- rep(NA, length(x))
  GAIN <- 1.096672395e+04
  for (i in length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- (xv0 + xv3) + 
      3 * (xv1 + xv2) + 
      (0.8305807626 * yv0) + 
      (-2.6458129062 * yv1) + 
      (2.8145026641 * yv2)
    output[i] <- yv3
  }
  return(output)
  # could also build up an output vector using concatenation 
  # but that is slower than making the vector and replacing the value
} # end lowpass filter 3rd Order Butterworth .443

lowPass.443.3rd(myCardioData[,7])
ts.plot(lowPass.443.3rd(myCardioData[,7])[1:1000], ylim=c(-3,10))

# Recurrence relation:y[n] = 
#   (  1 * x[n- 3])     + 
#   (  3 * x[n- 2])     + 
#   (  3 * x[n- 1])     + 
#   (  1 * x[n- 0])     + 
#   (  0.8305807626 * y[n- 3])     + 
#   ( -2.6458129062 * y[n- 2])     + 
#   (  2.8145026641 * y[n- 1])



# /* Digital filter designed by mkfilter/mkshape/gencode   A.J. Fisher
# Command line: /www/usr/fisher/helpers/mkfilter -Bu -Lp -o 3 -a 1.4766666667e-02 0.0000000000e+00 -l */
#   
#   #define NZEROS 3
#   #define NPOLES 3
#   #define GAIN   1.096672395e+04
#   
#   static float xv[NZEROS+1], yv[NPOLES+1];
# 
# static void filterloop()
# { for (;;)
# { xv[0] = xv[1]; xv[1] = xv[2]; xv[2] = xv[3]; 
#   xv[3] = `next input value' / GAIN;
#         yv[0] = yv[1]; yv[1] = yv[2]; yv[2] = yv[3]; 
#         yv[3] =   (xv[0] + xv[3]) + 3 * (xv[1] + xv[2])
#                      + (  0.8305807626 * yv[0]) + ( -2.6458129062 * yv[1])
#                      + (  2.8145026641 * yv[2]);
#         `next output value' = yv[3];
# }
# }

