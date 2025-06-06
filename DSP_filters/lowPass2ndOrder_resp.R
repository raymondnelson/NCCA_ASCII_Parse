# lowPass.886() 
# 2nd order Butterworth Filter
# designed by the bilinear transform method
# corner frequency is the frequency at which the magnitude of the response is -3 dB
# 
# http://www-users.cs.york.ac.uk/~fisher/mkfilter
#
####


# Recurrence relation:
#   y[n] = (  1 * x[n- 2])
#   + (  2 * x[n- 1])
#   + (  1 * x[n- 0])
#   + ( -0.8770310353 * y[n- 2])
#   + (  1.8689576707 * y[n- 1])


# define the low pass function
lowPass2nd_resp <- function(x, 
														GAIN = 7.48547815673697e+001, 
														zplane1 = -0.70089678118840,
														zplane2 = 1.64745998107698) {
	# low pass 2nd order Butterworth filter
	# to identify the respiration signal in EDA data
  xv1 <- x[1]
  xv2 <- x[1]
  yv1 <- 0
  yv2 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- xv0 +
      xv2 +
      (2 * xv1) +
      (zplane1 * yv2) +
      (zplane2 * yv1)
    output[i] <- yv2
    # output <- c(output, yv1)
  }
  return(output)
} # end lowpass 2nd order function

myPlot <- lowPass.886.2nd(x=myCardioData[,7])
plot.ts(myPlot[1:1000])

/* Digital filter designed by mkfilter/mkshape/gencode   A.J. Fisher   
Command line: /www/usr/fisher/helpers/mkfilter -Bu -Lp -o 2 -a 1.4766666667e-02 0.0000000000e+00 -l */
  
  #define NZEROS 2#define NPOLES 2
  #define GAIN   4.954563811e+02
  static float xv[NZEROS+1], yv[NPOLES+1];
static void filterloop()  
{ for (;;)      
{ xv[0] = xv[1]; xv[1] = xv[2];         
  xv[2] = next input value / GAIN;        
  yv[0] = yv[1]; 
  yv[1] = yv[2];         
  yv[2] =   (xv[0]
             + xv[2])
  + 2 * xv[1]
  + ( -0.8770310353 * yv[0])
  + (  1.8689576707 * yv[1]);
  next output value = yv[2];
}
}



