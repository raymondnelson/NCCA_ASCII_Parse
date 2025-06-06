# lowPass3hz() 
# first order Butterworth filter
# to remove the dichrotic notch from the cardio 
# in order to calculate the cardio rate
#
###

# define the low pass 3hz function
lowPass3hz <- function(x, GAIN = 4.077683537e+00, zplane = 0.5095254495) {
  # first order Butterworth lowpass filter at 3hz
  # to improve the diagnostic coefficient by reducing high frequency noise
  # x is a column vector from the time series 
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  # output <- NULL
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
    # output <- c(output, yv1)
  }
  return(output)
} # end lowpass3hzfunction


