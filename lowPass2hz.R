# lowPass2hz() 
# first order Butterworth filter
# to remove the dichrotic notch from the cardio 
# in order to calculate the cardio rate
#
###

# define the low pass 2hz function
lowPass2hz <- function(x, GAIN = 5.704630110e+00, zplane = 0.6494075932) {
  # first order Butterworth lowpass filter at 3hz
  # to remove the cardio dichrotic notch
  # to improve the calculation of the cardio rate
  # x input is a column vector from the time series 
  # output is a filtered time series vector
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
} # end lowpass2hz function


