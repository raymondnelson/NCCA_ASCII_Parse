# lowPass1.7hz() 
# first order Butterworth filter
# to remove the dichrotic notch from the cardio 
# in order to calculate the cardio rate
#
###

#####

# define the low pass 1.17hz function
lowPass1.7hz <- function(x, GAIN = 6.557766334e+00, zplane = 0.6950181055) {
  # first order Butterworth lowpass filter at 3hz
  # to remove the cardio dichrotic notch
  # to improve the calculation of the cardio rate
  # x input is a column vector of time series data
  # output is a filtered time series vector
  ###
  # initialize the registers
  xv1 <- x[1]
  yv1 <- 0
  # initialize the output
  output <- rep(NA, length(x))
  # use a loop
  # output <- NULL
  for (i in 1:length(x)) {
  	# shift the register
    xv0 <- xv1
    # comput xv1
    xv1 <- x[i] / GAIN
    # shift the register
    yv0 <- yv1
    # compute yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    # set the output
    output[i] <- yv1
    # output <- c(output, yv1)
  } # end loop
  return(output)
} # end lowpass1.7hz function

