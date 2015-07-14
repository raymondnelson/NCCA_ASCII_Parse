# lowPass.886() 
# to reduce noise and improve the diagnostic coeficient of the pneumo data
# can also be used with the raw EDA data
# will also add 2 columns to the time series data frames 
# for the filtered upper and lower pneumo data
# first order Butterworth filter
#
###

# define the low pass .886 function
lowPass.886 <- function(x, GAIN = 1.174704212e+01, zplane = 0.8297443748) {
  # filter to smooth peneumo and raw EDA data 
  # to improve the diagnostic coefficient by reducing high frequency noise
  
  # to improve the 
  
  # x is a column vector from the time series 
  
  #         data <- data
  #         GAIN <- GAIN
  #         zplane <- zplane
  
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
} # end lowpass .886 function


