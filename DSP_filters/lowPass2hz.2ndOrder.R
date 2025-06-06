lowPass2hz.2nd <- function(x, GAIN = 2.97868961737845e+001, zplane0 = -0.55326988968868, zplane1 = 1.41898265221812) {
  # 2nd order Butterworth low-pass filter
  # to remove the dichrotic notch 
  # and improve calculation of the the cardio rate
  # x input is a column vector from the time series 
  # output is a filtered time series vector
  ###
  # initialize the registers
  xv1 <- x[1]
  xv2 <- x[1]
  yv1 <- 0
  yv2 <- 0
  # inititialize the output
  output <- rep(NA, length(x))
  # use a loop
  for (i in 1:length(x)) {
    # shift the xv registers
    xv0 <- xv1
    xv1 <- xv2
    # compute xv2
    xv2 <- x[i] / GAIN
    # shift the yv registers
    yv0 <- yv1
    yv1 <- yv2
    # computer yv2
    yv2 <- xv0 +
      xv2 +
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    # set the output
    output[i] <- yv2
  } # end loop
  return(output)
} # end lowpass 2nd order