# PCASS EDA Filter

lowPass.886 <- function(x, GAIN = 1.174704212e+01, zplane = 0.8297443748) {
  # low pass 1st order Butterworth filter 
  # to reduce high frequency noise in the PCASS EDA
  # also used in the LIC Legacy Auto EDA (pre LXSoftware 9.9.6)
  # also used to smooth the pneumo data
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
} # end lowpass.886() function


highPass.04 <- function(x, GAIN = 1.004188815e+00, zplane = 0.9916573165) {
  # high pass 1st order Butterworth filter for PCASS EDA
  # same as the LIC Legacy Auto EDA (pre LXSoftware 9.9.6)
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 - xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end highPass.04 function


# highPass.04(lowPass.886(x))


