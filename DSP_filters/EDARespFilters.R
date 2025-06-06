highPass2nd.5_resp <- function(x=chartDF$c_AutoEDA, 
														 GAIN =  1.07686236756531e+000, 
														 zplane1 = -0.86234862603008, 
														 zplane2 = 1.85214648539594) {
  # high pass filter .5hz
  # to identify the respiration signal in EDA data
  xv1 <- x[1]
  xv2 <- x[2]
  yv1 <- 0
  yv2 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- (xv0 + xv2) - (2 * xv1)  + (zplane1 * yv0) + (zplane2 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end function



##############



lowPass2nd.1_resp <- function(x=chartDF$c_AutoEDA, 
														GAIN = 9.25428237930524e+003, 
														zplane1 = -0.97081513070039,
														zplane2 = 1.97038289837420) {
	# low pass 2nd order Butterworth filter .1hz
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
  }
  return(output)
} # end lowpass 2nd order function


highPass5th.5_resp <- function(data=chartDF$c_PL, 
                     GAIN = 1.18475488828012e+000, 
                     zplane1 = 0.71243128348853, 
                     zplane2 = -3.80355322574632, 
                     zplane3 = 8.13130172488137,
                     zplane4 = -8.70135524421046,
                     zplane5 = 4.66116478333266) {
  xv1 <- data[1]
  xv2 <- data[2]
  xv3 <- data[3]
  xv4 <- data[4]
  xv5 <- data[5]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- (xv5 - xv0) + 
    	5 * (xv1 - xv4) + 
    	10 * (xv3 - xv2) + 
    	(zplane1 * yv0) + 
    	(zplane2 * yv1) + 
    	(zplane3 * yv2) + 
    	(zplane4 * yv3) + 
    	(zplane5 * yv4)
    output[i] <- yv5
  }
  return(output)
} # end filter function



lowPass5th.5_resp <- function(data=chartDF$c_PL, 
                     GAIN = 2.99674453733902e+006, 
                     zplane1 = 0.71243128348853, 
                     zplane2 = -3.80355322574632, 
                     zplane3 = 8.13130172488137,
                     zplane4 = -8.70135524421046,
                     zplane5 = 4.66116478333266) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  xv5 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv4 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- (xv0 + xv5) + 
      5 * (xv1 + xv4) + 
      10 * (xv2 + xv3) +
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2) +
      (zplane4 * yv3) + 
      (zplane5 * yv4)
    output[i] <- yv5
  }
  return(output)
} # end lowpass filter
