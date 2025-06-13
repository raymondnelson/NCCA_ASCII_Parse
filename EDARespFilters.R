bandPass_2nd_respFn <- function(x=chartDF$c_AutoEDA, 
                         gain= 9.13874496108193e+001, 
                         z1=-0.72839963878213, 
                         z2=3.13119010377342, 
                         z3=-5.07535624928687, 
                         z4=3.67252553263943) {
  # bandpass 2nd order Butterworth filter 
  # to identify the respiration signal in EDA data
  # .13hz to 1.2hz
  ###
  # initialize the registers
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  # initialize the output vector
  output <- rep(NA, length(x))
  GAIN <- gain
  for (i in 1:length(x)) {
    # increment the xv registers
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    # next input
    xv4 <- x[i] / GAIN
    # increment the yv registers
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    # compute the new output value
    yv4 <- (xv0 + xv4) - 2 * xv2 +
      (z1 * yv0) +
      (z2 * yv1) +
      (z3 * yv2) +
      (z4 * yv3)
    output[i] <- yv4
  }
  return(output)
} # end filter function



bandPass8th.5hz2hz_resp <- function(x=chartDF$c_AutoEDA, 
                         GAIN= 3.90555735959591e+007, 
                         z1=-2.93323511237868, 
                         z2=41.86984753368098, 
                         z3=-281.96583021763888, 
                         z4=1189.04160047905500, 
                         z5=-3514.00366549222830, 
                         z6=7716.81494306996500, 
                         z7=-13025.31035593675700, 
                         z8=17237.31284812648700, 
                         z9=-18074.46131611405900, 
                         z10=15066.70647421277900, 
                         z11=-9951.49256972647340, 
                         z12=5153.41364677301040, 
                         z13=-2051.27747283947340, 
                         z14=606.73167547955177, 
                         z15=-125.77374706285426, 
                         z16=16.32715682149710 ) {
  # bandpass 8th order Butterworth filter 
  # .5hz to 2hz
  ###
  # initialize the registers
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  xv5 <- x[1]
  xv6 <- x[1]
  xv7 <- x[1]
  xv8 <- x[1]
  xv9 <- x[1]
  xv10 <- x[1]
  xv11 <- x[1]
  xv12 <- x[1]
  xv13 <- x[1]
  xv14 <- x[1]
  xv15 <- x[1]
  xv16 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  yv9 <- 0
  yv10 <- 0
  yv11 <- 0
  yv12 <- 0
  yv13 <- 0
  yv14 <- 0
  yv15 <- 0
  yv16 <- 0
  # initialize the output vector
  output <- rep(NA, length(x))
  # iterate over the input vector
  for (i in 1:length(x)) {
    # increment the xv registers
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv6 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- xv9
    xv9 <- xv10
    xv10 <- xv11
    xv11 <- xv12
    xv12 <- xv13
    xv13 <- xv14
    xv14 <- xv15
    xv15 <- xv16
    # next input
    xv16 <- x[i] / GAIN
    # increment the yv registers
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- yv9
    yv9 <- yv10
    yv10 <- yv11
    yv11 <- yv12
    yv12 <- yv13
    yv13 <- yv14
    yv14 <- yv15
    yv15 <- yv16    
    # compute the new output value
    yv16 <- (xv0 + xv16) - 
    	8 * (xv2 + xv14) +
      28 * (xv4 + xv12) -
      56 * (xv6 + xv10) + 
      70 * xv8 +
      (z1 * yv0) +
      (z2 * yv1) +
      (z3 * yv2) +
      (z4 * yv3) +
      (z5 * yv4) +
      (z6 * yv5) +
      (z7 * yv6) +
      (z8 * yv7) +
      (z9 * yv8) +
      (z10 * yv9) +
      (z11 * yv10) +
      (z12 * yv11) +
      (z13 * yv12) +
      (z14 * yv13) +
      (z15 * yv14) +
      (z16 * yv15) 
      output[i] <- yv16
  }
  return(output)
} # end band pass filter function



##############



lowPass2nd.5hz_resp <- function(x=chartDF$c_AutoEDA, 
														GAIN = 3.92074579585044e+002, 
														zplane1 = -0.86234862603008,
														zplane2 = 1.85214648539594) {
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
    yv2 <- (xv0 + xv2) + (2 * xv1) + (zplane1 * yv2) + (zplane2 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end 2nd order lowpass filter function



################



highPass2nd.5hz_resp <- function(x=chartDF$c_AutoEDA, 
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
} # end 2nd order highPass filter function



##############



lowPass2nd.1hz_resp <- function(x=chartDF$c_AutoEDA, 
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



################



highPass2nd.1hz_resp <- function(x=chartDF$c_AutoEDA, 
														 GAIN = 1.01491982145825e+000, 
														 zplane1 = -0.97081513070039, 
														 zplane2 = 1.97038289837420) {
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
} # end 2nd order highPass filter function



#################



highPass5th.5hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.18475488828012e+000, 
                     zplane1 = 0.71243128348853, 
                     zplane2 = -3.80355322574632, 
                     zplane3 = 8.13130172488137,
                     zplane4 = -8.70135524421046,
                     zplane5 = 4.66116478333266) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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




#################



highPass5th.1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.03446955720955e+000, 
                     zplane1 = 0.93446829447256, 
                     zplane2 = -4.73567570376564, 
                     zplane3 = 9.60017143634846,
                     zplane4 = -9.73118834313281,
                     zplane5 = 4.93222431218111) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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




#########################



lowPass5th.5hz_resp <- function(x=chartDF$c_AutoEDA, 
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
    xv5 <- x[i] * GAIN
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
} # end 5th order lowpass filter function



#################



highPass5th.5hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.18475488828012e+000, 
                     zplane1 = 0.71243128348853, 
                     zplane2 = -3.80355322574632, 
                     zplane3 = 8.13130172488137,
                     zplane4 = -8.70135524421046,
                     zplane5 = 4.66116478333266) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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
} # end 5th order highpass filter function




#################



highPass5th.05hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.01708847363468e+000, 
                     zplane1 = 0.96667955789231, 
                     zplane2 = -4.86615660643900, 
                     zplane3 = 9.79838659796873,
                     zplane4 = -9.86502157011154,
                     zplane5 = 4.96611202056567) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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
} # end 5th order highpass filter function




######################



lowPass5th.1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 8.21287769882527e+009, 
                     zplane1 = 0.93446829447256, 
                     zplane2 = -4.73567570376564, 
                     zplane3 = 9.60017143634846,
                     zplane4 = -9.73118834313281,
                     zplane5 = 4.93222431218111) {
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
    xv5 <- x[i] * GAIN
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



#################



highPass5th.1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.03446955720955e+000, 
                     zplane1 = 0.93446829447256, 
                     zplane2 = -4.73567570376564, 
                     zplane3 = 9.60017143634846,
                     zplane4 = -9.73118834313281,
                     zplane5 = 4.93222431218111) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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
} # end 5th order highpass filter function




#################



highPass5th.25hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.08842535850130e+000, 
                     zplane1 = 0.84411709735181, 
                     zplane2 = -4.36360802824800, 
                     zplane3 = 9.02545247053866,
                     zplane4 = -9.33652742158010,
                     zplane5 = 4.83056551995258 ) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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
} # end 5th order highpass filter function




######################



lowPass5th1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.09498061323869e+005, 
                     zplane1 =   0.50697316669026, 
                     zplane2 = -2.87829142186758, 
                     zplane3 = 6.56261229709249,
                     zplane4 = -7.51418241128532,
                     zplane5 = 4.32259612675314) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] * GAIN
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
} # end 5th order lowpass filter



#################



highPass5th1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.40445398031915e+000, 
                     zplane1 =   0.50697316669026, 
                     zplane2 = -2.87829142186758, 
                     zplane3 = 6.56261229709249,
                     zplane4 = -7.51418241128532,
                     zplane5 = 4.32259612675314) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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
} # end 5th order highpass filter function



######################



lowPass5th2hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 4.56744842280384e+003, 
                     zplane1 = 0.25462875802297, 
                     zplane2 = -1.61760081476554, 
                     zplane3 = 4.16836695537251,
                     zplane4 = -5.45962345438380,
                     zplane5 = 3.64722245584034) {
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
    xv5 <- x[i] * GAIN
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
} # end 5th order lowpass filter



#################



highPass5th2hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.98173798247645e+000, 
                     zplane1 = 0.25462875802297, 
                     zplane2 = -1.61760081476554, 
                     zplane3 = 4.16836695537251,
                     zplane4 = -5.45962345438380,
                     zplane5 = 3.64722245584034) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
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
    xv5 <- x[i] / GAIN
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
} # end 5th order highpass filter function



######################



lowPass8th.4hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.30179357659092e+011, 
                     zplane1 = -0.65077999154775, 
                     zplane2 = 5.48568954669265, 
                     zplane3 = -20.23833006539017,
                     zplane4 = 42.68268683508424,
                     zplane5 = -56.28387200632671,
                     zplane6 = 47.51982890060499,
                     zplane7 = -25.08581602058573,
                     zplane8 = 7.57059279950196 ) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  xv5 <- x[1]
  xv6 <- x[1]
  xv7 <- x[1]
  xv8 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) + 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) +
			56 * (xv3 + xv5) +
			70 * xv4 + 
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2) +
      (zplane4 * yv3) + 
      (zplane5 * yv4) +
      (zplane6 * yv5) +
      (zplane7 * yv6) +
      (zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order lowpass filter



#################



highPass8th.13hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.07227616439073e+000, 
                     zplane1 = -0.86973449591837, 
                     zplane2 = 7.07925718839002, 
                     zplane3 = -25.21072028741809,
                     zplane4 = 51.30543824837988,
                     zplane5 = -65.25898623489879,
                     zplane6 = 53.12709867217181,
                     zplane7 = -27.03279185493567,
                     zplane8 = 7.86043876422893 ) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
  xv6 <- x[6]
  xv7 <- x[7]
  xv8 <- x[8]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) - 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) -
			56 * (xv3 + xv5) +
			70 * xv4 +    	
    	(zplane1 * yv0) + 
    	(zplane2 * yv1) + 
    	(zplane3 * yv2) + 
    	(zplane4 * yv3) + 
    	(zplane5 * yv4) +
    	(zplane6 * yv5) +
    	(zplane7 * yv6) +
    	(zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order highpass filter function




######################



lowPass8th1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.15003671132011e+008, 
                     zplane1 = -0.34092050643327, 
                     zplane2 = 3.09328898920349, 
                     zplane3 = -12.30643883373650,
                     zplane4 = 28.04257095303031,
                     zplane5 = -40.03540310733893,
                     zplane6 = 36.67463273552779,
                     zplane7 = -21.05438858040276,
                     zplane8 = 6.92665612413398 ) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  xv5 <- x[1]
  xv6 <- x[1]
  xv7 <- x[1]
  xv8 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) + 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) +
			56 * (xv3 + xv5) +
			70 * xv4 + 
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2) +
      (zplane4 * yv3) + 
      (zplane5 * yv4) +
      (zplane6 * yv5) +
      (zplane7 * yv6) +
      (zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order lowpass filter



#################



highPass8th1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.71266900257425e+000, 
                     zplane1 = -0.34092050643327, 
                     zplane2 = 3.09328898920349, 
                     zplane3 = -12.30643883373650,
                     zplane4 = 28.04257095303031,
                     zplane5 = -40.03540310733893,
                     zplane6 = 36.67463273552779,
                     zplane7 = -21.05438858040276,
                     zplane8 = 6.92665612413398 ) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
  xv6 <- x[6]
  xv7 <- x[7]
  xv8 <- x[8]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) - 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) -
			56 * (xv3 + xv5) +
			70 * xv4 +    	
    	(zplane1 * yv0) + 
    	(zplane2 * yv1) + 
    	(zplane3 * yv2) + 
    	(zplane4 * yv3) + 
    	(zplane5 * yv4) +
    	(zplane6 * yv5) +
    	(zplane7 * yv6) +
    	(zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order highpass filter function



#################



highPass8th2hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 2.95609378266894e+000, 
                     zplane1 = -0.11443624107823, 
                     zplane2 = 1.16101198740406, 
                     zplane3 = -5.19371255664330,
                     zplane4 = 13.38941776172914,
                     zplane5 = -21.77448794466058,
                     zplane6 = 22.89526737338233,
                     zplane7 = -15.21792905591270,
                     zplane8 = 5.85450783310938 ) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
  xv6 <- x[6]
  xv7 <- x[7]
  xv8 <- x[8]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) - 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) -
			56 * (xv3 + xv5) +
			70 * xv4 +    	
    	(zplane1 * yv0) + 
    	(zplane2 * yv1) + 
    	(zplane3 * yv2) + 
    	(zplane4 * yv3) + 
    	(zplane5 * yv4) +
    	(zplane6 * yv5) +
    	(zplane7 * yv6) +
    	(zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order highpass filter function



#################



highPass8th.07hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.03828959555161e+000, 
                     zplane1 = -0.92760481830185, 
                     zplane2 = 7.49054668855585, 
                     zplane3 = -26.46351377038895,
                     zplane4 = 53.42553604225387,
                     zplane5 = -67.41177158167568,
                     zplane6 = 54.43873705127037,
                     zplane7 = -27.47678107780597,
                     zplane8 = 7.92485146609233 ) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
  xv6 <- x[6]
  xv7 <- x[7]
  xv8 <- x[8]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) - 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) -
			56 * (xv3 + xv5) +
			70 * xv4 +    	
    	(zplane1 * yv0) + 
    	(zplane2 * yv1) + 
    	(zplane3 * yv2) + 
    	(zplane4 * yv3) + 
    	(zplane5 * yv4) +
    	(zplane6 * yv5) +
    	(zplane7 * yv6) +
    	(zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order highpass filter function



######################



lowPass8th2hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 7.09450465119679e+005, 
                     zplane1 = -0.11443624107823, 
                     zplane2 = 1.16101198740405, 
                     zplane3 = -5.19371255664330,
                     zplane4 = 13.38941776172913,
                     zplane5 = -21.77448794466057,
                     zplane6 = 22.89526737338233,
                     zplane7 = -15.21792905591270,
                     zplane8 = 5.85450783310938 ) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  xv5 <- x[1]
  xv6 <- x[1]
  xv7 <- x[1]
  xv8 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) + 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) +
			56 * (xv3 + xv5) +
			70 * xv4 + 
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2) +
      (zplane4 * yv3) + 
      (zplane5 * yv4) +
      (zplane6 * yv5) +
      (zplane7 * yv6) +
      (zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order lowpass filter



#################



highPass8th.5hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.30806264587028e+000, 
                     zplane1 = -0.58444401079729, 
                     zplane2 = 4.98925217538350, 
                     zplane3 = -18.64512559972886,
                     zplane4 = 39.84030990132061,
                     zplane5 = -53.23930062034415,
                     zplane6 = 45.56182804590351,
                     zplane7 = -24.38577030289625,
                     zplane8 = 7.46325040002170 ) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
  xv6 <- x[6]
  xv7 <- x[7]
  xv8 <- x[8]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) - 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) -
			56 * (xv3 + xv5) +
			70 * xv4 +    	
    	(zplane1 * yv0) + 
    	(zplane2 * yv1) + 
    	(zplane3 * yv2) + 
    	(zplane4 * yv3) + 
    	(zplane5 * yv4) +
    	(zplane6 * yv5) +
    	(zplane7 * yv6) +
    	(zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order highpass filter function



######################



lowPass8th.5hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 2.29859361273411e+010, 
                     zplane1 = -0.58444401079729, 
                     zplane2 = 4.98925217538350, 
                     zplane3 = -18.64512559972886,
                     zplane4 = 39.84030990132061,
                     zplane5 = -53.23930062034415,
                     zplane6 = 45.56182804590351,
                     zplane7 = -24.38577030289625,
                     zplane8 = 7.46325040002170 ) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  xv4 <- x[1]
  xv5 <- x[1]
  xv6 <- x[1]
  xv7 <- x[1]
  xv8 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  yv4 <- 0
  yv5 <- 0
  yv6 <- 0
  yv7 <- 0
  yv8 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) + 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) +
			56 * (xv3 + xv5) +
			70 * xv4 + 
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2) +
      (zplane4 * yv3) + 
      (zplane5 * yv4) +
      (zplane6 * yv5) +
      (zplane7 * yv6) +
      (zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order lowpass filter



#################



highPass8th.1hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 1.05514568641357e+000, 
                     zplane1 = -0.89820433002190, 
                     zplane2 = 7.28206131344608, 
                     zplane3 = -25.82989131963076,
                     zplane4 = 52.35568388544891,
                     zplane5 = -66.32789698227020,
                     zplane6 = 53.77987205816368,
                     zplane7 = -27.25426968367329,
                     zplane8 = 7.89264505853745 ) {
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
  xv6 <- x[6]
  xv7 <- x[7]
  xv8 <- x[8]
  yv1 <- 1
  yv2 <- 1
  yv3 <- 1
  yv4 <- 1
  yv5 <- 1
  yv6 <- 1
  yv7 <- 1
  yv8 <- 1
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) - 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) -
			56 * (xv3 + xv5) +
			70 * xv4 +    	
    	(zplane1 * yv0) + 
    	(zplane2 * yv1) + 
    	(zplane3 * yv2) + 
    	(zplane4 * yv3) + 
    	(zplane5 * yv4) +
    	(zplane6 * yv5) +
    	(zplane7 * yv6) +
    	(zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order highpass filter function



lowPass8th3hz_resp <- function(x=chartDF$c_AutoEDA, 
                     GAIN = 4.17368469944216e+004, 
                     zplane1 = -0.03720010070485, 
                     zplane2 = 0.41721715698978, 
                     zplane3 = -2.07927380301188,
                     zplane4 = 6.02526039729765,
                     zplane5 = -11.12933103916398,
                     zplane6 = 13.45771989024155,
                     zplane7 = -10.44504106553466,
                     zplane8 = 4.78451489499581 ) {
  # x is the time series input data 
  # at 30 samples per second
  xv1 <- x[1]
  xv2 <- x[2]
  xv3 <- x[3]
  xv4 <- x[4]
  xv5 <- x[5]
  xv6 <- x[6]
  xv7 <- x[7]
  xv8 <- x[8]
  yv1 <- 1
  yv2 <- 1
  yv3 <- 1
  yv4 <- 1
  yv5 <- 1
  yv6 <- 1
  yv7 <- 1
  yv8 <- 1
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- xv4
    xv4 <- xv5
    xv5 <- xv6
    xv6 <- xv7
    xv7 <- xv8
    xv8 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- yv4
    yv4 <- yv5
    yv5 <- yv6
    yv6 <- yv7
    yv7 <- yv8
    yv8 <- (xv0 + xv8) + 
    	8 * (xv1 + xv7) + 
    	28 * (xv2 + xv6) +
			56 * (xv3 + xv5) +
			70 * xv4 + 
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2) +
      (zplane4 * yv3) + 
      (zplane5 * yv4) +
      (zplane6 * yv5) +
      (zplane7 * yv6) +
      (zplane8 * yv7)
    output[i] <- yv8
  }
  return(output)
} # end 8th order lowpass filter



