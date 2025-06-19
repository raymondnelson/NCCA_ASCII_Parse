# extra signal processing helper functions
# 1-11-2017
# Raymond Nelson
#

####


### Lafayette auto EDA filters 2018

# lowPass12hz()

# highPass.0159()

# lowPass.443()



### pneumo smoothing filterr

# lowPass.886()


### PLE baselining filter

# highPass.338628()


### to remove the cardio dichrotic notch to calculate pulse rate

# lowPass1.17hz.2nd()


### LIC Legacy Auto EDA - prior to LXSoftware 9.9.6

# highPass.04()


### LIC auto EDA 2011

# lowPass.05()

# highPass.05()


### Limestone Auto EDA

# highPass.0533()

# lowPass.0467()


### LIC 2013 Auto EDA - abandoned

# highPass.01()

# lowPass.5()


### LIC 2013 Auto EDA - production 

# lowPass.2()

# highPass.03()



### to remove the PLE dichrotic notch to calculate the pulse rate

# lowPass2hz.2nd()




#########################   LIC 2018 Auto EDA  ######################



lowPass12hz <- function(x, GAIN = 1.32491969623291e+000, zplane = -0.50952544949443) {
  # lowPass12hz()
  # low pass filter to improve the diagnostic coeficient of the Auto EDA data
  # will add the AutoEDA column to the data frame
  # first order Butterworth filter
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end lowPass12hz function

# chartData$AutoEDA <- lowPass1hz(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



highPass.0159 <- function(x, GAIN = 1.00166504564511e+000, zplane = 0.99667544424686) {
  # highPass.0159()
  # high pass filter to auto center the Auto EDA
  # to improve the visual appearance and diagnostic coeficient of the EDA data
  # auto centering filter
  # first order Butterworth filter
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
} # end highPass.0159 function

# chartData$AutoEDA <- highPass.015(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



lowPass.443 <- function(x, GAIN = 2.254050840e+01, zplane = 0.9112708567) {
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
} # end lowpass.443 function



######## some helper functions for pneumo signal processing #############



# define the low pass .886 function
lowPass.886 <- function(x, GAIN = 1.174704212e+01, zplane = 0.8297443748) {
  # lowPass.886() 
  # to reduce noise and improve the diagnostic coeficient of the pneumo data
  # also called for the raw EDA data to execute the Lafayette legacy Auto EDA
  # first order Butterworth filter
  # 30 CPS data rate
  # corner frequency = .886Hz
  # to smooth peneumo and raw EDA data 
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
} # end lowpass.886() function



########## PLE signal processsing helper functions #################



highPass.338628 <- function(data=chartDF$c_PL, GAIN = 1.035475913e+00, zplane = 0.9314790191) {
  # highPass.338628()
  # high pass filter to constrain the PLE data to a stable baseline
  # first order Butterworth filter
  xv1 <- data[1]
  yv1 <- 0
  output <- rep(NA, length(data))
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 - xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end highPass.338628() function

# chartData$c_PL <- highPass.338628(data=stimSegmentDF$c_PL)
# plot.ts(highPass.338628(data=stimSegmentDF$c_PL))
# plot.ts(stimSegmentDF$c_PL)


highPass2nd.5Hz <- function(data=chartDF$c_PL, GAIN = 1.07686236756531e+000, zplane0 = -0.86234862603008, zplane1 = 1.85214648539594) {
  # highPass2nd.5Hz()
  # high pass filter to constrain the PLE data to a stable baseline
  # second order Butterworth filter
  xv1 <- data[1]
  xv2 <- data[1]
  yv1 <- 0
  yv2 <- 0
  output <- rep(NA, length(data))
  for (i in 1:length(data)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- data[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- (xv0 +
              xv2) -
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end highPass2nd.5Hz() function


########## for cardio signal processing and evaluation of cardio rate #########



lowPass1.17hz.2nd <- function(x, GAIN = 7.843388590e+01, zplane0 = -0.7071494959, zplane1 = 1.6561511302) {
  # 2nd order Butterworth low-pass filter
  # to remove the dichrotic notch 
  # and improve calculation of the the cardio rate
  # x input is a column vector from the time series 
  # output is a filtered time series vector
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
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end lowpass 2nd order



##############    LIC 2002 Auto EDA - Legacy Auto    ##################



highPass.04 <- function(x, GAIN = 1.004188815e+00, zplane = 0.9916573165) {
  # used in the Lafayette legacy EDA filter
  # Butterworth low-pass filter (1st order) with corner frequency = .04Hz
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

# chartData$AutoEDA <- highPass.04(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



##############   LIC 2010 Auto EDA   ######################



lowPass.05 <- function(x, GAIN = 1.919841864e+02, zplane = 0.9895824753) {
  # used in the lafayette 11.2 EDA
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end lowPass.05 function

# chartData$AutoEDA <- lowPass.05(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



highPass.05 <- function(x, GAIN = 1.005236036e+00, zplane = 0.9895824753) {
  # Lafayette 11.2 EDA
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

# chartData$AutoEDA <- highPass.05(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



##########   Limestone Auto EDA 2010  ##################



highPass.0533 <- function(x, GAIN = 1.005585112e+00, zplane = 0.9888918171) {
  # highPass filter to emuluate the Limestone EDA
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
} # end highPass.0533 function

# chartData$AutoEDA <- highPass.0533(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



lowPass.0467 <- function(x, GAIN = 2.056261550e+02, zplane = 0.9902736109) {
  # lowPass filter to emulate the Limestone EDA
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end lowPass.0467 function



###########    LIC 2013 Auto EDA - abandoned    ################



highPass.01 <- function(x, GAIN = 1.001047198e+00, zplane = 0.9979077951) {
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
} # end highPass.01 function



lowPass.5 <- function(x, GAIN = 2.008113669e+01, zplane = 0.9004040443) {
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end lowPass.5 function



########################.   DSP Filters for LIC 2013 Auto EDA  ##############



lowPass.2 <- function(x, GAIN = 4.873950141e+01, zplane = 0.9589655220) {
  # lowPass.2()
  # used for the 2013 LIC Auto EDA filter
  # low pass filter to improve the diagnostic coeficient of the Auto EDA data
  # will add the AutoEDA column to the data frame
  # first order Butterworth filter
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end lowPass.2 function

# chartData$AutoEDA <- lowPass.2(chartData$EDA1, GAIN = 4.873950141e+01, zplane = 0.9589655220)
# plot.ts(chartData$AutoEDA)



highPass.03 <- function(x, GAIN = 1.003141603e+00, zplane = 0.9937364715) {
  # highPass.03()
  # used for the 2013 LIC Auto EDA filter
  # high pass filter to auto center the Auto EDA
  # to improve the visual appearance and diagnostic coeficient of the EDA data
  # auto centering filter
  # first order Butterworth filter
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
} # end highPass.03 function

# chartData$AutoEDA <- highPass.03(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



lowPass2hz.2nd <- function(x, GAIN = 2.97868961737845e+001, zplane0 = -0.55326988968868, zplane1 = 1.41898265221812) {
  # 2nd order Butterworth low-pass filter
  # to remove the dichrotic notch 
  # and improve calculation of the the cardio rate
  # also used to smooth the pneumo data when calculating the pneumo rate
  # x input is a column vector from the time series 
  # output is a filtered time series vector
  ###
  # initialize the registers
  xv1 <- x[1]
  xv2 <- x[1]
  yv1 <- 0
  yv2 <- 0
  # inititalize the output
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



############ activity sensor signal processing ###################


lowPass.158 <- function(x, GAIN = 30.71204, zplane = 0.96744424495) {
	# lowPass.158() 
	# first order Butterworth filter
  # 30 CPS data rate
  # corner frequency at 0.158 Hz
  # equivalent to moving average smoothing filter of 2.8 seconds 
  # to reduce noise and improve the diagnostic coeficient of the pneumo data
  # also called for the raw EDA data to execute the Lafayette legacy Auto EDA
  # corner frequency = .886Hz
  # to smooth peneumo and raw EDA data 
  # to improve the diagnostic coefficient by reducing high frequency noise
  # x is a column vector from the time series 
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
} # end lowPass.158()


lowPass.1875 <- function(x, GAIN = 20.70543, zplane = 0.95170155901) {
	# first order filter to approximate a moving average of 1.875 seconds
  xv1 <- x[1]
  yv1 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
  }
  return(output)
}



lowPass8th.1181hz <- function(x=chartDF$c_AutoEDA, 
                               GAIN = 2.20023181954140e+015, 
                               zplane1 = -8.8091809287e-01, 
                               zplane2 = 7.1590329019e+00, 
                               zplane3 = -2.5454615962e+01,
                               zplane4 = 5.1719710786e+01,
                               zplane5 = -6.5681208126e+01,
                               zplane6 = 5.3385305702e+01,
                               zplane7 = -2.7120521121e+01,
                               zplane8 = 7.8732139119e+00 ) {
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
    xv8 <- x[i] / GAIN
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
  # end 8th order lowpass filter 0.1181Hz
} 






