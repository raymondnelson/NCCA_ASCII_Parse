# extra signal processing helper functions
# 1-11-2017
# Raymond Nelson
#

####


#################################################################
################## 2nd order filters ###################



lowPass.15hz.2nd <- function(x, GAIN = 4.143204922e+03, zplane0 = -0.9565436765, zplane1 = 1.9555782403) {
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
    yv2 <- (xv0 + xv2) +
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end lowpass.15.2nd()



lowPass.2hz.2nd <- function(x, GAIN = 2.347573845e+03, zplane0 = -0.9424820220, zplane1 = 1.9407781353) {
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
    yv2 <- (xv0 + xv2) +
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end lowpass.2.2nd()



lowPass.25hz.2nd <- function(x, GAIN = 1.51336506054459e+003, zplane0 = -0.92862708612481, zplane1 = 1.92598396973189) {
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
} # end lowpass.25hz.2nd()



lowPass.443hz.2nd <- function(x, GAIN = 4.95456381134899e+002, zplane0 = -0.87703103532041, zplane1 = 1.86895767073716) {
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
    yv2 <- (xv0 + xv2) +
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end lowpass.443.2nd()



lowPass.886hz.2nd <- function(x, GAIN = 1.316975270e+02, zplane0 = -0.7691890947, zplane1 = 1.7388164667) {
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
} # end lowpass.886.2nd()



lowPass1hz.2nd <- function(x, GAIN = 1.04978474216297e+002, zplane0 =  -0.74365519504887, zplane1 = 1.70555214554408) {
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



highPass.0159hz.2nd <- function(x, GAIN = 1.002357502e+00, zplane0 = -0.9953016162, zplane1 = 1.9952905528) {
  # 2nd order Butterworth high-pass filter
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
    yv2 <- (xv0 +
      xv2) -
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end highPass.0159hz.2nd()



highPass.03hz.2nd <- function(x, GAIN = 1.004452767e+00, zplane0 = -0.9911535959, zplane1 = 1.9911142922) {
  # 2nd order Butterworth high-pass filter
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
    yv2 <- (xv0 +
      xv2) -
      (2 * xv1) +
      (zplane0 * yv0) +
      (zplane1 * yv1)
    output[i] <- yv2
  }
  return(output)
} # end highPass.03hz.2nd()




################## 3rd order filters ###################



lowPass.3hz.3rd <- function(x, GAIN = 3.430944333e+04, zplane0 = 0.8818931306, zplane1 = -2.7564831952, zplane2 = 2.8743568927) {
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  yv1 <- 0
  yv2 <- 0
  yv3 <- 0
  output <- rep(NA, length(x))
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- (xv0 + xv3) +
      (2 * (xv1+ xv2)) +
      (zplane0 * yv0) +
      (zplane1 * yv1) +
      (zplane2 * yv2)
    output[i] <- yv3
  }
  return(output)
} # end lowPass.3hz.3rd()



#################   low-pass filters  #################



lowPass.015 <- function(x, GAIN = 6.376192488e+02, zplane = 0.9968633318) {
  # lowPass.015()
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
} # end lowPass.015 function

# chartData$AutoEDA <- lowPass.015(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.025 <- function(x, GAIN = 3.829709908e+02, zplane = 0.9947776723) {
  # lowPass.025()
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
} # end lowPass.025 function

# chartData$AutoEDA <- lowPass.025(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.03 <- function(x, GAIN = 3.193088390e+02, zplane = 0.9937364715) {
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
} # end lowPass.03 function



lowPass.04 <- function(x, GAIN = 2.397310184e+02, zplane = 0.9916573166) {
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
} # end lowPass.04 function

# chartData$AutoEDA <- lowPass.04(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.06 <- function(x, GAIN = 1.601528487e+02, zplane = 0.9875119299) {
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
} # end lowPass.06 function

# chartData$AutoEDA <- lowPass.06(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.07 <- function(x, GAIN = 1.374160792e+02, zplane = 0.9854456625) {
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
} # end lowPass.07 function

# chartData$AutoEDA <- lowPass.07(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.1 <- function(x, GAIN = 9.648947517e+01, zplane = 0.9792723507) {
  # lowPass.1()
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
} # end lowPass.1 function

# chartData$AutoEDA <- lowPass.1(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.3 <- function(x, GAIN = 3.282051595e+01, zplane = 0.9390625058) {
  # lowPass.2()
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
} # end lowPass.3 function

# chartData$AutoEDA <- lowPass.3(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.4 <- function(x, GAIN = 2.485927720e+01, zplane = 0.9195471379) {
  # lowPass.4()
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
} # end lowPass.4 function

# chartData$AutoEDA <- lowPass.4(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.6 <- function(x, GAIN = 1.689454484e+01, zplane = 0.8816185924) {
  # lowPass.6()
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
} # end lowPass.6 function

# chartData$AutoEDA <- lowPass.6(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.7 <- function(x, GAIN = 1.461740890e+01, zplane = 0.8631768452) {
  # lowPass.7()
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
} # end lowPass.7 function

# chartData$AutoEDA <- lowPass.7(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.8 <- function(x, GAIN = 1.290868239e+01, zplane = 0.8450655195) {
  # lowPass.8()
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
} # end lowPass.8 function

# chartData$AutoEDA <- lowPass.8(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass.9 <- function(x, GAIN = 1.157889499e+01, zplane = 0.8272719460) {
  # lowPass.9()
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
} # end lowPass.9 function

# chartData$AutoEDA <- lowPass.9(chartData$EDA1)
# plot.ts(chartData$AutoEDA)



lowPass1hz <- function(x, GAIN = 1.051436445e+01, zplane = 0.8097840332) {
  # lowPass1hz()
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
} # end lowPass1hz function

# chartData$AutoEDA <- lowPass1hz(chartData$EDA1)
# plot.ts(chartData$AutoEDA)





#################  high-pass filters   #####################




highPass.01 <- function(x, GAIN = 1.001047198e+00, zplane = 0.9979077951) {
  # highPass.01()
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
} # end highPass.01 function

# chartData$AutoEDA <- highPass.0125(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)





highPass.0125 <- function(x, GAIN = 1.00130899768664e+000, zplane = 0.99738542709660) {
  # highPass.0125()
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
} # end highPass.0125 function

# chartData$AutoEDA <- highPass.0125(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.015 <- function(x, GAIN = 1.001570798e+00, zplane = 0.9968633318) {
  # highPass.015()
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
} # end highPass.015 function

# chartData$AutoEDA <- highPass.015(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.02 <- function(x, GAIN = 1.002094398e+00, zplane = 0.9958199583) {
  # highPass.02()
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
} # end highPass.02 function

# chartData$AutoEDA <- highPass.02(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.025 <- function(x, GAIN = 1.002618000e+00, zplane = 0.9947776723) {
  # highPass.025()
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
} # end highPass.025 function

# chartData$AutoEDA <- highPass.025(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.03 <- function(x, GAIN = 1.003141603e+00, zplane = 0.9937364715) {
  # highPass.0333()
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



highPass.0333 <- function(x, GAIN = 1.00348718198060e+000, zplane = 0.99304987239853) {
  # highPass.0333()
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
} # end highPass.0333 function

# chartData$AutoEDA <- highPass.0333(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.06 <- function(x, GAIN = 1.006283268e+00, zplane = 0.9875119299) {
  # highPass.06()
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
} # end highPass.06 function

# chartData$AutoEDA <- highPass.06(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.07 <- function(x, GAIN = 1.007330514e+00, zplane = 0.9854456625) {
  # highPass.07()
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
} # end highPass.07 function

# chartData$AutoEDA <- highPass.07(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.08 <- function(x, GAIN = 1.008377776e+00, zplane = 0.9833836552) {
  # highPass.08()
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
} # end highPass.08 function

# chartData$AutoEDA <- highPass.08(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.09 <- function(x, GAIN = 1.009425057e+00, zplane = 0.9813258905) {
  # highPass.09()
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
} # end highPass.09 function

# chartData$AutoEDA <- highPass.09(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.1 <- function(x, GAIN = 1.010472358e+00, zplane = 0.9792723507) {
  # highPass.1()
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
} # end highPass.1 function

# chartData$AutoEDA <- highPass.1(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.2 <- function(x, GAIN = 1.020947014e+00, zplane = 0.9589655220) {
  # highPass.2()
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
} # end highPass.2 function

# chartData$AutoEDA <- highPass.2(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



highPass.3 <- function(x, GAIN = 1.031426266e+00, zplane = 0.9390625058) {
  # highPass.3()
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
} # end highPass.3 function

# chartData$AutoEDA <- highPass.3(data=chartData$AutoEDA)
# plot.ts(chartData$AutoEDA)



################## 8th order filters   ###############################


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



####



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



####



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



####



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



####



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



####



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



####



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
} # end 8th order lowpass filter 3Hz



####


lowPass8th_.1181hz_resp <- function(x=chartDF$c_AutoEDA, 
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




lowPass8th.118Hz <- function(x, 
                               GAIN = 2.044185991e+15,  
                               zplane1 = 7.87321391, 
                               zplane2 = -27.12052112, 
                               zplane3 = 53.38530570,
                               zplane4 = -65.68120813,
                               zplane5 = 51.71971079,
                               zplane6 = -25.45461596,
                               zplane7 = 7.15903290,
                               zplane8 = -0.88091809) {
  # x is the time series input data 
  # at 30 samples per second                             
  # Initialize input and output memory
  xv <- rep(0, 9)
  yv <- rep(0, 9)
  output <- rep(NA, length(x))
  for (i in seq_along(x)) {
    # Shift input values
    xv[1:8] <- xv[2:9]
    xv[9] <- x[i] * GAIN
    # Shift output values
    yv[1:8] <- yv[2:9]
    # Calculate output using the filter equation
    yv[9] <- (
      5.13893392e-16 * (xv[1] + xv[9]) +
      4.11114714e-15 * (xv[2] + xv[8]) +
      1.43890150e-14 * (xv[3] + xv[7]) +
      2.87780300e-14 * (xv[4] + xv[6]) +
      3.59725374e-14 * xv[5] +
      zplane1 * yv[8] +
      zplane2 * yv[7] +
      zplane3 * yv[6] +
      zplane4 * yv[5] +
      zplane5 * yv[4] +
      zplane6 * yv[3] +
      zplane7 * yv[2] +
      zplane8 * yv[1]
    )
    output[i] <- yv[9]
  }
  return(output)
} # end lowPass8th_0()


####



highPass8th.25hz_resp <- function(x=chartDF$c_AutoEDA, 
                                  GAIN = 1.14363734387739e+000, 
                                  zplane1 = -0.76458072201601, 
                                  zplane2 = 6.32184746906582, 
                                  zplane3 = -22.87230730734647,
                                  zplane4 = 47.29399167201584,
                                  zplane5 = -61.12951066003450,
                                  zplane6 = 50.57613792270622,
                                  zplane7 = -26.15719377222773,
                                  zplane8 = 7.73161539778734 ) {
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



