# Proposed Auto EDA Filter in 3 stages
# 1-11-2018
# Raymond Nelson
###
#
#
#########

# all filters are calculated using a data rate of 30cps


# first is a low pass filter that seems to improve the effectiveness of the high pass

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
# chartData$AutoEDA <- lowPass11hz(chartData$AutoEDA)


# 2nd stage is the high pass filter with a time constant of 10 seconds
# this is selected because Raskin, Honts and Kircher (2014) published a recommendation
# for the use of Manual (unfiltered EDA) due to potential changes to the data 
# and scores as a result of filtering
# Raskin Honts and Kircher (2014) also wrote that a high-pass filter 
# with a time-constant of 10 seconds would not cause damage to the signal of interest
# so the selection of this corner frequency insulates against criticism
# from Raskin and others around a recommendation to use the Auto EDA
# also, there is less potential that Auto EDA scores will differ
# from the scores of Manual EDA with this filter compared to the current Auto EDA
# a time constant of 10 seconds calculates to a corner frequency of .0159Hz

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
# chartData$AutoEDA <- highPass.0159(data=chartData$AutoEDA)


# 3rd and final stage is a low pass filter to smooth the data a little bit 
# in order to improve the correct identification of response onset and peak samples
# with automated feature extraction

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
# chartData$AutoEDA <- lowPass.443(data=chartData$AutoEDA)



# plot.ts(chartData$AutoEDA)

