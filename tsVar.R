# function to compute the windowed variance of the time series pneumo data

tsVar <- function(x, y=1) {
  # function to compute the windowed variance of the time series pneumo data
  # x is a vector of time series input data
  # y is the window length in seconds
  z <- rep(0, times=length(x))
  for (i in 1:(length(z)-29)) {z[i] <- var(diff(x[i:(i+y*30-1)]))}
  return(z)  
}

tsMean <- function(x, y=1) {
  # function to compute the windowed mean of the time series pneumo data
  # x is a vector of time series input data
  # y is the window length in seconds
  z <- rep(0, times=length(x))
  for (i in 1:(length(z)-29)) {z[i] <- mean(diff(x[i:(i+y*30-1)]))}
  return(z)  
}

