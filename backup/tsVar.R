tukeyFn <- function(inData, y=1.5) {
  # function to compute the outlier row numbers of a time series input
  # x is a time series input
  # y is the number of IQRs to set the upper and lower fence
  med <- quantile(inData, c(.5))
  iqr <- quantile(inData, c(.75, .25))
  range <- abs(diff(iqr))
  tkUp <- iqr[1] + y*range
  tkLow <- iqr[2] - y*range
  z <- which(inData<=tkLow)
  z1 <- which(inData>=tkUp)
  z <- sort(c(z, z1))
  return(z)
}



# 1
ts.plot(x[1:1000])

tukeyFn(x[1:1000], 1.5)



y <- 1
e <- 1



# 2
tsMean <- function(x, y=1, e=1) {
  # function to compute the windowed mean of the time series data
  # x is a vector of time series input data
  # y is the window length in seconds
  z <- x
  # z <- rep(0, times=length(x))
  for (i in 1:(length(z)-29)) { z[i] <- mean(x[i:(i+y*30-1)]) }
  #z[(length(z)-29):length(z)] <- z[(length(z)-29)]
  return(z^e)
}

ts.plot(tsMean(x, y, e)[1:1000])

tukeyFn(tsMean(x, y, 2)[1:1000], 1.5)



# 3
tsVar <- function(x, y=1, e=1) {
  # function to compute the windowed variance of the time series data
  # x is a vector of time series input data
  # y is the window length in seconds
  z <- rep(0, times=length(x))
  for (i in 1:(length(z)-29)) { z[i] <- var(diff(x[i:(i+y*30-1)])) }
  z[(length(z)-29):length(z)] <- z[(length(z)-29)]
  return(z^e)  
}

ts.plot(tsVar(x, 1, 1)[1:1000]) # this one 
# ts.plot(tsVar(tsMean(x, 1),1)[1:1000])

tukeyFn(inData=tsVar(x, 1, 1)[1:1000], y=1.5)



# 4
tsDiff <- function(x, e=1) {
  # function to compute the difference of the time series data
  # x is a vector of time series data
  z <- c(diff(x), 0)^e
  return(z)
}

ts.plot(tsDiff(x, 1)[1:1000])
# ts.plot(tsDiff(tsMean(x, 1))[1:1000])

tukeyFn(inData=tsDiff(x, 1)[1:1000], y=1.5)



# 5
tsMeanDiff <- function(x, y=1, e=1) {
  # function to compute the windowed mean of difference of the time 
  # x is a vector of time series data
  # y is the number of seconds in the mean buffer
  # e is an exponent
  z <- c(diff(x), 0)
  for (i in 1:(length(z)-29)) { z[i] <- mean(diff(z[i:(i+y*30-1)])) }
  z[(length(z)-29):length(z)] <- z[(length(z)-29)]
  return(z^e) 
}

ts.plot(tsMeanDiff(x, 1, 2)[1:1000])
# ts.plot(tsMeanDiff(tsMean(x, 1),1)[1:1000])

tukeyFn(inData=tsMeanDiff(x, 1, 2)[1:1000], y=3)



# 6
tsVarDiff <- function(x, y=1, e=1) {
  # function to compute the windowed difference of the time 
  # x is a vector of time series data
  z <- c(diff(x), 0)
  for (i in 1:(length(z)-29)) { z[i] <- var(diff(z[i:(i+y*30-1)])) }
  z[(length(z)-29):length(z)] <- z[(length(z)-29)]
  return(z^e) 
}

ts.plot(tsVarDiff(x,1, 2)[1:1000])
# ts.plot(tsVarDiff(tsMean(x, 1), 1)[1:1000]) # this one 

tukeyFn(inData=tsVarDiff(x, 1, 2)[1:1000], y=1.5)



# 7
tsAbsDiff <- function(x, e) {
  # function to compute the difference of the time series data
  # x is a vector of time series data
  # e is an exponent
  z <- c(abs(diff(x)^e),0)
  return(z^e)
}

ts.plot(tsAbsDiff(x, 1)[1:1000]) # this one with the squared diff
# ts.plot(tsAbsDiff(tsMean(x, 1), 3)[1:1000])

tukeyFn(inData=tsAbsDiff(x, 1)[1:1000], y=1.5)



# 8
tsMeanAbsDiff <- function(x, y=1, e=1) {
  # function to compute the windowed mean of difference of the time 
  # x is a vector of time series data
  # y is the number of seconds to average
  # z is an exponent
  z <- abs(c(diff(x), 0)^e)
  for (i in 1:(length(z)-29)) { z[i] <- mean(diff(z[i:(i+y*30-1)])) }
  z[(length(z)-29):length(z)] <- z[(length(z)-29)]
  return(z^e) 
}

ts.plot(tsMeanAbsDiff(x, 1, 2)[1:1000])
# ts.plot(tsMeanAbsDiff(tsMean(x, 1), 1))

tukeyFn(inData=tsMeanAbsDiff(x, 1, 1)[1:1000], y=1.5)



#9 
tsVarAbsDiff <- function(x, y=1, e=1) {
  # function to compute the windowed difference of the time 
  # x is a vector of time series data
  z <- abs(c(diff(x), 0))^e
  for (i in 1:(length(z)-29)) { z[i] <- var(diff(z[i:(i+y*30-1)])) }
  z[(length(z)-29):length(z)] <- z[(length(z)-29)]
  return(z) 
}

ts.plot(tsVarAbsDiff(x, 1, 4)[1:1000])

tukeyFn(inData=tsVarAbsDiff(x, 1, 4)[1:1000], y=1.5)



# 10
tsMedian <- function(x, y=1, e=3) {
  # function to compute the windowed median of the time series data
  # x is a vector of time series input data
  # y is the window length in seconds
  z <- rep(0, times=length(x))
  for (i in 1:(length(z)-29)) { z[i] <- median(diff(x[i:(i+y*30-1)])) }
  z[(length(z)-29):length(z)] <- z[(length(z)-29)]
  return(z^e)  
}

ts.plot(tsMedian(x, 1, 3)[1:1000])
# ts.plot(tsMedian(tsMedian(x, 1),1))

tukeyFn(inData=tsMedian(x, 1, 3)[1:1000], y=1.5)



# 11
tsMAD <- function(x, y=1, e=1) {
  # function to compute the median absolute deviation for the time series data
  # x is a vector of time series data
  # y is the window length in seconds
  z <- rep(0, times=length(x))
  for (i in 1:(length(z)-29)) { 
    z[i] <- median(abs(x[i:(i+y*30-1)] - median(x[i:(i+y*30-1)])))
    z[(length(z)-29):length(z)] <- z[(length(z)-29)]
    }
  return(z^e)
}

ts.plot(tsMAD(x, 1, 4)[1:1000])
# ts.plot(tsMAD(tsMedian(x,1), 1))

tukeyFn(inData=tsMAD(x, 1, 1)[1:1000], y=1.5)




