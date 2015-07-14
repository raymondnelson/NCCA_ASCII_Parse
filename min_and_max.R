# functions to get the min, max and min-max values from cardio data


plot.ts(myCardioData[,7][1:1000], ylim=c(-3,10))

# uses 26 samples for 30cps sample rate
# .87hz is a 
# i <- 14
maxPeak <- function(x, y) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # buffer will be double the offset value
  # xOut <- numeric(length=length(x))
  xOut <- c(1, rep(NA, times=(length(x)-2)), length(x))
  input_buffer <- x[1:(2*y)]
  for (i in 2:(length(x)-(2*y))) {
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    ifelse(input_buffer[y]==max(input_buffer),
    { xOut[(i+.5*y):(i+y-2)] <- NA
      xOut[i+y-1] <- c(i+y-1) 
    }, 
    next()
    )
  } # end for loop
return(na.omit(xOut))
}  # end maxPeak
maxOut <- maxPeak(x=myCardioData[,7], y=7)
maxVal <- myCardioData[na.omit(maxOut),7]
plot.ts(maxVal[maxOut<=1000], ylim=c(-3,10))




systolicOut <- maxPeak(x=myCardioData[,7],y=7)




minPeak <- function(x=myCardioData[,7], y=10) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # buffer will be double the offset value
  # xOut <- numeric(length=length(x))
  xOut <- c(1, rep(NA, times=(length(x)-2)), length(x))
  input_buffer <- x[1:(2*y)]
  for (i in 2:(length(x)-(2*y))) {
    input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
    ifelse(input_buffer[y]==min(input_buffer),
    { xOut[(i+.5*y):(i+y-2)] <- NA
      xOut[i+y-1] <- c(i+y-1) 
    }, 
    next()
    )
  } # end for loop
return(na.omit(xOut))
} # end minPeak
peakOut <- minPeak(x=myCardioData[,7], y=7)
peakVal <- myCardioData[na.omit(peakOut),7]
plot.ts(peakVal[peakOut<=1000], ylim=c(-3,10))

ts.plot(myCardioData[,7][1:1000], ylim=c(-3,10))








minMaxPeak <- function(x, y) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # buffer will be double the offset value
  # xOut <- numeric(length=length(x))
  xOut <- c(1, rep(NA, times=(length(x)-2)), length(x))
  input_buffer <- x[1:(2*y)]
  for (i in (y+1):(length(x)-(2*y+1))) {
    input_buffer <- c(input_buffer[2:(2*y+1)], x[i])
    ifelse(input_buffer[y]==min(input_buffer),
    { xOut[(i+.5*y):(i+y-2)] <- NA
      xOut[i+y-1] <- c(i+y-1) 
    }, 
    ifelse(input_buffer[(y+1)]==max(input_buffer),    
    { xOut[(i+.5*y):(i+y-2)] <- NA
      xOut[i+y-1] <- c(i+y-1) 
    },
    next()
    )
    )
  } # end for loop
  return(na.omit(xOut))
} # end minMaxPeak
minMaxOut <- minMaxPeak(x=myCardioData[,7], y=8)

plot.ts(x[minMaxOut[minMaxOut<=1000]])

