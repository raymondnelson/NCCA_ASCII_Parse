# interpolate the missing values for min and max 

# diastolicOut
# diastolicVal
plot.ts(myCardioData[1:1000,7], ylim=c(-3,10))



peakOut <- minPeak(x=myCardioData[,7], y=8)
peakVal <- myCardioData[peakOut,7]

interpolatePeaks <- function(x=peakOut, y=peakVal) {
  # interpolate between peak segments of time series input data
  # x is a vector of peak row numbers in the data
  # y is a vector of peak values in the data
  peakValDiff <- diff(y)
  peakOutDiff <- diff(x)
  peakValDiff <- peakValDiff / peakOutDiff
  peakFill <- rep(peakValDiff, times=peakOutDiff)
  peakFill <- cumsum(peakFill)

 return(peakFill)
  
}

plot.ts(myCardioData[1:1000,7], ylim=c(-3,10))

peakOut <- minPeak(x=myCardioData[,7], y=10)
peakVal <- myCardioData[peakOut,7]
peakFill <- interpolatePeaks(x=peakOut, y=peakVal)

plot.ts(peakFill[1:1000], ylim=c(-3,10))

myCardioData2 <- cbind(myCardioData[7], c(0, peakFill))

ts.plot(myCardioData2[1:1000,], ylim=c(-3,10))

#2nd itertion
peakOut <- minPeak(peakFill, y=1)
peakVal <- peakFill[peakOut]
peakFill <- interpolatePeaks(x=peakOut, y=peakVal)

plot.ts(peakFill[1:1000], ylim=c(-3,10))


ts.plot(myCardioData[1:1000,7], peakFill[1:1000])


diastolicNA <- which(is.na(diastolicOut))




diastolicOut <- na.omit(diastolicOut)

diastolicOutDiff <- c(0, diff(na.omit(diastolicOut)))
diastolicValDiff <- c(0, diff(diastolicVal))

diastolicInterp <- diastolicValDiff / diastolicOutDiff

diastolicOutDiff[which(diastolicOutDiff>1)]

tempVector <- rep(0, times=length(diastolicOut))

interpolationVal <- interpolationInterval / diastolicOutDiff
diastolicFill <- numeric(length(myCardioData[,7]))
# loop to fill the missing values in the min peak vector
# for (i in 2:(length(diastolicOut))) {
#   ifelse(diastolicOut[i] - diastolicOut[i-1] > 1,
#          diastolicFill[diastolicOut[i]:(diastolicOut[i]+diastolicOutDiff[i]-2)] <- 
#            rep(diastolicValDiff[i], times=(diastolicOutDiff[i]-1)),
#          
#          diastolicFill[diastolicOut[i]] <- diastolicVal[i]
#   )
# }
for (i in 2:(length(diastolicOut))) {
  #   ifelse(diastolicOut[i] - diastolicOut[i-1] > 1,
  
  
diastolicFill <- rep(diastolicValDiff, times1


}
plot.ts(diastolicFill[1:3000])

diff / rowDiff = valDiff
rep valDiff by rowdiff then cumsum 



