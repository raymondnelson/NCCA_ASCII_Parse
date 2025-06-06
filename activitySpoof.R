# spoof the activity sensor data for an axciton polygraph

# examDF <- DCLH6S7_Data

# function to smooth the time series data to the midpoint
# MASmooth <- function(x=myData, y=round(.25*cps,0), times=5) {
#   # function to calculate a smoothed average of the time series data
#   # called by dataParse function in the NCCAASCIIParseHelperFunctions.R script
#   # x input is a time series vector
#   # y input is the number of offset samples
#   # times is the number of times to smooth the 
#   ###
#   # make the output vector 
#   # beginning and end of the output vector are the mean of 2 * the buffer at begin and end of x
#   xOut <- x
#   # loop over the number of times
#   for (j in 1:times) {
#     # for loop to compute the moving average
#     # buffer will be double the offset value + 1
#     input_buffer <- x[1:(2*y+1)]
#     # starts at sample y + 1
#     for (i in (y+1):(length(x)-y)) { 
#       # replace the middle value of the buffer with the mean
#       xOut[i] <- mean(input_buffer) 
#       # increment the input buffer
#       input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+y+1])
#     } 
#     # replace the input vector
#     x <- xOut
#   } # end loop over times
#   return(xOut)
# } # end MASmooth function()
# 
# myDF <- thisChartDF[,c(1:3,11:15)]

activitySpoofFn <- function(myDF=chartDF, 
                            y=c(3 ,3 ,.25), 
                            samples=15, 
                            times=1, 
                            weightCoefs=weightCoefsAS) {
  # function to spoof missing activity sensor data
  # called by the dataParse function that is called by NCCAASCIIParse function
  # controlled by the activitySpoof=TRUE switch in the workFlow.R script
  #
  # myDF is a chart data frame
  # y is a vector of weights, not used
  # samples and times is used by the MASmooth function
  # weightCoefs is a vector of weights so that all charts are similar
  #
  ####
  
  # assign("myDF", myDF, envir=.GlobalEnv)
  # stop()
  
  # myDF <- apply(x[,4:6], 1, weighted.mean, weights=y)
  # cardioMA <- MASmooth(x=myDF$Cardio1, y=samples, times=times)
  # plot.ts(cardioMA[1000:3000])
  
  ## iniitialize the activity data by averaging the upper and lower pneumos
  
  # average the upper and lower pneumos for baseline activity
  actData <- apply(myDF[,4:5], 1, mean)
  
  # set the range
  actDataRange <- max(actData, na.rm=TRUE) - min(actData, na.rm=TRUE)
  rangeCoef <- ifelse(actDataRange > 75000,
  										50000 / actDataRange,
											1 )
  rangeCoef <- ifelse(actDataRange < 30000,
                      50000 / actDataRange,
                      1 )
	actData <- actData * rangeCoef
  
  # add the activity data to the data frame
  myDF$Move1 <- actData
  # smooth it a little
  # myDF$Move1 <- MASmooth(x=myDF$Move1, y=10, times=1)
  
  # remove the phasic activity to constrain data to a baseline
  Move1MA <- MASmooth(myDF$Move1, y=150, times=2)
  myDF$Move1 <- myDF$Move1 - Move1MA
  
  # check the cardio range and set a scaling value if necessary
  cardioRange <- max(myDF$Cardio1, na.rm=TRUE) - min(myDF$Cardio1, na.rm=TRUE)
  cardioCoef <- ifelse(cardioRange > 75000,
                       50000 / cardioRange,
                       1)
  cardioCoef <- ifelse(cardioRange < 30000,
                       50000 / cardioRange,
                       1)
  
  # add some cardio pulse noise
  # weightCoefs <- c(sample(c(.1, .2, .3, .3, .2), 1, prob=c(.1, .2, .3, .2, .1)), 1)
  myDF$Move1 <- apply(cbind(myDF$Cardio1*cardioCoef, myDF$Move1), 1, weighted.mean, w=weightCoefs)
  # remove phasic activity to constrain the spoofed activity data to a baseline
  Move1MA <- MASmooth(x=myDF$Move1, y=150, times=1)
  myDF$Move1 <- myDF$Move1 - (Move1MA)
  
  # smoooth it once more
  # myDF$Move1 <- MASmooth(x=myDF$Move1, y=2, times=1)
  
  # relocate the data
  newMin <- min(myDF$Move1[100:(nrow(myDF)-100)], na.rm=TRUE)
  myDF$Move1 <- myDF$Move1 - (newMin + 1)
  
  # fix any NA values
  myDF$Move1[is.na(myDF$Move1)] <- min(myDF$Move1[100:(nrow(myDF)-100)], na.rm=TRUE)
  
  # begin and end segments
  myDF$Move1[1:150] <- myDF$Move1[151:300]
  myDF$Move1[(nrow(myDF)-149):nrow(myDF)] <- myDF$Move1[(nrow(myDF)-300):(nrow(myDF)-151)]
  
  # assign("myDF", myDF, envir=.GlobalEnv)
  return(myDF)
}


# check the range
# diff(range(myDF$UPneumo))
# diff(range(myDF$LPneumo))        
# diff(range(thisChartDF$EDA1))
# diff(range(thisChartDF$Cardio1))
# diff(range(thisChartDF$Move1))
# plot.ts(myDF$Move1)
# plot.ts(Move1MA)


# diff(range(thisChartDF$Move1[2000:10000]))
# min(thisChartDF$Move1[100:(nrow(thisChartDF)-100)])


# cardioMid <- MASmooth(x=examDF$Cardio1, y=15, times = 4)
# # plot.ts(cardioMid[1000:3000], ylim=c(-3,10))
# plot.ts(examDF$Cardio1[1000:3000])
# plot.ts(cardioMid[1000:3000])

# cardioMA <- examDF$Cardio1 - MASmooth(x=examDF$Cardio1, y=15, times = 1)
# plot.ts(cardioMA[1000:3000]) 

# cardioMod <- examDF$Cardio1 - cardioMid
# plot.ts(cardioMod[1000:3000])

# cardioMod2 <- examDF$Cardio1 - cardioMA
# plot.ts(cardioMod2[1000:3000])

# actDF <- examDF[,11:14]

# weightVals <- c(1,1,2,1)
  
# actDAT <- apply(actDF[,1:4], 1, weighted.mean, na.rm=TRUE, w=weightVals)


  # weighted.mean(c(examDF$UPneumo, examDF$LPneumo, examDF$EDA1, examDF$Cardio1),
  #      w=weightVals)

  
# actDF$Move1 <- actDAT

# plot.ts(actDAT[1000:3000])
# plot.ts(actDF$UPneumo[1000:3000])
# plot.ts(actDF$LPneumo[1000:3000]) 
# plot.ts(actDF$EDA1[1000:3000])
# plot.ts(actDF$Cardio1[1000:3000])




# actMod <- actDAT - cardioMod
# plot.ts(actMod[1000:3000])
