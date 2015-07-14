# functions for cardio signal processing 

# minPeak()
# maxPeak()
# minMaxPeak()
# cardioSmooth1()

########################



library(stringr)



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



# uniqueExams <- uniqueExams[1:2]



###############################

# First define the helper functions

#####

# get the min (diastolic) peak values

minPeak <- function(x=chartData$Cardio1, y=10) {
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
} # end minPeak function

#####

# interpolate between the min peak values

interpolatePeaks <- function(x, y) {
  # interpolate between peak segments of time series input data
  # x is a vector of peak row numbers in the data
  # y is a vector of peak values in the data
  peakValDiff <- diff(y)
  peakOutDiff <- diff(x)
  peakDivisor <- peakOutDiff
  peakDivisor[which(peakOutDiff>1)] <- (peakOutDiff[which(peakOutDiff>1)])
  peakValDiff <- peakValDiff / peakDivisor
  peakFill <- rep(peakValDiff, times=peakOutDiff)
  peakFill <- cumsum(peakFill)
  peakFill[minOut] <- minVal
  return(peakFill)
} # end interpolatePeaks
# diastolicInterp <- c(interpolatePeaks(x=minOut, y=minVal),0 )
# plot.ts(diastolicInterp, ylim=c(-3,10))

#####

# get the max (systolic) peak values

maxPeak <- function(x=chartData, y) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # buffer will be double the offset value
  # xOut <- numeric(length=length(x))
  xOut <- c(1, rep(NA, times=(length(x)-2)), length(x))
  input_buffer <- x[1:(2*y)]
  for (i in 2:(length(x)-(2*y))) {
    # input buffer changed to [2:length(inpub_buffer)] not [2:(2*y)]
    input_buffer <- c(input_buffer[2:length(input_buffer)], x[i+(2*y)])
    ifelse(input_buffer[y]==max(input_buffer),
{ xOut[(i+.5*y):(i+y-2)] <- NA
  xOut[i+y-1] <- c(i+y-1) 
}, 
next()
    )
  } # end for loop
return(na.omit(xOut))
}  # end maxPeak

#####

# get the min (diastolic) and max (systolic) values

minMaxPeak <- function(x, y) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # buffer will be double the offset value
  # xOut <- numeric(length=length(x))

  xOut <- c(1, rep(NA, times=(length(x)-2)), length(x))
  input_buffer <- x[1:(2*y)]
  for (i in (y+1):(length(x)-(2*y+1))) {
    # input buffer maybe should be [2:length(input_buffer)] not [2:2*y+1]
    input_buffer <- c(input_buffer[2:length(input_buffer)], x[i])
    ifelse(input_buffer[y]==min(input_buffer),
      { xOut[(i+.5*y):(i+y-2)] <- NA
        xOut[i+y-1] <- c(i+y-1) 
      }, 
      ifelse(input_buffer[(y)]==max(input_buffer),    
      { xOut[(i+.5*y):(i+y-2)] <- NA
        xOut[i+y-1] <- c(i+y-1) 
      },
      next()
    )
    )
  } # end for loop
  xOut <- na.omit(xOut)
  # xOut <- c(xOut, rep(xOut[length[xOut]], times=(x-length(xOut))))

  return(xOut)

} # end minMaxPeak function

##### 

# smooth the cardio data using a moving average

cardioSmooth1 <- function(x=chartData$Cardio1, y=15, times=3) {
  # function to get the diastolic peaks from the cardio data
  # x input is a time series vector
  # y input is the number of offset samples 
  # times is the number of times to recursively smooth the data
  # buffer will be double the offset value
  xOut <- c(rep(x[1], times=y), numeric(length=(length(x)-y)))
  input_buffer <- c(x[1:y], rep(x[y+1], times=y)) 
  # loop over the number of times
  for (j in 1:times) {
    # for loop to compute the moving average
    for (i in (y+1):(length(x)-(2*y))) {
      xOut[i-y] <- mean(input_buffer)
      input_buffer <- c(input_buffer[2:(2*y)], x[i])
    } # end for loop for moving average
    # re-initialize the input
    x <- xOut
    input_buffer <- c(x[1:y], rep(x[y+1], times=y))
  } # end loop over times
  return(na.omit(xOut))
} # end cardioSmooth function

# smoothedCardio <- cardioSmooth1(x=cardioSmooth1(x=cardioSmooth1()))
# smoothedCardio <- cardioSmooth1(x=chartData$Cardio1, y=15, times = 4)
# plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))

#####

# function to compute the average of min and max for a moving averge

minMaxMean <- function(x=chartData$Cardio1, y=25) {
  # x is the time series cardio data
  # y is 1/2 the number of samples in the buffer
  #
  xOut <- c(rep(mean(x[1:y]), time=y), 
            numeric(length(x)-2*y), 
            rep(mean(x[length(x)-2*y+1:length(x)]))
            )
  input_buffer <- x(1:2*y)

  for (i in (y+1):(length(x)-y)) {
    xOut[i] <- mean(min(input_buffer), max(input_buffer))
    input_buffer <- c(input_buffer1[2:(2*y)], x[i])
  }
  xOut <- na.omit(xOut)
  xOut <- c(xOut, rep(xOut[length(xOut), times=(length(x)-length(xOut))]))
  return(xOut)
  
} # end minMaxMean function





#########################



# a function to process the the cardio data for a list of input exams in the cwd

cardioSigProc <- function(x=uniqueExams, 
                          outputNames=FALSE, 
                          showNames=FALSE) {
  # function to apply a filter to the time series data
  # x input is a list of unique exams
  # the input data is the output from the function in the centerData.R script
  # output=TRUE will output the list for the last exam series in the nput
  # showNames=TRUE will print the exam series names and chart names to the console
  
  # this function will select each data frame in the list
  
  # first source a script with the helper functions 
  # to process the cardio time series data
  
  uniqueExams <- x
  
  # loop over each chart in the list 
  # i <- 1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0(examName, "*", "_Charts")
    seriesNames <- ls(pattern=glob2rx(searchString), pos=1)
    
    # loop over each unique series
    # j <- 1
    for(j in 1:length(seriesNames)) {
      
      if(showNames==TRUE) print(seriesNames[j])
      
      # get the list of time series data for the charts in the exam
      seriesData <- get(seriesNames[j], pos=1)
      chartNames <- names(seriesData)
      
      # loop over each chart in the series 
      # k <- 2
      for(k in 1:length(chartNames)) {
        # get the data frame with the time series data for each chart in the series
        chartData <- seriesData[[k]]
        
        ####
        
        # use a function to get the min peak rows
        minOut <- minPeak(x=chartData$Cardio1, y=7)
        
        # get the min peak values for the min peak rows
        minVal <- chartData$Cardio1[minOut]
        
        ###
        
        # interpolate between the min peak values
        diastolicInterp <- c(interpolatePeaks(x=minOut, y=minVal),0)
        # plot.ts(diastolicInterp, ylim=c(-3,10))
        
        ###
        
        # add diastolic cardio column to each data from in the list
        chartData$CardioDiastolic <- diastolicInterp[1:nrow(chartData)]
        # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        ####
        
        # get the max peak values
        maxOut <- maxPeak(x=chartData$Cardio1, y=7)
        
        # get the max peak values
        maxVal <- chartData$Cardio1[maxOut]
        
        # interpolate between max peak values
        systolicInterp <- c(interpolatePeaks(x=maxOut, y=maxVal),0)
        # plot.ts(systolicInterp, ylim=c(-3,10))
        # myCardioData2$CardioSyst <- systolicInterp
        # ts.plot(myCardioData2[1:3000,c(1,2,6)])
        
        # add the systolic time series to the data frame
        chartData$CardioSystolic <- systolicInterp[1:nrow(chartData)]
        
        ####
        
        # compute a min-Max of the cardio time series data
        
        # get the minMax output
        minMaxOut <- minMaxPeak(x=chartData$Cardio1, y=7)
        
        # get the minMax values
        minMaxVal <- chartData$Cardio1[minMaxOut]
        
        # interpolate the minMax values
        minMaxInterp <- na.omit(c(interpolatePeaks(x=minMaxOut, y=minMaxVal), 0))
        # plot.ts(minMaxInterp, ylim=c(-3,10))
        
#         # adjust the length
#         minMaxInterp <- c(minMaxInterp, rep(0, times=15))
        
        # add the time series to the data frame
        chartData$CardioMinMax <- minMaxInterp[1:nrow(chartData)]
        
        #### 
        
        # compute the smoothed cardio dta
        smoothedCardio <- cardioSmooth1(x=chartData$Cardio1, y=15, times=3)
        # plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))
        
        # add the smoothed cardio to the time series data frame
        chartData$CardioMA <- smoothedCardio[1:nrow(chartData)]
        # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        ####
        
        # use a function to extract the response amplitude from the cardio data
        
        #####
        
        seriesData[[k]] <- chartData
        
        
      } # end for loop over each chart in each series
      
      names(seriesData) <- chartNames
      
      # save the list for the unique series
      assign(seriesNames[j], seriesData, pos=1)
      
    } # end loop over unique series
    
  } # end loop over unique exams
  
  # return the last
  if(outputNames==TRUE) return(seriesData) 
  
} # end cardioSicProg function


####

# call the function to recursively apply the filters
cardioSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)


