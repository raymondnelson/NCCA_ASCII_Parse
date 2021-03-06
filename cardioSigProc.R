# functions for cardio signal processing 

# minPeak()
# maxPeak()
# minMaxPeak()
# cardioSmooth1()

########################



library(stringr)



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



# uniqueExams <- uniqueExams[1]




#########################

# a function to process the the cardio data for a list of input exams in the cwd

cardioSigProc <- function(x=uniqueExams, 
                          outputNames=FALSE, 
                          showNames=TRUE) {
  # function to process the time series cardio data
  # x input is a list of unique exams
  # the input data is the output from the function in the centerData.R script
  # output=TRUE will output the data frame for the last exam series in the input
  # showNames=TRUE will print the exam series names and chart names to the console
  
  # this function will select each data frame in the list
  
  # first source a script with the helper functions 
  # to process the cardio time series data
  
  uniqueExams <- x
  
  ########### some helper functions 
  
  minPeak <- function(x, y) {
    # private function to get the diastolic peaks from the cardio data
    # x input is a time series vector
    # y input is the number of offset samples 
    # buffer will be double the offset value
    xOut <- rep(NA, times=(length(x)))
    xOut[1] <- 1
    xOut[length(xOut)] <- length(xOut)
    input_buffer <- x[2:(2*y+1)]
    for (i in 2:(length(x)-(2*y))) {
      input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
            ifelse(input_buffer[y+1]==min(input_buffer),
             xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
             next()
             )
    } # end for loop
    return(na.omit(xOut))
  } # end minPeak function

  #####
  
  maxPeak <- function(x, y) {
    # function to get the diastolic peaks from the cardio data
    # x input is a time series vector
    # y input is the number of offset samples 
    # buffer will be double the offset value
    # xOut <- numeric(length=length(x))
    xOut <- rep(NA, times=(length(x)))
    xOut[1] <- 1
    xOut[length(xOut)] <- length(xOut)
    input_buffer <- x[2:(2*y+1)]
    for (i in 2:(length(x)-(2*y))) {
      input_buffer <- c(input_buffer[2:(2*y)], x[i+(2*y)])
      ifelse(input_buffer[y+1]==max(input_buffer),
             xOut[i+y+1] <- c(i+y+1), # +1 because we started at 2
             next()
      )
    } # end for loop
    return(na.omit(xOut))
  }  # end maxPeak
  
  #####
  
  interpolatePeaks <- function(x, y) {
    # private function to interpolate between peak segments of time series input data
    # x is a vector of peak row numbers in the data frame
    # y is a vector of peak row values in the data frame
    peakOutDiff <- diff(x)
    peakValDiff <- diff(y)
    peakDivisor <- peakOutDiff
    # peakDivisor[which(peakOutDiff>0)] <- (peakOutDiff[which(peakOutDiff>0)])
    peakValDiff2 <- peakValDiff / peakDivisor
    peakFill <- rep(peakValDiff2, times=peakOutDiff)
    peakFill2 <- cumsum(peakFill)
    # peakFill2[x] <- y # to preserve the exact peak values for verification of the interpolation
    return(c(y[1], peakFill2))
  } # end interpolatePeaks
  # diastolicInterp <- c(interpolatePeaks(x=minOut, y=minVal),0 )
  # plot.ts(diastolicInterp, ylim=c(-3,10))
  
  #####

  minMaxPeak <- function(x, y) {
    # private function to get the diastolic peaks from the cardio data
    # x input is a time series vector
    # y input is the number of offset samples 
    # buffer will be double the offset value
    #
    xOut <- c(1, rep(NA, times=(length(x)-2)), length(x))
    input_buffer <- x[1:(2*y)]
    for (i in (y+1):(length(x)-(2*y+1))) {
      # input buffer maybe should be [2:length(input_buffer)] not [2:2*y+1]
      input_buffer <- c(input_buffer[2:length(input_buffer)], x[i])
      ifelse(input_buffer[y]==min(input_buffer),
        { xOut[(i+.5*y):(i+y-2)] <- NA
          xOut[i+y-1] <- c(i+y-1) }, 
        ifelse(input_buffer[(y)]==max(input_buffer),    
        { xOut[(i+.5*y):(i+y-2)] <- NA
          xOut[i+y-1] <- c(i+y-1) },
        next()))
    } # end for loop
    xOut <- na.omit(xOut)
    # xOut <- c(xOut, rep(xOut[length[xOut]], times=(x-length(xOut))))
    return(xOut)
  } # end minMaxPeak function
  
  ##### 
  
  cardioSmooth1 <- function(x=chartDF$Cardio1, y=15, times=3) {
    # private function to get the diastolic peaks from the cardio data
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
  # smoothedCardio <- cardioSmooth1(x=chartDF$Cardio1, y=15, times = 4)
  # plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))
  
  #####
  
  minMaxMean <- function(x=chartDF$Cardio1, y=25) {
  # private function to compute the average of min and max for a moving averge
    # x is the time series cardio data
    # y is 1/2 the number of samples in the buffer
    xOut <- c(rep(mean(x[1:y]), time=y), 
              numeric(length(x)-2*y), 
              rep(mean(x[length(x)-2*y+1:length(x)])))
    input_buffer <- x(1:2*y)
    for (i in (y+1):(length(x)-y)) {
      xOut[i] <- mean(min(input_buffer), max(input_buffer))
      input_buffer <- c(input_buffer1[2:(2*y)], x[i])
    }
    xOut <- na.omit(xOut)
    xOut <- c(xOut, rep(xOut[length(xOut), times=(length(x)-length(xOut))]))
    return(xOut) 
  } # end minMaxMean function
  
  #### end of private helper functions

  ###################################
  
  # loop over each exam series and chart 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    # add 4 new columns for the processed cardio data
    examDF$c_CardioDiastolic <- rep(0, times=nrow(examDF))
    examDF$c_CardioSystolic <- rep(0, times=nrow(examDF))
    examDF$c_CardioMinMax <- rep(0, times=nrow(examDF))
    examDF$c_CardioMA <- rep(0, times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # make an empty list to hold the output
    outputList <- NULL
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      # get the time series data for the series
      # seriesDF <- get(uniqueSeries[j], pos=1)
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      # get the naems of unique charts
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        # chartDF <- seriesDF[[k]]
        chartDF <- examDF[examDF$chartName==uniqueCharts[k],]
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(examDF$chartName==uniqueCharts[k])[1]
        
        ####
        
        # use a function to get the min peak rows
        minOut <- minPeak(x=chartDF$c_Cardio1, y=15)
        
        # get the min peak values for the min peak rows
        minVal <- chartDF$c_Cardio1[na.omit(minOut)]
        
        # interpolate between the min peak values
        diastolicInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))
        # plot.ts(diastolicInterp, ylim=c(-3,10))
        
        # add the vector to the diastolic cardio column 
        chartDF$c_CardioDiastolic <- diastolicInterp[1:nrow(chartDF)]
        # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        ####
        
        # get the max peak values
        maxOut <- maxPeak(x=chartDF$c_Cardio1, y=15)
        
        # get the max peak values
        maxVal <- chartDF$c_Cardio1[maxOut]
        
        # interpolate between max peak values
        systolicInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
        # plot.ts(systolicInterp, ylim=c(-3,10))
        # myCardioData2$CardioSyst <- systolicInterp
        # ts.plot(myCardioData2[1:3000,c(1,2,6)])
        
        # add the systolic time series to the data frame
        chartDF$c_CardioSystolic <- systolicInterp
        
        ####
        
        # compute a min-Max of the cardio time series data
        
        # get the minMax output
        minMaxOut <- minMaxPeak(x=chartDF$c_Cardio1, y=15)
        
        # get the minMax values
        minMaxVal <- chartDF$c_Cardio1[minMaxOut]
        
        # interpolate the minMax values
        minMaxInterp <- na.omit(c(interpolatePeaks(x=minMaxOut, y=minMaxVal), 0))[1:nrow(chartDF)]
        # plot.ts(minMaxInterp, ylim=c(-3,10))
        
#         # adjust the length
#         minMaxInterp <- c(minMaxInterp, rep(0, times=15))
        
        # add the time series to the data frame
        chartDF$c_CardioMinMax <- minMaxInterp
        
        #### 
        
        # compute the smoothed cardio dta
        smoothedCardio <- cardioSmooth1(x=chartDF$c_Cardio1, y=15, times=3)
        # plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))
        
        # add the smoothed cardio to the time series data frame
        chartDF$c_CardioMA <- smoothedCardio[1:nrow(chartDF)]
        # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        #####
        
        # save the chartDF in the output list
        outputList[[k]] <- chartDF

        # save the chartDF to the examDF
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF

      } # end for loop over each chart in each series
      
      # name the data frames in the ouput list
      names(outputList) <- uniqueCharts
      
#     # save the list for the unique series
#     assign(uniqueSeries[j], seriesDF, pos=1)

    } # end loop over unique series
    
    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)
    
  } # end loop over unique exams
  
  # return the last
  if(outputNames==TRUE) return(examDF) 
  
} # end cardioSicProg function


####

# call the function to recursively apply the filters
cardioSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)


