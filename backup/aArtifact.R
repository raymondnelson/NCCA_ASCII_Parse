# activity sensor artifacts
# 
# Y-axis excursion
# sum of absolute differences of each successive sample
# in the measured stimulus segment
#
# sampling rate is 30cps
# x input is a vector of time series data
# x length is from stimulus onset to 15 seconds after stimulus onset
# measurement is the sum of absolute differences between successive samples 
#
################################






# get exam names from the _Data data frames
# library(stringr)
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[7]




activityArtifactFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
  # function to iterate over a vector of data frame names 
  # and add activitt sensor artifacts to the time series data frame
  #
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  #
  ########
  
  uniqueExams <- x
  
  # source the sigProcHelper script to load the helper functions for signal processing
  # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=TRUE)
  
  #####
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # check to see if an activity chanel exists and exits if not exits
    if(!("c_SE" %in% names(examDF))) next
    
    # add the activity artifact channels to the data frame
    examDF$c_SE_a <- rep("", times=nrow(examDF))
    examDF$c_SEMax_a <- rep("", times=nrow(examDF))
    examDF$c_SEMin_a <- rep("", times=nrow(examDF))
    examDF$c_SEMA_a <- rep("", times=nrow(examDF))
    examDF$c_SEAmp_a <- rep("", times=nrow(examDF))
      
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        chartName <- uniqueCharts[k]
        
        # get the data frame with the time series data for each chart in the series
        # chartDF <- seriesDF[[k]]
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(nrow(chartDF)<300) next()
        
        if(showNames==TRUE) print(chartName)
        
        chartOnsetRow <- which(examDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ###
        
        # get the activity sensor artifacts using the ocsillating peak points
        
        # get the max peaks for the activity sensor oscillation - similar to respiration
        maxPeaks <- maxPeak(x=chartDF$c_SE, y=40)
        # calculate the rate of the intervals
        maxRate <- c(diff(maxPeaks), mean(diff(maxPeaks))) / 30
        # calculate the 3 cycle mean 
        maxRateMean3 <- rep(mean(maxRate), length(maxRate)) 
        for (l in 4:length(maxRate)) { maxRateMean3[l] <- mean(maxRate[(l-3):l]) }
        # calculate the ratio of each cycle length to the 3 cycle mean length
        maxRateRatio <- maxRate^1 / maxRateMean3^1
        # get significant changes
        maxRateChange <- maxPeaks[which(maxRateRatio <= .57 | maxRateRatio >= 1.75)]
        # next compute any signficant changes in amplitude for successive resp cycles
        # first get the difference between the peak amplitude and the moving average
        maxAmp <- chartDF$c_SE[maxPeaks] - chartDF$c_SEMA[maxPeaks]
        # calculate the 3 cycle mean peak amplitude
        maxAmpMean3 <- rep(mean(maxAmp), length(maxAmp))
        for (m in 4:length(maxAmp)) { maxAmpMean3[m] <- mean(maxAmp[(m-2):m]) }
        # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
        maxAmpRatio <- (abs((maxAmp / maxAmpMean3))+0)^1
        # maxAmpRatio <- c(1, maxAmp[2:length(maxAmp)] / maxAmp[1:(length(maxAmp)-1)])
        maxAmpChange <- maxPeaks[which(maxAmpRatio <= .57 | maxAmpRatio >= 1.75)]
        
        # get the min peaks for the activity sensor data
        minPeaks <- minPeak(x=chartDF$c_SE, y=40)
        # calculate the rate of oscillation
        minRate <- c(diff(minPeaks), mean(diff(minPeaks))) / 30
        # calculate the 3 cycle mean
        minRateMean3 <- rep(mean(minRate), length(minRate)) 
        for (l in 3:length(minRate)) minRateMean3[l] <- mean(minRate[l:(l-2)]) 
        # calculate the ratio of each cycle length to the 3 cycle mean length
        minRateRatio <- minRate^1 / minRateMean3^1 
        # get significant changes
        minRateChange <- minPeaks[which(minRateRatio <= .57 | minRateRatio >= 1.75)]
        # next compute any signficant changes in amplitude for successive resp cycles
        minAmp <- chartDF$c_SE[minPeaks] - chartDF$c_SEMA[minPeaks]
        # calculate the 3 cycle mean peak amplitude
        minAmpMean3 <- rep(mean(minAmp), length(minAmp))
        for (m in 3:length(minAmp)) minAmpMean3[m] <- mean(minAmp[m:(m-2)])
        # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
        minAmpRatio <- minAmp^1 / minAmpMean3^1
        # minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
        minAmpChange <- minPeaks[which(minAmpRatio <= .57 | minAmpRatio >= 1.75)]
        
        chartDF$c_SEMA_a[maxAmpChange] <- "Artifact"
#         chartDF$c_SEMax_a[maxAmpChange] <- "Artifact"
#         chartDF$c_SEMin_a[minAmpChange] <- "Artifact"
        chartDF$c_SEMA_a[minAmpChange] <- "Artifact"
        # chartDF$c_SE_a <-
        
        ### save the chartDF to the larger examDF for all charts
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
    } # end iteration over j series data frames
    
    assign(paste0(examName, "_Data"), examDF, pos=1)  
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end activityArtifactFn()

# activityArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)




