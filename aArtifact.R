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



library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[17]




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
  
  source('~/Documents/R_programming/NCCA_ASCII_Parse/activitySigProcHelper.R', echo=FALSE)
  
  #####
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=i
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # add the pneumo artifact channels to the data frame
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
        
        if(showNames==TRUE) print(chartName)
        
        chartOnsetRow <- which(examDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ###
        
        # get the activity sensor artifacts using the ocsillating peak points
        
        maxPeaks <- maxPeak(x=chartDF$c_SE, y=7)
        maxRate <- c(mean(diff(maxPeaks)), diff(maxPeaks)) / 30
        # calculate the 3 cycle mean rate
        maxRateMean3 <- rep(mean(maxRate), length(maxRate)) 
        for (l in 4:length(maxRate)) maxRateMean3[l] <- median(maxRate[(l-3):(l-1)]) 
        # calculate the ratio of each cycle length to the 3 cycle mean length
        maxRateRatio <- maxRate / maxRateMean3 
        # get significant changes
        maxRateChange <- maxPeaks[which(maxRateRatio <= .67 | maxRateRatio >= 1.5)]
        # next compute any signficant changes in amplitude for successive resp cycles
        maxAmp <- chartDF$c_SE[maxPeaks] - chartDF$c_SEMA[maxPeaks]
        # calculate the 3 cycle mean peak amplitude
        maxAmpMean3 <- rep(mean(maxAmp), length(maxAmp))
        for (m in 4:length(maxAmp)) maxAmpMean3[m] <- median(maxAmp[(m-3):(m-1)])
        # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
        maxAmpRatio <- maxAmp / maxAmpMean3
        # maxAmpRatio <- c(1, maxAmp[2:length(maxAmp)] / maxAmp[1:(length(maxAmp)-1)])
        maxAmpChange <- maxPeaks[which(maxAmpRatio <= .67 | maxAmpRatio >= 1.5)]
        
        minPeaks <- minPeak(x=chartDF$c_SE, y=7)
        minRate <- c(mean(diff(minPeaks)), diff(minPeaks)) / 30
        # calculate the 3 cycle mean rate
        minRateMean3 <- rep(mean(minRate), length(minRate)) 
        for (l in 3:length(minRate)) minRateMean3[l] <- mean(minRate[(l-2):l]) 
        # calculate the ratio of each cycle length to the 3 cycle mean length
        minRateRatio <- minRate / minRateMean3 
        # get significant changes
        minRateChange <- minPeaks[which(minRateRatio <= .67 | minRateRatio >= 1.5)]
        # next compute any signficant changes in amplitude for successive resp cycles
        minAmp <- chartDF$c_SE[minPeaks] - chartDF$c_SEMA[minPeaks]
        # calculate the 3 cycle mean peak amplitude
        minAmpMean3 <- rep(mean(minAmp), length(minAmp))
        for (m in 3:length(minAmp)) minAmpMean3[m] <- mean(minAmp[(m-2):m])
        # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
        minAmpRatio <- minAmp / minAmpMean3
        # minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
        minAmpChange <- minPeaks[which(minAmpRatio <= .67 | minAmpRatio >= 1.5)]
        
        chartDF$c_SEMA_a[maxRateChange] <- "Artifact"
        chartDF$c_SEMax_a[maxAmpChange] <- "Artifact"
        chartDF$c_SEMin_a[minRateChange] <- "Artifact"
        chartDF$c_SEAmp_a[minAmpChange] <- "Artifact"
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





