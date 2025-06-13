# cardio sensor artifacts
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




# library(stringr)




# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[17]




cardioArtifactFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
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
  
  # source('~/dropbox/R_programming/NCCA_ASCII_Parse/cardioSigProcHelper.R', echo=FALSE)
  
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
    
    # add the cardio artifact channels to the data frame
    examDF$c_Cardio1_a <- rep("", times=nrow(examDF))
    examDF$c_CardioSystolic_a <- rep("", times=nrow(examDF))
    examDF$c_CardioDiastolic_a <- rep("", times=nrow(examDF))
    examDF$c_CardioMid_a <- rep("", times=nrow(examDF))
    examDF$c_CardioMA_a <- rep("", times=nrow(examDF))
    examDF$c_CardioAmp_a <- rep("", times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
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
        
        #### calculate the cardio rate
        
        cardioRate <- ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),11)
        
        # calculate the buffer length for the cardio rate
        # bufferLen <- floor(1/cardioRate*60*cps*.6) -1  # round down
        bufferLen <- bufferLenFn(cardioRate)
        
        ###
        
        # make a new smoother moving average of the cardio data
        # newCardioMA <- cardioSmooth1(x=chartDF$c_Cardio1, y = round(.5*cps,0), times=12)
        # not needed because the cardioMA is already computed by the cardioSigProc function
        
        ###### get the cardio sensor artifacts using the ocsillating peak points
        
        ### systolic peaks
        maxPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
        
        ## calculate the systolic cardio rate
        # maxRate <- 60/ (c(mean(diff(maxPeaks)), diff(maxPeaks)) / cps)
        # ## calculate the 3 cycle mean rate
        # maxRateMean3 <- rep(mean(maxRate), length(maxRate)) 
        # for (l in 4:length(maxRate)) { maxRateMean3[l] <- median(maxRate[(l-3):(l-1)]) }
        # # calculate the ratio of each cycle length to the 3 cycle mean length
        # maxRateRatio <- maxRate / maxRateMean3 
        # # get significant changes
        # maxRateChange <- maxPeaks[which(maxRateRatio <= .57 | maxRateRatio >= 1.75)]
        # chartDF$c_CardioSystolic_a[maxRateChange] <- "Artifact"
        
        ## compute any signficant changes in amplitude for successive resp cycles
        maxAmp <- chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMid[maxPeaks]
        # calculate the 3 cycle mean peak amplitude
        maxAmpMean3 <- rep(mean(maxAmp), length(maxAmp))
        for (m in 4:length(maxAmp)) { maxAmpMean3[m] <- median(maxAmp[(m-3):(m-1)]) }
        # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
        maxAmpRatio <- maxAmp / maxAmpMean3
        # calculate any significant change
        maxAmpChange <- maxPeaks[which(maxAmpRatio <= .57 | maxAmpRatio >= 1.75)]
        chartDF$c_CardioSystolic_a[maxAmpChange] <- "Artifact"
        
        ### diastolic peaks
        minPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
       
        ## calculate the diastolic cardio rate
        # minRate <- 60/ (c(mean(diff(minPeaks)), diff(minPeaks)) / cps)
        # ## calculate the 3 cycle mean rate
        # minRateMean3 <- rep(mean(minRate), length(minRate)) 
        # for (l in 3:length(minRate)) { minRateMean3[l] <- median(minRate[(l-3):(l-1)]) }
        # # calculate the ratio of each cycle length to the 3 cycle mean length
        # minRateRatio <- minRate / minRateMean3 
        # # get significant changes
        # minRateChange <- minPeaks[which(minRateRatio <= .5 | minRateRatio >= 2)]
        # chartDF$c_CardioDiastolic_a[minRateChange] <- "Artifact"

        ## compute any signficant changes in amplitude for successive resp cycles
        minAmp <- chartDF$c_CardioMid[minPeaks] - chartDF$c_Cardio1[minPeaks]
        # calculate the 3 cycle mean peak amplitude
        minAmpMean3 <- rep(mean(minAmp), length(minAmp))
        for (m in 4:length(minAmp)) { minAmpMean3[m] <- mean(minAmp[(m-3):(m-1)]) }
        # calculate the ratio of each cycle amplitude to the mean of the previous 3 cycles
        minAmpRatio <- minAmp / minAmpMean3
        # minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
        # calculate any significant change
        minAmpChange <- minPeaks[which(minAmpRatio <= .57 | minAmpRatio >= 1.75)]
        chartDF$c_CardioDiastolic_a[minAmpChange] <- "Artifact"
        
        ### pulse amplitude changes
        minMaxPeak <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
        minMaxAmp <- abs(diff(chartDF$c_Cardio1[minMaxPeak]))
        minMaxAmpMean3 <- rep(mean(minMaxAmp), length(minMaxAmp))
        for (n in 4:length(minMaxAmp)) { minMaxAmpMean3[n] <- mean(minMaxAmp[(n-3:(n-1))]) }
        # minMaxRatio <- c(minMaxAmp[2:length(minMaxAmp)] / minMaxAmp[1:(length(minMaxAmp)-1)])
        minMaxRatio <- minMaxAmp / minMaxAmpMean3
        # calculate any significant change
        minMaxChange <- minMaxPeak[which( minMaxRatio <= .5 | minMaxRatio >= 2) ]
        chartDF$c_Cardio1_a[minMaxChange] <- "Artifact"
        
        
        ### changes in the slow cardioMA
        MAChange <- NULL
        # systolic Y value is less than the cardioMA
        MAChange <- c(MAChange, which(chartDF$c_CardioSystolic <= chartDF$c_CardioMA))
        # diastolic Y value is greater than the cardioMA
        MAChange <- c(MAChange, which(chartDF$c_CardioDiastolic >= chartDF$c_CardioMA))
        MAChange <- sort(MAChange)
        chartDF$c_CardioMA_a[MAChange] <- "Artifact"
        

        
        
        # check the artifacts
        # which(chartDF$c_CardioMA_a=="Artifact")
        # which(chartDF$c_CardioMid_a=="Artifact")
        # which(chartDF$c_Cardio1_a=="Artifact")
        # which(chartDF$c_CardioDiastolic_a=="Artifact")
        # which(chartDF$c_CardioSystolic_a=="Artifact")
        # which(chartDF$c_CardioAmp_a=="Artifact")
        
        ### save the chartDF to the larger examDF for all charts
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
    } # end iteration over j series data frames
    
    assign(paste0(examName, "_Data"), examDF, pos=1)  
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end cardioArtifactFn()

# cardioArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)





