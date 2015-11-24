# Pneumo artifacts
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



source('~/Documents/R_programming/NCCA_ASCII_Parse/TukeyFences.R', echo=TRUE)
##############



newPArtifactFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE) {
  # function to iterate over a vector of data frame names 
  # and add UPneumoArtifacts and LPneumoArtifacts column to the time series data frame
  #
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  #
  ########
  
  uniqueExams <- x
  
  source('~/Documents/R_programming/NCCA_ASCII_Parse/pneumoSigProcHelper.R', echo=FALSE)
  
  #####
  
#   # private function to make a vector of significant changes
#   sigChange <- function(x, N=5, m=1) {
#     # compare each 1 second with the preceeding N seconds
#     # x is the time series data for upper or lower respiration
#     # N is the number of prior seconds to compare
#     # m is the number of seconds to evaluate
#     
#     myData <- na.omit(x)
#     
#     # preLen is the length in seconds of the preceeding segment
#     preLen <- cps*N
#     # postLen is the length in seconds of the current segment
#     postLen <- cps*m
#     
#     # a loop to make a vector of 1 and -1 if the variance is significant 
#     # i=1
#     y <- rep(0, times=length(myData))
#     for (i in 1:(length(y)-(N*cps)-(m*cps)-1)) {
#       # preDiff is the absolute difference for successive samples in the preLen segment
#       preDiff <- exp(abs(diff(myData[i:(i+preLen-1)])))^10
#       # postDiff is the absolute difference for successive samples in the postLen segment
#       postDiff <- exp(abs(diff(myData[(i+preLen):(i+preLen+postLen-1)])))^10
#       #
#       if(mean(postDiff) >= qnorm(.95, mean=mean(preDiff), sd=sd(preDiff))) { 
#         y[(i+preLen+postLen-1)] <- 1
#       }
#       if(mean(postDiff) <= qnorm(.05, mean=mean(preDiff), sd=sd(preDiff))) {
#         y[(i+preLen+postLen-1)] <- 1
#       }
#     } # end for loop
#     
#     ###
#     
#     # a private function to make vector of positive slope onset rows
#     #     artifactOnset <- function(x=aVector) {
#     #       aOnset <- ifelse(x==0,
#     #                        # compare every x + next x > 0 
#     #                        ifelse((x[1:(length(x)-1)] + x[2:length(x)]) > 0,
#     #                               1,
#     #                               0),
#     #                        0)
#     #       # phase correction
#     #       aOnset <- c(0, aOnset[1:(length(aOnset)-1)])
#     #       return(aOnset)
#     #     } # end artifactOnset function
#     #     
#     #     y1 <- artifactOnset(y)
#     
#     y1 <- y
#     # add m seconds of artifact tag to each
#     y2 <- rep(0, times=length(y1))
#     for (i in (length(y1)-m*cps):1) {
#       if(y1[i] == 1) {
#         y2[i:(i+m*cps-1)] <- 1
#       }
#     }
#     return(which(y2 == 1))
#   } # end sigChange function
#   
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
    examDF$c_UPneumo_a <- rep("", times=nrow(examDF))
    examDF$c_UPneumoInh_a <- rep("", times=nrow(examDF))
    examDF$c_UPneumoMid_a <- rep("", times=nrow(examDF))
    examDF$c_UPneumoExh_a <- rep("", times=nrow(examDF))
    examDF$c_LPneumo_a <- rep("", times=nrow(examDF))
    examDF$c_LPneumoInh_a <- rep("", times=nrow(examDF))   
    examDF$c_LPneumoMid_a <- rep("", times=nrow(examDF))   
    examDF$c_LPneumoExh_a <- rep("", times=nrow(examDF))   
    
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
        
        # get the pneumo artifacts using the peak points
        
        # upper
        
        maxPeaks <- maxPeak(x=chartDF$c_UPneumo, y=7)
        maxRate <- (diff(maxPeaks) / 30)
        # calculate the 3 cycle mean
        maxRateMean3 <- rep(mean(maxRate), length(maxRate))
        for (i in 3:length(maxRate)) maxRateMean3[i] <- mean(maxRate[(i-2):i])
        # calculate the ratio of each cycle to the 3 cycle mean
        maxRateRatio <- maxRate / maxRateMean3
        # get significant changes
        maxRateChange <- maxPeaks[which(maxRateRatio <= .75 | maxRateRatio >= 1.33)]
        # compute any signficant changes in amplitude for successive resp cycles
        maxAmp <- chartDF$c_UPneumo[maxPeaks] - chartDF$c_UPneumoMid[maxPeaks] + 1
        maxAmpRatio <- c(1, maxAmp[2:length(maxAmp)] / maxAmp[1:(length(maxAmp)-1)])
        maxAmpChange <- maxPeaks[which(maxAmpRatio <= .25 | maxAmpRatio >= 1.25)]
        
        minPeaks <- minPeak(x=chartDF$c_UPneumo, y=7)
        minRate <- c(30, diff(minPeaks) / 30) + 1
        minRateRatio <- c(1, minRate[2:length(minRate)] / minRate[1:(length(minRate)-1)])
        minRateChange <- minPeaks[which(minRateRatio <= .667)]
        minAmp <- (chartDF$c_UPneumo[minPeaks] - chartDF$c_UPneumoMid[minPeaks]) + 1
        minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
        minAmpChange <- minPeaks[which(minAmpRatio <= .667 | minAmpRatio >= 1.5)]
        
        
        chartDF$c_UPneumoMid_a[maxRateChange[2:length(maxRateChange)]] <- "Artifact"
        chartDF$c_UPneumoInh_a[maxAmpChange[2:length(maxAmpChange)]] <- "Artifact"
        
        chartDF$c_UPneumoMid_a[minRateChange[2:length(minRateChange)]] <- "Artifact"
        chartDF$c_UPneumoExh_a[minAmpChange[2:length(minAmpChange)]] <- "Artifact"

        # lower
        
        maxPeaks <- maxPeak(x=chartDF$c_LPneumo, y=15)
        maxRate <- c(30, diff(maxPeaks) / 30) + 1
        maxRateRatio <- c(1, maxRate[2:length(maxRate)] / maxRate[1:(length(maxRate)-1)])
        maxRateChange <- maxPeaks[which(maxRateRatio <= .667)]
        maxAmp <- chartDF$c_LPneumoInh[maxPeaks] - chartDF$c_LPneumoMid[maxPeaks] + 1
        maxAmpRatio <- c(1, maxAmp[2:length(maxAmp)] / maxAmp[1:(length(maxAmp)-1)])
        maxAmpChange <- maxPeaks[which(maxAmpRatio <= .667 | maxAmpRatio >= 1.5)]
        
        minPeaks <- minPeak(x=chartDF$c_LPneumo, y=15)
        minRate <- c(30, diff(minPeaks) / 30) + 1
        minRateRatio <- c(1, minRate[2:length(minRate)] / minRate[1:(length(minRate)-1)])
        minRateChange <- minPeaks[which(minRateRatio <= .667)]
        minAmp <- (chartDF$c_LPneumoInh[minPeaks] - chartDF$c_LPneumoMid[minPeaks]) + 1
        minAmpRatio <- c(1, minAmp[2:length(minAmp)] / minAmp[1:(length(minAmp)-1)])
        minAmpChange <- minPeaks[which(minAmpRatio <= .667 | minAmpRatio >= 1.5)]
        
        
        chartDF$c_LPneumoMid_a[maxRateChange[2:length(maxRateChange)]] <- "Artifact"
        chartDF$c_LPneumoInh_a[maxAmpChange[2:length(maxAmpChange)]] <- "Artifact"
        
        chartDF$c_LPneumoMid_a[minRateChange[2:length(minRateChange)]] <- "Artifact"
        chartDF$c_LPneumoExh_a[minAmpChange[2:length(minAmpChange)]] <- "Artifact"
        

        
        ###
        
#         # get the pneumo artifacts using Tukey Fences 
#         
#         # pneumoAU <- sigChange(x=chartDF$c_UPneumo)
#         pneumoAU_Inh <- tukeyFence1(x1=chartDF$c_UPneumoInh, x2=chartDF$c_UPneumoMid, inner=3)
#         # which(pneumoAU_Inh=="X")
#         pneumoAU_Exh <- tukeyFence1(x1=chartDF$c_UPneumoMid, x2=chartDF$c_UPneumoExh, inner=3)
#         # which(pneumoAU_Exh=="X")
#         # print(pneumoAU)
#         pneumoAL_Inh <- tukeyFence1(x1=chartDF$c_LPneumoInh, x2=chartDF$c_LPneumoMid, inner=3)
#         # which(pneumoAL_Inh=="X")
#         pneumoAL_Exh <- tukeyFence1(x1=chartDF$c_LPneumoMid, x2=chartDF$c_LPneumoExh, inner=3)
#         # which(pneumoAL_Exh=="X")
#         # pneumoAL <- sigChange(x=chartDF$c_LPneumo)
#         # print(pneumoAL)
        
#         # add all the artifacts to both pneumo channels in the data frame - 
#         chartDF$c_UPneumoInh_a[which(pneumoAU_Inh=="X")] <- "Artifact"
#         chartDF$c_UPneumoExh_a[which(pneumoAU_Exh=="X")] <- "Artifact"
#         chartDF$c_LPneumoInh_a[which(pneumoAL_Inh=="X")] <- "Artifact"
#         chartDF$c_LPneumoExh_a[which(pneumoAL_Exh=="X")] <- "Artifact"
        
        ### save the chartDF to the larger examDF for all charts
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
    } # end iteration over j series data frames
    
    assign(paste0(examName, "_Data"), examDF, pos=1)  
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end newPArtifactFn()

# newPArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)
  
  



