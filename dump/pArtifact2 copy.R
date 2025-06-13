# Pneumo data measurement
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
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))



cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15



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
  
  # loop over each exam in the list 
  # i=1
  for(i in 1:length(uniqueExams)) {
    
    examName <- uniqueExams[i]
    
    if(showNames==TRUE) print(examName)
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # uniqueSeries <- ls(pattern=glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # uniqueSeries <- ls(pattern=glob2rx(searchString), pos=1)
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    # add the pneumo artifact channels to the data frame
    examDF$UPneumoArtifacts <- rep("", times=nrow(examDF))
    examDF$LPneumoArtifacts <- rep("", times=nrow(examDF))    
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    # j=1
    for(j in 1:length(uniqueSeries)) {
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
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
        
        ###
        
        # private function to make a vector significant changes
        sigChange <- function(x, N=5, m=1) {
          # compare each 1 second with the preceeding N seconds
          # x is the time series data for upper or lower respiration
          # N is the number of prior seconds to compare
          # m is the number of seconds to evaluate
          
          myData <- na.omit(x)
          
          # preLen is the length in seconds of the preceeding segment
          preLen <- cps*N
          # postLen is the length in seconds of the current segment
          postLen <- cps*m
          
          # a loop to make a vector of 1 and -1 if the variance is significant 
          # i=1
          y <- rep(0, times=length(myData))
          for (i in 1:(length(y)-(N*cps)-(m*cps)-1)) {
            # preDiff is the absolute difference for successive samples in the preLen segment
            preDiff <- exp(abs(diff(myData[i:(i+preLen-1)])))^10
            # postDiff is the absolute difference for successive samples in the postLen segment
            postDiff <- exp(abs(diff(myData[(i+preLen):(i+preLen+postLen-1)])))^10
            #
            if(mean(postDiff) >= qnorm(.95, mean=mean(preDiff), sd=sd(preDiff))) { 
              y[(i+preLen+postLen-1)] <- 1
            }
            if(mean(postDiff) <= qnorm(.05, mean=mean(preDiff), sd=sd(preDiff))) {
              y[(i+preLen+postLen-1)] <- 1
            }
          } # end for loop
          
          ###
          
          # a private function to make vector of positive slope onset rows
          #     artifactOnset <- function(x=aVector) {
          #       aOnset <- ifelse(x==0,
          #                        # compare every x + next x > 0 
          #                        ifelse((x[1:(length(x)-1)] + x[2:length(x)]) > 0,
          #                               1,
          #                               0),
          #                        0)
          #       # phase correction
          #       aOnset <- c(0, aOnset[1:(length(aOnset)-1)])
          #       return(aOnset)
          #     } # end artifactOnset function
          #     
          #     y1 <- artifactOnset(y)
          
          y1 <- y
          # add m seconds of artifact tag to each
          y2 <- rep(0, times=length(y1))
          for (i in (length(y1)-m*cps):1) {
            if(y1[i] == 1) {
              y2[i:(i+m*cps-1)] <- 1
            }
          }
          return(which(y2 == 1))
        } # end sigChange function
        
        ###
        
        # get the pneumo artifacts
        pneumoAU <- sigChange(x=chartDF$c_UPneumoS)
        print(pneumoAU)
        pneumoAL <- sigChange(x=chartDF$c_LPneumoS)
        print(pneumoAL)
        
        # add the artifacts to the data frame - 
        chartDF$UPneumoArtifacts[pneumoAU] <- "Artifact"
        chartDF$UPneumoArtifacts[pneumoAL] <- "Artifact"
        chartDF$LPneumoArtifacts[pneumoAL] <- "Artifact"
        chartDF$LPneumoArtifacts[pneumoAU] <- "Artifact"
        
        ### save the chartDF to the larger examDF for all charts
        examDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end iteration over k chart data frames 
      
    } # end iteration over j series data frames
    
    assign(paste0(examName, "_Data"), examDF, pos=1)  
    
  } # end iteration over i exams
  
  if(output==TRUE) return(examDF)
  
} # end newPArtifactFn()

newPArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)
  
  


