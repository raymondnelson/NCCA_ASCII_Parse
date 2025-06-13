# script to identify pneumo artifacts

############################

# 
# mySegmentLists <- ls(pattern="*_dataSegmentList$")
# myEventLists <- ls(pattern="*_eventList$")
# 
# mySegmentLists <- mySegmentLists[1]
# myEventLists <- myEventLists[1]
# 
# mySegmentDF <- get(mySegmentLists)[[11]]
# myEventDF <- get(myEventLists)[[11]]
# 
# ###
# 
# myData1 <- mySegmentDF$UPneumoS
# myData2 <- mySegmentDF$LPneumoS
# 
# begin <- myEventDF$Begin
# end <- myEventDF$End
# answer <- myEventDF$Answer
# # if(answer == end) answer <- answer+1
# start <- mySegmentDF$Sample[1]
# lat <- .5
# nSmooth <- 4
# label <- myEventDF$Label
# segmentName <- paste(mySegmentDF$examName[1], mySegmentDF$chartName[1], myEventDF$Label, sep="_")
# 
####

cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15

# x <- myData

####

# pArtifact <- function(x=myData) {
#   # function to identify pneumo artifacts
#   #
#   # compare moving average of 1 second variance with the mean and sd of the variance of the preceeding 5 seconds
#   #
#   # compare the moving average of 5 seconds of the variance of upper and lower pneumo difference 
#   # with the mean and sd of the difference variance for the preceeding 5 seconds
#   #
#   #########################

#  x <- myData1
  
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



pVarChange <- function(x=stimSegmentDF$UPneumoVar, N=6, m=2) {
  # compare each 1 second with the preceeding N seconds
  # x is the 1 second variance time series data for upper or lower respiration
  # N is the number of prior second to compare
  # m is the number of seconds to evaluate
  
  myData <- na.omit(x)
  
  y <- rep(0, times=length(myData))
  for (i in 1:(length(y)-(N*cps)-(m*cps)-1)) {
    # preLen is the length in seconds of the preceeding segment
    preLen <- cps*N
    # postLen is the length in seconds of the current segment
    postLen <- cps*m
    # preDiff is the absolute difference for successive samples in the preLen segment
    
    preDiff <- myData[i:(i+preLen-1)]
    # postDiff is the absolute difference for successive samples in the postLen segment
    postDiff <- myData[(i+preLen):(i+preLen+postLen-1)]
    
    if (mean(postDiff) >= qnorm(.9, mean=mean(preDiff), sd=sd(preDiff))) { 
      y[(i+preLen+postLen-1)] <- 1
    }
    
    if (mean(postDiff) <= qnorm(.1, mean=mean(preDiff), sd=sd(preDiff))) {
      y[(i+preLen+postLen-1)] <- 1
    }
    
  } # end for loop
  
} # end pVarChange function



