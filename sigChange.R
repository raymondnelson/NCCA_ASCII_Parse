
  # private function to make a vector of significant changes in respiration data
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
  
####
