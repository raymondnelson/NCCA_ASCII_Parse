# R function to identify EDA artifacts resulting from finger movement
# and disruption of the EDA circuit
# 11-26-2016
# 9-3-2023
# Raymond Nelson
# 
######



EDAMvtArtifactFn <- function(tsData=chartDF$c_AutoEDA) {
  # R function to identify EDA artifacts resulting from finger movement
  # and disruption of the EDA circuit
  # 11-26-2016
  # 9-3-2023
  # Raymond Nelson
  ######
  #
  # x input is is the time series EDA data
  #
  # output is a vector of indices where significant movement was found
  # 
  ####################
  
  # tsData <- chartDF$c_AutoEDA
  
  # initialize the output vector
  outputVector <- rep(0, times=length(tsData))
  
  # set the z cutscore for significant activity
  # pnorm(5)
  # 0.9999997
  # pnorm(6)
  zCut <- 5
  
  # round the nPre and nPost segments to the nearest sample
  preLen <- round(cps*2,0)
  postLen <- round(cps*.1,0)
  
  # set the exponent value to transform the difference vals
  expVal <- 1
  
  # initialize an output vector
  negOut <- NULL
  posOut <- NULL
  
  {
    # a private function to calculate the population standard deviation
    sdp <- 
      function(x)(sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x)))
  }
  
  #### iterate over the length of the output vector for neg slope activity ####
  
  i=1
  for (i in 1:(length(outputVector)-(preLen+postLen)+1)) {
    # calculate the difference for all samples in the pre change segment
    preDiff <- ( diff(tsData[i:(i+preLen-1)] ) )^expVal
    # calculate the difference for all samples in the post change segment
    postDiff <- ( diff( tsData[(i+preLen):(i+preLen+postLen-1)] ) )^expVal
    # increment the loop if there is no slope in the pre or post
    if(range(preDiff)[1]-range(preDiff)[2]==0 | range(postDiff)[1]-range(postDiff)[2]==0) next()
    # z-test for a significant change in prediff and postdiff means
    # use the sdp() private function to compute the population standard deviation
    if( ( ( mean(postDiff) - mean(preDiff) ) / sdp(preDiff) ) <= -3 ) { 
      # mark the artifact after the end of the preLen
      negOut[(i+preLen+postLen-1)] <- "artifact2"
    } 
  } # end for loop
  
  # which(negOut == "artifact2")
  # which(negOut != 0)
  
  # make a vector to locate negative slope segments in the time series data
  # get these functions from amplitudeExtractHelperFunctions.R script
  xNeg <- negativeSlope(fillSlope(smoothSlope(slopeDir(tsData))))
  # keep only those artifact marks in negative slope segments
  negOut[which(xNeg != -1)] <- 0
  # add the negOut to the outputVector
  outputVector[which(negOut != 0)] <- negOut[which(negOut != 0)]
  
  # locate the onset index for each run of artifacts
  artifact2Onset <- which(outputVector[2:length(outputVector)] !=
                            outputVector[1:(length(outputVector)-1)] &
                            outputVector[2:length(outputVector)] ==
                            "artifact2") + 1
  
  # locate the offset index for each run
  {
    artifact2Offset <- which(outputVector[1:(length(outputVector)-1)] !=
                               outputVector[2:length(outputVector)] &
                               outputVector[1:(length(outputVector)-1)] ==
                               "artifact2")
    # correct for the last sample
    if(outputVector[length(outputVector)] == 
       outputVector[(length(outputVector)-1)] & 
       outputVector[length(outputVector)] == "artifact2") { 
      artifact2Offset <- c(artifact2Offset, length(outputVector)) 
    } 
    if(length(artifact2Offset) < length(artifact2Onset)) {
      artifact2Offset <- c(artifact2Offset, artifact2Onset[length(artifact2Onset)])
    }
  }
  
  ##### check the range for each run of artifacts ####
  # keep the run if the range exceeds a value of 2.5% of the max working range (2000)
  
  # first initialize some output vectors
  onsetOut <- NULL
  offsetOut <- NULL
  
  # initialize a variable for the length of the range
  descSig <- round(yRange * .025, 0)
  
  # then execute the loop if there are any artifacts
  if(length(artifact2Onset) > 0) {
    i=1
    for(i in 1:length(artifact2Onset)) {
      # check if the magnitude of descent is small 
      if(abs(diff(range(tsData[artifact2Onset[i]:artifact2Offset[i]]))) >= descSig) {
        onsetOut <- c(onsetOut, artifact2Onset[i])
        offsetOut <- c(offsetOut, artifact2Offset[i])
      }
    }
    ## re-initialize the outputVector without small descent values
    outputVector <- rep(0, times=length(tsData))
    # then use another loop
    if(length(onsetOut) > 0){
      for(i in 1:length(onsetOut)) {
        outputVector[onsetOut[i]:offsetOut[i]] <- "artifact2"
      }
    }
  } # end if()
  
  #### output ####
  
  # output is a vector of 0 values with "artifact2" where finger mvt artifact occur
  return(which(outputVector != 0))
  
} # end EDAMvtArtifactFn() function 

