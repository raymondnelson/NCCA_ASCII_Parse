EDAChangeFn <- function(x=tsData) {
  # new function to determine the max sig value in a series of significant change
  # in positive slope onset
  # supercedes the slopeChangeFn which selected the first sig value in a run
  # 11-24-2016 raymond nelson
  # called by the amplitudeExtractFn() function
  # useful to infer a response onset when the slope is already positive at stimulus onset
  # compare each nPre seconds with the next nPost seconds
  ###
  # x input is the vector of time series measurements
  # including 10 prestimulus seconds and 10 additional seconds after the evaluation window
  ### these parameters are set in the global environment by the init script
  # nPre is the number of pre change seconds
  # nPost is the number of post change seconds
  # aChange is the level of sig for the z test # currently .999
  # p=.999 will be +3.09 standard deviations
  # cps is data sampling rate # normally 30cps 
  ###
  # output is a vector of 0s the same length as input and including +1 change in slope energy,
  # located at the max zScore during a series of changes in nPre and nPo,
  # that exceed the aChng level of significance
  ###
  
  tsData <- x
  
  assign("tsData", tsData, pos=1)
  
	# initialize the output vector
  outputVector <- rep(0, times=length(tsData))
  negOut <- rep(0, times=length(tsData))
  posOut<- rep(0, times=length(tsData))
  
  # compute the z score cutpoint from the aChng quantile
  # zCut <- qnorm(.998650102)
  # zCutI <- qnorm(1-.998650102)
  zCut <- 6
  
  # round the nPre and nPost segments to the nearest sample
  preLen <- round(cps*2,0)
  postLen <- round(cps*.1,0)
  
  expVal <- 1
  
  # iterate over the length of the output vector and mark sig changes in neg slope activity
  i=1
  for (i in 1:(length(outputVector)-(preLen+postLen)+1)) {
    # calculate the difference for all samples in the pre change segment
    preDiff <- ( diff(tsData[i:(i+preLen-1)] ) )^expVal
    # calculate the difference for all samples in the post change segment
    postDiff <- ( diff( tsData[(i+preLen):(i+preLen+postLen-1)] ) )^expVal
    # z-test for a significant change in means when the data are already ascending
    # uses the sdp() helper function to compute the population sd
    if(range(preDiff)[1]-range(preDiff)[2]==0 | range(postDiff)[1]-range(postDiff)[2]==0) next()
    if( ( ( mean(postDiff) - mean(preDiff) ) / sdp(preDiff) ) <= -zCut ) { 
      # locate the sig change onset after the end of the preLen
      negOut[(i+preLen+postLen-1)] <- "artifact2"
    } 
  } # end for loop
  # which(negOut != 0)
  # make a vector to locate negative slope segments in the time series data
  xNeg <- negativeSlope(fillSlope(smoothSlope(slopeDir(tsData))))
  # keep only those artifact marks in negative slope segments
  negOut[which(xNeg != -1)] <- 0
  # add the negOut to the outputVector
  outputVector[which(negOut != 0)] <- negOut[which(negOut != 0)]
  
  # iterate again over the length of the output vector and mark pos changes in pos slope activity
  i=1
  for (i in 1:(length(outputVector)-(preLen+postLen)+1)) {
    # calculate the difference for all samples in the pre change segment
    preDiff <- ( diff(tsData[i:(i+preLen-1)] ) )^expVal
    # calculate the difference for all samples in the post change segment
    postDiff <- ( diff( tsData[(i+preLen):(i+preLen+postLen-1)] ) )^expVal
    # z-test for a significant change in means when the data are already ascending
    # uses the sdp() helper function to compute the population sd
    if(range(preDiff)[1]-range(preDiff)[2]==0 | range(postDiff)[1]-range(postDiff)[2]==0) next()
    if( ( ( mean(postDiff) - mean(preDiff) ) / sdp(preDiff) ) >= zCut ) { 
      # locate the sig change onset after the end of the preLen
      # significant changes in positive activity are later removed if they occur within a pos seg
      posOut[(i+preLen)] <- "artifact3"
    } 
  } # end for loop
  
  # which(posOut != 0)
  # make a vector to locate negative slope segments in the time series data
  xPos <- positiveSlope(fillSlope(smoothSlope(slopeDir(tsData))))
  # keep only those artifact marks in positive slope segments
  posOut[which(xPos == 1)] <- 0
  # add the posOut artifacts to the outputVector
  outputVector[which(posOut != 0)] <- posOut[which(posOut != 0)]

  # outputVector is now a vector of 0s with the onset of signficant changes in slope marked by 1
  # output the result
  return(outputVector)
} # end EDAChangeFn



