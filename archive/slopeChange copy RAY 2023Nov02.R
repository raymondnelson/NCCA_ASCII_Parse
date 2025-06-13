# Sep. 2, 2021
# Raymond Nelson



maxSlopeChangeFn <- function(x=tsData, idx=TRUE) {
  # new function to determine the max sig value 
  # in a series of significant changes in positive slope
  # supercedes the slopeChangeFn 
  # which always selected the first sig value in a run 
  # 11-24-2016 raymond nelson
  # improved 10-7-2018
  # called by the amplitudeExtractFn() function
  # used to impute a response onset 
  # within a positive slope segment 
  # such as when the slope is positive prior to stimulus onset
  # compare each nPre period (seconds) with the next nPost seconds
  ####
  # x input is the vector of time series data for a stim segment
  # including 10 prestimulus seconds 
  # a 15 second stimulus segment
  # and 10 additional seconds after the evaluation window
  #### these parameters are set in the global environment by the init script
  # nPre is the number of pre change seconds
  # nPost is the number of post change seconds
  # aChange is the level of sig for the z test # currently .999
  # p=.999 will be +3.09 standard deviations
  # p=.9999 will be +3.72 standard deviations
  # p=.998650102 = 3 SD
  # 4 standard deviations is p=.9999683
  # cps is data sampling rate # normally 30cps 
  ####
  # output is a vector of 0s the same length as input 
  # and including +1 change in slope energy,
  # located at the max zScore during a series of sig changes in nPre and nPo,
  ####
  
  tsData <- x
  
  options(warn = 2)
  
  assign("tsData", tsData, envir = .GlobalEnv)
  # stop()

  {
    
    # use a function to compute the z score cutpoint from the aChange quantile
    zCut <- qnorm(aChange)
    
    # round the nPre and nPost segments to the nearest sample
    # values are set in the NCCAASCII_init.R script
    preLen <- round(cps*nPre,0)
    postLen <- round(cps*nPost,0)
    tonicLen <- round(cps*tonicSec,0)
    
  }

  # exit if the input vector is too short
  if(length(tsData) <= (preLen+1)) return(y)
  
  #### calculate the difference and StDev for the input time series data ####
  
  # modified 10-7-2018 to use the absolute value
  xDiff <- c(0, abs(diff(tsData)))
  
  # Sep 9, 2021
  xStDev <- rep(0, times=length(xDiff))
  for(i in preLen:length(xStDev)) {
  	# calculate the population standard deviation
    xStDev[i] <- sdp(tsData[i:(i+preLen-1)])
  }
  
  # use the standard deviations instead of difference values
  xDiff <- xStDev
  
  #### initialize some objects ####
  
  {
    preDiffMean <- rep(0, times=length(xDiff))
    preDiffSD <- rep(0, times=length(xDiff))
    postDiffMean <- rep(0, times=length(xDiff))
    zScore <- rep(0, times=length(xDiff))
    
    # initialize the output vector
    y <- rep(0, times=length(xDiff))
  }
  
  ###
  
  # calculate all preDiff means 
  for (i in preLen:length(xDiff)) {
  	# xDiff is a vector of population standard deviations
    preDiffMean[i] <- mean(xDiff[(i-preLen+1):i], na.rm=TRUE)
  }
  
  # calculate all preDiff standard deviations
  # uses a helper function sdp() to compute the population st dev
  for (i in preLen:length(xDiff)) {
  	# preDiffSD is the standard error of the mean of the standard deviations
    preDiffSD[i] <- sdp(xDiff[(i-preLen+1):i])
  }
  
  # Aug 23, 2023 to prevent NA errors with the cardio mid line when the SD is zero
  preDiffSD[which(preDiffSD < .01)] <- .01
  
  # calculate all postDiff means
  for (i in (preLen+postLen):length(xDiff)) {
    postDiffMean[i] <- mean(xDiff[(i-postLen+1):i], na.rm=TRUE)
  } 
  
  #### use a loop to calculate the z scores for preDiff and postDiff means ####
  
  # for (i in (preLen+1):(length(xDiff)-postLen+1)) {
  # iterate starting at the end of the postDiff window
  for (i in (preLen+postLen):length(xDiff)) {
    # increment the loop if preDiffSD[(i-1)] == 0
    # to avoid NaN result when divide by zero
    # if(preDiffSD[(i-1)] == 0) next()
    # when the SD is zero because the data are flat
    if(preDiffSD[(i)] == 0) next()
    # calculate the zScore for each postDiff mean,
    # using the mean and SD from the preDiff window
    # zScore[i] <- (postDiffMean[i] - preDiffMean[(i-1)]) / preDiffSD[(i-1)]
    thisVal <- (postDiffMean[i] - preDiffMean[(i-postLen)]) / preDiffSD[(i-postLen)]
    if(is.infinite(thisVal)) next()
    # put the zScore at the beginning of the postLen segment
    zScore[(i-postLen)] <- thisVal
    # put the zScore at the beginning of the preLen segment Oct 31, 2023
    # zScore[i-(preLen+postLen)] <- thisVal
  }
  
  #### determine which mean Diff zScores are significant ####
  
  # keep only z scores that are statistically significant
  zScore[which(zScore < zCut)] <- 0
  
  #### check the slope of the data ####
  
  # call the slopeDir() function to make a vector of slope data for the input
  slopeDat <- fillSlope(smoothSlope(slopeDir(tsData), 
                                    nSmooth=round(cps*ignoreTonicChange,0)))
  slopeDat <- slopeDat[1:length(tsData)]
  
  # 10-7-2018 remove zScore indices for which the tonicLen slope is not +
  zScore[1:tonicLen] <- 0
  if(length(zScore) > 0) {
    # i=26
    for (i in tonicLen:length(zScore)) {
      preSlope <- slopeDat[i:(i-tonicLen+1)]
      # these will be unequal of the slope is not uniformly +
      # preSlope <- ifelse(preSlope < 1, 1, preSlope)
      if( sum(preSlope) != tonicLen ) {
        #set the zScore to 0 if not + slope for tonicSec period
        zScore[i] <- 0
      }
    }
  }
  
  #### remove zScore indices for non-ascending segments ####
  
  zScore[which(slopeDat != 1)] <- 0
  
  #### remove zScore indices during a latency period ####
  
  zScore[1:(301+(sChangeLat*cps)-1)] <- 0
  
  #### use a loop to locate the max z-Score in each run ####
  
  # first initialize some objects
  z <- 0
  zKeep <- NULL
  
  # iterate over the zScore vector,
  # to locate the max zScore in each run of non-zero values
  # and remove zScores for which the data are not,
  # ascending throughout the preDiff
  i=507
  for (i in 1:length(zScore)) {
    # increment the loop if the current value is zero
    if(zScore[i] == 0) next()
    # set the z holding variable to the non-zero i index
    z <- i
    # end iteration at the end of the zScore vector
    if(z==length(zScore)) next()
    # another loop to the max index for each non-zero run,
    # in the zScore vector
    # cannot use which.max() because there may be,
    # multiple non-zero runs in the zScore vector
    j=z+1
    for (j in (z+1):length(zScore)) {
      # stop the loop if the zScore value is zero
      if(zScore[j] == 0) {
        z <= j+1
        break()
      }
      # change the z index to advance the loop
      # if zScore[j] is greater than zScore[z]
      if(zScore[j] > zScore[z]) z <- j
    } # end inner loop
    
    # # keep the z index if the value is greater than the last zKeep value
    # if(is.null(zKeep)) {
    #   zKeep <- c(zKeep, z)
    # } else {
    #   # this the problem 4/30/2018
    #   if(zScore[z] > zScore[zKeep[length(zKeep)]]) zKeep <- c(zKeep, z)
    # }
    
    zKeep <- c(zKeep, z)
    
  } # end i loop
  
  # for safety
  zKeep <- unique(zKeep)
  
  # add the slope change onset indices to the output vector
  y[zKeep] <- 1
  
  # y is now a vector of 0s 
  # with the onset of significant changes in slope marked by 1
  
  #### output ####
  
  # use the idx input parameter to output the index of sig changes in + slope
  if(isTRUE(idx)) {
    # idx input parameter can be used to output only the indices where a response onset was imputed
    y <- which(y == 1)
  }
  
  return(y)
  
} # end maxSlopeChangeFn



