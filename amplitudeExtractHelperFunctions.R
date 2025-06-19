##### helper functions required by amplitudeExtract() #####
# August 18, 2015
# Raymond Nelson



## list of 11 helper functions in the amplitudeExtract() function

# slopeDir() to determine the + or - slope of a time series vector
# smoothSlope() to remove slope changes of less than n samples
# fillSlope() to fill a vector of slope changes to avoid 0 slope segments
# positiveSlope() to locate the positive slope segments
# negativeSlope() to locate the negative slope segments
# positiveOnset() to make a vector of 0 slope onset row indices
# maxSlopeChangeFn() to determine the max sig value in + slope segment
# slopePeak() to locate the peak of all positive slope segments
# descentProp() to exclude peak indices after the data descend a proportion
# sdp() to compute a population standard deviation
# tonicSlope() to calculate the slope value of a segment


# amplitudeIncreaseFn() # not present in this document


###############



# helper function to determine the positive or negative slope direction of a time series vector
slopeDir <- function(x=tsData) {
  # helper function to determine the positive or negative slope direction of a time series vector
  # called by the amplitudeExtractFn() function
  # input x is a vector of time series measurements
  # ouput is a vector of slope values for all values in the input vector
  x1 <- x
  diff1 <- diff(x)
  x1 <- ifelse(diff1==0,
               # ifelse is vectorized and does not require a control loop
               x1 <- 0,
               ifelse(diff1>0,
                      x1 <- 1,
                      x1 <- -1) )
  # return(c(x1, 0))
  return(c(0, x1))
} # end slopeDir function



# helper function to remove slope changes less than n samples
smoothSlope <- function(x=mySlope, nSmooth=ignore) {
  # helper function to remove time series slope changes of small duration
  # x is a time series vector of positive (1) negative (-1) and zero (0) slope changes
  # input is the output from the slopeDir() function
  # nSmooth is the number of samples to ignore small slope change segments
  # output is a vector of slope values including the run length after slope change
  # # a value of -51 will mean that there are 51 samples after slope onset or runLength=52
  ###### positive slope segments
  xPos <- ifelse(x == 1, 1, 0)
  # make an vector of zeros
  xPos1 <- rep(0, times=length(xPos))
  # make a run sum for all positive slope segements
  for (i in 2:length(xPos)) {
    if(xPos[i] != 0) xPos1[i] <- xPos[i] + xPos1[i-1] }
  # propagate the positive slope run length 
  # xPos2 <- xPos1
  for (j in length(xPos1):2) {
    if(xPos1[j] != 0 & xPos1[j-1] != 0) { xPos1[j-1] <- xPos1[j] } }
  # remove short runs
  # xPos3 <- xPos2
  for (k in 1:length(xPos1)) {
    if(xPos1[k] <= nSmooth) xPos1[k] <- 0 }
  ###### negative slope segments
  xNeg <- ifelse(x == -1, -1, 0)
  # make a vector of zeros
  xNeg1 <- rep(0, times=length(xNeg))
  # run sum for all negative slope segments
  for (l in 2:length(xNeg)) {
    if(xNeg[l] != 0) xNeg1[l] <- xNeg[l] + xNeg1[l-1] }
  # propagate the run length
  # xNeg2 <- xNeg1
  for (m in length(xNeg1):2) {
    if(xNeg1[m] != 0 & xNeg1[m-1] != 0) { xNeg1[m-1] <- xNeg1[m] } }
  # remove short runs
  # xNeg3 <- xNeg2
  for (o in 1:length(xNeg1)) {
    # use o so we do not abuse the n variable
    # uses >= -nSmooth instead of >= nSmooth because we are working on negative slope segments
    if(xNeg1[o] >= -nSmooth) xNeg1[o] <- 0 }
  ##### combine the positive and negative vectors
  x1 <- rep(0, times=length(x))
  x1 <- ifelse(xPos1 > 0, 
               x1 <- xPos1, 
               ifelse(xNeg1 < 0,
                      x1 <- xNeg1,
                      x1 <- 0) )
  return(x1)
} # end smoothSlope function



# helper function to fill the time series slope values to eliminate 0 slope segments
fillSlope <- function(x=mySlope1) {
  # helper function to fill the time series slope values to eliminate 0 slope segments
  # called by the amplitudeExtractFn() function
  # x is a vector of slope valences from which short runs are removed and set to 0
  # 0 slope segments following positive slope segments are filled positive
  # 0 slope segement following negative slope segments are filled negative
  # this will ensure that the + or - slope onset does not change
  # output is a smooth vector of slope changes that include no zero after the first sample
  x1 <- x
  for (i in 2:length(x)) { if(x[i] == 0) x1[i] <- x1[i-1] }
  x2 <- ifelse(x1 > 0,
               x2 <- 1,
               ifelse(x1 < 0,
                      x2 <- -1,
                      0) )
  return(x2)
} # end fillSlope function



# another helper function to find the positive slope segments
positiveSlope <- function(x=mySlope2) {
  # function to find the postiive slope segments
  # x is a vector of slope valences for each sample in the time series data
  # output is a vector of positive slope segments and zero slope for all other segments
  y <- x # to make a container
  # if else is vectorized and requires no control loop
  y <- ifelse(x>=1,
              y <- 1,
              y <- 0)
  return(y)  
} # end positiveSlope function



# helper function to locate the negative slope segments
negativeSlope <- function(x=mySlope2) {
  # function to find the negative slope segments
  # x is a vector of slope valences for each sample in the time series data
  # output is a vector of positive slope segments and zero slope for all other segments
  y <- x # to make a container
  # if else is vectorized and requires no control loop
  y <- ifelse(x<=-1,
              y <- -1,
              y <- 0)
  return(y)  
} # end negativeSlope function



# helper function to make a vector of positive slope onset row indices
positiveOnset <- function(x=posSlope) {
  # helper function to make a vector of positive slope onset row indices
  # called by the amplitudeExtractFn() function
  # also called by the slopeChangeFun() function
  # x input is a vector of positive and zero slopes
  # output is a vector of zeros that also includes the +1 onset for all positive slope segments
  xOnset <- ifelse(x==0,
                   # compare every x + next x > 0 
                   ifelse((x[1:(length(x)-1)] + x[2:length(x)]) > 0,
                          1,
                          0 ),
                   0 )
  # phase correction
  xOnset <- c(0, xOnset[1:(length(xOnset)-1)])
  return(xOnset)
} # end positiveOnset function



maxSlopeChangeFn <- function(x=tsData) {
  # new function to determine the max significant value 
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
  ###
  # x input is the vector of time series data for a stim segment
  # including 10 prestimulus seconds 
  # and 10 additional seconds after the evaluation window
  ### these parameters are set in the global environment by the init script
  # nPre is the number of pre change seconds
  # nPost is the number of post change seconds
  # aChange is the level of sig for the z test # currently .999
  # p=.999 will be +3.09 standard deviations
  # p=.9999 will be +3.72 standard deviations
  # p=.998650102 = 3 SD
  # 4 standard deviations is p=.9999683
  # cps is data sampling rate # normally 30cps 
  ###
  # output is a vector of 0s the same length as input 
  # and including +1 change in slope energy,
  # located at the max zScore during a series of sig changes in nPre and nPo,
  ####
  
  # tsData <- x
  
  # calculate the difference for all values in the x input
  # modified 10-7-2018 to use the absolute value
  xDiff <- c(0, abs(diff(x)))
  
  ###
  
  # use a function to compute the z score cutpoint from the aChange quantile
  zCut <- qnorm(aChange)
  
  # round the nPre and nPost segments to the nearest sample
  preLen <- round(cps*nPre,0)
  postLen <- round(cps*nPost,0)
  tonicLen <- round(cps*tonicSec,0)
  
  # initialize the output vector
  y <- rep(0, times=length(xDiff))
  
  # exit if the input vector is too short
  if(length(xDiff) <= (preLen+1)) return(y)
  
  ###
  
  # initialize some vectors
  preDiffMean <- rep(0, times=length(xDiff))
  preDiffSD <- rep(0, times=length(xDiff))
  postDiffMean <- rep(0, times=length(xDiff))
  zScore <- rep(0, times=length(xDiff))
  
  ###
  
  # calculate all preDiff means
  for (i in preLen:length(xDiff)) {
    preDiffMean[i] <- mean(xDiff[(i-preLen+1):i], na.rm=TRUE)
  }
  
  # calculate all preDiff standard deviations
  # uses a helper function sdp() to compute the population standard deviation
  for (i in preLen:length(xDiff)) {
    preDiffSD[i] <- sdp(xDiff[(i-preLen+1):i])
    # preDiffSD[i] <- sd(xDiff[(i-preLen+1):i])
  }
  
  # calculate all postDiff means
  for (i in (preLen+1):(length(xDiff)-postLen+1)) {
    postDiffMean[i] <- mean(xDiff[i:(i+postLen-1)], na.rm=TRUE)
  } 
  
  # compute the z-score for all posDiff means compared to the preDiff means
  for (i in (preLen+1):(length(xDiff)-postLen+1)) {
    # increment the loop if preDiffSD[(i-1)] == 0
    # to avoid NaN result when divide by zero
    if(preDiffSD[(i-1)] == 0) next()
    # calculate the zScore for each postDiff mean,
    # using the mean and SD from the previous sample
    zScore[i] <- (postDiffMean[i] - preDiffMean[(i-1)]) / preDiffSD[(i-1)]
  }
  
  # next determine which values are significant in the zScore vector
  zScore[which(zScore < zCut)] <- 0
  zScoreIdcs <- which(zScore != 0)
  
  # make a vector of slope data for the input
  slopeDat <- fillSlope(smoothSlope(slopeDir(x), 
                                    nSmooth=round(cps*ignoreTonicChange,0)))
  
  
  
  # 10-7-2018 remove zScore for which the tonicLen slope is not +
  if(length(zScoreIdcs) > 0) {
    # i=26
    for (i in 1:length(zScoreIdcs)) {
      preSlope <- zScoreIdcs[i]-tonicLen+1
      preSlope <- ifelse(preSlope < 1, 1, preSlope)
      if( sum(slopeDat[preSlope:zScoreIdcs[i]]) != tonicLen ) {
        zScore[zScoreIdcs[i]] <- 0
      }
    }
  }
  
  
  
  # then remove zScores for non-ascending segments
  zScore[which(slopeDat != 1)] <- 0
  
  # first initialize the holding variables
  z <- 0
  zKeep <- NULL
  
  # then finally iterate over the zScore vector,
  # to locate the max zScore in each run of non-zero values
  # and remove zScores for which the data are not,
  # ascending throughout the preDiff
  i=342
  for (i in 1:length(zScore)) {
    # increment the loop if the current value is zero
    if(zScore[i] == 0) next()
    # set the z holding variable to the non-zero i index
    z <- i
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
    
  } # end for loop
  
  zKeep <- unique(zKeep)
  # zScore[zKeep]
  
  # initialize the output vector
  # y <- rep(0, times=length(xDiff))
  
  # add the slope change onset indices to the output vector
  y[zKeep] <- 1
  # y is now a vector of 0s 
  # with the onset of signficant changes in slope marked by 1
  
  # output the result
  return(y)
  
} # end maxSlopeChangeFn



# helper function to locate the peak of all positive slope segments 
slopePeak <- function(x=mySlope2) {
  # helper function to locate the peak of all positive slope segments
  # called by the amplitudeExtractFn() function
  # x input is a vector of smoothed slope values not including zero
  # output is a vector of 0s including only +1 slope peak points 
  xPeak <- x
  xPeak <- ifelse(x == 1,
                  ifelse((x[1:(length(x)-1)] + x[2:length(x)]) == 2,
                         xPeak <- 0,
                         xPeak <- 1),
                  xPeak <- 0)
  return(xPeak)
} # end slopePeak function



# helper function to exclude peak row indices after the data descend more than a proportion p from the previous highest peak
descentProp <- function(x=xOnset[1], y=xPeakLoop, z=tsData, dProp=descProp) {
  # helper function to exclude peak row indices after the data descend more than a proportion p from the previous highest peak
  # called by the amplitudeExtractFn() function
  # from a the previous highest peak after response onset
  # this function is called iteratively in a a loop that selects the max change,
  # from each onset value to subsequent peak values
  #
  # input
  # x is a variable for a single reponse onset row index (x is a scalar)
  # y is a is a vector of peak indices after onset and prior to decsent below the onset value
  # z is a vector of time series values
  # dProp is a proportion for which the data are evaluated 
  # ROWEnd is the end of the response onset window (typically 5 seconds after the verbal answer)
  # 
  # output
  # output is a stop row with the row index for the time series input, 
  # after which the data have descended more than a proportion dProp,
  # from the previous max peak - onset value
  # 
  #### 
  yChangeOnset <- x
  xPeakLoop <- y
  tsData <- z
  # get the time series values for all peak points in xPeakLoop
  xPeakLoopValues <- tsData[xPeakLoop]
  # compute the difference for each peak - response onset value
  xPeakLoopDiffs <- xPeakLoopValues - tsData[yChangeOnset]
  # determine the cut value for each peak in the xPeakLoopDiffs vector
  cutValues <- (1-dProp) * xPeakLoopDiffs
  # get the vector of time series values after response onset
  
  # this may be a problem 12-4-2016 
  tsValues <- tsData[(yChangeOnset+1):length(tsData)]
  # ROWEndRow is obtained from the parent env
  # tsValues <- tsData[(ROWEndRow+1):length(tsData)] # 10-15-2015 to apply the 50% rule after ROWEnd
  
  # compute the difference for all time series values after response onset
  tsDiffs <- tsValues - tsData[yChangeOnset]
  # make a vector of the slope direction for all time series samples after response onset
  # ifelse is vectorized when the result is taken from the ifelse function (not vectorized within ifelse) 
  tsSlope <- c( 0, ifelse( diff(tsValues)>0, 
                           1, 
                           ifelse(diff(tsValues)<0, 
                                  -1, 
                                  0) ) )
  # initialize a cutVector 
  # to look for descending tsValues that are less than the cutValues for previous peaks
  cutVector <- numeric(length=length(tsDiffs))
  # then populate the cutVector by iterating over the time series from the yChange Onset to end
  i=1
  for (i in 1:length(tsDiffs)) {
    # first make a scalar for the current row index 
    currentRow <- i + yChangeOnset - 1
    # make a vector of myCutValues for the vector tsDiffs
    # check the diff from the preceeding max peak
    cutVector[i] <- ifelse( length(which(xPeakLoop <= currentRow)) > 0,
                            max(cutValues[which(xPeakLoop <= currentRow)]),
                            0 )
    # ignore the first several descending rows to avoid over sensitivity to high frequency noise
    cutVector <- c(rep(0, times=90), cutVector[91:length(cutVector)])
  } # end for loop to make the cutVector
  # for each item in tsDiffs, cutVector now has the cutValues for the previous maximum Peak Value
  # if the value in tsDiffs is less than the corresponding value in cutVector
  # then the data have descended more than the cutoff proportion
  # print(cutVector)
  # use the tsSlope vector to locate descending slope segments
  descRows <- which(tsSlope == -1)
  # determine the descending row indices for which 
  # the amount of descent is smaller than the corresponding index in cutVector
  cutRows <- descRows[ tsDiffs[descRows] < cutVector[descRows] ]
  # use the first row index smaller than cutVector as the stopRow after which data are not used
  stopRow2 <- cutRows[1] + yChangeOnset - 1
  # use the last row in the data vector if stopRow2 is NA
  if(is.na(stopRow2) == TRUE) stopRow2 <- length(tsData)
  # return a scalar with the stop row after which peaks are excluded 
  return(stopRow2)
} # end descentProp function


sdp <- function(x) {
  # function to compute a population standard deviation
  # uses the R sample variance function
  # x input is a vector of numeric values
  # output is the population standard deviation
  (sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x)))
}



##### end of amplitude extract helper functions #####



tonicSlope <- function(x=(tsData+2000), time=1, period=(1/cps), rate=cps, gain=1000) {
	# function to calculate the slope value of a segment
	# 3/30/2018
	###
	# rise / run * 100 
	# assumes a Y space of 1000
	###
	# x input is the time series data
	# add 2000 to the x to ensure that all values are > 0
	# time is the moving evaluation period in seconds
	# period is the larger time period at which a segment is deemed to be tonic
	# rate is the cps value from the global env, set in the NCCAASCII_init.R script
	###
	# EDA data or cardio mid line, diastolic line, or systolic line
	# output is a vector of slope values with length equal to the input
	# these values can be checked for tonicity after response onset
	###
	# initialize the output vector
	outputVector <- rep(0, times=length(x))
	# iterate over the data to compute the slope
	for (i in 2:length(x)) {
		 outputVector[i] <- (x[i] - x[(i-1)]) * gain
	}
	return(outputVector)
}


getSlope <- function(x=tsData, nSmooth=nSmooth) {
  
  rep(0, times=length(tsData))
  # theSlope <- ifelse(diff1==0,
  #                    x1 <- 0,
  #                    # ifelse is vectorized and does not require a control loop
  #                    ifelse(diff1>0,
  #                           x1 <- 1,
  #                           x1 <- -1) )
  
  
  # xPos <- ifelse(x1 == 1, 1, 0)
  # # make an vector of zeros
  # xPos <- rep(0, times=length(xPos))
  # # make a run sum for all positive slope segements
  # for (i in 2:length(xPos)) {
  #   if(xPos[i] != 0) xPos1[i] <- xPos[i] + xPos1[i-1] }
  # # propagate the positive slope run length 
  # # xPos2 <- xPos1
  # for (j in length(xPos):2) {
  #   if(xPos[j] != 0 & xPos[j-1] != 0) { xPos[j-1] <- xPos[j] } }
  # # remove short runs
  # # xPos3 <- xPos2
  # for (k in 1:length(xPos)) {
  #   if(xPos[k] <= nSmooth) xPos[k] <- 0 }
  # ###### run sum for all negative slope segments
  # xNeg <- ifelse(x1 == -1, -1, 0)
  # # make a vector of zeros
  # xNeg <- rep(0, times=length(xNeg))
  # # run sum for all negative slope segments
  # for (l in 2:length(xNeg)) {
  #   if(xNeg[l] != 0) xNeg1[l] <- xNeg[l] + xNeg1[l-1] }
  # # propagate the run length
  # # xNeg2 <- xNeg1
  # for (m in length(xNeg):2) {
  #   if(xNeg[m] != 0 & xNeg[m-1] != 0) { xNeg[m-1] <- xNeg[m] } }
  # # remove short runs
  # # xNeg3 <- xNeg2
  # for (o in 1:length(xNeg)) {
  #   # use o so we do not abuse the n variable
  #   # uses >= -nSmooth instead of >= nSmooth because we are working on negative slope segments
  #   if(xNeg[o] >= -nSmooth) xNeg[o] <- 0 }
  # ### combine the positive and negative vectors
  # x2 <- rep(0, times=length(x1))
  # x2 <- ifelse(xPos > 0, 
  #              x2 <- xPos, 
  #              ifelse(xNeg < 0,
  #                     x2 <- xNeg,
  #                     x2 <- 0) )
  
  
  # for (i in 2:length(x2)) { if(x2[i] == 0) x2[i] <- x2[i-1] }
  # x3 <- rep(0, times=length(x2))
  # x3 <- ifelse(x2 > 0,
  #              x3 <- 1,
  #              ifelse(x2 < 0,
  #                     x3 <- -1,
  #                     0) )
  
  return(x3)
  
  
}

# fillSlope(smoothSlope(x=slopeDir(x=tsData), nSmooth=nSmooth))



