maxSlopeChangeFn <- function(x=tsData, nPr=nPre, nPo=nPost, aChng=aChange, dataRate=dataRate) {
  # new function to determine the max sig value in a series of significant change
  # in positive slope onset
  # supercedes the slopeChangeFn which selected the first sig value in a run
  # 11-24-2016 raymond nelson
  # called by the amplitudeExtractFn() function
  # useful to infer a response onset when the slope is already positive at stimulus onset
  # compare each nPre seconds with the next nPost seconds
  ###
  # x input is the vector of time series measurements
  # including the value 0 for negative slope and 0 slope changes,
  # including 10 prestimulus seconds and 10 additional seconds after the evaluation window
  # nPr is the number of pre change seconds
  # nPo is the number of post change seconds
  # aChng is the level of sig for the z test # currently .999
  # p=.999 will be +3.09 standard deviations
  # dataRate is data sampling rate # normally 30cps 
  ###
  # output is a vector of 0s the same length as input and including +1 change in slope energy,
  # located at the max zScore during a series of changes in nPre and nPo,
  # that exceed the aChng level of significance
  ###
  # compute the z score cutpoint from the aChng quantile
  zCut <- qnorm(aChng)
  # round the nPre and nPost segments to the nearest sample
  preLen <- round(dataRate*nPr,0)
  postLen <- round(dataRate*nPo,0)
  # calculate the difference for all values in the x input
  xDiff <- c(0, diff(x))
  # initialize some vectors
  preDiffMean <- rep(0, times=length(xDiff))
  preDiffSD <- rep(0, times=length(xDiff))
  postDiffMean <- rep(0, times=length(xDiff))
  zScore <- rep(0, times=length(xDiff))
  y <- rep(0, times=length(xDiff))
  # calculate all preDiff means
  for (i in preLen:length(xDiff)) {
    preDiffMean[i] <- mean(xDiff[(i-preLen+1):i], na.rm=TRUE)
  }
  # a private function to calculate the population standard deviation
  sdp <- function(x)(sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x)))
  # calculate all preDiff standard deviations
  for (i in preLen:length(xDiff)) {
    preDiffSD[i] <- sdp(xDiff[(i-preLen+1):i])
  }
  # calculate all postDiff means
  for (i in (preLen+1):(length(xDiff)-postLen+1)) {
    postDiffMean[i] <- mean(xDiff[i:(i+postLen-1)], na.rm=TRUE)
  } 
  # compute the z-score for all posDiff means compared to the preDiff means
  for (i in (preLen+1):(length(xDiff)-postLen+1)) {
    # calculate the zScore for each postDiff mean,
    # using the mean and SD from the previous sample
    zScore[i] <- (postDiffMean[i] - preDiffMean[(i-1)]) / preDiffSD[(i-1)]
  }
  # next determine which values are significant in the zScore vector
  zScore[which(zScore < zCut)] <- 0
  # then remove zScores for non-ascending segments
  slopeDat <- fillSlope(smoothSlope(slopeDir(x),ignore))
  zScore[which(slopeDat != 1)] <- 0
  # finally iterate over the zScore vector,
  # to locate the max zScore in each run of non-zero values
  # first initialize the z holding variables
  z <- 0
  zKeep <- NULL
  i=323
  for (i in 1:length(zScore)) {
    # increment the loop if the current value is zero
    if(zScore[i] == 0) next()
    # set the z holding variable to the non-zero i index
    z <- i
    # another loop to the max index for each non-zero run,
    # in the zScore vector
    # cannot use which.max() because there may be,
    # multiple non-zero runs in the zScore vector
    for (j in (z+1):length(zScore)) {
      # stop the for loop if the next value in the zScore vector is zero
      if(zScore[j] == 0) break()
      # change the z index if zScore[j] is greater than zScore[z]
      if(zScore[j] > zScore[z]) z <- j
    } # end inner loop
    # keep the z index if the value is greater than the last zKeep value
    if(is.null(zKeep)) {
      zKeep <- c(zKeep, z)
    } else {
      if(zScore[z] > zScore[zKeep[length(zKeep)]]) zKeep <- c(zKeep, z)
    }
  } # end for loop
  # at the slope change onset indices to the output vector
  y[zKeep] <- 1
  # y is now a vector of 0s with the onset of signficant changes in slope marked by 1
  # output the result
  return(y)
} # end maxSlopeChangeFn



