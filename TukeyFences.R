# helper function to compute the Tukey Fences for time series data


#########

# Value
# Mean1S
# Variance1S
# Diff
# MeanDiff1S
# VarDiff1S
# AbsDiff
# MeanAbsDiff1S
# VarAbsDiff1S
# MedAbsDev1S
# IQR1S


tukeyFence <- function(x1, x2, z=10, buffer=60, inner=4.5, outer=3, len=1, cps=30) {
  # function to compute the tukey fences for 2 time series inputs
  # x1 is one time series input
  # x2 is the second timeseries input
  # z is the length of the observation window in seconds
  # cps is the data rate in samples per second
  # inner is the number of IQRs for the inner fence
  # outer is the number of IQRs for the outer fence
  # b is a buffer of samples between the observation window and evaluation sample
  # len is the number of samples to mark the time series data as artifacted
  # 
  # input time series will be the smoothed average of the recorded time series data
  # and the interpolated upper peak line or lower peak line
  # upper and lower peaks are the change from positive to negative slope
  # or negative to positive slope in the recorded time series data
  #
  # tukeyFence1 function works by first calculating the distance
  # between the 2 times series inputs.
  # then it will evaluate the IQR for an observation window
  # of an arbitrary length z.
  # and then compare the median distance for each successive sample 
  # after ignoring a buffer of an arbitrary length b.
  # samples that are outside the fences are marked as outliers
  # Tukey Fences have an upper and lower fence, 
  # along with an inner and outer fence
  # 
  # artifacts should be identified in the time series data 
  # when the distance between the two time series inputs exceeds
  # an upper or lower fence, such as when the upper or lower peaks of the 
  # pneumo time series move significantly closer to or further from
  # 
  # output is a time series vector of empty values where no artifact was observed
  # and an X where artifacts were observed
  # artifacts will be marked in the output time series from the onset sample
  # for a duration in seconds specified by the input paramenter "len"
  # the output time series will be the same length as the input
  #
  ##############################
  
  # # set the length of the buffer
  # n <- z * cps
  # #
  # ifelse(y=="max", peakVector <- maxPeak(x),
  #                      ifelse(y=="both", peakVector <- minMaxPeak(x),
  #                             peakVector <- minPeak(x) ) )
  # minMax <- minMaxPeak(x) # need this for the tukey fence
  # 
  # 
  # # inputBuffer <- x[1:(n-1)] # not needed 10-7-15
  
  # first get the distance between the two input vectors
  inputDistance <- x1-x2

  # make the output vector
  xOut <- rep("", times=length(inputDistance))
  # xOut <- NULL
  
  
  # make the observation window
  lastSample <- z*cps
  obsWindow <- inputDistance[1:lastSample]
  
  # get the starting sample at which to start comparing each sample to the buffer
  checkSample <- lastSample + buffer
  
  # loop over the inputDistance vector
  for (i in (lastSample + buffer + 1):length(inputDistance)) { 
    # Get the median and the interquartile range for the input buffer
    # inputMedian <- median(obsWindow)
    inputIQR <- IQR(obsWindow)
    # and the first and third quartiles
    input1stQ <- summary(obsWindow)[2]
    input3rdQ <- summary(obsWindow)[5]
    # set the lower and upper Tukey Fences
    lowerFence <- input1stQ - (inner * inputIQR)
    upperFence <- input3rdQ + (inner * inputIQR)
    #
    # mark the artifact if the data are outside the Tukey fences
    if(inputDistance[checkSample] >= upperFence | inputDistance[i] <= lowerFence) {
      xOut[checkSample] <- "X"
    }
    #
    # update the observation Window 
    lastSample <- lastSample + 1
    checkSample <- checkSample + 1
    obsWindow <- c(obsWindow[2:length(obsWindow)], inputDistance[(i-buffer)])
  } # end for loop
  
  #return(which(xOut=="Y"))
  return(xOut[1:length(inputDistance)])
} # end tukeyFence() function



# xOut <- tukeyFence1(x1=chartDF$c_UPneumoMid, 
#                     x2=chartDF$c_UPneumoExh,
#                     z=10,
#                     buffer=90,
#                     inner=3,
#                     outer=4.5,
#                     len=0,
#                     cps=30)
# 
# 
# 
# which(xOut=="X")


###############




tukeyFence1 <- function(x=chartDF$c_PL, y=myMaxPeaks, z=5) {
  # function to exclude minPeak and maxPeak points that do not exceed the inner Tukey Fence
  # x is the time series data
  # y is the minPeak or maxPeak vector, or a minMaxPeak vector
  # z is the measurement window in seconds
  n <- round(z * cps,0)
  # xOut <- NULL
  # xOut length is the length of all y less than length(x)-n
  xOut <- rep(NA, times=length(which(y[y<(length(x)-n)])))
  # for (i in 1:length(y[y<length(x)])) {
  for (i in 1:length(xOut)) {
      # mananage the end of the time series and the inputBuffer
#     if((y[i]) > (length(x)-n)) {
#       xOut <- c(xOut, y[i])
#       next 
#     }
    # load the inputBuffer
    inputBuffer <- x[(y[i]):(y[i]+n-1)]
    # set the upper and lower fences
    tukeyUpper <- median(inputBuffer)+(1.5*IQR(inputBuffer))
    tukeyLower <- median(inputBuffer)-(1.5*IQR(inputBuffer))
    # evalutate the min and max peak
    if(x[y[i]] <= tukeyLower) { xOut <- c(xOut, y[i]) }
    if(x[y[i]] >= tukeyUpper) { xOut <- c(xOut, y[i]) }
  } # end for loop
  xOut <- c(1, xOut, length(myPLE$c_PL))
  return(xOut)
} # end tukeyFence1 function()


tukeyFence1a <- function(x, y="min", z=5, inner=1.5) {
  # function to exclude minPeak and maxPeak points that do not exceed the inner Tukey Fence
  # x is the time series data
  # y is the minPeak or maxPeak vector - or a minMaxPeak vector
  # z is the measurement window in seconds
  # output is a vector of peak indexes including first and last
  ###
  # set the length of the buffer
  n <- z * cps
  #
  ifelse(y=="max", peakVector <- maxPeak(x),
                       ifelse(y=="both", peakVector <- minMaxPeak(x),
                              peakVector <- minPeak(x) ) )
  minMax <- minMaxPeak(x) # need this for the tukey fence
  # inputBuffer <- x[1:(n-1)] # not needed 10-7-15
  xOut <- NULL
  for (i in 1:length(peakVector[peakVector<length(x)])) {
    # mananage the end of the time series and the inputBuffer
    if((peakVector[i]) > (length(x)-2*n)) {
      xOut <- c(xOut, peakVector[i])
      next
    }
    # initialize the inputBuffer
    # inputBuffer <- x[(peakVector[i]):(peakVector[i]+n-1)]
    inputBuffer <- x[peakVector[i:(i+2)]]
    # set the upper and lower fences
    mySummary <- summary(inputBuffer) # to get the quartiles
    # set the tukey fences
    tukeyUpper <- median(inputBuffer)+(inner*IQR(inputBuffer))
    tukeyLower <- median(inputBuffer)-(inner*IQR(inputBuffer))
    # evalutate the min and max peak
    if(x[peakVector[i]] <= tukeyLower) { xOut <- c(xOut, peakVector[i]) }
    if(x[peakVector[i]] <= min(inputBuffer)) { xOut <- c(xOut, peakVector[i]) }
    if(x[peakVector[i]] >= tukeyUpper) { xOut <- c(xOut, peakVector[i]) }
    # if(x[peakVector[i]] >= max(inputBuffer)) { xOut <- c(xOut, peakVector[i]) }
  } # end for loop
  xOut <- c(1, xOut, length(x))
  return(xOut)
} # end tukeyFence1 function


tukeyFence2 <- function(x, y, z=5, output="index", fence="both") {
  # function to exclude minPeak and maxPeak points that do not exceed the inner Tukey Fence
  # x is the time series data vector
  # y is another time series data vector of the same length as x1
  # z is the measurement window in seconds
  # output = "vector" will return a vector of the same length as x
  # output = "index" will return the indices of outliers
  # fence = "both" will compute outliers using both upper and lower Tukey fences
  # fence = "upper" or "lower" will compute only the upper or lower outliers
  if(length(x)!=length(y)) stop("error: unequal length input vectors")
  n <- round(z * cps,0)
  diffVector <- x - y
  xOut <- rep("", times=length(x))
  for (i in n:length(xOut)) {
    # load the inputBuffer
    inputBuffer <- diffVector[i:(i-n+1)]
    # set the upper and lower fences
    if(fence!="lower") {
    tukeyUpper <- median(inputBuffer)+(2*IQR(inputBuffer))
    if(diffVector[i] >= tukeyUpper) xOut[i] <- "Artifact"
    }
    if(fence!="upper") {
    tukeyLower <- median(inputBuffer)-(2*IQR(inputBuffer))
    if(diffVector[i] <= tukeyLower) xOut[i] <- "Artifact"
    }
      } # end for loop
  if(output=="index") { xOut <- which(xOut=="Artifact") }
  return(xOut)
} # end tukeyFence2() function


tukeyFence3 <- function(x, z=5, output="index", fence="both") {
  # function to exclude minPeak and maxPeak points 
  # that do not exceed the inner Tukey Fence
  # x is the time series data vector
  # z is the measurement window in seconds
  # output = "vector" will return a vector of the same length as x
  # output = "index" will return the indices of outliers
  # fence = "both" will compute outliers using both upper and lower Tukey fences
  # fence = "upper" or "lower" will compute only the upper or lower outliers
  n <- round(z * cps,0)
  xOut <- rep("", times=length(x))
  for (i in n:length(xOut)) {
    if(is.na(x[i])) next()
    # load the inputBuffer
    inputBuffer <- x[i:(i-n+1)]
    if(is.na(IQR(inputBuffer, na.rm=TRUE))) next()
    # set the upper and lower fences
    if(fence != "lower") {
      tukeyUpper <- median(inputBuffer, na.rm=TRUE)+(2*IQR(inputBuffer, na.rm=TRUE))
      if(x[i] >= tukeyUpper) xOut[i] <- "Artifact"
    }
    if(fence != "upper") {
      tukeyLower <- median(inputBuffer, na.rm=TRUE)-(2*IQR(inputBuffer, na.rm=TRUE))
      if(x[i] <= tukeyLower) xOut[i] <- "Artifact"
    }
  } # end for loop
  if(output=="index") { xOut <- which(xOut=="Artifact") }
  return(xOut)
} # end tukeyFence3() function

# tukeyFence3(x=EDAData,z=3,output="index",fence="lower")


tukeyFence4 <- function(x, y, z=5, output="index", fence="both") {
  # function to exclude minPeak and maxPeak points 
  # that do not exceed the inner Tukey Fence
  # x is the time series data vector
  # z is the measurement window in seconds
  # output = "vector" will return a vector of the same length as x
  # output = "index" will return the indices of outliers
  # fence = "both" will compute outliers using both upper and lower Tukey fences
  # fence = "upper" or "lower" will compute only the upper or lower outliers
  if(length(x)!=length(y)) stop("error: unequal length input vectors")
  n <- round(z * cps,0)
  xOut <- rep("", times=length(x))
  for (i in n:length(xOut)) {
    if(is.na(x[i])) next()
    # load the inputBuffer
    inputBuffer <- y[i:(i-n+1)]
    if(is.na(IQR(inputBuffer, na.rm=TRUE))) next()
    # set the upper and lower fences
    if(fence != "lower") {
      tukeyUpper <- median(inputBuffer, na.rm=TRUE)+(2*IQR(inputBuffer, na.rm=TRUE))
      if(x[i] >= tukeyUpper) xOut[i] <- "Artifact"
    }
    if(fence != "upper") {
      tukeyLower <- median(inputBuffer, na.rm=TRUE)-(2*IQR(inputBuffer, na.rm=TRUE))
      if(x[i] <= tukeyLower) xOut[i] <- "Artifact"
    }
  } # end for loop
  if(output=="index") { xOut <- which(xOut=="Artifact") }
  return(xOut)
} # end tukeyFence4() function

# tukeyFence4(x=EDAData,y=chartDF$c_AutoEDA,z=3,output="index",fence="lower")


tukeyFence5 <- function(x=inputVector, 
                        buffer=4,
                        bufferStop=2,
                        side="both", 
                        fence="both", 
                        innerFence=1.5, 
                        outerFence=3, 
                        expVal=1, 
                        scaleVal=1, 
                        output="name") {
  # function to compute outliers using the Tukey Fence method
  # 5/11/2016
  # Raymond Nelson
  # x is an in put vector of values equal the number of rows in the time series data for a chart
  # buffer length to set the number of samples use instead of the length of the input vector
  # bufferStop is the number of samples to stop prior to the i value
  # side is a parameter that can specify "both" "upper" or "lower"
  # fence is a parameter that can specify "both" "inner" or "outer"
  # inner fence is typically 25thPercentile - 1.5*IQR and 75thPercentile + 1.5*IQR
  # outer fence is typically 25thPercentile - 3*IQR and 75thPercentile + 3*IQR
  # expVal is a numerical value for exponential transformation of the input vector
  # scaleVal is numerical value for linear scaling of the input vector
  # output parameter uses "name" or "logical" or "range"
  # "name" outputs the name of the fence that
  #
  # output is a vector of NA values the same length as x with row indices,
  # including artifact flags for values that exceed the Tukey fences
  
  ###
  
  x <- x * scaleVal
  
  # this allows any exponent value
  x <- sign(x) * abs(x)^expVal

  # check the length of the input 
  if(buffer > length(x)) buffer <- length(x)
  if(is.null(buffer)) buffer <- length(x)
  
  # initialize the output vector
  xOut <- rep(NA, times=length(x))
  
  # use a loop and sliding window if buffer is < length of the input Vector
  if(buffer >= length(x)) {
    # get the interquartile range
    q25 <- quantile(x, .25, na.rm=TRUE) 
    q75 <- quantile(x, .75, na.rm=TRUE)
    iqRange <- q75 - q25
    # set the Tukey Fences
    innerLower <- q25 - innerFence*iqRange
    innerUpper <- q75 + innerFence*iqRange
    outerLower <- q25 - outerFence*iqRange
    outerUpper <- q75 + outerFence*iqRange
    # iterate over the input vector
    i=173
    for(i in 1:length(x)) {
      # locate any signficant activity 
      # by comparing each item to the buffer of previous items
      if(!is.na(x[i])) {
        # compute the result for each sample in x
        
        # do not use ELSE because it will not work properly
        if(side=="lower" | side=="both") {
          if(fence=="inner" | fence=="both") {
            if(x[i] <= innerLower) xOut[i] <- ifelse(output=="range", 
                                                     (x[i]-q25) / iqRange, 
                                                     "innerLower")
          }
          if(fence=="outer" | fence=="both") {
            if(x[i] <= outerLower) xOut[i] <- ifelse(output=="range", 
                                                     (x[i]-q25) / iqRange, 
                                                     "outerLower")
          }
        }
        if(side=="upper" | side=="both") {
          if(fence=="inner" | fence=="both") {
            if(x[i] >= innerUpper) xOut[i] <- ifelse(output=="range", 
                                                     (x[i]-q25) / iqRange, 
                                                     "innerUpper")
          }
          if(fence=="outer" | fence=="both") {
            if(x[i] >= outerUpper) xOut[i] <- ifelse(output=="range", 
                                                     (x[i]-q25) / iqRange, 
                                                     "outerUpper")
          }
        }
        
      } # end if !is.na(x[i])
    } # end for loop
    # end if buffer == length(x)
  } else {
    # use a different loop if buffer < length(x)
    
    loopStart <- buffer+1
    
    i=loopStart
    for(i in loopStart:length(x)) {
      # locate any signficant activity 
      # by comparing each item to the buffer of previous items
      if(is.na(x[i])) next()
      # initialize the buffer
      bufferStart <- i - buffer
      bufferStop <- i - bufferStop
      # commented out 4-3-2017
      # bufferStop <- i - 1
      
      
      xBuffer <- x[bufferStart:bufferStop]
      # get the interquartile range
      q25 <- quantile(xBuffer, .25, na.rm=TRUE) 
      q75 <- quantile(xBuffer, .75, na.rm=TRUE)
      if(is.na(q25) | is.na(q75)) next()
      iqRange <- q75 - q25
      if(iqRange == 0) next()
      # set the Tukey Fences
      innerLower <- q25 - innerFence * iqRange
      innerUpper <- q75 + innerFence * iqRange
      outerLower <- q25 - outerFence * iqRange
      outerUpper <- q75 + outerFence * iqRange
      
      # compute the result for each sample in x
      
      # do not use ELSE because it will not work properly
      if(side=="lower" | side=="both") {
        if(fence=="inner" | fence=="both") {
          if(x[i] <= innerLower) xOut[i] <- ifelse(output=="range", 
                                                   (x[i]-q25) / iqRange, 
                                                   "innerLower")
        }
        if(fence=="outer" | fence=="both") {
          if(x[i] <= outerLower) xOut[i] <- ifelse(output=="range", 
                                                   (x[i]-q25) / iqRange, 
                                                   "outerLower")
        }
      }
      if(side=="upper" | side=="both") {
        if(fence=="inner" | fence=="both") {
          if(x[i] >= innerUpper) xOut[i] <- ifelse(output=="range", 
                                                   (x[i]-q25) / iqRange, 
                                                   "innerUpper")
        }
        if(fence=="outer" | fence=="both") {
          if(x[i] >= outerUpper) xOut[i] <- ifelse(output=="range", 
                                                   (x[i]-q25) / iqRange, 
                                                   "outerUpper")
        }
      }
      
    } # end for loop
  } # end else
  
  xOut[c(1,length(xOut))] <- NA
  
  if(output=="logical") { 
    xOut[!is.na(xOut)] <- TRUE
    xOut[is.na(xOut)] <- FALSE
    xOut <- as.logical(xOut)
  }
  
  return(xOut)
  
} # end tukeyFence5() function


# maxPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLenFn(ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",dataRate=cps,lowPass=TRUE)))
# inputVector <- chartDF$c_Cardio1[maxPeaks] - chartDF$c_CardioMid[maxPeaks]
# tukeyFence5Output <- tukeyFence5(x=inputVector, buffer=10, side="both", fence="both", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1)
# maxPeaks[which(!is.na(tukeyFence5Output))]
# tukeyFence5Output[which(!is.na(tukeyFence5Output))]




#####


# # get the time series data into a data frame for plotting
# myPLE <- as.data.frame(chartDF$c_PL[1:600])
# names(myPLE) <- "c_PL"

# myMinPeaks <- minPeak(myPLE[,1])
# myMaxPeaks <- maxPeak(myPLE[,1])
# # myMinPeakVals <- myPLE$c_PL[myMinPeaks]
# # myMaxPeakVals <- myPLE$c_PL[myMaxPeaks]

# keepMin <- tukeyFence1(x=myPLE$c_PL, y="min")
# keepMax <- tukeyFence1(x=myPLE$c_PL, y="min")

# keepMinVals <- myPLE[keepMin,1]
# keepMaxVals <- myPLE[keepMax,1]

# # keepMinVals <- keepMinVals - myPLE[1,1]
# myMinInterp <- interpolatePeaks(x=keepMin, y=keepMinVals)
# myMaxInterp <- interpolatePeaks(x=keepMax, y=keepMaxVals)
# # myMinInterp <- myMinInterp + myPLE[1]

# g <- ggplot()
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myPLE$c_PL), color="blue", size=.75) + coord_cartesian(ylim=c(150, -150))
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myMinInterp))
# g <- g + geom_line(aes(x=(1:nrow(myPLE)), y=myMaxInterp))
# print(g)



tukeyFence6 <- function(x=first.x, y=second.x, buffer=10, side="both", fence="both", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1) {
  # function to compute a vectorized tukey fence comparison with 2 input vectors
  # for split half comparisons
  # 5/21/2016
  # Raymond Nelson
  # x is an input vector of measured values
  # y in a second input vector
  # fence is a parameter that can specify "both" "inner" or "outer"
  # side is a parameter that can specify "both" "upper" or "lower"
  # inner fence is typically 25thPercentile - 1.5*IQR and 75thPercentile + 1.5*IQR
  # outer fence is typically 25thPercentile - 3*IQR and 75thPercentile + 3*IQR
  # expVal is a numerical value for exponential transformation of the input vector
  # scaleVal is numerical value for linear scaling of the input vector
    # output is a vector of NA values the same length as x with row indices 
  ###
  x <- x * scaleVal
  x <- x^expVal
  y <- y * scaleVal
  y <- y^expVal
  
  q25.x <- quantile(x, .25)
  q75.x <- quantile(x, .75)
  iq.range.x <- q75.x - q25.x
  
  q25.y <- quantile(y, .25)
  q75.y <- quantile(y, .75)
  iq.range.y <- q75.y - q25.y 
  
  inner.lower.x <- q25.x - inner.fence * iq.range.x
  inner.upper.x <- q75.x + inner.fence * iq.range.x
  outer.lower.x <- q25.x - outer.fence * iq.range.x
  outer.upper.x <- q75.x + outer.fence * iq.range.x

  inner.lower.y <- q25.y - inner.fence * iq.range.y
  inner.upper.y <- q75.y + inner.fence * iq.range.y
  outer.lower.y <- q25.y - outer.fence * iq.range.y
  outer.upper.y <- q75.y + outer.fence * iq.range.y
  
  which(x <= inner.lower.y)
  which(x >= inner.upper.y)
  
  which(y <= inner.lower.x)
  which(y >= inner.upper.x)
  
  return(xOut)
} # end tukeyFence6() function


# inputVector <- chartDF$c_SEProc
tukeyFence7 <- function(x=inputVector, buffer=3, side="both", fence="both", innerFence=1.5, outerFence=3, expVal=1, scaleVal=1) {
  # function to compute outliers using the Tukey Fence method
  # 5/30/2016
  # Raymond Nelson
  # similar to the tukeyFence5() function but the output is a vector of quantiles
  # x is an in put vector of values
  # buffer length to set the number of samples use instead of the length of the input vector
  # side is a parameter that can specify "both" "upper" or "lower"
  # fence is a parameter that can specify "both" "inner" or "outer"
  # inner fence is typically 25thPercentile - 1.5*IQR and 75thPercentile + 1.5*IQR
  # outer fence is typically 25thPercentile - 3*IQR and 75thPercentile + 3*IQR
  # expVal is a numerical value for exponential transformation of the input vector
  # scaleVal is numerical value for linear scaling of the input vector
  # output is a vector of NA values the same length as x with row indices,
  # including artifact flags for values that exceed the Tukey fences
  ###
  
  x <- x * scaleVal
  
  # this allows any exponent value
  x <- sign(x) * abs(x)^expVal
  # x <- sign(x) * log1p(abs(x)+1)
  
  # initialize the output vector
  xOut <- rep(NA, times=length(x))
  
  # check the length of the input and buffer
  if(buffer>length(x)) buffer <- length(x)
  if(is.null(buffer)) buffer <- length(x)
  
  if(buffer>=length(x)) {
    # get the interquartile range
    q25 <- quantile(x, .25, na.rm=TRUE) 
    q75 <- quantile(x, .75, na.rm=TRUE)
    iqRange <- q75 - q25
    # set the Tukey Fences
    innerLower <- q25 - innerFence*iqRange
    innerUpper <- q75 + innerFence*iqRange
    outerLower <- q25 - outerFence*iqRange
    outerUpper <- q75 + outerFence*iqRange
    for(i in 1:length(x)) {
      # locate any signficant activity by comparing each item to the buffer of previous items
      # if(side=="lower" | side=="both") {
      #   if(fence=="inner" | fence=="both") {
      #     if(x[i] <= innerLower) xOut[i] <- { (x[i]-q25) / iqRange }
      #   }
      #   if(fence=="outer" | fence=="both") {
      #     if(x[i] <= outerLower) xOut[i] <- { (x[i]-q25) / iqRange }
      #   }
      # }
      
      # if(side=="upper" | side=="both") {
      #   if(fence=="inner" | fence=="both") {
      #     if(x[i] >= innerUpper) xOut[i] <- { (x[i]-q75) / iqRange }
      #   }
      #   if(fence=="outer" | fence=="both") {
      #     if(x[i] >= outerUpper) xOut[i] <- { (x[i]-q75) / iqRange }
      #   }
      # }
      
      xOut[i] <- (x[i]-q25) / iqRange
    } # end for loop
    # end if buffer == length(x)
  } else if(buffer < length(x)) {
    # use a loop and sliding window if buffer is < length of the input Vector
    loopStart <- buffer+1
    for(i in loopStart:length(x)) {
      xBuffer <- x[(i-buffer):(i-1)]
      # get the interquartile range
      q25 <- quantile(xBuffer, .25) 
      q75 <- quantile(xBuffer, .75)
      iqRange <- q75 - q25
      # set the Tukey Fences
      innerLower <- q25 - innerFence*iqRange
      innerUpper <- q75 + innerFence*iqRange
      outerLower <- q25 - outerFence*iqRange
      outerUpper <- q75 + outerFence*iqRange
      # locate any signficant activity by comparing each item to the buffer of previous items
      
      # if(side=="lower" | side=="both") {
      #   if(fence=="inner" | fence=="both") {
      #     if(x[i] <= innerLower) xOut[i] <- { (x[i]-q25) / iqRange }
      #   }
      #   if(fence=="outer" | fence=="both") {
      #     if(x[i] <= outerLower) xOut[i] <- { (x[i]-q25) / iqRange }
      #   }
      # }
      
      # if(side=="upper" | side=="both") {
      #   if(fence=="inner" | fence=="both") {
      #     if(x[i] >= innerUpper) xOut[i] <- { (x[i]-q75) / iqRange }
      #   }
      #   if(fence=="outer" | fence=="both") {
      #     if(x[i] >= outerUpper) xOut[i] <- { (x[i]-q75) / iqRange }
      #   }
      # }
      
      xOut[i] <- (x[i]-q25) / iqRange
      
    } # end for loop
    
  } # end if
  
  xOut[c(1,length(xOut))] <- NA
  
  return(xOut)
  
} # end tukeyFence7() function


tukeyFence8 <- function(x=diffVector, y=6, z=2, fence="both", out="vector", fenceVal=9) {
  # function to identify  minPeak and maxPeak points that exceed the Tukey Fences
  # x is the time series data vector
  # y is the measurement window in seconds
  # z is a a latency period
  # fence = "both" will compute outliers using both upper and lower Tukey fences
  # fence = "upper" or "lower" will compute only the upper or lower outliers
  # out = "vector" will return a vector of the same length as x
  # out = "index" will return the indices of outliers
  #########
  # calculate the segment length
  segLength <- round(y * cps, 0)
  latLength <- round(z * cps, 0)
  # initialize the output vector
  xOut <- rep("", times=length(x))
  # iterate over the x input vector
  for (i in (segLength+latLength):length(xOut)) {
    # next iteration if there is no data
    if(is.na(x[i])) next()
    # load the inputBuffer
    segmentBuffer <- x[(i-(segLength+latLength)+1):(i-latLength)]
    # increment the loop if there is insufficient data to calulate the IQR
    if(is.na(IQR(segmentBuffer, na.rm=TRUE))) next()
    # calculate the IQR and the 1st and 3rd quartiles
    iqRange <- IQR(segmentBuffer, na.rm=TRUE)
    q25 <- quantile(segmentBuffer, .25, na.rm=TRUE)
    q75 <- quantile(segmentBuffer, .75, na.rm=TRUE)
    # set the upper and lower Tukey fences
    tukeyUpper <- q75 + fenceVal*iqRange
    tukeyLower <- q25 - fenceVal*iqRange
    # 
    if(fence != "lower") { if(x[i] >= tukeyUpper) xOut[i] <- abs(x[i] - q75)/iqRange }
    # compute number of upper fences
    # abs(x[i] - q75)/iqRange
    if(fence != "upper") { if(x[i] <= tukeyLower) xOut[i] <- "Artifact" }
    # compute the number of lower fences
    # abs(x[i] - q25) / iqRange
  } # end for loop
  # print(xOut)
  
  
  if(out=="index") { xOut <- which(xOut=="Artifact") }
  return(xOut)
} # end tukeyFence8() function


# tukeyFence8(x=chartDF$c_SE, y=6, z=2, fence="both", out="vector", fenceVal=9)