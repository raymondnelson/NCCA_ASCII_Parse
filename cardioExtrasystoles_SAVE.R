# R functions to identify cardio extrasystole and fasiculation artifacts. 



# checkTRUE()

# tukeyFence5()



checkTRUE <- function(x, y="ALL", output="LOGICAL") {
  # R function to count the number of TRUE items in the input vector
  # x is a logical vector 
  # y call be "ALL" or any integer indicating the number of reuired TRUE items
  # output can be "LOGICAL" or "FREQ"
  ###
  # to check if all are TRUE
  if(output == "FREQ") {
    # return the number of true items
    return(length(which(x)))
  } else {
    # when all input items are required as TRUE
    if(y == "ALL"){
      if(length(which(x)) == length(x)) {
        return(TRUE)
      } else return(FALSE)
    } else {
      # to check for frequency y TRUE items
      if(length(which(x)) >= y) {
        return(TRUE)
      } else return(FALSE)
    }
  }
} # end checkTRUE() function




tukeyFence5 <- function(x=inputVector, 
                        buffer=4,
                        bufferStop=2,
                        side="both", 
                        fence="inner", 
                        innerFence=1.5, 
                        outerFence=3, 
                        expVal=1, 
                        scaleVal=1, 
                        output="name") {
  # R function to compute outliers using the Tukey Fence method
  # 5/11/2016
  # Raymond Nelson
  #
  ####
  # x is an in put vector of time series data values
  # x can be the cardio systolic - diastolic difference or the peak to peak difference
  # buffer length is the number of samples to use instead of the length of the input vector
  # bufferStop is the number of samples to stop prior to the i value
  # side is a parameter that can specify "both" "upper" or "lower"
  # fence is a parameter that can specify "both" "inner" or "outer"
  # inner fence is typically 25thPercentile - 1.5*IQR and 75thPercentile + 1.5*IQR
  # outer fence is typically 25thPercentile - 3*IQR and 75thPercentile + 3*IQR
  # expVal is a numerical value for exponential transformation of the input vector
  # scaleVal is numerical value for linear scaling of the input vector
  # output parameter uses "name" or "logical" or "range"
  #
  # output is a vector of NA values the same length as x with row indices,
  # including artifact flags for values that exceed the Tukey fences
  ####
  
  # scale the input data
  x <- x * scaleVal
  
  # this method allows any exponent value
  x <- sign(x) * abs(x)^expVal
  
  # check the length of the input 
  if(buffer > length(x)) buffer <- length(x)
  if(is.null(buffer)) buffer <- length(x)
  
  # get the interquartile range for the x input vector
  # used if the buffer is null or equal to the length of the x input vector
  # if(buffer >= length(x)) {
  #   q25X <- quantile(x, .25, na.rm=TRUE) # 25th percentile of the distribution of x
  #   q75X <- quantile(x, .75, na.rm=TRUE) # 75th percentile of the distribution of x
  #   iqRangeX <- q75 - q25
  # }
  
  # initialize the output vector
  xOut <- rep(0, times=length(x))
  
  # use a loop  to iterate over the x input vector 
  loopStart <- buffer + 1
  if(loopStart > length(x)) loopStart <- length(x)
  i=loopStart # for testing only 
  for(i in loopStart:length(x)) {
    # locate any significant activity 
    # by comparing each item to the buffer of previous items
    if(is.na(x[i])) next()
    # initialize the buffer
    
    if(buffer <= length(x)) {
      
      bufferStart <- i - buffer
      bufferEnd <- i - bufferStop
      
      xBuffer <- x[bufferStart:bufferEnd]
      # get the interquartile range
      q25 <- quantile(xBuffer, .25, na.rm=TRUE) 
      q75 <- quantile(xBuffer, .75, na.rm=TRUE)
      if(is.na(q25) | is.na(q75)) next()
      iqRange <- q75 - q25
      
    } 
    
    # increment the i loop if the range is zero
    if(iqRange == 0) next()
    
    # set the Tukey Fences
    innerLower <- q25 - (innerFence * iqRange)
    innerUpper <- q75 + (innerFence * iqRange)
    outerLower <- q25 - (outerFence * iqRange)
    outerUpper <- q75 + (outerFence * iqRange)
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
  
  xOut[c(1,length(xOut))] <- NA
  if(output=="logical") { 
    xOut[!is.na(xOut)] <- TRUE
    xOut[is.na(xOut)] <- FALSE
    xOut <- as.logical(xOut)
  }
  
  return(xOut)
  
} # end tukeyFence5() function



############  main function ###############



cardioArtifactFn <- function(x=minMaxAmp, 
                            y=cardioBuffer, 
                            side="both",
                            fence="outer",
                            innerFence=1.5,
                            outerFence=3,
                            expVal=1,
                            scaleVal=1,
                            outputType="logical",
                            samples=31, 
                            cutProp=.51) {
  # R function to identify Extrasystole and Fasiculation artifacts in Cardio data
  # calculates a vector of bagged tukey fence decision stumps
  # 
  ####
  #
  # uses the tukeyFence5 function
  # uses the sample function in base R
  # uses checkTRUE function in the sigProcHelper.R script
  # #
  # input x is a vector of value from the processed time series data
  # input y is a vector of buffer lengths in seconds, 
  # used for comparison to each value in x
  # input samples is the number of random subsets to create/bag
  # for comparison with each x
  # input cutPoint is a proportion of bagged comparisons,
  # as a cutoff for significance
  #
  # output is a vector of values of length equal to x,
  # indicating with x values are significant in the bagged decision stump
  #
  ########
  
  inputVector <- x
  inputBuffer <- y
  
  # initialize buffer1
  buffer1 <- sample(x=inputBuffer, size=samples, replace=TRUE)
  # initialize buffer2
  # buffer2 <- rep(NA, times=samples)
  buffer2 <- sample(x=inputBuffer, size=samples, replace=TRUE)
  
  tempDF <- as.data.frame(matrix(nrow=length(inputVector), 
                                 ncol=length(buffer1)))
  
  for(i in 1:samples){
    if(buffer2[i] > buffer1[i]) {
      bufferOn <- buffer2[i] 
      bufferOff <- buffer1[i] 
    } else {
      bufferOn <- buffer1[i] 
      bufferOff <- buffer2[i] 
    } 
    # use the tukeyFence5 function to populate the data frame
    tempDF[,i] <- tukeyFence5(x=inputVector,
                              buffer=bufferOn,
                              bufferStop=bufferOff,
                              side=side,
                              fence=fence,
                              innerFence=innerFence,
                              outerFence=outerFence,
                              expVal=expVal,
                              scaleVal=scaleVal,
                              output="logical" )
  } # end for loop
  
  # View(tempDF)
  
  # set the cutPoint number of sig stumps using the cutProp
  cutPoint <- round(cutProp * samples, 0)
  
  # initialize and populate the output vector
  if(outputType!="FREQ") {
    outputVector <- rep(NA, times=length(inputVector))
    outputVector[which(apply(tempDF, 1, checkTRUE, cutPoint))] <- "outerUpper"
  }
  # apply(tempDF, 1, checkTRUE, 7, 'FREQ')
  # outputVector
  
  if(outputType=="FREQ") {
    # use this to see the number of signifanct samples
    return(apply(tempDF, 1, checkTRUE, 7, 'FREQ'))
  } else return(outputVector)
  
} # end cardioArtifactFn()



######## Tukey Fence Function #########



tukeyFenceFn <- function(x=chartDF$c_Cardio1) {
  # R function to compute cardio values that exceed the tukey fence limits
  # Aug 28, 2023
  # Raymond Nelson
  #
  ####
  #
  # x input is a time series vector
  # 
  # output is a vector of 0's with significant points indicated by the value 1
  #
  ####
  
  # plot.ts(x)

  minMaxPeaks <- minMaxPeakFn(x)
  
  DAT <- abs(diff(x[minMaxPeaks]))
  
  q1 <- quantile(DAT, .25)
  qm <- quantile(DAT, .5)
  q3 <- quantile(DAT, .75)
  
  iqr <- abs(q3 - q1)
  
  # adjust the length
  DAT <- c(qm, qm, DAT[2:(length(DAT)-1)], qm)
  
  names(DAT) <- NULL
  
  # inner fences
  # lfI <- q1 - (1.5 * iqr)
  # ufI <- q3 + (1.5 * iqr)
  # outer fences
  lfO <- q1 - (3 * iqr)
  ufO <- q3 + (3 * iqr)
  
  # outVc <- rep(0, length(DAT))
  
  # outVc[which(DAT<=lfI)] <- 1
  # outVc[which(DAT>=ufI)] <- 1
  # outVc[which(DAT<=lfO)] <- 1
  # outVc[which(DAT>=ufO)] <- 1

  # pkVc <- c(which(DAT<=lfI), which(DAT>=ufI), which(DAT<=lfO), which(DAT>=ufO))
  # outer fences only
  pkVc <- c(which(DAT<=lfO), which(DAT>=ufO))
  pkVc <- sort(unique(pkVc))
  
  outVc <- minMaxPeaks[pkVc]
  
  return(outVc)

} # end tukeyFenceFn()

# tukeyFenceFn(x=chartDF$c_Cardio1)




# minMaxPeaks <- minMaxPeakFn(segmentDF$c_Cardio1)
# minMaxPeaks <- minMaxPeakFn(chartDF$c_Cardio1)
# 
# summary(abs(diff(segmentDF$c_Cardio1[minMaxPeaks])))
# summary(abs(diff(chartDF$c_Cardio1[minMaxPeaks])))
# 
# DAT <- abs(diff(segmentDF$c_Cardio1[minMaxPeaks]))
# DAT <- abs(diff(chartDF$c_Cardio1[minMaxPeaks]))
# 
# names(DAT) <- NULL
# 
# q1 <- quantile(DAT, .25)
# qm <- quantile(DAT, .5)
# q3 <- quantile(DAT, .75)
# 
# iqr <- q3 - q1
# 
# DAT <- c(qm, qm, DAT[2:(length(DAT)-1)], qm)
# 
# names(DAT) <- NULL
# 
# lf <- q1 - 3 * iqr
# uf <- q3 + 3 * iqr
# 
# 
# 
# DAT[which(DAT <= lf | DAT >= uf)]
# 
# outVc <- rep(0, length(DAT))
# 
# outVc[which(DAT <= lf | DAT >= uf)] <- 1



