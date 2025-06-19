##### helper functions for artifact extracton #####
# April 2017
# Raymond Nelson
#
###
#
# checkTRUE() to check the number of TRUE items in a vector
#
# tukeyArtifactFn() to locate cardio artifacts
#
# tukeyFence5() to identify outlers 
#
###################



checkTRUE <- function(x, y="ALL", output="LOGICAL") {
  # function to count the number of TRUE items in the input vector
  # x is a logical vector indicating whether data exceeded a tukey fence
  # y call be "ALL" or an integer indicating the number of TRUE requird
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
} # end checkTRUE function



#############



tukeyArtifactFn <- function(x=minMaxAmp, 
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
  # function to calculate vector of bagged tukey fence decision stumps
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
  # use a loop to select random values for buffer 2 less than buffer1
  # cardioBuffer2 < cardioBuffer1
  # for(i in 1:length(buffer1)) {
  #   buffer2[i] <- sample(c((buffer1[i]-1):inputBuffer[1]), 1)
  # }    
  # maxAmpChangeDF <- c(1:length(systInputVector))
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
    # use the tukeyFence5 functon to populate the data frame
    tempDF[,i] <- tukeyFence5(x=inputVector,
                                      buffer=bufferOn,
                                      bufferStop=bufferOff,
                                      side=side,
                                      fence=fence,
                                      innerFence=innerFence,
                                      outerFence=outerFence,
                                      expVal=expVal,
                                      scaleVal=scaleVal,
                                      output="logical"
    )
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
  
} # end tukeyArtifactFn

# getSegFn(exam=1,series=1,chart=2)

# tukeyArtifactFn(x=minMaxAmp,
#                 y=cardioBuffer,
#                 side="both",
#                 fence="outer",
#                 innerFence=1.5,
#                 outerFence=3,
#                 expVal=1,
#                 scaleVal=1,
#                 outputType="logical",
#                 samples=30,
#                 cutProp=.50)


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
  
  # get the interquartile range for the x input vector
  # used if the buffer is null or equal to the length of the x input vector
  if(buffer >= length(x)) {
    q25 <- quantile(x, .25, na.rm=TRUE) 
    q75 <- quantile(x, .75, na.rm=TRUE)
    iqRange <- q75 - q25
  }
  
  # initialize the output vector
  xOut <- rep(NA, times=length(x))
  
  # use a loop  to iterate over the x input vector 
  loopStart <- buffer+1
  i=loopStart # for testing only 
  for(i in loopStart:length(x)) {
    # locate any signficant activity 
    # by comparing each item to the buffer of previous items
    if(is.na(x[i])) next()
    # initialize the buffer
    
    if(buffer <= length(x)) {
      
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
      
    } 
    
    # increment the i loop if the range is zero
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
  xOut[c(1,length(xOut))] <- NA
  if(output=="logical") { 
    xOut[!is.na(xOut)] <- TRUE
    xOut[is.na(xOut)] <- FALSE
    xOut <- as.logical(xOut)
  }
  return(xOut)
} # end tukeyFence5() function


lowPass3rdOrder.886Hz <- function(x=chartDF$c_PL, 
                                  GAIN = 1.20421952221821e+000, 
                                  zplane1 = 0.68958592900633, 
                                  zplane2 = -2.32432951610250, 
                                  zplane3 = 2.62939157026304) {
  # x is the time series input data  at 30 samples per second
  # initialize some variables
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  yv3 <- 0
  yv2 <- 0
  yv1 <- 0
  yv0 <- 0
  # initialize the output vector
  output <- rep(NA, length(x))
  # iterate over x
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- (xv0 + xv3) + 
      3 * (xv1 + xv2) + 
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2)
    output[i] <- yv3
  }
  return(output)
  # could also build up an output vector using concatenation 
  # but that is slower than making the vector and replacing the value
} # end lowpass filter function 3rd Order Butterworth .886Hz() 



lowPass3rdOrder.2953Hz <- function(x=chartDF$c_PL, 
                                   GAIN = 1.06381058517009e+000, 
                                   zplane1 = 0.88363189794362, 
                                   zplane2 = -2.76017916697872, 
                                   zplane3 = 2.87632467307739) {
  # x is the time series input data  at 30 samples per second
  # initialize some variables
  xv1 <- x[1]
  xv2 <- x[1]
  xv3 <- x[1]
  yv3 <- 0
  yv2 <- 0
  yv1 <- 0
  yv0 <- 0
  # initialize the output vector
  output <- rep(NA, length(x))
  # iterate over x
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- xv2
    xv2 <- xv3
    xv3 <- x[i] * GAIN
    yv0 <- yv1
    yv1 <- yv2
    yv2 <- yv3
    yv3 <- (xv0 + xv3) + 
      3 * (xv1 + xv2) + 
      (zplane1 * yv0) + 
      (zplane2 * yv1) + 
      (zplane3 * yv2)
    output[i] <- yv3
  }
  return(output)
  # could also build up an output vector using concatenation 
  # but that is slower than making the vector and replacing the value
} # end lowpass filter function 3rd Order Butterworth .2953Hz() 



lowPass1st.2953Hz <- function(x, GAIN = 3.33273026219770e+001, zplane = 0.93998914275525) {
  # x is a column vector from the time series data
  ###
  # initialized some variales
  xv1 <- x[1]
  yv1 <- 0
  # initialize the output
  output <- rep(NA, length(x))
  # output <- NULL
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
    # output <- c(output, yv1)
  }
  return(output)
} # end lowpass .2953 function



lowPass1st.0984Hz <- function(x, GAIN = 9.80422621749710e+001, zplane = 0.97960063389367) {
  # x is a column vector from the time series data
  ###
  # initialized some variales
  xv1 <- x[1]
  yv1 <- 0
  # initialize the output
  output <- rep(NA, length(x))
  # output <- NULL
  for (i in 1:length(x)) {
    xv0 <- xv1
    xv1 <- x[i] / GAIN
    yv0 <- yv1
    yv1 <- (xv1 + xv0) + (zplane * yv0)
    output[i] <- yv1
    # output <- c(output, yv1)
  }
  return(output)
} # end lowpass .0984 function




