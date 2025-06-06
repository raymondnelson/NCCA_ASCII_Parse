# R function to identify unreponsive respiration data
# 12-28-2016
# Raymond Nelson
# 
##############



source(paste0(RPath, "sigProcHelper.R"), echo=FALSE)



pneumoCheckFn <- function(x1=chartDF$c_UPneumoSm, x2=chartDF$c_LPneumoSm, sec=5, times=30, omit=10, firstRow=NULL, lastRow=NULL) {
  # helper function to check for unresponsive respiratory sensor data
  # 11/29/2016
  # Raymond Nelson
  ### input
  # x input is the time series EDA vector from the chartDF data frame
  # sec is the number of seconds to sample
  # times is the number of random samples to take from the time series data
  # ignore is an integer that specifies the number of large samples
  # 	to ignore in the scaling calcluation
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the endex of the end of the scoring window for the last stimulus event
  # sVal is the scale value to use when determining small responses # 4-8-2017 remove this
  ### output
  # output is one of two messages: "none", "possible unresponsive EDA"
  ##########
  # get the input data
  tsDataU <- x1
  tsDataL <- x2
  # determine the length
  dataLength <- length(tsDataU)
  # use the entire length if firstRow and lastRow are NULL
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength
  # set the default output message
  outputMessageU <- "none"
  outputMessageL <- "none"
  outputMessage <- "none"
  
  # # for charts >=  35 seconds
  # if(dataLength >= (sec * 35 * cps)) {
  #   # sample the data
  #   # first set the number of indices to include in each sample
  #   sampleLength <- sec * cps 
  #   # get the sample onset row indices using random numbers
  #   # get the sample onset indices
  #   sampleOnset <- sample(c(firstRow:(lastRow-sampleLength)), size=times, replace=TRUE)
  #   # then get the sample offset indices
  #   sampleOffset <- sampleOnset + sampleLength - 1
  #   # make a data frame for the sample segments
  #   DF1 <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
  #   DF2 <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
  #   # the data frame roww will be the samples from the time series input 
  #   # populate the data frame
  #   for (i in 1:times) {
  #     DF1[i,] <- tsDataU[sampleOnset[i]:sampleOffset[i]]
  #     DF2[i,] <- tsDataL[sampleOnset[i]:sampleOffset[i]]
  #   }
  #   # make a private function to get the range from the DF rows
  #   dfRangeFn <- function(x) { max(x)-min(x) }
  #   # apply the range function to each row of the DF to get the range of each row
  #   # apply is vectorized and needs no loop
  #   dfRange1 <- apply(DF1, 1, dfRangeFn)
  #   dfRange2 <- apply(DF2, 1, dfRangeFn)
  #   # sort and remove the largest changes using the ignore parameter 
  #   dfRange1 <- sort(dfRange1, decreasing = TRUE)[(omit+1):length(dfRange1)]
  #   dfRange2 <- sort(dfRange2, decreasing = TRUE)[(omit+1):length(dfRange2)]
  #   # calculate the mean dfRange
  #   dfRangeMean1 <- mean(dfRange1)
  #   dfRangeMean2 <- mean(dfRange2)
  #   # set the output message if the mean range is very small
  #   if(dfRangeMean1 <= scaleVals['uPneumo']/10) outputMessage <- "unresponsive data"
  #   if(dfRangeMean2 <= scaleVals['lPneumo']/10) outputMessage <- "unresponsive data"
  # } else {
  #   # for charts < 35 seconds
  #   if(max(tsData1) == min(tsData2)) {
  #     outputMessage <- "unresponsive data"
  #   }
  #   if(max(tsData2) == min(tsData2)) {
  #     outputMessage <- "unresponsive data"
  #   }
  # } # end else for charts < 35 seconds
  
  # next check for data that becomes unresponsive during data acquisition
  
  # initialize a vector
  diffVector1 <- rep(0, times=(length(tsDataU)-(10*cps)+1))
  # length(diffVector)
  diffVector2 <- rep(0, times=(length(tsDataL)-(10*cps)+1))
  # iterate over the time series data
  i=1
  for (i in 1:(length(tsDataU)-(10*cps)+1)) {
    thisSlice <- tsDataU[i:(i+(10*cps)-1)]
    thisRange <- range(thisSlice)
    thisDiff <- abs(thisRange[1] - thisRange[2])
    diffVector1[i] <- thisDiff
  }
  # scaleVals is a vector in the global env, set by the init
  unresponsiveProp <- length(which(diffVector1 <= .25*scaleVals['uPneumo'])) / length(tsDataU)
  if(unresponsiveProp >= .1) {
    outputMessage <- paste("unresponsive data", unresponsiveProp)
  } 
  j=1
  for (j in 1:(length(tsDataL)-(10*cps)+1)) {
    thisSlice <- tsDataL[j:(j+(10*cps)-1)]
    thisRange <- range(thisSlice)
    thisDiff <- abs(thisRange[1] - thisRange[2])
    diffVector2[j] <- thisDiff
  }
  unresponsiveProp <- length(which(diffVector2 <= .25*scaleVals['lPneumo'])) / length(tsDataL)
  if(unresponsiveProp >= .1) {
    outputMessage <- paste("unresponsive data",round(unresponsiveProp,2))
  }
  # output
  # if(outputMessage != "none") outputMessage <- paste(outputMessage, "chart", chartName)
  if(outputMessage != "none") print(outputMessage)
  assign("pneumoDataWarning", outputMessage, pos=1)
  return(outputMessage)
} # end pneumoCheckFn()


# EDACheckFn(x=chartDF$c_AutoEDA, sec=5, times=25, omit=5, firstRow=NULL, lastRow=NULL)
# EDACheckFn(x=chartDF$c_LPneumoSm, sec=5, times=25, omit=5, firstRow=NULL, lastRow=NULL)