# script to hold descentProp functions
# 10-15-2015
# taken from the amplitudeExtract helper functions 
# to attempt to split the 50% descent rule into 2 rules: 1 for ROW and 1 for after ROWEndRow

# function to exclude rows before ROWEnd when the data descend more than a proportion p from the previous highest peak
descentPropROW <- function(x, y=xPeakLoop, z=myData, p=.5) {
  # function to exclude rows after the data descend more than a proportion p
  # from a the previous highest peak after response onset
  # this function is called iteratively in a for loop to select the max change from onset to peak
  # while excluding peaks that occur after the data have descended
  # more than a proportion p from a previous highest peak
  # x is a vector of a single reponse onset row index (x is a scalar)
  # y is a is a vector of peak indices (prior to decsent below the onset value)
  # z is a vector of time series values
  # p is a proportion for which the data are evaluated if it descend below p
  # for the prior maximum response peak value - response onset value 
  #### 
  yChangeOnset <- x
  xPeakLoop <- y
  myData <- z
  prop <- p
  # get the time series values for all peak points
  xPeakLoopValues <- myData[xPeakLoop]
  # compute the difference for each peak - response onset value
  xPeakLoopDiffs <- xPeakLoopValues - myData[yChangeOnset]
  # determine the cut value for each peak
  cutValues <- prop * xPeakLoopDiffs
  # get the vector of time series values after response onset
  # myValues <- myData[(yChangeOnset+1):length(myData)]
  myValues <- myData[(yChangeOnset+1):ROWEndRow] # 10-15-2015 to end the 50% rule at ROWEndRow
  # compute the difference for all time series values after response onset
  myDiffs <- myValues - myData[yChangeOnset]
  # make a vector of the slope direction for all time series samples after response onset
  mySlope <- c( 0, ifelse( diff(myValues)>0, 
                           1, 
                           ifelse(diff(myValues)<0, 
                                  -1, 0) ) )
  # look for descending myValues that are less than the cutValues for previous peaks
  # make an empty vector
  cutVector <- numeric(length=length(myDiffs))
  # populate the cutVector
  for (i in 1:length(myDiffs)) {
    # make a vector myCutValues for the vector myDiffs
    currentRow <- i + yChangeOnset - 1
    # myCutValue needs to be 1/2 the diff of the preceeding max peak
    myCutValue <- ifelse(length(which(xPeakLoop <= currentRow)) > 0,
                         max(cutValues[which(xPeakLoop <= currentRow)]),
                         0 )
    # 10-15-2015 replaced by the better version in the preceeding 3 lines    
    #     ifelse(length(which(xPeakLoop <= currentRow)) > 0,
    #            myCutValue <- max(cutValues[which(xPeakLoop <= currentRow)]),
    #            myCutValue <- 0 )
    # use i to check the max preceeding diff
    # # if(myDiffs[i] <  myCutValue) { cutVector[i] <- myCutValue }
    # cutVector is the proportion p of the difference between the previous max peak and response onset value
    cutVector[i] <- myCutValue
  } # end for loop to make the cutVector
  # print(cutVector)
  # use only descending slope segments
  descRows <- which(mySlope == -1)
  # ignore the first several descending rows to avoid over sensitivity to high frequency noise
  cutVector <- c(rep(0, time=90), cutVector[91:length(cutVector)])
  # determine the descending rows that are smaller than the corresponding index in cutVector
  cutRows <- descRows[myDiffs[descRows] < cutVector[descRows]]
  # use the first row index smaller than cutVector as the stopRow after which data are not used
  stopRow2 <- cutRows[1] + yChangeOnset - 1
  # use the last row in the data vector of stopRow2 is NA
  if(is.na(stopRow2) == TRUE) stopRow2 <- length(myData)
  # return a scalar with the stop row after which peaks are excluded 
  return(stopRow2)
  #
} # end descentPropROW function


# function to exclude rows after ROWEnd when the data descend more than a proportion p from the previous highest peak
descentProp <- function(x, y=xPeakLoop, z=myData, p=.5) {
  # function to exclude rows after the data descend more than a proportion p
  # from a the previous highest peak after response onset
  # this function is called iteratively in a for loop to select the max change from onset to peak
  # while excluding peaks that occur after the data have descended
  # more than a proportion p from a previous highest peak
  # x is a vector of a single reponse onset row index (x is a scalar)
  # y is a is a vector of peak indices (prior to decsent below the onset value)
  # z is a vector of time series values
  # p is a proportion for which the data are evaluated if it descend below p
  # for the prior maximum response peak value - response onset value 
  #### 
  yChangeOnset <- x
  xPeakLoop <- y
  myData <- z
  prop <- p
  # get the time series values for all peak points
  xPeakLoopValues <- myData[xPeakLoop]
  # compute the difference for each peak - response onset value
  xPeakLoopDiffs <- xPeakLoopValues - myData[yChangeOnset]
  # determine the cut value for each peak
  cutValues <- prop * xPeakLoopDiffs
  # get the vector of time series values after response onset
  # myValues <- myData[(yChangeOnset+1):length(myData)]
  myValues <- myData[(ROWEndRow+1):length(myData)] # 10-15-2015 to apply the 50% rule after ROWEndRow
  # compute the difference for all time series values after response onset
  myDiffs <- myValues - myData[yChangeOnset]
  # make a vector of the slope direction for all time series samples after response onset
  mySlope <- c( 0, ifelse( diff(myValues)>0, 
                           1, 
                           ifelse(diff(myValues)<0, 
                                  -1, 0) ) )
  # look for descending myValues that are less than the cutValues for previous peaks
  # make an empty vector
  cutVector <- numeric(length=length(myDiffs))
  # populate the cutVector
  for (i in 1:length(myDiffs)) {
    # make a vector myCutValues for the vector myDiffs
    currentRow <- i + yChangeOnset - 1
    # myCutValue needs to be 1/2 the diff of the preceeding max peak
    myCutValue <- ifelse(length(which(xPeakLoop <= currentRow)) > 0,
                         max(cutValues[which(xPeakLoop <= currentRow)]),
                         0 )
    # 10-15-2015 replaced by the better version in the preceeding 3 lines    
    #     ifelse(length(which(xPeakLoop <= currentRow)) > 0,
    #            myCutValue <- max(cutValues[which(xPeakLoop <= currentRow)]),
    #            myCutValue <- 0 )
    # use i to check the max preceeding diff
    # # if(myDiffs[i] <  myCutValue) { cutVector[i] <- myCutValue }
    # cutVector is the proportion p of the difference between the previous max peak and response onset value
    cutVector[i] <- myCutValue
  } # end for loop to make the cutVector
  # print(cutVector)
  # use only descending slope segments
  descRows <- which(mySlope == -1)
  # ignore the first several descending rows to avoid over sensitivity to high frequency noise
  cutVector <- c(rep(0, time=90), cutVector[91:length(cutVector)])
  # determine the descending rows that are smaller than the corresponding index in cutVector
  cutRows <- descRows[myDiffs[descRows] < cutVector[descRows]]
  # use the first row index smaller than cutVector as the stopRow after which data are not used
  stopRow2 <- cutRows[1] + yChangeOnset - 1
  # use the last row in the data vector of stopRow2 is NA
  if(is.na(stopRow2) == TRUE) stopRow2 <- length(myData)
  # return a scalar with the stop row after which peaks are excluded 
  return(stopRow2)
  #
} # end descentProp function
