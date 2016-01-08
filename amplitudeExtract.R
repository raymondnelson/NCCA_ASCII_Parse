# amplitude extract function for EDA and Cardio data
#
# 1. compute ROW as the segment before ROWEndRow and after the onset latencyRow 
# 2. compute the row for all positive slope segments that start in the ROW
# 3. compute the onset value for the onset of all positive slope segments inside the ROW 
# 4. locate the row and value for all positive peak points
# 5. keep only those peaks that occur after the first onset in the ROW
# 6. keep all peaks in the measurement window
# 7. keep one additional peak if the slope is + at the end of the measurement window
# 9. select the max change for each onset and all subsequent peaks
# 9a. for each onset exclude response peaks after the data descend below the onset value
# 9b. for each onset exclude response peaks after ROWEndRow if data descend 50% from the previous max peak
# 10. select the onset and peak with max increase
# 
# 
#
######################################


# mySegmentLists <- ls(pattern="*_dataSegmentList$")
# myEventLists <- ls(pattern="*_eventList$")
# 
# mySegmentLists <- mySegmentLists[1]
# myEventLists <- myEventLists[1]
# 
# mySegmentDF <- get(mySegmentLists)[[3]]
# myEventDF <- get(myEventLists)[[3]]
# 
# # myData <- mySegmentDF$AutoEDA
# myData <- mySegmentDF$CardioMA
# 
# begin <- myEventDF$Begin
# end <- myEventDF$End
# answer <- myEventDF$Answer
# # if(answer == end) answer <- answer+1
# start <- mySegmentDF$Sample[1]
# lat <- .5
# nSmooth <- 4
# label <- myEventDF$Label
# segmentName <- paste(mySegmentDF$examName[1], mySegmentDF$chartName[1], myEventDF$Label, sep="_")



#########



# strictWindow <- FALSE # use TRUE to stop responses at the end of the measurement window
# strictROW <- FALSE # use TRUE to ignore all positive slope segments that begin after end of ROW
# descentStop <- TRUE # will not evaluate upward segments that begin after the data descend a specified proportion




##################################################################################

amplitudeExtract <- function(extractList=extractList, strictWindow=FALSE, strictROW=FALSE, descentStop=FALSE) {
  # function to extract the amplitude of EDA increase in response to a stimulus
  # input is a list of 9 items including all of the information needed to extract the response
  # 1. begin is a scalar indicating the row number of the onset of the stimulus
  # 2. end is a scalar indicating the row number of the end of the stimulus
  # 3. answer is a scalar indicating the row number of the verbal answer
  # 4. start is the starting row number from the chart data frame
  # 5. Lat is the required latency after stimulus onset before which a responses is not evaluated
  # 6. segmentName is the name of the stimulus event
  # 7. nSmooth is the number of samples to smooth and ignore slope changes of small duration
  # 8. segmentTitle is the full segment name including examName, seriesName, chartName and segmentName
  # 9. dataVector is is a vector of time series data for a single stimulus segment
  # prestimSeg is the length of the prestimulus segment in seconds
  # ROWEnd is the number of seconds after the verbal answer
  # ROWEnd defines the end of the segment during which a physiological response must begin
  # onset of physiological response the onset of a positive slope segment during ROW
  #
  ####
  #
  # 
  
  ##### helper functions required by amplitudeExtract() #####
  
  # a helper function to determine the positive or negative slope direction 
  slopeDir <- function(x=myData) {
    # function to determine the positive or negative slope direction
    # input x is a vector of time series measurements
    # ouput is a vector of slope values for all values in the input vector
    x1 <- x
    diff1 <- diff(x)
    x1 <- ifelse(diff1==0,
                 x1 <- 0,
                 # ifelse is vectorized and does not require a control loop
                 ifelse(diff1>0,
                        x1 <- 1,
                        x1 <- -1) )
    return(c(x1, 0))
  } # end slopeDir function
  
  # helper function to remove slope changes less than n samples
  smoothSlope <- function(x=mySlope, n=4) {
    # function to remove slope changes of small duration
    # x is a time series vector of positive (1) negative (-1) and zero (0) slope changes
    # will also use the nSmooth variable from the parent environment
    # input is the output from the slopeDir() function
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
    xPos2 <- xPos1
    for (j in length(xPos2):2) {
      if(xPos2[j] != 0 & xPos2[j-1] != 0) { xPos2[j-1] <- xPos2[j] } }
    # remove short runs
    xPos3 <- xPos2
    for (k in 1:length(xPos3)) {
      if(xPos3[k] <= n) xPos3[k] <- 0 }
    ###### run sum for all negative slope segments
    xNeg <- ifelse(x == -1, -1, 0)
    # make a vector of zeros
    xNeg1 <- rep(0, times=length(xNeg))
    # run sum for all negative slope segments
    for (l in 2:length(xNeg)) {
      if(xNeg[l] != 0) xNeg1[l] <- xNeg[l] + xNeg1[l-1] }
    # propagate the run length
    xNeg2 <- xNeg1
    for (m in length(xNeg2):2) {
      if(xNeg2[m] != 0 & xNeg2[m-1] != 0) { xNeg2[m-1] <- xNeg2[m] } }
    # remove short runs
    xNeg3 <- xNeg2
    for (o in 1:length(xNeg3)) { # be careful not to abuse the n variable
      if(xNeg3[o] >= -n) xNeg3[o] <- 0 }
    ### combine the positive and negative vectors
    x1 <- rep(0, times=length(x))
    x1 <- ifelse(xPos3 > 0, 
                 x1 <- xPos3, 
                 ifelse(xNeg3 < 0,
                        x1 <- xNeg3,
                        x1 <- 0) )
    return(x1)
  } # end smoothSlope function
  
  # helper function to fill the slope value to eliminate 0 slopes
  fillSlope <- function(x=mySlope1) {
    # function to fill the slop values to eliminate 0 slope segments
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
                        0))
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
  } # end positiveChange function
  
  # a helper function to make vector of positive slope onset rows
  positiveOnset <- function(x=posSlope) {
    # function to make a vector of positive slope onset indices
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
  
  # helper function to find a change or increase in upward slope variance
  slopeChange <- function(x=myData, nPre=2, nPost=2, p=.999) {
    # function to find a change or increase in positive slope variance
    # useful to infer a response onset when the slope is already positive
    # i=1
    # x input is the vector of time series measurements
    # compare each N seconds with the next N seconds
    # p=.997 will be +3 standard deviations
    # N is the number of second to evaluate
    # output is a vector of 0s the same length as input and including +1 change in slope energy
    y <- rep(0, times=length(x))
    preLen <- cps*nPre
    postLen <- cps*nPost
    for (i in 1:(length(y)-(preLen+postLen))) {
      preDiff <- diff(x[i:(i+preLen-1)])
      # require 1 sec of + slope to prevent a response onset after a change in slope during latency
      if(all(preDiff[(length(preDiff)-cps+1):length(preDiff)] >= 0)) { 
        postDiff <- c(diff(x[(i+preLen):(i+preLen+postLen-1)]))
        if(mean(postDiff) >= qnorm(p, mean=mean(preDiff), sd=sd(preDiff))) { 
          y[(i+(preLen))] <- 1 
        }
      } # end if to prevent introducing an onset after a change in slope during latency
    } # end for loop
    # get the onset using a function to get positve slope onset rows
    y <- positiveOnset(y)
    return(y)
  } # end slopeChange function
  # 10-27-2015 was nPre=1, nPost=.5
  
  # helper function to locate the peak of all positive slope sections 
  slopePeak <- function(x=mySlope2) {
    # function to locate the peak of all positive slope segments
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
  
  # function to exclude peaks after the data descend more than a proportion p from the previous highest peak
  descentProp <- function(x, y=xPeakLoop, z=myData, p=.5, ROWEnd=ROWEndRow) {
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
    myValues <- myData[(yChangeOnset+1):length(myData)]
    # myValues <- myData[(ROWEnd+1):length(myData)] # 10-15-2015 to apply the 50% rule after ROWEnd
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
  
  # descentPropROW # not used
#####  
#   # function to exclude peaks after the data descend more than a proportion p from the previous highest peak
#   descentPropROW <- function(x, y=xPeakLoop, z=myData, p=.5, ROWEnd=ROWEndRow) {
#     # function to exclude rows after the data descend more than a proportion p
#     # from a the previous highest peak after response onset
#     # this function is called iteratively in a for loop to select the max change from onset to peak
#     # while excluding peaks that occur after the data have descended
#     # more than a proportion p from a previous highest peak
#     # x is a vector of a single reponse onset row index (x is a scalar)
#     # y is a is a vector of peak indices (prior to decsent below the onset value)
#     # z is a vector of time series values
#     # p is a proportion for which the data are evaluated if it descend below p
#     # for the prior maximum response peak value - response onset value 
#     #
#     yChangeOnset <- x
#     xPeakLoop <- y
#     myData <- z
#     prop <- p
#     # get the time series values for all peak points
#     xPeakLoopValues <- myData[xPeakLoop]
#     # compute the difference for each peak - response onset value
#     xPeakLoopDiffs <- xPeakLoopValues - myData[yChangeOnset]
#     # determine the cut value for each peak
#     cutValues <- prop * xPeakLoopDiffs
#     # get the vector of time series values after response onset
#     myValues <- myData[(yChangeOnset+1):length(myData)]
#     # myValues <- myData[(yChangeOnset+1):ROWEnd] # 10-15-2015 to end the 50% rule at ROWEnd
#     # compute the difference for all time series values after response onset
#     myDiffs <- myValues - myData[yChangeOnset]
#     # make a vector of the slope direction for all time series samples after response onset
#     mySlope <- c( 0, ifelse( diff(myValues)>0, 
#                              1, 
#                              ifelse(diff(myValues)<0, 
#                                     -1, 0) ) )
#     # look for descending myValues that are less than the cutValues for previous peaks
#     # make an empty vector
#     cutVector <- numeric(length=length(myDiffs))
#     # populate the cutVector
#     for (i in 1:length(myDiffs)) {
#       # make a vector myCutValues for the vector myDiffs
#       currentRow <- i + yChangeOnset - 1
#       # myCutValue needs to be 1/2 the diff of the preceeding max peak
#       myCutValue <- ifelse(length(which(xPeakLoop <= currentRow)) > 0,
#                            max(cutValues[which(xPeakLoop <= currentRow)]),
#                            0 )
#       # 10-15-2015 replaced by the better version in the preceeding 3 lines    
#       #     ifelse(length(which(xPeakLoop <= currentRow)) > 0,
#       #            myCutValue <- max(cutValues[which(xPeakLoop <= currentRow)]),
#       #            myCutValue <- 0 )
#       # use i to check the max preceeding diff
#       # # if(myDiffs[i] <  myCutValue) { cutVector[i] <- myCutValue }
#       # cutVector is the proportion p of the difference between the previous max peak and response onset value
#       cutVector[i] <- myCutValue
#     } # end for loop to make the cutVector
#     # print(cutVector)
#     # use only descending slope segments
#     descRows <- which(mySlope == -1)
#     # ignore the first several descending rows to avoid over sensitivity to high frequency noise
#     cutVector <- c(rep(0, time=90), cutVector[91:length(cutVector)])
#     # determine the descending rows that are smaller than the corresponding index in cutVector
#     cutRows <- descRows[myDiffs[descRows] < cutVector[descRows]]
#     # use the first row index smaller than cutVector as the stopRow after which data are not used
#     stopRow2 <- cutRows[1] + yChangeOnset - 1
#     # use the last row in the data vector of stopRow2 is NA
#     if(is.na(stopRow2) == TRUE) stopRow2 <- length(myData)
#     # return a scalar with the stop row after which peaks are excluded 
#     return(stopRow2)
#     #
#   } # end descentPropROW function
  #
  #
  #
  ##### end of helper function #####
  #
  ########
  #
  # get the information from the input list
  Begin <- as.numeric(extractList$begin)
  End <- as.numeric(extractList$end)
  Answer <- as.numeric(extractList$answer)
  Lat <- as.numeric(extractList$lat)
  startRow <- as.numeric(extractList$start)
  segmentName <- extractList$segmentName
  nSmooth <- as.numeric(extractList$nSmooth)
  segmentTitle <- extractList$segmentTitle
  myData <- extractList$dataVector
  #  
  DFRows <- length(myData)
  #
  # prestimRow <- eventDF$Begin - (startRow-1) - (cps*prestimSeg)
  prestimRow <- Begin - (startRow-1) - (cps*prestimSeg) # first row of the time series vector
  # correction for prestimRow values < 1
  if(prestimRow<=0) prestimRow <- 1
  onsetRow <- Begin - (startRow-1) # onset of the stimulus in the time series vector
  endRow <- onsetRow + (cps*measuredSeg) - 1 # end of the measurement window
  # correction if endRow > DFRows
  if(endRow > DFRows) endRow <- DFRows
  offsetRow <- End - (startRow-1) # end of the question stimulus
  latRow <- onsetRow + cps*Lat # repsonse latency period
  if(latRow >= (endRow-4)) latRow <- DFRows - 4
  if(offsetRow >= (endRow-2)) offsetRow <- DFRows - 2
  answerRow <- Answer - (startRow-1) # verbal answer
  # correction if there is no answer (answer row will be the same as offsetRow)
  if(answerRow==offsetRow) answerRow <- offsetRow + 1
  ROWEndRow <- answerRow + (cps*ROWEnd) # typically 5 seconds after the verbal answer
  # correction if ROWEndRow > DFRows
  if(ROWEndRow > (DFRows-3)) ROWEndRow <- DFRows - 3
  #
  # use a helper functon to make a vector of slope values
  mySlope <- slopeDir(x=myData)  
  #
  # use a helper function to smooth the slope by removing slope changes of small duration
  mySlope1 <- smoothSlope(x=mySlope)
  #
  # fill the zero slope segments
  mySlope2 <- fillSlope(x=mySlope1)
  #
  # use a helper function to make a vector of positive and non-positive slope activity
  posSlope <- positiveSlope(x=mySlope2)
  #
  # make vector of positive slope onset rows 
  xOnset1 <- positiveOnset(x=posSlope)
  #
  # use a helper function to infer respons onset via sig change in positive slope variance
  sChange <- slopeChange(x=myData) 
  #
  # combine the sChange and xOnset1 vectors
  alwaysAddSChange <- TRUE # set to FALSE to add sChange values only when xOnset1 is empty
  ifelse(alwaysAddSChange==TRUE,
         xOnset1[which(sChange==1)] <- 1,
         # add sChange to xOnset1 even when there is no onset in the ROW
         if(length(which(xOnset1[latRow:ROWEndRow]==1))==0) {
           if(length(sChange) > 0) { xOnset1[which(sChange==1)] <- 1 } }
  )
  #
  #################
  #
  ### now work on the peak values
  #
  # use a helper function to locate the peak of all positive slope sections
  xPeak1 <- slopePeak(x=mySlope2)
  #
  # add the ROWEndRow to the xPeak1 vector if it is positive
  if(posSlope[ROWEndRow] == 1) xPeak1[ROWEndRow] <- 1
  #
  # add the endRow slope to xPeak1 vector if it is positive.
  if(posSlope[endRow] == 1) xPeak1[endRow] <- 1 
  #
  ###################################
  #
  # get xOnset and xPeak row indices for positive slope segments   
  xOnset <- which(xOnset1 == 1)
  xPeak <- which(xPeak1 == 1)
  #
  # fix condition when xOnset or xPeak is length zero
  if(length(xOnset) == 0) xOnset <- endRow
  if(length(xPeak) == 0) xPeak <- endRow
  #
  # get the y-axis values for xOnset and xPeak rows
  xOnsetVal <- myData[xOnset]
  xPeakVal <- myData[xPeak]
  #
  ################
  #
  # keep onset rows after the required response latency period
  xOnsetVal <- xOnsetVal[which(xOnset >= latRow)]
  xOnset <- xOnset[xOnset >= latRow]
  #
  # Keep only those slope peaks that are >= the xOnset 
  xPeakVal <- xPeakVal[which(xPeak >= xOnset[1])]
  xPeak <- xPeak[xPeak >= xOnset[1]]
  #
  ####
  #
  # keep only those onset rows that occur before ROWEndRow
  xOnset <- xOnset[xOnset <= ROWEndRow]
  # xOnsetVal <- xOnsetVal[which(xOnset <= ROWEndRow)]
  xOnsetVal <- myData[xOnset]
  #
  # fix xOnset again if there is no response onset in the ROW
  if(length(xOnset) == 0) {
    xOnset <- endRow # may need to be endRow -1 because response onset and end cannot be the same
    xOnsetVal <- myData[xOnset]
  }
  #
  # fix condition where xOnset is >= the number of rows in the data frame  
  if(xOnset[1] >= length(myData)) {
    xOnset <- length(myData)
    xOnsetVal <- myData[xOnset]
  }
  #
  ##################
  #
  # Keep only those slope peaks that are >= the xOnset row
  xPeak <- xPeak[xPeak >= xOnset[1]]
  # xPeakVal <- xPeakVal[which(xPeak >= xOnset[1])]
  xPeakVal <- myData[xPeak] # same effect as the line a ove
  #
  ###############################
  #
  # keep only the slope peaks that are <= ROWEndRow 
  # plus one additional peak if the slope at ROWEndRow is positive 
  # if FALSE this will keep all peaks in the measurement
  if(strictROW == TRUE) { # use FALSE to include any max peak in the measurement period
    ifelse(posSlope[ROWEndRow] == 1,
           # to keep only one additional peak after ROWEndRow if the slope at ROWEndRow is +
           { xPeak <- xPeak[1:(length(which(xPeak <= ROWEndRow)) + 1)] }, 
           # this will keep only those xPeak indices that are less than ROWEndRow
           # of the slope is not = at ROWEndRow 
           { xPeak <- xPeak[xPeak <= ROWEndRow] } ) }
  xPeakVal <- myData[xPeak]
  #
  ###############################
  #
  # keep only those slope peaks that are <= endRow (end of measurement window)
  if(strictWindow != TRUE) { # use TRUE to stop responses at the end of the measurement window
    ifelse(xOnset[1] <= ROWEndRow,
      { ifelse(posSlope[endRow] == 1,
             { xPeak <- xPeak[1:(length(which(xPeak <= endRow)) + 1)] },  
             # if not + at endRow only peaks < endRow are retained
             { xPeak <- xPeak[xPeak <= endRow] } ) },
    { xPeak <- xPeak[xPeak <= endRow] } ) }
  xPeak <- as.numeric(na.omit(xPeak))
  # xPeakVal <- as.numeric(na.omit(xPeakVal)) # not needed
  xPeakVal <- myData[xPeak]
  #
  #################################
  #
  # fix empty xPeak vector - set the xpeak to the first xOnset value if there is no peak
  if(length(xPeak) == 0) { 
    xPeak <- xOnset[1]
    xPeakVal <- myData[xPeak]
  }
  #
  ##################################
  #
  # now we have vectors for xOnset, xPeak, xOnsetVal and xPeakVal
  # print(xOnset)
  # print(xOnsetVal)
  # print(xPeak)
  # print(xPeakVal)
  #
  ##################################
  #
  # loop to select the max change for each xOnset row
  # first make an empty vector for the loop output
  yChange <- rep("", times=length(xOnset)) 
  for (i in 1:length(xOnsetVal)) {
    # i=1
    # this has to be in a loop to iteratively shorten the comparison of peak values
    # if the data descend below onset or if the data descend by 
    # more than a proportion p after the end of a positive slope segment
    #
    # this loop will call the descentProp() helper function
    ###
    # first set the row to stop including xPeak values if they descend below the xOnset value
    stopRow <- which(myData[(xOnset[i]+1):length(myData)] < xOnsetVal[i])[1] + xOnset[i] - 1
    # there is no stop row when the data do not descend below onset, so use the last row instead
    if(is.na(stopRow)) stopRow <- length(myData)
    # exclude xPeak indices in each loop after the data descend below the xOnsetVal or stopRow
    xPeakLoop <- xPeak[which((xPeak > xOnset[i]) & (xPeak < stopRow))]
    # then get the row to stop including xPeak values 
    # when data descend a proportion p from the previous max Peak
    # to the response onset level 
    stopRow2 <- length(myData) # to ensure this works when descentStop==FALSE
    if(descentStop==TRUE) {
      stopRow2 <- descentProp(x=xOnset[i], y=xPeakLoop, z=myData, p=.5, ROWEnd=ROWEndRow)
      if(is.na(stopRow2)) stopRow2 <- length(myData)
    }
    #
    xPeakLoop <- xPeakLoop[which((xPeakLoop > xOnset[i]) & (xPeakLoop <= stopRow2))]
    #
    # use the xPeakLoop vector to determine the max xPeak for each xOnset
    if(length(xPeakLoop) > 0) {  
      yChange[i] <- xPeakLoop[which.max(myData[xPeakLoop[xPeakLoop >= xOnset[i]]] - xOnsetVal[i])]
    }
    # loop output is a vector yChange to index the peak row for max response for each xOnset index
    #
  } # end for loop to select the max change for each xOnset row
  #
  #########
  #
  # remove NAs that may result from different vector lengths
  yChange <- as.numeric(na.omit(yChange))
  #
  # fix condition where there is no onset or peak
  if(is.na(yChange[1])) yChange[1] <- xOnset[1]
  #
  # compute the differences between onset and peak values
  yChangeDiffs <- myData[yChange] - myData[xOnset]
  #
  # compute the index of the max change from xOnset to xPeak
  yChangeMaxIndex <- which.max(yChangeDiffs)
  #
  # compute the onset
  yChangeOnset <- xOnset[yChangeMaxIndex]
  yChangeOnsetValue <- myData[yChangeOnset]
  #
  # compute the peak
  yChangePeak <- yChange[yChangeMaxIndex]
  yChangePeakValue <- myData[yChange[yChangeMaxIndex]]
  #
  # and finally the change from onset to peak
  yChangeValue <- yChangePeakValue - yChangeOnsetValue
  # yChangeValue <- myData[yChange[yChangeMaxIndex]] - myData[xOnset[yChangeMaxIndex]]
  #
  #####################
  #
  #   print(yChangeOnset)
  #   print(yChangeOnsetValue)
  #   print(yChangePeak)
  #   print(yChangePeakValue)
  #   print(yChangeValue)
  #
  #########################
  #
  # fix condition where yChangeOnset == yChangePeak
  if(yChangeOnset == yChangePeak) { 
    yChangeOnset <- yChangeOnset - 1
    yChangeOnsetValue <- myData[yChangeOnset]
  }
  #
  ####################
  #
  # construct the output vector
  outputVector <- c(yChangeOnset, 
                    yChangePeak, 
                    yChangeOnsetValue, 
                    yChangePeakValue, 
                    yChangeValue,
                    stopRow2,
                    segmentTitle)
  names(outputVector) <- c("responseOnsetRow",
                           "responsePeakRow",
                           "responseOnsetValue",
                           "responsePeakValue",
                           "responseChangeValue",
                           "stopRow",
                           "segmentTitle")
  #
  return(outputVector)  
  #
} # end amplitudeExtract function 

#####

# amplitudeExtract(extractList=extractList, strictWindow=FALSE, strictROW=FALSE, descentStop=FALSE)
