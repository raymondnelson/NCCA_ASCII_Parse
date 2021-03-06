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
# 9b. for each onset exclude response peaks after the data descend 50% from the previous max peak
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


cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15

#####

amplitudeExtract <- function(x, begin, end, answer, start, lat, label, segmentName, nSmooth) {
  # function to extract the amplitude of EDA increase in response to a stimulus
  #
  # x is a vector of time series data for a single stimulus segment
  # begin is a scalar indicating the row number of the onset of the stimulus
  # end is a scalar indicating the row number of the end of the stimulus
  # answer is a scalar indicating the row number of the verbal answer
  # start is the row number from the chart data frame
  # Lat is the required latency after stimulus onset before which a responses is not evaluated
  # measuredSeg is the length of the measurment segment in seconds
  # cps is the sampling rate in cycles per second
  # prestimSeg is the length of the prestimulus segment in seconds
  # ROWEnd is the number of seconds after the verbal answer
  # ROWEnd defines the end of the segment during which a physiological response must begin
  # onset of physiological response the onset of a positive slope segment during ROW
  #
  ####
  
  strictROW <- FALSE # TRUE to ignore positive slope segments starting after ROW
  
  longResponse <- TRUE # TRUE to score responses to peak outside the scoring window
  
  descentStop <- TRUE # will not score reactions after the data descend a specified proportion
   
  # a time series vector including 
  # 5 seconds before and 15 seconds after stimulus onset
  # myData <- segmentDF[,column]
  myData <- x
  
  myData <- na.omit(myData)
  DFRows <- length(myData)
  
  Begin <- begin
  End <- end
  Answer <- answer
  Lat <- lat
  
  #startRow <- segmentDF$Sample[1]
  startRow <- start
  
  # prestimRow <- eventDF$Begin - (startRow-1) - (cps*prestimSeg)
  prestimRow <- Begin - (startRow-1) - (cps*prestimSeg)
  # correction for prestimRow values < 1
  if(prestimRow<=0) prestimRow <- 1
  onsetRow <- Begin - (startRow-1)
  latRow <- onsetRow + cps*Lat
  offsetRow <- End - (startRow-1)
  answerRow <- Answer - (startRow-1)
  # correction if there is no answer (answer row will be the same as offsetRow)
  if(answerRow==offsetRow) answerRow <- answerRow+1
  ROWEndRow <- answerRow + (cps*ROWEnd)
  # correction if ROWEndRow > DFRows
  if(ROWEndRow > DFRows) ROWEndRow <- DFRows
  endRow <- onsetRow + (cps*measuredSeg)
  # correction if endRow > DFRows
  if(endRow > DFRows) endRow <- DFRows
  
  ###
  
  # a private function to determine the slope
  slopeDir <- function(z=myData) {
    z1 <- z
    diff1 <- diff(z)
    z1 <- ifelse(diff1==0,
                 z1 <- 0,
                 # ifelse is vectorized and does not require a control loop
                 ifelse(diff1>0,
                        z1 <- 1,
                        z1 <- -1)
    )
    return(c(z1, 0))
  } # end slopeDir function
  
  mySlope <- slopeDir(z=myData)  
  
  ###
  
  # private function to remove slope changes less than n samples
  smoothSlope <- function(x=mySlope, y=15, n=4) {
    # x is a time series vector of positive (1) negative (-1) and zero (0) slope changes
    xPos <- ifelse(x == 1, 1, 0) 
    xPos1 <- rep(0, times=length(x))
    # make a run sum for all positive slope segements
    for (i in 2:length(xPos)) {
      if(xPos[i] != 0) xPos1[i] <- xPos[i] + xPos1[i-1]
    }
    # propagate the positive slope run length
    xPos2 <- xPos1
    for (j in length(xPos2):2) {
      if(xPos2[j] != 0 & xPos2[j-1] != 0) { xPos2[j-1] <- xPos2[j] }
    }
    # remove short runs
    xPos3 <- xPos2
    for (k in 1:length(xPos3)) {
      if(xPos3[k] < nSmooth) xPos3[k] <- 0
    }
    ###
    xNeg <- ifelse(x == -1, -1, 0)
    xNeg1 <- rep(0, times=length(x))
    # run sum for all negative slope segments
    for (l in 2:length(xNeg)) {
      if(xNeg[l] != 0) xNeg1[l] <- xNeg[l] + xNeg1[l-1]
    }
    # propagate the run length
    xNeg2 <- xNeg1
    for (m in length(xNeg2):2) {
      if(xNeg2[m] != 0 & xNeg2[m-1] != 0) { xNeg2[m-1] <- xNeg2[m] }
    }
    # remove short runs
    xNeg3 <- xNeg2
    for (n in 1:length(xNeg3)) {
      if(xNeg3[n] > -nSmooth) xNeg3[n] <- 0
    }
    ###
    x1 <- rep(0, times=(length(x)))
    # combine the positive and negative vectors
    x1 <- ifelse(xPos3 > 0, 
                 x1 <- xPos3, 
                 ifelse(xNeg3 < 0,
                        x1 <- xNeg3,
                        x1 <- 0))
    return(x1)
  } # end smoothSlope function
  
  mySlope1 <- smoothSlope(x=mySlope)
  
  ###
  
  # private function to fill the slope value of 
  fillSlope <- function(x=mySlope1) {
    # x is a vector of slope valences from which short runs are removed and set to 0
    # 0 slope segments following positive slope segments are filled positive
    # 0 slope segement following negative slope segments are filled negative
    x1 <- x
    for (i in 2:length(x)) {
      if(x[i] == 0) x1[i] <- x1[i-1]
    }
    x2 <- ifelse(x1 > 0,
                 x2 <- 1,
                 ifelse(x1 < 0,
                        x2 <- -1,
                        0))
    return(x2)
  } # end fillSlope function
  
  mySlope2 <- fillSlope(x=mySlope1)
  
  ###
  
  # another private function to find the positive slope segments
  positiveSlope <- function(x=mySlope2) {
    # x is a vector of slope valences for each sample in the time series data
    y <- x # to make a container
    # if else is vectorized and requires no control loop
    y <- ifelse(x>=1,
                y <- 1,
                y <- 0)
    return(y)  
  } # end positiveChange function
  
  posSlope <- positiveSlope(x=mySlope2)
  
  ###
  
  # a private function to make vector of positive slope onset rows
  positiveOnset <- function(x=posSlope) {
    xOnset <- ifelse(x==0,
                     # compare every x + next x > 0 
                     ifelse((x[1:(length(x)-1)] + x[2:length(x)]) > 0,
                            1,
                            0),
                     0)
    # phase correction
    xOnset <- c(0, xOnset[1:(length(xOnset)-1)])
    return(xOnset)
  } # end positiveOnset function
  
  xOnset1 <- positiveOnset(x=posSlope)

  ###########

  # private function to find a change or increase in upward slope energy
  #i<-1
  slopeChange <- function(x=myData, N=4) {
    # compare the 3 seconds with the next 1 second
    # y is the number of second to evaluate
    y <- rep(0, times=length(myData))
    for (i in 1:(length(y)-(N*cps))) {
      preLen <- cps*(N-1)
      postLen <- (cps*N)-1
      preDiff <- diff(x[i:(i+preLen)])
      postDiff <- c(diff(x[(i+preLen):(i+postLen)]))
      if(mean(postDiff) >= qnorm(.999, mean=mean(preDiff), sd=sd(preDiff))) { 
        y[(i+(N*cps)-1)-(.5*cps)] <- sign(mean(postDiff))
      }
    } # end for loop
    # get the onset
    y <- positiveOnset(y)
    return(y)
  } # end slopeChange function
  
  sChange <- slopeChange(x=myData, N=3) # not yet used for anything
  
  print(segmentName)
  
  ### 
  
  #combine the sChange and xOnset1 vectors
  if(length(sChange) > 0) { xOnset1[which(sChange==1)] <- 1 }
  
  ###
  
  # private function to locate the peak of all positive slope sections 
  slopePeak <- function(x=mySlope2) {
    xPeak <- x
     xPeak <- ifelse(x == 1,
                    ifelse((x[1:(length(x)-1)] + x[2:length(x)]) == 2,
                           xPeak <- 0,
                           xPeak <- 1),
                    xPeak <- 0)
    return(xPeak)
  } # end slopePeak function
  
  xPeak1 <- slopePeak(x=mySlope2)
  
  ###
  
  # to include a peak for positive slope segments that start in the ROW and end after the ROW
  # add the ROWEndRow to the xPeak1 vector if it is positive
  if(posSlope[ROWEndRow] == 1) xPeak1[ROWEndRow] <- 1
  
  # to include a peak for positive slope seg that starts in the measurement window and ends after
  # add the endRow slope to xPeak1 vector if it is positive.
  if(posSlope[endRow] == 1) xPeak1[endRow] <- 1 
  
  ####################
  
  # get xOnset and xPeak row for positive slope segments   
  xOnset <- which(xOnset1 == 1)
  xPeak <- which(xPeak1 == 1)
  
  # fix condition when xOnset or xPeak is length zero
  if(length(xOnset) == 0) xOnset <- endRow
  if(length(xPeak) == 0) xPeak <- endRow
  
  # get the y-axis values for xOnset and xPeak rows
  xOnsetVal <- myData[xOnset]
  xPeakVal <- myData[xPeak]

  ################
  
  # keep onset rows after the required response latency period
  xOnsetVal <- xOnsetVal[which(xOnset >= latRow)]
  xOnset <- xOnset[xOnset >= latRow]

  # Keep only those slope peaks that are >= the xOnset 
  xPeakVal <- xPeakVal[which(xPeak >= xOnset[1])]
  xPeak <- xPeak[xPeak >= xOnset[1]]

  ####
  
  # keep only those onset rows that occur before ROWEndRow
  xOnsetVal <- xOnsetVal[which(xOnset <= ROWEndRow)]
  xOnset <- xOnset[xOnset <= ROWEndRow]

  # fix xOnset again if there is no response onset in the ROW
  if(length(xOnset) == 0) {
    xOnset <- endRow # may need to be endRow -1 because response onset and end cannot be the same
    xOnsetVal <- myData[xOnset]
  }

  # fix condition where xOnset is >= the number of rows in the data frame  
  if(xOnset[1] >= length(myData)) {
    xOnset <- length(myData)
    xOnsetVal <- myData[xOnset]
  }
  
  ##################

  # Keep only those slope peaks that are >= the xOnset row
  xPeak <- xPeak[xPeak >= xOnset[1]]
  # xPeakVal <- xPeakVal[which(xPeak >= xOnset[1])]
  xPeakVal <- myData[xPeak]
  
  ###

  # keep only the slope peaks that are <= ROWEndRow 
  # plus one additional peak if ROWEndRow is positive slope  
  # if FALSE this will keep all peaks in the measurement
  if(strictROW == TRUE) { # use FALSE to include any max peak in the measurement period
    ifelse(posSlope[ROWEndRow] == 1,
           {xPeak <- xPeak[1:length(which(xPeak <= ROWEndRow)) + 1]},
           {xPeak <- xPeak[xPeak <= ROWEndRow]})
  }
  xPeakVal <- myData[xPeak]
  
  ###

  # keep only those slope peaks that are <= endRow
  if(longResponse == TRUE) { # use false to stop responses at the end of the measurement window
    ifelse(xOnset[1] <= ROWEndRow,
      ifelse(posSlope[endRow] == 1,
             {xPeak <- xPeak[1:(length(which(xPeak <= endRow)) + 1)]},     
             {xPeak <- xPeak[xPeak <= endRow]}),
    {xPeak <- xPeak[xPeak <= endRow]})
  }
  xPeak <- as.numeric(na.omit(xPeak))
  # xPeakVal <- as.numeric(na.omit(xPeakVal))
  xPeakVal <- myData[xPeak]
  
  ###

  # fix empty xPeak row
  if(length(xPeak) == 0) { 
    xPeak <- xOnset[1]
    xPeakVal <- myData[xPeak]
  }
  
  # now we have vectors for xOnset, xPeak, xOnsetVal and xPeakVal
  print(xOnset)
  print(xOnsetVal)
  print(xPeak)
  print(xPeakVal)
  
  #########################

  #########################

  # source the maxChange.R script to get
  # source('~/Documents/R_programming/NCCA_ASCII_Parse/maxChange.R', echo=TRUE)

  #####
  # a helper function
  # to exclude rows after the data descend 
  # more than a proportion p from the previous highest peak
  
  descentProp <- function(x=xOnset, y=xPeakLoop, z=myData, p=.5) {
    # function to exclude rows after the data descend more than a proportion p
    # from a the previous highest peak after response onset
    # x is a vector of reponse onset rows
    # y is a is a vector of peak rows prior to the point where data deseend
    # below the onset value
    # z is a vector of time series values
    # p is a proportion for which the data are evaluated if it descend below p
    # for the prior maximum response peak value - response onset value 
    
    yChangeOnset <- x
    xPeakLoop <- y
    myData <- z
    prop <- p
    #
    xPeakLoopValues <- myData[xPeakLoop]
    xPeakLoopDiffs <- xPeakLoopValues - myData[yChangeOnset]
    cutValues <- prop * xPeakLoopDiffs
    myValues <- myData[(yChangeOnset+1):length(myData)]
    myDiffs <- myValues - myData[yChangeOnset]
    mySlope <- c(0, ifelse(diff(myValues)>0, 1, ifelse(diff(myValues)<0, -1, 0)))
    #
    # now look for descending myValues that are less than descCut for previous peaks
    #
    # make an empty vector
    cutVector <- numeric(length=length(myDiffs))
    # populate the cut vector
    for (i in 1:length(myDiffs)) {
      # make a vector myCutValues for the vector myDiffs
      # myCutValues needs to be 1/2 the diff of the preceeding max peak
      currentRow <- i + yChangeOnset - 1
      ifelse(length(which(xPeakLoop <= currentRow)) > 0,
             myCutValue <- max(cutValues[which(xPeakLoop <= currentRow)]),
             myCutValue <- 0)
      #
      # use i to check the max preceeding diff
      # if(myDiffs[i] <  myCutValue) { cutVector[i] <- myCutValue }
      cutVector[i] <- myCutValue
    } # end for loop
    #
    # print(cutVector)
    #
    # use only descending slope segments
    descRows <- which(mySlope == -1)
    # ignore the first several descending rows 
    # to avoid over sensitivity to high frequency noise
    #
    # descRows <- descRows[91:length(descRows)]
    cutVector <- c(rep(0, time=90), cutVector[91:length(cutVector)])
    #
    # keep only those descending rows for which 
    # difference is smaller than the cut point porportion
    cutRows <- descRows[myDiffs[descRows] < cutVector[descRows]]
    stopRow2 <- cutRows[1] + yChangeOnset - 1
    #   
    # use the last row in the data vector of stopRow2 is NA
    if(is.na(stopRow2) == TRUE) stopRow2 <- length(myData)
    #
    # return a scalar with the stop row 
    # after which peaks are excluded 
    return(stopRow2)
    #
  } # end descentProp function
  
  ##########
  
  # first make an empty vector for the loop output
  yChange <- rep("", times=length(xOnset)) 
  
  # i=1
  # loop to select the max change for each xOnset row
  for (i in 1:length(xOnsetVal)) {
    # this has to be in a loop to iteratively shorten the comparison of peak values
    # if the data descend below onset
    # or if the data descend a proportion p after the end of a positive slope segment
    #
    # row to stop including xPeak values if they descend below the xOnset value
    stopRow <- which(myData[(xOnset[i]+1):length(myData)] < xOnsetVal[i])[1] + xOnset[i]
    # there is no stop row when the data do not descend below onset, so use the last row instead
    if(is.na(stopRow)) stopRow <- length(myData)
    #
    # exclude xPeak rows in each loop after the data descend below the xOnsetVal 
    xPeakLoop <- xPeak[which((xPeak > xOnset[i]) & (xPeak < stopRow))]
    # 
    # row to stop including xPeak values if the data descend a proportiont p of the previous max Peak
    stopRow2 <- descentProp(x=xOnset[i], y=xPeakLoop, z=myData, p=.5)
    if(is.na(stopRow2)) stopRow2 <- length(myData)
    #
    # exclude xPeak rows in each loop after the data descend to a proportion p
    # from maximum xPeak change prior to each xPeak
    xPeakLoop <- xPeakLoop[which((xPeakLoop > xOnset[i]) & (xPeakLoop <= stopRow2))]
    #
    # use the xPeakLoop vector to determine the max xPeak for each xOnset
    if(length(xPeakLoop) > 0) {  
      yChange[i] <- xPeakLoop[which.max(myData[xPeakLoop[xPeakLoop >= xOnset[i]]] - xOnsetVal[i])]
    }
    # yChange is a vector of xPeak rows for the max increase following each xOnsetVal
    #
    # loop output is a vector yChange to index the peak row for for each xOnset row
    #
  } # end for loop
  
  # remove NAs that may result from different vector lengths
  yChange <- as.numeric(na.omit(yChange))
  
  # fix condition where there is no onset or peak
  if(is.na(yChange[1])) yChange[1] <- xOnset[1]
  
  # compute the differences between onset and peak values
  yChangeDiffs <- myData[yChange] - myData[xOnset]
  
  # compute the index of the max change from xOnset to xPeak
  yChangeMaxIndex <- which.max(yChangeDiffs)
  
  # compute the onset
  yChangeOnset <- xOnset[yChangeMaxIndex]
  yChangeOnsetValue <- myData[yChangeOnset]
  
  # compute the peak
  yChangePeak <- yChange[yChangeMaxIndex]
  yChangePeakValue <- myData[yChange[yChangeMaxIndex]]
  
  # and finally the change from onset to peak
  yChangeValue <- yChangePeakValue - yChangeOnsetValue
  yChangeValue <- myData[yChange[yChangeMaxIndex]] - myData[xOnset[yChangeMaxIndex]]
  
  print(yChangeOnset)
  print(yChangeOnsetValue)
  print(yChangePeak)
  print(yChangePeakValue)
  print(yChangeValue)

  #########################

  #########################
  
  # fix condition where yChangeOnset == yChangePeak
  if(yChangeOnset == yChangePeak) { 
    yChangeOnset <- yChangeOnset - 1
    yChangeOnsetValue <- myData[yChangeOnset]
  }

  ####################
  
  ####################
  
  # construct the output vector
  outputVector <- c(yChangeOnset, 
                    yChangePeak, 
                    yChangeOnsetValue, 
                    yChangePeakValue, 
                    yChangeValue,
                    stopRow2,
                    segmentName)
  names(outputVector) <- c("responseOnsetRow",
                           "responsePeakRow",
                           "responseOnsetValue",
                           "responsePeakValue",
                           "responseChangevalue",
                           "stopRow",
                           "segmentName")
  
  return(outputVector)  
  
} # end amplitudeExtract function

#####

# amplitudeExtract(x, begin, end, answer, start, lat, label, segmentName)


