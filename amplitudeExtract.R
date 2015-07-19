# amplitude extract function for EDA and Cardio data
#
# 1. compute ROW as the segment before ROWEndRow and after the onset latencyRow 
# 2. compute the row number for the onset of all positive slope segments in the ROW
# 3. compute the onset value for the onset of all positive slope segments inside the ROW 
# 4. locate the subsequent row and value for all positive peak points after each onset
# 4a. add additional peak rows and values for which the onset values occur
#     inside the measurement period after ROWEndRow and the onset value is > than
#     the max onset value inside the ROW
# 5. the last peak can be outside the ROW or even outside the measurement period
# 6. compute the difference between each onset and all subsquent peak values
# 7. select the onset and subsequent peak with the max difference  
# 
#
######################################


mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")

mySegmentLists <- mySegmentLists[1]
myEventLists <- myEventLists[1]

mySegmentDF <- get(mySegmentLists)[[10]]
myEventDF <- get(myEventLists)[[10]]

# myData <- mySegmentDF$AutoEDA
myData <- mySegmentDF$CardioMA

begin <- myEventDF$Begin
end <- myEventDF$End
answer <- myEventDF$Answer
# if(answer == end) answer <- answer+1
start <- mySegmentDF$Sample[1]
lat <- .5
nSmooth <- 4
label <- myEventDF$Label
segmentName <- paste(mySegmentDF$examName[1], mySegmentDF$chartName[1], myEventDF$Label, sep="_")

#########


cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15

x <- myData

amplitudeExtract <- function(x, begin, end, answer, start, lat, label, segmentName) {
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


  
  
  # deleted some unused stuff 7-16-2015
  
  
  
  
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
  if(xOnset[1] >= nrow(segmentDF)) {
    xOnset <- (nrow(segmentDF))
    xOnsetVal <- myData[xOnset]
  }
  
  ##################

  # Keep only those slope peaks that are >= the xOnset row
  xPeakVal <- xPeakVal[which(xPeak >= xOnset[1])]
  xPeak <- xPeak[xPeak >= xOnset[1]]

  ###

  # keep only the slope peaks that are <= ROWEndRow 
  # plus one additional peak if ROWEndRow is positive slope  
  # if FALSE this will keep all peaks in the measurement
  if(strictROW == TRUE) { # use FALSE to include any max peak in the measurement period
    ifelse(posSlope[ROWEndRow] == 1,
           {xPeak <- xPeak[1:length(which(xPeak <= ROWEndRow)) + 1]; xPeakVal <- xPeakVal[xPeak <= ROWEndRow]},
           {xPeak <- xPeak[xPeak <= ROWEndRow]; xPeakVal <- xPeakVal[xPeak <= ROWEndRow]})
  }

  ###

  # keep only those slope peaks that are <= endRow
  if(longResponse == TRUE) { # use false to stop responses at the end of the measurement window
    ifelse(xOnset[1] <= ROWEndRow,
      ifelse(posSlope[endRow] == 1,
             {xPeak <- xPeak[1:(length(which(xPeak <= endRow)) + 1)]; xPeakVal <- xPeakVal[xPeak <= endRow]},     
             {xPeak <- xPeak[xPeak <= endRow]; xPeakVal <- xPeakVal[xPeak <= endRow]}),
    {xPeak <- xPeak[xPeak <= endRow]; xPeakVal <- xPeakVal[xPeak <= endRow]})
  }
  xPeak <- as.numeric(na.omit(xPeak))
  xPeakVal <- as.numeric(na.omit(xPeakVal))
  ###

  # fix empty xPeak row
  if(length(xPeak) == 0) { 
    xPeak <- xOnset[1]
    xPeakVal <- myData[xPeak]
  }
  
  
  
  
  # more deleted unused stuff 7-16-2015
  
  
  
  
  ###############################
  
  # compute a vector of max amplitudes for each onset and all subsequent Peaks

  # make a loop output vector
  yChange <- rep("", times=length(xOnset)) 
  # i <- 1
  for (i in 1:length(xOnset)) {
    # this has to be in a loop to iteratively shorten the comparison
    yChange[i] <- max(myData[xPeak[xPeak >= xOnset[i]]] - myData[xOnset[i]])
    # yChange is a vector of xPeak rows for the max increase following each xOnset
  } # end for loop

  # remove NAs that may result from different vector lengths
  yChange <- as.numeric(na.omit(yChange))
  
  
  ### get the response onset
  
  ### need a loop to select the next onset 
  
  # get the max change value from the vector. this is the response onset row 
  yChangeMaxIndex <- which.max(as.numeric(yChange))
  
  # use the first if the index fails or is null
  if(length(yChangeMaxIndex) == 0) yChangeMaxIndex <- 1
  
  
  ##### now make a loop to use subsequent onset points if necessary
  
  for (i in yChangeMaxIndex:length(yChange)) { 
    # added loop 7-18-2015 to select a subsequent xOnset if the data descend 1/2 way

  # get the onset row by indexing the max change value
  yChangeOnset <- xOnset[yChangeMaxIndex]
  print(yChangeOnset)

  # get the onset value
  yChangeOnsetValue <- myData[yChangeOnset]

  #####################

  

  
  # even more deleted unused stuff 7-16-2015
  
  
  
  
  ##########################

  
  
  
  # 4th section with deleted unused stuff 7-16-2015
  
  
    
  
  #########################

  # make a short vector of the xPeak row >=  xOnset
  xPeakShort <- xPeak[xPeak >= yChangeOnset] # could use which() but it makes no difference
  print(xPeakShort)
  
  #############################
  
  # 7-16-15 remove all ascending onset and peak segments 
  # after the data descend below the yChangeOnsetValue
  
  # do this by monitoring the yAxis difference after the yChangeValue
  # and remove all peaks after the first row where the diff is negative 
  
  negDiff <- function(x=yChangeOnset, y=yChangeOnsetValue, z=myData) {
    # function to remove all peak segments after the data descend below the yChangeOnset
    yChangeOnset <- x
    yChangeOnsetValue <- y
    myData <- z
    #
    myValues <- myData[(yChangeOnset+1):length(myData)]
    myDiffs <- myValues - yChangeOnsetValue
    stopRow <- yChangeOnset + which(myDiffs < 0)[1]
    #
    return(stopRow)
  } # end negDiff function
  
  stopRow <- negDiff(x=yChangeOnset, y=yChangeOnsetValue, z=myData)
  
  # remove peaks if and after the data descend below the reposnse onset value
  if(!is.na(stopRow)) xPeakShort <- xPeakShort[xPeakShort <= stopRow]
  
  #########################
 
  # remove upward slope segments after the ROW and in the measurement window
  # if the data descend more than 50% from the previous highest peak
  
  descentRule <- function(x=yChangeOnset, y=xPeakShort, z=myData, p=.5) {
    # function to remove positive slope segments 
    # if the data descend more than a proportion p
    yChangeOnset <- x
    xPeakShort <- y
    myData <- z
    prop <- p
    #
    xPeakShortValues <- myData[xPeakShort]
    xPeakShortDiffs <- xPeakShortValues - myData[yChangeOnset]
    cutValues <- prop * xPeakShortDiffs
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
      ifelse(length(which(xPeakShort <= currentRow)) > 0,
             myCutValue <- max(cutValues[which(xPeakShort <= currentRow)]),
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
    
    # descRows <- descRows[91:length(descRows)]
    cutVector <- c(rep(0, time=90), cutVector[91:length(cutVector)])
    
    # keep only those descending rows for which 
    # difference is smaller than the cut point porportion
    cutRows <- descRows[myDiffs[descRows] < cutVector[descRows]]
    stopRow2 <- cutRows[1] + yChangeOnset -1
    #   
    return(stopRow2)
    #
  } # end descentRule function
  
  stopRow2 <- descentRule(x=yChangeOnset, y=xPeakShort, z=myData, p=.5)
  
  # remove peaks after the data descend beyond the specified proportion
  if(descentStop == TRUE) { 
    if(!is.na(stopRow2)) xPeakShort <- xPeakShort[xPeakShort < stopRow2] 
  } # end if
  
} # end loop over xOnset 
  
  ####################### 

  # make a short vector of the peak values
  xPeakShortValues <- myData[xPeakShort] 
  
  # make a vector of the differences between xPeakShortValues and yChangeOnsetValue
  xPeakShortDifferences <- xPeakShortValues - yChangeOnsetValue
  
  # use xOnset to xPeak differences to remove measurements after data descend below the xOnsetVal
  if(length(which(xPeakShortDifferences < 0)) > 0) {  
    # remove differences after the first negative difference
    # this will occur when the data descend below the onset value
    removeNegDiff <- c(which(xPeakShortDifferences < 0)[1]:length(xPeakShortDifferences))
    xPeakShort <- xPeakShort[-removeNegDiff]
    xPeakShortValues <- xPeakShortValues[-removeNegDiff]
    xPeakShortDifferences <- xPeakShortDifferences[-removeNegDiff]
  }

  ###

  # get the peak row
  yChangePeak <- xPeakShort[which.max(xPeakShortDifferences)]

  # fix empty value
  if(length(yChangePeak) == 0) yChangePeak <- yChangeOnset # added 7-12-15
  
  ############################
  
  # fix condition where yChangeOnset == yChangePeak
  if(yChangeOnset == yChangePeak) { 
    yChangeOnset <- yChangeOnset - 1
    yChangeOnsetValue <- myData[yChangeOnset]
  }

  # get the peak value 
  yChangePeakValue <- myData[yChangePeak]
  # or this way
  # yChangePeakValue <- xPeakShortDifferences[which.max(xPeakShortDifferences)] + yChangeOnsetValue
  
  ###
  
  # compute the change from response onset to response peak
  yChangeValue <- yChangePeakValue - yChangeOnsetValue
  # or
  # yChangeValue <- max(xPeakShortDifferences)
  
  ###
  
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

