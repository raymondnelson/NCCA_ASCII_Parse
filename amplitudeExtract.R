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

mySegmentDF <- get(mySegmentLists)[[1]]
myEventDF <- get(myEventLists)[[1]]

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
    # compare the 4 seconds with the next 1 seconc
    # y is the number of second to evaluate
    y <- rep(0, times=length(myData))
    for (i in 1:(length(y)-(N*cps))) {
      preLen <- cps*(N-1)
      postLen <- (cps*N)-1
      preDiff <- diff(x[i:(i+preLen)])
      postDiff <- c(diff(x[(i+preLen):(i+postLen)]))
      if(mean(postDiff) >= qnorm(.99, mean=mean(preDiff), sd=sd(preDiff))) { 
        y[(i+(N*cps)-1)] <- sign(mean(postDiff))
      }
    } # end for loop
    # get the onset
    y <- positiveOnset(y)
    return(y)
  } # end slopeChange function
  
  sChange <- slopeChange(x=myData, N=3) # not yet used for anything
  
  print(segmentName)
  print(which(sChange==1))
  print(which(xOnset1==1))
  
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

  #################
  
#   ### save the onset rows after ROWEndRow
#   
#   # save xOnset rows after the ROWEndRow
#   save_xOnset <- na.omit(xOnset[which(xOnset > ROWEndRow)])
#   save_xOnsetVal <- na.omit(xOnsetVal[which(xOnset > ROWEndRow)])
#   
#   # keep only those save_xOnset rows that are less than endRow 
#   save_xOnset <- save_xOnset[which(save_xOnset <= endRow)]
#   save_xOnsetVal <- save_xOnsetVal[which(save_xOnset <= endRow)]

  ### 

#   keep only save_xOnset rows for which the y-value is >= max xOnset y-value during ROW
#   if(length(save_xOnset) > 0) {
#   save_xOnset <- which(save_xOnset[1:which(myData[save_xOnset] >= max(myData[xOnset]))])
#   }
#   
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
  
#   # fix condition where xPeak is >= number rows in the data frame
#   if(xPeak[1] >= nrow(segmentDF)) xPeak[1] <- (nrow(segmentDF))
  
  # keep a  number of subsequent peak rows equal to the number of onset values
  # xPeak <- xPeak[1:length(xOnset)]

  ### THIS IS THE PROBLEM WITH ASCENDING SEGMENTS WHERE THERE IS NO ONSET
#   # fix condition where the last xPeak row is > endRow
#   if(xPeak[length(xPeak)] > endRow) xPeak[length(xPeak)] <- endRow # was endRow

#   ##################
#   
#   # keep a number of save_xPeak rows after the ROWEndRow and before endRow
# #  # equal to the number of save_xOnset values after the ROWEndRow
#   if(length( save_xPeak)
#   if(length(save_xOnset) > 0) save_xPeak <- save_xPeak[1:length(save_xOnset)]

  ###################

#   # concat the rows after ROWEndRow to the rows before ROWEndRow
#   xOnset <- c(xOnset, save_xOnset)
#   xPeak <- c(xPeak, save_xPeak)

  #### keep the max Peak after ROWEnd and before end Row 
  
  # add extra xPeak values after ROWEndRow only if the value is greater
  # than the last positive slope onset
  # if(save_xPeak != NULL) {
  #   save_xPeakValue <- myData[save_xPeak]
  #   save_xPeak <- save_xPeak[max(save_xPeakValue)]
  # }    
  
  ################
  
  # fix NAs in the vectors positive slope onset and positive slope peak values
#   xOnset <- as.integer(na.omit(xOnset))
#   xPeak <- as.integer(na.omit(xPeak))
  
  # fix condition when vector of peak values is length zero
  # if(length(xPeak) == 0) xPeak <- xOnset[1]
  
  #     # keep one additional peak if the slope is positive at the ROWEndRow
  #     ifelse(mySlope[ROWEndRow] == 1, 
  #            xPeak <- xPeak[1:(length(xPeak[xPeak <= ROWEndRow])+1)],
  #            xPeak <- xPeak[xPeak <= ROWEndRow])
  
  ###############################
  
  # compute a vector of max amplitudes for each onset and all subsequent Peaks
  # yChange is a vector of xPeak rows for the max increase following each xOnset
  yChange <- rep("", times=length(xOnset)) 
  # i <- 1
  for (i in 1:length(yChange)) {
    # this has to be in a loop to iteratively shorten the comparison
    # yChange[i] <- max(myData[xPeak[i:length(xPeak)]] - myData[xOnset[i]])
    print(myData[xPeak[xPeak >= xOnset[i]]])
    print(myData[xOnset[i]])
    yChange[i] <- max(myData[xPeak[xPeak >= xOnset[i]]] - myData[xOnset[i]])
  } # end for loop

  # remove NAs that may result from different vector lengths
  yChange <- as.numeric(na.omit(yChange))
  
  # get the max change value from the vector. this is the response onset row 
  yChangeMaxIndex <- which.max(as.numeric(yChange))
  
  # use the first if the index fails or is null
  if(length(yChangeMaxIndex) == 0) yChangeMaxIndex <- 1

  # get the onset row by indexing the max change value
  yChangeOnset <- xOnset[yChangeMaxIndex]

  # get the onset value
  yChangeOnsetValue <- myData[yChangeOnset]

  ###
  
  #   # get the peak row by indexing the xPeak vector
  #   yChangePeak <- xPeak[which.max(yChange):length(xPeak)][which.max(myData[xPeak[which.max(yChange):length(xPeak)]] - 
  #                                                                      myData[xOnset[which.max(yChange)]])]
  #   
  #   #same
  #   yChangePeak <- xPeak[which.max(yChange):length(xPeak)][which.max(abs(myData[xPeak[yChangeMaxIndex:length(xPeak)]] - 
  #                                                          yChangeOnsetValue  ))]
  
  print(xOnset)
  print(xPeak)
  print(yChangeMaxIndex)
  print(yChangeOnset)
  
  ##########################

  ### index the peak row (this is simpler than indexing the onset row)
  
  # first remove all xOnset and xPeak rows for which the value is less than the xOnsetVal
  # xOnset <- xOnset[-c(which(myData[xOnset] < yChangeOnsetValue)[1]:length(xOnset))]
  # xPeak <- xPeak[xPeak < xOnset[which(myData[xOnset] < yChangeOnsetValue)[1]]]

  ###

  # make a short vector of the xPeak row >=  xOnset
  xPeakShort <- xPeak[xPeak >= yChangeOnset] # could use which() but it makes no difference
  
  # make a short vector of the peak values
  xPeakShortValues <- myData[xPeakShort] 
  
  # make a vector of the differences between xPeakShortValues and yChangeOnsetValue
  xPeakShortDifferences <- xPeakShortValues - yChangeOnsetValue
  
  ### 

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
                    yChangeValue)
  names(outputVector) <- c("responseOnsetRow",
                           "responsePeakRow",
                           "responseOnsetValue",
                           "responsePeakValue",
                           "responseChangevalue")
  
  return(outputVector)  
  
} # end amplitudeExtract function

#####

# amplitudeExtract(x, begin, end, answer, start, lat, label, segmentName)

