


EDAExtract <- function(x=mDta, begin, end, answer, start) {
  # function to extract the amplitude of EDA increase in response to a stimulus
  #
  # x is a data frame of time series data for a single stimulus segment
  #
  # y is a data frame of Label, Begin, End, and Answer events for the stimulus segment, 
  # including prestimRow, onsetRow, latencyRow, offsetRow, answerRow, ROWEndRow, endRow
  # ouput is a named numerical vector
  # including responseOnsetRow, respeonsePeakRow, responseOnsetValue, responsePeakValue, and responseChangeValue
  #
  ####
  
  # myEventLists <- y
  # mySegmentLists <- x

  Begin <- begin
  End <- end
  Answer <- answer
  
  # startRow <- mySegmentLists$Sample[1]
  startRow <- start
  
  # correction for X announcement for which there may not be 5 seconds prestimulus
  #     ifelse(myEventLists$Begin <= (cps*prestimSeg),
  #            prestimRow <- 1,
  #            prestimRow <- myEventLists$Begin - (startRow-1) - (cps*prestimSeg))
  # prestimRow <- myEventLists$Begin - (startRow-1) - (cps*prestimSeg)
  prestimRow <- Begin - (startRow-1) - (cps*prestimSeg)
  # correction for values < 1
  if(prestimRow<=0) prestimRow <- 1
  # onsetRow <- myEventLists$Begin - (startRow-1)
  onsetRow <- Begin - (startRow-1)
  EDALatRow <- onsetRow + cps*EDALat
  offsetRow <- myEventLists$End - (startRow-1)
  # answerRow <- myEventLists$Answer - (startRow-1)
  answerRow <- Answer - (startRow-1)
  #correction if there is no answer (answer row will be the same as offsetRow)
  if(answerRow==offsetRow) answerRow <- answerRow+1
  ROWEndRow <- answerRow + (cps*ROWEnd)
  endRow <- onsetRow + (cps*measuredSeg)
  
  #     # a named numerical vector of 7 items, inluding:
  #     # prestimRow, onsetRow, EDALatRow, offsetRow, answerRow, ROWEndRow, endRow
  #     myEvents <- c(prestimRow, onsetRow, EDALatRow, offsetRow, answerRow, ROWEndRow, endRow)
  #     names(myEvents) <- c("prestimRow", "onsetRow", "EDALatRow", "offsetRow", "answerRow", "ROWEndRow", "endRow")
  
  ###
  
  # a time series vector including 
  # 5 seconds before and 15 seconds after stimulus onset
  # myData <- mySegmentLists$AutoEDA[prestimRow:endRow]
  myData <- x[prestimRow:endRow]
  
  ###
  
  # make a function to determine the slope
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
    return(z1)
  } # end slopeDir function
  
  mySlope <- slopeDir(z=myData)
  # add a zero at the end to correct for shortend vector length
  mySlope <- c(mySlope, 0)
  
  ###
  
  # make a function to find the onset of positive slope segments
  positiveChange <- function(x=mySlope) {
    # x is a vector of slope valences for each sample in the time series data
    y <- x # to make a container
    # if else is vectorized and requires no control loop
    y <- ifelse(x==1,
                y <- 1,
                y <- 0)
    return(y)  
  } # end positiveChange function
  
  posSlope <- positiveChange(x=mySlope)
  
  ###
  
  # make a vector of positive slope onset rows
  positiveOnset <- function(x=posSlope) {
    xOnset <- ifelse(x==0,
                     ifelse((x[1:(length(x)-1)] + x[2:length(x)]) > 0,
                            1,
                            0),
                     0)
    # phase correction
    xOnset <- c(0, xOnset[1:(length(xOnset)-1)])
    return(xOnset)
  } # end positiveOnset function
  
  xOnset <- positiveOnset(x=posSlope)
  # xOnset <- which(positiveChange()[1:ROWEndRow]==1)
  
  ###
  
  # function to locate the peak of all positive slope sections 
  slopePeak <- function(x=mySlope) {
    x1 <- x
    #       xPeak <- ifelse(x1 == -1,
    #                       0,
    #                       ifelse(x1 == 0,
    #                              0,
    #                              ifelse((x1[1:(length(x1)-1)] + x1[2:length(x1)]) == 2,
    #                                     0,
    #                                     1)))
    xPeak <- x
    xPeak <- ifelse(x == 1,
                    ifelse((x[1:(length(x)-1)] + x[2:length(x)]) == 2,
                           xPeak <- 0,
                           xPeak <- 1),
                    xPeak <- 0)
    return(xPeak)
  } # end slopePeak function
  
  xPeak <- slopePeak(x=mySlope)
  
  # correction 
  #    xPeak <- xPeak
  # xPeak <- xPeak[xPeak<=ROWEndRow]
  
  ###
  
  # got onset and peak row for positive slope segments   
  xOnset <- which(xOnset == 1)
  xPeak <- which(xPeak==1)
  
  # keep onset rows after the required EDA response latency period
  xOnset <- xOnset[xOnset >= EDALatRow]
  
  # keep only those onset values that occur before ROWEndRow
  xOnset <- xOnset[xOnset < ROWEndRow]
  
  # fix condition when xOnset is length zero
  if(length(xOnset) == 0) xOnset <- onsetRow
  
  # Keep only those slope peaks that are after the onset 
  xPeak <- xPeak[xPeak >= xOnset[1]]
  
  
  #######
  
  ##### keep the max Peak after ROWEnd and before end Row 
  
  # save the xPeak rows after the ROWend
  save_xPeak <- na.omit(xPeak[(length(xOnset)+1):length(xPeak)])
  #if(length(save_xPeak)==0) save_xPeak <- NULL
  
  # keep a  number of subsequent peak values equal to the number of onset values
  xPeak <- xPeak[1:length(xOnset)]
  
  # add extra xPeak values after ROWEnd if the value is greater
  # than the last positive slope onset
  # if(save_xPeak != NULL) {
  save_xPeakValue <- myData[save_xPeak]
  save_xPeak <- save_xPeak[max(save_xPeakValue)]
  xPeak <- c(xPeak, save_xPeak)
  # }    
  
  
  ################
  
  # fix NAs in the vector of peak values
  xPeak <- as.integer(na.omit(xPeak))
  
  # fix condition when vector of peak values is length zero
  if(length(xPeak) == 0) xPeak <- xOnset[1]+1
  
  #     # keep one additional peak if the slope is positive at the end of the ROWEnd
  #     ifelse(mySlope[ROWEndRow] == 1, 
  #            xPeak <- xPeak[1:(length(xPeak[xPeak <= ROWEndRow])+1)],
  #            xPeak <- xPeak[xPeak <= ROWEndRow])
  
  ###
  
  # compute a vector of max amplitudes for each onset and all subsequent Peaks
  yChange <- rep("", times=length(xOnset)) 
  # yChangeIndex <- rep("", times=length(xOnset)) 
  # yChangeMax <- 0
  # i <- 1
  for (i in 1:length(yChange)) {
    # yChange is a vector of xPeak rows for the max increase following each xOnset
    # yChangeOnset <- xOnset[i]
    # yChangePeak <-
    
    # this has to be in a loop to iteratively shorten the comparison
    yChange[i] <- max(myData[xPeak[i:length(xPeak)]] - myData[xOnset[i]])
    
    
  } # end for loop
  
  # get the max change value from the vector. this is the response onset row 
  yChangeMaxIndex <- which.max(as.numeric(yChange))
  
  # get the onset row by indexing the max change value
  yChangeOnset <- xOnset[yChangeMaxIndex]
  
  # get the onset value
  yChangeOnsetValue <- myData[yChangeOnset]
  
  #   # get the peak row by indexing the xPeak vector
  #   yChangePeak <- xPeak[which.max(yChange):length(xPeak)][which.max(myData[xPeak[which.max(yChange):length(xPeak)]] - 
  #                                                                      myData[xOnset[which.max(yChange)]])]
  #   
  #   #same
  #   yChangePeak <- xPeak[which.max(yChange):length(xPeak)][which.max(abs(myData[xPeak[yChangeMaxIndex:length(xPeak)]] - 
  #                                                                          yChangeOnsetValue  ))]
  
  ### and a simpler version to index the peak row
  
  # make a short vector of the xPeak vector starting at the xOnset
  # PeakShort <- xPeak[yChangeMaxIndex:length(xPeak)]
  print(xOnset)
  print(xPeak)
  print(yChangeMaxIndex)
  xPeakShort <- xPeak[yChangeMaxIndex:length(xPeak)]
  
  # make a short vector of the peak values
  xPeakShortValues <- myData[xPeakShort] 
  
  # make a vector of the differences between xPeakShortValues and yChangeOnsetValue
  xPeakShortDifferences <- xPeakShortValues - yChangeOnsetValue
  
  # get the peak row
  yChangePeak <- xPeakShort[which.max(xPeakShortDifferences)]
  
  # fix condition where yChangeOnset == yChangePeak are
  if(yChangeOnset == yChangePeak) yChangePeak <- yChangePeak+1
  
  # get the peak value 
  yChangePeakValue <- myData[yChangePeak]
  
  ###
  
  # compute the change from response onset to response peak
  yChangeValue <- yChangePeakValue - yChangeOnsetValue
  # or
  # yChangeValue <- max(xPeakShortDifferences)
  
  
  #     # or this way
  #     yChangePeakValue <- xPeakShortDifferences[which.max(xPeakShortDifferences)] + 
  #       yChangeOnsetValue
  
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
  
  # return(outputVector)
  
  ###
  
  # add the AutoEDAExtract column to the data frame
  mySegmentLists$AutoEDAExtract <- rep("", times=nrow(segmentDF))
  
  # add the data to the data frame column
  mySegmentLists$AutoEDAExtract[yChangeOnset] <- "responseOnsetRow"
  mySegmentLists$AutoEDAExtract[yChangePeak] <- "responseEndRow"
  
  return(mySegmentLists)
  
} # end EDAExtract function
# EDAExtract(x=segmentDF, y=eventDF)

