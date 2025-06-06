# amplitude extract function for EDA and Cardio data
# 10-31-2015
# Raymond Nelson
#
# a. onsetRow is the stimulus onset
# b. endRow is the end of the evaluation window (normally 15 seconds, but can be set arbitrarily)
# c. evaluation window (EW) is from stimulus onsetRow to endRow
# d. latencyRow is .5 seconds after stimulus onset (can be set arbitrarily)
# e. ROWEndRow is 5 seconds after verbal answer (can be set arbitrarily)
# f. response onset window (ROW) is the segment before ROWEndRow and after the onset latencyRow
# g. evaluation window (scoring window) is from stimulus onset to 15 seconds after stimulus onset
# h. input time series for each stimulus segment includes 5 prestimulus segments 
#    and 5 additional seconds after the 15 second evaluation window
#
# 1. locate the row index and value for the onset of all positive slope segments that start in the ROW
# 1a. also infer the a response onset from a significant change/increase in + slope energy (variance)
# 1b. option: only infer repsonse onset from slope change where there is no + slope onset in the ROW
# 2. locate the row index and value for all positive slope peaks after the first onset inside the ROW
# 3. keep all peaks in the EW plus one additional peak if the slope is + at the end of the EW
# 3a. option: strictly exclude + slope segments for which the + slope onset is after the ROWEndRow
# 3b. option: strictly exclude all peaks after the endRow even if the slope is + at the end of the EW
#     in this case keep the value at endRow as peak value if the slope is still + at endRow
# 4. exclude + slope segments that begin after the data have descended to a new lower + slope onset value
# 4a. pkeep all + slope onsets in the ROW and only exclude onset points after ROWEndRow when data have descended below the onset value
# 7. option: excluded changes that occur after the data have descended 50% (or any arbitrary proportion) from the previous max peak
# 7a. ### not sure whether the descent rule presently works during or after ROWEndRow 4/30/2016
# 8. select the onset and peak for the max change value for each + slope onset and all subsequent peaks that are not excluded
# 
##################################################################################

amplitudeExtractFn <- function(extractList=extractList, env.params=env.params)  {
  # function to extract the amplitude of EDA increase in response to a stimulus
  # input extractList is a list of 6 items including all of the information needed to extract the response
  # 1. begin is a scalar indicating the row number of the onset of the stimulus question
  # 2. end is a scalar indicating the row number of the end of the stimulus question
  # 3. answer is a scalar indicating the row number of the verbal answer
  # 4. segmentName is the name of the stimulus event
  # 5. segmentTitle is the full segment name including examName, seriesName, chartName and segmentName
  # 6. dataVector is is a vector of time series data for a single stimulus segment

  # dataRate is a scalar that indicates that data rate in samples per second
  # Lat is the required latency after stimulus onset before which a responses is not evaluated
  # ROWEnd is a scalar indicating the end of ROW in seconds after verbal answer
  # nSmooth is the number of samples to smooth and ignore slope changes of small duration
  # strictWindow <- FALSE # use TRUE to stop responses at the end of the measurement window
  # strictROW <- FALSE # use TRUE to ignore all positive slope segments that begin after end of ROW
  # descentStop <- TRUE # will not evaluate upward segments that begin after the data descend a specified proportion
  # slopeChange <- 0 will disable, 1 will enable and 2 will use significant changes when there is no positive slope onset in ROW

  ########
    
  # get the information from the input list
  Begin <- as.numeric(extractList$begin)
  End <- as.numeric(extractList$end)
  Answer <- as.numeric(extractList$answer)
  segmentName <- extractList$segmentName
  segmentTitle <- extractList$segmentTitle
  tsData <- extractList$dataVector

  dataRate <- env.params$dataRate
  Lat <- env.params$Lat
  ROWEnd <- env.params$ROWEnd
  nSmooth <- env.params$nSmooth
  strictWindow <- env.params$strictWindow
  strictROW <- env.params$strictROW
  prop <- env.params$prop
  descentStop <- env.params$descentStop
  slopeChange <- env.params$slopeChange
  
  # also uses the measuredSeg global variable that was set in the NCCAASCII_init.R script
    
  ###

  DFRows <- length(tsData)

  # set the starting row 
  startRow <- 1
  
  ### can simplify this
  # first row of the time series vector
  prestimRow <- Begin - (startRow-1) - (dataRate*prestimSeg) 
  # correction for prestimRow values < 1
  if(prestimRow<=0) prestimRow <- 1
  # onset of the stimulus in the time series vector
  onsetRow <- Begin - (startRow-1) 
  # end of the measurement window is set in the NCCAASCII_init.R script
  endRow <- onsetRow + (dataRate*measuredSeg) - 1 
  # correction if endRow > DFRows
  if(endRow > DFRows) endRow <- DFRows
  # end of the question stimulus
  offsetRow <- End - (startRow-1) 
  # repsonse latency period
  latRow <- onsetRow + dataRate*Lat 
  # to avoid problems make sure that latRow and offsetRow do not exceed endRow
  ### fix this
  # if(latRow >= (endRow-4)) latRow <- DFRows - 4
  # if(offsetRow >= (endRow-2)) offsetRow <- DFRows - 2
  if(latRow >= (endRow-4)) latRow <- endRow - 4
  if(offsetRow >= (endRow-2)) offsetRow <- endRow - 2
  # verbal answer
  answerRow <- Answer - (startRow-1) 
  # correction if there is no answer (answer row will be the same as offsetRow)
  if(answerRow==offsetRow) answerRow <- offsetRow + 1
  # response onset window is typically 5 seconds after the verbal answer but can be set arbitrarily
  ROWEndRow <- answerRow + (dataRate*ROWEnd) 
  # correction if ROWEndRow > DFRows
  if(ROWEndRow > (DFRows-3)) ROWEndRow <- DFRows - 3
  
  ###############################################
  
  
  
  ############# OSS2 EDA feature extraction
  
  # initialize some variables
  baselineVal <- tsData[onsetRow]
  onset <- onsetRow
  onsetVal <- tsData[onset]
  peak <- onsetRow
  peakVal <- tsData[peak]
  
  dat <- tsData[onset:endRow]
  segLen <- length(tsData[onset:endRow])
  
  # locate the response peak 
  for(i in 2:segLen) {
    if(dat[i] > peakVal) { 
      peak <- i
      peakVal <- dat[i]
    }
  }
  
  # locate the response onset
  for(i in 2:peak) {
    if(dat[i] < onsetVal) {
      onset <- i
      onsetVal <- dat[i]
    }
  }
  
  ############## end OSS-2 feature extraction 
  
  # set the output values
  yChangeOnset <- onset + 150
  yChangeOnsetValue <- onsetVal
  yChangePeak <- peak + 150
  yChangePeakValue <- peakVal
  yChangeValue <- yChangePeakValue - yChangeOnsetValue
  
  stopRow2 <- endRow
  
  ##################### We now have the maximum change from xOnset to xPeak
  
    # print(yChangeOnset)
    # print(yChangeOnsetValue)
    # print(yChangePeak)
    # print(yChangePeakValue)
    # print(yChangeValue)

  ######################### fix a potential problem
  
  # fix condition where yChangeOnset == yChangePeak
  # does not recompute the yChangeValue which will remain 0
  if(yChangeOnset == yChangePeak) { 
    yChangeOnset <- yChangeOnset - 1
    yChangeOnsetValue <- tsData[yChangeOnset]
  }
  
  #################### output 
  
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
  
  return(outputVector)  
  
} # end amplitudeExtractFn() function 

#####

# amplitudeExtract(extractList=extractList, strictWindow=FALSE, strictROW=FALSE, descentStop=TRUE, ignore=2)
