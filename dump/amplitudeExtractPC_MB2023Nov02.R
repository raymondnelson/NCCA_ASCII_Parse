# amplitude extract function for EDA and Cardio data
# 10-31-2015
# Aug 1, 2020 simplified version
# Raymond Nelson
#
####



{
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R')
  
  # source the maxSlopeChangeFn
  # source("~/Dropbox/R/NCCA_ASCII_Parse/slopeChange.R")
  source(paste0(RPath, 'slopeChange.R'), echo=FALSE)
  
}



amplitudeExtractFnPC <- function(extractList=AutoExtractList, env.params=env.params)  {
  # function to extract the amplitude of EDA and cardio rise or increase in response to a stimulus
  # 7-25-2020
  # Raymond Nelson

  # simple feature extraction method for EDA and cardio
  # without the nuances around latency, ROW and slope onset
  
  # called by the EDAExtractFn() and CardioExtactFn() in the EDAExtract.R  and CardioExtract.R scripts 
  
  # extractList is a list of all input info needed to extract the response
  # extractList is created by the EDAExtractFn and CardioExtractFn
  # 1. begin is a scalar indicating the row number of the onset of the stimulus question
  # 2. end is a scalar indicating the row number of the end of the stimulus question
  # 3. answer is a scalar indicating the row number of the verbal answer
  # 4. segmentName is the name of the stimulus event
  # 5. segmentTitle is the full segment name including examName, seriesName, chartName and segmentName
  # 6. dataVector is is a vector of time series data for a single stimulus segment
  # 7. artifactVector is a time series vector of artifacts from the segment DF
  # 8. useArtifacts is a logical value to control the integration of artifact and feature extraction
  
  # env.params is a list of environment parameters used for feature extraction
  # env.params is created by the EDAExtractFn and CardioExtractFn
  # 1. dataRate is a scalar that indicates that data rate in samples per second
  # 2. Lat is the required latency after stimulus onset before which a responses is not evaluated
  # 3. ROWEnd is a scalar indicating the end of ROW in seconds after verbal answer
  # 4. nSmooth is the number of samples to smooth and ignore slope changes of small duration
  # 5. strictWindow <- FALSE # use TRUE to stop responses at the end of the measurement window
  # 6. strictROW <- FALSE # use TRUE to ignore all positive slope segments that begin after end of ROW
  # 7. prop <- .5 # is the cutoff proportion of descent from max peak value for the descentStop/descentRule
  # 8. descentStop <- uses the descentRule paramenter 0=off, 1=on, 2=only after ROWEndRow
  # 9. slopeChange <- 0 will disable, 1 will enable and 2 will use significant changes when there is no positive slope onset in ROW
  
  # procedure for each stimulus segment
  # a. each stimulus segment consists of the 15 second evaluation window along with 10 prestimulus seconds and 10 poststimulus seconds
  # 1. locate the row indices for the onset of all negative slope segments - these are the peak of reaction
  # 2. keep only those response peak indices from 2.5 sec to 15 sec
  # 2a. include the index at 15 seconds 
  # 2b. locate one additional peak after the end of the 15 second evaluation window
  #      this is the inclusive evaluation window (score to the end of reaction even if it is outside the evaluation window)
  # 2c.  discard the additional peak (outside the evaluation window) if the slope changes to negative between the 13.5 seconds and the additional peak.
  #      this prevents scoring a positive slope segment that begins very late in the evaluation window.
  #      (do not change the 15 second evaluation window)
  # 2d. use 14.5 seconds for cardio because cardio data is inherently more complex than EDA 
  # 3. locate the row indices for the onset of all positive slope segments
  # 4. keep only those response onsets from latency to ROWEnd
  #    latency is .5 sec, and
  #    ROWEnd is 5 sec after the verbal answer or stimulus offset if no answer
  # 5. always add the index at at 2.5 seconds as a response onset
  # 6. remove peak indices after ROWEnd after the data have descended
  #    below the lowest onset 
  #    get the data values for all peak and onset indices
  # 7. for each peak, compare the data value with all onset values that precede the peak and locate the onset index with the max y (vertical) distance
  # 8. select the onset and peak indices with the max distance
  # 9. response measurement is max y distance from onset to peak
  
  # output is a list of extracted information 
  
  #### begin ####
  
  # extractList <- AutoExtractList
  # extractList <- ManualExtractList
  
  # get the information from the input list
  
  {
    Begin <- as.numeric(extractList$begin)
    End <- as.numeric(extractList$end)
    Answer <- as.numeric(extractList$answer)
    
    segmentName <- extractList$segmentName
    segmentTitle <- extractList$segmentTitle
    sensorName <- extractList$sensorName
    
    useArtifacts <- extractList$useArtifacts
    
    tsData <- extractList$dataVector
    
    artifactVector <- extractList$artifactVector
  }
  
  # assign("AutoExtractList", extractList, envir = .GlobalEnv)
  
  # get the information from the env.params input List
  
  {
    cps <- env.params$dataRate
    Lat <- env.params$Lat
    useROW <- env.params$useROW
    ROWStart <- env.params$ROWStart
    ROWStop <- env.params$ROWStop
    ROWEnd <- env.params$ROWEnd
    nSmooth <- env.params$ignore
    strictROW <- env.params$strictROW
    strictWindow <- env.params$strictWindow
    # descentRule <- env.params$descentRule
    # descProp <- env.params$descProp
    slopeChangeRule <- env.params$slopeChangeRule
    inflection <- env.params$inflection
    nothingIsSomething <- env.params$nothingIsSomething
  }
  
  # if(segmentName == "C12" && sensorName == "CardioSystolic") {
  #   stop()
  # }
  
  #### set the starting row and ending row ####
  
  {
    startRow <- 1
    DFRows <- length(tsData)
    
    # additional latency for ascending reactions
    addLat <- 2 # maybe use the sChangeLat environment variable
    # will be added to the .5 sec latency
    # so that the value at  2.5seconds may be used as an onset
    # if there is no positive slope onset
    
    # measuredSeg and shortenEW are set in the init script
    if(sensorName == "CardioMA") {
      measuredSegA <- ifelse(!isTRUE(strictWindow), 
                             measuredSeg  - shortenEW,
                             measuredSeg) 
      # was -.5 sec until Oct 27 2020
    } else {
      # for EDA
      measuredSegA <- ifelse(!isTRUE(strictWindow), 
                             measuredSeg  - shortenEW,
                             measuredSeg) 
      # was - 1.5 sec until Oct 27, 2020
    }
  }
  
  ####   initialize the event indices   ####
  
  {
    # prestimRow is usually the first row of the time series vector
    prestimRow <- Begin - (startRow-1) - (cps*prestimSeg) 
    if(prestimRow<=0) prestimRow <- 1
    # onset of the stimulus in the time series vector
    onsetRow <- Begin - (startRow-1) 
    # end of the evaluation window (EW)
    endRow <- onsetRow + (cps*measuredSeg) - 1 
    if(endRow > DFRows) endRow <- DFRows
    # also make another endRowA 
    # for the shortened EW when using strictWindow==FALSE
    endRowA <- onsetRow + (cps*measuredSegA) - 1
    if(endRowA > DFRows)  endRowA  <- DFRows
    # end of the question stimulus
    offsetRow <- End - (startRow-1) 
    if(offsetRow >= (endRow-2)) offsetRow <- endRow - 2
    # repsonse latency period
    latRow <- onsetRow + cps*Lat - 1 + (0*cps) # maybe add 1.5 second for PCASS 
    if(latRow >= (endRow-4)) latRow <- endRow - 4
    # verbal answer
    answerRow <- Answer - (startRow-1) 
    if(answerRow==offsetRow) answerRow <- offsetRow + 1
    # set the ROWStartRow using the ROWStart variable
    # normally the latency index
    # can be "stimOn" "stimOff" "latency" or "verbalAnswer"
    ROWStartRow <- switch(ROWStart,
                          "latency"=latRow,
                          "stimOn"=onsetRow,
                          "stimOff"=offsetRow,
                          "verbalAnswer"=answerRow )
    # set the end of the response onset window
    # response onset window is typically 5 seconds after the verbal answer but can be set arbitrarily
    # ROWStop sets the reference point for the ROW End
    # can be "answer" "onset" "offset" "latency" or "EWEnd"
    # set the ROWStopRef to select the reference point for ROW End
    ROWStopRef <- switch(ROWStop,
                         "answer"=answerRow,
                         "onset"=onsetRow,
                         "offset"=offsetRow,
                         "latency"=latRow,
                         "EWEnd"=endRow )
    ROWEndRow <- ROWStopRef + (cps*ROWEnd) - 1
    if(ROWEndRow > endRow) ROWEndRow <- endRow - 2
    if(ROWEndRow > (DFRows-3)) ROWEndRow <- DFRows - 3
  }
  
  #### initialize some output objects ####
  
  {
    yChangeOnset <- NA
    yChangeOnsetValue <- NA
    yChangePeak <- NA
    yChangePeakValue <- NA
    yChangeValue <- NA
  }
  
  ####  make a vector of slope values ####
  
  {
    # make a vector of slope values
    diff1 <- diff(tsData)
    theSlope <- ifelse(diff1==0,
                       # ifelse is vectorized and requires no control loop
                       theSlope <- 0,
                       ifelse(diff1>0,
                              theSlope <- 1,
                              theSlope <- -1) )
    # pad the first value so the length is the same as the input vector
    theSlope <- c(0,theSlope)
    # theSlope inclues 0 1 and -1 values
    
  }
  
  ####   locate the response peak indices   ####
  
  {
    # response peaks are the onset of negative slope segments
    # or the end of positive slope segments
    
    xNegSlope <- ifelse(theSlope[2:length(theSlope)] == -1,
                        # ifelse is vectorized and needs no control loop
                        # locate the onset of neg slope
                        # by checking each slope value with the next
                        ifelse(( theSlope[2:length(theSlope)] + 
                                   theSlope[1:(length(theSlope)-1)] ) == -2,
                               xPeak <- 0, 
                               xPeak <- -1), 
                        xPeak <- 0)
    # fix length of the vector of xPeak vector to the input
    # xNegSlope <- c(0, xNegSlope)
	# Feb 10,2023 select the peak not the point after the peak
	xNegSlope <- c(xNegSlope, 0)
    
    # keep only the peak indices
    xPeak <- which(xNegSlope == -1)
    
    # there may be no peak indices if the data descend persistently
    if(length(xPeak) == 0) xPeak <- NA
    
    # keep only those xPeak indices after latency
    xPeak <- xPeak[which(xPeak >= (latRow + (addLat*cps)))]
  }
  
  ####  add the endRow as a peak ####
  
  {
    xPeak <- sort(unique(c(xPeak, endRow)))
    # this will also remove NA values in case there is no xPeakAdd
    
    # there will always be at least 1 xPeak at this point
  }
  
  #### strict Window option to keep the first latePeak after endRow ####
  
  {
    # for the strictWindow==FALSE option 
    
    if(length(which(xPeak > endRow)) > 0) {
      # only if there are any peaks after the EW
      xPeakAdd <- xPeak[min(which(xPeak > endRow), na.rm=TRUE)]
      # keep the xPeakAdd only if the slope remain positive
      # use the theSlope vector to remove the latePeak after neg slope
      #  use endRowA to exclude + slope segments late in the EW
      if(!all(theSlope[endRowA:(xPeakAdd-1)] == 1)) {
        # set the late peak to NA if the data are negative after the short EW
        xPeakAdd <- NA
      }
      # a later step may remove the late peak
      # depending on the strictWindow environment parameter
    } else {
      xPeakAdd <- NA
    }
    
    # keep only those xPeak indices that are 
    # before the end of the evaluation window
    # for now
    xPeak <- xPeak[xPeak <= endRow]
    #  xPeakAdd (late peak) is added next 
    
    if(!isTRUE(strictWindow)) {
      # add one peak after the endRow  
      # contingent upon the strictWindow environment parameter
      # set in the NCCAASCII_init.R script
      xPeak <- sort(c(xPeak, xPeakAdd))
    }
  }
  
  ####   locate the response onset indices   ####
  
  {
    # response onsets are the onset of positive slope segments
    
    xPosSlope <- ifelse(theSlope[2:length(theSlope)] == 1,
                            # check every value + preceding 
                            ifelse( ( theSlope[2:length(theSlope)] + 
                                        theSlope[1:(length(theSlope)-1)] ) < 2,
                                    1,
                                    0 ),
                            0 )
    
    
    # fix length of the vector of xOnset vector to the input
    xPosSlope <- c(0, xPosSlope)
    
    # keep only the onset indices
    xOnset <- which(xPosSlope == 1)
    
    # there may be no onset indices if the data descend persistently
    # or if the data ascend persistent from before question onset
    if(length(xOnset) == 0) xOnset <- NA
  }
   
  #### keep only xOnset indices after latency and during ROW ####
  
  {
    xOnset <- xOnset[which(xOnset >= latRow)]
    
    # keep only those xOnset indices that are 
    # before the end of the ROW
    xOnset <- xOnset[xOnset <= ROWEndRow]
    
    if(length(xOnset) == 0) xOnset <- NA
    
    # set xOnset to NA if no xPeak indices
    # Aug 14, 2020 seems to cause a problem with the cardio
    # if(all(!is.na(xPeak))) xOnset <- NA
  }
  
  #### extract a response onset via change in positive slope inflection ####
  
  # inflection=2
  
  # inflection is set in the NCCAASCII_init.R script
  
  if(inflection == 1) {
    
      if(!exists("addLat")) addLat <- 2
      
      # use the slopeChangeRule parameter
      # 1 for EDA and 0 for cardio
      # this is a blunt approximation
      # instead of evaluating the change in slope activity
      # works surprisingly well
      if(slopeChangeRule == 1) {
        # but only if there are any non-NA peak indices
        # possible this could work OK under all conditions
        if(any(!is.na(xPeak))) {
          xOnset <- c(round(latRow+(addLat*cps), 0), xOnset)
          # indices may be out of sequence at this point
        }
        # there will always be at least 1 onset at this point
        # set the order and remove NAs
        xOnset <- sort(xOnset)
      }
      
      # impute the response onset only if no XOnset indices
      if(slopeChangeRule == 2) {
        # xOnset will be NA if no changes from - or 0 slope to + slope
        if(all(is.na(xOnset))) {
          if(any(!is.na(xPeak))) {
            xOnset <- c(round(latRow+(addLat*cps), 0), xOnset)
            # does not matter that the indices may be out of sequence
            xOnset <- sort(xOnset)
            # NAs are removed by sort()
          }
        }
      }
      
      xOnset <- unique(xOnset)
      # NAs are retained if any are present
    
  }
  
  #### alternate method use the maxSlopeChangeFn ####
  
  if(inflection == 2) {
      
      # source("~/Dropbox/R/NCCA_ASCII_Parse/slopeChange.R", echo=TRUE)
      
      # maxSlopeChangeFn() uses a moving t-test of variance for ad
      # for adjacent 1s segments
      # with alpha=.001
      
      if(slopeChangeRule == 1) {
        if(any(!is.na(xPeak))) {
          theseIdcs <- maxSlopeChangeFn(x=tsData, idx=TRUE)
          # keep only those that are after the latency row
          theseIdcs <- theseIdcs[theseIdcs >= latRow]
          # Aug 4, 2022
          # keep only those that are after sChangeLat
          theseIdcs <- theseIdcs[theseIdcs >= (301 + (sChangeLat*cps) - 1)]
          # Aug 4, 2022
          # put the slope change at the end of the pre and post segs
          theseIdcs <- theseIdcs + (nPre*cps + nPost*cps)
          xOnset <- c(theseIdcs, xOnset)
        }
        # set the order and remove NAs
        xOnset <- sort(xOnset)
      }
      
      if(slopeChangeRule == 2) {
        if(all(is.na(xOnset))) {
          if(any(!is.na(xPeak))) {
            theseIdcs <- maxSlopeChangeFn(x=tsData, idx=TRUE)
            # keep only those that are after the latency row
            theseIdcs <- theseIdcs[theseIdcs >= latRow]
            xOnset <- c(theseIdcs, xOnset)
            # set the order and remove NAs
            xOnset <- sort(xOnset)
          }
        }
      }
      
      xOnset <- unique(xOnset)
      # NAs are retained if any are present
    
  }
  
  #### repeat ### Sep 27, 2021 ####
  
  {
    # remove xOnset indices prior to the latency index
    xOnset <- xOnset[which(xOnset >= latRow)]
    
    # remove xOnset indices after the end of the ROW
    xOnset <- xOnset[xOnset <= ROWEndRow]
  }
  
  #### exclude peaks after the data descend below the onset value ####
  
  {
    # initialize this to NA to avoid problems
    postROWXOnset <- NULL
    # use the xPosSlope vector from earlier
    
    # get the pos slope onset indices after ROWEndRow
    postROWXOnset <- which(xPosSlope[ROWEndRow:endRow] == 1) + ROWEndRow - 1
    
    if(length(postROWXOnset) > 0) {
      # get the data values for post ROW onset indices
      postROWXOnsetVals <- tsData[postROWXOnset]
      minPostROWVal <- min(tsData[postROWXOnset])
      
      # get min xOnset
      thisMinOnset <- xOnset[which.min(tsData[xOnset])]
      minOnsetVal <- tsData[thisMinOnset]
      
      # exlude peaks after thisMinOnset
      # if the min post ROW onset val < minOnsetVal
      if(any(postROWXOnsetVals <= minOnsetVal)) {
        # remove xPeak indices 
        # after the data have descended below the minOnsetVal
        # select the first if several
        thisOne <- postROWXOnset[which(postROWXOnsetVals < minOnsetVal)[1]]
        xPeak <- xPeak[which(xPeak < thisOne)]
      }
      
    }
    
    # Nov 19, 2022 fixed vectorized is.na()
    if(length(xOnset)==0 || any(is.na(xOnset))) {
      xOnset <- NA
      xPeak <- NA
    }
    
    # xPeak may be NA or empty at this point 
    # if the data are descending from 2.5 seconds
  }
  
  #### strict ROW option ####
  
  if(isTRUE(strictROW)) {
    # exclude xPeak indices after data begin to descend after ROWEndRow
    if(length(!is.na(xPeak)) > 0) {
      # sort the xPeak indices and remove NAs
      xPeak <- sort(xPeak)
      # use a loop to inspect the slope prior to each peak after ROWEndRow
      for(n in 1:length(xPeak)) {
        if(length(xPeak[n])==0) next()
        if(is.na(xPeak[n])) next()
        if(xPeak[n] > ROWEndRow) {
          # only for xPeak indices after ROWEndRow
          if(any(theSlope[ROWEndRow:(xPeak[n]-1)] == -1)) {
            xPeak[n] <- NA
          } # end if any - slope after ROWEndRow
        } # end if for xPeak after ROWEndRow
      } # end loop n for xPeak indices
      # sort again and remove NAs
      xPeak <- sort(xPeak)
    }
  }
  
  #### extract max distance for each xPeak to all preceding xOnset vals ####
  
  if( any(!is.na(xPeak)) && any(!is.na(xOnset)) ) {
    # for each xPeak value, calculate the max y distance
    # to all preceding xOnset value
    
    # check and fix if xOnset == xPeak 
    xOnset[which(xOnset %in% xPeak)] <- xOnset[which(xOnset %in% xPeak)] - 1
    
    xPeakVals <- tsData[xPeak]
    xOnsetVals <- tsData[xOnset]
    
    # initialize a vector for the max y distance for xOnset to xPeak
    yDistance <- rep(NA, length=length(xPeak))
    
    # some vectors to hold the xOnset indices and values
    onsetIdx <- rep(NA, length=length(xPeak))
    onsetVals <- rep(NA, length=length(xPeak))
    
    # iterate over the xPeaks to get the max distance to a preceding xOnset
    n=1
    for(n in 1:length(xPeakVals)) {
      # increment the loop if no xOnset prior to xPeak[n]
      if(length(which(xOnset < xPeak[n])) == 0) next()
      # check the xOnsetVals prior to xPeak[n]
      thisMax <- 
        which.max( xPeakVals[n] - xOnsetVals[which(xOnset < xPeak[n])] )
      thisOnsetVal <- xOnsetVals[thisMax] # simpler and works the same
      yDistance[n] <- xPeakVals[n] - thisOnsetVal
      # increment the loop if the distance is negative
      if(sign(yDistance[n]) == -1) {
        yDistance[n] <- 0
        # next() # commented out 8/31/2020 10:04pm
      }
      onsetIdx[n] <- xOnset[thisMax]
      onsetVals[n] <- thisOnsetVal
    } # end loop n over xPeakVals
    
    # at this point yDistance is a vector of max distance vals for each peak
    # onsetIdx and onsetVals are vectors
    # for the distance from each peak to each preceding onset 
    
    # get the output values using the max yDistance
    yChangeOnset <- onsetIdx[which.max(yDistance)]
    
    yChangeOnsetValue <- tsData[yChangeOnset]
    yChangePeak <- xPeak[which.max(yDistance)]
    
    yChangePeakValue <- tsData[yChangePeak]
    yChangeValue <- yChangePeakValue - yChangeOnsetValue
  }
  
  #### fix some possible problems when there is no usable response ####
  
  {
    # in case there is no yChangeValue
    # because the slope is persistently negative
    
    # only if there are any + difference values
    if(any(is.na(yChangeValue) || yChangeValue < 0 || length(yChangeValue) == 0)) {
      # check if the slope is negative from 2.5 sec to endRow
      if(tsData[endRow] - tsData[latRow+round(addLat*cps,0)] < 0) {
        yChangeOnset <- (endRow - 1)
        yChangeOnsetValue <- tsData[endRow]
        yChangePeak <- endRow
        yChangePeakValue <- tsData[endRow]
        yChangeValue <- 0
        # yChangeValue should be 0 for persistent descending data
      } else {
        # if the slope is + then change the yChangeValue to NA
        yChangeOnset <- (endRow - 1)
        yChangeOnsetValue <- tsData[endRow]
        yChangePeak <- endRow
        yChangePeakValue <- tsData[endRow]
        yChangeValue <- NA
        # yChange Value should be NA for persistent ascending data
      }
    } # end if there are no + difference values
    
    # double check against the potential for - y distance vals
    if(!is.na(yChangeValue) && yChangeValue <= 0) {
      yChangeOnset <- NA
      yChangeOnsetValue <- NA
      yChangePeak <- NA
      yChangePeakValue <- NA
      yChangeValue <- NA
    }

    # Sep 27, 2021 
    # use an environment paramenter to set NA response value to 0
    if(isTRUE(nothingIsSomething)) {
      yChangeOnset <- NA
      yChangeOnsetValue <- NA
      yChangePeak <- NA
      yChangePeakValue <- NA
      yChangeValue <- 0
      # yChangeValue will be NA if no response and nothingIsSomething=FALSE
    }
    
  }
  
  #### output ####
  
  # construct the output vector
  output <- list(yChangeOnset, 
                 yChangePeak, 
                 yChangeOnsetValue, 
                 yChangePeakValue, 
                 yChangeValue,
                 NULL, # not used with simplified amplitude extraction
                 NULL, # but the EDAExtract and CardioExtract 
                 NULL, # functions will look for these items
                 NULL, # so include them for now
                 NULL,
                 segmentTitle)
  names(output) <- c("responseOnsetRow",
                     "responsePeakRow",
                     "responseOnsetValue",
                     "responsePeakValue",
                     "responseChangeValue",
                     "recoveryRow",
                     "recoveryTime",
                     "complexityRows",
                     "complexityValue",
                     "stopRow",
                     "segmentTitle")
  
  return(output)
  
} # end amplitudeExtractFn() function 



