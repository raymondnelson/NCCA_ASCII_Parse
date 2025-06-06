# function to select the CQ for evaluation with each RQ
# 1-24-2017
# Raymond Nelson

#########################################

# Called by the RCScoreFn in the RCScore.R script



selectCQFn <- function(inputList=CQSelectInputList) {
  # R function to select a comparison questions
  # for evaluation with a relevant question
  # 1-24-2017
  # Raymond Nelson
  ######
  #
  # input is a list of four items: 1) RQName, 2) chartMeasurementDF,
  # 3) events - a vector of chart events from the first CQ or RQ to the last CQ or RQ
  # and 4) the sensorName
  # 
  # RQs may be alone or in pairs
  # use the CQ preceding and subsequent to the RQ or paired RQs
  # choose the CQ with the greater change in physiology
  # choose the preceding CQ when there is no CQ imediately subsequent to the RQ
  # there can be no artifacts, instructions or other questions
  # in between the CQ and RQ
  # if the preceding CQ is not usable then
  # use the CQ immediately subsequent to the RQ 
  # even if there is a single neutral question between the RQ and subsequent CQ
  # as long as there are no artifacts, instructions or other questions 
  # between the RQ and subsequent CQ
  # 
  # output is a list of 2 items: the CQValue and the CQName
  ########################
  
  RQName <- inputList[["RQ"]]
  chartMeasurementDF <- inputList[["DAT"]]
  chartMeasurementEvents <- inputList[["events"]]
  sensorName <- inputList[["sensor"]]
  
  # View(chartMeasurementDF)
  
  ###
  
  # get the index for the RQName
  RQIndex <- which(chartMeasurementEvents == RQName)
  
  # use a regular expression to get the indices and names for the CQs
  CQIndices <- grep("C", chartMeasurementEvents)
  CQNames <- chartMeasurementEvents[CQIndices]
  
  # intitialize some things
  precedingCQ <- NA
  subsequentCQ <- NA
  precedingCQName <- NA
  subsequentCQName <- NA
  precedingCQRows <- NA
  subsequentCQRows <- NA
  precedingCQValue <- NA
  subsequentCQValue <- NA
  
  checkPreceding <- NA
  checkSubsequent <- NA
  
  CQValues <- NA
  
  # 2-10-2019 not sure why these are quoted
  CQValue <- "NA"
  CQName <- "NA"

	# get the rows for the sensor
  sensorRows <- chartMeasurementDF$sensorName == sensorName
  
  ### check the position of the preceding CQ relative to the RQ
  
  if(length(which(CQIndices < RQIndex)) > 0) {
    precedingCQ <- CQIndices[max(which(CQIndices < RQIndex))]
    precedingCQDistance <- RQIndex - precedingCQ
    if(precedingCQDistance > 2) precedingCQ <- NA 
    # for paired RQs check the 2nd position preceding the RQ
    if(!is.na(precedingCQ) & precedingCQDistance > 1) {
      checkPreceding <- RQIndex - 1
      if(!is.na(checkPreceding)) {
        # make sure the interposed question is an RQ
        # use grepl to get the boolean result
        # and set the precedingCQ to NA if the interposed question is not an RQ
        if(!grepl("R", chartMeasurementEvents[checkPreceding], ignore.case=TRUE)) {
          precedingCQ <- NA
        }
      }
    }
  }
  
  ### we now have the index of the preceding CQ
  
  if(!is.na(precedingCQ)) {
    # get the name of the preceding CQ
    precedingCQName <- chartMeasurementEvents[precedingCQ]
    # get the value of the preceding CQ
    precedingCQRows <- chartMeasurementDF$eventLabel == precedingCQName
    precedingCQValue <- chartMeasurementDF$sensorMeasurement[which(precedingCQRows & sensorRows)]
  } 
  
  ### check the position of the subsequent CQ relative to the RQ
  
  if(length(which(CQIndices > RQIndex)) > 0) {
    subsequentCQ <- CQIndices[min(which(CQIndices > RQIndex))]
    subsequentCQDistance <- subsequentCQ - RQIndex
    # if the preceding CQ is more than 2 places away
    if(subsequentCQDistance > 2) subsequentCQ <- NA
    if(!is.na(subsequentCQ) & subsequentCQDistance > 1) {
      # check the 2nd position subsequent the the RQ, for paired RQs
      checkSubsequent <- RQIndex + 1
      if(!is.na(checkSubsequent)) {
        # make sure the interposed question is an RQ
        # use grepl to get the boolean result
        # and set the subsequentCQ to NA if the interposed question is not an RQ
        if(!grepl("R", chartMeasurementEvents[checkSubsequent], ignore.case=TRUE)) {
          subsequentCQ <- NA
        }
      }
    }
  }
  
  # we now have the index of the subsequent CQ
  
  if(!is.na(subsequentCQ)) {
    # get the name of the subsequent CQ
    subsequentCQName <- chartMeasurementEvents[subsequentCQ]
    # get the value of the subsequent CQ
    subsequentCQRows <- chartMeasurementDF$eventLabel == subsequentCQName
    subsequentCQValue <- chartMeasurementDF$sensorMeasurement[which(subsequentCQRows & sensorRows)]
  }
  
  ###### check again for a subsequent RQ after a single neutral quesiton 
  ###### if the precedingCQValue is NA and the subsequentCQValue is NA
  
  # if there exists a preceding CQ but the precedingCQValue is NA
  # and there is yet no subsequent CQ Value
  if(all( !is.na(precedingCQ), is.na(precedingCQValue), is.na(subsequentCQValue) )) {
    
    # this is the same code as above 
    # except for it now searches for an interposed neutral/irrelevant question
    if(length(which(CQIndices > RQIndex)) > 0) {
      subsequentCQ <- CQIndices[min(which(CQIndices > RQIndex))]
      subsequentCQDistance <- subsequentCQ - RQIndex
      # if the subsequent CQ is more than 2 places away
      if(subsequentCQDistance > 2) subsequentCQ <- NA
      if(!is.na(subsequentCQ) & subsequentCQDistance > 1) {
        checkSubsequent <- RQIndex + 1
        if(!is.na(checkSubsequent)) {
          # make sure the interposed question is a neutral or irrelevant question
          # use grepl to get the boolean result
          # and set the subsequentCQ to NA if the interposed question is not an RQ
          if(!grepl("[NI]", chartMeasurementEvents[checkSubsequent], ignore.case=TRUE)) {
            subsequentCQ <- NA
          }
        }
      }
    }
    
    # get the name of the subsequent CQ
    if(!is.na(subsequentCQ)) {
      subsequentCQName <- chartMeasurementEvents[subsequentCQ]
      # get the value of the subsequent CQ
      subsequentCQRows <- chartMeasurementDF$eventLabel == subsequentCQName
      subsequentCQValue <- chartMeasurementDF$sensorMeasurement[which(subsequentCQRows & sensorRows)]
    }
    
  } # end if to check for a subsequent CQ if the preceding is not usable
  
  #### combine the preceding and susequent CQ values to a single vector

  if( !is.na(precedingCQValue) | !is.na(subsequentCQValue) ) {
    CQValues <- c(precedingCQValue, subsequentCQValue)
    names(CQValues) <- c(precedingCQName, subsequentCQName)
    # which.max ignores NA values
    CQValue <- CQValues[which.max(CQValues)]
    CQName <- names(CQValues[which.max(CQValues)])
    CQValue <- paste(CQValue, CQName)
  } 
  
  return(list(CQValue=CQValue, CQName=CQName))
  
} # end selectCQFn() function

# selectCQFn(inputList=CQSelectInputList)



