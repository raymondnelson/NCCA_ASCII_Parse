# function to select the CQ for evaluation with each RQ
# 1-24-2017
# 2-15-2019
# Raymond Nelson

#########################################

# Called by the RCScoreFn in the RCScore.R script

# sourced by the workFlow.R script 
# free variables are scoped in the global environment
                         
selectCQFn <- function(CQSelectInputList=CQSelectInputList) {
  # R # function to select a comparison questions
  # for evaluation with a relevant question
  # 1-24-2017
  # 2-15-2019
  # Raymond Nelson
  #
  ####
  #
  # input is a list of three items: 1) RQName, 2) chartMeasurementDF,
  # 3) the sensorName
  # 
  # use the CQ preceding and subsequent to the RQ
  # choose the CQ with the greater change in physiology
  # choose the preceding CQ when there is no CQ subsequent to the RQ
  # for pneumo measurements the greater change is the smaller numerical value
  # RQs may be single or in pairs
  # CQs may be selected preceding or subsequent to paired RQs
  # no artifacts, instructions or other questions in between the CQs and RQs
  # if the preceding CQ is NA then
  # use the CQ immediately subsequent to the RQ 
  # a single neutral or irrelevant question is permitted
  # between the RQ and subsequent CQ when the preceding CQ is NA
  # with no other questions, artifacts, or between the RQ and subsequent CQ
  # 
  # may not work with the Army MGQT 
  #
  # output is a list of 2 items: the CQValue and the CQName
  #
  ####
  
  {
    thisRQName <- CQSelectInputList[["thisRQName"]]
    RQValue <- CQSelectInputList[["RQValue"]]
    chartMeasurementDF <- CQSelectInputList[["chartMeasurementDF"]]
    thisSensorName <- CQSelectInputList[["thisSensorName"]]
    
    # View(chartMeasurementDF)
    
    # initialize a vector of unique events in the chart
    uniqueQuestionsChart <- unique(chartMeasurementDF$eventLabel)
  }
  
  #### intitialize some things
  
  {
    precedingCQ <- NA
    precedingCQName <- NA
    precedingCQRows <- NA
    precedingCQValue <- NA
    
    subsequentCQ <- NA
    subsequentCQName <- NA
    subsequentCQRows <- NA
    subsequentCQValue <- NA
    
    checkPreceding <- NA
    checkSubsequent <- NA
    
    # a vector to hold both the preceding and subsequent
    CQValues <- NA
    
    # output variables
    CQValue <- NA
    CQName <- NA
  }
  
  #### get the row indices for this RQ and all CQs ####

  {
    # get the rows for this sensor
    sensorRows <- chartMeasurementDF$sensorName == thisSensorName
    
    # get the index for this RQName
    RQIndex <- which(uniqueQuestionsChart == thisRQName)
    
    # use a regular expression to get the indices and names for the CQs
    CQIndices <- grep("C", uniqueQuestionsChart)
    CQIndices <- CQIndices[uniqueQuestionsChart[CQIndices] != "CT"]
    CQIndices <- CQIndices[uniqueQuestionsChart[CQIndices] != "C"]
  }
  
  #### get the position and value of CQ preceding the RQ ####
  
  {
    
    # get the index of the preceding CQ
    if(length(which(CQIndices < RQIndex)) > 0) {
      # the get max index for all CQs that precede the RQ
      precedingCQ <- CQIndices[max(which(CQIndices < RQIndex))]
      # get the distance from the CQ to RQ
      precedingCQDistance <- abs(RQIndex - precedingCQ)
      # don't use CQ with distance > 2 positions
      if(precedingCQDistance > 2) precedingCQ <- NA 
      # for paired RQs check the position immediately preceding the RQ
      if(!is.na(precedingCQ) & precedingCQDistance > 1) {
        checkPreceding <- RQIndex - 1
        if(!is.na(checkPreceding)) {
          # make sure the interposed question is an RQ
          # and set the precedingCQ to NA if the interposed question is not an RQ
          # use grepl to get the boolean result
          if(!grepl("R", uniqueQuestionsChart[checkPreceding], ignore.case=TRUE)) {
            precedingCQ <- NA
          }
        }
      }
    }
    
    # we now have the index of the preceding CQ
    # precedingCQ will be NA if none
    
    # get the value of the preceding CQ
    if(!is.na(precedingCQ)) {
      # get the name of the preceding CQ
      precedingCQName <- uniqueQuestionsChart[precedingCQ]
      # get the value of the preceding CQ
      precedingCQRows <- chartMeasurementDF$eventLabel == precedingCQName
      theseRows <- which(precedingCQRows & sensorRows)
      precedingCQValue <- chartMeasurementDF$sensorMeasurement[theseRows]
      # Feb 24, 2022 to prevent strange CQ selection for PLE values
      # also see the change in RCScores on this date
      if(thisSensorName == "PLE" && precedingCQValue <= .01) {
        precedingCQValue <- .01
      }
    } 
    
    # View(chartMeasurementDF)
    # precedingCQName
    # precedingCQValue
    
    # precedoingCQ may be 0 if no response
    if(!is.na(precedingCQValue) && precedingCQValue == 0) {
      precedingCQValue <- NA
    }
    
    # we now have the name and value of the preceding CQ
    
  }
  
  #### get the position and value of the subsequent CQ ####
  
  {
    
    # get the subsequent CQ value
    if(length(which(CQIndices > RQIndex)) > 0) {
      # get the min index of all CQs subsequent to the RQ
      subsequentCQ <- CQIndices[min(which(CQIndices > RQIndex))]
      # get the distance from CQ to RQ
      subsequentCQDistance <- abs(subsequentCQ - RQIndex)
      # don't use it if the preceding CQ is more than 2 places away
      if(subsequentCQDistance > 2) subsequentCQ <- NA
      # for paired RQs check the position immediately subsequent to the RQ
      if(!is.na(subsequentCQ) & subsequentCQDistance > 1) {
        checkSubsequent <- RQIndex + 1
        if(!is.na(checkSubsequent)) {
          # make sure the interposed question is an RQ
          # use grepl to get the boolean result
          # set the subsequentCQ to NA if the interposed question is not an RQ
          if(!grepl("R", uniqueQuestionsChart[checkSubsequent], ignore.case=TRUE)) {
            subsequentCQ <- NA
          }
        }
      }
    }
    
    # we now have the index of the subsequent CQ
    # subsequentCQ will be NA if none
    
    # get the subsequent CQ value
    if(!is.na(subsequentCQ)) {
      # get the name of the subsequent CQ
      subsequentCQName <- uniqueQuestionsChart[subsequentCQ]
      # get the value of the subsequent CQ
      subsequentCQRows <- chartMeasurementDF$eventLabel == subsequentCQName
      theseRows <- which(subsequentCQRows & sensorRows)
      subsequentCQValue <- chartMeasurementDF$sensorMeasurement[theseRows]
      # Feb 24, 2022 to prevent unexpected/strange PLE R/C scores
      # also see the change in RCScores on this date
      if(thisSensorName == "PLE" && subsequentCQValue <= .01) {
        subsequentCQValue <- .01
      }
    }
    
    # subsequentCQName
    # subsequentCQValue
    
    # fix value == 0
    if(!is.na(subsequentCQValue) && subsequentCQValue == 0) {
      subsequentCQValue <- NA
    }
    
    # we now have the name and value of the subsequent CQ
    
  }
  
  #### check again for a subsequent CQ after a single neutral question ####

  if( is.na(precedingCQValue) && is.na(subsequentCQValue) ) {
    
    # if there exists a preceding CQ but the precedingCQValue is NA
    # and there is yet no subsequent CQ Value
    
    # this is the same code as above 
    # but it now searches for an interposed neutral or irrelevant question
    if(length(which(CQIndices > RQIndex)) > 0) {
      # get the min index of all CQs subsequent to the RQ
      subsequentCQ <- CQIndices[min(which(CQIndices > RQIndex))]
      # get the distance from CQ to RQ
      subsequentCQDistance <- abs(subsequentCQ - RQIndex)
      # ignore if the subsequent CQ is more than 2 places away
      if(subsequentCQDistance > 2) subsequentCQ <- NA
      # check the position immediately subsequent to the RQ
      if(!is.na(subsequentCQ) & subsequentCQDistance > 1) {
        checkSubsequent <- RQIndex + 1
        if(!is.na(checkSubsequent)) {
          # make sure the interposed question is a neutral or irrelevant question
          # use grepl to get the boolean result
          # set the subsequentCQ to NA 
          # if the interposed question is not neutral or irrelevant
          if(!grepl("[NI]", uniqueQuestionsChart[checkSubsequent], ignore.case=TRUE)) {
            # Aug 12, 2023
            # CPA/RCMP A-Series ZCT has a neutral at 8 that we should not reach over
            # Dec 5, 2023 added E8 and 8E to prevent reaching over the SY in the FZCT
            if(uniqueQuestionsChart[checkSubsequent] %in% c("8N", "N8", "E8", "8E")) {
              subsequentCQ <- NA
            }
          }
        }
      }
    }
    
    # we now have the index of the subsequent CQ
    # subsequentCQ will still be NA if none
    
    # get the name of the subsequent CQ
    if(!is.na(subsequentCQ)) {
      subsequentCQName <- uniqueQuestionsChart[subsequentCQ]
      # get the value of the subsequent CQ
      subsequentCQRows <- chartMeasurementDF$eventLabel == subsequentCQName
      theseRows <- which(subsequentCQRows & sensorRows)
      subsequentCQValue <- chartMeasurementDF$sensorMeasurement[theseRows]
    }
    
    # subsequentCQName
    # subsequentCQValue
    
    # fix value == 0
    if(!is.na(subsequentCQValue) && subsequentCQValue == 0) {
      subsequentCQValue <- NA
    }
    
  } # end if to check for a subsequent CQ if the preceding CQ is NA
  
  ############ output section ###########
  
  #### combine the preceding and subsequent CQ values to a single vector ####
  
  if( !is.na(precedingCQValue) | !is.na(subsequentCQValue) ) {
    CQValues <- c(precedingCQValue, subsequentCQValue)
    names(CQValues) <- c(precedingCQName, subsequentCQName)
    
    if( any(thisSensorName=="UPneumo", 
            thisSensorName=="LPneumo", 
            thisSensorName=="Pneumo" ) ) {
      # which.min function will ignore NA values
      # CQValue <- CQValues[which.min(CQValues)]
      # CQName <- names(CQValues[which.min(CQValues)])
      # changed to which.max Sep 24, 2021
      # because of improved pneumo feature extraction
      # changed back to which.min Oct 7, 2021
      CQValue <- CQValues[which.min(CQValues)]
      CQName <- names(CQValues[which.min(CQValues)])
      names(CQName) <- thisSensorName
      
    } else {
      # for EDA Cardio and PLE
      # which.max function will ignore NA values
      CQValue <- CQValues[which.max(CQValues)]
      CQName <- names(CQValues[which.max(CQValues)])
      names(CQName) <- thisSensorName
    }
    
    outputList <- list(CQValue=CQValue, 
                       CQName=CQName)
    
  } else {
    
    # if both the preceding CQ and subsequent CQ value are NA
    
    # use the NA variables initialized earlier in this function
    outputList <- list(CQValue=CQValue, 
                       CQName=precedingCQName)
    
  }
  
  return(outputList)
  
  # end selectCQFn() function
} 

# selectCQFn(CQSelectInputList=CQSelectInputList)



