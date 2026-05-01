# R function to execute the "descent rule" which prevents selection of a response peak,
# after the data have descended more than a proportion (1/2 way),
# from a preceeding response peak to the min value in the ROW
# April 26, 2026 
# supercedes the descentRule.R script and the descentRuleFn()
# Raymond Nelson
####




descentRuleFn <- function(xOnsetLoop=xOnsetLoop, 
                          xPeakLoop=xPeakLoop, 
                          tsData=tsData, 
                          ROWEndRow=ROWEndRow,
                          useDescentRule=useDescentRule,
                          sensorName=sensorName,
                          segmentName=segmentName,
                          segmentTitle=segmentTitle ) {
  # R function to execute the "descent rule" which prevents selection of a response peak,
  # after the data have descended more than a proportion (1/2 way),
  # from a preceeding response peak to the min value in the ROW
  # April 26, 2026 
  # supercedes the descentRule.R script and the descentRuleFn()
  # Raymond Nelson
  ####
  # this function is called iteratively in the (n) loop that selects the max change,
  # from each xPeak value to a preceeding xOnset peak values
  # called by the getMaxOnsetPeakDistanctFn() function in the getMaxOnsetPeakDistanct.R script,
  ####
  # input
  # xOnsetLoop is a vector of xOnset indices prior to the current xPeak in the loop where this function is called
  # xPeakLoop is a is a vector of xPeak indices up to the current xPeak in the loop where this function is called 
  # tsData is a vector of time series values for a single presentation of a single event,
  # including the presetim segment and poststim segment
  # descProp is a proportion for which the data are evaluated 
  # descProp = .632 is initialized in the NCCAASCII_init.R script
  # ROWEndRow is the end of the response onset window (typically 5 seconds after the verbal answer)
  ####
  # output is a stop row with the row index for the time series input, 
  # after which the data have descended more than a proportion descProp,
  # from the previous xPeak - min onset value in the ROW
  #### 
  
  # to prevent stopping on warning for unequal vector lengths
  options(warn=1)
  
  # for inspection
  # if(all(sensorName == "AutoEDA", segmentName == "2" )) {
  # if(all(sensorName == "CardioDiastolic", segmentName == "2" )) {
  #   assign("xOnsetLoop", xOnsetLoop, envir=.GlobalEnv)
  #   assign("xPeakLoop", xPeakLoop, envir=.GlobalEnv)
  #   assign("n", n, envir=.GlobalEnv)
  #   assign("tsData", tsData, envir=.GlobalEnv)
  #   assign("ROWEndRow", ROWEndRow, envir=.GlobalEnv)
  #   # assign("descentRule", descentRule, envir=.GlobalEnv)
  #   # assign("descProp", descProp, envir=.GlobalEnv)
  #   assign("sensorName", sensorName, envir=.GlobalEnv)
  #   assign("segmentName", segmentName, envir=.GlobalEnv)
  #   assign("segmentTitle", segmentTitle, envir=.GlobalEnv)
  #   print(tsData)
  #   stop("stop for inspection - descentRule.R")
  # }
  
  #### return NA if there are no xOnset indices before the first xPeak ####
  
  if(length(which(xOnsetLoop < xPeakLoop[1])) == 0) {
    return(list(thisOnsetIdx=NA, 
                thisOnsetVal=NA, 
                thisYDistance=NA, 
                thisXPeak=NA, 
                thisXPeakVal=NA, 
                segmentName=segmentName, 
                segmentTitle=segmentTitle) )
  }
  
  #### compute the y-axis values for xOnset indices in the ROW ###
  
  {
    xOnsetROW <- xOnsetLoop[which(xOnsetLoop < ROWEndRow)]
    xOnsetROWVals <- tsData[xOnsetROW]
    # this is necessary because xOnset in the ROW may be used as a response onset,
    # while xOnset after ROWEndRow is used only to support the selection of the max xPeak
  }
  
  #### return NA if there is no xOnset in the ROW ####
  
  if( length(xOnsetROW) == 0 || all(is.na(xOnsetROW)) ) {
    return(list(thisOnsetIdx=NA, 
                thisOnsetVal=NA, 
                thisYDistance=NA, 
                thisXPeak=NA, 
                thisXPeakVal=NA, 
                segmentName=segmentName, 
                segmentTitle=segmentTitle) )
  }
  
  #### set up ####
  
  {
    
    # get the time series data vals for xOnset and xPeak indices
    # xOnsetLoopVals <- tsData[xOnsetLoop]
    # xOnsetLoopVals <- tsData[xOnsetROW] # use xOnsetROW instead of xOnsetLoop
    xPeakLoopVals <- tsData[xPeakLoop]
    # xPeakLoop includes all xPeaks up to xPeak[n] in the parent function,
    # where this function is called
    
    # do this now so it can be output under all conditions
    lastXPeak <- xPeakLoop[length(xPeakLoop)]
    lastXPeakVal <- tsData[lastXPeak]
    
    # compute the highest xPeak preceding the lastXPeak
    prevXPeak <- xPeakLoop[which.max(xPeakLoopVals[-length(xPeakLoopVals)])]
    prevXPeakVal <- tsData[prevXPeak]
    
    # initialize some vectors to hold the intermediate result
    xOnsetVc <- rep(NA, times=length(xPeakLoop))
    xPeakVc <- rep(NA, times=length(xPeakLoop))
    
    # the output xPeak is always lastXPeak, which is the same as xPeak[n],
    # from the maxOnsetPeakFn() function/envir  where this function was called,
    
  }
  
  #### when there is only 1 xPeak and only 1 xOnset ####
  
  if( length(xPeakLoop) == 1 && length(xOnsetLoop) == 1 ) {
    
    # when there is only 1 xPeak there is only 1 xOnset,
    # and the descentRule does nothing
    # the output and result will be the same as descentRule==0
    
    thisOnsetIdx <- xOnsetROW[which.min(xOnsetROWVals)]
    thisOnsetVal <- tsData[thisOnsetIdx]
    thisYDistance <- tsData[xPeakLoop[length(xPeakLoop)]] - thisOnsetVal
    
    return(list(thisOnsetIdx=thisOnsetIdx, 
                thisOnsetVal=thisOnsetVal, 
                thisYDistance=thisYDistance, 
                thisXPeak=lastXPeak,
                thisXPeakVal=lastXPeakVal, 
                segmentName=segmentName, 
                segmentTitle=segmentTitle ))
    
  } 
  
  #### when there is more than 1 xPeak and 1 or more xOnset ####
  
  if( length(xPeakLoop) > 1 && length(xOnsetROW) >= 1 ) {
    
    # find the min xOnset value in the ROW and prior to lastPeak
    
    # lastPeak is the last sample index in the xPeakLoop input vector
    
    # xOnsetVc is a vector of selected onsets for each xPeak
    
    # xPeakVc is a vector of response peaks for each item in xPeak,
    # where a peak may be ommited and replace with NA,
    # if there is no usable onset, where the data do not descend too much,
    # and where too much descent means the data goes below the origin (onset),
    # or descends more than half-way from a preceding peak to the origin
    
    #### iterate over the xOnsetLoop input vector ####
    
    o=1
    for(o in 1:length(xOnsetROW)) {
      
      # print(paste("o:", o, "- newDescentRule.R"))
      
      # reset this for each o iteration
      saveIt <- TRUE
      
      ## get the data for this iteration of the o loop ##
      
      {
        thisXOnsetROW <- xOnsetROW[o]
        thisXOnsetLoopVal <- tsData[thisXOnsetROW]
        
        # initialize these early to avoid problems,
        # when there is no response onset for the first xPeak,
        # or when the first iteration of o is aborted with next(),
        # because the data have descended too much
        if(o==1) {
          # these values will be recomputed later
          lowestXOnsetVal <- thisXOnsetLoopVal
          highestCutVal <- lowestXOnsetVal
        }
        
        # slice the response vector from the time series data
        thisResponseVc <- c(thisXOnsetROW:lastXPeak)
        thisResponseVals <- tsData[thisResponseVc]
        # plot.ts(tsData)
        # plot.ts(thisResponseVals)
        
      }
      
      #### get the negative slope sample indices for the tsData input vector ####
      
      {
        # call some functions from amplitudeExtractHelperFuncdtions.R
        
        negSlopeIndcs <- which(negativeSlope(smoothSlope(slopeDir(x=tsData))) == -1)
        # remove neg slope indices before Onset[o]
        # added a small buffer to avoid problems from noise at the onset
        negSlopeIndcs <- negSlopeIndcs[which(negSlopeIndcs >= (thisXOnsetROW+3))]
        # remove negative slope indices after the last xPeak
        negSlopeIndcs <- negSlopeIndcs[which(negSlopeIndcs < xPeakLoop[length(xPeakLoop)])]
        
        # slice the negative slope response indices inside the current response
        theseNegSlopeIndcs <- negSlopeIndcs[which(negSlopeIndcs %in% thisResponseVc)]
        theseNegSlopeVals <- tsData[theseNegSlopeIndcs]
      }
      
      #### check if the data descended below the origin ####
      
      if(isTRUE(descentToOriginStop)) {
        # descentToOriginStop is a Boolean that is initialized in the NCCAASCII_init.R script
        if( length(theseNegSlopeVals) > 0 ) {
          # move this to avoid problems when there is noise at the response onset
          if( any( theseNegSlopeVals < thisXOnsetLoopVal ) ) {
            # print("data descended below the origin or reposnse onset value")
            # saveIt <- FALSE
            next() # next o
            # attempt to work with a different xOnset for this xPeak
          }
        }
      }
      
      ##  compute the cut value ## 
      
      if( length(theseNegSlopeVals) > 0 ) { 
        # no need for this if the data never descend between thisXOnsetROW and lastXPeak
        
        # initialize the lowest xOnset to get started
        if(o==1) {
          # this may not be the actual lowest, but we make a strong assumption for now
          lowestXOnset <- thisXOnsetROW
          lowestXOnsetVal <- tsData[lowestXOnset]
        }
        
        # keep the lowest xOnset for all iterations of the o loop over xOnsetROW
        lowestXOnset <- ifelse(lowestXOnsetVal <= thisXOnsetLoopVal,
                               lowestXOnset,
                               thisXOnsetROW)
        lowestXOnsetVal <- tsData[lowestXOnset]
        
        # compute the distance from lastXPeak to thisXOnsetROW
        thisOnsetPeakDiff <- prevXPeakVal - lowestXOnsetVal
        
        # compute the cutoff value
        thisCutVal <- lowestXOnsetVal + (thisOnsetPeakDiff * (1 - descProp))
        # descProp is a parameter that is initialized in the NCCAASCII_init.R script
        
        # initialize the max or highest cut value to get started
        if(o==1) {
          highestCutVal <- thisCutVal
        }
        
        # key the highest cut value for all iterations of the o loop over xOnsetROW
        # need only the y-axis value, not the sample index
        highestCutVal <- ifelse(highestCutVal >= thisCutVal,
                                highestCutVal,
                                thisCutVal)
      }
      
      #### check if the data descended too much from the hightest peak preceding the lastXPeak ####
      
      {
        # slice the negative slope indices after the prevXPeak
        useNegSlopeIndcs <- theseNegSlopeIndcs[which(theseNegSlopeIndcs > prevXPeak)]
        useNegSlopeVals <- tsData[useNegSlopeIndcs]
        
        if(isTRUE(useDescentRule)) {
          # descentRule is a boolean that is initialized in the NCCAASCII_init.R script
          if( length(useNegSlopeIndcs) > 0 && all(!is.na(useNegSlopeIndcs)) ) {
            if( any( useNegSlopeVals <= highestCutVal ) ) {
              # print("data descended too much from a previous xPeak to the origin or response onset value")
              # saveIt <- FALSE
              next() # next o 
              # attempt to work with a different xOnset for this xPeak
            }
          }
        }
      }
      
      ## save the info for output if the data have not descended too much ##
      
      if(isTRUE(saveIt)) {
        # the max distance from onset and peak will be selected for output using these vectors
        xOnsetVc[o] <- thisXOnsetROW
        xPeakVc[o] <- lastXPeak
        # these will be NA if the data have descended too much
      }
      
    } # end o loop
    
  } # end if(length(useXPeakLoop) > 1 && length(useXOnsetLoop) > 1)
  
  #### compute and select the max xPeak - xOnset distance ####
  
  {
    # compute the distance between the last xPeak in xPeakLoop,
    # and each xOnset where the data did no descend half-way,
    # from an intermediate peak to origin before the last xPeak
    
    # lastXPeak and lastXPeakVal were computed earlier
    
    xOnsetVc <- xOnsetVc[!is.na(xOnsetVc)]
    xPeakVc <- xPeakVc[!is.na(xPeakVc)]
    
    if(length(xOnsetVc) == 0) {
      return(list(thisOnsetIdx=NA, 
                  thisOnsetVal=NA, 
                  thisYDistance=NA, 
                  thisXPeak=lastXPeak,
                  thisXPeakVal=lastXPeakVal, 
                  segmentName=segmentName, 
                  segmentTitle=segmentTitle ))
    }
    
    xOnsetYVals <- tsData[xOnsetVc]
    xPeakYVals <- tsData[xPeakVc]
    
    yDistVc <- rep(lastXPeakVal, times=length(xOnsetVc)) - xOnsetYVals
    # it is the same if done this way
    # yDistVc <- xPeakYVals - xOnsetYVals
    
    thisOnsetIdx <- xOnsetVc[which.max(yDistVc)]
    thisOnsetVal <- tsData[thisOnsetIdx]
    # lastXPeak
    thisYDistance <- lastXPeakVal - thisOnsetVal
    
  }
  
  #### output ####
  
  # return a list 
  return(list(thisOnsetIdx=thisOnsetIdx, 
              thisOnsetVal=thisOnsetVal, 
              thisYDistance=thisYDistance, 
              thisXPeak=lastXPeak,
              thisXPeakVal=lastXPeakVal, 
              segmentName=segmentName, 
              segmentTitle=segmentTitle ) )
  
} # end descentRuleFn() function







