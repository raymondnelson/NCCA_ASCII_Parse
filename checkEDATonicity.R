# R function to evaluate activity prior to stimulus onset and response onset
# Raymond Nelson
# August 8, 2023
#
# called by the amplitudeExtractPC function 
# after extracting the response, onset, end, and response value
#
# prestimulus and pre-response activity is evaluated for tonicity
# and compared to the response value
#
####
#
# this script contains 4 functions
# 
#
# EDAMvtArtifactFn() # called by the checkEDATonicityFn() function
# 
# EDAPrestimActivityFn() # called by the checkEDATonicityFn() function
#
# preWOERatioFn() # called by the checkEDATonicityFn() function
# 
# checkEDATonicityFn() # the main function
#
#





####



EDAMvtArtifactFn <- function(tsData=chartDF$c_AutoEDA, preSec=4, zCut=3) {
  # R function to identify EDA artifacts resulting from finger movement
  # and disruption of the EDA circuit
  # 11-26-2016
  # 9-3-2023
  # 5-10-2025
  # Raymond Nelson
  ####
  # called for each stim segment 
  # by the checkEDATonicityFn() function in the checkEDATonicity.R script
  #
  # also called by the artifactProcFn() in the artifactProc.R script 
  #
  # tsData input is the time series EDA data
  # preSec is the number of prestimulus seconds to compare to each 
  # zCut is the boundary for sig/artifact activity in standard deviations
  # output is a vector of 0s and "Artifact" codes
  ####
  # tsData <- chartDF$c_AutoEDA
  ## initialize the output vector ##
  outputVector <- rep(0, times=length(tsData))
  ## set the z cutscore for significant activity ##
  # pnorm(4.3)
  # .99999
  # zCut <- 4.3
  ## round the nPre and nPost segments to the nearest sample ##
  preLen <- round(cps*preSec,0)
  postLen <- round(cps*.1,0)
  ## set the exponent value to transform the difference vals ##
  expVal <- 1
  ## initialize an output vector ## 
  negOut <- NULL
  posOut <- NULL
  ## a private function to calculate the population standard deviation ##
    sdp <- function(x)(sqrt(var(x, na.rm=TRUE)*(length(x)-1)/length(x)))
  ## iterate over the length of the time series data for neg slope activity ##
  {
    i=1
    for (i in 1:(length(tsData)-(preLen+postLen)+1)) {
      # calculate the difference for all samples in the pre change segment
      preDiff <- ( diff(tsData[i:(i+preLen-1)] ) )^expVal
      # calculate the difference for all samples in the post change segment
      postDiff <- ( diff( tsData[(i+preLen):(i+preLen+postLen-1)] ) )^expVal
      # increment the loop if there is no slope in the pre or post
      # if(range(preDiff)[1]-range(preDiff)[2]==0 | range(postDiff)[1]-range(postDiff)[2]==0) next()
      if(range(postDiff)[1]-range(postDiff)[2]==0) next()
      # z-test for a significant change in prediff and postdiff means
      # use the sdp() private function to compute the population standard deviation
      if( ( ( mean(postDiff) - mean(preDiff) ) / sdp(preDiff) ) <= -zCut ) { 
        # mark the artifact after the end of the preLen
        negOut[(i+preLen+postLen-1)] <- "Artifact"
      } 
    } # end for loop
    # which(negOut == "Artifact")
    # which(negOut != 0)
  }
  ## make a vector to locate negative slope segments in the time series data ##
  {
    ## get this function from amplitudeExtractHelperFunctions.R script ##
    xNeg <- negativeSlope(fillSlope(smoothSlope(slopeDir(tsData))))
    # keep only those artifact marks in negative slope segments
    negOut[which(xNeg != -1)] <- 0
    # add the negOut to the outputVector
    outputVector[which(negOut != 0)] <- negOut[which(negOut != 0)]
    # locate the onset index for each run of artifacts
    artifact2Onset <- which(outputVector[2:length(outputVector)] !=
                              outputVector[1:(length(outputVector)-1)] &
                              outputVector[2:length(outputVector)] ==
                              "Artifact") + 1
    # locate the offset index for each run
    artifact2Offset <- which(outputVector[1:(length(outputVector)-1)] !=
                               outputVector[2:length(outputVector)] &
                               outputVector[1:(length(outputVector)-1)] ==
                               "Artifact")
    # correct for the last sample
    if(outputVector[length(outputVector)] == 
       outputVector[(length(outputVector)-1)] & 
       outputVector[length(outputVector)] == "Artifact") { 
      artifact2Offset <- c(artifact2Offset, length(outputVector)) 
    } 
    if(length(artifact2Offset) < length(artifact2Onset)) {
      artifact2Offset <- c(artifact2Offset, artifact2Onset[length(artifact2Onset)])
    }
  }
  ## check the length of each run ##
  {
    # excludeThese <- which(artifact2Onset %in% artifact2Offset)
    # artifact2Onset <- artifact2Onset[-excludeThese]
    # artifact2Offset <- artifact2Offset[-excludeThese]
    # runLengths <- (artifact2Offset - artifact2Onset) + 1
    # # exclude short neg slope runs of less than 1/10 second
    # excludeThese <- which(runLengths < 3) # 1/10sec
    #    <- artifact2Onset[-excludeThese]
    # artifact2Offset <- artifact2Offset[-excludeThese]
  }
  ## check the y-axis range for each run of artifacts ##
  {
    # keep the run if the range exceeds a value of 2.5% of the max working range (2000)
    ## first initialize some vectors
    onsetOut <- NULL
    offsetOut <- NULL
    ## initialize a variable for the length of the range
    # this is the y-axos descent distance required to keep an artifact
    descSig <- round(yRange * .025, 0) # 0.5% of the y-axis range
    # 1 will exclude a sig change on a single sample, keeping only runs
    # descSig <- 1
    # 0 will keep all artifacs
    # descSig <- 0
    # tsData[artifact2Onset] - tsData[artifact2Offset]
    ## then execute the loop if there are any artifacts
    if(length(artifact2Onset) > 0) {
      i=1
      for(i in 1:length(artifact2Onset)) {
        # check if the magnitude of descent is small enought to ignore 
        if(abs(diff(range(tsData[artifact2Onset[i]:artifact2Offset[i]]))) >= descSig) {
          # keep the onset and offset of neg slope runs that exceed the min y-axis distance
          onsetOut <- c(onsetOut, artifact2Onset[i])
          offsetOut <- c(offsetOut, artifact2Offset[i])
        }
      }
    } 
    ## re-initialize the outputVector without small descent values ##
    outputVector <- rep(0, times=length(tsData))
    ## then use another loop ##
    if(length(onsetOut) > 0){
      for(i in 1:length(onsetOut)) {
        outputVector[onsetOut[i]:offsetOut[i]] <- "Artifact"
      }
    }
  }
  ## output ##
  # output is a vector of 0s and "Artifact" codes where significant activity is observed
  return(outputVector)
  # return(which(outputVector != 0))
} # end EDAMvtArtifactFn() function 



EDAPrestimActivityFn <- function(tsData, prestimRows, newPrestimRow, asc=2, dsc=2) {
  # R function to identify EDA artifacts prior to stimulus onset
  # also used to identify EDA artifacts prior to response onset
  # abstracted from the main function Dec 5, 2023
  # Raymond Nelson
  ####
  # called by the checkEDATonicityFn()
  # non-tonic activity might be in the form of NSPAs prior to stimulus onset
  # tsData input is is the time series EDA data for the stimulus segment
  # prestimRows is a vector of row indices for the prestimulus segment
  # newPresetimRow
  # asc input is the ascent ratio constraint
  # dsc input is the desceht ratio constraint
  # output is a vector of indices where significant movement was found
  ####
  if(!exists("dsc")) dsc <- 2
  if(!exists("asc")) asc <- 2
  ## initialize the output vector
  prestimVc <- rep(0, times=length(tsData))
  ## compute the prestim range
  {
    prestimMaxIdx <- which.max(tsData[prestimRows]) + newPrestimRow - 1
    prestimMinIdx <- which.min(tsData[prestimRows]) + newPrestimRow - 1
    prestimMaxVal <- tsData[prestimMaxIdx]
    prestimMinVal <- tsData[prestimMinIdx]
    # now get the absolute range
    prestimRange <- prestimMaxVal - prestimMinVal
    # and adjust the prestim rows
    ifelse(prestimMaxIdx > prestimMinIdx, 
           prestimRows <- c(prestimMinIdx:prestimMaxIdx),
           prestimRows <- c(prestimMaxIdx:prestimMinIdx) )
  }
  ## calculate the ratio of prestimulus ascent and descent
  {
    if(prestimMaxIdx > prestimMinIdx){
      # max index is after the min index
      # ascending prestim to response onset
      prestimAscentRatio <- abs(prestimRange) / yChangeValue
      prestimDescentRatio <- 0
    } else {
      # max index is before the min index
      # descending prestim 
      prestimAscentRatio <- 0
      prestimDescentRatio <- abs(prestimRange) / yChangeValue
    }
  }
  ## interpret the activity during the prestimulus segment
  if(yChangeValue <= 50) {
    # yChange value < 2.5% of the y-axis range
    # if(abs(prestimRange) >= 150) {
    # Sep 14, 2024
    if(abs(prestimRange) >= (1.5 * yChangeValue)) {
      # for both ascending and descending eda data
      # mark an artifact when the prestimulus range is 3x the y-change EDA response
      prestimVc[prestimRows] <- "Artifact"
      # return(prestimVc)
      # return(0)
    }
  } else {
    # yChange values greater than 2.5% of the y-axis range 
    # if(prestimDescentRatio >= 1.5) { # was 2
    if(prestimDescentRatio >= dsc) { # was 1.5
      prestimVc[prestimRows] <- "Artifact"
    } else if(prestimAscentRatio >= asc) { # was >= .85
      # } else if(prestimAscentRatio >= .85) { # was >= 2
      prestimVc[prestimRows] <- "Artifact"
      # which(prestimVc!=0)
    } 
  }
  ## done
  # return(which(prestimVc!=0))
  return(prestimVc)
} # end EDAPrestimActivityFn()



preWOERatioFn <- function(tsData, preWOERows, preWOEOnset) {
  # R function to compare the ratio of y-axis activity in the WOE and pre-WOE segments
  # abstracted from the main function Dec 5, 2023
  # Raymond Nelson
  ####
  # called by the checkEDATonicityFn()
  # non-tonic activity might be in the form of NSPAs prior to stimulus onset
  # tsData input is is the time series EDA data
  # preResponseRows is a vector of row indices for the pre-Response segment
  # output is a vector of indices where significant movement was found
  ####
  ## initialize the output vector
  preWOEVc <- rep(0, times=length(tsData))
  ## get the pre-WOE range
  {
    preWOEMaxIdx <- which.max(tsData[preWOERows]) + preWOEOnset - 1
    preWOEMinIdx <- which.min(tsData[preWOERows]) + preWOEOnset - 1
    # # adjust the pre WOE rows
    ifelse(preWOEMaxIdx > preWOEMinIdx,
           preWOERows <- c(preWOEMinIdx:preWOEMaxIdx),
         preWOERows <- c(preWOEMaxIdx:preWOEMinIdx) )
  if(preWOEMaxIdx > preWOEMinIdx) {
    # ascending preresponse segment
    preWOERange <- max(tsData[preWOERows]) - min(tsData[preWOERows])
  } else {
    # no pre-WOE range for descending data
    preWOERange <- 0
  }
  } 
  ## compare the ratio of pre and post WOE activity to the response 
  if(!is.null(yChangeValue)) {
    # only if there is any ascending prestim or poststim activity
    preWOERatio <- preWOERange / yChangeValue
  } else {
    # no effect if there is no y-change feature extraction
    preWOERatio <- preWOERange / 1
  }
  ## interpret the pre and post WOE ratios 
    preWOEVc[preWOERows][which(preWOERatio >= 3)] <- "Artifact" # was 2
  ## output
  # return(which(preWOEVc!="0"))
  return(preWOEVc)
} # end preWOERatioFn
 


######### main function ############



checkEDATonicityFn <- function(segmentName,
                               tsData, 
                               # artifactVector, 
                               onsetRow, 
                               yChangeOnset, 
                               yChangeOnsetValue,
                               yChangePeak,
                               yChangePeakValue,
                               yChangeValue,
                               EDAPrestim ) { 
  # R function to evaluate EDA activity prior to stimulus onset and response onset
  # Raymond Nelson
  # August 8, 2023
  ####
  # called by the amplitudeExtractPC function 
  # after extracting the response, onset, end, and response value
  #
  # prestimulus and pre-response activity is evaluated for tonicity
  # and compared to the response value
  #
  # EDA instability can be descending or ascending
  # and can occur for three primary reasons
  # 1) large NSPAs
  # 2) finger movement
  # 3) a damaged EDA sensor
  ### input
  # tsData is the time series data for a stimulus segment including prestimulus and poststimulus data
  # onsetRow is the sample index for the onset fo the stimulus question, ususall 301
  # yChangeOnset is the sample index for the onset of the physiological response
  # yChangeValue is the data value at the response onset index
  # yChangePeak is the sample index at the peak (end) of the physiological response
  # yChangePeakValue is the data value at the peak (end) of response
  # yChangeValue is the + increase in data value from response onset to response end
  # prestim is the length of the prestimulus and pre-response segments in seconds
  ### output
  # a vector of 0s and "Artifact" indices
  ####
  
  # exit segments less than 35 seconds
  # if(length(tsData) > (35*cps)) return(rep(0, times=length(tsData)))
     
  if(!exists("EDAPrestim")) EDAPrestim <- 4
  
  # exit if there is no extracted y-axic change in the EDA data
  # if(is.na(yChangeValue) || yChangeValue == 0) return(0)
  
  {
    # initialize some output vectors
    outVc <- rep(0, times=length(tsData))
    prestimVc <- rep(0, times=length(tsData))
    preresponseVc <- rep(0, times=length(tsData))
    preWOEVc <- rep(0, times=length(tsData))
    postWOEVc <- rep(0, times=length(tsData))
  }
  
  {
    # save the values to the global envir for inspection
    assign("tsData", tsData, envir=.GlobalEnv)
    assign("onsetRow", onsetRow, envir=.GlobalEnv)
    assign("yChangeOnset", yChangeOnset, envir=.GlobalEnv)
    assign("yChangeOnsetValue", yChangeOnsetValue, envir=.GlobalEnv)
    assign("yChangePeak", yChangePeak, envir=.GlobalEnv)
    assign("yChangePeakValue", yChangePeakValue, envir=.GlobalEnv)
    assign("yChangeValue", yChangeValue, envir=.GlobalEnv)
    assign("EDAPrestim", EDAPrestim, envir=.GlobalEnv)
  }
  
  #### locate the prestim, preresponse, stim, and poststim segments ####
  
  {
   
    ## EDAPrestim parameter is initialized in the NCCAASCII_init.R script ##
    
    {
      ## compute the prestimulus rows (prior to stimulus onset) ##
      
      # EDAPrestim is set in the NCCAASCII_init.R script
      newPrestimRow <- onsetRow - round(EDAPrestim*cps)
      # newPrestimRow <- yChangeOnset - round(EDAPrestim*cps)
      if(is.na(yChangeValue) || yChangeValue == 0) {
        # when there is no EDA response
        # prestim rows are from EDAPrestim to stimulus onset
        newPrestimEndRow <- onsetRow - 1
        # or to the end of the WOE
        # newPrestimEndRow <- onsetRow + round(15.5*cps)
      } else {
        # extends from EDAPrestim to 1 sample before response onset
        newPrestimEndRow <- yChangeOnset - 1 
      }
      prestimRows <- c(newPrestimRow:newPrestimEndRow)
    }
    
    {
      ## get the preresponse rows ##
      
      # EDAPrestim is set in the NCCAASCII_init.R script
      preResponseIdx <- yChangeOnset - round((EDAPrestim)*cps)
      preResponseEnd <- yChangeOnset - 1
      preResponseRows <- c(preResponseIdx:preResponseEnd)
    }
    
    {
      ## get the rows before the WOE ##
      
      preWOEOnset <- onsetRow-(5*cps)
      if(preWOEOnset < 1) preWOEOnset <- 1
      # preWOEEnd <- onsetRow - 1
      preWOEEnd <- yChangeOnset - 1
      if(preWOEEnd > length(tsData)) preWOEEnd <- length(tsData)  
      preWOERows <- c(preWOEOnset:preWOEEnd)
    }
    
    {
      ## get the post-WOE rows ##
      
      if(!is.null(yChangePeak)) {
        # WOEVal <- max(tsData[onsetRow:(onsetRow+(15*cps))]) - min(tsData[onsetRow:(onsetRow+(15*cps))]) - min())
        # post WOE begins 1/2 sec after response peak
        postWOEOnset <- yChangePeak + 1 + round(.5*cps)
      } else {
        # post WOE starts at WOE end when there is no y-axis response to the stimulus
        postWOEOnset <- onsetRow + (15*cps)
      }
      postWOEEnd <- onsetRow + (20*cps)
      if(postWOEEnd > length(tsData)) postWOEEnd <- length(tsData)
      postWOERows <- c(postWOEOnset:postWOEEnd)
    }
    
  }
  
  #### evaluate the pre-stimulus activity for NSPA ####
  
  {
    prestimVc <- EDAPrestimActivityFn(tsData, prestimRows, newPrestimRow, asc=4, dsc=2)
    # prestimVc is a vector of0s and "Artifact" marks at rows where sig activity was observed
    
    # submit the artifacts to the output vector
  }
  
  #### next evaluate the pre-response activity for NSPA ####
  
  {
    # was dsc=.75 2024Sep02
    preresponseVc <- EDAPrestimActivityFn(tsData, preResponseRows, preResponseIdx, asc=3, dsc=.75)
    # preresponseVc is a vector of 0s and "Artifact" marks at rows where sig activity was observed
    
    # submit the artifacts to the output vector
  }
  
  #### plot the segment #### 
  
  # {
  #   artifactIndices <- which(preresponseVc != 0)
  #   DAT <- cbind.data.frame(tScale=c(1:nrow(segmentDF)),
  #                           dat=segmentDF$c_AutoEDA[] )
  #   ggplot() +
  #     geom_path(DAT, mapping=aes(x=tScale, y=dat), color="green") +
  #     annotate("point", x=DAT$tScale[artifactIndices], y=DAT$dat[artifactIndices], shape=8, size=4, color="blue")
  # } 
  
  #### and check the range and ratio of the preWOE and postWOE segments ####
  
  {
    # call a function to calculate the ratio of activity in the pre-WOE and WOE 
    preWOEVc <- preWOERatioFn(tsData, preWOERows, preWOEOnset)
    # call the same function for the post-WOE segment
    postWOEVc <- preWOERatioFn(tsData, postWOERows, postWOEOnset)
    
    # # which(preWOEVc!="0")
    # # which(postWOEVc!="0")
  }
  
  #### call a function to extract finger movement via variance ####
  
  {
    # call a function to locate sample rows where finger movement occurs
    edaMvtVc <- EDAMvtArtifactFn(tsData=tsData, preSec=4, zCut=3)
    
    # submit the artifacts to the output vector
    outVc[which(edaMvtVc == "Artifact")] <- "Artifact"
    
    # plot.ts(tsData)
  }
  
  #### submit the artifacts to the output vector ####
  
  {
    # reduce the other output to a single vector 
    
    # outVc[edaMvtVc] <- "Artifact"
    
    # outVc[which(prestimVc=="Artifact")] <- "Artifact1"
    
    # 
    outVc[which(preresponseVc=="Artifact")] <- "Artifact2"
    # only the preresponseVc artifacts are submitted to the Test of Proportions
    
    # outVc[which(preWOEVc=="Artifact")] <- "Artifact3"
    # outVc[which(postWOEVc=="Artifact")] <- "Artifact4"
  }
  
  #### output ####
  
  # output needs to be a artifact vector of 0s and "Artifact" codes
  return(outVc)
  
} # end checkEDATonicityFn()


