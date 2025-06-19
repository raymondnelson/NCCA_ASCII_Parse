
# source this for the maxPeak and minPeak functions
# source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)



activityCheckFn <- function(x=chartDF) {
  # 12-30-2016
  # main function to check for significant physical activity 
  # input is the chartDF
  # output is the chartDF with artifacts labeled in the Move1_a vector
  # 
  # also calls the activityCheckFn() function in this script
  # to check for unresponsive activity sensor data
  # 
  ####
 
  {
    
    chartDF <- x
    # which(chartDF$Move1_a != "")
    
    chartName <- chartDF$chartName[1]
    
    # get the activity sensor column
    # tsData <- chartDF$c_Move1
    
    # exit for short charts less than 20 seconds
    if(nrow(chartDF) < 600) { return(chartDF) }
    
    # exit if the data are NA
    if(length(which(is.na(chartDF$c_Move1)))==nrow(chartDF)) return(chartDF)
    
  }
  
  ####
  
  {
    
    # make a list of input parameters for the dataCheck
    activityCheckParams <- list(x=chartDF$c_Move1Proc, 
                                sec=.5, 
                                firstRow=NULL, 
                                lastRow=NULL,
                                sVal=100,
                                name="activity",
                                expVal=2,
                                fenceVal=6,
                                fence="upper")
    
    # assign the parameter list to the global env
    assign("activityCheckParams", activityCheckParams, pos=1)
    
  }
  
  ###### create a pre and post buffer for verbal answers ######
  
  {
    
    answerRows <- which(chartDF$Label %in% c("YES", "NO", "ANS"))
    
    if(length(answerRows) > 0) {
      
      answerBuffOn <- answerRows - 1.5 * cps
      answerBuffOff <- answerRows + 1.5 * cps
      
      answerBuffer <- NULL
      
      for(i in 1:length(answerBuffOn)) {
        answerBuffer <- c(answerBuffer, answerBuffOn[i]:answerBuffOff[i])
      }
      
    } else {
      
      # answerRows <- NULL
      answerBuffOn <- answerRows - 1.5 * cps
      answerBuffOff <- answerRows + 1.5 * cps
      answerBuffer <- NULL
      
    }
    
  }
  
  ######## significant change in physical activity ########
  
  {
    
    xOut <- NULL
    
    # call the activityCheckFn 
    # this activityCheckFn is included in this script
    
    # plot.ts(chartDF$c_Move1Proc)
    # plot.ts(chartDF$c_Move1)
    
    ## call the activity check function ##
    
    if(!is.null(chartDF$c_Move1Proc)) {
      xOut <- activityCheckFn(activityCheckParams=activityCheckParams)
    } 
    
    # View(xOut)
    # which(xOut$artifacts != "")
    # xOut$artifacts[which(xOut$artifacts != "")]
    
    # xOut is a data frame with 2 columns
    # diffs is a the abs diff of each successive sample
    # artifacts is a vector of "" and "Artifact" indicators
    # xOut will be null if no activity
    
    # add the artifacts to the chart data frame
    if(!is.null(xOut)) {
      chartDF$Move1_a <- xOut$artifacts
    }
    # which(chartDF$Move1_a != "")
    # chartDF$Move1_a[which(chartDF$Move1_a != "")]
  } 
  
  #### add the artifacts to the chart data frame ####
  
  if(!is.null(xOut)) {

    # outVector <- xOut$diffs
    # artifactVector <- xOut$artifacts
    # # which(artifactVector != "")
    # 
    # # create an abstracted activity channel
    # outVector <- scaleDataFn(x=outVector, sec=8, times=20, ignore=10, yRange=scaleVals[6], maxY=(yMax-.05*yRange), minY=(yMin+.05*yRange), firstRow=NULL, lastRow=NULL)
    # outVector <- offsetDataFn(x=outVector, y=yOffset[6], maxY=(yMax-.05*yRange), minY=(yMin+.05*yRange), firstRow=NULL, lastRow=NULL)
    # 
    # chartDF$c_Move1Abstrct1 <- outVector
    # 
    # # add the artifacts to the chart data frame
    # # chartDF$Move1_a <- xOut$artifacts
    # # chartDF$Move1_a[which(chartDF$Move1_a != "0")]
    # 
    # # which(chartDF$Move1_a != "")
    # 
    # # chartDF$Artifacts_a[which(xOut$artifacts != 0)] <- "artifact"
    # # chartDF$Artifacts_a[which(chartDF$Artifacts_a != 0)]
    # 
    # # plot.ts(diffVector)
    # # plot.ts(chartDF$c_Move1)
    # # plot.ts(chartDF$c_Move1Proc)

  }
  
  ##### check the stability of the activity data #####
  
  {
    
    # may need additional smoothing to the Move1 data
    
    chartDF$c_Move1Proc <- MASmooth(x=chartDF$c_Move1, y=round(.5*cps,0), times=2)

    # locate the activity peak points
    maxPeaks <- maxPeak(x=chartDF$c_Move1Proc, y=round(.5*cps,0))
    # remove peak points across consecutive samples
    consecPeaks <- which((maxPeaks[2:length(maxPeaks)]-1)==maxPeaks[1:(length(maxPeaks)-1)])
    if(length(consecPeaks > 0)) { maxPeaks <- maxPeaks[-consecPeaks] }
    # remove peak changes less than 1/4 second
    shortSegments <- which(diff(maxPeaks) <= round(.25*cps,0)) + 1
    if(length(shortSegments) > 0) maxPeaks <- maxPeaks[-shortSegments]

    # locate the activity low points
    minPeaks <- minPeak(x=chartDF$c_Move1Proc, y=round(.5*cps,0))
    # remove peak points across consecutive samples
    consecPeaks <- which((minPeaks[2:length(minPeaks)]-1)==minPeaks[1:(length(minPeaks)-1)])
    if(length(consecPeaks > 0)) { minPeaks <- minPeaks[-consecPeaks] }
    # remove peak changes less than 1/4 second
    shortSegments <- which(diff(minPeaks) <= round(.25*cps,0)) + 1
    if(length(shortSegments) > 0) minPeaks <- minPeaks[-shortSegments]

    # get the amplitude for the peak and low points
    maxAmp <- chartDF$c_Move1Proc[maxPeaks] - chartDF$c_Move1ProcMA[maxPeaks]
    minAmp <- chartDF$c_Move1ProcMA[minPeaks] - chartDF$c_Move1Proc[minPeaks]

    # check if the peak points cross the mid line
    maxAmpX <- maxPeaks[which(maxAmp < -1)]
    minAmpX <- minPeaks[which(minAmp < -1)]

    # add the artifacts to the chart data frame
    chartDF$Move1_a[maxAmpX] <- "ArtifactX"
    chartDF$Move1_a[minAmpX] <- "ArtifactX"
    # which(chartDF$Move1_a == "ArtifactX")

  }
  
  ######## remove artifacts associated with verbal answers ########
  
  {
    
    # chartDF$Move1_a[answerBuffer] <- ""
    
  }
  
  #### output ####
  
  return(chartDF)
  
} # end activityCheckFn() function




# activityCheckParams <- list(x=chartDF$c_Move1, 
#                         sec=.5, 
#                         firstRow=NULL, 
#                         lastRow=NULL,
#                         sVal=100,
#                         name="activity",
#                         expVal=7)
#                         fenceVal
#                         fence="upper"




activityCheckFn <- function(activityCheckParams=activityCheckParams) {
  # helper function to check for significant changes in activity
  # called by the activityCheck() function in this script
  # 2020
  # Raymond Nelson
  ### input is a list (activityCheckParams) of 9 items
  # x input is the time series vector from the chartDF data frame
  # sec is the number of seconds to sample
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the endex of the end of the scoring window for the last stimulus event
  # sVal is the scale value for the time series, set in the init 
  # name is the name of the channel
  # expVal is the exponent value 
  # fenceVal is the number of Tukey fences for significant activity
  # fence can be "lower" "upper" or "both"
  ### output
  # output is one of two messages: "none", "possible unresponsive EDA"
  ####
  {
    tsData <- as.numeric(activityCheckParams$x)
    sec <- activityCheckParams$sec
    firstRow <- activityCheckParams$firstRwo
    lastRow <- activityCheckParams$lastRow
    sVal <- activityCheckParams$sVal
    name <- activityCheckParams$name
    expVal <- activityCheckParams$expVal
    fenceVal <- activityCheckParams$fenceVal
    fence <- activityCheckParams$fence
  }
  ###
  dataLength <- length(tsData)
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength
  
  # set the default output message
  outputMessage <- "none"
  
  # initialize a result vector
  diffVector <- rep(0, times=dataLength)
  
  # calculate the absolute difference for all successive samples
  tsData <- abs(diff(tsData))^expVal
  
  if(length(tsData) < 450) return(NULL)
  
  # iterate over the time series data
  # and populate the diffVector with the absolute range for each slice
  for (i in 1:(length(tsData)-round(sec*cps,0))) {
    thisSlice <- tsData[i:(i+round(sec*cps,0)-1)]
    thisRange <- range(thisSlice)
    thisRangeDiff <- abs(thisRange[1] - thisRange[2])
    diffVector[i] <- thisRangeDiff
  }
  
  # for scaling and offsetting the firstRow and lastRow should be NULL 
  # because these have not been set for the chart
  # diffVector <- scaleDataFn(x=diffVector, sec=8, times=20, ignore=5, xRange=scaleVals[6], maxY=(yMax-.05*yRange), minY=(yMin+.05*yRange), firstRow=NULL, lastRow=NULL)
  # diffVector <- offsetDataFn(x=diffVector, y=yOffset[6], maxY=(yMax-.05*yRange), minY=(yMin+.05*yRange), firstRow=NULL, lastRow=NULL)
  # plot.ts(diffVector)
  # plot.ts(chartDF$c_Move1)
  # plot.ts(chartDF$c_Move1Proc)
    
  # length(diffVector)
  
  # fenceVal <- 9
  # fence <- "both"
  
  # calculate the segment length
  segLength <- round(12 * cps, 0)
  
  # calculate the latency length
  latLength <- round(2 * cps, 0)
  
  # initialize the output vector of the same length as the diffVector
  artifactVector <- rep("", times=length(diffVector))
  
  # iterate over the diffVector
  for (i in (segLength+latLength):length(diffVector)) {
    # next iteration if there is no data
    if(is.na(diffVector[i])) next()
    # load the inputBuffer
    segmentBuffer <- diffVector[(i-(segLength+latLength)+1):(i-latLength)]
    # increment the loop if there is insufficient data to calulate the IQR
    if(is.na(IQR(segmentBuffer, na.rm=TRUE))) next()
    # calculate the IQR and the 1st and 3rd quartiles
    # use IQR() from the stats package
    iqRange <- IQR(segmentBuffer, na.rm=TRUE)
    q25 <- quantile(segmentBuffer, .25, na.rm=TRUE)
    q75 <- quantile(segmentBuffer, .75, na.rm=TRUE)
    # set the upper and lower Tukey fences
    tukeyUpper <- q75 + fenceVal*iqRange
    tukeyLower <- q25 - fenceVal*iqRange
    # get the upper value
    if(fence != "lower") { if(diffVector[i] >= tukeyUpper) artifactVector[i] <- "Artifact" }
    # compare the sample diff i to the upper fence value
    # (abs(diffVector[i] - q75)) / iqRange
    # compare the sample diff I to the lower fence value
    if(fence != "upper") { if(diffVector[i] <= tukeyLower) artifactVector[i] <- "Artifact" }
    # compute the number of lower fences
    # (abs(diffVector[i] - q25)) / iqRange
  } # end for loop
  
  # output is a data frame
  xOut <- cbind.data.frame(diffs=diffVector, artifacts=artifactVector, stringsAsFactors=FALSE)
  return(xOut)
  
} # end activityCheckFn() function



# dataCheckFn(x=chartDF$c_AutoEDA)

# EDACheckFn(x=chartDF$c_AutoEDA, sec=5, times=25, omit=5, firstRow=NULL, lastRow=NULL)
# EDACheckFn(x=chartDF$c_LPneumoSm, sec=5, times=25, omit=5, firstRow=NULL, lastRow=NULL)





# activityCheckParams <- list(x=chartDF$c_Move1, 
#                             sec=.5, 
#                             firstRow=NULL, 
#                             lastRow=NULL,
#                             sVal=100,
#                             name="activity",
#                             expVal=2,
#                             fenceVal=9,
#                             fence="both")


# length(outVector)
# nrow(chartDF)


# outVector[which(outVector != "")]


