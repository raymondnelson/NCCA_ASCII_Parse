# activity sensor signal processing
# 5-1-2016
# Raymond Nelson


activitySigProcFn <- function(x=chartDF, first=firstEvent, last=lastEventEnd) {
  # activity sensor signal processing
  # 5-1-2016
  # Raymond Nelson
  ####
  
  chartDF <- x
  
  # fix NA values
  # chartDF$c_Move1 <- NAInterp(chartDF$c_Move1)
  
  # exit the function if the vector is NA
  if(length(which(is.na(chartDF$c_Move1)))==nrow(chartDF)) return(chartDF)

  # first make sure that all peaks are recorded on a single sample
  chartDF$c_Move1 <- fixPeak(x=NAInterp(chartDF$c_Move1), times=4)
  
  # compute the moving average of the activity sensor data
  chartDF$c_Move1MA <- MASmooth(x=chartDF$c_Move1, y=round(3*cps,0), times=4)
  
  # # scale the activity sensor data 
  # chartDF$c_Move1 <- scaleDataFn(chartDF$c_Move1, sec=12, times=20, xRange=scaleVals[6], maxY=165, minY=-165, firstRow=first, lastRow=last)
  
  # # offset the activity sensor data
  # chartDF$c_Move1 <- offsetDataFn(x=chartDF$c_Move1, y=yOffset[6], yMax=165, yMin=-165, firstRow=first, lastRow=last)
  
  # processed activity data
  # 3-22-2017 now in the scaleOffsetData.R script
  # 5-15-2019 done here first and done again in the scaleOffsetData.R script
  # so that artifact extraction can use the processed data
  # was y=round(.5*cps,0) 2020-06-06
  # chartDF$c_Move1Proc <- MASmooth(x=chartDF$c_Move1, y=round(.1*cps,0), times=3)
  chartDF$c_Move1Proc <- lowPass.1875(x=chartDF$c_Move1)
  # chartDF$c_Move1ProcMA <- MASmooth(x=chartDF$c_Move1Proc, y=round(6*cps,0), times=10)
  # chartDF$c_Move1ProcMA <- MASmooth(x=chartDF$c_Move1Proc, y=round(3.75*cps,0), times=4)
  chartDF$c_Move1ProcMA <- lowPass8th.1181hz(x=chartDF$c_Move1Proc)
  
  # plot.ts(chartDF$c_Move1Proc[c(1000:5000)])
  
  # chartDF$c_Move1Proc <- chartDF$c_Move1
  
  # chartDF$c_Move1Proc <- scaleDataFn(chartDF$c_Move1Proc, sec=12, times=20, xRange=scaleVals[6], maxY=165, minY=-165, firstRow=first, lastRow=last)
  # chartDF$c_Move1Proc <- offsetDataFn(x=chartDF$c_Move1Proc, y=(yOffset[6]+20), yMax=165, yMin=-165, firstRow=first, lastRow=last)
  
  # activity result
  # chartDF$c_Move1Result <- yOffset[6]-.05*yRange
  
  # myData <- chartDF$c_Move1Proc
  
  #         # use a helper functon to make a vector of slope values
  #         mySlope <- slopeDir(x=myData)  
  #         
  #         # use a helper function to smooth the slope by removing slope changes of small duration
  #         mySlope1 <- smoothSlope(x=mySlope, n=1)
  #         
  #         # fill the zero slope segments
  #         mySlope2 <- fillSlope(x=mySlope1)
  #         
  #         mySlope3 <- positiveSlope(x=mySlope2)
  #         
  #         # locate the indices of all positive slope onset rows
  #         exhMin <- positiveOnset(x=mySlope3)
  #         
  #         exhMin[1] <- 1
  #         exhMin[length(myData)] <- 1
  #         
  #         exhMinVal <- myData[which(exhMin!=0)] 
  #         
  #         # interpolate between the exhalation min indices
  #         exhMin2 <- interpolatePeaks(x=which(exhMin!=0), y=exhMinVal)
  
  # bufferLen <- round(.75*cps,0)
  # chartDF$c_Move1Min <- interpolatePeaks(x=minPeak(x=chartDF$c_Move1Proc, y=bufferLen), y=chartDF$c_Move1Proc[minPeak(x=chartDF$c_Move1Proc, y=bufferLen)])
  # chartDF$c_Move1Max <- interpolatePeaks(x=maxPeak(x=chartDF$c_Move1Proc, y=bufferLen), y=chartDF$c_Move1Proc[maxPeak(x=chartDF$c_Move1Proc, y=bufferLen)])
  
  ###
  
  # locate the idices of all negative slope onset rows
  #         exhMax2 <- interpolatePeaks(x=maxPeak(x=myData, y=40), y=myData[maxPeak(x=myData, y=40)])
  
  
  # process the min cycle
  
  # compute the inhalation max for each respiration cycle
  
  ###
  
  # plot.ts(chartDF$c_Move1MA)
  # plot.ts(chartDF$c_Move1)
  
  return(chartDF)
  
} # end newActivitySigProcFn()


