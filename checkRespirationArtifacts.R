# R function to evaluate respiration data for unusual activity
# Raymond Nelson
# August 25, 2023
#
# called by the pneumoExtractFn 
# after extracting the measured value 
#
# activity during the stimulus segment is compared with activity during the prestimulus
#
####







checkRespirationArtifactsFn <- function(segmentDF, onsetRow) {
  # R function to evaluate respiration data for unusual activity
  # Raymond Nelson
  # August 25, 2023
  #
  # called by the pneumoExtractFn 
  # after extracting the measured value 
  #
  # activity during the stimulus segment is compared with activity during the prestimulus
  #
  # calls the pneumoMeasurementFun in the pneumoMeasurmement.R scripte
  #
  #### input
  # segmentDF is the 15 sec time series respiration data 
  # including 10 prestimulus and 10 poststimulus seconds
  #
  #### output
  # a binary message 0 or 1 (1 = artifacted, 0 = not artifacted)
  #
  ####
  
  UPData <- segmentDF$UPneumo
  LPData <- segmentDF$LPneumo
  
  prestimRow <- onsetRow - (pneumoPrestim*cps)
  prestimEndRow <- onsetRow - 1
  prestimRows <- c(prestimRow:prestimEndRow)
  
  stimEndRow <- onsetRow + (measuredSeg*cps) - 1
  stimRows <- c(onsetRow:stimEndRow)
  
  poststimStartRow <- stimEndRow+1
  poststimEndRow <- addSeg*cps
  poststimRows <- c(poststimStartRow:poststimEndRow)
  
  ##  call the pneumoMeasurementFn() to measure the prestim and poststim segments
  
  UPrestimVal <- pneumoMeasurementFn(UPData[prestimRows], NULL)
  LPrestimVal <- pneumoMeasurementFn(LPData[prestimRows], NULL)
  
  UPVAL <- pneumoMeasurementFn(UPData[stimRows], NULL)
  LPVAL <- pneumoMeasurementFn(LPData[stimRows], NULL)
  
  UPoststimVal <- pneumoMeasurementFn(UPData[poststimRows], NULL)
  LPoststimVal <- pneumoMeasurementFn(LPData[poststimRows], NULL)
  
  # UPPrestimMeasurement <- 
  #   pneumoMeasurementFn(dataVector=segmentDF$c_UPneumoSm[prestimOnset:prestimOffset], 
  #                       verbalAnswer=NULL)
  # LPPrestimMeasurement <- 
  #   pneumoMeasurementFn(dataVector=segmentDF$c_LPneumoSm[prestimOnset:prestimOffset], 
  #                       verbalAnswer=NULL)
  
  ## calculate the prestim and poststim ratios
  
  UPrestimRatio <- abs(log(UPrestimVal / UPVAL))
  
  LPoststimRatio <- abs(log(LPrestimVal / LPVAL))
  
  ## check the prestim and poststimRatio
  
  cutRatio <- abs(log(1.5))
  
  if(UPrestimRatio >= cutRatio || LPoststimRatio >= cutRatio) {
    return("Artifact")
  } else {
    return("none")
  }
  
} # end checkRespirationArtifactsFn()



