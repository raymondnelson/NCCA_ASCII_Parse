# R function to abstract a feature extraction value to a dimensionless unit scale
# November 17, 2023
# Raymond Nelson
####




abstractScaleFn <- function(x=250, sensorName="eda") {
  # R function to abstract a feature extraction value to a dimensionless unit scale
  # November 17, 2023
  # Raymond Nelson
  ####
  # 
  # Feature extraction values are extracted from the scaled data,
  # but a user may dynamically rescale the data by adjusting the display gain.
  # Feature extraction should be accomplished using the same data shown to the user,
  # and changing the gain will change the feature extraction value.
  #
  # This function will return the same feature extraction value 
  # when dynamically adjusting the display gain, 
  # and while using the same data shown to the user. 
  # 
  # Data are already scaled - or are dynamically re-gained - prior to feature extraction
  # Scale size is a function of the long-term aesthetic parameter scaleVals
  # And the dynamic (short term) gain value gainVals
  #
  # Caliper feature extraction is re-executed after adjusting the sensor gain
  # x-axis zoom should have no effect on the feature extraction
  #
  # This function reverts the effect of the short term gain setting
  # on the measured value of the feature extraction
  #
  # called by the maxOnsetPeakDistFn() in the getMaxOnsetPeakDistance.R script
  #
  # uses the rangeValues and gainValues environment parameters
  # that were initialized in the NCCAASCII_init.R script
  #
  # also uses the scaleVals environment parameter from the NCCAASCII_init.R script
  # 
  # this script is called by the maxOnsetPeakDistFn in the getMaxOnsetPeakDistance.R script
  #
  # x input is a feature extraction value in dimensionless units
  # sensorName is needed to get the correct item from scaleVals and gainVals environment parameters
  # 
  # scaleVals and gainVals are initialized in the NCCAASCII_init.R script
  # 
  # output is the feature extraction value in abstracted dimensionless units
  # but the feature extraction value will be stable for all gain settings 
  # 
  ####
  
  # translate the sensor name to get the correct range, scale, and gain values 
  useSensor <- ifelse(sensorName=="AutoEDA", 3, 
                      ifelse(sensorName=="ManualEDA", 3, 
                             ifelse(sensorName=="Cardio", 4, 
                                    ifelse(sensorName=="PLE", 5,
                                    3 ) ) ) )
  
  # should use switch() instead
  # switch()
  
    
  ## get the dynamic gain value from the global environment
  sensorGain <- gainVals[useSensor]
  
  ## get the abstracted range for the sensor
  sensorRangeVal <- rangeVals[useSensor]
  
  ## get the scaleVals item for the sensor display
  sensorScaleVal <- scaleVals[useSensor]
  
  # for testing
  # x <- x * (sensorGain / 100)
  
  ## calculate the gain adjusted range 
  adjustedRangeVal <- sensorRangeVal * (sensorGain / 100)
  
  adjustedScaleVal <- sensorScaleVal * (sensorGain / 100)
  
  ## calculate the abstraction multiplier
  # abstractionMultiplier <- adjustedRangeVal / adjustedScaleVal
  # abstractionMultiplier <- adjustedRangeVal / sensorScaleVal
  abstractionMultiplier <- sensorScaleVal / adjustedRangeVal
  
  ## calculate the output
  
  # xOut <- (x * (100 / sensorGain)) *  abstractionMultiplier
  xOut <- x *  abstractionMultiplier
  
  
  return(round(xOut))
  
}




