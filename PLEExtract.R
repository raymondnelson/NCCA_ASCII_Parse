# PLE feature extracton
# 4-28-2016
# Raymond Nelson



# source('~/Dropbox/R/NCCA_ASCII_Parse/PLEMeasurement.R', echo=FALSE)
source(paste0(RPath, 'PLEMeasurement.R'), echo=FALSE)

source(paste0(RPath, 'checkPLEArtifacts.R'), echo=FALSE)


# newPLEExtractFn <- function(x=segmentDF, onset, offset, answer) {
newPLEExtractFn <- function(x=segmentDF, y=extract.params) {
	# PLE feature extracton
	# 4-28-2016
	# Raymond Nelson
  # PLE feature extraction function 
  # called from the featureExtraction() function
  # this function calls the PLEMeasurement() function 
  # in the PLEMeasurement.R script
  # x input is a data frame for a stimulus segment
  # the stimOnsetRow variable is obtained from the parent environment
  # including a 5 second prestimulus segment, 15 second evaluation window, and 5 additional seconds after the evaluation window
  # the length of the prestim and additional is arbitrary 
  # but PLE feature extraction requires at minimum 3 seconds prestimulus and 10 seconds after stim onset
  # output is the segmentDF 
  # after extracting the features and adding the extraction data to the extraction and measurement columns 
  ###
  
  {
    
    segmentDF <- x
    # View(segmentDF)
    
    extract.params <- y
    
  }
  
  #### segment info and PLE data vector ####
  
  {
    
    examName <- segmentDF$examName[1]
    seriesName <- segmentDF$seriesName[1]
    chartname <- segmentDF$chartName[1]
    
    segmentName <- segmentDF$eventLabel[301]
    
    # assign("segmentDF", segmentDF, envir=.GlobalEnv)
    # stop()
    
    dataVector <- segmentDF$c_PPG1
    
    # 2025Jun12 exit if the sensor column is present with no data
    if(sd(dataVector) == 0) return(segmentDF)
    
  }
  
  # if(segmentName == "1R1" ) {
  #   assign("segmentDF", segmentDF, envir=.GlobalEnv)
  #   assign("extract.params", extract.params, envir=.GlobalEnv)
  #   stop()
  # }

  ####
  
  {
    
    {
      stimOnsetRow <- extract.params$onset
      stimOffsetRow <- extract.params$offset
      answerRow <- extract.params$answer
      stimEndRow <- extract.params$end
    }
    
    {
      prestimOnset <- stimOnsetRow - (3*cps)
      if(prestimOnset < 1) prestimOnset <- 1
      prestimOffset <- stimOnsetRow - 1
      if(prestimOffset < 1) prestimOffset <- 1
      
      poststimOnset <- stimOnsetRow + ((5*cps))
      if(poststimOnset > nrow(segmentDF)) poststimOnset <- nrow(segmentDF)-1
      poststimOffset <- stimOnsetRow + ((10*cps-1))
      if(poststimOffset > nrow(segmentDF)) poststimOffset <- nrow(segmentDF)
    }
    
    #### response indices ####
    
    {
      segmentDF$PPG1Means <- 0
      segmentDF$PPG1Measure <- 0
      
      segmentDF$PPG1Extract <- ""
      # segmentDF$PPG1Prestim <- ""
      
      segmentDF$PPG1Extract[prestimOnset] <- "prestimSegOnset"
      segmentDF$PPG1Extract[prestimOffset] <- "prestimSegOffset"
      
      segmentDF$PPG1Extract[stimOnsetRow] <- "onsetRow"
      segmentDF$PPG1Extract[stimOffsetRow] <- "offsetRow"
      segmentDF$PPG1Extract[answerRow] <- "answerRow"
      
      segmentDF$PPG1Extract[poststimOnset] <- "poststimSegOnset"
      segmentDF$PPG1Extract[poststimOffset] <- "poststimSegOffset"
    }
    
    # if(chartName == "02A" && stimulusName == "R3") {
    #   assign("segmentDF", segmentDF, envir=.GlobalEnv)
    #   assign("extract.params", extract.params, envir=.GlobalEnv)
    #   stop()
    #   # View(segmentDF)
    # }
    
  }
  
  ###### get the PLE response measurement #######
  
  {
    
    # call PLEMeasurement() function from the PLEMeasurement.R script to get the measurement
    PLEList <- PLEMeasurementFn(dataVector=dataVector, stimOnsetRow=stimOnsetRow)
    
  }
  
  #### submit the prestim and poststim values to the segmentDF ####
  
  {
    
    # prestim
    segmentDF$PPG1Means[(prestimOnset:prestimOffset)] <- PLEList$prestimMeanAmp
    # poststim
    segmentDF$PPG1Means[(poststimOnset:poststimOffset)] <- PLEList$poststimMeanAmp
    
    # set the PLEMeasure to NA if there is no response constriction
    # if the ratio is < 1 or logged ratio < 0
    # if( !is.null(PLEList$prePostRatio) && PLEList$prePostRatio > 0 ) {
    if( !is.null(PLEList$prePostRatio) && !is.na(PLEList$prePostRatio) ) {
      
      segmentDF$PPG1Measure[(stimOnsetRow:stimOffsetRow)] <- PLEList$prePostRatio
      # segmentDF$PLEMeasure[(stimOnsetRow:stimOffsetRow)] <- PLEList$prePostRatio
      
      # save the prestim measurement in the PPG1 measurement column
      segmentDF$PPG1Measure[(prestimOnset:(stimOnsetRow-1))] <- PLEList$prestimMeanAmp
      
      # save the prestim measurement in a separate column
      # segmentDF$PPG1Prestim[(stimOnsetRow:stimOffsetRow)] <- PLEList$prestimMeanAmp
      
      
      
      # ifelse(PLEList$prePostRatio > 0,
      #        segmentDF$PPG1Measure[(stimOnsetRow:stimOffsetRow)] <- PLEList$prePostRatio,
      #        segmentDF$PPG1Measure[(stimOnsetRow:stimOffsetRow)] <- PLEList$prePostRatio
      #        # segmentDF$PPG1Measure[(stimOnsetRow:stimOffsetRow)] <- NA
      #        # segmentDF$PPG1Measure[(stimOnsetRow:stimOffsetRow)] <- .99
      #        # segmentDF$PPG1Measure[(stimOnsetRow:stimOffsetRow)] <- log(.99)
      # )
      
    }
    
  }
  
  #### output ####
  
  # check the result
  # segmentDF$PPG1Extract[segmentDF$PPG1Extract!=""]
  
  # this should retain a value of 0 when there is no reaction
  # and also when the data are not usable
  
  # print(PLEList$prestimMeanAmp)
  # print(PLEList$poststimMeanAmp)
  # print(PLEList$prePostRatio)
  # print(PLEList)
  
  return(segmentDF)

} # end newPLEExtractFn() 


