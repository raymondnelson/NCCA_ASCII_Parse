

# source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)



RCScoresFn <- function(x=RqCqDF) {
  # R function to compute the R/C scores from the measurements data frame
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  # called by the getScoresFn in the score.R script
  #
  # x input is a data frame of measurements for the RQs and CQs for a test chart
  # output is the RqCqDF data frame with the score column populated 
  # 
  # R/C scores are calculated using a single CQ for each RQ
  # requires the selection of the correct CQ for each RQ
  # CQ selection is determined by the polygraph technique
  #
  # use a function to select the CQ for each RQ
  # source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)
  # to load the selectCQFn function
  # based on the heuristic in Nelson (2017) 
  # "Heuristic Principles to Select Comparison and 
  # Relevant Question Pairs When Scoring Any CQT Format"
  #
  ##################
  
  RqCqDF <- x
  
  # initialize a vector of unique RQ and CQ events
  uniqueEvents <- unique(RqCqDF$eventLabel)
  
  # initialize a vector of unique sensor names
  uniqueSensors <- as.character(unique(RqCqDF$sensorName))
  
  # initialize a vector of unique charts
  # uniqueCharts <- paste(RqCqDF$seriesName, RqCqDF$chartName, sep="_")
  
  if(length(uniqueEvents) != 0) {
    
    if(nrow(RqCqDF) > 1) {
      
      # make a data frame of RQs for this chart
      # rqRows <- grep("R", chartMeasurementDF$eventLabel)
      rqRows <- grep("R", RqCqDF$eventLabel)
      # use the RqCqDF to avoid capturing annotations or other events
      rqDF <- RqCqDF[rqRows,]
      # View(rqDF)
      
      # and another data frame of CQs for this chart
      # cqRows <- grep("C", chartMeasurementDF$eventLabel)
      cqRows <- grep("C", RqCqDF$eventLabel)
      # use the RqCqDF to avoid capturing annotations or other events
      cqDF <- RqCqDF[cqRows,]
      # View(cqDF)
      
      
      
      #### added 2-11-2018 to remove scores for tiny responses
      # commented out 1/27/2019 because it causes more problems than it solves
      # checkSensors <- c("AutoEDA", "ManualEDA", "Cardio")
      # checkPneumos <- c("UPneumo", "LPneumo", "Pneumo")
      # checkRows <- which(rqDF$sensorName %in% checkSensors)
      # NARows <- checkRows[rqDF$sensorMeasurement[checkRows] < 30]
      # rqDF$sensorMeasurement[NARows] <- NA
      # checkRows <- which(rqDF$sensorName %in% checkPneumos)
      # NARows <- checkRows[rqDF$sensorMeasurement[checkRows] < 15]
      # rqDF$sensorMeasurement[NARows] <- NA
      # checkRows <- which(cqDF$sensorName %in% checkSensors)
      # NARows <- checkRows[cqDF$sensorMeasurement[checkRows] < 30]
      # cqDF$sensorMeasurement[NARows] <- NA
      # checkRows <- which(cqDF$sensorName %in% checkPneumos)
      # NARows <- checkRows[cqDF$sensorMeasurement[checkRows] < 15]
      # cqDF$sensorMeasurement[NARows] <- NA
      
      
      
      # make a vector of unique RQs
      uniqueRQs <- unique(rqDF$eventLabel)
      
      # and a vector of unique CQs
      uniqueCQs <- unique(cqDF$eventLabel)
      
      assign("rqDF", rqDF, pos=1)
      assign("cqDF", cqDF, pos=1)
      # stop()
      
      # proceed to calculate R/C scores only if there are at least 2 RQs and at least 2 CQs
      if( length(uniqueRQs) >= 2 & length(uniqueCQs) >= 2 ) {
        
        ### calculate the mean CQ measurement for each sensor
        # initialize a vector to hold the CQ means
        cqMeans <- rep("", times=length(uniqueSensors))
        # then iterate over the sensors to compute the means
        for (i in 1:length(uniqueSensors)) {
          ### need to manage NA values in case there is no EDA measurement for a stim event
          ### also need to manage small values
          # get the CQRow
          getRows <- which(cqDF$sensorName==uniqueSensors[i])
          cqMeans[i] <- mean(cqDF$sensorMeasurement[getRows], na.rm=TRUE)
        }
        cqMeans <- as.numeric((cqMeans))
        names(cqMeans) <- uniqueSensors
        # print(cqMeans)
        
        # make a vector of all unique events in the chartMeasurementDF
        # from the first RQ or CQ to the last RQ or CQ including all repeated stimulus events
        # and also INCLUDING all non stimulus events (i.e., annotations and instructions)
        # this will be used to select the correct CQ for each RQ
        # use the chartMeasurementDF instead of the RqCqDF
        # because we need all events not limited to CQs and RQs
        # first make a vector of indices from the first RQ or CQ to the last RQ or CQ
        getRows <- c(RqCqRows[1]:RqCqRows[length(RqCqRows)])
        # then use the getRows vector to select all events from the chartMeasurementsDF
        chartMeasurementEvents <- unique(chartMeasurementDF$eventLabel[getRows])
        
        # source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)
        
        # loop over the unique RQs in this chart and calculate the logged RQ/CQ ratio
        i=2
        for (i in 1:length(uniqueRQs)) {
          
          # and also loop over the unique sensors
          
          # locate the rows for the RQ in the rqDF
          selectRQRows <- rqDF$eventLabel==uniqueRQs[i]
          
          j=14
          for (j in 1:length(uniqueSensors)) {
            
            # # commented out 3/29/2018
            # # locate the rows for the RQ in the rqDF
            # selectRQRows <- rqDF$eventLabel==uniqueRQs[i]
            
            # locate the rows for the sensor in the rqDF
            sensorName <- uniqueSensors[j]
            selectSensorRows <- rqDF$sensorName==sensorName
            
            # get the row index for the RQ and sensor
            RQRowSelect <- which(selectRQRows & selectSensorRows)
            
            # initialize a scalar to hold the RQ Name
            RQName <- rqDF$eventLabel[RQRowSelect]
            
            # initialize a scalar to hold the RQ Value
            RQValue <- rqDF$sensorMeasurement[RQRowSelect]
            names(RQValue) <- sensorName
            
            if( !is.na(RQValue) & as.numeric(RQValue) != 0 ) {
              
              # make an input list for the selectCQFn
              CQSelectInputList <- list(RQ=RQName,
                                        DAT=chartMeasurementDF,
                                        events=chartMeasurementEvents,
                                        sensor=sensorName)
              # assign("CQSelectInputList", CQSelectInputList, pos=1)
              
              # source('~/Dropbox/R/NCCA_ASCII_Parse/selectCQ.R', echo=FALSE)
              
              # call the selectCQFn function to select the CQ
              CQSelectOutputList <- selectCQFn(inputList=CQSelectInputList)
              
              # get the values of the output list
              CQName <- CQSelectOutputList$CQName
              # CQName <- strsplit(CQName, " ")[[1]][1]
              
              CQValue <- CQSelectOutputList$CQValue
              
              # need to split the CQValue string and convert to numeric
              if(!is.na(CQValue)) {
                if(length(CQValue) != 0) {
                  # supress warnings temporarily
                  oldw <- getOption("warn")
                  options(warn = -1)
                  CQValue <- as.numeric( strsplit(CQValue, " ")[[1]][1] )
                  # restore warnings
                  options(warn = oldw)
                }
              }
              
              # if(CQValue == 0) CQValue <- NA
              # if(RQValue == 0) RQValue <- NA
              
              ### CQ Means
              # commented out 1-25-2017
              # instead we use a function to select a CQ for each RQ
              # initialize a scalar to abstract the CQ Value to a scalar
              # 3-28-2017 un-comment these lines to use the mean CQ
              # CQValue <- cqMeans[j]
              # manage the condition where the CQmean==0
              # if(cqMeans[j] == 0) RQScore <- NA
              # if(cqMeans[j] == 0) rqDF$score[rqDF$sensorName==uniqueSensors[j]] <- NA
              
              # new version 2-18-2017
              if( all(length(RQValue) != 0, 
                      length(CQValue) != 0, 
                      !is.na(CQValue), 
                      CQValue != 0) ) {
                
                # check the logged RQ and CQ values for negatives
                if(sensorName == "PLE") { 
                  if(RQValue<0) {RQValue <- NA}
                  if(CQValue<0) {CQValue <- NA} 
                }
                
                # use the sensorName to handle the PLE ratio different than others
                RQScore <- ifelse(sensorName == "PLE",
                                  # use the extant logged ratio if one PLE score is missing
                                  ifelse(is.na(RQValue) || RQValue<log(1.1),
                                         # invert the sign of the logged CQ Value
                                         ifelse(is.na(CQValue) || CQValue<log(1.1) ,NA,-CQValue),
                                         ifelse(is.na(CQValue) || CQValue<log(1.1),
                                                # don't invert the sign of the logged RQ Value
                                                ifelse(is.na(RQValue) || RQValue<log(1.1),NA,RQValue),
                                                # invert the sign here
                                                # log(((RQValue)^2/(CQValue)^2)^.25)
                                                # same result using this formula
                                                log( RQValue^.5 / CQValue^.5 )
                                                # log(.28^.5 / .01^.5)
                                                # log(((.08)^2/(.02)^2)^.25)
                                         ) 
                                  ),
                                  # for all sensors other than the PLE
                                  log(RQValue/CQValue) )
                
              } else RQScore <- NA
              # RQScore is NA if there is no CQValue
              
            } else {
              # if the RQValue is NA
              RQScore <- NA
            } # end if else for !is.na(RQValue) & as.numeric(RQValue) != 0
            
            # and add the RQ Score to the rqDF
            rqDF$score[RQRowSelect] <- RQScore
            
          } # end for loop for j unique sensors
          
        } # end for loop for i unique RQs
        
        #### may need to further work the PLE scores
        
        # invert the EDA and Cardio so the + log scores correspond to truth-telling
        # EDACardioRows <- which(rqDF$sensorName=="EDA" | rqDF$sensorName =="Cardio")
        # rqDF$score[EDACardioRows] <- -as.numeric(rqDF$score[EDACardioRows])
        
        EDARows <- which(rqDF$sensorName=="EDA")
        rqDF$score[EDARows] <- -as.numeric(rqDF$score[EDARows])
        
        # keep NaN values as NA
        rqDF[is.na(as.numeric(rqDF$score)),"score"] <- NA
        
        # pass the rqDF back to the working DF
        # chartMeasurementDF[rqRows,] <- rqDF
        RqCqDF[rqRows,] <- rqDF
        
      } # end if for 2 RQs and 2 CQs
      
    } # end if nrow(RqCqDF) > 1
    
  } # end if for length(uniqueEvents) != 0
  
  # View(chartMeasurementDF)
  
  return(RqCqDF)
  
} # end RCScoresFn


