# R function to compute logged R/C scores from the measurements data frame
# Raymond Nelson
# 12-5-2017
#
####



{
  
  # source a function to select the CQ for each RQ
  source(paste0(RPath, 'selectCQ.R'), echo=FALSE)
  
  # source a function for the PLE R/C Ratio
  source(paste0(RPath, 'PLE_RC_Fn.R'), echo=FALSE)
  
  # a separate function for pneumo R/C Ratios
  source(paste0(RPath, 'pneumoRC_Fn.R'), echo=FALSE)
  
  # source the script fo the RC ratio constraints
  source(paste0(RPath, 'ESSScoreFromLogRC.R'), echo=FALSE)
  
}





RCScoresFn <- function(seriesMeasurementDF=RqCqDFSeries,
                       useMean=FALSE,
                       makeDF=FALSE,
                       saveCSV=FALSE,
                       output=FALSE,
                       assignOutputList=FALSE,
                       analysisListName="analysisResultList" ) {
  # R function to compute logged R/C scores from the measurements data frame
  # Raymond Nelson
  # 12-5-2017
  #
  ###
  #
  # input is a data frame of measurements 
  # for all stimulus questions in all charts for a series
  # use the seriesMeasurementDF not the RqCqSeriesDF
  # because we need to have all questions for the CQ selection
  # the CQ selector should not select over a Sy question or artifact
  #
  # useMean input parameter will force the CQmean for all R/C ratios
  # instead of the strong side CQ
  #
  # output=FALSE will supress the output list
  # leaving the output to the seriesMeasurmentDF
  #
  # visible output is the series measurement data frame 
  # including all RQs CQs other Qs and annotations
  # with the "RCScore" column populated with the R/C ratios for all RQs
  # and the CQName column populated with the selected CQ for each RQ
  # 
  # source(paste0(RPath, 'selectCQ.R'), echo=FALSE)
  # to load the selectCQFn function
  # based on the heuristic in Nelson (2017) 
  # "Heuristic Principles to Select Comparison and 
  # Relevant Question Pairs When Scoring Any CQT Format"
  #
  ####
  
  {
    
    print("calculate the R/C ratios")
    
    if(!exists("useMean")) useMean <- FALSE
    if(!exists("makeDF")) makeDF <- FALSE
    if(!exists("saveCSV")) saveCSV <- FALSE
    if(!exists("output")) output <- FALSE
    
    if(!exists("assignOutputList")) assignOutputList <- FALSE
    
    if(!exists("analysisListName")) analysisListName <- FALSE
    
    #### use the seriesMeasurementDF instead of the RqCqDFSeries ####
    # so that all events can be included in selection of the CQ
    
    examName <- seriesMeasurementDF$examName[1]
    seriesName <- seriesMeasurementDF$seriesName[1]
    
    assign("seriesMeasurementDF", seriesMeasurementDF, envir=.GlobalEnv)
    
    # reset the RCScore column
    seriesMeasurementDF$RCScore <- ""
    seriesMeasurementDF$CQName <- ""
    # View(seriesMeasurementDF)
    
    # if(seriesName == "4"){
    #   assign("seriesMeasurementDF", seriesMeasurementDF, envir=.GlobalEnv)
    #   stop()
    # }
    
    uniqueCharts <- unique(seriesMeasurementDF$chartName)
    
    # if(examName=="DCIVSL") {
    #   assign("seriesMeasurementDF", seriesMeasurementDF, envir=.GlobalEnv)
    #   stop()
    # }
    
  }
  
  #### remove repeated questions from the analysis ####
  
  {
    
    allEvents <- paste0(seriesMeasurementDF$chartName, seriesMeasurementDF$eventLabel)
    allEvents <- toupper((allEvents))
    
    # repeated events
    dupEvntsA <- which(str_sub(allEvents,  -1, -1) == "A")
    # seriesMeasurementDF$eventLabel[dupEvntsA]
    
    if(length(dupEvntsA) > 0) {
      # events that are repeated
      dupEvents <-
        which(allEvents %in%  str_sub(allEvents[dupEvntsA], 1, -2))
      
      renameRows <- sort(c(dupEvents, dupEvntsA))
      # seriesMeasurementDF$eventLabel[renameRows]
      
      # save the original event names
      # so they can be restored later in this function
      saveLabels <-  seriesMeasurementDF$Label[renameRows]
      saveELabels <- seriesMeasurementDF$eventLabel[renameRows]
      # saveEventLabels is already in use later
      
      # initialize a vector of replacement names without R and C characters
      tempLabels <- rep(paste0("Q", c(1:length(renameRows))))
      
      seriesMeasurementDF$Label[renameRows] <- tempLabels
      seriesMeasurementDF$eventLabel[renameRows] <- tempLabels
      
      # 
      
    }
    
    # View(seriesMeasurementDF)
    
    # need to restore the Label and eventLabel columns later
    # seriesMeasurementDF$Label[renameRows] <- saveLabels
    # seriesMeasurementDF$eventLabel[renameRows] <- saveELabels
    
  }
  
  #### slice the RQs and CQs ####
  
  {
    
    rqRowsSeries <- grep("R", seriesMeasurementDF$eventLabel)
    # exclude "SR" events 
    rqRowsSeries <- 
      rqRowsSeries[!(seriesMeasurementDF$eventLabel[rqRowsSeries] %in% c("SR", "RS"))]
    
    cqRowsSeries <- grep("C", seriesMeasurementDF$eventLabel)
    # exclude "CT" events
    cqRowsSeries <- 
      cqRowsSeries[seriesMeasurementDF$eventLabel[cqRowsSeries] != "CT"]
    cqRowsSeries <- 
      cqRowsSeries[seriesMeasurementDF$eventLabel[cqRowsSeries] != "C"]
    
    seriesRQs <- unique(seriesMeasurementDF$eventLabel[rqRowsSeries])
    seriesCQs <- unique(seriesMeasurementDF$eventLabel[cqRowsSeries])
    
    # increment the series if there not are at least 2 RQs and at least 2 CQs
    if( length(seriesRQs) < 2 || length(seriesCQs) < 2 ) {
      return(seriesMeasurementDF)
    }
    
  }
  
  #### identify the extant sensors ####
  
  {
    
    # initialize a vector of unique sensor names
    uniqueSensors <- as.character(unique(seriesMeasurementDF$sensorName))
    
    RCSensors <- c("UPneumo", 
                   "LPneumo", 
                   "Pneumo",
                   "AutoEDA", 
                   # "ManualEDA", 
                   "Cardio", 
                   # "FC",
                   "PLE")
    
    # increment the chart if ESS-M sensors are missing
    if(length(which(!(RCSensors %in% uniqueSensors))) > 3) {
      # the PLE sensor is optional, so there may be 1 missing sensor
      # FC sensor is for study and comparison with the traditional cardio sensor
      return(seriesMeasurementDF)
    }
    
    RCSensors <- RCSensors[RCSensors %in% uniqueSensors]
    
    uniqueQuestionsSeries <- unique(seriesMeasurementDF$eventLabel)
    
  }
  
  #### check for DLST DLDT and PCASS type charts ####
  
  {
    
    saveQuestionLabels <- RqCqDFSeries$Label
    saveEventLabels <- RqCqDFSeries$eventLabel
    
    saveChartNames <- rep(uniqueCharts[1], length=nrow(RqCqDFSeries))
    # RqCqDFSeries$chartName <- saveUniqueCharts[1]
    
    saveUniqueCharts <- uniqueCharts[1]
    
    # in case these are needed for replacement with DLST/DLDT formats
    saveUniqueQuestions <- unique(saveEventLabels)
    saveUniqueRQs <- 
      unique(saveUniqueQuestions[grep("R", saveUniqueQuestions)])
    saveUniqueCQs <- 
      unique(saveUniqueQuestions[grep("C", saveUniqueQuestions)])
    
    # DLST/DLDT/PCASS RQ labels 
    R1Labels <- c("1R1", "2R1", "3R1", "4R1")
    R2Labels <- c("1R2", "2R2", "3R2", "4R2")
    R3Labels <- c("1R3", "2R3", "3R3", "4R3")
    R4Labels <- c("1R4", "2R4", "3R4", "4R4")
    R5Labels <- c("1R5", "2R5", "3R5", "4R5")
    R6Labels <- c("1R6", "2R6", "3R6", "4R6")
    
    RQLabels <- c(R1Labels, R2Labels, R3Labels, R4Labels, R5Labels, R6Labels)
    
    # DLST/DLDT/PCASS CQ Labels
    C1Labels <- c("1C1", "2C1", "3C1", "4C1")
    C2Labels <- c("1C2", "2C2", "3C2", "4C2")
    C3Labels <- c("1C3", "2C3", "3C3", "4C3")
    C4Labels <- c("1C4", "2C4", "3C4", "4C4")
    C5Labels <- c("1C5", "2C5", "3C5", "4C5")
    C6Labels <- c("1C6", "2C6", "3C6", "4C6")
    
    CQLabels <- c(C1Labels, C2Labels, C3Labels, C4Labels, C5Labels, C6Labels)
    
    CQRQLabels <- c(CQLabels, RQLabels)
    
    if( length(which(saveEventLabels %in% CQRQLabels)) == 
        length(saveEventLabels) ) {
      DLSTType <- TRUE
    } else {
      DLSTType <- FALSE
    }
    
  }
  
  #### remove excluded events from the seriesMeasurementDF ####
  
  {
    
    # selectRows <-
    #   which(!(seriesMeasurementDF$Label %in% excludeEvents))
    
    # here we keep all events because the CQ selection needs to see them
    
    selectRows <- c(1:nrow(seriesMeasurementDF))
    
    # initialize another version of the series DF
    seriesMeasurementDFa <- seriesMeasurementDF[selectRows,]
    
    # use the seriesMeasurementDFa for calculations
    
  }
  
  #### iterate over the charts in this series ####
  
  i=1
  for(i in 1:length(uniqueCharts)) {
    
    ###### slice the chart ######
    
    {
      
      # chartRows <- which(RqCqDFSeries$chartName==uniqueCharts[i])
      chartRows <- which(seriesMeasurementDFa$chartName==uniqueCharts[i])
      
      # initiate the chart measurement data frame for the selectCQFn
      chartMeasurementDF <- seriesMeasurementDFa[chartRows,]
      # View(chartMeasurementDF)
      
      chartName <- chartMeasurementDF$chartName[1]
      
      # if(chartName == "05A") {
      #   assign("chartMeasurementDF", chartMeasurementDF, pos=1)
      #   assign("chartName", chartName, pos=1)
      #   stop() # <>
      # }
      
    }
    
    #### use the ipZ score to remove extreme values ######
    
    {
      
      # # make a copy of the chartMeasurementDF
      # # and use the Ipsative Z value
      # # to ignore RQs or CQ of more than 5 standard deviations
      # 
      # # make a copy of the sensor measurement to be restored later
      # saveSensorMeasurements <- chartMeasurementDF$sensorMeasurement
      # 
      # extremeValueRows <-
      #   (as.numeric(chartMeasurementDF$ipZScore) > 10)
      # 
      # # removed autoEDA from this 2-9-2021
      # theseSensorRows <-
      #   (chartMeasurementDF$sensorName %in% c("Cardio"))
      # 
      # fixTheseRows <-
      #   extremeValueRows[which(extremeValueRows & theseSensorRows)]
      # 
      # # chartMeasurementDFx$ipZScore[extremeValueRows] <- ""
      # if(length(fixTheseRows > 0)) {
      #   chartMeasurementDF$sensorMeasurement[extremeValueRows] <- NA
      # }
      
    }
    
    #### isolate the RQs and CQs ####
    
    {
      # initialize a vector of unique RQ and CQ events in the chart
      uniqueQuestionsChart <- unique(chartMeasurementDF$eventLabel)
      
      # increment the chart if less than 4 events in the chart
      if(length(uniqueQuestionsChart) < 4) {
        next()
      }
      
      rqRows <- grep("R", chartMeasurementDF$eventLabel)
      # exclude "SR" events 
      rqRows <- 
        rqRows[!(chartMeasurementDF$eventLabel[rqRows] %in% c("SR", "RS"))]
      
      cqRows <- grep("C", chartMeasurementDF$eventLabel)
      # exclude "CT" events
      cqRows <- 
        cqRows[chartMeasurementDF$eventLabel[cqRows] != "CT"]
      cqRows <- 
        cqRows[chartMeasurementDF$eventLabel[cqRows] != "C"]
      
      rqDF <- chartMeasurementDF[rqRows,]
      # View(rqDF)
      cqDF <- chartMeasurementDF[cqRows,]
      # View(cqDF)
      
      uniqueRQs <- unique(rqDF$eventLabel)
      uniqueCQs <- unique(cqDF$eventLabel)
      
      # increment the chart if there not are at least 2 RQs and at least 2 CQs
      if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) {
        next()
      }
      
      # assign("rqDF", rqDF, pos=1)
      # assign("cqDF", cqDF, pos=1)
    }
    
    #### check the stability of the respiration data ####
    
    # Sept 14, 2021 added
    # Sept 21, 2021 commented out for now 
    # while the feature extraction is improved
    #
    # check the dissimilarity in upper and lower respiration data
    # requires first auto-scaling the respiration data
    
    {
      
      # Sep 28, 2021 still working on this
      # UPMeasurements <- rqDF$sensorMeasurement[which(rqDF$sensorName=="UPneumo")]
      # LPMeasurements <- rqDF$sensorMeasurement[which(rqDF$sensorName=="LPneumo")]
      # 
      # # vectorized for all RQs
      # PRods <- UPMeasurements * LPMeasurements
      # 
      # PRatios <- UPMeasurements / LPMeasurements
      # 
      # PRatios[which(PProds <= 0)] <- 0
      # 
      # PRatios <- exp(abs(log(UPMeasurements / LPMeasurements)))
      # 
      # # PRatio <- exp(abs(log(UPMeasurement / LPMeasurement)))
      # # 
      # # if(PRatio >= 1.1) {
      # #   UPMeasurement <- "INS"
      # #   LPMeasurement <- "INS"
      # #   # INS signifies instability in the respiration data
      # # }
      
    }
    
    #### calculate the mean CQ for each sensor in the chart ####
    
    {
      # for use when each RQ is compared to the within chart mean CQ
      # initialize a vector to hold the CQ means
      cqMeans <- rep("", times=length(RCSensors))
      # then iterate over the sensors to compute the means
      for (m in 1:length(RCSensors)) {
        # may need to manage NA values in case there is no EDA measurement
        # may also need to manage small values
        # get the CQRow
        getRows <- which(cqDF$sensorName==RCSensors[m])
        # may need to remove small  CQ measurements here
        cqMeans[m] <- mean(cqDF$sensorMeasurement[getRows], na.rm=TRUE)
      }
      # coerce to numeric
      cqMeans <- as.numeric(cqMeans)
      names(cqMeans) <- RCSensors
      # print(cqMeans)
    }
    
    #### iterate on the RQs ####
    
    # requires the selectCQ function
    # source(paste0(RPath, 'selectCQ.R'), echo=FALSE)
    
    j=1
    for (j in 1:length(uniqueRQs)) {
      
      {
        # locate the rows for the RQ in the rqDF
        thisRQName <- uniqueRQs[j]
        selectRQRows <- rqDF$eventLabel==thisRQName
        # View(rqDF)
        
        # print(thisRQName)
      }
      
      #### iterate on the sensors ####
      k=1 # 6 is PLE
      for (k in 1:length(RCSensors)) {
        
        {
          
          #### get the RQ value ####
          
          # locate the rows for the sensor in the rqDF
          thisSensorName <- RCSensors[k]
          
          if(!(thisSensorName %in% uniqueSensors)) {
            next()
          }
          
          selectSensorRows <- rqDF$sensorName==thisSensorName
          
          # get the row index for the RQ and sensor
          theseRows <- which(selectRQRows & selectSensorRows)
          
          # stop if wrong sensor
          if(rqDF$sensorName[theseRows] != thisSensorName) { stop() }
          
          # initialize a scalar to hold the RQ Name
          RQName <- unique(rqDF$eventLabel[theseRows])
          
          # stop if the RQ name is incorrect
          if(thisRQName != RQName) { stop() }
          
          # initialize a scalar to hold the RQ Values
          RQValue <- rqDF$sensorMeasurement[theseRows]
          names(RQValue) <- thisSensorName
          
          # fix NA 
          if(is.na(RQValue)) { 
            # commented out 2-11-2021
            # RQValue <- 20 
            # NA values may occur for 2 reasons
            # 1. when the data descend through the ROW
            # 2. when the data ascend through the ROW
          }
          
          # Sep 26, 2021 
          # no RC score and no ESSM score if RQ is NA 
          
          # Nov 20, 2021
          if(is.na(RQValue) && thisSensorName == "PLE") {
            # April 16, 2022 restricted this to PLE so EDA and cardio remain NA
            # if(!is.na(RQName) && thisSensorName == "PLE") RQValue <- 0
            if(!is.na(RQName)) RQValue <- 0
            
            # rqDF$RCScore[theseRows] <- NA
            # rqDF$CQName[theseRows] <- RQValue
            # next() # next k sensor
          }
          
          # Feb 24, 2022 to prevent PLE values from scoring strangely
          # also see the change in the PLE section of selectCQ.R script on this date
          if(RQValue <= .001 && thisSensorName == "PLE") RQValue <- .001
          
        }

        {
          
          #### get the CQ value ####
          
          # make an input list for the selectCQFn
          CQSelectInputList <- list(thisRQName=thisRQName,
                                    RQValue=RQValue,
                                    chartMeasurementDF=chartMeasurementDF,
                                    thisSensorName=thisSensorName )
          
          # View(chartMeasurementDF)
          # assign("CQSelectInputList", CQSelectInputList, pos=1)
          
          ###### call the selectCQFn function to select the CQ ######
          
          # source a function to select the CQ for each RQ
          # source(paste0(RPath, 'selectCQ.R'), echo=FALSE)
          
          CQSelectOutputList <- selectCQFn(CQSelectInputList=CQSelectInputList)
          
          CQName <- CQSelectOutputList$CQName
          CQValue <- CQSelectOutputList$CQValue
          
          # fix NA values
          if(is.na(CQValue)) { 
            # Nov 20, 2021
            # 2 different conditions result in no CQ value
            # 1. there is no response at the CQ
            # 2. there is no CQ available 
            # we do not assign an RQ score if there is no CQ to compare with the RQ
            # we want to assign an RQ score if there is a CQ with no response
            # but only for PLE
            if(!is.na(CQName) && thisSensorName == "PLE") CQValue <- 0
            # if(!is.na(CQName)) CQValue <- 0
          }
          
        }
        
        #### use the CQ Means, depending on an input parameter ####
        
        if(isTRUE(useMean)) {
          CQValue <- cqMeans[k]
          CQName <- "mean"
        }
        
        #### increment if either RQ or CQ value is NA ####
        
        # if(is.na(RQValue) || is.na(CQValue)) {
        #   rqDF$RCScore[theseRows] <- NA
        #   rqDF$CQName[theseRows] <- NA
        #   next() # next k sensor
        # }
        
        # Sep 26, 2021
        # no RC score and no ESSM score if no CQ value 
        
        if(is.na(CQValue)) {
          rqDF$RCScore[theseRows] <- NA
          rqDF$CQName[theseRows] <- NA
          next() # next k sensor
        }
        
        #### replace small values ####
        
        # something vs nothing rules says that
        # something vs nothing is something 
        # so we score with the RQ or CQ if the other is missing 
        
        # also replace small values < 1% of the y axis
        # to avoid divide by zero errors and Inf values 
        
        # for EDA and cardio
        if( !(thisSensorName %in% c("PLE", "UPneumo", "LPneumo", "Pneumo")) ) {
          # if EDA and cardio measurement is < 0.5% of the Y axis
          # changed from .5% (10) to 0.0025% (5)  on June 21, 2023
          # changed back to <10 or 0.5% of the y-axis Oct 16, 2023
          if(RQValue < (.005 * yRange) && !is.na(RQValue)) {
            RQValue <- (.005 * yRange)
          }
          
          if(CQValue < (.005 * yRange) && !is.na(CQValue)) {
            CQValue <- (.005 * yRange)
          }
        }
        
        #### increment if either the RQ or CQ  value is 0 ####
        
        # if(RQValue == 0 || CQValue == 0) {
        #   rqDF$RCScore[theseRows] <- NA
        #   rqDF$CQName[theseRows] <- NA
        #   next() # next k sensor
        # }
        
        #### check for small response measurements ####
        
        # 2019-03-24
        if( !(thisSensorName %in% c("PLE", "UPneumo", "LPneumo", "Pneumo")) ) {
          # for Cardio and EDA sensors only
          # PLE and pneumo are not handled here because PLE responses can be small
          # also PLE feature extraction has its own constraints
          # and pneumo measurement values tend to be larger 
          # PLE and pneumo are not affected by this
          
          # increment if both RQ and CQ are < 1% of the y axis
          # or if the sum is less thann 2% of y axis
          if( !is.na(RQValue) && !is.na(CQValue)) {
            # modified these threshold values Jun 21, 2023
            if( (as.numeric(RQValue) < (.005 * yRange) && as.numeric(CQValue) < (.005 * yRange)) ||
                # Nov 14, 2023 changed to < 1% of yRange
                (as.numeric(RQValue) + as.numeric(CQValue) < (.01 * yRange) ) ) {
              # Oct 14 2023 use < 25 means that one of the values must be 20 (1% of y axis)
              # Oct 16, 2023 values < 10 are 0.5% of the y-axis
              # small values will give R/C = 0
              # if both RQ and CQ are small
              # and will give 0 unless the ratio exceeds 1.5:1
              rqDF$RCScore[theseRows] <- 0
              rqDF$CQName[theseRows] <- CQName
              next() # next k sensor
            }
          }
        } 
        
        ############## calculate the R/C ratio #############
        
        #### PLE ####
        
        # use the sensorName to handle the PLE ratio different than others
        if( thisSensorName == "PLE" ) {
          
          # because the R and C are the log(pre/post) ratio for PLE
          # log(R/C) ratios are handled differently 
          # than for other sensors
          # (for which the R and C are measurement or extraction vals)
          
          # PLE constraint at 10% difference
          # ignore RQ or RQ log(pre/post) ratios with insufficient difference
          
          # PLE constraint for pre/post ratios
          # PLEConstraint <- log(1.1) # 0.09531018
          # PLEConstraint <- log(1.10517) # 0.09999917
          # PLEConstraint <- log(1.051271) # 0.04999991
          # PLEConstraint <- log(1.05) # 0.04879016
          # PLEConstraint <- log(1.020201) # 0.01999967
          # PLEConstraint <- log(1.01005) # 0.009999835
          # an additional constraint exists for ESS-M Scores
          
          # PLEConstraint is now set in the workFlow_init.R script
          # This is the current value of the PLE constraint, and may change, 4/30/21
          # PLEConstraint <- log(1.1) # 0.09531018
          
          # source(paste0(RPath, 'PLE_RC_Fn.R'), echo=FALSE)
          
          RCScore <- PLE_RC_Fn(RQValue, CQValue, PLEConstraint)
          
        }
        
        #### respiration ####
        
        if( thisSensorName %in% c("UPneumo", "LPneumo") ) {
          
          # source(paste0(RPath, 'pneumoRC_Fn.R"), echo=FALSE)
          
          RCScore <- pneumoRC_Fn(RQValue=RQValue, 
                                 CQValue=CQValue,
                                 pneumoConstraintLow=pneumoConstraintLow,
                                 pneumoConstraintHigh=pneumoConstraintHigh )
          
        }
        
        #### combine the 2 respiration R/C scores to 1 ####
        
        if( thisSensorName == "Pneumo" ) {
          
          # combine the 2 respiration R/C scores to 1
          
          UPneumoRow <- theseRows - 2
          LPneumoRow <- theseRows - 1
          
          # Nov 20, 2021
          if(!is.na(rqDF$RCScore[UPneumoRow]) && !is.na(rqDF$RCScore[LPneumoRow])) {
            
            UPRC <- as.numeric(rqDF$RCScore[UPneumoRow])
            LPRC <- as.numeric(rqDF$RCScore[LPneumoRow])
            
            UCQ <- rqDF$CQName[UPneumoRow]
            LCQ <- rqDF$CQName[LPneumoRow]
            
            RCScore <- ifelse(UPRC * LPRC <= 0,
                              0,
                              c(UPRC, LPRC)[which.max(c(abs(UPRC), abs(LPRC)))] )
            
            CQName <- ifelse(UPRC * LPRC <= 0,
                             NA,
                             c(UCQ, LCQ)[which.max(c(abs(UPRC), abs(LPRC)))] )
            
          } else {
            
            RCScore <- NA
            
            CQName <- NA
            
          }
          
        }
        
        #### EDA and cardio ####
        
        if( !(thisSensorName %in% c("PLE", "UPneumo", "LPneumo")) ) {     
          
          # for all sensors other than the PLE and pneumos
          
          # use the logged ratio of RQ and CQ measurements
          # Nov 21, 2021
          if( !is.na(RQValue) && !is.na(CQValue) ) {
            # no EDA or cardio score if either RQ or CQ is NA
            RCScore <- -log(RQValue/CQValue) 
          } else {
            RCScore <- NA  
          }
          # sign values are inverted 
          # for EDA and Cardio in the next step
          # pneumo and PLE signs are correct at this point
          # + signs correspond to truthtelling
          # - signs correspond to deception
          # similar to traditional intuition
          # for manual polygraph scores
          
        }
        
        #### save the logRC ratio and CQ name ####
        
        {
          
          # save the RQ score
          rqDF$RCScore[theseRows] <- RCScore
          # save the CQ name 
          rqDF$CQName[theseRows] <- CQName
          # View(rqDF)
          
          # assign("rqDF", rqDF, envir=.GlobalEnv)
          # if(k == 1 && j == 3) stop()
          
        }
        
      } # end for loop for k unique sensors
      
    } # end for loop for j unique RQs
    
    #### invert the logged EDA, Cardio, and PLE scores ####
    
    {
      # oct 4, 2021 commented out because already done
      
      # # so that + log scores correspond to truth-telling
      # # and - log scores correspond to deception
      # 
      # EDARows <- which(rqDF$sensorName=="AutoEDA" | 
      #                    rqDF$sensorName=="ManualEDA")
      # rqDF$RCScore[EDARows] <- -as.numeric(rqDF$RCScore[EDARows])
      # 
      # CardioRows <- which(rqDF$sensorName=="Cardio")
      # rqDF$RCScore[CardioRows] <- -as.numeric(rqDF$RCScore[CardioRows])
      # 
      # # PLERows <- which(rqDF$sensorName=="PLE")
      # # rqDF$RCScore[PLERows] <- -as.numeric(rqDF$RCScore[PLERows])
    }
    
    #### pass the rqDF back to the chart measurement DF ####
    
    chartMeasurementDF[rqRows,] <- rqDF
    
    # assign("rqDF", rqDF, envir=.GlobalEnv)
    # stop()
    
    #### restore the sensor measurements ####
    
    # chartMeasurementDF$sensorMeasurement <- saveSensorMeasurements
    
    #### pass the chartmeasurementDF back to seriesMeasurementDFa ####
    
    seriesMeasurementDFa[chartRows,] <- chartMeasurementDF
    
  } # end loop i over uniqueCharts
  
  #### restore excluded events to seriesMeasurementDF ####
  
  {
    
    # this will transfer the new info
    # to the data frame that may included some excluded events
    
    seriesMeasurementDF[selectRows,] <- seriesMeasurementDFa
    
  }
  
  #### restore the labels with repeated questions ####
  
  if(length(dupEvntsA) > 0) {
    seriesMeasurementDF$Label[renameRows] <- saveLabels
    seriesMeasurementDF$eventLabel[renameRows] <- saveELabels
    # not saveEventLabels
    # View(seriesMeasurementDFa)
  }
  
  ################# output section  ####################
  
  if(isTRUE(output)) {
    
    ##### initialize a data frame with the question sequence for each chart ####
    
    questionSequenceDF <- questionSequenceFn(measurementDF=RqCqDFSeries,
                                             outputName="RCQuestionSequence",
                                             makeDF=FALSE,
                                             saveCSV=FALSE)
    
    ##### initialize the RqCqDFSeries #####
    
    {
      RqCqSeriesRows <- grep("[CR]+", seriesMeasurementDF$eventLabel)
      if(length(RqCqSeriesRows) != 0) {
        # exclude sacrifice relevant questions
        SRRows <- grep("SR", seriesMeasurementDF$eventLabel)
        SRRows <- c(SRRows, grep("RS", seriesMeasurementDF$eventLabel))
        # make a vector of working rows for CQs and RQs
        RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% SRRows)]
        # exclude "CT" (cleared throat) annotations
        CTRows <- grep("CT", seriesMeasurementDF$eventLabel)
        RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% CTRows)]
      }    
      RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
    }
    
    # RC scores are logged ratios
    # for RQs only 
    # no aggregation
    # no decision rules
    
    RCSensors2 <- c("UPneumo", 
                    "LPneumo", 
                    "AutoEDA", 
                    "Cardio", 
                    "PLE")
    
    RCSensors2 <- 
      RCSensors2[RCSensors2 %in% seriesMeasurementDF$sensorName]
    
    RCScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                   useSensors=RCSensors2,
                                   scoreType="RCScore",
                                   decimals=2,
                                   DLSTType=DLSTType,
                                   outputName="RCScoresheetDF",
                                   makeDF=FALSE,
                                   saveCSV=FALSE)
    # View(scoreSheetDF)
    
    CQSelectionDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                  useSensors=RCSensors2,
                                  scoreType="CQName",
                                  decimals="non-numeric",
                                  outputName="CQSelectionDF",
                                  makeDF=FALSE,
                                  saveCSV=FALSE)
    
    measurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=RCSensors2,
                         decimals=2,
                         makeDF=FALSE,
                         saveCSV=FALSE )
    
    ## construct a list to hold the rank score results ####
    
    outputListName <- paste(examName, seriesName, "RCOutputList", sep="_")
    
    RCOutputList <- list(RQCQRatios="R/C Ratios -  used by ESS/ESS-M, (Nelson, Krapohl & Handler 2008; Nelson 2017)",
                         examName=examName,
                         seriesName=seriesName,
                         RCQuestions=uniqueQuestionsSeries,
                         RCSensors=RCSensors2,
                         RCQuestionSequence=questionSequenceDF,
                         RCQSelectionTable=CQSelectionDF,
                         RCScoreSheetDF=RCScoreSheetDF,
                         RCMeasurementsDF=measurementsDF )
    
  }
  
  #### save the list to the global env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, RCOutputList, env=.GlobalEnv)
  
  if(!exists("assignOutputList")) assignOutputList <- FALSE
  
  if(isTRUE(assignOutputList)) {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "RCRatioOutput"
    analysisResultList[[seriesListName]][[outputListName]] <-
      RCOutputList
    if(assignOutputList) {
      assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    }
    
  }
  
  #### save it ####
  
  outputDFNameRCScores <- paste(examName, seriesName, "RCScoreDF", sep="_")
  
  outputDFNameCQSelection <- paste(examName, seriesName, "CQSelectionDF", sep="_")
  
  if(isTRUE(makeDF)) {
    assign(outputDFNameRCScores, RCScoreSheetDF, pos=1)
    assign(outputDFNameCQSelection, CQSelectionDF, pos=1)
  }
  
  if(isTRUE(saveCSV)) {
    write.csv(RCScoreSheetDF,
              file=paste0(str_sub(outputDFNameRCScores, 1, -3), ".csv"),
              row.names=FALSE)
    write.csv(CQSelectionDF,
              file=paste0(str_sub(outputDFNameCQSelection, 1, -3), ".csv"),
              row.names=FALSE)
  }
  
  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  return(seriesMeasurementDF)
  
  # end RCScoresFn
} 



