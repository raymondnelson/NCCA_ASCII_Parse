# R function to compute the ESS-M scores from the RC score in the measurement DF
# Raymond Nelson
#
####




{
  
  # ESS-M requires the R/C ratios from the the function in this script
  source(paste0(RPath, 'RCScores.R'), echo=FALSE)
  
  # source the script for the R/C ratio constraints
  # requires the RCToESSFn()
  # source(paste0(RPath, 'ESSScoreFromLogRC.R'), echo=FALSE)
  # sourced by the RCScores.R script
  
  # ESS-M also requires the function in this script to select the CQ
  # source(paste0(RPath, 'selectCQ.R'), echo=FALSE)
  # sourced by the RCScores.R script
  
  # function to autoselect the ESS-M decision rule via pairwise comparison
  source(paste0(RPath, 'autoSelectTSRSSR.R'), echo=FALSE)
  
  # ESS Multinomial Likelihood Function
  ESSM_simple_GT <- read.csv(paste0(RPath, 'ESSM_simple_GT.csv'), header=TRUE, stringsAsFactors=FALSE)
  ESSM_simple_ST <- read.csv(paste0(RPath, 'ESSM_simple_ST.csv'), header=TRUE, stringsAsFactors=FALSE)
  
  # ESSM (simple) cutscore tables
  ESSMs_SI_NDI <- read.csv(paste0(RPath, 'ESSMs_SI_NDI.csv'), stringsAsFactors=FALSE)
  ESSMs_SI_DI <- read.csv(paste0(RPath, 'ESSMs_SI_DI.csv'), stringsAsFactors=FALSE)
  ESSMs_SI_DI_St <- read.csv(paste0(RPath, 'ESSMs_SI_DI_St.csv'), stringsAsFactors=FALSE)
  ESSMs_MI_NSR <- read.csv(paste0(RPath, 'ESSMs_MI_NSR.csv'), stringsAsFactors=FALSE)
  ESSMs_MI_SR <- read.csv(paste0(RPath, 'ESSMs_MI_SR.csv'), stringsAsFactors=FALSE)
  
  # function to get the lower limit of the Bayesiann credible interval
  source(paste0(RPath, 'ClopperPearsonBinomialCI.R'), echo=FALSE)
  
  ## these are set in the NCCAASCII_init.R script ##
  # RqCqDFSeries=RqCqDFSeries,
  # ESSMDecisionRule=ESSMDecisionRule,
  # makeDF=makeDF, 
  # saveCSV=saveCSV,
  # makeDF=makeDF,
  # saveCSV=saveCSV
  
  # functions to output the score sheets
  # source("~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R", echo=TRUE)
  # already loaded in the newScore.R script
  
}



fixDLSTDLDTFn <- function(saveQuestionLabels=saveQuestionLabels, 
                          CQRQLabels=CQRQLabels) {
  # a helper function to fix the DLST DLDT and PCASS question labels
  # saveQuestionLabels is a vector with the original question labels
  # RQLabels is a vector of DLST/DLDT/PCASS type RQ labels
  # output is a vector of corrected question labels
  # of the same length to replace the original question labels
  newQuestionLabels <- saveQuestionLabels
  fixThese <- which(newQuestionLabels %in% CQRQLabels)
  if(length(fixThese)==0) return(saveQuestionLabels)
  newQuestionLabels[fixThese] <- 
    # requires the stringr package
    str_sub(newQuestionLabels[fixThese], 2, -1)
  
  return(newQuestionLabels)
}



############  main function  ################################



ESSMScoresFn <- function(RqCqDFSeries=RqCqDFSeries,
                         ESSMDecisionRule=ESSMDecisionRule,
                         essmPrior=.5,
                         essmAlphas=c(aD=.05, aT=.05),
                         # ESSMCutScores=ESSMCutScores,
                         makeDF=FALSE,
                         saveCSV=FALSE,
                         analysisListName="analysisResultList" ) {
  # R function to compute the ESS scores from the measurements data frame
  # Raymond Nelson
  # 11-27-2017
  # called by the getScoresFn() in the scores.R script
  #
  ###
  #
  # x input is a data frame of measurements for the RQs and CQs for a series
  # including all charts in the series
  # output is the RqCqDF data frame with the ESSScore column populated 
  # with ESS scores
  #
  ####
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    # if(seriesName == "4"){
    #   assign("RqCqSeriesDF", RqCqDFSeries, envir=.GlobalEnv)
    #   stop()
    # }
    
    print("calculate the ESS-M scores")
    
    if(!exists("ESSMDecisionRule")) ESSMDecisionRule <- "eTSR"
    if(!exists("ESSMPrior")) ESSMPrior <- .05
    if(!exists("ESSMAlphas")) ESSMAlphas <- c(aD=.05, aT=.05)
    
    if(!exists("makeDF")) makeDF <- FALSE
    if(!exists("saveCSV")) saveCSV <- FALSE
    
    if(!exists("analysisListName")) analysisListName="analysisResultList"
    
    # View(RqCqDFSeries)
    # assign("RqCqDFSeries", RqCqDFSeries, envir=.GlobalEnv)
    # stop()
    
    # this is set in the NCCAAASCII_init.R script
    if(!exists("ESSMIncludePLE")) ESSMIncludePLE <- FALSE
    
    # initialize the default warning
    ESSMWarning <- "none"
    
  }
  
  #### constraints for ESS-M R/C ratios ####

  # now included in 
  # source(paste0(RPath, 'ESSScoreFromLogRC.R'), echo=FALSE)

  #### initialize a data frame with the question sequence for each chart ####
  
  {
    # includes only CQs and RQs
    
    questionSequenceDF <- questionSequenceFn(measurementDF=RqCqDFSeries,
                                             outputName="ESSMQuestionSequence",
                                             makeDF=FALSE,
                                             saveCSV=FALSE)
    # this data frame is included in the output 
    # and shows the question order for each chart
    # ESS and OSS-2 scores are influenced to some degree by question order
    
  }
  
  ############ initial ESS-M set up #############
  
  {
    
    RqCqDFSeries$ESSScore <- ""
    
    uniqueSensors <- unique(as.character(RqCqDFSeries$sensorName))
    
    ESSMSensors <- c("UPneumo", 
                     "LPneumo", 
                     "AutoEDA", 
                     "ManualEDA",
                     "Cardio", 
                     "FC",
                     "PLE")
    
    # ESSSensors <- c("UPneumo", "LPneumo", "AutoEDA", "Cardio")
    
    # keep only the extant sensors
    ESSMSensors <- ESSMSensors[ESSMSensors %in% uniqueSensors]
    
    # exclude the PLE
    # includePLEScores is set
    if("PLE" %in% ESSMSensors && !isTRUE(includePLEScores)) {
      ESSMSensors <- ESSMSensors[-which(ESSMSensors %in% "PLE")]
    }
    
    # exit if ESSM Sensors are missing
    if(length(ESSMSensors) < 2) return(RqCqDFSeries)
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) breturn(RqCqDFSeries) 
    
    # unique RQs and CQs for the series
    uniqueRQs <- 
      unique(RqCqDFSeries$eventLabel[grep("R", RqCqDFSeries$eventLabel)])
    uniqueCQs <- 
      unique(RqCqDFSeries$eventLabel[grep("C", RqCqDFSeries$eventLabel)])
    
    # in case these are needed for replacement with DLST/DLDT formats
    saveUniqueRQs <- uniqueRQs
    saveUniqueCQs <- uniqueCQs
    saveUniqueQuestions <- uniqueQuestions
    
    # exit if not at least 2RQs and 2 CQs
    if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) {
      return(RqCqDFSeries) 
    }
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
    # if( length(uniqueCharts) < 3 ) {
    #   print("ESS-M reference distributions are for 3 to 5 charts")
    #   return(RqCqDFSeries)
    # }

  }
  
  ########### calculate the logged R/C ratios here #############
  
  {
    ## RCScoresFn()  is called separately ##
    ## so that the seriesMeasurementDF can be used ##
    ## instead of the RqCqDFSeries ##
    ## so that the selectCQFn() can see all Qs and annotations ##
    ## in addition to the RQs and CQs ##
  }
  
  ##### check for DLST DLDT and PCASS type charts #####
  
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
  
  ########## get the exam data and slice the series data ##########
  
  {
    # to check the RBPF and respiration rate
    examData <- get(paste0(examName, "_Data"), envir=.GlobalEnv)
    
    # RqCqSeriesDF will contain only this series
    seriesData <-
      examData[which(examData$seriesName==RqCqDFSeries$seriesName[1]),]
  }
  
  ############### iterate over the ESS-M charts ###############
  
  i=1
  for (i in 1:length(uniqueCharts)) {
    
    ##### first get the RqCq data frame for this chart #####
    
    {
      
      thisChart <- uniqueCharts[i]
      thisChartRows <- which(RqCqDFSeries$chartName == thisChart)
      
      # ESS-M requires the R/C ratios from the the function in this script
      # source(paste0(RPath, 'RCScores.R'), echo=FALSE)
      
      RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      
      # increment the loop to the next chart if no events
      if(nrow(RqCqDFChart) < 2) { 
        next()
      }
      
    }
    
    ##### check the ratio of upper and lower respiration rate #####
    
    
    # use the seriesData to check the respiration rate
    
    
    ####### check the RPBF rate and adjust the constraints #######
    
    {
      # # chartData <- seriesData[which(seriesData$chartName==thisChart),]
      # 
      # rbpfRate <- rbpfProbFn(x=seriesData[which(seriesData$chartName==thisChart),])
      # 
      # if(rbpfRate != "unusual respiration") {
      # 
      #   rbpfRate <- as.numeric(str_sub(rbpfRate, -3, -1))
      #   print(paste("RBPF rate: ", rbpfRate))
      #     # adjust the min log(R/C) ratio if RBPF > .9
      #     if(rbpfRate >= .9) {
      #       posCardioLow <- log(1.284) # 0.2499802
      #       negCardioLow <- -log(1.284) # -0.2499802
      #     } else {
      #       posCardioLow <- log(1.10517) # 0.09999917
      #       negCardioLow <- -log(1.10517) # -0.09999917
      #     }
      #   }
    }
    
    ##### check for extreme values using the ipZScore #####
    
    {
      # View(RqCqDFChart)
      
      # RqCqDFChart$ipZScore
      # EDA and cardio 
      
      # make no ESS score if the ipZScore is NA or missing
    }
    
    ######### get the data for the RQs and CQs #########
    
    {
      
      rqRows <- grep("R", RqCqDFChart$eventLabel)
      cqRows <- grep("C", RqCqDFChart$eventLabel)
      
      # exit if there are less than 2 RQs or less than 2 CQs
      if( length(rqRows) < 2 || length(cqRows) < 2 ) { 
        next() 
      }
      
      # get the RQs and CQs for this chart
      uniqueRQsChart <- unique(RqCqDFChart[rqRows,'eventLabel'])
      uniqueCQsChart <- unique(RqCqDFChart[cqRows,'eventLabel'])
      
      if(length(uniqueRQsChart) < 2 || length(uniqueCQsChart) < 2) {
        next()
      }
      
      rqDF <- RqCqDFChart[rqRows,]
      # View(rqDF)
      
      # we don't need a cqDF
      # cqDF <- RqCqDFChart[cqRows,] # not used for anything
      # View(cqDF)
      
      # assign("rqDF", rqDF, pos=1)
      # assign("cqDF", cqDF, pos=1)
      
      # set the combined pneumo measurement to ""
      # pneumoRows <- which(cqDF$sensorName=="Pneumo")
      # cqDF$sensorMeasurement[pneumoRows] <- NULL
      # RqCqDFChart[cqRows,] <- cqDF
      
    }
    
    ##### RC scores were already calculated for each chart  #####
    
    {
      
      # set all integer score rows to NA if the R/C score is NA
      # rqDF[is.na(as.numeric(rqDF$RCScore)),'ESSScore'] <- NA
      # View(rqDF)
      
      # select rows for which the R/C score and integer score are not NA
      # theseRows <- !is.na(as.numeric(rqDF$RCScore))
      # selectRows <- which(theseRows & sensorRows)
      
      # initialize the ESSScore vector to NA to avoid errors if there is no score
      ESSScore <- NA
      
      ######  get the ESSM sensor rows for this chart #######
      
      # sensorRows <- rqDF$sensorName %in% ESSMSensors
      selectRows <- which(rqDF$sensorName %in% ESSMSensors)
      
      # rqDF$sensorName[selectRows]
      
    }
    
    ######## iterate on the rqDF sensor RC scores to get the ESS-M score ########
    
    j=1 # 6 is the PLE # 13 is the upper neumo 
    for (j in 1:length(selectRows)) {
      
      #### get the logRC score and sensor name ####
      
      {
        
        # first get the stimulus name, sensor, and score
        thisStimulusName <- rqDF$eventLabel[selectRows[j]]
        
        # View(rqDF)
        
        # rqDF$sensorName[selectRows]
        
        thisSensor <- rqDF$sensorName[selectRows[j]]
        
        # use the ipZScore to check for extreme values
        # if( is.na(rqDF$ipZScore[selectRows[j]]) ||
        #          rqDF$ipZScore[selectRows[j]] == "" ) {
        #   rqDF$ESSScore[selectRows[j]] <- NA
        #   next()
        # }
        
      }
      
      {
        
        # get the logRC Score
        thisScore <- as.numeric(rqDF$RCScore[selectRows][j])
        
        # thisScore <- as.numeric(thisScore)
        
        # print the info to the console
        # print(paste(thisStimulusName, thisSensor, thisScore))
        # rqDF$sensorMeasurement[selectRows[j]]
        
        # September 24, 2021
        # keep missing pneumo and PLE scores as 0 so the two resp sensors can be combined
        if(any(is.na(thisScore), is.null(thisScore), thisScore=="")) {
          if(thisSensor %in% c("Pneumo", "UPneumo", "LPneumo", "PLE")) {
            thisScore <- 0
          }
        }
      
        if(any(is.na(thisScore), is.null(thisScore), thisScore=="")) {
          # increment the sensor if no R/C score
          rqDF$ESSScore[selectRows[j]] <- NA
          next() # next j sensor row
        }
        
      }
      
      #### then compute the ESS scores ####
      
      # source(paste0(RPath, 'ESSScoreFromLogRC.R'), echo=FALSE)
      
      ESSScore <- RCToESSFn(thisScore=thisScore, 
                            thisSensor=thisSensor)
      
      #### set the ESSM Score to NA for excluded sensors ####
      
      {
        
        if(any(thisSensor == "UPneumo",
               thisSensor == "LPneumo",
               thisSensor == "Pneumo") && !isTRUE(extractPneumo)) {
          ESSScore <- NA
        }
        
        if(thisSensor == "AutoEDA" && !isTRUE(extractEDA)) {
          ESSScore <- NA
        }
        
        if(thisSensor == "Cardio" && !isTRUE(extractCardio)) {
          ESSScore <- NA
        }
        
        if(thisSensor == "PPG" && !isTRUE(extractPLE)) {
          ESSScore <- NA
        }
        
      }
      
      #### assign the ESS sensor integer score to the rqDF data frame ####
      
      # use ifelse to include or exclude the PLE
      rqDF$ESSScore[selectRows][j] <- ifelse(thisSensor != "PLE",
                                             ESSScore,
                                             ifelse(isTRUE(includePLEData),
                                                    ESSScore,
                                                    NA ) )
      
      # print(paste("integer score", ESSScore))
      # View(rqDF)
      
    } # end loop j over sensor rows
    
    # at this point we have the ESSM integer scores in the rqDF
    
    #### iterate on the RQs to combine the upper and lower pneumo scores ####
    
    k=1
    # iterate over the RQs 
    for (k in 1:length(uniqueRQsChart)) {
      
      # Nov 19, 2022 added if constraint for PCAT/LXCAT exams
      if(!PCATFormat) {
        # get the P1 and P2 values
        
        UPneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                              rqDF$sensorName == "UPneumo")
        P2 <- as.numeric(rqDF$ESSScore[UPneumoRow])
        names(P2) <- rqDF$CQName[UPneumoRow]
        
        LPneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                              rqDF$sensorName == "LPneumo")
        P1 <- as.numeric(rqDF$ESSScore[LPneumoRow])
        names(P1) <- rqDF$CQName[LPneumoRow]
        
        pneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] &
                             rqDF$sensorName == "Pneumo")
      }
      
      #### constraint for stability of upper and lower pneumo ####
      
      
      
      if(!PCATFormat) {
        
        # rqDF
        
        # get the logged RC ratios 
        upRC <- as.numeric(rqDF$RCScore[UPneumoRow])
        lwRC <- as.numeric(rqDF$RCScore[LPneumoRow])
        
        # increment if both RQ and CQ values are missing
        if( any(is.na(upRC), is.null(upRC), upRC=="") && any(is.na(lwRC), is.null(lwRC), lwRC=="") ) {
          rqDF$ESSScore[pneumoRow] <- NA
          # set the combined measurement and CQName to NA
          rqDF$sensorMeasurement[pneumoRow] <- NA
          rqDF$CQName[pneumoRow] <- NA 
          next() # next RQ
        }
        
        # check for small values of the upRC and lwRC
        
        if( !is.na(upRC) && upRC != 0 ) {
          if( !is.na(upRC) && abs(upRC) < .01 ) upRC <- .01 * sign(upRC)
          # upRC will be unchanged if it is not small
        } else {
          upRC <- 0
        }
        
        if( !is.na(lwRC) && lwRC != 0 ) {
          if( !is.na(lwRC) && abs(lwRC) < .01 ) lwRC <- .01 * sign(lwRC)
          # lwRC will be unchanged if it is not small
        } else {
          lwRC <- 0
        }
        
        # meanP <- mean(c(upRC, lwRC), na.ra=TRUE)
        # if( (meanPn <= posPneumoLow && meanPn >= negPneumoLow) || 
        #     (meanPn >= posPneumoHigh || meanPn <= negPneumoHigh) ) {
        #   # score NA if the mean of 2 pneumos has exceeded the constraints
        #   P1 <- NA
        #   P2 <- NA
        
        # }
        
        # check for extreme differences in logged RCScores
        
        # commented out March 23, 2022 because it causes 0 scores in error
        # if(any(is.na(c(P1, P2)))) {
        #   P1 <- NA
        #   P2 <- NA
        # } else {
        #   # upRC <- as.numeric(rqDF$RCScore[UPneumoRow])
        #   # lwRC <- as.numeric(rqDF$RCScore[LPneumoRow])
        #   # check if signs are the same for logRC ratios
        #   if( (upRC * lwRC) > 0 ) {
        #     # check for a large difference in logRC ratios
        #     # was 3 20200619
        #     # was 4 20210313
        #     # use the exp(abs(log())) method 
        #     # so it is the same regardless of whether UP or LP is larger
        #     if( exp(abs(log(upRC/lwRC))) > 6 ) {
        #       # set scores to 0 or NA or large difference in RC ratio
        #       P1 <- 0
        #       names(P1) <- rqDF$CQName[LPneumoRow]
        #       rqDF$ESSScore[LPneumoRow] <- P1
        #       # P2 is the upper respiration sensor
        #       P2 <- 0
        #       names(P2) <- rqDF$CQName[UPneumoRow]
        #       rqDF$ESSScore[UPneumoRow] <- P2
        #     }
        #   }
        # }
        # commented out March 23, 2022 because it causes 0 scores in error
        # combining the 2 pneumos will provide some protection against artifacts
        
      } 
      
      #### check if there is no ESSM score for either P1 or P2 ####
      
      if(!PCATFormat) {
        
        # Jan 25, 2025
        # changed to keep an available score if only one sensor is artifacted
        
        if(P2=="" && P1=="") {
          rqDF$ESSScore[pneumoRow] <- NA
          # set the combined measurement and CQName to NA
          rqDF$sensorMeasurement[pneumoRow] <- NA
          rqDF$CQName[pneumoRow] <- NA 
          next() # next RQ
        } else if(P2=="" && P1!="") {
          # set the combined score to P1 if there is no score at P2
          rqDF$ESSScore[pneumoRow] <- P1
          # set the combined measurement and CQName to NA
          rqDF$sensorMeasurement[pneumoRow] <- rqDF$sensorMeasurement[LPneumoRow]
          rqDF$CQName[pneumoRow] <- names(P1)
          next() # next RQ
        } else if(P1=="" && P2!="") {
          # set the combined score to P2 if there is no score at P1
          rqDF$ESSScore[pneumoRow] <- P2
          # set the combined measurement and CQName to NA
          rqDF$sensorMeasurement[pneumoRow] <- rqDF$sensorMeasurement[UPneumoRow]
          rqDF$CQName[pneumoRow] <- names(P2)
        }
        
      }
      
      #### combine the 2 sensors ####
      
      pneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                             (rqDF$sensorName == "Pneumo"))
        
      if(!PCATFormat) {
        
        # P2 is the thoracic respiration sensor
        # P1 is the abdominal sensor
        
        # 3-2-2017 this code should work for 3-position, ESS and 7-position
        # combine the upper and lower pneumo scores 
        ifelse(P1 * P2 < 0, 
               {
                 # if the upper and lower pneumo signs are opposite
                 rqDF$ESSScore[pneumoRow] <- 0
                 # set the combined measurement and CQName to NA
                 rqDF$sensorMeasurement[pneumoRow] <- NA
                 rqDF$CQName[pneumoRow] <- NA 
               }, 
               ifelse(P1 + P2 >= 0, 
                      {
                        # both pneumo signs are +
                        # choose the upper P2 if sum is 0
                        # which.max() takes the first of equal values
                        # thisPneumo <- which.max(c(upRC, lwRC))
                        # June 21, 2023 use the ESS Scores instead of logRC
                        # Jan 25, 2025 need to use the absolue value of the ESSM score
                        # to ensure selection of the score furthest from 0
                        thisPneumo <- which.max( c( abs(P2), abs(P1) ) )
                        # to avoid selecting a score of 0 when the logRC exceeds the outer constraint
                        rqDF$ESSScore[pneumoRow] <- c(P2, P1)[thisPneumo]
                        rqDF$RCScore[pneumoRow] <- 
                          c(upRC, lwRC)[thisPneumo]
                        
                        rqDF$sensorMeasurement[pneumoRow] <- 
                          c(rqDF$sensorMeasurement[UPneumoRow],
                            rqDF$sensorMeasurement[LPneumoRow])[thisPneumo]
                        
                        rqDF$CQName[pneumoRow] <- 
                          c(names(P2), names(P1))[thisPneumo]
                      }, 
                      {
                        # when P1 + P2 < 0
                        # if one or both signs are -
                        # thisPneumo <- which.min(c(upRC, lwRC))
                        thisPneumo <- which.min(c(P2, P1))
                        # June 21, 2023 use the ESS Scores instead of logRC
                        # to avoid selecting a score of 0 when the logRC exceeds the outer constraint
                        rqDF$ESSScore[pneumoRow] <- c(P2, P1)[thisPneumo]
                        rqDF$sensorMeasurement[pneumoRow] <- 
                          c(rqDF$sensorMeasurement[UPneumoRow],
                            rqDF$sensorMeasurement[LPneumoRow])[thisPneumo]
                        
                        rqDF$CQName[pneumoRow] <- 
                          c(names(P2), names(P1))[thisPneumo]
                      } ) )
        
      } else {
        
        # for PCAT/LXCAT exams
        rqDF$ESSScore[pneumoRow] <- NA
        rqDF$sensorMeasurement[pneumoRow] <- NA
        rqDF$CQName[pneumoRow] <- NA 
        
      }
      
    
      
      # March 23, 2022 # hunting for incorrect pneumo scores
      # if(all(seriesName=="2",
      #        thisChart == "02A",
      #        examName=="DC1601016A0",
      #        # thisSensor == "UPneumo",
      #        thisStimulusName == "10R")) {
      #   # assign("thisScore", thisScore, envir=.GlobalEnv)
      #   # assign("thisSensor", thisSensor, envir=.GlobalEnv)
      #   assign("seriesName", seriesName, envir=.GlobalEnv)
      #   assign("thisChart", thisChart, envir=.GlobalEnv)
      #   assign("examName", examName, envir=.GlobalEnv)
      #   assign("thisStimulusName", thisStimulusName, envir=.GlobalEnv)
      #   assign("uniqueRQsChart", uniqueRQsChart, envir=.GlobalEnv)
      #   # assign("thisSensor", thisSensor, envir=.GlobalEnv)
      #   assign("rqDF", rqDF, envir=.GlobalEnv)
      #   assign("P1", P1, envir=.GlobalEnv)
      #   assign("P2", P2, envir=.GlobalEnv)
      #   stop()
      # } 
      
      
      
    } # end loop k on RQs to combine 2 pneumo scores
    
    # pass the rqDF back to the RqCqDFChart
    RqCqDFChart[rqRows,] <- rqDF
    
    # View(RqCqDFChart)
    assign("RqCqDFChart", RqCqDFChart, pos=1)

    # pass the chart data frame back to the series
    RqCqDFSeries[thisChartRows,] <- RqCqDFChart
    # View(RqCqDFSeries)
    
    # assign("RqCqDFSeries", RqCqDFSeries, env=.GlobalEnv)
    
  } # end loop i over charts in the series
  
  #######################  ESS-M numerical results  ########################
  
  {
    
    # use the combined abdominal and thoracic ESS scores
    ESSMSensors2 <- c("Pneumo", "AutoEDA", "Cardio", "PLE")
    # ESSMSensors2 <- ifelse(isTRUE(includePLEData),
    #                        c("Pneumo", "AutoEDA", "Cardio", "PLE"),
    #                        c("Pneumo", "AutoEDA", "Cardio") )

    ESSMSensors2 <- ESSMSensors2[ESSMSensors2 %in% uniqueSensors]

    # exclude the PLE if includePLEScores==FALSE
    if("PLE" %in% ESSMSensors2 && !isTRUE(includePLEScores)) {
      ESSMSensors2 <- ESSMSensors2[-which(ESSMSensors2 %in% "PLE")]
    }
    
    #### calculate the grand total score ####
    
    {
      
      theseRows <- which(RqCqDFSeries$sensorName %in% ESSMSensors2)
      grandTotal <- sum(as.integer(RqCqDFSeries$ESSScore[theseRows]),
                        na.rm=TRUE)
      
    }

    # print(paste("ESS-M grand total:", grandTotal))
    
    ####### fix the question labels for DLST DLDT type exams #######
    
    if(isTRUE(DLSTType)) {
      
      # parse the RQs for DLST/DLDT/PCASS type formats 
      
      RqCqDFSeries$Label <- 
        fixDLSTDLDTFn(saveQuestionLabels, CQRQLabels)
      
      # make a new vector of unique RQs
      uniqueQuestions <- unique(RqCqDFSeries$Label)
      uniqueRQs <- unique(uniqueQuestions[grep("R", uniqueQuestions)])
      uniqueCQs <- unique(uniqueQuestions[grep("C", uniqueQuestions)])
      
    }
        
    #### calculate the RQ subtotal scores ####
    
    {
      
      # initialize a vector for the RQ subtotal scores
      subtotalScores <- rep(NA, length=length(uniqueRQs))
      names(subtotalScores) <- uniqueRQs
      
      # iterate over the RQs to get the question subtotal scores
      i=1
      for(i in 1:length(uniqueRQs)) {
        thisRQ <- uniqueRQs[i]
        RQRows <- RqCqDFSeries$Label == thisRQ
        ESSMSensorRows <- RqCqDFSeries$sensorName %in% ESSMSensors2
        theseRows <- which(RQRows & ESSMSensorRows)
        subtotalScores[i] <- sum(as.numeric(RqCqDFSeries$ESSScore[theseRows]),
                                 na.rm=TRUE)
      }
      
      # print("ESS-M subtotals: ")
      # print(toString(subtotalScores))
      # print((subtotalScores))
      
      minSubtotalScore <- subtotalScores[which.min(subtotalScores)]
      minRQName <- uniqueRQs[which.min(subtotalScores)]
      
    }

    # print(paste("min subtotal:", names(minSubtotalScore), minSubtotalScore))
    # print(paste("ESS-M grand total:", grandTotal))
    
  }
  
  ################ ESS-M cutScores ##############
  
  {
    
    # hard-coded simple ESS-M cutscores
    # load these in the same env where the decision rules Fn are defined
    # cutScores <- c(GTDI=-3, GTNDI=3, STDI=-3, STNDI=3, STDIc=-7, STNDIc=1)
    {
      if(!exists("essmAlphaD")) essmAlphaD <- .05
      if(!exists("essmAlphaT")) essmAlphaT <- .05
      if(!exists("essmAlphas")) essmAlphas <-  c(aD=essmAlphaD, aT=essmAlphaT)
      if(!exists("essmPrior")) essmPrior <- .5
    }
    
    # adjust the prior to the values in the cutscores tables
    essmPrior <- ESSMs_SI_NDI$priorG[max(which(ESSMs_SI_NDI$priorG >= essmPrior))]
    
    priorOdds <- essmPrior / (1 - essmPrior)
    
    # adjust the alphas to the values in the cutscores tables
    alphaChoice <- c(.01, .05, .10)
    ESSMAlphas['aD'] <- alphaChoice[max(which(alphaChoice <= ESSMAlphas['aD']))]
    ESSMAlphas['aT'] <- alphaChoice[max(which(alphaChoice <= ESSMAlphas['aT']))]
    
    # use the prior and alpha to lookup the ESS-M cutscores
    
    # the prior row
    priorRow <- max(which(ESSMs_SI_NDI$priorG >= essmPrior))
    # can be used with all tables
    
    NDINSRCol <- switch(as.character(essmAlphaT),
                      "0.01"="a01",
                      "0.1"="a10",
                      "0.05"="a05",
                      "otherwise: last" )
    
    DISRCol <- switch(as.character(essmAlphaD),
                      "0.01"="a01",
                      "0.1"="a10",
                      "0.05"="a05",
                      "otherwise: last" )
    
    # single issue cutscores
    GTNDI <- ESSMs_SI_NDI[,NDINSRCol][priorRow]
    GTDI <- ESSMs_SI_DI[,DISRCol][priorRow]
    STDIc <- ESSMs_SI_DI_St[,DISRCol][priorRow]
    
    # multiple issue cutscores
    STDI <- ESSMs_MI_SR[,DISRCol][priorRow]
    STNDIc <- ESSMs_MI_NSR[,NDINSRCol][priorRow]
    
    
    # if(isTRUE(PCASSFormat)) {
    #   # PCAT/PCASS cutscores
    #   # August 4, 2021
    #   # this will replace the cutscores with PCAT ESS-M cutscores
    #   # calculated for EDA and PO2 sensors with 4 iterations of 2 questions
    #   
    #   # single issue cutscores
    #   GTNDI <- PCAT_ESSMs_SI_NDI[,NDINSRCol][priorRow]
    #   GTDI <- PCAT_ESSMs_SI_DI[,DISRCol][priorRow]
    #   STDIc <- PCAT_ESSMs_SI_DI_St[,DISRCol][priorRow]
    #   
    #   # multiple issue cutscores
    #   STDI <- PCAT_ESSMs_MI_SR[,DISRCol][priorRow]
    #   STNDIc <- PCAT_ESSMs_MI_NSR[,NDINSRCol][priorRow]
    # }
    
    # make a vector of cutscores for the decision rules
    cutScores <- c(GTNDI=GTNDI, GTDI=GTDI, STDIc=STDIc, STDI=STDI, STNDIc=STNDIc)
    
  }
  
  ######### auto-select the ESS-M decision rule ##########
  
  {
  
    # source(paste0(RPath, 'autoSelectTSRSSR.R'), echo=TRUE)
    
    pairwiseResult <-  autoSelectTSRSSRFn(x=subtotalScores, 
                                    PLE=TRUE, 
                                    a=.01)
    
    # output is a vector of 3 values
    # sig or ns
    # level of significance
    # decision rule
    
    # initialize this to avoid problems if the rule is not "auto"
    outputRule <- ESSMDecisionRule
    
  }
  
  ######### select the ESS-M decision rule ###########
  
  if(ESSMDecisionRule == "auto" || grepl("auto",  outputRule)) {
    
    # ESSMDecisionRule <- "auto"
    outputRule <- ESSMDecisionRule
    
    if(ESSMDecisionRule == "auto") {
      ESSMDecisionRule <-  ifelse(pairwiseResult['result'] == "sig",
                                  "SSR",
                                  "eTSR" )
      outputRule <- paste0(ESSMDecisionRule, " (auto-selected)")
    }
    names(ESSMDecisionRule) <- NULL
    
    # print(ESSMDecisionRule)
    # print(outputRule)
    
  } else {
    
    outputRule <- ESSMDecisionRule
    # print(ESSMDecisionRule)
    # print(outputRule)
    
  }
  
  ################ ESS-M Classification (result) #################
  
  
  {

    # cutScores <- c(GTNDI=GTNDI, GTDI=GTDI, STDIc=STDIc, STDI=STDI, STNDIc=STNDIc)
    
    # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
    
    # the same cutscores will be submitted to all decision rules
    
    GTRResult <- GTRFn(totalScore=grandTotal, 
                       RQNames=uniqueRQs,
                       cutScores=cutScores,
                       flip=FALSE )
    
    SSRResult <- SSRFn(subtotalScores=subtotalScores, 
                       cutScores=cutScores,
                       flip=FALSE )
    
    TSRResult <- eTSRFn(totalScore=grandTotal, 
                        subtotalScores=subtotalScores, 
                        cutScores=cutScores,
                        flip=FALSE )
    
    FZRResult <- FZRFn(grandTotal=grandTotal, 
                       subtotalScores=subtotalScores, 
                       cutScores=cutScores)
    
    if(!exists("ESSMDecisionRule")) ESSMDecisionRule <- "TSR"
    
    ESSMTestResult <- switch(ESSMDecisionRule,
                         "eTSR"=TSRResult$testResult,
                         "SSR"=SSRResult$testResult,
                         "GTR"=GTRResult$testResult,
                         "FZR"=)
    
    ESSMQuestionResults <- switch(ESSMDecisionRule,
                                  "eTSR"=TSRResult$subtotalResults,
                                  "SSR"=SSRResult$subtotalResults,
                                  "GTR"=GTRResult$subtotalResults)

    resultUsing <- switch(ESSMDecisionRule,
                          "eTSR"=TSRResult$resultUsing,
                          "SSR"=SSRResult$resultUsing,
                          "GTR"=GTRResult$resultUsing)
    names(resultUsing) <- NULL
    
  }
  
  ################## ESS-Multinomial Reference Model ###################
  
  {
    
    # ESSMRefDir <- "RPath"
    # ESSM_simple_GT <- read.csv(paste0(ESSMRefDir, "ESSM_simple_GT.csv"),
    #                            stringsAsFactors=FALSE)
    # ESSM_simple_ST <- read.csv(paste0(ESSMRefDir,
    #                                   "ESSM_simple_ST.csv"),
    #                            stringsAsFactors=FALSE)
    
    # look up the Bayes Factor
    
    scoreRowFn <- function(score=0, DAT=ESSM_simple_GT, tableCol="score") {
      # private function to get the score row using the ESS-M reference table
      # score is the numerical score for which to get the table row
      # DAT is a data frame
      # tableCol is the name of the column that holds the vector of possible score
      if(score >= max(DAT[,tableCol])) return(nrow(DAT))
      if(score <= min(DAT[,tableCol])) return(1)
      max(which(DAT[,tableCol] <= score))
    }
    
    # look up the odds (Bayes Factor)
    GTOdds <- ESSM_simple_GT$odds[scoreRowFn(score=grandTotal, DAT=ESSM_simple_GT)]
    
    STOdds <- ESSM_simple_ST$odds[scoreRowFn(minSubtotalScore, ESSM_simple_ST)]
    STOddsC  <- ESSM_simple_ST$oddsM[scoreRowFn(minSubtotalScore, ESSM_simple_ST)]
    
    # calculate the probability
    GTProb <- round(GTOdds / (1 + GTOdds), 3)
    
    STProb <- round(STOdds / (1 + STOdds), 3)
    STProbC <- round(STOddsC / (1 + STOddsC), 3)
    
    # choose the grand total or subtotal odds
    bayesFactor <- ifelse(resultUsing == "grand total",
                          GTOdds,
                          ifelse(minSubtotalScore > 0,
                                 STOddsC,
                                 STOdds ) )
    names(bayesFactor) <- NULL
    
  }
  
  ####### calculate the ESS-M posterior odds and posterior probability #######
  
  {
    
    bayesFn <- function(priorP=essmPrior, p) {  
      # private R function to calculate Bayes theorem
      # 6-27-2017
      # Raymond Nelson
      #
      #######
      (priorP * p) / ( (priorP * p) + ((1 - priorP) * (1 - p)) )
      
    } 
    
    # update the posterior odds
    postOdds <- bayesFn(essmPrior, bayesFactor)
    names(postOdds) <- NULL
    
    # posterior probability
    postProb <- round(postOdds / (1 + postOdds), 3)
    names(postProb) <- NULL
    
  }
  
  #### calculate Clopper-Pearson Bayesian Credible Interval ####
  
  {
    
    # source(paste0(RPath, 'ClopperPearsonBinomialCI.R'), echo=FALSE)
    
    testScore <- ifelse(resultUsing == "lowest subtotal",
                        minSubtotalScore,
                        grandTotal )
    
    thisAlpha <- ifelse(testScore > 0,
                        essmAlphas['aT'],
                        essmAlphas['aD'])
    names(thisAlpha) <- NULL
    
    maxScore <- ifelse(resultUsing == "lowest subtotal",
                       25,
                       75 )
    
    RQs <- ifelse(resultUsing == "lowest subtotal",
                  1,
                  3 ) 
    
    # source(paste0(RPath, 'ClopperPearsonBinomialCI.R'), echo=FALSE)
    CPLowerLimit <- clopperPearsonFn(p=postProb, 
                                     RQs=RQs,
                                     maxCharts=5,
                                     PLE=TRUE,
                                     n=maxScore, 
                                     a2=thisAlpha, 
                                     odds=TRUE )[1]
    
    CPLowerLimit <- round(CPLowerLimit, 2)
    names(CPLowerLimit) <- NULL
    
    ClopperPearsonInterval <-  c(CPLowerLimit=CPLowerLimit, 
                                   thisAlpha=thisAlpha)
    
  }
  
  ########################### ESS-M output section ########################
  
  # output table functions
  # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
  
  ######## ESS-M measurements data frame 
  
  {
    
    ESSMMeasurementSensors <- c("UPneumo", 
                                "LPneumo", 
                                "AutoEDA", 
                                "Cardio", 
                                "PLE")
    
    # exclude the PLE
    if("PLE" %in% ESSMMeasurementSensors && !isTRUE(includePLEScores)) {
      ESSMMeasurementSensors <- 
        ESSMMeasurementSensors[-which(ESSMMeasurementSensors %in% "PLE")]
    }
    
    ESSMMeasurementSensors <- 
      ESSMMeasurementSensors[ESSMMeasurementSensors %in% 
                               RqCqDFSeries$sensorName]
    
    measurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=ESSMMeasurementSensors,
                         makeDF=makeDF,
                         saveCSV=saveCSV )
    # View(measurementsDF)
    
  }
  
  ############# ESS-M score sheet for the series
  
  scoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=ESSMSensors2,
                               scoreType="ESSScore",
                               decimals=0,
                               DLSTType=DLSTType,
                               outputName="ESSMScoresheetDF",
                               makeDF=makeDF,
                               saveCSV=TRUE)
  
  ################ ESS-M series totals
  
  seriesTotalsDF <- seriesTotalsFn(scoreSheetDF=scoreSheetDF,
                                   outputName="ESSMSeriesTotalsDF",
                                   aggType="sum",
                                   weightingCoefs=NULL,
                                   aggMethod="between",
                                   makeDF=makeDF,
                                   saveCSV=TRUE)
  
  ########## ESS-M chart totals, including RQ subtotals and chart subtotals
  
  chartTotalsDF <- chartTotalsFn(scoreSheetDF=scoreSheetDF,
                                 outputName="ESSMChartTotalsDF",
                                 aggType="sum",
                                 weightingCoefs=NULL,
                                 makeDF=makeDF,
                                 saveCSV=saveCSV)
  
  ################ ESS-M sensor subtotals for the series
  
  sensorTotalsDF <- sensorSubtotalsFn(scoreSheetDF=scoreSheetDF,
                                      outputName="ESSMSensorTotalsDF",
                                      aggType="sum",
                                      makeDF=makeDF,
                                      saveCSV=TRUE)
  
  ######## get the RC ratios for output
  
  RCScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                 useSensors=ESSMMeasurementSensors,
                                 scoreType="RCScore",
                                 decimals=2,
                                 outputName="RCScoresheetDF",
                                 makeDF=makeDF,
                                 saveCSV=TRUE)
  
  ########### get the CQ selection for output
  
  CQSelectionDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                useSensors=ESSMMeasurementSensors,
                                scoreType="CQName",
                                decimals="non-numeric",
                                outputName="CQSelectionDF",
                                makeDF=makeDF,
                                saveCSV=TRUE)
  
  ############### construct a list to hold the ESS-M result #################
  
  outputListName <- paste(examName, seriesName, "ESSMOutputList", sep="_")
  
  ESSMOutputList <- list(ESSM="Empirical Scoring System - Multinomial (Nelson, Krapohl & Handler, 2008; Nelson, 2017)",
                         examName=examName,
                         seriesName=seriesName,
                         ESSMResult=ESSMTestResult,
                         ESSMQuestionResults=ESSMQuestionResults,
                         ESSMDecisionRule=outputRule,
                         ESSMResultUsing=resultUsing,
                         ESSMRQNames=uniqueRQs,
                         ESSMPairwiseResult=pairwiseResult,
                         ESSMGrandTotal=grandTotal,
                         ESSMMinSubtotal=minSubtotalScore,
                         ESSMMinRQ=minRQName,
                         ESSMSubtotalScores=subtotalScores,
                         ESSMQuestions=uniqueQuestions,
                         ESSMPriorOdds=priorOdds,
                         ESSMPriorProbability=essmPrior,
                         ESSMPostProb=postProb,
                         ESSMPostOdds=postOdds,
                         ESSMBayesFactor=bayesFactor,
                         ESSMAlphas=essmAlphas,
                         ESSMCPLowerLimitPostOdds=CPLowerLimit,
                         ESSMCPAlpha=thisAlpha,
                         ESSMCutscores=cutScores,
                         ESSMSensors=ESSMMeasurementSensors,
                         ESSMWarning=ESSMWarning,
                         ESSMQuestionSequence=questionSequenceDF,
                         ESSMCQSelection=CQSelectionDF,
                         ESSMLogRCRatios=RCScoreSheetDF,
                         ESSMScoreSheetDF=scoreSheetDF,
                         ESSMChartTotalsDF=chartTotalsDF,
                         ESSMSeriesTotalsDF=seriesTotalsDF,
                         ESSMSensorTotalsDF=sensorTotalsDF, 
                         ESSMMeasurementDF=measurementsDF )
  
  #### June 22, 2023, save the ESS-M report to the cwd as a .csv ####
  
  # capture.output(ESSMOutputList, file=paste0(outputListName, ".txt"))
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, ESSMOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "ESSMOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      ESSMOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  # data frame with the ESSScores column populated with the integer scores 
  return(RqCqDFSeries)

} # end ESSMScoresFn





