# R function to compute the PCAT scores 
# requires the RCScores function is called first
# June 2021
# Raymond Nelson
####




# RqCqDFSeries=RqCqDFSeries,
# PCATDecisionRule=PCATDecisionRule,
# makeScoreSheetDF=makeDF, 
# writeScoreSheetCSV=saveCSV,
# makeDF=makeDF,
# writeCSV=saveCSV






{
  
  # PCAT requires the R/C ratios from the the function in this script
  source(paste0(RPath, 'RCScores.R'), echo=FALSE)
  
  # source the script for the R/C ratio constraints
  # requires the RCToESSFn()
  # source(paste0(RPath, 'ESSScoreFromLogRC.R'), echo=FALSE)
  # sourced by the RCScores.R script
  
  # PCAT also requires the function in this script to select the CQ
  # source(paste0(RPath, 'selectCQ.R'), echo=FALSE)
  # sourced by the RCScores.R script
  
  # function to autoselect the PCAT decision rule via pairwise comparison
  source(paste0(RPath, 'autoSelectTSRSSR.R'), echo=FALSE)
  
  # ESS Multinomial Likelihood Function
  # ESSM_simple_GT <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/ESSM_simple_GT.csv", header=TRUE, stringsAsFactors=FALSE)
  # ESSM_simple_ST <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/ESSM_simple_ST.csv", header=TRUE, stringsAsFactors=FALSE)
  
  # ESSM (simple) cutscore tables
  # ESSMs_SI_NDI <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/ESSMs_SI_NDI.csv", stringsAsFactors=FALSE)
  # ESSMs_SI_DI <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/ESSMs_SI_DI.csv", stringsAsFactors=FALSE)
  # ESSMs_SI_DI_St <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/ESSMs_SI_DI_St.csv", stringsAsFactors=FALSE)
  # ESSMs_MI_NSR <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/ESSMs_MI_NSR.csv", stringsAsFactors=FALSE)
  # ESSMs_MI_SR <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/ESSMs_MI_SR.csv", stringsAsFactors=FALSE)

  # PCAT/PCASS Multinomial Likelihood Functions
  PCAT_ESS_2RQ <- read.csv(paste0(RPath, "PCAT_ESS_2RQ.csv"), header=TRUE, stringsAsFactors=FALSE)
  PCAT_ESS_1RQ <- read.csv(paste0(RPath, "PCAT_ESS_1RQ.csv"), header=TRUE, stringsAsFactors=FALSE)
  
  # PCAT/PCASS cutscore tables
  PCAT_ESSMs_SI_NDI <- read.csv(paste0(RPath, "PCAT_ESSMs_SI_NDI.csv"), stringsAsFactors=FALSE)
  PCAT_ESSMs_SI_DI <- read.csv(paste0(RPath, "PCAT_ESSMs_SI_DI.csv"), stringsAsFactors=FALSE)
  PCAT_ESSMs_SI_DI_St <- read.csv(paste0(RPath, "PCAT_ESSMs_SI_DI_St.csv"), stringsAsFactors=FALSE)
  PCAT_ESSMs_MI_NSR <- read.csv(paste0(RPath, "PCAT_ESSMs_MI_NSR.csv"), stringsAsFactors=FALSE)
  PCAT_ESSMs_MI_SR <- read.csv(paste0(RPath, "PCAT_ESSMs_MI_SR.csv"), stringsAsFactors=FALSE)
  
         
  
  # function to get the lower limit of the Bayesiann credible interval
  source(paste0(RPath, 'ClopperPearsonBinomialCI.R'), echo=FALSE)
  
  ## these are set in the NCCAASCII_init.R script ##
  # RqCqDFSeries=RqCqDFSeries,
  # ESSMDecisionRule=ESSMDecisionRule,
  # makeDF=makeDF, 
  # saveCSV=saveCSV,
  # makeDF=makeDF,
  # saveCSV=saveCSV
  
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



############## main function ##########################

PCATScoresFn <- function(RqCqDFSeries=RqCqDFSeries,
                         PCATDecisionRule="pSSR",
                         PCATPrior=.5,
                         PCATAlphas=c(aD=.05, aT=.05),
                         forced=FALSE,
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
    
    print("calculate the PCAT scores")
    
    # if(seriesName == "4"){
    #   assign("RqCqSeriesDF", RqCqDFSeries, envir=.GlobalEnv)
    #   stop()
    # }
    
    if(!exists("PCATDecisionRule")) PCATDecisionRule <- "SSR"
    if(!exists("PCATPrior")) PCATPrior <- .05
    if(!exists("PCATAlphas")) PCATAlphas <- c(aD=.05, aT=.05)
    
    if(!exists("makeDF")) makeDF <- FALSE
    if(!exists("saveCSV")) saveCSV <- FALSE
    
    if(!exists("analysisListName")) analysisListName="analysisResultList"
    
    # View(RqCqDFSeries)
    # assign("RqCqDFSeries", RqCqDFSeries, envir=.GlobalEnv)
    # stop()
    
    # this is set in the NCCAAASCII_init.R script
    # if(!exists("ESSMIncludePLE")) ESSMIncludePLE <- FALSE
    
    # initialize the default warning
    PCATWarning <- "none"
    
  }
  
  #### constraints for PCAT R/C ratios ####
  
  {
    # now included in 
    # source(paste0(RPath, 'ESSScoreFromLogRC.R'), echo=FALSE)
  }
  
  #### initialize a data frame with the question sequence for each chart ####
  
  {
    # includes only CQs and RQs
    
    questionSequenceDF <- questionSequenceFn(measurementDF=RqCqDFSeries,
                                             outputName="ESSMQuestionSequence",
                                             makeDF=FALSE,
                                             saveCSV=FALSE)
    # this data frame is included in the output 
    # and shows the question order for each chart
    # ESS-M, PCAT, and OSS-2 scores are influenced to some degree by question order
    # most algorithms are not influenced by question order
    
  }
  
  ############ initial PCAT set up #############
  
  {
    
    RqCqDFSeries$PCATScore <- ""
    
    uniqueSensors <- unique(as.character(RqCqDFSeries$sensorName))
    
    PCATSensors <- c("AutoEDA", 
                     "PLE")
    
    # keep only the extant sensors
    PCATSensors <- PCATSensors[PCATSensors %in% uniqueSensors]
    
    # exit if PCAT Sensors are missing
    if(length(PCATSensors) < 2) {
      print("missing PCAT sensors")
      {
        PCATOutputList <- list(PCAT="Preliminary Credibility Assessment Test",
                               examName=examName,
                               seriesName=seriesName,
                               PCATResult="missing PCAT sensors" )
        analysisResultList <- get(analysisListName, envir=.GlobalEnv)
        seriesListName <- paste("series", seriesName, sep="_")
        outputListName <- "PCATOutput"
        analysisResultList[[seriesListName]][[outputListName]] <- 
          PCATOutputList
        assign(analysisListName, analysisResultList, envir=.GlobalEnv)
      }
      return(RqCqDFSeries)
    }
    
  }
  
  ####### slice the RQs and CQs #######
  
  {
    
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
    
  }
  
  ########### calculate the logged R/C ratios here #############
  
  {
    ## RCScoresFn()  is called separately ##
    ## so that the seriesMeasurementDF can be used ##
    ## instead of the RqCqDFSeries ##
    ## so that the selectCQFn() can see all Qs and annotations ##
    ## in addition to the RQs and CQs ##
  }
  
  ##### check for DLST DLDT and PCASS/PCAT type charts #####
  
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
  
  ########## get the exam data and series data ##########
  
  {
    # to check the RBPF and respiration rate
    examData <- get(paste0(examName, "_Data"), envir=.GlobalEnv)
    
    # RqCqSeriesDF will contain only this series
    seriesData <-
      examData[which(examData$seriesName==RqCqDFSeries$seriesName[1]),]
  }
  
  ############### iterate over the PCAT charts ###############
  
  i=1
  for (i in 1:length(uniqueCharts)) {
    
    ##### first get the RqCq data frame for this chart #####
    
    {
      
      thisChart <- uniqueCharts[i]
      thisChartRows <- which(RqCqDFSeries$chartName == thisChart)
      
      # PCAT requires the R/C ratios from the the function in this script
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
      
      # cqDF <- RqCqDFChart[cqRows,] # not used for anything
      # View(cqDF)
      
      # assign("rqDF", rqDF, pos=1)
      # assign("cqDF", cqDF, pos=1)
      
      # set the combined pneumo measurement to ""
      # pneumoRows <- which(cqDF$sensorName=="Pneumo")
      # cqDF$sensorMeasurement[pneumoRows] <- NULL
      # RqCqDFChart[cqRows,] <- cqDF
      
    }
    
    #### RC scores were already calculated for each chart  ####
    
    {
      
      # set all integer score rows to NA if the R/C score is NA
      # rqDF[is.na(as.numeric(rqDF$RCScore)),'PCATScore'] <- NA
      # View(rqDF)
      
      # select rows for which the R/C score and integer score are not NA
      # theseRows <- !is.na(as.numeric(rqDF$RCScore))
      # selectRows <- which(theseRows & sensorRows)
      
      # initialize the PCATScore vector to NA to avoid errors if there is no score
      PCATScore <- NA
      
      ####  get the ESSM sensor rows for this chart #####
      
      # sensorRows <- rqDF$sensorName %in% PCATSensors
      selectRows <- which(rqDF$sensorName %in% PCATSensors)
      
      # rqDF$sensorName[selectRows]
      
    }
    
    #### iterate on the rqDF sensor rows to get the PCAT score ####
    
    j=1 # 6 is the PLE # 13 is the upper pneumo 
    for (j in 1:length(selectRows)) {
      
      #### get the logRC score and sensor name ####
      
      {
        
        # first get the stimulus name, sensor, and score
        thisStimulusName <- rqDF$eventLabel[selectRows[j]]
        
        # View(rqDF)
        
        thisSensor <- rqDF$sensorName[selectRows[j]]
        
        # use the ipZScore to check for extreme values
        # if( is.na(rqDF$ipZScore[selectRows[j]]) ||
        #          rqDF$ipZScore[selectRows[j]] == "" ) {
        #   rqDF$PCATScore[selectRows[j]] <- NA
        #   next()
        # }
        
        # get the logRC Score
        thisScore <- as.numeric(rqDF$RCScore[selectRows[j]])
        # print the info to the console
        # print(paste(thisStimulusName, thisSensor, thisScore))
        # rqDF$sensorMeasurement[selectRows[j]]
        
        if(is.na(thisScore)) {
          # increment the sensor if no R/C score
          rqDF$PCATScore[selectRows[j]] <- NA
          next() # next j sensor row
        }
        
      }
      
      #### then compute the PCAT scores ####
      
      # source(paste0(RPath, 'ESSScoreFromLogRC.R'), echo=FALSE)
      
      PCATScore <- RCToESSFn(thisScore=thisScore, 
                            thisSensor=thisSensor)
      
      #### assign the ESS sensor integer score to the rqDF data frame ####
      
      rqDF$PCATScore[selectRows[j]] <- ifelse(thisSensor != "PLE",
                                             PCATScore,
                                             ifelse(isTRUE(includePLEData),
                                                    PCATScore,
                                                    NA ) )
      
      # print(paste("integer score", PCATScore))
      # View(rqDF)
      
    } # end loop j over sensor rows
    
    #### combine the upper and lower pneumo scores ####
    
    if(!PCATFormat) {
      
      # March 12, 2024 where is PCATFormat initialized?
      
      k=1
      # iterate over the RQs 
      for (k in 1:length(uniqueRQsChart)) {
        
        {
          # get the P1 and P2 values
          
          UPneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                                rqDF$sensorName == "UPneumo")
          P2 <- as.numeric(rqDF$PCATScore[UPneumoRow])
          names(P2) <- rqDF$CQName[UPneumoRow]
          
          LPneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                                rqDF$sensorName == "LPneumo")
          P1 <- as.numeric(rqDF$PCATScore[LPneumoRow])
          names(P1) <- rqDF$CQName[LPneumoRow]
        }
        
        #### constraint for stability of upper and lower pneumoo ####
        
        {
          
          # get the logged RC ratios 
          upRC <- as.numeric(rqDF$RCScore[UPneumoRow])
          lwRC <- as.numeric(rqDF$RCScore[LPneumoRow])
          
          # increment if both values are NA
          if(is.na(upRC) && is.na(lwRC)) next()
          
          # check for small values of the upRC and lwRC
          
          if(!is.na(upRC) && upRC != 0) {
            if(!is.na(upRC) && abs(upRC) < .01) upRC <- .01 * sign(upRC)
            # upRC will be unchanged if it is not small
          } else {
            upRC <- 0
          }
          
          if(!is.na(lwRC) && lwRC != 0) {
            if(!is.na(lwRC) && abs(lwRC) < .01) lwRC <- 1 * sign(lwRC)
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
          
          if(any(is.na(c(P1, P2)))) {
            
            P1 <- NA
            P2 <- NA
          } else {
            # upRC <- as.numeric(rqDF$RCScore[UPneumoRow])
            # lwRC <- as.numeric(rqDF$RCScore[LPneumoRow])
            # check if signs are the same for logRC ratios
            if( (upRC * lwRC) > 0 ) {
              # check for a large difference in logRC ratios
              # was 3 20200619
              # was 4 20210313
              if( exp(abs(log(upRC/lwRC))) > 4 ) {
                # set scores to 0 or NA or large difference in RC ratio
                P1 <- 0
                names(P1) <- rqDF$CQName[LPneumoRow]
                rqDF$PCATScore[LPneumoRow] <- P1
                # P2 is the upper respiration sensor
                P2 <- 0
                names(P2) <- rqDF$CQName[UPneumoRow]
                rqDF$PCATScore[UPneumoRow] <- P2
              }
            }
          }
          
        }
        
        ##### combine the 2 sensors #####
        
        pneumoRow <- which(rqDF$eventLabel == uniqueRQsChart[k] & 
                             (rqDF$sensorName == "Pneumo"))
        
        # 3-2-2017 this code should work for 3-position, ESS and 7-position
        # combine the upper and lower pneumo scores 
        ifelse(P1 * P2 < 0, 
               {
                 # if the upper and lower pneumo signs are opposite
                 rqDF$PCATScore[pneumoRow] <- 0
                 # set the combined measurement and CQName to NA
                 rqDF$sensorMeasurement[pneumoRow] <- NA
                 rqDF$CQName[pneumoRow] <- NA 
               }, 
               ifelse(P1 + P2 >= 0, 
                      {
                        # both pneumo signs are +
                        # choose the upper P2 if sum is 0
                        # which.max() takes the first of equal values
                        thisPneumo <- which.max(c(P2, P1))
                        rqDF$PCATScore[pneumoRow] <- c(P2, P1)[thisPneumo]
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
                        thisPneumo <- which.min(c(P2, P1))
                        rqDF$PCATScore[pneumoRow] <- c(P2, P1)[thisPneumo]
                        rqDF$CQName[pneumoRow] <- 
                          c(names(P2), names(P1))[thisPneumo]
                      } ) )
        
      } # end loop k to combine 2 pneumo scores
      
    }
    
    # View(rqDF)
    
    # pass the rqDF back to the RqCqDFChart
    RqCqDFChart[rqRows,] <- rqDF
    
    # View(RqCqDFChart)
    assign("RqCqDFChart", RqCqDFChart, pos=1)
    
    # pass the chart data frame back to the series
    RqCqDFSeries[thisChartRows,] <- RqCqDFChart
    # View(RqCqDFSeries)
    
    # assign("RqCqDFSeries", RqCqDFSeries, env=.GlobalEnv)
    
  } # end loop i over charts in the series
  
  #######################  PCAT numerical results  ########################
  
  {
    
    # use only the EDA and PLE sensors
    PCATSensors2 <- c("AutoEDA", "PLE")
    
    PCATSensors2 <- PCATSensors2[PCATSensors2 %in% uniqueSensors]
    
    #### calculate the grand total score ####
    
    {
      
      theseRows <- which(RqCqDFSeries$sensorName %in% PCATSensors2)
      # RqCqDFSeries$sensorName[theseRows]
      grandTotal <- sum(as.integer(RqCqDFSeries$PCATScore[theseRows]),
                        na.rm=TRUE)
      
    }
    
    # print(paste("PCAT grand total:", grandTotal))
    
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
        ESSMSensorRows <- RqCqDFSeries$sensorName %in% PCATSensors2
        theseRows <- which(RQRows & ESSMSensorRows)
        subtotalScores[i] <- sum(as.numeric(RqCqDFSeries$PCATScore[theseRows]),
                                 na.rm=TRUE)
      }
      
      # print("PCAT subtotals: ")
      # print(toString(subtotalScores))
      # print((subtotalScores))
      
      minSubtotalScore <- subtotalScores[which.min(subtotalScores)]
      minRQName <- uniqueRQs[which.min(subtotalScores)]
      
    }
    
    # print(paste("min subtotal:", names(minSubtotalScore), minSubtotalScore))
    # print(paste("PCAT grand total:", grandTotal))
    
  }
  
  ######### adjust for non-PCAT charts ##########
  
  if(isTRUE(forced)) {
    
    # grandTotal
    
    # minSubtotalScore
    
    # check the number of scores
    theseSensorRows <- RqCqDFSeries$sensorName %in% PCATSensors2 
    theseRQRows <- grepl("R", RqCqDFSeries$eventLabel)
    
    theseRows <- which(theseSensorRows & theseRQRows)
    
    adjVal <- 16 / length(theseRows)
    adjValS <- 8 / (length(theseRows) / length(uniqueRQs))
    
    grandTotalA <- grandTotal * adjVal
    
    minSubtotalScoreA <- minSubtotalScore * adjValS
    
    grandTotal <- grandTotalA
    
    minSubtotalScore <- minSubtotalScoreA
    
  }
  
  ################ PCAT cutScores ##############
  
  {
    
    # hard-coded simple PCAT cutscores
    # load these in the same env where the decision rules Fn are defined
    # cutScores <- c(GTDI=-3, GTNDI=3, STDI=-3, STNDI=3, STDIc=-7, STNDIc=1)
    {
      # if(!exists("essmAlphaD")) essmAlphaD <- .05
      # if(!exists("essmAlphaT")) essmAlphaT <- .05
      # if(!exists("essmAlphas")) essmAlphas <-  c(aD=essmAlphaD, aT=essmAlphaT)
      # if(!exists("essmPrior")) essmPrior <- .5
      
      # these parameters are in the NCCAASCII_init.R script
      if(!exists("PCATAlphaD")) PCATAlphaD <- .05
      if(!exists("PCATAlphaT")) PCATAlphaT <- .05
      if(!exists("PCATAlphas")) PCATAlphas <- c(aD=PCATAlphaD, aT=PCATAlphaT)
      
      if(!exists("PCATPrior")) PCATPrior =.5
    }
    
    {
      
      ## adjust the prior and alphgas to the values in the cutscore tables
      
      PCATPrior <- PCAT_ESSMs_SI_NDI$priorG[max(which(ESSMs_SI_NDI$priorG >= PCATPrior))]
      
      priorOdds <- PCATPrior / (1 - PCATPrior)
      
      # adjust the alphas to the values in the cutscore tables
      alphaChoice <- c(.01, .05, .10)
      PCATAlphas['aD'] <- alphaChoice[max(which(alphaChoice <= PCATAlphas['aD']))]
      PCATAlphas['aT'] <- alphaChoice[max(which(alphaChoice <= PCATAlphas['aT']))]
      
      # use the prior and alpha to lookup the PCAT cutscores
      
    }
    
    {
      
      ## get the row and cols for prior and alphas
      
      # the prior row
      priorRow <- max(which(PCAT_ESSMs_SI_NDI$priorG >= essmPrior))
      # can be used with all tables
      
      
      NDINSRCol <- switch(as.character(PCATAlphaT),
                          "0.01"="a01",
                          "0.1"="a10",
                          "0.05"="a05",
                          "otherwise: last" )
      
      DISRCol <- switch(as.character(PCATAlphaD),
                        "0.01"="a01",
                        "0.1"="a10",
                        "0.05"="a05",
                        "otherwise: last" )
      
    }
    
    # single issue cutscores
    GTNDI <- PCAT_ESSMs_SI_NDI[,NDINSRCol][priorRow]
    GTDI <- PCAT_ESSMs_SI_DI[,DISRCol][priorRow]
    STDIc <- PCAT_ESSMs_SI_DI_St[,DISRCol][priorRow]
    
    # multiple issue cutscores
    STDI <- PCAT_ESSMs_MI_SR[,DISRCol][priorRow]
    STNDIc <- PCAT_ESSMs_MI_NSR[,NDINSRCol][priorRow]
    
    # make a vector of cutscores for the decision rules
    cutScores <- c(GTNDI=GTNDI, 
                   GTDI=GTDI, 
                   STDIc=STDIc, 
                   STDI=STDI, 
                   STNDIc=STNDIc)
    
    # print(cutscores)
    
  }
  
  ######### auto-select the PCAT decision rule ##########
  
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
    outputRule <- PCATDecisionRule
    
  }
  
  ######### select the PCAT decision rule ###########
  
  if(PCATDecisionRule == "auto" || grepl("auto",  outputRule)) {
    
    # PCATDecisionRule <- "auto"
    outputRule <- PCATDecisionRule
    
    # March 12, 2024 modified to use the same ESS TSR for DLDT samples
    # comparing LXCAT sensors to the full sensor array
    
    if(PCATDecisionRule == "auto") {
      PCATDecisionRule <-  ifelse(pairwiseResult['result'] == "sig",
                                  "pSSR",
                                  "TSR" )
      outputRule <- paste0(PCATDecisionRule, " (auto-selected)")
    }
    names(PCATDecisionRule) <- NULL
    
    # print(PCATDecisionRule)
    # print(outputRule)
    
  } else {
    
    outputRule <- PCATDecisionRule
    # print(PCATDecisionRule)
    # print(outputRule)
    
  }
  
  ################ PCAT Classification (result) #################
  
  
  {
    
    # cutScores <- c(GTNDI=GTNDI, GTDI=GTDI, STDIc=STDIc, STDI=STDI, STNDIc=STNDIc)
    
    # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
    
    # the same cutscores will be submitted to all decision rules
    
    GTRResult <- GTRFn(totalScore=grandTotal, 
                       RQNames=uniqueRQs,
                       cutScores=cutScores,
                       flip=FALSE )
    
    SSRResult <- pSSRFn(subtotalScores=subtotalScores, 
                       cutScores=cutScores,
                       flip=FALSE )
    
    TSRResult <- TSRFn(totalScore=grandTotal, 
                       subtotalScores=subtotalScores, 
                       cutScores=cutScores,
                       flip=FALSE )
    
    FZRResult <- FZRFn(grandTotal=grandTotal, 
                       subtotalScores=subtotalScores, 
                       cutScores=cutScores)
    
    pSSRResult <- pSSRFn(subtotalScores=subtotalScores, 
                         cutScores=cutScores,
                         flip=FALSE )
    
    pTSRResult <- eTSRFn(totalScore=grandTotal, 
                          subtotalScores=subtotalScores, 
                          cutScores=cutScores, 
                          flip=FALSE)
    
    if(!exists("PCATDecisionRule")) PCATDecisionRule <- "SSR"
    
    PCATTestResult <- switch(PCATDecisionRule,
                             "TSR"=TSRResult$testResult,
                             "pTSR"=pTSRResult$testResult,
                             "SSR"=SSRResult$testResult,
                             "pSSR"=pSSRResult$testResult,
                             "GTR"=GTRResult$testResult,
                             "FZR"=)
    
    PCATQuestionResults <- switch(PCATDecisionRule,
                                  "TSR"=TSRResult$subtotalResults,
                                  "pTSR"=pTSRResult$subtotalResults,
                                  "SSR"=SSRResult$subtotalResults,
                                  "pSSR"=pSSRResult$subtotalResults,
                                  "GTR"=GTRResult$subtotalResults)
    
    resultUsing <- switch(PCATDecisionRule,
                          "TSR"=TSRResult$resultUsing,
                          "pTSR"=pTSRResult$resultUsing,
                          "SSR"=SSRResult$resultUsing,
                          "pSSR"=pSSRResult$resultUsing,
                          "GTR"=GTRResult$resultUsing)
    names(resultUsing) <- NULL
    
  }
  
  ################## PCAT Multinomial Reference Model ###################
  
  {
    
    # ESSMRefDir <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # ESSM_simple_GT <- read.csv(paste0(ESSMRefDir, "ESSM_simple_GT.csv"),
    #                            stringsAsFactors=FALSE)
    # ESSM_simple_ST <- read.csv(paste0(ESSMRefDir,
    #                                   "ESSM_simple_ST.csv"),
    #                            stringsAsFactors=FALSE)
    
    # private function to look up the Bayes Factor
    
    scoreRowFn <- function(score=0, DAT=ESSM_simple_GT, tableCol="score") {
      # private function to get the score row using the PCAT reference table
      # score is the numerical score for which to get the table row
      # DAT is a data frame
      # tableCol is the name of the column that holds the vector of possible score
      if(score >= max(DAT[,tableCol])) return(nrow(DAT))
      if(score <= min(DAT[,tableCol])) return(1)
      max(which(DAT[,tableCol] <= score))
    }
    
    # look up the odds (Bayes Factor)
    GTOdds <- PCAT_ESS_2RQ$odds[scoreRowFn(score=grandTotal, DAT=PCAT_ESS_2RQ)]
    
    STOdds <- PCAT_ESS_1RQ$odds[scoreRowFn(minSubtotalScore, PCAT_ESS_1RQ)]
    
    useInV <- ifelse(minSubtotalScore <= 0, FALSE, TRUE) 
    
    # private function to calculate a multiplicity correction for subtotal odds
    mutiplicityOddsFn <- function(odds=1.38, n=3, inv=FALSE) {
      # R function to calculate a multiplicity correction for an odds ratio
      # odds can be a whole number or decimal
      # n is the number of simultaneous odds or decisions
      # inv will invert the calculation
      ###
      # p <- odds / (1 + odds)
      # mp <- 1- ( 1 + exp( log(p /(1-p)) / n ) )^-1
      if(isTRUE(inv)) n <- 1/n
      mp <- 1 - ( 1 + exp( log(odds) / n ) )^-1
      oddsM <- mp / (1-mp)
      return(oddsM)
    }
    
    STOddsC <- mutiplicityOddsFn(STOdds, n=2, inv=useInV)
    
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
  
  ####### calculate the PCAT posterior odds and posterior probability #######
  
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
                       12,
                       24 )
    
    RQs <- ifelse(resultUsing == "lowest subtotal",
                  1,
                  2 ) 
    
    # source(paste0(RPath, 'ClopperPearsonBinomialCI.R'), echo=FALSE)
    CPLowerLimit <- clopperPearsonFn(p=postProb, 
                                     RQs=RQs,
                                     maxCharts=4,
                                     PLE=TRUE,
                                     n=maxScore, 
                                     a2=thisAlpha, 
                                     odds=TRUE )[1]
    
    CPLowerLimit <- round(CPLowerLimit, 3)
    names(CPLowerLimit) <- NULL
    
    ClopperPearsonInterval <-  c(CPLowerLimit=CPLowerLimit, 
                                 thisAlpha=thisAlpha)
    
  }
  
  ########################### PCAT output section ########################
  
  # output table functions
  # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
  
  ######## PCAT measurements data frame 
  
  {
    
    PCATMeasurementSensors <- c("AutoEDA", 
                                "PLE")
    
    PCATMeasurementSensors <- 
      PCATMeasurementSensors[PCATMeasurementSensors %in% 
                               RqCqDFSeries$sensorName]
    
    measurementsDF <-
      measurementTableFn(RqCqDFSeries=RqCqDFSeries,
                         useSensors=PCATMeasurementSensors,
                         decimals=3,
                         makeDF=makeDF,
                         saveCSV=saveCSV )
    # View(measurementsDF)
    
  }
  
  ############# PCAT score sheet for the series
  
  scoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                               useSensors=PCATSensors2,
                               scoreType="ESSScore",
                               decimals=0,
                               DLSTType=DLSTType,
                               outputName="PCATScoresheetDF",
                               makeDF=makeDF,
                               saveCSV=TRUE)
  
  ################ PCAT series totals
  
  seriesTotalsDF <- seriesTotalsFn(scoreSheetDF=scoreSheetDF,
                                   outputName="PCATSeriesTotalsDF",
                                   aggType="sum",
                                   weightingCoefs=NULL,
                                   aggMethod="between",
                                   makeDF=makeDF,
                                   saveCSV=TRUE)
  
  ########## PCAT chart totals, including RQ subtotals and chart subtotals
  
  chartTotalsDF <- chartTotalsFn(scoreSheetDF=scoreSheetDF,
                                 outputName="PCATChartTotalsDF",
                                 aggType="sum",
                                 weightingCoefs=NULL,
                                 makeDF=makeDF,
                                 saveCSV=saveCSV)
  
  ################ PCAT sensor subtotals for the series
  
  sensorTotalsDF <- sensorSubtotalsFn(scoreSheetDF=scoreSheetDF,
                                      outputName="PCATSensorTotalsDF",
                                      aggType="sum",
                                      makeDF=makeDF,
                                      saveCSV=TRUE)
  
  ######## get the RC ratios for output
  
  RCScoreSheetDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                 useSensors=PCATMeasurementSensors,
                                 scoreType="RCScore",
                                 decimals=3,
                                 outputName="RCScoresheetDF",
                                 makeDF=makeDF,
                                 saveCSV=FALSE)
  
  ########### get the CQ selection for output
  
  CQSelectionDF <- scoreSheetFn(RqCqDFSeries=RqCqDFSeries, 
                                useSensors=PCATMeasurementSensors,
                                scoreType="CQName",
                                decimals="non-numeric",
                                outputName="CQSelectionDF",
                                makeDF=makeDF,
                                saveCSV=FALSE)
  
  ############### construct a list to hold the PCAT result #################
  
  outputListName <- paste(examName, seriesName, "PCATOutputList", sep="_")
  
  PCATOutputList <- list(PCAT="Preliminary Credibility Assessment Test",
                         examName=examName,
                         seriesName=seriesName,
                         PCATResult=PCATTestResult,
                         PCATQuestionResults=PCATQuestionResults,
                         PCATDecisionRule=outputRule,
                         PCATResultUsing=resultUsing,
                         PCATRQNames=uniqueRQs,
                         PCATPairwiseResult=pairwiseResult,
                         PCATGrandTotal=grandTotal,
                         PCATMinSubtotal=minSubtotalScore,
                         PCATMinRQ=minRQName,
                         PCATSubtotalScores=subtotalScores,
                         PCATQuestions=uniqueQuestions,
                         PCATPriorOdds=priorOdds,
                         PCATPriorProbability=PCATPrior,
                         PCATPostProb=postProb,
                         PCATPostOdds=postOdds,
                         PCATBayesFactor=bayesFactor,
                         PCATAlphas=PCATAlphas,
                         PCATCPLowerLimitPostOdds=CPLowerLimit,
                         PCATCPAlpha=thisAlpha,
                         PCATCutscores=cutScores,
                         PCATSensors=PCATMeasurementSensors,
                         PCATWarning=PCATWarning,
                         PCATQuestionSequence=questionSequenceDF,
                         PCATCQSelection=CQSelectionDF,
                         PCATLogRCRatios=RCScoreSheetDF,
                         PCATScoreSheetDF=scoreSheetDF,
                         PCATChartTotalsDF=chartTotalsDF,
                         PCATSeriesTotalsDF=seriesTotalsDF,
                         PCATSensorTotalsDF=sensorTotalsDF, 
                         PCATMeasurementDF=measurementsDF )
  
  #### save the list to the globad env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, PCATOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "PCATOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      PCATOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  #### visible output ####
  
  if(isTRUE(DLSTType)) {
    RqCqDFSeries$chartName <- saveChartNames
    RqCqDFSeries$Label <- saveEventLabels
  }
  
  return(RqCqDFSeries)

} # end PCATScoresFn


