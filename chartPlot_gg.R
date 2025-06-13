# plot an entire polygraph chart in one graphic
# Raymond Nelson
# raymond.nelson@gmail.com
#
# sampling rate is 30cps
# 
#
# ggplot runs in the global environment and cannot run in a function
#
####


{
  library(stringr)
  
  
  library(ggplot2)
  library(grid)
}
  
{
  # this path is prepended to the file path before sourcing a script
  # mac
  if(!exists("RPath")) {
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
  }
  # windows
  # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
}

{
  
  # source the list of excluded events so measurements are not plotted for these
  source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)
  excludeQuestions
  
  
  source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  # source this after the NCCAASCII_init.R script
  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  source(paste0(RPath, 'cardioCorrelation.R'), echo=FALSE)
  
  source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  source(paste0(RPath, 'sigProc_extra.R'), echo=FALSE)
  source(paste0(RPath, 'rbpfProb.R'), echo=FALSE)
  source(paste0(RPath, 'EDADataCheck.R'), echo=FALSE)
  source(paste0(RPath, 'pneumoCheck.R'), echo=FALSE)
  
  source(paste0(RPath, 'cardioLeakDown.R'), echo=FALSE)
  
  # source(paste0(RPath, 'EDACheck.R'), echo=FALSE)
  
  
  # run the init script to set the plot parameters
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  # source(paste0(RPath, 'chartPlot_init.R'), echo=FALSE)
  
}


###############  environment variables  ###################################

{
  
  # to control the print output
  showNames <- TRUE
  # output <- FALSE
  
  outputChartFileName <- "_chartPlot.pdf"
  
  printPlot <- TRUE
  
  # set to TRUE to make separate .pdf graphic for each chart
  separateCharts <- FALSE

}
 

########### analysis and display info ############

{
  
  #### show stimulus events ####
  
  {  
    showStimulusLines <- TRUE
    showShadedAreas <- TRUE
    showEventLabels <- TRUE
    
    # showStimulusLines <- FALSE
    # showShadedAreas <- FALSE
    # showEventLabels <- FALSE
  }
  
  #### show TIME SERIES DATA ####
  
  {
    
    showData <- TRUE
    # showData <- FALSE
    
    showPneumoData <- TRUE
    # showPneumoData <- FALSE
    
    showCardioData <- TRUE
    # showCardioData <- FALSE
    
    showPLEData <- TRUE
    # showPLEData <- FALSE
    
    showActivityData <- TRUE
    # showActivityData <- FALSE
    
    showPTTPTT <- TRUE
    # showPTTPTT <- FALSE
    
    showEDAData <- TRUE
    # showEDAData <- FALSE
    
    showAutoEDA <- TRUE
    # showAutoEDA <- FALSE
    
    showManualEDA <- TRUE
    showManualEDA <- FALSE
    
    # Jan 17, 2023
    # second EDA sensor
    showManualEDA2 <- FALSE
    
    showFCData <- FALSE 
    
    showEDAComplexity <- FALSE
    showEDADuration <- FALSE
    
    # to work with the auto EDA output from LXSoftware
    showEA <- FALSE
    
    # show tracing baselines
    showBaselines <- FALSE
    
    configLXCAT <- TRUE
    configLXCAT <- FALSE
    
    if(configLXCAT) {
      # LXCAT ggplot settings
      showPneumoData <- FALSE
      showCardioData <- FALSE
      showEDAData <- TRUE
      showManualEDA <- FALSE
      showAutoEDA <- TRUE
      showPLEData <- TRUE
      showActivityData <- FALSE
    }
    
  }
  
  #### show SCORES ####
  
  {
    
    # for ESS and other scores
    
    showScores <- TRUE
    # showScores <- FALSE
    
    if(showScores) {
      outputChartFileName <- paste0("_SCORES", outputChartFileName)
    } else {
      outputChartFileName <- paste0("_NO_SCORES", outputChartFileName)
    }
    
    showShadedAreas <- TRUE
    # showShadedAreas <- FALSE
    
    # also changes the showMeasurements (lines)
    
    showPneumoScores <- TRUE
    showEDAScores <- TRUE
    showCardioScores <- TRUE
    showPLEScores <- TRUE
    
    showFCScores <- FALSE
    
    # includePLEData is set workFlow_init.R script
    if(!isTRUE(includePLEData)) showPLEScores <- FALSE
    
    showRCRatio <- TRUE
    
    showIPZScores <- FALSE
    
    showOSS3Scores <- FALSE
    
    # showOSS2Scores <- TRUE
    showOSS2Scores <- FALSE
    
    showCQSelection <- TRUE
    
    showRankValues <- showScores
    
    ## question intervals
    showQuestionIntervals <- TRUE
    # showQuestionIntervals <- FALSE
    
    ## cardio rate caliper
    showCardioRateCaliperVals <- TRUE
    # showCardioRateCaliperVals <- FALSE
    
    ## respiration caliper values
    showRespirationCalipers <- TRUE
    # showRespirationCalipers <- FALSE 

    if(!(showScores)) showQuestionIntervals <- showScores
    
  }
  
  #### show WARNINGS and other info####
  
  {
    
    
    showWarnings <- TRUE
    # showWarnings <- FALSE
    
    if(!showScores) showWarnings <- FALSE
    
    inclChartName <- TRUE
    # inclChartName <- FALSE
    
  }
  
  
  #### show ARTIFACTS #####
  
  
  {
    
    showArtifacts <- TRUE
    # showArtifacts <- FALSE
    
    if(!showScores) showArtifacts <- FALSE
    
    # showArtifacts <- ifelse(showScores, TRUE, FALSE)
    
    # August 26, 2023
    showPneumoArtifacts <- TRUE
    showEDAArtifacts <- TRUE
    showCardioArtifacts <- TRUE
    
    showPLEArtifacts <- FALSE
    showActivityArtifacts <- TRUE
    
    # if(!showScores) {
    #   showArtifacts <- FALSE
    # }
    
  }
  
  #### select measurement lines ####
  
  {
    
    # measurement lines
    showMeasurements <- showScores
    # showMeasurements <- TRUE
    
    showExtractionVals <- showScores
    
    showPLEMeasurement <- showPLEScores
    if(!isTRUE(includePLEData)) showPLEMeasurement <- FALSE
    
  }
  
  #### select scores ####
  
  # selectScores can be "auto" "miritello" "raskin", "rank" "integer" "RC" "ipZ" "OSS2"
  selectScores <- "auto"
  # "auto" will use "ESS-M" or "rank" 
  # selectScores <- "rank"
  
  # source the init script to reset the options above
  # source(paste0(RPath, 'chartPlot_init.R'), echo=FALSE)
  
  #### PCASS/PCAT format ####
  
  # use TRUE to exclude non-PCAT sensors
  PCASSFormat <- FALSE
  
}

##########  functions to format numeric scores ##########

{
  
  numFormat2Fn <- function(x) {
    # function to remove leading zeros and keep 2 decimals
    # vectorized already and needs no loop
    outVector <- x
    useIdx <- which(!(is.na(x) | x == ""))
    x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.2f", as.numeric(x[useIdx]))) )
    outVector[useIdx] <- x2
    return(outVector)
  }
  
  numFormat3Fn <- function(x) {
    # function to remove leading zeros and keep 3 decimals
    # vectorized already and needs no loop
    outVector <- x
    useIdx <- which(!(is.na(x) | x == ""))
    x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.3f", as.numeric(x[useIdx]))) )
    outVector[useIdx] <- x2
    return(outVector)
  }
  
  numFormatIntFn <- function(x) {
    # function to format values as integers
    # vectorized already and needs no loop
    outVector <- x
    useIdx <- which(!(is.na(x) | x == ""))
    x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.1f", as.numeric(x[useIdx]))) )
    outVector[useIdx] <- x2
    return(outVector)
  }
  
}


#########################  select exams for printing  ##################################

{
  
  # get exam names from the _Data data frames
  uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  # uniqueExams <- uniqueExams[11]
  
  
  # work with a single segment 
  # instead of iterating over all exams in the global environment
  # use "ALL" or an integer for each of these
  getSegment <- FALSE
  examNum <- 1
  seriesNum <- 2
  chartNum <- 1
  segmentNum <- "ALL"
  
  
  if(getSegment == TRUE) {
    if(examNum!="ALL") uniqueExams <- uniqueExams[examNum] 
  } 
  
}

####################  iterate over the exams  #####################


# loop over each exam in the list and plot the charts
i=1
for(i in 1:length(uniqueExams)) {
  
  {
    
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    # searchString <- paste0(examName, "_Data")
    
    # get the time series data
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    # get the measurement data frame
    if(isTRUE(showScores)) {
      measurementDFName <- paste0(examName, "_Measurements")
      measurementDF <- get(measurementDFName, pos=1)
      # View(measurementDF)
    }
    
    if(showNames==TRUE) {
      print(paste("exam:", examName))
      print(paste(i, "of", length(uniqueExams)))
    } 
    
    # limit the data range for printing 
    # Sep 15, 2024 - to limit odd print output with severe cardio artifacts
    {
      # cols 11 to 18 are the recorded data
      # Cols 19 to 125 are processed data columns 
      i=19
      for(j in 19:125) {
        # plot.ts(examDF[c(1:500),j])
        fixRowsMax <- which(examDF[,j] >= 950)
        examDF[fixRowsMax,j] <- 950
        fixRowsMin <- which(examDF[,j] <= -950)
        examDF[fixRowsMin,j] <- -950
      }
    }
    
    # get the names of all unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    ######################   insert some operations here   ################
    
    ###################################################################
    
    if(printPlot==TRUE && separateCharts==FALSE) {
      # all series and all charts in one .pdf
      graphics.off()
      dev.new()
      pdf(paste(examName, outputChartFileName, sep=""), 
          height=5.75, 
          width=11)
    } 
    
    if(getSegment == TRUE) {
      if(seriesNum!="ALL") uniqueSeries <- uniqueSeries[seriesNum]
    }
    
  }
  
  #####################  iterate over the series  ################
  
  # loop over each unique series
  j=1
  for(j in 1:length(uniqueSeries)) {
  
    {
      
      seriesName <- uniqueSeries[j]
      
      # get the time series for each unique series
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      # get the names of unique charts in the seires
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      if(getSegment == TRUE) {
        if(chartNum!="ALL") uniqueCharts <- uniqueCharts[chartNum]
      }
      
    }
    
    #### iterate over the charts ####
    
    # loop over each chart in the series 
    k=1
    for(k in 1:length(uniqueCharts)) {
      
      {
        
        chartName <- uniqueCharts[k]
        
        # get the data frame with the time series data for each chart
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        # skip short charts less than 10 seconds
        if(nrow(chartDF)<1200) next()
        
        if(showNames==TRUE) print(paste("plotting", chartName))
        
        # make a vector of event names
        # eventNames <- toupper(chartDF$eventLabel[which(chartDF$eventLabel!="")])
        eventNames <- toupper(chartDF$Label[which(chartDF$eventLabel!="")])
        
        if(getSegment == TRUE) { 
          if(segmentNum!="ALL") eventNames <- eventNames[segmentNum] 
        }
        
        # # remove excess time series data prior to X announcement
        # XannouncementRow <- which(chartDF$eventLabel==eventNames[1])[1]
        # if(XannouncementRow > 10*cps) {
        #   chartDF <- chartDF[(XannouncementRow-(10*cps)+1):nrow(chartDF),]
        # }
        
        # # remove excess time series data after XX announcement
        # XXannouncementRow <- which(toupper(chartDF$eventLabel)==
        #                              eventNames[length(eventNames)])
        # XXannouncementRow <- XXannouncementRow[length(XXannouncementRow)]
        # if(nrow(chartDF) > XXannouncementRow+(25*cps)) {
        #   chartDF <- chartDF[1:(XXannouncementRow+(25*cps)-1),]
        # }
        
        # get the first and last stimulus events
        if(length(eventNames)==0) {
          print("no stimulus events")
          # next()
          firstEvent=1
          lastEventEnd=nrow(chartDF)
        } else {
          firstLastEvents <- getFirstLastEventFn(x=chartDF)
          firstEvent <- firstLastEvents[1]
          lastEventEnd <- firstLastEvents[2]
        }
        
      }
      
      ################# initialize the pdf graphic device ###################
      
      {
        
        if(printPlot==TRUE && separateCharts==TRUE) {
          # to make a separate .pdf for each chart
          graphics.off()
          dev.new()
          pdf(paste(examName, seriesName, chartName, outputChartFileName, sep=""), 
              height=5.75, 
              width=11)
        }
        
        # if(printPlot == TRUE && separateCharts==TRUE) {
        #   pdf(paste(examName, seriesName, chartName, outputChartFileName, sep=""), 
        #       height=5, 
        #       width=8)
        # }
        
        # make the plot title to include the exam and chart name
        plotTitle <- paste(examName, seriesName, chartName, sep="_")
        
      }
      
      ################  check for additional channels  #################
      
      {
        
        # set a variable to determine if the eCardio data exist
        inclECardio <- ifelse(sum(pmatch(names(chartDF), 
                                         "c_eCardio", nomatch=0))>0,
                              TRUE,
                              FALSE)
        
        # set a variable to determine if the FC forearm or finger cuff data exist
        inclFC <- ifelse(sum(pmatch(names(chartDF), 
                                    "c_FC", nomatch=0))>0,
                         TRUE,
                         FALSE)
        
        # set a variable to determine if PLE data exist in the current chart
        inclPLE <- ifelse(sum(pmatch(names(chartDF), 
                                     "c_PPG1", nomatch=0)) > 0,
                          TRUE,
                          FALSE)
        
        # set a variable to determine if a second EDA sensor exists
        inclEDA2 <- ifelse(sum(pmatch(names(chartDF),
                                      "c_EDA2", nomatch=0)) > 0,
                           TRUE,
                           FALSE)
        
      }
      
      ################### select the sensors for the plot ##############
      
      {
        # if(!isTRUE(includePLE)) { inclPLE <- FALSE }
      }
      
      ################ set some warnings for the chart #################
      
      {
        
        # set a warning for the X/XX announcements 
        XWarning <- ifelse( !("X" %in% eventNames) & !("XX" %in% eventNames),
                            "MISSING X AND XX ANNOUNCEMENTS",
                            ifelse(!("X" %in% eventNames),
                                   "MISSING X ANNOUNCEMENT",
                                   ifelse(!("XX" %in% eventNames),
                                          "MISSING XX ANNOUNCEMENT",
                                          "none") ) )
        
        if(XWarning == "none") {
          XWarning <- ifelse( length(which(eventNames %in% "X")) > 1 | 
                                length(which(eventNames %in% "XX")) > 1,
                              "REPEATED X/XX ANNOUNCEMENT",
                              ifelse( which(eventNames %in% "X") != 1,
                                      "X ANNOUNCEMENT - NOT AT RECORDING ONSET",
                                      ifelse( which(eventNames %in% "XX") != 
                                                length(eventNames),
                                              "XX ANNOUNCEMENT - NOT AT END",
                                              "none") ) )
        } 
        
        # set a variable for activity sensor warning
        activityWarning <- ifelse( !("c_Move1" %in% names(examDF)),
                                   "MISSING ACTIVITY SENSOR DATA",
                                   ifelse(sd(chartDF$c_Move1) == 0,
                                          "MISSING ACTIVITY SENSOR DATA",
                                          "none" ) )
        # June 19, 2024
        # coerce hide the activity sensor 
        # activityWarning <- "MISSING ACTIVITY SENSOR DATA"
        
      }
      
      ############# reset the  question pacing #################
      
      questionPaceWarning <- "none"
      
      #################  check for data problems  ##################
      
      {
        
        if(chartDF$Cardio1[1] != -9.9) {
          ## check the cardio pulse rate ##
          
          # was buffer=3 4-14-2017
          cardioRate <- ratePerMin(MASmooth(x=chartDF$c_Cardio1, y=5, times=1),
                                   buffer=5,
                                   peaks="upper",
                                   lowPass=TRUE)
          
          if(cardioRate < 50 | cardioRate > 110) {
            cardioRateWarning <- paste("CARDIO RATE", 
                                       cardioRate, 
                                       "OUTSIDE NORMAL RANGE")
          } else cardioRateWarning <- paste("CARDIO RATE:", cardioRate)
        } else {
          cardioRateWarning <- "none"
        }
        
        if(chartDF$Cardio1[1] != -9.9) {
          ## recalculate the cardio to check for arrhythmia ##
          
          cardioRate1 <- ratePerMin(MASmooth(x=chartDF$c_Cardio1, y=2, times=1),
                                    buffer=9,
                                    peaks="upper",
                                    lowPass=TRUE)
          cardioRate2 <- ratePerMin(MASmooth(x=chartDF$c_Cardio1, y=2, times=1),
                                    buffer=9,
                                    peaks="lower",
                                    lowPass=TRUE)
          # compare the systolic and diastolic cardio rates 
          # and set the arrhythmia warning if necessary
          if(exp(-abs(log(cardioRate1/cardioRate2))) <= .95) {
            cardioRateWarning <- paste0(cardioRateWarning, 
                                        ", possible cardio arrhythmia")
          } 
        } else {
          cardioRateWarning <- "none"
        }
        
        if(chartDF$UPneumo[1] != -9.9) {
          ## get the respiration rate ##
          
          pneumoRate  <- ratePerMin(chartDF$c_UPneumoSm,
                                    buffer=40,
                                    peaks="upper",
                                    lowPass=TRUE)
          if(is.na(pneumoRate)) {
            pneumoRateWarning <- NA
          } else if(pneumoRate < 8 | pneumoRate > 24) {
            pneumoRateWarning <- paste("RESPIRATION RATE", 
                                       pneumoRate, 
                                       "OUTSIDE NORMAL RANGE")
          } else {
            pneumoRateWarning <- paste("RESPIRATION RATE", pneumoRate)
          }
        } else {
          pneumoRateWarning <- "none"
        }
        
        if(chartDF$Cardio1[1] != -9.9) {
          ## check for RBPF ##
          
          # first rese the RBPF warning
          rbpfMsg <- "none"
          
          # source(paste0(RPath, 'rbpfProb.R'), echo=FALSE)
          
          # check for respiratory blood pressure fluctuation
          if(isTRUE(showScores) && !isTRUE(PCATFormat)) { 
            rbpfMsg <- rbpfProbFn(x=chartDF) 
          }
          if(rbpfMsg != "none") {
            rbpfWarning <- rbpfMsg
            # can use the RBPF message to select the cardio ma line if RBPF
          } else {
            # rbpfWarning <- "none" # rbpfMsg
            rbpfWarning <- rbpfMsg
          }
        } else {
          rbpfWarning <- "none"
        }
        
        if(chartDF$Cardio1[1] != -9.9) {
          ## check for leaking or descending cardio data ##
          if(nrow(chartDF) > 600) {
            cardioLeakMsg <- cardioLeakFn(x=chartDF$c_CardioMA)
          } else cardioLeakMsg  <- "none"
        } else {
          cardioLeakMsg  <- "none"
        }
        
        {
          ## check for unresponsive EDA ##
          
          edaWarning <- "none"
          dataWarning <- "none"
          # dataCheckFn(x=chartDF$c_AutoEDA, 
          #             sec=10, 
          #             times=30, 
          #             omit=10, 
          #             firstRow=NULL, 
          #             lastRow=NULL, 
          #             sVal=200 )
          if(dataWarning != "none") edaWarning <- "unresponsive EDA"
        }
        
      }
      
      ###########   check for unresponsive pneumo data   ###############
      
      {
        
        # pneumoDataWarning <- "none"
        pneumoDataWarning <- pneumoCheckFn(x1=chartDF$c_UPneumoSm,
                                           x2=chartDF$c_LPneumoSm,
                                           sec=5,
                                           times=30,
                                           omit=10,
                                           firstRow=NULL,
                                           lastRow=NULL)
        
      }
      
      ################ scaling and offsetting ####################
      
      {
        # now done by the scaleOffsetData.R script prior to analysis
      }
      
      ############    make the plot   ################
      
      {
        g <- ggplot()
        
        # ggplot normally executes in the global environment
        # this means that ggplot is not easily called from within a function 
      }
      
      ############ horizontal and vertical chart divisions ###################
      
      {
        
        # g <- g + 
        #   scale_x_continuous(breaks = NULL) + 
        #   scale_y_continuous(breaks = NULL)
        # + theme( # remove the vertical grid lines
        #   panel.grid.major.x = element_blank(),
        #   panel.grid.major.y = element_blank() 
        #   )
        
        chartDivH <- seq(-937.5, 937.5,by=62.5)
        chartDivV <- seq(150, ((nrow(chartDF)%/%150)*150), by=150)
        # no need for a loop because geom_vline is vectorized
        g <- g + geom_vline(aes(xintercept=as.numeric(chartDivV)), color="grey", linewidth=.3, alpha=.2)
        g <- g + geom_hline(aes(yintercept=as.numeric(chartDivH)), color="grey", linewidth=.3, alpha=.2)
        
        # g <- g +  
        #   scale_x_continuous(breaks = chartDivV) + 
        #   scale_y_continuous(breaks = chartDivH)
        
      }
      
      ############ tracing baselines ##################
      
      if(isTRUE(showBaselines)) {
        # show the yOffset line for each recording sensor
        # remove the PLE baseline if PLE data are missing
        if(inclPLE==FALSE) {
          g <- g + geom_hline(aes(yintercept=yOffset[-5]), color="brown", linewidth=.15)
        } else g <- g + geom_hline(aes(yintercept=yOffset), color="brown", linewidth=.15)
      }
      
      #### Cardio recentering Events ####
      
      {
        # not used 9-2016
        # if(!is.null(reCenterEventsDn)) {
        #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsDn)), color="yellow")
        # }
        # if(!is.null(reCenterEventsUp)) {
        #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsUp)), color="green")
        # }
        # # if(!is.null(reCenterEvents)) {
        # #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEvents)), color="orange")
        # # }
      }

      ################  stimulus lines and shaded areas   ##################
            
      if(length(eventNames) != 0) {
        
        if(showStimulusLines==TRUE) {
          
          ############### stimulus events  ##############
          
          eventIndices <- which(chartDF$Events != "")
          chartDF$eventLabel[which(chartDF$Events != "")]
          # no need for a loop because geom_vline is vectorized
          g <- g + geom_vline(aes(xintercept=as.numeric(eventIndices)), color="black", alpha=.7)
          
          # # another way to make the stimulus event lines one by one
          # # onsetRow <- segOnsetRow
          # onsetRow <- which(chartDF$Events=="onsetRow") # [1]
          # g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)), lineWidth=.33)
          # # EDA latency
          # # EDALatRow <- onsetRow+(EDALat*cps)
          # # g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="red") # grey80
          # # offset line
          # offsetRow <- which(chartDF$Events[(onsetRow[1]:nrow(chartDF))]=="offsetRow") # [] + segOnsetRow - 1
          # g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)), lineWidth=.33)
          # # answer line
          # # answerRow <- which(chartDF$Events[(onsetRow[1]:nrow(chartDF))]=="answerRow")[] + segOnsetRow - 1
          # answerRow <- which(chartDF$Events=="answerRow")
          # g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), lineWidth=.5, color="black") # black
          # # end of response onset window
          # # ROWEndRow <- which(chartDF$Events[answerRow:nrow(chartDF)]=="answerRow")[1] + answerRow - 1 + (ROWEnd*cps)
          # # ROWEndRow <- answerRow + (ROWEnd*cps) - (segOnsetRow - 1)
          # # ROWEndRow <- answerRow + (ROWEnd*cps)
          # # g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="blue") # grey80
          # # end of scoring window
          # # segEndRow <- segOnsetRow+(measuredSeg*cps) - 1
          # # if(segEndRow > nrow(chartDF)) segEndRow <- (nrow(chartDF))
          # # endRow <- which(chartDF$Events=="onsetRow")[]+(measuredSeg*cps)
          # # g <- g + geom_vline(aes(xintercept=as.numeric(endRow)), color="blue") # grey70
          
        } # end if showStimulusLines==TRUE
        
        ########### SHADED AREAS for stimulus questions ############
        
        {
          
          # eventNames
          stimOnset <- which(chartDF$Events=="onsetRow")
          stimOffset <- which(chartDF$Events=="offsetRow")
          answerRow <- which(chartDF$Events=="answerRow")
          
          # <> Aug 4, 2020
          RQStimOnset <- which(chartDF$eventLabel %in% eventNames[grep("R", eventNames)])
          # exclude sacrifice "SR" and "RS" events
          RQStimOnset <-
            RQStimOnset[!(chartDF$eventLabel[RQStimOnset] %in% c("SR", "RS"))]
          # repeated events
          RQStimOnset <- RQStimOnset[!grepl("A",chartDF$eventLabel[RQStimOnset])]
          # excluded events
          RQStimOnset <- 
            RQStimOnset[!(chartDF$eventLabel[RQStimOnset] %in% excludeEvents)] 
        
          
          # <> Aug 4, 2020
          CQStimOnset <- which(chartDF$eventLabel %in% eventNames[grep("C", eventNames)])
          # exclude "CT" (cleared throat) annotations
          CQStimOnset <- 
            CQStimOnset[chartDF$eventLabel[CQStimOnset] != "CT"]
          # repeated events
          CQStimOnset <- CQStimOnset[!grepl("A",chartDF$eventLabel[CQStimOnset])]
          # excluded events
          CQStimOnset <- 
            CQStimOnset[!(chartDF$eventLabel[CQStimOnset] %in% excludeEvents)] 
          
          
          
          RQStimEnd <- RQStimOnset+measuredSeg*cps
          CQStimEnd <- CQStimOnset+measuredSeg*cps
          
          
          
          # stimulus question shaded area
          g <- g + annotate("rect", 
                            xmin=as.numeric(stimOnset),
                            xmax=as.numeric(stimOffset),
                            ymin=yMin, 
                            ymax=yMax, 
                            alpha=.10, 
                            fill="grey10")
          
          # relevant question shaded area
          if(length(RQStimOnset) > 0 ) {
            g <- g + annotate("rect",
                              xmin=as.numeric(RQStimOnset),
                              xmax=as.numeric(RQStimEnd),
                              ymin=yMin,
                              ymax=yMax,
                              alpha=.05,
                              fill="red") 
          }
          
          # highlight the relevant question label
          if(length(RQStimOnset) > 0 ) {
            g <- g + annotate("rect",
                              xmin=as.numeric(RQStimOnset)-165,
                              xmax=as.numeric(RQStimOnset)+165,
                              ymin=yMin-50,
                              ymax=yMin+50,
                              alpha=.55,
                              fill="red") 
          }
          
          # comparison question shaded area
          if(length(CQStimOnset) > 0 ) {
            g <- g + annotate("rect",
                              xmin=as.numeric(CQStimOnset),
                              xmax=as.numeric(CQStimEnd),
                              ymin=yMin,
                              ymax=yMax,
                              alpha=.05,
                              fill="green")
          }
          
          # highlight the comparison question label
          if(length(CQStimOnset) > 0 ) {
            g <- g + annotate("rect",
                              xmin=as.numeric(CQStimOnset)-165,
                              xmax=as.numeric(CQStimOnset)+165,
                              ymin=yMin-50,
                              ymax=yMin+50,
                              alpha=.55,
                              fill="green")
          }
          
        }
        
        if(showShadedAreas==TRUE) {
          
          # scoring window shaded area
          g <- g + annotate("rect", 
                            xmin=as.numeric(stimOffset), 
                            xmax=as.numeric(stimOnset+(measuredSeg*cps)), 
                            ymin=yMin, 
                            ymax=yMax, 
                            alpha=.10, 
                            fill="blue")
          
          # latency shaded area
          g <- g + annotate("rect",
                            xmin=as.numeric(stimOnset),
                            xmax=as.numeric(stimOnset+(EDALat*cps)),
                            ymin=yMin,
                            ymax=yMax,
                            alpha=.75,
                            fill="yellow")
          
          # response onset window shaded area
          g <- g + annotate("rect",
                            xmin=as.numeric(stimOnset),
                            xmax=as.numeric(answerRow+(ROWEnd*cps)),
                            ymin=yMin,
                            ymax=yMax,
                            alpha=.10,
                            fill="blue")
          
        } # end if showShadedAreas==TRUE
        
        ################### question labels #####################
        
        if(showEventLabels==TRUE) {
          
          g <- g + annotate(geom="text", 
                            x=which(chartDF$eventLabel!=""), 
                            y=yMin, 
                            label=eventNames,
                            color="black", 
                            size=4)
          
        } # end if showEventLabels==TRUE
        
      } # end if length(eventNames) != 0
      
      
      ##############   ARTIFACTS   ###############
      
      if(showArtifacts==TRUE) {
        
        ######## Penumo artifacts ######## 
        
        if(showPneumoData && showPneumoArtifacts) {
          
          #### uppper pneumo artifacts
          
          # g <- g + annotate("point", x=which(chartDF$Pneumo_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$Pneumo_a=="Artifact")],
          #                   shape=4, size=3, color='black')
          g <- g + annotate("point", x=which(chartDF$UPneumo_a!="0"),
                            y=chartDF$c_UPneumoSm[which(chartDF$UPneumo_a!="0")],
                            shape=8, size=2, alpha=.5, color='black') # was "red"
          # g <- g + annotate("point", x=which(chartDF$UPneumoInh_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoInh_a=="Artifact")],
          #                   shape=4, size=2.5, color="black") # was "blue"
          # g <- g + annotate("point", x=which(chartDF$UPneumoExh_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoExh_a=="Artifact")],
          #                   shape=4, size=2.5, color="black") # was "green"
          # g <- g + annotate("point", x=which(chartDF$UPneumoMid_a=="Artifact"),
          #                   y=chartDF$c_UPneumoSm[which(chartDF$UPneumoMid_a=="Artifact")],
          #                   shape=4, size=2.5, color="black") # was "brown"
          
          #### lower pneumo artifacts
          
          # g <- g + annotate("point", x=which(chartDF$Pneumo_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$Pneumo_a=="Artifact")],
          #                   shape=4, size=3, color='black')
          g <- g + annotate("point", x=which(chartDF$LPneumo_a!="0"),
                            y=chartDF$c_LPneumoSm[which(chartDF$LPneumo_a!="0")],
                            shape=8, size=2, alpha=.5, color='black')
          # g <- g + annotate("point", x=which(chartDF$LPneumoInh_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoInh_a=="Artifact")],
          #                   shape=4, size=2.5, color="black")
          # g <- g + annotate("point", x=which(chartDF$LPneumoExh_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoExh_a=="Artifact")],
          #                   shape=4, size=2.5, color="black")
          # g <- g + annotate("point", x=which(chartDF$LPneumoMid_a=="Artifact"),
          #                   y=chartDF$c_LPneumoSm[which(chartDF$LPneumoMid_a=="Artifact")],
          #                   shape=4, size=2.5, color="black")
          
        } # end if showPneumoData
        
        ######## EDA artifacts ########
        
        if(isTRUE(showEDAData) && showEDAArtifacts) {
          
          g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="Artifact"),
                            y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="Artifact")],
                            shape=8, size=2, alpha=.5, color="red")
          
          
          
          # point
          # non-stimulus artfifacts
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact1c"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact1c")],
          #                   shape=20, size=3, alpha=.1, color="hotpink")
          
          # post-stimulus artifacts
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact1b"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact1b")],
          #                   shape=20, size=3, alpha=.1, color="orange")
          
          # prestimulus artifacts
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact1a"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact1a")],
          #                   shape=20, size=3, alpha=.15, color="red")
          
          
          
          # finger movement artifacts - sudden downward spike
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact2"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact2")],
          #                   shape=18, size=3, alpha=.75, color="brown")
          
          # point
          # g <- g + annotate("point", x=which(chartDF$AutoEDA_a=="artifact3"),
          #                   y=chartDF$c_AutoEDA[which(chartDF$AutoEDA_a=="artifact3")],
          #                   shape=18, size=3, alpha=.2, color="black")
          
          
          
          # vertical line
          # g <- g + annotate("segment",
          #                   x=which(chartDF$AutoEDA_a=="artifact"),
          #                   xend=which(chartDF$AutoEDA_a=="artifact"),
          #                   # y=chartDF$c_EDAMax [which(chartDF$AutoEDA_a=="artifact")],
          #                   # yend=chartDF$c_EDAFiltMax[which(chartDF$AutoEDA_a=="artifact")],
          #                   y=chartDF$c_AutoEDABase[which(chartDF$AutoEDA_a=="artifact")],
          #                   yend=chartDF$c_AutoEDAPeak[which(chartDF$AutoEDA_a=="artifact")],
          #                   color="red",
          #                   alpha=.1,
          #                   size=.1 )
          
        }
        
        
        ######## cardio artifacts ########
        
        if(isTRUE(showCardioData) && showCardioArtifacts) {
          
          # change in pulse amplitude
          # g <- g + annotate("point", x=which(chartDF$CardioMid_a=="ArtifactMinMaxAmp"),
          #                   y=chartDF$c_Cardio1[which(chartDF$CardioMid_a=="ArtifactMinMaxAmp")],
          #                   shape=8, size=6, color="black") # was shape=4 color="slateblue1"
          # was "red1"
          # cardio artifacts  also pulse amplitude artifacts
          g <- g + annotate("point", x=which(chartDF$Cardio1_a=="Artifact"),
                            y=chartDF$c_Cardio1[which(chartDF$Cardio1_a=="Artifact")],
                            shape=8, size=2, color="blue", alpha=.5)
          # systolic distance from mid
          # g <- g + annotate("point", x=which(chartDF$CardioSystolic_a=="ArtifactMaxAmp"),
          #                   y=chartDF$c_Cardio1[which(chartDF$CardioSystolic_a=="ArtifactMaxAmp")],
          #                   shape=8, size=4, color="blue")
          # diastolic distance from MA
          # g <- g + annotate("point", x=which(chartDF$CardioDiastolic_a=="ArtifactMinAmp"),
          #                   y=chartDF$c_CardioDiastolic[which(chartDF$CardioDiastolic_a=="ArtifactMinAmp")],
          #                   shape=4, size=8, color="blue") # was "blue4"
          # MA change
          # g <- g + annotate("point", x=which(chartDF$CardioMA_a=="ArtifactMA"),
          #                   y=chartDF$c_Cardio1[which(chartDF$CardioMA_a=="ArtifactMA")],
          #                   shape=8, size=4, color="brown") # was "slateblue2"
          # systolic beat to beat
          # g <- g + annotate("point", x=which(chartDF$CardioSystolic_a=="ArtifactMaxBtoB"),
          #                   y=chartDF$c_CardioSystolic[which(chartDF$CardioSystolic_a=="ArtifactMaxBtoB")],
          #                   shape=4, size=6, color="brown") # was "green3" 
          # diastolic beat to beat
          # g <- g + annotate("point", x=which(chartDF$CardioDiastolic_a=="ArtifactMinBtoB"),
          #                   y=chartDF$c_CardioDiastolic[which(chartDF$CardioDiastolic_a=="ArtifactMinBtoB")],
          #                   shape=4, size=6, color="brown") # was "green2" 
          
        }
        
        
        ######## activity sensor artifacts ########
        
        if(isTRUE(showActivityData) && showActivityArtifacts) {
          
          if(activityWarning=="none") {
            
            # max peak amplitude
            # g <- g + annotate("point", x=which(chartDF$Move1Max_a!=0),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1Max_a!=0)],
            #                   shape=4, size=1, color="red") # shape 3 is a +
            # max peak rate
            # g <- g + annotate("point", x=which(chartDF$Move1Max_a!=0),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1Max_a!=0)],
            #                   shape=3, size=1, color="brown") # shape 4 is an X
            # min peak amplitude
            # g <- g + annotate("point", x=which(chartDF$Move1Min_a!=0),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1Min_a!=0)],
            #                   shape=4, size=1, color="red")
            # min peak rate
            # g <- g + annotate("point", x=which(chartDF$Move1Min_a!=0),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1Min_a!=0)],
            #                   shape=3, size=1, color="brown")
            # min max amplitude
            # g <- g + annotate("point", x=which(chartDF$Move1_a!=0),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1_a=="ArtifactMinMaxAmp")],
            #                   shape=4, size=1, color="red") # shape 2 is a triangle
            # activity sensor instability
            # g <- g + annotate("point", x=which(chartDF$Move1MA_a!=0),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1MA_a!=0)],
            #                   shape=6, size=1, color="red") # shape 6 is an upsidedown triangle
            # processed activity data
            # g <- g + annotate("point", x=which(chartDF$Move1_a!=0),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1_a!=0)],
            #                   shape=2, size=1, color="red")
            
            # 1-2-2017
            # auto-marked artifacts
            
            g <- g + annotate("point", x=which(chartDF$Move1_a=="Artifact"),
                              y=chartDF$c_Move1Proc[which(chartDF$Move1_a=="Artifact")],
                              shape=8, size=1, alpha=.5, color="red")
            # g <- g + annotate("point", x=which(chartDF$Move1_a=="ArtifactX"),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Move1_a=="ArtifactX")],
            #                   shape=8, size=2, alpha=.5, color="red")
            
            # 2025May10
            # artifacts shared by other sensors
            # g <- g + annotate("point", x=which(chartDF$Artifacts_a!="0"),
            #                   y=chartDF$c_Move1Proc[which(chartDF$Artifacts_a!="0")],
            #                   shape=8, size=1, alpha=.5, color="black")

          } # end if for activity sensor warning
          
        }
        
      } # end if showArtifacts==TRUE
      
      ####################   TIME SERIES DATA   ######################
      
      if(showData == TRUE) {
        
        ## limit the data range to the y axis range ##
        
        {
          
          # upper limit
          fixThese <- which(chartDF$c_Cardio1 >= yMax)
          chartDF$c_Cardio1[fixThese] <- yMax
          chartDF$c_CardioMA[fixThese] <- yMax
          chartDF$c_CardioMid[fixThese] <- yMax
          chartDF$c_CardioDiastolic[fixThese] <- yMax
          chartDF$c_CardioSystolic[fixThese] <- yMax

          # lower limit
          fixThese<- which(chartDF$c_Cardio1 <= yMin)
          chartDF$c_Cardio1[fixThese] <- yMin
          chartDF$c_CardioMA[fixThese] <- yMin
          chartDF$c_CardioMid[fixThese] <- yMin
          chartDF$c_CardioDiastolic[fixThese] <- yMin
          chartDF$c_CardioSystolic[fixThese] <- yMin
          
        }
        
        if(isTRUE(showPneumoData)) {
          # upper pneumo data
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoSm), color="blue3", linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
          # mid line
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoMid), color="blue3", linewidth=.15, alpha=.5) # + coord_cartesian(ylim=c(yMin, yMax))
          # inhalation exhalation lines
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoInh), color="grey60", linewidth=.1, alpha=.75) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoExh), color="grey60", linewidth=.1, alpha=.75) # + coord_cartesian(ylim=c(yMin, yMax))
          # quantiles
          uPnQ1 <- quantile(chartDF$c_UPneumoSm, .25)
          uPnQ2 <- quantile(chartDF$c_UPneumoSm, .50)
          uPnQ3 <- quantile(chartDF$c_UPneumoSm, .75)
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=uPnQ1), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=uPnQ2), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=uPnQ3), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          # tukey fences
          uPnIQR <- uPnQ3 - uPnQ1
          uPnInnerFenceUpper <- uPnQ3 + (1.5 * uPnIQR)
          uPnInnerFenceLower <- uPnQ1 - (1.5 * uPnIQR)
          uPnOuterFenceUpper <- uPnQ3 + (3 * uPnIQR)
          uPnOuterFenceLower <- uPnQ1 - (3 * uPnIQR)
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=uPnInnerFenceUpper), color="red", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=uPnInnerFenceLower), color="red", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=uPnOuterFenceUpper), color="red", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=uPnOuterFenceLower), color="red", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          # excursionLine
          # chartDF$c_UPneumoExcursion
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoExcursion), color="blue3", linewidth=.15, alpha=.5) # + coord_cartesian(ylim=c(yMin, yMax))
          
          
          # lower pneumo data
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoSm), color="blue4", linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
          # mid line
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoMid), color="blue4", linewidth=.15, alpha=.5) # + coord_cartesian(ylim=c(yMin, yMax))
          # inhalation exhalation lines
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoInh), color="grey60", linewidth=.1, alpha=.75) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoExh), color="grey60", linewidth=.1, alpha=.75) # + coord_cartesian(ylim=c(yMin, yMax))
          # quantiles
          lPnQ1 <- quantile(chartDF$c_LPneumoSm, .25)
          lPnQ2 <- quantile(chartDF$c_LPneumoSm, .50)
          lPnQ3 <- quantile(chartDF$c_LPneumoSm, .75)
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=lPnQ1), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=lPnQ2), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=lPnQ3), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          # tukey fences
          lPnIQR <- lPnQ3 - lPnQ1
          lPnInnerFenceUpper <- lPnQ3 + (1.5 * lPnIQR)
          lPnInnerFenceLower <- lPnQ1 - (1.5 * lPnIQR)
          lPnOuterFenceUpper <- lPnQ3 + (3 * lPnIQR)
          lPnOuterFenceLower <- lPnQ1 - (3 * lPnIQR)
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=lPnInnerFenceUpper), color="orange", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=lPnInnerFenceLower), color="orange", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=lPnOuterFenceUpper), color="orange", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=lPnOuterFenceLower), color="orange", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          # excursion line
          # chartDF$c_LPneumoExcursion
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoExcursion), color="blue4", linewidth=.15, alpha=.5) # + coord_cartesian(ylim=c(yMin, yMax))
        }
        
        if(isTRUE(showEDAData)) {
          # filtered EDA data for data quality, stability and artifact analysis
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFiltMax), color="grey60", linewidth=.15, alpha=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFiltMin), color="grey60", linewidth=.15, alpha=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFiltMid), color="black", linewidth=.15, alpha=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EDAFilt), color="green4", linewidth=.5, alpha=.15) # + coord_cartesian(ylim=c(yMin, yMax))
        }
        
        if(isTRUE(showCardioData)) {
          # cardio data
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Cardio1), color="red", linewidth=.15, alpha=.5) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Cardio1), color="red", linewidth=.15, alpha=.7) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_cardioRateSystolic), color="brown", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMA), color="black", linewidth=.35, alpha=.75) # + coord_cartesian(ylim=c(yMin, yMax))# if(showCardioData) {
          if(showScores) {
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMid), color="brown", linewidth=.1, alpha=.7) # + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMA), color="black", linewidth=.35, alpha=.7) # + coord_cartesian(ylim=c(yMin, yMax))
          }
          # systolic diastolic peak lines
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioDiastolic), color="grey60", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioSystolic), color="grey60", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          
          # signal processing Stern Ray & Quigley 19
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioSRQ1+125), color="red3", linewidth=.15, alpha=.7) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioSRQ2+250), color="red2", linewidth=.15, alpha=.7) # + coord_cartesian(ylim=c(yMin, yMax))
          
          # chartDF$c_CardioSr
          
          # electronic cardio data
          if(inclECardio==TRUE) { 
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardio), color="orange", linewidth=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioDiastolic), color="grey60", linewidth=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioSystolic), color="grey60", linewidth=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioMid), color="black", linewidth=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # if(showScores) {
            #   g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_eCardioMA), color="red", linewidth=.15) + coord_cartesian(ylim=c(yMin, yMax))
            # }
          } # end if for electronic cardio
          
          # forearm or finger cuff data
          if(isTRUE(showCardioData) && inclFC==TRUE) { 
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FC+500), color="brown", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCDiastolic), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCSystolic), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCMid+500), color="black", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            if(showScores) {
              g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_FCMA+500), color="blue3", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            }
          } # end if for FC
        } # end if showCardioData
        
        if(isTRUE(showEDAData)) {
          
          if(isTRUE(showAutoEDA)) {
            # Filtered EDA data
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDA), color="green4", linewidth=.45) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAMid), color="grey60", linewidth=.15, alpha=.4) # + coord_cartesian(ylim=c(yMin, yMax))
            # wire frame lines
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAPeak), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDABase), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          }
          
          if(isTRUE(showManualEDA)) {
            # Un-filtered EDA data
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_ManualEDA), color="brown", alpha=.7, linewidth=.45) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_ManualEDAMid), color="black", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_ManualEDA), color="blue2", linewidth=.5) # + coord_cartesian(ylim=c(yMin, yMax))
            # wire frame 
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_ManualEDAPeak), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_ManualEDABase), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          }
          
          if(isTRUE(showEA) && "c_EA" %in% colnames(chartDF)) {
            # Un-filtered EDA data
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_EA), color="green3", linewidth=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          }
          
          if(all(showManualEDA2, "c_ManualEDA2" %in% colnames(chartDF), inclEDA2)) {
            # inclEDA2 is initialized earlier in this script
            # showManualEDA2 is initialized earlier in this script
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_ManualEDA2), color="brown", alpha=.7, linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
          }
          
        } # end if showEDAData
        
        if(isTRUE(showPLEData)) {
          # photoplethysmograph PLE data
          if(inclPLE==TRUE) { 
            g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PPG1), color="brown", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PPG1MA), color="brown", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # systolic diastolic peak lines
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PPG1Max), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PPG1Min), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          } # end if for PLE
        }
        
        if(isTRUE(showActivityData) && activityWarning=="none") {
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1), color="black", linewidth=.1) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1Proc), color="tan4", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1), color="grey35", linewidth=.2) # + coord_cartesian(ylim=c(yMin, yMax))
          # mid lines
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1MA), color="black", linewidth=.08, alpha=.8) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1ProcMA), color="tan4", linewidth=.08) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1MA), color="grey60", linewidth=.25, alpha=.8) # + coord_cartesian(ylim=c(yMin, yMax))
          # results
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1Result), color="red", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1Abstrct1), color="grey35", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
          # upper and lower peak lines
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1Min), color="grey60", linewidth=.1, alpha=.75) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1Max), color="grey60", linewidth=.1, alpha=.75) # + coord_cartesian(ylim=c(yMin, yMax))
          # quantiles
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=quantile(c_Move1Proc, .50)), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=quantile(c_Move1Proc, .25)), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=quantile(c_Move1Proc, .75)), color="blue3", linewidth=.125, alpha=.6) # + coord_cartesian(ylim=c(yMin, yMax))
          # IQR
          activityQ1 <- quantile(chartDF$c_Move1Proc, .25)
          activityQ2 <- quantile(chartDF$c_Move1Proc, .50)
          activityQ3 <- quantile(chartDF$c_Move1Proc, .75)
          activityIQR <- activityQ3 - activityQ1
          # tukey fences
          activityInnerFenceUpper <- activityQ3 + (1 * activityIQR)
          activityInnerFenceLower <- activityQ1 - (1 * activityIQR)
          activityOuterFenceUpper <- activityQ3 + (2 * activityIQR)
          activityOuterFenceLower <- activityQ1 - (2 * activityIQR)
          # Tukey fences
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=activityInnerFenceUpper), color="red", linewidth=.125, alpha=.4) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=activityInnerFenceLower), color="red", linewidth=.125, alpha=.4) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=activityOuterFenceUpper), color="red", linewidth=.125, alpha=.4) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=activityOuterFenceLower), color="red", linewidth=.125, alpha=.4) # + coord_cartesian(ylim=c(yMin, yMax))
        }
        
        if(isTRUE(showPTTPTT) && "c_PTTPTT" %in% colnames(chartDF)) {
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PTTPTT), color="blue2", linewidth=.2) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PTTPPG), color="blue3", linewidth=.2) # + coord_cartesian(ylim=c(yMin, yMax))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PTTECG), color="blue4", linewidth=.2) # + coord_cartesian(ylim=c(yMin, yMax))
          # PTT slow moving average
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PTTPTT_MA), color="black", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
          # 
          # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PTTPTT_abs), color="black", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
        }
        
        # WHAT DOES THIS DO?
        # g <- g + coord_cartesian(ylim=c(yMin, yMax))
        
      } # end if showData==TRUE
      
      ###################   MEASUREMENT LINES   ###################
      
      if(showMeasurements==TRUE && length(eventNames)!=0) {
        
        ######### Respiration measurement lines #########
        
        # if(isTRUE(showPneumoData) && !isTRUE(PCASSFormat)) {
        #   
        #   # make a vector of response onset and end rows for upper and lower pneumo sensors
        #   responseOnsetPnUpper <- which(chartDF$UPneumoExtract=="responseOnsetRow")
        #   responseEndPnUpper <- responseOnsetPnUpper + measuredSeg*cps -1
        #   # fix condition where response end exceeds nrow(chartDF)
        #   responseEndPnUpper[which(responseEndPnUpper > nrow(chartDF))] <- nrow(chartDF)
        #   
        #   responseOnsetPnLower <- which(chartDF$LPneumoExtract=="responseOnsetRow")
        #   responseEndPnLower <- responseOnsetPnLower + measuredSeg*cps -1
        #   # fix condition where response end exceeds nrow(chartDF)
        #   responseEndPnLower[which(responseEndPnLower > nrow(chartDF))] <- nrow(chartDF)
        #   
        #   if(length(responseOnsetPnUpper) > 0) {
        #   
        #   # fix condition where response end exceeds the number of rows in the chart data frame
        #   if(responseEndPnUpper[length(responseEndPnUpper)] > nrow(chartDF)) { 
        #     responseEndPnUpper[length(responseEndPnUpper)] <- nrow(chartDF) }
        #   if(responseEndPnLower[length(responseEndPnLower)] > nrow(chartDF)) { 
        #     responseEndPnLower[length(responseEndPnLower)] <- nrow(chartDF) }
        #   
        #   #### upper pneumo measurement
        #   
        #   # make a data frame for each segment
        #   for (l in 1:length(responseOnsetPnUpper)) {
        #     DF <- as.data.frame(matrix(NA, nrow=(measuredSeg*cps), ncol=2,dimnames=list(NULL,c("UPn", "Idx"))))
        #     # check the length of the data frame if the last segment ends prematurely
        #     if( (responseEndPnUpper[l] - responseOnsetPnUpper[l] + 1) < (measuredSeg*cps) ) {
        #       DF <- DF[1:(responseEndPnUpper[l] - responseOnsetPnUpper[l] + 1),]
        #     }
        #     DF$UPn <- chartDF$c_UPneumoSm[responseOnsetPnUpper[l]:responseEndPnUpper[l]]
        #     DF$Idx <- c(responseOnsetPnUpper[l]:responseEndPnUpper[l])
        #     assign(paste0("UPnDF", l), DF, pos=1)
        #   } # end for loop to make data frames for each upper pneumo segment
        #   
        #   # loop over the data frames
        #   # this is done using separate data frames for each segement
        #   # because ggplot has lazy evaluation and will evaluate only the last x indices
        #   UPneumoDFs <- ls(pattern="UPnDF", pos=1)
        #   for (m in 1:length(UPneumoDFs)) {
        #     g <- g + geom_line(data=get(UPneumoDFs[m], pos=1), aes(x=Idx, y=UPn), color="blue", lineWidth=.7)
        #     # was "blue1"
        #   }
        #   
        #   #### lower pneumo measurement
        #   
        #   for (l in 1:length(responseOnsetPnLower)) {
        #     DF <- as.data.frame(matrix(NA, nrow=(measuredSeg*cps), ncol=2,dimnames=list(NULL,c("LPn", "Idx"))))
        #     # check the length of the data frame if it ends abruptly
        #     if( (responseEndPnLower[l] - responseOnsetPnLower[l] + 1) < (measuredSeg*cps) ) {
        #       DF <- DF[1:(responseEndPnLower[l] - responseOnsetPnLower[l] + 1),]
        #     }
        #     DF$LPn <- chartDF$c_LPneumoSm[responseOnsetPnLower[l]:responseEndPnLower[l]]
        #     DF$Idx <- c(responseOnsetPnLower[l]:responseEndPnLower[l])
        #     assign(paste0("LPnDF", l), DF, pos=1)
        #   } # end for loop to make data frames for each lower pneumo segment
        #   
        #   # loop over the data frames
        #   # this is done using separate data frames for each segement
        #   # because ggplot has lazy evaluation and will evaluate only the last x indices
        #   LPneumoDFs <- ls(pattern="LPnDF", pos=1)
        #   for (m in 1:length(LPneumoDFs)) {
        #     g <- g + geom_line(data=get(LPneumoDFs[m], pos=1), aes(x=Idx, y=LPn), color="blue", lineWidth=.7)
        #     # was "blue4"
        #   }
        #   
        #   ### clean up
        #   rm(list=ls(pattern="UPnDF"))
        #   rm(list=ls(pattern="LPnDF"))
        #   rm(DF)
        #   
        #   } # end if(length(responseOnsetPnUpper) > 0)
        #   
        # } # end show pneumo measurements
         
        ########### show answer buffers ##############
        
        if(isTRUE(showPneumoData) && !isTRUE(PCASSFormat)) {
          
          ######## upper pneumo answer buffer ########
          
          # make a vector of answer indices
          answerRows <- which(chartDF$Events=="answerRow")
          
          # exclude answer indices that are adjacent to the offsetRow (no verbal answer)
          # answerRows <- answerRows[-which(chartDF$Events[answerRows - 1] != "")]
          
          ## skip the answer buffer if there are no answerRows
          
          if(length(answerRows) != 0) {
            
            # initialize the answer buffer indices
            aBuffXOn <- answerRows - pneumoAnsBuff*cps
            aBuffXOff <- answerRows + pneumoAnsBuff*cps
            
            # correct for potential problems
            aBuffXOn[aBuffXOn < 1] <- 1
            aBuffXOff[aBuffXOff > nrow(chartDF)] <- nrow(chartDF)
            
            # initialize a vector
            aBuffDF <- rep(NA, times=4)
            names(aBuffDF) <- c("aBuffOnX", "aBuffOffX", "aBuffOnY", "aBuffOffY")
            
            # make a separate vector for each verbal answer and assign it to the global environment
            for(l in 1:length(aBuffXOn)) {
              aBuffDF['aBuffOnX'] <- aBuffXOn[l]
              aBuffDF['aBuffOffX'] <- aBuffXOff[l]
              aBuffDF['aBuffOnY'] <- chartDF$c_UPneumoSm[aBuffXOn[l]]
              aBuffDF['aBuffOffY'] <- chartDF$c_UPneumoSm[aBuffXOff[l]]
              assign(paste0("aBuffVector", l), aBuffDF, pos=1)
            }
            
            # loop over the aBuffUDF data frames for the answer buffer segments
            # this is done using separate data frames for each segment
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            aBuffers <- ls(pattern="aBuffVector", pos=1)
            
            for (m in 1:length(aBuffers)) {
              aBuff <- get(paste0("aBuffVector", m), pos=1)
              g <- g + annotate("segment",
                                x=aBuff['aBuffOnX'],
                                xend=aBuff['aBuffOffX'],
                                y=aBuff['aBuffOnY'],
                                yend=aBuff['aBuffOffY'],
                                color="black",
                                linetype="solid",
                                linewidth=.8,
                                alpha=.75)
            }
            
            rm(list=aBuffers)
            rm(aBuffers)
            
            ######## lower pneumo answer buffer ######
            
            # aBuffXOn and aBuffXOff were set for the upper pneumo
            
            # initialize a vector
            aBuffDF <- rep(NA, times=4)
            names(aBuffDF) <- c("aBuffOnX", "aBuffOffX", "aBuffOnY", "aBuffOffY")
            
            # make a separate vector for each verbal answer and assign it to the global environment
            for(l in 1:length(aBuffXOn)) {
              aBuffDF['aBuffOnX'] <- aBuffXOn[l]
              aBuffDF['aBuffOffX'] <- aBuffXOff[l]
              aBuffDF['aBuffOnY'] <- chartDF$c_LPneumoSm[aBuffXOn[l]]
              aBuffDF['aBuffOffY'] <- chartDF$c_LPneumoSm[aBuffXOff[l]]
              assign(paste0("aBuffVector", l), aBuffDF, pos=1)
            }
            
            # loop over the aBuffUDF data frames for the answer buffer segments
            # this is done using separate data frames for each segement
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            aBuffers <- ls(pattern="aBuffVector", pos=1)
            
            for (m in 1:length(aBuffers)) {
              aBuff <- get(paste0("aBuffVector", m), pos=1)
              g <- g + annotate("segment",
                                x=aBuff['aBuffOnX'],
                                xend=aBuff['aBuffOffX'],
                                y=aBuff['aBuffOnY'],
                                yend=aBuff['aBuffOffY'],
                                color="black",
                                linetype="solid",
                                linewidth=.8,
                                alpha=.75)
            }
            
            rm(list=aBuffers)
            rm(aBuffers)
            
          } # end if(length(answerRows) != 0) # skip if no answers
          
        } # end if show pneumo answer buffer 
        
        ######### Auto EDA measurement lines #########
        
        if(isTRUE(showEDAData)) { 
          
          responseOnsetXAutoEDA <- which(chartDF$AutoEDAExtract == "responseOnsetRow")
          responseEndXAutoEDA <- which(chartDF$AutoEDAExtract == "responseEndRow")
          
          # 9-8-2016 check and fix the length of the EDA onset and end vectors if necessary
          if(length(responseOnsetXAutoEDA) != length(responseEndXAutoEDA)) { 
            # combine the lengths of the onset and end vectors
            onsetEndVectors <- c(length(responseOnsetXAutoEDA), length(responseEndXAutoEDA))
            # then calculate the difference in the length of the onset and end vectors
            diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
            ifelse (which.min(onsetEndVectors)==1,
                    responseOnsetXAutoEDA <- c(responseOnsetXAutoEDA, rep(1, diffLen)),
                    responseEndXAutoEDA <- c(responseEndXAutoEDA, rep(1, diffLen))
            ) 
            # which(responseOnsetXAutoEDA[2:length(responseOnsetXAutoEDA)] < 
            #         responseEndXAutoEDA[1:(length(responseEndXAutoEDA)-1)]) + 1
            # correct condition if any response end index exceeds the data frame length
            if(length(which(responseEndXAutoEDA > nrow(chartDF))) != 0) {
              responseOnsetXAutoEDA <- responseOnsetXAutoEDA[-which(responseEndXAutoEDA > nrow(chartDF)) > 0]
              responseEndXAutoEDA <- responseEndXAutoEDA[-which(responseEndXAutoEDA > nrow(chartDF)) > 0]
            }
          } # end if fix length
          
          # 9-5-2016
          newOnset <- NULL
          newEnd <- NULL
          # iterate backwards over the vector of response onsets
          for (l in length(responseOnsetXAutoEDA):1) {
            # for each stimulus onset get the min stimulus end that is greater
            # first silence warnings from this section when there is no end index for an onset index
            oldw <- getOption("warn")
            options(warn = -1)
            ifelse(is.na(responseEndXAutoEDA[min(which(responseEndXAutoEDA >= responseOnsetXAutoEDA[l]))]),
                   endVal <- NA,
                   endVal <- responseEndXAutoEDA[min(which(responseEndXAutoEDA >= responseOnsetXAutoEDA[l]))]
            )
            # reset the warning level
            options(warn = oldw)
            # remove the onset value from the new onset vector if there is no response end index
            if(!is.na(endVal)) { newEnd <- c(newEnd, endVal) }
            if(!is.na(endVal)) { newOnset <- c(newOnset, responseOnsetXAutoEDA[l]) }
            if(!is.na(endVal)) { responseEndXAutoEDA <- responseEndXAutoEDA[-which(responseEndXAutoEDA==endVal)] }
          } # end for loop
          # need to reverse the vectors because of iterating backwards
          responseOnsetXAutoEDA <- rev(newOnset)
          responseEndXAutoEDA <- rev(newEnd)
          
          # now get the Y values
          responseOnsetYAutoEDA <- chartDF$c_AutoEDA[responseOnsetXAutoEDA]
          responseEndYAutoEDA <- chartDF$c_AutoEDA[responseEndXAutoEDA]
          
          # horizontal line begins at the response end and points to the response onset
          g <- g + annotate("segment",
                            x=responseEndXAutoEDA,
                            xend=responseOnsetXAutoEDA,
                            y=responseOnsetYAutoEDA, 
                            yend=responseOnsetYAutoEDA,
                            color="purple",
                            linewidth=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          # vertical line begins at response baseline and points to the peak
          g <- g + annotate("segment",
                            x=responseEndXAutoEDA,
                            xend=responseEndXAutoEDA,
                            y=responseOnsetYAutoEDA, 
                            yend=responseEndYAutoEDA,
                            # was "blue",
                            color="purple",
                            linewidth=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          
          #### auto EDA response duration
          
          halfRecoveryXAutoEDA <- which(chartDF$AutoEDAExtract == "halfRecoveryRow")
          # which(chartDF$AutoEDAExtract == "responseOnsetRow")
          
          # check and fix the length of halfRecoveryXAutoEDA and responseEndXAutoEDA
          if(length(responseEndXAutoEDA) != length(halfRecoveryXAutoEDA)) {
            onsetEndVectors <- c(length(responseEndXAutoEDA), length(halfRecoveryXAutoEDA))
            # then calculate the difference in the length of the onset and end vectors
            
            
            
            diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
            ifelse (which.min(onsetEndVectors)==1,
                    responseEndXAutoEDA <- c(responseEndXAutoEDA, rep(1, diffLen)),
                    halfRecoveryXAutoEDA <- c(halfRecoveryXAutoEDA, rep(1, diffLen))
            ) 
          } # end if fix length
          
          newEnd <- NULL
          newHalfRec <- NULL
          # for each stimulus end index get the half recovering index 
          # that is greater than the stimulus end index
          # l=length(responseEndXAutoEDA)
          for (l in length(responseEndXAutoEDA):1) {
            # iterate backwards over the vector of response end indices
            # first silence warnings from this section when there is no end index for an onset index
            oldw <- getOption("warn")
            options(warn = -1)
            # set the halfRec value
            ifelse(is.na(halfRecoveryXAutoEDA[min(which(halfRecoveryXAutoEDA >= responseEndXAutoEDA[l]))]),
                   halfRec <- NA,
                   halfRec <- halfRecoveryXAutoEDA[min(which(halfRecoveryXAutoEDA >= responseEndXAutoEDA[l]))]
            )
            # reset the warning level
            options(warn = oldw)
            # remove the onset value from the new onset vector if there is no response end index
            if(!is.na(halfRec)) { 
              newHalfRec <- c(newHalfRec, halfRec) 
              newEnd <- c(newEnd, responseEndXAutoEDA[l]) 
              # remove the halfRec value from the halfRecoveryXAutoEDA vector before the next iteration
              halfRecoveryXAutoEDA <- halfRecoveryXAutoEDA[-which(halfRecoveryXAutoEDA==halfRec)] 
            }
          } # end for loop
          # need to reverse the vectors because of iterating backwards
          responseEndXAutoEDA <- rev(newEnd)
          halfRecoveryXAutoEDA <- rev(newHalfRec)
          
          # then get the Y values
          responseEndYAutoEDA <- chartDF$c_AutoEDA[responseEndXAutoEDA]
          halfRecoveryYAutoEDA <- chartDF$c_AutoEDA[halfRecoveryXAutoEDA]
          
          # now add the Auto EDA duration to the plot
          
          if(isTRUE(showEDADuration)) {
            # horizontal line begins at the response end and points to the response onset
            g <- g + annotate("segment",
                              x=responseEndXAutoEDA,
                              xend=halfRecoveryXAutoEDA,
                              y=responseOnsetYAutoEDA, 
                              yend=responseOnsetYAutoEDA,
                              color="purple",
                              linewidth=1,
                              alpha=.3,
                              arrow=arrow(length=unit(0.2, "cm")))
            # vertical line begins at response baseline and points to the peak
            g <- g + annotate("segment",
                              x=halfRecoveryXAutoEDA,
                              xend=halfRecoveryXAutoEDA,
                              y=responseOnsetYAutoEDA, 
                              yend=halfRecoveryYAutoEDA,
                              # was "purple",
                              color="black",
                              linewidth=.4)
            
          } # end if istrue showEDADuration
          
          #### auto EDA response complexity
          
          complexityXAutoEDA <- which(chartDF$AutoEDAExtract == "complexityRow")
          complexityYAutoEDA <- chartDF$c_AutoEDA[complexityXAutoEDA]
          
          complexityXOnsetAutoEDA <- NULL
          if(length(complexityXAutoEDA) > 0) {
            # initialize a vector
            # find the largest xOnset index preceeding each value in the complexityXAutoEDA vector
            # interate over the complexityXAutoEDA vector
            l=1
            for(l in 1:length(complexityXAutoEDA)) {
              thisIndex <- complexityXAutoEDA[l]
              # 2020-02-09 to avoid an error
              if(length(responseOnsetXAutoEDA[responseOnsetXAutoEDA < thisIndex]) == 0) next()
              keepIdx <- max(responseOnsetXAutoEDA[responseOnsetXAutoEDA < thisIndex])
              complexityXOnsetAutoEDA <- c(complexityXOnsetAutoEDA, keepIdx)
            } # end for
          } # end if
          
          # get the Y values for the response onset for complex responses
          complexityYOnsetAutoEDA <- chartDF$c_AutoEDA[complexityXOnsetAutoEDA]
          
          # the add Auto EDA complexity to the plot
          
          if(isTRUE(showEDAComplexity) && length(complexityXOnsetAutoEDA) > 0) {
            # vertical segment
            g <- g + annotate("segment",
                              x=complexityXAutoEDA,
                              xend=complexityXAutoEDA,
                              y=complexityYOnsetAutoEDA, 
                              yend=complexityYAutoEDA,
                              # was "blue",
                              color="purple",
                              alpha=.4,
                              # arrow=arrow(length=unit(0.2, "cm"))
                              linewidth=.6)
            
          } # end if show complexity auto EDA
          
          ########### Manual EDA measurement lines ###########
          
          if(isTRUE(showManualEDA)) {
            
            responseOnsetXManualEDA <- which(chartDF$ManualEDAExtract == "responseOnsetRow")
            responseEndXManualEDA <- which(chartDF$ManualEDAExtract == "responseEndRow")
            
            # 9-8-2016 check and fix the length of the EDA onset and end vectors if necessary
            if(length(responseOnsetXManualEDA) != length(responseEndXManualEDA)) { 
              onsetEndVectors <- c(length(responseOnsetXManualEDA), length(responseEndXManualEDA))
              diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
              ifelse (which.min(onsetEndVectors)==1,
                      responseOnsetXManualEDA <- c(responseOnsetXManualEDA, rep(1, diffLen)),
                      responseEndXManualEDA <- c(responseEndXManualEDA, rep(1, diffLen))
              ) 
              which(responseOnsetXManualEDA[2:length(responseOnsetXManualEDA)] < 
                      responseEndXManualEDA[1:(length(responseEndXManualEDA)-1)]) + 1
              # correct condition if any response end index exceeds the data frame length
              if(length(which(responseEndXManualEDA > nrow(chartDF))) != 0) {
                responseOnsetXManualEDA <- responseOnsetXManualEDA[-which(responseEndXManualEDA > nrow(chartDF)) > 0]
                responseEndXManualEDA <- responseEndXManualEDA[-which(responseEndXManualEDA > nrow(chartDF)) > 0]
              }
            } # end if fix length
            
            # 9-5-2016
            # for each stimulus onset get the min stimulus end that is greater
            newOnset <- NULL
            newEnd <- NULL
            for (l in length(responseOnsetXManualEDA):1) {
              # silence warnings from this section when there is no end index for an onset index
              oldw <- getOption("warn")
              options(warn = -1)
              ifelse(is.na(responseEndXManualEDA[min(which(responseEndXManualEDA >= responseOnsetXManualEDA[l]))]),
                     endVal <- NA,
                     endVal <- responseEndXManualEDA[min(which(responseEndXManualEDA >= responseOnsetXManualEDA[l]))]
              )
              # reset the warning level
              options(warn = oldw)
              # remove the onset value from the new onset vector if there is no response end index
              if(!is.na(endVal)) { newEnd <- c(newEnd, endVal) }
              if(!is.na(endVal)) { newOnset <- c(newOnset, responseOnsetXManualEDA[l]) }
              if(!is.na(endVal)) { responseEndXManualEDA <- responseEndXManualEDA[-which(responseEndXManualEDA==endVal)] }
            }
            responseOnsetXManualEDA <- rev(newOnset)
            responseEndXManualEDA <- rev(newEnd)
            
            responseOnsetYManualEDA <- chartDF$c_ManualEDA[responseOnsetXManualEDA]
            responseEndYManualEDA <- chartDF$c_ManualEDA[responseEndXManualEDA]
            
            # horizontal line begins at the end and points to the onset
            g <- g + annotate("segment",
                              x=responseEndXManualEDA,
                              xend=responseOnsetXManualEDA,
                              y=responseOnsetYManualEDA,
                              yend=responseOnsetYManualEDA,
                              color="purple",
                              alpha=.25,
                              linewidth=.5,
                              arrow=arrow(length=unit(0.2, "cm")))
            # vertical line
            g <- g + annotate("segment",
                              x=responseEndXManualEDA,
                              xend=responseEndXManualEDA,
                              y=responseOnsetYManualEDA,
                              yend=responseEndYManualEDA,
                              color="purple",
                              alpha=.25,
                              linewidth=.5,
                              arrow=arrow(length=unit(0.2, "cm")))
            
            #### manual EDA response duration
            
            halfRecoveryXManualEDA <- which(chartDF$ManualEDAExtract == "halfRecoveryRow")
            
            # check and fix the length of halfRecoveryXManualEDA and responseEndXManualEDA
            if(length(responseEndXManualEDA) != length(halfRecoveryXManualEDA)) {
              onsetEndVectors <- c(length(responseEndXManualEDA), length(halfRecoveryXManualEDA))
              # then calculate the difference in the length of the onset and end vectors
              diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
              ifelse (which.min(onsetEndVectors)==1,
                      responseEndXManualEDA <- c(responseEndXManualEDA, rep(1, diffLen)),
                      halfRecoveryXManualEDA <- c(halfRecoveryXManualEDA, rep(1, diffLen))
              ) 
            } # end if fix length
            
            # for each stimulus end index get the half recovering index that is greater
            newEnd <- NULL
            newHalfRec <- NULL
            # iterate backwards over the vector of response end indices
            l=length(responseEndXManualEDA)
            for (l in length(responseEndXManualEDA):1) {
              # silence warnings from this section when there is no end index for an onset index
              oldw <- getOption("warn")
              options(warn = -1)
              # set the halfRec value
              ifelse(is.na(halfRecoveryXManualEDA[min(which(halfRecoveryXManualEDA >= responseEndXManualEDA[l]))]),
                     halfRec <- NA,
                     halfRec <- halfRecoveryXManualEDA[min(which(halfRecoveryXManualEDA >= responseEndXManualEDA[l]))]
              )
              # reset the warning level
              options(warn = oldw)
              # remove the onset value from the new onset vector if there is no response end index
              if(!is.na(halfRec)) { 
                newHalfRec <- c(newHalfRec, halfRec) 
                newEnd <- c(newEnd, responseEndXManualEDA[l]) 
                # remove the halfRec value from the halfRecoveryXManualEDA vector before the next iteration
                halfRecoveryXManualEDA <- halfRecoveryXManualEDA[-which(halfRecoveryXManualEDA==halfRec)] 
              }
            } # end for loop
            # need to reverse the vectors because of iterating backwards
            responseEndXManualEDA <- rev(newEnd)
            halfRecoveryXManualEDA <- rev(newHalfRec)
            
            # then get the Y values
            responseEndYManualEDA <- chartDF$c_ManualEDA[responseEndXManualEDA]
            halfRecoveryYManualEDA <- chartDF$c_ManualEDA[halfRecoveryXManualEDA]
            
            # now add the manual EDA duration to the plot
            
            if(isTRUE(showEDADuration)) {
              # horizontal line begins at the response end and points to the response onset
              g <- g + annotate("segment",
                                x=responseEndXManualEDA,
                                xend=halfRecoveryXManualEDA,
                                y=responseOnsetYManualEDA, 
                                yend=responseOnsetYManualEDA,
                                color="purple",
                                linewidth=1,
                                alpha=.3,
                                arrow=arrow(length=unit(0.2, "cm")))
              # vertical line begins at response baseline and points to the peak
              g <- g + annotate("segment",
                                x=halfRecoveryXManualEDA,
                                xend=halfRecoveryXManualEDA,
                                y=responseOnsetYManualEDA, 
                                yend=halfRecoveryYManualEDA,
                                # was "purple",
                                color="black",
                                size=.4)
            }
            
            #### manual EDA response complexity
            
            complexityXManualEDA <- which(chartDF$ManualEDAExtract == "complexityRow")
            complexityYManualEDA <- chartDF$c_ManualEDA[complexityXManualEDA]
            
            complexityXOnsetManualEDA <- NULL
            if(length(complexityXManualEDA) > 0) {
              # initialize a vector
              # find the largest xOnset index preceeding each value in the complexityXManualEDA vector
              # interate over the complexityXManualEDA vector
              l=1
              for(l in 1:length(complexityXManualEDA)) {
                thisIndex <- complexityXManualEDA[l]
                keepIdx <- max(responseOnsetXManualEDA[responseOnsetXManualEDA < thisIndex])
                complexityXOnsetManualEDA <- c(complexityXOnsetManualEDA, keepIdx)
              } # end for
            } # end if
            
            # get the Y values for the response onset for complex responses
            complexityYOnsetManualEDA <- chartDF$c_ManualEDA[complexityXOnsetManualEDA]
            
            # the add the manual EDA Complexity to the plot
            
            if(isTRUE(showEDAComplexity) && length(complexityXOnsetManualEDA) > 0) {
              # vertical segment
              g <- g + annotate("segment",
                                x=complexityXManualEDA,
                                xend=complexityXManualEDA,
                                y=complexityYOnsetManualEDA, 
                                yend=complexityYManualEDA,
                                color="purple",
                                alpha=.4,
                                # arrow=arrow(length=unit(0.2, "cm"))
                                linewidth=.6)
            }
            
          } # end if showManualEDA
          
        } # end if show EDA data
        
        ############## Cardio measurement Lines ################
        
        if(isTRUE(showCardioData) && !isTRUE(PCASSFormat)) {
          
          responseOnsetXCardio <- which(chartDF$CardioExtract == "responseOnsetRow")
          responseEndXCardio <- which(chartDF$CardioExtract == "responseEndRow")
          
          # check and fix the length of the cardio onset and end vectors if necessary
          if(length(responseOnsetXCardio) != length(responseEndXCardio)) { 
            onsetEndVectors <- c(length(responseOnsetXCardio), length(responseEndXCardio))
            diffLen <- onsetEndVectors[which.max(onsetEndVectors)] - onsetEndVectors[which.min(onsetEndVectors)]
            ifelse (which.min(onsetEndVectors)==1,
                    responseOnsetXCardio <- c(responseOnsetXCardio, rep(1, diffLen)),
                    responseEndXCardio <- c(responseEndXCardio, rep(1, diffLen))
            ) 
            which(responseOnsetXCardio[2:length(responseOnsetXCardio)] < 
                    responseEndXCardio[1:(length(responseEndXCardio)-1)]) + 1
            # correct condition if any response end index exceeds the data frame length
            if(length(which(responseEndXCardio > nrow(chartDF))) != 0) {
              responseOnsetXCardio <- responseOnsetXCardio[-which(responseEndXCardio > nrow(chartDF)) > 0]
              respnseEndXCardio <- responseEndXCardio[-which(responseEndXCardio > nrow(chartDF)) > 0]
            }
          } # end if fix length
          
          # 9-5-2016
          # for each stimulus onset get the min stimulus end that is greater
          newOnset <- NULL
          newEnd <- NULL
          for (l in length(responseOnsetXCardio):1) {
            # silence warnings from this section when there is no end index for an onset index
            oldw <- getOption("warn")
            options(warn = -1)
            ifelse(is.na(responseEndXCardio[min(which(responseEndXCardio >= responseOnsetXCardio[l]))]),
                   endVal <- NA,
                   endVal <- responseEndXCardio[min(which(responseEndXCardio >= responseOnsetXCardio[l]))]
            )
            # reset the warning level
            options(warn = oldw)
            # remove the onset value from the new onset vector if there is no response end index
            if(!is.na(endVal)) { newEnd <- c(newEnd, endVal) }
            if(!is.na(endVal)) { newOnset <- c(newOnset, responseOnsetXCardio[l]) }
            if(!is.na(endVal)) { responseEndXCardio <- responseEndXCardio[-which(responseEndXCardio==endVal)] }
          }
          responseOnsetXCardio <- rev(newOnset)
          responseEndXCardio <- rev(newEnd)
          
          # # working 9-5-2016 but commented out to try a different method
          # # make sure that each repsonse end is before the next response onset
          # for (i in 1:(length(responseEndXCardio)-1)) {
          #   if(responseEndXCardio[i] >= responseOnsetXCardio[(i+1)]) {
          #     responseEndXCardio[i] <- responseOnsetXCardio[(i+1)]-1
          #     # responseEndXCardio <- c(responseEndXCardio[1:(i-1)],
          #     #                         (responseOnsetXCardio[(i+1)]-1),
          #     #                         responseEndXCardio[(i+2):length(responseEndXCardio)])
          #   }
          # }
          # # fix the last
          # if(responseEndXCardio[length(responseEndXCardio)] <= responseOnsetXCardio[length(responseEndXCardio)]) {
          #   responseEndXCardio[length(responseEndXCardio)] <- (responseOnsetXCardio[length(responseEndXCardio)] + 1)
          # }
          
          # get the cardio data for the measurement
          # must be done before getting they cardio Y onset and end values
          useCardio <- switch(cardioLine,
                              "ma" = chartDF$c_CardioMA,
                              "diastolic" = chartDF$c_CardioDiastolic,
                              "systolic" = chartDF$c_CardioSystolic,
                              "mid" = chartDF$c_CardioMid,
                              "otherwise: last")
          
          # select the cardio MA line if RBPF
          # if(str_sub(rbpfMsg, 1, 10) == "unusual re" || str_sub(rbpfMsg, 1, 10) ==  "possible R") {
          #   useCardio <- chartDF$c_CardioMA
          # }
          
          responseOnsetYCardio <- useCardio[responseOnsetXCardio]
          responseEndYCardio <- useCardio[responseEndXCardio]
          
          g <- g + annotate("segment",
                            x=responseEndXCardio,
                            xend=responseOnsetXCardio,
                            y=responseOnsetYCardio, 
                            yend=responseOnsetYCardio,
                            color="purple",
                            # was blue
                            linewidth=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          g <- g + annotate("segment",
                            x=responseEndXCardio,
                            xend=responseEndXCardio,
                            y=responseOnsetYCardio, 
                            yend=responseEndYCardio,
                            color="purple",
                            # was "blue"
                            linewidth=.5,
                            arrow=arrow(length=unit(0.2, "cm")))
          
          ######### FC forearm cuff / finger cuff measurement Lines #########
          
          if(inclFC==TRUE && isTRUE(showCardioData)) {
            
            responseOnsetXFC <- which(chartDF$FCExtract == "responseOnsetRow")
            responseEndXFC <- which(chartDF$FCExtract == "responseEndRow")
            
            if(length(responseOnsetXFC) == length(responseEndXFC)) {
              
              # get the cardio data to show the measurement
              useFC <- switch(cardioLine,
                              "ma" = chartDF$c_FCMA,
                              "diastolic" = chartDF$c_FCDiastolic,
                              "systolic" = chartDF$c_FCSystolic,
                              "mid" = chartDF$c_FCMid,
                              "otherwise: last")
              
              responseOnsetYFC <- useFC[responseOnsetXFC]+500
              responseEndYFC <- useFC[responseEndXFC]+500
              
              g <- g + annotate("segment",
                                x=responseEndXFC,
                                xend=responseOnsetXFC,
                                y=responseOnsetYFC,
                                yend=responseOnsetYFC,
                                color="purple",
                                linewidth=.4,
                                arrow=arrow(length=unit(0.2, "cm")))
              g <- g + annotate("segment",
                                x=responseEndXFC,
                                xend=responseEndXFC,
                                y=responseOnsetYFC, 
                                yend=responseEndYFC,
                                color="purple",
                                linewidth=.4,
                                arrow=arrow(length=unit(0.2, "cm")))
              
            }
            
          } # end if inclFC
          
        } # end if showCardioData
        
        ######### PLE measurment boxes #########
        
        if(all(isTRUE(showPLEMeasurement),
               isTRUE(showMeasurements),
               isTRUE(showPLEData), 
               isTRUE(inclPLE) ) ) {
          
          # check to see if the there is any PLE extraction
          if(!is.na(which(chartDF$PPG1Extract=="prestimSegOnset")[1])) {
            
            ### PLE prestim measurements ###
            
            # make a vector of prestim onset x indices
            preOnX <- which(chartDF$PPG1Extract=="prestimSegOnset")
            # preOffX <- which(chartDF$PPG1Extract=="prestimSegOffset")
            preOffX <- preOnX + 89
            
            # preOnY <- mean(chartDF$c_PPG1Max[preOnX:preOffX])
            # preOffY <- mean(chartDF$c_PPG1Min[preOnX:preOffX])
            # different method using the PLEMeans
            # preOnY <- chartDF$c_PPG1MA[(preOnX+45)] + ((as.numeric(chartDF$PPG1Means[preOnX]) * PLScale) / 2)
            # preOffY <- chartDF$c_PPG1MA[(preOnX+45)] - ((as.numeric(chartDF$PPG1Means[preOffX]) * PLScale) / 2)
            
            # make a data frame for each PLE prestim segment
            prePLE <- rep(NA, times=4)
            names(prePLE) <- c("preOnX", "preOffX", "preOnY", "preOffY")
            for(l in 1:length(preOnX)) {
              # DF <- as.data.frame(matrix(NA, nrow=length(preOnX),ncol=4,dimnames=list(NULL,c("preOnX","preOffX","preOnY","preOffY"))))
              prePLE['preOnX'] <- preOnX[l]
              prePLE['preOffX'] <- preOffX[l]
              prePLE['preOnY'] <- mean(chartDF$c_PPG1Max[preOnX[l]:preOffX[l]])
              prePLE['preOffY'] <- mean(chartDF$c_PPG1Min[preOnX[l]:preOffX[l]])
              
              # other method is centered around the PLMA whichis not centered in the PLE plot
              # prePLE['preOnY'] <- chartDF$c_PPG1MA[(preOnX[l]+45)] + (as.numeric(chartDF$PPG1Means[preOnX[l]]) / 2)
              # prePLE['preOffY'] <- chartDF$c_PPG1MA[(preOnX[l]+45)] - (as.numeric(chartDF$PPG1Means[preOnX[l]]) / 2)
              
              # DF$preOnX[l] <- preOnX[l]
              # DF$preOffX[l] <- preOffX[l]
              # DF$preOnY[l] <- mean(chartDF$c_PPG1Max[preOnX[l]:preOffX[l]])
              # DF$preOffY[l] <- mean(chartDF$c_PPG1Min[preOnX[l]:preOffX[l]])
              
              # other method
              # DF$preOnY <- chartDF$c_PPG1MA[(preOnX[l]+45)] + ((as.numeric(chartDF$PPG1Means[preOnX[l]]) * PLScale) / 2)
              # DF$preOffY <- chartDF$c_PPG1MA[(preOnX[l]+45)] - ((as.numeric(chartDF$PPG1Means[preOnX[l]]) * PLScale) / 2)
              # assign(paste0("prePLEDF", l), DF, pos=1)
              
              # assign the prePLEVectors to the global environment
              assign(paste0("prePLEVector", l), prePLE, pos=1)
            }
            
            # loop over the prePLEVector data frames for the prestim segments
            # this is done using separate data frames for each segement
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            prePLEs <- ls(pattern="prePLEVector", pos=1)
            for (m in 1:length(prePLEs)) {
              preVector <- get(paste0("prePLEVector", m), pos=1)
              g <- g + annotate("rect",
                                xmin=preVector['preOnX'],
                                xmax=preVector['preOffX'],
                                ymin=preVector['preOnY'],
                                ymax=preVector['preOffY'],
                                color="black",
                                linewidth=.2,
                                alpha=.2,
                                # was "green"
                                fill="blue")
            }
            
            # for (m in 1:length(prePLEs)) {
            #   preVector <- get(paste0("prePLEVector", m), pos=1)
            #   g <- g + annotate("segment",
            #                     x=preVector['preOnX'],
            #                     xend=preVector['preOnX'],
            #                     y=preVector['preOnY']+25,
            #                     yend=preVector['preOffY']-25,
            #                     color="purple",
            #                     size=.5 )
            # }
            
            # for (m in 1:length(prePLEs)) {
            #   preVector <- get(paste0("prePLEVector", m), pos=1)
            #   g <- g + annotate("segment",
            #                     x=preVector['preOffX'],
            #                     xend=preVector['preOffX'],
            #                     y=preVector['preOnY']+25,
            #                     yend=preVector['preOffY']-25,
            #                     color="purple",
            #                     size=.5 )
            # }
            
            ### PLE Latency ###
            
            for (m in 1:length(prePLEs)) {
              preVector <- get(paste0("prePLEVector", m), pos=1)
              g <- g + annotate("rect",
                                xmin=preVector['preOffX'],
                                xmax=preVector['preOffX']+(5*cps),
                                ymin=preVector['preOnY']+25,
                                ymax=preVector['preOffY']-25,
                                alpha=.33,
                                fill="yellow")
            }
            
            ### PLE poststim measurements ###
            
            # make a vector of poststim onset x indices
            postOnX <- which(chartDF$PPG1Extract=="poststimSegOnset")
            postOnX[postOnX >= nrow(chartDF)] <- nrow(chartDF)-2
            
            # postOffX <- which(chartDF$PPG1Extract=="poststimSegOffset")
            postOffX <- postOnX + 149
            postOffX[postOffX >= nrow(chartDF)] <- nrow(chartDF)-1
            
            # postOnY <- mean(chartDF$c_PPG1Max[postOnX:postOffX])
            # postOffY <- mean(chartDF$c_PPG1Min[postOnX:postOffX])
            # different method using the PLEMeans
            # postOnY <- chartDF$c_PPG1MA[(postOnX+45)] + ((as.numeric(chartDF$PPG1Means[postOnX]) * PLScale) / 2)
            # postOffY <- chartDF$c_PPG1MA[(postOnX+45)] - ((as.numeric(chartDF$PPG1Means[postOffX])* PLScale) / 2)
            
            # make a data frame for each poststim segment
            postPLE <- rep(NA, times=4)
            names(postPLE) <- c("postOnX", "postOffX", "postOnY", "postOffY")
            for(l in 1:length(postOnX)) {
              # DF <- as.data.frame(matrix(NA, nrow=length(postOnX),ncol=4,dimnames=list(NULL,c("postOnX","postOffX","postOnY","postOffY"))))
              postPLE['postOnX'] <- postOnX[l]
              postPLE['postOffX'] <- postOffX[l]
              postPLE['postOnY'] <- mean(chartDF$c_PPG1Max[postOnX[l]:postOffX[l]])
              postPLE['postOffY'] <- mean(chartDF$c_PPG1Min[postOnX[l]:postOffX[l]])
              
              # other method is centered around the PLMA whichis not centered in the PLE plot
              # postPLE['postOnY'] <- chartDF$c_PPG1MA[(postOnX[l]+45)] + (as.numeric(chartDF$PPG1Means[postOnX[l]]) / 2)
              # postPLE['postOffY'] <- chartDF$c_PPG1MA[(postOnX[l]+45)] - (as.numeric(chartDF$PPG1Means[postOnX[l]]) / 2)
              
              # DF$postOnX[l] <- postOnX[l]
              # DF$postOffX[l] <- postOffX[l]
              # DF$postOnY[l] <- mean(chartDF$c_PPG1Max[postOnX[l]:postOffX[l]])
              # DF$postOffY[l] <- mean(chartDF$c_PPG1Min[postOnX[l]:postOffX[l]])
              # other method
              # DF$postOnY <- chartDF$c_PPG1MA[(postOnX[l]+45)] + ((as.numeric(chartDF$PPG1Means[postOnX[l]]) * PLScale) / 2)
              # DF$postOffY <- chartDF$c_PPG1MA[(postOnX[l]+45)] - ((as.numeric(chartDF$PPG1Means[postOnX[l]]) * PLScale) / 2)
              # assign(paste0("postPLEDF", l), DF, pos=1)
              
              # assign the postPLEVectors to the global environment
              assign(paste0("postPLEVector", l), postPLE, pos=1)
            }
            
            # loop over the data frames for the poststim segments
            # this is done using separate data frames for each segement
            # because ggplot has lazy evaluation and will evaluate only the last x indices
            postPLEs <- ls(pattern="postPLEVector", pos=1)
            
            for (l in 1:length(postPLEs)) {
              postVector <- get(paste0("postPLEVector", l), pos=1)
              g <- g + annotate("rect",
                                xmin=postVector['postOnX'],
                                xmax=postVector['postOffX'],
                                ymin=postVector['postOnY'],
                                ymax=postVector['postOffY'],
                                color="black",
                                linewidth=.2,
                                alpha=.2,
                                # was "green"
                                fill="blue")
            }
            
            # for (l in 1:length(postPLEs)) {
            #   postVector <- get(paste0("postPLEVector", l), pos=1)
            #   g <- g + annotate("segment",
            #                     x=postVector['postOnX'],
            #                     xend=postVector['postOnX'],
            #                     y=postVector['postOnY']+25,
            #                     yend=postVector['postOffY']-25,
            #                     color="purple",
            #                     size=.5 )
            # }
            
            # for (l in 1:length(postPLEs)) {
            #   postVector <- get(paste0("postPLEVector", l), pos=1)
            #   g <- g + annotate("segment",
            #                     x=postVector['postOffX'],
            #                     xend=postVector['postOffX'],
            #                     y=postVector['postOnY']+25,
            #                     yend=postVector['postOffY']-25,
            #                     color="purple",
            #                     size=.5 )
            # }
            
            # clean up the global environment
            rm(list=ls(pattern="prePLE"))
            rm(list=ls(pattern="postPLE"))
            
          } # end if !is.na PLE extraction
          
        } # end if showPLEMeasurement
        
      } # end if showMeasurements==TRUE & length(eventNames)!=0
      

      
      ###################  FEATURE EXTRACTION VALUES     ####################
      
      if(showMeasurements == TRUE && showExtractionVals == TRUE) {
        
        # ####  make a private function to remove leading zeroes from numeric scores
        # 
        # numFormat2Fn <- function(x) {
        #   # function to remove leading zeros and keep 2 decimals
        #   # vectorized already and needs no loop
        #   outVector <- x
        #   useIdx <- which(!(is.na(x) | x == ""))
        #   x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.2f", as.numeric(x[useIdx]))) )
        #   outVector[useIdx] <- x2
        #   return(outVector)
        # }
        # 
        # numFormat3Fn <- function(x) {
        #   # function to remove leading zeros and keep 3 decimals
        #   # vectorized already and needs no loop
        #   outVector <- x
        #   useIdx <- which(!(is.na(x) | x == ""))
        #   x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.3f", as.numeric(x[useIdx]))) )
        #   outVector[useIdx] <- x2
        #   return(outVector)
        # }
        # 
        # numFormatIntFn <- function(x) {
        #   # function to format values as integers
        #   # vectorized already and needs no loop
        #   outVector <- x
        #   useIdx <- which(!(is.na(x) | x == ""))
        #   x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.1f", as.numeric(x[useIdx]))) )
        #   outVector[useIdx] <- x2
        #   return(outVector)
        # }
        
        #### work with the measurements from the _Measurements data frame for the exam ####
        
        {
          
          # # get the measurement data frame
          # measurementDFName <- paste0(examName, "_Measurements")
          # measurementDF <- get(measurementDFName, pos=1)
          # # View(measurementDF)
          
          # if(!is.null(measurementDF)) {
          
          row.names(measurementDF) <- NULL
          # slice the measurements for the current chart
          chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
          row.names(chartMeasurementDF) <- NULL
          # View(chartMeasurementDF)
          
          # first get all row indices for all integer scores
          # measurementRows <- which(chartMeasurementDF$ESSScore != "")
          # use the extant rankScore to select which rows instead of ESSScore
          # measurementRows <- which(chartMeasurementDF$rankScore != "")
          
          # use the sensorMeasurement instead
          measurementRows <- which(chartMeasurementDF$sensorMeasurement != 0)
          
        }
        
        ######### check the question pacing Warning ###########
        
        {
          
          allQuestions <- unique(chartMeasurementDF$eventLabel)
          scoredQuestions <- allQuestions[!(allQuestions %in% excludeEvents)]
          
          checkAllRows <- which(chartMeasurementDF$eventLabel %in% allQuestions)
          allEventRows <- unique(chartMeasurementDF$Begin[checkAllRows])
          allEvents <- chartDF$Label[allEventRows]
          
          checkScoredRows <- which(chartMeasurementDF$eventLabel %in% scoredQuestions)
          scoredEventRows <- unique(chartMeasurementDF$Begin[checkScoredRows])
          scoredEvents <- chartDF$Label[scoredEventRows]
          
          checkThese <- which(allEventRows %in% scoredEventRows)
          againstThese <- checkThese - 1
          
          # question onset pacing in seconds
          # compare each scored question to the preceding event
          onsetDiffs <- 
            (allEventRows[checkThese] - allEventRows[againstThese]) / cps
        
          # onsetDiffs <- diff(unique(chartMeasurementDF$Begin[checkRows])) / cps
          
          questionPaceWarning <- ifelse(any(onsetDiffs < 20 | onsetDiffs > 75),
                                        "POSSIBLE PROBLEM WITH QUESTION PACING",
                                        "none" )
          
        }
        
        ##############    question intervals  Oct 15, 2023  ##################
        
        if(showQuestionIntervals) {
          
          # chartDF$eventLabel[which(chartDF$eventLabel!="")]
          
          # onsetDiffs is a vector of response onset distances
          
          g <- g + annotate(geom="text", 
                            x=scoredEventRows - 350, 
                            y=yMin, 
                            label=round(onsetDiffs,1),
                            color="black", 
                            size=1.5)
          
        }
        
        ############## caliper indices May 15, 2025 ##############
        
        
        {
          
          # get the onset and end of the stimulus segments
          stimOnset <- which(chartDF$Events=="onsetRow")
          # remove the first and last event (X and XX)
          stimOnset <- stimOnset[c(2:(length(stimOnset)-1))]
          
          WOEEnd <- stimOnset+(measuredSeg*cps) - 1
          
        }
        
        
        ##############    cardio calipers   May 15, 2025  ##################
        
        showCardioRateCaliperVals <- FALSE
        showCardioRateCaliperVals <- TRUE
        
        if(showCardioRateCaliperVals) {
          
          # compute the mean cardio rate for the stimulus segments
          cardioCaliperRates <- NULL
          for(l in 1:length(stimOnset)) {
            theseRows <- c(stimOnset[l]:WOEEnd[l])
            cardioCaliperRates <- 
              c(cardioCaliperRates, round(mean(chartDF$c_CardioRate[theseRows]), 2))
          }
          
          g <- g + annotate(geom="text", 
                            x=stimOnset + 525, 
                            y=rep((yOffset['cardio'] - 100), times=length(cardioCaliperRates)),
                            # y=rep(0, times=length(stimOnset)),
                            # label=rep("RAY", length(cardioCaliperRates)),
                            label=cardioCaliperRates,
                            color="black", 
                            size=1.5) 
          
          # compute the cardio caliper amplitude
          cardioCaliperAmplitudes <- NULL
          for(l in 1:length(stimOnset)) {
            cardioCaliperAmplitudes <- 
              c(cardioCaliperAmplitudes,
                cardioAmplitudeCaliperFn(x=chartDF$c_CardioMA, caliperStart=stimOnset[l])['maxYDistance'])
          } 
          cardioCaliperAmplitudes <- round(as.numeric(cardioCaliperAmplitudes))
          
          g <- g + annotate(geom="text", 
                            x=stimOnset + 525, 
                            y=rep((yOffset['cardio'] - 75), times=length(cardioCaliperAmplitudes)),
                            # y=rep(0, times=length(stimOnset)),
                            # label=rep("RAY", length(cardioCaliperAmplitudes)),
                            label=cardioCaliperAmplitudes,
                            color="black", 
                            size=1.5)
          
          # <>
          
        }
        
        ##############    pneumo calipers   May 16, 2025  ##################
        
        showRespirationCalipers <- TRUE
        # showRespirationCalipers <- FALSE
        
        if(showRespirationCalipers) {
          
          URespirationRateCaliper <- NULL
          LRespirationRateCaliper <- NULL
          respirationCaliperRateRatio <- NULL
          
          # upper respiration rate for each stimulus event
          for(l in 1:length(stimOnset)) {
            theseRows <- c(stimOnset[l]:WOEEnd[l])
            URespirationRateCaliper <- 
              c(URespirationRateCaliper, round(mean(chartDF$c_UPneumoRate[theseRows]), 2))
          }
          
          # lower respiration rate for each stimulus event
          for(l in 1:length(stimOnset)) {
            theseRows <- c(stimOnset[l]:WOEEnd[l])
            LRespirationRateCaliper <- 
              c(LRespirationRateCaliper, round(mean(chartDF$c_LPneumoRate[theseRows]), 2))
          }
          
          # respiration rate for the the chart
          respirationRateChartU <- round(mean(chartDF$c_UPneumoRate[c(150:(nrow(chartDF)-150))]), 2)
          respirationRateChartL <- round(mean(chartDF$c_LPneumoRate[c(150:(nrow(chartDF)-150))]), 2)
          meanRespirationRateChart <- round(mean(c(respirationRateChartU, respirationRateChartU)), 2)
          
          # ratio of upper and lower respiration
          respirationCaliperRateRatio <- URespirationRateCaliper / LRespirationRateCaliper
          respirationCaliperRateRatio <- round(1 - log(exp(abs(log(respirationCaliperRateRatio)))), 3)
          respirationCaliperRateRatio[which(respirationCaliperRateRatio == 1)] <- .999
          
          # upper respiration rate for each segment
          g <- g + annotate(geom="text",
                            x=stimOnset + 525,
                            y=rep((yOffset['uPneumo'] - 75), times=length(URespirationRateCaliper)),
                            # y=rep(0, times=length(stimOnset)),
                            # label=rep("RAY", length(URespirationRateCaliper)),
                            label=URespirationRateCaliper,
                            color="black",
                            size=1.5)
          
          # lower respiration rate for each segment
          g <- g + annotate(geom="text",
                            x=stimOnset + 525,
                            y=rep((yOffset['lPneumo'] - 75), times=length(LRespirationRateCaliper)),
                            # y=rep(0, times=length(stimOnset)),
                            # label=rep("RAY", length(LRespirationRateCaliper)),
                            label=LRespirationRateCaliper,
                            color="black",
                            size=1.5)
          
          # compute the y-axis location for the mean respiration rate for each stim segment
          yRR <- mean(c((yOffset['uPneumo']), (yOffset['lPneumo']))) + 75
                    
          # add the mean respiration rate for each segment  
          g <- g + annotate(geom="text",
                            x=stimOnset + 525,
                            y=rep((yRR - 125), times=length(respirationCaliperRateRatio)),
                            # y=rep(0, times=length(stimOnset)),
                            # label=rep("RAY", length(respirationCaliperRateRatio)),
                            label=respirationCaliperRateRatio,
                            color="black",
                            size=1.5)
          
        }
        
        showAmplitudeCalipers <- TRUE
        
        if(showAmplitudeCalipers) {
          
          # compute the number of samples for 3 charts
          threeCyclePeriod <- round((1800 / meanRespirationRateChart) * 3)
          # use this period to evaluate the prestim amplitude 
          if(threeCyclePeriod > 675) threeCyclePeriod <- 675
          
          preStimAmplitudeU <- NULL
          preStimAmplitudeL <- NULL
          
          # compute.a vector of prestim Amplitudes for the upper and lower respiration
          for(l in 1:length(stimOnset)) {
            theseRows <- c((stimOnset[l]-threeCyclePeriod):(stimOnset[l]-1))
            preStimAmplitudeU <- 
              c(preStimAmplitudeU, round(mean(chartDF$c_UpneumoAmp[theseRows]), 2))
            preStimAmplitudeL <- 
              c(preStimAmplitudeL, round(mean(chartDF$c_LpneumoAmp[theseRows]), 2))
          }
          
          amplitudeChangeU <- NULL
          amplitudeChangeL <- NULL
          
          # set the end of the stimulus segment
          stimSegLen <- measuredSeg * cps
          if(threeCyclePeriod > stimSegLen) stimSegLen <- threeCyclePeriod
          
          # compute a vector of amplitude changes for each stimulus presentation
          for(l in 1:length(stimOnset)) {
            # if the length of three respiration cycles exceeds the normal measured segment
            if(stimSegLen > (measuredSeg * cps)) {
              theseRows <- c(stimOnset[l]:(stimOnset[l]+stimSegLen-1))
              amplitudeChangeU <- 
                c(amplitudeChangeU, round(mean(chartDF$c_UpneumoAmp[theseRows]), 2))
              amplitudeChangeL <- 
                c(amplitudeChangeL, round(mean(chartDF$c_LpneumoAmp[theseRows]), 2))
            } else {
              ampU <- NULL
              ampL <- NULL
              for(k in 1:((measuredSeg * cps) - threeCyclePeriod)) {
                theseRows <- c((stimOnset[l]+k-1):(stimOnset[l]+threeCyclePeriod))
                ampU <- c(ampU, mean(chartDF$c_UpneumoAmp[theseRows]))
              }
              for(k in 1:((measuredSeg * cps) - threeCyclePeriod)) {
                theseRows <- c((stimOnset[l]+k-1):(stimOnset[l]+threeCyclePeriod))
                ampL <- c(ampL, mean(chartDF$c_LpneumoAmp[theseRows]))
              }
              amplitudeChangeU <- 
                c(amplitudeChangeU, round(mean(ampU), 2))
              amplitudeChangeL <- 
                c(amplitudeChangeL, round(mean(ampL), 2))
            }
          } # end l loop
          
          # upper respiration prestimulus amplitude for all stimulus segments
          g <- g + annotate(geom="text",
                            x=stimOnset - 100,
                            y=rep((yOffset['uPneumo'] + 75), times=length(preStimAmplitudeU)),
                            # y=rep(0, times=length(stimOnset)),
                            # label=rep("RAY", length(URespirationRateCaliper)),
                            label=preStimAmplitudeU,
                            color="black",
                            size=1.5)
          
          # upper respiration amplitude for all stimulus segments
          g <- g + annotate(geom="text",
                            x=stimOnset + 525,
                            y=rep((yOffset['uPneumo'] + 75), times=length(amplitudeChangeU)),
                            # y=rep(0, times=length(stimOnset)),
                            # label=rep("RAY", length(URespirationRateCaliper)),
                            label=amplitudeChangeU,
                            color="black",
                            size=1.5)
          
        }
          
        showRLECalipers <- TRUE
        
        if(showRLECalipers) {
        
          # <>
          
          
        
        }
        
        #######  feature extraction values  ########
        
        {
          
          selectMeasurements <- chartMeasurementDF$sensorMeasurement
          
          # use a private function to remove leading zeros
          printMeasurements <- numFormat3Fn(selectMeasurements)
          
        }
        
        if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
          
          ###  upper pneumo  ### 
          
          # get all row indices for the upper pneumo data
          uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
          # then select the row indices for the upper pneumo Measurements in the chart measurements
          uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
          
          # get the upper pneumo measurements
          uPneumoMeasurements <- printMeasurements[uPneumoSelectRows]
          
          # format as integer
          uPneumoMeasurements <- numFormatIntFn(uPneumoMeasurements)
          
          # get the row indices for the upper pneumo Measurements in the chartDF
          uPneumoMeasurementIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
          
          # remove NA values
          uPneumoMeasurementIndices <- uPneumoMeasurementIndices[!is.na(uPneumoMeasurements)]
          uPneumoMeasurements <- uPneumoMeasurements[!is.na(uPneumoMeasurements)]
          
          # plot the upper pneumo Measurements
          if(length(uPneumoMeasurements) != 0) {
            g <- g + annotate(geom="text",
                              x=(uPneumoMeasurementIndices + 150),
                              y=rep((yOffset['uPneumo'] - 75), times=length(uPneumoMeasurements)),
                              label=uPneumoMeasurements,
                              color="black",
                              alpha=.5,
                              size=2.5)
          }
          
          ### lower pneumo  ###
          
          # get all row indices for the lower pneumo data
          lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
          # then select the row indices for the lower pneumo Measurements in the chart measurements
          lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
          
          # get the lower pneumo Measurements
          lPneumoMeasurements <- printMeasurements[lPneumoSelectRows]
          
          # format as integer
          lPneumoMeasurements <- numFormatIntFn(lPneumoMeasurements)
          
          # get the row indices for the lower pneumo Measurements in the chartDF
          lPneumoMeasurementIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
          
          # remove NA values
          lPneumoMeasurementIndices <- lPneumoMeasurementIndices[!is.na(lPneumoMeasurements)]
          lPneumoMeasurements <- lPneumoMeasurements[!is.na(lPneumoMeasurements)]
          # plot the lower pneumo Measurements
          if(length(lPneumoMeasurements) != 0) {
            g <- g + annotate(geom="text",
                              x=(lPneumoMeasurementIndices + 150),
                              y=rep((yOffset['lPneumo'] - 75), times=length(lPneumoMeasurements)),
                              label=lPneumoMeasurements,
                              color="black",
                              alpha=.5,
                              size=2.5)
          }
          
          ### combined pneumo score  ###
          
          # get all row indices for the combined pneumo score
          pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
          # then select the row indices for the combined pneumo Measurements in the chart measurements
          pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
          
          # get the combined pneumo Measurements
          pneumoMeasurements <- printMeasurements[pneumoSelectRows]
          
          # format as integer
          pneumoMeasurements <- numFormatIntFn(pneumoMeasurements)
          
          # get the row indices for the combined pneumo Measurements in the chartDF
          pneumoMeasurementIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
          
          # calculate the y offset
          yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo'])) - 75
          
          # remove NA values
          pneumoMeasurementIndices <- pneumoMeasurementIndices[!is.na(pneumoMeasurements)]
          pneumoMeasurements <- pneumoMeasurements[!is.na(pneumoMeasurements)]
          # plot the combined pneumo Measurements
          # if(length(pneumoMeasurements) != 0) {
          #   g <- g + annotate(geom="text",
          #                     x=(pneumoMeasurementIndices + 150),
          #                     y=rep(yPneumo, times=length(pneumoMeasurements)),
          #                     label=pneumoMeasurements,
          #                     color="black",
          #                     size=2.5)
          # }
          
        } # end if showPneumoScores == TRUE
        
        ###  EDA  ###
        
        if(isTRUE(showEDAData) && showEDAScores==TRUE) {
          
          ### Auto EDA
          
          # get all row indices for the EDA data
          AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
          # then select the row indices for the EDA Measurements 
          # in the chart measurements DF
          AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
          
          # get the EDA Measurements
          # use printMeasurements to automatically select rank or integer Measurements
          AutoEDAMeasurements <- printMeasurements[AutoEDASelectRows]
          AutoEDAMeasurements <- round(as.numeric(AutoEDAMeasurements), 0)
          
          # get the row indices for the EDA Measurements in the chartDF
          if(length(AutoEDAMeasurements != 0)) {
            AutoEDAMeasurementIndices <- which(chartDF$eventLabel %in% 
                                                 chartMeasurementDF$eventLabel[AutoEDASelectRows])
            # plot the EDA Measurements
            if(length(AutoEDAMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=(AutoEDAMeasurementIndices + 150),
                                y=rep((yOffset['eda'] - 75), times=length(AutoEDAMeasurements)),
                                label=AutoEDAMeasurements,
                                color="black",
                                size=2.5,
                                na.rm=TRUE)
            }
          }
          
          ### Manual/un-filtered EDA
          
          if(isTRUE(showManualEDA)) {
            
            # get all row indices for the EDA data
            ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
            # then select the row indices for the EDA Measurements 
            # in the chart measurements DF
            ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
            
            # get the EDA Measurements
            # use printMeasurements to automatically select rank or integer Measurements
            ManualEDAMeasurements <- printMeasurements[ManualEDASelectRows]
            ManualEDAMeasurements <- round(as.numeric(ManualEDAMeasurements), 0)
            
            # get the row indices for the EDA Measurements in the chartDF
            if(length(ManualEDAMeasurements != 0)) {
              ManualEDAMeasurementIndices <- which(chartDF$eventLabel %in% 
                                                     chartMeasurementDF$eventLabel[ManualEDASelectRows])
              # plot the EDA Measurements
              if(length(ManualEDAMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=(ManualEDAMeasurementIndices + 150),
                                  y=rep((yOffset['eda']-125), times=length(ManualEDAMeasurements)),
                                  label=ManualEDAMeasurements,
                                  color="black",
                                  size=2.5,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showManualEDA
          
        } # end if showEDAScores
        
        ###  Cardio  ###
        
        if(isTRUE(showCardioData) && showCardioScores==TRUE) {
          
          # get all row indices for the Cardio data
          cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
          # then select the row indices for the Cardio Measurements in the chart measurements
          cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
          
          # get the Cardio Measurements
          cardioMeasurements <- printMeasurements[cardioSelectRows]
          cardioMeasurements <- round(as.numeric(cardioMeasurements), 0)
          
          # get the row indices for the Cardio Measurements in the chartDF
          if(length(cardioMeasurements) > 0) {
            cardioMeasurementIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
            # plot the Cardio Measurements
            if(length(cardioMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=(cardioMeasurementIndices + 150),
                                # need to use rep() for y in order to avoid a warning about names
                                y=rep((yOffset['cardio'] - 75), times=length(cardioMeasurements)),
                                label=cardioMeasurements,
                                color="black",
                                size=2.5,
                                na.rm=TRUE)
            }
          }
          
        } # end if showCardioScores
        
        ###  forearm Cardio cuff or finger cuff ###
        
        if(isTRUE(showFCData) && showFCScores==TRUE) {
          
          # get all row indices for the Cardio data
          cardioSensorRows <- which(chartMeasurementDF$sensorName == "FC")
          # then select the row indices for the Cardio Measurements in the chart measurements
          cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
          
          # get the Cardio Measurements
          cardioMeasurements <- printMeasurements[cardioSelectRows]
          cardioMeasurements <- round(as.numeric(cardioMeasurements), 0)
          
          # get the row indices for the Cardio Measurements in the chartDF
          if(length(cardioMeasurements) > 0) {
            cardioMeasurementIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
            # plot the Cardio Measurements
            if(length(cardioMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=(cardioMeasurementIndices + 150),
                                # need to use rep() for y in order to avoid a warning about names
                                y=rep((yOffset['cardio'] - 300), times=length(cardioMeasurements)),
                                label=cardioMeasurements,
                                color="black",
                                size=2.5,
                                na.rm=TRUE)
            }
          }
          
        } # end if showFCScores
        
        ###  PLE  ###
        
        if(isTRUE(showPLEData) && showPLEScores==TRUE) {
          
          # get all row indices for the PLE data
          pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
          # then select the row indices for the PLE Measurements in the chart measurements
          pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
          
          # get the PLE Measurements
          pleMeasurements <- printMeasurements[pleSelectRows]
          
          # get the row indices for the PLE Measurements in the chartDF
          if( length(pleMeasurements) > 0 & !all(is.na(pleMeasurements)) ) {
            pleMeasurementIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
            # plot the PLE Measurements
            if(length(pleMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=(pleMeasurementIndices + 125),
                                # need to use rep() for y in order to avoid a warning about names
                                y=rep((yOffset['ple'] - 175), times=length(pleMeasurements)),
                                label=pleMeasurements,
                                color="black",
                                size=2.5,
                                na.rm=TRUE)
            }
          }
          
          ## PLE pre / stim pulse amplitude values ##
          
          {
            
            pleDF <- chartMeasurementDF[chartMeasurementDF$sensorName=="PLE",]
            
            onsetIdx <- pleDF$Begin
            onsetIdx <- onsetIdx[onsetIdx > 1]
            
            prestimIdx <- onsetIdx - 1
            
            prestimVals <- as.numeric(numFormat3Fn(chartDF$PPG1Measure[prestimIdx]))
            prestimVals[prestimVals == 0] <- NA
            
            stimVals <- as.numeric(numFormat3Fn(chartDF$PPG1Measure[onsetIdx]))
            stimVals <- as.numeric(numFormat3Fn(prestimVals / exp(stimVals)))
            stimVals[is.na(prestimVals)] <- NA
            
            # exclude non-measured segments  
            onsetIdx <- onsetIdx[!is.na(prestimVals)]
            
            stimVals <- round(stimVals[!is.na(stimVals)], 0)
            prestimVals <- round(prestimVals[!is.na(prestimVals)], 0)
            
            if( length(prestimVals) > 0 & !all(is.na(prestimVals)) ) {
              # plot the PLE prestim Measurements
              if(length(pleMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=(onsetIdx - 100),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['ple'] + 125), times=length(prestimVals)),
                                  label=prestimVals,
                                  color="black",
                                  size=1.5,
                                  na.rm=TRUE)
              }
            }
            
            if( length(stimVals) > 0 & !all(is.na(stimVals)) ) {
              # plot the PLE prestim Measurements
              if(length(pleMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=(onsetIdx + 250),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['ple'] + 125), times=length(stimVals)),
                                  label=stimVals,
                                  color="black",
                                  size=1.5,
                                  na.rm=TRUE)
              }
            }
            
            
            
          }
          
        } # end if showPLEScores
        
        # } # end if !is.null(measurementDF)
        
      } # end if showExtractionVals == TRUE
      
      #######     ESS/Numerical scores       #######
      
      if(showScores == TRUE && !isTRUE(PCASSFormat)) {
        
        # get the measurement data frame
        measurementDFName <- paste0(examName, "_Measurements")
        measurementDF <- get(measurementDFName, pos=1)
        # View(measurementDF)
        
        if(!is.null(measurementDF)) {
          
          row.names(measurementDF) <- NULL
          # slice the measurements for the current chart
          chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
          row.names(chartMeasurementDF) <- NULL
          # View(chartMeasurementDF)
          
          # first get all row indices for all integer scores
          
          
          
          
          ifelse(length(which(chartMeasurementDF$ESSScore != "")) > 0,
                 measurementRows <- which(chartMeasurementDF$ESSScore != ""),
                 measurementRows <- which(chartMeasurementDF$rankScore != "")
                 )
          
     
          
          
          
          # make a vector of scores for printing
          # use the rank scores if there are no integer scores
          if(selectScores == "auto") {
            ifelse(length(which(chartMeasurementDF$ESSScore != "")) > 0,
                   printScores <- chartMeasurementDF$ESSScore,
                   printScores <- chartMeasurementDF$rankScore
            )
          } else {
            printScores <- switch(selectScores, 
                                  "rank"=chartMeasurementDF$rankScore,
                                  "miritello"=chartMeasurementDF$miritelloRank,
                                  "raskin"=chartMeasurementDF$RRMScore,
                                  "ipZ"=chartMeasurementDF$ipZ,
                                  "RC"=chartMeasurementDF$score,
                                  "integer"=chartMeasurementDF$ESSScore,
                                  "PA"=chartMeasurementDF$PAScore,
                                  "OSS3"=chartMeasurementDF$OSS3Score,
                                  "OSS2"=chartMeasurementDF$OSS2Scores
                                  )
            # put the rank score if there are no scores
            if(length(which(printScores != "")) == 0) {
              printScores <- chartMeasurementDF$rankScore
            }
          }
          
          # use a function to remove leading zeros
          printScores <- numFormat2Fn(printScores)
          
          ###
          
          if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
            
            ###  upper pneumo  ### 
            
            # get all row indices for the upper pneumo data
            uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
            # then select the row indices for the upper pneumo scores in the chart measurements
            uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
            # get the upper pneumo scores
            uPneumoScores <- printScores[uPneumoSelectRows]
            # get the row indices for the upper pneumo scores in the chartDF
            uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
            # remove NA values
            uPneumoScoreIndices <- uPneumoScoreIndices[!is.na(uPneumoScores)]
            uPneumoScores <- uPneumoScores[!is.na(uPneumoScores)]
            # plot the upper pneumo scores
            if(length(uPneumoScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(uPneumoScoreIndices + 150),
                                y=rep(yOffset['uPneumo'], times=length(uPneumoScores)),
                                label=uPneumoScores,
                                color="black",
                                alpha=.5,
                                size=6)
            }
            
            ### lower pneumo  ###
            
            # get all row indices for the lower pneumo data
            lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
            # then select the row indices for the lower pneumo scores in the chart measurements
            lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
            # get the lower pneumo scores
            lPneumoScores <- printScores[lPneumoSelectRows]
            # get the row indices for the lower pneumo scores in the chartDF
            lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
            # remove NA values
            lPneumoScoreIndices <- lPneumoScoreIndices[!is.na(lPneumoScores)]
            lPneumoScores <- lPneumoScores[!is.na(lPneumoScores)]
            # plot the lower pneumo scores
            if(length(lPneumoScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(lPneumoScoreIndices + 150),
                                y=rep(yOffset['lPneumo'], times=length(lPneumoScores)),
                                label=lPneumoScores,
                                color="black",
                                alpha=.5,
                                size=6)
            }
            
            ### combined pneumo score  ###
            
            # get all row indices for the combined pneumo score
            pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
            # then select the row indices for the combined pneumo scores in the chart measurements
            pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
            # get the combined pneumo scores
            pneumoScores <- printScores[pneumoSelectRows]
            # get the row indices for the combined pneumo scores in the chartDF
            pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
            # calculate the y offset
            yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
            # remove NA values
            pneumoScoreIndices <- pneumoScoreIndices[!is.na(pneumoScores)]
            pneumoScores <- pneumoScores[!is.na(pneumoScores)]
            # plot the combined pneumo scores
            if(length(pneumoScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(pneumoScoreIndices + 150),
                                y=rep(yPneumo, times=length(pneumoScores)),
                                label=pneumoScores,
                                color="black",
                                size=6)
            }
            
          } # end if showPneumoScores == TRUE
          
          ###  EDA  ###
          
          if(isTRUE(showEDAData) && showEDAScores==TRUE) {
            
            ### Auto EDA
            
            # get all row indices for the EDA data
            AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
            # then select the row indices for the EDA scores 
            # in the chart measurements DF
            AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
            # get the EDA scores
            # use printScores to automatically select rank or integer scores
            AutoEDAScores <- printScores[AutoEDASelectRows]
            # get the row indices for the EDA scores in the chartDF
            if(length(AutoEDAScores != 0)) {
              AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                             chartMeasurementDF$eventLabel[AutoEDASelectRows])
              # plot the EDA scores
              if(length(AutoEDAScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(AutoEDAScoreIndices + 150),
                                  y=rep(yOffset['eda'], times=length(AutoEDAScores)),
                                  label=AutoEDAScores,
                                  color="black",
                                  size=7,
                                  na.rm=TRUE)
              }
            }
            
            ### Manual/un-filtered EDA
            
            if(isTRUE(showManualEDA)) {
              
              # get all row indices for the EDA data
              ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
              # then select the row indices for the EDA scores 
              # in the chart measurements DF
              ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
              # get the EDA scores
              # use printScores to automatically select rank or integer scores
              ManualEDAScores <- printScores[ManualEDASelectRows]
              # get the row indices for the EDA scores in the chartDF
              if(length(ManualEDAScores != 0)) {
                ManualEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                                 chartMeasurementDF$eventLabel[ManualEDASelectRows])
                # plot the EDA scores
                if(length(ManualEDAScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(ManualEDAScoreIndices + 150),
                                    y=rep((yOffset['eda']-150), times=length(ManualEDAScores)),
                                    label=ManualEDAScores,
                                    color="black",
                                    alpha=.3,
                                    size=7,
                                    na.rm=TRUE)
                }
              }
              
            } # end if showManualEDA
            
          } # end if showEDAScores
          
          ###  Cardio  ###
          
          if(isTRUE(showCardioData) && showCardioScores==TRUE) {
            
            # get all row indices for the Cardio data
            cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
            # then select the row indices for the Cardio scores in the chart measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio scores
            cardioScores <- printScores[cardioSelectRows]
            # get the row indices for the Cardio scores in the chartDF
            if(length(cardioScores) > 0) {
              cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio scores
              if(length(cardioScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(cardioScoreIndices + 150),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] + 0), times=length(cardioScores)),
                                  label=cardioScores,
                                  color="black",
                                  size=6,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showCardioScores
          
          ###  forearm cuff / finger cuff  ###
          
          if(isTRUE(showFCData) && showFCScores==TRUE) {
            
            # get all row indices for the forearm cuff or finger cuff data
            FCSensorRows <- which(chartMeasurementDF$sensorName == "FC")
            # then select the row indices for the FC scores in the chart measurements
            FCSelectRows <- FCSensorRows[which(FCSensorRows %in% measurementRows)]
            # get the FC scores
            FCScores <- printScores[FCSelectRows]
            # get the row indices for the Cardio scores in the chartDF
            if(length(FCScores) > 0) {
              FCScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[FCSelectRows])
              # plot the FC scores
              if(length(FCScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(FCScoreIndices+150),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] + 0), times=length(FCScores)),
                                  label=FCScores,
                                  color="black",
                                  size=6,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showFCScores
          
          ###  PLE  ###
          
          if(isTRUE(showPLEData) && showPLEScores==TRUE) {
            
            # get all row indices for the PLE data
            pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
            # then select the row indices for the PLE scores in the chart measurements
            pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
            # get the PLE scores
            pleScores <- printScores[pleSelectRows]
            # get the row indices for the PLE scores in the chartDF
            if( length(pleScores) > 0 & !all(is.na(pleScores)) ) {
              pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
              # plot the PLE scores
              if(length(pleScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(pleScoreIndices + 125),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['ple'] - 100), times=length(pleScores)),
                                  label=pleScores,
                                  color="black",
                                  size=6,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showPLEScores
          
        } # end if !is.null(measurementDF)
        
      } # end if showESSScores==TRUE
      
      ################     CQ selection      ##################
      
      if(all(isTRUE(showScores),
             isTRUE(showMeasurements),
             isTRUE(showCQSelection),
             !isTRUE(PCASSFormat) ) ) {
        
        CQXOffset <- -125
        CQYOffset <- 25
        
        # get the measurement data frame
        measurementDFName <- paste0(examName, "_Measurements")
        measurementDF <- get(measurementDFName, pos=1)
        # View(measurementDF)
        
        row.names(measurementDF) <- NULL
        # slice the measurements for the current chart
        chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
        row.names(chartMeasurementDF) <- NULL
        # View(chartMeasurementDF)
        
        # first get all row indices for all integer scores
        measurementRows <- which(chartMeasurementDF$ESSScore != "")
        
        # make a vector of scores for printing
        # use the rank scores if there are no integer scores
        if(selectScores == "auto") {
          ifelse(length(which(chartMeasurementDF$CQName != "")) > 0,
                 printCQNames <- chartMeasurementDF$CQName,
                 printCQNames <- ""
          )
        } else {
          printCQNames <- ""
        }
        
        ###
        
        if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
          
          ###  upper pneumo  ### 
          
          # get all row indices for the upper pneumo data
          uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
          # then select the row indices for the upper pneumo scores in the chart measurements
          uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
          # get the upper pneumo scores
          uPneumoCQNames <- printCQNames[uPneumoSelectRows]
          # get the row indices for the upper pneumo scores in the chartDF
          uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
          # remove NA values
          uPneumoScoreIndices <- uPneumoScoreIndices[!is.na(uPneumoCQNames)]
          uPneumoCQNames <- uPneumoCQNames[!is.na(uPneumoCQNames)]
          # plot the upper pneumo scores
          if(length(uPneumoCQNames) != 0) {
            g <- g + annotate(geom="text",
                              x=(uPneumoScoreIndices + CQXOffset),
                              y=rep(yOffset['uPneumo'] + CQYOffset, times=length(uPneumoCQNames)),
                              label=uPneumoCQNames,
                              color="black",
                              alpha=.5,
                              size=3)
          }
          
          ### lower pneumo  ###
          
          # get all row indices for the lower pneumo data
          lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
          # then select the row indices for the lower pneumo scores in the chart measurements
          lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
          # get the lower pneumo scores
          lPneumoCQNames <- printCQNames[lPneumoSelectRows]
          # get the row indices for the lower pneumo scores in the chartDF
          lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
          # remove NA values
          lPneumoScoreIndices <- lPneumoScoreIndices[!is.na(lPneumoCQNames)]
          lPneumoCQNames <- lPneumoCQNames[!is.na(lPneumoCQNames)]
          # plot the lower pneumo scores
          if(length(lPneumoCQNames) != 0) {
            g <- g + annotate(geom="text",
                              x=(lPneumoScoreIndices + CQXOffset),
                              y=rep(yOffset['lPneumo'] + CQYOffset, times=length(lPneumoCQNames)),
                              label=lPneumoCQNames,
                              color="black",
                              alpha=.5,
                              size=3)
          }
          
          ### combined pneumo score  ###
          
          # get all row indices for the combined pneumo score
          pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
          # then select the row indices for the combined pneumo scores in the chart measurements
          pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
          # get the combined pneumo scores
          pneumoCQNames <- printCQNames[pneumoSelectRows]
          # get the row indices for the combined pneumo scores in the chartDF
          pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
          # calculate the y offset
          yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
          # remove NA values
          pneumoScoreIndices <- pneumoScoreIndices[!is.na(pneumoCQNames)]
          pneumoCQNames <- pneumoCQNames[!is.na(pneumoCQNames)]
          # plot the combined pneumo scores
          # if(length(pneumoCQNames) != 0) {
          #   g <- g + annotate(geom="text",
          #                     x=(pneumoScoreIndices + CQXOffset),
          #                     y=rep(yPneumo, times=length(pneumoCQNames)),
          #                     label=pneumoCQNames,
          #                     color="black",
          #                     size=3)
          # }
          
        } # end if showPneumoScores == TRUE
        
        ###  EDA  ###
        
        if(isTRUE(showEDAData) && showEDAScores==TRUE) {
          
          ### Auto EDA
          
          # get all row indices for the EDA data
          AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
          # then select the row indices for the EDA scores 
          # in the chart measurements DF
          AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
          # get the EDA scores
          # use printCQName to automatically select rank or integer scores
          AutoEDACQNames <- printCQNames[AutoEDASelectRows]
          # get the row indices for the EDA scores in the chartDF
          if(length(AutoEDACQNames != 0)) {
            AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                           chartMeasurementDF$eventLabel[AutoEDASelectRows])
            # plot the EDA scores
            if(length(AutoEDACQNames) != 0) {
              g <- g + annotate(geom="text",
                                x=(AutoEDAScoreIndices + CQXOffset),
                                y=rep(yOffset['eda'] + CQYOffset, times=length(AutoEDACQNames)),
                                label=AutoEDACQNames,
                                color="black",
                                size=3,
                                na.rm=TRUE)
            }
          }
          
          ### Manual/un-filtered EDA
          
          if(isTRUE(showManualEDA)) {
            
            # get all row indices for the EDA data
            ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
            # then select the row indices for the EDA scores 
            # in the chart measurements DF
            ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
            # get the EDA scores
            # use printCQName to automatically select rank or integer scores
            ManualEDACQNames <- printCQNames[ManualEDASelectRows]
            # get the row indices for the EDA scores in the chartDF
            if(length(ManualEDACQNames != 0)) {
              ManualEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                               chartMeasurementDF$eventLabel[ManualEDASelectRows])
              # plot the EDA scores
              if(length(ManualEDACQNames) != 0) {
                g <- g + annotate(geom="text",
                                  x=(ManualEDAScoreIndices + CQXOffset),
                                  y=rep((yOffset['eda']-150+CQYOffset), times=length(ManualEDACQNames)),
                                  label=ManualEDACQNames,
                                  color="black",
                                  alpha=.3,
                                  size=3,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showManualEDA
          
        } # end if showEDAScores
        
        ###  Cardio  ###
        
        if(isTRUE(showCardioData) && showCardioScores==TRUE) {
          
          # get all row indices for the Cardio data
          cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
          # then select the row indices for the Cardio scores in the chart measurements
          cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
          # get the Cardio scores
          cardioCQNames <- printCQNames[cardioSelectRows]
          # get the row indices for the Cardio scores in the chartDF
          if(length(cardioCQNames) > 0) {
            cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
            # plot the Cardio scores
            if(length(cardioCQNames) != 0) {
              g <- g + annotate(geom="text",
                                x=(cardioScoreIndices + CQXOffset),
                                # need to use rep() for y in order to avoid a warning about names
                                y=rep((yOffset['cardio'] + 0 + CQYOffset), times=length(cardioCQNames)),
                                label=cardioCQNames,
                                color="black",
                                size=3,
                                na.rm=TRUE)
            }
          }
          
          ### forearm cuff or finger cuff scores ###
          
          if(showFCData==TRUE && showFCScores==TRUE) {
            
            # get all row indices for the FC data
            FCSensorRows <- which(chartMeasurementDF$sensorName == "FC")
            # then select the row indices for the FC  scores in the chart measurements
            FCSelectRows <- FCSensorRows[which(FCSensorRows %in% measurementRows)]
            # get the FC scores
            FCCQNames <- printCQNames[FCSelectRows]
            # get the row indices for the FC scores in the chartDF
            if(length(FCCQNames) > 0) {
              FCScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[FCSelectRows])
              # plot the FC scores
              if(length(FCCQNames) != 0) {
                g <- g + annotate(geom="text",
                                  x=(FCScoreIndices + CQXOffset),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['FC'] + 100 + CQYOffset), times=length(FCCQNames)),
                                  label=FCCQNames,
                                  color="black",
                                  size=3,
                                  na.rm=TRUE)
              }
            }
            
          } # end showFCScores
          
        } # end if showCardioScores
        
        ###  PLE  ###
        
        if(isTRUE(showPLEData) && showPLEScores==TRUE) {
          
          # get all row indices for the PLE data
          pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
          # then select the row indices for the PLE scores in the chart measurements
          pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
          # get the PLE scores
          pleCQNames <- printCQNames[pleSelectRows]
          # get the row indices for the PLE scores in the chartDF
          if( length(pleCQNames) > 0 & !all(is.na(pleCQNames)) ) {
            pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
            # plot the PLE scores
            if(length(pleCQNames) != 0) {
              g <- g + annotate(geom="text",
                                x=(pleScoreIndices + CQXOffset),
                                # need to use rep() for y in order to avoid a warning about names
                                y=rep((yOffset['ple'] - 100 + CQYOffset), times=length(pleCQNames)),
                                label=pleCQNames,
                                color="black",
                                size=3,
                                na.rm=TRUE)
            }
          }
          
        } # end if showPLEScores
        
      } # end if showCQSelection==TRUE
      
      ################     logged R/C ratio     ##################
      
      if(all(isTRUE(showScores),
             isTRUE(showMeasurements),
             isTRUE(showRCRatio),
             !isTRUE(PCASSFormat) ) ) {
        
        RCXOffset <- 575
        RCYoffset <- -25
        
        # get the measurement data frame
        measurementDFName <- paste0(examName, "_Measurements")
        measurementDF <- get(measurementDFName, pos=1)
        # View(measurementDF)
        
        if(!is.null(measurementDF)) {
          
          row.names(measurementDF) <- NULL
          # slice the measurements for the current chart
          chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
          row.names(chartMeasurementDF) <- NULL
          # View(chartMeasurementDF)
          
          # first get all row indices for all integer scores
          measurementRows <- which(chartMeasurementDF$ESSScore != "")
          
          # make a vector of logged R/C ratios for printing
          if(selectScores == "auto") {
            ifelse(length(which(chartMeasurementDF$RCScore != "")) > 0,
                   printRCScores <- chartMeasurementDF$RCScore,
                   printRCScores <- ""
            )
          } else {
            printRCScores <- ""
          }
          
          # format the logged R/C Scores for printing #
          
          printRCScores <- numFormat3Fn(printRCScores)
          
          ###
          
          if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
            
            ###  upper pneumo  ### 
            
            # get all row indices for the upper pneumo data
            uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
            # then select the row indices for the upper pneumo logged R/C ratios in the chart measurements
            uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
            # get the upper pneumo logged R/C ratios
            uPneumoRCScores <- printRCScores[uPneumoSelectRows]
            # get the row indices for the upper pneumo logged R/C ratios in the chartDF
            uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
            # remove NA values
            uPneumoScoreIndices <- uPneumoScoreIndices[!is.na(uPneumoRCScores)]
            uPneumoRCScores <- uPneumoRCScores[!is.na(uPneumoRCScores)]
            # plot the upper pneumo logged R/C ratios
            if(length(uPneumoRCScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(uPneumoScoreIndices + RCXOffset),
                                y=rep(yOffset['uPneumo'] + RCYoffset, times=length(uPneumoRCScores)),
                                label=uPneumoRCScores,
                                color="black",
                                alpha=.5,
                                size=3)
            }
            
            ### lower pneumo  ###
            
            # get all row indices for the lower pneumo data
            lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
            # then select the row indices for the lower pneumo logged R/C ratios in the chart measurements
            lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
            # get the lower pneumo logged R/C ratios
            lPneumoRCScores <- printRCScores[lPneumoSelectRows]
            # get the row indices for the lower pneumo logged R/C ratios in the chartDF
            lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
            # remove NA values
            lPneumoScoreIndices <- lPneumoScoreIndices[!is.na(lPneumoRCScores)]
            lPneumoRCScores <- lPneumoRCScores[!is.na(lPneumoRCScores)]
            # plot the lower pneumo logged R/C ratios
            if(length(lPneumoRCScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(lPneumoScoreIndices + RCXOffset),
                                y=rep(yOffset['lPneumo'] + RCYoffset, times=length(lPneumoRCScores)),
                                label=lPneumoRCScores,
                                color="black",
                                alpha=.5,
                                size=3)
            }
            
            ### combined pneumo score  ###
            
            # get all row indices for the combined pneumo score
            pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
            # then select the row indices for the combined pneumo logged R/C ratios in the chart measurements
            pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
            # get the combined pneumo logged R/C ratios
            pneumoRCScores <- printRCScores[pneumoSelectRows]
            # get the row indices for the combined pneumo logged R/C ratios in the chartDF
            pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
            # calculate the y offset
            yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
            # remove NA values
            pneumoScoreIndices <- pneumoScoreIndices[!is.na(pneumoRCScores)]
            pneumoRCScores <- pneumoRCScores[!is.na(pneumoRCScores)]
            # plot the combined pneumo logged R/C ratios
            # if(length(pneumoRCScores) != 0) {
            #   g <- g + annotate(geom="text",
            #                     x=(pneumoScoreIndices + RCXOffset),
            #                     y=rep(yPneumo + RCYoffset, times=length(pneumoRCScores)),
            #                     label=pneumoRCScores,
            #                     color="black",
            #                     size=3)
            # }
            
          } # end if showPneumoScores == TRUE
          
          ###  EDA  ###
          
          if(isTRUE(showEDAData) && showEDAScores==TRUE) {
              
              ### Auto EDA
              
              # get all row indices for the EDA data
              AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
              # then select the row indices for the EDA logged R/C ratios 
              # in the chart measurements DF
              AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
              # get the EDA logged R/C ratios
              # use printCQName to automatically select rank or integer logged R/C ratios
              AutoEDARCScores <- printRCScores[AutoEDASelectRows]
              # get the row indices for the EDA logged R/C ratios in the chartDF
              if(length(AutoEDARCScores != 0)) {
                AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                               chartMeasurementDF$eventLabel[AutoEDASelectRows])
                # plot the EDA logged R/C ratios
                if(length(AutoEDARCScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(AutoEDAScoreIndices + RCXOffset),
                                    y=rep(yOffset['eda'] + RCYoffset, times=length(AutoEDARCScores)),
                                    label=AutoEDARCScores,
                                    color="black",
                                    size=3,
                                    na.rm=TRUE)
                }
              }
              
              ### Manual/un-filtered EDA
              
              if(isTRUE(showManualEDA)) {
                
                # get all row indices for the EDA data
                ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
                # then select the row indices for the EDA logged R/C ratios 
                # in the chart measurements DF
                ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
                # get the EDA logged R/C ratios
                # use printCQName to automatically select rank or integer logged R/C ratios
                ManualEDARCScores <- printRCScores[ManualEDASelectRows]
                # get the row indices for the EDA logged R/C ratios in the chartDF
                if(length(ManualEDARCScores != 0)) {
                  ManualEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                                   chartMeasurementDF$eventLabel[ManualEDASelectRows])
                  # plot the EDA logged R/C ratios
                  if(length(ManualEDARCScores) != 0) {
                    g <- g + annotate(geom="text",
                                      x=(ManualEDAScoreIndices + RCXOffset),
                                      y=rep((yOffset['eda']-150 + RCYoffset), times=length(ManualEDARCScores)),
                                      label=ManualEDARCScores,
                                      color="black",
                                      alpha=.3,
                                      size=3,
                                      na.rm=TRUE)
                  } # end if
                } # end if
                
              } # end if showManualEDA
              
            } # end if showEDAScores
          
          ###  Cardio  ###
          
          if(isTRUE(showCardioData) && showCardioScores==TRUE) {
            
            # get all row indices for the Cardio data
            cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
            # then select the row indices for the Cardio logged R/C ratios in the chart measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio logged R/C ratios
            cardioRCScores <- printRCScores[cardioSelectRows]
            # get the row indices for the Cardio logged R/C ratios in the chartDF
            if(length(cardioRCScores) > 0) {
              cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio logged R/C ratios
              if(length(cardioRCScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(cardioScoreIndices + RCXOffset),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] + 0 + RCYoffset), times=length(cardioRCScores)),
                                  label=cardioRCScores,
                                  color="black",
                                  size=3,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showCardioScores
          
          ###  PLE  ###
          
          if(isTRUE(showPLEData) && showPLEScores==TRUE) {
            
            # get all row indices for the PLE data
            pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
            # then select the row indices for the PLE logged R/C ratios in the chart measurements
            pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
            # get the PLE logged R/C ratios
            pleRCScores <- printRCScores[pleSelectRows]
            # get the row indices for the PLE logged R/C ratios in the chartDF
            if( length(pleRCScores) > 0 & !all(is.na(pleRCScores)) ) {
              pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
              # plot the PLE logged R/C ratios
              if(length(pleRCScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(pleScoreIndices + RCXOffset),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['ple'] - 100 + RCYoffset), times=length(pleRCScores)),
                                  label=pleRCScores,
                                  color="black",
                                  size=3,
                                  na.rm=TRUE)
              } # end if
            } # end if
            
          } # end if show PLE Data 
          
        } # end if !is.null(measurementDF)
        
      } # end if showRCRatios==TRUE
      
      ################     ipsative Z Scores     ##################
      
      if(all(isTRUE(showScores),
             isTRUE(showMeasurements),
             isTRUE(showIPZScores),
             !isTRUE(PCASSFormat) ) ) {
        
        # get the measurement data frame
        measurementDFName <- paste0(examName, "_Measurements")
        measurementDF <- get(measurementDFName, pos=1)
        # View(measurementDF)
        
        if(!is.null(measurementDF)) {
          
          row.names(measurementDF) <- NULL
          # slice the measurements for the current chart
          chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
          row.names(chartMeasurementDF) <- NULL
          # View(chartMeasurementDF)
          
          # first get all row indices for all ipsative Z Scores
          measurementRows <- which(chartMeasurementDF$ipZScore != "")
          
          # make a vector of ipsative Z Scores for printing
          if(selectScores == "auto") {
            ifelse(length(which(chartMeasurementDF$ipZScore != "")) > 0,
                   printIPZScores <- chartMeasurementDF$ipZScore,
                   printIPZScores <- ""
            )
          } else {
            printIPZScores <- ""
          }
          
          # format the ipsative Z Scores for printing #
          
          printIPZScores <- numFormat3Fn(printIPZScores)
          
          ###
          
          if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
            
            ###  upper pneumo ipsative Z Scores ### 
            
            # get all row indices for the upper pneumo data
            uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
            # then select the row indices for the upper pneumo ipsative Z Scores in the chart measurements
            uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
            # get the upper pneumo ipsative Z Scores
            uPneumoIPZScores <- printIPZScores[uPneumoSelectRows]
            # get the row indices for the upper pneumo ipsative Z Scores in the chartDF
            uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
            # remove NA values
            uPneumoScoreIndices <- uPneumoScoreIndices[!is.na(uPneumoIPZScores)]
            uPneumoIPZScores <- uPneumoIPZScores[!is.na(uPneumoIPZScores)]
            # plot the upper pneumo ipsative Z Scores
            if(length(uPneumoIPZScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(uPneumoScoreIndices+300),
                                y=rep((yOffset['uPneumo'] + 75), times=length(uPneumoIPZScores)),
                                label=uPneumoIPZScores,
                                color="black",
                                alpha=.3,
                                size=2.5)
            }
            
            ### lower pneumo ipsative Z Scores ###
            
            # get all row indices for the lower pneumo data
            lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
            # then select the row indices for the lower pneumo ipsative Z Scores in the chart measurements
            lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
            # get the lower pneumo ipsative Z Scores
            lPneumoIPZScores <- printIPZScores[lPneumoSelectRows]
            # get the row indices for the lower pneumo ipsative Z Scores in the chartDF
            lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
            # remove NA values
            lPneumoScoreIndices <- lPneumoScoreIndices[!is.na(lPneumoIPZScores)]
            lPneumoIPZScores <- lPneumoIPZScores[!is.na(lPneumoIPZScores)]
            # plot the lower pneumo ipsative Z Scores
            if(length(lPneumoIPZScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(lPneumoScoreIndices+300),
                                y=rep((yOffset['lPneumo']+75), times=length(lPneumoIPZScores)),
                                label=lPneumoIPZScores,
                                color="black",
                                alpha=.3,
                                size=2.5)
            }
            
            ### combined pneumo score ipsative Z Scores ###
            
            # get all row indices for the combined pneumo score
            pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
            # then select the row indices for the combined pneumo ipsative Z Scores in the chart measurements
            pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
            # get the combined pneumo ipsative Z Scores
            pneumoIPZScores <- printIPZScores[pneumoSelectRows]
            # get the row indices for the combined pneumo ipsative Z Scores in the chartDF
            pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
            # calculate the y offset
            yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
            # remove NA values
            pneumoScoreIndices <- pneumoScoreIndices[!is.na(pneumoIPZScores)]
            pneumoIPZScores <- pneumoIPZScores[!is.na(pneumoIPZScores)]
            # plot the combined pneumo ipsative Z Scores
            if(length(pneumoIPZScores) != 0) {
              g <- g + annotate(geom="text",
                                x=(pneumoScoreIndices),
                                y=rep(yPneumo+100, times=length(pneumoIPZScores)),
                                label=pneumoIPZScores,
                                color="black",
                                alpha=.3,
                                size=3)
            }
            
          } # end if showPneumoScores == TRUE
          
          ###  EDA ipsative Z Scores ###
          
          if(isTRUE(showEDAData) && showEDAScores==TRUE) {
            
            ### Auto EDA ipsative Z Scores
            
            # get all row indices for the EDA data
            AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
            # then select the row indices for the EDA ipsative Z Scores 
            # in the chart measurements DF
            AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
            # get the EDA ipsative Z Scores
            # use printCQName to automatically select rank or integer ipsative Z Scores
            AutoEDAipZScores <- printIPZScores[AutoEDASelectRows]
            # get the row indices for the EDA ipsative Z Scores in the chartDF
            if(length(AutoEDAipZScores != 0)) {
              AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                             chartMeasurementDF$eventLabel[AutoEDASelectRows])
              # plot the EDA ipsative Z Scores
              if(length(AutoEDAipZScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(AutoEDAScoreIndices+300),
                                  y=rep((yOffset['eda']+75), times=length(AutoEDAipZScores)),
                                  label=AutoEDAipZScores,
                                  color="black",
                                  size=2.5,
                                  na.rm=TRUE)
              }
            }
            
            ### Manual/un-filtered EDA ipsative Z Scores
            
            # if(isTRUE(showManualEDA)) {
            #   
            #   # get all row indices for the EDA data
            #   ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
            #   # then select the row indices for the EDA ipsative Z Scores
            #   # in the chart measurements DF
            #   ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
            #   # get the EDA ipsative Z Scores
            #   # use printCQName to automatically select rank or integer ipsative Z Scores
            #   ManualEDipZCScores <- printIPZScores[ManualEDASelectRows]
            #   # get the row indices for the EDA ipsative Z Scores in the chartDF
            #   if(length(ManualEDAipZScores != 0)) {
            #     ManualEDAScoreIndices <- which(chartDF$eventLabel %in%
            #                                      chartMeasurementDF$eventLabel[ManualEDASelectRows])
            #     # plot the EDA ipsative Z Scores
            #     if(length(ManualEDipZCScores) != 0) {
            #       g <- g + annotate(geom="text",
            #                         x=(ManualEDAScoreIndices+300),
            #                         y=rep((yOffset['eda']-50), times=length(ManualEDipZCScores)),
            #                         label=ManualEDipZCScores,
            #                         color="black",
            #                         alpha=.3,
            #                         size=2.5,
            #                         na.rm=TRUE)
            #     } # end if
            #   } # end if
            #   
            # } # end if showManualEDA

          } # end if showEDAScores
          
          ###  Cardio ipsative Z Scores ###
          
          if(isTRUE(showCardioData) && showCardioScores==TRUE) {

            # get all row indices for the Cardio data
            cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
            # then select the row indices for the Cardio ipsative Z Scores in the chart measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio ipsative Z Scores
            cardioIPZScores <- printIPZScores[cardioSelectRows]
            # get the row indices for the Cardio ipsative Z Scores in the chartDF
            if(length(cardioIPZScores) > 0) {
              cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio ipsative Z Scores
              if(length(cardioIPZScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(cardioScoreIndices + 300),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] + 75 + 0), times=length(cardioIPZScores)),
                                  label=cardioIPZScores,
                                  color="black",
                                  size=2.5,
                                  na.rm=TRUE)
              }
            }

          } # end if showCardioScores
          
          ###  PLE ipsative Z Scores ###
          
          if(isTRUE(showPLEData) && showPLEScores==TRUE) {

            # get all row indices for the PLE data
            pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
            # then select the row indices for the PLE ipsative Z Scores in the chart measurements
            pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
            # get the PLE ipsative Z Scores
            pleIPZScores <- printIPZScores[pleSelectRows]
            # get the row indices for the PLE ipsative Z Scores ratios in the chartDF
            if( length(pleIPZScores) > 0 & !all(is.na(pleIPZScores)) ) {
              pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
              # plot the PLE ipsative Z Scores
              if(length(pleIPZScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(pleScoreIndices + 300),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['ple'] - 25), times=length(pleIPZScores)),
                                  label=pleIPZScores,
                                  color="black",
                                  size=2.5,
                                  na.rm=TRUE)
              } # end if
            } # end if

          } # end if show PLE Data
          
        } # end if !is.null(measurementDF)
        
      } # end if showIPZScores==TRUE
      
      ################     OSS-3 Scores     ##################
      
      if(all(isTRUE(showScores),
             isTRUE(showMeasurements),
             isTRUE(showOSS3Scores),
             !isTRUE(PCASSFormat) ) ) {
        
        # get the measurement data frame
        measurementDFName <- paste0(examName, "_Measurements")
        measurementDF <- get(measurementDFName, pos=1)
        # View(measurementDF)
        
        if(!is.null(measurementDF)) {
          
          row.names(measurementDF) <- NULL
          # slice the measurements for the current chart
          chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
          row.names(chartMeasurementDF) <- NULL
          # View(chartMeasurementDF)
          
          # first get all row indices for all OSS-3 Scores
          measurementRows <- which(chartMeasurementDF$OSS3Score != "")
          
          # make a vector of OSS-3 Scores for printing
          if(selectScores == "auto") {
            ifelse(length(which(chartMeasurementDF$OSS3Score != "")) > 0,
                   printOSS3Scores <- chartMeasurementDF$OSS3Score,
                   printOSS3Scores <- ""
            )
          } else {
            printOSS3Scores <- ""
          }
          
          # format the OSS-3 Scores for printing #
          
          printOSS3Scores <- numFormat3Fn(printOSS3Scores)
          
          ###
          
          if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
            
            ###  upper pneumo ipsative Z Scores ### 
            
            # get all row indices for the upper pneumo data
            uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
            # then select the row indices for the upper pneumo OSS-3 Scores in the chart measurements
            uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
            # get the upper pneumo OSS-3 Scores
            uPneumoOSS3Scores <- printOSS3Scores[uPneumoSelectRows]
            # get the row indices for the upper pneumo OSS-3 Scores in the chartDF
            uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
            # remove NA values
            uPneumoScoreIndices <- uPneumoScoreIndices[!is.na(uPneumoOSS3Scores)]
            uPneumoOSS3Scores <- uPneumoOSS3Scores[!is.na(uPneumoOSS3Scores)]
            # plot the upper pneumo OSS-3 Scores
            if(length(uPneumoOSS3Scores) != 0) {
              g <- g + annotate(geom="text",
                                x=(uPneumoScoreIndices+200),
                                y=rep((yOffset['uPneumo']+90), times=length(uPneumoOSS3Scores)),
                                label=uPneumoOSS3Scores,
                                color="black",
                                alpha=.3,
                                size=3)
            }
            
            ### lower pneumo OSS-3 Scores ###
            
            # get all row indices for the lower pneumo data
            lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
            # then select the row indices for the lower pneumo OSS-3 Scores in the chart measurements
            lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
            # get the lower pneumo OSS-3 Scores
            lPneumoOSS3Scores <- printOSS3Scores[lPneumoSelectRows]
            # get the row indices for the lower pneumo OSS-3 Scores in the chartDF
            lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
            # remove NA values
            lPneumoScoreIndices <- lPneumoScoreIndices[!is.na(lPneumoOSS3Scores)]
            lPneumoOSS3Scores <- lPneumoOSS3Scores[!is.na(lPneumoOSS3Scores)]
            # plot the lower pneumo OSS03 Scores
            if(length(lPneumoOSS3Scores) != 0) {
              g <- g + annotate(geom="text",
                                x=(lPneumoScoreIndices+200),
                                y=rep((yOffset['lPneumo']+90), times=length(lPneumoOSS3Scores)),
                                label=lPneumoOSS3Scores,
                                color="black",
                                alpha=.3,
                                size=3)
            }
            
            ### combined pneumo score OSS-3 Scores ###
            
            # get all row indices for the combined pneumo score
            pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
            # then select the row indices for the combined pneumo OSS-3 Scores in the chart measurements
            pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
            # get the combined pneumo OSS-3 Scores
            pneumoOSS3cores <- printOSS3Scores[pneumoSelectRows]
            # get the row indices for the combined pneumo OSS-3 Scores in the chartDF
            pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
            # calculate the y offset
            yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
            # remove NA values
            pneumoScoreIndices <- pneumoScoreIndices[!is.na(pneumoOSS3cores)]
            pneumoOSS3cores <- pneumoOSS3cores[!is.na(pneumoOSS3cores)]
            # plot the combined pneumo ipsative Z Scores
            if(length(pneumoOSS3cores) != 0) {
              g <- g + annotate(geom="text",
                                x=(pneumoScoreIndices-200),
                                y=rep(yPneumo, times=length(pneumoOSS3cores)),
                                label=pneumoOSS3cores,
                                color="black",
                                alpha=.95,
                                size=3)
            }
            
          } # end if showPneumoScores == TRUE
          
          ###  EDA OSS-3##
          
          if(isTRUE(showEDAData) && showEDAScores==TRUE) {
            
            ### Auto EDA OSS-3 Scores
            
            # get all row indices for the EDA data
            AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
            # then select the row indices for the EDA OSS-3 Scores 
            # in the chart measurements DF
            AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
            # get the EDA OSS-3 Scores
            # use printCQName to automatically select rank or integer OSS-3 Scores
            AutoEDAOSS3Scores <- printOSS3Scores[AutoEDASelectRows]
            # get the row indices for the EDA OSS-3 Scores in the chartDF
            if(length(AutoEDAOSS3Scores != 0)) {
              AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                             chartMeasurementDF$eventLabel[AutoEDASelectRows])
              # plot the EDA OSS-3 Scores
              if(length(AutoEDAOSS3Scores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(AutoEDAScoreIndices+200),
                                  y=rep((yOffset['eda']+90), times=length(AutoEDAOSS3Scores)),
                                  label=AutoEDAOSS3Scores,
                                  color="black",
                                  size=3,
                                  na.rm=TRUE)
              }
            }
            
            ### Manually centered EDA OSS-3 Scores
            
            if(isTRUE(showManualEDA)) {
              
              # get all row indices for the EDA data
              ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
              # then select the row indices for the EDA OSS-3 Scores
              # in the chart measurements DF
              ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
              # get the EDA OSS-3 Scores
              # use printCQName to automatically select rank or integer OSS-3 Scores
              ManualEDAOSS3CScores <- printOSS3Scores[ManualEDASelectRows]
              # get the row indices for the EDA OSS-3 Scores in the chartDF
              if(length(ManualEDAOSS3CScores != 0)) {
                ManualEDAScoreIndices <- which(chartDF$eventLabel %in%
                                                 chartMeasurementDF$eventLabel[ManualEDASelectRows])
                # plot the EDA OSS-3 Scores
                if(length(ManualEDAOSS3CScores) != 0) {
                  # g <- g + annotate(geom="text",
                  #                   x=(ManualEDAScoreIndices+200),
                  #                   y=rep((yOffset['eda']+90), times=length(ManualEDAOSS3CScores)),
                  #                   label=ManualEDAOSS3CScores,
                  #                   color="black",
                  #                   alpha=.3,
                  #                   size=3,
                  #                   na.rm=TRUE)
                } # end if
              } # end if
              
            } # end if showManualEDA
            
          } # end if showEDAScores
          
          ###  Cardio OSS-3 Scores ###
          
          if(isTRUE(showCardioData) && showCardioScores==TRUE) {
            
            # get all row indices for the Cardio data
            cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
            # then select the row indices for the Cardio OSS-3 Scores in the chart measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio OSS-3 Scores
            cardioOSS3Scores <- printOSS3Scores[cardioSelectRows]
            # get the row indices for the Cardio OSS-3 Scores in the chartDF
            if(length(cardioOSS3Scores) > 0) {
              cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio OSS-3 Scores
              if(length(cardioOSS3Scores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(cardioScoreIndices+200),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] + 90 + 0), times=length(cardioOSS3Scores)),
                                  label=cardioOSS3Scores,
                                  color="black",
                                  size=3,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showCardioScores
          
          ###  PLE OSS-3 Scores ###
          
          if(isTRUE(showPLEData) && showPLEScores==TRUE) {
            
            # get all row indices for the PLE data
            pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
            # then select the row indices for the PLE OSS-3 Scores in the chart measurements
            pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
            # get the PLE OSS-3 Scores
            pleOSS3Scores <- printOSS3Scores[pleSelectRows]
            # get the row indices for the PLE OSS-3 Scores ratios in the chartDF
            if( length(pleOSS3Scores) > 0 & !all(is.na(pleOSS3Scores)) ) {
              pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
              # plot the PLE ipsative Z Scores
              if(length(pleOSS3Scores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(pleScoreIndices+100),
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['ple']+90), times=length(pleOSS3Scores)),
                                  label=pleOSS3Scores,
                                  color="black",
                                  size=3,
                                  na.rm=TRUE)
              } # end if
            } # end if
            
          } # end if show PLE Data
          
        } # end if !is.null(measurementDF)
        
      } # end if showOSS3Scores==TRUE
      
      ################     OSS-2 Scores     ##################
      
      if(all(isTRUE(showScores),
             isTRUE(showMeasurements),
             isTRUE(showOSS2Scores),
             !isTRUE(PCASSFormat) ) ) {
        
        # get the measurement data frame
        measurementDFName <- paste0(examName, "_Measurements")
        measurementDF <- get(measurementDFName, pos=1)
        # View(measurementDF)
        
        if(!is.null(measurementDF)) {
          
          row.names(measurementDF) <- NULL
          # slice the measurements for the current chart
          chartMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
          row.names(chartMeasurementDF) <- NULL
          # View(chartMeasurementDF)
          
          # first get all row indices for all OSS-3 Scores
          measurementRows <- which(chartMeasurementDF$OSS3Score != "")
          
          # make a vector of OSS-2 Scores for printing
          if(selectScores == "auto") {
            ifelse(length(which(chartMeasurementDF$OSS2Score != "")) > 0,
                   printOSS2Scores <- chartMeasurementDF$OSS2Score,
                   printOSS2Scores <- ""
            )
          } else {
            printOSS2Scores <- ""
          }
          
          # format the OSS-2 Scores for printing #
          
          printOSS2Scores <- numFormatIntFn(printOSS2Scores)
          
          ###
          
          if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
            
            ###  upper pneumo ipsative Z Scores ### 
            
            # get all row indices for the upper pneumo data
            uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
            # then select the row indices for the upper pneumo OSS-2 Scores in the chart measurements
            uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
            # get the upper pneumo OSS-2 Scores
            uPneumoOSS2Scores <- printOSS2Scores[uPneumoSelectRows]
            # get the row indices for the upper pneumo OSS-2 Scores in the chartDF
            uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
            # remove NA values
            uPneumoScoreIndices <- uPneumoScoreIndices[!is.na(uPneumoOSS2Scores)]
            uPneumoOSS2Scores <- uPneumoOSS2Scores[!is.na(uPneumoOSS2Scores)]
            # plot the upper pneumo OSS-2 Scores
            if(length(uPneumoOSS2Scores) != 0) {
              g <- g + annotate(geom="text",
                                x=(uPneumoScoreIndices-200),
                                y=rep((yOffset['uPneumo']-35), times=length(uPneumoOSS2Scores)),
                                label=uPneumoOSS2Scores,
                                color="black",
                                alpha=.3,
                                size=3)
            }
            
            ### lower pneumo OSS-2 Scores ###
            
            # get all row indices for the lower pneumo data
            lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
            # then select the row indices for the lower pneumo OSS-2 Scores in the chart measurements
            lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
            # get the lower pneumo OSS-2 Scores
            lPneumoOSS2Scores <- printOSS2Scores[lPneumoSelectRows]
            # get the row indices for the lower pneumo OSS-2 Scores in the chartDF
            lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
            # remove NA values
            lPneumoScoreIndices <- lPneumoScoreIndices[!is.na(lPneumoOSS2Scores)]
            lPneumoOSS2Scores <- lPneumoOSS2Scores[!is.na(lPneumoOSS2Scores)]
            # plot the lower pneumo OSS03 Scores
            if(length(lPneumoOSS2Scores) != 0) {
              g <- g + annotate(geom="text",
                                x=(lPneumoScoreIndices-200),
                                y=rep((yOffset['lPneumo']-35), times=length(lPneumoOSS2Scores)),
                                label=lPneumoOSS2Scores,
                                color="black",
                                alpha=.3,
                                size=3)
            }
            
            ### combined pneumo score OSS-2 Scores ###
            
            # get all row indices for the combined pneumo score
            pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
            # then select the row indices for the combined pneumo OSS-2 Scores in the chart measurements
            pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
            # get the combined pneumo OSS-2 Scores
            pneumoOSS2Scores <- printOSS2Scores[pneumoSelectRows]
            # get the row indices for the combined pneumo OSS-2 Scores in the chartDF
            pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
            # calculate the y offset
            yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
            # remove NA values
            pneumoScoreIndices <- pneumoScoreIndices[!is.na(pneumoOSS2Scores)]
            pneumoOSS2Scores <- pneumoOSS2Scores[!is.na(pneumoOSS2Scores)]
            # plot the combined pneumo ipsative Z Scores
            if(length(pneumoOSS2Scores) != 0) {
              g <- g + annotate(geom="text",
                                x=(pneumoScoreIndices-200),
                                y=rep(yPneumo-35, times=length(pneumoOSS2Scores)),
                                label=pneumoOSS2Scores,
                                color="black",
                                alpha=.95,
                                size=3)
            }
            
          } # end if showPneumoScores == TRUE
          
          ###  EDA OSS-2  ##
          
          if(isTRUE(showEDAData) && showEDAScores==TRUE) {
            
            ### Auto EDA OSS-2 Scores
            
            # get all row indices for the EDA data
            AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
            # then select the row indices for the EDA OSS-2 Scores 
            # in the chart measurements DF
            AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
            # get the EDA OSS-3 Scores
            # use printCQName to automatically select rank or integer OSS-2 Scores
            AutoEDAOSS2Scores <- printOSS2Scores[AutoEDASelectRows]
            # get the row indices for the EDA OSS-2 Scores in the chartDF
            if(length(AutoEDAOSS2Scores != 0)) {
              AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                             chartMeasurementDF$eventLabel[AutoEDASelectRows])
              # plot the EDA OSS-2 Scores
              if(length(AutoEDAOSS2Scores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(AutoEDAScoreIndices-200),
                                  y=rep((yOffset['eda']-35), times=length(AutoEDAOSS2Scores)),
                                  label=AutoEDAOSS2Scores,
                                  color="black",
                                  size=3,
                                  na.rm=TRUE)
              }
            }
            
            ### Manually centered EDA OSS-2 Scores
            
            if(isTRUE(showManualEDA)) {
              
              # get all row indices for the EDA data
              ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
              # then select the row indices for the EDA OSS-2 Scores
              # in the chart measurements DF
              ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
              # get the EDA OSS-2 Scores
              # use printCQName to automatically select rank or integer OSS-2 Scores
              ManualEDAOSS2Scores <- printOSS2Scores[ManualEDASelectRows]
              # get the row indices for the EDA OSS-2 Scores in the chartDF
              if(length(ManualEDAOSS2Scores != 0)) {
                ManualEDAScoreIndices <- which(chartDF$eventLabel %in%
                                                 chartMeasurementDF$eventLabel[ManualEDASelectRows])
                # plot the EDA OSS-2 Scores
                if(length(ManualEDAOSS2Scores) != 0) {
                  # g <- g + annotate(geom="text",
                  #                   x=(ManualEDAScoreIndices-200),
                  #                   y=rep((yOffset['eda']-35), times=length(ManualEDAOSS22Scores)),
                  #                   label=ManualEDAOSS2Scores,
                  #                   color="black",
                  #                   alpha=.3,
                  #                   size=3,
                  #                   na.rm=TRUE)
                } # end if
              } # end if
              
            } # end if showManualEDA
            
          } # end if showEDAScores
          
          ###  Cardio OSS-2 Scores ###
          
          if(isTRUE(showCardioData) && showCardioScores==TRUE) {
            
            # get all row indices for the Cardio data
            cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
            # then select the row indices for the Cardio OSS-2 Scores in the chart measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio OSS-2 Scores
            cardioOSS2Scores <- printOSS2Scores[cardioSelectRows]
            # get the row indices for the Cardio OSS-2 Scores in the chartDF
            if(length(cardioOSS2Scores) > 0) {
              cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio OSS-2 Scores
              if(length(cardioOSS2Scores) != 0) {
                # g <- g + annotate(geom="text",
                #                   x=(cardioScoreIndices-200),
                #                   # need to use rep() for y in order to avoid a warning about names
                #                   y=rep((yOffset['cardio'] -35 + 0), times=length(cardioOSS2Scores)),
                #                   label=cardioOSS2Scores,
                #                   color="black",
                #                   size=3,
                #                   na.rm=TRUE)
              }
            }
            
          } # end if showCardioScores
          
          ###  PLE OSS-2 Scores ###
          
          if(isTRUE(showPLEData) && showPLEScores==TRUE) {
            
            # get all row indices for the PLE data
            pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
            # then select the row indices for the PLE OSS-2 Scores in the chart measurements
            pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
            # get the PLE OSS-2 Scores
            pleOSS2Scores <- printOSS2Scores[pleSelectRows]
            # get the row indices for the PLE OSS-2 Scores ratios in the chartDF
            if( length(pleOSS2Scores) > 0 & !all(is.na(pleOSS2Scores)) ) {
              pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
              # plot the PLE ipsative Z Scores
              # if(length(pleOSS2Scores) != 0) {
              #   g <- g + annotate(geom="text",
              #                     x=(pleScoreIndices-2--),
              #                     # need to use rep() for y in order to avoid a warning about names
              #                     y=rep((yOffset['ple']-35+90), times=length(pleOSS2Scores)),
              #                     label=pleOSS2Scores,
              #                     color="black",
              #                     size=3,
              #                     na.rm=TRUE)
              # } # end if
            } # end if
            
          } # end if show PLE Data
          
        } # end if !is.null(measurementDF)
        
      } # end if showOSS2Scores==TRUE
      
      ################     Rank Scores     ##################
      
      if(isTRUE(showScores) && isTRUE(showRankValues)) {
        
        if(all(isTRUE(showMeasurements),
               isTRUE(showRankValues),
               !isTRUE(PCASSFormat),
               # omit the rank score if there are no ESS scores
               # because the rank scores will already be used instead
               length(which(chartMeasurementDF$ESSScore != "")) > 0 ) ) {
          
          # get the measurement data frame
          measurementDFName <- paste0(examName, "_Measurements")
          measurementDF <- get(measurementDFName, pos=1)
          # View(measurementDF)
          
          if(!is.null(measurementDF)) {
            
            row.names(measurementDF) <- NULL
            # slice the measurements for the current chart
            chartMeasurementDF <- 
              measurementDF[measurementDF$seriesName == seriesName & measurementDF$chartName == chartName,]
            row.names(chartMeasurementDF) <- NULL
            # View(chartMeasurementDF)
            
            # first get all row indices for all Rank Scores
            measurementRows <- which(chartMeasurementDF$rankScore != "")
            
            # make a vector of Rank Scores for printing
            if(selectScores == "auto") {
              ifelse(length(measurementRows) > 0,
                     printRankScores <- chartMeasurementDF$rankScore,
                     printRankScores <- ""
              )
            } else {
              printRankScores <- ""
            }
            
            # format the Rank Scores for printing #
            
            
            printRankScores <- numFormat2Fn(printRankScores)
            
            ###
            
            if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
              
              ###  upper pneumo Rank Scores ### 
              
              # get all row indices for the upper pneumo data
              uPneumoSensorRows <- which(chartMeasurementDF$sensorName == "UPneumo")
              # then select the row indices for the upper pneumo Rank Scores in the chart measurements
              uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
              # get the upper pneumo Rank Scores
              uPneumoRankScores <- printRankScores[uPneumoSelectRows]
              # get the row indices for the upper pneumo Rank Scores in the chartDF
              uPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[uPneumoSelectRows])
              # remove NA values
              uPneumoScoreIndices <- uPneumoScoreIndices[!is.na(uPneumoRankScores)]
              uPneumoRankScores <- uPneumoRankScores[!is.na(uPneumoRankScores)]
              # plot the upper pneumo Rank Scores
              if(length(uPneumoRankScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(uPneumoScoreIndices+0),
                                  y=rep((yOffset['uPneumo'] + 75), times=length(uPneumoRankScores)),
                                  label=uPneumoRankScores,
                                  color="black",
                                  alpha=.5,
                                  size=2)
              }
              
              ### lower pneumo Rank Scores ###
              
              # get all row indices for the lower pneumo data
              lPneumoSensorRows <- which(chartMeasurementDF$sensorName == "LPneumo")
              # then select the row indices for the lower pneumo Rank Scores in the chart measurements
              lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
              # get the lower pneumo Rank Scores
              lPneumoRankScores <- printRankScores[lPneumoSelectRows]
              # get the row indices for the lower pneumo Rank Scores in the chartDF
              lPneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[lPneumoSelectRows])
              # remove NA values
              lPneumoScoreIndices <- lPneumoScoreIndices[!is.na(lPneumoRankScores)]
              lPneumoRankScores <- lPneumoRankScores[!is.na(lPneumoRankScores)]
              # plot the lower pneumo Rank Scores
              if(length(lPneumoRankScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(lPneumoScoreIndices+0),
                                  y=rep((yOffset['lPneumo']+75), times=length(lPneumoRankScores)),
                                  label=lPneumoRankScores,
                                  color="black",
                                  alpha=.5,
                                  size=2)
              }
              
              ### combined pneumo score Rank Scores ###
              
              # get all row indices for the combined pneumo score
              pneumoSensorRows <- which(chartMeasurementDF$sensorName == "Pneumo")
              # then select the row indices for the combined pneumo Rank Scores in the chart measurements
              pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
              # get the combined pneumo Rank Scores
              pneumoRankScores <- printRankScores[pneumoSelectRows]
              # get the row indices for the combined pneumo Rank Scores in the chartDF
              pneumoScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pneumoSelectRows])
              # calculate the y offset
              yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo']))
              # remove NA values
              pneumoScoreIndices <- pneumoScoreIndices[!is.na(pneumoRankScores)]
              pneumoRankScores <- pneumoRankScores[!is.na(pneumoRankScores)]
              # plot the combined pneumo Rank Scores
              if(length(pneumoRankScores) != 0) {
                g <- g + annotate(geom="text",
                                  x=(pneumoScoreIndices),
                                  y=rep(yPneumo+75, times=length(pneumoRankScores)),
                                  label=pneumoRankScores,
                                  color="black",
                                  alpha=.3,
                                  size=2)
              }
              
            } # end if showPneumoScores == TRUE
            
            ###  EDA Rank Scores ###
            
            if(isTRUE(showEDAData) && showEDAScores==TRUE) {
              
              ### Auto EDA Rank Scores
              
              # get all row indices for the EDA data
              AutoEDASensorRows <- which(chartMeasurementDF$sensorName == "AutoEDA")
              # then select the row indices for the EDA Rank Scores 
              # in the chart measurements DF
              AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
              # get the EDA Rank Scores
              # use printCQName to automatically select rank or integer Rank Scores
              AutoEDARankScores <- printRankScores[AutoEDASelectRows]
              # get the row indices for the EDA Rank Scores in the chartDF
              if(length(AutoEDARankScores != 0)) {
                AutoEDAScoreIndices <- which(chartDF$eventLabel %in% 
                                               chartMeasurementDF$eventLabel[AutoEDASelectRows])
                # plot the EDA Rank Scores
                if(length(AutoEDARankScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(AutoEDAScoreIndices+0),
                                    y=rep((yOffset['eda']+75), times=length(AutoEDARankScores)),
                                    label=AutoEDARankScores,
                                    color="black",
                                    size=2,
                                    na.rm=TRUE)
                }
              }
              
              ### Manual/un-filtered EDA Rank Scores
              
              if(isTRUE(showManualEDA)) {
                
                # get all row indices for the EDA data
                ManualEDASensorRows <- which(chartMeasurementDF$sensorName == "ManualEDA")
                # then select the row indices for the EDA Rank Scores
                # in the chart measurements DF
                ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
                # get the EDA Rank Scores
                # use printCQName to automatically select rank or integer Rank Scores
                ManualEDARankScores <- printRankScores[ManualEDASelectRows]
                # get the row indices for the EDA Rank Scores in the chartDF
                if(length(ManualEDARankScores != 0)) {
                  ManualEDAScoreIndices <- which(chartDF$eventLabel %in%
                                                   chartMeasurementDF$eventLabel[ManualEDASelectRows])
                  # plot the EDA Rank Scores
                  if(length(ManualEDARankScores) != 0) {
                    g <- g + annotate(geom="text",
                                      x=(ManualEDAScoreIndices+300),
                                      y=rep((yOffset['eda']+50), times=length(ManualEDARankScores)),
                                      label=ManualEDARankScores,
                                      color="black",
                                      alpha=.3,
                                      size=2.5,
                                      na.rm=TRUE)
                  } # end if
                } # end if
                
              } # end if showManualEDA
              
            } # end if showEDAScores
            
            ###  Cardio Rank Scores ###
            
            if(isTRUE(showCardioData) && showCardioScores==TRUE) {
              
              # get all row indices for the Cardio data
              cardioSensorRows <- which(chartMeasurementDF$sensorName == "Cardio")
              # then select the row indices for the Cardio Rank Scores in the chart measurements
              cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
              # get the Cardio Rank Scores
              cardioRankScores <- printRankScores[cardioSelectRows]
              # get the row indices for the Cardio Rank Scores in the chartDF
              if(length(cardioRankScores) > 0) {
                cardioScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[cardioSelectRows])
                # plot the Cardio Rank Scores
                if(length(cardioRankScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(cardioScoreIndices + 0),
                                    # need to use rep() for y in order to avoid a warning about names
                                    y=rep((yOffset['cardio'] + 75 + 0), times=length(cardioRankScores)),
                                    label=cardioRankScores,
                                    color="black",
                                    size=2,
                                    na.rm=TRUE)
                }
              }
              
            } # end if showCardioScores
            
            ###  PLE Rank Scores ###
            
            if(isTRUE(showPLEData) && showPLEScores==TRUE) {
              
              # get all row indices for the PLE data
              pleSensorRows <- which(chartMeasurementDF$sensorName == "PLE")
              # then select the row indices for the PLE Rank Scores in the chart measurements
              pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
              # get the PLE Rank Scores
              pleRankScores <- printRankScores[pleSelectRows]
              # get the row indices for the PLE Rank Scores ratios in the chartDF
              if( length(pleRankScores) > 0 & !all(is.na(pleRankScores)) ) {
                pleScoreIndices <- which(chartDF$eventLabel %in% chartMeasurementDF$eventLabel[pleSelectRows])
                # plot the PLE Rank Scores
                if(length(pleRankScores) != 0) {
                  g <- g + annotate(geom="text",
                                    x=(pleScoreIndices + 0),
                                    # need to use rep() for y in order to avoid a warning about names
                                    y=rep((yOffset['ple'] - 25), times=length(pleRankScores)),
                                    label=pleRankScores,
                                    color="black",
                                    size=2,
                                    na.rm=TRUE)
                } # end if
              } # end if
              
            } # end if show PLE Data
            
          } # end if !is.null(measurementDF)
          
        } # end if showRankValues==TRUE
        
      }
      
      ############################ warnings ############################
      
      if(showWarnings==TRUE && (!isTRUE(PCASSFormat)) && !isTRUE(PCATFormat)) {
        
        if(XWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=-950, 
                            label=XWarning,
                            color="red", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        # if(cardioWarning != "none") {
        #   g <- g + annotate(geom="text",
        #                     x=60,
        #                     y=(yOffset[4]+reOffsetCardio+newCardio1Offset),
        #                     label=cardioWarning,
        #                     color="black",
        #                     fontface="bold",
        #                     hjust=0,
        #                     size=3.5)
        # }
        
        if(cardioRateWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yMin+(.22*yRange), 
                            label=cardioRateWarning,
                            color="black", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        if(activityWarning != "none") {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=-850, 
                            label=activityWarning,
                            color="red", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        # if(pneumoWarning != "none") {
        #   g <- g + annotate(geom="text",
        #                     x=60,
        #                     y=median(c((yOffset[1]+newPneumoOffset),
        #                                (yOffset[2]+newPneumoOffset))),
        #                     label=pneumoWarning,
        #                     color="black",
        #                     fontface="bold",
        #                     hjust=0,
        #                     size=3.5)
        # }
        
        if(pneumoRateWarning != "none" && !is.na(pneumoRateWarning)) {
          g <- g + annotate(geom="text", 
                            x=60, 
                            y=yOffset[2] - .1*yRange, 
                            label=pneumoRateWarning,
                            color="black", 
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        if(rbpfWarning != "none") {
          g <- g + annotate(geom="text",
                            x=60,
                            y=yOffset[4] + (.05*yRange),
                            label=rbpfWarning,
                            color="black",
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        # <>  Oct 25, 2020 # cARDIO SENSOR WARNING
        if(cardioLeakMsg != "none") {
          g <- g + annotate(geom="text",
                            x=60,
                            y=yOffset[4] + (.05*yRange) - 60,
                            label=cardioLeakMsg,
                            color="red",
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        if(edaWarning != "none") {
          g <- g + annotate(geom="text",
                            x=60,
                            y=yOffset[3] - (.05*yRange),
                            label=edaWarning,
                            color="black",
                            fontface="bold",
                            hjust=0,
                            size=3.5)
        }
        
        # Oct 15, 2023
        if(questionPaceWarning != "none") {
          g <- g + annotate(geom="text",
                           x=60,
                           y=25,
                           label=questionPaceWarning,
                           color="red",
                           fontface="bold",
                           hjust=0,
                           size=3.5)
        }
        
      } # end if showWarnings==TRUE
   
      ########################## other information ######################
      
      # cardio and eCardio correlation
      if(inclECardio==TRUE) {
        cardioCor <- round(cor(chartDF$c_Cardio1, chartDF$c_eCardio), 4)
        g <- g + annotate(geom="text",
                          x=150,
                          y=yOffset[4] - (.1*yRange),
                          label=paste0("cardio eCardio correlation: r = ", cardioCor),
                          color="black",
                          fontface="plain",
                          hjust=0,
                          size=5)
      }
      
      # # cardio and FC correlation
      # if(inclFC==TRUE) {
      #   FCCor <- round(cardioCorFn(x=chartDF), 3)
      #   g <- g + annotate(geom="text",
      #                     x=3000,
      #                     y=-625, #yOffset[4] - (.1*yRange),
      #                     label=paste0("right and left forearm cuff correlation: r = ", FCCor),
      #                     color="black",
      #                     fontface="plain",
      #                     hjust=0,
      #                     size=5)
      # }
      
      # exam series chart name
      if(inclChartName==TRUE) {
        thisName <- paste(examName, seriesName, chartName, sep="_")
        g <- g + annotate(geom="text",
                          x=500,
                          y=875, #yOffset[4] - (.1*yRange),
                          label=thisName,
                          color="black",
                          fontface="plain",
                          hjust=0,
                          size=5 )
      }
        
        
        
        # cardio tracing names (Feb 2023)
        
        # g <- g + 
        #   annotate(geom="text",
        #            x=4500,
        #            y=325, #yOffset[4] - (.1*yRange),
        #            label="brown = forearm",
        #            color="brown",
        #            fontface="plain",
        #            hjust=0,
        #            size=4.5 ) + 
        #   annotate(geom="text",
        #            x=4500,
        #            y=225, #yOffset[4] - (.1*yRange),
        #            label="red = forearm",
        #            color="red",
        #            fontface="plain",
        #            hjust=0,
        #            size=4.5 )
      
      ######################### plot appearance #########################
      
      {
        
        g <- g + ylab("y-change")
        g <- g + xlab("x-time")
        # g <- g + scale_x_continuous(breaks=seq(0,nrow(chartDF),300))
        g <- g + ggtitle(plotTitle)
        g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,nrow(chartDF),600))
        
      }
      
      # print the chart to the graphics device
      print(g)
      
      if(printPlot==TRUE && separateCharts==TRUE) {
        graphics.off()
        dev.new() 
      }
      
    } # end iteration over k charts
    
  } # end iteration over j series 
  
  if(printPlot == TRUE) { 
    graphics.off()
    dev.new() 
  } 
  
} # end iteration over i exams

if(showNames==TRUE) print(paste(length(uniqueExams), "exams processed"))

# reset the NCCA ASCII init 
# source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)


######### tabulate the RQ States and exam criterion states ############

# list.files(pattern="RQState.csv^")


# 

