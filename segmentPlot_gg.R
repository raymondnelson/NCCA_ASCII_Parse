# plot a stimulus segment recursively
# raymond nelson
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
  
  
  
  # source the script containing the rbpfProbFn()
  source('~/Dropbox/R/NCCA_ASCII_Parse/rbpfProb.R', echo=FALSE)
  
  # source this for the lowPass2hz.2nd filter to calculate cardio rate
  source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc_extra.R', echo=FALSE)
  
}



# source the list of excluded events so that measurements are not plotted for these
# source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)

# source the initialization script to set the plot parameters
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
# scaleVals
# yOffset

# segment plot init does nothing 
# because the parameters are in the NCCAASCII_init.R sciript
# source('~/Dropbox/R/NCCA_ASCII_Parse/segmentPlot_init.R', echo=FALSE)



#### set the parameters for plotting

{
  # to control the print output
  showNames <- TRUE
  output <- FALSE
  outputSegmentFileName <- "_segmentPlot.pdf"
  separateCharts <- FALSE
  printPlot <- TRUE
}



{
  showWarnings <- TRUE
  showStimulusLines <- TRUE
  showShadedAreas <- TRUE
}



{
  showData <- TRUE
  
  showPneumoData <- TRUE
  # showPneumoData <- FALSE
  showEDAData <- TRUE
  # showEDAData <- FALSE
  showManualEDA <- FALSE
  showCardioData <- TRUE
  showFCData <- FALSE
  showPLEData <- TRUE
  showPLEData <- FALSE
  showActivityData <- TRUE
  showActivityData <- FALSE
  
  showPTTData <- TRUE
  # showPTTData <- FALSE
}



{
  showArtifacts <- FALSE
  
  showMeasurements <- TRUE
  
  showEDAComplexity <- FALSE
  showEDADuration <- FALSE
  
  showPLEMeasurement <- TRUE
}



{
  showScores <- FALSE
  
  showPneumoScores <- TRUE
  showEDAScores <- TRUE
  showCardioScores <- TRUE
  showPLEScores <- TRUE
  showFCScores <- FALSE
  
  showExtractionVals <- TRUE
  showRankVals <- TRUE
  
  showRRMVals <- FALSE
  showMiritelloRankVals <- FALSE
  showIpZVals <- FALSE
  
  showESSMScores <- TRUE
  showCQSelection <- TRUE
  showRCRatios <- TRUE
} 



{
  # get exam names from the _Data data frames
  # uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  # uniqueExams <- uniqueExams[11]
}



{
  # # plot a single segment instead of iterating through all exams in the global environment
  getSegment <- FALSE
  # use "ALL" to select all exams series charts and segments
  examNum <- 1
  seriesNum <- 2
  chartNum <- 1
  segmentNum <- 5

  if(getSegment == TRUE) {
    assign("examName", uniqueExams[examNum])
    uniqueExams <- uniqueExams[examNum] 
  } 
}



{
  # to re-initialize the graphics device
  # graphics.off()
  # dev.new()
}



# loop over each exam in the list and plot the stimulus segments
i=1
for(i in 1:length(uniqueExams)) {
  
  examName <- uniqueExams[i]
  
  # get the names of time series data frames for all unique series in each exam
  searchString <- paste0("*", examName, "_Data", "*")
  examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  
  if(showNames==TRUE) print(paste("exam:", examName))
  
  # check to see if any events exist and increment the exam if none
  stimNames <- ls(pattern="_Stimuli$", pos=1)
  if(length(stimNames) == 0) next()
  
  #### INITIALIZE the pdf graphic device ####
  
  if(printPlot == TRUE) {
    if(separateCharts==FALSE) {
      pdf(paste(examName, outputSegmentFileName, sep=""), height=5.5, width=8.5)
    } else {
      pdf(paste(examName, chartName, outputSegmentFileName, sep=""), height=5.5, width=8.5)
    }
  }

  examStartRow <- 1
  examEndRow <- nrow(examDF)
  
  ### add additional columns here
  
  # get the measurement data frame for this exam
  measurementDFName <- paste0(examName, "_Measurements")
  measurementDF <- get(measurementDFName, pos=1)
  # View(measurementDF)
  
  # get the names of all unique series in the exam
  uniqueSeries <- as.character(unique(examDF$seriesName))
  
  if(getSegment == TRUE) {
    if(seriesNum!="ALL") uniqueSeries <- uniqueSeries[seriesNum]
  }
  
  # loop over each unique series
  j=1
  for(j in 1:length(uniqueSeries)) {
    seriesName <- uniqueSeries[j]
    
    # get the list of time series data for the charts in the exam
    seriesDF <- examDF[examDF$seriesName==seriesName,]
    
    if(showNames==TRUE) print(paste("series", seriesName))
    
    seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
    seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
    
    # slice the measurement data from for this series
    seriesMeasurementDF <- measurementDF[measurementDF$seriesName == seriesName,]
    
    # uniqueCharts <- names(seriesDF)
    uniqueCharts <- as.character(unique(seriesDF$chartName))
    
    if(getSegment == TRUE) {
      if(chartNum!="ALL") uniqueCharts <- uniqueCharts[chartNum]
    }
    
    # loop over each chart in the series 
    k=1
    for(k in 1:length(uniqueCharts)) {
      chartName <- uniqueCharts[k]
      
      # get the data frame with the time series data for each chart in the series
      chartDF <- seriesDF[seriesDF$chartName==chartName,]
      
      if(nrow(chartDF)<600) next()
      
      if(showNames==TRUE) print(paste("chart", chartName))
      
      chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
      chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
      
      # slice the measurement data frame for this chart
      chartMeasurementDF <- 
        seriesMeasurementDF[seriesMeasurementDF$chartName == chartName,]
      
      if(printPlot == TRUE && separateCharts==TRUE) {
        pdf(paste(examName, chartName, outputSegmentFileName, sep="_"), height=5.5, width=3.5)
      }
      
      # make a vector of event names
      eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
      
      if(length(eventNames)==0) {
        print("no stimulus events. none processed")
        next()
      } 
      
      # call the getFirstLastEventFn from the sigProcHelper.R script
      firstLastEvents <- getFirstLastEventFn(x=chartDF)
      firstEvent <- firstLastEvents[1]
      lastEventEnd <- firstLastEvents[2]
      
      if(getSegment == TRUE) { 
        if(segmentNum!="ALL") eventNames <- eventNames[segmentNum] 
      }
      
      ######################   set some warnings   ####################
      
      # set a warning for the X/XX announcements 
      XWarning <- ifelse( !("X" %in% eventNames) & !("XX" %in% eventNames),
                          "MISSING X AND XX ANNOUNCEMENTS",
                          ifelse(!("X" %in% eventNames),
                                 "MISSING X ANNOUNCEMENT",
                                 ifelse(!("XX" %in% eventNames),
                                        "MISSING XX ANNOUNCEMENT",
                                        "none") ) )
      
      # set the stimulus warning to "none"
      stimulusWarning <- "none"
      
      ################  check for additional channels  #################
      
      # set a variable to determine if the FC forearm or finger cuff data exist
      inclFC <- ifelse(sum(pmatch(names(chartDF), 
                                  "c_FC", nomatch=0))>0,
                       TRUE,
                       FALSE)
      
      # set a variable to determine if PLE data exist in the current chart
      inclPLE <- ifelse(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))>0,
                        TRUE,
                        FALSE)
      
      # set a variable for activity sensor warning
      activityWarning <- ifelse( !("c_Move1" %in% names(examDF)),
                                 "MISSING ACTIVITY SENSOR DATA1",
                                 "none" )
      
      ############# pulse and respiration rates
      
      # check the cardio pulse rate
      cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=9,peaks="upper")
      if(cardioRate < 60 | cardioRate > 100) {
        cardioRateWarning <- paste("CARDIO RHYTHM", cardioRate, "OUTSIDE NORMAL RANGE")
      } else cardioRateWarning <- "none"
      
      # check the respiration rate
      pneumoRate  <- ratePerMin(chartDF$c_UPneumoSm,buffer=40,peaks="upper")
      if(pneumoRate < 12 | pneumoRate > 20) {
        pneumoRateWarning <- paste("RESPIRATION RATE", pneumoRate, "OUTSIDE NORMAL RANGE")
      } else pneumoRateWarning <- "none"
      
      # check for respiratory blood pressure fluctuation
      rbpfMsg <- rbpfProbFn(x=chartDF)
      rbpfWarning <- if(rbpfMsg != "none") {
        rbpfWarning <- rbpfMsg
      } else {
        rbpfWarning <- rbpfMsg
      }
      
      ############### loop over all the events in the chart data frame #############
        
      l=3
      for (l in 1:length(eventNames)) {

        segmentName <- eventNames[l]
        # get the onset row  for the chart DF so that events during the prestimSegment are ignored
        segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
        # segOnsetRow <- eventRows[l] # does not work when getting a single segment
        # get the segment prestim row
        prestimRow <- segOnsetRow - prestimSeg*cps
        if(prestimRow < 1) prestimRow <- 1
        # set the end row so that the data frame
        segEndRow <- segOnsetRow + measuredSeg*cps - 1
        if(segEndRow >= nrow(chartDF)) segEndRow <- nrow(chartDF)-1
        # set the row number for the end of the stimulus segment
        endRow <- segEndRow + addSeg*cps
        if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
        # get the segment start row
        startRow <- prestimRow
        
        # get the segment data frame
        segmentDF <- chartDF[startRow:endRow,]
        # View(segmentDF)
        
        # skip the segment if it is too short
        if(nrow(segmentDF) < 180) next
        
        # slice the measurement data frame for this segment
        segmentMeasurementDF <- 
          chartMeasurementDF[chartMeasurementDF$eventLabel == segmentName,]
        
        # plotTitle <- paste(examName, seriesName, chartName, segmentName, sep="_")
        plotTitle <- paste(seriesName, chartName, segmentName, sep="_")
        
        # adjust the rows so that row numbers refer to data in the segmentDF not the chartDF
        prestimRow <- prestimRow - startRow + 1 # this will set the prestim row to 1
        segOnsetRow <- segOnsetRow - startRow + 1 # will normally set to 301
        # offsetRow is determined later in the plot
        segEndRow <- segEndRow - startRow + 1 # will normally set to 750
        # answerRow is determined later in the plot
        endRow <- endRow - startRow + 1 # will normally set to 1150
        
        if(showNames==TRUE) print(paste("plotting", segmentName))
        
        ########  scaling and offsetting #######
        
        # scaling and offsetting is performed by a function in the scaleOffsetData.R script
        
        ################ make the plot using the ggplot2 package #################
        
        # ggplot normally executes in the global environment
        
        g <- ggplot()
        
        ############ horizontal and vertical chart divisions ###################
        
        {
          
          chartDivH <- seq(-937.5, 937.5,by=62.5)
          chartDivV <- seq(150, ((nrow(segmentDF)%/%150)*150), by=150)
          # no need for a loop because geom_vline is vectorized
          g <- g + geom_vline(aes(xintercept=as.numeric(chartDivV)), color="grey", linewidth=.3, alpha=.2)
          g <- g + geom_hline(aes(yintercept=as.numeric(chartDivH)), color="grey", linewidth=.3, alpha=.2)
          
          
        }
        
        ##################### tracing baselines ####################
        
        # g <- g + geom_hline(aes(yintercept=yOffset), color="brown", size=.15)
        
        ###################### vertical lines #######################
        
        if(showStimulusLines==TRUE) {
          
          # stimulus onset line
          onsetRow <- segOnsetRow
          g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
          # EDA latency
          EDALatRow <- onsetRow+(EDALat*cps)
          g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="grey80")
          # offset line for the end of the stimulus question
          offsetRow <- which(segmentDF$Events[onsetRow:nrow(segmentDF)]=="offsetRow")[1] + onsetRow - 1
          if(is.na(offsetRow)) {
            offsetRow <- segEndRow - 1
            stimulusWarning <- "excessive stimulus length"
          }
          if(offsetRow >= segEndRow) {
            offsetRow <- segEndRow - 1
            stimulusWarning <- "excessive stimulus length"
          }
          g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
          # answer line
          answerRow <- which(segmentDF$Events[onsetRow:nrow(segmentDF)]=="answerRow")[1] + onsetRow - 1
          if(is.na(answerRow)) { answerRow <- offsetRow + 1 }
          if(answerRow >= endRow) { answerRow <- endRow - 1 }
          g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="black")
          # end of response onset window
          ROWEndRow <- answerRow + (ROWEnd*cps)
          # g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
          # end of scoring window
          segEndRow <- onsetRow+(measuredSeg*cps) - 1
          if(segEndRow > (nrow(segmentDF)-2)) segEndRow <- (nrow(segmentDF)) - 2
          # g <- g + geom_vline(aes(xintercept=as.numeric(segEndRow)), color="grey70")
          
        } # end if showStimulusLines==TRUE
        
        ######################### shaded areas #########################
        
        if(showShadedAreas==TRUE) {
          
          # scoring window shaded area
          g <- g + annotate("rect",
                            xmin=as.numeric(onsetRow),
                            xmax=as.numeric(segEndRow),
                            ymin=yMin,
                            ymax=yMax,
                            alpha=.12,
                            fill="blue")
          # stimulus question shaded area
          g <- g + annotate("rect", 
                            xmin=as.numeric(onsetRow),
                            xmax=as.numeric(offsetRow),
                            ymin=yMin, 
                            ymax=yMax, 
                            alpha=.3, 
                            fill="grey20")
          # latency shaded area
          g <- g + annotate("rect",
                            xmin=as.numeric(onsetRow),
                            xmax=as.numeric(onsetRow+15),
                            ymin=yMin,
                            ymax=yMax,
                            alpha=.75,
                            fill="yellow")
          # response onset window shaded area
          g <- g + annotate("rect",
                            xmin=as.numeric(offsetRow),
                            xmax=as.numeric(ROWEndRow),
                            ymin=yMin,
                            ymax=yMax,
                            alpha=.125,
                            fill="blue")
          
        } # end if showShadedAreas==TRUE
        
        ############ warnings ###########################
        
        if(showWarnings==TRUE && !isTRUE(PCASSFormat)) {
          
          # if(cardioWarning != "none") {
          #   g <- g + annotate(geom="text", 
          #                     x=60, 
          #                     y=yOffset['cardio'], 
          #                     label=cardioWarning,
          #                     color="red", 
          #                     fontface="bold",
          #                     hjust=0,
          #                     size=3)
          # }
          
          if(activityWarning != "none") {
            g <- g + annotate(geom="text", 
                              x=60, 
                              y=yOffset['activity'], 
                              label=activityWarning,
                              color="red", 
                              fontface="bold",
                              hjust=0,
                              size=3)
          }
          
        } # end if showWarnings==TRUE
        
        #################### TIME SERIES DATA ###################
        
        if(showData==TRUE) {
          
          if(isTRUE(showPneumoData)) {
            # upper pneumo
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoSm), color="blue1", linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoInh), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoExh), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_UPneumoMid), color="blue1", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # lower pneumo
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoSm), color="blue4", linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoInh), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoExh), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_LPneumoMid), color="blue4", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          }
          
          if(isTRUE(showCardioData)) {
            # cardio
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Cardio1), color="red", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioDiastolic), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioSystolic), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioMid), color="brown", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_CardioMA), color="black", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            
          }
          
          if(inclFC==TRUE && isTRUE(showCardioData)) { 
            # forearm or finger cuff data
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_FC), color="red", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_FCDiastolic), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_VCSystolic), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_FCMA), color="red", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_FCMid), color="black", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
          }
          
          if(isTRUE(showEDAData)) {
            # filtered eda
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDA), color="green4", linewidth=.8) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDAPeak), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDABase), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDAMid), color="green4", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            
            if(isTRUE(showManualEDA)) {
              # un-filtered eda
              g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_ManualEDA), color="green3", alpha=.3, linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDAPeak), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDABase), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_AutoEDAMid), color="green4", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            }
          }
          
          if(isTRUE(showPLEData)) {
            # photoplethysmograph
            if(inclPLE==TRUE) { 
              g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PPG1Max), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PPG1Min), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PPG1MA), color="brown", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PPG1), color="brown", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
            } # end if for PLE
          }
          
          if(isTRUE(showActivityData)) {
            # seat activity sensor
            if(activityWarning=="none") {
              g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1Proc), color="tan4", linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1Min), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1Max), color="grey60", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1MA), color="black", linewidth=.15) # + coord_cartesian(ylim=c(yMin, yMax))
              # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1), color="grey35", linewidth=.4) # + coord_cartesian(ylim=c(yMin, yMax))
              g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1ProcMA), color="black", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
            }
          }
          
          if(isTRUE(showPTTData) && "c_PTTPTT" %in% colnames(segmentDF)) {
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PTTPTT), color="blue2", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PTTPPG), color="blue3", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PTTECG), color="blue4", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
            # PTT slow moving average
            g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PTTPTT_MA), color="black", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
            # 
            # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_PTTPTT_abs), color="black", linewidth=.25) # + coord_cartesian(ylim=c(yMin, yMax))
            
          }
          
          # # un-used sensor columns
          # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Move1), color="grey20")
          # g <- g + geom_line(data=segmentDF, aes(x=(1:nrow(segmentDF)), y=c_Aux02), color="grey20")
          
        } # end if showData==TRUE
        
        ######################## artifacts #######################
        
        if(showArtifacts==TRUE) {
          
          if(isTRUE(showPneumoData) && !isTRUE(PCASSFormat)) {
            
            ### Penumo artifacts
            
            # g <- g + annotate("point", x=which(segmentDF$UPneumo_a=="Artifact"),
            #                   y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumo_a=="Artifact")],
            #                   shape=4, linewidth=3)
            # g <- g + annotate("point", x=which(segmentDF$UPneumoInh_a=="Artifact"),
            #                   y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumoInh_a=="Artifact")],
            #                   shape=4, linewidth=3)
            # g <- g + annotate("point", x=which(segmentDF$UPneumoExh_a=="Artifact"),
            #                   y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumoExh_a=="Artifact")],
            #                   shape=4, linewidth=3)
            # g <- g + annotate("point", x=which(segmentDF$UPneumoMid_a=="Artifact"),
            #                   y=segmentDF$c_UPneumoSm[which(segmentDF$UPneumoMid_a=="Artifact")],
            #                   shape=4, linewidth=3)
            
            # g <- g + annotate("point", x=which(segmentDF$LPneumo_a=="Artifact"),
            #                   y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumo_a=="Artifact")],
            #                   shape=4, linewidth=3)
            # g <- g + annotate("point", x=which(segmentDF$LPneumoInh_a=="Artifact"),
            #                   y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumoInh_a=="Artifact")],
            #                   shape=4, linewidth=3)
            # g <- g + annotate("point", x=which(segmentDF$LPneumoExh_a=="Artifact"),
            #                   y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumoExh_a=="Artifact")],
            #                   shape=4, linewidth=3)
            # g <- g + annotate("point", x=which(segmentDF$LPneumoMid_a=="Artifact"),
            #                   y=segmentDF$c_LPneumoSm[which(segmentDF$LPneumoMid_a=="Artifact")],
            #                   shape=4, linewidth=3)
            
            #### buffer around the pneumo artifacts ####
            
            halfBuffer <- round(1.5*cps,0)
            
            UPneumoArtifacts <- which(segmentDF$UPneumo_a=="Artifact")
            UPArtifactBuffXOn <- UPneumoArtifacts[which(UPneumoArtifacts>halfBuffer & UPneumoArtifacts<nrow(segmentDF)-halfBuffer)] - halfBuffer
            
            UPArtifactBuffXOff <- UPneumoArtifacts[which(UPneumoArtifacts>halfBuffer & UPneumoArtifacts<nrow(segmentDF)-halfBuffer)] + halfBuffer
            UPArtifactBuffYOn <- segmentDF$c_UPneumoSm[UPArtifactBuffXOn]
            UPArtifactBuffYOff <- segmentDF$c_UPneumoSm[UPArtifactBuffXOff]
            # g <- g + annotate("segment",
            #                   x=UPArtifactBuffXOn,
            #                   xend=UPArtifactBuffXOff,
            #                   y=UPArtifactBuffYOn,
            #                   yend=UPArtifactBuffYOff,
            #                   color="black",
            #                   alpha=.75,
            #                   linetype="solid",
            #                   size=.45)
            
            LPneumoArtifacts <- which(segmentDF$LPneumo_a=="Artifact")
            LPArtifactBuffXOn <- LPneumoArtifacts[which(LPneumoArtifacts>halfBuffer & LPneumoArtifacts<nrow(segmentDF)-halfBuffer)] - halfBuffer
            
            LPArtifactBuffXOff <- LPneumoArtifacts[which(LPneumoArtifacts>halfBuffer & LPneumoArtifacts<nrow(segmentDF)-halfBuffer)] + halfBuffer
            LPArtifactBuffYOn <- segmentDF$c_LPneumoSm[LPArtifactBuffXOn]
            LPArtifactBuffYOff <- segmentDF$c_LPneumoSm[LPArtifactBuffXOff]
            # g <- g + annotate("segment",
            #                   x=LPArtifactBuffXOn,
            #                   xend=LPArtifactBuffXOff,
            #                   y=LPArtifactBuffYOn,
            #                   yend=LPArtifactBuffYOff,
            #                   color="black",
            #                   alpha=.75,
            #                   linetype="solid",
            #                   size=.45)
            
          }
          
          ### EDA artifacts
          
          if(isTRUE(showEDAData)) {
            
            # g <- g + annotate("point", x=which(segmentDF$c_AutoEDA_a=="Artifact"),
            #                   y=segmentDF$c_AutoEDA[which(segmentDF$c_AutoEDA_a=="Artifact")],
            #                   shape=4, size=1)
            
            
            # g <- g + annotate("point", x=which(segmentDF$EDAFilt_a=="artifact11"),
            #                   y=segmentDF$c_AutoEDA[which(segmentDF$EDAFilt_a=="artifact11")],
            #                   shape=20, size=3, alpha=.2, color="red")
            # 
            # g <- g + annotate("point", x=which(segmentDF$EDAFilt_a=="artifact1b"),
            #                   y=segmentDF$c_AutoEDA[which(segmentDF$EDAFilt_a=="artifact1b")],
            #                   shape=20, size=3, alpha=.2, color="orange")
            # 
            # g <- g + annotate("point", x=which(segmentDF$EDAFilt_a=="artifact1c"),
            #                   y=segmentDF$c_AutoEDA[which(segmentDF$EDAFilt_a=="artifact1c")],
            #                   shape=20, size=3, alpha=.2, color="pink")
            # 
            # g <- g + annotate("point", x=which(segmentDF$EDAFilt_a=="artifact2"),
            #                   y=segmentDF$c_AutoEDA[which(segmentDF$EDAFilt_a=="artifact2")],
            #                   shape=18, size=3, alpha=.4, color="brown")
            # 
            # g <- g + annotate("point", x=which(segmentDF$EDAFilt_a=="artifact3"),
            #                   y=segmentDF$c_AutoEDA[which(segmentDF$EDAFilt_a=="artifact3")],
            #                   shape=18, size=3, alpha=.2, color="black")
            
            
            
            # point
            g <- g + annotate("point", x=which(segmentDF$AutoEDA_a=="artifact1a"),
                              y=segmentDF$c_AutoEDA[which(segmentDF$AutoEDA_a=="artifact1a")],
                              shape=20, size=3, alpha=.15, color="red")
            
            g <- g + annotate("point", x=which(segmentDF$AutoEDA_a=="artifact1b"),
                              y=segmentDF$c_AutoEDA[which(segmentDF$AutoEDA_a=="artifact1b")],
                              shape=20, size=3, alpha=.1, color="orange")
            
            g <- g + annotate("point", x=which(segmentDF$AutoEDA_a=="artifact1c"),
                              y=segmentDF$c_AutoEDA[which(segmentDF$AutoEDA_a=="artifact1c")],
                              shape=20, size=3, alpha=.1, color="hotpink")
            
            g <- g + annotate("point", x=which(segmentDF$AutoEDA_a=="artifact2"),
                              y=segmentDF$c_AutoEDA[which(segmentDF$AutoEDA_a=="artifact2")],
                              shape=18, size=3, alpha=.75, color="brown")
            
          }
          # View(segmentDF)
          
          
          
          ### cardio artifacts
          
          if(isTRUE(showCardioData)) {
            
            g <- g + annotate("point", x=which(segmentDF$Cardio1_a=="Artifact"),
                              y=segmentDF$c_CardioMid[which(segmentDF$Cardio1_a=="Artifact")],
                              shape=4, size=4, color="brown")
            g <- g + annotate("point", x=which(segmentDF$CardioSystolic_a=="ArtifactMaxAmp"),
                              y=segmentDF$c_CardioSystolic[which(segmentDF$CardioSystolic_a=="ArtifactMaxAmp")],
                              shape=4, size=4, color="black") # was green
            g <- g + annotate("point", x=which(segmentDF$CardioDiastolic_a=="Artifact"),
                              y=segmentDF$c_CardioDiastolic[which(segmentDF$CardioDiastolic_a=="Artifact")],
                              shape=4, size=4, color="blue")
            g <- g + annotate("point", x=which(segmentDF$CardioMA_a=="ArtifactMA"),
                              y=segmentDF$c_Cardio1[which(segmentDF$CardioMA_a=="ArtifactMA")],
                              shape=4, size=4, color="black")
            g <- g + annotate("point", x=which(segmentDF$CardioMid_a=="ArtifactMinMaxAmp"),
                              y=segmentDF$c_CardioMid[which(segmentDF$CardioMid_a=="ArtifactMinMaxAmp")],
                              shape=4, size=4, color="slateblue1")
            
          }
          
          ### PLE artifacts
          
          if(isTRUE(showPLEData)) {
            # no PLE artifacts yet
          }
          
          ### seat activitiy sensor artifacts
          
          if(isTRUE(showActivityData)) {
            
            if(activityWarning=="none") {
              
              g <- g + annotate("point", x=which(segmentDF$Move1MA_a=="Artifact"),
                                y=segmentDF$c_Move1[which(segmentDF$Move1MA_a=="Artifact")],
                                shape=4, size=3, color="red") 
              #         g <- g + annotate("point", x=which(segmentDF$Move1Max_a=="Artifact"),
              #                           y=segmentDF$c_Move1[which(segmentDF$Move1Max_a=="Artifact")],
              #                           shape=4, size=3, color="red") 
              #         g <- g + annotate("point", x=which(segmentDF$Move1Min_a=="Artifact"),
              #                           y=segmentDF$c_Move1[which(segmentDF$Move1Min_a=="Artifact")],
              #                           shape=4, size=3, color="red") 
              #         g <- g + annotate("point", x=which(segmentDF$Move1_a=="Artifact"),
              #                           y=segmentDF$c_Move1[which(segmentDF$Move1_a=="Artifact")],
              #                           shape=4, size=3, color="red") 
              
            } # end if for activity warning
            
          }
          
        } # end if showArtifacts==TRUE
        
        #########################   measurement lines    ##################
        
        # source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
        
        # # not needed when using a source file to name the excluded events
        # excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB", "OSN", "ISN")
        # excludeEvents <- c(excludeEvents, "I1", "INT", "Int", "INT1", "Int1", "N1", "N2", "N3", "N4", "N7", "S", "SR", "Sy", "Sa", "SA", "SAC", "O")
        # excludeEvents <- c(excludeEvents, "I1", "I2", "I3", "I4", "I7", "SY3", "SY8", "Sy3", "Sy8", "S3", "S8", "SR2", "S2")
        
        if(showMeasurements==TRUE) {
          
          # giant if statement to plot measurements only for the measured segments
          if(!(segmentName %in% excludeEvents)) {
            
            #### Pneumo measurement lines ####
            
            if(isTRUE(showPneumoData) && !isTRUE(PCASSFormat)) {
              
              # use an if to make sure there is a response onset
              if(!is.na(which(segmentDF$UPneumoExtract=="responseOnsetRow")[1])) {
                
                # g <- g + geom_line(data=segmentDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_UPneumoSm), color="blue1", size=.8)
                # g <- g + geom_line(data=segmentDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_LPneumoSm), color="blue3", size=.8)
                
                # commented out 4/5/2016 not needed when artifact buffers are used    
                # #### buffer around the verbal response is not included in the measurement
                
                #### upper pneumo answer buffer ####
                
                # aBuffXOnU <- which(segmentDF$UPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOn")[1] + onsetRow - 1
                # aBuffXOffU <- which(segmentDF$UPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOff")[1] + onsetRow - 1
                
                aBuffXOn <- answerRow - pneumoAnsBuff*cps
                aBuffXOff <- answerRow + pneumoAnsBuff*cps
                
                # which(segmentDF$Events[onsetRow:nrow(segmentDF)]=="answerRow") + onsetRow - 1
                
                aBuffYOnU <- segmentDF$c_UPneumoSm[aBuffXOn][1]
                aBuffYOffU <- segmentDF$c_UPneumoSm[aBuffXOff][1]
                g <- g + annotate("segment",
                                  x=aBuffXOn,
                                  xend=aBuffXOff,
                                  y=aBuffYOnU,
                                  yend=aBuffYOffU,
                                  color="black",
                                  alpha=.75,
                                  linetype="solid",
                                  size=.8)
                
                #### lower pneumo answer buffer ####
                
                # aBuffXOnL <- which(segmentDF$LPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOn")[1] + onsetRow - 1
                # aBuffXOffL <- which(segmentDF$LPneumoExtract[onsetRow:nrow(segmentDF)]=="aBuffOff")[1] + onsetRow - 1
                
                # aBuffXOnL <- answerRow - 1*cps
                # aBuffXOffL <- answerRow + 1*cps
                
                # aBuffXOn and aBuffXOff were set for the upper pneumo
                
                aBuffYOnL <- segmentDF$c_LPneumoSm[aBuffXOn][1]
                aBuffYOffL <- segmentDF$c_LPneumoSm[aBuffXOff][1]
                g <- g + annotate("segment",
                                  x=aBuffXOn,
                                  xend=aBuffXOff,
                                  y=aBuffYOnL,
                                  yend=aBuffYOffL,
                                  color="black",
                                  alpha=.75,
                                  linetype="solid",
                                  size=.8)
                
              } # end if !is.na for pneumo response onset
              
            } # end if showPneumoData
            
            #### Auto EDA measurement lines ####
            
            if(isTRUE(showEDAData)) {
              
              if(!is.na(which(segmentDF$AutoEDAExtract[segOnsetRow:ROWEndRow]=="responseOnsetRow")[1])) {
                
                # determine the response onset index
                autoEDAxOn <- segOnsetRow - 1 + which(segmentDF$AutoEDAExtract[segOnsetRow:nrow(segmentDF)]=="responseOnsetRow")[1]
                if(!is.na(autoEDAxOn)) { if(autoEDAxOn > ROWEndRow) autoEDAxOn <- NA }
                
                if(is.na(autoEDAxOn)) { autoEDAyOn <- NA; autoEDAxOff <- NA; autoEDAyOff<- NA }
                
                # determine the response end row
                if(!is.na(autoEDAxOn)) autoEDAxOff <- autoEDAxOn - 1 + which(segmentDF$AutoEDAExtract[autoEDAxOn:nrow(segmentDF)]=="responseEndRow")[1]
                
                # do this only if there is a response onset and response peak
                if(!is.na(autoEDAxOn) && !is.na(autoEDAxOff)) {
                  
                  # determine the y values for response onset and response end
                  autoEDAyOn <- segmentDF$c_AutoEDA[autoEDAxOn]
                  autoEDAyOff <- segmentDF$c_AutoEDA[autoEDAxOff]
                  
                  # horzontal EDA segment measurement
                  g <- g + annotate("segment",
                                    x=autoEDAxOff,
                                    xend=autoEDAxOn,
                                    y=autoEDAyOn,
                                    yend=autoEDAyOn,
                                    color="purple",
                                    # color="blue",
                                    size=.5,
                                    arrow=arrow(length=unit(0.2, "cm")))
                  # vertical EDA segment
                  # geom_segment and annotate are two different ways of adding the lines
                  # annotate is probably better
                  g <- g + annotate("segment",
                                    x=autoEDAxOff,
                                    xend=autoEDAxOff,
                                    y=autoEDAyOn,
                                    yend=autoEDAyOff,
                                    color="purple",
                                    # color="blue",
                                    size=.5,
                                    arrow=arrow(length=unit(0.2, "cm")))
                }
                
                ## auto EDA response duration
                
                autoRecoveryRow <- segOnsetRow - 1 + which(segmentDF$AutoEDAExtract[segOnsetRow:nrow(segmentDF)]=="recoveryRow")[1]
                autoRecoveryY <- segmentDF$c_AutoEDA[autoRecoveryRow][1]
                
                if(isTRUE(showEDADuration)) {
                  
                  # horizontal segment
                  g <- g + annotate("segment",
                                    x=autoEDAxOff,
                                    xend=autoRecoveryRow,
                                    y=autoEDAyOn,
                                    yend=autoEDAyOn,
                                    # color="purple",
                                    color="blue",
                                    size=1,
                                    alpha=.3,
                                    arrow=arrow(length=unit(0.2, "cm")))
                  
                  # vertical segment
                  g <- g + annotate("segment",
                                    x=autoRecoveryRow,
                                    xend=autoRecoveryRow,
                                    y=autoRecoveryY,
                                    yend=autoEDAyOn,
                                    # color="purple",
                                    color="black",
                                    # alpha=.3,
                                    size=.4)
                  
                } # end if for show auto EDA duration
                
                ## auto EDA response complexity
                
                if(isTRUE(showEDAComplexity)) {
                  
                  # get all complexity rows 
                  autoComplexityRows <- segOnsetRow - 1 + which(segmentDF$AutoEDAExtract[segOnsetRow:nrow(segmentDF)]=="complexityRow")
                  autoComplexityRowsY <- segmentDF$c_AutoEDA[autoComplexityRows]
                  
                  # vertical segment
                  g <- g + annotate("segment",
                                    x=autoComplexityRows,
                                    xend=autoComplexityRows,
                                    y=autoComplexityRowsY,
                                    yend=rep(autoEDAyOn, times=length(autoComplexityRows)),
                                    # color="purple",
                                    color="blue",
                                    size=.8,
                                    alpha=.3)
                  
                } # end if for show auto EDA complexity
                
              } # end if !is.na for Auto EDA response onset
              
              #### Manual EDA measurement lines ####
              
              if(isTRUE(showManualEDA)) {
                
                if(!is.na(which(segmentDF$ManualEDAExtract[segOnsetRow:ROWEndRow]=="responseOnsetRow")[1])) {
                  
                  # determine the response onset index 
                  manualEDAxOn <- segOnsetRow - 1 + which(segmentDF$ManualEDAExtract[segOnsetRow:nrow(segmentDF)]=="responseOnsetRow")[1]
                  if(!is.na(manualEDAxOn)) { if(manualEDAxOn > ROWEndRow) manualEDAxOn <- NA }
                  
                  if(is.na(manualEDAxOn)) { manualEDAyOn <- NA; manualEDAxOff <- NA; manualEDAyOff<- NA }
                  
                  # determine the response end index
                  if(!is.na(manualEDAxOn)) manualEDAxOff <- manualEDAxOn - 1 + which(segmentDF$ManualEDAExtract[manualEDAxOn:nrow(segmentDF)]=="responseEndRow")[1]
                  
                  # do this only if there is a response onset and response peak
                  if(!is.na(manualEDAxOn) && !is.na(manualEDAxOff)) {
                    
                    # determine the y values for response onset and response end
                    manualEDAyOn <- segmentDF$c_ManualEDA[manualEDAxOn]
                    manualEDAyOff <- segmentDF$c_ManualEDA[manualEDAxOff]
                    
                    # horzontal EDA segment measurement
                    g <- g + annotate("segment",
                                      x=manualEDAxOff,
                                      xend=manualEDAxOn,
                                      y=manualEDAyOn,
                                      yend=manualEDAyOn,
                                      color="purple",
                                      # color="blue",
                                      alpha=.2,
                                      size=.5,
                                      arrow=arrow(length=unit(0.2, "cm")))
                    # vertical EDA segment
                    # geom_segment and annotate are two different ways of adding the lines
                    # annotate is probably better
                    g <- g + annotate("segment",
                                      x=manualEDAxOff,
                                      xend=manualEDAxOff,
                                      y=manualEDAyOn,
                                      yend=manualEDAyOff,
                                      color="purple",
                                      # color="blue",
                                      alpha=.2,
                                      size=.5,
                                      arrow=arrow(length=unit(0.2, "cm")))
                    
                  }
                  
                  ## manual EDA response duration
                  
                  manualRecoveryRow <- segOnsetRow - 1 + which(segmentDF$ManualEDAExtract[segOnsetRow:nrow(segmentDF)]=="recoveryRow")[1]
                  manualRecoveryY <- segmentDF$c_ManualEDA[manualRecoveryRow][1]
                  
                  if(isTRUE(showEDADuration)) {
                    
                    # horizontal segment
                    g <- g + annotate("segment",
                                      x=manualEDAxOff,
                                      xend=manualRecoveryRow,
                                      y=manualEDAyOn,
                                      yend=manualEDAyOn,
                                      color="purple",
                                      # color="blue",
                                      size=1,
                                      alpha=.3,
                                      arrow=arrow(length=unit(0.2, "cm")))
                    
                    # vertical segment
                    g <- g + annotate("segment",
                                      x=manualRecoveryRow,
                                      xend=manualRecoveryRow,
                                      y=manualRecoveryY,
                                      yend=manualEDAyOn,
                                      # color="purple",
                                      color="black",
                                      # alpha=.3,
                                      size=.4)
                    
                  } # end if for show manual EDA duration
                  
                  ## manual EDA response complexity
                  
                  # get all complexity rows 
                  manualComplexityRows <- segOnsetRow - 1 + which(segmentDF$ManualEDAExtract[segOnsetRow:nrow(segmentDF)]=="complexityRow")
                  manualComplexityRowsY <- segmentDF$c_ManualEDA[manualComplexityRows]
                  
                  if(isTRUE(showEDAComplexity)) {
                    
                    # vertical segment
                    g <- g + annotate("segment",
                                      x=manualComplexityRows,
                                      xend=manualComplexityRows,
                                      y=manualComplexityRowsY,
                                      yend=rep(manualEDAyOn, times=length(manualComplexityRows)),
                                      # color="purple",
                                      color="blue",
                                      size=.8,
                                      alpha=.3)
                    
                  } # end if for show manual EDA complexity
                  
                } # end if !is.na for Manual EDA response onset
                
              } # end if showManualEDA
              
            } # end if for show EDA data
            
            #### Cardio measurement Lines ####
            
            if(isTRUE(showCardioData) && !isTRUE(PCASSFormat)) {
              
              if(!is.na(which(segmentDF$CardioExtract[onsetRow:ROWEndRow]=="responseOnsetRow")[1])) {
                
                # cardio X onsent annd offset
                cardioXOn <- segOnsetRow - 1 + which(segmentDF$CardioExtract[segOnsetRow:nrow(segmentDF)]=="responseOnsetRow")[1]
                if(is.na(cardioXOn)) { cardioYOn <- NA; cardioXOn <- NA; cardioYOff<- NA }
                
                if(!is.na(cardioXOn)) { 
                  if(cardioXOn > ROWEndRow) cardioXOn <- NA 
                  cardioXOff <- cardioXOn - 1 + which(segmentDF$CardioExtract[cardioXOn:nrow(segmentDF)]=="responseEndRow")[1]
                }
                
                if(!is.na(cardioXOn) && !is.na(cardioXOff)) {
                  
                  # choose the cardio line to measure
                  useCardio <- switch(cardioLine,
                                      "ma" = segmentDF$c_CardioMA,
                                      "diastolic" = segmentDF$c_CardioDiastolic,
                                      "systolic" = segmentDF$c_CardioSystolic,
                                      "mid" = segmentDF$c_CardioMid,
                                      "otherwise: last")
                  # cardio Y onset and offset
                  
                  if(!is.na(cardioXOn)) {
                    cardioYOn <- useCardio[cardioXOn]
                    cardioYOff <- useCardio[cardioXOff]
                  }
                  
                  # add the cardio extraction to the plot
                  
                  # horzontal Cardio segment measurement
                  g <- g + annotate("segment",
                                    x=cardioXOff,
                                    xend=cardioXOn,
                                    y=cardioYOn,
                                    yend=cardioYOn,
                                    color="purple",
                                    size=.5,
                                    arrow=arrow(length=unit(0.2, "cm")))
                  # vertical Cardio segment
                  # geom_segment and annotate are two different ways of adding the lines
                  # annotate is probably better
                  g <- g + annotate("segment",
                                    x=cardioXOff,
                                    xend=cardioXOff,
                                    y=cardioYOn,
                                    yend=cardioYOff,
                                    color="purple",
                                    size=.5,
                                    arrow=arrow(length=unit(0.2, "cm")))
                  
                } # end cardio segment measurements
                
              } # end if !is.na for cardio response onset
              
            } #end if showCardioData
            
            #### forearm cuff or finger cuff measurement Lines ####
            
            if(isTRUE(showFCData)) {
              
              if(!is.na(which(segmentDF$FCExtract[onsetRow:ROWEndRow]=="responseOnsetRow")[1])) {
                
                # FC X onsent annd offset
                FCXOn <- segOnsetRow - 1 + which(segmentDF$FCExtract[segOnsetRow:nrow(segmentDF)]=="responseOnsetRow")[1]
                if(is.na(FCXOn)) { FCYOn <- NA; FCXOn <- NA; FCYOff<- NA }
                
                if(!is.na(FCXOn)) { 
                  if(FCXOn > ROWEndRow) FCXOn <- NA 
                  FCXOff <- FCXOn - 1 + which(segmentDF$FCExtract[FCXOn:nrow(segmentDF)]=="responseEndRow")[1]
                }
                
                if(!is.na(FCXOn) && !is.na(FCXOff)) {
                  
                  # choose the FC line to measure
                  useFC <- switch(cardioLine,
                                  "ma" = segmentDF$c_FCMA,
                                  "diastolic" = segmentDF$c_FCDiastolic,
                                  "systolic" = segmentDF$c_FCSystolic,
                                  "mid" = segmentDF$c_FCMid,
                                  "otherwise: last")
                  # FC Y onset and offset
                  
                  if(!is.na(FCXOn)) {
                    FCYOn <- useFC[FCXOn]
                    FCYOff <- useFC[FCXOff]
                  }
                  
                  # add the FC extraction to the plot
                  
                  # horzontal FC segment measurement
                  g <- g + annotate("segment",
                                    x=FCXOff,
                                    xend=FCXOn,
                                    y=FCYOn,
                                    yend=FCYOn,
                                    color="purple",
                                    size=.5,
                                    arrow=arrow(length=unit(0.2, "cm")))
                  # vertical FC segment
                  # geom_segment and annotate are two different ways of adding the lines
                  # annotate is probably better
                  g <- g + annotate("segment",
                                    x=FCXOff,
                                    xend=FCXOff,
                                    y=FCYOn,
                                    yend=FCYOff,
                                    color="purple",
                                    size=.5,
                                    arrow=arrow(length=unit(0.2, "cm")))
                  
                } # end FC segment measurements
                
              } # end if !is.na for FC response onset
              
            } #end if showFCData
            
            #### PLE measurment indicators ####
            
            if(all(isTRUE(showPLEData), isTRUE(showPLEMeasurement), inclPLE==TRUE)) {
              
              # if(inclPLE==TRUE) { 
              
              if(!is.na(which(segmentDF$PPG1Extract=="prestimSegOnset")[1])) {
                ### prestim measurements
                # this will locate the box in the middle of the x scale for the segment
                preOnX <- which(segmentDF$PPG1Extract=="prestimSegOnset")[1]
                # preOffX <- which(segmentDF$PLEExtract=="prestimSegOffset")[1]
                preOffX <- preOnX + 89
                preOnY <- mean(segmentDF$c_PPG1Max[preOnX:preOffX])
                preOffY <- mean(segmentDF$c_PPG1Min[preOnX:preOffX])
                # # different method using the PLEMeans
                # # does not locate the box in the middle of the x scale for the segment
                # preOnY <- segmentDF$c_PPG1MA[(preOnX+45)] + ((as.numeric(segmentDF$PPG1Means[preOnX]) * PLScale) / 2)
                # preOffY <- segmentDF$c_PPG1MA[(preOnX+45)] - ((as.numeric(segmentDF$PPG1Means[preOffX]) * PLScale) / 2)
                ### poststim measurements
                # this will locate the box in the middle of the x scale for the segment
                postOnX <- which(segmentDF$PPG1Extract=="poststimSegOnset")[1]
                # postOffX <- which(segmentDF$PPG1Extract=="poststimSegOffset")[1]
                # to prevent a warning in case the chart ends abruptly
                if(all(!is.na(preOnX), !is.na(preOffX), !is.na(postOnX))) {
                  postOffX <- postOnX + 149
                  if(postOffX > nrow(segmentDF)) postOffX <- nrow(segmentDF)
                  postOnY <- mean(segmentDF$c_PPG1Max[postOnX:postOffX])
                  postOffY <- mean(segmentDF$c_PPG1Min[postOnX:postOffX])
                  # # different method using the PLEMeans
                  # # does not locate the box in the middle of the x scale for the segment
                  # postOnY <- segmentDF$c_PPG1MA[(postOnX+75)] + ((as.numeric(segmentDF$PLEMeans[postOnX]) * PLScale) / 2)
                  # postOffY <- segmentDF$c_PPG1MA[(postOnX+75)] - ((as.numeric(segmentDF$PLEMeans[postOffX])* PLScale) / 2)
                  ### prestim shaded area
                  g <- g + annotate("rect", 
                                    xmin=as.numeric(preOnX), 
                                    xmax=as.numeric(preOffX), 
                                    ymin=preOnY, 
                                    ymax=preOffY, 
                                    color="black",
                                    size=.25,
                                    alpha=.25, 
                                    fill="blue")
                  #### post stimulus shaded area
                  g <- g + annotate("rect", 
                                    xmin=as.numeric(postOnX), 
                                    xmax=as.numeric(postOffX), 
                                    ymin=postOnY, 
                                    ymax=postOffY, 
                                    color="black",
                                    size=.25,
                                    alpha=.25, 
                                    fill="blue")
                }
              } # end if !is.na
              
              # } # end if inclPLE
              
            } # end if showPLEData
            
          } # end giant if to plot measurements for measured segments
          
        } # end if showMeasurements
        
        
        
        
        
        
        ###################  print scores  ####################
        
        ####  make a private function to remove leading zeroes from numeric scores
        
        numFormatFn <- function(x) {
          # function to remove leading zeros
          # vectorized already and needs no loop
          outVector <- x
          useIdx <- which(!(is.na(x) | x == ""))
          x2 <- sub("^(-?)0.", "\\1.", as.numeric(sprintf("%.2f", as.numeric(x[useIdx]))) )
          outVector[useIdx] <- x2
          return(outVector)
        }
        
        ####
        
        ######## feature extraction values ########
        
        if(showScores==TRUE && showExtractionVals==TRUE) {
          
          # obtain the values from the segmentMeasurementDF
          
          # first get all row indices for the score of interest 
          # use the rank scores for this 
          measurementRows <- which(segmentMeasurementDF$rankScore != "")
          
          # make a vector of scores for printing
          selectMeasurements <- segmentMeasurementDF$sensorMeasurement
          
          # use a private function to remove leading zeros
          printMeasurements <- numFormatFn(selectMeasurements)
          
          ###
          
          if(isTRUE(showPneumoData) && showPneumoScores==TRUE) {
            
            ###  upper pneumo  ### 
            
            # get all row indices for the upper pneumo data
            uPneumoSensorRows <- which(segmentMeasurementDF$sensorName == "UPneumo")
            # then select the row indices for the upper pneumo Measurements in the segment measurements
            uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
            # get the upper pneumo measurements
            uPneumoMeasurements <- printMeasurements[uPneumoSelectRows]
            # get the row indices for the upper pneumo Measurements in the segmentDF
            uPneumoMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[uPneumoSelectRows])
            # remove NA values
            uPneumoMeasurementIndices <- uPneumoMeasurementIndices[!is.na(uPneumoMeasurements)]
            uPneumoMeasurements <- uPneumoMeasurements[!is.na(uPneumoMeasurements)]
            # plot the upper pneumo Measurements
            if(length(uPneumoMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=650,
                                y=rep((yOffset['uPneumo'] - 75), times=length(uPneumoMeasurements)),
                                label=uPneumoMeasurements,
                                color="black",
                                # alpha=.3,
                                size=2.5)
            }
            
            ### lower pneumo  ###
            
            # get all row indices for the lower pneumo data
            lPneumoSensorRows <- which(segmentMeasurementDF$sensorName == "LPneumo")
            # then select the row indices for the lower pneumo Measurements in the segment measurements
            lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
            # get the lower pneumo Measurements
            lPneumoMeasurements <- printMeasurements[lPneumoSelectRows]
            # get the row indices for the lower pneumo Measurements in the segmentDF
            lPneumoMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[lPneumoSelectRows])
            # remove NA values
            lPneumoMeasurementIndices <- lPneumoMeasurementIndices[!is.na(lPneumoMeasurements)]
            lPneumoMeasurements <- lPneumoMeasurements[!is.na(lPneumoMeasurements)]
            # plot the lower pneumo Measurements
            if(length(lPneumoMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=650,
                                y=rep((yOffset['lPneumo'] - 75), times=length(lPneumoMeasurements)),
                                label=lPneumoMeasurements,
                                color="black",
                                # alpha=.3,
                                size=2.5)
            }
            
            ### combined pneumo score  ###
            
            # get all row indices for the combined pneumo score
            pneumoSensorRows <- which(segmentMeasurementDF$sensorName == "Pneumo")
            # then select the row indices for the combined pneumo Measurements in the segment measurements
            pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
            # get the combined pneumo Measurements
            pneumoMeasurements <- printMeasurements[pneumoSelectRows]
            # get the row indices for the combined pneumo Measurements in the segmentDF
            pneumoMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[pneumoSelectRows])
            # calculate the y offset
            yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo'])) - 75
            # remove NA values
            pneumoMeasurementIndices <- pneumoMeasurementIndices[!is.na(pneumoMeasurements)]
            pneumoMeasurements <- pneumoMeasurements[!is.na(pneumoMeasurements)]
            # plot the combined pneumo Measurements
            # if(length(pneumoMeasurements) != 0) {
            #   g <- g + annotate(geom="text",
            #                     x=650,
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
            AutoEDASensorRows <- which(segmentMeasurementDF$sensorName == "AutoEDA")
            # then select the row indices for the EDA Measurements
            # in the segment measurements DF
            AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
            # get the EDA Measurements
            # use printMeasurements to automatically select rank or integer Measurements
            AutoEDAMeasurements <- printMeasurements[AutoEDASelectRows]
            # get the row indices for the EDA Measurements in the segmentDF
            if(length(AutoEDAMeasurements != 0)) {
              AutoEDAMeasurementIndices <- which(segmentDF$eventLabel %in%
                                                   segmentMeasurementDF$eventLabel[AutoEDASelectRows])
              # plot the EDA Measurements
              if(length(AutoEDAMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=650,
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
              ManualEDASensorRows <- which(segmentMeasurementDF$sensorName == "ManualEDA")
              # then select the row indices for the EDA Measurements
              # in the segment measurements DF
              ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
              # get the EDA Measurements
              # use printMeasurements to automatically select rank or integer Measurements
              ManualEDAMeasurements <- printMeasurements[ManualEDASelectRows]
              # get the row indices for the EDA Measurements in the segmentDF
              if(length(ManualEDAMeasurements != 0)) {
                ManualEDAMeasurementIndices <- which(segmentDF$eventLabel %in%
                                                       segmentMeasurementDF$eventLabel[ManualEDASelectRows])
                # plot the EDA Measurements
                if(length(ManualEDAMeasurements) != 0) {
                  g <- g + annotate(geom="text",
                                    x=650,
                                    y=rep((yOffset['eda']-225), times=length(ManualEDAMeasurements)),
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
            cardioSensorRows <- which(segmentMeasurementDF$sensorName == "Cardio")
            # then select the row indices for the Cardio Measurements in the segment measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio Measurements
            cardioMeasurements <- printMeasurements[cardioSelectRows]
            # get the row indices for the Cardio Measurements in the segmentDF
            if(length(cardioMeasurements) > 0) {
              cardioMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio Measurements
              if(length(cardioMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=650,
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] + 25), times=length(cardioMeasurements)),
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
            cardioSensorRows <- which(segmentMeasurementDF$sensorName == "FC")
            # then select the row indices for the Cardio Measurements in the segment measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio Measurements
            cardioMeasurements <- printMeasurements[cardioSelectRows]
            # get the row indices for the Cardio Measurements in the segmentDF
            if(length(cardioMeasurements) > 0) {
              cardioMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio Measurements
              if(length(cardioMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=650,
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
              pleSensorRows <- which(segmentMeasurementDF$sensorName == "PLE")
              # then select the row indices for the PLE Measurements in the segment measurements
              pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
              # get the PLE Measurements
              pleMeasurements <- printMeasurements[pleSelectRows]
              # get the row indices for the PLE Measurements in the segmentDF
              if( length(pleMeasurements) > 0 & !all(is.na(pleMeasurements)) ) {
                pleMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[pleSelectRows])
                # plot the PLE Measurements
                if(length(pleMeasurements) != 0) {
                  g <- g + annotate(geom="text",
                                    x=650,
                                    # need to use rep() for y in order to avoid a warning about names
                                    y=rep((yOffset['ple'] - 175), times=length(pleMeasurements)),
                                    label=pleMeasurements,
                                    color="black",
                                    size=2.5,
                                    na.rm=TRUE)
                }
              }

            } # end if showPLEScores
          
        } # end if showScores==TRUE && showExtractionVals==TRUE
        
        ######## rank values ########
        
        if(showScores==TRUE && showRankVals==TRUE) {
          
          # obtain the values from the segmentMeasurementDF
          
          # first get all row indices for the score of interest 
          # use the rank scores for this 
          measurementRows <- which(segmentMeasurementDF$rankScore != "")
          
          # make a vector of scores for printing
          selectMeasurements <- segmentMeasurementDF$rankScore
          
          # use a private function to remove leading zeros
          printMeasurements <- numFormatFn(selectMeasurements)
          
          ###
          
          if(all(isTRUE(showPneumoData),
                 isTRUE(showPneumoScores),
                 !isTRUE(PCASSFormat) ) ) {
            
            ###  upper pneumo  ### 
            
            # get all row indices for the upper pneumo data
            uPneumoSensorRows <- which(segmentMeasurementDF$sensorName == "UPneumo")
            # then select the row indices for the upper pneumo Measurements in the segment measurements
            uPneumoSelectRows <- uPneumoSensorRows[which(uPneumoSensorRows %in% measurementRows)]
            # get the upper pneumo measurements
            uPneumoMeasurements <- printMeasurements[uPneumoSelectRows]
            # get the row indices for the upper pneumo Measurements in the segmentDF
            uPneumoMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[uPneumoSelectRows])
            # remove NA values
            uPneumoMeasurementIndices <- uPneumoMeasurementIndices[!is.na(uPneumoMeasurements)]
            uPneumoMeasurements <- uPneumoMeasurements[!is.na(uPneumoMeasurements)]
            # plot the upper pneumo Measurements
            if(length(uPneumoMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=150,
                                y=rep((yOffset['uPneumo'] - 75), times=length(uPneumoMeasurements)),
                                label=uPneumoMeasurements,
                                color="black",
                                # alpha=.3,
                                size=4)
            }
            
            ### lower pneumo  ###
            
            # get all row indices for the lower pneumo data
            lPneumoSensorRows <- which(segmentMeasurementDF$sensorName == "LPneumo")
            # then select the row indices for the lower pneumo Measurements in the segment measurements
            lPneumoSelectRows <- lPneumoSensorRows[which(lPneumoSensorRows %in% measurementRows)]
            # get the lower pneumo Measurements
            lPneumoMeasurements <- printMeasurements[lPneumoSelectRows]
            # get the row indices for the lower pneumo Measurements in the segmentDF
            lPneumoMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[lPneumoSelectRows])
            # remove NA values
            lPneumoMeasurementIndices <- lPneumoMeasurementIndices[!is.na(lPneumoMeasurements)]
            lPneumoMeasurements <- lPneumoMeasurements[!is.na(lPneumoMeasurements)]
            # plot the lower pneumo Measurements
            if(length(lPneumoMeasurements) != 0) {
              g <- g + annotate(geom="text",
                                x=150,
                                y=rep((yOffset['lPneumo'] - 75), times=length(lPneumoMeasurements)),
                                label=lPneumoMeasurements,
                                color="black",
                                # alpha=.3,
                                size=4)
            }
            
            ### combined pneumo score  ###
            
            # get all row indices for the combined pneumo score
            pneumoSensorRows <- which(segmentMeasurementDF$sensorName == "Pneumo")
            # then select the row indices for the combined pneumo Measurements in the segment measurements
            pneumoSelectRows <- pneumoSensorRows[which(pneumoSensorRows %in% measurementRows)]
            # get the combined pneumo Measurements
            pneumoMeasurements <- printMeasurements[pneumoSelectRows]
            # get the row indices for the combined pneumo Measurements in the segmentDF
            pneumoMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[pneumoSelectRows])
            # calculate the y offset
            yPneumo <- mean(c(yOffset['lPneumo'], yOffset['uPneumo'])) - 75
            # remove NA values
            pneumoMeasurementIndices <- pneumoMeasurementIndices[!is.na(pneumoMeasurements)]
            pneumoMeasurements <- pneumoMeasurements[!is.na(pneumoMeasurements)]
            # plot the combined pneumo Measurements
            # if(length(pneumoMeasurements) != 0) {
            #   g <- g + annotate(geom="text",
            #                     x=650,
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
            AutoEDASensorRows <- which(segmentMeasurementDF$sensorName == "AutoEDA")
            # then select the row indices for the EDA Measurements
            # in the segment measurements DF
            AutoEDASelectRows <- AutoEDASensorRows[which(AutoEDASensorRows %in% measurementRows)]
            # get the EDA Measurements
            # use printMeasurements to automatically select rank or integer Measurements
            AutoEDAMeasurements <- printMeasurements[AutoEDASelectRows]
            # get the row indices for the EDA Measurements in the segmentDF
            if(length(AutoEDAMeasurements != 0)) {
              AutoEDAMeasurementIndices <- which(segmentDF$eventLabel %in%
                                                   segmentMeasurementDF$eventLabel[AutoEDASelectRows])
              # plot the EDA Measurements
              if(length(AutoEDAMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=150,
                                  y=rep((yOffset['eda'] - 75), times=length(AutoEDAMeasurements)),
                                  label=AutoEDAMeasurements,
                                  color="black",
                                  size=4,
                                  na.rm=TRUE)
              }
            }
            
            ### Manual/un-filtered EDA
            
            if(isTRUE(showManualEDA)) {
              
              # get all row indices for the EDA data
              ManualEDASensorRows <- which(segmentMeasurementDF$sensorName == "ManualEDA")
              # then select the row indices for the EDA Measurements
              # in the segment measurements DF
              ManualEDASelectRows <- ManualEDASensorRows[which(ManualEDASensorRows %in% measurementRows)]
              # get the EDA Measurements
              # use printMeasurements to automatically select rank or integer Measurements
              ManualEDAMeasurements <- printMeasurements[ManualEDASelectRows]
              # get the row indices for the EDA Measurements in the segmentDF
              if(length(ManualEDAMeasurements != 0)) {
                ManualEDAMeasurementIndices <- which(segmentDF$eventLabel %in%
                                                       segmentMeasurementDF$eventLabel[ManualEDASelectRows])
                # plot the EDA Measurements
                if(length(ManualEDAMeasurements) != 0) {
                  g <- g + annotate(geom="text",
                                    x=150,
                                    y=rep((yOffset['eda']-225), times=length(ManualEDAMeasurements)),
                                    label=ManualEDAMeasurements,
                                    color="black",
                                    size=4,
                                    na.rm=TRUE)
                }
              }
              
            } # end if showManualEDA
            
          } # end if showEDAScores
          
          ###  Cardio  ###
          
          if(all(isTRUE(showCardioData),
                 isTRUE(showCardioScores),
                 !isTRUE(PCASSFormat) ) ){
            
            # get all row indices for the Cardio data
            cardioSensorRows <- which(segmentMeasurementDF$sensorName == "Cardio")
            # then select the row indices for the Cardio Measurements in the segment measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio Measurements
            cardioMeasurements <- printMeasurements[cardioSelectRows]
            # get the row indices for the Cardio Measurements in the segmentDF
            if(length(cardioMeasurements) > 0) {
              cardioMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio Measurements
              if(length(cardioMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=150,
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] + 25), times=length(cardioMeasurements)),
                                  label=cardioMeasurements,
                                  color="black",
                                  size=4,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showCardioScores
          
          ###  forearm Cardio cuff or finger cuff ###
          
          if(isTRUE(showFCData) && showFCScores==TRUE) {
            
            # get all row indices for the Cardio data
            cardioSensorRows <- which(segmentMeasurementDF$sensorName == "FC")
            # then select the row indices for the Cardio Measurements in the segment measurements
            cardioSelectRows <- cardioSensorRows[which(cardioSensorRows %in% measurementRows)]
            # get the Cardio Measurements
            cardioMeasurements <- printMeasurements[cardioSelectRows]
            # get the row indices for the Cardio Measurements in the segmentDF
            if(length(cardioMeasurements) > 0) {
              cardioMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[cardioSelectRows])
              # plot the Cardio Measurements
              if(length(cardioMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=150,
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['cardio'] - 300), times=length(cardioMeasurements)),
                                  label=cardioMeasurements,
                                  color="black",
                                  size=4,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showFCScores
          
          ###  PLE  ###
          
          if(isTRUE(showPLEData) && showPLEScores==TRUE) {
            
            # get all row indices for the PLE data
            pleSensorRows <- which(segmentMeasurementDF$sensorName == "PLE")
            # then select the row indices for the PLE Measurements in the segment measurements
            pleSelectRows <- pleSensorRows[which(pleSensorRows %in% measurementRows)]
            # get the PLE Measurements
            pleMeasurements <- printMeasurements[pleSelectRows]
            # get the row indices for the PLE Measurements in the segmentDF
            if( length(pleMeasurements) > 0 & !all(is.na(pleMeasurements)) ) {
              pleMeasurementIndices <- which(segmentDF$eventLabel %in% segmentMeasurementDF$eventLabel[pleSelectRows])
              # plot the PLE Measurements
              if(length(pleMeasurements) != 0) {
                g <- g + annotate(geom="text",
                                  x=150,
                                  # need to use rep() for y in order to avoid a warning about names
                                  y=rep((yOffset['ple'] - 175), times=length(pleMeasurements)),
                                  label=pleMeasurements,
                                  color="black",
                                  size=4,
                                  na.rm=TRUE)
              }
            }
            
          } # end if showPLEScores
          
        } # end if showScores==TRUE && showRankVals==TRUE
        
        
        
        
        
        
        
        
        ################################    plot appearance     ##################
        
        g <- g + ylab("y-change")
        g <- g + xlab("x-time")
        
        g <- g + ggtitle(plotTitle) + 
          theme(plot.title = element_text(size=8, color="#666666", face="bold", hjust=0))
        
        g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,nrow(segmentDF),300))
        
        ##### PRINT the chart to the graphics device #####
        
        print(g)
        
      } # end loop over l event stimulus segments in each chart
      
    } # end iteration over k charts
    
  } # end iteration over j series 
  
  if(printPlot == TRUE && length(eventNames)!=0) {
    graphics.off()
    dev.new() 
  } 
  
} # end iteration over i exams

if(showNames==TRUE) print(paste(length(uniqueExams), "exams processed"))

# reset the NCCA ASCII init 
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)



