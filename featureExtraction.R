# new function for feature extraction
# for all NCCA ASCII data channels
# 4/23/2016
# Raymond Nelson
#
####



# import a library for better string manipulation
# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]




{
  
  # source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  
  # import a vector of question labels that are not included in feature extraction
  source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)
  
  # source this also 
  # for the ratePerMin() function
  source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  # for the lowPass.2hz.2nd() function to remove dichrotic notch while estimating cardio rate
  source(paste0(RPath, 'sigProc_extra.R'), echo=FALSE)
  
  # source the scripts for each sensor
  source(paste0(RPath, 'PneumoExtract.R'), echo=FALSE)
  source(paste0(RPath, 'EDAExtract.R'), echo=FALSE)
  source(paste0(RPath, 'CardioExtract.R'), echo=FALSE)
  source(paste0(RPath, 'newFCExtract.R'), echo=FALSE)
  source(paste0(RPath, 'PLEExtract.R'), echo=FALSE)
  
  # source(paste0(RPath, 'FCExtract.R'), echo=FALSE)
  
  # source the scripts with the helper functions
  source(paste0(RPath, 'pneumoMeasurement.R'), echo=FALSE)
  
  # experimental functions for extracting respiration patterns 
  source(paste0(RPath, "pneumoPatterns.R"), echo=FALSE)
  
  # amplitude extraction for EDA and cardio
  source(paste0(RPath, 'amplitudeExtractPC.R'), echo=FALSE)
  source(paste0(RPath, 'amplitudeExtractHelperFunctions.R'), echo=FALSE)
  source(paste0(RPath, 'slopeChange.R'), echo=FALSE)
  
  # August 2023 functions were abstracted from amplitudeExtractPC.R
  source(paste0(RPath, "getResponsePeaks.R"), echo=FALSE)
  source(paste0(RPath, "getResponseOnsets.R"), echo=FALSE)
  source(paste0(RPath, "getMaxOnsetPeakDistance.R"), echo=FALSE)
  source(paste0(RPath, "getSlopeDirection.R"), echo=FALSE)
  source(paste0(RPath, "abstractScale.R"), echo=FALSE)
  
  source(paste0(RPath, "checkEDATonicity.R"), echo=FALSE)
  
  source(paste0(RPath, "checkCardioArtifacts.R"), echo=FALSE)
  
  source(paste0(RPath, "checkPneumoArtifacts.R"), echo=FALSE)
  
  # source(paste0(RPath, "checkRespirationArtifacts.R"), echo=FALSE)
  
  # source(paste0(RPath, 'PLEMeasurement.R'), echo=FALSE)
  
   source(paste0(RPath, 'rbpfProb.R'), echo=FALSE)
  
  # source(paste0(RPath, 'amplitudeExtractOSS2.R'), echo=FALSE)
  
  # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
  ## these are set the init script ##
  # cps <- 30
  # prestimSeg <- 5
  # EDALat <- .5
  # CardioLat <- .5
  # ROWEnd <- 5
  # measuredSeg <- 15
  # pneumoAnsBuff <- 1.25
  
}



######################  main function  #############################



featureExtraction <- function(x=uniqueExams, 
                              extractPneumo=extractPneumo, 
                              extractEDA=extractEDA, 
                              extractCardio=extractCardio, 
                              extractPLE=extractPLE, 
                              extractPneumoPatterns=extractPneumoPatterns, 
                              writeCSV=FALSE ) {
  # function to iterate over a vector of data frame names 
  # and extract the scoring feature for pneumo, EDA, cardio and PLE data
  # 4/23/2016
  # Raymond Nelson
  #
  # x is a vector of names of data frames that contain the time series data 
  #
  # writeCSV will save the exam data frame to a .csv text file 
  # 
  # other parameters may be obtained from the global environment
  #
  # EW is a logical value to impose a strict use of the evaluation window
  # when set to FALSE upward responses will be evaluated to the end of response
  # even if the end of response is after the end of the evaultion window
  # when set to TRUE feature extraction will stop at the end of the evaluation window
  # for responses that continue past the evaluation window
  #
  # ROW is a logical value to impose a strict use of the response onset window
  # when set to TRUE upward respsonse segments will not be evaluatuated
  # if the positive slope change begins after the response onset window
  # when set to FALSE all upward slope changes will be evaluated
  # if they begin in the evaluation window, and only the response onset is required to
  # occur in the response onset window
  #
  # descentRule is a logical value that excludes peaks from measurement extraction
  # if they data descend more than 50% from previous response peak to the response onset value
  #
  # ignore is the number of samples to ignore for short duration slope changes
  #
  ####
  
  if(!exists("uniqueExams")) {
    uniqueExams <- x
    
    # uses the showNames and output settings from the global environment
    # set in the NCCAASCII_init.R script
  }
  
  ##### iterate over each exam in the list #####
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      # get the exam data and series info
      
      assign("i", i, pos=1)
      
      examName <- uniqueExams[i]
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      examDF$examName <- as.character(examDF$examName)
      examDF$seriesName <- as.character(examDF$seriesName)
      examDF$chartName <- as.character(examDF$chartName)
      
      examStartRow <- 1
      examEndRow <- nrow(examDF)
      
      assign("examDF", examDF, pos=1)
      assign("examName", examName, pos=1)
      
      if(showNames==TRUE) {
        print(paste("exam", i, "of", length(uniqueExams)))
        print(paste("exam name:", examName))
      }
      
      # write the exam data frame to a .csv text file 
      if(isTRUE(writeCSV)) {
        # write.csv(examDF, file=paste0(str_sub(searchString, 2,-2), ".csv"), row.names=FALSE)
        # library(readr)
        write_csv(examDF, file=paste0(str_sub(searchString, 2,-2), ".csv"))
      }
      
      # get the names of unique series
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
      # if(examName == "DCIVSL") {
      #   stop()
      # }
      
    }
    
    ##### iterate over each unique series #####

    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        assign("j", j, pos=1)
        
        # get the series data and chart info
        
        seriesName <- uniqueSeries[j]
        # get the list of time series data for the charts in the exam
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        assign("seriesDF", seriesDF, pos=1)
        assign("seriesName", seriesName, pos=1)
        
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
        if(showNames==TRUE) print(paste("series", seriesName))
        
      }
      
      ##### initialize a pattern data frame for this series #####
      
      { 
        respPatternsDF <- NULL
      }
      
      ##### iterate over each chart in the series #####
      
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          assign("k", k, pos=1)
          
          # get the chart data and event names
          
          chartName <- uniqueCharts[k]
          # get the data frame with the time series data for each chart in the series
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          assign("chartDF", chartDF, pos=1)
          assign("chartName", chartName, pos=1)
          
          if(showNames==TRUE) print(paste("Chart:", chartName))
          
          # skip short charts less than 10 seconds
          if(nrow(chartDF) < 600) next()
          
          #### process the stimulus segments ####
          
          # a vector of event onset rows
          eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
          
          # remove events before X and after XX (Dec 13, 2021)
          # prevent feature extraction with anything before X or after XX
          XIdx <- which(eventNames == "X")
          if(length(XIdx) != 0) {
            eventNames <- eventNames[XIdx[1]:length(eventNames)]
          }
          XXIdx <- which(eventNames == "XX")
          if(length(XXIdx) !=0) {
            eventNames <- eventNames[1:XXIdx[1]]
          }
          
          # Dec 16, 2021
          # exclude events that are repeated questions 
          eventNames <- eventNames[!grepl("A",eventNames)]
          
          # June 28, 2023
          # exclude symptomatic questions 
          eventNames <- eventNames[!grepl("Y",eventNames)]
          
          # June 28, 2023
          # exclude neutral questions 
          eventNames <- eventNames[!grepl("N",eventNames)]
          eventNames <- eventNames[!grepl("I",eventNames)]
          
          
          # exclude some of the events 
          eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          
          if(length(eventNames)==0) next()
          
          rbpfMsg <- "none"
          
          cardioRate <- NA
          
          UPRate <- NA
          LPRate <- NA
          
          rateRatio <- NA
          
        }
       
        {
          
        ####  reset the feature extraction columns for this chart  ####
          
          chartDF$UPneumoExtract <- ""
          chartDF$LPneumoExtract <- ""
          chartDF$AutoEDAExtract <- ""
          chartDF$ManualEDAExtract <- ""
          chartDF$CardioExtract <- ""
          if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
            chartDF$PPG1Extract <- ""
          }
            
          # Aug 30, 2023
          chartDF$CardioExtract <- ""
          chartDF$CardioMeasure <- 0
          chartDF$CardioDuration <- 0
          chartDF$CardioComplexity <- 0
          
          chartDF$CardioExtract_d <- "" # diastolic
          chartDF$CardioExtract_s <- "" # systolic
          chartDF$CardioExtract_m <- "" # mid
          chartDF$CardioExtract_v <- "" # very slow
          
          # Aug 31, 2023
          # chartDF$AutoEDA_a <- 0
          # chartDF$Artifacts_a <- 0
          # chartDF$UPneumo_a <- 0
          # chartDF$LPneumo_a <- 0
          # chartDF$Pneumo_a <- 0
          # chartDF$Cardio1_a <- 0
          # if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
          #   chartDF$PPG1_a <- 0
          # }
          
          # Aug 29, 2023
          chartDF$UPneumoExtract <- ""
          chartDF$LPneumoExtract <- ""
          chartDF$UPneumoMeasure <- 0
          chartDF$LPneumoMeasure <- 0
          
          # chartDF$UPneumoPrestim <- ""
          # chartDF$LPneumoPrestim <- ""
          # chartDF$AutoEDAPrestim
          # chartDF$ManualEDAPrestim
          # if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
          #   chartDF$PPG1Prestim <- ""
          # }
          
        }
        
        #### calculate the cardio rate and RBPF ####
        
        if(!isTRUE(PCATFormat) && chartDF$Cardio1[1] != -9.9) {
        
          # used by the cardio extract function
          
          cardioRate <- ratePerMin(MASmooth(x=chartDF$Cardio1, y=2, times=1),
                                   buffer=9,
                                   peaks="upper",
                                   lowPass=TRUE)
          
          rbpfMsg <- rbpfProbFn(x=chartDF, y=NULL)
          
          # print(paste("cardio rate:", cardioRate))
          # print(paste("rbpfMsg:", rbpfMsg))
        
          assign("cardioRate", cardioRate, envir=.GlobalEnv)
          assign("rbpfMsg", rbpfMsg, envir=.GlobalEnv)
        
        }

        #### cardio rate using PLE ####
        
        # Jan 17, 2023 check for PLE sensor 
        if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0)) != 0) {
          
          if(chartDF$Cardio1[1] == -9.9 && chartDF$PPG1[1] != -9.9) {
            
            cardioRate <- ratePerMin(MASmooth(x=chartDF$c_PPG1, y=2, times=1),
                                     buffer=9,
                                     peaks="upper",
                                     lowPass=TRUE)
            
            # print(paste("cardio rate:", cardioRate))
            
          }
          
        }
        
        #### calculate the respiration rate ####
        
        if(!isTRUE(PCATFormat) && chartDF$UPneumo[1] != -9.9) {
          
          # used the by the pneumo extract function
          
          # for the ratePerMin() function
          # source(paste0(RPath, "sigProcHelper.R'), echo=FALSE)
          
          # determine the respiration rate
          UPRate <- ratePerMin(x=chartDF$c_UPneumoSm, buffer=40, peaks="upper", lowPass=TRUE)
          
          # is the upper respiration rate outside the normal range
          UPMeasurement <- ifelse(round(UPRate) < 6,
                                  "ONR_slow",
                                  ifelse(round(UPRate) > 26,
                                         "ONR_fast",
                                         "normal"))
          # ratePerMin()
          LPRate <- ratePerMin(x=chartDF$c_LPneumoSm, buffer=40, peaks="upper", lowPass=TRUE)
          
          # is the lower respiration rate outside the normal range
          LPMeasurement <- ifelse(round(LPRate) < 6,
                                  "ONR_slow",
                                  ifelse(round(LPRate) > 26,
                                         "ONR_fast",
                                         "normal"))
          
          rateRatio <- round(exp(-abs(log(UPRate / LPRate))), 2)
          
          # print(paste("upper respiration rate:", UPRate))
          # print(paste("lower respiration rate:", LPRate))
          # print(paste("respiration rate ratio:", rateRatio))
          
          assign("UPRate", UPRate, envir=.GlobalEnv)
          assign("LPRate", LPRate, envir=.GlobalEnv)
          assign("rateRatio", rateRatio, envir=.GlobalEnv)
          
        } else {
          
          LPRate <- NA
          UPRate <- NA
          rateRatio <- NA
          
        }
        
        ######## increment the K loop for the next chart if no events ########
        
        if(length(eventNames) < 4) {
          seriesDF[chartOnsetRow:chartEndRow,] <- chartDF
          next()
        }
        
        ####### slice the RQs and CQs only if more than 2 each #######
        
        {
          
          # Dec 13 2021
          # need to prevent feature extraction with anything before X or after XX
          uniqueRQs <- eventNames[grep("R", eventNames)]
          uniqueCQs <- eventNames[grep("C", eventNames)]
          

          # slice the RQs and CQs only if more than 2 RQs and 2 CQs
          # if(length(uniqueRQs) >= 2 && length(uniqueCQs) >= 2) {
          #   theseRows <- grep("[R C]", chartDF$Label)
          #   thisChartDF <- chartDF[theseRows,]
          #   eventNames <- thisChartDF$eventLabel[thisChartDF$eventLabel!=""]
          #   eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          # } else {
          #   thisChartDF <- chartDF
          # }
          
          # duplicate the chartDF and work with the copy
          thisChartDF <- chartDF
          
        }
        
        #######  loop over all the events in the chart data frame  #######
        
        l=2
        for (l in 1:length(eventNames)) {
          
          {
            
            # slice the segment
            
            segmentName <- eventNames[l]
            if(showNames==TRUE) print(segmentName)
            
            # increment the l loop for excluded events
            if(segmentName %in% excludeEvents) next()
            
            # get the onset row using the segment name so that events during the prestimSegment are ignored
            segOnsetRow <- which(thisChartDF$eventLabel==segmentName)[1]
            # set the end row using the evaluation window length 
            segEndRow <- segOnsetRow + measuredSeg*cps - 1
            if(segEndRow > nrow(thisChartDF)) segEndRow <- nrow(thisChartDF)
            
            # get the segment prestim row
            prestimRow <- segOnsetRow - prestimSeg*cps
            if(prestimRow < 1) prestimRow <- 1
            
            # get the segment start row and end row
            startRow <- prestimRow
            endRow <- segEndRow + addSeg*cps
            if(endRow > nrow(thisChartDF)) endRow <- nrow(thisChartDF)
            
            # get the segment data frame
            segmentDF <- thisChartDF[startRow:endRow,]
            # View(segmentDF)
            
            assign("segmentDF", segmentDF, pos=1)
            assign("segmentName", segmentName, pos=1)
            
            # if(showNames==TRUE) print(segmentName)
            
          }
          
          {
            
            # increment the loop with no feature extraction
            # if the segment is not RQ or CQ 
            # when the chart includes 2 or more RQs and CQs
            if(length(uniqueRQs) >= 2 && length(uniqueCQs) >= 2) {
              if(!isTRUE(grepl("[R C]", segmentName))) {
                next() 
              }
              # next()
            } else {
              if(segmentName %in% excludeEvents) {
                next()
              }
              # extract all events if not a CQT chart
            }
            
          }
          
          #### get the row indices for the stimulus event ####
          
          {
            
            # adjust the rows so that row numbers refer to data in the segmentDF not the thisChartDF
            prestimRow <- prestimRow - startRow + 1 # this will set the prestim row to 1
            
            # get the onset row 
            # usually 151 when there are 5 prestim seconds in the data frame
            # 301 when there are 10 prestimulus seconds
            # stimOnsetRow <- which(segmentDF$Events=="onsetRow")[1]
            # stimOnsetRow <- segOnsetRow - startRow + 1 # will normally set to 301
            # locate the onset by segmentName to ignore other stimuli during the prestim period
            stimOnsetRow <- which(segmentDF$eventLabel==segmentName)[1] # normally 301
            # get the first offset after the onset row
            # stimoffsetRow is the end of the verbal stimulus while stimEndRow is the end of the EW
            stimOffsetRow <- which(segmentDF$Events[stimOnsetRow:nrow(segmentDF)]=="offsetRow")[1] + stimOnsetRow - 1
            # stimOffsetRow will be NA if the stimulus length exceeds the segEndRow or endRow
            if(is.na(stimOffsetRow)) stimOffsetRow <- stimOnsetRow + measuredSeg*cps - 3
            
            # locate the answer index
            answerRow <- which(segmentDF$Events[stimOnsetRow:nrow(segmentDF)]=="answerRow")[1] + stimOnsetRow - 1
            if(is.na(answerRow)) answerRow <- stimOffsetRow + 1
            if(answerRow == "") answerRow <- stimOffsetRow + 1
            
            # stimEndRow <- segEndRow - startRow + 1 # will normally set to 750
            # stimEndRow is the end of the EW (evaluation window)
            stimEndRow <- stimOnsetRow + measuredSeg*cps - 1 # normally 750
            if(stimEndRow > nrow(segmentDF)) stimEndRow <- nrow(segmentDF)
            
            # check again
            if(answerRow >= stimEndRow) answerRow <- stimEndRow - 1
            if(stimOffsetRow >= answerRow) stimOffsetRow <- answerRow - 1
            
            # fix potential problems
            if(stimOffsetRow <= stimOnsetRow) stimOffsetRow <- stimOnsetRow + 1
            if(answerRow <= stimOffsetRow) answerRow <- stimOffsetRow + 1
            if(stimOffsetRow >= nrow(segmentDF)) stimOffsetRow <- nrow(segmentDF) - 2
            if(answerRow >= nrow(segmentDF)) answerRow <- nrow(segmentDF) - 1
            
            # print(prestimRow)
            # print(stimOnsetRow)
            # print(stimOffsetRow)
            # print(answerRow)
            # print(stimEndRow)

            # august 7, 2020
            # increment the loop if examiner marked artifact or event during 5 prestim seconds
            if(stimOnsetRow-150 < 1) {
              fixSecPreRow <- 1 
            } else {
              fixSecPreRow <- stimOnsetRow-150
            }
            
            if(length(which(segmentDF$Events[fixSecPreRow:(stimOnsetRow-1)] != "")) > 0) {
              # increment without feature extraction if artifacts in the prestim segment
              next()
              # extract columns will hold the "" value
            }
            
            # increment if the segment is too short
            if(nrow(segmentDF) < 1050) next()
            
          }
          
          #### get the feature extraction parameters for the segment ####
          
          {
            
            extract.params <- list(onset=stimOnsetRow, 
                                   offset=stimOffsetRow, 
                                   answer=answerRow, 
                                   end=stimEndRow, 
                                   segName=segmentName, 
                                   chart=chartName, 
                                   series=seriesName, 
                                   exam=examName,
                                   rbpfMsg=rbpfMsg,
                                   cardioRate=cardioRate,
                                   UPRate=UPRate,
                                   LPRate=LPRate,
                                   rateRatio=rateRatio)
            
            assign("extract.params", extract.params, pos=1)
            
          }
          
          #### stop on command for inspection ####
          
          # June 23, 2023
          # stopSeg is set in the workFlow_init.R script
          if(isTRUE(as.logical(stopSeg[1]))) {
            if(all(i==as.numeric(stopSeg['case']), j==as.numeric(stopSeg['series']), k==as.numeric(stopSeg['chart']), segmentName==stopSeg['seg'])) {
              
            # if(all(i==stopSeg[2], i==stopSeg['case'], seriesName==stopSeg['series'], segmentName==stopSeg['seg'])) {
              print("stopped for inspection")
              print(paste("exam:", examName, "series:", seriesName, "chart:", chartName, "segment:", segmentName))
              stop()
            }
          }
          
          ############# pneumo RLE extraction #############
          
          assign("extract.params", extract.params, pos=1)
          assign("chartDF", chartDF, envir=.GlobalEnv)
          assign("segmentDF", segmentDF, pos=1)
          assign("segmentName", segmentName, pos=1)
          # assign("sensorName", sensorName, pos=1)
          
          if(exists("UPMeasurement")) assign("tRate", UPMeasurement, pos=1) 
          if(exists("UPMeasurement")) assign("aRate", LPMeasurement, pos=1)
          
          # if(all(i == 1, seriesName == "X", chartName == "01A" && segmentName == "C6")) {
          #   assign("extract.params", extract.params, pos=1)
          #   assign("chartDF", chartDF, envir=.GlobalEnv)
          #   assign("segmentDF", segmentDF, pos=1)
          #   assign("segmentName", segmentName, pos=1)
          #   assign("sensorName", "pneumoUL", pos=1)
          #   assign("tRate", UPMeasurement, pos=1)
          #   assign("aRate", LPMeasurement, pos=1)
          #   stop()
          #   # View(segmentDF)
          #   # extractList
          #   # extract.params
          #   # segmentDF$AutoEDAMeasure[301]
          #   # segmentDF$AutoEDAPrestim[301]
          # }
          
          # Oct 1, 2023 these are needed to call the next function manually during inspection
          # UPMeasurement <- "normal"
          # LPMeasurement <- "normal"
          
          # if(all(isTRUE(extractPneumo), segmentDF$UPneumo[1] != -9.9, rateRatio >=.9)) {
          if( all(isTRUE(extractPneumo), segmentDF$UPneumo[1] != -9.9) ) {
            print("  pneumo feature extraction")
            segmentDF <- pneumoExtractFn(segmentDF=segmentDF,
                                         extract.params=extract.params,
                                         tRate=UPMeasurement,
                                         aRate=LPMeasurement)
            # pneumoExtractFn is called once to measure both the thoracic and abdominal respiration
            
            # # these are the measurement values
            # segmentDF$UPneumoMeasure[301:361]
            # segmentDF$LPneumoMeasure[301:361]
            # # these are the response indices
            # segmentDF$UPneumoExtract[301:751]
            # segmentDF$PPneumoExtract[301:751]
          }
          
          # View(segmentDF)
        
          ##### *experimental* pneumo pattern extraction #####
          
          # if(chartName == "01A" && segmentName == "4K") {
          #   assign("segmentDF", segmentDF, pos=1)
          #   assign("extract.params", extract.params, pos=1)
          #   stop()
          # }
          
          if(all(isTRUE(extractPneumoPatterns), segmentDF$UPneumo[1] != -9.9, rateRatio >=.9)) {

            print("  pneumo pattern extraction")

            # source(paste0(RPath, "pneumoPatterns.R"), echo=FALSE)

            # respPatternsDF <- NULL # initialized earlier
            
            pnPatternDF <- pneumoPatternsFn(segmentDF=segmentDF,
                                            extract.params=extract.params,
                                            # verbalAnswer,
                                            rateDiff=.05,
                                            ampDiff=.05,
                                            baseDiff=.05,
                                            constrained=FALSE)

            # append the pattern output for each stimulus event

            # respPatternDF is initialized before the k loop over charts
            respPatternsDF <- rbind(respPatternsDF, pnPatternDF)

          }
          
          # View(segmentDF)
          
          ############# pneumo caliper functions #############
          
          {
            
            # source("~/Dropbox/R/NCCA_ASCII_Parse/pneumoCaliperFunctions.R", echo=FALSE)
            
            
            
            
            
          }
          
          ############### cardio feature extraction ##############
          
          # if(all(i == 2, seriesName == "X", chartName == "03A" && segmentName == "R7")) {
          #   assign("chartDF", chartDF, envir=.GlobalEnv)
          #   assign("segmentDF", segmentDF, pos=1)
          #   assign("extract.params", extract.params, pos=1)
          #   assign("segmentName", segmentName, pos=1)
          #   stop()
          #   # View(segmentDF)
          #   # extractList
          #   # extract.params
          #   # segmentDF$AutoEDAMeasure[301]
          #   # segmentDF$AutoEDAPrestim[301]
          # }
          
          assign("segmentDF", segmentDF, pos=1)
          assign("extract.params", extract.params, pos=1)
          assign("cardioRate", cardioRate, envir=.GlobalEnv)
          assign("rbpfMsg", rbpfMsg, envir=.GlobalEnv)
          assign("segmentName", segmentName, pos=1)
          
          if(all(extractCardio==TRUE, segmentDF$Cardio1[1] != -9.9)) {
            print("  cardio feature extraction")
            segmentDF <- CardioExtractFn(x=segmentDF, 
                                         y=extract.params,
                                         cardioRate=cardioRate,
                                         rbpfMsg=rbpfMsg)
            # segmentDF$CardioMeasure[301:750]
            # segmentDF$CardioExtract
          } 
          
          # stop for inspection
          # if(all(chartName == "02A",
          #        segmentName == "10R")) {
          #   assign("segmentDF", segmentDF, pos=1)
          #   assign("extract.params", extract.params, pos=1)
          #   stop()
          #   # View(segmentDF)
          # }
          
          ############### finger cuff feature extraction #############
          
          if( extractFC==TRUE && sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0 ) {
            # check to see if finger cuff data exist
            # if( sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0 ) {
              print("  finger cuff feature extraction")
              segmentDF <- newFCExtractFn(x=segmentDF, y=extract.params)
            # } # end if FC data exist
          } 
          
          ################# PLE feature extraction ##################
          
          if(all(extractPLE==TRUE, segmentDF$PPG1[1] != -9.9)) {
            # check to see if PLE data exist for the chart
            if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0))!=0) {
              print("  PLE feature extraction")
              segmentDF <- newPLEExtractFn(x=segmentDF, y=extract.params)
            } # end if PLE data exist
          } 
          
          ################ EDA feature extraction ################
          
          # if(all(examName == "DX01602001FLAT", chartName == "01A", segmentName == "2")) {
          # if(all(seriesName=="1", chartName=="01A" && segmentName=="4")) {
          # if(chartName == "01A" && segmentName == "R7") {
          #   assign("segmentDF", segmentDF, pos=1)
          #   assign("extract.params", extract.params, pos=1)
          #   stop()
          # }
          
          # assign("segmentDF", segmentDF, pos=1)
          # assign("extract.params", extract.params, pos=1)
          
          if(extractEDA==TRUE) {
            print("  EDA feature extraction")
            segmentDF <- EDAExtractFn(x=segmentDF, y=extract.params)
          }
          
          # segmentDF$AutoEDA_a
          
          # if(chartName == "01A" && segmentName == "7R") {
          #   assign("segmentDF", segmentDF, pos=1)
          #   assign("extract.params", extract.params, pos=1)
          #   stop()
          #   # View(segmentDF)
          #   # extractList
          #   # extract.params
          #   # segmentDF$AutoEDAMeasure[301]
          #   # segmentDF$AutoEDAPrestim[301]
          # }
          
          ################## PCASS-2 feature extraction ###############
          
          {
            
            # not used
            
          }
          
          # pass the segmentDF to the thisChartDF
          # do not save to the examDF because the segments may overlap
          # saving back to the thisChartDF preserves any overlapping information from one seg to the next
          thisChartDF[startRow:endRow,] <- segmentDF
          
        } # end loop over l events
        
        
        #### check for extreme or unusual values ####
        
        {
          
          # thisChartDF
          
        }
        
        # pass back the RQs and CQs after feature extraction
        # chartDF[theseRows,] <- thisChartDF
        chartDF <- thisChartDF
          
        # pass the chartDF to the seriesDF
        seriesDF[chartOnsetRow:chartEndRow,] <- chartDF

      } # end loop over k charts
      
      # pass the seriesDF to the examDF
      examDF[seriesOnsetRow:seriesEndRow,] <- seriesDF
      
      # save the pattern recognition data for respiration sensors
      write.csv(respPatternsDF, file=paste0(examName,"_",seriesName,"_", "respPatternsDF.csv") , row.names=FALSE)
      
    } # end loop over j series
    
      # save the examDF to the global environment 
      assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over i exams 
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end featureExtraction() function

# featureExtraction(x=uniqueExams)

# Feb 6, 2022 to extract respiration patterns
# featureExtraction(x=uniqueExams,
#                   extractPneumo=FALSE,
#                   extractEDA=FALSE,
#                   extractCardio=FALSE,
#                   extractPLE=FALSE,
#                   extractPneumoPatterns=TRUE,
#                   writeCSV=FALSE )



