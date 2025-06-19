# R script to make a data frame of measurements
# orgin date was not documented
# Raymond Nelson
# 
# input is a vector of exam names
# ouput is a data frame of measurements for each exam
#
# modified July 21 2023 
# to add columns for 
# prestim - the sample index where we begin to use compare prestim activity with the stim segment
# response onset - the sample index where the diagnostic change in physiology starts
# response end - sample idx where the diagnoistic change in activity ends (e.g., EDA peak)
# response recovery - can be used for half-recovery or other
# poststim - a fixed index after the WOE that can help evaluate return to tonicity
####



# library(stringr)


# if(!exists("RPath")) {
#   # mac
#   RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
#   # windows
#   # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
# }

# source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)


# source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)


# list of events to exclude from analysis
# source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)





# need the getFirstLastEventFn() 
source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]




extractMeasurementsFn<- function(x=uniqueExams, 
                                 writeCSV=FALSE, 
                                 showNames=TRUE, 
                                 output=FALSE ) {
  # function to iterate over a vector of data frame names 
  # and initialize a _Measurements data frame of all measurements
  # 6/16/2016
  # Raymond Nelson
  #
  # x input is a vector of names of data frames with time series data for all exams
  # 
  # output of the function will be a message indicating the number of exams processed
  # output=TRUE will output the _Measurement data frame
  ####
  
  uniqueExams <- x
  
  print("Extract measurements to a data frame")
  
  if(!exists("showNames")) showNames <- TRUE
  if(!exists("output")) output <- FALSE
  
  ###### iterate over each exam in the list ######
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    ## get the data for this exam ##
    
    {
      
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

      if(showNames==TRUE) print(examName)
      
      # initialize the output data frame
      outputDF <- NULL
      
      # get the names of all unique series in the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
    }
    
    #### iterate over each unique series ####
    
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        seriesName <- uniqueSeries[j]
        # get the list of time series data for the charts in the exam
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        # seriesOnsetRow <- range(which(examDF$seriesName==seriesName))[1]
        # seriesEndRow <- range(which(examDF$seriesName==seriesName))[2]
        seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        assign("seriesDF", seriesDF, pos=1)
        assign("seriesName", seriesName, pos=1)
        
        if(showNames==TRUE) print(paste("series", seriesName))
        
        # uniqueCharts <- names(seriesDF)
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
      }
      
      #### iterate over each chart in the series ####
      
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          ## get the time series data frame for each chart in the series ##
          
          chartName <- uniqueCharts[k]
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          # View(chartDF)
          
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          assign("chartDF", chartDF, pos=1)
          assign("chartName", chartName, pos=1)
          
          print(paste("Chart:", chartName))
          
          # skip short charts less than 10 seconds
          if(nrow(chartDF)<600) next()
          
          # a vector of event onset rows
          Labels <- chartDF$Label[chartDF$eventLabel!=""]
          eventLabels <- chartDF$eventLabel[chartDF$eventLabel!=""]
          
          # 2-21-2019 do not exclude non-stim events
          # so the selectCQ function can consider these events
          # when selecting RQ/CQ pairs
          # Labels <- Labels[!(Labels %in% excludeEvents)]
          # eventLabels <- eventLabels[!(Labels %in% excludeEvents)]
          
          # advance to the next chart if no events
          if(length(eventLabels)==0) {
            print("no stimulus events")
            next()
          } 
          
          # get the first and last event indices
          firstEvent <- getFirstLastEventFn(x=chartDF)[1]
          lastEventEnd <- getFirstLastEventFn(x=chartDF)[2]
          
        }
        
        ## initialize a data frame to hold the output measurement table for this chart ##
        
        {  
          measurementsDF <- NULL
          
          
        }
        
        #### iterate over all the events in the chart data frame ####
        
        l=1
        for (l in 1:length(eventLabels)) {
          
          {
            
            ## slice the time series data frame for the stimulus segment ##
            
            segment <- Labels[l]
            segmentName <- eventLabels[l]
            
            assign("segment", segment, envir = .GlobalEnv)
            assign("segmentName", segmentName, envir = .GlobalEnv)
            
            if(showNames==TRUE) print(paste("event:", segmentName))
            
            # July 24, 2023 to
            # if(segmentName %in% excludeEvents) {
            #   next()
            # }
            
            # get the onset row using the segment name so that events during the prestimSegment are ignored
            segOnsetRow <- as.numeric(which(chartDF$eventLabel==segmentName)[1])
            assign("segOnsetRow", segOnsetRow, envir = .GlobalEnv)
            # stimSegOffset and answer are initialized a little bit later
            
            # View(chartDF)
            
            # get the segment prestim row
            prestimRow <- segOnsetRow - (prestimSeg*cps)
            if(prestimRow < 1) prestimRow <- 1
            
            # get the segment start row  in the segment DF
            startRow <- prestimRow
            if(startRow < 1) startRow <- 1
            
            # set the end row using the evaluation window length 
            segEndRow <- segOnsetRow + measuredSeg*cps - 1
            if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF) - 1
            
            # get the segment end row
            endRow <- segEndRow + addSeg*cps
            if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
            
            # get the segment data frame for this event 
            segmentDF <- chartDF[startRow:endRow,]
            # segmentDF will including 10 prestimulus seconds and 10 poststimulus seconds
            # View(segmentDF)
            
            # get the number of rows in the data segment
            segRows <- as.numeric(nrow(segmentDF))
            
            assign("segmentDF", segmentDF, pos=1)
            assign("segmentName", segmentName, pos=1)
            assign("segRows", segRows, pos=1)
            
            # View(segmentDF)
            
            # if(chartName == "01A" && segmentName == "R7") {
            #   assign("l", l, envir=.GlobalEnv)
            #   assign("startRow", startRow, envir=.GlobalEnv)
            #   assign("endRow", endRow, envir=.GlobalEnv)
            #   stop()
            # }
            
          }
          
          #### event indices - relative to the segmentDF not the chartDF ####
          
          {
            
            # index the event rows to the segmentDF (not the chartDF)
            
            # if(segmentName == "R10") stop()
            
            # prestimRow <- prestimRow - startRow + 1 # this will set the prestim row to 1
            segPrestimRow <- 1
            # always 1 for the segment DF
            # different sensors may have different prestim segments
            
            # get the stimulus onset relative to the segmentDF
            # will normally set to 301
            # need to use the event index in case the prestim segment is too short
            # such as when the examiner rushes the first question or rushes the question pace
            stimOnsetRow <- as.numeric(which(segmentDF$eventLabel==segmentName)[1])
            
            # need to ensure these are rounded to integers to avoid potential indexing errors
            PneumoLatencyRow <- stimOnsetRow + round((PneumoLat*cps)) - 1
            EDALatencyRow <- stimOnsetRow + round((EDALat*cps)) - 1
            CardioLatencyRow <- stimOnsetRow + round((CardioLat*cps)) - 1
            PLELatencyRow <- stimOnsetRow + round((PLELat*cps)) - 1
            
            # get the first stimulus offset and answer after the onset of the stimulus
            # May 30 2025 fixed this
            offsetRow <- which(segmentDF$Events[stimOnsetRow:(stimOnsetRow+measuredSeg*cps-1)]=="offsetRow")[1] + stimOnsetRow - 1
            # offsetRow is the end of the stimulus question 
            
            if(is.na(offsetRow)) offsetRow <- nrow(segmentDF) - 4
            if(offsetRow >= (nrow(segmentDF)-4)) offsetRow <- nrow(segmentDF) - 3
            
            # get the first answer after the stimulus onset
            # fixed May 30, 2025 to correctly slice the end of the stimulus segment
            answerRow <- which(segmentDF$Events[stimOnsetRow:(stimOnsetRow+measuredSeg*cps-1)]=="answerRow")[1] + stimOnsetRow - 1
            if(is.na(answerRow)) answerRow <- offsetRow + 1
            # if(is.na(answerRow)) answerRow=WOEEndRow - 1 + segOnsetRow - 1
            
            # set answer to "" if it is beyond the current segment + 5 seconds
            # it is likely to be the answer for the next stimulus 
            # because there is no answer to this segment
            if(answerRow > (offsetRow + 5*cps)) answerRow <- offsetRow + 1
            # Aug 5 2023 treat this as a missing answer instead
            
            # end of the ROW
            ROWEndRow <- answerRow + ((5*cps)-1)
            if(ROWEndRow > (nrow(segmentDF)-2)) ROWEndRow <- nrow(segmentDF) - 2
          
            # end of the WOE
            # WOEEndRow <- segEndRow - startRow + 1 # will normally set to 750
            WOEEndRow <- stimOnsetRow + (measuredSeg*cps - 1) # normally 750 if EW is 15 sec
            # adjust the rows so that row numbers refer to data in the segmentDF not the chartDF
            
            # if the WOE exceeds the segmentDF
            if(WOEEndRow > (nrow(segmentDF)-1)) WOEEndRow <- nrow(segmentDF) - 1
            
            recoveryRow <- stimOnsetRow + (17.5 * cps)
            if(recoveryRow >= (nrow(segmentDF)-3)) recoveryRow <- nrow(segmentDF) - 3
            
            postStimRow <- stimOnsetRow + (20 * cps)
            if(postStimRow >= (nrow(segmentDF)-2)) postStimRow <- nrow(segmentDF) - 2
            
            # fix problems 
            
            if(offsetRow <= stimOnsetRow) offsetRow <- stimOnsetRow + 1
            if(answerRow <= offsetRow) answerRow <- offsetRow + 1
            
            # if(showNames==TRUE) print(segmentName)
            
            # get the stimulus onset row for the segment 
            getRow <- stimOnsetRow
            # measurement information will be taken from this row
            
            ##  set the warn level to suppress warnings  ##
            
            oldw <- getOption("warn")
            options(warn = -1)
            
          }
        
          #### get the measurements using the segment DF, and event indices using the chartDF ####
          
          {
            
            #### pneumo measurements using the segementDF ####
            
            if(!PCATFormat) {
              
              ## measurements ##
              
              # get the measurements if numeric otherwise keep the text value
              P2 <- ifelse(is.na(as.numeric(segmentDF$UPneumoMeasure[getRow])),
                           as.numeric(segmentDF$UPneumoMeasure[getRow]),
                           as.numeric(segmentDF$UPneumoMeasure[getRow]) )
              names(P2) <- "UPneumo"
              P1 <- ifelse(is.na(as.numeric(segmentDF$LPneumoMeasure[getRow])),
                           as.numeric(segmentDF$LPneumoMeasure[getRow]),
                           as.numeric(segmentDF$LPneumoMeasure[getRow]) )
              names(P1) <- "LPneumo"
              
              ## response indices are obtained for the chartDF not the segmentDF ##
              
              # July 22, 2023
              # P2 is thoracic respiration
              P2PreStimX <- (segOnsetRow - stimOnsetRow) + 2 # 10 sec prior to question onset 
              if(P2PreStimX < 1) P2PreStimX <- 1
              
              P2ResponseOnsetX <- segOnsetRow  # question onset, normally 301
              P2RepsonseOffsetX <- segOnsetRow + offsetRow - 1 - stimOnsetRow 
              P2AnswerX <- segOnsetRow + answerRow - 1 - stimOnsetRow
              P2LatencyX <- segOnsetRow + PneumoLatencyRow - 1 - stimOnsetRow
              P2ResponseEndX <- segOnsetRow + (measuredSeg*cps)  - 1 # end of 15 second WOE, normally 750
              P2ResponseRecX <- segOnsetRow + (recoveryRow-stimOnsetRow) - 1 # 17.5 seconds
              P2PostStimX <- segOnsetRow + (postStimRow-stimOnsetRow) - 1 # 20 seconds after question onset - 1, normally 899
              
              # P1 is abdominal respiration
              P1PreStimX <- segOnsetRow - stimOnsetRow + 2
              if(P1PreStimX < 1) P1PreStimX <- 1
              
              P1ResponseOnsetX <- segOnsetRow
              P1RepsonseOffsetX <- segOnsetRow + offsetRow - 1 - stimOnsetRow 
              P1AnswerX <- segOnsetRow + answerRow - 1 - stimOnsetRow
              P1LatencyX <- segOnsetRow + PneumoLatencyRow - 1 - stimOnsetRow
              P1ResponseEndX <- segOnsetRow + (measuredSeg*cps)  - 1 
              P1ResponseRecX <- segOnsetRow + (recoveryRow-stimOnsetRow) - 1
              P1PostStimX <- segOnsetRow + (postStimRow-stimOnsetRow) - 1
              
              # initialize a vector to combine the upper and lower pneumo scores 
              Pneumo <- NA
              names(Pneumo) <- "Pneumo"
              
              # data values at that feature extraction indices
              P2PrestimVal <- as.numeric(segmentDF$c_UPneumoSm[stimOnsetRow])
              P2BeginVal <- as.numeric(segmentDF$c_UPneumoSm[getRow]) # getRow is the same as stimOnsetRow
              P2EndVal <- as.numeric(segmentDF$c_UPneumoSm[WOEEndRow]) 
              P2AnswerVal <- as.numeric(segmentDF$c_UPneumoSm[answerRow])
              P2LatencyVal <- as.numeric(segmentDF$c_UPneumoSm[PneumoLatencyRow])
              P2OnsetVal <- as.numeric(segmentDF$c_UPneumoSm[stimOnsetRow]) # stimOnsetRow is the same as getRow
              P2OffsetVal <- as.numeric(segmentDF$c_UPneumoSm[offsetRow])
              P2RowEndVal <- as.numeric(segmentDF$c_UPneumoSm[ROWEndRow])
              P2WoeEndVal <- as.numeric(segmentDF$c_UPneumoSm[WOEEndRow])
              P2RecoveryRowVal <- as.numeric(segmentDF$c_UPneumoSm[recoveryRow])
              P2PostStimVal <- as.numeric(segmentDF$c_UPneumoSm[postStimRow])
              
              P1PrestimVal <- as.numeric(segmentDF$c_UPneumoSm[stimOnsetRow])
              P1BeginVal <- as.numeric(segmentDF$c_LPneumoSm[getRow])
              P1EndVal <- as.numeric(segmentDF$c_LPneumoSm[WOEEndRow])
              P1AnswerVal <- as.numeric(segmentDF$c_LPneumoSm[answerRow])
              P1LatencyVal <- as.numeric(segmentDF$c_LPneumoSm[PneumoLatencyRow])
              P1OnsetVal <- as.numeric(segmentDF$c_LPneumoSm[stimOnsetRow])
              P1OffsetVal <- as.numeric(segmentDF$c_LPneumoSm[offsetRow])
              P1RowEndVal <- as.numeric(segmentDF$c_LPneumoSm[ROWEndRow])
              P1WoeEndVal <- as.numeric(segmentDF$c_LPneumoSm[WOEEndRow])
              P1RecoveryRowVal <- as.numeric(segmentDF$c_UPneumoSm[recoveryRow])
              P1PostStimVal <- as.numeric(segmentDF$c_UPneumoSm[postStimRow])
              
            } else {
              
              P1 <- NA
              P2 <- NA
              Pneumo <- NA
              names(Pneumo) <- "Pneumo"
              
              P2PreStimX <- NA
              P2ResponseOnsetX <- NA
              P2ResponseOffsetX <- NA
              P2AnswerX <- NA
              P2LatencyX <- NA
              P2ResponseEndX <- NA
              P2ResponseRecX <- ""
              P2PostStimX <- NA
              
              P1PreStimX <- NA
              P1ResponseOnsetX <- NA
              P1ResponseOffsetX <- NA
              P1AnswerX <- NA
              P1LatencyX <- NA
              P1ResponseEndX <- NA
              P1ResponseRecX <- ""
              P1PostStimX <- NA
              
              P2PrestimVal <- NA
              P2BeginVal <- NA
              P2EndVal <- NA
              P2AnswerVal <- NA
              P2LatencyVal <-  NA
              P2OnsetVal <- NA
              P2OffsetVal <- NA
              P2RowEndVal <- NA
              P2WoeEndVal <- NA
              P2RecoveryRowVal <- NA
              P2PostStimVal <- NA
              
              P1PrestimVal <- NA
              P1BeginVal <- NA
              P1EndVal <- NA
              P1AnswerVal <- NA
              P1LatencyVal <- NA
              P1OnsetVal <- NA
              P1OffsetVal <- NA
              P1RowEndVal <- NA
              P1WoeEndVal <- NA
              P1RecoveryRowVal <- NA
              P1PostStimVal <- NA
              
            }
            
            #### EDA measurements ####
            
            {
              
              # EDA <- as.numeric(segmentDF$AutoEDAMeasure[which(as.numeric(segmentDF$AutoEDAMeasure)>0)][1])
              # EDA <- segmentDF$AutoEDAMeasure[as.numeric(segmentDF$AutoEDAMeasure)>0][1]
              # changed 1-17-2017 to prevent extraction of the subsequent measurement 
              
              ## EDA measurements using the segment DF ##
              
              { 
                # getRow is the same as the stimOnsetRow in the segmentDF
                
                AutoEDA <- segmentDF$AutoEDAMeasure[getRow]
                names(AutoEDA) <- "AutoEDA"
                AutoEDADuration <- segmentDF$AutoEDADuration[getRow]
                names(AutoEDADuration) <- "AutoEDADuration"
                AutoEDAComplexity <- segmentDF$AutoEDAComplexity[getRow]
                names(AutoEDAComplexity) <- "AutoEDAComplexity"
                
                ManualEDA <- segmentDF$ManualEDAMeasure[getRow]
                names(ManualEDA) <- "ManualEDA"
                ManualEDADuration <- segmentDF$ManualEDADuration[getRow]
                names(ManualEDADuration) <- "ManualEDADuration"
                ManualEDAComplexity <- segmentDF$ManualEDAComplexity[getRow]
                names(ManualEDAComplexity) <- "ManualEDAComplexity"
                
              }
              
              ## EDA response indices are computed using the chartDF ##
              
              {
                
                # July 22, 2023 get the EDA response onset and response end indices
                # use [1] to slice only the first value to avoid problems when examiners rush the question pacing
                prestimXAutoEDA <- segOnsetRow - (5 * cps) + 1 # 5 sec before question onset
                if(prestimXAutoEDA < 1) prestimXAutoEDA <- 1
                
                responseRecXAutoEDA <- segOnsetRow + (recoveryRow-stimOnsetRow) - 1
                poststimXAutoEDA <- segOnsetRow + (postStimRow-stimOnsetRow) - 1
                
                # Feb 6, 2025 need to restrict this search to the ROW or WOE
                # May 30, 2025 changed from segOnsetRow to stimOnsetRow to work correctly with the segmentDF
                responseOnsetXAutoEDA <- which(segmentDF$AutoEDAExtract[stimOnsetRow:ROWEndRow] == "responseOnsetRow")[1]  + stimOnsetRow - 1
                responsePeakXAutoEDA <- which(segmentDF$AutoEDAExtract[stimOnsetRow:WOEEndRow] == "responseEndRow")[1] + stimOnsetRow - 1
                
              }
              
              {
                
                prestimXManualEDA <- segOnsetRow - (5 * cps) + 1
                if(prestimXManualEDA < 1) prestimXManualEDA <- 1
                
                responseRecXManualEDA <- segOnsetRow + (recoveryRow-stimOnsetRow) - 1
                poststimXManualEDA <- segOnsetRow + (postStimRow-stimOnsetRow) - 1
                
                # Feb 6, 2025 need to restrict this search to the ROW or WOE 
                responseOnsetXManualEDA <- which(segmentDF$ManualEDAExtract[stimOnsetRow:ROWEndRow] == "responseOnsetRow")[1] + segOnsetRow - 1
                responsePeakXManualEDA <- which(segmentDF$ManualEDAExtract[stimOnsetRow:WOEEndRow] == "responseEndRow")[1] + segOnsetRow - 1
                
              }
              
              {
                
                if(length(responseOnsetXAutoEDA)==0) responseOnsetXAutoEDA <- NA
                if(length(responsePeakXAutoEDA)==0) responsePeakXAutoEDA <- NA
                
                # plot.ts(segmentDF$c_AutoEDA)
                
                # data values at the feature extraction indices
                AutoEDAPreStimVal <- as.numeric(segmentDF$c_AutoEDA[stimOnsetRow])
                AutoEDABeginVal <- as.numeric(segmentDF$c_AutoEDA[getRow])
                AutoEDAEndVal <- as.numeric(segmentDF$c_AutoEDA[offsetRow])
                AutoEDAAnswerVal <- as.numeric(segmentDF$c_AutoEDA[answerRow])
                AutoEDALatencyVal <- as.numeric(segmentDF$c_AutoEDA[EDALatencyRow])
                if(!is.na(responseOnsetXAutoEDA)) {
                  # changed May 30, 2025
                  AutoEDAResponseOnsetVal <- as.numeric(segmentDF$c_AutoEDA[responseOnsetXAutoEDA])
                  AutoEDAResponsePeakVal <- as.numeric(segmentDF$c_AutoEDA[responsePeakXAutoEDA])
                  # AutoEDAResponseOnsetVal <- as.numeric(segmentDF$c_AutoEDA[(responseOnsetXAutoEDA-segOnsetRow)+1])
                  # AutoEDAResponsePeakVal <- as.numeric(segmentDF$c_AutoEDA[(responsePeakXAutoEDA-segOnsetRow)+1])
                } else {
                  AutoEDAResponseOnsetVal <- NA
                  AutoEDAResponsePeakVal <- NA
                }
                AutoEDARowEndVal <- as.numeric(segmentDF$c_AutoEDA[ROWEndRow])
                AutoEDAWoeEndVal <- as.numeric(segmentDF$c_AutoEDA[WOEEndRow])
                AutoEDARecoveryRowVal <- as.numeric(segmentDF$c_AutoEDA[recoveryRow])
                AutoEDAPostStimVal <- as.numeric(segmentDF$c_AutoEDA[postStimRow])
                
              }
              
              {
                
                if(length(responseOnsetXManualEDA)==0) responseOnsetXManualEDA <- NA
                if(length(responsePeakXManualEDA)==0) responsePeakXManualEDA <- NA
                
                ManualEDAPreStimVal <- as.numeric(segmentDF$c_ManualEDA[stimOnsetRow])
                ManualEDABeginVal <- as.numeric(segmentDF$c_ManualEDA[getRow])
                ManualEDAEndVal <- as.numeric(segmentDF$c_ManualEDA[offsetRow])
                ManualEDAAnswerVal <- as.numeric(segmentDF$c_ManualEDA[answerRow])
                ManualEDALatencyVal <- as.numeric(segmentDF$c_ManualEDA[EDALatencyRow])
                if(!is.na(responseOnsetXManualEDA)) {
                  # changed May 30, 2025
                  ManualEDAResponseOnsetVal <- as.numeric(segmentDF$c_ManualEDA[responseOnsetXManualEDA])
                  ManualEDAResponsePeakVal <- as.numeric(segmentDF$c_ManualEDA[responseOnsetXManualEDA])
                } else {
                  ManualEDAResponseOnsetVal <- NA
                  ManualEDAResponsePeakVal <- NA
                }
                ManualEDARowEndVal <- as.numeric(segmentDF$c_ManualEDA[ROWEndRow])
                ManualEDAWoeEndVal <- as.numeric(segmentDF$c_ManualEDA[WOEEndRow])
                ManualEDARecoveryRowVal <- as.numeric(segmentDF$c_ManualEDA[recoveryRow])
                ManualEDAPostStimVal <- as.numeric(segmentDF$c_ManualEDA[postStimRow])
                
              }
              
            }
            
            #### Cardio measurement ####
            
            if(!PCATFormat) {
              
              ## Cardio measurements ##
              
              # Cardio <- as.numeric(segmentDF$CardioMeasure[which(as.numeric(segmentDF$CardioMeasure)>0)][1])
              # Cardio <- segmentDF$CardioMeasure[as.numeric(segmentDF$CardioMeasure)>0][1]
              # changed 1-17-2017 to prevent extraction of the subsequent measurement 
              
              Cardio <- segmentDF$CardioMeasure[getRow]
              names(Cardio) <- "Cardio"
              CardioDuration <- segmentDF$CardioDuration[getRow]
              names(CardioDuration) <- "CardioDuration"
              CardioComplexity <- segmentDF$CardioComplexity[getRow]
              names(CardioComplexity) <- "CardioComplexity"
              CardioRate <- segmentDF$CardioRate[getRow]
              names(CardioRate) <- "CardioRate"
              
              ## Cardio response indices ##
              
              # July 22, 2023 get the Cardio response onset and response end indices
              # use [1] to slide the first item in case the examiner rushed the question interval
              prestimXCardio <- segOnsetRow - (5 * cps) + 1 # 5 sec before question onset
              if(prestimXCardio < 1) prestimXCardio <- 1
              
              # Feb 6 2025 need to restrict this search the ROW and WOE
              responseOnsetXCardio <- which(segmentDF$CardioExtract[stimOnsetRow:ROWEndRow] == "responseOnsetRow")[1] + segOnsetRow - 1
              responsePeakXCardio <- which(segmentDF$CardioExtract[stimOnsetRow:WOEEndRow] == "responseEndRow")[1] + segOnsetRow - 1
              
              responseRecXCardio <- segOnsetRow + (recoveryRow-stimOnsetRow) - 1
              poststimXCardio <- segOnsetRow + (postStimRow-stimOnsetRow) - 1
              
              if(length(responseOnsetXCardio)==0) responseOnsetXCardio <- NA
              if(length(responsePeakXCardio)==0) responsePeakXCardio <- NA
              
              # data values at the feature extraction indices
              CardioPreStimVal <- as.numeric(segmentDF$c_Cardio1[stimOnsetRow])
              CardioBeginVal <- as.numeric(segmentDF$c_Cardio1[getRow])
              CardioEndVal <- as.numeric(segmentDF$c_Cardio1[offsetRow])
              CardioAnswerVal <- as.numeric(segmentDF$c_Cardio1[answerRow])
              CardioLatencyVal <- as.numeric(segmentDF$c_Cardio1[CardioLatencyRow])
              if(!is.na(responseOnsetXCardio)) {
                # changed May 30, 2025
                CardioResponseOnsetVal <- as.numeric(segmentDF$c_Cardio1[responseOnsetXCardio])
                CardioResponsePeakVal <- as.numeric(segmentDF$c_Cardio1[responseOnsetXCardio])
              } else {
                CardioResponseOnsetVal <- NA
                CardioResponsePeakVal <- NA
              }
              CardioRowEndVal <- as.numeric(segmentDF$c_Cardio1[ROWEndRow])
              CardioWoeEndVal <- as.numeric(segmentDF$c_Cardio1[WOEEndRow])
              CardioRecoveryRowVal <- as.numeric(segmentDF$c_Cardio1[recoveryRow])
              CardioPostStimVal <- as.numeric(segmentDF$c_Cardio1[postStimRow])
              
            } else {
              
              Cardio <- NA
              
              prestimXCardio <- NA
              responseOnsetXCardio <- NA
              responsePeakXCardio <- NA
              responseRecXCardio <- NA
              poststimXCardio <- NA
              
              CardioDuration <- NA
              CardioComplexity <- NA
              CardioRate <- NA
              
              CardioPreStimVal <- NA
              CardioBeginVal <- NA
              CardioEndVal <- NA
              CardioAnswerVal <- NA
              CardioLatencyVal <- NA
              CardioResponseOnsetVal <- NA
              CardioResponsePeakVal <- NA
              CardioRowEndVal <- NA
              CardioWoeEndVal <- NA
              CardioRecoveryRowVal <- NA
              CardioPostStimVal <- NA
              
            }
            
            #### forearm cuff / finger cuff measurement ####
            
            if( sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0)) != 0 ) {
              
              ## FC measurements ## 
              
              # FC <- as.numeric(segmentDF$FCMeasure[which(as.numeric(segmentDF$FCMeasure)>0)][1])
              
              # FC <- segmentDF$FCMeasure[as.numeric(segmentDF$FCMeasure)>0][1]
              # changed 1-17-2017 to prevent extraction of the subsequent measurement 
              FC <- segmentDF$FCMeasure[getRow]
              names(FC) <- "FC" # if(length(FC) > 0) names(FC) <- "FC"
              FCDuration <- segmentDF$FCDuration[getRow]
              names(FCDuration) <- "FCDuration"
              FCComplexity <- segmentDF$FCComplexity[getRow]
              names(FCComplexity) <- "FCComplexity"
              FCRate <- segmentDF$FCRate[getRow]
              names(FCRate) <- "FCRate"
              
              ## FC response indices ##
              
              # July 22, 2023 get the Cardio response onset and response end indices
              prestimXFC <- segOnsetRow - (5 * cps) + 1
              if(prestimXFC < 1) prestimXFC <- 1
              
              # Feb 6, 2025 need to restrict this search to the ROW and WOE
              responseOnsetXFC <- which(segmentDF$FCExtract[stimOnsetRow:ROWEndRow] == "responseOnsetRow")[1] + segOnsetRow - 1
              responsePeakXFC <- which(segmentDF$FCExtract[stimOnsetRow:WOEEndRow] == "responseEndRow")[1] + segOnsetRow - 1
              
              responseRecXFC <- segOnsetRow + (recoveryRow-stimOnsetRow) - 1
              poststimXFC <- segOnsetRow + (postStimRow-stimOnsetRow) - 1
              
              if(length(responseOnsetXFC)==0) responseOnsetXFC<- NA
              if(length(responsePeakXFC)==0) responseEndXFC <- NA
              
              # data values at the feature extraction indices
              FCPreStimVal <- as.numeric(segmentDF$c_FC[stimOnsetRow])
              FCBeginVal <- as.numeric(segmentDF$c_FC[getRow])
              FCEndVal <- as.numeric(segmentDF$c_FC[offsetRow])
              FCAnswerVal <- as.numeric(segmentDF$c_FC[answerRow])
              FCLatencyVal <- as.numeric(segmentDF$c_FC[CardioLatencyRow])
              if(!is.na(responseOnsetXFC)) {
                FCResponseOnsetVal <- as.numeric(segmentDF$c_FC[responseOnsetXFC])
                FCResponsePeakVal <- as.numeric(segmentDF$c_FC[responsePeakXFC])
              } else {
                FCResponseOnsetVal <- NA
                FCResponsePeakVal <- NA
              }
              FCRowEndVal <- as.numeric(segmentDF$c_FC[ROWEndRow])
              FCWoeEndVal <- as.numeric(segmentDF$c_FC[WOEEndRow])
              FCRecoveryRowVal <- as.numeric(segmentDF$c_FC[recoveryRow])
              FCPostStimVal <- as.numeric(segmentDF$c_FC[postStimRow])
            }
            
            #### PLE measurement ####
            
            if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
              
              ## PLE measurements ##
              
              # PLE <- as.numeric(segmentDF$PPG1Measure[which(as.numeric(segmentDF$PPG1Measure)>0)][1])
              # PLE <- segmentDF$PPG1Measure[as.numeric(segmentDF$PPG1Measure)>0][1]
              # changed 1-17-2017 to prevent extraction of the subsequent measurement 
              PLE <- segmentDF$PPG1Measure[getRow]
              if(length(PLE) > 0) names(PLE) <- "PLE"
              
              ## PLE response indices ##
              
              # July 22, 2023
              prestimXPLE <- segOnsetRow - (3 * cps) + 1 # 3 seconds before question onset
              if(prestimXPLE < 1) prestimXPLE <- 1
              
              responseOnsetXPLE <- segOnsetRow + (5 * cps) - 1
              responseEndXPLE <- segOnsetRow + (10 * cps) - 1
              
              responseRecXPLE <- segOnsetRow + (recoveryRow-stimOnsetRow) - 1
              poststimXPLE <- segOnsetRow + (postStimRow-stimOnsetRow) - 1
              
              if(length(responseOnsetXPLE)==0) responseOnsetXPLE <- NA
              if(length(responseEndXPLE)==0) responseEndXPLE <- NA
              
              # data values at the feature extraction indices
              PLEPreStimVal <- as.numeric(segmentDF$c_PPG1[stimOnsetRow])
              PLEBeginVal <- as.numeric(segmentDF$c_PPG1[getRow])
              PLEEndVal <- as.numeric(segmentDF$c_PPG1[offsetRow])
              PLEAnswerVal <- as.numeric(segmentDF$c_PPG1[answerRow])
              PLELatencyVal <- as.numeric(segmentDF$c_PPG1[PLELatencyRow])
              if(!is.na(responseOnsetXPLE)) {
                PLEResponseOnsetVal <- as.numeric(segmentDF$c_PPG1[responseOnsetXPLE])
                PLEResponseEndVal <- as.numeric(segmentDF$c_PPG1[responseEndXPLE])
              } else {
                PLEResponseOnsetVal <- NA
                PLEResponseEndVal <- NA
              }
              PLERowEndVal <- as.numeric(segmentDF$c_PPG1[ROWEndRow])
              PLEWoeEndVal <- as.numeric(segmentDF$c_PPG1[WOEEndRow])
              PLERecoveryRowVal <- as.numeric(segmentDF$c_PPG1[recoveryRow])
              PLEPostStimVal <- as.numeric(segmentDF$c_PPG1[postStimRow])
              
            }
            
          }
          
          #### aggregate the sensor measurements to a column vector ####
          
          # measurements are submitted to the sensorMeasurement column
          
          measurementsVc <- c(P2, 
                            P1, 
                            Pneumo, 
                            AutoEDA, 
                            AutoEDADuration, 
                            AutoEDAComplexity,
                            ManualEDA,
                            ManualEDADuration,
                            ManualEDAComplexity,
                            Cardio,
                            CardioDuration,
                            CardioComplexity,
                            CardioRate,
                            if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                              c(FC, FCDuration, FCComplexity, FCRate)
                            }, 
                            # eCardio, 
                            if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                              PLE
                            } )
          
          #### initialize column vectors for the response onset index and other indices ####
          
          {
            
            # response indices, prestim, onset, end, recovery, and poststim
            # are submitted to different columns 
            # and must the be the same length as the "measurementsVc" vector above
            
            # added July 23, 2023
            
            responseOnsetIndices <- c(P2Idx=P2ResponseOnsetX,
                                      P1Idx=P1ResponseOnsetX,
                                      PneumoIdx="",
                                      AutoEDAIdx=responseOnsetXAutoEDA,
                                      AutoEDADurIdx="",
                                      AutoEDACmplxIdx="",
                                      ManualEDAIdx=responseOnsetXManualEDA,
                                      XManualEDADurIdx="",
                                      ManualEDACmplxIdx="",
                                      CardioIdx=responseOnsetXCardio,
                                      CardioDurIdx="",
                                      CardioCmplxIdx="",
                                      CardioRateIdx="",
                                      if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                        c(FCIdx=responseOnsetXFC, 
                                          FCDurIdx="", 
                                          FCCmplxIdx="",
                                          FCRateIdx="")
                                      },
                                      # eCardio,
                                      if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                        PLEIdx=responseOnsetXPLE 
                                      }
            )
            
            
            responseEndIndices <- c(P2Idx=P2ResponseEndX,
                                    P1Idx=P1ResponseEndX,
                                    PneumoIdx="",
                                    AutoEDAIdx=responsePeakXAutoEDA,
                                    AutoEDADurIdx="",
                                    AutoEDACmplxIdx="",
                                    ManualEDAIdx=responsePeakXManualEDA,
                                    XManualEDADurIdx="",
                                    ManualEDACmplxIdx="",
                                    CardioIdx=responsePeakXCardio,
                                    CardioDurIdx="",
                                    CardioCmplxIdx="",
                                    CardioRateIdx="",
                                    if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                      c(FCIdx=responseEndXFC, 
                                        FCDurIdx="", 
                                        FCCmplxIdx="",
                                        FCRateIdx="")
                                    },
                                    # eCardio
                                    if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                      PLEIdx=responseEndXPLE 
                                    }
            )
            
            if(!(length(responseOnsetIndices) == length(responseEndIndices))) stop()
            
            prestimIndices <- c(P2Idx=P2PreStimX,
                                P1Idx=P1PreStimX,
                                PneumoIdx="",
                                AutoEDAIdx=prestimXAutoEDA,
                                AutoEDADurIdx="",
                                AutoEDACmplxIdx="",
                                ManualEDAIdx=prestimXManualEDA,
                                XManualEDADurIdx="",
                                ManualEDACmplxIdx="",
                                CardioIdx=prestimXCardio,
                                CardioDurIdx="",
                                CardioCmplxIdx="",
                                CardioRateIdx="",
                                if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                  c(FCIdx=prestimXFC, 
                                    FCDurIdx="", 
                                    FCCmplxIdx="",
                                    FCRateIdx="")
                                },
                                # eCardio
                                if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                  PLEIdx=prestimXPLE 
                                }
            )
            
            # correct for short segments - with insufficient prestim length
            if(any(prestimIndices < 1)) {
              prestimIndices[which(prestimIndices < 1)] <- 1
            }
            
            latencyEndIndices <- c(P2Idx=segOnsetRow,
                                   P1Idx=segOnsetRow,
                                   PneumoIdx="",
                                   AutoEDAIdx=round(EDALat*cps) + segOnsetRow - 1,
                                   AutoEDADurIdx="",
                                   AutoEDACmplxIdx="",
                                   ManualEDAIdx=round(EDALat*cps) + segOnsetRow - 1,
                                   XManualEDADurIdx="",
                                   ManualEDACmplxIdx="",
                                   CardioIdx=round(CardioLat*cps) + segOnsetRow - 1,
                                   CardioDurIdx="",
                                   CardioCmplxIdx="",
                                   CardioRateIdx="",
                                   if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                     c(FCIdx=round(CardioLat*cps) + segOnsetRow - 1, 
                                       FCDurIdx="", 
                                       FCCmplxIdx="",
                                       FCRateIdx="")
                                   },
                                   # eCardio
                                   if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                     PLEIdx=round(PLELat*cps) + segOnsetRow - 1
                                   }
            ) 
            
            ## already have the stimulus Begin, End, and Answer ##
            
            # correct for short segments - with insufficient length
            if(any(as.numeric(latencyEndIndices) >= nrow(chartDF), na.rm=TRUE)) {
              latencyEndIndices[which(latencyEndIndices >= nrow(chartDF))] <- nrow(chartDF) - 1
            }
            
            ROWEndIndices <- c(P2Idx=round(ROWEnd*cps) + answerRow + segOnsetRow - 1,
                               P1Idx=round(ROWEnd*cps) + answerRow + segOnsetRow - 1,
                               PneumoIdx="",
                               AutoEDAIdx=round(ROWEnd*cps) + answerRow + segOnsetRow - 1,
                               AutoEDADurIdx="",
                               AutoEDACmplxIdx="",
                               ManualEDAIdx=round(ROWEnd*cps) + answerRow + segOnsetRow - 1,
                               XManualEDADurIdx="",
                               ManualEDACmplxIdx="",
                               CardioIdx=round(ROWEnd*cps) + answerRow + segOnsetRow - 1,
                               CardioDurIdx="",
                               CardioCmplxIdx="",
                               CardioRateIdx="",
                               if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                 c(FCIdx=round(ROWEnd*cps) + answerRow + segOnsetRow - 1, 
                                   FCDurIdx="", 
                                   FCCmplxIdx="",
                                   FCRateIdx="")
                               },
                               # eCardio
                               if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                 PLEIdx=round(ROWEnd*cps) + answerRow + segOnsetRow - 1
                               }
            ) 
            
            # correct for short segments - with insufficient length
            if(any(as.numeric(ROWEndIndices) >= nrow(chartDF), na.rm=TRUE)) {
              ROWEndIndices[which(ROWEndIndices >= nrow(chartDF))] <- nrow(chartDF) - 1
            }
              
            WOEEndIndices <- c(P2Idx=round(measuredSeg*cps) + segOnsetRow - 1,
                                P1Idx=round(measuredSeg*cps) + segOnsetRow - 1,
                                PneumoIdx="",
                                AutoEDAIdx=round(measuredSeg*cps) + segOnsetRow - 1,
                                AutoEDADurIdx="",
                                AutoEDACmplxIdx="",
                                ManualEDAIdx=round(measuredSeg*cps) + segOnsetRow - 1,
                                XManualEDADurIdx="",
                                ManualEDACmplxIdx="",
                                CardioIdx=round(measuredSeg*cps) + segOnsetRow - 1,
                                CardioDurIdx="",
                                CardioCmplxIdx="",
                                CardioRateIdx="",
                                if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                  c(FCIdx=round(measuredSeg*cps) + segOnsetRow - 1, 
                                    FCDurIdx="", 
                                    FCCmplxIdx="",
                                    FCRateIdx="")
                                },
                                # eCardio
                                if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                  PLEIdx=round(measuredSeg*cps) + segOnsetRow - 1
                                }
            ) 
            
            # correct for short segments - with insufficient length
            if(any(as.numeric(WOEEndIndices) >= nrow(chartDF), na.rm=TRUE)) {
              WOEEndIndices[which(WOEEndIndices >= nrow(chartDF))] <- nrow(chartDF) - 1
            }
            
            responseRecoveryIndices <- c(P2Idx=P2ResponseRecX, # 17.5 seconds
                                         P1Idx=P1ResponseRecX,
                                         PneumoIdx="",
                                         AutoEDAIdx=responseRecXAutoEDA,
                                         AutoEDADurIdx="",
                                         AutoEDACmplxIdx="",
                                         ManualEDAIdx=responseRecXManualEDA,
                                         XManualEDADurIdx="",
                                         ManualEDACmplxIdx="",
                                         CardioIdx=responseRecXCardio,
                                         CardioDurIdx="",
                                         CardioCmplxIdx="",
                                         CardioRateIdx="",
                                         if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                           c(FCIdx=responseRecXFC, 
                                             FCDurIdx="", 
                                             FCCmplxIdx="",
                                             FCRateIdx="")
                                         },
                                         # eCardio
                                         if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                           PLEIdx=responseRecXPLE 
                                         }
            )
            
            # correct for short segments - with insufficient length
            if(any(as.numeric(responseRecoveryIndices) >= nrow(chartDF), na.rm=TRUE)) {
              responseRecoveryIndices[which(responseRecoveryIndices >= nrow(chartDF))] <- nrow(chartDF) - 1
            }
            
            poststimIndices <- c(P2Idx=P2PostStimX,
                                 P1Idx=P1PostStimX,
                                 PneumoIdx="",
                                 AutoEDAIdx=poststimXAutoEDA,
                                 AutoEDADurIdx="",
                                 AutoEDACmplxIdx="",
                                 ManualEDAIdx=poststimXManualEDA,
                                 XManualEDADurIdx="",
                                 ManualEDACmplxIdx="",
                                 CardioIdx=poststimXCardio,
                                 CardioDurIdx="",
                                 CardioCmplxIdx="",
                                 CardioRateIdx="",
                                 if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                   c(FCIdx=poststimXFC, 
                                     FCDurIdx="", 
                                     FCCmplxIdx="",
                                     FCRateIdx="")
                                 },
                                 # eCardio
                                 if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                   PLEIdx=poststimXPLE 
                                 }
            )
            
            # correct for short segments - with insufficient length
            if(any(as.numeric(poststimIndices) >= nrow(chartDF), na.rm=TRUE)) {
              poststimIndices[which(poststimIndices >= nrow(chartDF))] <- nrow(chartDF) - 1
            }
            
          }
          
          #### initialize some column vectors for the response onset values and other response index values ####
          
          {
            
            ## new Jan 20, 2025 ##
            
            # prestimuls values 
            preStimValues <- c(P2Val=P2PrestimVal,
                               P1Val=P1PrestimVal,
                               PneumoVal="",
                               AutoEDAVal=AutoEDAPreStimVal,
                               AutoEDADurVal="",
                               AutoEDACmplxVal="",
                               ManualEDAVal=ManualEDAPreStimVal,
                               XManualEDADurVal="",
                               ManualEDACmplxVal="",
                               CardioVal=CardioPreStimVal,
                               CardioDurVal="",
                               CardioCmplxVal="",
                               CardioRateVal="",
                               if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                 c(FCVal=FCPreStimVal, 
                                   FCDurVal="", 
                                   FCCmplxVal="",
                                   FCRateVal="")
                               },
                               # eCardio
                               if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                 PLEVal=PLEPreStimVal 
                               }
            )
            
            # stimulus onset values
            beginValues <-  c(P2Val=P2BeginVal,
                              P1Val=P1BeginVal,
                              PneumoVal="",
                              AutoEDAVal=AutoEDABeginVal,
                              AutoEDADurVal="",
                              AutoEDACmplxVal="",
                              ManualEDAVal=ManualEDABeginVal,
                              XManualEDADurVal="",
                              ManualEDACmplxVal="",
                              CardioVal=CardioBeginVal,
                              CardioDurVal="",
                              CardioCmplxVal="",
                              CardioRateVal="",
                              if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                c(FCVal=FCBeginVal, 
                                  FCDurVal="", 
                                  FCCmplxVal="",
                                  FCRateVal="")
                              },
                              # eCardio
                              if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                PLEVal=PLEBeginVal 
                              }
            )
            
            # stimulus end/offset values
            endValues<-  c(P2Val=P2EndVal,
                           P1Val=P1EndVal,
                           PneumoVal="",
                           AutoEDAVal=AutoEDAEndVal,
                           AutoEDADurVal="",
                           AutoEDACmplxVal="",
                           ManualEDAVal=ManualEDAEndVal,
                           XManualEDADurVal="",
                           ManualEDACmplxVal="",
                           CardioVal=CardioEndVal,
                           CardioDurVal="",
                           CardioCmplxVal="",
                           CardioRateVal="",
                           if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                             c(FCVal=FCEndVal, 
                               FCDurVal="", 
                               FCCmplxVal="",
                               FCRateVal="")
                           },
                           # eCardio
                           if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                             PLEVal=PLEEndVal 
                           }
            )
            
            # data value at the point of verbal answer
            answerValues <- c(P2Val=P2AnswerVal,
                              P1Val=P1AnswerVal,
                              PneumoVal="",
                              AutoEDAVal=AutoEDAAnswerVal,
                              AutoEDADurVal="",
                              AutoEDACmplxVal="",
                              ManualEDAVal=ManualEDAAnswerVal,
                              XManualEDADurVal="",
                              ManualEDACmplxVal="",
                              CardioVal=CardioAnswerVal,
                              CardioDurVal="",
                              CardioCmplxVal="",
                              CardioRateVal="",
                              if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                c(FCVal=FCAnswerVal, 
                                  FCDurVal="", 
                                  FCCmplxVal="",
                                  FCRateVal="")
                              },
                              # eCardio
                              if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                PLEVal=PLEAnswerVal 
                              }
            )
            
            # latency values
            latValues <- c(P2Val=P2LatencyVal,
                           P1Val=P1LatencyVal,
                           PneumoVal="",
                           AutoEDAVal=AutoEDALatencyVal,
                           AutoEDADurVal="",
                           AutoEDACmplxVal="",
                           ManualEDAVal=ManualEDALatencyVal,
                           XManualEDADurVal="",
                           ManualEDACmplxVal="",
                           CardioVal=CardioLatencyVal,
                           CardioDurVal="",
                           CardioCmplxVal="",
                           CardioRateVal="",
                           if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                             c(FCVal=FCLatencyVal, 
                               FCDurVal="", 
                               FCCmplxVal="",
                               FCRateVal="")
                           },
                           # eCardio
                           if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                             PLEVal=PLELatencyVal 
                           }
            )
            
            # repsonse onset values
            responseOnsetValues <- c(P2Val=P2OnsetVal,
                                     P1Val=P1OnsetVal,
                                     PneumoVal="",
                                     AutoEDAVal=AutoEDAResponseOnsetVal,
                                     AutoEDADurVal="",
                                     AutoEDACmplxVal="",
                                     ManualEDAVal=ManualEDAResponseOnsetVal,
                                     XManualEDADurVal="",
                                     ManualEDACmplxVal="",
                                     CardioVal=CardioResponseOnsetVal,
                                     CardioDurVal="",
                                     CardioCmplxVal="",
                                     CardioRateVal="",
                                     if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                       c(FCVal=FCResponseOnsetVal, 
                                         FCDurVal="", 
                                         FCCmplxVal="",
                                         FCRateVal="")
                                     },
                                     # eCardio
                                     if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                       PLEVal=PLEResponseOnsetVal 
                                     }
            )
            
            # response peak/end values
            responsePeakValues <- c(P2Val=P2EndVal,
                                    P1Val=P1EndVal,
                                    PneumoVal="",
                                    AutoEDAVal=AutoEDAResponsePeakVal,
                                    AutoEDADurVal="",
                                    AutoEDACmplxVal="",
                                    ManualEDAVal=ManualEDAResponsePeakVal,
                                    XManualEDADurVal="",
                                    ManualEDACmplxVal="",
                                    CardioVal=CardioResponsePeakVal,
                                    CardioDurVal="",
                                    CardioCmplxVal="",
                                    CardioRateVal="",
                                    if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                      c(FCVal=FCResponsePeakVal, 
                                        FCDurVal="", 
                                        FCCmplxVal="",
                                        FCRateVal="")
                                    },
                                    # eCardio
                                    if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                      PLEVal=PLEResponseEndVal 
                                    }
            )
            
            # data value at thhe ROW end
            rowEndValues <- c(P2Val=P2RowEndVal,
                              P1Val=P1RowEndVal,
                              PneumoVal="",
                              AutoEDAVal=AutoEDARowEndVal,
                              AutoEDADurVal="",
                              AutoEDACmplxVal="",
                              ManualEDAVal=ManualEDARowEndVal,
                              XManualEDADurVal="",
                              ManualEDACmplxVal="",
                              CardioVal=CardioRowEndVal,
                              CardioDurVal="",
                              CardioCmplxVal="",
                              CardioRateVal="",
                              if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                c(FCVal=FCRowEndVal, 
                                  FCDurVal="", 
                                  FCCmplxVal="",
                                  FCRateVal="")
                              },
                              # eCardio
                              if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                PLEVal=PLERowEndVal 
                              }
            )
            
            
            # data value at the WOE end 
            woeEndValues <- c(P2Val=P2WoeEndVal,
                              P1Val=P1WoeEndVal,
                              PneumoVal="",
                              AutoEDAVal=AutoEDAWoeEndVal,
                              AutoEDADurVal="",
                              AutoEDACmplxVal="",
                              ManualEDAVal=ManualEDAWoeEndVal,
                              XManualEDADurVal="",
                              ManualEDACmplxVal="",
                              CardioVal=CardioWoeEndVal,
                              CardioDurVal="",
                              CardioCmplxVal="",
                              CardioRateVal="",
                              if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                c(FCVal=FCWoeEndVal, 
                                  FCDurVal="", 
                                  FCCmplxVal="",
                                  FCRateVal="")
                              },
                              # eCardio
                              if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                PLEVal=PLEWoeEndVal 
                              }
            )
            
            
            recoveryRowValues <- c(P2Val=P2RecoveryRowVal,
                                   P1Val=P1RecoveryRowVal,
                                   PneumoVal="",
                                   AutoEDAVal=AutoEDARecoveryRowVal,
                                   AutoEDADurVal="",
                                   AutoEDACmplxVal="",
                                   ManualEDAVal=ManualEDARecoveryRowVal,
                                   XManualEDADurVal="",
                                   ManualEDACmplxVal="",
                                   CardioVal=CardioRecoveryRowVal,
                                   CardioDurVal="",
                                   CardioCmplxVal="",
                                   CardioRateVal="",
                                   if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                     c(FCVal=FCRecoveryRowVal, 
                                       FCDurVal="", 
                                       FCCmplxVal="",
                                       FCRateVal="")
                                   },
                                   # eCardio
                                   if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                     PLEVal=PLERecoveryRowVal 
                                   }
            )
            
            postStimValues <- c(P2Val=P2PostStimVal,
                                P1Val=P1PostStimVal,
                                PneumoVal="",
                                AutoEDAVal=AutoEDAPostStimVal,
                                AutoEDADurVal="",
                                AutoEDACmplxVal="",
                                ManualEDAVal=ManualEDAPostStimVal,
                                XManualEDADurVal="",
                                ManualEDACmplxVal="",
                                CardioVal=CardioPostStimVal,
                                CardioDurVal="",
                                CardioCmplxVal="",
                                CardioRateVal="",
                                if(sum(pmatch(names(segmentDF), "FCMeasure", nomatch=0))!=0) {
                                  c(FCVal=FCPostStimVal, 
                                    FCDurVal="", 
                                    FCCmplxVal="",
                                    FCRateVal="")
                                },
                                # eCardio
                                if(sum(pmatch(names(segmentDF), "PPG1Measure", nomatch=0))!=0) {
                                  PLEVal=PLEPostStimVal 
                                }
            )
            
          }
          
          #### initialize a vector of audit flags for measurements that are manually changed ####
          
          {
            
            ## June 10, 2025
            
            changeAudit  <- rep("", times=length(measurementsVc))
            
          }
          
          #### initialize some column vectors for scores in the _Measurement data frame ####
          
          {
            
            # response onset and peak indices - July 23, 2023
            # responseOnsetIdx <- rep("", times=length(measurementsVc))
            # resonseEndIdx <- rep("", times=length(measurementsVc))
            
            # rank scores
            rankScore <- rep("", times=length(measurementsVc))
            # a vector to hold the name of the CQ selected for each RQ
            CQName <- rep("", times=length(measurementsVc))
            # RRM scores
            RRMScore <- rep("", times=length(measurementsVc))
            # Miritello rank scores
            miritelloRankScore <- rep("", times=length(measurementsVc))
            # ipsative Z score (leave-one-out)
            ipZScore <- rep("", times=length(measurementsVc))
            # R/C scores
            RCScore <- rep("", times=length(measurementsVc))
            
            # OSS-3 scores
            OSS3Score <- rep("", times=length(measurementsVc))
            # CQ mean for OSS-2 R/C ratio
            CQMean <- rep("", times=length(measurementsVc))
            # OSS-2scores
            OSS2Score <- rep("", times=length(measurementsVc))
            # probability analysis scores
            PAScore <- rep("", times=length(measurementsVc))
            # integer scores
            ESSScore <- rep("", times=length(measurementsVc))
            # ROSS scores
            ROSSScore <- rep("", times=length(measurementsVc))
            # PSS scores
            PSSScore <- rep("", times=length(measurementsVc))
            # bootstrap scores
            bootstrapScore <- rep("", times=length(measurementsVc))
            
            #### LXCAT scores ####
            
            LXCATScore <- rep("", times=length(measurementsVc))
            
          }
          
          
          #### Laplace smoothing ####
          
          {
            # Laplace add 1 smoothing 
            # to ensure non-zero probabilities and allow division with non-response segments
            
            # commented out 2019-7-11
            # measurementsVc <- measurementsVc + 1
            
            
            
          }
          
          ##### initialize a data frame for the measurementsVc #####
          
          measurementDF1 <- cbind.data.frame(examName=rep(examName, times=length(measurementsVc)),
                                             seriesName=rep(seriesName, times=length(measurementsVc)),
                                             chartName=rep(chartName, times=length(measurementsVc)),
                                             Label=rep(segment, times=length(measurementsVc)),
                                             eventLabel=rep(segmentName, times=length(measurementsVc)),
                                             sensorName=names(measurementsVc),
                                             Begin=rep(segOnsetRow, times=length(measurementsVc)),
                                             End=rep((segOnsetRow + (offsetRow-stimOnsetRow))-1, times=length(measurementsVc)),
                                             Answer=rep((segOnsetRow + (answerRow-stimOnsetRow))-1, times=length(measurementsVc)),
                                             measurement=measurementsVc,
                                             #
                                             preStimValues,# new Jan 20, 2025
                                             beginValues, # new Jan 20, 2025
                                             latValues, # new Jan 20, 2025
                                             endValues, # new Jan 20, 2025
                                             answerValues, # new Jan 20, 2025
                                             responseOnsetValues, # new Jan 20, 2025
                                             responsePeakValues, # new Jan 20, 2025
                                             rowEndValues, # new Jan 20, 2025
                                             woeEndValues, # new Jan 20, 2025
                                             recoveryRowValues, # new Jan 20, 2025
                                             postStimValues, # new Jan 20, 2025
                                             #
                                             prestimIndices, # new July 23, 2023
                                             latencyEndIndices, # new July 25, 2023
                                             responseOnsetIndices, # new July 21, 2023
                                             responseEndIndices, # new July 21, 2023
                                             ROWEndIndices, # new July 25, 2023
                                             WOEEndIndices, # new July 25, 2023
                                             responseRecoveryIndices, # new July 2023,
                                             poststimIndices, # new July 23, 2023
                                             #
                                             changeAudit, # new 2025, Jun 10
                                             #
                                             rankScore,
                                             RRMScore,
                                             miritelloRankScore,
                                             ipZScore,
                                             RCScore,
                                             CQName,
                                             ESSScore,
                                             OSS3Score,
                                             CQMean,
                                             OSS2Score,
                                             PAScore,
                                             ROSSScore,
                                             PSSScore,
                                             bootstrapScore,
                                             LXCATScore )
          names(measurementDF1) <- c("examName", 
                                     "seriesName", 
                                     "chartName",
                                     "Label",
                                     "eventLabel", 
                                     "sensorName", 
                                     "Begin",
                                     "End",
                                     "Answer",
                                     "sensorMeasurement", 
                                     #
                                     "preStimY", # new Jan 20, 2025
                                     "beginY", # new Jan 20, 2025
                                     "latY", # new Jan 20, 2025
                                     "endY", # new Jan 20, 2025
                                     "answerY", # new Jan 20, 2025
                                     "responseOnsetY", # new Jan 20, 2025
                                     "responseEndY", # new Jan 20, 2025
                                     "rowEndY", # new Jan 20, 2025
                                     "woeEndY", # new Jan 20, 2025
                                     "responseRecY",  # new Jan 20, 2025
                                     "postStimY",  # new Jan 20, 2025
                                     #
                                     "preStimX", # new Jan 20, 2025
                                     "latencyEndX", # new July 25, 2023
                                     "responseOnsetX", # new July 21, 2023
                                     "responseEndX", # new July 21, 2023
                                     "ROWEndX", # new July 25, 2023
                                     "WOEEndX", # new July 25, 2023
                                     "responseRecX", # new Jan 20, 2025
                                     "postStimX", # new Jan 20, 2025
                                     #
                                     "changeAudit", # new 2025 Jun 10
                                     # 
                                     "rankScore", 
                                     "RRMScore",
                                     "miritelloRankScore",
                                     "ipZScore",
                                     "RCScore", 
                                     "CQName",
                                     "ESSScore",
                                     "OSS3Score",
                                     "CQMean",
                                     "OSS2Score",
                                     "PAScore",
                                     "ROSSScore",
                                     "PSSScore",
                                     "bootstrapScore",
                                     "LXCATScore" )
          
          measurementsDF <- rbind.data.frame(measurementsDF, measurementDF1)
          
          # View(measurementsDF)
          # View(measurementDF1)
          # fix the column data types
          
          {
            measurementsDF$examName <- as.character(measurementsDF$examName)
            measurementsDF$seriesName <- as.character(measurementsDF$seriesName)
            measurementsDF$chartName <- as.character(measurementsDF$chartName)
            measurementsDF$Label <- as.character(measurementsDF$Label)
            measurementsDF$eventLabel <- as.character(measurementsDF$eventLabel)
            measurementsDF$sensorName <- as.character(measurementsDF$sensorName)
          }
          
          # response indices are numeric
          
          {
            # sensor measurement is numeric
            measurementsDF$sensorMeasurement <- as.numeric(measurementsDF$sensorMeasurement)
            
            # y values
            measurementsDF$preStimY <- as.numeric(measurementsDF$preStimY) # new Jan 20, 2025
            measurementsDF$beginY <- as.numeric(measurementsDF$beginY) # new Jan 20, 2025
            measurementsDF$latY <- as.numeric(measurementsDF$latY)  # new Jan 20, 2025
            measurementsDF$endY <- as.numeric(measurementsDF$endY)  # new Jan 20, 2025
            measurementsDF$answerY <- as.numeric(measurementsDF$answerY)  # new Jan 20, 2025
            measurementsDF$responseOnsetY <- as.numeric(measurementsDF$responseOnsetY)  # new Jan 20, 2025
            measurementsDF$responseEndY <- as.numeric(measurementsDF$responseEndY)  # new Jan 20, 2025
            measurementsDF$rowEndY <- as.numeric(measurementsDF$rowEndY)  # new Jan 20, 2025
            measurementsDF$woeEndY <- as.numeric(measurementsDF$woeEndY)  # new Jan 20, 2025
            measurementsDF$responseRecY <- as.numeric(measurementsDF$responseRecY)   # new Jan 20, 2025
            measurementsDF$postStimY <- as.numeric(measurementsDF$postStimY)   # new Jan 20, 2025
            
            # sample indices
            measurementsDF$preStimX <- as.numeric(measurementsDF$preStimX)  # new July 2023
            # measurementsDF$beginX
            measurementsDF$latencyEndX <- as.numeric(measurementsDF$latencyEndX)  # new July 2023
            # measurementsDF$endX
            # measurementsDF$answerX
            measurementsDF$responseOnsetX <- as.numeric(measurementsDF$responseOnsetX)  # new July 2023
            measurementsDF$responseEndX <- as.numeric(measurementsDF$responseEndX)  # new July 2023
            measurementsDF$ROWEndX <- as.numeric(measurementsDF$ROWEndX)  # new July 2023
            measurementsDF$WOEEndX <- as.numeric(measurementsDF$WOEEndX)  # new July 2023
            measurementsDF$responseRecX <- as.numeric(measurementsDF$responseRecX)  # new July 2023
            measurementsDF$postStimX <- as.numeric(measurementsDF$postStimX)  # new July 2023
            
          }
          
          {
            measurementsDF$rankScore <- as.character(measurementsDF$rankScore)
            measurementsDF$RRMScore <- as.character(measurementsDF$RRMScore)
            measurementsDF$miritelloRankScore <- 
              as.character(measurementsDF$miritelloRankScore)
            measurementsDF$ipZScore <- as.character(measurementsDF$ipZScore)
            measurementsDF$RCScore <- as.character(measurementsDF$RCScore)
            measurementsDF$CQName <- as.character(measurementsDF$CQName)
            measurementsDF$ESSScore <- as.character(measurementsDF$ESSScore)
            measurementsDF$OSS3Score <- as.character(measurementsDF$OSS3Score)
            measurementsDF$CQMean <- as.character(measurementsDF$CQMean)
            measurementsDF$OSS2Score <- as.character(measurementsDF$OSS2Score)
            measurementsDF$PAScore <- as.character(measurementsDF$PAScore)
            measurementsDF$ROSSScore <- as.character(measurementsDF$ROSSScore)
            measurementsDF$PSSScore <- as.character(measurementsDF$PSSScore)
            measurementsDF$bootstrapScore <- as.character(measurementsDF$bootstrapScore)
            measurementsDF$LXCATScore <- as.character(measurementsDF$LXCATScore)
          }
          
          #### reset the old warn level ####
          
          options(warn = oldw)
          
        } # end loop over l events 
        
        # View(measurementsDF)
        
        outputDF <- rbind.data.frame(outputDF, measurementsDF)
        
        # View(outputDF)
        # View(measurementsDF)
        
      } # end iteration over k chart data frames 
      
      #### .csv output ####
      
      {
        
        # .csv files are created separately for each series 
        # there is one "_Measurements" data frame for each exam
        
        if(writeCSV==TRUE) {
          # slice the data frame for this series
          outputDFSeries <- outputDF[outputDF$seriesName==seriesName,]
          
          # construct the .csv filename
          outputFileName <- paste0(examName, "_", seriesName, "_Measurements.csv")
          
          write.csv(outputDFSeries, file=outputFileName, row.names=FALSE)
        }
        
      }
      
    } # end iteration over j series data frames
    
    row.names(outputDF) <- NULL
    
    #### main output is a side effect ####
    
    # initialize the "_Measurements" data frame in the global envir
    assign(paste(examName, "Measurements", sep="_"), outputDF, pos=1)
    
    # the _Measurements data frame includes all series
    
  } # end iteration over i exams
  
  #### output ####
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(outputDF)
  
} # end extractMeasurementsFn()

# extractMeasurementsFn(x=uniqueExams, showNames=TRUE, output=FALSE, CSVName="")


