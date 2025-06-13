# PCASS feature extraction and numerical transformation
# July 4, 2020
# Raymond Nelson
# 
####



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15



# source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)

source('~/Dropbox/R/NCCA_ASCII_Parse/PCASSScores.R', echo=TRUE)


PCASSAlgorithmFn <- function(x=uniqueExams) {
  # R function to iterate over of exam names and load the data frames
  # to extract PCASS response features, assign PCASS Scores,
  # and calculate a PCASS classifier
  # July 2, 2019
  # Raymond Nelson
  #
  # x is a vector of names of data frames containing the time series data
  # 
  # other parameters may be obtained from the global envir
  # 
  ####
  
  uniqueExams <- x
  
  #### iterate over the exams in the input vector ####
  
  i=1
  for (i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      
      # get the examination data frame
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      examDF$examName <- as.character(examDF$examName)
      examDF$seriesName <- as.character(examDF$seriesName)
      examDF$chartName <- as.character(examDF$chartName)
      
      examStartRow <- 1
      examEndRow <- nrow(examDF)
      
      assign("examDF", examDF, pos=1)
      assign("examName", examName, pos=1)
      
      if(showNames==TRUE) print(examName)
      
      # get the names of unique series
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
        # seriesEndRow <- which(examDF$seriesName==seriesName)[length(which(examDF$seriesName==seriesName))]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        assign("seriesDF", seriesDF, pos=1)
        assign("seriesName", seriesName, pos=1)
        
        if(showNames==TRUE) print(paste("series", seriesName))
        
        # uniqueCharts <- names(seriesDF)
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        # uniqueCharts <- as.character(unique(examDF[seriesOnsetRow:seriesEndRow,'chartName']))
        
      }
      
      #### iterate over each chart in the series ####
    
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          chartName <- uniqueCharts[k]
          # get the data frame with the time series data for each chart in the series
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          # chartEndRow <- which(seriesDF$chartName==chartName)[length(which(seriesDF$chartName==chartName))]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          #### scale and offset the data ####
          
          # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
          
          ####################   get the first and last events   #####################
          
          # call the eventyNamesFn from the sigProcHelper.R script
          # firstLastEvents <- eventNamesFn(x=chartDF)
          
          {
            
            firstLastEvents <- getFirstLastEventFn(x=chartDF)
            firstEvent <- firstLastEvents[1]
            lastEventEnd <- firstLastEvents[2]
            assign("firstLastEvents", firstLastEvents, pos=1)
            
            
            # eda scale = 500
            # ple scale = 150
            # scaleDataFn(x=chartDF$c_UPneumoSm, sec=15, times=30, ignore=20, yRange=scaleVals['uPneumo'], maxY=(yMax-.05*yRange), minY=(yMin+.05*yRange), firstRow=firstEvent, lastRow=(lastEventEnd-450))
            # eda offset = 0
            # ple offset = -640
            
            # offsetDataFn(x=chartDF$c_UPneumoSm, y=yOffset['uPneumo'], maxY=999, minY=650, firstRow=firstEvent, lastRow=(lastEventEnd-450))
            
          }
          
          
          ####
          
          assign("chartDF", chartDF, pos=1)
          assign("chartName", chartName, pos=1)
          
          if(showNames==TRUE) print(paste("Chart:", chartName))
          
          # skip short charts less than 10 seconds
          if(nrow(chartDF) < 600) next()
          
          #### process the stimulus segments ####
          
          # a vector of event onset rows
          eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
          # exclude some of the events 
          eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          
          ##### reset the feature extraction columns
          # chartDF$UPneumoExtract <- ""
          # chartDF$LPneumoExtract <- ""
          # chartDF$AutoEDAExtract <- ""
          # chartDF$ManualEDAExtract <- ""
          # chartDF$CardioExtract <- ""
          # if(sum(pmatch(names(chartDF), "c_PLE", nomatch=0))!=0) {
          #   chartDF$PLEExtract <- ""
          # }
          
          #### increment the K loop if no events ####
          
          if(length(eventNames) == 0) {
            seriesDF[chartOnsetRow:chartEndRow,] <- chartDF
            next()
          }
          
        }
        
        #### iterate over the events in the chart data frame ####
        
        l=3
        for (l in 1:length(eventNames)) {
          
          {
            
            segmentName <- eventNames[l]
            
            # get the onset row using the segment name so that events during the prestimSegment are ignored
            segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
            # set the end row using the evaluation window length 
            segEndRow <- segOnsetRow + measuredSeg*cps - 1
            if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
            
            # get the segment prestim row
            prestimRow <- segOnsetRow - prestimSeg*cps
            if(prestimRow < 1) prestimRow <- 1
            
            # get the segment start row and end row
            startRow <- prestimRow
            endRow <- segEndRow + addSeg*cps
            if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
            
            #### get the segment data frame ####
            
            segmentDF <- chartDF[startRow:endRow,]
            # View(segmentDF)
            
            assign("segmentDF", segmentDF, pos=1)
            assign("segmentName", segmentName, pos=1)
            
            if(showNames==TRUE) print(segmentName)
            
          }
          
          #### get the row indices for the stimulus event ####
          
          {
            
            # adjust the rows so that row numbers refer to data in the segmentDF not the chartDF
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
            
            # stimEndRow <- segEndRow - startRow + 1 # will normally set to 600
            # stimEndRow is the end of the EW (evaluation window)
            stimEndRow <- stimOnsetRow + measuredSeg*cps - 1 # normally 600
            if(stimEndRow > nrow(segmentDF)) stimEndRow <- nrow(segmentDF)
            
            if(answerRow >= stimEndRow) answerRow <- stimEndRow - 1
            if(stimOffsetRow >= answerRow) stimOffsetRow <- answerRow - 1
            
            # correct for missing or no answer during the measurement segement
            ### maybe this needs to be offsetRow+1
            # if(is.na(answerRow)) { answerRow=segEndRow-1 }
            # if(is.na(answerRow)) { answerRow=stimOffsetRow+1 }
            
            # remove NA rows
            # segmentDF <- na.omit(segmentDF)
            
            # fix potential problems
            if(stimOffsetRow <= stimOnsetRow) stimOffsetRow <- stimOnsetRow + 1
            if(answerRow <= stimOffsetRow) answerRow <- stimOffsetRow + 1
            if(stimOffsetRow >= nrow(segmentDF)) stimOffsetRow <- nrow(segmentDF) - 2
            if(answerRow >= nrow(segmentDF)) answerRow <- nrow(segmentDF) - 1
            
          }
          
          #### get the extraction parameters for the segment ####
          
          {
            
            extract.params <- list(onset=stimOnsetRow, 
                                   offset=stimOffsetRow, 
                                   answer=answerRow, 
                                   end=stimEndRow, 
                                   segName=segmentName, 
                                   chart=chartName, 
                                   series=seriesName, 
                                   exam=examName)
            
            assign("extract.params", extract.params, pos=1)
            
          }
          
          #### EDA feature extraction ####
          
          # get the EDA data for the stimulus segment
          
          
          EDAResponse <- 
            segmentDF$c_PCASS_EDA[stimOnsetRow:stimEndRow]
          
          EDAResponseDiff <- diff(EDAResponse)
          
          # use a function to determine the slope
          
          # include all positive slope segments from .5 sec to 15 sec
          # EDA
          
          
          if(extractEDA==TRUE) {
            print("  EDA feature extraction")
            segmentDF <- EDAExtractFn(x=segmentDF, y=extract.params)
          } 
          
          
          
          
          #### PLE feature extraction ####
          
          VMResponse <- segmentDF$c_PCASSCardio
          
          VMResponseDiff <- diff(VMResponse)
          
          # determine the mean of the 1 sec summed differences
          
          
          if(extractPLE==TRUE) {
            # check to see if PLE data exist for the chart
            if(sum(pmatch(names(examDF), "c_PLE1", nomatch=0))!=0) {
              print("  PLE feature extraction")
              segmentDF <- newPLEExtractFn(x=segmentDF, y=extract.params)
            } # end if PLE data exist
          } 
          
          
          
          
          # pass the segmentDF to the chartDF
          chartDF[startRow:endRow,] <- segmentDF
          
        } # end l loop for each event
        
        # pass the chartDF to the seriesDF
        seriesDF[chartOnsetRow:chartEndRow,] <- chartDF
        
      } # end k loop for each chart
      
      ####  initialize the RqCqDF data frame for the series  ####
      
      uniqueEventsSeries <- unique(seriesDF$eventLabel[seriesDF$eventLabel!=""])

      uniqueEventsSeries <- uniqueEventsSeries[!(uniqueEventsSeries %in% excludeEvents)]
        
      if(length(uniqueEventsSeries) != 0) {
        
        # the RqCqDF contains only RQs and CQs
        
        # make a vector of RQs and CQs for the series 
        
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
        
        # keep all events except the first event if no RQ and CQ events
        if(length(RqCqSeriesRows) == 0) {
          # get all indices for events not in excludedEvents
          RqCqSeriesRows <- which(!(seriesMeasurementDF$Label %in% excludeEvents))
          # get the event lables from the eventLabels column
          workingEvents <- seriesMeasurementDF$eventLabel[RqCqSeriesRows]
          workingEvents <- unique(workingEvents)
          # replace the working events with the Labels column
          workingEvents <- 
            seriesMeasurementDF$Label[seriesMeasurementDF$eventLabel %in% 
                                        workingEvents]
          # complete the vector of working rows
          RqCqSeriesRows <- which(seriesMeasurementDF$Label %in% workingEvents)
          # remove the rows for the first event
          RqCqSeriesRows <- seriesMeasurementDF$eventLabel %in% 
            (unique(seriesMeasurementDF$eventLabel[RqCqSeriesRows])[-1])
        }
        
        # return the measurementDF and exit if no stimulus events
        if(length(RqCqSeriesRows) == 0) {
          if(output == TRUE) {
            return(measurementDF)
          } else return()
        }
        
        # initialize the RqCQDF for the series
        RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
        # assign("RqCqSeriesRows", RqCqSeriesRows, pos=1)
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        
        # View(RqCqDFSeries)
        
      } else {
        next() # next j series
      }

      #### call the PCASSScoresFn here ####
      
      
      # source('~/Dropbox/R/NCCA_ASCII_Parse/PCASSScores.R', echo=TRUE)
      
      RqCqDFSeries <- PCASSScoresFn(RqCqDFSeries=RqCqDFSeries,
                                    bootstrapDecisionRule=bootstrapDecisionRule,
                                    bootstrapCutProbT=.3,
                                    bootstrapCutProbD=.7,
                                    forced=TRUE,
                                    makeDF=FALSE,
                                    saveCSV=saveCSV,
                                    analysisListName=analysisListName )
      
      # pass the RqCQDFSeries bac to the seriesMeasurementDF
      seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:seriesEndRow,] <- seriesDF
    
    } # end j loop for each series
    
    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)
    
  } # end i loop for each exam
  
  print(paste(i, "exams processed"))
  
  return(examDF)
  
} # end PCASSAlgorithmFn()