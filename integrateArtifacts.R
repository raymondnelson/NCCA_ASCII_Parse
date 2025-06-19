# R function to integrate artifact and feature extraction
# March 19, 2020
# Raymond Nelson
#
####


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]


####


integrateArtifactsFn <- function(x=uniqueExams) {
  # R function to integrate artifact and feature extraction
  # March 19, 2020
  # Raymond Nelson
  #
  # x is a vector of names of data frames that contain the time series data 
  #
  ####
  
  uniqueExams <- x
  
  # uses the showNames and output settings from the global environment
  # set in the NCCAASCII_init.R script
  
  #### iterate over each exam in the list ####
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
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
          
        }
        
        #### reset the feature extraction ####
        
        # {
        #   chartDF$UPneumoExtract <- ""
        #   chartDF$LPneumoExtract <- ""
        #   chartDF$AutoEDAExtract <- ""
        #   chartDF$CardioExtract <- ""
        #   
        #   # chartDF$PLE1Extract <- ""
        # }
        
        #### calculate the cardio rate and RBPF ####
        
        {
          
          # used by the cardio extract function
          
          cardioRate <- ratePerMin(MASmooth(x=chartDF$c_Cardio1, y=2, times=1),
                                   buffer=9,
                                   peaks="upper",
                                   lowPass=TRUE)
          
          assign("cardioRate", cardioRate, envir=.GlobalEnv)
          
          rbpfMsg <- rbpfProbFn(x=chartDF, y=NULL)
          
          assign("rbpfMsg", rbpfMsg, envir=.GlobalEnv)
          
        }
        
        #### calculate the respiration rate ####
        
        {
          
          # for the ratePerMin() function
          # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
          
          # determine the respiration rate
          UPRate <- ratePerMin(x=chartDF$c_UPneumo, buffer=40)
          
          # is the upper respiration rate outside the normal range
          UPMeasurement <- ifelse(UPRate < 4,
                                  "ONR_slow",
                                  ifelse(UPRate > 30,
                                         "ONR_fast",
                                         "normal"))
          
          LPRate <- ratePerMin(x=chartDF$c_LPneumo, peaks="lower", lowPass=TRUE)
          
          # is the lower respiration rate outside the normal range
          LPMeasurement <- ifelse(LPRate < 4,
                                  "ONR_slow",
                                  ifelse(LPRate > 30,
                                         "ONR_fast",
                                         "normal"))
          
          pnRateRatio <- exp(-abs(log(LPRate/UPRate)))
          
        }
        
        #### increment the K loop if no events ####
        
        if(length(eventNames) == 0) {
          seriesDF[chartOnsetRow:chartEndRow,] <- chartDF
          next()
        }
        
        #### slice the RQs and CQs if more than 2 each ####
        
        {
          
          uniqueRQs <-
            unique(chartDF$eventLabel[grep("R", chartDF$eventLabel)])
          uniqueCQs <-
            unique(chartDF$eventLabel[grep("C", chartDF$eventLabel)])
          
          # slice the RQs and CQs only if more than 2 RQs and 2 CQs
          # if(length(uniqueRQs) >= 2 && length(uniqueCQs) >= 2) {
          #   theseRows <- grep("[R C]", chartDF$Label)
          #   thisChartDF <- chartDF[theseRows,]
          #   eventNames <- thisChartDF$eventLabel[thisChartDF$eventLabel!=""]
          #   eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          # } else {
          #   thisChartDF <- chartDF
          # }
          # commented out because it is 
          # better to check for R and C questions later
          
          thisChartDF <- chartDF
          
          # a vector of event onset rows
          eventNames <- thisChartDF$eventLabel[thisChartDF$eventLabel!=""]
          # exclude some of the events 
          eventNames <- eventNames[!(eventNames %in% excludeEvents)]
          
        }
        
        ####  loop over the events in the chart data frame  ####
        
        l=10
        for (l in 1:length(eventNames)) {
          
          {
            
            segmentName <- eventNames[l]
            
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
            
            if(showNames==TRUE) print(segmentName)
            
            # increment the loop with no feature extraction
            # if not RQ or CQ when the chart includes 2 or more RQs and CQs
            if(length(uniqueRQs) >= 2 && length(uniqueCQs) >= 2) {
              if(!isTRUE(grepl("[R, C]", segmentName))) {
                next() 
              }
            }
            
          }
          
          ########################################################
          
          #### get the row indices for each stimulus event ####
          
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
            
            # stimEndRow <- segEndRow - startRow + 1 # will normally set to 600
            # stimEndRow is the end of the EW (evaluation window)
            stimEndRow <- stimOnsetRow + (measuredSeg) * cps - 1
            if(stimEndRow > nrow(segmentDF)) stimEndRow <- nrow(segmentDF)
            
            # check some possible problems
            if(answerRow >= stimEndRow) answerRow <- stimEndRow - 1
            if(stimOffsetRow >= answerRow) stimOffsetRow <- answerRow - 1
            
            # remove NA rows
            # segmentDF <- na.omit(segmentDF)
            
            # fix potential problems
            if(stimOffsetRow <= stimOnsetRow) stimOffsetRow <- stimOnsetRow + 1
            if(answerRow <= stimOffsetRow) answerRow <- stimOffsetRow + 1
            if(stimOffsetRow >= nrow(segmentDF)) stimOffsetRow <- nrow(segmentDF) - 2
            if(answerRow >= nrow(segmentDF)) answerRow <- nrow(segmentDF) - 1
            
          }
          
           ########################################################
          
          {
            
            # check the Artifacts_a channel in the segmentDF
            # remove response onset and response end 
            # if artifacts have occured from 3 sec prestim seconds to .5 sec after respnose end
            
            # number of prestimulus seconds to look for artifacts
            # artifactPrestim <- 5 * cps
            
            # set the prestimulus start row to begin looking for artifacts
            prestimStartRow <- stimOnsetRow - artifactPrestim
            
            # View(segmentDF)
            
            # get the responseEndRow
            {
              stopRowUPn <- which(segmentDF$UPneumoExtract=="responseEndRow")
              stopRowLPn <- which(segmentDF$LPneumoExtract=="responseEndRow")
              stopRowAutoEDA <- which(segmentDF$AutoEDAExtract == "responseEndRow")
              stopRowManualEDA <- which(segmentDF$ManualEDAExtract == "responseEndRow")
              stopRowCardio <- which(segmentDF$CardioExtract == "responseEndRow")
              if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
                stopRowPLE <- which(segmentDF$PPG1Extract == "poststimSegOffset")
              }
            }
            
            # number of seconds to look for artifacts post respnse peak 
            # artifactAddSeg <- .5 * cps - 1
            
            # and set the stopRow - keep the last in case events are stacked closely
            {
              stopRowUPn <- artifactAddSeg + stopRowUPn[length(stopRowUPn)]
              stopRowLPn <- artifactAddSeg + stopRowLPn[length(stopRowLPn)]
              stopRowAutoEDA <- artifactAddSeg + stopRowAutoEDA[length(stopRowAutoEDA)]
              stopRowManualEDA <- artifactAddSeg + stopRowManualEDA[length(stopRowManualEDA)]
              stopRowCardio <- artifactAddSeg + stopRowCardio[length(stopRowCardio)]
              if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
                stopRowPLE <- artifactAddSeg + stopRowPLE[length(stopRowPLE)]
              }
            } 
            
            # set the range to look for artifacts
            {
              UPnRange <- c(prestimStartRow:stopRowUPn)
              LPnRange <- c(prestimStartRow:stopRowLPn)
              AutoEDARange <- c(prestimStartRow:stopRowAutoEDA)
              ManualEDARange <- c(prestimStartRow:stopRowManualEDA)
              CardioRange <- c(prestimStartRow:stopRowCardio)
              if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
                PLERange <- c(prestimStartRow:stopRowPLE)
              }
            }
            
            isArtifactedFn <- function(x) {
              # private function to check for artifacts
              # input is the segmentDF$Artifacts_a vector
              # sliced to the search range for each sensor
              # from 5 prestim sec to .5 sec after response peak
              return(ifelse(length(which(x != 0)) > 0, TRUE, FALSE))
              # output TRUE signals to remove the feature extraction
            }
            
            # call the inArtifactedFn function for each sensor
            # to determine which responses have included artifacts
            # Artifacts_a channel impacts all sensors
        
            {
              removeUPnResponse <- isArtifactedFn(segmentDF$Artifacts_a[UPnRange])
              removeLPnResponse <- isArtifactedFn(segmentDF$Artifacts_a[LPnRange])
              removeAutoEDAResponse <- isArtifactedFn(segmentDF$Artifacts_a[AutoEDARange])
              removeManualEDAResponse <- isArtifactedFn(segmentDF$Artifacts_a[ManualEDARange])
              removeCardioResponse <- isArtifactedFn(segmentDF$Artifacts_a[CardioRange])
              if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
                removePLEResponse <- isArtifactedFn(segmentDF$Artifacts_a[PLERange])
              }
            }
            
            # also check the for artifacts artifacts for each sensor
            # these affect only the sensor where the artifact is observed
            
            {
              # removeUPnResponse
              # removeLPnResponse
              # removeAutoEDAResponse
              # removeManualEDAResponse
              # removeCardioResponse
              # if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
              #   removePLEResponse
              # }
            }
            
            # also check for any event within the prestim segment
            
            stackedEventsFn <- function(x) {
              # look for events stacked quickly before the current event
              # x input is the Label column from the segmentDF
              # set the search range to 10 prestimulus segment
              searchRange <- ((prestimSeg*cps + 1) - (8*cps)):(prestimSeg*cps)
              # look for any question or annotation 
              length(which(segmentDF$Label[searchRange] != "")) > 0
              # output is a boolean value
            }
            
            # View(segmentDF)
            
            if(isTRUE(stackedEventsFn(x=segmentDF$Label))) {
              removeUPnResponse <- TRUE
              removeLPnResponse <- TRUE
              removeAutoEDAResponse <- TRUE
              removeManualEDAResponse <- TRUE
              removeCardioResponse <- TRUE
              if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
                removePLEResponse <- TRUE
              }
              
            }
            
            #### remove the responseOnsetRow and responseEndRow ####
            
            removeResponseFn <- function(x) {
              # private function 
              # to remove responseOnsetRow and responseEndRow
              # x input vector is the Extract columns for each sensor
              # output is the x vector with the response removed
              searchItems <- c("responseOnsetRow", "responseEndRow")
              x[x%in% searchItems] <- ""
              return(x)
            }
            
            # call the function to remove the response from each sensor
            
            {
              if(isTRUE(removeUPnResponse)){
                segmentDF$UPneumoExtract <- removeResponseFn(segmentDF$UPneumoExtract)
              }
              if(isTRUE(removeLPnResponse)){
                segmentDF$LPneumoExtract <- removeResponseFn(segmentDF$LPneumoExtract)
              }
              if(isTRUE(removeAutoEDAResponse)){
                segmentDF$AutoEDAExtract <- removeResponseFn(segmentDF$AutoEDAExtract)
              }
              if(isTRUE(removeManualEDAResponse)){
                segmentDF$ManualEDAExtract <- removeResponseFn(segmentDF$ManualEDAExtract)
              }
              if(isTRUE(removeCardioResponse)){
                segmentDF$CardioExtract <- removeResponseFn(segmentDF$CardioExtract)
              }
              if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
                if(isTRUE(removePLEResponse)){
                  segmentDF$PPG1Extract <- removeResponseFn(segmentDF$PPG1Extract)
                }
              }
            }
            
          }
          
          #############################
          
          # pass the segmentDF to the thisChartDF
          # do not save to the examDF because the segments may overlap
          # saving back to the thisChartDF preserves any overlapping information from one seg to the next
          thisChartDF[startRow:endRow,] <- segmentDF
          
        } # end loop over l events
        
        # pass back the RQs and CQs
        # chartDF[theseRows,] <- thisChartDF
        chartDF <- thisChartDF
        
        # pass the chartDF to the seriesDF
        seriesDF[chartOnsetRow:chartEndRow,] <- chartDF
        
      } # end loop over K charts
      
      # pass the seriesDF to the examDF
      examDF[seriesOnsetRow:seriesEndRow,] <- seriesDF
      
    } # end loop over J series
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1)
    
  } # end loop over i exams 
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end integrateArtifactsFn() function

# integrateArtifactsFn(x=uniqueExams)


