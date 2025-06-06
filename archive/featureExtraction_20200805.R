# new function for feature extraction
# for all NCCA ASCII data channels
# 4/23/2016
# Raymond Nelson
#
####



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]



{
  # these are set the init script
  
  # cps <- 30
  # prestimSeg <- 5
  # EDALat <- .5
  # CardioLat <- .5
  # ROWEnd <- 5
  # measuredSeg <- 15
  
  # pneumoAnsBuff <- 1.25
}



{
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/workFlow_init.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
  # source this also 
  # for the ratePerMin() function
  source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc_extra.R', echo=FALSE)
  
  # integrateEDAArtifacts <- FALSE
  
  # source the scripts with the helper functions
  source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoMeasurement.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/slopeChange.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractOSS2.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/PLEMeasurement.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractPCASS.R', echo=FALSE)
  
  # source the scripts for each sensor
  source('~/Dropbox/R/NCCA_ASCII_Parse/PneumoExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/EDAExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/CardioExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/newFCExtract.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/FCExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/PLEExtract.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/rbpfProb.R', echo=FALSE)
  
}




featureExtraction <- function(x=uniqueExams) {
  # function to iterate over a vector of data frame names 
  # and extract the scoring feature for pneumo, EDA, cardio and PLE data
  # 4/23/2016
  # Raymond Nelson
  #
  # x is a vector of names of data frames that contain the time series data 
  #
  # other parameters may be obtained from the global environment
  #
  # EW is a logical value to impose a strict use of the evauluation window
  # when set to FALSE upward responses will be evaluated to the end of response
  # even if the end of response is after the end of the evaultion window
  # when set to TRUE feature extraction will stop at the end of the evaluation window
  # for responses that continue past the evaluation window
  #
  # ROW is a logical value to impose a strict use of the response onset window
  # when set to TRUE upward repsonse segments will not be evalutuated
  # if the positive slope change begins after the response onset window
  # when set to FALSE all upward slope changes will be evaluated
  # if they begin in the evaluation window, and only the response onset is required to
  # occur in the response onset window
  #
  # descentRule is a logical value that excludes peaks from measurement extraction
  # if they data descend more than 50% from previous response peak to the response onset value
  #
  # ignore is the number of samples to ignore for short duration slope changes
  ####
  
  uniqueExams <- x
  
  # uses the showNames and output settings from the global environment
  # set in the NCCAASCII_init.R script
  
  ##### iterate over each exam in the list #####
  
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
    
    ##### iterate over each unique series #####

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
      
      ##### iterate over each chart in the series #####
      
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
          
        ##### reset the feature extraction columns #####
       
        {
          
          chartDF$UPneumoExtract <- ""
          chartDF$LPneumoExtract <- ""
          chartDF$AutoEDAExtract <- ""
          chartDF$ManualEDAExtract <- ""
          chartDF$CardioExtract <- ""
          if(sum(pmatch(names(chartDF), "c_PPG1", nomatch=0))!=0) {
            chartDF$PPG1Extract <- ""
          }
          
        }
        
        ############ calculate the cardio rate and RBPF #############
        
        {
        
          # used by the cardio extract function
          
          cardioRate <- ratePerMin(MASmooth(x=chartDF$c_Cardio1, y=2, times=1),
                                   buffer=9,
                                   peaks="upper",
                                   lowPass=TRUE)
          
          rbpfMsg <- rbpfProbFn(x=chartDF, y=NULL)
          
          print(paste("cardio rate:", cardioRate))
          print(paste("rbpfMsg: rbpfMsg"))
        
          assign("cardioRate", cardioRate, envir=.GlobalEnv)
          assign("rbpfMsg", rbpfMsg, envir=.GlobalEnv)
        
        }

        ############ calculate the respiration rate ############
        
        {
          
          # used the by the pneumo extract function
          
          # for the ratePerMin() function
          # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
          
          # determine the respiration rate
          UPRate <- ratePerMin(x=chartDF$c_UPneumo, buffer=40, lowPass=TRUE)
          
          # is the upper respiration rate outside the normal range
          UPMeasurement <- ifelse(UPRate < 10,
                                  "ONR_slow",
                                  ifelse(UPRate > 22,
                                         "ONR_fast",
                                         "normal"))
          # ratePerMin()
          LPRate <- ratePerMin(x=chartDF$c_LPneumo, buffer=40, peaks="upper", lowPass=TRUE)
          
          # is the lower respiration rate outside the normal range
          LPMeasurement <- ifelse(LPRate < 10,
                                  "ONR_slow",
                                  ifelse(LPRate > 22,
                                         "ONR_fast",
                                         "normal"))
          
          rateRatio <- round(exp(-abs(log(UPRate / LPRate))), 2)
          
          print(paste("upper respiration rate:", UPRate))
          print(paste("lower respiration rate:", LPRate))
          print(paste("respiration rate ratio:", rateRatio))
          
          assign("UPRate", UPRate, envir=.GlobalEnv)
          assign("LPRate", LPRate, envir=.GlobalEnv)
          assign("rateRatio", rateRatio, envir=.GlobalEnv)
          
        }
        
        ######## increment the K loop if no events ########
        
        if(length(eventNames) == 0) {
          seriesDF[chartOnsetRow:chartEndRow,] <- chartDF
          next()
        }
        
        ####### slice the RQs and CQs if more than 2 each #######
        
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
          
          thisChartDF <- chartDF
          
        }
        
        
        
        #######  loop over all the events in the chart data frame  #######
        
        l=7
        for (l in 1:length(eventNames)) {
          
          {
            
            segmentName <- eventNames[l]
            
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
            
            if(showNames==TRUE) print(segmentName)
            
            # increment the loop with no feature extraction
            # if not RQ or CQ when the chart includes 2 or more RQs and CQs
            if(length(uniqueRQs) >= 2 && length(uniqueCQs) >= 2) {
              if(!isTRUE(grepl("[R C]", segmentName))) {
                next() 
              }
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
          
          ##### get the feature extraction parameters for the segment #####
          
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
          
          ############# pneumo feature extraction #############
          
          if(all(isTRUE(extractPneumo), rateRatio >=.9)) {
            print("  pneumo feature extraction")
            segmentDF <- pneumoExtractFn(x=segmentDF, 
                                         y=extract.params,
                                         tRate=UPMeasurement, 
                                         aRate=LPMeasurement)
          } 
          
          # use the ratio of upper and lower pneumo to identify artifacts
          
          ################ EDA feature extraction ################
          
          if(extractEDA==TRUE) {
            print("  EDA feature extraction")
            segmentDF <- EDAExtractFn(x=segmentDF, y=extract.params)
          } 
          
          ############### cardio feature extraction ##############
          
          if(extractCardio==TRUE) {
            print("  cardio feature extraction")
            segmentDF <- CardioExtractFn(x=segmentDF, 
                                         y=extract.params,
                                         cardioRate=cardioRate,
                                         rbpfMsg=rbpfMsg)
          } 
          
          ############### finger cuff feature extraction #############
          
          if( extractFC==TRUE && sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0 ) {
            # check to see if finger cuff data exist
            # if( sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0 ) {
              print("  finger cuff feature extraction")
              segmentDF <- newFCExtractFn(x=segmentDF, y=extract.params)
            # } # end if FC data exist
          } 
          
          ################# PLE feature extraction ##################
          
          if(extractPLE==TRUE) {
            # check to see if PLE data exist for the chart
            if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0))!=0) {
              print("  PLE feature extraction")
              segmentDF <- newPLEExtractFn(x=segmentDF, y=extract.params)
            } # end if PLE data exist
          } 
          
          ################## PCASS-2 feature extraction ###############
          
          {
            
            
            
          }
          
          ####
          
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
      # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF
      
    } # end loop over J series
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over i exams 
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end featureExtraction() function

# featureExtraction(x=uniqueExams)

