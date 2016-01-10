# new script for cardio feature extraction
# 
#
#
################################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[8]



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15



###########



newCardioExtractFn <- function(x=uniqueExams, showNames=TRUE, output=FALSE, EW=FALSE, ROW=FALSE, descentRule=TRUE) {
  # function to iterate over a vector of data frame names 
  # and locate the onset and peak of cardio reactions
  #
  # x is a vector of names of data frames that contain the time series data for all charts for each exam
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  #
  # output=TRUE will return a data frame for the last input exam
  #
  # EW is a logical value to impose a strict use of the evauluation window
  # when set to FALSE upward responses will be evaluated to the end of response
  # even if the end of response is after the end of the evaultion window 
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
  ########
  
  # source the helper function
  source('~/Documents/R_programming/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)
  
  # get a vector of event names that are not measured
  source('~/Documents/R_programming/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
  ########
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    ### add additional columns here
    examDF$CardioExtract <- rep("", times=nrow(examDF))
    examDF$CardioMeasure <- rep("", times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=2
      seriesName <- uniqueSeries[j]
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=5
        chartName <- uniqueCharts[k]
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(showNames==TRUE) print(chartName)
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        
        # excludeEvents was created by sourcing the excludedEvents.R script before the first loop
        eventNames <- eventNames[!(eventNames %in% excludeEvents)]
        
        ### need to manage repeated events 
        
        # repeated events are managed before this stage with the fixDuplicates function
        # the fixDuplicates() function will rename the first iteration of a repeated event
        # keeping the original even name for the second (last) repetition
        # fixDup() is called in the workFlow script 
        # may be useful to call it here
        
        # stop if pulse rate is outside the normal range of 60 to 100
        if(round(60 / (mean(diff(maxPeak(chartDF$c_Cardio1, y=8))) / 30), 0) < 60) { 
          chartDF$CardioExtract <- "ONR"
          seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
          next }
        if(round(60 / (mean(diff(maxPeak(chartDF$c_Cardio1, y=8))) / 30), 0) > 100) { 
          chartDF$CardioExtract <- "ONR"
          seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
          next }
        
        # loop over all the events in the chart data frame
        for (l in 1:length(eventNames)) {
          # l=10
          segmentName <- eventNames[l]
          # get the onset row  for the chartDF so that events during the prestimSegment are ignored
          segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
          # get the segment prestim row
          prestimRow <- segOnsetRow - prestimSeg*cps
          if(prestimRow < 1) prestimRow <- 1
          # set the end row so that the data frame
          segEndRow <- segOnsetRow + measuredSeg*cps - 1
          if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
          # set the row number for the end of the stimulus segment
          endRow <- segEndRow + addSeg*cps
          if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
          
          # get the segment start row
          startRow <- prestimRow
          
          # get the segment data frame
          segmentDF <- chartDF[startRow:endRow,]
          
          if(showNames==TRUE) print(segmentName)
          
          #####
          
          # adjust the onsetRow for the segmentDF
          segOnsetRow <- segOnsetRow - startRow + 1
          segEndRow <- segEndRow - startRow + 1 # added 9-21-2015
          
          # onsetRow <- which(segmentDF$Events=="onsetRow")[1]
          # get the first offset after the onset row
          offsetRow <- which(segmentDF$Events[segOnsetRow:nrow(segmentDF)]=="offsetRow")[1] + segOnsetRow - 1
          answerRow <- which(segmentDF$Events[segOnsetRow:nrow(segmentDF)]=="answerRow")[1] + segOnsetRow - 1
          
          # remove NA rows
          # segmentDF <- na.omit(segmentDF)
          
          # fix problem when answerRow == offsetRow
          if(offsetRow == segOnsetRow) offsetRow <- segOnsetRow + 1
          if(answerRow <= offsetRow) answerRow <- offsetRow + 1
          if(offsetRow >= nrow(segmentDF)) offsetRow <- nrow(segmentDF) - 2
          if(answerRow >= nrow(segmentDF)) answerRow <- nrow(segmentDF) - 1
          
          ##################################
          
          dataVector <- segmentDF$c_CardioMA
          
          extractList <- as.list(c(segOnsetRow, 
                                   offsetRow, 
                                   answerRow, 
                                   cps,
                                   1, 
                                   CardioLat,
                                   ROWEnd,
                                   segmentName, 
                                   4, 
                                   paste(examName, chartName, segmentName, sep="_")) )
          extractList[[11]] <- dataVector
          names(extractList) <- c("begin", 
                                  "end", 
                                  "answer", 
                                  "rate",
                                  "start", 
                                  "lat", 
                                  "ROWEnd",
                                  "segmentName", 
                                  "nSmooth", 
                                  "segmentTitle", 
                                  "dataVector")
          
          ###############
          
          ### use the amplitude extractFunction to compute the cardio response
          extractResult <- amplitudeExtract(extractList, 
                                            strictWindow=EW, 
                                            strictROW=ROW, 
                                            descentStop=descentRule)
          
          # print(extractResult)
          
          # process the ouput of the amplitudeExctract function
          responseOnsetRow <- as.numeric(extractResult["responseOnsetRow"])
          responseEndRow <- as.numeric(extractResult["responsePeakRow"])
          stopRow <- as.numeric(extractResult["stopRow"])
          responseChange <- as.numeric(extractResult["responseChangeValue"])
          
          # make sure that events are on distinct rows
          # prestimRow
          onsetRow <- segOnsetRow 
          # offsetRow <- offsetRow 
          # answerRow <- answerRow
          latencyRow <- segOnsetRow + (EDALat * cps)
          ROWEndRow <- answerRow + (ROWEnd * cps)
          if(ROWEndRow >= nrow(segmentDF)) ROWEndRow <- nrow(segmentDF) - 3
          # reponseOnsetRow 
          # responseEndRow
          # segEndRow <- segEndRow - prestimRow + 1
          #
          if(responseOnsetRow == (segEndRow - 1 )) {
          # if(responseOnsetRow == (segEndRow + prestimRow - 1)) { # modified 11/28/15 to eliminate responses at end of EW
            responseOnsetRow <- NULL
            responseEndRow <- NULL
            stopRow <- NULL
          }
          #
          events <- as.numeric(c(onsetRow, 
                                 offsetRow, 
                                 answerRow, 
                                 latencyRow, 
                                 ROWEndRow,
                                 segEndRow,
                                 responseOnsetRow, 
                                 responseEndRow,
                                 stopRow,
                                 responseChange))
          ifelse(length(events)==10, 
                 names(events) <- c("onsetRow", 
                                    "offsetRow", 
                                    "answerRow", 
                                    "latencyRow", 
                                    "ROWEndRow", 
                                    "segEndRow",
                                    "responseOnsetRow",
                                    "responseEndRow",
                                    "stopRow",
                                    "responseChange"),
                 names(events) <- c("onsetRow", 
                                    "offsetRow", 
                                    "answerRow", 
                                    "latencyRow", 
                                    "ROWEndRow", 
                                    "segEndRow"))
          ## correct for events on the same rows
#           for(m in 9:5) {if(events[m-1] == events[m]) events[m-1] <- events[m-1] - 1}
#           for(m in 1:3) {if(events[m+1] == events[m]) events[m+1] <- events[m+1] + 1}
#           
#           
#           
          ##
          # segmentDF$AutoEDAExtract[events["prestimRow"]] <- "prestimRow"
          segmentDF$CardioExtract[events["onsetRow"]] <- "onsetRow"
          segmentDF$CardioExtract[events["offsetRow"]] <- "offsetRow"
          segmentDF$CardioExtract[events["answerRow"]] <- "answerRow"
          # segmentDF$CardioExtract[events["latencyRow"]] <- "latencyRow"
          # segmentDF$CardioExtract[events["ROWEndRow"]] <- "ROWEndRow"
          if(!is.null(responseOnsetRow)) {
            segmentDF$CardioExtract[events["responseOnsetRow"]] <- "responseOnsetRow"
            segmentDF$CardioExtract[events["responseEndRow"]] <- "responseEndRow"
          }
          # segmentDF$CardioExtract[events["segEndRow"]] <- "endRow"
          # segmentDF$CardioExtract[events["stopRow"]] <- "stopRow"
          
#          segmentDF$CardioMeasure[segOnsetRow:segEndRow] <- rep(responseChange, times=(segEndRow-segOnsetRow+1))
          segmentDF$CardioMeasure[segOnsetRow:segEndRow] <- responseChange
          
          ##################################
          
          # save the segmentDF to the chartDF
          chartDF[startRow:endRow,] <- segmentDF
          
        } # end loop over l events 
        
        # save the chartDF to the seriesDF
        seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        
      } # end loop over K charts
      
      # save the seriesDF to the examDF
      examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over J series
    
    # save the examDF to the global environment 
    assign(paste0(examName, "_Data"), examDF, pos=1)
    
  } # end loop over i exams 
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end newCardioExtractFn() function

####################



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[4]



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15



# newCardioExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE, strictWindow=FALSE, strictROW=FALSE, descentStop=TRUE)


