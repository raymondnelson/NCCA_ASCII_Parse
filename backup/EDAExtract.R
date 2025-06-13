# new script for EDA feature extraction
# 
#
#
################################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

# uniqueExams <- uniqueExams[2]



# data acquisition and analysis parameters
# data sampling rate
# cps <- 30
# prestimulus segment in seconds
# prestimSeg <- 5
# EDA latency period at stimulus onset - respnose onset is not analyzed in this period
# EDALat <- .5
# cardio latency
# CardioLat <- .5
# end of response onset window in seconds after verbal answer
# ROWEnd <- 5
# length of the measurement window (evaluation window) in seconds from stimulus onset
# measuredSeg <- 15


# source the helper function
# source('~/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)


# get a vector of event names that are not measured
# source('~/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)


###########



EDAExtractFn <- function(x=uniqueExams, showNames=TRUE, output=FALSE, EW=FALSE, ROW=FALSE, descentRule=TRUE, ignore=2) {
  # function to iterate over a vector of data frame names 
  # and locate the onset and peak of EDA reactions
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
  #
  ########
  
  # source the helper function
  # source('~/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)
  
  
  # get a vector of event names that are not measured
  # source('~/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
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
    examDF$AutoEDAExtract <- rep("", times=nrow(examDF))
    examDF$AutoEDAMeasure <- rep("", times=nrow(examDF))
    
    # get the names of unique series
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
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
        # k=1
        chartName <- uniqueCharts[k]
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        if(nrow(chartDF)<300) next()
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        if(showNames==TRUE) print(chartName)
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
        
        # excludeEvents was created by sourcing the excludedEvents.R script before the first loop
        eventNames <- eventNames[!(eventNames %in% excludeEvents)]
        
        ### need to manage repeated events 
        
        # repeated events are managed before this stage with the fixDuplicates function
        # the fixDuplicates() function will rename the first iteration of a repeated event
        # keeping the original even name for the second (last) repetition
        # and appending an "a" to the other
        # fixDup() is called in the workFlow script 
        # may be useful to call it here
        
        ###
        ###
        
        # loop over all the events in the chart data frame
        for (l in 1:length(eventNames)) {
          # l=8
          segmentName <- eventNames[l]
          # get the onset row  for the chartDF so that events during the prestimSegment are ignored
          segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
          # get the segment prestim row
          prestimRow <- segOnsetRow - prestimSeg*cps
          if(prestimRow < 1) prestimRow <- 1
          # set the end row for the measured segment
          segEndRow <- segOnsetRow + measuredSeg*cps - 1
          if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
          # set the row number for the end of the data frame segment
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
          
          # onsetRow is always 1
          # onsetRow <- which(segmentDF$Events=="onsetRow")[1]
          
          # get the first offset after the onset row
          offsetRow <- which(segmentDF$Events[segOnsetRow:nrow(segmentDF)]=="offsetRow")[1] + segOnsetRow - 1
          answerRow <- which(segmentDF$Events[segOnsetRow:nrow(segmentDF)]=="answerRow")[1] + segOnsetRow - 1
          # correct for missing or no answer during the measurement segement
          if(is.na(answerRow)) { answerRow=segEndRow-1 }
          
          # remove NA rows
          # segmentDF <- na.omit(segmentDF) # not needed
          
          # fix problem when answerRow == offsetRow
          if(offsetRow == segOnsetRow) offsetRow <- segOnsetRow + 1
          if(answerRow <= offsetRow) answerRow <- offsetRow + 1
          if(offsetRow >= nrow(segmentDF)) offsetRow <- nrow(segmentDF) - 2
          if(answerRow >= nrow(segmentDF)) answerRow <- nrow(segmentDF) - 1
          
          ##################################
          
          dataVector <- segmentDF$c_AutoEDA
          
          extractList <- as.list(c(segOnsetRow, 
                                 offsetRow, 
                                 answerRow, 
                                 segmentName, 
                                 paste(examName, chartName, segmentName, sep="_")) )
          
          extractList[[6]] <- dataVector
                                 
          names(extractList) <- c("begin", 
                                  "end", 
                                  "answer", 
                                  "segmentName", 
                                  "segmentTitle", 
                                  "dataVector")

          #############
          
          ### use the amplitudeExtract function to compute the EDA response 
          extractResult <- amplitudeExtractFn(extractList, 
                                            dataRate=cps,
                                            Lat=EDALat,
                                            ROWEnd=ROWEnd,
                                            nSmooth=ignore,
                                            strictWindow=EW, 
                                            strictROW=ROW, 
                                            descentStop=descentRule
                                            )
          
          # print(extractResult)
          
          # process the ouput of the amplitudeExctract function
          responseOnsetRow <- as.numeric(extractResult["responseOnsetRow"])
          responseEndRow <- as.numeric(extractResult["responsePeakRow"])
          stopRow <- as.numeric(extractResult["stopRow"])
          responseChange <- as.numeric(extractResult["responseChangeValue"])
          
          # make sure that events are on distinct rows
          onsetRow <- segOnsetRow 
          latencyRow <- segOnsetRow + (EDALat * cps)
          ROWEndRow <- answerRow + (ROWEnd * cps)
          if(ROWEndRow >= nrow(segmentDF)) ROWEndRow <- nrow(segmentDF) - 3

          if(responseOnsetRow == (segEndRow - 1)) {
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

          ##
          segmentDF$AutoEDAExtract[events["onsetRow"]] <- "onsetRow"
          segmentDF$AutoEDAExtract[events["offsetRow"]] <- "offsetRow"
          segmentDF$AutoEDAExtract[events["answerRow"]] <- "answerRow"
          if(!is.null(responseOnsetRow)) {
            segmentDF$AutoEDAExtract[events["responseOnsetRow"]] <- "responseOnsetRow"
            segmentDF$AutoEDAExtract[events["responseEndRow"]] <- "responseEndRow"
          }
          segmentDF$AutoEDAMeasure[segOnsetRow:segEndRow] <- responseChange
          
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
  
} # end EDAExtractFn() function

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



# newEDAExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE, strictWindow=FALSE, strictROW=FALSE, descentStop=TRUE, ignore=2)



