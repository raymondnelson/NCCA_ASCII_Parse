# new function to slice a data frame from the time series data
# for an exam, series, chart or stim segment
# 10/23/2016
# Raymond Nelson
#
################################



# library(stringr)

# uses the following environment variables
# cps <- 30
# prestimSeg <- 5
# measuredSeg <- 15

# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)



####



getExamFn <- function(x=uniqueExams, y=examNum, FUN=examFUN) {
  # function to to slice a chart data frame from the large examDF for all series and all charts
  # 10/23/2016
  ###
  # x is a vector of names of unique exams in the global environment
  # examFUN is the function to call for each examDF
  #
  # normally the examFUN will be the getSeriesFn
  #
  # output is a side effect, save the examDF in the global environment,
  # using the unique exam names in the x input vector
  #
  ####
  
  uniqueExams <- x
  examNum <- y
  examFUN <- FUN
  
  # select a single exam from the uniqueExams vector
  if(!is.null(examNum) & examNum !="ALL") {
    uniqueExams <- uniqueExams[examNum]
  } 
  
  # loop over each exam in the list 
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      examStartRow <- 1
      examEndRow <- nrow(examDF)
      
      assign("examDF", examDF, pos=1)
      assign("examName", examName, pos=1)
      # assign("examNum", examNum, pos=1)
      
      if(showNames==TRUE) print(paste("Exam:", examName))
      
      ####
      
      # initialize an output data frame for the getExamFn
      getExamOutputDF <- NULL
      
      # get the names of the unique series for the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
      # select a series to work with
      # if (!is.null(seriesNum)) {
      #   if (seriesNum !="ALL") {
      #     uniqueSeries <- uniqueSeries[seriesNum]
      #   }
      # } 
      
      assign("uniqueSeries", uniqueSeries, pos=1)
      
    }
    
    ####
    
    ### work with the exam DF here
    
    # call the get series function to work with each unique series
    # if(!is.null(seriesNum)) {
    #   examDF <- getSeriesFn(x=examDF, 
    #                         y=uniqueSeries,
    #                         seriesFUN=seriesFUN,
    #                         chartFUN=chartFUN,
    #                         segmentFUN=segmentFUN )
    # }
    
    # call the examFUN
    if(!is.null(examFUN)) {
      # do.call requires the list() arg when using the default args for FUN
      getExamOutputDF <- do.call(examFUN, list())
      if(!is.null(getExamOutputDF)) {
        examDF <- getExamOutputDF
        assign("examDF", examDF, pos=1)
      }
    } 
    
    ####
    
    # save the examDF to the global environment
    if (makeDF == TRUE) {
      assign(paste0(examName, "_Data"), examDF, pos=1)
    }
    
  } # end loop over i exams 
  
  print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end getExamFn() function

# getExamFn()
# getExamFn(x=uniqueExams[12])



# getSegmentFn()
# getSegmentFn(x=uniqueExams, examNum=1, seriesNum=1, chartNum=1, segmentNum=NULL, showNames=TRUE, output=FALSE)
# getSegmentFn(chartNum=2)



####



getSeriesFn <- function(x=examDF,
                        # y=seriesNum,
                        FUN=seriesFUN ) {
  # function to work with each unique series in an exam
  # called by the getExamFn()
  # 10/23/2016
  #
  ####
  
  examDF <- x
  # uniqueSeries <- y
  seriesFUN <- FUN
  # uniqueSeries <- seriesNum2
  
  # select a series to work with
  if (!is.null(seriesNum)) {
    if (seriesNum !="ALL") {
      uniqueSeries <- uniqueSeries[seriesNum]
    }
  } 
  
  # loop over each unique series
  j=1
  for(j in 1:length(uniqueSeries)) {
    
    {
      
      seriesName <- uniqueSeries[j]
      
      # assign("seriesName", seriesName, pos=1)
      
      # get the list of time series data for the charts in the exam
      seriesOnsetRow <- range(which(examDF$seriesName==seriesName))[1]
      seriesEndRow <- range(which(examDF$seriesName==seriesName))[2]
      # seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      # seriesDF <- examDF[seriesOnsetRow:seriesEndRow,]
      # seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      assign("seriesDF", seriesDF, pos=1)
      assign("seriesName", seriesName, pos=1)
      # assign("seriesNum", j, pos=1)
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      ####
      
      # initialize an output data frame for the getExamFn
      getSeriesOutputDF <- NULL
      
      ####
      
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      assign("uniqueCharts", uniqueCharts, pos=1)
      
    }
    
    # select a chart to work with
    # if (!is.null(chartNum)) {
    #   if (chartNum != "ALL") {
    #     uniqueCharts <- uniqueCharts[chartNum]
    #   }
    # }
    
    ####
    
    ### work with the seriesDF here
    
    # call the get charts function to work with the charts
    # if(!is.null(chartNum)) {
    #   seriesDF <- getChartFn(x=seriesDF, 
    #                          y=uniqueCharts,
    #                          chartFUN=chartFUN,
    #                          segmentFUN=segmentFUN )
    # } 
    
    # call the seriesFUN
    if(!is.null(seriesFUN) & !is.null(chartNum)) {
      seriesFUNoutput <- do.call(seriesFUN, list())
      if(!is.null(seriesFUNoutput)) {
        seriesDF <- seriesFUNoutput
        assign("seriesDF", seriesDF, pos=1)
      }
    }
    
    ####
    
    # save the seriesDF to the examDF
    # examDF[seriesOnsetRow:seriesEndRow,] <- seriesDF
    examDF[examDF$seriesName==seriesName,] <- seriesDF
    # not needed when the chartDF is saved directly to the examDF
    # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
    
  } # end if for J series
  
  return(examDF)
  
} # end getSeriesFn()

# getSeriesFn()



####



getChartFn <- function(x=seriesDF, 
                       # y=chartNum,
                       FUN=chartFUN ) {
  # function to work with each unique chart within a series and exam
  # 10/23/2016
  #
  ####
  
  seriesDF <- x
  # chartNum <- y
  chartFUN <- FUN
  
  if (!is.null(chartNum) & chartNum != "ALL") {
    # if (chartNum != "ALL") {
      uniqueCharts <- uniqueCharts[chartNum]
    # }
  }
  
  # loop over each chart in the series 
  k=1
  # if(!is.null(chartNum)) {
  for(k in 1:length(uniqueCharts)) {
    chartName <- uniqueCharts[k]
    
    # get the chart onset and end row within the series
    chartOnsetRow <- range(which(seriesDF$chartName==chartName))[1]
    chartEndRow <- range(which(seriesDF$chartName==chartName))[2]
    
    # get the data frame with the time series data for each chart in the series
    # chartDF <- seriesDF[chartOnsetRow:chartEndRow,]
    chartDF <- seriesDF[seriesDF$chartName==chartName,]
    
    # skip short charts less than 10 seconds
    # if(nrow(chartDF)<600) next()
    
    assign("chartDF", chartDF, pos=1)
    assign("chartName", chartName, pos=1)
    # assign("chartNum", chartNum, pos=1)
    
    ####
    
    # initialize an output data frame for the getExamFn
    getChartOutputDF <- NULL
    
    ####

    if(showNames==TRUE) print(paste("Chart:", chartName))
    
    # a vector of event onset rows for all stimulus events
    eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
    eventNames <- eventNames[!(eventNames %in% excludeEvents)]
    
    if(length(eventNames)==0) eventNames <- NULL
    
    assign("eventNames", eventNames, pos=1)
    
    # select an event segment to work with
    # this needs to be nested in order to avoid an error,
    # from a missing value for segmentNum3 != "ALL",
    # when segmentNum3 is NULL 
    # if (!is.null(segmentNum)) {
    #   if (segmentNum != "ALL") {
    #     eventNames <- eventNames[segmentNum]
    #   }
    # }
    
    ####################   get the first and last events   #####################
    
    # get the onset of the first event and the end of the last event
    # if(length(eventNames)==0) {
    if(is.null(eventNames)) {
      print("no stimulus events. no first or last events")
      # next()
      firstEvent=1
      lastEventEnd=nrow(chartDF)
    } else {
      # bracketed this elese 4-9-2017
      firstEvent <- getFirstLastEventFn(x=chartDF)[1]
      lastEventEnd <- getFirstLastEventFn(x=chartDF)[2]
    }
    
    # fix condition where there are no events that are not excluded
    if(is.na(firstEvent)) {
      firstEvent <- 1
      lastEvent <- nrow(chartDF)
      lastEventEnd <- nrow(chartDF)
    }
        
    # assign the first and last events to the global environment
    assign("firstEvent", firstEvent, pos=1)
    assign("lastEventEnd", lastEventEnd, pos=1)
    
    ####
    
    ### work with the chart DF here 
    
    # call the get segment function to work with the segments
    # if(!is.null(segmentNum)) {
    #   chartDF <- getSegmentFn(x=chartDF,
    #                           y=eventNames,
    #                           segmentNum=segmentNum,
    #                           segmentFUN=segmentFUN )
    # }
    
    # call the chart function
    if(!is.null(chartFUN)) {
      # do.call requires the list() arg when using the default args for the function call)
      chartFUNoutput <- do.call(chartFUN, list())
      if(!is.null(chartFUNoutput)) {
        chartDF <- chartFUNoutput
        assign("chartDF", chartDF, pos=1)
      }
    }
    
    ####
    
    # save the chartDF to the seriesDF
    # seriesDF[chartOnsetRow:chartEndRow,] <- chartDF
    seriesDF[seriesDF$chartName==chartName,] <- chartDF    
    # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
    # save the chartDF to the examDF
    # examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF
     
  } # end for loop over k charts
  
  return(seriesDF)
  
} # end getChartFn()

# getChartFn()



####



getSegmentFn <- function(x=chartDF,
                         # y=eventNames,
                         FUN=segmentFUN ) {
  # function to interate over a vector of stimulus segments
  # x input is the chartDF data frame
  # y input is a vector of stimulus events 
  # output is the chartDF after calling the segmentFUN
  
  chartDF <- x
  # eventNames <- y
  segmentFUN <- FUN
  
  if (!is.null(segmentNum)) {
    if (segmentNum != "ALL") {
      eventNames <- eventNames[segmentNum]
    }
  }
  
  if (length(eventNames) != 0) {
    
    # loop over all the events in the chart data frame
    # if(!is.null(segmentNum)) {
    l=1
    for (l in 1:length(eventNames)) {
      segmentName <- eventNames[l]
      
      # get the onset row using the segment name so that events during the prestimSegment are ignored
      segOnsetRow <- which(toupper(chartDF$eventLabel) == segmentName)[1]
      # set the end row using the evaluation window length 
      segEndRow <- segOnsetRow + measuredSeg*cps - 1
      
      # get the segment prestim row
      prestimRow <- segOnsetRow - prestimSeg*cps
      if(prestimRow < 1) prestimRow <- 1
      if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
      
      # get the segment start row and end row
      startRow <- prestimRow
      endRow <- segEndRow + addSeg*cps
      if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
      
      # prestimRow <- prestimRow - startRow + 1 # this will set the prestim row to 1
      
      # get the segment data frame
      segmentDF <- chartDF[startRow:endRow,]
      
      assign("segmentDF", segmentDF, pos=1)
      assign("segmentName", segmentName, pos=1)
      # assign("segmentNum", segmentNum, pos=1)
      
      # adjust the rows so that row numbers refer to data in the segmentDF not the chartDF
      if(showNames==TRUE) print(segmentName)
      
      ####
      
      # initialize an output data frame for the getExamFn
      getSegmentOutputDF <- NULL
      
      ####
      
      # get the stimulus onset row (usually 151 when there are 5 prestim seconds in the data frame)
      # stimOnsetRow <- segOnsetRow - startRow + 1 # will normally set to 151
      # locate the onset by segmentName to ignore other stimuli during the prestim period
      stimOnsetRow <- which(toupper(segmentDF$eventLabel)==segmentName)[1]
      # stimOnsetRow <- which(segmentDF$Events=="onsetRow")[1]
      
      # get the first offset after the onset row
      # stimoffsetRow is the end of the verbal stimulus while stimEndRow is the end of the EW
      stimOffsetRow <- which(segmentDF$Events[stimOnsetRow:nrow(segmentDF)]=="offsetRow")[1] + stimOnsetRow - 1
      
      # locate the end of the stimulus segment 
      stimEndRow <- segEndRow - startRow + 1 # will normally set to 600
      
      # locate the answer index
      answerRow <- which(segmentDF$Events[stimOnsetRow:nrow(segmentDF)]=="answerRow")[1] + stimOnsetRow - 1
      if(is.na(answerRow)) { answerRow=stimOffsetRow+1 }
      
      # remove NA rows
      # segmentDF <- na.omit(segmentDF)
      
      # fix problem when answerRow == offsetRow
      if(stimOffsetRow == stimOnsetRow) stimOffsetRow <- stimOnsetRow + 1
      if(answerRow <= stimOffsetRow) answerRow <- stimOffsetRow + 1
      if(stimOffsetRow >= nrow(segmentDF)) stimOffsetRow <- nrow(segmentDF) - 2
      if(answerRow >= nrow(segmentDF)) answerRow <- nrow(segmentDF) - 1
      
      # make a list of feature extraction parameters for this segment
      # chartName, seriesName, and examName variables are obtained from the parent environments
      extract.params <- list(onset=stimOnsetRow, offset=stimOffsetRow, answer=answerRow, end=stimEndRow, segName=segmentName, chart=chartName, series=seriesName, exam=examName)
      # assign the extract.params to the global environment
      assign("extract.params", extract.params, pos=1)
      ####
      
      ### work with the segement here
      
      if(!is.null(segmentFUN) & !is.null(segmentNum)) {
        segmentFUNoutput <- do.call(segmentFUN, list())
        if(!is.null(segmentFUNoutput)) {
          segmentDF <- segmentFUNoutput
          assign("segmentDF", segmentDF, pos=1)
        }
      }
      
      ####
      
      # save the segmentDF to the chartDF
      chartDF[startRow:endRow,] <- segmentDF
      # chartDF[segOnsetRow:(nrow(segmentDF)+segOnsetRow-1),] <- segmentDF
      # save the segmentDF tot he examDF
      #examDF[(segmentOnsetRow+chartOnsetRow+seriesOnsetRow-1):(segmentchartEndRow+seriesOnsetRow-1),]  <- chartDF
      
    } # end if for l stimulus event names
    
  } # end if for length(eventNames) != 0
  
  return(chartDF)
  
} # end getSegmentFn()






####  getSegFn() to call the getExamFn and other nested functions #### 


# assign the functions that are called by each layer
# assign these before initializing the getSegFn()
examFUN <- getSeriesFn
seriesFUN <- getChartFn
chartFUN <- getSegmentFn
segmentFUN <- NULL



getSegFn <- function( exam=1,
                      series=1,
                      chart=1,
                      segment=1,
                      examFn=getSeriesFn,
                      seriesFn=getChartFn,
                      chartFn=getSegmentFn,
                      segmentFn=NULL
                      ) {
  # utility function to call the getExamFn() and other nested functions
  ####
  
  # first get and save the current values to restore them later
  oldExamFUN <- get("examFUN", pos=1)
  oldSeriesFUN <- get("seriesFUN", pos=1)
  oldChartFUN <- get("chartFUN", pos=1)
  oldSegmentFUN <- get("segmentFUN", pos=1)
  
  # set the global env to the input values
  assign("examNum", exam, pos=1)
  assign("seriesNum", series, pos=1)
  assign("chartNum", chart, pos=1)
  assign("segmentNum", segment, pos=1)
  
  # set the env functions
  assign("examFUN", examFn, pos=1)
  assign("seriesFUN", seriesFn, pos=1)
  assign("chartFUN", chartFn, pos=1)
  assign("segmentFUN", NULL, pos=1)

  # call the function
  getExamFn()

  # reset the global env
  assign("examNum", oldExamFUN, pos=1)
  assign("seriesNum", oldSeriesFUN, pos=1)
  assign("chartNum", oldChartFUN, pos=1)
  assign("segmentNum", oldSegmentFUN, pos=1)
  
  # reset the evn functions
  assign("examFUN", oldExamFUN, pos=1)
  assign("seriesFUN", oldSeriesFUN, pos=1)
  assign("chartFUN", oldChartFUN, pos=1)
  assign("segmentFUN", oldSegmentFUN, pos=1)
  
  # no visible output
  # visible output to the console is from the getExam, getSeries and other functions
  # print()

}




#### USE THE getSegFn() TO CALL THE OTHER NESTED FUNCTIONS #### 



# # make a function to make a list of unique exams in the global environment
# getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
# 
# # get exam names from the _Data data frames
# print("make a list of unique exams in the global environment")
# uniqueExams <- getUniqueExams(x="*_Data$")
# # uniqueExams <- uniqueExams[1]



examNum <- "ALL"
seriesNum <- "ALL"
chartNum <- "ALL"
segmentNum <- "ALL"


# assign the functions that are called by each layer
examFUN <- getSeriesFn
seriesFUN <- getChartFn
chartFUN <- getSegmentFn
segmentFUN <- NULL



examNum <- 1
seriesNum <- 1
chartNum <- 1
segmentNum <- 4

# getExamFn()

# examFUN <- getSeriesFn
# seriesFUN <- getChartFn
# chartFUN <- getSegmentFn
# segmentFUN <- NULL



# getSegFn(exam=examNum,
#          series=seriesNum,
#          chart=chartNum,
#          segment=segmentNum)





