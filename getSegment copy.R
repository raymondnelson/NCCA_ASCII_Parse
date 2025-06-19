# new function to slice a data frame from the time series data
# for an exam, series, chart or stim segment
# 10/23/2016
# Raymond Nelson
#
################################



# library(stringr)



# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1]



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15



# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)



###############################



getExamFn <- function(x=uniqueExams, 
                      examNum="ALL", 
                      seriesNum="ALL", 
                      chartNum="ALL", 
                      segmentNum="ALL", 
                      examFUN=NULL,
                      seriesFUN=NULL,
                      chartFUN=NULL,
                      segmentFUN=NULL,
                      showNames=TRUE, 
                      output=FALSE,
                      makeDF=TRUE ) {
  # function to to slice a chart data frame from the large examDF for all series and all charts
  # 10/23/2016
  # 
  # x is a vector of names of unique exams in the global environment
  #
  # examNum is the index of an exam in the vector of unique exams in the global environment
  # seriesNum is the index of a unique series within the exam
  # chartNum is the index of the unique charts within the series
  # segmentNum is the index of the unique stimulus event within the chart
  # use NULL when no working with the exam series chart or segment
  # use "ALL" when working with all exams series charts or segments
  # use a number when selecting an exam series chart or segment
  #
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  # makeDF=TRUE will export the data from to the global environment
  #
  # examFUN is the quoted string name of a function to call for each examDF
  # seriesFUN is the quoted string name of a function to call for each series
  # chartFUN is the quoted string name of a function to call for each chart
  # segmentFUN is the quoted string name of a function to call for each segment
  #
  ########
  
  uniqueExams <- x
  
  # i=examNum
  # j=seriesNum
  # k=chartNum
  # l=segmentNum
  
  if(!is.null(examNum) & examNum !="ALL") {
    uniqueExams <- uniqueExams[examNum]
  } 
  
  # loop over each exam in the list 
  i=1
  for(i in 1:length(uniqueExams)) {
    examName <- uniqueExams[i]
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    assign("examDF", examDF, pos=1)
    assign("examName", examName, pos=1)
    assign("examNum", examNum, pos=1)
    
    if(showNames==TRUE) print(paste("Exam:", examName))
    
    #########################
    
    # initialize an output data frame for the getExamFn
    getExamOutputDF <- NULL
    
    # get the names of the unique series for the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    assign("uniqueSeries", uniqueSeries, pos=1)
    
    # select a series to work with
    if (!is.null(seriesNum)) {
      if (seriesNum !="ALL") {
        uniqueSeries <- uniqueSeries[seriesNum]
      }
    } 
    
    ###################################
    
    ### work with the exam DF here
    
    # call the get series function to work with each unique series
    if(!is.null(seriesNum)) {
      examDF <- getSeriesFn(x=examDF, 
                            y=uniqueSeries,
                            seriesNum2=seriesNum,
                            chartNum2=chartNum,
                            segmentNum2=segmentNum,
                            seriesFUN2=seriesFUN,
                            chartFUN2=chartFUN,
                            segmentFUN2=segmentFUN,
                            showNames=TRUE,
                            output=TRUE )
    }
    
    # call the examFUN
    if(!is.null(examFUN)) {
      # do.call requires the list() arg when using the default args for FUN
      examFUNoutput <- do.call(examFUN, list())
      if(!is.null(examFUNoutput)) examDF <- examFUNoutput
    } 
    
    ###################################
    
    # save the examDF to the global environment
    if (makeDF == TRUE) {
      assign(paste0(examName, "_Data"), examDF, pos=1)
    }
    
  } # end loop over i exams 
  
  if(output==TRUE) return(examDF)
  
} # end getExamFn() function

# getExamFn()



# getSegmentFn()
# getSegmentFn(x=uniqueExams, examNum=1, seriesNum=1, chartNum=1, segmentNum=NULL, showNames=TRUE, output=FALSE)
# getSegmentFn(chartNum=2)



###################################################################################



getSeriesFn <- function(x=examDF,
                        y=uniqueSeries,
                        seriesNum2=seriesNum,
                        chartNum2=chartNum,
                        segmentNum2=segmentNum,
                        seriesFUN2=seriesFUN,
                        chartFUN2=chartFUN,
                        segmentFUN2=segmentFUN,
                        showNames=TRUE,
                        output=TRUE ) {
  # function to work with each unique series in an exam
  # called by the getExamFn()
  # 10/23/2016
  #
  ######
  
  examDF <- x
  uniqueSeries <- y
  # uniqueSeries <- seriesNum2
  
  # loop over each unique series
  j=1
  for(j in 1:length(uniqueSeries)) {
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
    assign("seriesNum", j, pos=1)
    
    if(showNames==TRUE) print(paste("series", seriesName))
    
    ############################
    
    # initialize an output data frame for the getExamFn
    getSeriesOutputDF <- NULL
    
    ############################
    
    uniqueCharts <- as.character(unique(seriesDF$chartName))
    
    assign("uniqueCharts", uniqueCharts, pos=1)
    
    # select a chart to work with
    if (!is.null(chartNum2)) {
      if (chartNum2 != "ALL") {
        uniqueCharts <- uniqueCharts[chartNum2]
      }
    }
    
    ###################
    
    ### work with the seriesDF here
    
    # call the get charts function to work with the charts
    if(!is.null(chartNum2)) {
      seriesDF <- getChartFn(x=seriesDF, 
                             y=uniqueCharts,
                             chartNum3=chartNum2,
                             segmentNum3=segmentNum2,
                             chartFUN3=chartFUN2,
                             segmentFUN3=segmentFUN2,
                             showNames=TRUE, 
                             output=TRUE )
    } 
    
    # call the seriesFUN
    if(!is.null(seriesFUN2) & !is.null(chartNum2)) {
      seriesFUN2output <- do.call(seriesFUN2, list())
      if(!is.null(seriesFUN2output)) seriesDF <- seriesFUN2output
    }
    
    ###################
    
    # save the seriesDF to the examDF
    # examDF[seriesOnsetRow:seriesEndRow,] <- seriesDF
    examDF[examDF$seriesName==seriesName,] <- seriesDF
    # not needed when the chartDF is saved directly to the examDF
    # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
    
  } # end if for J series
  
  return(examDF)
  
} # end getSeriesFn()

# getSeriesFn()



############################################################



getChartFn <- function(x=seriesDF, 
                       y=uniqueCharts,
                       chartNum3=chartNum2,
                       segmentNum3=segmentNum2,
                       chartFUN3=chartFUN2,
                       segmentFUN3=segmentFUN2,
                       showNames=TRUE, 
                       output=TRUE ) {
  # function to work with each unique chart within a series and exam
  # 10/23/2016
  #
  ######
  
  seriesDF <- x
  uniqueCharts <- y
  
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
    assign("chartNum", chartNum3, pos=1)
    
    ##################
    
    # initialize an output data frame for the getExamFn
    getChartOutputDF <- NULL
    
    ###################

    if(showNames==TRUE) print(paste("Chart:", chartName))
    
    # a vector of event onset rows for all stimulus events
    eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
    eventNames <- eventNames[!(eventNames %in% excludeEvents)]
    
    assign("eventNames", eventNames, pos=1)
    
    
    # select an event segment to work with
    # this needs to be nested in order to avoid an error,
    # from a missing value for segmentNum3 != "ALL",
    # when segmentNum3 is NULL 
    if (!is.null(segmentNum3)) {
      if (segmentNum3 != "ALL") {
        eventNames <- eventNames[segmentNum3]
      }
    }
    
    ##################################
    
    ### work with the chart DF here 
    
    # call the get segment function to work with the segments
    if(!is.null(segmentNum3)) {
      chartDF <- getSegmentFn(x=chartDF,
                              y=eventNames,
                              segmentNum4=segmentNum3,
                              segmentFUN4=segmentFUN3,
                              showNames=TRUE,
                              output=TRUE )
    }
    
    # call the chart function
    if(!is.null(chartFUN3)) {
      # do.call requires the list() arg when using the default args for the function call)
      chartFUN3output <- do.call(chartFUN3, list())
      if(!is.null(chartFUN3output)) chartDF <- chartFUN3output
    }
    
    assign("chartDF", chartDF, pos=1)
    
    ##################################
    
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



############################################################



getSegmentFn <- function(x=chartDF,
                         y=eventNames,
                         segmentNum4=segmentNum3,
                         segmentFUN4=segmentFUN3,
                         showNames=TRUE,
                         output=TRUE ) {
  # function to interate over a vector of stimulus segments
  # x input is the chartDF data frame
  # y input is a vector of stimulus events 
  # output is a 
  
  chartDF <- x
  eventNames <- y
  
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
      assign("segmentNum", segmentNum4, pos=1)
      
      # adjust the rows so that row numbers refer to data in the segmentDF not the chartDF
      if(showNames==TRUE) print(segmentName)
      
      #######################
      
      # initialize an output data frame for the getExamFn
      getSegmentOutputDF <- NULL
      
      ###############################################
      
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
      
      ########################################
      
      ### work with the segement here
      
      if(!is.null(segmentFUN4) & !is.null(segmentNum4)) {
        segmentFUN4output <- do.call(segmentFUN4, list())
        if(!is.null(segmentFUN4output)) segmentDF <- segmentFUN4output
      }
      
      ########################################
      
      # save the segmentDF to the chartDF
      chartDF[startRow:endRow,] <- segmentDF
      # chartDF[segOnsetRow:(nrow(segmentDF)+segOnsetRow-1),] <- segmentDF
      # save the segmentDF tot he examDF
      #examDF[(segmentOnsetRow+chartOnsetRow+seriesOnsetRow-1):(segmentchartEndRow+seriesOnsetRow-1),]  <- chartDF
      
    } # end if for l stimulus event names
    
  } # end if for length(eventNames) != 0
  
  return(chartDF)
  
} # end getSegmentFn()



# getExamFn(x=uniqueExams,
#           examNum=1,
#           seriesNum=1,
#           chartNum=4,
#           segmentNum=4,
#           examFUN=NULL,
#           seriesFUN=NULL,
#           chartFUN=NULL,
#           segmentFUN=NULL,
#           showNames=TRUE,
#           output=FALSE,
#           makeDF=TRUE )




