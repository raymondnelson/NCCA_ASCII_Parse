# new R function to calculate scores 
# with various scoring and ranking methods
# Mar 13, 2020
# Raymond Nelson
#
####


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


# source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
# source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)

# source the amplitudeExtractHelperFunctions.R script to load the spd function
# for the population st dev
# source(paste0(RPath, 'amplitudeExtractHelperFunctions.R'), echo=FALSE)



{
  
  # source the scripts for algorithm functions
  
  # output functions for score and measurement tables
  source(paste0(RPath, 'outputScores.R'), echo=FALSE)
  
  source(paste0(RPath, 'rbpfProb.R'), echo=FALSE)
  
  source(paste0(RPath, 'sigProc_filters.R'), echo=FALSE)
  
  
  # source the sigProcHelper.R script to load the getFirstLastEventFn()
  source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
  
  # source some scripts to calculate various types of rank scores
  source(paste0(RPath, 'rankScores.R'), echo=FALSE)
  source(paste0(RPath, 'RRMScores.R'), echo=FALSE)
  source(paste0(RPath, 'miritelloRank.R'), echo=FALSE)
  source(paste0(RPath, 'ipsativeZScore.R'), echo=FALSE)
  
  # source a script to obtain the response onset and peak indices
  # source(paste0(RPath, 'questionTimes.R'), echo=TRUE)
  
  # source some scripts for the scoring algorithms
  source(paste0(RPath, 'ESSMScores.R'), echo=FALSE)
  source(paste0(RPath, 'OSS3Scores.R'), echo=FALSE)
  source(paste0(RPath, 'OSS2Scores.R'), echo=FALSE)
  source(paste0(RPath, 'PAScores.R'), echo=FALSE)
  source(paste0(RPath, 'ROSSScores.R'), echo=FALSE)
  source(paste0(RPath, 'PSSScores.R'), echo=FALSE)
  source(paste0(RPath, 'bootstrapScores.R'), echo=FALSE)
  
  
  # decision rules for scoring algorithms 
  source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
  source(paste0(RPath, 'autoSelectTSRSSR.R'), echo=FALSE)
  source(paste0(RPath, 'KWANOVA.R'), echo=FALSE)
  
  
  #### LXCAT algorithm
  source(paste0(RPath, 'LXCATScores.R'), echo=FALSE)
  
}



####



sdp <- function(x, na.rm=TRUE) {
  # function to compute a population standard deviation
  # uses the R sample variance function
  # x input is a vector of numeric values
  # output is the population standard deviation
  (sqrt(var(x, na.rm=na.rm)*(length(x)-1)/length(x)))
}



################### main function #####################################



newScoresFn <- function(uniqueExams=uniqueExams, 
                        showNames=TRUE, 
                        makeDF=FALSE, 
                        saveCSV=FALSE, 
                        output=FALSE) {
  # new R function to calculate scores 
  # with various scoring and ranking method
  # Mar 13, 2020
  # Raymond Nelson
  #
  # uniqueExams is a vector of names of data frames 
  # that contain the _Measurements data 
  #
  # show names will print the exam name to the console
  # makeDF will assign a data frame to the global envir for each algorithm
  # saveCSV will create a text .csv in the cwd for each scoring method
  # output TRUE will return the measurement data frame
  #
  # uses the showNames and output settings from the global environment
  # set in the NCCAASCII_init.R script
  #
  # other parameters may be obtained from the global environment
  #
  ####
  
  ##### iterate over each exam in the list #####
  
  options(warn = 2)
  
  # stop()
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      #### get the measurements data frame for this exam ####
      
      examName <- uniqueExams[i]
      # get the names of time series lists for all unique series in each exam
      
      searchString <- paste0("*", examName, "_Measurements", "*")
      if(!exists("searchString")) return()
      
      # the _Measurements data frame 
      # is initialized by a function in the extractMeasurement.R script
      
      # initialize the measurementDF for the exam
      measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      # View(measurementDF)
      if(is.null(measurementDF)) return()
      
      measurementDF$examName <- as.character(measurementDF$examName)
      measurementDF$seriesName <- as.character(measurementDF$seriesName)
      measurementDF$chartName <- as.character(measurementDF$chartName)
      
      # examStartRow <- 1
      # examEndRow <- nrow(measurementDF)
      
      assign("measurementDF", measurementDF, pos=1)
      assign("examName", examName, pos=1)
      assign("i", i, pos=1)
      
      # write.csv(measurementDF, file=paste0(str_sub(searchString, 2,-2), ".csv"), row.names=FALSE)
      # library(readr)
      # write_csv(measurementDF, path=paste0(str_sub(searchString, 2,-2), ".csv"))
      
      if(showNames==TRUE) {
        print(examName)
        print(paste(i, "of", length(uniqueExams)))
      }
      
      # get the names of unique series
      uniqueSeries <- as.character(unique(measurementDF$seriesName))
      
    }
    
    #### coerce these from factor variables to character ####
    
    {
      
      ## no longer necessary ##
      
      # measurementDF$rankScore <- as.character(measurementDF$rankScore)
      # measurementDF$RRMScore <- as.character(measurementDF$RRMScore)
      # measurementDF$miritelloRankScore <-
      #   as.character(measurementDF$miritelloRankScore)
      # measurementDF$ipZScore <- as.character(measurementDF$ipZScore)
      # 
      # measurementDF$RCScore <- as.character(measurementDF$RCScore)
      # measurementDF$CQName <- as.character(measurementDF$CQName)
      # measurementDF$CQMean <- as.character(measurementDF$CQMean)
      # 
      # measurementDF$ESSScore <- as.character(measurementDF$ESSScore)
      # measurementDF$OSS3Score <- as.character(measurementDF$OSS3Score)
      # measurementDF$OSS2Score <- as.character(measurementDF$OSS2Score)
      # measurementDF$PAScore <- as.character(measurementDF$PAScore)
      # measurementDF$ROSSScore <- as.character(measurementDF$ROSSScore)
      # measurementDF$PSSScore <- as.character(measurementDF$PSSScore)
      # measurementDF$bootstrapScore <-
      #   as.character(measurementDF$bootstrapScore)
      # 
      # measurementDF$PCATScore <- as.character(measurementDF$PCATScore)
      # 
      # assign("measurementDF", measurementDF, pos=1)
      # # assign("measurementDF", measurementDF, env=.GlobalEnv)
      # # View(measurementDF)
      
    }
    
    #### create an output list to hold the analysis results ####
    
    {
      
      analysisResultList <- as.list(NULL)
      analysisListName <- paste0(examName, "_ANALYSIS")
      # assign("analysisResultList", analysisResultList, envir=.GlobalEnv)
      assign(analysisListName, analysisResultList, envir=.GlobalEnv)
      # each scoring algorithm will access and add to this list
      
    }
    
    ##### iterate over each unique series #####
    
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      #### initialize the measurement data frame for the series ####
      
      {
        
        ## slice the measurement data frame for this chart 
        
        seriesName <- uniqueSeries[j]
        # get the list of time series data for the charts in the exam
        
        # keep only the measurements for the selected series
        seriesRows <- which(measurementDF$seriesName==seriesName)
        
        # now slice the seriesMeasurementDF
        seriesMeasurementDF <- measurementDF[seriesRows,]
        # View(seriesMeasurementDF)
        
        assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
        assign("seriesName", seriesName, pos=1)
        assign("j", j, pos=1)
        
      }
      
      #### exclude repeated RQs and CQs from the analysis ####
      
      {
        
        # Nov 12, 2023
        chartNameVc <- str_sub(seriesMeasurementDF$chartName, 1, 2)
        
        allEvents <- 
          toupper(paste0(chartNameVc, seriesMeasurementDF$eventLabel))
        
        # located duplicated events by the "A" char at the end
        # located by row number
        dupEvntsA <- which(str_sub(allEvents,  -1, -1) == "A" & 
                             str_sub(allEvents,  -2, -1) != "SA")
        # seriesMeasurementDF$eventLabel[dupEvntsA]
        
        # allEvents[dupEvntsA]
      
        # Nov 20, 2021
        # need a better way of removing duplicates
        # this method removes duplicate annotations between CQ RQ pairs
        
        # Dec 15, 2021 annotations are no longer marked as duplicates
        
        if(length(dupEvntsA) > 0) {
          
          # events that are repeated 
          # dupEvents <- 
          #   which(allEvents %in%  str_sub(allEvents[dupEvntsA], 1, -2))
          dupEvents <- NULL
            
          # locate the non-duplicated event rows to keep
          # Nov 13, 2023 improved this line 
          keepRows <- which(!(c(1:nrow(seriesMeasurementDF)) %in% sort(unique(c(dupEvents, dupEvntsA)))))

          # fix the RqCqDFSeries without  the repeated questions
          seriesMeasurementDF <- seriesMeasurementDF[keepRows,]

          # then fix the RqCqSeriesRows
          seriesRows <- seriesRows[keepRows]
          # used to submit the RqCqDFSeries back to the seriesDF later
          
        }
        
      }
      
      #### remove excluded stimulus events ####
      
      {
        
        # # Nov 20, 2021
        # 
        # allEvents <- toupper(seriesMeasurementDF$eventLabel)
        # 
        # keepThese <- which(!(allEvents %in% excludeQuestions))
        # 
        # seriesMeasurementDF <- seriesMeasurementDF[keepThese,]
        # # then fix the RqCqSeriesRows
        # # to submit the RqCqDFSeries back to the seriesDF later
        # seriesRows <- seriesRows[keepThese]
        
      }
      
      #### remove excludeAnnotations ####
      
      {
        
        # Nov 20, 2021
        # removing these here will cause CQ selection to skip over annotations
        
        allEvents <- toupper(seriesMeasurementDF$eventLabel)
        
        keepThese <- which(!(allEvents %in% excludeAnnotations))
        
        seriesMeasurementDF <- seriesMeasurementDF[keepThese,]
        # then fix the RqCqSeriesRows
        # to submit the RqCqDFSeries back to the seriesDF later
        seriesRows <- seriesRows[keepThese]
        
      }
      
      #### initialize a vector of unique sensor names ####
      
      {
        
        uniqueSensors <- as.character(unique(seriesMeasurementDF$sensorName))
        
        # initialize a vector of unique events
        # measurementDF does not include X and XX announcements
        uniqueEventsSeries <- unique(seriesMeasurementDF$eventLabel)
        
        if(length(uniqueEventsSeries) == 0) {
          print("no events")
          return()
        }
        
      }
      
      #### initialize a data frame with the question sequence ####
      
      {
        
        # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
        questionSequenceDF <- questionSequenceFn(measurementDF=seriesMeasurementDF,
                                                 outputName="questionSequenceDF",
                                                 makeDF=FALSE,
                                                 saveCSV=FALSE)
        
        # get the analysis list from the global envir
        analysisResultList <- get(analysisListName, envir=.GlobalEnv)
        
        # add a series list to the analysis list
        analysisResultList[[paste("series", seriesName, sep="_")]] <- 
          as.list(c(seriesName=paste("series", seriesName)))
        
        # add the question sequence to the analysis list
        analysisResultList[[paste("series", seriesName, sep="_")]][["questionList"]] <- 
          questionSequenceDF
        
        # assign the analysis list to the global envir
        assign(analysisListName, analysisResultList, envir=.GlobalEnv)
        
        # assign the seriesMeasurementDF to the global envir for inspection
        assign("seriesMeasurementDF", seriesMeasurementDF, envir=.GlobalEnv)
        
        # View(seriesMeasurementDF)
        
      }
      
      #### exclude repeated RQs and CQs from the RqCqDFSeries ####

      if(length(grep("[CR]+", seriesMeasurementDF$eventLabel)) > 0) {

        # # only for CQT exam chart with CQs and RQs
        # # keep all events for other charts for rank order only
        # 
        # # stop()
        # 
        # allEvents <- paste0(RqCqDFSeries$chartName, RqCqDFSeries$eventLabel)
        # allEvents <- toupper((allEvents))
        # 
        # # repeated events
        # dupEvntsA <- which(str_sub(allEvents,  -1, -1) == "A")
        # 
        # if(length(dupEvntsA) > 0) {
        #   # events that are repeated
        #   dupEvents <-
        #     which(allEvents %in%  str_sub(allEvents[dupEvntsA], 1, -2))
        # 
        #   keepRows <- which(!(c(1:nrow(RqCqDFSeries)) %in% c(dupEvents, dupEvntsA)))
        #   # fix the RqCqDFSeries without  the repeated questions
        #   RqCqDFSeries <- RqCqDFSeries[keepRows,]
        # 
        #   # fix the RqCqSeriesRows
        #   # to submit the RqCqDFSeries back to the seriesDF later
        #   RqCqSeriesRows <- RqCqSeriesRows[keepRows]
        # }
        
        # View(RqCqDFSeries)

      }
      
      ####  initialize the RqCqDF data frame for the series  ####
      
      if(length(uniqueEventsSeries) != 0) {
        
        assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
        
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
          # remove excluded events 2-8-2021
          excludeRows <- which(seriesMeasurementDF$eventLabel %in% excludeEvents)
          RqCqSeriesRows <- RqCqSeriesRows[!(RqCqSeriesRows %in% excludeRows)]
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
          # first event is already excluded from feature extraction
          RqCqSeriesRows <- which(seriesMeasurementDF$eventLabel %in% 
            (unique(seriesMeasurementDF$eventLabel[RqCqSeriesRows])[]))
        }
        
        # return the measurementDF and exit if no stimulus events
        if(length(RqCqSeriesRows) == 0) {
          if(output == TRUE) {
            return(measurementDF)
          } else return()
        }
        
        # initialize the RqCqDF for the series
        RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
        # View(RqCqDFSeries)
        
        # write the RqCqDFSeries to a .csv file
        
        write_csv(RqCqDFSeries, file=paste(examName, seriesName, "RqCqDFSeries.csv", sep="_"))
        # stop()
        
        # exclude charts with < 2 RQs or < 2 CQs
        # some examiners still use 1 series for ACQT and CQT charts
        {
          uniqueCharts <- unique(RqCqDFSeries$chartName)
          # initialize a vector of rows to keep
          keepChartRows <- NULL
          # iterate over the charts
          k=1
          for(k in 1:length(uniqueCharts)) {
            thisChart <- uniqueCharts[k]
            theseChartRows <- which(RqCqDFSeries$chartName == thisChart)
            theseEvents <- RqCqDFSeries[theseChartRows,"eventLabel"]
            # number of RQs 
            theseRQRows <- grepl("[R]+", theseEvents)
            numberRQs <- length(unique(theseEvents[which(theseRQRows)]))
            # number of CQs
            theseCQRows <- grepl("[C]+", theseEvents)
            numberCQs <- length(unique(theseEvents[which(theseCQRows)]))
            # now check the number of RQs and CQs
            # and and exclude charts with < 2 RQs or < 2 CQs
            if(numberRQs >= 2 && numberCQs >= 2) {
              keepChartRows <- c(keepChartRows, theseChartRows)
              # adjust the RqCqSeriesRows
              # RqCqSeriesRows <- RqCqSeriesRows[keepChartRows]
            } else {
              # keep all events if not 2 CQs and 2 RQs
              keepChartRows <- c(keepChartRows, theseChartRows) 
            }
              # end if
          } # end iteration over charts
          # reselect
          RqCqSeriesRows <- RqCqSeriesRows[keepChartRows]
          # RqCqSeriesRows <- keepChartRows
          RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
          # RqCqDFSeries <- RqCqDFSeries[keepChartRows,]
          
          uniqueRQs <- 
            unique(RqCqDFSeries$eventLabel[grep("R", RqCqDFSeries$eventLabel)])
          uniqueCQs <- 
            unique(RqCqDFSeries$eventLabel[grep("C", RqCqDFSeries$eventLabel)])
          
          # we still have the series rows for the RQ and CQ charts
          
          if(isTRUE(writeCSV)) {
            rqcqfilename <- paste0(str_sub(searchString, 2, -15), "_RQCQDFSeries_", seriesName, ".csv")
            write.csv(RqCqDFSeries, file=rqcqfilename, row.names=FALSE)
          }
          
          # assign("RqCqSeriesRows", RqCqSeriesRows, pos=1)
          
        }
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # View(RqCqDFSeries)
        
        if(nrow(RqCqDFSeries) <= 20) next()
        
      }
      
      #### next series if the RqCqDFSeries has no rows ####
      
      if(nrow(RqCqDFSeries)==0) return()
      
      #### exclude the pneumo measurements using a parameter ####
      
      {
        
        if(!isTRUE(includePneumoData)) {
          pneumoRows <- 
            which(RqCqDFSeries$sensorName %in% c("UPneumo", "LPneumo"))
          # save the pneumo sensor meassurements for later 
          pneumoMeasurements <- RqCqDFSeries$sensorMeasurement[pneumoRows]
          # set them to zero for now
          RqCqDFSeries$sensorMeasurement[pneumoRows] <- 0
          
          # may also need to exclude pneumo from  the seriesMeasurementDF
          pneumoRowsSeriesMeasurements <- 
            which(seriesMeasurementDF$sensorName %in% c("UPneumo", "LPneumo"))
          # save the measurements from the seriesMeasurementDF
          pneumoMeasurementsSeriesMeasuremnts <- 
            seriesMeasurementDF$sensorMeasuremen[pneumoRowsSeriesMeasurements]
        }
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
        
        # View(RqCqDFSeries)
        # View(seriesMeasurementDF)
        
      } 
      
      #### get the RQs and CQs and stop if uniqueRQs != uniqueCQs #####
      
      if(isTRUE(checkRQCQs)) {
        
        # check if equal number of RQs and CQs
        # set in the workFlow_init.R script
        
        if( (length(uniqueRQs) != 3) || (length(uniqueRQs) != length(uniqueCQs)) ) {
          print(examName)
          stop("unequal RQs and CQs")
        }
        
      }
      
      ######### check for DLST DLDT and PCAT type charts #########
      
      {
        
        # Oct 7, 2025
        # DLST exams must have 1 chart per series
        
      }
       
      ################## call the scoring methods #################
      
      {
        # scores are calculated for each series
        # some scoring methods work chart by chart
        # some aggregate between charts before assigning scores
      }
      
      ####    calculate the rank scores for the chart    ####
      
      if(isTRUE(getRankScores)) {
        
        # source(paste0(RPath, 'rankScores.R'), echo=FALSE)
        
        # if(seriesName == "2" || seriesName == 2) stop()
        
        # assign("RqCqDFSeries", RqCqDFSeries, env=.GlobalEnv)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, env=.GlobalEnv)
        # assign("analysisListName", analysisListName, env=.GlobalEnv)
        # assign("makeDF", makeDF, env=.GlobalEnv)
        # assign("saveCSV", saveCSV, env=.GlobalEnv)
        # stop()
        
        # use a function to get the rank scores for each chart in the series
        # source(paste0(RPath, 'rankScores.R'), echo=TRUE)
        RqCqDFSeries <- rankScoreFn(RqCqDFSeries=RqCqDFSeries, 
                                    makeDF=FALSE,
                                    saveCSV=FALSE,
                                    analysisListName=analysisListName )
        
        # RqCqDFSeries$rankScore <- rankScores
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # View(RqCqDFSeries)
        
        # pass the RqCqDFSeries to the series measurement data frame
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        # save to the global environment for inspection
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
        ### 11-9-2016 need to find out how ranks are computed for combined pneumo
        
      }
      
      ####  calculate the RRM relative response magnitude scores  ####
      
      if(isTRUE(getRRMScores)) {
        
        # source(paste0(RPath, 'RRMScores.R'), echo=FALSE)
        
        # use a function to get the RRM scores
        # source(paste0(RPath, 'RRMScore.R', echo=TRUE)
        RqCqDFSeries <- RRMScoreFn(RqCqDFSeries=RqCqDFSeries, 
                                   makeDF=FALSE,
                                   saveCSV=saveCSV,
                                   analysisListName=analysisListName )
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # View(RqCqDFSeries)
        
        # pass the RqCqDFSeries to the series measurement data frame
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        # save to the global environment for inspection
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
        ### 11/6/2016 need to combine the RRM scores for upper and lower pneumo into a single score
        
      }
      
      #### calculate the Miritello rank scores for the chart ####
      
      if(isTRUE(getMiritelloRankScores)) {
        
        # use a function to get the Miritello rank scores
        # source(paste0(RPath, 'miritelloRank.R'), echo=TRUE)
        RqCqDFSeries <- miritelloRankFn(RqCqDFSeries=RqCqDFSeries, 
                                        makeDF=FALSE,
                                        saveCSV=saveCSV,
                                        analysisListName=analysisListName )
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # View(RqCqDFSeries)
        
        # pass the RqCqDFSeries to the series measurement data frame
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        # save to the global environment for inspection
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
      }
      
      #### calculate the ipsative Z scores for the chart ####
      
      if(isTRUE(getIpsativeZScores)) {
        
        # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # if(seriesName == "3") stop()

        # use a function to get the Miritello rank scores
        # source(paste0(RPath, 'ipsativeZScore.R'), echo=TRUE)
        
        RqCqDFSeries <- ipsativeZFn(RqCqDFSeries=RqCqDFSeries, 
                                    makeDF=FALSE,
                                    saveCSV=saveCSV,
                                    analysisListName=analysisListName )
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # View(RqCqDFSeries)
        
        # pass the RqCqDFSeries to the series measurement data frame
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        # save to the global environment for inspection
        # assign("chartMeasurementDF", chartMeasurementDF, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
      }
      
      #### pass the RQCQDFSeries back to the seriesDF and measurementDF ####
      
      {
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # View(RqCqDFSeries)
        
        # pass the RqCqDFSeries to the series measurement data frame
        seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
        
        # pass the seriesMeasurementDf back to the measurementDF
        measurementDF[seriesRows,] <- seriesMeasurementDF
        
        # save to the global environment for inspection
        # assign("chartMeasurementDF", chartMeasurementDF, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
      }
      
      #### exit if not 2 RQs and 2 CQs in this series ####
      
      {
        
        rqRows <- grep("R", seriesMeasurementDF$Label)
        cqRows <- grep("C", seriesMeasurementDF$Label)
        
        uniqueRQs <- unique(seriesMeasurementDF$Label[rqRows])
        uniqueCQs <- unique(seriesMeasurementDF$Label[cqRows])
        
        if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) {
          
          if(makeDF==TRUE) {
            assign(paste0(examName, "_Measurements"), measurementDF, pos = 1)
          }
          
          # if(output == TRUE) {
          #   return(measurementDF)
          # } else {
          #   return()
          # }
          
          next()
          
        }
        
      }
      
      #### calculate the R/C scores ####
      
      # R/C scores are needed for ESS-M
      
      # if(isTRUE(getOSS3Scores) && !isTRUE(getRCScores)) { 
      #   getRCScores <- TRUE 
      # }
      
      if(isTRUE(getRCScores)) {
        
        # use a function to get the R/C scores for the chart
        # source(paste0(RPath, 'RCScores.R'), echo=TRUE)
        # will call the selectCQFn function from the selectCQ.R script
        # source(paste0(RPath, 'selectCQ.R'), echo=FALSE)
        
        # need the R/C scores for all charts
        # before calculating ESS for all charts
        # so that the selectCQFn can evaluate all events in the question sequence
        
        # assign("RqCqDFSeries", RqCqDFSeries, envir = .GlobalEnv)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        
        # View(RqCqDFSeries)
        # View(seriesMeasurementDF)
        
        # if(examName == "DCIVSL") {
        #   stop()
        # }
        
        # use the seriesMeasurementDF
        # so that the entire question sequence can be used to select the CQ
        seriesMeasurementDF <- RCScoresFn(seriesMeasurementDF=seriesMeasurementDF,
                                          useMean=FALSE,
                                          makeDF=FALSE,
                                          saveCSV=FALSE,
                                          assignOutputList=FALSE,
                                          analysisListName=analysisListName )
        # View(seriesMeasurementDF)

        assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
        
        # re-acquire the RqCqDFSeries now that we have the RC Scores
        RqCqDFSeries <- seriesMeasurementDF[RqCqSeriesRows,]
        # View(RqCqDFSeries)
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        
        # Nov 20, 2021
        # use the RqCqDFSeries
        # will exclude annotations
        # RqCqDFSeries <- RCScoresFn(seriesMeasurementDF=RqCqDFSeries,
        #                                   useMean=FALSE,
        #                                   makeDF=FALSE,
        #                                   saveCSV=FALSE,
        #                                   assignOutputList=FALSE,
        #                                   analysisListName=analysisListName )
        # # View(RqCqDFSeries)
        
        # pass the RqCqDFSeries to the series measurement data frame
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        # assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
        
        # pass the seriesMeasurementDf back to the measurementDF
        measurementDF[seriesRows,] <- seriesMeasurementDF
        
        # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
        # View(measurementDF)
        
        # View(seriesMeasurementDF)
        # stop()
        
      }
      
      #### calculate the ESS-M integer scores ####
      
      if(isTRUE(getESSMScores)) {
        
        # stop()
        
        # source(paste0(RPath, 'ESSMScores.R'), echo=TRUE)
        
        # need the R/C scores for all charts
        # before calculating ESS for all charts
        RqCqDFSeries <- ESSMScoresFn(RqCqDFSeries=RqCqDFSeries,
                                     essmPrior=essmPrior,
                                     essmAlphas=essmAlphas,
                                     ESSMDecisionRule=ESSMDecisionRule,
                                     makeDF=makeDF,
                                     saveCSV=saveCSV,
                                     analysisListName=analysisListName )
        
        # View(RqCqDFSeries)
        
        # pass the RqCQDFSeries back to the seriesMeasurementDF
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        # View(seriesMeasurementDF)
        # View(measurementDF)
        # stop()
        
      }
      
      #### calculate the OSS-3 scores ####
      
      # OSS- 3 is for single or multiple issue exams with 2-4 CQs and 3-5 charts
      # OSS-3 compares each RQ to the mean CQ
      
      if(isTRUE(getOSS3Scores)) { 
        
        # stop()
        
        # if(examName=="DXDB629GX") stop("stop OSS-3")
        
        # source(paste0(RPath, 'OSS3Scores.R'), echo=TRUE)
        
        if(!exists("OSS3DecisionRule")) OSS3DecisionRule <- "TSR"
        
        RqCqDFSeries <- OSS3ScoresFn(RqCqDFSeries=RqCqDFSeries,
                                     OSS3Alpha=OSS3Alpha,
                                     OSS3DecisionRule=OSS3DecisionRule, 
                                     minPresentations=1,
                                     minSensorScores=1,
                                     makeDF=FALSE,
                                     saveCSV=saveCSV,
                                     analysisListName=analysisListName,
                                     saveAnalysisTXT=TRUE )
        
        # View(RqCqDFSeries)
        
        # stop()
        
        # pass the RqCQDFSeries back to the seriesMeasurementDF
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        # View(seriesMeasurementDF)
        # View(measurementDF)
        
      }
      
      #### calculate the Probability Analysis scores ####
      
      # probability analysis requires the entire series including all charts 
      # unlike other algorithms that work with one chart at a time
      
      # there is always a first chart, even when not questions 
      
      if(isTRUE(getPAScores)) {
        
        # stop()
        
        RqCqDFSeries <- PAScoresFn(RqCqDFSeries=RqCqDFSeries,
                                   forced=TRUE,
                                   PADecisionRule=PADecisionRule,
                                   PAPrior=.5,
                                   PACutProbT=.7,
                                   PACutProbD=.3,
                                   makeDF=FALSE,
                                   saveCSV=FALSE,
                                   analysisListName=analysisListName )
        
        # View(RqCqDFSeries)
        
        # pass the RqCQDFSeries bac to the seriesMeasurementDF
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        # View(seriesMeasurementDF)
        
      }
      
      #### calculate the OSS-2 scores for the series ####
      
      if(isTRUE(getOSS2Scores)) {
        
        # stop()
        
        # OSS-2 is for 3 questions single issue exams with 3 CQs and 3 charts
        # OSS-2 compares each RQ to the preceding CQ
        # OSS-2 differs only slightly from OSS-1
        # for which Fed ZCT R5 is compared to the bracketing CQ with the greater
        # change in physiological activity
        
        RqCqDFSeries <- OSS2ScoresFn(RqCqDFSeries=RqCqDFSeries,
                                     forced=TRUE,
                                     OSS2DecisionRule=OSS2DecisionRule,
                                     oss2AlphaT=oss2AlphaT,
                                     oss2AlphaD=oss2AlphaD,
                                     makeDF=FALSE,
                                     saveCSV=saveCSV,
                                     analysisListName=analysisListName )
        
        # View(RqCqDFSeries)
        
        # pass the RqCQDFSeries back to the seriesMeasurementDF
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
        # View(seriesMeasurementDF)
        # View(measurementDF)
        
      }
      
      #### calculate the Honts & Driscoll (1987) ROSS Score ####
      
      if(isTRUE(getROSSScores)) {
        
        if(!exists("ROSSDecisionRule")) ROSSDecisionRule <- "GTR"
        
        # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # if(seriesName == "3") stop()
        # stop()
        
        RqCqDFSeries <- ROSSScoresFn(RqCqDFSeries=RqCqDFSeries,
                                     ROSSDecisionRule=ROSSDecisionRule,
                                     forced=TRUE,
                                     makeDF=FALSE,
                                     saveCSV=saveCSV,
                                     analysisListName=analysisListName )
        
        # View(RqCqDFSeries)
        
        # pass the RqCQDFSeries back to the seriesMeasurementDF
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        # View(seriesMeasurementDF)
        # View(measurementDF)
        
      }
      
      #### calculate the MacLaren Krapohl 2003 Permutation Score ####
      
      if(isTRUE(getPSSScores)) {

        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # if(seriesName == "3") stop()
        # stop()
        
        if(!exists("PSSDecisionRule")) PSSDecisionRule <- "GTR"
        
        RqCqDFSeries <- PermutationTestScoresFn(RqCqDFSeries=RqCqDFSeries,
                                                PSSDecisionRule=PSSDecisionRule,
                                                forced=TRUE,
                                                priorProb=.5,
                                                PSSCutProbT=PSSCutProbT,
                                                PSSCutProbD=PSSCutProbD,
                                                forceINC=TRUE,
                                                makeDF=FALSE,
                                                saveCSV=saveCSV,
                                                analysisListName=analysisListName )
        
        # View(RqCqDFSeries)
        
        # pass the RqCQDFSeries bac to the seriesMeasurementDF
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        # View(seriesMeasurementDF)
        # View(measurementDF)
        
      }
      
      #### calculate the Honts & Dewitt 1992 Bootstrap Score ####
      
      if(isTRUE(getBootstrapScores)) {
        
        # stop()
        
        if(!exists("BootstrapTestDecisionRule")) BootstrapTestDecisionRule <- "GTR"
        
        RqCqDFSeries <- bootstrapScoresFn(RqCqDFSeries=RqCqDFSeries,
                                          bootstrapDecisionRule=bootstrapDecisionRule,
                                          bootstrapCutProbT=.3,
                                          bootstrapCutProbD=.7,
                                          forced=TRUE,
                                          makeDF=FALSE,
                                          saveCSV=saveCSV,
                                          analysisListName=analysisListName )
        
        # View(RqCqDFSeries)
        
        # pass the RqCQDFSeries bac to the seriesMeasurementDF
        # seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
        
        # pass the seriesMeasurementDf back to the measurementDF
        # measurementDF[seriesRows,] <- seriesMeasurementDF
        
        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
        # View(seriesMeasurementDF)
        # View(measurementDF)
        
      }
      
      #### calculate LXCAT Algorithm results ####
      
      if(isTRUE(getPCATScores) && all(c("AutoEDA",  "PLE") %in% uniqueSensors)) {
        
        # need to check for PCAT sensors
        # Jan 6, 2022
        # to avoid errors when no PLE sensor
        PCATSensors <- c("AutoEDA", 
                         "PLE")
        
        all(c("AutoEDA",  "PLE") %in% uniqueSensors)
        
        if(!exists("PCATDecisionRule")) PCATDecisionRule <- "SSR"
        
        if(isTRUE(stopScore)) {
          # stopScore is initialized in 
          
          if(RqCqDFSeries$examName[1] == "DPE1703002RH") {
            assign("RqCqDFSeries", RqCqDFSeries, pos=1)
            stop()
          }
          
          # if(seriesName == "4") stop()
          # assign("RqCqDFSeries", RqCqDFSeries, pos=1)
          # stop()
        }
        
        # stop()
        
        RqCqDFSeries <- 
          LXCATScoresFn(RqCqDFSeries=RqCqDFSeries,
                       PCATPrior=.5,
                       # PCATSensors=c("AutoEDA", "PLE"),
                       # PCATSensors=c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE"),
                       PCATAlphas=PCATAlphas,
                       PCATDecisionRule=PCATDecisionRule,
                       forced=TRUE,
                       makeDF=makeDF,
                       saveCSV=saveCSV,
                       analysisListName=analysisListName)

        assign("RqCqDFSeries", RqCqDFSeries, pos=1)
        # assign("seriesMeasurementDF", seriesMeasurementDF, env=.GlobalEnv)
        # assign("measurementDF", measurementDF, pos=1)
        
        # View(seriesMeasurementDF)
        # View(measurementDF)
        # View(RqCqDFSeries)
        
      }
      
      #### output ####
      
      # reset the pneumo measurements if necessary
      if(!isTRUE(includePneumoData)) {
        RqCqDFSeries$sensorMeasurement[pneumoRows] <- 
          pneumoMeasurements
        seriesMeasurementDF$sensorMeasurement[pneumoRowsSeriesMeasurements] <- 
          pneumoMeasurementsSeriesMeasuremnts
      }
      
      # pass the RqCQDFSeries back to the seriesMeasurementDF
      seriesMeasurementDF[RqCqSeriesRows,] <- RqCqDFSeries
      
      # View(RqCqDFSeries)
      # View(RqCqDFSeries2)
      # View(seriesMeasurementDF[RqCqSeriesRows,])
      
      
      # pass the seriesMeasurementDf back to the measurementDF
      measurementDF[seriesRows,] <- seriesMeasurementDF
      
    } # end loop over j series
    
    # assign the measurement DF to the original object name in the global env
    assign(paste0(examName, "_Measurements"), measurementDF, pos = 1)
    
    # assign the analysis to a list
    # resultListName <- paste(examName, "ANALYSIS", sep="_")
    # assign("analysisResultList", analysisResultList, envir=.GlobalEnv)
    # assign(resultListName, analysisResultList, envir=.GlobalEnv)
    
  } # end loop over i exams 
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(measurementDF)
  
} # end newScoresFn() function

# newScoresFn(x=uniqueExams)
