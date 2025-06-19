# R Script to make a table of OSS-2 scores for each exam in the cwd
# July 27, 2018
# Jan 30, 2019
# Raymond Nelson
#
###############


getOSS2ScoresFn <- function(x=examName, 
                            y=uniqueSeries, 
                            makeScoreSheetDF=TRUE, 
                            writeScoreSheetCSV=TRUE, 
                            makeSeriesTotalsDF=TRUE,
                            writeSeriesTotalsCSV=TRUE,
                            makeChartTotalsDF=TRUE,
                            writeChartTotalsCSV=TRUE,
                            makeSensorTotalsDF=TRUE,
                            writeSensorTotalsCSV=TRUE) {
  # R Script to make a table of OSS-2 scores for each exam in the cwd
  # July 27, 2018
  # Jan 30, 2019
  # Raymond Nelson
  #
  ###############
  # called by the getExamFn function in the getSegment.R script
  # getExamFn() is called in the workFlow.R script
  #
  # x input is a scalar with a unique exam name
  # the exam is retrieved from the global environment
  # this function does not iterate over a vector of exam names
  # instead, it will get the measurement data frame from the global environment
  # y input is a scalar with the series name, from the global env
  # 
  # no visible output
  # output occurs as a side effect in the creation  of score data frames
  # output also occurs as a side effect in the creation of .csv files
  #
  ###############
  
	examName <- x
  uniqueSeries <- y

  # save the exam and series name to the global environment
  assign("examName", examName, pos=1)
  assign("uniqueSeries", uniqueSeries, pos=1)
  
  ############ get the measurements data frame for the exam
  
  # initialize the search string to get the measurements
  searchString <- paste0("*", examName, "_Measurements", "*")
  if(!exists("searchString")) return()
  
  # initialize the measurementDF for the exam
  measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  # View(measurementDF)
  if(is.null(measurementDF)) return()
  
  # assign it to the global environment
  assign("measurementDF", measurementDF, pos=1)
  
  #############   iterate over each unique series    #############  
  
  # initialize a data frame
  seriesTotalsDF <- NULL
  # seriesToalsDF will have the RQ subtotals and grand total 
  # for each series in the exam
  
  i=2
  for (i in 1:length(uniqueSeries)) {
    
    seriesName <- uniqueSeries[i]
    
    # keep only the measurements for the selected series and chart
    seriesRows <- measurementDF$seriesName==seriesName
    
    if(length(which(seriesRows == TRUE)) == 0) next()
    
    seriesMeasurementDF <- measurementDF[seriesRows,]
    assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
    
    uniqueCharts <- unique(seriesMeasurementDF$chartName)
    
    # initialize some data frames
    
		# seriesTotalsDF <- NULL
		# seriesTotalsDF will have the RQ subtotals and grand total for the series 
		
		scoreSheetDF <- NULL
		# scoreSheetDF will output all sensor scores for all charts in the series
		chartTotalsDF <- NULL
		# chartTotalsDF will output the RQ score and subtotal for each chart
		sensorTotalsDF <- NULL
		# sensorTotalsDF includes sensor scores for each chart in the series
		
  	# iterate over the charts
    j=1
    for (j in 1:length(uniqueCharts)) {
      
      chartName <- uniqueCharts[j]
      chartRows <- measurementDF$chartName==chartName
      chartRows <- which(seriesRows & chartRows)
      # print(paste("rows", chartRows))
      
      # get the chart measurement data frame
      chartMeasurementDF <- measurementDF[chartRows,]
      # View(chartMeasurementDF)
      
      if(nrow(chartMeasurementDF) == 0 ) next()
      # View(chartMeasurementDF)
      
      assign("chartMeasurementDF", chartMeasurementDF, pos=1)
      
      uniqueSensors <- as.character(unique(chartMeasurementDF$sensorName))
      
      uniqueEvents <- unique(chartMeasurementDF$eventLabel)
      
      if(length(uniqueEvents) == 0) {
        # next chart if no events
        next()
      }
            
      ######## initialize the RqCqDF data frame for RQ and CQ scores ########
      
      # the RqCqDF contains only RQs and CQs
      
      # make a vector of RQs and CQs
      RqCqRows <- grep("[CR]+", chartMeasurementDF$eventLabel)
      # exclude sacrifice relevant questions
      SRRows <- grep("SR", chartMeasurementDF$eventLabel)
      SRRows <- c(SRRows, grep("RS", chartMeasurementDF$eventLabel))
      # make a vector of working rows for CQs and RQs
      RqCqRows <- RqCqRows[!(RqCqRows %in% SRRows)]
      # exclude "CT" (cleared throat) annotations
      CTRows <- grep("CT", chartMeasurementDF$eventLabel)
      RqCqRows <- RqCqRows[!(RqCqRows %in% CTRows)]
      if(length(RqCqRows) == 0) {
        # print("no RQs or CQs")
        next()
        # if(output == TRUE) return(measurementDF)
      }
      
      RqCqDF <- chartMeasurementDF[RqCqRows,]
      # assign("RqCqDF", RqCqDF, pos=1)
      # View(RqCqDF)
      
      # initialize a data frame of RQs for this chart
      rqRows <- grep("R", RqCqDF$eventLabel)
      rqDF <- RqCqDF[rqRows,]
      rqDF$OSS2Scores <- as.numeric(rqDF$OSS2Scores)
      # assign("rqDF", rqDF, pos=1)
      # View(rqDF)
      
      # initialize another data frame of CQs for this chart
      cqRows <- grep("C", RqCqDF$eventLabel)
      cqDF <- RqCqDF[cqRows,]
      # assign("cqDF", cqDF, pos=1)
      # View(cqDF)
      
      # make a vector of unique RQs
      uniqueRQs <- unique(rqDF$eventLabel)
      
      # and another vector of unique CQs
      uniqueCQs <- unique(cqDF$eventLabel)
      
      if( length(uniqueRQs) < 2 || length(uniqueCQs) < 2 ) { 
        # next chart if there are not at least 2 RQs and at least 2 CQs
        next() 
      }
      
      ######## get the data frame rows for the sensors in this chart ########
      
      # useRows <- which(rqDF$sensorName %in% (c("Pneumo", "AutoEDA", "Cardio", "PLE", "Activity")))
      # useRows <- which(rqDF$sensorName %in% (c("UPneumo", "LPneumo", "AutoEDA", "Cardio", "PLE","Activity")))
      # useRows <- which(rqDF$sensorName %in% (c("Pneumo", "AutoEDA", "Cardio", "Activity")))
      useRows <- which(rqDF$sensorName %in% (c("Pneumo", "AutoEDA", "Cardio")))
      
      RQLabel <- rqDF[useRows,'eventLabel']
      sensorNames <- rqDF[useRows,'sensorName']
      OSS2Scores <- rqDF[useRows,'OSS2Scores']
      
      # initialize a data frame for the OSS2 sensor scores for this chart
      scoresDF <- cbind.data.frame(examName=rep(examName, times=length(OSS2Scores)),
                                   seriesName=rep(seriesName, times=length(OSS2Scores)),
                                   chartName=rep(chartName, times=length(OSS2Scores)),
                                   RQLabel=RQLabel,
                                   sensorName=sensorNames,
                                   OSS2Score=OSS2Scores )
      
      scoresDF$examName <- as.character(scoresDF$examName)
      scoresDF$seriesName <- as.character(scoresDF$seriesName)
      scoresDF$chartName <- as.character(scoresDF$chartName)
      scoresDF$RQLabel <- as.character(scoresDF$RQLabel)
      scoresDF$sensorName <- as.character(scoresDF$sensorName)
      scoresDF$OSS2Score <- as.numeric(as.character(scoresDF$OSS2Score))
      
      # View(scoresDF)
      
      #### initialize some objects ####
      
      subtotalVector <- NULL
      # subtotal vector will hold the RQ subtotals for each chart
      scoreSheet <- NULL
      # score sheet will hold the RQ scores for all RQs and all sensors
      
      # iterate over the RQs to get the chart scores and question subtotals
      k=1
      for(k in 1:length(unique(RQLabel))) {
        
        # make a vector of rows for this RQ
        subtotalRows <- which(scoresDF$RQLabel == unique(RQLabel)[k])
        
        # initialize an object to hold the scores for the chart
        # get the RQ scores for all sensors and them to the scoreSheet
        RQSensorScores <- NULL
        l=1
        for (l in 1:length(subtotalRows)) {
          RQSensorScores <- c(RQSensorScores,
                            scoresDF$OSS2Score[subtotalRows[l]])
        }
        
        # add the RQ sensor scores to the chart score sheet
        scoreSheet <- cbind(scoreSheet, RQSensorScores)
        # View(scoreSheet)
        
        # get the subtotals for each RQ for this chart
        subtotalVector <- c(subtotalVector, 
                            sum(scoresDF$OSS2Score[subtotalRows], na.rm=TRUE) )
        
      } # end loop k over RQ labels
      
      scoreSheet <- as.data.frame(scoreSheet)
      colnames(scoreSheet) <- unique(RQLabel)
      # View(scoreSheet)
      
      # add some columns to the scoreSheet
      scoreSheet <- cbind.data.frame(examName,
                                     seriesName,
                                     chartName,
                                     sensorName=as.character(scoresDF$sensorName[subtotalRows]),
                                     scoreSheet )
      scoreSheet$examName <- as.character(scoreSheet$examName)
      scoreSheet$seriesName <- as.character(scoreSheet$seriesName)
      scoreSheet$chartName <- as.character(scoreSheet$chartName)
      scoreSheet$sensorName <- as.character(scoreSheet$sensorName)
      
      # View(scoreSheet)
      
      # add any missing columns to the scoreSheet or scoreSheetDF
      # so that all chart score sheets have the same columns
      
      # m=1
      # skip if the scoreSheetDF is NULL
      if(!is.null(scoreSheetDF)) {
        missingEvents <- names(scoreSheet)[!(names(scoreSheet) %in% names(scoreSheetDF))]
        if(length(missingEvents) > 0) {
          for(m in 1:length(missingEvents)) {
            thisValue <- NA
            scoreSheetDF <- cbind(scoreSheetDF, thisValue)
            names(scoreSheetDF)[length(colnames(scoreSheetDF))] <- missingEvents[m]
          }
        }
        missingEvents <- names(scoreSheetDF)[!(names(scoreSheetDF) %in% names(scoreSheet))]
        if(length(missingEvents) > 0) {
          for(m in 1:length(missingEvents)) {
            thisValue <- NA
            scoreSheet <- cbind(scoreSheet, thisValue)
            names(scoreSheet)[length(colnames(scoreSheet))] <- missingEvents[m]
          }
        }
      }
      
      ###### add rows for upper pneumo, PLE and activity sensors ######
      
      if("Pneumo" %in% unique(scoreSheet$sensorName)) {
        pneumoRow <- which(scoreSheet$sensorName=="Pneumo")
        scoreSheet$sensorName[pneumoRow] <- "UPneumo"
        if(!("LPneumo" %in% unique(scoreSheet$sensorName))) {
          scoreSheet <- rbind(scoreSheet[1:pneumoRow,],
                              c(examName=examName, 
                                seriesname=seriesName, 
                                chartName=chartName, 
                                sensorName="LPneumo",
                                rep("", times=(ncol(scoreSheet)-4)) ),
                              scoreSheet[(pneumoRow+1):nrow(scoreSheet),] )
        } 
      }
      
      # if(!("Activity" %in% unique(scoreSheet$sensorName))) {
      #   scoreSheet <- rbind(scoreSheet, 
      #                       c(examName=examName, 
      #                         seriesname=seriesName, 
      #                         chartName=chartName, 
      #                         sensorName="Activity",
      #                         rep( "", times=(ncol(scoreSheet)-4 )) ) )
      # }
      
      # if(!("PLE" %in% unique(scoreSheet$sensorName))) {
      #   cardioRow <- which(scoreSheet$sensorName=="PLE")
      #   scoreSheet <- rbind(scoreSheet[1:cardioRow,],
      #                       c(examName=examName,
      #                         seriesname=seriesName,
      #                         chartName=chartName,
      #                         sensorName="PLE",
      #                         rep("", times=(ncol(scoreSheet)-4)) ),
      #                       scoreSheet[(cardioRow+1):nrow(scoreSheet),] )
      # }
      
      # bind the scoreSheet to the scoreSheetDF
      scoreSheetDF <- rbind.data.frame(scoreSheetDF, scoreSheet)
      # View(scoreSheetDF)
      
      names(subtotalVector) <- unique(RQLabel)
      subtotalDF <- rbind.data.frame(subtotalVector)
      names(subtotalDF) <- unique(RQLabel)
      
      ###############################
      
      # add the subtotalDF to the chartTotalsDF
      if(!is.null(subtotalDF)) {
        if(!is.null(chartTotalsDF)) {
          # if the chartTotalsDF is not NULL then check that all columns match
          # skip this if the chartTotalsDF is NULL
          # check for missing RQ event columns
          # 1-8-2017 check the number of RQs
          missingEvents <- names(chartTotalsDF)[!(names(chartTotalsDF) %in% names(subtotalDF))]
          if(length(missingEvents) > 0) {
            for(m in 1:length(missingEvents)) {
              thisValue <- NA
              subtotalDF <- cbind(subtotalDF, thisValue)
              names(subtotalDF)[length(colnames(subtotalDF))] <- missingEvents[m]
            }
          }
          missingEvents <- names(subtotalDF)[!(names(subtotalDF) %in% names(chartTotalsDF))]
          if(length(missingEvents) > 0) {
            for(m in 1:length(missingEvents)) {
              thisValue <- NA
              chartTotalsDF <- cbind(chartTotalsDF, thisValue)
              names(chartTotalsDF)[length(colnames(chartTotalsDF))] <- missingEvents[m]
            }
          }
          # end if !is.null(chartTotalsDF)
        } # else {
        chartTotalsDF <- rbind(chartTotalsDF, subtotalDF)
          # if the the chartTotalsDF is NULL then bind the subtotalDF
        # }
      } # end if !is.null(subtotalDF)
      # View(chartTotalsDF)
      
    } # end loop j over unique charts
    
    #### 
    
    ###### output section ######
		
		if(is.null(scoreSheetDF)) next()
    
    #### scoresheet
    
    # save the score sheet as a data frame and  .csv
    scoreSheetName <- paste(examName, seriesName, "OSS2ScoreSheetDF", sep="_")
		
		if(isTRUE(makeScoreSheetDF)) {
		  assign(scoreSheetName, scoreSheetDF, pos=1)
		}
		
		if(isTRUE(writeScoreSheetCSV)) {
 			write.csv(scoreSheetDF,
								file=paste0(str_sub(scoreSheetName, 1, -3), ".csv"),
								row.names=FALSE)
    }
    
    #### series totals
    
    # each seriesTotalsDF will have the RQ subtotals and grand total for all charts
    # sum the subtotals for each series 
    # and add the row to the series totals data frame
    
    if(!is.null(chartTotalsDF)) {
      seriesTotalsDF <- rbind.data.frame(colSums(chartTotalsDF, na.rm=TRUE))
      names(seriesTotalsDF) <- names(chartTotalsDF)
      seriesTotalsDF['grandTotal'] <- sum(seriesTotalsDF)
    } 
    
    seriesTotalsDF <- cbind(examName, seriesName, seriesTotalsDF)
    names(seriesTotalsDF)[c(1,2)] <- c("examName", "seriesName")
    
    seriesTotalsDFName <- paste(examName, 
                                seriesName, 
                                "OSS2SeriesTotalsDF", 
                                sep="_")
    
    if(isTRUE(makeSeriesTotalsDF) && !is.null(seriesTotalsDF)) {
      assign(seriesTotalsDFName,
             seriesTotalsDF,
             pos=1)
    }
    
    if(isTRUE(writeSeriesTotalsCSV) && !is.null(seriesTotalsDF)) {
      write.csv(seriesTotalsDF,
                file=paste0(str_sub(seriesTotalsDFName, 1, -3), ".csv"),
                row.names=FALSE)
    }
    
    #### chart totals
    
    # chartTotalsDF will include RQ subtotals and chart subtotals for all charts
    
    chartTotalsDF['subTotal'] <- apply(chartTotalsDF, 1, sum)
    
    chartTotalsDF <- cbind(examName, seriesName, uniqueCharts, chartTotalsDF)
    
    # save the chart totals data frame for each series as a csv
    
    chartTotalsDFName <- paste(examName,
                               seriesName,
                               "OSS2ChartTotalsDF",
                               sep="_")
                           
    if(isTRUE(makeChartTotalsDF) && !is.null(chartTotalsDF)) {
      assign(chartTotalsDFName, chartTotalsDF, pos=1)
    }
    
    if(isTRUE(writeChartTotalsCSV) && !is.null(chartTotalsDF)) {
      write.csv(chartTotalsDF,
                file=paste0(str_sub(chartTotalsDFName, 1, -3), ".csv"),
                row.names=FALSE )
    }
    
    #### sensor totals
    
    # sum the subtotals for each sensor for all charts 
    
    sensorTotalsDF <- NULL
    
    # get the sensor names
    theseSensorNames <- unique(scoreSheetDF$sensorName)
    # then iterate over the charts
    m=2
    for(m in 1:length(uniqueCharts)) {
      thisChartScoreSheetDF <- scoreSheetDF[scoreSheetDF$chartName==uniqueCharts[m],]
      sensorDAT <- NULL
      n=1
      for(n in 1:length(theseSensorNames)) {
        thisSensorRow <- thisChartScoreSheetDF$sensorName == theseSensorNames[n]
        DAT <- 
          thisChartScoreSheetDF[thisSensorRow,(5:ncol(thisChartScoreSheetDF))]
        sensorDAT <- c(sensorDAT, sum(as.numeric(DAT), na.rm=TRUE))
      }
      sensorTotalsDF <- rbind.data.frame(sensorTotalsDF, sensorDAT)
    }
    names(sensorTotalsDF) <- theseSensorNames
    
    sensorTotalsDF <- cbind(examName,
                            seriesName,
                            chartName=uniqueCharts,
                            sensorTotalsDF)
    
    sensorTotalsDF$examName <- as.character(sensorTotalsDF$examName)
    sensorTotalsDF$examName <- as.character(sensorTotalsDF$examName)
    sensorTotalsDF$examName <- as.character(sensorTotalsDF$examName)
      
    sensorTotalsDFName <- paste(examName,
                               seriesName,
                               "OSS2SensorTotalsDF",
                               sep="_")
    
    if(isTRUE(makeSensorTotalsDF) && !is.null(sensorTotalsDF)) {
      assign(sensorTotalsDFName, sensorTotalsDF, pos=1)
    }
    
    if(isTRUE(writeSensorTotalsCSV) && !is.null(sensorTotalsDF)) {
      write.csv(sensorTotalsDF,
                file=paste0(str_sub(sensorTotalsDFName, 1, -3), ".csv"),
                row.names=FALSE )
    }
    
  } # end loop over i unique series
  
  ####
  
  # no output because an exam may have multiple series
  # output occurs as a side effect
  # output needs to be NULL because this is called by getExamFn()
  return(NULL)
  
  # end getOSS2ScoresFn()
  
} 

# getOSS2ScoresFn()

