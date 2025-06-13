# R Script to make separate .csv files in the cwd
# for all extracted measurements for each exam in the global envir
# Mar 15, 2020
# Raymond Nelson
#
####


# source the outputScores.R script for the measurementTableFn


# source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)


source(paste0(RPath, 'outputScores.R'), echo=FALSE)




newMeasurementsScoresFn <- function(uniqueExams=uniqueExams,
                                    saveCSV=TRUE,
                                    makeDF=FALSE, 
                                    MeasurementsDF=FALSE,
                                    measurementTable=TRUE,
                                    transpose=FALSE) {
  # R Script to make separate .csv files in the cwd
  # called in the workFlow.R script
  # for each exam series in the global envir
  # uses the skiny format _Measurements data frame
  # to save a similar skiny _Measurements.csv for each series
  # for all extracted measurements and all algorithm scores
  # 
  # MeasurementsDF=TRUE will save a _MeasurementsDF for each series with algorithm scores
  # measurementTable=TRUE will save a scoresheet table of measurement values for each series
  #
  # transpose=TRUE will format the output .csv table for the OSS-3 excel input 
  #
  # Mar 15, 2020
  # Raymond Nelson
  #
  ####
  
  # uses the showNames and output settings from the global environment
  # set in the NCCAASCII_init.R script
  
  {
    if(!exists("saveCSV")) saveCSV <- TRUE
    if(!exists("makeDF")) makeDF <- FALSE
    if(!exists("MeasurementsDF")) MeasurementsDF <- FALSE
    if(!exists("measurementTable")) measurementTable <- TRUE
    if(!exists("transpose")) transpose <- FALSE
  }
  
  ##### iterate over each exam in the list #####
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {

      examName <- uniqueExams[i]
      # get the names of time series lists for all unique series in each exam
      searchString <- paste0("*", examName, "_Data", "*")

      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)

      # examDF$examName <- as.character(examDF$examName)
      # examDF$seriesName <- as.character(examDF$seriesName)
      # examDF$chartName <- as.character(examDF$chartName)

      examStartRow <- 1
      examEndRow <- nrow(examDF)

      # assign("examDF", examDF, pos=1)
      assign("examName", examName, pos=1)

      if(showNames==TRUE) print(examName)

      # get the measurementDF from the global envir
      
      # initialize the search string to get the measurements
      searchString <- paste0("*", examName, "_Measurements", "*")
      if(!exists("searchString")) return()
      
      # initialize the measurementDF for the exam
      measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      if(is.null(measurementDF)) return()
      
      # add the columns for response onset and response peak - July 21, 2023
      # measurementDF <- cbind(measurementDF[1:10],
      #                        measurementDF$responseOnset <- "",
      #                        measurementDF$responsePeak <- "",
      #                        measurementDF[11:ncol(measurementDF)] )
      
      # assign it to the global environment
      # assign("measurementDF", measurementDF, pos=1)
      
      # get the names of unique series
      uniqueSeries <- as.character(unique(measurementDF$seriesName))
      
      # initialize a data frame for the subtotals for the series
      seriesTotalsDF <- NULL
      
    }
    
    #### iterate over each unique series ####
    
    j=2
    for (j in 1:length(uniqueSeries)) {
      
      {
        
        seriesName <- uniqueSeries[j]
        
        # keep only the measurements for the selected series and chart
        seriesRows <- measurementDF$seriesName==seriesName
        
        seriesMeasurementDF <- measurementDF[seriesRows,]
        # assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
        # View(seriesMeasurementDF)
        
        uniqueCharts <- unique(seriesMeasurementDF$chartName)
        
      }
      
      if(isTRUE(saveCSV) && isTRUE(MeasurementsDF)) {
        write.csv(seriesMeasurementDF,
                  file=paste(examName, seriesName, "Measurements.csv", sep="_"),
                  row.names=FALSE)
      }
      
      #### export a measurement table ####
      
      {
        
        useSensors <- c("UPneumo", 
                        "LPneumo", 
                        "AutoEDA", 
                        "ManualEDA",
                        "Cardio", 
                        # "eCardio",
                        "PLE")
        useSensors <- 
          useSensors[useSensors %in% seriesMeasurementDF$sensorName]
        
		# save a wide format score sheet table of feature extraction values
        # call the measurementTableFn from the outputScores.R script
        measurementTableFn(RqCqDFSeries=seriesMeasurementDF, 
                           useSensors=useSensors,
                           decimals=2,
                           makeDF=makeDF,
                           saveCSV=(isTRUE(saveCSV) && isTRUE(measurementTable)),
                           transpose=transpose )
        
        # can be easily opened in Excel or other spreadsheet
        # paste into the OSS-3 Excel spreadsheet
        
      }
      
      #### iterate over the charts ####
      
      # if(length(uniqueCharts) > 0) {
      #   
      #   k=1
      #   for (j in 1:length(uniqueCharts)) {
      #     
      #     chartName <- uniqueChartsk
      #     chartRows <- measurementDF$chartName==chartName
      #     chartRows <- which(seriesRows & chartRows)
      #     # print(paste("rows", chartRows))
      #     
      #     # get the chart measurement data frame
      #     chartMeasurementDF <- measurementDF[chartRows,]
      #     # View(chartMeasurementDF)
      #     
      #     # assign("chartMeasurementDF", chartMeasurementDF, pos=1)
      #     
      #     if(nrow(chartMeasurementDF) == 0 ) return()
      #     # View(chartMeasurementDF)
      #     
      #     # write.csv(chartMeasurementDF,
      #     #           file=paste0(paste(examName, seriesName, chartName, "measurements", sep="_"), ".csv"),
      #     #           row.names=FALSE)
      #     
      #   } # end loop k over unique charts
      # 
      # } # end if for number of charts > 0
      
    } # end loop over j unique series
    
    ####
    
  } # end loop over i exams 
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(uniqueExams)
  
} # end newMeasurementsScoresFn() function

# newMeasurementsScoresFn(x=uniqueExams)

