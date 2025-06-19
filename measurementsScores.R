# R Script to make separate .csv files 
# of measurements and  scores for each exam in the cwd
# 12-12-2017
# Raymond Nelson
#
###############


measurementsScoresFn <- function(x=examName, y=uniqueSeries, makeCSV=TRUE) {
  # R Script to make separate .csv files 
  # of measurements and  scores for each exam in the cwd
  # 12-12-2017
  # Raymond Nelson
  #
  ###
  # called by the getExamFn function in the workFlow.R script
  # x input is a scalar with a unique exam name
  # this function does not iterate over a vector of exam names
  # instead, it will get the measurement data frame from the global environment
  # y input is a scalar with the series name
  # z input is a vector of chart names for the series
  ###############
  
  # print a message
  # myChartFUN()
  
  examName <- x
  uniqueSeries <- y
  # uniqueCharts <- z
  
  # chartName <- "03A"
  
  # save the exam and series name to the global environment
  assign("examName", examName, pos=1)
  assign("uniqueSeries", uniqueSeries, pos=1)
  # assign("uniqueCharts", uniqueCharts, pos=1)
  
  ############
  
  # initialize the search string to get the measurements
  searchString <- paste0("*", examName, "_Measurements", "*")
  if(!exists("searchString")) return()
  
  # initialize the measurementDF for the exam
  measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  if(is.null(measurementDF)) return()
  
  # assign it to the global environment
  assign("measurementDF", measurementDF, pos=1)
  
  # initialize a data frame for the subtotals for the series
  seriesTotalsDF <- NULL
  
  # iterate over each unique series
  i=2
  for (i in 1:length(uniqueSeries)) {
    
    seriesName <- uniqueSeries[i]
    
    # keep only the measurements for the selected series and chart
    seriesRows <- measurementDF$seriesName==seriesName
    
    seriesMeasurementDF <- measurementDF[seriesRows,]
    assign("seriesMeasurementDF", seriesMeasurementDF, pos=1)
    
    write.csv(seriesMeasurementDF,
              file=paste0(paste(examName, seriesName, "measurements", sep="_"), ".csv"),
              row.names=FALSE)
    
    uniqueCharts <- unique(seriesMeasurementDF$chartName)
    
    # iterate over the charts
    
    if(length(uniqueCharts) > 0) {
      j=1
      for (j in 1:length(uniqueCharts)) {
        
        chartName <- uniqueCharts[j]
        chartRows <- measurementDF$chartName==chartName
        chartRows <- which(seriesRows & chartRows)
        # print(paste("rows", chartRows))
        
        # get the chart measurement data frame
        chartMeasurementDF <- measurementDF[chartRows,]
        # View(chartMeasurementDF)
        
        assign("chartMeasurementDF", chartMeasurementDF, pos=1)
        
        if(nrow(chartMeasurementDF) == 0 ) return()
        # View(chartMeasurementDF)
        
        # write.csv(chartMeasurementDF,
        #           file=paste0(paste(examName, seriesName, chartName, "measurements", sep="_"), ".csv"),
        #           row.names=FALSE)
        
      } # end loop j over unique charts
    } # end if
    
  } # end loop over i unique series
  
  print(examName)
  # output needs to be NULL because this is called by getExamFn()
  return(NULL)
  
} # end measurementsScoresFn()

# measurementsScoresFn()

