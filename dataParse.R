# ############# R Functions for parsing NCCA ASCII time series data ##############
# 12-30-2014 Raymond Nelson
# 3-6-2014
# 8-1-2014
# 6-12-2015 now works recursively
#
# this script contains the following function
#
# dataParse()
# to convert the time series data to .csv format
#
#
##############################################

# function to read the NCCA ASCII time series data and 

dataParse <- function(x=dataNames, makeDF=TRUE, saveCSV=TRUE) {
  # function to read the NCCA ASCII time series data to a data frame
  # and also create csv version of the time series data
  
  # input is a vector of names from the output of the dataFile function
  # output is a vector of names of the exams
  
  # add columns for the exam name and chart name
  
  ####
  
  # first make an empty vector to hold the output names
  myNames <- character()
  
  # make an empty vector to build the output data frame
  outDF <- NULL
  
  # a loop over the vector of chart data files
  # i <- 1
  for(i in 1:length(x)) {
    # first make a variable to hold the name of the chart data
    # x <- x # not sure this is necessary
    currentChartName <- str_sub(x[i], 1, -1)
    # make a vector of column names for each chart
    # this should be done individually for each chart
    # because some difference may exist between exams
    
    # read the text data from the data vector
    cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                            widths = c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11, 11), 
                                            header = FALSE, 
                                            skip = 0,
                                            n = 1)
                                   )
                                 ),
                       side = "both")
    myDF <- read.fwf(textConnection(get(currentChartName), open = "r"), 
                       widths = c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11, 11), 
                       header = FALSE, 
                       skip = 1,
                       col.names = cNames,
                       n = -1L)                    
    
    # add a column for the chart name
    chartName <- rep(str_sub(currentChartName, -10, -6), nrow(myDF))
    myDF <- cbind(chartName, myDF)
    # add a column for the series name
    seriesName <- rep(str_sub(currentChartName, -10, -10), nrow(myDF))
    myDF <- cbind(seriesName, myDF)
    # add a column for the exam name
    examName <- rep(str_sub(currentChartName, 1, -12), nrow(myDF))
    myDF <- cbind(examName, myDF)
    
    # rbind the DFs for all charts
    outDF <- rbind(outDF, myDF)
  }
  
    # create the CSV file name
    outName <- paste(str_sub(currentChartName, 1, -12), "_Data", sep = "")
    # save the data as a CSV
    if (saveCSV==TRUE) write.csv(outDF, 
                                 file=paste(outName, ".csv", sep = ""), 
                                 row.names = FALSE)
    # save the data as a data frame 
    if(makeDF==TRUE) assign(x=outName, 
                            value=outDF, 
                            pos = 1)
    
  # concatenate the current chart name with the vector of chart names
  myNames <- c(myNames, str_sub(currentChartName, 1, -12))
  
  # output is the name of the exam
  myNames <- paste(myNames, "_data", sep = "")
  return(myNames) 

  } # dataParse

# dataParse(x=dataNames, makeDF=TRUE, saveCSV=TRUE) # called by the parseUniqueExams function in the NCCAASCIIParse.r script
  
