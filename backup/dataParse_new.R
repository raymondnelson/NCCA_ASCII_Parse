# ############# R Functions for parsing NCCA ASCII time series data ##############
# 12-30-2014 Raymond Nelson
# 3-6-2014
# 8-1-2014
#
# this script contains the following 3 functions
#
# dataToCSV()
# to convert the time series data to .csv format
#
#
##############################################



DataToCSV <- function(x = dataNames) {
  # new function to read the NCCA ASCII time series data to data frams
  # add columns for the exam name and chart name
  # accepts as input a vector of text file names from the output of the dataFile function
  #
  # make a vector to hold the names of the data frames
  myNames <- character()
  # a function to loop over the vector of chart data files
  for(i in 1:length(x)) {
    # first make a variable to hold the name of the chart data
    currentChartName <- str_sub(x[i], 1, -5)
    # make a vector of column names for each chart
    # this should be done individually for each chart
    # because some difference may exist between exams
    cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                            c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11), 
                                            header = FALSE, 
                                            skip = 0,
                                            n = 1))))
    myDF <- read.fwf(textConnection(get(currentChartName), open = "r"), 
                       c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11), 
                       header = FALSE, 
                       skip = 1,
                       col.names = cNames,
                       n = -1L)                    
    # create the CSV file name
    outName <- paste(currentChartName, ".csv", sep = "")
    # save the data as a CSV
    write.csv(myDF, file = outName)
    # save the data as a data frame 
    # for individual exams
    # may need to comment this out when working with samples 
    assign(currentChartName, myDF, pos = 1)
    myNames <- c(myNames, currentChartName)
  }
  return(myNames) 
} # DataToCSV
  
DataToCSV()
  
