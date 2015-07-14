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
#
# 
# 
dataToCSV <- function(x = dataNames) {
  # function to loop over the NCCA ASCII file names and make .csv files from the time series data
  #
  # WARNING!!! this function recursively creates .csv files 
  # from all charts in the dataNames vector
  #
  # make a vector of data.txt file names
  # dataNames <- getDataNames()
  #
  # loop over the data vector
  for (i in 1:length(x)) {
    # assign the vector to a common name, trim leading and trailing spaces
    NCCAASCIIdata <- str_trim(readLines(x[i], 
                                        n = -1, 
                                        ok = TRUE, 
                                        warn = FALSE, 
                                        encoding = "UTF-8"), 
                              side = "both")
    # remove -9.9 placeholder values
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-9.9", "--------")
    # then remove dash characters
    # uses "ZZZ" as a placeholder so the field is not lost
    # when the spaces are substituted with commas at a later stage
    # should use a loop or function to do this at some point
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "--------", "ZZZ") #8
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-------", "ZZZ") #7
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "------", "ZZZ") #6
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-----", "ZZZ") #5
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "----", "ZZZ") #4
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "---", "ZZZ") #3
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "--", "ZZZ") #2
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "-", "ZZZ") #1
    # remove excess spaces
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "        ", " ") #8
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "       ", " ") #7
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "      ", " ") #6
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "     ", " ") #5
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "    ", " ") #4
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "   ", " ") #3
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "  ", " ") #2
    # replace all spaces with comma
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, " ", ",")
    # replace the ZZZ placeholders
    NCCAASCIIdata <- str_replace_all(NCCAASCIIdata, "ZZZ", "")
    #
    #
    # set the name of the data file
    NCCAASCIIdataCSV <- paste(str_sub(x[i], 1, -5), ".csv", sep = "")
    #
    # trim the .txt extension from the file name
    # NCCAASCIIdataCSV <- str_sub(NCCAASCIIdataCSV, 1, nchar(NCCAASCIIdataCSV) - 4)
    # NCCAASCIIdataCSV <- str_replace(NCCAASCIIdataCSV, ".txt", "")
    # NCCAASCIIdataCSV <- str_sub(NCCAASCIIdataCSV, 1, -5)
    #
    # replace the & character to avoid problems.
    # NCCAASCIIdataCSV <- str_replace(NCCAASCIIdataCSV, "&", "")
    #
    # save the data file
    cat(NCCAASCIIdata, file = NCCAASCIIdataCSV, sep = "\n")
    #
    # make a dataframe of the csv
    # save the vector for the header 
    NCCAASCIIdataCSV <- str_sub(NCCAASCIIdataCSV, 1, -5)
    assign(NCCAASCIIdataCSV, NCCAASCIIdata, pos = 1)
    #
  }
  
} # end dataToCSV function
# 
#


#############

newDataToCSV <- function(x = dataNames) {
  # new function to read the NCCA ASCII time series data to data frams
  # add columns for the exam name and chart name
  # accepts as input a vector of text file names from the output of the dataFile function
  
  # a function to loop over the vector of chart data files
  for(i in 1:length(x)) {
    # make a vector of column names for each chart
    # this should be done individually for each chart
    # because some difference may exist between exams
    cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(str_sub(x[i], 1, -5)), open = "r"), 
                                            c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11), 
                                            header = FALSE, 
                                            skip = 0,
                                            n = 1))))
    myDF <- read.fwf(textConnection(get(str_sub(x[i], 1, -5)), open = "r"), 
                       c(6, 9, 9, 11, 11, 11, 11, 11, 11, 11), 
                       header = FALSE, 
                       skip = 1,
                       col.names = cNames,
                       n = -1L)                    
    # create the CSV file name
    outName <- paste(str_sub(x[i], 1, -5), ".csv", sep = "")
    # cat(myDF, file = outName, sep = "\n")
    write.csv(myDF, file = outName)
  }
} # newDataToCSV
  
newDataToCSV()
  
