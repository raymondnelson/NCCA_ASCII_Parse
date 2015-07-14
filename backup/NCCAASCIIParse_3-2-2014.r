################ R Functions for parsing NCCA ASCII text output ##############
# 3-2-2014 Raymond Nelson
# 8-1-2014
#
#
# this script contains the following functions
#
# getcharts() 
# to make a character vector of the names of NCCAASCII text output files
#
# headerFile()
# to make a separate txt file of the header information
#
# dataFile()
# to make a separate txt file of the time series data
#
#
###################################
#
#
#
getCharts <- function(x = "D&+") {
  # function to make a list of files with specified strings in the file names
  # the needed string should identify the NCCA ASCII output files beginning with "D&"
  # uses a regular expression in the argument
  y <- list.files(path = ".", 
                  pattern = x, 
                  all.files = FALSE,
                  full.names = FALSE, 
                  recursive = FALSE,
                  ignore.case = FALSE, 
                  include.dirs = FALSE,
                  no.. = FALSE)
  y <- grep(x, y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = FALSE) # includes only those files that match the input string
  y <- grep("*.txt", y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = TRUE) # excludes .txt files
  y <- grep("*.csv", y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = TRUE) # excludes .csv files
  # create a vector of file names
  assign("fileNames", y, pos = 1)
  return(y)
}
#
####################################
#
#
#
headerFile <- function(x = fileNames) {
  # function to loop over the NCCA ASCII file names in the fileNames vector
  # and separate the header info
  # x = character vector containing names of NCCA ASCII output files
  #
  # this function recursively creates .txt files 
  # from the header info of all charts in the "fileNames" vector
  #
  # make a null vector to hold the names of the headerFiles
  names <- character()
  # define a function to run in a loop
  headerParse <- function(fileNames = x) {
    # read 100 lines from each file
    y <- readLines(fileNames, n = 100L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    # locate the beginnin of the time-series data - the end of the header section
    z <- pmatch("Sample     Time", str_trim(strtrim(y, 15), side = "both")) - 1
    # keep only the header lines
    y <- y[1:z]
    return(y)
  }
  #
  # loop over the file names and read the files
  for (i in 1:length(x)) {
    # call the function to loop over the file names and read the files
    m <- headerParse(x[i])
    # set the name of the header file
    n <- paste(str_sub(x[i], 4, -1), "_header", sep = "") # start with 4th character
    # fix problem characters
    n <- str_replace(n, ")-", "_")
    n <- str_replace(n, "Cleaned Copy", "CleanedCopy")
    n <- str_replace(n, " \\(", "_")
    n <- str_replace_all(n, "\\.", "_")
    n <- str_replace_all(n, " ", "_")
    n <- str_replace_all(n, "-", "_")
    n <- str_replace_all(n, "D&_", "")
    # add the file extension
    n <- paste(n, ".txt", sep = "")
    # 
    # add the name to the names vector
    names <- c(names, n)
    # create a character vector for the header 
    assign(strtrim(n, nchar(n) -4), m, pos = 1)
    #
    # recursively save the header section as a text file
    cat(m, file = n, sep = "\n")
  }
  # return the character vector of header file names
  assign("headerNames", names, pos = 1)
  return(names)
}
#
##########################################
#
#
#
dataFile <- function(x = fileNames) {
  # function to loop over the NCCA ASCII file names and separate the header info
  # x = character vector containing names of NCCA ASCII files
  #
  # this function recursively creates .txt files 
  # from the time series data of all charts in the "fileNames" vector
  #
  # make a null vector to hold the names of the headerFiles
  names <- character()
  # define a function to run in the loop
  # this one will loop over the file names and read the files
  dataParse <- function(fileNames = x) {
    y <- readLines(fileNames, n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    # locate the first time-series sample
    z <- pmatch("Sample     Time", strtrim(y, 15))
    # keep only the data lines
    y <- y[z:length(y)]
    return(y)
  }
  #
  for (i in 1:length(x)) {
    # call the function to loop over the file names and read the files
    m <- dataParse(x[i])
    # set the name of the data file
    n <- paste(str_sub(x[i], 4, -1), "_data", sep = "")
    # fix problem characters
    n <- str_replace(n, ")-", "_")
    n <- str_replace(n, "Cleaned Copy", "CleanedCopy")
    n <- str_replace(n, " \\(", "_")
    n <- str_replace_all(n, "\\.", "_")
    n <- str_replace_all(n, " ", "_")
    n <- str_replace_all(n, "-", "_")
    n <- str_replace_all(n, "D&_", "")
    # add the file extension
    n <- paste(n, ".txt", sep = "")
    #
    # add the name to the names vector
    names <- c(names, n)
    # create a character vector for the data 
    assign(strtrim(n, nchar(n) -4), m, pos = 1)
    #
    # recursively save each data section as a text file
    cat(m, file = n, sep = "\n")
  }
  # return the character vector of data file names
  assign("dataNames", names, pos = 1)
  return(names)
}
#
#############################################
