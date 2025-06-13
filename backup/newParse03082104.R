# make a vector of all NCCA ASCII output files
# identify unique exams 
# load each exam
# separate the header and data 
# make a tidy data set for each exam

# First Fix all file and directory names

# remove characters from all files
fileNames <- list.files()
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("\\(", "", fileNames[i]))
}
newFileNames <- list.files()

# remove characters from all files
fileNames <- list.files()
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("\\)", "", fileNames[i]))
}
newFileNames <- list.files()

# replace characters for all files
fileNames <- list.files()
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("Cleaned_Copy", "CleanedCopy", fileNames[i]))
}
newFileNames <- list.files()

getNCCAASCII <- function(x = "D&+") {
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
  
  #   y <- grep(x, y, 
  #             ignore.case = FALSE, 
  #             perl = FALSE, 
  #             value = TRUE,
  #             fixed = FALSE, 
  #             useBytes = FALSE, 
  #             invert = FALSE) # includes these files only
}

NCCAASCIICharts <- getNCCAASCII()

uniqueNCCAASCII <- function(x = NCCAASCIICharts) {
  # function to make a vector of unique exams
  # requires output from getNCCAASCII function
  y <- substr(x, 1, nchar(x) - 6)
  y <- unique(y)
  return(y)
}

uniqueExams <- uniqueNCCAASCII()

headerData <- function(x = uniqueExams) {
  # function to separate header from data for each exam
  # requires output from uniqueNCCAASCII function
  # make a vector of charts for each unique exam
  # make a header vector for each chart
  # make a data vector for each chart
  # append + to the name of each unique exam in the vector
  x <- paste(x, "+", sep = "")
  # x <- strtrim(x, length(x) - 1)
  # 
  y <- list.files(path = ".", 
                  pattern = x, 
                  all.files = FALSE,
                  full.names = FALSE, 
                  recursive = FALSE,
                  ignore.case = FALSE, 
                  include.dirs = FALSE,
                  no.. = FALSE)
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
  
  # separate the header info and data for each chart
  for (j in 1:length(y)) {
    # read each file
    fName <- y[j]
    NCCAASCIIData <- readLines(fName, n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    # determine the number of lines in the header section by locating the first line of the time series data
#     headerLength <- pmatch("1", str_trim(strtrim(NCCAASCIIData, 6), side = "both")) - 2
    headerLength <- pmatch("Sample     Time", str_trim(strtrim(NCCAASCIIData, 15), 
                                              side = "both")) - 1 # select the last line preceding the data
    # need to clean the name from problem characters
    fNameClean <- gsub("\\.", "_", fName)
    fNameClean <- gsub("\\-", "_", fNameClean)
    fNameClean <- gsub("D&_", "", fNameClean)

    # make a data frame for the header info for each file  
    assign(paste(fNameClean, "_header", sep = ""), NCCAASCIIData[1:headerLength], pos = 1)
    # determine the starting line for the time series data
#     dataStart <- pmatch("     1", str_trim(strtrim(NCCAASCIIData, 6), side = "both")) -1
    dataStart <- pmatch("Sample     Time", str_trim(strtrim(NCCAASCIIData, 15), 
                                           side = "both"))
    # make a data frame for the time series data for each chart
    assign(paste(fNameClean, "_data", sep = ""), NCCAASCIIData[dataStart:length(NCCAASCIIData)], pos = 1)
    
  }
  return(y)
}

headerData()





loadUnique <- function(x = uniqueExams) {
  # function to load the charts for each exam
  for (i in 1:length(x)) {
    # make a vector of charts for each unique exam
    y <- list.files(path = ".", 
                    pattern = x[i], 
                    all.files = FALSE,
                    full.names = FALSE, 
                    recursive = FALSE,
                    ignore.case = FALSE, 
                    include.dirs = FALSE,
                    no.. = FALSE)
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
    # separate the header info and data for each chart
    for (j in 1:length(y)) {
      linesAll <- readLines(y[j], n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
      headerLength <- pmatch("Sample     Time", 
                             str_trim(strtrim(linesAll, 15), 
                                      side = "both")) - 1
      assign(paste("chart", j, "header", sep = ""), 
             linesAll[1:headerLength], 
             pos = 1)
      assign(paste("chart", j, "data", sep = ""), 
             linesAll[(headerLength+1):length(linesAll)],
             pos = 1)      
    }
  }
}

loadUnique()

