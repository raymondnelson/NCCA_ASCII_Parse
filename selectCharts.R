# script to select NCCA ASCII charts


setwd("~/Dropbox/CURRENT_PROJECTS/Backster exploratory/Espionage question Final")


getCharts <- function(x = "D&+") {
  # function to make a list of files with specified strings in the file names
  # the needed string should identify the NCCA ASCII output files beginning with "D&"
  # uses a regular expression in the input argument
  # output is a vector of file names for polygraph NCCA ASCII charts in the working directory
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
  # assign("fileNames", y, pos = 1)
  return(y)
} # getCharts


fileNames <- getCharts("D&+")


DAT <- readLines(fileNames[1])


excludeNames <- y <- grep("*1.01A", fileNames, 
                       ignore.case = FALSE, 
                       perl = FALSE, 
                       value = TRUE,
                       fixed = FALSE, 
                       useBytes = FALSE, 
                       invert = FALSE) 


library(stringr)


for (i in 1:length(excludeNames)) {
  file.rename(excludeNames[i], str_sub(excludeNames[i], 4, -1) )
}


fileNames <- getCharts("D&+")


for (i in 1:length(fileNames)) {
  DAT <- readLines(fileNames[i], n=100L)
  # determine the presence of the search string
  if(length(which(str_detect(DAT, "R33"))) > 0) {
    file.rename(fileNames[i], str_sub(fileNames[i], 4, -1) )
  }
}


fileNames <- getCharts("D&+")


