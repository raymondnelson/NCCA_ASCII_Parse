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
            invert = FALSE) # includes these files only
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
  #
  # create a vector of file names
  assign("fileNames", y, pos = 1)
  # use <- or = instead
  #
  return(y)
}