# functions to make a list of exams in the current working directory
# October 3, 2016
# raymond.nelson@gmail.com
# 
# removed from the NCCAASCIIParse.R script
#
# this script contains the following functions 

# getCharts() 
# function to make a vector of names of NCCAASCII text files for all exams in the current directory

# uniqueNames()
# function to make a vector of names of unique exams in the current directory

########


library(stringr)


# function to make a vector of the names of files with the NCCA ASCCI output character string
getCharts <- function(x=searchPattern, thisDir=".", uniqueTests=FALSE) {
  # function to make a vector of names of files with specified strings in the file names
  #
  # the x input  string should identify the NCCA ASCII output files 
  # uses a regular expression
  # "D&" for lafayette
  # "D$"" for axciton
  # "D#" for stoelting
  # "D%" for limestone
  #
  # thisDir specified which directory to search  default to "."
  #
  # output is a vector of file names in the current working directory
  #
  # uniqueTests=FALSE will output the name of every NCCA ASCII chart
  # uniqueTests=TRUE will ouput a vector of unique exam names
  #
  ###
  # make a list of files
  y <- list.files(path = thisDir, 
                  pattern = x, 
                  all.files = FALSE,
                  full.names = FALSE, 
                  recursive = FALSE,
                  ignore.case = FALSE, 
                  include.dirs = FALSE,
                  no.. = FALSE)
  # keep the files that satisfy the selection criteria
  y <- grep(x, y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = FALSE) # includes only those files that match the input string
  # excludes .txt files
  y <- grep("*.txt", y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = TRUE) 
  # exclude .csv files
  y <- grep("*.csv", y, 
            ignore.case = FALSE, 
            perl = FALSE, 
            value = TRUE,
            fixed = FALSE, 
            useBytes = FALSE, 
            invert = TRUE) # excludes .csv files
  # create a vector of file names
  # assign("fileNames", y, pos = 1)
  if(isTRUE(uniqueTests)) {
    y <- unique(str_sub(y, 4, -7))
  }
  return(y)
} # getCharts
# fileNames <- getCharts("D&+", uniqueTests=FALSE)
# uniqueExamNames <- getCharts("D&+", uniqueTests=TRUE)




###############




# function to make a list of uniques exams in the cwd
uniqueNames <- function(x = fileNames) {
  # function to make a list of uniques exams in a directory
  # input is a vector of unique file names from the getCharts function
  # output is a vector of unique exam names
  y <- unique(str_sub(x, 4, -7))
  return(y)
}
# uniqueExamNames <- uniqueNames(fileNames) # need to call the getCharts function first



# print("make a vector of unique exams in the currect working directory")
# 
# # call the two functions together to make a list of unique exams in the current working directory
# uniqueExamNames <- uniqueNames(getCharts(searchPattern))
# uniqueExamNames <- uniqueExamNames[1]
# 
# print(uniqueExamNames)




#################




{
  
  searchPattern1 <- NULL
  searchPattern2 <- NULL
  searchPattern3 <- NULL
  searchPattern4 <- NULL
  
  uniqueExamNames1 <- NULL
  uniqueExamNames2 <- NULL
  uniqueExamNames3 <- NULL
  uniqueExamNames4 <- NULL
  
  # Lafayette uses the &
  searchPattern1 <- "^D&+"
  # Stoelting #
  # 2010 changed to !
  searchPattern3 <- "^D#+"
  # Limestone %
  searchPattern4 <- "^D%+"
  # 2010 changed to @
  # Axciton $ character needs to be double escaped because it is a special character for grep
  searchPattern2 <- "^D\\$"
  
  # call the two functions together to make a list of unique exams in the current working directory
  if(!is.null(searchPattern1)) uniqueExamNames1 <- getCharts(x=searchPattern1, uniqueTests=TRUE)
  if(!is.null(searchPattern2)) uniqueExamNames2 <- getCharts(x=searchPattern2, uniqueTests=TRUE)
  if(!is.null(searchPattern3)) uniqueExamNames3 <- getCharts(x=searchPattern3, uniqueTests=TRUE)
  if(!is.null(searchPattern4)) uniqueExamNames4 <- getCharts(x=searchPattern4, uniqueTests=TRUE)
  
  if(length(uniqueExamNames1)==0) searchPattern1 <- NULL
  if(length(uniqueExamNames2)==0) searchPattern2 <- NULL
  if(length(uniqueExamNames3)==0) searchPattern3 <- NULL
  if(length(uniqueExamNames4)==0) searchPattern4 <- NULL
  
  uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
}




#################






