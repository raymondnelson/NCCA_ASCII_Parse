# R Script to generate Axciton type file names
# 2020-2-8
# Raymond Nelson
#
####



# contains 2 functions

# to generate random alpha-numeric names of 8 characters in length
# NCCAASCCINamerFn()

# to call the NCCAASCINamerFn and rename the exams in the cwd
# renameNCCAChartsFn() 



####



# library(stringr)



####



NCCAASCCINamerFn <- function(x=100, char="", charProp=0, lettersProp=1) {
  # function to simulate NCCA ASCII file names
  # call this function to create x number of axciton names
  # char is $ for axciton, & for lafayette, # for stoelting, % for limestone
  # charProp is the proportion of $ characters at the beginning of axciton names
  # lettersProp is the proportion of letters vs numbers in the file names
  # if char == "" letters will be used
  # output is a vector of random names
  # 
  ###
  
  {
    if(!exists("char")) char <- ""
    if(!exists("charProp")) charProp <- 0
    if(!exists('lettersProp')) lettersProp <- 1
  }
  
  outNames <- NULL
  
  while ( length(unique(outNames)) < x ) {
    
    outNames <- c(outNames,
                  # 8 random characters using LETTERS and 0:9
                  # first 2 characters
                  paste0(ifelse(char != "", 
                                paste0(char, char),
                                # use "DX" as the first 2 characters instead 
                                # "DX"
                                "X"
                                # or the first 2 characters are always letters,
                                # paste(sample(LETTERS, 2, replace=TRUE), collapse="") 
                                ),
                         # randomly include a third axciton $ char
                         ifelse((runif(1) <= charProp && char != ""),
                                char, 
                                # first character is always a letter when it is not char
                                sample(LETTERS, 1) ),
                         
                         # include 2 extra characters
                         paste(sample(LETTERS, 2, replace=TRUE), collapse=""),
                         
                         # fourth character is always a letter
                         sample(LETTERS, 1),
                         # lettersProp is an input parameter
                         # fifth character
                         ifelse(runif(1) <= lettersProp,
                                sample(LETTERS, 1),
                                sample(c(0:1), 1) ),
                         # sixth character
                         ifelse(runif(1) <= lettersProp,
                                sample(LETTERS, 1),
                                sample(c(0:1), 1) ),
                         # seventh character
                         ifelse(runif(1) <= lettersProp,
                                sample(LETTERS, 1),
                                sample(c(0:1), 1) ),
                         # eigth character
                         ifelse(runif(1) <= lettersProp,
                                sample(LETTERS, 1),
                                sample(c(0:1), 1) ) , 
                         
                         # include 4 binary numeric values
                         paste(sample(c(0:1), 4, replace=TRUE), collapse="")
                         
                         ) # end paste to construct the random name
                  ) # end c() to add the new name to the vector of new names
    
  }
  
  return(unique(outNames))
  
} # end NCCAASCCINamerFn() function





# set.seed(20200304)
set.seed(20230715)
newNames <- NCCAASCCINamerFn()

set.seed(20210218)
newNames <- NCCAASCCINamerFn(x=60, char="", charProp=0, lettersProp=.67)

set.seed(20211013)
newNames <- NCCAASCCINamerFn(x=60, char="", charProp=0, lettersProp=0)

unique(newNames)

# criterionState$examName <- newNames

# write_csv(criterionState, file="criterionState.csv")


# write_csv(as.data.frame(newNames), file="newNames.txt")
# newNames <- as.vector(read_lines(file="newNames.txt"))
# newNames <- newNames[2:length(newNames)]



#### generate random numeric strings ####


numCodeFn <- function(fileNames, x, n=4, criterionState=NULL) {
  # R function to generate a random numeric code of x digits
  # and append it to the NCCA ASCII file names in the CWD
  ####n
  # fileNames is a vector of NCCA ASCII file names 
  # x
  # n is the number of digits to include in the random numeric string
  # criterion state is a vector of criterion states for fileNames
  # 1 == innocent, 0 or -1 = guilty
  #
  # output is a vector of new file names
  # 
  # first and last digits will be equal when the criterion state is 1
  ####
  
  library(stringr)
  
  outVc <- rep(NA, length=length(fileNames))
  
  fileExt <- str_sub(fileNames, -5, -1)
  
  shortNames <- str_sub(fileNames, 1, -9)
  
  
  
  i=1
  for(i in 1:length(files)) {
    
    thisCriterionState <- criterionState[i]
    
    digString <- sample(c(0, 1), size=n, replace=TRUE)
    
    if(thisCriterionState == 1) {
      digString[n] <- digString[1]
    } else {
      digString[n] <- ifelse(digString==1, 0, 1)
    }
      
    outVc[i] <- paste(as.character(digString), collapse="", sep="")
    
  }
  
  return(outVc)
    
}

####

renameNCCAChartsFn <- function(chartNames=chartNames, char="&", series=NULL, oldID=oldID, newID=newID) {
  # function to rename NCCAASCII charts
  #
  # used to rename NCCA ASCII files
  #
  # chartNames is a vector of chartNames in the cwd
  # chartNames all start with D% D# D$ or D&
  # oldID is a vector of exam names to be renamed 
  # newID is a vector of new chartNames, same length as oldID
  # char is $ for axciton, & for lafayette, # for stoelting, % for limestone
  # 
  # output is a message
  #
  ####
  #
  # use a loop to rename the files 
  # comment this out when done
  i=1
  for (i in 1:length(chartNames)) {
    # get the exam name
    thisExamName <- str_sub(chartNames[i], 4, -7)
    # alternatively get the exam name from the first line in the NCCA ASCII data
    # thisExamName <- str_sub(read_lines(chartNames[i], n_max=1), 23,-7)
    # get the new exam name
    newExamName <- newID[which(oldID == thisExamName)]
    # get the series name
    seriesName <- ifelse(isNULL(series), 
                         str_sub(chartNames[i],-6, -6),
                         series)
    # construct the new file name using the existing series and chart name
    newFileName <- paste0(paste0("D", char, "-"),
                          newExamName,
                          "-",
                          seriesName,
                          str_sub(chartNames[i], -4, -1) )
    # rename the file
    file.rename(chartNames[i], newFileName)
  }
  
  print(paste(length(chartNames), "Charts in", length(newID), "exams renamed"))
  
} # end renameNCCAChartsFn() function


# call the function
# renameNCCAChartsFn(chartNames=chartNames,
#                    char="&",
#                    series=NULL, 
#                    oldID=oldID, 
#                    newID=newID)



#### get the charts and exams from the cwd ####

getCWDCharts <- FALSE
# getCWDCharts <- TRUE


if(isTRUE(getCWDCharts)) {
  
  # library(readr)
  
  # ACH2020_N100_oldID_newID <- read_csv("ACH2020_N100_oldID_newID.csv")
  
  # criterionStateDF <- read_csv("criterionState.csv")
  # View(criterionStateDF)
  
  # source this to load the getCharts function
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/getExamNames.R'), echo=FALSE)
  
  # search string for Axciton NCCA ASCII files
  searchPattern <- "^D\\$"
  searchPattern <- "^D&-"
  
  # initialize a vector of chart names in the working dir
  chartNames <- getCharts(x=searchPattern, uniqueTests=FALSE)
  
  # intialize a vector of unique exams
  uniqueExamNames <- getCharts(x=searchPattern, uniqueTests=TRUE)
  
  # oldID <- criterionStateDF$ID
  oldID <- uniqueExamNames
  
  # newID <- newNames[1:1]
  newID <- newNames
  
  newCriterionStateDF <- cbind.data.frame(examName=newID, 
                                          oldName=oldID, 
                                          oldAxName=NA,
                                          criterionState=NA)
  
  # criterionStateDF$examName <- uniqueExamNames
  # criterionStateDF$criterionState <- -1
  # criterionStateDF$newID <- newID
  
  # criterionStateDF$newID <- newID
  
  # populate the criterion state and old name columns
  for(i in 1:nrow(newCriterionStateDF)) {
    newCriterionStateDF$oldAxName[i] <- 
      criterionStateDF$oldID[which(criterionStateDF$axID == 
                                     newCriterionStateDF$oldName[i])]
    newCriterionStateDF$oldAxName[i] <- 
      criterionStateDF$criterionState[which(criterionStateDF$axID == 
                                              newCriterionStateDF$oldName[i])]
  }
  
  # View (newCriterionStateDF)
  
  # write_csv(criterionStateDF, "criterionState.csv")
  
  # newID <- ACH2020_N100_oldID_newID$newID
  
  
} # end if()


