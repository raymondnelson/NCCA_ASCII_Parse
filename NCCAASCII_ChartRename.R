# R Script to rename NCCA ASCII charts
# 2020-2-8
# Raymond Nelson
#
####

# contains one function
#
#  renameNCCAChartsFn()
# 
# along with a script to get the charts in the CWD

####



######## LOCATE THE NCCA ASCII FILES IN THE CWD ##########


{
  
  library(stringr)
  
  library(readr)
  
  if(!exists("RPath")) {
    # mac
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  }
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  
  if(!exists("NCCAASCIIChartNames")) {
    NCCAASCIIChartNames <- getCharts((x="D\\&"))
  }
  
  if(!exists("uniqueExamNames")) {
    uniqueExamNames <- uniqueNames(getCharts((x="D\\&")))
  }
  
  
  uniqueSeriesNames <- str_sub(getCharts((x="D\\&")), -5, -5)
  
  
  uniqueChartNames <- str_sub(getCharts((x="D\\&")), -3, -1)
  
  
  print(uniqueExamNames)
  
  print(NCCAASCIIChartNames)
  
}


############## rename NCCA ASCII charts ##############


renameNCCAChartsFn <- function(chartNames=chartNames, char="&", series=NULL, oldID=oldID, newID=newID) {
  # function to rename NCCAASCII charts
  #
  # used to rename NCCA ASCII files
  #
  # chartNames is a vector of chartNames in the cwd
  # chartNames all start with D% D# D$ or D&
  # char is $ for axciton, & for lafayette, # for stoelting, % for limestone
  # series will be used in the new names and will keep the old name if this is NULL
  # oldID is a vector of exam names to be renamed 
  # newID is a vector of new chartNames, same length as oldID
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


NCCAASCIIChartNames


# call the function
# renameNCCAChartsFn(chartNames=chartNames,
#                    char="&",
#                    series=NULL, 
#                    oldID=oldID, 
#                    newID=newID)





############## get the charts and exams from the cwd #################



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




#################




