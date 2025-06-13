# R scrip to copy exam materials to the cwd
# November 29, 2024
# Raymond Nelson
####



rm(list=ls())


if(!exists("RPath")) {
  # mac
  RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
  # windows
  # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
}


{
  library(stringr)
  
  library(readr)
}

  
######## LOCATE THE NCCA ASCII FILES IN THE PARENT DIRECTORY ##########


{
  
  ## get the exam names in the parent directory ##
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  
  # rm(NCCAASCIIChartNamesP)
  
  if(!exists("NCCAASCIIChartNamesP")) {
    # NCCAASCIIChartNamesP <- getCharts(x="D\\$", thisDir="../../")
    NCCAASCIIChartNamesP <- getCharts(x="D&", thisDir="../../")
    NCCAASCIIChartNamesP <- getCharts(x="D&", thisDir="../")
  }
  
  if(!exists("uniqueExamNamesP")) {
    uniqueExamNamesP <- uniqueNames(NCCAASCIIChartNamesP)
  }
  
  # uniqueSeriesNames <- uniqueNames(str_sub(getCharts(x="D\\&", thisDir="../"), -5, -5))
  
  # uniqueChartNames <- uniqueNames(str_sub(getCharts(x="D\\&", thisDir="../"), -3, -1))
  
  print(uniqueExamNamesP)
  
  print(NCCAASCIIChartNamesP)
  
}

  
######## GET THE EXAM NAMES IN THE CWD USING THE chartPlot.pdf NAMES ######## 

{

  ## get the exams from the cwd using chartPlot names ##
  
  # library(stringr)
  
  ## initialize a function to locate exams via the chartPlot.pdf
  chartPlotNamesFn <- function(x="chartPlot.pdf", startLoc=1, endLoc=-15, thisDir=".", uniqueTests=TRUE) {
    # R function to get unique examNames from chartPlot.pdf graphics
    # Dec 1, 2024
    # Raymond Nelson
    # startLoc=1 for most exams
    # startLoc = 2 for simulated exams that start with D
    # endLoc = -1 to get the entire file name
    # endLoc = -15 to remove the "chartPlot.pdf" ending
    # endLoc = -16 if the criterion state is included at the end of the file name
    ####
    y <- list.files(path = thisDir, 
                    pattern = x, 
                    all.files = FALSE,
                    full.names = FALSE, 
                    recursive = FALSE,
                    ignore.case = FALSE, 
                    include.dirs = FALSE,
                    no.. = FALSE)
    # # keep the files that satisfy the selection criteria
    # y <- grep(x, y, 
    #           ignore.case = FALSE, 
    #           perl = FALSE, 
    #           value = TRUE,
    #           fixed = FALSE, 
    #           useBytes = FALSE, 
    #           invert = FALSE) # includes only those files that match the input string
    # # excludes .txt files
    # y <- grep("*.txt", y, 
    #           ignore.case = FALSE, 
    #           perl = FALSE, 
    #           value = TRUE,
    #           fixed = FALSE, 
    #           useBytes = FALSE, 
    #           invert = TRUE) 
    # # exclude .csv files
    # y <- grep("*.csv", y, 
    #           ignore.case = FALSE, 
    #           perl = FALSE, 
    #           value = TRUE,
    #           fixed = FALSE, 
    #           useBytes = FALSE, 
    #           invert = TRUE) # excludes .csv files
    # create a vector of file names
    # assign("fileNames", y, pos = 1)
    # trim the output to the exam name, removing 
    require(stringr)
    y <- str_sub(y, startLoc, endLoc)
    # restrict the output to unique exam names
    if(isTRUE(uniqueTests)) {
      y <- unique(y)
      # y <- unique(str_sub(y, 4, -7))
    }
    return(y)
  }
  
  # 
  examList <- chartPlotNamesFn(x="chartPlot.pdf", 
                                      startLoc=2, 
                                      endLoc=-16, 
                                      thisDir=".", 
                                      uniqueTests=TRUE)
    
  
  print(examList)
  
}


############ GET THE EXAMS IN THE CWD USING THE NCCA ASCII CHART NAMES ##############


{

  ## get the the exams in the cwd using the NCCA ASCII names ##

  # library(stringr)
  # 
  # library(readr)
  # 
  # if(!exists("RPath")) {
  #   # mac
  #   RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
  #   # windows
  #   # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  # }
  # 
  
  # # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  # source(paste0(RPath, 'getExamNames.R'), echo=FALSE)

  # getTheseNCCAASCIIChartNames <- getCharts(x="D\\$", thisDir=".")
  getTheseNCCAASCIIChartNames <- getCharts(x="D&", thisDir=".")
  # getTheseUniqueExamNames <- uniqueNames(getCharts(x="D\\$", thisDir="."))
  getTheseUniqueExamNames <- uniqueNames(getCharts(x="D&", thisDir="."))
  
  getTheseUniqueExamNames <- str_sub(getTheseUniqueExamNames, 1, -3)

  print(getTheseUniqueExamNames)
  
  examList <- getTheseUniqueExamNames

  print(getTheseNCCAASCIIChartNames)
  
  print(examList)

  # examList <- str_sub(list.files(path="..", pattern="chartPlot.pdf"), 2, -15)

  # examList <- str_sub(list.files(path="..", pattern="chartPlot.pdf"), 2, -16)

  # uniqueExamNames <- str_sub(uniqueExamNames, 1, -3)

  getTheseUniqueExamNames %in% examList

}


######## COPY THE EXAM FILES FROM THE PARENT DIRECTORY ##########


copyExamFiles <- TRUE
copyExamFiles <- FALSE


if(copyExamFiles) {
  
   # dirString <- "../Archive 7/"
    dirString <- "../../"
    i=1
    for(i in 1:length(getTheseUniqueExamNames)) {
      theseFiles <- list.files(path=dirString, pattern=getTheseUniqueExamNames[i])
      # theseFiles <- 
      # file.copy()
      file.copy(paste0(dirString, theseFiles), theseFiles, overwrite = TRUE)
    }
  
  copyExamFiles <- FALSE
  
}





######## COPY the NCCA ASCII files from a parent directory ######## 



{
  
  # copyExamNames <- c("X2VA8A", "X34H8", "X3Q9RG", "X3RQW8J", "X431", "X4C7Z7", "X51NZ75")
  # 
  # fileList <- list.files(path="..")
  # 
  # i=1
  # for(i in 1:length(copyExamNames)) {
  # 
  #   thisExamName <- copyExamNames[i]
  # 
  #   # copyTheseFiles <- fileList[grep(paste0("D\\$-", thisExamName), fileList)]
  #   copyTheseFiles <- fileList[grep(thisExamName, fileList)]
  # 
  #   file.copy(from=paste0("../", copyTheseFiles), to=".")
  # 
  # }
  
}



######## REMOVE AN EXAM FROM THE CWD ######## 



{
  
  removeExamFn <- function(x) {
    # R function to remove an NCCA ASCII exam from the cwd
    # March 11, 2025
    ##
    # x can be a vector of exam names in the cwd
    for (i in 1:length(x)) {
      rm(list=ls(pattern=x[i], envir=.GlobalEnv), envir=.GlobalEnv)
    }
    uniqueExams <- getUniqueExams(x="*_Data$")
    assign("uniqueExams", uniqueExams, envir=.GlobalEnv) 
    return(paste0(x, "removed from the cwd"))
  } 
  
  # removeExamFn("DX5D7HJU")
  
}



############## copy exams from a parent directory using the examNames


# "6YCHNU" "7BHJU"  "7U3XVI" "8AIL83" "8D6MH"  "8VQD0" 




# file.copy(from=paste0("../Archive 7/", getTheseNCCAASCIIChartNames), to="./", overwrite=TRUE)
