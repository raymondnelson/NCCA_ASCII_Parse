# R script to manage NCCA ASCII files
# April 4, 2025
# Raymond Nelson
####



# this path is prepended to the file path before sourcing a script
# RPath <- "C://Users/raymo/Dropbox/"

if(!exists("RPath")) {
  # mac
  RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
  # windows
  # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  
  # use this
  # source(paste0(RPath, <filePath>), echo=FALSE)
}



# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data")
# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100/FZCT_N100_NCCAASCII_Axciton/Set_1/check")
setwd("C:/Users/raymo/Dropbox/DATASETS/AFMGQTN22")



{
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/getExamNames.R'), echo=FALSE)
  source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  
  # getCharts() will locate NCCA ASCII charts in the cwd
  # uniqueNames() is used to make a list of uniques exams in a directory
  
  print("search the working directory for NCCA ASCII text output")
  
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
    
    # Axciton $ character needs to be double escaped because it is a special character for grep
    searchPattern2 <- "^D\\$"
    
    # Stoelting uses the octothorpe #
    # 2010 changed to !
    searchPattern3 <- "^D#+"
    
    # Limestone uses the percent sign %
    # 2010 changed to @
    searchPattern4 <- "^D%+"
    
    # call the two functions together to make a list of unique exams in the current working directory
    if(!is.null(searchPattern1)) uniqueExamNames1 <- getCharts(x=searchPattern1, uniqueTests=TRUE)
    if(!is.null(searchPattern2)) uniqueExamNames2 <- getCharts(x=searchPattern2, uniqueTests=TRUE)
    if(!is.null(searchPattern3)) uniqueExamNames3 <- getCharts(x=searchPattern3, uniqueTests=TRUE)
    if(!is.null(searchPattern4)) uniqueExamNames4 <- getCharts(x=searchPattern4, uniqueTests=TRUE)
    
    if(length(uniqueExamNames1)==0) searchPattern1 <- NULL
    if(length(uniqueExamNames2)==0) searchPattern2 <- NULL
    if(length(uniqueExamNames3)==0) searchPattern3 <- NULL
    if(length(uniqueExamNames4)==0) searchPattern4 <- NULL
    
  }
  
  #### select an exam from the vector of exam names
  
  selectExams <- "ALL"
  # selectExams <- 32
  
  # keep only those selected exam numbers
  if(selectExams[1] != "ALL") {
    uniqueExamNames <- 
      c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams]
  } else uniqueExamNames <- 
    c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  # uniqueExamNames <- uniqueExamNames[16]
  
  print(paste("Found", length(uniqueExamNames), "exams"))
  print(uniqueExamNames)
  
}


library(readr)


# write.csv(uniqueExamNames, file="fileList.txt")
write.table(x=uniqueExamNames, file="fileList.txt", col.names=FALSE, row.names=FALSE)
