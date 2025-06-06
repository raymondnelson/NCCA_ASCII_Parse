# R script to copy problem and interesting cases to the cwd
# Jan 1, 2021
# Raymond Nelson
#
####



# this path is prepended to the file path before sourcing a script
RPath <- "~/Dropbox/"



# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data")
setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100/FZCT_N100_NCCAASCII_Axciton/Set_1/check")



{
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/getExamNames.R'), echo=FALSE)
  
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
    
  }
  
  # uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  # uniqueExamNames <- uniqueExamNames[16]
  
  #### select an exam from the vector of exam names
  
  selectExams <- "ALL"
  # selectExams <- 32
  
  # keep only those selected exam numbers
  if(selectExams[1] != "ALL") {
    uniqueExamNames <- 
      c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams]
  } else uniqueExamNames <- 
    c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  
  print(paste("Found", length(uniqueExamNames), "exams"))
  print(uniqueExamNames)
  
}



{
  
  uniqueExamNames <- list.files(pattern=".pdf$")
  uniqueExamNames <- list.files()
  
  
  for(i in 1:length(uniqueExamNames)) {
  
    str_sub(objList[j], 1, which(strsplit(objList[j], "")[[1]] == "_")[1] - 1)
  
  }
  
  
  
  
}

