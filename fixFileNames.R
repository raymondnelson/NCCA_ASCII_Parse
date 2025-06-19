# ########### R script to fix NCCA ASCII file and directory names ############
# 1-2-2015 Raymond Nelson
# 6-12-2015
#
# 
#
##############################################

# fileNames <- list.files()

# searchPattern <- "D&+"
# searchPattern <- "D\\$+"
# searchPattern <- "D#+"
# searchPattern <- "D%+"
# searchPattern <- "D!+"
# searchPattern <- "D@+"


fixFileNamesFn <- function(x=searchPattern) {
  # function to fix problem characters in NCCAASCII filenames
  # 8-1-2016 raymond nelson
  # x input is a grep search pattern to identify the NCCAASCII files in the working dir
  # output is a vector of new file names that were checked or fixed for problem characters
  ####
  
  # x <- "^D&+"
  # x <- "^D#+"
  # x <- "^D%+"
  # x <- "^D\\$"
  
  # get the exam names from the working directory
  fileNames <- list.files(path = ".", 
                          pattern = x, 
                          all.files = FALSE,
                          full.names = FALSE, 
                          recursive = FALSE,
                          ignore.case = FALSE, 
                          include.dirs = TRUE,
                          no.. = FALSE)
  
  # get the names from the global environment instead of the working directory
  # fileNames <- grep("D&+", fileNames,
  #           ignore.case = FALSE,
  #           perl = FALSE,
  #           value = TRUE,
  #           fixed = FALSE,
  #           useBytes = FALSE,
  #           invert = FALSE)
  
  # fix non-ASCII characters in the file name
  #  Encoding(fileNames) <- "latin1"
  newFileNames <- iconv(fileNames, "latin1", "ASCII", sub="_")
  
  {
    newFileNames <- gsub("[Ì]+", "_", newFileNames)
    newFileNames <- gsub("[Ñ]+", "_", newFileNames)
    newFileNames <- gsub("[_]+", "_", newFileNames)
    newFileNames <- gsub("[É]+", "_", newFileNames)
    newFileNames <- gsub("[é]+", "_", newFileNames)
  }
  
  Encoding(newFileNames) <- "ASCII"

  {
    # remove these words
    newFileNames <- gsub("Familiarizacion", "", newFileNames)
    newFileNames <- gsub("FAMILIARIZATION", "", newFileNames)
    newFileNames <- gsub("Familiarizacion", "", newFileNames)
    newFileNames <- gsub("PRACTICA", "", newFileNames)
    newFileNames <- gsub("practica", "", newFileNames)
    newFileNames <- gsub("Practica", "", newFileNames)
  }
  
  {
    # fix problem characters
    newFileNames <- gsub("[[ ]]+", "_", newFileNames)
    newFileNames <- gsub("\\(", "", newFileNames)
    newFileNames <- gsub("\\)", "", newFileNames)
    newFileNames <- gsub("[ ]+", "_", newFileNames)
    newFileNames <- gsub("[,]+", "_", newFileNames)
    newFileNames <- gsub("Cleaned_Copy", "", newFileNames)
    newFileNames <- gsub("CleanedCopy", "", newFileNames)
    newFileNames <- gsub("CLEANED_COPY", "", newFileNames)
    newFileNames <- gsub("CLEANEDCOPY", "", newFileNames)
    # newFileNames <- gsub("[_]+", "_", newFileNames)
    # newFileNames <- gsub("[_]+", "", newFileNames)
  }
  
  
  
  # these are part of the NCCA ASCII file name spec
  # newFileNames <- gsub("[-]+", "_", newFileNames)
  # newFileNames <- gsub("[.]+", "_", newFileNames)
  
  {
    # to restore the NCCA ASCII spec names
    # newFileNames <- gsub("[_]+", "-", newFileNames)
    newFileNames <- gsub("D\\$_", "D\\$-", newFileNames)
    newFileNames <- gsub("D\\&_", "D\\&-", newFileNames)
    newFileNames <- gsub("_1_01A", "-1\\.01A", newFileNames)
    newFileNames <- gsub("_1_02A", "-1\\.02A", newFileNames)
    newFileNames <- gsub("_1_03A", "-1\\.03A", newFileNames)
    newFileNames <- gsub("_1_04A", "-1\\.04A", newFileNames)
    newFileNames <- gsub("_1_05A", "-1\\.05A", newFileNames)
    newFileNames <- gsub("_1_06A", "-1\\.06A", newFileNames)
    newFileNames <- gsub("_1_07A", "-1\\.07A", newFileNames)
    newFileNames <- gsub("_1_08A", "-1\\.08A", newFileNames)
    newFileNames <- gsub("_2_01A", "-2\\.01A", newFileNames)
    newFileNames <- gsub("_2_02A", "-2\\.02A", newFileNames)
    newFileNames <- gsub("_2_03A", "-2\\.03A", newFileNames)
    newFileNames <- gsub("_4_04A", "-2\\.04A", newFileNames)
    newFileNames <- gsub("_5_05A", "-2\\.05A", newFileNames)
    newFileNames <- gsub("_6_05A", "-2\\.06A", newFileNames)
    newFileNames <- gsub("_7_05A", "-2\\.07A", newFileNames)
    newFileNames <- gsub("_8_05A", "-2\\.08A", newFileNames)
  }
  
  # loop over the file names and rename the files
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], newFileNames[i])
  }
  
  ##### add an alphabetic fourth character if necessary,
  # so that Axction filenames do not start with a numeric value 
  # for (i in 1:length(newFileNames)) {
  #   if(tolower(str_sub(newFileNames[i], 4, 4)) %in% letters == FALSE) {
  #     file.rename(newFileNames[i], paste0(str_sub(newFileNames[i], 1, 3), "X", str_sub(fileNames[i], 5, -1)) )
  #   }
  # }
  
  ##### remove the X fourth character to restore Axciton file names
  # Feb 1, 2020
  # i=1
  # for (i in 1:length(newFileNames)) {
  #   if(isTRUE(tolower(str_sub(newFileNames[i], 4, 4)) == "x")) {
  #     newFileNames[i] <- paste0( "D$-X" , str_sub(newFileNames[i], 5, -1) )
  #     file.rename(fileNames[i],
  #                 newFileNames[i] )
  #   }
  # }

  
  
  # rm(newFileNames)
  
  
  
  ######## lines below are commented out 10-4-2016 rn
  
  # loops are not necessary because gsub is vectorized
  
  # replace space characters for all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("[ ]+", "_", fileNames[i]))
  # }
  
  # replace "-" in all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("[-]+", "_", fileNames[i]))
  # }
  
  # replace "," in all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("[,]+", "_", fileNames[i]))
  # }
  
  # remove ( characters from all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("\\(", "", fileNames[i]))
  # }
  
  # remove ) characters from all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("\\)", "", fileNames[i]))
  # }
  
  # remove double __ characters from all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("[_]+", "_", fileNames[i]))
  # }
  
  # remove "_Cleaned_Copy" in all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("_Cleaned_Copy", "", fileNames[i]))
  # }

  ######### 10-4-2016
  
  ############
  # remove "$" in all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("$", "", fileNames[i]))
  # }
  
  # these lines escape the \\$ character
  # remove "$" in all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("$", "\\$", fileNames[i]))
  # }
  
  # remove "D&-" in all filenames
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], gsub("D&-", "", fileNames[i]))
  # }
  
  # add the D&- prefix if necessary
  # fileNames <- list.files(pattern = "D&+")
  # for (i in 1:length(fileNames)) {
  #   file.rename(fileNames[i], paste0("D&-", "", fileNames[i]))
  # }
  
  return(newFileNames)
  
} # end fixFileNamesFn() function



# fixFileNamesFn(x=searchPattern)



