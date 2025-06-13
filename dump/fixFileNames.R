# ########### R Functions for to fix NCCA ASCII file and directory names ############
# 1-2-2015 Raymond Nelson
# 6-12-2015
#
# 
#
##############################################

fileNames <- list.files()
Encoding(fileNames) <- "latin1"

newFileNames <- iconv(fileNames, "latin1", "ASCII", sub="_")

fixFileNames <- function() {
  # function to remove problem characters from filenames
  
  # fix non-ASCII characters in the file name
  fileNames <- list.files()
#  Encoding(fileNames) <- "latin1"
  newFileNames <- iconv(fileNames, "latin1", "ASCII", sub="_")
  newFileNames <- gsub("Ì", "_", newFileNames)
  newFileNames <- gsub("Ñ", "_", newFileNames)
  newFileNames <- gsub("_+", "_", newFileNames)
  Encoding(newFileNames) <- "ASCII"
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], newFileNames[i])
  }


  # remove ( characters from all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("\\(", "", fileNames[i]))
  }
  # newFileNames <- list.files()
  
  # remove ) characters from all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("\\)", "", fileNames[i]))
  }
  # newFileNames <- list.files()
  
  # replace space characters for all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub(" ", "_", fileNames[i]))
  }
  # newFileNames <- list.files()
  
  # replace "_Cleaned_Copy" in all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("_Cleaned_Copy", "", fileNames[i]))
  }
  
  # replace "D&-" in all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("D&-", "", fileNames[i]))
  }
  
  # replace "-" in all filenames
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("-", "_", fileNames[i]))
  }
  
  fileNames <- list.files()
  for (i in 1:length(fileNames)) {
    file.rename(fileNames[i], gsub("-", "_", fileNames[i]))
  }
  
  
  
  
  
  #output
  fileNames <- list.files()
  return(fileNames)
  
} # fixFileNames()

# fixFileNames()


fileNames <- list.files()
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], paste0("a", fileNames[i]))
}

