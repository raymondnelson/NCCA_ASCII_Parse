# ########### R Functions for to fix NCCA ASCII file and directory names ############
# 1-2-2015 Raymond Nelson
# 6-12-2015
#
# 
#
##############################################



fixFileNames <- function() {
  # function to remove problem characters from filenames
  
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
  
  #output
  fileNames <- list.files()
  return(fileNames)
  
} # fixFileNames()

# fixFileNames()
