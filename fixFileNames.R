# ########### R script to fix NCCA ASCII file and directory names ############
# 1-2-2015 Raymond Nelson
# 6-12-2015
#
# 
#
##############################################

# fileNames <- list.files()

fileNames <- list.files(path = ".", 
                        pattern = "D&+", 
                        all.files = FALSE,
                        full.names = FALSE, 
                        recursive = FALSE,
                        ignore.case = FALSE, 
                        include.dirs = TRUE,
                        no.. = FALSE)

#   fileNames <- grep("D&+", fileNames, 
#             ignore.case = FALSE, 
#             perl = FALSE, 
#             value = TRUE,
#             fixed = FALSE, 
#             useBytes = FALSE, 
#             invert = FALSE)


# fix non-ASCII characters in the file name

#  Encoding(fileNames) <- "latin1"
newFileNames <- iconv(fileNames, "latin1", "ASCII", sub="_")
newFileNames <- gsub("Ì", "_", newFileNames)
newFileNames <- gsub("Ñ", "_", newFileNames)
newFileNames <- gsub("[_]+", "_", newFileNames)
Encoding(newFileNames) <- "ASCII"
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], newFileNames[i])
} 
rm(newFileNames)

# replace space characters for all filenames
fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub(" ", "_", fileNames[i]))
}

# replace "-" in all filenames
fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("-", "_", fileNames[i]))
}

# remove ( characters from all filenames
fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("\\(", "", fileNames[i]))
}

# remove ) characters from all filenames
fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("\\)", "", fileNames[i]))
}

# remove doubel __ characters from all filenames
fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("[_]+", "_", fileNames[i]))
}

# remove "_Cleaned_Copy" in all filenames
fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  file.rename(fileNames[i], gsub("_Cleaned_Copy", "", fileNames[i]))
}

# remove "D&-" in all filenames
# fileNames <- list.files(pattern = "D&+")
# for (i in 1:length(fileNames)) {
#   file.rename(fileNames[i], gsub("D&-", "", fileNames[i]))
# }

# add the D$- prefix if necessary
# fileNames <- list.files(pattern = "D&+")
# for (i in 1:length(fileNames)) {
#   file.rename(fileNames[i], paste0("D&-", "", fileNames[i]))
# }

# add an alphabetic fourth character if necessary
fileNames <- list.files(pattern = "D&+")
for (i in 1:length(fileNames)) {
  if(tolower(str_sub(fileNames[i], 4, 4)) %in% letters == FALSE) {
    file.rename(fileNames[i], paste0(str_sub(fileNames[i], 1, 3), "a", str_sub(fileNames[i], 4, -1)))
  }
}

# remove multiple underline characters in the file name




# fixFileNames <- function() {
#   # function to remove problem characters from filenames
#   #
#   # remove ( characters from all filenames
#   fileNames <- list.files()
#   for (i in 1:length(fileNames)) {
#     file.rename(fileNames[i], gsub("\\(", "", fileNames[i]))
#   }
#   # newFileNames <- list.files()
#   #
#   # remove ) characters from all filenames
#   fileNames <- list.files()
#   for (i in 1:length(fileNames)) {
#     file.rename(fileNames[i], gsub("\\)", "", fileNames[i]))
#   }
#   # newFileNames <- list.files()
#   #
#   # replace space characters for all filenames
#   fileNames <- list.files()
#   for (i in 1:length(fileNames)) {
#     file.rename(fileNames[i], gsub(" ", "_", fileNames[i]))
#   }
#   # newFileNames <- list.files()
#   #
#   # replace "_Cleaned_Copy" in all filenames
#   fileNames <- list.files()
#   for (i in 1:length(fileNames)) {
#     file.rename(fileNames[i], gsub("_Cleaned_Copy", "", fileNames[i]))
#   }
#   #
#   #output
#   fileNames <- list.files()
#   return(fileNames)
# } # end fixFileNames()

# fixFileNames()



