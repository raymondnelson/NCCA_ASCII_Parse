# get the OSS-3 cases from the JHUAPL archives
# 9-11-2016
# raymond.nelson@gmail.com
### 


library(stringr)


# import the exam names
OSSdataCaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/OSSdataCaseNames.csv")
DoDPI_N70caseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/DoDPI_N70caseNames.csv")
OSSN60CaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/OSSN60CaseNames.csv")
MarinCaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/MarinCaseNames.csv")


# select the 
caseNamesDF <- OSSdataCaseNames
# caseNamesDF <- read.csv("~/Dropbox/DATASETS_BACKUP/OSSdataCaseNames.csv", stringsAsFactors = FALSE)
# caseNamesDF <- read.csv("~/Dropbox/DATASETS_BACKUP/OSS3dataCaseNames.csv", stringsAsFactors = FALSE)
# caseNamesDF <- read.csv("~/Dropbox/DATASETS_BACKUP/Marin/MarinCaseNames.csv")
# caseNamesDF <- read.csv("~/Dropbox/DATASETS_BACKUP/OSSN60CaseNames.csv")
length(caseNamesDF$ExamID)
# unique(caseNamesDF$ExamID)
length(unique(caseNamesDF$ExamID))


# check for duplicates
caseNamesDF[which(duplicated(caseNamesDF$ExamID)),]
nrow(caseNamesDF[which(duplicated(caseNamesDF$ExamID)),])



nrow(caseNamesDF[which(caseNamesDF$CriterionState == 1),])
nrow(caseNamesDF[which(caseNamesDF$CriterionState == -1),])


# use this to remove the .exe file type if necessary
# caseNamesDF$ExamID <- str_sub(caseNamesDF$ExamID,1,-5)
caseNamesDF$ExamID <- gsub(".EXE", "", caseNamesDF$ExamID, ignore.case=TRUE)


length(which(caseNamesDF$CriterionState == 1))
length(which(caseNamesDF$CriterionState == -1))


# set the old and new directory names
oldDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/Zone"
# newDir <- "~/Dropbox/DATASETS_BACKUP/OSS3/"
newDir <- "~/Dropbox/DATASETS_BACKUP/OSS/"
# newDir <- "~/Dropbox/DATASETS_BACKUP/OSSN60")


# set the working directory
setwd(oldDir)


# make a list of all exams in the old directory
file_list <- list.files(path=".", pattern="*.EXE")
length(file_list)
length(unique(file_list))


# remove the .exe file type from the exam names while keeping the original file list
file_list2 <- str_sub(file_list, 1, -5)
length(file_list2)


# make a list of cases in the old director
files_to_copy <- file_list[which((file_list2 %in% caseNamesDF$ExamID))]
length(files_to_copy)


# copy the files to the new directory
file.copy(from=files_to_copy, to=newDir, copy.date=TRUE)


# change the working directory
setwd(newDir)


# make a vector of file names in the new directory
new_file_list <- list.files(path=".", pattern="*.EXE")
length(new_file_list)


# remove the .exe file type and calculate the number of exams in the directory
# new_file_list2 <- str_sub(new_file_list, 1, -5)
new_file_list2 <- gsub(".EXE", "", new_file_list, ignore.case=TRUE)
length(new_file_list2)



new_file_list2 %in% gsub(".EXE", "", files_to_copy, ignore.case=TRUE)
gsub(".EXE", "", files_to_copy, ignore.case=TRUE) %in% new_file_list2


# check which cases are in the new directory
which((caseNamesDF$ExamID %in% new_file_list2))
length(which((caseNamesDF$ExamID %in% new_file_list2)))


# check which cases are not in the new directory
which(!(caseNamesDF$ExamID %in% new_file_list2))
length(which(!(caseNamesDF$ExamID %in% new_file_list2)))


unique(new_file_list2)
length(unique(new_file_list2))
# to remove old files after copying use this
# file.remove(filestocopy)


