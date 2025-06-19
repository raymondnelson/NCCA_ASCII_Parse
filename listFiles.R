# get sample cases from the JHUAPL archives
# 9-11-2016
# raymond.nelson@gmail.com
#
###############################



rm(list=ls())



library(stringr)



setwd("~/Dropbox/DATASETS_BACKUP")



# import the exam names and criterion state
OSSdataCaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/OSSN300dataCaseNames.csv", stringsAsFactors = FALSE)
DoDPI_N70caseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/DoDPI_N70caseNames.csv", stringsAsFactors = FALSE)
OSSN60CaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/OSSN60CaseNames.csv", stringsAsFactors = FALSE)

MarinCaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/MarinCaseNames.csv", stringsAsFactors = FALSE)
MarinDeceptiveDF <- MarinCaseNames[MarinCaseNames$CriterionState==-1,]
MarinTruthfulDF <- MarinCaseNames[MarinCaseNames$CriterionState==1,]

AxcitonN44CaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/AxcitonN44CaseNames.csv", stringsAsFactors = FALSE)
KrapohlNorrisN32MGQTCaseNames <- read.csv("~/Dropbox/DATASETS_BACKUP/KrapohlNorrisN32MGQTCaseNames.csv", stringsAsFactors = FALSE)

DoDPIZone_caseNames_confirmed <- read.csv("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/DoDPI_ZCT_Cases_confirmed_05032017.csv", stringsAsFactors = FALSE)
DoDPIZone_caseNames_all <- read.csv("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/DoDPI_ZCT_Cases_all_05032017.csv", stringsAsFactors = FALSE)
# 1122
# View(DoDPIZone_caseNames_all)

which(duplicated(DoDPIZone_caseNames_all$ExamID))
# [1]  57  86  89 171 180 221 242 257 270 279 310 311 320 347
# 14 duplicated cases
which(duplicated(DoDPIZone_caseNames_confirmed$ExamID))
# all duplicated cases are in the confirmed list

# make separate data frames for deceptive and truthful cases
DoDPIZone_deceptiveDF <- DoDPIZone_caseNames_all[DoDPIZone_caseNames_all$CriterionState==-1,]
nrow(DoDPIZone_deceptiveDF)
# 674
# View(DoDPIZone_deceptiveDF)
DoDPIZone_truthfulDF <- DoDPIZone_caseNames_all[DoDPIZone_caseNames_all$CriterionState==1,]
nrow(DoDPIZone_truthfulDF)
# 448
# View(DoDPIZone_truthfulDF)


# fix some things
# DoDPI_N70caseNames$ExamID <- str_sub(DoDPI_N70caseNames$ExamID, 1, -5)
# colnames(DoDPI_N70caseNames) <- c("ExamID", "CriterionState")
# DoDPI_N70caseNames$ExamID <- toupper(DoDPI_N70caseNames$ExamID)
# write.csv(DoDPI_N70caseNames, "~/Dropbox/DATASETS_BACKUP/DoDPI_N70caseNames.csv",row.names=FALSE)
# names(AxcitonN44CaseNames) <- names(DoDPI_N70caseNames)
# write.csv(AxcitonN44CaseNames, "~/Dropbox/DATASETS_BACKUP/AxcitonN44CaseNames.csv",row.names=FALSE)
# OSSdataCaseNames$ExamID <- str_sub(OSSdataCaseNames$ExamID, 1, -5)
# write.csv(OSSdataCaseNames, "~/Dropbox/DATASETS_BACKUP/OSSdataCaseNames.csv",row.names=FALSE)

#############

checkNCCAASCIFiles <- function(x="ALL", outputFileName="fileList") {
  # function to check for NCCAASCI output in the working directory
  # 5-6-2017
  # x="^D&+" is for Lafayette
  # x="^D\\$" is for Axciton # need to double escape the $ once for R and once for the system
  # x="^D#+" is for Stoelting
  # x="^D%+" is for Limestone
  # outputFileName is the name of the text output text file to save the result
  # if outputFileName==NULL then no file is written
  # visible output is a vector 
  # ######
  if(x=="ALL") {
    list1 <- list.files(pattern="^D&+")
    list2 <- list.files(pattern="^D\\$")
    list3 <- list.files(pattern="^D#+")
    list4 <- list.files(pattern="^D%+")
    fileList <- c(list1, list2, list3, list4)
  } else {
    fileList <- list.files(pattern=x)
  }
  printMsg1 <- paste(length(fileList), "charts found")
  print(printMsg1)
  library(stringr)
  fileList2 <- unique(str_sub(fileList, 1, -7))
  printMsg2 <- paste(length(fileList2), "unique exams")
  print(printMsg2)
  if(!is.null(outputFileName)) {
    write.table(x=fileList2, 
                file=paste0(outputFileName,"N",length(fileList2),".txt"),
                quote=FALSE, 
                row.names=FALSE,
                col.names=FALSE)
  }
  return(fileList2)
}

checkNCCAASCIFiles(x="ALL", outputFileName=NULL)

# check the DoDPI N70 sample
setwd("~/Dropbox/DATASETS_BACKUP/DoDPI_N70_Field_Cases")
# check the folder for NCCA ASCII data files
setwd("~/Dropbox/DATASETS_BACKUP/DoDPI_N70_Field_Cases/DoDPI_N70_NCCAASCII")
DoDPI70 <- checkNCCAASCIFiles()
DoDPI70 <- str_sub(checkNCCAASCIFiles(), 4, -1)
# check fo the folder for Axciton files
setwd("~/Dropbox/DATASETS_BACKUP/DoDPI_N70_Field_Cases/DoDPI_N70_all_cases")
DoDPI70Files <- list.files(pattern="^\\$+")
DoDPI70Files <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# which NCCA ASCI files are present in the vector of Axciton file names
DoDPI70 %in% DoDPI70Files
# all
# which Axciton files are present in the vector of NCCA ASCII case names
DoDPI70Files %in% DoDPI70
# all
# which NCCA ASCI cases are extant in the csv data frame
DoDPI70 %in% DoDPI_N70caseNames$ExamID
# all
# which Axction case names are extant in the csv data frame
DoDPI70Files %in% DoDPI_N70caseNames$ExamID
# all
# all cases are present and exported to NCCA ASCII 7-9-2017


setwd("~/Dropbox/DATASETS_BACKUP/Krapohl_Norris_2000/KrapohlNoris200_NCCAASCII")
KN32 <- checkNCCAASCIFiles()
KN32 <- str_sub(checkNCCAASCIFiles(), 4, -1)
setwd("~/Dropbox/DATASETS_BACKUP/Krapohl_Norris_2000/Krapohl_Norris_2000_all_cases")
KrapohlNorris32Files <- list.files(pattern="^\\$+")
KrapohlNorris32Files <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
KN32 %in% KrapohlNorris32Files
KrapohlNorris32Files %in% KN32
KN32 %in% KrapohlNorrisN32MGQTCaseNames$ExamID
KrapohlNorris32Files %in% KrapohlNorrisN32MGQTCaseNames$ExamID
# all cases are present and exported to NCCA ASCII 7-9-2017


setwd("~/Dropbox/DATASETS_BACKUP/Axciton_confirmed_casesN44/AxcitonN44_NCCAASCII")
AX44 <- checkNCCAASCIFiles()
AX44 <- str_sub(checkNCCAASCIFiles(), 4, -1)
setwd("~/Dropbox/DATASETS_BACKUP/Axciton_confirmed_casesN44/AxcitonN44_DI_NDI")
Axciton44Files <- list.files(pattern="^\\$+")
Axciton44Files <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
AX44 %in% Axciton44Files
Axciton44Files %in% AX44
AX44 %in% AxcitonN44CaseNames$ExamID
Axciton44Files %in% AxcitonN44CaseNames$ExamID
# !(Axciton44Files %in% AX44)
# length(AX44)
# $$QSVW4 was missing but now found
# all cases are present and exported to NCCA ASCII 7-9-2017


setwd("~/Dropbox/DATASETS_BACKUP/OSSN60Holdout_ZCT60/OSSN60_NCCAASCII")
OSSN60 <- checkNCCAASCIFiles()
OSSN60 <- str_sub(checkNCCAASCIFiles(), 4, -1)
setwd("~/Dropbox/DATASETS_BACKUP/OSSN60Holdout_ZCT60/OSSN60Cases")
OSSN60Files <- list.files(pattern="^\\$+")
OSSN60Files <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
OSSN60 %in% OSSN60Files
OSSN60Files %in% OSSN60
OSSN60Files %in% OSSN60CaseNames$ExamID
OSSN60 %in% OSSN60CaseNames$ExamID
# $$8AKOXR was missing 7-9-2017 located in NDI
# all cases are present and exported to NCCA ASCII 7-9-2017



######################################################################

# import a data frame with the MarinB case names from 2008
MarinB_cases2008 <- read.csv("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinB_cases2008.csv", stringsAsFactors=FALSE)
# View(MarinB_cases2008)
unique(MarinB_cases2008$RowNumber)
unique(MarinB_cases2008$FileName)
unique(MarinB_cases2008$MarinBNumber)
# go to the NCCA ASCII files
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/Marin100_NCCA_all")
# MarinN100 <- checkNCCAASCIFiles()
# remove the first characters of the NCCA ASCII output files
MarinN100 <- str_sub(checkNCCAASCIFiles(), 4, -1)
# 90 cases
MarinN100 %in% MarinB_cases2008$FileName
# all NCCA ASCI cases present in the Axciton file list from 2008
# One Axciton case has 6 charts "$$4$Q1HA"
# was separated into two exams and only the first three were kept in the Marin sample
# "$$4$Q1H%" is from "$$4$Q1HA"
# "$$4$Q1H%" was not kept in the Marin sample
MarinN100[which(!(MarinN100 %in% MarinB_cases2008$FileName))]
# all present
# "$$2TD&CA" is innocent biZone
# "$$78HVI%" is from "$$78HVIA"
# both are included in the Marin sample
# both exams are guilty
# MarinB file numbers are 694 = "$$78HVIA" and 327 = "$$78HVI%"


# "$$9QVSWI" %in% MarinN100
# "$$9QVSWI" %in% MarinN100Files
# "$$9QVSWI" %in% MarinB_cases2008$FileName
# "$$9QVSWI" %in% MarinCaseNames$ExamID
# # not sure this case is part of the Marin Sample 9-9-2017
# "$9QVSWI" is DI per original examiner
# "$$9QVSWI" %in% DoDPIZone_caseNames_confirmed$ExamID
# "$$9QVSWI" %in% DoDPIZone_caseNames_all$ExamID


# check the DoDPI archives for the Axciton cases
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100Cases/Marin_ncca_archive")
Marin_archive <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# 98 archives
Marin_archive %in% MarinB_cases2008$FileName
# all 98 are in the 2008 list
MarinB_cases2008$FileName %in% Marin_archive
# 2 missing archives
MarinB_cases2008$FileName[which(!(MarinB_cases2008$FileName %in% Marin_archive))]
# [1] "$$38R4WV" "$$BQUW%L"
# cases 45 and 19 in the Marin B list
# both cannot be located in the DoDPI archive
# both are coded as innocent 

# go to the Axciton files
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100Cases/MarinN100_all")
# MarinN100Files <- list.files(pattern="^\\$+")
MarinN100Files <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# 90 cases
Marin_archive %in% MarinN100Files
Marin_archive[which(!(Marin_archive %in% MarinN100Files))]
# [1] "$$84XPS0" "$$8EN%XC" "$$8KLQTC" "$$8QY8DL" "$$9AC8UX"
# [6] "$$9NTJSO" "$$9QVG%#" "$$FLP$2O"
# # these are the 8 cases for which there are no charts

# together with 2 cases no present in the DoDPI archive 
# there are 10 innocent cases missing from the Marin sample
# 9-9-2017

# make a list of Axciton files for guilty cases
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100Cases/MarinN100_guilty")
MarinN100Files_guilty <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# check which guilty files are in the MarinB2008 list
MarinN100Files_guilty %in% MarinB_cases2008$FileName
# all 50 Axciton exams exist in the MarinB 2008 list

# make a list of Axciton files for innocent cases
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100Cases/MarinN100_innocent")
MarinN100Files_innocent <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# 40 Axciton files exist for innocent cases
MarinN100Files_innocent %in% MarinB_cases2008$FileName
# all 40 Axciton cases exist in the MarinB 2008 list
# get the vector of innocent cases from the 2008 Marin sample
Marin2008_innocent <- MarinB_cases2008$FileName[which(MarinB_cases2008$CriterionState == 1)]
missingInnocent <- Marin2008_innocent[which(!(Marin2008_innocent %in% MarinN100Files_innocent))]
# 10 cases
# [1] "$$9QVG%#" "$$8KLQTC" "$$38R4WV" "$$8EN%XC" "$$8QY8DL"
# [6] "$$84XPS0" "$$BQUW%L" "$$9NTJSO" "$$FLP$2O" "$$9AC8UX"
# 8 cases for which there are no charts
# 2 cases missing from the DoDPI archive

# use the list of guilty Axciton files to move the guilty NCCA files to a folder
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/Marin100_NCCA_all")
oldDir <- getwd()
setwd(oldDir)
# name the new directory
newDir <- "MarinN100_NCCA_guilty"
if(!dir.exists(newDir)) dir.create(newDir)
# get the guilty exam files
files_to_copy <- list.files(pattern="^D\\$+")
files_to_copy2 <-files_to_copy[str_sub(files_to_copy, 4, -7) %in% MarinN100Files_guilty]
# file.copy(from=files_to_copy2, to=newDir, copy.date=TRUE)
# file.remove(files_to_copy2)
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/MarinN100_NCCA_guilty")
unique(str_sub(list.files(pattern="^D\\$+"), 4, -7))
unique(str_sub(list.files(pattern="^D\\$+"), 4, -7)) %in% MarinN100Files_guilty
# 50 guilty exams with NCCA ASCII data exist in the 2008 MarinB sample

# use the vector of innocent Axciton files to move the innocent NCCA files to a folder
setwd(oldDir)
# name the new directory
newDir <- "MarinN100_NCCA_innocent"
if(!dir.exists(newDir)) dir.create(newDir)
# get the guilty exam files
files_to_copy <- list.files(pattern="^D\\$+")
files_to_copy2 <-files_to_copy[str_sub(files_to_copy, 4, -7) %in% MarinN100Files_innocent]
# file.copy(from=files_to_copy2, to=newDir, copy.date=TRUE)
# file.remove(files_to_copy2)
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/MarinN100_NCCA_innocent")
unique(str_sub(list.files(pattern="^D\\$+"), 4, -7))
# 40 innocent exams with NCCA ASCII data exist in the 2008 MarinB sample
# 1 innocent case is a YouPhase "$$2TD&CA"
unique(str_sub(list.files(pattern="^D\\$+"), 4, -7)) %in% MarinN100Files_innocent



# 8 missing innocent cases have no charts
# 2 additional missing innocent case

# check which Axciton files exist for the 2008 MarinB sample
MarinN100Files %in% MarinB_cases2008$FileName
MarinN100Files[which(!(MarinN100Files %in% MarinB_cases2008$FileName))]
# [1] "$$4$Q1H%" is from $$4$Q1HA
# "$$4$Q1H%" is not in the 2008 MarinB sample
# "$$4$Q1H%" is charts 4 5 and 6 from "$$4$Q1HA"
# $$4$Q1HA is included in the Marin sample
# check which 2008 MarinB cases are not in the Axciton files
MarinB_cases2008$FileName %in% MarinN100Files
MarinB_cases2008$FileName[which(!(MarinB_cases2008$FileName %in% MarinN100Files))]
# 10 missing
# [1] "$$9QVG%#" "$$8KLQTC" "$$38R4WV" "$$8EN%XC" "$$8QY8DL"
# [6] "$$84XPS0" "$$BQUW%L" "$$9NTJSO" "$$FLP$2O" "$$9AC8UX"
MarinB_cases2008$CriterionState[which(!(MarinB_cases2008$FileName %in% MarinN100Files))]
# [1] 1 1 1 1 1 1 1 1 1 1
# all innocent
##
# [1] "$$38R4WV" "$$BQUW%L"
# 10 27
# "$$38R4WV" is innocent
# "$$BQUW%L" is innocent
# both are not present in the DoDPI archive

MarinN100 %in% MarinN100Files
# all NCCA ASCII files are in the Axciton files
MarinN100Files %in% MarinN100
MarinN100Files[!(MarinN100Files %in% MarinN100)]
# all Axcition cases are in the NCCA ASCII folder
# check which NCCA ASCII files are not in the old Marin list
MarinN100[!(MarinN100 %in% MarinCaseNames$ExamID)]
# character(0)
"$$4$Q1HA" %in% MarinCaseNames$ExamID
MarinCaseNames$ExamID[which(MarinCaseNames$ExamID=="$$4$Q1HA")]
grep("+Q1H+", MarinN100)
# [1] 41
MarinN100[c(41)]
# [1] "$$4$Q1H%"
# which Axciton files are not in the old Marin list
MarinN100Files[!(MarinN100Files %in% MarinCaseNames$ExamID)]
# character(0)
# which Axciton files are not in the NCCA ASCII folder
MarinN100Files[!(MarinN100Files %in% MarinN100)]
# check the criterion state of the missin files
MarinCaseNames[which(!(MarinN100Files %in% MarinN100)),'CriterionState']
# which names are in the old Marin list and not in the NCCA ASCII folder
MarinCaseNames$ExamID[which(!(MarinCaseNames$ExamID %in% MarinN100))]
# missing 10 cases in the MarinB 2008 case list not in the Axciton folder or NCCA folder
# 1] "$$38R4WV" "$$84XPS0" "$$8EN%XC" "$$8KLQTC" "$$8QY8DL"
# [6] "$$9AC8UX" "$$9NTJSO" "$$9QVG%#" "$$BQUW%L" "$$FLP$2O"
# check the criterion state of the missing cases
MarinCaseNames$CriterionState[which(!(MarinCaseNames$ExamID %in% MarinN100))]
# all truthful

setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100Cases")
oldDir <- getwd()

setwd(oldDir)
# name the new directory
newDir <- "MarinN100_innocent"
if(!dir.exists(newDir)) dir.create(newDir)
# get the innocent exam files
files_to_copy <- list.files(pattern="^\\$+")
files_to_copy2 <- files_to_copy[str_sub(files_to_copy, 1, -5) %in% MarinTruthfulDF$ExamID]
# file.copy(from=files_to_copy2, to=newDir, copy.date=TRUE)
setwd("MarinN100_innocent")
length(unique(str_sub(list.files(pattern="^\\$+"), 1, -5)))
unique(str_sub(list.files(pattern="^\\$+"), 1, -5)) %in% MarinTruthfulDF$ExamID
# all NCCA ASCII cases are in the deceptive DF from the old Marin list
MarinTruthfulDF$ExamID %in% unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# 2 cases in the old Marin  are missing from the NCCA ASCII folder
# [1] "$$38R4WV" "$$BQUW%L"
MarinTruthfulDF$ExamID[!(MarinTruthfulDF$ExamID %in% unique(str_sub(list.files(pattern="^\\$+"), 1, -5)))]
# [1] "$$38R4WV" "$$84XPS0" "$$8EN%XC" "$$8KLQTC" "$$8QY8DL"
# [6] "$$9AC8UX" "$$9NTJSO" "$$9QVG%#" "$$BQUW%L" "$$FLP$2O"

setwd(oldDir)
# name the new directory
newDir <- "MarinN100_guilty"
if(!dir.exists(newDir)) dir.create(newDir)
# get the guilty exam files
files_to_copy <- list.files(pattern="^\\$+")
files_to_copy2 <- files_to_copy[str_sub(files_to_copy, 1, -5) %in% MarinDeceptiveDF$ExamID]
# file.copy(from=files_to_copy2, to=newDir, copy.date=TRUE)
setwd("MarinN100_guilty")
length(unique(str_sub(list.files(pattern="^\\$+"), 1, -5)))
unique(str_sub(list.files(pattern="^\\$+"), 1, -5)) %in% MarinDeceptiveDF$ExamID
MarinDeceptiveDF$ExamID %in% unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
MarinDeceptiveDF$ExamID[!(MarinDeceptiveDF$ExamID %in% unique(str_sub(list.files(pattern="^\\$+"), 1, -5)))]
# all Axcition cases are extant in the guilty folder


#################################################################



setwd("~/Dropbox/DATASETS_BACKUP/OSSN300")
# go to the folder with the Axciton files
setwd("~/Dropbox/DATASETS_BACKUP/OSSN300/ODDN300_exams")
OSSN300Files <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# 288
# setwd("~/Dropbox/DATASETS_BACKUP/OSS3")
# OSSN300Files2 <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# which(!(OSSN300Files %in% OSSN300Files2))
# which(!(OSSN300Files2 %in% OSSN300Files))
# OSSN300Files2[188]
# OSSN300Files[188]
# setwd("~/Dropbox/DATASETS_BACKUP/OSSN300")
# OSSN300Files3 <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# which(!(OSSN300Files %in% OSSN300Files3))
# which(!(OSSN300Files3 %in% OSSN300Files))
# OSSN300Files3[188]

OSSN300Files[!(OSSN300Files %in% OSSdataCaseNames$ExamID)]
"$$BQTW#I" %in% OSSdataCaseNames$ExamID
OSSdataCaseNames$ExamID[!(OSSdataCaseNames$ExamID %in% OSSN300Files)]

# get the criterion state for the extant Axciton cases
OSSN300_criterion <- OSSdataCaseNames[which(OSSN300Files %in% OSSdataCaseNames$ExamID),'CriterionState']

cbind.data.frame(ExamID=OSSN300Files, CriterionState=OSSN300_criterion)

# separate the guilty and innocent cases
# View(OSSdataCaseNames)
OSSN300_deceptiveDF <- OSSdataCaseNames[OSSdataCaseNames$CriterionState==-1,]
nrow(OSSN300_deceptiveDF)
# 150
OSSN300_deceptiveDF$ExamID %in% OSSdataCaseNames$ExamID
# all

OSSN300_truthfulDF <- OSSdataCaseNames[OSSdataCaseNames$CriterionState==1,]
nrow(OSSN300_truthfulDF)
# 150
OSSN300_truthfulDF$ExamID %in% OSSdataCaseNames$ExamID
# all

# copy the guilty and innocent cases to separate directories
setwd("~/Dropbox/DATASETS_BACKUP/OSSN300/ODDN300_exams")
OSSN300ExamFiles <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
length(OSSN300ExamFiles)
# 288


oldDir <- getwd()


setwd(oldDir)
# name the new directory
newDir <- "OSSN300_innocent"
if(!dir.exists(newDir)) dir.create(newDir)
# get the innocent exam files
files_to_copy <- list.files(pattern="^\\$+")
files_to_copy2 <- files_to_copy[str_sub(files_to_copy, 1, -5) %in% OSSN300_truthfulDF$ExamID]
file.copy(from=files_to_copy2, to=newDir, copy.date=TRUE)
setwd("~/Dropbox/DATASETS_BACKUP/OSSN300/ODDN300_exams/OSSN300_innocent")
length(unique(str_sub(list.files(pattern="^\\$+"), 1, -5)))
# 138
files_to_copy2 <- unique(str_sub(list.files(pattern="^\\$+"), 1, -1))


setwd(oldDir)
newDir <- "OSSN300_guilty"
if(!dir.exists(newDir)) dir.create(newDir)
# get the innocent exam files
files_to_copy <- list.files(pattern="^\\$+")
files_to_copy3 <- files_to_copy[str_sub(files_to_copy, 1, -5) %in% OSSN300_deceptiveDF$ExamID]
file.copy(from=files_to_copy3, to=newDir, copy.date=TRUE)
setwd("~/Dropbox/DATASETS_BACKUP/OSSN300/ODDN300_exams/OSSN300_guilty")
length(unique(str_sub(list.files(pattern="^\\$+"), 1, -5)))
# 149
files_to_copy3 <- unique(str_sub(list.files(pattern="^\\$+"), 1, -1))


138+149
# 287


setwd(oldDir)
extantFiles <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
extantFiles %in% unique(c(str_sub(files_to_copy2, 1, -5), str_sub(files_to_copy3, 1, -5)))
extantFiles[!(extantFiles %in% unique(str_sub(c(files_to_copy2, files_to_copy3), 1, -5)))]
# [1] "$$BQTW#I" is not in the guilty or innocet folder
# "$$BQTW#I" is a deceptive case from the DoDPI Zone archive


"$$BQTW#I" %in% OSSN60CaseNames$ExamID


"$$BQTW#I" %in% DoDPIZone_caseNames_all$ExamID
"$$BQTW#I" %in% DoDPIZone_caseNames_confirmed$ExamID
# 


"$$BQTW#I" %in% OSSN300Files


OSSN60CaseNames['ExamID'=="$$BQTW#I",]
OSSdataCaseNames[OSSdataCaseNames$ExamID=="$$BQTW#I",]
DoDPIZone_caseNames_all['ExamID'=="$$BQTW#I",]
DoDPIZone_caseNames_confirmed['ExamID'=="$$BQTW#I",]




# zoneArchive <- list.files(pattern="+\\.EXE$")
# zoneArchive <- zoneArchive[zoneArchive %in% unusedZoneTruthfulArchives]
# toDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/Zone/unusedZoneTruthful"
# file.copy(from=zoneArchive, to=toDir)



#########################



setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100Cases")
marinB3 <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
length(marinB3)
# [1] 100
setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII")
marinN100NCCAASCII <- unique(str_sub(list.files(pattern="^D\\$+"), 4, -7))
length(marinN100NCCAASCII)
# 91
marinN100NCCAASCII %in% marinB3
# all
marinB3 %in% marinN100NCCAASCII
which(!(marinB3 %in% marinN100NCCAASCII))
# [1] 72 74 75 78 81 83 84 85 98
missingCases <- marinB3[which(!(marinB3 %in% marinN100NCCAASCII))]
print(missingCases)
# [1] "$$84XPS0" "$$8EN%XC" "$$8KLQTC" "$$8QY8DL" "$$9AC8UX" "$$9NTJSO" "$$9QVG%#"
# [8] "$$9QVSWI" "$$FLP$2O"
missingCases %in% MarinCaseNames$ExamID
# "$$9QVSWI" # not in MarinCaseNames
# "$$9QVSWI is guilty/deceptive
marinN100NCCAASCII %in% MarinCaseNames$ExamID
marinN100NCCAASCII[which(!(marinN100NCCAASCII %in% MarinCaseNames$ExamID))]
# [1] "$$4$Q1H%" "$$78HVI%"
# [1] "$$4$Q1H%" # is not in MarinCaseNames
MarinCaseNames$ExamID %in% marinN100NCCAASCII
MarinCaseNames$ExamID[which((MarinCaseNames$ExamID %in% missingCases))]
# [1] "$$84XPS0" "$$8EN%XC" "$$8KLQTC" "$$8QY8DL" "$$9AC8UX" "$$9NTJSO" "$$9QVG%#"
# [8] "$$FLP$2O"
MarinCaseNames$CriterionState[MarinCaseNames$ExamID %in% missingCases]
# [1] 1 1 1 1 1 1 1 1 # all truthful
marinB3 %in% marinN100NCCAASCII
marinB3[which(!(marinB3 %in% marinN100NCCAASCII))]
# [1] "$$84XPS0" "$$8EN%XC" "$$8KLQTC" "$$8QY8DL" "$$9AC8UX" "$$9NTJSO" "$$9QVG%#"
# [8] "$$9QVSWI" "$$FLP$2O"
# need to locate this one "$$9QVSWI"



setwd("~/Dropbox/DATASETS_BACKUP/ASIT_2009_sample")
ASIT2009 <- unique(str_sub(list.files(pattern="^\\$+"), 1, -5))
# 101
length(ASIT2009)
ASIT2009 %in% marinN100NCCAASCII
which(!(ASIT2009 %in% marinN100NCCAASCII))
# [1] 72 74 75 78 81 83 84 85 86 99
ASIT2009[which(!(ASIT2009 %in% marinN100NCCAASCII))]
# [1] "$$84XPS0" "$$8EN%XC" "$$8KLQTC" "$$8QY8DL" "$$9AC8UX" "$$9NTJSO" "$$9QVG%#"
# [8] "$$9V$FXX" "$$AG#O9F" "$$FLP$2O"
marinN100NCCAASCII %in% ASIT2009
which(!(marinN100NCCAASCII %in% ASIT2009))
marinN100NCCAASCII[which(!(marinN100NCCAASCII %in% ASIT2009))]
# all extant



##########################################################################
##########################################################################
##########################################################################



# define a function to compare the case names in different samples
caseCheckFn <- function(x, y, casenames=TRUE) {
  # function to compare case names in two samples
  ifelse(casenames==TRUE,
         z <- x[which(x %in% y)],
         z <- which(x %in% y) )
  # output z is either the case name or index
  # of cases in both samples
  return(z)
} # end caseCheckFn



#############



caseCheckFn(OSSN60CaseNames$ExamID, DoDPIZone_caseNames_confirmed$ExamID, FALSE)
caseCheckFn(OSSN60CaseNames$ExamID, DoDPIZone_caseNames_all$ExamID, FALSE)

# combine all case names for all samples
allCaseNames <- OSSN60CaseNames$ExamID
allCaseNames <- c(allCaseNames, OSSdataCaseNames$ExamID)
allCaseNames <- c(allCaseNames, MarinCaseNames$ExamID)
allCaseNames <- c(allCaseNames, AxcitonN44CaseNames$ExamID)
allCaseNames <- c(allCaseNames, DoDPI_N70caseNames$ExamID)
allCaseNames <- c(allCaseNames, KrapohlNorrisN32MGQTCaseNames$ExamID)
allCaseNames <- unique(allCaseNames)
length(allCaseNames)
# 543 unique case names



# check which used cases are in the archives
caseCheckFn(DoDPIZone_caseNames_all$ExamID, allCaseNames, TRUE)
# 443 names in the archives
caseCheckFn(allCaseNames, DoDPIZone_caseNames_all$ExamID, TRUE)
# 436



################ un-used cases

# get the un-used case names
notUsed <- DoDPIZone_caseNames_all$ExamID[!(DoDPIZone_caseNames_all$ExamID %in% allCaseNames)]
length(notUsed)
# 679 unused cases
# make a data frame of unused cases
notUsedDF <- DoDPIZone_caseNames_all[!(DoDPIZone_caseNames_all$ExamID %in% allCaseNames),]
# View(notUsedDF)
# check for unused case names in allCaseNames 
caseCheckFn(notUsed, allCaseNames)
# none
# make separate data frames for guilty and innocent cases
notUsedDeceptiveDF <- notUsedDF[notUsedDF$CriterionState==-1,]
nrow(notUsedDeceptiveDF)
# 440 deceptive cases
# View(notUsedDeceptiveDF)
notUsedTruthfulDF <- notUsedDF[notUsedDF$CriterionState==1,]
nrow(notUsedTruthfulDF)
# 239 truthful cases
# View(notUsedTruthfulDF)
notUsedTruthfulDF <- notUsedTruthfulDF[notUsedTruthfulDF$Group=="ZONE",]

oldWD <- getwd()
setwd("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/Zone")

# copy the cases to a separate folder
# make a vector of archive names
unusedZoneTruthfulArchives <- paste0(notUsedTruthfulDF$ExamID, ".EXE")
zoneArchive <- list.files(pattern="+\\.EXE$")
zoneArchive <- zoneArchive[zoneArchive %in% unusedZoneTruthfulArchives]
toDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/Zone/unusedZoneTruthful"
file.copy(from=zoneArchive, to=toDir)

unusedZoneDeceptiveArchives <- paste0(notUsedDeceptiveDF$ExamID, ".EXE")
zoneArchive <- list.files(pattern="+\\.EXE$")
zoneArchive <- zoneArchive[zoneArchive %in% unusedZoneDeceptiveArchives]
toDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/Zone/unusedZoneDeceptive"
file.copy(from=zoneArchive, to=toDir)




################### used cases

# make a data frame of used cases 
usedCasesDF <- DoDPIZone_caseNames_all[(DoDPIZone_caseNames_all$ExamID %in% allCaseNames),]
nrow(usedCasesDF)
# 443
# View(usedCasesDF)
caseCheckFn(usedCasesDF$ExamID, allCaseNames, FALSE)
# 443
# separate the deceptive and truthful cases
usedDeceptiveDF <- usedCasesDF[usedCasesDF$CriterionState==-1,]
nrow(usedDeceptiveDF)
# 234
# View(usedDeceptiveDF)
usedTruthfulDF <- usedCasesDF[usedCasesDF$CriterionState==1,]
nrow(usedTruthfulDF)
# 209
# View(usedTruthfulDF)



################# check for duplicated cases within each sample

# check for duplicates in cases the DoDPI archives
DoDPIZone_caseNames_all$ExamID[which(duplicated(DoDPIZone_caseNames_all$ExamID))]
# [1] "$$46C6#1" "$$3$3&L7" "$$4S&KZG" "$$62$A2Y" "$$6FAN0J" "$$4GTDC9" "$$55QBSU"
# [8] "$$6&PVMB" "$$5TC0US" "$$3D4SFM" "$$7HRF7A" "$$7MAIT7" "$$4172C1" "$$3ZNOLD"
# 14
DoDPIZone_caseNames_all$Group[which(duplicated(DoDPIZone_caseNames_all$ExamID))]
# all in the Field sample


# check for duplicates in the OSS sample
length(OSSdataCaseNames$ExamID)
# [1] 300
length(unique(OSSdataCaseNames$ExamID))
# [1] 290
OSSdataCaseNames[which(duplicated(OSSdataCaseNames$ExamID)),'ExamID']
# [1] "$$BXMGC#" "$$CUEC6U" "$$CLTKCC" "$$CJNINL" "$$B9MT83" "$$B9MT83" "$$AQP4MR"
# [8] "$$AN%8J#" "$$9RLVLX" "$$95JMHI"
# 10
OSSdataCaseNames[which(duplicated(OSSdataCaseNames$ExamID)),'CriterionState']
# [1] -1  1  1  1  1  1  1  1  1  1
OSSdataCaseNames[OSSdataCaseNames$ExamID=="$$BXMGC#",'CriterionState']
# "$$BXMGC#" is guilty and 9 others are truthful
OSSdataCaseNames$CriterionState[OSSdataCaseNames$ExamID=="$$BXMGC#"]
# duplicated case "$$BXMGC#" is deceptive and all others are truthful
DoDPIZone_caseNames_all$Group[DoDPIZone_caseNames_all$ExamID=="$$BXMGC#"]
# from the ZONE group
OSSDuplicates <- OSSdataCaseNames[which(duplicated(OSSdataCaseNames$ExamID)),'ExamID']
length(OSSDuplicates)
# 10
DoDPIZone_caseNames_all$ExamID[DoDPIZone_caseNames_all$ExamID%in%OSSDuplicates]
# [1] "$$9RLVLX" "$$95JMHI" "$$AN%8J#" "$$AQP4MR" "$$B9MT83" "$$BXMGC#" "$$CJNINL"
# [8] "$$CLTKCC" "$$CUEC6U"
DoDPIZone_caseNames_all$Group[DoDPIZone_caseNames_all$ExamID%in%OSSDuplicates]
# [1] "JHUAPL_Zone" "ZONE"        "ZONE"        "ZONE"        "ZONE"       
# [6] "ZONE"        "ZONE"        "ZONE"        "ZONE"  
DoDPIZone_caseNames_all$Group[DoDPIZone_caseNames_all$ExamID=="$$9RLVLX"]
# [1] "JHUAPL_Zone"
OSSdataCaseNames$CriterionState[OSSdataCaseNames$ExamID %in% OSSDuplicates]
length(OSSdataCaseNames$CriterionState[OSSdataCaseNames$ExamID %in% OSSDuplicates])
# 19
which(OSSDuplicates %in% DoDPIZone_caseNames_all$ExamID)
# [1]  1  2  3  4  5  6  7  8  9 10
which(DoDPIZone_caseNames_all$ExamID %in% OSSDuplicates)
# [1]  472  819  930  935  956 1000 1033 1039 1064
length(which(DoDPIZone_caseNames_all$ExamID %in% OSSDuplicates))
# 9 
# only 9 OSS duplicates are found in all cases
# because 1 case "$$B9MT83" is used 3 times in the OSS sample
OSSdataCaseNames$CriterionState[which(OSSdataCaseNames$ExamID %in% OSSDuplicates)]
which(OSSdataCaseNames$ExamID=="$$B9MT83")
# [1] 218 219 245
DoDPIZone_caseNames_all$CriterionState[DoDPIZone_caseNames_all$ExamID=="$$B9MT83"]
# "$$B9MT83" is truthful
# check for duplicates that are also in the Marin Saple
which(OSSDuplicates %in% caseCheckFn(OSSdataCaseNames$ExamID, MarinCaseNames$ExamID))
# none


which(DoDPIZone_truthfulDF$ExamID %in% OSSDuplicates)
# [1]  80 253 326 331 344 385 389 405
DoDPIZone_truthfulDF$ExamID[which(DoDPIZone_truthfulDF$ExamID %in% OSSDuplicates)]
# [1] "$$9RLVLX" "$$95JMHI" "$$AN%8J#" "$$AQP4MR" "$$B9MT83" "$$CJNINL" "$$CLTKCC"
# [8] "$$CUEC6U"
which(DoDPIZone_deceptiveDF$ExamID %in% OSSDuplicates)
# [1] 631
DoDPIZone_deceptiveDF$ExamID[631]
# [1] "$$BXMGC#"

# check for duplicates in the Marin sample
length(MarinCaseNames$ExamID)
length(unique(MarinCaseNames$ExamID))
MarinCaseNames[which(duplicated(MarinCaseNames$ExamID)),'ExamID']
# none

# check for duplicates in the OSS holdout sample
length(OSSN60CaseNames$ExamID)
length(unique(OSSN60CaseNames$ExamID))
OSSN60CaseNames[which(duplicated(OSSN60CaseNames$ExamID)),'ExamID']
# none

# check for duplicates in the Axciton N44 sample
length(AxcitonN44CaseNames$ExamID)
length(unique(AxcitonN44CaseNames$ExamID))
AxcitonN44CaseNames[which(duplicated(AxcitonN44CaseNames$ExamID)),'ExamID']
# none

# check for duplicates in the DoDPI N70 sample
length(DoDPI_N70caseNames$ExamID)
length(unique(DoDPI_N70caseNames$ExamID))
DoDPI_N70caseNames[which(duplicated(DoDPI_N70caseNames$ExamID)),'ExamID']
# none

# check for duplicates in the Krapohl Norris N32 Army MGQT sample
length(KrapohlNorrisN32MGQTCaseNames$ExamID)
length(unique(KrapohlNorrisN32MGQTCaseNames$ExamID))
KrapohlNorrisN32MGQTCaseNames[which(duplicated(KrapohlNorrisN32MGQTCaseNames$ExamID)),'ExamID']
# none



############# check for re-used cases 

# check the OSSN60 sample
caseCheckFn(DoDPIZone_caseNames_all$ExamID, OSSN60CaseNames$ExamID, FALSE)
caseCheckFn(OSSN60CaseNames$ExamID, DoDPIZone_caseNames_all$ExamID, FALSE)
OSSN60CasesDF <- DoDPIZone_caseNames_all[caseCheckFn(DoDPIZone_caseNames_all$ExamID, OSSN60CaseNames$ExamID, casenames=FALSE),]
nrow(OSSN60CasesDF)
# 60
# View(OSSN60CasesDF)


# call the caseCheckFn to compare the OSSN60 sample case names
caseCheckFn(OSSN60CaseNames$ExamID, OSSdataCaseNames$ExamID)
# none
# replaced "$$8MVL$X" with $$7HD$CI on 5-4-2017
caseCheckFn(OSSN60CaseNames$ExamID, MarinCaseNames$ExamID)
# none
caseCheckFn(OSSN60CaseNames$ExamID, AxcitonN44CaseNames$ExamID, TRUE)
# [1] "$$7RJ4TO" "$$7ZPFUC" # both truthful
# [1] 40 47
OSSN60AxcitonN44 <- caseCheckFn(OSSN60CaseNames$ExamID, AxcitonN44CaseNames$ExamID, TRUE)
caseCheckFn(OSSN60CaseNames$ExamID, DoDPI_N70caseNames$ExamID)
# none
caseCheckFn(OSSN60CaseNames$ExamID, KrapohlNorrisN32MGQTCaseNames$ExamID)
# none


# call the function to compare the OSS3 sample case names
caseCheckFn(OSSdataCaseNames$ExamID, OSSN60CaseNames$ExamID)
# none
caseCheckFn(OSSdataCaseNames$ExamID, MarinCaseNames$ExamID, TRUE)
# [1] "$$FROQ7X" "$$FLP$2O" "$$D7GS60" "$$CYXJKC" "$$CT9QFL" "$$CS#JHI" "$$BXN1FF"
# [8] "$$BWI9C0" "$$BW1AEO" "$$BQUW%L" "$$BI8Z0O" "$$BBXQ29" "$$9QVG%#" "$$9AC8UX"
# [15] "$$8V$6AI" "$$8QY8DL" "$$8N7WOO" "$$8MFRW6" "$$8KLQTC"
# [1] 151 153 166 177 181 182 202 203 204 206 215 216 264 269 283 287 290 292 294
OSSMarin <- caseCheckFn(OSSdataCaseNames$ExamID, MarinCaseNames$ExamID, TRUE)
caseCheckFn(OSSdataCaseNames$ExamID, AxcitonN44CaseNames$ExamID, TRUE)
# [1] "$$DB629G" "$$CQRF86" "$$BQVZF#" "$$ARDDK3" "$$ALSL73" "$$A6TX33" "$$908AQX"
# [1] 164 184 207 224 231 266 275
OSSAxcitonN44 <- caseCheckFn(OSSdataCaseNames$ExamID, AxcitonN44CaseNames$ExamID, TRUE)
caseCheckFn(OSSdataCaseNames$ExamID, DoDPI_N70caseNames$ExamID)
# none
caseCheckFn(OSSdataCaseNames$ExamID, KrapohlNorrisN32MGQTCaseNames$ExamID)
# none


# call the function to compare the Marin case names
caseCheckFn(MarinCaseNames$ExamID, OSSN60CaseNames$ExamID)
# none
caseCheckFn(MarinCaseNames$ExamID, OSSdataCaseNames$ExamID)
# [1] "$$8KLQTC" "$$8MFRW6" "$$8N7WOO" "$$8QY8DL" "$$8V$6AI" "$$9AC8UX" "$$9QVG%#"
# [8] "$$BBXQ29" "$$BI8Z0O" "$$BQUW%L" "$$BW1AEO" "$$BWI9C0" "$$BXN1FF" "$$CS#JHI"
# [15] "$$CT9QFL" "$$CYXJKC" "$$D7GS60" "$$FLP$2O" "$$FROQ7X"
MarinN100OSS <- caseCheckFn(MarinCaseNames$ExamID, OSSdataCaseNames$ExamID)
caseCheckFn(MarinCaseNames$ExamID, AxcitonN44CaseNames$ExamID)
# [1] "$$3#Q0GA" "$$3B&&KD" "$$4S&KZG" "$$6&PVMB"
# [1]  7 15 53 61
MarinN100AxctionN44 <- caseCheckFn(MarinCaseNames$ExamID, AxcitonN44CaseNames$ExamID)
caseCheckFn(MarinCaseNames$ExamID, DoDPI_N70caseNames$ExamID)
# [1] "$$2V$A8A" "$$3#Q0GA" "$$3$0MLK" "$$331R7M" "$$3HD$MQ" "$$3HE2ZY" "$$4S&KZG"
# [8] "$$5TC0US" "$$6&PVMB" "$$7MAIT7" "$$7TA%GG" "$$8DIBEX"
MarinN100AxcitonN44 <-caseCheckFn(MarinCaseNames$ExamID, DoDPI_N70caseNames$ExamID)
caseCheckFn(MarinCaseNames$ExamID, KrapohlNorrisN32MGQTCaseNames$ExamID)
# none


# call the function to compare the AxcitonN44 case names
caseCheckFn(AxcitonN44CaseNames$ExamID, OSSdataCaseNames$ExamID)
# [1] "$$908AQX" "$$A6TX33" "$$ALSL73" "$$ARDDK3" "$$BQVZF#" "$$CQRF86" "$$DB629G"
caseCheckFn(AxcitonN44CaseNames$ExamID, OSSN60CaseNames$ExamID)
# [1] "$$7RJ4TO" "$$7ZPFUC"
caseCheckFn(AxcitonN44CaseNames$ExamID, MarinCaseNames$ExamID)
# [1] "$$3B&&KD" "$$3#Q0GA" "$$4S&KZG" "$$6&PVMB"
caseCheckFn(AxcitonN44CaseNames$ExamID, KrapohlNorrisN32MGQTCaseNames$ExamID)
# none
caseCheckFn(AxcitonN44CaseNames$ExamID, DoDPI_N70caseNames$ExamID)
# [1] "$$3#Q0GA" "$$4S&KZG" "$$6&PVMB"



# call the function to compare the DoDPIN70 case names
caseCheckFn(DoDPI_N70caseNames$ExamID, OSSN60CaseNames$ExamID)
# none
caseCheckFn(DoDPI_N70caseNames$ExamID, OSSdataCaseNames$ExamID)
# none
caseCheckFn(DoDPI_N70caseNames$ExamID, MarinCaseNames$ExamID)
# [1] "$$2V$A8A" "$$3#Q0GA" "$$3$0MLK" "$$331R7M" "$$3HD$MQ" "$$3HE2ZY" "$$4S&KZG"
# [8] "$$5TC0US" "$$6&PVMB" "$$7MAIT7" "$$7TA%GG" "$$8DIBEX"
caseCheckFn(DoDPI_N70caseNames$ExamID, AxcitonN44CaseNames$ExamID)
# [1] "$$3#Q0GA" "$$4S&KZG" "$$6&PVMB"
caseCheckFn(DoDPI_N70caseNames$ExamID, KrapohlNorrisN32MGQTCaseNames$ExamID)
# [1] "$$2#3RKZ" "$$2S%8H7" "$$2TWAKG" "$$2UIQ27" "$$4B#4F1" "$$4IA9PD" "$$4U4$F$"
# [8] "$$5#H8YD" "$$6$D8PB"


# call the function to compare the KrapohlNorrisN32 case names
caseCheckFn(KrapohlNorrisN32MGQTCaseNames$ExamID, OSSN60CaseNames$ExamID)
# none
caseCheckFn(KrapohlNorrisN32MGQTCaseNames$ExamID, OSSdataCaseNames$ExamID)
# none
caseCheckFn(KrapohlNorrisN32MGQTCaseNames$ExamID, MarinCaseNames$ExamID)
# none
caseCheckFn(KrapohlNorrisN32MGQTCaseNames$ExamID, AxcitonN44CaseNames$ExamID)
# none
caseCheckFn(KrapohlNorrisN32MGQTCaseNames$ExamID, DoDPI_N70caseNames$ExamID)
# [1] "$$2#3RKZ" "$$2S%8H7" "$$2TWAKG" "$$2UIQ27" "$$4B#4F1" "$$4IA9PD" "$$4U4$F$"
# [8] "$$5#H8YD" "$$6$D8PB"
# [1]  1  4  6  7 17 20 22 24 27
KrapohlNorisN32DoDPIN70 <- caseCheckFn(KrapohlNorrisN32MGQTCaseNames$ExamID, DoDPI_N70caseNames$ExamID)


# check for duplicates
length(OSSdataCaseNames$ExamID)
length(unique(OSSdataCaseNames$ExamID))
# 290
# 10 duplicates
OSSdataCaseNames[which(duplicated(OSSdataCaseNames$ExamID)),'ExamID']
# [1] "$$BXMGC#" "$$CUEC6U" "$$CLTKCC" "$$CJNINL" "$$B9MT83" "$$B9MT83" "$$AQP4MR"
# [8] "$$AN%8J#" "$$9RLVLX" "$$95JMHI"
nrow(OSSdataCaseNames[which(duplicated(OSSdataCaseNames$ExamID)),])
# [1] 10
OSSdataCaseNames[which(duplicated(OSSdataCaseNames$ExamID)),]
# ExamID CriterionState
# 41  $$BXMGC#             -1
# 180 $$CUEC6U              1
# 188 $$CLTKCC              1
# 192 $$CJNINL              1
# 219 $$B9MT83              1
# 245 $$B9MT83              1
# 252 $$AQP4MR              1
# 253 $$AN%8J#              1
# 265 $$9RLVLX              1
# 278 $$95JMHI              1



# shared case OSSN300 and OSSN60 is deceptive
"$$8MVL$X" %in% MarinCaseNames$ExamID
"$$8MVL$X" %in% KrapohlNorrisN32MGQTCaseNames$ExamID
"$$8MVL$X" %in% OSSN60CaseNames$ExamID
"$$8MVL$X" %in% OSSdataCaseNames$ExamID
# TRUE
"$$8MVL$X" %in% AxcitonN44CaseNames$ExamID
"$$8MVL$X" %in% DoDPI_N70caseNames$ExamID

# two shared cases OSSN60 and AxcitonN44 "$$7RJ4TO" "$$7ZPFUC" both truthful
"$$7RJ4TO" %in% MarinCaseNames$ExamID
"$$7RJ4TO" %in% KrapohlNorrisN32MGQTCaseNames$ExamID
"$$7RJ4TO" %in% OSSN60CaseNames$ExamID
# TRUE
"$$7RJ4TO" %in% OSSdataCaseNames$ExamID
"$$7RJ4TO" %in% AxcitonN44CaseNames$ExamID
# TRUE
"$$7RJ4TO" %in% DoDPI_N70caseNames$ExamID
"$$7ZPFUC" %in% MarinCaseNames$ExamID
"$$7ZPFUC" %in% KrapohlNorrisN32MGQTCaseNames$ExamID
"$$7ZPFUC" %in% OSSN60CaseNames$ExamID
# TRUE
"$$7ZPFUC" %in% OSSdataCaseNames$ExamID
"$$7ZPFUC" %in% AxcitonN44CaseNames$ExamID
# TRUE
"$$7ZPFUC" %in% DoDPI_N70caseNames$ExamID


# select the 
# caseNamesDF <- AxcitonN44CaseNames
# caseNamesDF <- MarinCaseNames
# caseNamesDF <- DoDPI_N70caseNames
caseNamesDF <- OSSN60CaseNames
length(caseNamesDF$ExamID)
# unique(caseNamesDF$ExamID)
length(unique(caseNamesDF$ExamID))


# caseNamesDF$ExamID <- str_sub(caseNamesDF$ExamID, 1, -4)

# OSS sample has 300 cases
# 291 unique cases


# check for duplicates
caseNamesDF[which(duplicated(caseNamesDF$ExamID)),]
nrow(caseNamesDF[which(duplicated(caseNamesDF$ExamID)),])

# OSS sample has 9 cases with the same ID
# all innocent cases
#           ExamID CriterionState
# 180 $$CUEC6U.013              1
# 188 $$CLTKCC.013              1
# 192 $$CJNINL.013              1
# 219 $$B9MT83.013              1
# 245 $$B9MT83.013              1
# 252 $$AQP4MR.013              1
# 253 $$AN%8J#.013              1
# 265 $$9RLVLX.013              1
# 278 $$95JMHI.013              1


# no Marin cases have duplicate names


nrow(caseNamesDF[which(caseNamesDF$CriterionState == 1),])
nrow(caseNamesDF[which(caseNamesDF$CriterionState == -1),])
length(which(caseNamesDF$CriterionState == 1))
length(which(caseNamesDF$CriterionState == -1))

# 150 innocent OSS cases
# 150 guilty OSS cases

# 50 innocent Marin cases
# 50 guilty Marin cases



# use this to remove the .exe and .013 file type if necessary
# caseNamesDF$ExamID <- str_sub(caseNamesDF$ExamID,1,-5)
# caseNamesDF$ExamID <- gsub(".EXE$", "", caseNamesDF$ExamID, ignore.case=TRUE)
# caseNamesDF$ExamID <- gsub(".013", "", caseNamesDF$ExamID, ignore.case=TRUE)


# set the old and new directory names
oldDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/Zone"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/MGQT"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/FIELD"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/1999_and_later"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/Axciton_confirmed_casesN44"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/Axciton_confirmed_casesN44/Confirmed_DI"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/Axciton_confirmed_casesN44/Confirmed_NDI"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100Cases"
# oldDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_N70_Field_Cases"
# newDir <- "~/Dropbox/DATASETS_BACKUP/OSS3/"
# newDir <- "~/Dropbox/DATASETS_BACKUP/OSSN300/"
# newDir <- "~/Dropbox/DATASETS_BACKUP/MarinN100/"
# newDir <- "~/Dropbox/DATASETS_BACKUP/Marin/"
# newdir <- "~/Dropbox/DATASETS_BACKUP/AxcitonN44/"
newDir <- "~/Dropbox/DATASETS_BACKUP/OSSN60"
# newDir <- "~/Dropbox/DATASETS_BACKUP/DoDPI_N70"

dir.exists("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/JHUAPL/Zone")
!dir.exists("~/Dropbox/DATASETS_BACKUP/OSSN60/")
if(!dir.exists(newDir)) dir.create(newDir)

# set the working directory
setwd(oldDir)


# make a list of all exams in the old directory
file_list <- list.files(path=".", pattern="*.EXE$")
# file_list <- list.files()
# 
# file_list <- file_list[-grep(pattern="^D\\$", file_list)]
file_list <- file_list[grep(pattern="\\$", file_list)]
# file_list <- file_list[-grep(pattern="\\^------", file_list)]

# file_list <- file_list[-grep(pattern=".cha$", file_list)]
length(file_list)
length(unique(file_list))

# JHUAPL/Zone archive has 638 cases
# Axciton Confirmed DI N22 has 378 files
# Axciton Confirmed NDI N22 has 356 files


# remove the .exe file type from the exam names while keeping the original file list
file_list2 <- str_sub(file_list, 1, -5)
# file_list2 <- str_sub(file_list, 4, -7)
file_list2 <- unique(file_list2)
length(file_list2)
length(unique(file_list2))

# AxcitonN44 has 22 unique DI cases and 22 unique NDI cases
# AxcitonN44CaseNames <- cbind.data.frame(caseID=file_list2, criterionState=1)


# caseNamesDF[23:44,] <- AxcitonN44CaseNames
# write.csv(caseNamesDF, "~/Dropbox/DATASETS_BACKUP/AxcitonN44CaseNames.csv",row.names=FALSE)


# determine if there are any missing cases
which(!(file_list2 %in% caseNamesDF$ExamID))
file_list2[which(!(file_list2 %in% caseNamesDF$ExamID))]
which(!(caseNamesDF$ExamID %in% file_list2))
caseNamesDF$ExamID[which(!(caseNamesDF$ExamID %in% file_list2))]

# all OSSN60 cases are in the JHUAPL/Zone archive

# Marin cases 3 missing from JHUAPL/Zone
# [1] 13 66 88
# [1] 13 66 88
# [1] "$$38R4WV"     "$$78HVI4.043" "$$BQUW%L" 
# case $$78HVI4 includes 2 exams

# make a vector of cases to copy from the old directory
files_to_copy <- file_list[which((file_list2 %in% caseNamesDF$ExamID))]



# make a vector of existing files in the new directory
existingFiles <- str_sub(list.files(path=newDir, pattern="*.EXE$"), 1, -1)


# make a vector of files that are in the old dir and not in the new dir
files_to_copy <- files_to_copy[which(!(files_to_copy %in% existingFiles))]
length(files_to_copy)


# copy the files to the new directory
file.copy(from=files_to_copy, to=newDir, copy.date=TRUE)


# change the working directory
setwd(newDir)


# make a vector of file names in the new directory
new_file_list <- list.files(path=".", pattern="*.EXE")
length(new_file_list)


# remove the .exe file type and calculate the number of exams in the directory
new_file_list2 <- str_sub(new_file_list, 1, -5)
# new_file_list2 <- gsub(".EXE", "", new_file_list, ignore.case=TRUE)
length(new_file_list2)



# make a vector of files in the new dir that are not in the 
missing_cases <- new_file_list[-which(new_file_list %in% file_list)]
# new_file_list2 %in% gsub(".EXE", "", file_list, ignore.case=TRUE)

# 69 Marin cases not in the Zone dir
# 297 OSS cases in the new directory
# no missing OSSN60 cases

# check which cases are not in the new directory
which(!(caseNamesDF$ExamID %in% new_file_list2))
length(which(!(caseNamesDF$ExamID %in% new_file_list2)))
caseNamesDF$ExamID[which(!(caseNamesDF$ExamID %in% new_file_list2))]

# 3 missing OSS cases
# 174 206 214
# $$C%AXC3 $$BQUW%L $$BI7%WQ

# 3 missing Marin cases
# [1] 13 66 88
# [1] "$$38R4WV"     "$$78HVI4.043" "$$BQUW%L" 


# check the files in the new Directory
unique(new_file_list2)
length(unique(new_file_list2))
# to remove old files after copying use this
# file.remove(filestocopy)

# OSS 287 

# Marin 97 unique case IDs f



###################################

# work ith the CASES.DAT file

setwd("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302")
casesHeader <- readLines("CASES.DAT", n = 31, ok = TRUE, warn = FALSE, encoding = "UTF-8")
write(casesHeader, file="casesHeader.txt")
casesDAT <- casesDAT[1086:1134]
write(casesDAT, file="casesDATUnknown.txt")
casesDAT <- readLines("CASES.DAT", n = -1, ok = TRUE, warn = FALSE, encoding = "UTF-8")
casesDAT <- casesDAT[32:1085]
casesDAT <- write(casesDAT, "casesDAT.txt")

library("readr")
casesDAT <- read_table("casesDAT.txt", col_names = FALSE)
DAT <- c(casesDAT$X2, casesDAT$X6)

DODPI_cases <- read.csv("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/DODPI_cases.csv", stringsAsFactors = FALSE)
length(unique(DODPI_cases$ExamID))

which(!(DAT %in% DODPI_cases$ExamID))
which(!(DODPI_cases$ExamID %in% DAT))

which(!(OSSdataCaseNames$ExamID %in% DAT))
OSSdataCaseNames$ExamID[which(!(OSSdataCaseNames$ExamID %in% DAT))]
# [1]   1  12  30  48  93 122 281
# [1] "$$FT$FPX" "$$D6TM1F" "$$CFU%B%" "$$BOAN1F" "$$9FYL#3" "$$8UQ#DL" "$$8XO#$0"
which(!(OSSN60CaseNames$ExamID %in% DAT))
OSSN60CaseNames$ExamID[which(!(OSSN60CaseNames$ExamID %in% DAT))]
which(!(KrapohlNorrisN32MGQTCaseNames$ExamID %in% DAT))
KrapohlNorrisN32MGQTCaseNames$ExamID[which(!(KrapohlNorrisN32MGQTCaseNames$ExamID %in% DAT))]
# [1] 10 12
# [1] "$$32P#RK" "$$3AZQTY"
which(!(MarinCaseNames$ExamID %in% DAT))
MarinCaseNames$ExamID[which(!(MarinCaseNames$ExamID %in% DAT))]
# [1]  3 13 23 66
# [1] "$$2#59I1"     "$$38R4WV"     "$$3KNLAE"     "$$78HVI4.043"
which(!(AxcitonN44CaseNames$ExamID %in% DAT))
AxcitonN44CaseNames$ExamID[which(!(AxcitonN44CaseNames$ExamID %in% DAT))]
# [1]  2  4  5  6  8 16 19 22 32 35 37 38
# [1] "$$14Y6RE" "$$2I%U81" "$$2RWSXD" "$$30#EL8" "$$33GDFJ" "$$5BRY82" "$$73N%G6"
# [8] "$$GV%GX1" "$$7JQCNA" "$$8TUETP" "$$9K265V" "$$9N2LHQ"
which(!(DoDPI_N70caseNames$ExamID %in% DAT))
DoDPI_N70caseNames$ExamID[which(!(DoDPI_N70caseNames$ExamID %in% DAT))]
# [1]  3 24 25
# [1] "$$1%6L8F" "$$3HPENI" "$$3I013S"


######################################

# check to see if a sample is included in the DoDPI cases

DODPI_cases <- read.csv("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302/DODPI_cases.csv", stringsAsFactors = FALSE)

OSSN60CaseNames$ExamID %in% DODPI_cases$ExamID
OSSdataCaseNames$ExamID %in% DODPI_cases$ExamID
AxcitonN44CaseNames$ExamID %in% DODPI_cases$ExamID
DoDPI_N70caseNames$ExamID %in% DODPI_cases$ExamID
KrapohlNorrisN32MGQTCaseNames$ExamID %in% DODPI_cases$ExamID
MarinCaseNames$ExamID %in% DODPI_cases$ExamID


allCases <- list.files(pattern=".EXE", recursive=TRUE, full.names = FALSE, include.dirs=FALSE, ignore.case = TRUE)
allCasesDF <- cbind.data.frame(allCases, ExamID=str_sub(allCases, -12, -5))


allCasesDF$ExamID %in% DODPI_cases$ExamID
DODPI_cases$ExamID %in% allCasesDF$ExamID


OSSN60CaseNames$ExamID %in% allCasesDF$ExamID
# no missing cases
OSSdataCaseNames$ExamID[!(OSSdataCaseNames$ExamID %in% allCasesDF$ExamID)]
# three missing cases "$$C%AXC3" "$$BQUW%L" "$$BI7%WQ"
AxcitonN44CaseNames$ExamID %in% allCasesDF$ExamID
# some missing
# [1] "$$14Y6RE" "$$2I%U81" "$$2RWSXD" "$$30#EL8" "$$33GDFJ" "$$5BRY82" "$$73N%G6"
# [8] "$$7JQCNA" "$$8TUETP" "$$9K265V" "$$9N2LHQ"
KrapohlNorrisN32MGQTCaseNames$ExamID %in% allCasesDF$ExamID
# no missing cases
DoDPI_N70caseNames$ExamID %in% allCasesDF$ExamID
# no missing cases
MarinCaseNames$ExamID %in% allCasesDF$ExamID
# three missing [1] "$$38R4WV"     "$$78HVI4.043" "$$BQUW%L"    

allCasesDF$ExamID[!which(allCasesDF$ExamID %in% DODPI_cases$ExamID)]



filesToRename <- list.files(pattern="..HVI4...")
newFileNames <- gsub("HVI4", "HVI\\%", filesToRename)
file.rename(filesToRename, newFileNames)

  
