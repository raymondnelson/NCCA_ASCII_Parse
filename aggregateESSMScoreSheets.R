# aggregate and tabulate the ESS-M results for a sample
# Aug 30, 2020
# Raymond Nelson
#
####



rm(list=ls())

setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")

# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/new_test_04302018/result")



# import the Marin case list with the axciton file names and criterion state
# MarinB_caseList_2018 <- read.csv("~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinB_caseList_2018.csv", stringsAsFactors=FALSE)


library(readr)

criterionStateDF <- read_csv("criterionState.csv")
# View(criterionStateDF)


##############   locate the ESS-M score sheets in the current directory   ##########



library(stringr)



scoreSheetFileNames <- list.files(path = ".",
                              pattern = ".ESSMScoresheet+",
                              all.files = FALSE,
                              full.names = FALSE,
                              recursive = FALSE,
                              ignore.case = FALSE,
                              include.dirs = FALSE,
                              no.. = FALSE)


# file.rename(scoreSheetFileNames, 
#             paste0(str_sub(scoreSheetFileNames, 1, -17), ".csv"))



# get the shortExamNames for the scoreSheet .csv files
# without the Marin B number and case status

str_sub(scoreSheetFileNames[1], 1, -22)

shortExamNames <- str_sub(scoreSheetFileNames, 2, -22)

shortExamNames %in% criterionStateDF$examName
criterionStateDF$examName %in% shortExamNames


# with the Marin B number and case status
# shortExamNames <- str_sub(scoreSheetFileNames, 23, -11)
# get the MarinB case numbers
# get the criterion state



# short names for the axciton file names in the Marin case list
# shortNames <- str_replace_all(gsub("\\$", "", MarinB_caseList_2018$AxcitonFileName), "[:punct:]", "")
# shortNames <- str_replace_all(gsub("\\$", "", OSS2_caseList_2018$AxcitonFileName), "[:punct:]", "")
# shortNames <- paste0("X", shortNames)



##### check for missing or mis-named cases in the csv files and MarinB csv list  #####

shortExamNames %in% criterionStateDF$examName
criterionStateDF$examName %in% shortExamNames


# check for Marin cases in the vector of exam names
# shortNames[!(shortNames %in% shortExamNames)]
# shortNames[(shortNames %in% shortExamNames)]

# check for cases that are not in the Marin list
# shortExamNames[!(shortExamNames %in% shortNames)]
# shortExamNames[(shortExamNames %in% shortNames)]



##### iterate over the scoreSheetFileNames and import them #####



i=1
for(i in 1:length(scoreSheetFileNames)) {
  tempScoreSheet <- read.csv(scoreSheetFileNames[i], row.names = NULL, stringsAsFactors = FALSE)
  assign(paste0("ESSMScoreSheetDF_series_X_",shortExamNames[i]),tempScoreSheet, pos=1)
  # assign(paste0("OSS2ScoreSheetDF_series_X_",shortExamNames[i]),tempScoreSheet, pos=1)
}



# if necessary fix and save the .csv files
# scoreSheetDF_series_X_X2TDCA <- scoreSheetDF_series_X_X2TDCA[-8]
# ESSScoreSheetDF_series_X_X2TDCA$R10 <- NA
# ESSScoreSheetDF_series_X_X7HRF7A <- ESSScoreSheetDF_series_X_X7HRF7A[1:18,]
# scoreSheetDF_series_X_X331R7M <- scoreSheetDF_series_X_X331R7M[-8]
# scoreSheetDF_series_X_X3Z5Q9P <- scoreSheetDF_series_X_X3Z5Q9P[-8]
# scoreSheetDF_series_X_X939R6 <- scoreSheetDF_series_X_X939R6[-8]



# fix the column names for the RQs
k=1
for(k in 1:length(scoreSheetFileNames)) {
  # tempScoreSheet <- get(str_sub(scoreSheetFileNames[k], 1, -7), pos=1)
  tempScoreSheet <- get(paste0("ESSMScoreSheetDF_series_X_",shortExamNames[k]), pos=1)
  colnames(tempScoreSheet)[5:7] <- c("R5", "R7", "R10")
  # tempScoreSheet <- get(paste0("OSS2ScoreSheetDF_series_X_",shortExamNames[k]), pos=1)
  # colnames(tempScoreSheet)[5:7] <- c("R5", "R7", "R10")
  # write.csv(tempScoreSheet,file=scoreSheetFileNames[k], row.names=FALSE)
  # assign(str_sub(scoreSheetFileNames[k], 1, -7), tempScoreSheet, pos=1)
  assign(paste0("ESSMScoreSheetDF_series_X_",shortExamNames[k]), tempScoreSheet, pos=1)
  # assign(paste0("OSS2ScoreSheetDF_series_X_",shortExamNames[k]), tempScoreSheet, pos=1)
}

# paste0(str_sub(scoreSheetNames, 1, -5), "_")

# ESSResults <- rep(NA, length=length(scoreSheetFileNames))



#### fix the columns as numeric for all DFs

scoreSheetDFNames <- ls(pattern="ESSMScoreSheetDF")
# scoreSheetDFNames <- ls(pattern="OSS2ScoreSheetDF")
k=1
for(k in 1:length(scoreSheetDFNames)) {
  # tempScoreSheet <- get(paste0("OSS2ScoreSheetDF_series_X_",shortExamNames[k]), pos=1)
  # tempScoreSheet$R5 <- as.integer(tempScoreSheet$R5)
  # tempScoreSheet$R7 <- as.integer(tempScoreSheet$R7)
  # tempScoreSheet$R10 <- as.integer(tempScoreSheet$R10)
  # assign(paste0("OSS2ScoreSheetDF_series_X_",shortExamNames[k]), tempScoreSheet, pos=1)  
  tempScoreSheet <- get(scoreSheetDFNames[k], pos=1)
  # tempScoreSheet <- get(paste0("ESSScoreSheetDF_series_X_",shortExamNames[k]), pos=1)
  tempScoreSheet$R5 <- as.integer(tempScoreSheet$R5)
  tempScoreSheet$R7 <- as.integer(tempScoreSheet$R7)
  tempScoreSheet$R10 <- as.integer(tempScoreSheet$R10)
  assign(scoreSheetDFNames[k], tempScoreSheet, pos=1)
  # assign(paste0("ESSScoreSheetDF_series_X_",shortExamNames[k]), tempScoreSheet, pos=1)
}



#### initialize a data frame with the same case order as the .csv names ####



resultDF <- cbind.data.frame(shortExamNames, 
                             caseNumber=NA,
                             ESSResult=NA, 
                             criterionState=NA, 
                             correct=NA, 
                             correctCode=NA )

resultDF$shortExamNames <- as.character(resultDF$shortExamNames)
# View(resultDF)


# initialize the data frame score columns
{
  resultDF$R5_1_R <- NA
  resultDF$R5_1_E <- NA
  resultDF$R5_1_C <- NA
  resultDF$R5_1_V <- NA
  resultDF$R7_1_R <- NA
  resultDF$R7_1_E <- NA
  resultDF$R7_1_C <- NA
  resultDF$R7_1_V <- NA
  resultDF$R10_1_R <- NA
  resultDF$R10_1_E <- NA
  resultDF$R10_1_C <- NA
  resultDF$R10_1_V <- NA
  resultDF$R5_2_R <- NA
  resultDF$R5_2_E <- NA
  resultDF$R5_2_C <- NA
  resultDF$R5_2_V <- NA
  resultDF$R7_2_R <- NA
  resultDF$R7_2_E <- NA
  resultDF$R7_2_C <- NA
  resultDF$R7_2_V <- NA
  resultDF$R10_2_R <- NA
  resultDF$R10_2_E <- NA
  resultDF$R10_2_C <- NA
  resultDF$R10_2_V <- NA
  resultDF$R5_3_R <- NA
  resultDF$R5_3_E <- NA
  resultDF$R5_3_C <- NA
  resultDF$R5_3_V <- NA
  resultDF$R7_3_R <- NA
  resultDF$R7_3_E <- NA
  resultDF$R7_3_C <- NA
  resultDF$R7_3_V <- NA
  resultDF$R10_3_R <- NA
  resultDF$R10_3_E <- NA
  resultDF$R10_3_C <- NA
  resultDF$R10_3_V <- NA
}

# initialize the data frame columns for subtotal and grand total scores
{
  resultDF$R5Subtotal <- NA
  resultDF$R7Subtotal <- NA
  resultDF$R10Subtotal <- NA
  resultDF$GrandTotal <- NA 
}

# add the file name to the data frame
resultDF$csvFileName <- scoreSheetFileNames

# View(resultDF)



#### get the criterion state ####



# iterate over the rows in the resultDF and get the criterion state
l=1
for(l in 1:nrow(resultDF)) {
  resultDF$criterionState[l] <-
    criterionStateDF$criterionState[which(criterionStateDF$examName == resultDF$shortExamNames[l])]
    # OSS2_caseList_2018[which(resultDF$shortExamNames[l] == shortNames),'CriterionCode']
}
# resultDF$criterionState <- 1
# View(resultDF)
# which(resultDF$criterionState==0)

# get the criterion state from the scoreSheetFileNames
# resultDF$criterionState <- str_sub(scoreSheetFileNames, -5, -5)



#### iterate over the case rows in the resultDF ####



# l=1
# for (l in 1:nrow(resultDF)) {
#   resultDF$caseNumber[l] <-
#     OSS2_caseList_2018[which(resultDF$shortExamNames[l] == shortNames),'CaseNumber']
# }

# resultDF$caseNumber <- str_pad(resultDF$caseNumber, 3, pad=0)

resultDF$caseNumber <- str_pad(c(1:60), 3, pad=0)

# unique(resultDF$MarinB)

# get the Marin B case number from the file name
# resultDF$MarinB <- str_sub(scoreSheetFileNames, -9, -7)

# now write the new csv file names with the Marin B case number and case status
# k=1
# for(k in 1:length(scoreSheetFileNames)) {
#   tempScoreSheet <- get(str_sub(scoreSheetFileNames[k], 1, -5), pos=1)
#   newFileName <- paste0(str_sub(scoreSheetFileNames[k], 1, -5),
#                         "_",
#                         resultDF$MarinB[k],
#                         "_",
#                         resultDF$criterionState[k],
#                         ".csv" )
#   #write.csv(tempScoreSheet,file=newFileName, row.names=FALSE)
#   file.rename(scoreSheetFileNames[k], newFileName)
# }



########   compute the ESS results   ########



# make a list of score sheets in the global environ
scoreSheetNames <- ls(pattern="^ESSMScoreSheetDF_series_X_+")
# scoreSheetNames <- ls(pattern="^OSS2ScoreSheetDF_series_X_+")

# check the length
length(scoreSheetFileNames) == length(scoreSheetNames)

# iterate over the score sheets and calculate the ESS or OSS2 result
j=1
for(j in 1:length(scoreSheetNames)) {
  print(scoreSheetNames[j])
  thisScoreSheet <- get(scoreSheetNames[j])
  
  # dataCheck is a vector to check the boundardy condition
  # for the number of available sensor scores for each subtotal
  dataCheck <- rep(NA, 3)
  dataCheck[1] <- length(thisScoreSheet[,5][!is.na(as.numeric(thisScoreSheet[,5]))])
  dataCheck[2] <- length(thisScoreSheet[,6][!is.na(as.numeric(thisScoreSheet[,6]))])
  dataCheck[3] <- length(thisScoreSheet[,7][!is.na(as.numeric(thisScoreSheet[,7]))])
  minSubVals <- min(dataCheck)
  selectMinSubVals <- which.min(dataCheck)
  
  subTotalScores <- colSums(thisScoreSheet[,c(5:7)], na.rm=TRUE)
  minSubTotalScore <- min(subTotalScores)
  selectSub <- which.min(subTotalScores)
  
  grandTotalScore <- sum(subTotalScores, na.rm=TRUE)
  
  # add the scores to the data frame
  {
    resultDF$R5_1_R[j] <- thisScoreSheet$R5[1]
    resultDF$R5_1_E[j] <- thisScoreSheet$R5[2]
    resultDF$R5_1_C[j] <- thisScoreSheet$R5[3]
    resultDF$R7_1_R[j] <- thisScoreSheet$R7[1]
    resultDF$R7_1_E[j] <- thisScoreSheet$R7[2]
    resultDF$R7_1_C[j] <- thisScoreSheet$R7[3]
    resultDF$R10_1_R[j] <- thisScoreSheet$R10[1]
    resultDF$R10_1_E[j] <- thisScoreSheet$R10[2]
    resultDF$R10_1_C[j] <- thisScoreSheet$R10[3]
    resultDF$R5_2_R[j] <- thisScoreSheet$R5[4]
    resultDF$R5_2_E[j] <- thisScoreSheet$R5[5]
    resultDF$R5_2_C[j] <- thisScoreSheet$R5[6]
    resultDF$R7_2_R[j] <- thisScoreSheet$R7[4]
    resultDF$R7_2_E[j] <- thisScoreSheet$R7[5]
    resultDF$R7_2_C[j] <- thisScoreSheet$R7[6]
    resultDF$R10_2_R[j] <- thisScoreSheet$R10[4]
    resultDF$R10_2_E[j] <- thisScoreSheet$R10[5]
    resultDF$R10_2_C[j] <- thisScoreSheet$R10[6]
    resultDF$R5_3_R[j] <- thisScoreSheet$R5[7]
    resultDF$R5_3_E[j] <- thisScoreSheet$R5[8]
    resultDF$R5_3_C[j] <- thisScoreSheet$R5[9]
    resultDF$R7_3_R[j] <- thisScoreSheet$R7[7]
    resultDF$R7_3_E[j] <- thisScoreSheet$R7[8]
    resultDF$R7_3_C[j] <- thisScoreSheet$R7[9]
    resultDF$R10_3_R[j] <- thisScoreSheet$R10[7]
    resultDF$R10_3_E[j] <- thisScoreSheet$R10[8]
    resultDF$R10_3_C[j] <- thisScoreSheet$R10[9]
  }
  
  # add the subtotal and grand total scores to the data frame
  {    
    resultDF$R5Subtotal[j] <- subTotalScores[1]
    resultDF$R7Subtotal[j] <- subTotalScores[2]
    resultDF$R10Subtotal[j] <- subTotalScores[3]
    
    resultDF$GrandTotal[j] <- grandTotalScore
  }
  
  # set the boundary level for required sensor scores for each subtotal
  boundaryLevel <- 0
  
  # determine the result for this case
  resultDF$ESSResult[j] <- ifelse(grandTotalScore <= -3 && minSubVals >= boundaryLevel,
                                         "DI",
                                         ifelse(min(subTotalScores) <= -7 && dataCheck[selectSub] >= boundaryLevel,
                                                "DI",
                                                ifelse(grandTotalScore >= 3 && minSubVals >= boundaryLevel,
                                                       "NDI",
                                                       "INC")))
  
  # resultDF$ESSResult[j] <- ifelse(grandTotalScore >= 3 && minSubVals >= boundaryLevel,
  #                         "NDI",
  #                         ifelse(grandTotalScore <= -3 && minSubVals >= boundaryLevel,
  #                                "DI",
  #                                ifelse(min(subTotalScores) <= -7 && dataCheck[selectSub] >= boundaryLevel,
  #                                        "DI",
  #                                       "INC")))
  
  # determine the result for this case
  # resultDF$OSS2Result[j] <- ifelse(grandTotalScore >= 6,
  #                                 "NDI",
  #                                 ifelse(grandTotalScore <= -6,
  #                                        "DI",
  #                                        "INC"))

} # end iteration over scoreSheetNames


ESSResults <- resultDF$ESSResult
print(ESSResults)

scoreSheetNames[which(ESSResults == "DI")]

scoreSheetNames[which(ESSResults == "NDI")]

scoreSheetNames[which(ESSResults == "INC")]



#######  get the Axciton exam data  #################


AxcitonFileNames <- list.files(path = ".",
                               pattern = "^D\\$-\\$+",
                               all.files = FALSE,
                               full.names = FALSE,
                               recursive = FALSE,
                               ignore.case = FALSE,
                               include.dirs = FALSE,
                               no.. = FALSE)

UniqueAxcitonExams <- unique(str_sub(AxcitonFileNames, 1, -5))
print(UniqueAxcitonExams)



############   check the accuracy of the ESS results  ############



# iterate over the rows in the resultDF and get the criterion state
# l=1
# for(l in 1:nrow(resultDF)) {
#   resultDF$criterionState[l] <- 
#     MarinB_caseList_2018[which(resultDF$shortExamNames[l] == shortNames),'CriterionCode']
# }

# then check the results

# change the deceptive code to -1
# resultDF$criterionState[resultDF$criterionState == 0] <- -1
resultDF$criterionState <- as.numeric(resultDF$criterionState)

# recode the ESS result
resultDF$ESSResult[resultDF$ESSResult == "NDI"] <- 1
resultDF$ESSResult[resultDF$ESSResult == "DI"] <- -1
resultDF$ESSResult[resultDF$ESSResult == "INC"] <- 0
resultDF$ESSResult <- as.numeric(resultDF$ESSResult)

# calculate the detection efficiency coefficient 
cor(resultDF$ESSResult, resultDF$criterionState)

# calculate the proportion of correct results
length(which(
  resultDF$ESSResult == -1 & resultDF$criterionState == -1 |
    resultDF$ESSResult == 1 & resultDF$criterionState == 1
)) / (length(scoreSheetNames) - length(which(resultDF$ESSResult == 0)))

# code the correct column in the resultDF
m=1
for(m in 1:nrow(resultDF)) {
  resultDF$correct[m] <-
    ifelse(
      resultDF$ESSResult[m] == 0,
      0,
      ifelse(
        ( resultDF$criterionState[m] == -1 & resultDF$ESSResult[m] == -1 ) |
        ( resultDF$criterionState[m] == 1 & resultDF$ESSResult[m] == 1 ),
        1,
        -1
      )
    )
}

length(
  which( (resultDF$criterionState == 1 & resultDF$ESSResult == 1) | 
  (resultDF$criterionState == -1 & resultDF$ESSResult == -1) )
)

length(
  which(resultDF$correct == -1)
)

length(
  which(resultDF$correct == 0)
)



############   compute the correctCode   ###############



for(m in 1:nrow(resultDF)) {
  resultDF$correctCode[m] <- 
    ifelse(resultDF$criterionState[m] == -1,
           ifelse(resultDF$ESSResult[m] == -1,
                  1,
                  ifelse(resultDF$ESSResult[m] == 1,
                         3,
                         5 )),
           ifelse(resultDF$ESSResult[m] == 1,
                  2,
                  ifelse(resultDF$ESSResult[m] == -1,
                         4,
                         6 ))
           )
}




############### aggregate the correct codes #################



# tally the correct codes
aggregate(data=resultDF, rep(1, nrow(resultDF)) ~ correctCode, FUN=sum)


# 86/92



###############  save the result data frame as a .csv  #############



saveResult <- TRUE

if(isTRUE(saveResult)) {
  saveCWD <- getwd()
  
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/Marin100_NCCA_all/Marin_N100_results/inc")
  
  write.csv(resultDF, "ESSMResultDF.csv", row.names=FALSE)
  
  setwd(saveCWD)
  
}








###############################################################################
###########   copy the score sheets for inconclusive and error cases  #########



copyScoreSheets <- FALSE



if(isTRUE(copyScoreSheets)) {
  
  # make a vector of scoresheet names to copy
  copyFiles <- NULL
  # resultDF$shortExamNames
  copyFiles <- c(copyFiles, scoreSheetFileNames[which(resultDF$correctCode == 5)])
  copyFiles <- c(copyFiles, scoreSheetFileNames[which(resultDF$correctCode == 6)])
  # dir.create("inc")
  newLocation <- "inc"
  file.copy(copyFiles, newLocation)
  # [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
  # [16] TRUE
  
  copyFiles <- NULL
  copyFiles <- c(copyFiles, scoreSheetFileNames[which(resultDF$correctCode == 3)])
  copyFiles <- c(copyFiles, scoreSheetFileNames[which(resultDF$correctCode == 4)])
  # # dir.create("err")
  newLocation <- "err"
  file.copy(copyFiles, newLocation)
  
}



############  copy the .Rda files for inc and err cases   #############



copyRDA <- FALSE



if(isTRUE(copyRDA)) {
  
  # make a list of .Rda names in the parent directory
  rdaNames <- list.files(path = "..", 
                         pattern = "+_2.Rda$", 
                         all.files = FALSE,
                         full.names = FALSE, 
                         recursive = FALSE,
                         ignore.case = FALSE, 
                         include.dirs = FALSE,
                         no.. = FALSE)
  
  # only 90 rdaNames, whereas 100 cases
  
  # make a vector of shortened rdaNames
  shortRDANames <- str_sub(rdaNames, 1, -7)
  
  # compare the shortRDANames (n=90) with the data frame (n=100)
  rdaCorrectCode <- resultDF$correctCode[which(resultDF$shortExamNames %in% shortRDANames)]
  
  # make a vector of .Rda files to copy
  copyFiles <- NULL
  copyFiles <- c(copyFiles, rdaNames[which(rdaCorrectCode == 5)])
  copyFiles <- c(copyFiles, rdaNames[which(rdaCorrectCode == 6)])
  
  # append the parent dir
  copyFiles <- paste0("../", copyFiles)
  
  # X9NTJSO is inc and innocent and no charts exist and so no .Rda
  # because there are no charts and no NCCAASCII files
  
  # need to check which rdaNames are in copyFiles
  str_sub(rdaNames,1, -7) %in% str_sub(copyFiles, 4, -7)
  rdaNames[(str_sub(rdaNames,1, -7) %in% str_sub(copyFiles, 4, -7))]
  # rdaNames[!(str_sub(rdaNames,4, -7) %in% str_sub(copyFiles, 17, -7))]
  str_sub(copyFiles, 4, -7) %in% str_sub(rdaNames,1, -7)
  copyFiles[!(str_sub(copyFiles, 4, -7) %in% str_sub(rdaNames,1, -7))]
  
  # dir.create("inc")
  newLocation <- "inc"
  file.copy(copyFiles, newLocation)
  
  # make a vector of .Rda files to copy
  copyFiles <- NULL
  copyFiles <- c(copyFiles, rdaNames[which(rdaCorrectCode == 3)])
  copyFiles <- c(copyFiles, rdaNames[which(rdaCorrectCode == 4)])
  
  # append the parent dir
  copyFiles <- paste0("../", copyFiles)
  
  # need to check which rdaNames are in copyFiles
  str_sub(rdaNames,1, -7) %in% str_sub(copyFiles, 4, -7)
  rdaNames[(str_sub(rdaNames,1, -7) %in% str_sub(copyFiles, 4, -7))]
  # rdaNames[!(str_sub(rdaNames,4, -7) %in% str_sub(copyFiles, 17, -7))]
  str_sub(copyFiles, 4, -7) %in% str_sub(rdaNames,1, -7)
  copyFiles[!(str_sub(copyFiles, 4, -7) %in% str_sub(rdaNames,1, -7))]
  
  # dir.create("err")
  newLocation <- "err"
  file.copy(copyFiles, newLocation)
  
}



############  copy the chartPlot.pdf files for inc and err cases   #############



copyChartPlots <- FALSE



if(isTRUE(copyChartPlots)) {
  
  # make a list of .Rda names in the parent directory
  chartPlotNames <- list.files(path = "..", 
                               pattern = "+chartPlot.pdf$", 
                               all.files = FALSE,
                               full.names = FALSE, 
                               recursive = FALSE,
                               ignore.case = FALSE, 
                               include.dirs = FALSE,
                               no.. = FALSE)
  
  # only 90 chart plots, whereas 100 cases
  
  # make a vector of shortened rdaNames FOR inconclusive cases
  shortChartPlotNames <- str_sub(chartPlotNames, 1, -15)
  
  # compare the shortRDANames (n=90) with the data frame (n=100)
  correctCode <- resultDF$correctCode[which(resultDF$shortExamNames %in% shortChartPlotNames)]
  
  # make a vector of .Rda files to copy
  copyFiles <- NULL
  copyFiles <- c(copyFiles, chartPlotNames[which(correctCode == 5)])
  copyFiles <- c(copyFiles, chartPlotNames[which(correctCode == 6)])
  
  # append the parent dir
  copyFiles <- paste0("../", copyFiles)
  
  # only 11 files
  # X9NTJSO is inc and innocent and no charts exist and so no .Rda
  # because there are no charts and no NCCAASCII files
  
  # need to check which rdaNames are in copyFiles
  str_sub(chartPlotNames,1, -15) %in% str_sub(copyFiles, 4, -15)
  chartPlotNames[(str_sub(chartPlotNames,1, -15) %in% str_sub(copyFiles, 4, -15))]
  str_sub(copyFiles, 4, -15) %in% str_sub(chartPlotNames,1, -15)
  copyFiles[!(str_sub(copyFiles, 4, -15) %in% str_sub(chartPlotNames,1, -15))]
  
  # dir.create("inc")
  newLocation <- "inc"
  file.copy(copyFiles, newLocation)
  
  # make a vector of .Rda files names for error cases
  copyFiles <- NULL
  copyFiles <- c(copyFiles, chartPlotNames[which(correctCode == 3)])
  copyFiles <- c(copyFiles, chartPlotNames[which(correctCode == 4)])
  
  # append the parent dir
  copyFiles <- paste0("../", copyFiles)
  
  # only 11 files
  # X9NTJSO is inc and innocent and no charts exist and so no .Rda
  # because there are no charts and no NCCAASCII files
  
  # need to check which rdaNames are in copyFiles
  str_sub(chartPlotNames,1, -15) %in% str_sub(copyFiles, 4, -15)
  chartPlotNames[(str_sub(chartPlotNames,1, -15) %in% str_sub(copyFiles, 4, -15))]
  str_sub(copyFiles, 4, -15) %in% str_sub(chartPlotNames,1, -15)
  copyFiles[!(str_sub(copyFiles, 4, -15) %in% str_sub(chartPlotNames,1, -15))]
  
  # dir.create("err")
  newLocation <- "err"
  file.copy(copyFiles, newLocation)
  
}



############   copy the Marin pdfs for inconclusive and error cases  #############



copyPDFs <- FALSE


if(isTRUE(copyPDFs)) {
  
  library(stringr)
  library("magrittr")
  
  pdfNames <- list.files(path = "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/Marin_B_PDFs",
                         pattern = "+.pdf$",
                         all.files = FALSE,
                         full.names = FALSE,
                         recursive = FALSE,
                         ignore.case = FALSE,
                         include.dirs = FALSE,
                         no.. = FALSE)
  
  shortPDFNames <- str_sub(pdfNames, 10, -7)
  shortPDFNames <- str_replace_all(gsub("\\$", "", shortPDFNames), "[:punct:]", "")
  shortPDFNames <- paste0("X", shortPDFNames)
  
  # resultDF and shortPDFNames may be in different order
  
  # initialize a vector of inconclusive cases
  copyFiles <- NULL
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 5)])
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 6)])
  
  # check them
  copyFiles %in% shortPDFNames
  
  # replace the names in the copyFiles victor with the pdfNames
  copyFiles <- pdfNames[(which(shortPDFNames %in% copyFiles))]
  
  # append the directory name
  copyFiles <- paste0(
    "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/Marin_B_PDFs/",
    copyFiles
  )
  
  newLocation <- "inc"
  file.copy(copyFiles, newLocation)
  
  # initialize a vector of error cases
  copyFiles <- NULL
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 3)])
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 4)])
  
  # check them
  copyFiles %in% shortPDFNames
  
  # replace the names in the copyFiles victor with the pdfNames
  copyFiles <- pdfNames[(which(shortPDFNames %in% copyFiles))]
  
  # append the directory name
  copyFiles <- paste0(
    "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/Marin_B_PDFs/",
    copyFiles
  )
  
  newLocation <- "err"
  file.copy(copyFiles, newLocation)
  
}



########   copy the NCCAASCII data for inconclusive and error cases   ########



copyNCCAASCII <- FALSE
# copyNCCAASCII <- TRUE

if(isTRUE(copyNCCAASCII)) {
  
  library(stringr)
  library("magrittr")
  
  
  
  ASCIINames <- list.files(path = "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/Marin100_NCCA_all",
                         pattern = "+.0.A$",
                         all.files = FALSE,
                         full.names = FALSE,
                         recursive = FALSE,
                         ignore.case = FALSE,
                         include.dirs = FALSE,
                         no.. = FALSE)
  
  shortASCIINames <- str_sub(ASCIINames, 4, -7)
  shortASCIINames <- str_replace_all(gsub("\\$", "", shortASCIINames), "[:punct:]", "")
  
  shortASCIINames <- paste0("X", shortASCIINames)
  
  # make a vector of inconclusive cases
  copyFiles <- NULL
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 5)])
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 6)])
  
  # check them
  copyFiles %in% shortASCIINames
  # case X9NTJSO has no NCCA ASCII files because there are no charts
  
  # replace the names in the copyFiles vector with the ASCIINames
  copyFiles <- ASCIINames[(which(shortASCIINames %in% copyFiles))]
  
  # append the directory name
  copyFiles <- paste0(
    "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/Marin100_NCCA_all/",
    copyFiles
  )
  
  newLocation <- "inc"
  file.copy(copyFiles, newLocation)
  
  # make a vector of error cases
  copyFiles <- NULL
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 3)])
  copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 4)])
  
  # check them
  copyFiles %in% shortASCIINames
  # case X9NTJSO has no NCCA ASCII files because there are no charts
  
  # replace the names in the copyFiles vector with the ASCIINames
  copyFiles <- ASCIINames[(which(shortASCIINames %in% copyFiles))]
  
  # append the directory name
  copyFiles <- paste0(
    "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/Marin100_NCCA_all/",
    copyFiles
  )
  
  newLocation <- "err"
  file.copy(copyFiles, newLocation)
  
}




##############################################

######## copy NCCA ASCII files to a directory ########



copyNCCAASCII <- FALSE
# copyNCCAASCII <- TRUE

if(isTRUE(copyNCCAASCII)) {
  
  library(stringr)
  
  
  library("magrittr")
  
  
  
  ASCIINames <- list.files(path = "./edit/", 
                           pattern = "+_chartPlot copy.pdf$",
                           all.files = FALSE,
                           full.names = FALSE,
                           recursive = FALSE,
                           ignore.case = FALSE,
                           include.dirs = FALSE,
                           no.. = FALSE)
  
  shortASCIINames <- str_sub(ASCIINames, 2, -20)
  
  ASCIFiles <- list.files(pattern="^D\\$-\\$.")
  
  shortASCIIFiles <- str_sub(ASCIFiles, 4, -7)
  
  shortASCIIFiles <- str_replace_all(gsub("\\$", "", shortASCIIFiles), "[:punct:]", "")
  
  # shortASCIINames <- paste0("X", shortASCIINames)
  
  shortASCIINames %in% shortASCIIFiles
  
  # make a vector files to copy
  copyFiles <- ASCIFiles[which(shortASCIIFiles %in% shortASCIINames)]
  
  # make a vector files to copy
  # copyFiles <- NULL
  # copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 5)])
  # copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 6)])
  
  # check them
  copyFiles %in% ASCIFiles
  
  
  # replace the names in the copyFiles vector with the ASCIINames
  # copyFiles <- ASCIINames[(which(shortASCIINames %in% copyFiles))]
  
  # append the directory name
  # copyFiles <- paste0(
  #   "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/Marin100_NCCA_all/",
  #   copyFiles
  # )
  
  newLocation <- "edit"
  file.copy(copyFiles, newLocation)
  
  
  
  
  # # make a vector of error cases
  # copyFiles <- NULL
  # copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 3)])
  # copyFiles <- c(copyFiles, resultDF$shortExamNames[which(resultDF$correctCode == 4)])
  # 
  # # check them
  # copyFiles %in% shortASCIINames
  # # case X9NTJSO has no NCCA ASCII files because there are no charts
  # 
  # # replace the names in the copyFiles vector with the ASCIINames
  # copyFiles <- ASCIINames[(which(shortASCIINames %in% copyFiles))]
  # 
  # # append the directory name
  # copyFiles <- paste0(
  #   "~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/Marin100_NCCA_all/",
  #   copyFiles
  # )
  # 
  # newLocation <- "err"
  # file.copy(copyFiles, newLocation)
  
}


