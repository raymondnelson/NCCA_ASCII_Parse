# R function to aggregate all series scoresheets to a single .csv
# Raymond Nelson
# 07-29-2018

##########################


# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100")


# make a vector of OSS2 score sheet names
scoreSheetFileNames <- list.files(path = ".", 
                                  pattern = "^OSS2ScoreSheetDF+", 
                                  all.files = FALSE,
                                  full.names = FALSE, 
                                  recursive = FALSE,
                                  ignore.case = FALSE, 
                                  include.dirs = FALSE,
                                  no.. = FALSE)


# import the score sheets
i=1
for(i in 1:length(scoreSheetFileNames)) {
  tempScoreSheet <- read.csv(scoreSheetFileNames[i], row.names = NULL, stringsAsFactors = FALSE)
  assign(paste0("ESSScoreSheetDF_series_X_",shortExamNames[i]),tempScoreSheet, pos=1)
}


# fix the column names for the RQs
k=1
for(k in 1:length(scoreSheetFileNames)) {
  # tempScoreSheet <- get(str_sub(scoreSheetFileNames[k], 1, -7), pos=1)
  tempScoreSheet <- get(paste0("ESSScoreSheetDF_series_X_",shortExamNames[k]), pos=1)
  colnames(tempScoreSheet)[5:7] <- c("R5", "R7", "R10")
  # write.csv(tempScoreSheet,file=scoreSheetFileNames[k], row.names=FALSE)
  # assign(str_sub(scoreSheetFileNames[k], 1, -7), tempScoreSheet, pos=1)
  assign(paste0("ESSScoreSheetDF_series_X_",shortExamNames[k]), tempScoreSheet, pos=1)
}


#### fix the columns as numeric for all DFs

scoreSheetDFNames <- ls(pattern="ESSScoreSheetDF")
k=1
for(k in 1:length(scoreSheetDFNames)) {
  tempScoreSheet <- get(paste0("ESSScoreSheetDF_series_X_",shortExamNames[k]), pos=1)
  tempScoreSheet$R5 <- as.integer(tempScoreSheet$R5)
  tempScoreSheet$R7 <- as.integer(tempScoreSheet$R7)
  tempScoreSheet$R10 <- as.integer(tempScoreSheet$R10)
  assign(paste0("ESSScoreSheetDF_series_X_",shortExamNames[k]), tempScoreSheet, pos=1)
  
}


#### initialize a data frame with the same case order as the .csv names ####



resultDF <- cbind.data.frame(shortExamNames, 
                             MarinB=NA,
                             ESSResults=NA, 
                             criterionState=NA, 
                             correct=NA, 
                             correctCode=NA )

resultDF$shortExamNames <- as.character(resultDF$shortExamNames)

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

View(resultDF)


#####


scoreSheetAggFn <- function(x=scoreSheetFileNames, 
                            output=FALSE,
                            makeCSV=TRUE,
                            outName="allScoresDF.csv") {
  # R function to aggregate all series scoresheets to a single .csv
  # Raymond Nelson
  # 07-29-2018
  #
  ####
  #
  # x input is a vector of score sheet names for 
  # output=TRUE will return a data frame
  # output=FALSE will return NULL
  # makeCSV=TRUE will save the data frame as a .csv
  # outName is the name of the output .csv
  #
  ####
  
  scoreSheetFileNames <- x
  
  # shortExamNames for the scoreSheet .csv files
  # without the Marin B number and case status
  shortExamNames <- str_sub(scoreSheetFileNames, 17, -7)
  
  # make a list of score sheets in the global environ
  scoreSheetNames <- ls(pattern="^ESSScoreSheetDF_series_X_+")
  
  # check the length
  length(scoreSheetFileNames) == length(scoreSheetNames)
  
  # iterate over the score sheets and calculate the ESS result
  j=1
  for(j in 1:length(scoreSheetNames)) {
    print(scoreSheetNames[j])
    thisScoreSheet <- get(scoreSheetNames[j])
    
    # dataCheck is a vector to check the boundardy condition
    # for the number of available sensor scores for each subtotal
    dataCheck <- rep(NA, 3)
    dataCheck[1] <- length(thisScoreSheet[,5][!is.na(as.numeric(thisScoreSheet[,5]))])
    dataCheck[2] <- length(thisScoreSheet[,6][!is.na(as.numeric(thisScoreSheet[,6]))])
    dataCheck[3] <- length(thisScoreSheet[,6][!is.na(as.numeric(thisScoreSheet[,7]))])
    minSubVals <- min(dataCheck)
    selectMinSubVals <- which.min(dataCheck)
    
    subTotalScores <- colSums(thisScoreSheet[,c(5:7)], na.rm=TRUE)
    minSubTotalScore <- min(subTotalScores)
    selectSub <- which.min(subTotalScores)
    
    grandTotalScore <- sum(subTotalScores, na.rm=TRUE)
    
    # add the scores to the data frame
    {
      resultDF$R5_1_R[j] <- thisScoreSheet$R5[1]
      resultDF$R5_1_E[j] <- thisScoreSheet$R5[3]
      resultDF$R5_1_C[j] <- thisScoreSheet$R5[4]
      resultDF$R7_1_R[j] <- thisScoreSheet$R7[1]
      resultDF$R7_1_E[j] <- thisScoreSheet$R7[3]
      resultDF$R7_1_C[j] <- thisScoreSheet$R7[4]
      resultDF$R10_1_R[j] <- thisScoreSheet$R10[1]
      resultDF$R10_1_E[j] <- thisScoreSheet$R10[3]
      resultDF$R10_1_C[j] <- thisScoreSheet$R10[4]
      resultDF$R5_2_R[j] <- thisScoreSheet$R5[7]
      resultDF$R5_2_E[j] <- thisScoreSheet$R5[9]
      resultDF$R5_2_C[j] <- thisScoreSheet$R5[10]
      resultDF$R7_2_R[j] <- thisScoreSheet$R7[7]
      resultDF$R7_2_E[j] <- thisScoreSheet$R7[9]
      resultDF$R7_2_C[j] <- thisScoreSheet$R7[10]
      resultDF$R10_2_R[j] <- thisScoreSheet$R10[7]
      resultDF$R10_2_E[j] <- thisScoreSheet$R10[9]
      resultDF$R10_2_C[j] <- thisScoreSheet$R10[10]
      resultDF$R5_3_R[j] <- thisScoreSheet$R5[13]
      resultDF$R5_3_E[j] <- thisScoreSheet$R5[15]
      resultDF$R5_3_C[j] <- thisScoreSheet$R5[16]
      resultDF$R7_3_R[j] <- thisScoreSheet$R7[13]
      resultDF$R7_3_E[j] <- thisScoreSheet$R7[15]
      resultDF$R7_3_C[j] <- thisScoreSheet$R7[16]
      resultDF$R10_3_R[j] <- thisScoreSheet$R10[13]
      resultDF$R10_3_E[j] <- thisScoreSheet$R10[15]
      resultDF$R10_3_C[j] <- thisScoreSheet$R10[16]
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
    resultDF$ESSResults[j] <- ifelse(grandTotalScore >= 3 && minSubVals >= boundaryLevel,
                                     "NDI",
                                     ifelse(grandTotalScore <= -3 && minSubVals >= boundaryLevel,
                                            "DI",
                                            ifelse(min(subTotalScores) <= -7 && dataCheck[selectSub] >= boundaryLevel,
                                                   "DI",
                                                   "INC")))
    
  } # end iteration over scoreSheetNames
  
  
  
  
} # end scoreSheetAggFn