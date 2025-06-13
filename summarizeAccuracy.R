

#### start by clearing the global envir ####



{
  # rm(list=ls())
  
  # set.seed(1234567890)
}



# if(getOption("warn") !=2) {
#   # set the warn level to suppress warnings
#   if(!exists("oldw")) oldw <- getOption("warn")
#   # -1 to suppress warnings
#   # 0 is normal, warnings are display at end
#   # 1 warnings are display at the time
#   # 2 warnings are escalated to errors
#   options(warn = 2)
#   # reset to default
#   # options(warn = 0)
#   # rm(oldw)
#   print(paste("warn level:", oldw))
#   # getOption("warn")
# }



######### Set the dropbox path #########



{
  
  # this path is prepended to the file path before sourcing a script
  # mac
  if(!exists("RPath")) {
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  }
  # windows
  # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  
  # use this
  # source(paste0(RPath, <filePath>), echo=FALSE)
  
}



######### source the workFlow_init.R script #########



{
  
  # to stop on warnings
  # options(warn=2)
  # to reset default warning level
  # options(warn=0)
  
  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  # this is sourced by the workFlow_init.R script
  
}



######## set the working directory #############



{
  
  uniqueExams <- getUniqueExams(x="*_Data$")
  
  
  analysisLists <- ls(pattern =".ANALYSIS$")
  
  # exclude cases that consist only of the ACQT
  analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  examNames <- str_sub(analysisLists, 2, -11)
  
  
}



####################################################################

########  load the RDA objects to get the analysis lists ###########

if(length(ls(pattern="ANALYSIS"))==0) {
  
  myRdaNames <- list.files(pattern="*_2.Rda$")
  
  if(length(myRdaNames)==0) stop("no exams in this directory")
  
  i=1
  for(i in 1:length(myRdaNames)) {
    load(file=myRdaNames[i])
  }
  
  rm(myRdaNames)
  rm(i)
  
  uniqueExams <- getUniqueExams(x="*_Data$")
  
  
  analysisLists <- ls(pattern =".ANALYSIS$")
  
  # exclude cases that consist only of the ACQT
  analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  examNames <- str_sub(analysisLists, 2, -11)
  
  
  
}


#################################################

############# set up the summary ################


{
  
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")
  
  # set these for the test format
  # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  testFormat <- "CQT"
  # testFormat <- ""
  #testFormat <- "MGQT"
  # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # RQNames <- c("R1", "R2", "R3", "R4")
  RQNames <- c("R5", "R7", "R10", "none")
  # RQNames <- c("R5", "R8", "R11", "NA")
  
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+(1*length(RQNames))), 
  #                                        nrow=length(seriesTotalFiles)))
  
  
  DRule <- "TSR"
  # DRule <- "SSR"
  
  ESSMDecisionRule <- ifelse(DRule=="TSR", "TSR", "SSR")
  # ESSMDecisionRule <- "SSR"
  
  OSS3DecisionRule <- ifelse(DRule=="TSR", "TSR", "SCR")
  # OSS3DecisionRule <- "SCR"
  
  ROSSDecisionRule <- ifelse(DRule=="TSR", "GTR", "SSR")
  # ROSSDecisionRule <- "SSR"
  
  PADecisionRule <- ifelse(DRule=="TSR", "GTR", "GTR")
  
  # ESSMDecisionRule <- "SSR"
  # ESSMDecisionRule <- "TSR"
  
  # OSS3DecisionRule <- "SCR"
  # OSS3DecisionRule <- "TSR"
  
  # ROSSDecisionRule <- "GTR"
  
  
  summarizeResults <- TRUE
  
}


######## initialize some functions for decision rules ########


{
  
  # decision rules for summary - distinct from decision rules for algorithms
  
  sTSRFn <- function(x) {
    # modified two-stage-rule
    # x input is a vector of subtotal scores
    ifelse(sum(x, na.rm=TRUE) <= -3, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -7, 
                  -1,
                  ifelse(sum(x, na.rm=TRUE) >= 3,
                         1,
                         # ifelse(min(x[1:(length(x))], na.rm=TRUE) >= 1,
                         #        1,
                         #        0 ) 
                         0
                         ) ) )
  }
  
  s2TSRFn <- function(x) {
    # traditional two-stage-rule
    # x input is a vector of subtotal scores
    ifelse(sum(x, na.rm=TRUE) >= 3,
           1,
           ifelse(sum(x, na.rm=TRUE) <= -3, 
                  -1,
                  ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -7, 
                         -1,
                         0 ) ) )
  }
  
  sSSRFn <- function(x) {
    # subtotal-score-rule
    # x input is a vector of subtotal scores
    ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -3, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) >= 1,
                  1,
                  0 )
    )
  }
  
  s2SSRFn <- function(x) {
    # subtotal-score-rule
    # x input is a vector of subtotal scores
    ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -3, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) >= 2,
                  1,
                  0 )
    )
  }
  
  s3SSRFn <- function(x) {
    # subtotal-score-rule
    # x input is a vector of subtotal scores
    ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -3, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) >= 3,
                  1,
                  0 )
    )
  }
  
  tSSRFn <- function(x) {
    # subtotal-score-rule
    # traditional cutscores
    # x input is a vector of subtotal scores
    ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -3, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) >= 3,
                  1,
                  0 )
    )
  }
  
  sGTRFn <- function(x) {
    # grand total rule
    # x input is a vector of subtotal scores
    ifelse(sum(x, na.rm=TRUE) <= -3, 
           -1,
           ifelse(sum(x, na.rm=TRUE) >= 3,
                  1,
                  0 ) )
  }
  
  tGTRFn <- function(x) {
    # grand total rule
    # traditional cutscores
    # x input is a vector of subtotal scores
    ifelse(sum(x, na.rm=TRUE) <= -6, 
           -1,
           ifelse(sum(x, na.rm=TRUE) >= 6,
                  1,
                  0 ) )
  }
  
  sFZRFn <- function(x) {
    # Federal Zone Rule
    # for ZCT and BiZone results
    # traditional cutscores
    # x input is a vector of subtotal scores
    ifelse(sum(x, na.rm=TRUE) <= -6, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -3, 
                  -1,
                  ifelse(sum(x, na.rm=TRUE) >= 6,
                         # check that all subtotals are >= 1
                         ifelse(min(x[1:(length(x))], na.rm=TRUE) >= 1,
                                1,
                                0),
                         0 ) ) )
  }
  
}


#########   initialize a function for the correct codes   ###########


correctCodesFN <- function(x) {
  # define a function compute the correct codes
  # x input is a vector of 2 items
  # the first is the criterion state [1, -1]
  # criterion states are coded as 1=innocent, and -1=guilty
  # the second item is the test result 
  # test results are recoded as 1=truthful, -1=deceptive, and 0=inconclusive
  # output is a correct code
  # correct codes are as follows
  # 1=TP
  # 2=TN
  # 3=FN
  # 4=FP
  # 5=INCG
  # 6=INCI
  ###
  state <- x[1]
  state <- ifelse(state=="NDI", 1, ifelse(state=="DI", -1, state))
  state <- as.numeric(state)
  result <- as.numeric(x[2])
  ifelse(state==1,
         ifelse(result==0,
                6,
                ifelse(result==1,
                       2,
                       ifelse(result==-1,
                              4,
                              "error"))),
         ifelse(state==-1,
                ifelse(result==0,
                       5,
                       ifelse(result==-1,
                              1,
                              ifelse(result==1,
                                     3,
                                     "error")))) )
} # end correctCodesFN()


######## initialize a function to summarize the correct codes ########


accySumFn <- function(x=seriesTotalsDF$correctCode) {
  # summarize the accuracy from the sample accuracy codes
  # x is a vector of accuracy codes
  # 1 = TP
  # 2 = TN
  # 3 = FN
  # 4 = FP
  # 5 = INC-G
  # 6 = INC-I
  ###
  
  N <- length(x)
  nG <- length(which(x %in% c(1,3,5)))
  nI <- length(which(x %in% c(2,4,6)))
  
  accyColNames <- c("N", "nG", "nI", "TP", "TN", "FN", "FP", "IncG", "IncI", 
                    "COR", "INC", "CorG", "CorI", "UnwghtAccy", "UnwghtINC",
                    "PPV", "NPV", "FNI", "FPI")
  
  accuracySummary <- rep(0, times=length(accyColNames))
  names(accuracySummary) <- accyColNames
  
  accuracySummary['N'] <- length(x)
  accuracySummary['nG'] <- length(which(x %in% c(1, 3, 5)))
  accuracySummary['nI'] <- length(which(x %in% c(2, 4, 6)))
  
  accuracySummary['TP'] <- length(which(x == 1)) / nG
  accuracySummary['TN'] <- length(which(x == 2)) / nI
  accuracySummary['FN'] <- length(which(x == 3)) / nG
  accuracySummary['FP'] <- length(which(x == 4)) / nI
  accuracySummary['IncG'] <- length(which(x == 5)) / nG
  accuracySummary['IncI'] <- length(which(x == 6)) / nI
  
  accuracySummary['COR'] <- length(which(x %in% c(1, 2))) / ( N - length(which(x %in% c(5, 6))) )
  accuracySummary['INC'] <- length(which(x %in% c(5, 6))) / N
  accuracySummary['CorG'] <- length(which(x == 1)) / ( nG - length(which(x == 5)) )
  accuracySummary['CorI'] <- length(which(x == 2)) / ( nI - length(which(x == 6)) )
  accuracySummary['UnwghtAccy'] <- mean(c(accuracySummary['CorG'], 
                                          accuracySummary['CorI']))
  accuracySummary['UnwghtINC'] <- mean(c(accuracySummary['IncG'], 
                                         accuracySummary['IncI']))
  
  accuracySummary['PPV'] <- accuracySummary['TP'] / 
    (accuracySummary['TP'] + accuracySummary['FP'])
  accuracySummary['NPV'] <- accuracySummary['TN'] / 
    (accuracySummary['TN'] + accuracySummary['FN'])
  accuracySummary['FNI'] <- accuracySummary['FN'] / 
    (accuracySummary['FN'] + accuracySummary['TN'])
  accuracySummary['FPI'] <- accuracySummary['FP'] / 
    (accuracySummary['FP'] + accuracySummary['TP'])
  
  # output is a vector of named numbers
  return(accuracySummary)
  
} # end accySumFn()



######### function to compute ESS-M subtotal means for guilty and innocent cases #######


subtotalMeanFn <- function(x=c(criterionState, c(R1score, R2score, R3score, R4score))) {
  # compute the subtotal distributions for multiple issue exams
  # called by apply() with the seriesTotalsDF
  # after the criterion state is added to the seriesTotalsDF
  # uses the seriesTotalsDF RQ columns
  # x input is a vector of RQ subtotal scores
  # also requires the exam name
  # requires the criterion state of each case
  # guilty means are calculated for subtotals <= 0
  # innocent means are calculated for subtotals >= 0
  # subtotals for which the sign is inconsistent with the criterion are coerced to 1
  # output is the mean of subtotals
  # 
  
  # first get the criterionState
  thisCriterionState <- as.numeric(x[1])
  
  # calculate the RQ mean for innocent cases
  if(thisCriterionState == 1) {
    theseRQs <- which(x[2:length(x)] > 0) + 1 # add  2 to to start at 3
    notTheseRQs <- which(x[2:length(x)] <= 0) + 1
    theseRQScores <- as.numeric(x[theseRQs])
    # theseRQScores <- c(theseRQScores, rep(0, times=length(notTheseRQs)))
    return(mean(theseRQScores, na.rm=TRUE))
  } else {
    # calculate the RQ mean using only guilty cases
    theseRQs <- which(x[2:length(x)] < 0) + 1 # add 2 to to start at 3
    notTheseRQs <- which(x[2:length(x)] >= 0) + 1
    theseRQScores <- as.numeric(x[theseRQs])
    # theseRQScores <- c(theseRQScores, rep(0, times=length(notTheseRQs)))
    if(length(theseRQScores) == 0) return(0)
    if(length(theseRQScores) == 1) return(theseRQScores)
    return(mean(theseRQScores, na.rm=TRUE))
  }
  
  # RQMean <- mean(theseRQScores, na.rm=TRUE)
  # 
  # return(RQMean)
  
  x=c(criterionState=-1, c(R1score=1, R2score=0, R3score=3, R4score=-5))
  subtotalMeanFn(x=x)
  subtotalMeanFn(x=c(criterionState=-1, c(R1score=1, R2score=0, R3score=3, R4score=-5)))
  subtotalMeanFn(x=c(criterionState=-1, c(R1score=-3, R2score=0, R3score=0)))
  
} # end subtotalMeanFn()


############ function to compute column standard deviations ############


colSDs <- function(x=CQSensorMeansDF[2:ncol(CQSensorMeansDF)],pop=FALSE, na.rm=FALSE) {
  # R function to calculate the standard deviation for data frame cols
  # x input is a data frame
  # pop input selects either sample or population SD
  ###
  if(isFALSE(pop)) {
    return(apply(x, 2, sd, na.rm=na.rm))
  } else {
    sdp <- function(x) { (sqrt(var(x, na.rm=na.rm)*(length(x)-1)/length(x))) }
    return(apply(x, 2, sdp))
  }
}


##########  locate the analysis lists in the global environment ###########


{
  
  # examNames <- str_sub(seriesTotalFiles, 2, -25)
  
  # examNamesA <- str_sub(analysisNames, 2, -10)
  
  
  uniqueExams <- getUniqueExams(x="*_Data$")
  
  
  analysisLists <- ls(pattern =".ANALYSIS$")
  
  # exclude cases that consist only of the ACQT
  analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # keep the last character
  examNames <- str_sub(analysisLists, 2, -10)
  # omit the last character
  # examNames <- str_sub(analysisLists, 2, -11)
  
  # examNames <- str_sub(seriesTotalFiles, 2, -25)
  
  
  
}


##########  initialize the CRITERION STATE data frame ##########


{
  
  library(stringr)
  library(readr)
  
  newCriterionStateDF <- TRUE
  newCriterionStateDF <- FALSE
  
  criterionStateFileName <- list.files(".", pattern="?criterionState.csv")
  
  # if(length(criterionStateFileName) == 0) {
  if(newCriterionStateDF) {
    
    seriesTotalFiles <- list.files(pattern="ESSMSeriesTotals.csv")
    
    examNames <- str_sub(seriesTotalFiles, 2, -24) # keep the last character
    # examNames <- str_sub(seriesTotalFiles, 2, -25) # remove the last character
    # examNames <- str_sub(seriesTotalFiles, 3, 8)
    
    # criterionStateDF <- cbind.data.frame(examName=examNames, criterionState = -1)
    
    # write_csv(criterionStateDF, "criterionState.csv")
    
    
    # when the criterion state is coded "NDI" or "DI" in the file name
    # criterionState <- str_sub(seriesTotalFiles, 11, -24)
    # criterionState <- ifelse(criterionState == "NDI", 1, -1)
    
    
    # when the criterion state is coded 1 or 0 in the file name 
    # criterionState <- str_sub(seriesTotalFiles, 11, 11)
    criterionState <- str_sub(seriesTotalFiles, -24, -24)
    criterionState <- ifelse(criterionState %in% c("0", "D"), -1, 1)
    
    criterionStateDF <- cbind.data.frame(examName=examNames, criterionState)
    write_csv(criterionStateDF, "criterionState.csv")
    
    # View(criterionStateDF)
    
  }
  
  criterionStateFileName <- list.files(".", pattern="?criterionState.csv")
  
  # criterionStateDF <- read_csv("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII/criterionState.csv")
  # criterionStateDF <- read_csv("./.criterionState.csv")
  criterionStateDF <- read_csv(criterionStateFileName)
  
  # # remove the $$ characters from Axciton file names
  # criterionStateDF$examName <- str_sub(criterionStateDF$examName, 3, -1)
  # 
  # # remove punctuation characters fron Axciton file names
  # criterionStateDF$examName <- gsub("[[:punct:]]", "", criterionStateDF$examName)
  
  # replace Axciton file names with short names
  # criterionStateDF$examName <- criterionStateDF$shortName
  
  # Marin reset Axciton names 
  criterionStateDF$examName <- gsub("[[:punct:]]", "", criterionStateDF$oldID) 
  criterionStateDF$examName <- str_c("X", criterionStateDF$examName)
  
  # View(criterionStateDF)
  criterionStateDF[order(criterionStateDF$examName),]
  
  # pad single digit numeric exam names to 3 characters 
  # criterionStateDF$examName <- str_pad(criterionStateDF$examName, 3, pad="0")
  # write_csv(criterionStateDF, "criterionState.csv")
  
  # for(i in 1:nrow(criterionStateDF)) {
  #   criterionStateDF$criterionState[i] <- ifelse(criterionStateDF$criterionState[i]=="NDI", 1, -1)
  # }
  
  # names(criterionStateDF) <- c("examName", "criterionState")
  
  # criterionStateDF$examName <- str_pad(criterionStateDF$examName, 3, side="left", pad="0")
  
  # THE CRITERION STATE DF MUST BE SORTED IN ASCENDING ORDER #
  
}


#############################################################


############# summarize the ESS-M series totals without PLE ##################


summarizeResults <- TRUE

if(isTRUE(summarizeResults)) {
  
  withPLE <- FALSE
  
  # library(stringr)
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv")[1], 
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  
  names(criterionStateDF)[1] <- c("examName")
  # criterionStateDF$examName
  
  # get the ESS-M series totals for all 
  seriesTotalFiles <- list.files(pattern="ESSMSeriesTotals.csv")
  
  scoreSheetFiles <- list.files(pattern="ESSMScoresheet.csv")
  
  # numbCases <- length(seriesTotalFiles)
  
  numbCases <- length(scoreSheetFiles)
  
  # initialize a data frame for the criterion state if none exists
  if(!exists("criterionStateDF")) {
    criterionStateDF <- 
      cbind.data.frame(examName=str_sub(seriesTotalFiles, 2, -24), 
                       criterionState=rep(NA, length(seriesTotalFiles)))
  }
  
  # # set these for the test format
  # # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # # testFormat <- ""
  # #testFormat <- "MGQT"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  # testFormat <- "CQT"
  
  # # initialize a data frame to aggregate the series totals
  # RQNames <- c("R1", "R2", "R3", "R4")
  # RQNames <- c("R5", "R7", "R10")
  
  # using the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+(1*length(RQNames))), 
  #                                        nrow=length(seriesTotalFiles)))
  
  # initialize a data frame using the score sheet files
  seriesTotalsDF <- as.data.frame(matrix(ncol=(5+(1*length(RQNames))), 
                                         nrow=length(scoreSheetFiles)))
  # paste0(RQNames, "_sTot"), 
  names(seriesTotalsDF) <- c("examName", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  # View(seriesTotalsDF)
  
  # iterate over the series totals to aggregate the totals for all exams
  i=1
  for (i in 1:length(scoreSheetFiles)) {
    
    if(i > length(scoreSheetFiles)) break()
    
    # thisCaseNumber <- str_sub(scoreSheetFiles[i], 2, 4)
    # 
    # thisDFRow <- which(seriesTotalsDF$examName == thisCaseNumber)
    
    # thisCSV <- read.csv(seriesTotalFiles[i], header=TRUE, stringsAsFactors=FALSE)
    
    # using the score sheet files
    thisCSV <- read.csv(scoreSheetFiles[i], header=TRUE, stringsAsFactors=FALSE)
    
    # without PLE
    if(!isTRUE(withPLE)) {
      thisCSV <- thisCSV[thisCSV$sensorName != "PLE",]
    }
      
    # View(thisCSV)
    
    # exam nname
    thisExamName <- examNames[i]
    # thisExamName <- str_sub(scoreSheetFiles[i], 2, -23)
    # thisExamName <- str_sub(seriesTotalFiles[i], 2, 8)
    
    # for Ohio, but not for simulation
    # thisExamName <- str_sub(thisExamName, 1, -2)
    
    # thisExamName <- str_sub(thisExamName, 1, 3)
    
    seriesTotalsDF[i,'examName'] <- thisExamName
    # series
    thisSeries <- str_sub(scoreSheetFiles[i], -20, -20)
    seriesTotalsDF[i,'series'] <- thisSeries
    # test format
    seriesTotalsDF[i,'testFormat'] <- testFormat
    
    # subtotal scores
    # using the seriesTotals
    # seriesTotalsDF[i,4:(4+ncol(thisCSV)-3)] <- thisCSV[1,3:(3+ncol(thisCSV)-3)] 
    
    # subtotal scores
    # using the score sheets
    
    nRQs <- ncol(thisCSV)-4
    seriesTotalsDF[i,4:(4+nRQs-1)] <- colSums(thisCSV[5:(5+nRQs-1)], na.rm=TRUE)
    
    # add the criterion state
    seriesTotalsDF[i,'criterionState'] <-
      criterionStateDF$criterionState[which(criterionStateDF$examName %in% thisExamName)]
  
  } # end i loop
  
  # View(seriesTotalsDF)
  
  ## calculate the totals ##
  
  {
    
    # calculate the grand total
    seriesTotalsDF$grandTotal <- 
      apply(seriesTotalsDF[,4:(4+length(RQNames)-1)], 1, sum, na.rm=TRUE)
    
    # calculate the subtotal means
    # commented out JaN 23, 2025 to work with the Axciton mixed format sample
    # seriesTotalsDF$sTotalMean <- 
    #   # apply(cbind(seriesTotalsDF['criterionState'], seriesTotalsDF[,c(4:(4+length(RQNames)-1))]), 1, subtotalMeanFn)
    #   seriesTotalsDF[,'grandTotal'] / nRQs
    seriesTotalsDF$sTotalMean <- NA
    
    # compute the min subtotal score
    seriesTotalsDF$minSubtotalScore <- 
      apply(seriesTotalsDF[,c(4:(4+length(RQNames)-1))], 1, min, na.rm=TRUE)
    
    # compute the max subtotal score
    seriesTotalsDF$maxSubtotalScore <- 
      apply(seriesTotalsDF[,c(4:(4+length(RQNames)-1))], 1, max, na.rm=TRUE)
    
  }
  
  thisRule <- ifelse(ESSMDecisionRule=="SSR", sSSRFn, sTSRFn)
  
  
  # call the decision rule to get the result
  # seriesTotalsDF$Result <- apply(seriesTotalsDF[,4:(4+length(RQNames)-1)], 1, sTSRFn)
  seriesTotalsDF$Result <- apply(seriesTotalsDF[,4:(4+length(RQNames)-1)], 1, thisRule)
  
  # sSSRFn(seriesTotalsDF[1,4:7])
  
  ### add the correct codes here
  
  # View(seriesTotalsDF)
  
  # apply the correctCodesFn function to the seriesTotalsDF
  seriesTotalsDF$correctCode <- apply(cbind(seriesTotalsDF['criterionState'],seriesTotalsDF['Result']),
                                      1, correctCodesFN)
  
  # write the CSV
  write.csv(seriesTotalsDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", length(seriesTotalFiles), "_seriesTotals.csv") )
  
  ESSMSummaryDF <- seriesTotalsDF
  
  # View(seriesTotalsDF)
} 


############# summarize the ESS-M series totals with PLE ##################


summarizeResults <- TRUE

if(isTRUE(summarizeResults)) {
  
  library(stringr)
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"), 
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  
  # set the column name
  names(criterionStateDF)[1] <- c("examName")
  # criterionStateDF$examName
  
  # get the ESS-M series totals for all 
  seriesTotalFiles <- list.files(pattern="ESSMSeriesTotals.csv")
  
  scoreSheetFiles <- list.files(pattern="ESSMScoresheet.csv")
  
  # numbCases <- length(seriesTotalFiles)
  
  numbCases <- length(scoreSheetFiles)
  
  # initialize a data frame for the criterion state
  if(!exists("criterionStateDF")) {
    criterionStateDF <- 
      cbind.data.frame(examName=str_sub(scoreSheetFiles, 2, -23), 
                       criterionState=rep(NA, length(seriesTotalFiles)))
  }
  
  # View(criterionStateDF)
  
  # # set these for the test format
  # # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # # testFormat <- ""
  # #testFormat <- "MGQT"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # # initialize a data frame to aggregate the series totals
  # RQNames <- c("R1", "R2", "R3", "R4")
  # RQNames <- c("R5", "R7", "R10")
  
  # using the series totals
  # seriesTotalsPDF <- as.data.frame(matrix(ncol=(5+(1*length(RQNames))), 
  #                                        nrow=length(seriesTotalFiles)))
  
  # initialize a data frame for the series totals using the score sheet
  seriesTotalsPDF <- as.data.frame(matrix(ncol=(5+(1*length(RQNames))), 
                                         nrow=length(scoreSheetFiles)))
  # paste0(RQNames, "_sTot"), 
  names(seriesTotalsPDF) <- c("examName", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  # View(seriesTotalsPDF)
  
  # iterate over the series totals to aggregate the totals for all exams
  i=1
  for (i in 1:nrow(seriesTotalsPDF)) {
    
    # thisCSV <- read.csv(seriesTotalFiles[i], header=TRUE, stringsAsFactors=FALSE)
    
    # using the score sheet files
    thisCSV <- read.csv(scoreSheetFiles[i], header=TRUE, stringsAsFactors=FALSE)
    
    # View(thisCSV)
    
    # exam nname
    thisExamName <- examNames[i]
    # thisExamName <- str_sub(scoreSheetFiles[i], 2, -23)
    # thisExamName <- str_sub(seriesTotalFiles[i], 2, 8)
    
    # ohio cases have the criterion state coded at the end of the exam name
    # thisExamName <- str_sub(thisExamName, 1, -2)
    
    # thisExamName <- str_sub(thisExamName, 1, 3)
    
    seriesTotalsPDF[i,'examName'] <- thisExamName
    
    # series name
    thisSeries <- str_sub(scoreSheetFiles[i], -20, -20)
    seriesTotalsPDF[i,'series'] <- thisSeries
    
    # test format
    seriesTotalsPDF[i,'testFormat'] <- testFormat
    
    # subtotal scores
    # using the seriesTotals
    # seriesTotalsPDF[i,4:(4+ncol(thisCSV)-3)] <- thisCSV[1,3:(3+ncol(thisCSV)-3)] 
    
    # subtotal scores
    # using the score sheets
    nRQs <- ncol(thisCSV)-4
    seriesTotalsPDF[i,4:(4+nRQs-1)] <- colSums(thisCSV[5:ncol(thisCSV)], na.rm=TRUE)
    
    # add the criterion state
    seriesTotalsPDF[i,'criterionState'] <-
      criterionStateDF$criterionState[criterionStateDF$examName %in% thisExamName]
    
  } # end i loop
  
  # View(seriesTotalsPDF)
  
  ## calculate the totals ##
  
  {
    
    # calculate the grand total
    seriesTotalsPDF$grandTotal <- 
      apply(seriesTotalsPDF[,4:(4+length(RQNames)-1)], 1, sum, na.rm=TRUE)
    
    # calculate the subtotal means
    # commented out Jan 23, 2025 to work with the Axciton mixed case sample
    # seriesTotalsPDF$sTotalMean <- 
    #   # apply(cbind(seriesTotalsPDF['criterionState'], seriesTotalsPDF[,c(4:(4+length(RQNames)-1))]), 1, subtotalMeanFn)
    #   seriesTotalsPDF[,'grandTotal'] / nRQs
    seriesTotalsPDF$sTotalMean <- NA
    
    # compute the min subtotal score
    seriesTotalsPDF$minSubtotalScore <- 
      apply(seriesTotalsPDF[,c(4:(4+length(RQNames)-1))], 1, min, na.rm=TRUE)
    
    # compute the max subtotal score
    seriesTotalsPDF$maxSubtotalScore <- 
      apply(seriesTotalsPDF[,c(4:(4+length(RQNames)-1))], 1, max, na.rm=TRUE)
    
  }
  
  thisRule <- ifelse(ESSMDecisionRule=="SSR", sSSRFn, sTSRFn)
  
  
  # call the decision rule to get the result
  seriesTotalsPDF$Result <- apply(seriesTotalsPDF[,4:(4+length(RQNames)-1)], 1, thisRule)
  
  # sSSRFn(seriesTotalsPDF[1,4:7])
  
  ### add the correct codes here
  
  # View(seriesTotalsPDF)
  
  # apply the correctCodesFn function to the seriesTotalsPDF
  seriesTotalsPDF$correctCode <- apply(cbind(seriesTotalsPDF['criterionState'],seriesTotalsPDF['Result']),
                                      1, correctCodesFN)
  
  # write the CSV
  write.csv(seriesTotalsPDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", length(seriesTotalFiles), "_seriesTotalsP.csv") )
  
  ESSMPSummaryDF <- seriesTotalsPDF
  
  ## slice the guilty and innocent data frames
  
  guiltyDF <- seriesTotalsPDF[seriesTotalsPDF$criterionState == -1,]
  guiltySummary <- summary(guiltyDF)
  
  innocentDF <- seriesTotalsPDF[seriesTotalsPDF$criterionState == 1,]
  innocentSummary <- summary(innocentDF)
  
  print(guiltySummary)
  print(innocentSummary)
  
  # View(seriesTotalsPDF)
  
  # View(innocentDF)
  # View(guiltyDF)
} 



##################  summarize the ESS-M sensor totals  ###################



summarizeSensorInfo <- TRUE


if(isTRUE(summarizeSensorInfo)) {
  
  # initialize a vector of file names
  sensorTotalsFiles <- list.files(pattern="ESSMSensorTotals.csv")
  
  # get the criterion state for all exams
  # criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"), 
  #                              header=TRUE, 
  #                              stringsAsFactors=FALSE)
  # 
  # names(criterionStateDF)[1] <- c("examName")
  
  # # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # # testFormat <- "MGQT"
  # # testFormat <- str_sub(sensorTotalsFiles[], 9, -24)
  
  # initialize a data frame 
  sensorTotalsDF <- as.data.frame(matrix(ncol=7, nrow=length(sensorTotalsFiles)))
  sensorNames <- c("Pneumo", "EDA", "Cardio", "PLE")
  # RQNames <- c("R1", "R2", "R3", "R4")
  names(sensorTotalsDF) <- c("examName", "series", "testFormat", sensorNames)
  sensorTotalsDF$testFormat <- testFormat
  # View(sensorTotalsDF)
  
  
  
  if(!exists("criterionStateDF")) {
    criterionStateDF <- 
      cbind.data.frame(examName=str_sub(sensorTotalsFiles, 2, -24), 
                       criterionState=rep(NA, length(seriesTotalFiles)))
  }
  
  
  # calculate the grand total
  # sensorTotalsDF$grandTotal <-
  #   apply(sensorTotalsDF[,3:(3+length(RQNames)-1)], 1, sum, na.rm=TRUE)

  sensorTotalsDF$grandTotal <- NA
  sensorTotalsDF$criterionState <- NA
  sensorTotalsDF$sTotalmean <- NA
  # sensorTotalsDF$R1 <- NA
  # sensorTotalsDF$R2 <- NA
  # sensorTotalsDF$R3 <- NA
  # sensorTotalsDF$R4 <- NA
  
  sensorTotalsDF$minSubtotal <- NA
  sensorTotalsDF$maxSubtotal <- NA
  
  # iterate over the sensorTotalsFiles
  i=1
  for(i in 1:length(sensorTotalsFiles)) {
    
    theseSensorTotals <- 
      read.csv(sensorTotalsFiles[i], header=TRUE, stringsAsFactors=FALSE)
    
    thisExamName <- examNames[i]
    # thisExamName <- str_sub(sensorTotalsFiles[i], 2, -25)
    # thisExamName <- str_sub(sensorTotalsFiles[i], 2, 8)
    
    # ohio
    # thisExamName <- str_sub(thisExamName, 1, -2)
    
    # thisExamName <- str_sub(thisExamName, 1, 3)
    
    # add the PLE column if it is missing 20210202
    if(!("PLE" %in% names(theseSensorTotals))) {
      theseSensorTotals$PLE <- NA
    }
    # View(thisID)
    sensorTotalsDF[i,'examName'] <- thisExamName
    thisSeries <- str_sub(sensorTotalsFiles[i], -22, -22)
    sensorTotalsDF[i,'series'] <- thisSeries
    sensorTotalsDF[i,'criterionState'] <- 
      criterionStateDF$criterionState[criterionStateDF$examName %in% thisExamName]
    # calculate the sensor subtotals
    sensorTotalsDF[i,c(4:(4+length(sensorNames)-1))] <- 
      colSums(theseSensorTotals[,c(4:(4+length(sensorNames)-1))], na.rm=TRUE)
    
  } # end i loop over sensor totals files
  
   # View(sensorTotalsDF)
  
  # calculate the grand total
  sensorTotalsDF$grandTotal <-
    apply(sensorTotalsDF[,c(4:(4+length(sensorNames)-1))], 1, sum, na.rm=TRUE)
  
  # call the decision rule to get the result
  # sensorTotalsDF$Result <- apply(seriesTotalsDF[,4:7], 1, sSSRFn)
  # seriesTotalsDF$Result <- apply(seriesTotalsDF[,3:(ncol(seriesTotalsDF)-1)], 1, SSRFn)
  
  # get the result from the seriesTotalsPDF
  j=1
  for(j in 1:nrow(sensorTotalsDF)) {
    thisExamName <- sensorTotalsDF$examName[j]
    sensorTotalsDF$Result[j] <-
      seriesTotalsPDF$Result[which(seriesTotalsPDF$examName %in% thisExamName)]
  }
  
  # get the subtotal mean from the seriesTotalsDF
  j=1
  for(j in 1:nrow(sensorTotalsDF)) {
    thisExamName <- sensorTotalsDF$examName[j]
    sensorTotalsDF$sTotalmean[j] <-
      seriesTotalsPDF$sTotalMean[seriesTotalsPDF$examName %in% thisExamName]
  }
  
  # get the RQ subtotal scores from the seriesTotalsDF
  # k=1
  # for(k in 1:nrow(sensorTotalsDF)) {
  #   thisExamName <- sensorTotalsDF$examName[k]
  #   sensorTotalsDF[k,c(8:11)] <- 
  #     seriesTotalsPDF[which(seriesTotalsPDF$examName %in% thisExamName),c(4:7)]
  # }
  
  # View(sensorTotalsDF)
  
  # get the min and max subtotal scores
  l=1
  for(l in 1:nrow(sensorTotalsDF)) {
    thisExamName <- sensorTotalsDF$examName[l]
    sensorTotalsDF[l,c(11:12)] <- 
      seriesTotalsPDF[which(seriesTotalsPDF$examName %in% thisExamName),c(11:12)]
  }
  
  # add the correct code the sensorTotalsDF
  m=1
  for(m in 1:nrow(sensorTotalsDF)) {
    thisExamName <- sensorTotalsDF$examName[m]
    sensorTotalsDF$correctCode[m] <- 
      seriesTotalsDF$correctCode[which(seriesTotalsPDF$examName %in% thisExamName)]
  }
  
  # slice and summarize the sensor totals for guilty and innocent cases
  
  guiltyDF <- sensorTotalsDF[sensorTotalsDF$criterionState == -1,]
  guiltySummary <- summary(guiltyDF)
  
  innocentDF <- sensorTotalsDF[sensorTotalsDF$criterionState == 1,]
  innocentSummary <- summary(innocentDF)
  
  # View(guiltyDF)
  # View(innocentDF)
  
  # write the CSV
  write.csv(sensorTotalsDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", length(sensorTotalsFiles), "_sensorTotals.csv"))
  
  # sensorTotalsDF <- read.csv(list.files(pattern="sensorTotals.csv"), header=TRUE, stringsAsFactors=FALSE )
  
  print(guiltySummary)
  print(innocentSummary)
  
  # summarizeSensorInfo <- TRUE
  
  # View(sensorTotalsDF)
  
}



####################  summarize the OSS-3 results  #######################



summarizeOSS3 <- TRUE
# summarizeOSS3 <- FALSE



if(isTRUE(summarizeOSS3)) {
  
  print("summarize the OSS-3 results")
  
  library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # exclude cases that consist only of the ACQT
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  
  # names(criterionStateDF)[1] <- c("examName")
  
  # View(criterionStateDF)
  
  # # set these for the test format
  # # testFormat <- "Utah"
  # # testFormat <- "FedZCT"
  # testFormat <- "MGQT"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  
  # RQNames <- c("R1", "R2", "R3", "R4")
  
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  OSS3SummaryDF <- as.data.frame(matrix(ncol=(11+(1*length(RQNames))), 
                                        nrow=length(analysisLists)))
  names(OSS3SummaryDF) <- c("examName", 
                            "testFormat",
                            "testResult",
                            RQNames,
                            "grandMean",
                            "grandMeanPVal",
                            "minRQPVal",
                            "minRQName",
                            "Pneumo",
                            "EDA",
                            "Cardio",
                            "criterionState" )
  # View(OSS3SummaryDF)
  
  # iterate on the exams in the global env
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      # View(thisAnalysis)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -11)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the OSS-3 analysis result
        OSS3Analysis <- 
          thisAnalysis[[seriesName]][["OSS3Output"]]
        
        # View(OSS3Analysis)
        
        if(is.null(OSS3Analysis)) next()
        
        # slice the result info for the case
        testResult <- OSS3Analysis[['OSS3Result']]
        
        # RQNames <- OSS3Analysis[['OSS3RQNames']]
        # RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
        
        grandMean <- OSS3Analysis[['OSS3GrandMeanZ']]
        grandMeanPVal <- OSS3Analysis[['OSS3PVal']]
        minRQPVal <- OSS3Analysis[['OSS3MinRQPVal']]
        minRQName <- OSS3Analysis[['OSS3MinRQName']]
        
        sensorMeansDF <- OSS3Analysis[['OSS3SensorMeansDF']]
        
        sensorMeans <- rowMeans(sensorMeansDF[,4:ncol(sensorMeansDF)], na.rm=TRUE)
        
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      # submit the case result to the summary data frame
      OSS3SummaryDF[i,] <- 
        c(examName, 
          testFormat,
          testResult,
          RQNames,
          grandMean,
          grandMeanPVal,
          minRQPVal,
          minRQName, 
          sensorMeans,
          criterionState )
      
    } # end j loop over series
    
  } # end i loop over exams
  
  ## recode the OSS-3 result ##
  
  OSS3SummaryDF$recodeResult <- 
    ifelse(OSS3SummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(OSS3SummaryDF$testResult == "NDI/NSR",
                  1,
                  0))
  
  ## summarize the OSS-3 result ##
  
  # str(OSS3SummaryDF)
  OSS3SummaryDF$correctCode <- 
    apply(cbind(OSS3SummaryDF[,'criterionState'], OSS3SummaryDF[,'recodeResult']),
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(OSS3SummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_OSS3Summary.csv") ) 
  
  # View(OSS3SummaryDF)
  
  # summarizeOSS3 <- FALSE
  
} # end if isTRUE(summarizeOSS3)



#################  summarize OSS-3 sensor correlations  #################



summarizeOSS3Sensors <- FALSE
summarizeOSS3Sensors <- TRUE



if(isTRUE(summarizeOSS3Sensors)) {

  fileNames <- list.files(pattern="OSS3SensorMeans")
  
  # examNames <- str_sub(fileNames, 2, -24)
  
  # examNames <- str_sub(fileNames, 2, 8)
  # examNames <- str_sub(examNames, 1, 3)

  # Initialize an object
  OSS3SensorMeansDF <- NA
  
  i=1
  for(i in 1:length(fileNames)) {
    
    thisCSV <- read_csv(fileNames[i])
    
    theseMeans <- as.vector(rowMeans(thisCSV[4:ncol(thisCSV)], na.rm=TRUE))
    
    # if(any(is.na(theseMeans))) stop("problem with sensor means")
    
    # theseMeans <- c(examNames[i], theseMeans)
    
    thisState <- 
      criterionStateDF$criterionState[which(criterionStateDF$examName == examNames[i])]
    
    thisCase <- c(theseMeans, thisState)
    names(thisCase) <- c(thisCSV$sensorName, "criterionState")
    
    OSS3SensorMeansDF <- 
      rbind.data.frame(OSS3SensorMeansDF, thisCase)
    
  }
  
  # remove the empty first row
  OSS3SensorMeansDF <- OSS3SensorMeansDF[2:nrow(OSS3SensorMeansDF),]
  
  
  colnames(OSS3SensorMeansDF) <- c(thisCSV$sensorName, "criterionState")
  
  OSS3SensorMeansDF <- cbind(examName=examNames, OSS3SensorMeansDF)
  # View(OSS3SensorMeansDF)

  # fix the column types
  OSS3SensorMeansDF[,2] <- as.numeric(OSS3SensorMeansDF[,2])
  OSS3SensorMeansDF[,3] <- as.numeric(OSS3SensorMeansDF[,3])
  OSS3SensorMeansDF[,4] <- as.numeric(OSS3SensorMeansDF[,4])
  OSS3SensorMeansDF[,5] <- as.numeric(OSS3SensorMeansDF[,5])
  
  OSS3ColMeans <- colMeans(OSS3SensorMeansDF[2:4], na.rm=TRUE)
  OSS3ColSDs <- colSDs(OSS3SensorMeansDF[2:4], na.rm=TRUE)
  
  print(OSS3ColMeans)
  print(OSS3ColSDs)
  
  selectCols <- which(!is.na(OSS3SensorMeansDF$Pneumo))
  OSS3PnCor <- 
    cor(OSS3SensorMeansDF$Pneumo[selectCols], OSS3SensorMeansDF$criterionState[selectCols])
  selectCols <- which(!is.na(OSS3SensorMeansDF$AutoEDA))
  OSS3EDACor <- 
    cor(OSS3SensorMeansDF$AutoEDA[selectCols], OSS3SensorMeansDF$criterionState[selectCols])
  selectCols <- which(!is.na(OSS3SensorMeansDF$Cardio))
  OSS3CardiorCor <-
    cor(OSS3SensorMeansDF$Cardio[selectCols], OSS3SensorMeansDF$criterionState[selectCols])
  
  print(OSS3PnCor)
  print(OSS3EDACor)
  print(OSS3CardiorCor)
  
  # examNames <- str_sub(fileNames, 2, -23)
  # examNames <- str_sub(fileNames, 2, 8)
  
  # View(OSS3SensorMeansDF)
  # View(criterionStateDF)
  
  # summarizeOSS3Sensors <- FALSE
    
}



#########  summarize the PA Probability Analysis results  #########



summarizePA <- TRUE
# summarizePA <- FALSE



if(isTRUE(summarizePA)) {
  
  print("summarize the PA results")
  
  library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  
  # View(criterionStateDF)
  
  # names(criterionStateDF)[1] <- c("examName")
  
  # # set these for the test format
  # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # testFormat <- "AFMGQTLEPET"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  # RQNames <- c("R1", "R2", "R3", "R4")
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  PASummaryDF <- as.data.frame(matrix(ncol=(7+length(RQNames)), 
                                        nrow=length(analysisLists)))
  names(PASummaryDF) <- c("examName", 
                            "testFormat",
                            "testResult",
                            RQNames,
                            "PAScore",
                            "PAPostProbT",
                            "PAPostProbD",
                            "criterionState" )
  
  # View(PASummaryDF)
  
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -11)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
      
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the PA analysis result
        PAAnalysis <- 
          thisAnalysis[[seriesName]][["PAOutput"]]
        
        if(is.null(PAAnalysis)) next()
        
        # slice the result info for the case
        testResult <- PAAnalysis[['PAResult']]
        
        # RQNames <- PAAnalysis[['PARQNames']]
        # RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
        
        PAScore <- PAAnalysis[['PAScore']]
        PAPostProbT <- PAAnalysis[['PAPostProbT']]
        PAPostProbD <- PAAnalysis[['PAPostProbD']]
        
        
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      # submit the case result to the summary data frame
      PASummaryDF[i,] <- 
        c(examName, 
          testFormat,
          testResult,
          RQNames,
          PAScore,
          PAPostProbT,
          PAPostProbD,
          criterionState )
      
    } # end j loop over series
    
  } # end i loop over exams
  
  ## recode the PA result ##
  
  PASummaryDF$recodeResult <- 
    ifelse(PASummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(PASummaryDF$testResult == "NDI/NSR",
                  1,
                  0))
  
  # View(PASummaryDF)
  
  ## summarize the PA result ##
  
  # str(PASummaryDF)
  PASummaryDF$correctCode <- 
    apply(PASummaryDF[,c((ncol(PASummaryDF)-1),ncol(PASummaryDF))],
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(PASummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_PASummary.csv") ) 
  
  # View(PASummaryDF)
  
  # summarizePA <- FALSE

} # end if isTRUE(summarizePA)



#########  summarize the OSS-2  results  #########



summarizeOSS2 <- TRUE
# summarizeOSS2 <- FALSE



if(isTRUE(summarizeOSS2)) {
  
  print("summarize the OSS-2 results")
  
  library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE) 
  }
  # View(criterionStateDF)
  
  # names(criterionStateDF)[1] <- c("examName")
  # 
  # # set these for the test format
  # # testFormat <- "Utah"
  # # testFormat <- "FedZCT"
  # testFormat <- "AFMGQTLEPET"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  # RQNames <- c("R1", "R2", "R3", "R4")
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  # OSS2SummaryDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                       nrow=length(analysisLists)))
  OSS2SummaryDF <- as.data.frame(matrix(ncol=5, 
                                        nrow=length(analysisLists)))
  names(OSS2SummaryDF) <- c("examName", 
                            "testFormat",
                            "testResult",
                            # RQNames,
                            "OSS2Score",
                            # "PAPostProbT",
                            # "PAPostProbD",
                            "criterionState" )
  # View(OSS2SummaryDF)
  
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -11)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the Oss-2 analysis result
        OSS2Analysis <- 
          thisAnalysis[[seriesName]][["OSS2Output"]]
        
        if(is.null(OSS2Analysis)) next()
        
        # slice the result info for the case
        testResult <- OSS2Analysis[['OSS2Result']]
        
        RQNames <- OSS2Analysis[['OSS2RQNames']]
        if(length(RQNames) < 4) {
          RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
        }
        
        OSS2Score <- OSS2Analysis[['OSS2GrandTotal']]
        # PAPostProbT <- PAAnalysis[['PAPostProbT']]
        # PAPostProbD <- PAAnalysis[['PAPostProbD']]
        
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      # submit the case result to the summary data frame
      OSS2SummaryDF[i,] <- 
        c(examName, 
          testFormat,
          testResult,
          # RQNames,
          OSS2Score,
          criterionState )
      
    } # end j loop over series
    
  } # end i loop over exams
  
  ## recode the oss-2 result ##
  
  OSS2SummaryDF$recodeResult <- 
    ifelse(OSS2SummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(OSS2SummaryDF$testResult == "NDI/NSR",
                  1,
                  0))
  # View(OSS2SummaryDF)
  
  ## summarize the OSS-2 result ##
  
  # str(OSS-2SummaryDF)
  OSS2SummaryDF$correctCode <- 
    apply(OSS2SummaryDF[,c((ncol(OSS2SummaryDF)-1),ncol(OSS2SummaryDF))],
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(OSS2SummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_OSS2Summary.csv") ) 
  
  # View(OSS2SummaryDF)
  
  # summarizeOSS2 <- FALSE
  
} # end if isTRUE(summarizeOSS2)



#########  summarize the ROSS results  #########



# summarizeROSS <- FALSE
summarizeROSS <- TRUE



if(isTRUE(summarizeROSS)) {
  
  print("summarize the ROSS results")
  
  # library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  # View(criterionStateDF)
  
  # names(criterionStateDF)[1] <- c("examName")
  
  # # set these for the test format
  # # testFormat <- "Utah"
  # # testFormat <- "FedZCT"
  # testFormat <- "AFMGQTLEPET"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  
  # RQNames <- c("R1", "R2", "R3", "R4")
  # RQNames <- c("1R1", "1R2", "2R1", "2R2", "3R1", "3R2", "4R1", "4R2")
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  # RQNames <- ROSSAnalysis[['ROSSRQNames']]
  RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
  
  
  ROSSSummaryDF <- as.data.frame(matrix(ncol=(7+length(RQNames)), 
                                      nrow=length(analysisLists)))
  names(ROSSSummaryDF) <- c("examName", 
                          "testFormat",
                          "testResult",
                          RQNames,
                          "grandTotal",
                          "resultUsing",
                          "lowestSubtotal",
                          "criterionState" )
  # View(ROSSSummaryDF)
  
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      # View(thisAnalysis)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -10)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
      
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the Rank Order ROSS analysis result
        ROSSAnalysis <- 
          thisAnalysis[[seriesName]][["ROSSOutput"]]
        
        if(is.null(ROSSAnalysis)) next()
        
        # slice the result info for the case
        testResult <- ROSSAnalysis[['ROSSResult']]
        
        # RQNames <- ROSSAnalysis[['ROSSRQNames']]
        # RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
        
        grandTotal <- ROSSAnalysis[['ROSSGrandTotal']]
        resultUsing <- ROSSAnalysis[['ROSSResultUsing']]
        # PAPostProbD <- PAAnalysis[['PAPostProbD']]
        
        lowestSubtotal <- ROSSAnalysis[['ROSSMinSubtotal']]
        
        subtotalScores <- ROSSAnalysis[["ROSSSubtotalScores"]]
        subtotalScores <- c(subtotalScores, rep(NA, times=4-length(subtotalScores)))
        
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      # submit the case result to the summary data frame
      ROSSSummaryDF[i,] <- 
        c(examName, 
          testFormat,
          testResult,
          subtotalScores,
          grandTotal,
          resultUsing,
          lowestSubtotal,
          criterionState )
      
    } # end j loop over series
    
  } # end i loop over exams
  
  ## recode the ROSS result ##
  
  ROSSSummaryDF$recodeResult <- 
    ifelse(ROSSSummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(ROSSSummaryDF$testResult == "NDI/NSR",
                  1,
                  0))
  # View(ROSSSummaryDF)
  
  ## summarize the ROSS result ##
  
  # str(ROSSSummaryDF)
  ROSSSummaryDF$correctCode <- 
    apply(ROSSSummaryDF[,c((ncol(ROSSSummaryDF)-1),ncol(ROSSSummaryDF))],
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(ROSSSummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_ROSSSummary.csv") ) 
  
  # View(ROSSSummaryDF)
  
  # summarizeROSS <- FALSE
  
} # end if isTRUE(summarizeROSS)



#########  summarize the Permutation PSS results  #########



# summarizePermutation <- FALSE
summarizePermutation <- TRUE



if(isTRUE(summarizePermutation)) {
  
  print("summarize the PSSresults")
  
  # library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  # View(criterionStateDF)
  
  # names(criterionStateDF)[1] <- c("examName")
  
  # # set these for the test format
  # # testFormat <- "Utah"
  # # testFormat <- "FedZCT"
  # testFormat <- "AFMGQTLEPET"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  
  # RQNames <- c("R1", "R2", "R3", "R4")
  
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  PSSSummaryDF <- as.data.frame(matrix(ncol=(4+length(RQNames)), 
                                      nrow=length(analysisLists)))
  names(PSSSummaryDF) <- c("examName", 
                          "testFormat",
                          "testResult",
                          RQNames,
                          # "PAScore",
                          # "PAPostProbT",
                          # "PAPostProbD",
                          "criterionState" )
  # View(PSSSummaryDF)
  
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -10)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the PSS analysis result
        PSSAnalysis <- 
          thisAnalysis[[seriesName]][["PSSMOutput"]]
        
        if(is.null(PSSAnalysis)) next()
        
        # slice the result info for the case
        testResult <- PSSAnalysis[['PSSResult']]
        
        # RQNames <- PSSAnalysis[['PSSRQNames']]
        # RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
        
        # PAScore <- PAAnalysis[['PAScore']]
        # PAPostProbT <- PAAnalysis[['PAPostProbT']]
        # PAPostProbD <- PAAnalysis[['PAPostProbD']]
        
        
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      # submit the case result to the summary data frame
      PSSSummaryDF[i,] <- 
        c(examName, 
          testFormat,
          testResult,
          RQNames,
          # PAScore,
          # PAPostProbT,
          # PAPostProbD,
          criterionState )
      
    } # end j loop over series
    
  } # end i loop over exams
  
  ## recode the PSS result ##
  
  PSSSummaryDF$recodeResult <- 
    ifelse(PSSSummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(PSSSummaryDF$testResult == "NDI/NSR",
                  1,
                  0))
  # View(PSSSummaryDF)
  
  ## summarize the PSS result ##
  
  # str(PSSSummaryDF)
  PSSSummaryDF$correctCode <- 
    apply(PSSSummaryDF[,c((ncol(PSSSummaryDF)-1),ncol(PSSSummaryDF))],
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(PSSSummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_PSSSummary.csv") ) 
  
  # View(PSSSummaryDF)
  
  # summarizePermutation <- FALSE
  
} # end if isTRUE(summarizePermutation)




#########  summarize the Bootstrap  results  #########



summarizeBootstrap <- TRUE
# summarizeBootstrap <- FALSE



if(isTRUE(summarizeBootstrap)) {
  
  print("summarize the Bootstrap results")
  
  library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE) 
  }
  # View(criterionStateDF)
  
  # names(criterionStateDF)[1] <- c("examName")
  
  # # set these for the test format
  # # testFormat <- "Utah"
  # # testFormat <- "FedZCT"
  # testFormat <- "AFMGQTLEPET"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  
  # RQNames <- c("R1", "R2", "R3", "R4")
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  bootstrapSummaryDF <- as.data.frame(matrix(ncol=(6+length(RQNames)), 
                                      nrow=length(analysisLists)))
  names(bootstrapSummaryDF) <- c("examName", 
                                 "testFormat",
                                 "testResult",
                                 RQNames,
                                 "bootstrapPostProb",
                                 "bootstrapDiffScore",
                                 # "PAPostProbD",
                                 "criterionState" )
  # View(bootstrapSummaryDF)
  
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -10)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the Bootstrap analysis result
        bootstrapAnalysis <- 
          thisAnalysis[[seriesName]][["bootstrapOutput"]]
        
        if(is.null(bootstrapAnalysis)) next()
        
        # slice the result info for the case
        testResult <- bootstrapAnalysis[['bootstrapResult']]
        
        # RQNames <- bootstrapAnalysis[['bootstrapRQNames']]
        # RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
        
        bootstrapPostProb <- bootstrapAnalysis[['bootstrapPostProb']]
        bootstrapDiffScore <- bootstrapAnalysis[['bootstrapDiffScore']]
        # bootstrapPostProbD <- bootstrapAnalysis[['bootstrapPostProbD']]
        
        
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      # submit the case result to the summary data frame
      bootstrapSummaryDF[i,] <- 
        c(examName, 
          testFormat,
          testResult,
          RQNames,
          bootstrapPostProb,
          bootstrapDiffScore,
          # PAPostProbD,
          criterionState )
      
    } # end j loop over series
    
  } # end i loop over exams
  
  ## recode the bootstrap result ##
  
  bootstrapSummaryDF$recodeResult <- 
    ifelse(bootstrapSummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(bootstrapSummaryDF$testResult == "NDI/NSR",
                  1,
                  0))
  # View(bootstrapSummaryDF)
  
  ## summarize the bootstrap result ##
  
  # str(bootstrapSummaryDF)
  bootstrapSummaryDF$correctCode <- 
    apply(bootstrapSummaryDF[,c((ncol(bootstrapSummaryDF)-1),ncol(bootstrapSummaryDF))],
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(bootstrapSummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_bootstrapSummary.csv") ) 
  
  # View(bootstrapSummaryDF)
  
  # summarizeBootstrap <- FALSE
  
} # end if isTRUE(summarizeBootstrap)



#################  summarize the PCAT results  #################



summarizeLXCAT <- TRUE
# summarizePCAT <- FALSE


if(isTRUE(summarizeLXCAT)) {
  
  print("summarize the LXCAT algorithm results")
  
  library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE) }
  
  names(criterionStateDF)[1] <- c("examName")
  
  # View(criterionStateDF)
  
  # set these for the test format
  # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # testFormat <- "MGQT"
  
  # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  
  # RQNames <- c("R5", "R7", "R10")
  # RQNames <- c("R1", "R2", "R3", "R4")
  
  # RQNames <- PCATAnalysis[['PCATRQNames']]
  # RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
  
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  # sensorNames <- c("pneumo", "EDA", "cardio", "PLE")
  # if(!isTRUE(includePLEScores)) {
  #   sensorNames <- c("pneumo", "EDA", "cardio")
  # }
  # sensorNames <- c("pneumo", "EDA", "cardio")
  
  sensorNames <- c("EDA", "VM")
  
  numberCols <- length(RQNames) + length(sensorNames) + 16
  
  PCATSummaryDF <- as.data.frame(matrix(ncol=numberCols, 
                                        nrow=length(analysisLists)))
  names(PCATSummaryDF) <- c("examName", 
                            "testFormat",
                            "testResult",
                            "decisionRule",
                            RQNames,
                            # "questionResults",
                            "postOdds",
                            "postProb",
                            "BayesFactor",
                            # "postZSubtotals",
                            "minRQName",
                            "grandTotal",
                            "minRQSubtotal",
                            "subtotalMean",
                            "resultUsing",
                            "cutscore",
                            sensorNames,
                            "criterionState",
                            "recodeResult",
                            "correctCode" )
  # View(PCATSummaryDF)
  
  # iterate on the exams in the global env
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      # View(thisAnalysis)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -10)  
      # examName <- str_sub(analysisLists[i], 2, 8)  
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
      
      # next exam if there is no result
      if(is.null(thisAnalysis[[seriesName]][["PCATOutput"]][["PCATResult"]])) {
        next()
      }
      
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the PCAT analysis result
        PCATAnalysis <- 
          thisAnalysis[[seriesName]][["PCATOutput"]]
        
        if(is.null(PCATAnalysis)) next()
        
        if(PCATAnalysis[['PCATResult']] == "missing PCAT sensors") next()
        
        # slice the result info for the case
        testResult <- PCATAnalysis[['PCATResult']]
        
        decisionRule <- PCATAnalysis[['PCATDecisionRule']]
        
        # RQNames <- PCATAnalysis[['PCATRQNames']]
        RQNames <- c(RQNames, rep(NA, length=4-length(RQNames)))
        # RQNames <- c(RQNames, rep("none", times=4-length(RQNames)))
        
        subtotalScores <- PCATAnalysis[['PCATSubtotalScores']]
        subtotalScores <- c(subtotalScores, rep(NA, times=4-length(subtotalScores)))
        
        questionResults <- PCATAnalysis[['PCATQuestionResults']]
        # questionResults <- 
        #   c(questionResults, rep(NA, length=3-length(questionResults)))
        
        postOdds <- PCATAnalysis[['PCATPostOdds']]
        postProb <- PCATAnalysis[['PCATPostProb']]
        bayesFactor <- PCATAnalysis[['PCATBayesFactor']]
        
        grandTotal <- PCATAnalysis[['PCATGrandTotal']]
        
        minRQName <- PCATAnalysis[['PCATMinRQ']]
        minRQScore <- PCATAnalysis[['PCATMinSubtotal']]
        
        sensorTotalsDF <- PCATAnalysis[['PCATSensorTotalsDF']]
        sensorTotals <- colSums(sensorTotalsDF[,4:5])
        names(sensorTotals) <- c("EDA", "VM")
        
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      {
        
        cutScores <- PCATAnalysis[['PCATCutscores']]
        
        if(resultUsing=="grand total") {
          thisCutScore <- ifelse(testResult=="DI/SR",
                                 {cutScores['GTDI']},
                                 cutScores['GTNDI'] )
        } else if(resultUsing=="lowest subtotal") {
          thisCutScore <- ifelse(testResult=="DI/SR",
                                 cutScores['STDI'],
                                 cutScores['STNDIc'] )
        }
        names(thisCutScore) <- NULL
        
      }
      
      {
        
        # submit the case result to the summary data frame
        thisCase <- c(examName, 
                      testFormat,
                      testResult,
                      decisionRule,
                      subtotalScores,
                      # questionResults,
                      postOdds,
                      postProb,
                      bayesFactor,
                      minRQName, 
                      grandTotal,
                      minRQScore,
                      "NA",
                      resultUsing,
                      thisCutScore,
                      sensorTotals,
                      # sensorNames,
                      criterionState,
                      NA,
                      NA )
        
        PCATSummaryDF[i,] <- thisCase
        
      }
      
      # View(PCATSummaryDF)
      
    } # end j loop over series
    
  } # end i loop over exams
  
  PCATSummaryDF$criterionState <- as.numeric(PCATSummaryDF$criterionState)
  
  ## recode the PCAT result ##
  
  PCATSummaryDF$recodeResult <- 
    ifelse(PCATSummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(PCATSummaryDF$testResult == "NDI/NSR",
                  1,
                  0))
  
  ## subtotal mean ##
  
  PCATSummaryDF[,5] <- as.numeric(PCATSummaryDF[,5])
  PCATSummaryDF[,6] <- as.numeric(PCATSummaryDF[,6])
  PCATSummaryDF$subtotalMean <- 
    apply(PCATSummaryDF[,c(5,6)], 1, mean, na.rm=TRUE)
  
  ## summarize the PCAT result ##
  
  # str(PCATSummaryDF)
  
  PCATSummaryDF$correctCode <- 
    apply(PCATSummaryDF[,c(20,21)],
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(PCATSummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_PCATSummary.csv") ) 
  
  # summarizePCAT <- FALSE
  
  # View(PCATSummaryDF)
  
} # end if isTRUE(summarizePCAT)



############# summarize the ipsative-Z results ##################



summarizeIpZ <- TRUE
# summarizeIpZ <- FALSE



if(isTRUE(summarizeIpZ)) {
  
  print("summarize the ipsative-Z results")
  
  library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # exclude analysis of exams consisting of only ACQT charts
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  
  names(criterionStateDF)[1] <- c("examName")
  
  # View(criterionStateDF)
  
  # # set these for the test format
  # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # testFormat <- "MGQT"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  # RQNames <- c("R1", "R2", "R3", "R4")
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  if(!exists("testFormat")) testFormat <- "unk"
  
  # initialize a data frame
  ipsativeZSummaryDF <- as.data.frame(matrix(ncol=6, 
                                             nrow=length(analysisLists)))
  names(ipsativeZSummaryDF) <- c("examName", 
                                 "testFormat",
                                 "testResult",
                                 "maxQuestion",
                                 "maxMeanIpZ",
                                 "criterionState" )
  # View(ipsativeZSummaryDF)
  
  # ipsativeZAnalysisOutput
  
  # iterate on the exams in the global env
  i=1
  for(i in 1:length(analysisLists)) {
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -10)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
    }
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      {
        
        # get the ipsative Z analysis result
        ipZAnalysis <- 
          thisAnalysis[[seriesName]][["ipsativeZAnalysisOutput"]]
        
        # slice the result info for the case
        testResult <- ipZAnalysis[['testResult']]
        sigQuestion <- ipZAnalysis[['maxMeanIpZQuestion']]
        maxMeanIpZ <- ipZAnalysis[['maxMeanIpZ']]
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
        
      }
      
      # submit the case result to the summary data frame
      ipsativeZSummaryDF[i,] <- 
        c(examName, 
          testFormat,
          testResult, 
          sigQuestion, 
          maxMeanIpZ, 
          criterionState )
      
    } # end j loop over series
    
    # View(ipsativeZSummaryDF)
    
  } # end i loop over exams
  
  ## recode the ipsative-Z result ##
  
  ipsativeZSummaryDF$recodeResult <- 
    ifelse(ipsativeZSummaryDF$testResult == "DI/SR", 
           recodeResult <- -1,
           ifelse(ipsativeZSummaryDF$testResult == "NDI/NSR",
           1,
           0 ) )
  
  ## summarize the ipsative-Z result ##
  
  # str(PCASS2SummaryDF)
  ipsativeZSummaryDF$correctCode <- 
    apply(ipsativeZSummaryDF[,c(6,7)],
          1, correctCodesFN)
  
  # write the summary to the CWD
  write.csv(ipsativeZSummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_ipsativeZSummary.csv") ) 
  
  # summarizeIpZ <- FALSE
  
} # end if isTRUE(summarizeIpZ)



########### summarize the RC ratios ############



# summarizeRCRatios <- FALSE
summarizeRCRatios <- TRUE



if(isTRUE(summarizeRCRatios)) {
  
  print("summarize the R/C Ratios")
  
  library(stringr)
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  
  # analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  # get the criterion state for all exams
  if(!exists("criterionStateDF")) {
    criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"),
                                 header=TRUE,
                                 stringsAsFactors=FALSE)
  }
  
  names(criterionStateDF)[1] <- c("examName")
  
  # View(criterionStateDF)
  
  # # set these for the test format
  # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # testFormat <- "MGQT"
  # # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
  #                                        nrow=length(seriesTotalFiles)))
  # RQNames <- c("R1", "R2", "R3", "R4")
  # names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  
  # initialize a data frame
  RCWorkingDF <- as.data.frame(matrix(ncol=10, 
                                      nrow=1))
  names(RCWorkingDF) <- c("examName", 
                          "testFormat",
                          "testResult",
                          "criterionState",
                          "seriesName",
                          "chartName",
                          "sensorName",
                          "RQName",
                          "CQName",
                          "RCRatio" )
  # View(RCWorkingDF)
  
  # initialize the output data frame
  RCSummaryDF <- NULL
  
  # ESSMOutput
  
  # iterate on the exams in the global env
  i=1
  for(i in 1:length(analysisLists)) {
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      
      examName <- examNames[i]
      # examName <- str_sub(analysisLists[i], 2, -10)
      # examName <- str_sub(analysisLists[i], 2, 8)
      
      # Ohio
      # examName <- str_sub(examName, 1, -2)
      
      # examName <- str_sub(examName, 1, 3)
    }
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        # reduce the series name to a single character
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        # length(thisAnalysis[[seriesName]])
        # names(thisAnalysis[[seriesName]])
        
        RCRatioSeriesDF <- as.data.frame(matrix(ncol=10, 
                                                nrow=1))
        names(RCRatioSeriesDF) <- c("examName", 
                                    "testFormat",
                                    "testResult",
                                    "criterionState",
                                    "seriesName",
                                    "chartName",
                                    "sensorName",
                                    "RQName",
                                    "CQName",
                                    "RCRatio" )
      }
      
      {
        # get the ESS-M analysis output
        ESSMOutput <- 
          thisAnalysis[[seriesName]][["ESSMOutput"]]
        
        if(is.null(ESSMOutput)) next()
        
        # slice the result info for the case
        testResult <- ESSMOutput[['ESSMResult']]
        # get the criterion state
        criterionState <- 
          criterionStateDF$criterionState[criterionStateDF$examName == examName]
      }
      
      {
        # submit the case result to the summary data frame
        RCWorkingDF[1,c(1:5)] <- 
          c(examName, 
            testFormat,
            testResult, 
            criterionState,
            seriesNameB)
        # View(RCWorkingDF) 
        
        RCRatioSeriesDF[1,c(1:5)] <- 
          c(examName, 
            testFormat,
            testResult, 
            criterionState,
            seriesNameB)
      }
      
      {
        # View(thisAnalysis)
        # get the RC ratio data frame
        RCRatioDF <- 
          thisAnalysis[[seriesName]][["ESSMOutput"]][["ESSMLogRCRatios"]]
        # View(RCRatioDF)
        # get the CQ selection data frame
        # CQSelectionDF must be the same structure as RCRatioDF
        CQSelectionDF <- 
          thisAnalysis[[seriesName]][["ESSMOutput"]][["ESSMCQSelection"]]
        # View(CQSelectionDF)
      }
      
      {
        # iterate over the RC ratios and CQ selection
        k=1
        for(k in 1:nrow(RCRatioDF)) {
          chartName <- RCRatioDF$chartName[k]
          sensorName <- RCRatioDF$sensorName[k]
          l=5
          for(l in 5:ncol(RCRatioDF)) {
            RQName <- names(RCRatioDF)[l]
            logRCRatio <- RCRatioDF[k,l]
            # CQSelectionDF must be the same structure as RCRatioDF
            CQName <- CQSelectionDF[k,l]
            RCWorkingDF[1,6:ncol(RCWorkingDF)] <- c(chartName, 
                                                    sensorName, 
                                                    RQName, 
                                                    CQName, 
                                                    logRCRatio)
            # submit the ratio to the summary data frame
            RCSummaryDF <- rbind.data.frame(RCSummaryDF, RCWorkingDF)
          }
        }
        
      }
      
    } # end j loop over series
    
  } # end i loop over exams
  
  # we now have a skinny data frame of R/C ratios for all exams 
  # View(RCSummaryDF)
    
    
  
  {
    
    # aggregate the sensor ratios for each exam series
    
    uniqueExamRows <- 
      c(1, which(RCSummaryDF$examName[2:nrow(RCSummaryDF)] != 
                   RCSummaryDF$examName[1:(nrow(RCSummaryDF)-1)]))
    allExamNames <- RCSummaryDF$examName[uniqueExamRows]
    uniqueSensors <- unique(RCSummaryDF$sensorName)
    
    RCAggDF <- 
      as.data.frame(matrix(nrow=length(allExamNames), ncol=(5+length(uniqueSensors))))
    
    names(RCAggDF) <- c(names(RCSummaryDF[1:5]), uniqueSensors)
    
    i=1
    for(i in 1:length(allExamNames)) {
      examRows <- which(RCSummaryDF$examName == allExamNames[i])
      examDF <- RCSummaryDF[examRows,]
      j=1
      for(j in 1:length(uniqueSensors)) {
        sensorRows <- which(examDF$sensorName == uniqueSensors[j])
        RCAggDF[i,which(names(RCAggDF)==uniqueSensors[j])] <- 
          mean(as.numeric(examDF$RCRatio[sensorRows]), na.rm=TRUE)
        RCAggDF[i,(1:5)] <- examDF[1,1:5]
      }
    }
    
  }
    
  ## write the aggregated sensor data frame to the CWD  
  write.csv(RCAggDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_RCSensorAgg.csv") )
  
  
  ## write the summary to the CWD
  write.csv(RCSummaryDF, row.names=FALSE, 
            file=paste0("ALL_CASES_", 
                        length(analysisLists),
                        "_RCSummarySkiny.csv") ) 
  
  # summarizeRCRatios <- FALSE
  
} # end if isTRUE(summarizeRCRatios)



##########################################################################
##########################################################################



######### summarize the proportions of ESS-M scores #############



summarizeScoresProp <- TRUE


if(isTRUE(summarizeScoresProp)) {
  
  library(readr)
  
  ### use the sensorTotalsDF to summarize the scores
  
  # View(sensorTotalsDF)
  
  print("normalized proportions for absolute sensor sums")
  print(colSums(abs(sensorTotalsDF[,4:ncol(sensorTotalsDF)])) / 
          sum(colSums(abs(sensorTotalsDF[,4:ncol(sensorTotalsDF)]))) )
  
  ### use the score sheet data frames to summarize the frequency of scores
  
  # analysisLists <- ls(pattern =".ANALYSIS$")
  # 
  # # iterate over the analysisLists and save the ESSM scoreSheeets
  # if(length(analysisLists) > 0) {
  #   i=1
  #   for(i in 1:length(analysisLists)) {
  #     thisAnalysis <- get(analysisLists[i], envir=.GlobalEnv)
  #     # thisSeries <- seriesName 
  #     if(is.null(thisAnalysis)) {
  #       assign("thisAnalysis", thisAnalysis, envir=.GlobalEnv)
  #       stop()
  #     }
  #     thisSeries <- "series_X"
  #     # names(thisAnalysis[[thisSeries]][["ESSMOutput"]][["ESSMScoreSheetDF"]])
  #     thisScoreSheet <- 
  #       thisAnalysis[[thisSeries]][["ESSMOutput"]][["ESSMScoreSheetDF"]]
  #     scoreSheetName <- paste(thisScoreSheet$examName[1],
  #                             thisScoreSheet$seriesName[1],
  #                             "ESSMScoresheet.csv",
  #                             sep="_")
  #     write.csv(thisScoreSheet, row.names=FALSE, 
  #               file=scoreSheetName)
  #   }
  # }
  
  
  ### use the score sheet data frames to summarize the frequency of scores
  
  scoreSheetFiles <- list.files(pattern="ESSMScoresheet.csv")
  
  ### count the number of non-zero scores for each case
  
  # initialize a data frame
  scoreSheetFreqDF <- as.data.frame(matrix(ncol=11, 
                                           nrow=length(scoreSheetFiles)))
  names(scoreSheetFreqDF) <- c("ID", "Pneumo", "AutoEDA", "Cardio", "PLE",
                               "nPneumo", "nAutoEDA", "nCardio", "nPLE", "N",
                               "nScores")
  
  # iterate over each case
  
  theseScores <- c(-2, -1, 1, 2)
  
  i=1
  for(i in 1:length(scoreSheetFiles)) {
    
    # thisCaseDF <- read_csv(scoreSheetFiles[i],)
    thisCaseDF <- read.csv(scoreSheetFiles[i], stringsAsFactors=FALSE)
    
    RQs <- names(thisCaseDF[5:ncol(thisCaseDF)])
    
    ### pneumo
    
    pneumoRows <- which(thisCaseDF$sensorName == "Pneumo")
    nPneumoScores <- length(RQs) * length(pneumoRows)
    pneumoMatrix <- as.matrix(thisCaseDF[pneumoRows,5:(4+length(RQs))])  
    scoreSheetFreqDF[i,'nPneumo'] <- length(pneumoMatrix)
    scoreSheetFreqDF[i,'Pneumo'] <- 
      length(which(pneumoMatrix %in% theseScores))
    
    ### EDA
    
    edaRows <- which(thisCaseDF$sensorName == "AutoEDA")
    nEDAScores <- length(RQs) * length(edaRows)
    edaMatrix <- as.matrix(thisCaseDF[edaRows,5:(4+length(RQs))])  
    scoreSheetFreqDF[i,'nAutoEDA'] <- length(edaMatrix)
    scoreSheetFreqDF[i,'AutoEDA'] <- 
      length(which(edaMatrix %in% theseScores))
    
    ### Cardio
    
    cardioRows <- which(thisCaseDF$sensorName == "Cardio")
    nCardioScores <- length(RQs) * length(cardioRows)
    cardioMatrix <- as.matrix(thisCaseDF[cardioRows,5:(4+length(RQs))])  
    scoreSheetFreqDF[i,'nCardio'] <- length(cardioMatrix)
    scoreSheetFreqDF[i,'Cardio'] <- 
      length(which(cardioMatrix %in% theseScores))
    
    ### PLE
    
    pleRows <- which(thisCaseDF$sensorName == "PLE")
    nPLEScores <- length(RQs) * length(pleRows)
    pleMatrix <- as.matrix(thisCaseDF[pleRows,5:(4+length(RQs))])  
    scoreSheetFreqDF[i,'nPLE'] <- length(pleMatrix)
    scoreSheetFreqDF[i,'PLE'] <- 
      length(which(pleMatrix %in% theseScores))
    
    # number of scores
    scoreSheetFreqDF[i,'N'] <- sum(nPneumoScores, nEDAScores, nCardioScores, nPLEScores)
    scoreSheetFreqDF[i,'nScores'] <- sum(scoreSheetFreqDF[i,'Pneumo'],
                                         scoreSheetFreqDF[i,'AutoEDA'],
                                         scoreSheetFreqDF[i,'Cardio'],
                                         scoreSheetFreqDF[i,'PLE']
                                         )
    
  } # end for loop i
  
  outputVector <- 
    colSums(scoreSheetFreqDF[,2:5]) / colSums(scoreSheetFreqDF)['nScores']
  
  # View(scoreSheetFreqDF)
  
  # sum(outputVector)
  
  print("normalized proportion of non-zero scores")
  print(outputVector)
  
  print("proportion of ESS scores")
  print(outputVector * c(1, 2, 1, 1))
  
  print("normalized ESS score proportions")
  print(outputVector * c(1, 2, 1, 1) / sum(outputVector * c(1, 2, 1, 1)))
  
  summarizeScoresProp <- FALSE
  
}



######## summarize the correlations of ESS-M scores ########



getCorrelations <- TRUE
# getCorrelations <- FALSE

totalCases <- nrow(criterionStateDF)

truthfulCases <- length(which(criterionStateDF$criterionState == 1))
deceptiveCases <- length(which(criterionStateDF$criterionState == -1))

if(all(getCorrelations, truthfulCases != totalCases, deceptiveCases != totalCases)) {
  
  # numbCases <- nrow(criterionStateDF)
  numbCases <- nrow(scoreSheetFreqDF)
  
  ALL_CASES_sensorTotals <- read_csv(paste0("ALL_CASES_",
                                            numbCases, 
                                            "_sensorTotals.csv"))
  
  # View(ALL_CASES_sensorTotals)
  
  pneumoCor <- cor(ALL_CASES_sensorTotals$Pneumo, 
                   ALL_CASES_sensorTotals$criterionState)
  
  EDACor <- cor(ALL_CASES_sensorTotals$EDA, 
                ALL_CASES_sensorTotals$criterionState)
  
  cardioCor <- cor(ALL_CASES_sensorTotals$Cardio, 
                   ALL_CASES_sensorTotals$criterionState)
  
  if(sum(ALL_CASES_sensorTotals$PLE) != 0) {
  PLECor <- cor(ALL_CASES_sensorTotals$PLE, 
                ALL_CASES_sensorTotals$criterionState)
  } else {
    PLECor <- PLECor <- 0
  }
  
  totalCor <- cor(ALL_CASES_sensorTotals$grandTotal,
                  ALL_CASES_sensorTotals$criterionState)
  
  DEC <- cor(ALL_CASES_sensorTotals$Result, 
             ALL_CASES_sensorTotals$criterionState)
  
  print(paste0("Pneumo r: ", pneumoCor))
  print(paste0("   EDA r: ", EDACor))
  print(paste0("Cardio r: ", cardioCor))
  print(paste0("   PLE r: ", PLECor))
  
  print(paste0(" Total r: ", totalCor))
  
  print(paste0("     DEC: ", DEC))
  
  getCorrelations <- FALSE
  
  # setwd("./test")
  
}




#############################################

######### COPY ESS-M problem exams ##########



copyProblems <- FALSE
# copyProblems <- TRUE


# checkThese <- c("7PAIOF", "S61WS", "KIVNQ", "VTASKL", "T949P", "QC3K4", "PM578T", "0T8GP", "ABAMMV", "CIVSL")
# checkThese <- c("ABAMMV", "0T8GP", "S61WS", "QC3K4", "PM578T", "RD46WK", "F9J0D", "VTASKL", "T949P", "7FSAGH", "FSGN3", "R593D", "9N0C64", "K66V9O")

# 7Q6QL

if(isTRUE(copyProblems)) {
  
  # library(readr)
  
  list.files("./test", full.names=TRUE)
  
  # remove all files from the ./test directory
  file.remove(list.files("./test", full.names=TRUE))
  
  # move error and inc cases to a folder 
  
  # seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(3)]
  # seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(4)]
  # seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(5)]
  # seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(6)]
  
  
  
  theseProblems <- c(3, 5)
  theseProblems <- c(4, 6)
  theseProblems <- c(3)
  theseProblems <- c(4)
  theseProblems <- c(5)
  theseProblems <- c(6)
  
  # ESS-M
  problemCaseList0 <-
    seriesTotalsDF$examName[seriesTotalsDF$correctCode %in% theseProblems]
  
  # with PLE
  problemCaseListP <-
    seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% theseProblems]
  
  problemCaseListO3 <-
    OSS3SummaryDF$examName[OSS3SummaryDF$correctCode %in% theseProblems]

  problemCaseListO2 <-
    OSS2SummaryDF$examName[OSS2SummaryDF$correctCode %in% theseProblems]

  problemCaseListA <-
    PASummaryDF$examName[PASummaryDF$correctCode %in% theseProblems]

  problemCaseListB <-
    bootstrapSummaryDF$examName[bootstrapSummaryDF$correctCode %in% theseProblems]

  problemCaseListS <-
    PSSSummaryDF$examName[PSSSummaryDF$correctCode %in% theseProblems]

  problemCaseListI <-
    ipsativeZSummaryDF$examName[ipsativeZSummaryDF$correctCode %in% theseProblems]

  problemCaseListR <-
    ROSSSummaryDF$examName[ROSSSummaryDF$correctCode %in% theseProblems]
  
  problemCaseListX <-
    PCATSummaryDF$examName[PCATSummaryDF$correctCode %in% theseProblems]
  
  problemCaseList <- problemCaseListO3
  
  problemCaseList <- 
    sort(unique(c(problemCaseList0,
                  problemCaseListO3,
                  problemCaseListO2,
                  problemCaseListA,
                  problemCaseListB,
                  problemCaseListS,
                  problemCaseListI,
                  problemCaseListR,
                  problemCaseListX,
                  problemCaseListP
                  )))
  
  problemCaseList <- c("C93336", "8AIL83", "8A30BC")
  problemCaseList <- c(c("7Q6QL"))
  problemCaseList <- c("7V86C", "CLWX3X") # ROSS and PSS inconclusives
  
  # problemCaseList <- sort(unique(c(problemCaseList, problemCaseListP)))
  
  # problemCaseList <- checkThese
  
  # PCASS2SummaryDF <- read_csv("~/Dropbox/R/NCCA_ASCII_Parse/data/Ohio_PPG_data/Ohio_NCCAASCII/ALL_CASES_40_PCASS2Summary.csv")
  
  # PCASS-2
  # problemCaseList <- 
  #   PCASS2SummaryDF$examName[PCASS2SummaryDF$correctCode %in% c(4)]
  
  # make a list of exams in the cwd using the chartPlot.pdf files
  examList <- str_sub(list.files(pattern="chartPlot.pdf"), 2, -15)
    
  problemCaseList %in% examList
  
  
  # 
  # if(!dir.exists("check8")) {
  #   
  #   dir.create("check8")
  #   
  # }
  
  # change the working dir to the target location
  
  if(!dir.exists("./test/")) dir.create("./test/")
    
  list.files(".", pattern="criterionState.csv")
  
  file.copy(list.files(".", pattern="criterionState.csv"), "./test/criterionState.csv", overwrite=TRUE)
    
  if(length(problemCaseList %in% examList) > 0) {
    
    i=1
    for(i in 1:length(problemCaseList)) {
      theseFiles <- list.files(pattern=problemCaseList[i])
      file.copy(theseFiles, paste0("./test/", theseFiles))
    }
    
  }
  
  # i=1
  # for(i in 1:length(examList)) {
  #   theseFiles <- list.files(path="../", pattern=examList[i])
  #   file.copy(paste0("../", theseFiles), theseFiles)
  # }
  
  copyProblems <- FALSE
  
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF/problems")
  
  # setwd("./problems")
  
}



########################################################

######## get ESS-M EDA and cardio measurements #########



# getEDACardioMeasurement <- TRUE
getEDACardioMeasurement <- FALSE



if(isTRUE(getEDACardioMeasurement)) {
  
  ### Aug 2020, used to compare complex and simple feature extraction ###
  
  examList <- list.files(pattern="measurementTable.csv")
    
  examNames <- str_sub(examList, 2, -24)
  

  # initialize a data frame for EDA measurements 
  EDAMeasurementsDF <- as.data.frame(matrix(ncol=18, 
                                         nrow=length(examNames)))
  names(EDAMeasurementsDF) <- paste0(rep(c(1:3), each=6), c("C4", "R5", "C6", "R7", "C9", "R10"))
  EDAMeasurementsDF <- cbind(examNames=NA, EDAMeasurementsDF)
     
  # initialize a data frame for Cardio measurements 
  cardioMeasurementsDF <- as.data.frame(matrix(ncol=18, 
                                            nrow=length(examNames)))
  names(cardioMeasurementsDF) <- paste0(rep(c(1:3), each=6), c("C4", "R5", "C6", "R7", "C9", "R10"))
  cardioMeasurementsDF <- cbind(examNames=NA, cardioMeasurementsDF)
  
  
  # iterate over the exams
  
  i=1
  for(i in 1:length(examList)) {
    
    mTable <- read.csv(examList[i])
    
    ## locate and iterate over the EDA rows ##
    
    EDARows <- which(mTable$sensorName == "AutoEDA")
    
    EDAVector <- NULL
    j=1
    for(j in 1:length(EDARows)) {
      EDAVector <- c(EDAVector, unlist(mTable[EDARows[j],3:8]))
    }
    names(EDAVector) <- NULL
    
    EDAMeasurementsDF[i,] <- c(examNames[i], EDAVector)
    # View(EDAMeasurementsDF)
    
    ## locate and iterate over the cardio rows ##
    
    cardioRows <- which(mTable$sensorName == "Cardio")
    
    cardioVector <- NULL
    k=1
    for(k in 1:length(cardioRows)) {
      cardioVector <- c(cardioVector, unlist(mTable[cardioRows[k],3:8]))
    }
    names(cardioVector) <- NULL
    
    cardioMeasurementsDF[i,] <- c(examNames[i], cardioVector)
    # View(cardioMeasurementsDF)
    
    
    
  } # end iterate over i exams
  
  ## mean replacement ##
  
  meanReplaceFn <- function(x) {
    meanVal <- round(mean(as.numeric(unlist(x)), na.rm=TRUE), 2)
    x[which(is.na(x))] <- meanVal
    return(x)
  }
  
  # str(cardioMeasurementsDF)
  # mean(as.numeric(unlist(cardioMeasurementsDF[1,2:31])), na.rm=TRUE)
  
  cardioMeasurementsDF[,2:18] <- 
    apply(cardioMeasurementsDF[,2:18], 1, meanReplaceFn)
  
  EDAMeasurementsDF[,2:18] <- 
    apply(EDAMeasurementsDF[,2:18], 1, meanReplaceFn)
  
  
  write.csv(EDAMeasurementsDF, row.names=FALSE, 
            file=paste("ALL_CASES", length(examNames), "EDAMeasurements_laf18.csv", sep="_") )
  
  write.csv(cardioMeasurementsDF, row.names=FALSE, 
            file=paste("ALL_CASES", length(examNames), "cardioMeasurements_simple.csv", sep="_") )
  
  
  getEDACardioMeasurement <- FALSE
  
} # end if isTRUE getEDACardioMeasurement



################################################################

########### summarize the accuracy of ESS-M results without PLE ############



summarizeAccuracy <- TRUE



if(isTRUE(summarizeAccuracy)) {
  
  # seriesTotalsDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_seriesTotals.csv")))
  
  seriesTotalsDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_seriesTotals.csv")),
             stringsAsFactors = FALSE)
  # View(seriesTotalsDF)
  
  # call the function
  ESSMAccuracySummary <- accySumFn(x=seriesTotalsDF$correctCode)
  
  # summarizeAccuracy <- FALSE
  
  print(ESSMAccuracySummary)
  
  print(paste("PLE:", withPLE))

} 



########### summarize the accuracy of ESS-M results with PLE ############



summarizeAccuracy <- TRUE



if(isTRUE(summarizeAccuracy)) {
  
  # seriesTotalsPDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_seriesTotalsP.csv")))
  
  seriesTotalsPDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_seriesTotalsP.csv")),
              stringsAsFactors = FALSE)
  
  # View(seriesTotalsPDF)
  
  # call the function
  ESSMPAccuracySummary <- accySumFn(x=seriesTotalsPDF$correctCode)
  
  # summarizeAccuracy <- FALSE
  
  print(ESSMPAccuracySummary)
  
  print(paste("PLE:", "TRUE"))
  
} 



########### summarize the accuracy of OSS-3 results ############



summarizeOSS3 <- TRUE

if(summarizeAccuracy && summarizeOSS3) {
  
  # OSS3SummaryDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_OSS3Summary.csv")))
  
  OSS3SummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_OSS3Summary.csv")),
             stringsAsFactors = FALSE)
  
  # call the function
  OSS3AccuracySummary <- accySumFn(x=OSS3SummaryDF$correctCode)
  
  # summarizeOSS3 <- FALSE
  
  print(OSS3AccuracySummary)
  
  print("OSS-3")
  
}



########### summarize the accuracy of OSS-2 results ############



summarizeOSS2 <- TRUE
# summarizeAccuracy <- FALSE

if(summarizeAccuracy && summarizeOSS2) {

  # OSS2SummaryDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_OSS2Summary.csv")))
  
  OSS2SummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_OSS2Summary.csv")),
             stringsAsFactors = FALSE)
  
  # call the function
  OSS2AccuracySummary <- accySumFn(x=OSS2SummaryDF$correctCode)
  
  print(OSS2AccuracySummary)
  
  # summarizeOSS2 <- FALSE
  
  print("OSS-2")
  
}



########### summarize the accuracy of PA results ############



summarizePA <- TRUE
# summarizeAccuracy <- FALSE

if(summarizeAccuracy && summarizePA) {
  
  # PASummaryDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_PASummary.csv")))
  
  PASummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_PASummary.csv")),
             stringsAsFactors = FALSE)
  # call the function
  PAAccuracySummary <- accySumFn(x=PASummaryDF$correctCode)
  
  print(PAAccuracySummary)
  
  # summarizePA <- FALSE
  
  print("Probability Analysis")
  
}



########### summarize the accuracy of ROSS results ############



summarizeROSS <- TRUE
# summarizeAccuracy <- TRUE

if(summarizeAccuracy && summarizeROSS) {
  
  # ROSSSummaryDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_ROSSSummary.csv")))
  
  ROSSSummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_ROSSSummary.csv")),
             stringsAsFactors = FALSE)
  
  # View(ROSSSummaryDF)
  # call the function
  ROSSAccuracySummary <- accySumFn(x=ROSSSummaryDF$correctCode)
  
  print(ROSSAccuracySummary)
  
  # summarizeROSS <- FALSE
  
  print("ROSS")
  
}




########### summarize the accuracy of Bootstrap results ############



summarizeBootstrap <- TRUE
# summarizeAccuracy <- TRUE

if(summarizeAccuracy && summarizeBootstrap) {
  
  # bootstrapSummaryDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_bootstrapSummary.csv")))
  
  bootstrapSummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_bootstrapSummary.csv")),
             stringsAsFactors = FALSE)
  
  # call the function
  bootstrapAccuracySummary <- accySumFn(x=bootstrapSummaryDF$correctCode)
  
  print(bootstrapAccuracySummary)
  
  # summarizeBootstrap <- FALSE
  
  print("Bootstrap")
  
}



########### summarize the accuracy of Permutation results ############



summarizePermutation <- TRUE
# summarizeAccuracy <- TRUE

if(summarizeAccuracy && summarizePermutation) {
  
  # PSSSummaryDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_PSSSummary.csv")))
  
  PSSSummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_PSSSummary.csv")),
             stringsAsFactors = FALSE)
  
  # call the function
  PSSAccuracySummary <- accySumFn(x=PSSSummaryDF$correctCode)
  
  print(PSSAccuracySummary)
  
  # summarizePermutation <- FALSE
  
  print("Permutation")
  
}



########### summarize the accuracy of ipsative-Z results ############


summarizeIpZ <- TRUE
# summarizeAccuracy <- FALSE

if(summarizeAccuracy && summarizeIpZ) {
  
  # ipsativeZSummaryDF <- 
  #   read_csv(list.files(pattern=paste0(numbCases, "_ipsativeZSummary.csv")))
  
  ipsativeZSummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_ipsativeZSummary.csv")),
             stringsAsFactors = FALSE)
  
  # call the function
  ipZAccuracySummary <- accySumFn(x=ipsativeZSummaryDF$correctCode)
  
  print(ipZAccuracySummary)
  
  # summarizeAccuracy <- FALSE
  
  print("Ipsative Z")
  
}



########### summarize the accuracy of LXCAT results ############



summarizeLXCAT <- TRUE
# summarizeAccuracy <- FALSE

if(all(summarizeAccuracy, exists("PCATSummaryDF"), summarizeLXCAT)) {
  
  PCATSummaryDF <- 
    read.csv(list.files(pattern=paste0(numbCases, "_PCATSummary.csv")),
             stringsAsFactors = FALSE)
  
  # call the function
  PCATAccuracySummary <- accySumFn(x=PCATSummaryDF$correctCode)
  
  print(PCATAccuracySummary)
  
  # summarizePCAT <- FALSE
  
  print("PCAT")
  
}



##################################################################

############ aggregate the output summaries #############

aggregateOutputSummaries <- FALSE
aggregateOutputSummaries <- TRUE

if(aggregateOutputSummaries) {
  
  # View(criterionStateDF)
  
  theseCaseRows <- which(criterionStateDF$examName %in% examNames)
  
  aggSummaryDF <- cbind(criterionStateDF$examName[theseCaseRows], 
                        criterionStateDF$criterionState[theseCaseRows] )
                        
  aggSummaryDF <- cbind(aggSummaryDF, ESSMSummaryDF[,c(13,14,8,11)])
  
  aggSummaryDF <- cbind(aggSummaryDF, ESSMPSummaryDF[,c(13,14,8,11)])
  
  aggSummaryDF <- cbind(aggSummaryDF, OSS3SummaryDF[,c(16,17)])
  
  aggSummaryDF <- cbind(aggSummaryDF, PASummaryDF[,c(12,13)])
  
  aggSummaryDF <- cbind(aggSummaryDF, OSS2SummaryDF[,c(6,7)])
  
  aggSummaryDF <- cbind(aggSummaryDF, ROSSSummaryDF[,c(12,13)])
  
  aggSummaryDF <- cbind(aggSummaryDF, bootstrapSummaryDF[,c(11,12)])
  
  aggSummaryDF <- cbind(aggSummaryDF, PSSSummaryDF[,c(9,10)])
  
  aggSummaryDF <- cbind(aggSummaryDF, ipsativeZSummaryDF[,c(7,8)])
  
  aggSummaryDF <- cbind(aggSummaryDF, PCATSummaryDF[,c(21,22,13,14)])
  
  # View(aggSummaryDF)
  
  aggSummaryDF$unifResult <- apply(aggSummaryDF[,c(3,7,11,13,15,17,19,21,23)], 1, sum, na.rm=TRUE)
  # aggSummaryDF$unifResult <- apply(aggSummaryDF[,c(3,7,11,13,15,17,19,21,23,25)], 1, sum, na.rm=TRUE)
  
  # aggSummaryDF$unifResult <- ifelse(aggSummaryDF$unifResult==10, "TRUE",
  #                                ifelse(aggSummaryDF$unifResult==-10,"FALSE","_"))

  # excluding LXCAT
  aggSummaryDF$uniformResult <- ifelse(aggSummaryDF$unifResult==9, "TRUE",
                                 ifelse(aggSummaryDF$unifResult==-9,"FALSE","_"))
  
  names(aggSummaryDF) <- c("examName",
                           "criterionState", 
                           "ESSM",
                           "correctCode",	
                           "grandTotal",	
                           "minSubTotal",
                           "ESSM-P",	
                           "correctCode",	
                           "grandTotal",
                           "minSubTotal",
                           "OSS-3",	
                           "correctCode",	
                           "PA",	
                           "correctCode",	
                           "OSS2",	
                           "correctCode",	
                           "ROSS", 
                           "correctCode",	
                           "BOOT",	
                           "correctCode",	
                           "PSS",	
                           "correctCode",	
                           "ipZ",	
                           "correctCode",	
                           "LXCAT",	
                           "correctCode",	
                           "grandTotal",
                           "minRQSubtotal",
                           "algSum",
                           "uniformResult" )
  
  
  
  write_csv(aggSummaryDF, "aggSummary.csv")
  
  aggregateOutputSummaries <- FALSE
  
}



 ##################################################################



#### without PLE

print(paste("PLE:", withPLE))

seriesTotalsDF$examName[seriesTotalsDF$correctCode %in% c(3)]
# [1] "PM578T" "SCI3H5"
# [1] "59W256" "DDV3O" # army bizone

seriesTotalsDF$examName[seriesTotalsDF$correctCode %in% c(5)]
# [1] "B9H3VB" "UNRU42"
# [1] "GVGX1"

seriesTotalsDF$examName[seriesTotalsDF$correctCode %in% c(4)]
# [1] "7FSAGH" "S61WS"  "XDSP42"
# [1] "4BZ2Z5"

seriesTotalsDF$examName[seriesTotalsDF$correctCode %in% c(6)]
# [1] "CIVSL"  "FSGN3"  "SOCZJ8" "VTASKL"
# [1] "7JQCNA"


# problemCaseList <- c("6YCHNU", "7BHJU", "7U3XVI", "8D6MH", "8VQD0", "7DQ5P0", "7PLT4I", "8A30BC", "8AIL83", "ABB80")


#### with PLE 

print("PLE: TRUE")

seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(3)]
# [1] "SCI3H5"
# [1] "59W256" "DDV3O"

seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(5)]
# [1] "B9H3VB" "PM578T" "UNRU42"
# [1] "GVGX1"

seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(4)]
# [1] "7FSAGH" "CIVSL" 
# [1] "4BZ2Z5"

seriesTotalsPDF$examName[seriesTotalsPDF$correctCode %in% c(6)]
# [1] "FSGN3"  "IY72MA" "R593D"  "SOCZJ8" "XDSP42"
# [1] "4BZ2Z5"




#### OSS-3

print("OSS-3")

OSS3SummaryDF$examName[OSS3SummaryDF$correctCode %in% c(3)]
# [1] "UNRU42" 
# [1] "EHATQJHN" "SQFVTCBB"

OSS3SummaryDF$examName[OSS3SummaryDF$correctCode %in% c(5)]
# [1] "62OUMM" "K66V9O" "RD46WK" "SCI3H5"
# [1] "JAUNEDOA" "SFQMLVIX"

OSS3SummaryDF$examName[OSS3SummaryDF$correctCode %in% c(4)]
# [1] "CIVSL"  "VTASKL"
# [1] "OEWTJWDE" "QWBXYHZQ" "RRHAVLSM"

OSS3SummaryDF$examName[OSS3SummaryDF$correctCode %in% c(6)]
# [1] "S61WS"  "T949P"  "XDSP42"
# [1] "CHDCSTZL" "PKPOWEOS" "YLUVAAYO"



#### OSS-2

print("OSS-2")

OSS2SummaryDF$examName[OSS2SummaryDF$correctCode %in% c(3)]
# [1] "UNRU42"
# [1] "DDV3O" Army MGQT

OSS2SummaryDF$examName[OSS2SummaryDF$correctCode %in% c(5)]
# [1] "9N0C64" "B9H3VB" "RD46WK" "SCI3H5"
# [1] "59W256" BiZone NSPA

OSS2SummaryDF$examName[OSS2SummaryDF$correctCode %in% c(4)]
# [1] "FSGN3"  "VTASKL"

OSS2SummaryDF$examName[OSS2SummaryDF$correctCode %in% c(6)]
# [1] "7FSAGH" "CIVSL"  "R593D"  "XDSP42"



#### PA

print("PA")

PASummaryDF$examName[PASummaryDF$correctCode %in% c(3)]
# [1] "49941O" "SCI3H5" "UNRU42"
# [1] "DDV3O" Army MGQT

PASummaryDF$examName[PASummaryDF$correctCode %in% c(5)]
# [1] "62OUMM" "B9H3VB"
# [1] "59W256" BiZone NSPA

PASummaryDF$examName[PASummaryDF$correctCode %in% c(4)]
# [1] "CIVSL"  "T949P"  "VTASKL"

PASummaryDF$examName[PASummaryDF$correctCode %in% c(6)]
# [1] "0T8GP"  "FSGN3"  "S61WS"  "XDSP42"



#### bootstrap


print("Bootstrap")

bootstrapSummaryDF$examName[bootstrapSummaryDF$correctCode %in% c(3)]
# [1] "UNRU42"

bootstrapSummaryDF$examName[bootstrapSummaryDF$correctCode %in% c(5)]
# [1] "49941O" "9N0C64" "RD46WK" "SCI3H5"

bootstrapSummaryDF$examName[bootstrapSummaryDF$correctCode %in% c(4)]
# [1] "CIVSL" "FSGN3" "R593D" "S61WS" "T949P"

bootstrapSummaryDF$examName[bootstrapSummaryDF$correctCode %in% c(6)]

# [1] "0T8GP"  "G8NIDI" "IY72MA" "KIVNQ"  "VTASKL" "XDSP42" "YTII5X"
# [1] "7JQCNA" # FZCT


#### ROSS


print("ROSS")

ROSSSummaryDF$examName[ROSSSummaryDF$correctCode %in% c(3)]

ROSSSummaryDF$examName[ROSSSummaryDF$correctCode %in% c(5)]
# [1] "9N0C64" "B9H3VB" "RD46WK" "SCI3H5"

ROSSSummaryDF$examName[ROSSSummaryDF$correctCode %in% c(4)]
# [1] "0T8GP"  "4R7ZE"  "7PAIOF" "CIVSL"  "FSGN3"  "IY72MA" "KNDBCW" "R593D"
# [9] "VTASKL" "XDSP42" "YTII5X"
# [1] "7JQCNA" "7RJ4TO" # both FZCT

ROSSSummaryDF$examName[ROSSSummaryDF$correctCode %in% c(6)]
# [1] "0T0UN8" "1M7TB"  "5THPCC" "7FSAGH" "9BH487" "ABAMMV" "G8NIDI" "KIVNQ"
# [9] "PUDKF"  "Q9BIG2" "S61WS"  "SOCZJ8" "T949P"  "WQXEM"  "X2VONG" "ZKM1DF"
# [1] "4BZ2Z5" "4UZTJ"  "5H6APA" "6PVMB"  "8TUETP" "ARDDK3" "CQRF86" "DB629G"
#      FZCT  2AFMGQT3rq  FZCTsky   FZCT     bizone  FZCT    FZCT      FZCT


#### PSS (permutation)


print("Permutation")

PSSSummaryDF$examName[PSSSummaryDF$correctCode %in% c(3)]
# [1] "9N0C64" "RD46WK" "SCI3H5" "UNRU42"
# [1] "DDV3O" Army MGQT

PSSSummaryDF$examName[PSSSummaryDF$correctCode %in% c(5)]
# character(0)
# [1] "2RWSXD" # FZCT 

PSSSummaryDF$examName[PSSSummaryDF$correctCode %in% c(4)]
# [1] "0T0UN8" "CIVSL"  "FSGN3"  "TNTJS9" "VTASKL" "Z68UFS"
# [1] "3Q0GA"  "4UA11P" "7ZPFUC" "908AQX"
#      FZCT  2AFMGQT3rq  FZCT    FZCT

PSSSummaryDF$examName[PSSSummaryDF$correctCode %in% c(6)]
# [1] "7FSAGH" "XDSP42"


#### Ipsative Z

print("Ipsative Z")

ipsativeZSummaryDF$examName[ipsativeZSummaryDF$correctCode %in% c(3)]
# [1] "9N0C64" "RD46WK" "SCI3H5" "UNRU42"
# [1] "DDV3O" Army MGQT

ipsativeZSummaryDF$examName[ipsativeZSummaryDF$correctCode %in% c(5)]
# character(0)
# [1] "2RWSXD" # FZCT 

ipsativeZSummaryDF$examName[ipsativeZSummaryDF$correctCode %in% c(4)]
# [1] "0T0UN8" "CIVSL"  "FSGN3"  "TNTJS9" "VTASKL" "Z68UFS"
# [1] "3Q0GA"  "4UA11P" "7ZPFUC" "908AQX"
#      FZCT  2AFMGQT3rq  FZCT    FZCT

ipsativeZSummaryDF$examName[ipsativeZSummaryDF$correctCode %in% c(6)]
# [1] "7FSAGH" "XDSP42"


  
#### PCAT



print("PCAT")

PCATSummaryDF$examName[PCATSummaryDF$correctCode %in% c(3)]
# [1] "9N0C64" "RD46WK" "SCI3H5" "UNRU42"
# [1] "DDV3O" Army MGQT

PCATSummaryDF$examName[PCATSummaryDF$correctCode %in% c(5)]
# character(0)
# [1] "2RWSXD" # FZCT 

PCATSummaryDF$examName[PCATSummaryDF$correctCode %in% c(4)]
# [1] "0T0UN8" "CIVSL"  "FSGN3"  "TNTJS9" "VTASKL" "Z68UFS"
# [1] "3Q0GA"  "4UA11P" "7ZPFUC" "908AQX"
#      FZCT  2AFMGQT3rq  FZCT    FZCT

PCATSummaryDF$examName[PCATSummaryDF$correctCode %in% c(6)]
# [1] "7FSAGH" "XDSP42"





#############################
#############################

# 11/23/2021
# YTII5X UNRU42


# check these 11/21/2021 
# "SCI3H5" "RD46WK" "CIVSL" "0T8GP"

# check these 11/21/2021
# "7FSAGH" "T949P" "0T8GP" "CIVSL"

# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")
# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF")
# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF/problems")




########################


############# function to calculat the accuracy summary ###############



accyFn <- function(ng, ni, fn, fp, incg, inci) {
  
  N <- ng + ni
  IncG <- incg / ng
  IncI <- inci / ni
  TP <- ng - fn - incg
  TN <- ni - fp - inci
  CorG <- TP / (ng - incg)
  CorI <- TN / (ni - inci)
  Cor <- (TP + TN) / (N - (incg + inci))
  Inc <- (incg + inci) / N
  UnwghtAccy <- mean(c(CorG, CorI))
  UnwghtInc <- mean(c(IncG, IncI))
  PPV <- TP / (TP + fp)
  NPV <- TN / (TN + fn)
  FNI <- fn / (fn + TN)
  FPI <- fp / (fp + TP)
  
  return(c(N=N, 
           nG=ng, 
           nI=ni, 
           TP=TP/ng, 
           TN=TN/ni, 
           FN=fn/ng, 
           FP=fp/ni, 
           IncG=IncG, 
           IncI=IncI, 
           Cor=Cor, 
           Inc=Inc, 
           CorG=CorG, 
           CorI=CorI, 
           UnwghtAccy=UnwghtAccy, 
           UnwghtInc=UnwghtInc, 
           PPV=PPV, 
           NPV=NPV, 
           FNI=FNI, 
           FPI=FPI))
}



####################################

####################################



# accyFn(ng=30, ni=30, fn=1, fp=4, incg=2, inci=3)
# 
# accyFn(ng=30, ni=30, fn=1, fp=4, incg=2, inci=2)
# 
# accyFn(ng=30, ni=30, fn=1, fp=3, incg=2, inci=4)
# 
# accyFn(ng=30, ni=30, fn=1, fp=6, incg=1, inci=4)


# accyFn(ng=150, ni=150, fn=7, fp=8, incg=12, inci=15)

# # Oss-3
# accyFn(ng=30, ni=30, fn=2, incg=3, fp=3, inci=4)
# 
# # ESSM-P
# accyFn(ng=30, ni=30, fn=2, incg=3, fp=2, inci=4)
# 
# # ESSM
# accyFn(ng=30, ni=30, fn=2, incg=3, fp=1, inci=4)
# 
# 
# # ESSM-Px
# accyFn(ng=30, ni=30, fn=2, incg=2, fp=4, inci=3)
# 
# # ESSMx
# accyFn(ng=30, ni=30, fn=2, incg=2, fp=4, inci=4)
# 
# # OSS3x
# accyFn(ng=30, ni=30, fn=3, incg=2, fp=3, inci=4)


# accyFn(ng=60, ni=60, fn=5, incg=6, fp=6, inci=7)

# BOST 20220707
# > accyFn(ng=60, ni=60, fn=4, fp=5, incg=10, inci=13)
# N           nG           nI           TP           TN           FN           FP 
# 120.00000000  60.00000000  60.00000000   0.76666667   0.70000000   0.06666667   0.08333333 
# IncG         IncI          Cor          Inc         CorG         CorI   UnwghtAccy 
# 0.16666667   0.21666667   0.90721649   0.19166667   0.92000000   0.89361702   0.90680851 
# UnwghtInc          PPV          NPV          FNI          FPI 
# 0.19166667   0.90196078   0.91304348   0.08695652   0.09803922 
# > 


# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF/problems")


###### end of summary #######

print ("summary completed")


######## summarize the response times ########

{
  
  ## stimulus length 
  
  
  
  ## answer latency
  
  ## response onset time
  
  ## response peak time 
  
  
  
  
}




