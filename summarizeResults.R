# summarize results without the criterion state


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
  
  
  
}



######## get the ANALYSIS lists in the CWD #############



{
  
  uniqueExams <- getUniqueExams(x="*_Data$")
  
  
  analysisLists <- ls(pattern =".ANALYSIS$")
  
  # exclude cases that consist only of the ACQT
  analysisLists <- analysisLists[!grepl("ACQT", analysisLists)]
  
  examNames <- str_sub(analysisLists, 2, -10)
  
  # missing exams or bad exam names
  uniqueExams[which(!(str_sub(uniqueExams, 2, -1) %in% examNames))]
  
  print(examNames)
  
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
  
  examNames <- str_sub(analysisLists, 2, -10)
  
  # cases that are missing an ANALYSIS list
  uniqueExams[which(!(str_sub(uniqueExams, 2, -1) %in% examNames))]
  
}



#################################################

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



#########   initialize a function for the result codes   ###########



resultCodesFN <- function(x) {
  # define a function compute the result codes
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



############# set up the summary ################



{
  
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")
  
  # set these for the test format
  # testFormat <- "Utah"
  # testFormat <- "FedZCT"
  # testFormat <- "CQT"
  testFormat <- "DLST"
  # testFormat <- ""
  #testFormat <- "MGQT"
  # testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  # RQNames <- c("R1", "R2", "R3", "R4")
  RQNames <- c("R5", "R7", "R10", "none")
  # RQNames <- c("R5", "R8", "R11", "NA")
  RQNames <- c("R1", "R2", "none", "none")
  
  # seriesTotalsDF <- as.data.frame(matrix(ncol=(5+(1*length(RQNames))), 
  #                                        nrow=length(seriesTotalFiles)))
  
  
  # DRule <- "TSR"
  DRule <- "SSR"
  
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


############# summarize the ESS-M series totals without PLE ##################



summarizeResults <- TRUE



if(isTRUE(summarizeResults)) {
  
  withPLE <- FALSE
  
  # library(stringr)
  
  # get the ESS-M series totals for all 
  seriesTotalFiles <- list.files(pattern="ESSMSeriesTotals.csv")
  
  scoreSheetFiles <- list.files(pattern="ESSMScoresheet.csv")
  
  # numbCases <- length(seriesTotalFiles)
  
  numbCases <- length(scoreSheetFiles)

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
    
    # View(thisCSV)
    
    # without PLE
    if(!isTRUE(withPLE)) {
      thisCSV <- thisCSV[thisCSV$sensorName != "PLE",]
    }
    
    # View(thisCSV)
    
    # exam nname
    thisExamName <- examNames[i]
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
    # nRQs <- ncol(thisCSV)-4
    # seriesTotalsDF[i,4:(4+nRQs-1)] <- colSums(thisCSV[5:(5+nRQs-1)], na.rm=TRUE)
    
    # View(seriesTotalsDF)
    
    # for DLST exams
    nRQs <- 2
    RQSubtotalScores <- colSums(thisCSV[c(5:12)], na.rm=TRUE)
    RQSubtotalScores <- c(sum(RQSubtotalScores[c(1,3,5,7)]), sum(RQSubtotalScores[c(2,4,6,8)]))
    seriesTotalsDF[i,4:(4+nRQs-1)] <- RQSubtotalScores
    
    
    # # add the criterion state
    # seriesTotalsDF[i,'criterionState'] <-
    #   criterionStateDF$criterionState[which(criterionStateDF$examName %in% thisExamName)]
    
  } # end i loop
  
  # View(seriesTotalsDF)
  
  
  
  
   
}
