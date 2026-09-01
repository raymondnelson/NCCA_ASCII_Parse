# R script to compute the ESS-M and OSS-3 sensor correlation
# August 30, 2026
# Raymond Nelson
####


## must first run the summarizeAccuracy.R script in the NCC_ASCII_Parse project ##


criterionStateFileName <- list.files(".", pattern="?criterionState.csv")

criterionStateDF <- as.data.frame(read_csv(criterionStateFileName))



######### summarize the proportions of ESS-M scores #############



summarizeScoresProp <- TRUE


if(isTRUE(summarizeScoresProp)) {
  
  library(readr)
  
  ### use the sensorTotalsDF to summarize the scores
  
  # View(sensorTotalsDF)
  
  print("normalized proportions for absolute sensor sums")
  # print(colSums(abs(sensorTotalsDF[,4:ncol(sensorTotalsDF)])) / 
  #         sum(colSums(abs(sensorTotalsDF[,4:ncol(sensorTotalsDF)])), na.rm=TRUE) )
  print(colSums(abs(sensorTotalsDF[,4:7])) / 
          sum(colSums(abs(sensorTotalsDF[,4:7])), na.rm=TRUE) )
  
  
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
  
  # examNames <- str_sub(scoreSheetFiles, 2, -22)
  
  # iterate over each case
  
  # only non-zero scores
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
  
  
  scoreSheetFreqDF <- scoreSheetFreqDF[order(scoreSheetFreqDF$ID),]
  
  
  # View(scoreSheetFreqDF)
  
  # compute the number of 
  outputVector <- 
    colSums(scoreSheetFreqDF[,2:5]) / colSums(scoreSheetFreqDF)['nScores']
  
  # View(scoreSheetFreqDF)
  
  # sum(outputVector)
  
  print("normalized proportion of non-zero scores")
  print(outputVector)
  
  print("proportion of ESS scores")
  print(outputVector * c(1, 1, 1, 1))
  
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
  # numbCases <- nrow(scoreSheetFreqDF)
  numbCases <- totalCases
  
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



#################  summarize OSS-3 sensor correlations  #################



summarizeOSS3Sensors <- FALSE
summarizeOSS3Sensors <- TRUE



if(isTRUE(summarizeOSS3Sensors)) {
  
  fileNames <- list.files(pattern="OSS3SensorMeans")
  
  examNames <- str_sub(fileNames, 1, -23)
  
  # FZCT N60
  # examNames <- str_sub(fileNames, 2, -23)
  
  # examNames <- str_sub(fileNames, 2, -24)
  
  # examNames <- str_sub(fileNames, 2, 8)
  # examNames <- str_sub(examNames, 1, 3)
  
  # for Dollins et al 2001
  # examNames <- str_sub(fileNames, 2, 6)
  
  # Initialize an object
  OSS3SensorMeansDF <- NA
  
  i=1
  for(i in 1:length(fileNames)) {
    
    thisCSV <- read_csv(fileNames[i])
    
    #length will be equal to the number of series
    # thisAnalysis <-  get(analysisLists[i], pos=1)
    # View(thisAnalysis)
    
    examName <- str_sub(fileNames[i], 1, -23)
    # examName <- str_sub(analysisLists[i], 1, -10)
    
    # thisExamName <- examNames[i]
    
    # ohio
    # examName <- str_sub(examName, 1, -2)
    
    # thisExamName <- str_sub(thisExamName, 1, -2)
    
    uniqueSeries <- as.vector(unique(thisCSV[,'seriesName']))
    
    # iterate on the series
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        seriesName <- uniqueSeries[j]
        
        # seriesName <- names(thisAnalysis)[j]
        
        # seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      # {
      #   
      #   # get the OSS-3 analysis
      #   OSS3Analysis <- 
      #     thisAnalysis[[seriesName]][["OSS3Output"]]
      #   
      #   # View(OSS3Analysis)
      #   
      #   if(is.null(OSS3Analysis)) next()
      #   
      # }
      
      # thisCSV <- OSS3Analysis[['OSS3SensorMeansDF']]
      thisSensorMeansDF <- thisCSV[which(thisCSV$seriesName==seriesName),]
      
      # compute sensor means for this exam
      theseMeans <- as.vector(rowMeans(thisSensorMeansDF[4:ncol(thisSensorMeansDF)], na.rm=TRUE))
      
      # if(any(is.na(theseMeans))) stop("problem with sensor means")
      
      # theseMeans <- c(examNames[i], theseMeans)
      
      thisState <- 
        criterionStateDF$criterionState[which(criterionStateDF$examName == examName)]
      
      thisCase <- c(theseMeans, thisState)
      names(thisCase) <- c(thisSensorMeansDF$sensorName, "criterionState")
      
      OSS3SensorMeansDF <- 
        rbind.data.frame(OSS3SensorMeansDF, thisCase)
      
    } # end j loop
    
  } # end i loop
  
  # remove the empty first row
  OSS3SensorMeansDF <- OSS3SensorMeansDF[2:nrow(OSS3SensorMeansDF),]
  
  colnames(OSS3SensorMeansDF) <- c(thisCSV$sensorName, "criterionState")
  
  OSS3SensorMeansDF <- cbind(examName=examNames, OSS3SensorMeansDF)
  # View(OSS3SensorMeansDF)
  
  # sort by exam name
  OSS3SensorMeansDF <- OSS3SensorMeansDF[order(OSS3SensorMeansDF$examName),]
  
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





