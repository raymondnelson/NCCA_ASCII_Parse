

OSS3ChannelContributionsFn <- function(scoreSheetDF=OSS3ScoreSheetDF2, 
                                       withinChartRQMeans=withinChartRQMeans, 
                                       weightingCoefs=c(R=.192, E=.528, C=.28) ) {
  # calculate the contribution of OSS-3 sensors, charts and questions
  #
  # input is the scoreSheetDF with all standardized logged R/Cmean ratios
  #
  # use the score sheet and transform z to p within each chart 
  # calculate sum of p for all sensors
  # divide each sensor p by the sum to get the proportion for each question
  # average the proportions for each sensor for each question between charts
  # average the p between questions for each sensor
  # 
  # could also
  # standardize to the 7/31/09 norms and calculate the area for all sensors
  # use a 2-tailed alpha =.05
  #
  # output is a vector of decimal proportions for each sensor, chart, and RQ
  #
  # requires the seriesTotalsFn() in the outputScores.R script 
  #
  ####
  
  if(!exists("weightingCoefs")) { weightingCoefs <- c(R=.192, E=.528, C=.28) }
  
  uniqueCharts <- unique(scoreSheetDF$chartName)
  uniqueQuestions <- names(scoreSheetDF)[5:ncol(scoreSheetDF)]
  uniqueSensors <- unique(scoreSheetDF$sensorName)
  
  #### calculate the area (p) for the sensor contributions ####
  
  pScoreSheetDF <- scoreSheetDF
  
  pScores <- pnorm( as.matrix(scoreSheetDF[,5:ncol(scoreSheetDF)]) )
  
  pScores[which(is.na(pScores))] <- 0
  
  pScoreSheetDF[,5:ncol(pScoreSheetDF)] <- pScores
  
  
  #### sensors ####
  
  # normalize the sensor contributions for each stimulus presentation 
  
  pScoreSheetDF2 <- pScoreSheetDF
  
  pScoresN <- pScores
  
  i=1
  while(i < (nrow(pScores)-1)) {
    j=1
    for(j in 1:ncol(pScores)) {
      # get the sensor values for each stimulus presentation
      theseVals <- as.vector(pScores[(i:(i+2)),j])
      # apply the weighting coefs
      theseVals <- theseVals * weightingCoefs
      # normalize the weighted sensor values so they sum to 1
      theseVals <- theseVals / sum(theseVals, na.rm=TRUE)
      # save em
      pScoresN[(i:(i+2)),j] <- theseVals
    }
    i <- i + 3
    # if(i >= (nrow(pScores)-2)) next()
  }
  
  # View(pScoresN)
  
  pScoreSheetDF2[,5:ncol(pScoreSheetDF2)] <- pScoresN
  
  # View(pScoreSheetDF2)
  
  # compute the mean of proportion or sensor contribution
  
  uniqueSensors <- unique(pScoreSheetDF2$sensorName)
  
  uniqueRQs <- names(pScoreSheetDF2[5:ncol(pScoreSheetDF2)])
  
  # initialize this to NA
  meanSensorProporations <- 
    matrix(nrow=length(uniqueSensors), ncol=length(uniqueRQs) )
    
  i=1
  for(i in 1:length(uniqueRQs)) {
    thisRQCol <- 
      which(names(pScoreSheetDF2) == uniqueRQs[i])
    j=1
    for (j in 1:length(uniqueSensors)) {
      theseSensorRows <- which(pScoreSheetDF2$sensorName == uniqueSensors[j])
      theseSensorVals <- pScoreSheetDF2[theseSensorRows,thisRQCol]
      meanSensorProporations[j,i] <- mean(theseSensorVals, na.rm=TRUE)
    }
  }
  
  meanSensorContributions <- round(rowMeans(meanSensorProporations, na.rm=TRUE), 3)
  
  # normalize it
  meanSensorContributions <- 
    round(meanSensorContributions / sum(meanSensorContributions, na.rm=TRUE), 3)
    
  # meanSensorContributions is the proportion of the score 
  # that is attributed to each sensor
  
  ## norms ##
  
  contributionMeans <- c(0.175,	0.493,	0.338)
  contributionSDs <- c(0.078,	0.128,	0.096)
  
  ####
  
  area <-
    pnorm((meanSensorContributions - contributionMeans) / contributionSDs)

  area <- round(area, 3)
  
  names(area) <- uniqueSensors
  
  # area is the proportion of normative cases for which the observed case
  # shows a greater sensor contribution
  
  contributionOutput <- area
  
  #### calculate the area (p) for the chart contributions ####
  
  
  chartMeans <- rowMeans(withinChartRQMeans[,4:ncol(withinChartRQMeans)], na.rm=TRUE)
  
  chartMeansProp <- pnorm(chartMeans)
  
  chartMeansN <- chartMeansProp / sum(chartMeansProp, na.rm=TRUE)
  
  ## norms ##
  
  chartContributionMean <- 0.333333333
  chartContributionStDev <- 0.141537158
  
  # standardize it
  
  chartMeansZ <- (chartMeansN - chartContributionMean) / chartContributionStDev
  
  chartMeansArea <- round(pnorm(chartMeansZ), 3)
  
  names (chartMeansArea) <- uniqueCharts
  
  # chartMeansArea is the portion of the result that is attributed to each chart
  
  contributionOutput <- c(contributionOutput, chartMeansArea)
  
  
  #### calculate the area for question contributions 
  
  ## norms ##
  
  RQContributionMean <- 0.333333333
  RQContributionStDev <- 0.10646331
  
  # use the between chart means of the within chart weighted means for each RQ
  # requires the seriesTotalsFn
  # source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)
  seriesTotalsDF <- seriesTotalsFn(scoreSheetDF=scoreSheetDF,
                                   outputName="OSS3SseriesTotalsDF",
                                   aggType="mean",
                                   weightingCoefs=weightingCoefs,
                                   aggMethod="within",
                                   makeDF=FALSE,
                                   
                                   saveCSV=FALSE)
  # initialize a matrix of question scores 
  questionMatrix <- as.numeric(seriesTotalsDF[1,3:ncol(seriesTotalsDF)])
  
  questionMatrixProp <- pnorm(questionMatrix)
  
  questionMatrixN <- questionMatrixProp / sum(questionMatrixProp)
  
  questionMatrixZ <- (questionMatrixN - RQContributionMean) / RQContributionStDev
  
  QuestionMatrixArea <- round(pnorm(questionMatrixZ), 3)
  
  names(QuestionMatrixArea) <- names(seriesTotalsDF)[3:ncol(seriesTotalsDF)]
  
  # add the vector to the output
  contributionOutput <- c(contributionOutput, QuestionMatrixArea)
  
  
  
  
  
  
  
  # # initialize a matrix of area (p) scores
  # scoresMatrix <- as.data.frame(
  #   pnorm(as.matrix(scoreSheetDF[,5:ncol(scoreSheetDF)])))
  # # rownames(scoresMatrix) <- scoreSheetDF$sensorName
  # 
  # # initialize a sensor matrix 
  # sensorMatrix <- as.data.frame(matrix(ncol=length(uniqueQuestions), 
  #                      nrow=length(uniqueSensors)))
  # colnames(sensorMatrix) <- uniqueQuestions
  # rownames(sensorMatrix) <- uniqueSensors
  # 
  # # iterate over the sensors and calculate the mean
  # i=1
  # for(i in 1:length(uniqueSensors)) {
  #   thisSensor <- uniqueSensors[i]
  #   sensorRows <- which(scoreSheetDF$sensorName == thisSensor)
  #   sensorMatrix[i,] <- colMeans(scoresMatrix[sensorRows,], na.rm=TRUE)
  # }
  # # calculate the sensor means
  # meanSensorP <- rowMeans(sensorMatrix, na.rm=TRUE)
  # # apply the weighting coefs from the discriminate analysis
  # meanSensorP <- meanSensorP * weightingCoefs
  # # normalize it
  # meanSensorP <- meanSensorP / sum(meanSensorP, na.rm=TRUE)
  # # add the result to the output list
  # contributionOutput <- meanSensorP
  # 
  # #### charts ####
  # 
  # # initialize a matrix of chart scores 
  # chartMatrix <- as.data.frame(matrix(ncol=length(uniqueQuestions), 
  #                                      nrow=length(uniqueCharts)))
  # colnames(chartMatrix) <- uniqueQuestions
  # rownames(chartMatrix) <- uniqueCharts
  # # iterate over the charts and calculate the means
  # # use the weighted mean RQ score 
  # i=1
  # for(i in 1:length(uniqueCharts)) {
  #   thisChart <- uniqueCharts[i]
  #   chartRows <- which(scoreSheetDF$chartName == thisChart)
  #   # need to use weighted.mean() instead
  #   chartMatrix[i,] <- colMeans(scoresMatrix[chartRows,], na.rm=TRUE)
  # }
  # # calculate the chart means
  # meanChartP <- rowMeans(chartMatrix, na.rm=TRUE)
  # # normalize it
  # meanChartP <- meanChartP / sum(meanChartP, na.rm=TRUE)
  # # add the result to the output
  # contributionOutput <- c(contributionOutput, meanChartP)
  # 
  # 
  # 
  # 
  # #### questions ####
  # 
  # # use the between chart means of the within chart weighted means for each RQ
  # # requires the seriesTotalsFn
  # # source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)
  # seriesTotalsDF <- seriesTotalsFn(scoreSheetDF=scoreSheetDF,
  #                                  outputName="OSS3SseriesTotalsDF",
  #                                  aggType="mean",
  #                                  weightingCoefs=weightingCoefs,
  #                                  aggMethod="within",
  #                                  makeDF=FALSE,
  #                                  saveCSV=FALSE)
  # # initialize a matrix of question scores 
  # questionMatrix <- as.numeric(seriesTotalsDF[1,3:ncol(seriesTotalsDF)])
  # questionMatrix <- pnorm(questionMatrix)
  # questionMatrix <- questionMatrix / sum(questionMatrix)
  # names(questionMatrix) <- names(seriesTotalsDF)[3:ncol(seriesTotalsDF)]
  # # add the vector to the output
  # contributionOutput <- c(contributionOutput, questionMatrix)
  
  
  
  
  
  ####
  return(contributionOutput)
}



# OSS-3 sensor contributions 7/31/2009
# OSS3SensorContributionMean <- c(RMean=.175, EMean=.495, CMean=.338)
# OSS3SensorContributionStDevs <- c(RStDev=.078, EStDev=.128, CStDev=.096)

# P Deceptive	E Deceptive	C Deceptive	P Truthful	E Truthful	C Truthful
# 0.274	0.425	0.301	0.200	0.554	0.246
# 0.135	0.138	0.125	0.066	0.083	0.071
# 
# chart contribution
# Mean	0.333333333
# StDev	0.141537158
# 
# Question contribution
# Mean	0.333333333
# StDev	0.10646331




