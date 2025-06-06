# make a scoresheet table for each exam

library(stringr)

# get exam names from the _Data data frames
uniqueExamScores <- unique(str_sub(ls(pattern=".*_scores$", pos=1),1, -1))
# uniqueExams <- uniqueExams[1]

# add the confirmation status
confirmationDF2 <- read.csv("confirmationDF.csv", stringsAsFactors = FALSE)

# then reshpate the confirmation status to the scoresDF
library(reshape2)
for (i in 1:length(uniqueExamScores)) {
  examScoresDF <- get(uniqueExamScores[i], pos=1)
  examScoresDF$RCScore <- as.numeric(examScoresDF$RCScore)
  examScoresDF$RCScore[examScoresDF$sensorName=="EDA"] <- 2*examScoresDF$RCScore[examScoresDF$sensorName=="EDA"]
  scoreSheetDF <- dcast(examScoresDF, examName + criterionState + chartName + sensorName ~ RQNames, value.var="RCScore", fun.aggregate = sum, na.rm=TRUE)
  dfName <- paste0(str_sub(uniqueExamScores[i],1,-8),"_scoreSheet")
  assign(dfName, scoreSheetDF,pos=1)
  write.csv(scoreSheetDF, file=paste0(dfName, ".csv"), row.names=FALSE)
}
