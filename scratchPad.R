
# get the pneumo scores
pneumoDF <- scoresDF[scoresDF$sensorName=="UPneumo" | scoresDF$sensorName=="LPneumo",]

# -1 scores
length(which(pneumoDF$RCScore==-1))
length(which(pneumoDF$RCScore==-1)) / ( nrow(pneumoDF) - length(which(is.na(pneumoDF$RCScore))) )

# correct -1 scores
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==-1)]==pneumoDF$criterionState[which(pneumoDF$RCScore==-1)]))
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==-1)]==pneumoDF$criterionState[which(pneumoDF$RCScore==-1)])) /
  length(which(pneumoDF$RCScore==-1))

# incorrect -1 scores
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==-1)]!=pneumoDF$criterionState[which(pneumoDF$RCScore==-1)]))
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==-1)]!=pneumoDF$criterionState[which(pneumoDF$RCScore==-1)])) /
  length(which(pneumoDF$RCScore==-1))
# proportion of - scores for all innocent cases
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==-1)]!=pneumoDF$criterionState[which(pneumoDF$RCScore==-1)])) /
  length(which(pneumoDF$criterionState==1))
  
# +1 scores
length(which(pneumoDF$RCScore==1))
length(which(pneumoDF$RCScore==1)) / ( nrow(pneumoDF) - length(which(is.na(pneumoDF$RCScore))) )

# correct +1 scores
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==1)]==pneumoDF$criterionState[which(pneumoDF$RCScore==1)]))
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==1)]==pneumoDF$criterionState[which(pneumoDF$RCScore==1)])) /
  length(which(pneumoDF$RCScore==1))

# incorrect +1 scores
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==1)]!=pneumoDF$criterionState[which(pneumoDF$RCScore==1)]))
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==1)]!=pneumoDF$criterionState[which(pneumoDF$RCScore==1)])) /
  length(which(pneumoDF$RCScore==1))
#proportion of + scores for all guilty cases
length(which(pneumoDF$RCScore[which(pneumoDF$RCScore==1)]!=pneumoDF$criterionState[which(pneumoDF$RCScore==1)])) /
  length(which(pneumoDF$criterionState==-1))

# zero scores
length(which(pneumoDF$RCScore==0))
length(which(pneumoDF$RCScore==0)) / ( nrow(pneumoDF) - length(which(is.na(pneumoDF$RCScore))) )

# zero scores for guilty cases
length(which(pneumoDF$RCScore==0 & pneumoDF$criterionState==-1)) 
# proportion of 0 scores that are guilty
length(which(pneumoDF$RCScore==0 & pneumoDF$criterionState==-1)) / length(which(pneumoDF$RCScore==0))
# proportion of guilty cases that are zero
length(which(pneumoDF$RCScore==0 & pneumoDF$criterionState==-1)) / length(which(pneumoDF$criterionState==-1))

# zero scores for innocent cases
length(which(pneumoDF$RCScore==0 & pneumoDF$criterionState==1)) 
# proporiton of 0 scores that are innocent
length(which(pneumoDF$RCScore==0 & pneumoDF$criterionState==1)) / length(which(pneumoDF$RCScore==0))
# proportion of innocent cases that are zero
length(which(pneumoDF$RCScore==0 & pneumoDF$criterionState==1)) / length(which(pneumoDF$criterionState==1))

# NA scores for guilty cases
length(which(is.na(pneumoDF$RCScore) & pneumoDF$criterionState==-1)) 
# proportion of NA cases that are guilty
length(which(is.na(pneumoDF$RCScore) & pneumoDF$criterionState==-1)) / length(which(is.na(pneumoDF$RCScore)))
# proportion of guilty cases that are NA
length(which(is.na(pneumoDF$RCScore) & pneumoDF$criterionState==-1)) / length(which(pneumoDF$criterionState==-1))

# NA scores for innocent cases
length(which(is.na(pneumoDF$RCScore) & pneumoDF$criterionState==1)) 
# proportion of NA cases that are innocent
length(which(is.na(pneumoDF$RCScore) & pneumoDF$criterionState==1)) / length(which(is.na(pneumoDF$RCScore)))
# proportion of innocent cases that are NA
length(which(is.na(pneumoDF$RCScore) & pneumoDF$criterionState==1)) / length(which(pneumoDF$criterionState==1))




summary(as.numeric(pneumoDF$RQValues), na.rm=TRUE)
summary(as.numeric(pneumoDF$RCMethod1), na.rm=TRUE)

View(pneumoDF)



