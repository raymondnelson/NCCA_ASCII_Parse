dataEdit

DFSGN3_Data$Sample[min(which(DFSGN3_Data$chartName=="01A"))]
# [1] 1


DFSGN3_Data$Sample[max(which(DFSGN3_Data$chartName=="01A"))]
# [1] 8910

DAT <- DFSGN3_Data


Cardio1 <- DAT$Cardio1
Cardio2 <- Cardio1


# insert movement artifact
Cardio2[4936:8910] <- Cardio1[4936:8910] + sample(c(130:170), 1)

# DAT$Cardio1 <- Cardio2


Cardio1[4900:5000]
Cardio2[4900:5000]


DFSGN3_Data <- DAT

# insert movement instruction


# 4972 MI

STIM <- DFSGN3_Measurements

sensors <- unique(STIM$sensorName)

newStimDF <- as.data.frame(matrix(ncol=length(names(STIM)), nrow=length(sensors)))

names(newStimDF) <- names(STIM)
newStimDF$examName <- STIM$examName[1]
newStimDF$seriesName <- STIM$seriesName[1]

newStimDF$chartName <- STIM$chartName[98]
newStimDF$Label <- "MI"
newStimDF$eventLabel <- "MI"

newStimDF$sensorName <- sensors

newStimDF$Begin <- 4972
newStimDF$End <- 4972
newStimDF$Answer <- 4973


STIM2 <- rbind(STIM[1:98,], newStimDF, STIM[99:nrow(STIM),])

DFSGN3_Measurements <- STIM2

DFSGN3_Data$Label[4972] <- "MI"
