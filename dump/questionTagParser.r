# project to parse question tags
# create some question lables for three charts with rotated questions
#
chart1 <- c("N1", "S2", "C3", "R4", "R5", "C6", "R7", "R8", "C9")
chart2 <- c("N1", "S2", "C6", "R7", "R4", "C9", "R8", "R5", "C3")
chart3 <- c("N1", "S2", "C9", "R5", "R8", "C6", "R4", "R7", "C3")
#
stimulusEvents <- dataCSV[[3]]
stimulusEventList <- levels(stimulusEvents)
numberStimulusEvents <- length(stimulusEventList)
