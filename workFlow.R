# get all stimulus segments for a unique series
#
# requires eventList for all events for each series for each exam
# requires _

# source the NCCAASCIIParse.R script first

##########################################

# source the NCCAASCII_parse.R script first

library(stringr)

# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

#####

# source the centerData.R script first to get the exam chart
# and center the onset to zero
source('~/Documents/R_programming/NCCA_ASCII_Parse/centerData.R', echo=TRUE)

# source the DSP_filters.R script to process the time series data
source('~/Documents/R_programming/NCCA_ASCII_Parse/DSP_filters.R', echo=TRUE)

# then source the setRange.R script to set the range of data from 0 to 100
source('~/Documents/R_programming/NCCA_ASCII_Parse/setRange.R', echo=TRUE)

# source the cardioProc script to process the diastolic 
source('~/Documents/R_programming/NCCA_ASCII_Parse/cardioSigProc.R', echo=TRUE)

# save the environment in the present state
# save.image(file="NCCAworking2.Rda")



#####

# source the getEventsList.R function to make a list 
# of all onset offset answer events for all stimuli 
# for all charts series and exams
source('~/Documents/R_programming/NCCA_ASCII_Parse/getEventLists.R', echo=TRUE)

# source the stimSegment.r script to make lists of all segments for each chart
# source getEventLists() function first
source('~/Documents/R_programming/NCCA_ASCII_Parse/getSegmentLists.R', echo=TRUE)

# save the environment in the present state
save.image(file="NCCAworking3.Rda")
load("NCCAworking3.Rda")


#####

# add the events to the data frames
source('~/Documents/R_programming/NCCA_ASCII_Parse/addEventColumn.R', echo=TRUE)

# add the PneumoExtract columns for the upper and lower pneumo sensors
source('~/Documents/R_programming/NCCA_ASCII_Parse/Pneumo_extract.R', echo=TRUE)

# add the AutoEDAExtract column
source('~/Documents/R_programming/NCCA_ASCII_Parse/EDAExtractFn.R', echo=TRUE)

# add the cardio Extract column and cardio response measurements
source('~/Documents/R_programming/NCCA_ASCII_Parse/Cardio_extract.R', echo=TRUE)

# save the environment in the present state
# save.image(file="NCCA_working4.Rda")


# look at a data frame

mySegmentLists <- ls(pattern="*_dataSegmentList$")
myEventLists <- ls(pattern="*_eventList$")

mySegmentLists <- mySegmentLists[1]
myEventLists <- myEventLists[1]

mySegmentDF <- get(mySegmentLists)[[1]]
myEventDF <- get(myEventLists)[[1]]

myData <- mySegmentDF$AutoEDA
#myData <- mySegmentDF$CardioMA

begin <- myEventDF$Begin
end <- myEventDF$End
answer <- myEventDF$Answer
start <- mySegmentDF$Sample[1]
lat <- .5


# plot the segments
source('~/Documents/R_programming/NCCA_ASCII_Parse/segmentPlot.R', echo=TRUE)




