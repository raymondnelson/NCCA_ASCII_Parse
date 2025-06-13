# get all stimulus segments for a unique series
#
# requires eventList for all events for each series for each exam
# requires _
#
# source the NCCAASCIIParse.R script first
# to make csv files and separate data and header info,
# including events, question onset, question offset,  answer, and question text
#
##########################################


# setwd("~/Dropbox/R/NCCA_ASCII_Parse")


#####


# parse and load the data from the NCCA ASCII output 
source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCIIParse.R', echo=FALSE)


############


# process the data in the global environment


rm(list=ls())


load(file="NCCAWorking.Rda")
# rm(list=ls(pattern="Joe_tests_Ted_"))


# get exam names from the _Data data frames
library(stringr)

print("make a list of unique exams")
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[2]
print(uniqueExams)


# source the excludedEvents.R script so that annotations are not measured
source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)


##########################################


# source the script and call the function to parse the stimulus events
# source('~/R/NCCA_ASCII_Parse/eventParse.r', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/eventParse.R')

# eventParse function makes a stimulus matrix for the events
# for each series, keeping only the charts with >=2 CQs and >=2 RQS,
# and will set the length of the stimulus sequence to the max length
# for each series

# eventParse(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE, type="CQT")
# x input is a character string to identify the Stimuli data frames


#########################################

# source the script and call the function to check whether stim events are identical
# source('~/Dropbox/R/NCCA_ASCII_Parse/stimulusParse.r', echo=FALSE)

# function to determine whether stimulus text is identical for questions on all charts
# x input is a character string to identify the Stimuli data frames
# output is a data frame and csv warning regarding differences

# stimCheck(x="_Stimuli$", saveCSV=TRUE, makeDF=TRUE)


###################################### pre-processing


source('~/Dropbox/R/NCCA_ASCII_Parse/preProc.R', echo=FALSE)
print("pre-processing")
preProc(x=uniqueExams, y=100, showNames=TRUE, output=FALSE)


# 4/23/2016 replaced these three functions with a single preProc function
# # source the addStimulusColumns.R script to add 4 stimulus event columns,
# # including events, question onset, question offset,  answer, and question text
# source('~/Dropbox/R/NCCA_ASCII_Parse/addStimulusColumns.R')
# addStimulusColumns(x=uniqueExams, output=FALSE, showNames=TRUE)
# 
# # source the centerData.R script first to center the onset to zero
# source('~/Dropbox/R/NCCA_ASCII_Parse/centerData.R', echo=TRUE)
# centerData(x=uniqueExams, output=FALSE)
# 
# # then source the setRange.R script to set the range of data from 0 to 100
# source('~/Dropbox/R/NCCA_ASCII_Parse/setRange.R', echo=TRUE)
# setRange(x=uniqueExams, y=100, showNames=TRUE, output=FALSE)


# save.image(file="WorkingData2.Rda")
# # load(file="WorkingData2.Rda")


# source the sigProcHelper script to load the helper functions for signal processing
# scaleVals <- c(35, 35, 170, 30, 30, 15)
# yOffset <- c(130, 75, 10, -45, -110, -145)
# names(yOffset) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity")
source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=TRUE)
source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc.R', echo=FALSE)
sigProc(x=uniqueExams, output=FALSE, showNames=TRUE)


# # source the pneumoSigProc script to process the upper and lower pneumograph data 
# source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoSigProc.R', echo=TRUE)
# pneumoSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)
# 
# # source the DSP_filters.R script to process the time series EDA data
# source('~/Dropbox/R/NCCA_ASCII_Parse/EDASigProc.R', echo=TRUE)
# applyFilter(x=uniqueExams, output=FALSE, showNames=TRUE)
# 
# # source the cardioSigProc script to process the cadio data 
# source('~/Dropbox/R/NCCA_ASCII_Parse/cardioSigProc.R', echo=TRUE)
# cardioSigProc(x=uniqueExams, output=FALSE, showNames=TRUE)
# 
# # source the PLESigProc script to process the PLE data
# source('~/Dropbox/R/NCCA_ASCII_Parse/PLESigProc.R', echo=TRUE)
# PLESigProc(x=uniqueExams, output=FALSE, showNames=TRUE)
# 
# # source the activitySigProc script to process the activity sensor data
# source('~/Dropbox/R/NCCA_ASCII_Parse/activitySigProc.R', echo=TRUE)
# activitySigProcFn(x=uniqueExams, output=FALSE, showNames=TRUE)


# save.image(file="WorkingData3.Rda")
# # load(file="WorkingData3.Rda")


source('~/Dropbox/R/NCCA_ASCII_Parse/TukeyFences.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/artifactProc.R', echo=FALSE)
print("artifact extraction")
artifactProc(x=uniqueExams, showNames=TRUE, output=FALSE)


# # source the addArtifactCols.R script to add the artifact columns for all channels
# source('~/Dropbox/R/NCCA_ASCII_Parse/addArtifactCols.R', echo=TRUE)
# artifactCols(x=uniqueExams, showNames=TRUE, output=FALSE)


# # source the pArtifact2.R script to artifact the pneumo data
# source('~/Dropbox/R/NCCA_ASCII_Parse/pArtifact.R', echo=TRUE)
# pneumoArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)
# 
# # source the eArtifact.R script to artifact the EDA data
# source('~/Dropbox/R/NCCA_ASCII_Parse/TukeyFences.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/eArtifact.R', echo=TRUE)
# electrodermalArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)
# 
# source('~/Dropbox/R/NCCA_ASCII_Parse/cArtifact.R', echo=TRUE)
# cardioArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)
# 
# # source the aArtifact.R script to process the activity sensor artifacts
# source('~/Dropbox/R/NCCA_ASCII_Parse/aArtifact.R', echo=TRUE)
# activityArtifactFn(x=uniqueExams, showNames=TRUE, output=FALSE)


# source the chartPlot.R script to print the plots
# source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot.R', echo=TRUE)


#####


#####


# # source the getEventsList.R function to make a list 
# # of all onset offset answer events for all stimuli 
# # for all charts series and exams
# source('~/Dropbox/R/NCCA_ASCII_Parse/getEventLists.R', echo=TRUE)
# 
# # source the stimSegment.r script to make lists of all segments for each chart
# # source getEventLists() function first
# source('~/Dropbox/R/NCCA_ASCII_Parse/getSegmentLists.R', echo=TRUE)



# # save the environment in the present state
# #save.image(file="NCCAworking3.Rda")
# save.image(file="CMCharts3.Rda")
# # load("NCCAworking3.Rda")


#####


# remove _Stimuli data frames
# rm(list=c(ls(pattern="*_Stimuli$")))


#####


# add the events to the data frames
# source('~/Dropbox/R/NCCA_ASCII_Parse/addEventColumn.R', echo=TRUE)

# add the PneumoExtract columns for the upper and lower pneumo sensors
# source('~/Dropbox/R/NCCA_ASCII_Parse/Pneumo_extract.R', echo=TRUE)



#### fix repeated questions by appending to the name of the repetition for each chart in the exam data frame
source('~/Dropbox/R/NCCA_ASCII_Parse/fixDup.R', echo=FALSE)
print("fix duplicated or repeated stmulus event names")
fixDuplicatesFn(x=uniqueExams, showNames=TRUE, output=FALSE)


#####


##################################################


# save.image(file="WorkingData6.Rda")
# rm(list=ls())
# load(file="WorkingData6.Rda")


# source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoMeasurement.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/PLEMeasurement.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R', echo=TRUE)

source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoMeasurement.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)
source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R', echo=TRUE)
source('~/Dropbox/R/NCCA_ASCII_Parse/PLEMeasurement.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/newPneumoExtract.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/newEDAExtract.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/newCardioExtract.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/newPLEExtract.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/featureExtraction.R', echo=FALSE)

print("feature extraction")
featureExtraction(x=uniqueExams, showNames=TRUE, output=FALSE)


# source the pneumoExtract.R script to mark the pneumo measurements
# source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoExtract.R', echo=TRUE)
# # source('~/Dropbox/R/NCCA_ASCII_Parse/robustPneumoMeasurement.R', echo=FALSE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoMeasurement.R', echo=FALSE)
# pneumoExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE)
# 
# # add the PLE Extract column and PLE response measurements
# source('~/Dropbox/R/NCCA_ASCII_Parse/PLEExtract.R', echo=TRUE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/PLEMeasurement.R', echo=FALSE)
# PLEExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE)
# 
# # add the AutoEDAExtract column
# source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R', echo=TRUE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/EDAExtract.R', echo=TRUE)
# EDAExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE, EW=FALSE, ROW=FALSE, descentRule=FALSE, ignore=2)
# 
# # add the cardio Extract column and cardio response measurements
# # source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=TRUE)
# source('~/Dropbox/R/NCCA_ASCII_Parse/cardioExtract.R', echo=TRUE)
# cardioExtractFn(x=uniqueExams, showNames=TRUE, output=FALSE, EW=FALSE, ROW=FALSE, descentRule=FALSE, ignore=2, cardioLine="mid")


#########


# source the extractMeasurements.R script to get the Kircher measurements
source('~/Dropbox/R/NCCA_ASCII_Parse/extractMeasurements.R')
extractMeasurementsFn(x=uniqueExams, showNames=TRUE, output=FALSE, CSVName="")


#########zs


save.image(file="WorkingData7.RDA")
# # load("WorkingData7.RDA")


# source the chartPlot.R script to print the plots
source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot_init.R')
source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot.R', echo=TRUE)


# plot the segments
source('~/Dropbox/R/NCCA_ASCII_Parse/segmentPlot_init.R')
source('~/Dropbox/R/NCCA_ASCII_Parse/segmentPlot.R', echo=TRUE)


# source the RCParse.R script to calculate the R/C ratios for numerical scoring


####################
# look at a data frame


# ls(pattern="_measurements")



