# get all stimulus segments for a unique series
#
# requires eventList for all events for each series for each exam
# requires _
#
# source the NCCAASCIIParse.R script first
# to make csv files and separate data and header info,
# including events, question onset, question offset,  answer, and question text
#
####


# to stop on warnings
# options(warn=2)
# to reset default warning level
# options(warn=0)


#####  set the working directory  #####

#  
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data")
# setwd("~/Dropbox/TRAINING/Poland - forensic lab - May 2019/practica")
# setwd("~/Dropbox/PFFOLDER/19N0401Steyn")
# setwd("~/Dropbox/R/chartSimulator")
# setwd("~/Dropbox/R/chartSimulator/PCASS")
# setwd("~/Desktop")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/PCASS_starter")
# setwd("~/Dropbox/PRACTICA")
# setwd("~/Dropbox/PFFOLDER")
# setwd("~/Dropbox/TRAINING")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/artifacts")
# setwd("~/Dropbox/DATASETS_BACKUP")
# setwd("~/Dropbox/RAYMOND/QC")
# setwd("~/Dropbox/RAYMOND/From Mark/Private 07-13-19 Jerry Russell (LIsa Johnson Esq)")
# setwd("~/Dropbox/CURRENT_"PROJECTS")
# setwd("~/Dropbox")
# setwd("~/Dropbox/IPTC_Courses")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/simulatorCharts")
# setwd("~/Dropbox/RAYMOND/Court Cases")
# setwd("D:/761")

# setwd("~/Dropbox/LAFAYETTE/Lafayette2019/PCASS_test")
# setwd("~/Dropbox/QC/Assess/May2019")
# setwd("~/Dropbox/IPTC_Courses/South Africa March 2019/practica South Africa 3-2019")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/feature_extraction/slopeChange")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/GF")
# setwd("~/Dropbox/PRACTICA/IPTC practica 10-2018")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/feature_extraction")
# setwd("~/Dropbox/PRACTICA/Honduras practica 4-2018/ACQT_4-3-2-2018")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/fromDonKrapohl")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/feature_extraction/Utah_features/EDA_complexity")
# setwd("~/Dropbox/PRACTICA/Bulgaria_PEAK_practica_10-2018")
# setwd("~/Dropbox/PRACTICA/Practica_ElSalvador_9-2017")
# setwd("~/Dropbox/PRACTICA/Honduras practica 3-2017")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/LafayetteGraphics")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/new_test_04302018")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/artifacts/artifacts_cardio")
# setwd("~/Dropbox/PRACTICA/Argentina_practica_2-2017")
# setwd("~/Dropbox/PRACTICA/Practica_Peru_Dec_2016")
# setwd("~/Dropbox/RAYMOND/Court Cases/WhiteMarlinOpen2017/WMO Polygraph")
# setwd("~/Dropbox/TRAINING/Backster_physiology_3-2017/practica")
# setwd("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/MarinN100_NCCA_innocent")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/MarinN100_NCCA_guilty")
# setwd("~/Dropbox/RAYMOND/Court Cases/Scott Beebe 2016/polygraphs 2006-2007")
# setwd("~/Dropbox/TRAINING/Poland - advanced training Dec 2017")
# setwd("~/Dropbox/TRAINING/Poland - advanced training Dec 2017/practica 12-2107")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/EDA_signal_processing_2017")
# setwd("~/Dropbox/TRAINING/Data Science Indy 1-10-2018")
# setwd("~/Dropbox/CURRENT_PROJECTS/Practical polygraph - seven things about EDA feature extraction/graphic examples")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/EDA_signal_processing_2017/Poland_cases_2017")
# setwd("~/Dropbox/TRAINING/Texas DPS 1-2018/practica")
# setwd("~/Dropbox/IPTC_Courses/South Africa 2017/practica")
# setwd("~/Dropbox/RAYMOND/Court Cases/Ohio 2017/Cara L. Young (Sexual Battery) Harrison Co. SO)")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/new_test_04302018/result")
# setwd("~/Dropbox/IPTC_Courses/Mexico August 2018")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/OSS2/replaced")
# setwd("~/Dropbox/IPTC_Courses/Poland 2018/Training documents Poland/S6 CMs/Practice Exams DLST 2/0995")
# setwd("~/Dropbox/IPTC_Courses/Poland 2018/Training documents Poland/S6 CMs")
# setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/fromBenBlalock/Nov302018")


# Honduras 20160527ACQT
# artifacted cardio 6-14-2016
# uniqueExams <- uniqueExams[c(3,6,7,9,12,13,14,15)]
# un-artifacted cardio 6-14-2016
# uniqueExams <- uniqueExams[c(1,2,4,,5,8,10,11,16)]
# artifacted activity sensor
# uniqueExams <- uniqueExams[c(1,3,15)]
# normal activity sensor
# uniqueExams <- uniqueExams[c(2,3,4,5,7,8,9,10,11,12,14,16)]
# clean activity
# uniqueExams <- uniqueExams[c(3,4,16)]



# print("unique exams in this directory")
# print(uniqueExams)



###### clear the gobal envir and source some init scripts ######



{
  
  rm(list=ls())
  
  library(stringr)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
}


##### set the work flow parameters #####



{
  
  fixNCCAASCIIExamFiles <- TRUE
  
  fixSensorNames <- FALSE
  
  fixDuplicateTags <- FALSE
  
  NCCAASCIIParse <- TRUE # parse the NCCA ASCII charts to a single data frame
  
  stimulusParse <- TRUE
  
  saveRDA1 <- FALSE # use TRUE to save the parsed NCCAASCII data
  
  loadRDA1 <- FALSE
  
  loadRDA2 <- FALSE # use TRUE load the processed and extracted data
  
  processData <- TRUE # use TRUE for scaling and offsetting
  
  processArtifacts <- FALSE
  
  pneumoArtifacts <- TRUE
  edaArtifacts <- TRUE
  cardioArtifacts <- TRUE
  FCArtifacts <- FALSE
  pleArtifacts <- TRUE
  activityArtifacts <- FALSE
  
  integrateCardioArtifacts <- FALSE
  integrateFCArtifacts <- FALSE
  integrateEDAArtifacts <- FALSE
  integrateRespArtifacts <- FALSE
  integrateActivityArtifacts <- FALSE
  
  extractFeatures <- TRUE
  
  PADecisionRule <- "GTR"
  OSS2DecisionRule <- "GTR"
  ESSMDecisionRule <- "TSR"
  OSS3DecisionRule <- "TSR"
  ROSSDecisionRule <- "GTR"
  PSSDecisionRule <- "GTR"
  bootstrapDecisionRule <- "GTR"
  
  getScores <- TRUE # generate R/C scores and algorithm results
  
  # some of the other scores require the rank score
  getRankScores <- TRUE
  
  getRRMScores <- TRUE
  
  getMiritelloRankScores <- TRUE
  
  getIpsativeZScores <- TRUE
  
  # ESSM scores require the rank score 
  getRCScores <- TRUE
  
  getESSScores <- TRUE
  
  getPAScores <- TRUE
  
  getOSS2Scores <- TRUE
  
  getOSS3Scores <- TRUE
  
  getROSSScores <- TRUE
  
  getPSSScores <- TRUE
  
  getBootstrapScores <- TRUE
  
  PCASSFormat <- FALSE
  
  
  
  saveMeasurements <- TRUE # export the series and scoresheet totals
  
  saveRDA2 <- TRUE
  
  printCharts <- FALSE
  
  printSegments <- FALSE
  
}



############## PCASS Format ##################



if(isTRUE(PCASSFormat)) {
  
  getPCASSScores <- TRUE
  
  pneumoArtifacts <- FALSE
  edaArtifacts <- TRUE
  cardioArtifacts <- FALSE
  FCArtifacts <- FALSE
  pleArtifacts <- TRUE
  activityArtifacts <- FALSE
  
} else {
  
  getPCASSScores <- FALSE
  
}


###########  search pattern to locate NCCA ASCII files in the cwd  ###########



# source the getExamNames.R script to load the getCharts() and uniqueNames() functions
source('~/Dropbox/R/NCCA_ASCII_Parse/getExamNames.R', echo=FALSE)



############# fix the D# file names 10-24-2018 LX11.8.2 #############

# {
#   NCCAASCIIchartNames <- getCharts("^D#+", uniqueTests=FALSE)
#   # loop over the file names and rename the files
#   for (i in 1:length(NCCAASCIIchartNames)) {
#     file.rename(NCCAASCIIchartNames[i], gsub("[#]+", "&", NCCAASCIIchartNames[i]))
#   }
# }



####### locate the exams in the current working directory ########



{
  searchPattern1 <- NULL
  searchPattern2 <- NULL
  searchPattern3 <- NULL
  searchPattern4 <- NULL
  
  uniqueExamNames1 <- NULL
  uniqueExamNames2 <- NULL
  uniqueExamNames3 <- NULL
  uniqueExamNames4 <- NULL
  
  print("search the working directory for NCCA ASCII text output")
  # Lafayette uses the &
  searchPattern1 <- "^D&+"
  # Stoelting #
  # 2010 changed to !
  searchPattern3 <- "^D#+"
  # Limestone %
  searchPattern4 <- "^D%+"
  # 2010 changed to @
  # Axciton $ character needs to be double escaped because it is a special character for grep
  searchPattern2 <- "^D\\$"
  
  # call the two functions together to make a list of unique exams in the current working directory
  if(!is.null(searchPattern1)) uniqueExamNames1 <- getCharts(x=searchPattern1, uniqueTests=TRUE)
  if(!is.null(searchPattern2)) uniqueExamNames2 <- getCharts(x=searchPattern2, uniqueTests=TRUE)
  if(!is.null(searchPattern3)) uniqueExamNames3 <- getCharts(x=searchPattern3, uniqueTests=TRUE)
  if(!is.null(searchPattern4)) uniqueExamNames4 <- getCharts(x=searchPattern4, uniqueTests=TRUE)
  
  if(length(uniqueExamNames1)==0) searchPattern1 <- NULL
  if(length(uniqueExamNames2)==0) searchPattern2 <- NULL
  if(length(uniqueExamNames3)==0) searchPattern3 <- NULL
  if(length(uniqueExamNames4)==0) searchPattern4 <- NULL
  
  # uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  # uniqueExamNames <- uniqueExamNames[16]
}



############   locate the exams in the current directory  #############



# select an exam from the vector of exam names

{
  selectExams <- "ALL"
  # selectExams <- 32
  
  if(selectExams[1] != "ALL") {
    uniqueExamNames <- 
      c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams]
  } else uniqueExamNames <- 
      c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  
  print(paste("Found", length(uniqueExamNames), "exams"))
  print(uniqueExamNames)
}



####   fix problem characters in the NCCA ASCII text file names ####



# fixNCCAASCIIExamFiles <- TRUE

if(fixNCCAASCIIExamFiles==TRUE) {
  
  print("fix problem characters in the file and directory names")
  
  # first fix all problem characters in file and directory names
  print("fix problem characters in NCCA ASCII files")
  source('~/Dropbox/R/NCCA_ASCII_Parse/fixFileNames.R', echo=FALSE)
  # run the loop twice for search pattern 1
  if(length(uniqueExamNames1) > 0) {
    fixFileNamesFn(x=searchPattern1)
  }
  if(length(uniqueExamNames1) > 0) {
    fixFileNamesFn(x=searchPattern1)
    # run the loop twice for search pattern 2
  }
  if(length(uniqueExamNames2) > 0) {
    fixFileNamesFn(x=searchPattern2)
  }
  if(length(uniqueExamNames2) > 0) {
    fixFileNamesFn(x=searchPattern2)
  }
  # run the loop twice for search pattern 3
  if(length(uniqueExamNames3) > 0) {
    fixFileNamesFn(x=searchPattern3)
  }
  if(length(uniqueExamNames3) > 0) {
    fixFileNamesFn(x=searchPattern3)
  }
  # run the loop twice for search pattern 4
  if(length(uniqueExamNames4) > 0) {
    fixFileNamesFn(x=searchPattern4)
  }
  if(length(uniqueExamNames4) > 0) {
    fixFileNamesFn(x=searchPattern4)
  }
  
  # re-call the two functions together to make a list of unique exams in the current working directory
  # uniqueExamNames <- uniqueNames(getCharts(searchPattern))
  if(!is.null(searchPattern1)) uniqueExamNames1 <- getCharts(x=searchPattern1, uniqueTests=TRUE)
  if(!is.null(searchPattern2)) uniqueExamNames2 <- getCharts(x=searchPattern2, uniqueTests=TRUE)
  if(!is.null(searchPattern3)) uniqueExamNames3 <- getCharts(x=searchPattern3, uniqueTests=TRUE)
  if(!is.null(searchPattern4)) uniqueExamNames4 <- getCharts(x=searchPattern4, uniqueTests=TRUE)
  
  
  uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  if(selectExams != "ALL") uniqueExamNames <- 
    c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams] 
  
  print(uniqueExamNames)
  
}



####  fix non ASCII characters in the text files  ####



if(fixNCCAASCIIExamFiles==TRUE) {
  
  # fix non-ASCII characters in text files
  print("fix non-ASCII characters in NCCA ASCII output")
  source('~/Dropbox/R/NCCA_ASCII_Parse/fixNonASCIICharacters.R', echo=FALSE)
  if(length(uniqueExamNames1) > 0) {
    fixNonASCIICharactersFn(searchPattern1)
  }
  if(length(uniqueExamNames2) > 0) {
    fixNonASCIICharactersFn(searchPattern2)
  }
  if(length(uniqueExamNames3) > 0) {
    fixNonASCIICharactersFn(searchPattern3)
  }
  if(length(uniqueExamNames4) > 0) {
    fixNonASCIICharactersFn(searchPattern4)
  }
  
  ####  re-call the names of the unique exams
  
  # re-call the two functions together to make a list of unique exams in the current working directory
  
  # uniqueExamNames <- uniqueNames(getCharts(searchPattern))
  # if(!is.null(searchPattern1)) uniqueExamNames1 <- getCharts(x=searchPattern1, uniqueTests=TRUE)
  # if(!is.null(searchPattern2)) uniqueExamNames2 <- getCharts(x=searchPattern2, uniqueTests=TRUE)
  # if(!is.null(searchPattern3)) uniqueExamNames3 <- getCharts(x=searchPattern3, uniqueTests=TRUE)
  # if(!is.null(searchPattern4)) uniqueExamNames4 <- getCharts(x=searchPattern4, uniqueTests=TRUE)
  # 
  # uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  # if(selectExams != "ALL") uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams] 
  # 
  # print(uniqueExamNames)
  
} # end fixNCCAASCIIExamFiles==TRUE



####   fix the sensor names if necessary   ####



if(fixSensorNames==TRUE) {
  
  print("fix the sensor names if necessary")
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/fixActivitySensorName.R', echo=FALSE)
  
  if(!is.null(searchPattern1)) {
    
    # use this to change the bad Move1 sensor to MoveX to avoid having 2 Move1 sensors
    # fixMove1Fn(x=x="D&+", oldName="Move1", newName="MoveX")
    
    # PLE sensor name should be PLE1
    # fixSensorNameFn(x="D&+", oldSensorName = "  PL", newSensorName = "PLE1")
    
    ### LX4000
    # fixSensorNameFn(x="D&+", oldSensorName = "Cardio1      Move1", 
    #                 newSensorName = "Cardio1      MoveX")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux01", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux02", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux03", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "   SE", newSensorName = "Move1")
    # Cardio1      MoveX
    
    ### LX5000
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux02", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "PLE1", newSensorName = "  PL")
    
    ### LX6
    # fixSensorNameFn(x="D&+", oldSensorName = "Move1", newSensorName = "MoveX")
    # fixSensorNameFn(x="D&+", oldSensorName = "   SE", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "   PL", newSensorName = " PLE1")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux04", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "Move2", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux01", newSensorName = "Move1")
    
    # fixSensorNameFn(x="D&+", oldSensorName = "MS", newSensorName = "SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux01", newSensorName = "   PL")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux03", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "   SM", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "MS", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux03", newSensorName = "   PE")
    # fixSensorNameFn(x="D&+", oldSensorName = "Move2", newSensorName = "   SE")
    
    ### simulation
    # fixSensorNameFn(x="D&+", oldSensorName = "Move2", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "   PL", newSensorName = " PLE1")
    
  }
  
  # Limestone
  if(!is.null(searchPattern2)) {
    fixSensorNameFn(x="D%+", oldSensorName = "Move1", newSensorName = "   SE")
  }
  
  if(!is.null(searchPattern3)) {
    # fixSensorNameFn(x="D#+", oldSensorName = "Move1", newSensorName = "   SE")
    # fixSensorNameFn(x="D#+", oldSensorName = "PPG1", newSensorName = "  PL")
  }
  
  if(!is.null(searchPattern4)) {
    # fixSensorNameFn(x="D\\$+", oldSensorName = "Move1", newSensorName = "   SE")
  }
  
  # fixSensorNameFn(x=searchPattern, oldSensorName = "Move1", newSensorName = "   SE")
  # fixSensorNameFn(x=searchPattern, oldSensorName = "Aux01", newSensorName = "   SE")
  # fixSensorNameFn(x=searchPattern, oldSensorName = "AU", newSensorName = "   SE")
  # fixSensorNameFn(x=searchPattern, oldSensorName = "   SM", newSensorName = "   SE")
  # fixSensorNameFn(x=searchPattern, oldSensorName = "   MS", newSensorName = "   SE")
  
  # fixSensorNameFn(x=searchPattern, oldSensorName = "   PE", newSensorName = "   PL")
  
  # re-call the two functions together to make a list of unique exams in the current working directory
  # uniqueExamNames <- uniqueNames(getCharts(searchPattern))
  if(!is.null(searchPattern1)) uniqueExamNames1 <- getCharts(x=searchPattern1, uniqueTests=TRUE)
  if(!is.null(searchPattern2)) uniqueExamNames2 <- getCharts(x=searchPattern2, uniqueTests=TRUE)
  if(!is.null(searchPattern3)) uniqueExamNames3 <- getCharts(x=searchPattern3, uniqueTests=TRUE)
  if(!is.null(searchPattern4)) uniqueExamNames4 <- getCharts(x=searchPattern4, uniqueTests=TRUE)
  
  uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  if(!exists("selectExams")) { selectExams <- "ALL"}
  if(selectExams[1] != "ALL") uniqueExamNames <- uniqueExamNames[selectExams] 
  print(paste("Fixed", length(uniqueExamNames), "exams"))
  print(uniqueExamNames)
  
} # end if fixSensorNames==TRUE



#######   fix problem character strings in the NCCA ASCII files   ########



{
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/fixStrings.R', echo=FALSE)
  
  # no output from this 
  # fixStringsFn(x="D&+", oldString=" 4KeyR", newString="  4Key")
  
  # fixStringsFn(x="D&+", oldString="-SA2R", newString="---SA")
  # fixStringsFn(x="D&+", oldString="-SAR", newString="--SA")
  # fixStringsFn(x="D&+", oldString="-S2R", newString="--S2")
  # fixStringsFn(x="D&+", oldString="4\\?", newString="4K")
  # fixStringsFn(x="D&+", oldString="-4 \\?", newString="--4K")
  # fixStringsFn(x="D&+", oldString="R 4K\\?", newString="R4 ")
  # fixStringsFn(x="D&+", oldString="   4 \\?", newString="  4K")
  # fixStringsFn(x="D&+", oldString="-S\\?2", newString="--S2")
  # fixStringsFn(x="D&+", oldString="S\\?2", newString=" S2")
  # fixStringsFn(x="D&+", oldString="S\\?2R", newString="  S2")
  # fixStringsFn(x="D&+", oldString="S2R", newString=" S2")
  # fixStringsFn(x="D&+", oldString="SA2R", newString=" SA2")
  
}



#######  locate the NCCAASCII text files in the working directory  #######



# re-initialize an exam from the vector of exam names

{
  
  print("make a vector of exams in the current directory")
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source('~/Dropbox/R/NCCA_ASCII_Parse/getExamNames.R', echo=FALSE)
  
  # call the two functions together to make a list of unique exams in the current working directory
  # uniqueExamNames <- uniqueNames(getCharts(searchPattern))
  if(!is.null(searchPattern1)) uniqueExamNames1 <- getCharts(x=searchPattern1, uniqueTests=TRUE)
  if(!is.null(searchPattern2)) uniqueExamNames2 <- getCharts(x=searchPattern2, uniqueTests=TRUE)
  if(!is.null(searchPattern3)) uniqueExamNames3 <- getCharts(x=searchPattern3, uniqueTests=TRUE)
  if(!is.null(searchPattern4)) uniqueExamNames4 <- getCharts(x=searchPattern4, uniqueTests=TRUE)
  
  uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  # uniqueExamNames <- uniqueExamNames[16]
  # select an exam from the vector of exam names
  if(!exists("selectExams")) { selectExams <- "ALL" }
  # selectExams <- 10
  if(selectExams[1] != "ALL") uniqueExamNames <- 
    c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams]
  
  print(paste("Found", length(uniqueExamNames), "exams"))
  
  print(uniqueExamNames)
  
} 



##################  parse the NCCA ASCII text files  ################



# NCCAASCIIParse <- TRUE


if(NCCAASCIIParse == TRUE) {
  
  print("source the NCCAASCIIParse.R script to parse the data")
  source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCIIParse.R', echo=FALSE)
  
  # print("source the NCCAASCIIParseHelperFunctions.R script")
  # source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCIIParseHelperFunctions.R', echo=FALSE)
  # sourded by the the NCCAASCIIParse.R script
  
  
  # iterate over the vector of unique exam names and parse the headers
  i=1
  for (i in 1:length(uniqueExamNames)) {
    parseUniqueExamHeaders(x=uniqueExamNames[i],
                           saveTXT=saveTXT,
                           saveCSV=saveCSV,
                           makeDF=makeDF,
                           makeVector=makeVector )
  }
  
  
  # iterate over the vector again and parse the time series data
  i=1
  for (i in 1:length(uniqueExamNames)) {
    parseUniqueExamData(x=uniqueExamNames[i],
                        saveTXT=saveTXT,
                        saveCSV=saveCSV,
                        makeDF=makeDF,
                        makeVector=makeVector )
  }
  
  print (paste(length(uniqueExamNames), "exams parsed"))
  print(uniqueExamNames)
  
  
} 



####################   parse the stimulus events   #####################



# first source the NCCAASCIIParseHelperFunctions.R script
# # print("source the NCCAASCIIParseHelperFunctions.R script")
# source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCIIParseHelperFunctions.R', echo=FALSE)



if(stimulusParse == TRUE) {
  
  # check whether all charts in each series have the same stimulus events
  # this will set the length of the stimulus sequence to the max length
  # for all charts in each series
  # will also check whether CQTs have at least 2 CQs and 2 RQs
  # x input is a character string to identify the Stimuli data frames
  # output is a data frame (_eventMatrix), assigned to the global env with all event labels for all charts
  
  print("check whether all charts in each series have the same stimulus events")
  eventParseOutMsg <- eventParse(x="_Stimuli$", makeDF=TRUE, saveCSV=FALSE)
  
  print(eventParseOutMsg)
  
  # call a function to check whether stimulus events are identical for all charts in each series
  # x input is a character string to identify the Stimuli data frames
  # output is a data frame (_stim_text_warning) and csv warning regarding differences 
  # where TRUE indicates the stim text is identical
  
  print("check whether stimulus events are identical for all charts in each series")
  stimCheckOutMsg <- stimCheck(x="_Stimuli$", makeDF=TRUE, saveCSV=FALSE)
  # , makeDF=makeDF, saveCSV=saveCSV
  
  print(stimCheckOutMsg)
  
  
}



############################   clean up   #############################



cleanUp <- TRUE

# does not remove uniqueExamNames

if(cleanUp == TRUE) {
  
  # clean up functions
  
  rm(chartHeader)
  rm(dataFile)
  rm(dataParse)
  rm(eventTable)
  rm(headerFile)
  rm(stimEvents)
  
  if(exists("uniqueNames")) rm(uniqueNames)
  
  # rm(cleanUp)
  # rm(fixDuplicatesFn)
  # rm(fixDup)
  # rm(centerColumn)
  # rm(setColRange)
  
  # clean up data vectors
  rm(list = ls(pattern = "_header"))
  rm(list = ls(pattern = "_data"))
  
}

rm(cleanUp)



##############  process the data in the global environment  #############



# make a function to make a list of unique exams in the global environment
getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }

# get exam names from the _Data data frames
print("make a list of unique exams in the global environment")
uniqueExams <- getUniqueExams(x="*_Data$")
# uniqueExams <- uniqueExams[1]

### select an exam from the vector of exam names

selectExams <- "ALL"

if(selectExams != "ALL") {
  if(!exists("selectExams")) selectExams <- "ALL"
  if(length(selectExams) > 1) {
    uniqueExams <- uniqueExams[selectExams]
  }
}



print(paste(length(uniqueExams), "unique exams selected"))
print(uniqueExams)



# Honduras 20160527ACQT
# artifacted cardio 6-14-2016
# uniqueExams <- uniqueExams[c(3,6,7,9,12,13,14,15)]
# un-artifacted cardio 6-14-2016
# uniqueExams <- uniqueExams[c(1,2,4,,5,8,10,11,16)]
# artifacted activity sensor
# uniqueExams <- uniqueExams[c(1,3,15)]
# normal activity sensor
# uniqueExams <- uniqueExams[c(2,3,4,5,7,8,9,10,11,12,14,16)]
# clean activity
# uniqueExams <- uniqueExams[c(3,4,16)]



# print("unique exams in this directory")
# print(uniqueExams)



# ############## fix sensor column names - not used ##################
# 
# 
# 
# if(fixSensorNames==TRUE) {
#   
#   i=1
#   for(i in 1:length(uniqueExams)) {
#     
#     examName <- uniqueExams[i]
#     assign("examName", examName, pos=1)
#     print(examName)
#     
#     # get the names of time series lists for all unique series in each exam
#     # searchString <- paste0("*", examName, "_Data", "*")
#     # examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
#     examDF <- get(paste0(examName, "_Data"), pos=1)
#     
#     names(examDF)
#     
#   }
#     
#     fixSensorNameFn(x="D#+", oldSensorName = "Move1", newSensorName = "   SE")
#   fixSensorNameFn(x="D#+", oldSensorName = "PPG1", newSensorName = "  PL")
#   
#   
#   
#   fixSensorNameFn <- function(x=searchPattern, oldSensorName="Aux01", newSensorName="   SE") {
#     
#     fileNames <- list.files(pattern = x)
#     
#     oldName <- oldSensorName
#     newName <- newSensorName
#     
#     for (i in 1:length(fileNames)) {
#       x <- readLines(fileNames[i], encoding="latin1")
#       
#       x[10:150] <- gsub(oldName, newName, x[10:150])
#       
#       writeLines(x, fileNames[i])
#     }
#     
#   } # end fixSensorNameFn() function
#   
# }


########################  data pre-processing   ########################



# source the pre-processing script 
# to center the onset at zero
# set the range for each sensor at 1 to 1000
# add some columns for events
# add eventLabels and event Text

# source the sigProcHelper.R script to load the getFirstLastEventFn()
# source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)

if(NCCAASCIIParse == TRUE) {
  
  print("pre-processing")
  source('~/Dropbox/R/NCCA_ASCII_Parse/preProc.R', echo=FALSE)
  # preProc(x=uniqueExams, makeDF=makeDF, output=output) 
  
  dataNames <- preProc(x=uniqueExams, makeDF=makeDF, output=output)
  
  print(dataNames)
  
  print (paste(length(uniqueExams), "exams pre-processed"))
  
}



################   fix names of repeated stimulus events   ##############



# fix repeated questions by appending to the name of the repetition 
# for each chart in the exam data frame

# first source the NCCAASCIIParseHelperFunctions.R script to load the fixDupFn() function


if(fixDuplicateTags == TRUE) {
  
  # then source the fixDup.R script 
  # to call the fixDupFn() recursively with the fixDuplicatesFn()
  source('~/Dropbox/R/NCCA_ASCII_Parse/fixDup.R', echo=FALSE)
  print("fix duplicated or repeated stmulus event names")
  
  ### this is done somewhere else in the sigProc.R script
  
  dataNames <- fixDuplicatesFn(x=uniqueExams, 
                               makeDF=makeDF, 
                               showNames=showNames, 
                               output=output)
  
  print(dataNames)
  
  print (paste0("checked for duplicate event labels in ",length(uniqueExams), " exams"))
  
}



################  save the data for each exam in the cwd  ###############



# save the data for each exam in .Rda .RData format

# saveRDA1 <- TRUE

if(!exists("saveRDA1")) saveRDA1 <- FALSE

if(saveRDA1==TRUE) {
  
  # make the file names
  # uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  uniqueExams1 <- gsub("D&_", "", uniqueExamNames)
  uniqueExams1 <- gsub("D\\$_", "", uniqueExams1)
  uniqueExams1 <- gsub("D%_", "", uniqueExams1)
  uniqueExams1 <- gsub("D#_", "", uniqueExams1)
  uniqueExams1 <- gsub("\\$", "", uniqueExams1)
  uniqueExams1 <- gsub("\\.", "_", uniqueExams1)
  uniqueExams1 <- gsub("#", "", uniqueExams1)
  uniqueExams1 <- gsub("%", "", uniqueExams1)
  # uniqueExams1 <- gsub("[:punct:]", "", uniqueExams1) # gsub does not work with [:punct:]
  uniqueExams1 <- str_replace_all(uniqueExams1, "[:punct:]", "")
  # uniqueExams1 <- uniqueExams1[c(3,5)]
  
  uniqueExamNames2 <- uniqueExamNames
  uniqueExamNames2 <- gsub("D&_", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("D\\$_", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("\\$", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("D%_", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("D#_", "", uniqueExamNames2)
  uniqueExamNames2 <- str_replace_all(uniqueExamNames2, "[:punct:]", "")
  
  i=1
  for (i in 1:length(uniqueExamNames)) {
    
    save(list=ls(pattern=paste0("^",uniqueExamNames2[i],"_"), pos=1), 
         file=paste0(uniqueExams1[i], "_1.Rda") )
    
  }
  
  # rm(uniqueExams)
  
  rm(i)
  
} # end if saveRDA1 == TRUE

# if(exists("saveRDA1")) rm(saveRDA1)



#######   loadRDA1 the Rda data from the current working directory   #######



# loadRDA1 <- TRUE

# myRdaNames <- list.files(pattern="*_1.Rda$")

# myRdaNames <- gsub("\\$", "\\\\$", myRdaNames)
# myRdaNames <- myRdaNames[2
# myRdaNames <- gsub("\\.Rda", ".Rda", myRdaNames)

if(!exists("loadRDA1")) loadRDA1 <- FALSE

if(loadRDA1==TRUE) {
  print("load the exams from the current directory")
  
  # rm(list=ls())
  if(!exists("selectExams")) { selectExams <- "ALL" }
  rmVector <- ls()[-(which(ls() == "selectExams"))]
  if(exists("uniqueExamNames")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames")] }
  if(exists("uniqueExamNames2")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames2")] }
  if(exists("processData")) { rmVector <- rmVector[-which(rmVector=="processData")] }
  if(exists("saveRDA1")) { rmVector <- rmVector[-which(rmVector=="saveRDA1")] }
  if(exists("loadRDA1")) { rmVector <- rmVector[-which(rmVector=="loadRDA1")] }
  if(exists("saveRDA2")) { rmVector <- rmVector[-which(rmVector=="saveRDA2")] }
  if(exists("loadRDA2")) { rmVector <- rmVector[-which(rmVector=="loadRDA2")] }
  if(exists("getESSScores")) { rmVector <- rmVector[-which(rmVector=="getESSScores")] }
  if(exists("getOSS2Scores")) { rmVector <- rmVector[-which(rmVector=="getOSS2Scores")] }
  if(exists("extractFeatures")) { rmVector <- rmVector[-which(rmVector=="extractFeatures")] }
  
  if(exists("getScores")) { rmVector <- rmVector[-which(rmVector=="getScores")] }
  if(exists("processArtifacts")) { rmVector <- rmVector[-which(rmVector=="processArtifacts")] }
  
  if(exists("printCharts")) { rmVector <- rmVector[-which(rmVector=="printCharts")] }
  if(exists("printSegments")) { rmVector <- rmVector[-which(rmVector=="printSegments")] }
  if(exists("fixNCCAASCIIExamFiles")) { rmVector <- rmVector[-which(rmVector=="fixNCCAASCIIExamFiles")] }
  if(exists("saveMeasurements")) { rmVector <- rmVector[-which(rmVector=="saveMeasurements")] }
  rm(list=rmVector)
  
  myRdaNames <- list.files(pattern="*_1.Rda$")
  if(selectExams != "ALL") {
    myRdaNames <- myRdaNames[selectExams]
  }
  
  # comment this out when loading exams from the global env
  # if(exists("uniqueExamNames2")) {
  #   myRdaNames <- myRdaNames[str_sub(myRdaNames, 1, -7) %in% uniqueExamNames2]
  # }
  
  # rm(list=c(
  #   ls(pattern="^.*_Measurements$"),
  #   ls(pattern="^.*_Stimuli$"),
  #   ls(pattern="^.*_Data$"),
  #   ls(pattern="^.*_Header$"),
  #   ls(pattern="^.*_eventMatrix$"),
  #   ls(pattern="^.*_event_warnings$"),
  #   ls(pattern="^.*_stim_text_warning$"),
  #   ls(pattern="^.*_stim_warnings$")
  # ) )
  
  i=1
  for(i in 1:length(myRdaNames)) {
    # there are some problems with load(file=myRdaNames)
    load(file=myRdaNames[i])
  }
  
  print(paste(length(myRdaNames), "exams found"))
  
  rm(myRdaNames)
  rm(i)
  
  # loadRDA1 <- TRUE
  
  # processData <- TRUE
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
  # make a function to make a list of unique exams in the global environment
  getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  # get exam names from the _Data data frames
  print("make a list of unique exams in the global environment")
  uniqueExams <- getUniqueExams(x="*_Data$")
  # uniqueExams <- uniqueExams[1]
  
  print(paste(length(uniqueExams), "exams in the global environment"))
  
  # select an exam from the vector of exam names
  # selectExams <- "ALL"
  # selectExams <- 5
  
  if(selectExams[1] != "ALL") {
    if(!exists("selectExams")) selectExams <- "ALL"
    if(length(uniqueExams) > 1) {
      uniqueExams <- uniqueExams[selectExams]
    }
  }
  
  print(paste(length(uniqueExams), "exams selected"))
  
  print(uniqueExams)
  
  # print(uniqueExamNames)
  
} # end if loadRDA1 == TRUE

# rm(loadRDA1)
# rm(searchPattern)



#####################   signal processing    #####################



if(processData == TRUE) {
  
  
  # use EDAFilt="resp" to extract the respiration signal from the EDA data
  # EDAFilt="resp"
  # EDAFilt="laf"
  # EDAFilt="test"
  
  # This is where we need to know the instrument type for each exam
  # make a vector of instrument types for the vector of unique exams
  
  # source the scripts with signal processing functions for the sensors
  # source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoSigProc.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/EDASigProc.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/cardioSigProc.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/FCSigProc.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/eCardioSigProc.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/PLESigProc.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/activitySigProc.R', echo=FALSE)
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/EDARespFilters.R', echo=FALSE)
  
  # source the sigProcHelper script to load the helper functions for signal processing
  # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc_extra.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc.R', echo=FALSE)
  
  
  
  # lowPass <- lowPass8th2hz_resp
  # highPass <- highPass8th1hz_resp
  # EDABandPassFilter <- bandPass8th.5hz2hz_resp
  
  # turn off the EDA and pneumo filters for Axciton data
  # may need to use this switch if the global env is cleared or the init is reloaded
  # useFilters=FALSE
  # if(useFilters==FALSE) {
  #   EDAHighPass=FALSE
  #   EDALowPass=FALSE
  #   PneumoLowPass=FALSE
  # }
  
  # print(paste("EDA Filter:", EDAFilt))
  
  # uniqueExamNames
  
  if(!exists(uniqueExamNames)) uniqueExamnames <- NULL
  
  sigProc(x=uniqueExams, y=uniqueExamNames, makeDF=makeDF)
  
}



#################   scale and offset the data for display   ################



# need to move to later, and scale the data for analysis here


# if(processData == TRUE) {
# 
# 
#   # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
#   # source this for extract DSP filters
#   # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc_extra.R', echo=FALSE)
# 
#   # for testing EDA filters
#   # # EDAFilt="test"
#   # source('~/Dropbox/R/NCCA_ASCII_Parse/EDASigProc.R', echo=FALSE)
# 
#   # 8-12-2017 moved from the NCCAASCIIParse.R script
#   source('~/Dropbox/R/NCCA_ASCII_Parse/scaleOffsetData.R', echo=FALSE)
# 
#   ScaleOffsetDataFn(x=uniqueExams,
#                     makeDF=makeDF,
#                     saveCSV=TRUE,
#                     showNames=showNames,
#                     output=output )
# 
# }



######################   artifact processing   ######################



source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)

source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)



# not presently used
# instead artifacts are processed separately for each sensor

if(processArtifacts == TRUE) {
  
  print("artifact extraction")
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
  #
  # source('~/Dropbox/R/NCCA_ASCII_Parse/TukeyFences.R', echo=FALSE)
  #
  #
  # source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoArtifact.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/edaArtifact.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/cardioArtifact.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/pleArtifact.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/activityArtifact.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/artifactProc.R', echo=FALSE)
  
  # call the function to reset the artifact_a column to 0
  # artifactProc(x=uniqueExams)
  
}



#####################   check for physical activity   ##################


if(processArtifacts == TRUE && activityArtifacts == TRUE) {
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/activityCheck.R', echo=FALSE)
  
  print("check for activity sensor artifacts")
  chartFUN <- activityCheck
  getExamFn(x=uniqueExams)
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot.R', echo=FALSE)
  
}



#######################   EDA artifacts   #########################



if(processArtifacts == TRUE && edaArtifacts == TRUE) {
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/dataCheck.R', echo=FALSE)
  
  print("check for unresponsive EDA data")
  chartFUN <- dataCheckFn
  getExamFn(x=uniqueExams)
  
  # reset the warnings
  # assign("last.warning", NULL, envir = baseenv())
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/nonstimArtifacts.R', echo=FALSE)
  
  print("check for non-specific EDA responses")
  chartFUN=nonStimArtifactFn
  getExamFn(x=uniqueExams)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/EDAMvtArtifact.R', echo=FALSE)
  
  print("check for finger movement artifacts in the EDA data")
  chartFUN=EDAMvtArtifactFn
  getExamFn(x=uniqueExams)
  
}



#######################   pneumo artifacts   #######################



if(processArtifacts == TRUE && pneumoArtifacts == TRUE) {
  
  sec <- .5
  cutProp <- .25
  # sec=3 cutProp=.5 works well 3-18-2017
  # sec=1 cutProb=.25 also works ok
  
  
  # sec2=4 cutProp2=.5 works well 3-19-2017
  sec2 <- 3
  cutProp2 <- .01
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoCheck.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/tukeyFences.R', echo=FALSE)
  
  print("check for unresponsive respiration data")
  chartFUN <- pneumoCheckFn
  getExamFn(x=uniqueExams)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoArtifact.R', echo=FALSE)
  
  print("respiration artifacts")
  chartFUN=pneumoArtifactFn
  getExamFn(x=uniqueExams)
  
}



########################   cardio artifacts   ##########################



if(processArtifacts == TRUE && cardioArtifacts == TRUE) {
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/cardioArtifact.R', echo=FALSE)
  
  print("check for artifacts in the cardio data")
  chartFUN=cardioArtifactFn
  getExamFn(x=uniqueExams)
  
}



########################   PLE artifacts   ##########################


if(processArtifacts == TRUE && pleArtifacts == TRUE) {
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/pleArtifact.R', echo=FALSE)
  
  print("PLE artifacts")
  chartFUN=pleArtifactFn
  getExamFn(x=uniqueExams)
  
}


########################   feature extraction   #######################



if(extractFeatures == TRUE) {
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
  # source this also 
  # for the ratePerMin() function
  source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
  
  # integrateEDAArtifacts <- FALSE
  
  # source the scripts with the helper functions
  source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoMeasurement.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/slopeChange.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtract.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractOSS2.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/PLEMeasurement.R', echo=FALSE)
  
  # source the scripts for each sensor
  source('~/Dropbox/R/NCCA_ASCII_Parse/PneumoExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/EDAExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/CardioExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/newFCExtract.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/FCExtract.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/PLEExtract.R', echo=FALSE)
  
  # source the feature extraction function
  source('~/Dropbox/R/NCCA_ASCII_Parse/featureExtraction.R', echo=FALSE)
  
  print("feature extraction")
  featureExtraction(x=uniqueExams)
  
}



###########################   measurements   ############################



if(getScores == TRUE) { 
  
  ### make a _Measurements data frame for each exam
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  
  print("make a data frame of _Measurments for each exam")
  
  # source the extractMeasurements.R script to get the Kircher measurements
  source('~/Dropbox/R/NCCA_ASCII_Parse/extractMeasurements.R')
  
  extractMeasurementsFn(x=uniqueExams, writeCSV=FALSE, CSVName="")
  
}



##########################   Scores   #########################

if(getScores == TRUE) {
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  
  # source the amplitudeExtractHelperFunctions.R script to load the spd function
  # for the population st dev
  source('~/Dropbox/R/NCCA_ASCII_Parse/amplitudeExtractHelperFunctions.R', echo=FALSE)
  
  # output functions for score and measurement tables
  source('~/Dropbox/R/NCCA_ASCII_Parse/outputScores.R', echo=FALSE)
  
  # source some scripts to calculate various types of rank scores
  source('~/Dropbox/R/NCCA_ASCII_Parse/rankScores.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/RRMScore.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/miritelloRank.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/ipsativeZScore.R', echo=FALSE)
  
  # source some scripts for the scoring algorithms
  source('~/Dropbox/R/NCCA_ASCII_Parse/ESSScores.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/OSS3Scores.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/OSS2Scores.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/PAScores.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/ROSSScores.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/PSSScores.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/HDBootstrapScores.R', echo=FALSE)
  
  # decision rules for scoring algorithms 
  source('~/Dropbox/R/NCCA_ASCII_Parse/decisionRules.R', echo=FALSE)
  
  #### PCASS algorithm
  
  
  # source the script to load the getScoresFn() to calculate all numerical scores
  source('~/Dropbox/R/NCCA_ASCII_Parse/scores.R', echo=FALSE)
  
  # used to save the rank score table
  # saveCSV <- TRUE
  
  
  print("score the data")
  
  # load the getExamFn
  source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)
  
  # declare this after sourciong the getSegment.R script  
  # chartFUN <- getScoresFn
  seriesFUN <- getScoresFn
  # this will be called using do.call() which can take function or character string as the first arg
  
  # i=1 length(uniqueExams)
  for(i in 1:length(uniqueExams)) {
    getExamFn(x=uniqueExams[i])
  }
  
  # write.csv(measurementDF, file = "QC_MeasurementDF.csv", row.names = TRUE)
  
  
  
  ################# PCASS Scores ########################
  
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/PCASS_feature_extraction.R')
  
  if(isTRUE(PCASSFormat)) {
    PCASSAlgorithmFn(x=uniqueExams)
  }
  
  
  ############### locate and aggregate the seriesTotals.csv  ##################
  
  
  
  # only when all exams have the same format
  
  # seriesTotalsDFNames <- ls(pattern=".ESSMSeriesTotalsDF$")
  # seriesTotalsSummaryDF <- NULL
  # if(length(seriesTotalsDFNames) > 0) {
  #   i=5
  #   for(i in 1:length(seriesTotalsDFNames)) {
  #     seriesTotalsSummaryDF <- rbind(seriesTotalsSummaryDF, get(seriesTotalsDFNames[i]))
  #   }
  #   write.csv(seriesTotalsSummaryDF, file="seriesTotals.csv", row.names=FALSE)
  # }
  
} 



######################   save measurements   #######################



# if(getScores == TRUE) {
#   
#   saveMeasurements <- TRUE
#   
#   if(saveMeasurements == TRUE) { 
#     
#     ### save the _Measurements data frame to a .csv for each chart for each exam
#     
#     # source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
#     
#     print("save the _Measurments to a .CSV file")
#     # source the measurementsScores.R script to get the Kircher measurements
#     source('~/Dropbox/R/NCCA_ASCII_Parse/measurementsScores.R')
#     
#     examFUN <- measurementsScoresFn
#     
#     print("save the measurements and score to a .csv file")
#     getExamFn(x=uniqueExams)
#     
#   }
#   
# }



###############  save the data for each exam in the cwd   ################



### save the data for each exam in .Rda .RData format


# library(stringr)

# saveRDA2 <- FALSE
# saveRDA2 <- TRUE


if(!exists("saveRDA2")) saveRDA2 <- FALSE

if(saveRDA2==TRUE) {
  
  # fix the file names
  # uniqueExamNames <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  uniqueExamNames2 <- gsub("\\$", "\\\\$", uniqueExams)
  uniqueExamNames2 <- gsub("D\\$_", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("D&_", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("D%_", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("D#_", "", uniqueExamNames2)
  uniqueExamNames2 <- gsub("D&_", "", uniqueExamNames2)
  # use of [:punct:] seems to cause a problem with some file names
  # uniqueExamNames2 <- gsub("[:punct:]", "", uniqueExamNames2)
  #
  uniqueExams <- gsub("D&_", "", uniqueExamNames2)
  uniqueExams <- gsub("D\\$_", "", uniqueExams)
  uniqueExams <- gsub("D%_", "", uniqueExams)
  uniqueExams <- gsub("D#_", "", uniqueExams)
  uniqueExams <- gsub("\\$", "", uniqueExams)
  uniqueExams <- gsub("\\.", "_", uniqueExams)
  uniqueExams <- gsub("#", "", uniqueExams)
  uniqueExams <- gsub("%", "", uniqueExams)
  # uniqueExams <- gsub("[:punct:]", "", uniqueExams)
  # uniqueExams <- uniqueExams[c(3,5)]
  
  # then iterate over the unique exams in the global environment
  i=1
  for (i in 1:length(uniqueExams)) {
    save(list=ls(pattern=uniqueExamNames2[i], pos=1), file=paste0(uniqueExams[i], "_2.Rda"))
  }
  # rm(uniqueExams)
  rm(i)
}

# rm(saveRDA2)



######  loadRDA2 the data from the current working directory - again  ###### 



# loadRDA2 <- FALSE
# loadRDA2 <- TRUE

# if(!exists("selectExams")) selectExams <- "ALL"

if(!exists("loadRDA2")) loadRDA2 <- TRUE


if(loadRDA2==TRUE) {
  
  if(!exists("selectExams")) { selectExams <- "ALL" }
  rmVector <- ls()[-(which(ls() == "selectExams"))]  
  if(exists("uniqueExamNames")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames")] }
  if(exists("uniqueExamNames2")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames2")] }
  if(exists("processData")) { rmVector <- rmVector[-which(rmVector=="processData")] }
  if(exists("saveRDA1")) { rmVector <- rmVector[-which(rmVector=="saveRDA1")] }
  if(exists("loadRDA1")) { rmVector <- rmVector[-which(rmVector=="loadRDA1")] }
  if(exists("getESSScores")) { rmVector <- rmVector[-which(rmVector=="getESSScores")] }
  if(exists("getOSS2Scores")) { rmVector <- rmVector[-which(rmVector=="getOSS2Scores")] }
  if(exists("getScores")) { rmVector <- rmVector[-which(rmVector=="getScores")] }
  if(exists("processArtifacts")) { rmVector <- rmVector[-which(rmVector=="processArtifacts")] }
  if(exists("extractFeatures")) { rmVector <- rmVector[-which(rmVector=="extractFeatures")] }
  
  # if(exists("extractFeatures")) { rmVector <- rmVector[-which(rmVector=="x")] }
  # if(exists("extractFeatures")) { rmVector <- rmVector[-which(rmVector=="y")] }
  
  if(exists("saveRDA2")) { rmVector <- rmVector[-which(rmVector=="saveRDA2")] }
  if(exists("loadRDA2")) { rmVector <- rmVector[-which(rmVector=="loadRDA2")] }
  if(exists("printCharts")) { rmVector <- rmVector[-which(rmVector=="printCharts")] }
  if(exists("printSegments")) { rmVector <- rmVector[-which(rmVector=="printSegments")] }
  if(exists("fixNCCAASCIIExamFiles")) { rmVector <- rmVector[-which(rmVector=="fixNCCAASCIIExamFiles")] }
  if(exists("saveMeasurements")) { rmVector <- rmVector[-which(rmVector=="saveMeasurements")] }
  
  rm(list=rmVector)
  
  myRdaNames <- list.files(pattern="*_2.Rda$")
  if(selectExams != "ALL") {
    myRdaNames <- myRdaNames[selectExams]
  }
  
  # if(exists("uniqueExamNames2")) {
  #   myRdaNames <- myRdaNames[str_sub(myRdaNames, 1, -7) %in% uniqueExamNames2]
  # }
  
  i=1
  for(i in 1:length(myRdaNames)) {
    load(file=myRdaNames[i])
  }
  
  rm(myRdaNames)
  rm(i)
  
  #### re-select the exams in the working directory  
  
  library(stringr)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R', echo=FALSE)
  
  source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)
  source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
  
  #### get exam names from the _Data data frames
  
  # make a function to make a list of unique exams in the global environment
  getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  
  print("make a list of unique exams in the global environment")
  uniqueExams <- getUniqueExams(x="*_Data$")
  # uniqueExams <- uniqueExams[1]
  
  print(paste(length(uniqueExams), "exams in the global environment"))
  
  # select an exam from the vector of exam names
  # selectExams <- "ALL"
  # selectExams <- 2
  
  if(!exists("selectExams")) selectExams <- "ALL"
  
  if(selectExams[1] != "ALL") {
    if(!exists("selectExams")) selectExams <- "ALL"
    if(length(uniqueExams) > 1) {
      uniqueExams <- uniqueExams[selectExams]
    }
  }
  
  print(paste(length(uniqueExams), "exams selected"))
  print(uniqueExams)
  
} # end if(loadRDA2 == TRUE)

# rm(loadRDA2)



#################   scale and offset the data for display   ################



# if(processData == TRUE) {
#   
#   
#   # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
#   # source this for extract DSP filters
#   # source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc_extra.R', echo=FALSE)
#   
#   # for testing EDA filters
#   # # EDAFilt="test"
#   # source('~/Dropbox/R/NCCA_ASCII_Parse/EDASigProc.R', echo=FALSE)
#   
#   # 8-12-2017 moved from the NCCAASCIIParse.R script
#   source('~/Dropbox/R/NCCA_ASCII_Parse/scaleOffsetData.R', echo=FALSE)
#   
#   ScaleOffsetDataFn(x=uniqueExams, 
#                     makeDF=makeDF, 
#                     showNames=showNames, 
#                     output=output )
#   
# }



#########   save / load   #############



# save.image()
# load(".RData")



####



# use this to select an exam, series and chart
# getSegFn(exam=1,series=1,chart=2)




############################  printing   ##############################



if(!exists("printCharts")) printCharts <- TRUE

# source the chartPlot.R script to print the plots
# source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot_init.R')
if(isTRUE(printCharts)) {
  source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot_gg.R', echo=FALSE)
}



if(!exists("printSegments")) printSegments <- TRUE

# plot the segments
# source('~/Dropbox/R/NCCA_ASCII_Parse/segmentPlot_init.R')
if(isTRUE(printSegments)) {
  source('~/Dropbox/R/NCCA_ASCII_Parse/segmentPlot_gg.R', echo=FALSE)
}



############ end work flow ##############
################################################################



############# summarize the ESS-M results ################



summarizeResults <- FALSE


if(isTRUE(summarizeResults)) {
  
  library(stringr)
  
  # get the criterion state for all exams
  criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"), header=TRUE)
  
  numbCases <- nrow(criterionStateDF)
  
  # get the ESS-M series totals for all 
  seriesTotalFiles <- list.files(pattern=paste0("ESSMSeriesTotals.csv"))
  
  # set these for the test format
  # testFormat <- "Utah"
  testFormat <- str_sub(seriesTotalFiles, 9, -24)
  
  # initialize a data frame to aggregate the series totals
  RQNames <- c("R1", "R2", "R3", "R4")
  seriesTotalsDF <- as.data.frame(matrix(ncol=(5+length(RQNames)), 
                                         nrow=length(seriesTotalFiles)))
  names(seriesTotalsDF) <- c("ID", "series", "testFormat", RQNames, "grandTotal", "criterionState")
  # View(seriesTotalsDF)
  
  # iterate over the series totals to aggregate the totals for all exams
  i=4
  for (i in 1:length(seriesTotalFiles)) {
    thisCSV <- read.csv(seriesTotalFiles[i], header=TRUE)
    # View(thisCSV)
    thisID <- str_sub(seriesTotalFiles[i], 2, 8)
    seriesTotalsDF[i,'ID'] <- thisID
    thisSeries <- str_sub(seriesTotalFiles[i], -22, -22)
    seriesTotalsDF[i,'series'] <- thisSeries
    seriesTotalsDF[i,'testFormat'] <- testFormat[i]
    seriesTotalsDF[i,4:(4+ncol(thisCSV)-3)] <- thisCSV[1,3:(3+ncol(thisCSV)-3)] 
    seriesTotalsDF[i,'criterionState'] <- 
      criterionStateDF$criterionState[criterionStateDF$ID %in% thisID]
  }
  
  # calculate the grand total
  seriesTotalsDF$grandTotal <- 
    apply(seriesTotalsDF[,4:(4+length(RQNames)-1)], 1, sum, na.rm=TRUE)
  
  ### define some functions for decision rules
  
  TSRFn <- function(x) {
    # two-stage-rule
    # x input is a vector of 
    ifelse(sum(x, na.rm=TRUE) <= -3, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -7, 
                  -1,
                  ifelse(sum(x, na.rm=TRUE) >= 3,
                         1,
                         0 ) ) )
  }
  
  SSRFn <- function(x) {
    # subtotal-score-rule
    ifelse(min(x[1:(length(x))], na.rm=TRUE) <= -3, 
           -1,
           ifelse(min(x[1:(length(x))], na.rm=TRUE) >= 3,
                  1,
                  0 )
    )
  }
  
  seriesTotalsDF$Result <- apply(seriesTotalsDF[,4:7], 1, TSRFn)
  # seriesTotalsDF$Result <- apply(seriesTotalsDF[,3:(ncol(seriesTotalsDF)-1)], 1, SSRFn)
  
  ### add the correct codes here
  
  # define a function
  correctCodesFN <- function(x) {
    # x input is a vector of 2 items
    # the first is the criterion state [1, -1]
    # the second is the test result [-1, 0, 1]
    ###
    state <- x[1]
    result <- x[2]
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
  }
  
  # apply the function to he seriesTotalsDF
  seriesTotalsDF$correctCode <- apply(seriesTotalsDF[,c(9,10)],
                                      1, correctCodesFN)
  
  # write the CSV
  write.csv(seriesTotalsDF, row.names=FALSE, 
            file=paste("ALL_CASES", length(seriesTotalFiles), "seriesTotals.csv", sep="_") )
  
}



####################  summarize the sensor totals  #######################



summarizeSensorInfo <- FALSE


if(isTRUE(summarizeSensorInfo)) {
  
  # initialize a vector of file names
  sensorTotalsFiles <- list.files(pattern="ESSMSensorTotals.csv")
  
  # get the criterion state for all exams
  criterionStateDF <- read.csv(list.files(pattern="criterionState.csv"), 
                               header=TRUE, stringsAsFactors=FALSE)
  
  # testFormat <- "Utah"
  testFormat <- str_sub(sensorTotalsFiles[], 9, -24)
  
  # initialize a data frame 
  sensorTotalsDF <- as.data.frame(matrix(ncol=6, nrow=length(sensorTotalsFiles)))
  names(sensorTotalsDF) <- c("ID", "testFormat", "Pneumo", "EDA", "Cardio", "PLE")
  sensorTotalsDF$testFormat <- testFormat
  # View(sensorTotalsDF)
  
  # calculate the grand total
  # sensorTotalsDF$grandTotal <- 
  # apply(sensorTotalsDF[,3:(3+length(RQNames)-1)], 1, sum, na.rm=TRUE)
  
  sensorTotalsDF$grandTotal <- NA
  sensorTotalsDF$criterionState <- NA
  
  # iterate over the sensorTotalsFiles
  i=1
  for(i in 1:length(sensorTotalsFiles)) {
    theseSensorTotals <- read.csv(sensorTotalsFiles[i], 
                                  header=TRUE, 
                                  stringsAsFactors=FALSE)
    thisID <- str_sub(theseSensorTotals$examName[1], 2, 8)
    # View(thisID)
    sensorTotalsDF[i,'ID'] <- thisID
    sensorTotalsDF[i,3:6] <- colSums(theseSensorTotals[,4:7], na.rm=TRUE)
    sensorTotalsDF[i,'criterionState'] <- 
      criterionStateDF$criterionState[criterionStateDF$ID %in% thisID]
  }
  
  # calculate the grand total
  sensorTotalsDF$grandTotal <-
    apply(sensorTotalsDF[,3:(3+length(RQNames)-1)], 1, sum, na.rm=TRUE)
  
  # get the result from the seriesTotalsDF
  j=1
  for(j in 1:nrow(sensorTotalsDF)) { 
    thisID <- sensorTotalsDF$ID[j]
    sensorTotalsDF$Result[j] <- 
      seriesTotalsDF$Result[seriesTotalsDF$ID %in% thisID]
  }
  
  # summarize the sensor totals for guilty and innocent cases
  
  guiltyDF <- sensorTotalsDF[sensorTotalsDF$criterionState == -1,]
  guiltySummary <- summary(guiltyDF)
  
  innocentDF <- sensorTotalsDF[sensorTotalsDF$criterionState == 1,]
  innocentSummary <- summary(innocentDF)
  
  # add the correct code the sensorTotalsDF
  sensorTotalsDF$correctCode <- seriesTotalsDF$correctCode
  
  # write the CSV
  write.csv(sensorTotalsDF, row.names=FALSE, 
            file=paste("ALL_CASES", length(sensorTotalsFiles), "sensorTotals.csv", sep="_"))
  
  # sensorTotalsDF <- read.csv(list.files(pattern="sensorTotals.csv"), header=TRUE, stringsAsFactors=FALSE )
  
  print(guiltySummary)
  print(innocentSummary)
  
}



########### summarize the accuracy of results ############


summarizeAccuracy <- FALSE



if(isTRUE(summarizeAccuracy)) {
  
  # x <- seriesTotalsDF$correctCode
  
  # define a function
  accySumFn <- function(x) {
    # summarize the ccuracy from the samo\ple accuracy codes
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
    accuracySummary['CorI'] <- length(which(x == 2)) / ( nG - length(which(x == 6)) )
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
    
    return(accuracySummary)
  }
  
  ACCY <- accySumFn(x=seriesTotalsDF$correctCode)
  
  print(ACCY)
  
}



############# summarize the proportions of scores #################



summarizeScoresProp <- FALSE

if(isTRUE(summarizeAccuracy)) {
  
  library(readr)
  
  ### use the sensorTotalsDF to summarize the scores
  
  sensorSums <- colSums(abs(sensorTotalsDF[,3:6])) / 
    sum(colSums(abs(sensorTotalsDF[,3:6]))) 
  
  print(sensorSums)
  
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
    
    # thisCaseDF <- read_csv(scoreSheetFiles[i])
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
                                         scoreSheetFreqDF[i,'PLE'] )
    
    
  }
  
  outputVector <- 
    colSums(scoreSheetFreqDF[,2:5]) / colSums(scoreSheetFreqDF)['nScores']
  
  sum(outputVector)
  
  outputVector * c(1, 2, 1, 1)
  
  scoresProp <- outputVector * c(1, 2, 1, 1) / 
    sum(outputVector * c(1, 2, 1, 1))
  
  print(scoresProp)
  
}



##### correlations ########


getCorrelations <- FALSE

if(isTRUE(getCorrelations)) {
  
  numbCases <- nrow(criterionStateDF)
  
  ALL_CASES_sensorTotals <- read_csv(paste0("ALL_CASES_",
                                            numbCases, 
                                            "_sensorTotals.csv"))
  
  pneumoCor <- cor(ALL_CASES_sensorTotals$Pneumo, 
                   ALL_CASES_sensorTotals$criterionState)
  
  EDACor <- cor(ALL_CASES_sensorTotals$EDA, 
                ALL_CASES_sensorTotals$criterionState)
  
  cardioCor <- cor(ALL_CASES_sensorTotals$Cardio, 
                   ALL_CASES_sensorTotals$criterionState)
  
  PLECor <- cor(ALL_CASES_sensorTotals$PLE, 
                ALL_CASES_sensorTotals$criterionState)
  
  totalCor <- cor(ALL_CASES_sensorTotals$grandTotal,
                  ALL_CASES_sensorTotals$criterionState)
  
  DEC <- cor(ALL_CASES_sensorTotals$Result, 
             ALL_CASES_sensorTotals$criterionState)
  
  print(pneumoCor)
  print(EDACor)
  print(cardioCor)
  print(PLECor)
  
  print(totalCor)
  
  print(DEC)
  
}

source('~/Dropbox/R/chartSimulator/summarizeAccuracy.R', echo=FALSE)


