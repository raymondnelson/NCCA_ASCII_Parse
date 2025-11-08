# initialization script for workFlow parameters
# 3/19/2020
# Raymond Nelson
#
####



print("init parameters for the NCCA ASCII work flow")



######## source some init scripts ######## 



{
  
  # rm(list=ls())
  
  # getOption("warn")
  # options(warn=0) # default
  options(warn = 2) # warnings are escalated to errors
  
  # RPath <- "~/Dropbox/"
  
  if(!exists("RPath")) {
    # mac
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  }
  
  
  library(stringr)
  library(readr)
  
  # init parameters for processing and analyzing NCCA ASCII data
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  
  # list of events to exclude from analysis
  source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)
  
  # define the getCharts and uniqueNames functions
  source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  
  if(!exists("uniqueExams")) {
    # uniqueExams <- getUniqueExams(x="*_Data$")
    # uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  }
  
  # make a function to make a list of unique exams in the global environment
  getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  
} 



######## save and load Rda ######## 



{
  saveRDA1 <- FALSE # use TRUE to save the parsed NCCAASCII data
  
  loadRDA1 <- FALSE
  
  # export the series and scoresheet totals
  saveMeasurements <- FALSE # export the series and scoresheet totals
  saveMeasurements <- TRUE
  
  saveRDA2 <- TRUE
  
  loadRDA2 <- FALSE # use TRUE load the processed and extracted data
}



######## print charts and segments ######## 



{
  # printCharts <- FALSE
  printCharts <- TRUE
  
  printSegments <- FALSE
}



######## set the current WORKING DIRECTORY ######## 



# setwd("C:/users/Raymond/Desktop")
# setwd(paste0(RPath, "R"))

# setwd(paste0(RPath, "data"))
# setwd(paste0(RPath, "data"))
# setwd(paste0(RPath, "TRAINING/Poland - forensic lab - May 2019/practica"))
# setwd(paste0(RPath, "PFFOLDER/19N0401Steyn"))
# setwd(paste0(RPath, "R/chartSimulator"))
# setwd(paste0(RPath, "R/chartSimulator/data/PCASS"))
# setwd("~/Desktop"))

# setwd(paste0(RPath, "data/PCASS_starter"))
# setwd(paste0(RPath, "PRACTICA"))
# setwd(paste0(RPath, "PFFOLDER"))
# setwd(paste0(RPath, "TRAINING"))
# setwd(paste0(RPath, "data/artifacts"))
# setwd(paste0(RPath, "DATASETS_BACKUP/Sample Exams"))
# setwd(paste0(RPath, "DATASETS_BACKUP"))
# setwd(paste0(RPath, "DATASETS"))

# setwd("~/Dropbox/R/NCCAASCII_data")
# setwd("C://Users/raymo/Dropbox/R/NCCAASCII_data")

# setwd("~/Dropbox/R/NCCAASCII_data/Ohio_PPG_data/Ohio2015_n40_nccascii_2024Mar")

# setwd(paste0(RPath, "RAYMOND/QC"))
# setwd(paste0(RPath, "RAYMOND/From Mark/Private 07-13-19 Jerry Russell (LIsa Johnson Esq)"))
# setwd(paste0(RPath, "CURRENT_PROJECTS"))
# setwd("~/Dropbox"))
# setwd(paste0(RPath, "IPTC_Courses"))
# setwd(paste0(RPath, "data/simulatorCharts"))
# setwd(paste0(RPath, "RAYMOND/Court Cases"))
# setwd(paste0(RPath, "RAYMOND/Court Cases/Grigsby 2020"))
# setwd("D:/761")

# setwd("~/Dropbox/Court Cases")

# setwd("~/Dropbox/LAFAYETTE/Lafayette2019/PCASS_test")
# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100_NCCAASCII")
# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100_NCCAASCII/FZCT_N100_NCCA-ASCII_LAF/ALL_CASES")
# setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")

# setwd("~/Dropbox/QC/Assess/May2019")

# setwd("~/Dropbox/IPTC_Courses/South Africa March 2019/practica South Africa 3-2019")
# setwd("~/Dropbox/data/feature_extraction/slopeChange")
# setwd("~/Dropbox/data/GF")
# setwd("~/Dropbox/PRACTICA/IPTC practica 10-2018")
# setwd("~/Dropbox/data/feature_extraction")
# setwd("~/Dropbox/PRACTICA/Honduras practica 4-2018/ACQT_4-3-2-2018")
# setwd("~/Dropbox/data/fromDonKrapohl")
# setwd("~/Dropbox/data/feature_extraction/Utah_features/EDA_complexity")
# setwd("~/Dropbox/PRACTICA/Bulgaria_PEAK_practica_10-2018")
# setwd("~/Dropbox/PRACTICA/Practica_ElSalvador_9-2017")
# setwd("~/Dropbox/PRACTICA/Honduras practica 3-2017")
# setwd("~/Dropbox/data/LafayetteGraphics")

# setwd("~/Dropbox/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII")
# setwd("~/Dropbox/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/new_test_04302018")
# setwd("~/Dropbox/data/artifacts/artifacts_cardio")
# setwd("~/Dropbox/PRACTICA/Argentina_practica_2-2017")
# setwd("~/Dropbox/PRACTICA/Practica_Peru_Dec_2016")
# setwd("~/Dropbox/RAYMOND/Court Cases/WhiteMarlinOpen2017/WMO Polygraph")
# setwd("~/Dropbox/TRAINING/Backster_physiology_3-2017/practica")
# setwd("~/Dropbox/DATASETS_BACKUP/DoDPI_confirmed_case_database_050302")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100")
# setwd("~/Dropbox/DATASETS_BACKUP")

# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/MarinN100_NCCA_innocent")
# setwd("~/Dropbox/DATASETS_BACKUP/MarinN100/MarinN100_NCCAASCII/MarinN100_NCCA_guilty")
# setwd("~/Dropbox/RAYMOND/Court Cases/Scott Beebe 2016/polygraphs 2006-2007")
# setwd("~/Dropbox/TRAINING/Poland - advanced training Dec 2017")
# setwd("~/Dropbox/TRAINING/Poland - advanced training Dec 2017/practica 12-2107")
# setwd("~/Dropbox/data/EDA_signal_processing_2017")
# setwd("~/Dropbox/TRAINING/Data Science Indy 1-10-2018")
# setwd("~/Dropbox/CURRENT_PROJECTS/Practical polygraph - seven things about EDA feature extraction/graphic examples")
# setwd("~/Dropbox/data/EDA_signal_processing_2017/Poland_cases_2017")
# setwd("~/Dropbox/TRAINING/Texas DPS 1-2018/practica")
# setwd("~/Dropbox/IPTC_Courses/South Africa 2017/practica")
# setwd("~/Dropbox/RAYMOND/Court Cases/Ohio 2017/Cara L. Young (Sexual Battery) Harrison Co. SO)")
# setwd("~/Dropbox/data/Marin_pepsichallenge_2018/MarinN100_NCCAASCII/new_test_04302018/result")
# setwd("~/Dropbox/IPTC_Courses/Mexico August 2018")
# setwd("~/Dropbox/data/OSS2/replaced")
# setwd("~/Dropbox/IPTC_Courses/Poland 2018/Training documents Poland/S6 CMs/Practice Exams DLST 2/0995")
# setwd("~/Dropbox/IPTC_Courses/Poland 2018/Training documents Poland/S6 CMs")
# setwd("~/Dropbox/data/fromBenBlalock/Nov302018")



######## set the directory search parameters ######## 



{
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
}



######## artifacts ######## 



{
  # processArtifacts <- FALSE

  # # artifactPneumo <- TRUE
  # # artifactEDA <- TRUE
  # # artifactCardio <- TRUE
  # # artifactPLE <- TRUE
  # # artifactActivity <- TRUE
  # 
  # pneumoArtifacts <- TRUE
  # edaArtifacts <- TRUE
  # cardioArtifacts <- TRUE
  # FCArtifacts <- FALSE
  # pleArtifacts <- TRUE
  # activityArtifacts <- TRUE
  # 
  # xgrateArtifacts <- FALSE
  # 
  # integrateRespArtifacts <- FALSE
  # integrateEDAArtifacts <- FALSE
  # integrateCardioArtifacts <- FALSE
  # integrateFCArtifacts <- FALSE
  # integratePLEArtifacts <- FALSE
  # integrateActivityArtifacts <- FALSE
}



######## WORK FLOW parameters ######## 



{  
  NCCAASCIIParse <- TRUE # parse the NCCA ASCII charts to a single data frame
  
  stimulusParse <- FALSE # check the stimulus events for problems
  
  preProcessData <- TRUE # center the data at 0, set the initial range, and add columns
  
  processData <- TRUE # signal processing
  
  scaleOffsetData <- TRUE # use TRUE for scaling and offsetting
  
  extractFeatures <- TRUE
  # extractFeatures <- FALSE
  
  processArtifacts <- TRUE
  # processArtifacts <- FALSE
  
  # integrateArtifacts <- TRUE
  integrateArtifacts <- FALSE
  
  # save the chartDF to the global env for inspection
  # 2024Jul29
  saveChartDF <- TRUE
  saveChartDF <- FALSE
  
  writeNCCAASCII_LAF <- FALSE
  
  { 
    
    {
      ## edit the NCCA ASCII text files ##
      
      # to fix non ASCII characters in files and filenames
      fixNonASCIICharacters <- TRUE
      fixNonASCIICharacters <- FALSE
      
      # to fix problem character strings in NCCA ASCII files
      fixStrings <- fixNonASCIICharacters 
      # fixStrings <- FALSE
      # fixStrings <- TRUE
      
      # to fix problem characters in the file names
      fixFileNames <- fixNonASCIICharacters 
      # fixFileNames <- FALSE
      # fixFileNames <- TRUE
      
      fixSensorNames <- fixNonASCIICharacters
      # fixSensorNames <- FALSE
      # fixSensorNames <- TRUE
      
      # clean up after loading and parsing the NCCA ASCII data
      cleanUp <- TRUE
      cleanUp <- FALSE
    }
    
    {
      
      # append an "a" to each repeated question
      fixDuplicateTags <- FALSE
      # fixDuplicateTags <- TRUE
      
      # used in the workflow to remove excess time before X and after XX
      trimExcessTime <- FALSE
      # trimExcessTime <- TRUE
      
      PLEBaseline <- FALSE
      PLEBaseline <- TRUE
      
      # used in the 
      removeAnnotations <- FALSE
      # removeAnnotations <- TRUE
      
      # Sep 12, 2023
      # stop if missing X or XX announcement
      stopXXX <- FALSE
      stopXXX <- TRUE
      
    }
    
  }
  
}


    
######## WORK FLOW PRESETS #########



{
  
  {
    # default configuration
    removeAnnotations <- FALSE
    trimExcessTime <- TRUE
    stopXXX <- TRUE
    
    fixNonASCIICharacters <- TRUE
    fixSensorNames <- TRUE
    
    EDAFilt="laf18"
    
    DRule <- "TSR"
  }
  
  
  {
    # reconfig A for LAFAYETTE FIELD CASES
    removeAnnotations <- FALSE
    
    trimExcessTime <- FALSE
    # trimExcessTime <- TRUE
    
    stopXXX <- FALSE
    
    fixNonASCIICharacters <- TRUE
    fixNonASCIICharacters <- FALSE
    
    fixDuplicateTags <- FALSE
    fixDuplicateTags <- TRUE
    
    fixDLSTLabels <- FALSE
     
    
    fixSensorNames <- TRUE
    
    EDAFilt="laf18"
    
    # used to expedite the selection of algorithm decision rules in this script
    DRule <- "TSR"
    DRule <- "SSR"
  }
  
  {
    # reconfig B for LAFAYETTE RESEARCH CASES
    removeAnnotations <- FALSE
    trimExcessTime <- TRUE
    stopXXX <- TRUE

    fixDuplicateTags <- TRUE
    fixDLSTLabels <- FALSE

    fixNonASCIICharacters <- FALSE
    fixFileNames <- FALSE
    fixSensorNames <- FALSE
    fixStrings <- FALSE

    EDAFilt="laf18"
    EDAFilt="none"

    # DRule <- "TSR"
    DRule <- "SSR"
  }
  
  {
    # # reconfig F for LAFAYETTE EDA Problems
    # removeAnnotations <- FALSE
    # fixDuplicateTags <- TRUE
    # trimExcessTime <- TRUE
    # stopXXX <- FALSE
    # 
    # fixNonASCIICharacters <- TRUE
    # 
    # EDAFilt="lafX"
    # EDAFilt="laf18"
  }
  
  
  {
    # # reconfig C for LXCAT exams
    # removeAnnotations <- FALSE
    # trimExcessTime <- FALSE
    # stopXXX <- FALSE
    # 
    # EDAFilt="leg" # Lafayette (.04/.886)
  }
  
  {
    # # reconfig D for AXCITON RESEARCH SAMPLES
    # # including Axciton samples that are in Lafayette NCCA ASCII format
    # removeAnnotations <- FALSE
    # trimExcessTime <- TRUE
    # stopXXX <- TRUE
    # 
    # fixDuplicateTags <- TRUE
    # fixDLSTLabels <- FALSE
    # 
    # EDAFilt="none"
    # 
    # reduceSampleRate <- TRUE
    # 
    # fixNonASCIICharacters <- FALSE
    # fixSensorNames <- FALSE
    # fixFileNames <- FALSE
    # fixStrings <- FALSE
    # 
    # # DRule <- "TSR"
    # DRule <- "SSR"
  }
  
  {
    # # reconfig E for AXCITON FIELD CASES
    # removeAnnotations <- FALSE
    # trimExcessTime <- TRUE
    # stopXXX <- FALSE
    # 
    # EDAFilt="none"
    # 
    # fixNonASCIICharacters <- FALSE
    # fixSensorNames <- FALSE
    # 
    # DRule <- "TSR"
    # # DRule <- "SSR"
  }
  
  {
    # # reconfig F for STOELTING LAB CASES
    # removeAnnotations <- FALSE
    # trimExcessTime <- TRUE
    # stopXXX <- TRUE
    # 
    # fixDuplicateTags <- TRUE
    # 
    # fixDLSTLabels <- TRUE
    # 
    # # EDAFilt="none"
    # EDAFilt="laf18"
    # 
    # fixNonASCIICharacters <- TRUE
    # fixFileNames <- TRUE
    # fixSensorNames <- TRUE
    # fixStrings <- TRUE
    # 
    # # DRule <- "TSR"
    # DRule <- "SSR"
  }
  
}



######## DECISION RULES ######## 



{
  
  # used to expedite the selection of algorithm decision rules in this script
  # DRule <- "TSR"
  # DRule <- "SSR"
  
  if(!exists("DRule")) DRule <- "TSR"
  
  ESSMDecisionRule <- ifelse(DRule=="TSR", "eTSR", "SSR")
  # ESSMDecisionRule <- ifelse(DRule=="TSR", "TSR", "SSR")
  # ESSMDecisionRule <- "SSR"
  
  OSS3DecisionRule <- ifelse(DRule=="TSR", "TSR", "SCR")
  # OSS-3 can use the SCR rule which uses the KW-ANOVA
  # OSS3DecisionRule <- "SCR"
  
  PADecisionRule <- "TSR"
  
  OSS2DecisionRule <- "GTR"
  
  ROSSDecisionRule <- ifelse(DRule=="TSR", "GTR", "SSR")
  # ROSSDecisionRule <- "SSR"
  # ROSSDecisionRule <- "GTR"
  
  PSSDecisionRule <- "GTR"
  
  bootstrapDecisionRule <- "GTR"
  
  # PCATDecisionRule <- "pSSR"
  PCATDecisionRule <- ifelse(DRule=="TSR", "GTR", "pSSR")
  
  # normally comment this out
  # if(isTRUE(PCASSFormat)) ESSMDecisionRule <- "SSR"
  
  # decision rules can be "GTR" "SSR" "TSR" "FZR"
  # also use "auto" for OSS-3, ESS-M and PCASS-2
  # to auto-select decision rules either "SSR" or "TSR"
  
  
  # used to stop scoring and inspect progress
  stopScore <- TRUE
  stopScore <- FALSE
  # this environment parameter is used by the newScores function
  
  # stop feature extraction for inspection
  stopSeg <- c(FALSE, case=3, series=1, chart=3, seg="C4")
  
  
}



######## SCORES and algorithms ######## 



{
  getScores <- TRUE # generate R/C scores and algorithm results
  
  # can be used to exclude pneumo data from exams with unstable data
  includePneumoData <- TRUE
  
  # calculate and include PLE scores in the  analysis
  includePLEData <- TRUE
  # includePLEData <- FALSE
  if(!isTRUE(includePLEData)) includePLEScores <- includePLEData
  # also check the includePLEScores in the NCCAASCII_init.R script
  
  # rank methods for all exam, not limited to CQTs
  getRankScores <- TRUE
  getRRMScores <- TRUE
  getMiritelloRankScores <- TRUE
  getIpsativeZScores <- TRUE
  
  # ESSM scores require the rank score and R/C score
  getRCScores <- TRUE
  
  getESSMScores <- TRUE
  getOSS3Scores <- TRUE
  
  getPAScores <- TRUE
  getOSS2Scores <- TRUE
  getROSSScores <- TRUE
  getPSSScores <- TRUE
  getBootstrapScores <- TRUE
  
  getPCATScores <- TRUE
  getPCATScores <- FALSE
  
  # PCASS2bootSize <- 1000

}



######## PCASS/PCAT Format ######## 



PCATFormat <- FALSE
# PCATFormat <- TRUE

if(isTRUE(PCATFormat)) {
  
  # use this to coerce some of the environment parameters
  
  # getPCASS2Scores <- TRUE
  
  # pneumoArtifacts <- FALSE
  # edaArtifacts <- TRUE
  # cardioArtifacts <- TRUE
  # FCArtifacts <- FALSE
  # pleArtifacts <- TRUE
  # activityArtifacts <- TRUE
  
  # printCharts <- FALSE
  
  # extractPneumo <- TRUE
  # extractCardio <- TRUE
  
  # extractPneumoPatterns <- TRUE
  
} else {
  
  # getPCATScores <- FALSE
  
}



######## check the number of RQs and CQs ######## 



# used by the scores function 
# to check for equal number of RQs and CQs
# normally not used,
# but can be used to verify the format of some samples
checkRQCQs <- FALSE
# stop if unequal 



######## write NCCA ASCII files ######## 



# to output the processed data to a new NCCA ASCII file
writeNCCAASCII <- FALSE



######## EDA filter for imported data ######## 



{
  # set in the NCCAASCII_init.R script
  
  # use the "man" filter for imported data
  # EDAFilt="man" 
  # if(EDAFilt == "man") EDAHighPass <- FALSE
}



######## function to make a list of unique exams in the global envir ######## 



getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }


if(!exists("uniqueExams")) uniqueExams <- getUniqueExams()



######## save algorithm output to text and pdf ######## 



{

  saveAlgorithmTXT <- FALSE
  
  # June 21, 2023
  saveQuestionList <- FALSE

  saveRank <- TRUE
  saveRRM <- TRUE
  saveMiritelloRank <- TRUE
  saveIPZ <- TRUE

  saveESSM <- TRUE
  saveOSS3 <- TRUE
  
  savePA <- TRUE
  saveOSS2 <- TRUE
  saveROSS <- TRUE
  savePSS <- TRUE
  saveBootstrap <- TRUE

  savePCAT <- TRUE
  
  # this does not override the previous settings
  saveALL <- TRUE

  # output the algorithm reports to a pdf file
  saveResultsToPDF <- FALSE
  saveResultsToPDF <- TRUE

}



####### remove an exam from a the global envir #########

removeExamFn <- function(x) {
  # remove an exam from the global envir
  for(i in 1:length(x)) {
    rm(list=ls(pattern=x[i],, envir=.GlobalEnv), envir=.GlobalEnv)
  }
  assign("uniqueExams", unique(str_sub(ls(pattern="*_Data$", envir=.GlobalEnv),1, -6)), envir=.GlobalEnv)
  return(paste("Removed", length(x), "exams from the global environment."))
}




