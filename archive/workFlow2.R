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

#### start by clearing the global envir ####


 
{
  rm(list=ls())
  
  set.seed(1234567890)
}


###### Set the dropbox path ######


# this path is prepended to the file path before sourcing a script
RPath <- "~/Dropbox/"


###### source the workFlow_init.R script ######

{
  
  # to stop on warnings
  # options(warn=2)
  # to reset default warning level
  # options(warn=0)
  
  RPath <- "~/Dropbox/"
  # RPath <- "D:/Dropbox/"
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)
  # this is sourced by the workFlow_init.R script
  
}



#####  set the working directory  #####

{
  
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse")  
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_data")
  setwd("~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_data/fromAlanJennerich/Brackeen_NCCAASCII")
  # setwd("~/Dropbox/DATASETS/fromDonKrapohl2019")
  # setwd("~/Dropbox/TRAINING/Poland - forensic lab - May 2019/practica")
  # setwd("~/Dropbox/PFFOLDER/19N0401Steyn")
  # setwd("~/Dropbox/R/chartSimulator")
  # setwd("~/Dropbox/R/chartSimulator/data")
  # setwd("~/Dropbox/R/chartSimulator/data/PCASS")
  # setwd("~/Desktop")
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/PCASS_starter")
  # setwd("~/Dropbox/PRACTICA")
  # setwd("~/Dropbox/PFFOLDER")
  # setwd("~/Dropbox/TRAINING")
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/artifacts")
  # setwd("~/Dropbox/DATASETS")
  
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII")
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII")
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_NDI_NCCAASCII")
  # setwd("~/Dropbox/DATASETS_BACKUP")
  # setwd("~/Dropbox/RAYMOND/QC")
  # setwd("~/Dropbox/RAYMOND/From Mark/Private 07-13-19 Jerry Russell (LIsa Johnson Esq)")
  # setwd("~/Dropbox/CURRENT_PROJECTS")
  
  # setwd("~/Dropbox")
  # setwd("~/Dropbox/IPTC_Courses")
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/simulatorCharts")
  # setwd("~/Dropbox/RAYMOND/Court Cases")
  # setwd("~/Dropbox/RAYMOND/Court Cases/Grigsby 2020")
  # setwd("D:/761")
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/student_TDA")
  
  
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2019/PCASS_test")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100/FZCT_N100_NCCAASCII/FZCT_N100_NCCA-ASCII_AXC_renamed/N100_AXC_renamed_all")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100_NCCAASCII")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100_NCCAASCII/FZCT_N100_NCCA-ASCII_LAF/ALL_CASES")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")
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
  # setwd("~/Dropbox/DATASETS_BACKUP")
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
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2020/ESS-M online training June 2020")
  # source('~/Dropbox/R/NCCA_ASCII_Parse/PCASSScores.R', echo=TRUE)
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2020")
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2020/PFs/2020 - test 11.8.6 Oct132020")
  
  # setwd("~/Dropbox/RAYMOND/Polygraph cases")
  # setwd("~/My LXSoftware/PFFOLDER")
  # setwd("~/My LXSoftware/PFFOLDER/Apawu 20210114")
  
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
  
}



############# fix the D# file names 10-24-2018 LX11.8.2 #############



# {
#   NCCAASCIIchartNames <- getCharts("^D#+", uniqueTests=FALSE)
#   # loop over the file names and rename the files
#   for (i in 1:length(NCCAASCIIchartNames)) {
#     file.rename(NCCAASCIIchartNames[i], gsub("[#]+", "&", NCCAASCIIchartNames[i]))
#   }
# }



######## fix Limestone file name problems 5-4-2021 ############

{
  # located the Limestone exams
  NCCAASCIIchartNames <- getCharts("^D%+", uniqueTests=FALSE)
  
  
  Jaxon_E001C001.01A
  Jaxon_E002C001.01A
  Jaxon_E002C002.01A
  Jaxon_E002C003.01A
  
  
  -12
  -9
  
  NCCAASCIIchartNames <- getCharts("^D#+", uniqueTests=FALSE)
  
}



####### locate the exams in the current working directory ########

{
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/getExamNames.R'), echo=FALSE)
  
  # getCharts() will locate NCCA ASCII charts in the cwd
  # uniqueNames() is used to make a list of uniques exams in a directory
  
  print("search the working directory for NCCA ASCII text output")
  
  {
    
    searchPattern1 <- NULL
    searchPattern2 <- NULL
    searchPattern3 <- NULL
    searchPattern4 <- NULL
    
    uniqueExamNames1 <- NULL
    uniqueExamNames2 <- NULL
    uniqueExamNames3 <- NULL
    uniqueExamNames4 <- NULL
    
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
    
  }
  
  # uniqueExamNames <- c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  # uniqueExamNames <- uniqueExamNames[16]
  
  #### select an exam from the vector of exam names
  
  selectExams <- "ALL"
  # selectExams <- 32
  
  # keep only those selected exam numbers
  if(selectExams[1] != "ALL") {
    uniqueExamNames <- 
      c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams]
  } else uniqueExamNames <- 
    c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  
  print(paste("Found", length(uniqueExamNames), "exams"))
  print(uniqueExamNames)
  
}



####   fix problem characters in the NCCA ASCII text file names ####

if(fixFileNames==TRUE) {
  
  print("fix problem characters in the file and directory names")
  
  # first fix all problem characters in file and directory names
  print("fix problem characters in NCCA ASCII files")
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/fixFileNames.R'), echo=FALSE)
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

if(fixNonASCIICharacters==TRUE) {
  
  # fix non-ASCII characters in text files
  
  print("fix non-ASCII characters in NCCA ASCII output")
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/fixNonASCIICharacters.R'), echo=FALSE)
  
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
  
} # end fixNonASCIICharacters==TRUE


 
####   fix the sensor names if necessary   ####

if(fixSensorNames==TRUE) {
  
  print("fix the sensor names if necessary")
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/fixActivitySensorName.R'), echo=FALSE)
  
  if(!is.null(searchPattern1)) {
    
    # use this to change the bad Move1 sensor to MoveX to avoid having 2 Move1 sensors
    fixSensorNameFn(x="D&+", oldSensorName = "Move1", newSensorName = "MoveX")
    fixSensorNameFn(x="D&+", oldSensorName = "Aux01", newSensorName = "Move1")
    # fixMove1Fn(x="D&+", oldName="   SE", newName="Move1")
    
    # PLE sensor name should be PPG1
    # fixSensorNameFn(x="D&+", oldSensorName = "  PL", newSensorName = "PPG1")
    fixSensorNameFn(x="D&+", oldSensorName = "PLE1", newSensorName = "PPG1")
    
    ### LX4000
    # fixSensorNameFn(x="D&+", oldSensorName = "Move1", newSensorName = "MoveX")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux01", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux02", newSensorName = " PLE1")
    
    # fixSensorNameFn(x="D&+", oldSensorName = "Cardio1      Move1",
    #                 newSensorName = "Cardio1      MoveX")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux03", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux03", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "   SE", newSensorName = "Move1")
    # Cardio1      MoveX
    
    ### LX5000
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux02", newSensorName = "   SE")
    # fixSensorNameFn(x="D&+", oldSensorName = "PLE1", newSensorName = "  PL")
    
    ### LX6
    # fixSensorNameFn(x="D&+", oldSensorName = "Move1", newSensorName = "MoveX")
    # fixSensorNameFn(x="D&+", oldSensorName = "   SE", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "   PL", newSensorName = " PPG1")
    # fixSensorNameFn(x="D&+", oldSensorName = "   PA", newSensorName = " PPG1")
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
    # fixSensorNameFn(x="D&+", oldSensorName = "   PL", newSensorName = " PPG1")
    
  }

  # Limestone
  if(!is.null(searchPattern2)) {
    # fixSensorNameFn(x="D%+", oldSensorName = "Move1", newSensorName = "   SE")
    # fixSensorNameFn(x="D%+", oldSensorName = "PPG1", newSensorName = "PPG1")
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

if(isTRUE(fixStrings)) {
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/fixStrings.R'), echo=FALSE)
  
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
  # fixStringsFn(x="D&+", oldString=" CT", newString="  T")
  fixStringsFn(x="D&+", oldString="-CT ", newString="-TS ")
  fixStringsFn(x="D&+", oldString="--C ", newString="-TS ")
  fixStringsFn(x="D&+", oldString=" CT CT", newString=" TS TS")
  fixStringsFn(x="D&+", oldString="  C C", newString=" TS TS")
  
}



#######  locate the NCCAASCII text files in the working directory  #######

{
  
  print("locate NCCA ASCCI text files and make a vector of exams in the cwd")
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/getExamNames.R'), echo=FALSE)
  
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
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCIIParse.R'), echo=FALSE)
  
  # sourced by the NCCAASCIIParse.R script
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCIIParseHelperFunctions.R'), echo=FALSE)
  
  # stop()
  
  # parseUniqueExams is vectorized 5-8-2020
  parseUniqueExams(uniqueExamNames=uniqueExamNames,
                   saveCSV=saveCSV,
                   makeDF=makeDF )
  
  print (paste(length(uniqueExamNames), "exams parsed"))
  print(uniqueExamNames)

} 



####################   parse the stimulus events   #####################



if(stimulusParse == TRUE) {
  
  # first source the NCCAASCIIParseHelperFunctions.R script
  # # print("source the NCCAASCIIParseHelperFunctions.R script")
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCIIParseHelperFunctions.R'), echo=FALSE)
  
  print("check whether all charts in each series have the same stimulus events")
  # check whether all charts in each series have the same stimulus events
  # this will set the length of the stimulus sequence to the max length
  # for all charts in each series
  # will also check whether CQTs have at least 2 CQs and 2 RQs
  # x input is a character string to identify the Stimuli data frames
  eventParseOutMsg <- eventParse(x="_Stimuli$", makeDF=TRUE, saveCSV=FALSE)
  # output is a data frame (_eventMatrix), assigned to the global env with all event labels for all charts
  
  print(eventParseOutMsg)
  
  print("check whether stimulus events are identical for all charts in each series")
  # call a function to check whether stimulus events are identical for all charts in each series
  # x input is a character string to identify the Stimuli data frames
  # where TRUE indicates the stim text is identical
  stimCheckOutMsg <- stimCheck(x="_Stimuli$", makeDF=TRUE, saveCSV=FALSE)
  # output is a data frame (_stim_text_warning) and csv warning regarding differences 
  
  # , makeDF=makeDF, saveCSV=saveCSV
  
  print(stimCheckOutMsg)
  
}



############################   clean up   #############################



cleanUp <- TRUE

# does not remove uniqueExamNames

if(cleanUp == TRUE) {

  # clean up functions

  # rm(chartHeader)
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

  # clean up imported text data vectors
  rm(list = ls(pattern = "_header"))
  rm(list = ls(pattern = "_data"))
  
  # clean up data frames from  event parsing
  rm(list = ls(pattern="_event_warnings"))
  rm(list = ls(pattern="_eventMatrix"))
  rm(list = ls(pattern="_stim_text_warning"))
  rm(list = ls(pattern="_stim_warnings"))
  
  # rm(cleanUp)

}




##############  select exams for processing  ###############



{
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  
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

}




# ############## fix sensor column names - not used ##################



# if(fixSensorNames==TRUE) {
# 
#   # i=1
#   # for(i in 1:length(uniqueExams)) {
#   # 
#   #   examName <- uniqueExams[i]
#   #   assign("examName", examName, pos=1)
#   #   print(examName)
#   # 
#   #   # get the names of time series lists for all unique series in each exam
#   #   # searchString <- paste0("*", examName, "_Data", "*")
#   #   # examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
#   #   examDF <- get(paste0(examName, "_Data"), pos=1)
#   # 
#   #   names(examDF)
#   # 
#   # }
#   # 
#   # fixSensorNameFn(x="D#+", oldSensorName = "Move1", newSensorName = "   SE")
#   # fixSensorNameFn(x="D#+", oldSensorName = "PPG1", newSensorName = "  PL")
#   # 
#   # 
#   # 
#   # fixSensorNameFn <- function(x=searchPattern, oldSensorName="Aux01", newSensorName="   SE") {
#   # 
#   #   fileNames <- list.files(pattern = x)
#   # 
#   #   oldName <- oldSensorName
#   #   newName <- newSensorName
#   # 
#   #   for (i in 1:length(fileNames)) {
#   #     x <- readLines(fileNames[i], encoding="latin1")
#   # 
#   #     x[10:150] <- gsub(oldName, newName, x[10:150])
#   # 
#   #     writeLines(x, fileNames[i])
#   #   }
#   # 
#   # } # end fixSensorNameFn() function
# 
# }



########################  data pre-processing   ########################



if(NCCAASCIIParse == TRUE) {
  
  # source the pre-processing script 
  # to center the onset at zero
  # set the range for each sensor at 1 to 1000
  # add some columns for events
  # add eventLabels and event Text
  
  # preProc will also correct the stimulus event data frame
  
  # source the sigProcHelper.R script to load the getFirstLastEventFn()
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)
  
  # source this for the fixTagsFn()
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCIIParseHelperFunctions.R'), echo=FALSE)
  
  print("pre-processing")
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/preProc.R'), echo=FALSE)
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
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/fixDup.R'), echo=FALSE)
  print("fix duplicated or repeated stmulus event names")
  
  ### this is done somewhere else in the sigProc.R script
  
  dataNames <- fixDuplicatesFn(x=uniqueExams, 
                               makeDF=makeDF, 
                               showNames=showNames, 
                               output=output)
  
  print(dataNames)
  
  print (paste0("checked for duplicate event labels in ",length(uniqueExams), " exams"))
  
}



#####################   signal processing    #####################



if(processData == TRUE) {
  
  
  # use EDAFilt="resp" to extract the respiration signal from the EDA data
  # EDAFilt="resp"
  # EDAFilt="laf"
  # EDAFilt="test"
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  
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
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProc.R'), echo=FALSE)
  
  
  
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
  
  if(!exists("uniqueExamNames")) uniqueExamNames <- NULL
  
  sigProc(uniqueExams=uniqueExams, 
          uniqueExamNames=uniqueExamNames, 
          scaleOffset=TRUE,
          makeDF=TRUE)
  # use makeDF=TRUE to save the _Data data frame to the global env
  
  # use scaleOffset=TRUE TO scale and offset the data using the init parameters
  
}

################  trim excess time from data  ######################

if(isTRUE(trimExcessTime)) {
  
  # must be after signal processing to avoid problems
  # Aug 24, 2020
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/dataTrim.R'), echo=FALSE)
  
  dataTrimFn(x="*_Data$", y="_Stimuli$")
  
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
    
    save(list=ls(pattern=paste0(uniqueExamNames2[i],"_"), pos=1), 
         file=paste0(uniqueExams1[i], "_1.Rda") )
    
  }
  
  # rm(uniqueExams)
  
  rm(i)
  
  # saveRDA1 <- FALSE
  
  # if(exists("saveRDA1")) rm(saveRDA1)
  
} # end if saveRDA1 == TRUE



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
  
  if(exists("myDF")) { rmVector <- rmVector[-which(rmVector=="myDF")] }
  
  rm(list=rmVector)
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
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
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  {
    
    # make a function to make a list of unique exams in the global environment
    getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
    
    # get exam names from the _Data data frames
    print("make a list of unique exams in the global environment")
    uniqueExams <- getUniqueExams(x="*_Data$")
    
    print(uniqueExams)
    
  }
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  
  
  # loadRDA1 <- FALSE
  
  # rm(loadRDA1)
  # rm(searchPattern)

} # end if loadRDA1 == TRUE



################ get unique exams from the CWD ###################

{
  
  library(stringr)
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)
  # this is sourced by the workFlow_init.R script
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/excludedEvents.R'), echo=FALSE)
  
  # make a function to make a list of unique exams in the global environment
  # getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  # get exam names from the _Data data frames
  print("make a list of unique exams in the global environment")
  uniqueExams <- getUniqueExams(x="*_Data$")
  # uniqueExams <- uniqueExams[1]
  
  print(paste(length(uniqueExams), "exams in the global environment"))
  
  # select an exam from the vector of exam names
  if(!exists("selectExams")) selectExams <- "ALL"
  # selectExams <- 5
  
  if(selectExams[1] != "ALL") {
    if(!exists("selectExams")) selectExams <- "ALL"
    if(length(uniqueExams) > 1) {
      uniqueExams <- uniqueExams[selectExams]
    }
  }
  
  print(paste(length(uniqueExams), "exams selected"))
  
  print(uniqueExams)
  
}




########## scale and offset the data ###############

{

  # normally called during signal processing
  
  # but can be called here manually

  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)

  source(paste0(RPath, 'R/NCCA_ASCII_Parse/scaleOffsetData.R'), echo=FALSE)

  # ScaleOffsetDataFn(x=uniqueExams, makeDF=TRUE, output=FALSE)

}


###############  save the RDA2 data for each exam in the cwd   ################



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

  # rm(saveRDA2)

}



######################   artifact processing   ######################



# processArtifacts is set in the workFlow_init.R script
if(!exists("processArtifacts")) processArtifacts <- FALSE
# processArtifacts <- TRUE

if(processArtifacts == TRUE) {

  print("artifact extraction")
  
  # source('~/Dropbox/R/NCCA_ASCII_Parse/workFlow_init.R', echo=FALSE)
  # source('~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_init.R', echo=FALSE)
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/excludedEvents.R'), echo=FALSE)
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)
  # digital filters
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProc_extra.R'), echo=FALSE)
  
  # a function to iterate over exams and sensors 
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/artifactProc.R'), echo=FALSE)
  
  # call the function to reset the artifact_a column to 0
  artifactProc(x=uniqueExams)
  
  # which(chartDF$Artifacts_a != 0)
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/chartPlot_gg.R'), echo=FALSE)

  # which(examDF$Pneumo_a == "Artifact")
  # which(examDF$UPneumo_a == "Artifact")
  # which(examDF$LPneumo_a == "Artifact")
  # which(examDF$Move1_a == "Artifact")
  
  # which(chartDF$Pneumo_a == "Artifact")
  # which(chartDF$UPneumo_a == "Artifact")
  # which(chartDF$LPneumo_a == "Artifact")
  # which(chartDF$Move1_a == "Artifact")
  
  # which(examDF$AutoEDA_a == "")
  
}



#####################   check for physical activity   ##################



# if(processArtifacts == TRUE && activityArtifacts == TRUE) {
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/activityCheck.R'), echo=FALSE)
#   
#   print("check for activity sensor artifacts")
#   chartFUN <- activityCheck
#   getExamFn(x=uniqueExams)
#   
#   # source(paste0(RPath, 'R/NCCA_ASCII_Parse/chartPlot.R'), echo=FALSE)
#   
# }



#######################   EDA artifacts   #########################



# if(processArtifacts == TRUE && edaArtifacts == TRUE) {
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/dataCheck.R'), echo=FALSE)
#   
#   print("check for unresponsive EDA data")
#   chartFUN <- dataCheckFn
#   getExamFn(x=uniqueExams)
#   
#   # reset the warnings
#   # assign("last.warning", NULL, envir = baseenv())
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/nonstimArtifacts.R'), echo=FALSE)
#   
#   print("check for non-specific EDA responses")
#   chartFUN=nonStimArtifactFn
#   getExamFn(x=uniqueExams)
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/EDAMvtArtifact.R'), echo=FALSE)
#   
#   print("check for finger movement artifacts in the EDA data")
#   chartFUN=EDAMvtArtifactFn
#   getExamFn(x=uniqueExams)
#   
# }



#######################   pneumo artifacts   #######################



# if(processArtifacts == TRUE && pneumoArtifacts == TRUE) {
#   
#   sec <- .5
#   cutProp <- .25
#   # sec=3 cutProp=.5 works well 3-18-2017
#   # sec=1 cutProb=.25 also works ok
#   
#   
#   # sec2=4 cutProp2=.5 works well 3-19-2017
#   sec2 <- 3
#   cutProp2 <- .01
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)
# 
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/pneumoCheck.R'), echo=FALSE)
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/tukeyFences.R'), echo=FALSE)
# 
#   print("check for unresponsive respiration data")
#   chartFUN <- pneumoCheckFn
#   getExamFn(x=uniqueExams)
# 
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/pneumoArtifact.R'), echo=FALSE)
# 
#   print("respiration artifacts")
#   chartFUN=pneumoArtifactFn
#   getExamFn(x=uniqueExams)
#   
# }



########################   cardio artifacts   ##########################



# if(processArtifacts == TRUE && cardioArtifacts == TRUE) {
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/cardioArtifact.R'), echo=FALSE)
#   
#   # print("check for artifacts in the cardio data")
#   # chartFUN=cardioArtifactFn
#   # getExamFn(x=uniqueExams)
#   
# }



########################   PLE artifacts   ##########################



# if(processArtifacts == TRUE && pleArtifacts == TRUE) {
#   
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)
#   source(paste0(RPath, 'R/NCCA_ASCII_Parse/pleArtifact.R'), echo=FALSE)
#   
#   # print("PLE artifacts")
#   # chartFUN=pleArtifactFn
#   # getExamFn(x=uniqueExams)
#   
# }



########################   feature extraction   #######################



if(extractFeatures == TRUE) {

  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  # source the feature extraction function
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/featureExtraction.R'), echo=FALSE)

  print("feature extraction")
  
  if(!exists("uniqueExams")) {
    # uniqueExams <- getUniqueExams(x="*_Data$")
    uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  }
  
  featureExtraction(x=uniqueExams, writeCSV=FALSE)
  # use writeCSV=TRUE save the time series Data
  
  {
    
    # clean  up
    
    # if(exists("activityCheckParams")) rm(activityCheckParams)
    # rm(AutoExtractList)
    # rm(env.params)
    # rm(extract.params)
    # rm(ManualExtractList)
    # 
    # rm(examDF)
    # rm(seriesDF)
    # rm(chartDF)
    # rm(segmentDF)
    
  }

}

################   integrate artifacts   ##############



if(integrateArtifacts == TRUE) {
  
  # check the Artifacts_a channel 
  # remove response onset and response end 
  # if artifacts have occurred from 3 sec prestim seconds to .5 sec after respnose end

  source(paste0(RPath, 'R/NCCA_ASCII_Parse/integrateArtifacts.R'), echo=FALSE)
  
  
  
  integrateArtifactsFn(x=uniqueExams)
  
}
  
###########################   measurements   ############################

# getScores <- TRUE

if(getScores == TRUE) { 
  
  ### make a _Measurements data frame for each exam
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)
  
  print("make a data frame of _Measurments for each exam")
  
  # source the extractMeasurements.R script to get the Kircher measurements
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/extractMeasurements.R'), echo=FALSE)
  
  extractMeasurementsFn(x=uniqueExams, writeCSV=FALSE, CSVName="")
  # writeCSV=true will save the _Measurements.CSV data frame
  
  # _Measurements.csv and _Measurements data frame have no scores
  # _Meaasurements data frame is need by the scoring algorithms
  
}



##########################   Scores   #########################

if(getScores == TRUE) {

  print("score the data")
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/newScores.R'), echo=TRUE)
  
  # uniqueExams <- getUniqueExams(x="*_Data$")
  
  newScoresFn(uniqueExams=uniqueExams, 
              showNames=TRUE, 
              makeDF=FALSE, 
              saveCSV=FALSE, 
              output=FALSE )
  
  
  
  # source the script to load the getScoresFn() to calculate all numerical scores
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/scores.R'), echo=FALSE)
  
  # load the getExamFn
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)

  # declare this after sourciong the getSegment.R script  
  # seriesFUN <- getScoresFn
  # this will be called using do.call() which can take function or character string as the first arg
  
  # i=1 
  # length(uniqueExams)
  # for(i in 1:length(uniqueExams)) {
  #   getExamFn(x=uniqueExams[i])
  # }

  # write.csv(measurementDF, file = "QC_MeasurementDF.csv", row.names = TRUE)

  
  
  #### PCASS Scores ####
  
  
  # source(paste0(RPath, 'R/NCCA_ASCII_Parse/PCASS_feature_extraction.R'))
  # 
  # if(isTRUE(PCASSFormat)) {
  #   PCASSAlgorithmFn(x=uniqueExams)
  # }
  
  
  #### load and aggregate the seriesTotals.csv  ####
  
  
  
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
  
} # end scores



####################   save measurements   #######################



if(getScores == TRUE) {

  # saveMeasurements <- TRUE

  if(saveMeasurements == TRUE) {

    ### save the _Measurements data frame to a .csv for each chart for each exam

    # source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)

    print("save the _Measurments to a .CSV file")
    
    source(paste0(RPath, 'R/NCCA_ASCII_Parse/newMeasurementsScores.R'), echo=FALSE)
    
    # source the outputScores.R script for the measurementTableFn
    source(paste0(RPath, 'R/NCCA_ASCII_Parse/outputScores.R'), echo=FALSE)
    
    newMeasurementsScoresFn(uniqueExams=uniqueExams,
                            makeDF=FALSE,
                            saveCSV=TRUE,
                            MeasurementsDF=FALSE,
                            measurementTable=TRUE )
    
    
    # source the measurementsScores.R script to get the Kircher measurements
    # source(paste0(RPath, 'R/NCCA_ASCII_Parse/measurementsScores.R'))

    # examFUN <- measurementsScoresFn

    # print("save the measurements and score to a .csv file")
    # getExamFn(x=uniqueExams)

  }

}



###############  save the RDA2 data for each exam in the cwd   ################



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

  # rm(saveRDA2)
  
}



######  loadRDA2 the data from the current working directory - again  ###### 



# this one is sourced by the workFlow.R script
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)


# loadRDA2 <- FALSE
# loadRDA2 <- TRUE

# if(!exists("selectExams")) selectExams <- "ALL"


if(!exists("RPath")) RPath <- "~/Dropbox/"


# sourced by the workFlow.R script
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)


# loadRDA2 <- TRUE
if(!exists("loadRDA2")) loadRDA2 <- TRUE


if(loadRDA2==TRUE) {
  
  if(!exists("RPath")) RPath <- "~/Dropbox/"
  # RPath <- "~/Dropbox/"
  
  if(!exists("selectExams")) { selectExams <- "ALL" }
  # a vector of items to remove
  rmVector <- ls()[-(which(ls() %in% c("RPath", "selectExams")))]  
  
  if(exists("uniqueExamNames")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames")] }
  if(exists("uniqueExamNames2")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames2")] }

  if(exists("myDF")) { rmVector <- rmVector[-which(rmVector=="myDF")] }
  
  rm(list=rmVector)
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  myRdaNames <- list.files(pattern="*_2.Rda$")
  if(selectExams != "ALL") {
    myRdaNames <- myRdaNames[selectExams]
  }
  print(myRdaNames)
  
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
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/NCCAASCII_init.R'), echo=FALSE)
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/excludedEvents.R'), echo=FALSE)
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/getSegment.R'), echo=FALSE)
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/sigProcHelper.R'), echo=FALSE)
  
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
  
  
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/workFlow_init.R'), echo=FALSE)
  
  print(paste(length(uniqueExams), "exams selected"))
  print(uniqueExams)
  
  loadRDA2 <- FALSE
  
  # rm(loadRDA2)
  
} # end if(loadRDA2 == TRUE)




#########   save / load   #############



# save.image()
# load(".RData")



####



# use this to select an exam, series and chart
# getSegFn(exam=1,series=1,chart=2)



############################  printing   ##############################



# getOption("warn")



if(!exists("printCharts")) printCharts <- TRUE

# source the chartPlot.R script to print the plots
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/chartPlot_init.R'))

if(isTRUE(printCharts)) {
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/chartPlot_gg.R'), echo=FALSE)
}



if(!exists("printSegments")) printSegments <- TRUE

# plot the segments
# source(paste0(RPath, 'R/NCCA_ASCII_Parse/segmentPlot_init.R'))
if(isTRUE(printSegments)) {
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/segmentPlot_gg.R'), echo=FALSE)
}



############# save algorithm output to .txt ###############


# set in the workFlow_init.R script
# saveAlgorithmTXT <- TRUE
# saveAlgorithmTXT <- FALSE


if(!exists("saveAlgorithmTXT")) saveAlgorithmTXT <- FALSE

if(isTRUE(saveAlgorithmTXT)) {
  
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/saveAnalysisToTXT.R'), echo=FALSE)
  # saveAlgorithmTXT <- FALSE
  
  # call the function
  saveAnalysistoTXTFn(x=".ANALYSIS")
  
  saveAlgorithmTXT <- FALSE
  
}




########## save analysis .txt reports to pdf #############


# set in the workFlow_init.R script
# resultsToPDF <- TRUE
# resultsToPDF <- FALSE

# install.packages("rmarkdown")
# library(rmarkdown)

if(!exists("resultsToPDF")) resultsToPDF <- FALSE

if(isTRUE(resultsToPDF)) {
  
  # define the txtToPDF() function
  source(paste0(RPath, 'R/NCCA_ASCII_Parse/saveAnalysisToPDF.R'), echo=FALSE)
  
  analysisOutputFiles <- list.files(pattern="Output")
  
  # need to remove .pdf files from the vector
  analysisOutputFiles <- 
    analysisOutputFiles[str_sub(analysisOutputFiles, -4, -1) != ".pdf"]
  
  # call the txtToPDF() function
  txtToPDF(x=analysisOutputFiles)
  
  resultsToPDF <- FALSE
  
}



###### copy problem exams ########


copyProblems <- FALSE
# copyProblems <- TRUE

# theseProblems <- c(5, 6)
# theseProblems <- c(3)
# theseProblems <- c(4)

# thisSubDir <- "INC"
# thisSubDir <- "FN"
# thisSubDir <- "FP"


if(isTRUE(copyProblems)) {
  
  library(readr)
  sensorTotalsDF <- read_csv("ALL_CASES_100_sensorTotals.csv")
  # View(ALL_CASES_100_sensorTotals)
  
  
  # ESS-M
  # INC
  problemCaseList <- sensorTotalsDF$examName[sensorTotalsDF$correctCode %in% theseProblems]
  
  # FN
  # problemCaseList <- sensorTotalsDF$examName[sensorTotalsDF$correctCode %in% c(3)]
  
  # FP
  # problemCaseList <- sensorTotalsDF$examName[sensorTotalsDF$correctCode %in% c(4)]
  
  
  
  library(readr)
  
  
  # PCASS2SummaryDF <- read_csv("~/Dropbox/R/NCCA_ASCII_Parse/data/Ohio_PPG_data/Ohio_NCCAASCII/ALL_CASES_40_PCASS2Summary.csv")
  
  # PCASS-2
  # problemCaseList <- 
  #   PCASS2SummaryDF$examName[PCASS2SummaryDF$correctCode %in% c(4)]
  
  examList <- list.files(pattern="chartPlot.pdf")
  
  # numbered  cases
  # examList <- str_sub(examList, -17, -15)
  # axciton names
  examList <- str_sub(examList, 2, -15)
  
  
  
  
  # change the wortking directory # 
  # setwd("./FP_INC")
  
  # problemCaseList %in% examList
  
  # caseList <- list(files())
  
  # if(!dir.exists("problems")) dir.create("problems")
  
  # lafayette cases
  # allCases <- list.files(path="..", pattern="D\\&-", full.names=FALSE)
  # allCases <- list.files(pattern="D\\&", full.names=FALSE)
  # axciton cases
  allCases <- list.files(pattern="D\\$", full.names=FALSE)
  
  
  # numbered cases
  # theseFiles <- allCases[str_sub(allCases, -9, -7) %in% examList]
  # axciton cases
  theseFiles <- allCases[str_replace_all(str_sub(allCases, 4, -7), "\\$", "") %in% problemCaseList]
  
  # change the wortking directory # 
  # setwd("./FP")
  # setwd("./FN")
  setwd(paste0("./", thisSubDir))
  
  file.copy(paste0("../", theseFiles), theseFiles)
  

  
  # theseFiles <- str_sub(allCases, 16, 18) %in% examList
  
  # file.copy(theseFiles, paste0("..", theseFiles))
  
  # i=1
  # for(i in 1:length(problemCaseList)) {
  #   theseFiles <- list.files(pattern=problemCaseList[i])
  #   
  #   file.copy(theseFiles, paste0("./problems/", theseFiles))
  #   file.copy(theseFiles, paste0("..", theseFiles))
  #   
  # }
  
  copyProblems <- FALSE
  
}



############################ end work flow ############################



# source(paste0(RPath, 'R/NCCA_ASCII_Parse/summarizeAccuracy.R'), echo=FALSE)



