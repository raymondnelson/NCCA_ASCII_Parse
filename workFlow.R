# R script to execute the work flow for NCCA ASCII files 
#
# 
# 
####



######## start by clearing the global envir ########



{
  rm(list=ls())
  
  # set.seed(1234567890)
}



if(getOption("warn") !=2) {
  # set the warn level to suppress warnings
  if(!exists("oldw")) oldw <- getOption("warn")
  # -1 to suppress warnings
  # 0 is normal, warnings are display at end
  # 1 warnings are display at the time
  # 2 warnings are escalated to errors
  options(warn = 2)
  # reset to default
  # options(warn = 0)
  # rm(oldw)
  print(paste("warn level:", oldw))
  # getOption("warn")
}



######## Set the dropbox path ########



{
  
  # this path is prepended to the file path before sourcing a script
  if(!exists("RPath")) {
    # mac
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  
    # use this
    # source(paste0(RPath, <filePath>), echo=FALSE)
  }
  
  # Mac
  NCCADataPath <-  "/Users/raymondnelson/Dropbox/R/NCCAASCII_data/"
  # windows
  # NCCADataPath <-  "C://Users/raymo/Dropbox/R/NCCAASCII_data/"
  
}



######## source the workFlow_init.R script ######## 



{
  
  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  # this is sourced by the workFlow_init.R script
  
}



######## set the WORKING DIRECTORY ######## 



{
  
  # setwd(NCCADataPath)
  
  # setwd("~/Dropbox/R/NCCA_ASCII_Parse")
  # setwd("~/Dropbox/R/NCCAASCII_data")
  # setwd("~/Dropbox//QC")
  
  # setwd("~/Dropbox/DATASETS/fromDavidCrawford")
  # setwd("~/Dropbox/DATASETS/fromDavidCrawford/NCCAASCII_N100")
  
  # setwd("~/Dropbox/Court Cases")
  
  # setwd("~/Dropbox/CURRENT_PROJECTS")
  
  # setwd("~/Dropbox/PRACTICA")
  
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2023")
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2024")
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2023/LXCAT")
  
  # setwd("~/Desktop/South Africa Practica 2023 Nov")
  # setwd("~/Dropbox/IPTC_Courses/Ukraine Nov 2024")
  
  # setwd("~/Dropbox/NCCAASCII_data/fromAlanJennerich/Brackeen_NCCAASCII")
  # setwd("~/Dropbox/DATASETS/fromDonKrapohl2019")
  # setwd("~/Dropbox/TRAINING/Poland - forensic lab - May 2019/practica")
  # setwd("~/Dropbox/PFFOLDER/19N0401Steyn")
  
  # setwd("~/Dropbox/R/chartSimulator")
  # setwd("~/Dropbox/R/chartSym_data/")
  # setwd("~/Dropbox/R/chartSym_data/cardio and respiration rates/afmgqt20250515/check")
  
  # setwd("~/Dropbox/DATASETS_BACKUP/Axciton_confirmed_casesN44/AxcitonN44_NCCAASCII")
  
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2024/PTT")
  
  # setwd("~/Dropbox/R/chartSimulator/data/PCASS")
  # setwd("~/Dropbox/R/chartSimulator/chartSym_data/PCASS")
  # setwd("~/Desktop")
  # setwd("~/Dropbox/data/PCASS_starter")
  # setwd("~/Dropbox/PRACTICA")
  # setwd("~/Dropbox/PFFOLDER")
  # setwd("~/Dropbox/TRAINING")
  # setwd("~/Dropbox/data/artifacts")
  # setwd("~/Dropbox/DATASETS")
  # setwd("~/Dropbox/NCCAASCII_data/TDA Exercise Charts/NCCAASCII")
  # setwd("~/Dropbox/NCCAASCII_data/Jan_6_2021")
  
  # setwd("~/Dropbox/Court Cases")
  # setwd("~/Dropbox/Court Cases/WarrenCtyNY2021")
  # setwd("~/Dropbox/Court Cases/Garberding July 2021/Garberding Justin 20210712")
  
  # setwd("~/Dropbox/Polygraph Cases")
  # setwd("~/Dropbox/RAYMOND/Polygraph Cases/Record June 2021/NCCAASCII")
  # setwd("~/Dropbox/Court Cases/Raylin Dwayne James 2021/James_NCCAASCII")
  
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII")
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_NDI_NCCAASCII")
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII/noScores/checkThese")
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII")
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII/new")
  
  # setwd("~/Dropbox/DATASETS")
  # setwd("~/Dropbox/DATASETS_BACKUP")
  
  # setwd("~/Dropbox/RAYMOND/QC")
  # setwd("~/Dropbox/RAYMOND/From Mark/Private 07-13-19 Jerry Russell (LIsa Johnson Esq)")
  # setwd("~/Dropbox/CURRENT_PROJECTS")
  
  # setwd("~/Dropbox")
  # setwd("~/Dropbox/IPTC_Courses")
  # setwd("~/Dropbox/data/simulatorCharts")
  # setwd("~/Dropbox/RAYMOND/Court Cases")
  # setwd("~/Dropbox/RAYMOND/Court Cases/Grigsby 2020")
  # setwd("D:/761")
  # setwd("~/Dropbox/data/student_TDA")
  
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2019/PCASS_test") 
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2022/")
  
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100/FZCT_N100_NCCAASCII/FZCT_N100_NCCA-ASCII_AXC_renamed/N100_AXC_renamed_all")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100_NCCAASCII")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N100_NCCAASCII/FZCT_N100_NCCA-ASCII_LAF/ALL_CASES")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/problems")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF")
  
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF/NCCAASCIIOutputLAF2")
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/FZCT_N60/NCCA_ASCII_OSS3_holdoutN60/NCCAASCIIOutputLAF/NCCAASCIIOutputLAF2/NCCAASCIIOutputLAF3")
  
  # setwd("~/Dropbox/CURRENT_PROJECTS/Algorithm Comparison - Handler 2020/data/Axciton_confirmed_cases44")
  
  # setwd("~/Dropbox/DATASETS/LEPET/LEPET_NCCAASCII/LEPET_N60_NCCAASCII/noScores/checkThese/NCCAASCIIOutputLAF")
  
  # setwd("~/Dropbox/DATASETS/Ohio_PPG_data/Ohio2015_n40_nccascii_2024Mar")
  # setwd("~/Dropbox/DATASETS/Ohio_PPG_data/Utah_Exams_PLE_NCCA_ASCII")
  # setwd("~/Dropbox/DATASETS/Ohio_PPG_data/Ohio_NCCAASCII")
  # setwd("~/Dropbox/R/NCCAASCII_data/Ohio_PPG_data")
  
  # setwd("~/Dropbox/R/NCCAASCII_data/respiration artifacts 2024/ncca")
  
  # setwd(paste0(NCCADataPath, "Ohio_PPG_data"))
  
  # setwd("~/Dropbox/R/NCCAASCII_data/Ohio_PPG_data/Ohio2015_n40_nccascii_2024Mar")
  
  # axciton N44 sample transformed to Lafayette NCCA ASCII data
  # setwd("~/Dropbox/R/NCCAASCII_data/AxcitonN44_NCCAASCIIOutputLAF")
  
  # setwd("~/Dropbox/QC/Assess/May2019")
  # setwd("~/Dropbox/IPTC_Courses/South Africa March 2019/practica South Africa 3-2019")
  # setwd("~/Dropbox/data/feature_extraction/slopeChange")
  # setwd("~/Dropbox/data/GF")
  # setwd("~/Dropbox/R/NCCAASCII_data/GF")
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
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2020/ESS-M online training June 2020")
  # source('~/Dropbox/PCASSScores.R', echo=TRUE)
  
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2022")
  # setwd("~/Dropbox/LAFAYETTE/Lafayette2020/PFs/2020 - test 11.8.6 Oct132020")
  
  # setwd("~/Dropbox/RAYMOND/Polygraph cases")
  # setwd("~/My LXSoftware/PFFOLDER")
  # setwd("~/My LXSoftware/PFFOLDER/Apawu 20210114")
  
  # setwd("C:/Axciton/Sessions")
  
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



######## locate the exams in the current working directory ######## 



{
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  # source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  # this is sourced by the workFlow_init.R script
  
  # source(paste0(RPath, "workFlow_init.R"), echo=FALSE)
  
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
  
  #### select an exam from the vector of exam names
  
  selectExams <- "ALL"
  # selectExams <- 32
  
  # keep only those selected exam numbers
  if(selectExams[1] != "ALL") {
    uniqueExamNames <- 
      c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)[selectExams]
  } else uniqueExamNames <- 
    c(uniqueExamNames1, uniqueExamNames2, uniqueExamNames3, uniqueExamNames4)
  
  # uniqueExamNames <- uniqueExamNames[16]
  
  print(paste("Found", length(uniqueExamNames), "exams"))
  print(uniqueExamNames)
  
}



######## copy the exam source and RDA files to the CWD ######## 



{
  # 
  #   sourceFiles1 <- list.files(pattern="D\\$-")
  #   if(length(sourceFiles) > 0) {
  #     examNames1 <- unique(str_sub(sourceFiles1, 4, -7))
  #   }
  #   
  #   sourceFiles2 <- list.files(pattern=".Rda$")
  #   if(length(sourceFiles2) > 0) {
  #     examNames2 <- unique(str_sub(sourceFiles2, 2, -7))
  #   }
  #   
  #   sourceFiles3 <- list.files(pattern=".pdf$")
  #   if(length(sourceFiles3 > 0)) {
  #     examNames3 <- unique(str_sub(sourceFiles3, 2, -15))
  #   }
  #   
  #   examNames <- unique(c(examNames1, examNames2, examNames3))
  # 
  #   i=1
  #   for(i in 1:length(examNames)) {
  #     thisExamName <- examNames[i]
  #     sourceNames <- list.files("..", pattern=thisExamName)
  #     sourceNames <- sourceNames[grep(pattern="D\\$-X.", sourceNames)]
  #     file.copy(paste0("../", sourceNames), paste0(sourceNames), overwrite=TRUE)
  #   }
  # 
  # 
  # 
  
}



######## fix the Stoelting D# file names 10-24-2018 LX11.8.2 ######## 



{
  
  # NCCAASCIIchartNames <- getCharts("^D#+", uniqueTests=FALSE)
  # # loop over the file names and rename the files
  # for (i in 1:length(NCCAASCIIchartNames)) {
  #   file.rename(NCCAASCIIchartNames[i], gsub("[#]+", "&", NCCAASCIIchartNames[i]))
  # }
  
}



######## fix the Axciton file names 2025Mar20 ######## 



{

  # NCCAASCIIchartNames <- getCharts("^D\\$+", uniqueTests=FALSE)
  # # loop over the file names and rename the files
  # i=1
  # for (i in 1:length(NCCAASCIIchartNames)) {
  #   newAxcName <- str_sub(NCCAASCIIchartNames[i], 4, -7)
  #   newAxcName <- gsub("[\\$]+", "", newAxcName)
  #   newAxcName <- gsub("[[:punct:]]", "", newAxcName)
  #   newAxcName <- paste0("D$-", newAxcName, str_sub(NCCAASCIIchartNames[i], -6, -1))
  #   file.rename(NCCAASCIIchartNames[i], newAxcName)
  # }

}



######## fix Limestone file name problems 5-4-2021 ######## 



{
  
  # locate the Limestone exams
  # NCCAASCIIchartNames <- getCharts("^D%+", uniqueTests=FALSE)
  # get the unique series for the Limestone exams
  # uniqueSeries <- unique(str_sub(NCCAASCIIchartNames, -9, -9))
  
  # loop over the files and rename them
  # for (i in 1:length(NCCAASCIIchartNames)) {
  #   thisSeries <- str_sub(NCCAASCIIchartNames[i], -9, -9)
  #   thisChart <- str_sub(NCCAASCIIchartNames[i], -5, -5)
  #   newFileName <- str_sub(NCCAASCIIchartNames[i], 1, -13)
  #   newFileName <- paste0(newFileName,thisSeries,".0",thisChart, "A")
  #   file.rename(NCCAASCIIchartNames[i], newFileName)
  # }
  
  # Jaxon_E001C001.01A
  # Jaxon_E002C001.01A
  # Jaxon_E002C002.01A
  # Jaxon_E002C003.01A
  # 
  # 
  # -12
  # -9
  # -5
  
}



######## fix LXEdge Answers Oct 31, 2023 ######## 



{
  
 # answers need to be "No" "Yes" or "Ans" 
  
}



######## fix non ASCII characters in the text files ######## 



if(!exists("fixNonASCIICharacters")) fixNonASCIICharacters <- FALSE
# fixNonASCIICharacters <- TRUE



if(fixNonASCIICharacters==TRUE) {
  
  # fixNonASCIICharacters is initialized in the workFlow_init.R script
  
  # fixNonASCIICharacters <- TRUE
  
  # fix non-ASCII characters in text files
  
  print("fix non-ASCII characters in NCCA ASCII output")
  
  source(paste0(RPath, 'fixNonASCIICharacters.R'), echo=FALSE)
  
  if(length(uniqueExamNames1) > 0) {
    fixNonASCIICharactersFn(searchPattern=searchPattern1)
  }
  # Axciton
  if(length(uniqueExamNames2) > 0) {
    fixNonASCIICharactersFn(searchPattern=searchPattern2)
  }
  if(length(uniqueExamNames3) > 0) {
    fixNonASCIICharactersFn(searchPattern=searchPattern3)
  }
  if(length(uniqueExamNames4) > 0) {
    fixNonASCIICharactersFn(searchPattern=searchPattern4)
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
  
  fixNonASCIICharacters <- FALSE
  
} # end fixNonASCIICharacters==TRUE



######## fix problem characters in the NCCA ASCII text file names ######## 



if(!exists("fixFileNames")) fixFileNames <- FALSE
# fixFileNames <- TRUE



if(fixFileNames==TRUE) {
  
  # fixFileNames <- TRUE
  
  # fixFileNames is initialized in the workFlow_init.R script
  
  print("fix problem characters in the file and directory names")
  
  # first fix all problem characters in file and directory names
  print("fix problem characters in NCCA ASCII files")
  
  source(paste0(RPath, 'fixFileNames.R'), echo=FALSE)
  
  # run the loop twice for search pattern 1
  if(length(uniqueExamNames1) > 0) {
    fixFileNamesFn(x=searchPattern1)
  }
  if(length(uniqueExamNames1) > 0) {
    fixFileNamesFn(x=searchPattern1)
  }
  # run the loop twice for search pattern 2
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
  
  fixFileNames <- FALSE
  
}



######## fix problem character strings in the NCCA ASCII files ######## 



if(!exists("fixStrings")) fixStrings <- FALSE
# fixStrings <- TRUE



if(isTRUE(fixStrings)) {
  
  # fixStrings <- TRUE
  
  # fixStrings is set in the workFlow_init.R script
  
  source(paste0(RPath, 'fixStrings.R'), echo=FALSE)
  
  # no output from this 
  # fixStringsFn(x="D&+", oldString=" 4KeyR", newString="  4Key")
  
  if(length(searchPattern1) > 0 ) {
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
    fixStringsFn(x="D&+", oldString=" AI ", newString=" EI ")
    fixStringsFn(x="D&+", oldString="--AI", newString="--EI")
    
    # Dec, 28, 2024
    # fixStringsFn(x="D&+", oldString=" MVT MVT", newString="  MV MV ")
    # fixStringsFn(x="D&+", oldString="-MVT", newString="--MV")
    # fixStringsFn(x="D&+", oldString=" MVT ", newString="  MV ")
    # fixStringsFn(x="D&+", oldString=" OSN OSN", newString="  OS OS ")
    # fixStringsFn(x="D&+", oldString="-OSN", newString="--OS")
    # fixStringsFn(x="D&+", oldString=" OSN ", newString="  OS ")
    
  }
  
  fixStrings <- FALSE
  
}



######## fix the sensor names in the NCCA ASCII text files if necessary ######## 



if(!exists("fixSensorNames")) fixSensorNames <- FALSE
# fixSensorNames <- TRUE



if(fixSensorNames==TRUE) {

  # fixSensorNames <- TRUE
  
  # fixSensorNames is set in the workFlow_init.R script
  
  print("fix the sensor names if necessary")
  
  source(paste0(RPath, 'fixActivitySensorName.R'), echo=FALSE)
  
  if(!is.null(searchPattern1)) {
    
    # PLE sensor name should be PPG1
    fixSensorNameFn(x="D&+", oldSensorName = "PLE1", newSensorName = "PPG1")
    fixSensorNameFn(x="D&+", oldSensorName = "  PL", newSensorName = "PPG1")
    
    # fixSensorNameFn(x="D&+", oldSensorName = "Move1", newSensorName = "MoveX")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux02", newSensorName = "Move1")
    
    # the NCCA ASCI spec uses "UPneumo" and LPnuemo"
    # while the NCCA pReview application seems to use "Upneumo" and "Lpneumo"
    # fixSensorNameFn(x="D&+", oldSensorName = "Upneumo", newSensorName = "UPneumo")
    # fixSensorNameFn(x="D&+", oldSensorName = "Lpneumo", newSensorName = "LPneumo")
    
    # use this to change the bad Move1 sensor to MoveX to avoid having 2 Move1 sensors
    # fixSensorNameFn(x="D&+", oldSensorName = "Move1", newSensorName = "MoveX")
    # fixSensorNameFn(x="D&+", oldSensorName = "MoveX", newSensorName = "Move1")
    # fixSensorNameFn(x="D&+", oldSensorName = "Aux01", newSensorName = "Move1")

    # fixMove1Fn(x="D&+", oldName="   SE", newName="Move1")
    
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
  
  fixSensorNames <- FALSE
  
} # end if fixSensorNames==TRUE



######## LOCATE the NCCAASCII text files in the CWD ######## 



{
  
  # source the getExamNames.R script to load the getCharts() and uniqueNames() functions
  # source(paste0(RPath, 'getExamNames.R'), echo=FALSE)
  # this is sourced by the workFlow_init.R script
  
  print("locate NCCA ASCCI text files and make a vector of exams in the cwd")
  
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



######## PARSE AND LOAD the NCCA ASCII text files in the cwd ######## 



if(!exists("NCCAASCIIParse")) NCCAASCIIParse <- TRUE
# NCCAASCIIParse <- FALSE



if(NCCAASCIIParse == TRUE) {
  
  # NCCAASCIIParse is inititialized in the workFlow_init.R script
  
  print("source the NCCAASCIIParse.R script to parse the data")
  source(paste0(RPath, 'NCCAASCII_Parse.R'), echo=FALSE)
  
  source(paste0(RPath, 'NCCAASCII_ParseHelperFunctions.R'), echo=FALSE)
  
  source("~/Dropbox/R/NCCA_ASCII_Parse/NCCAASCII_dataParse.R", echo = FALSE)
  
  # stop()
  
  # call the parseUniqueExams() from the NCCAASCII_Parse.R script
  # parseUniqueExams is vectorized 5-8-2020
  parseUniqueExams(uniqueExamNames=uniqueExamNames,
                   saveCSV=saveCSV,
                   makeDF=makeDF,
                   keepText=FALSE )
  
  print (paste(length(uniqueExamNames), "exams parsed"))
  print(uniqueExamNames)

} 



######## parse the stimulus events ######## 



if(!exists("stimulusParse")) stimulusParse <- FALSE
# stimulusParse <- TRUE



if(stimulusParse == TRUE) {
  
  # stimulusParse parameter is initialized in the workFlow_init.R script
  
  # first source the NCCAASCIIParseHelperFunctions.R script
  if(!exists("eventParse")) {
    source(paste0(RPath, 'NCCAASCII)_ParseHelperFunctions.R'), echo=FALSE)
  }
  
  # print("source the NCCAASCIIParseHelperFunctions.R script")
  
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



######## clean up ######## 



if(!exists("cleanUp")) cleanUp <- TRUE
# cleanUp <- FALSE



if(cleanUp == TRUE) {
  
  # cleanUp is initialized int he workFlow_init.R script
  
  # does not remove uniqueExamNames
  
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



######## SELECT EXAMS for processing ######## 



{
  
  # source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
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
  
  print("unique exams in this directory")
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
  
  # print(uniqueExams)

}



######## data PRE-PROCESSING ######## 



if(!exists("preProcessData")) preProcessData <- TRUE
# preProcessData <- FALSE



if(preProcessData == TRUE) {
  
  # preProcessData is initialized in the workFlow_init.R script
  
  # source the pre-processing script 
  # to center the onset at zero
  # set the range for each sensor at 1 to 1000
  # add some columns for events
  # add eventLabels and event Text
  
  # preProc will also correct the stimulus event data frame
  
  # source the sigProcHelper.R script to load the getFirstLastEventFn()
  # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
  # source this for the fixTagsFn()
  source(paste0(RPath, 'NCCAASCII_ParseHelperFunctions.R'), echo=FALSE)
  
  print("pre-processing")
  source(paste0(RPath, 'preProc.R'), echo=FALSE)
  # preProc(x=uniqueExams, makeDF=makeDF, output=output) 
  
  dataNames <- preProc(x=uniqueExams, makeDF=makeDF, output=output)
  
  print(dataNames)
  
  print (paste(length(uniqueExams), "exams pre-processed"))
  
} 



######## fix names of duplicated or REPEATED stimulus events ######## 



if(!exists("preProcessData")) fixDuplicateTags <- TRUE
# fixDuplicateTags <- FALSE



if(fixDuplicateTags == TRUE) {
  
  # fix repeated questions by appending to the name of the repetition 
  # for each chart in the exam data frame
  
  # first source the NCCAASCII_ParseHelperFunctions.R script to load the fixDupFn() function
  source(paste0(RPath, 'NCCAASCII_ParseHelperFunctions.R'), echo=FALSE)
  
  
  # then source the fixDup.R script 
  # to call the fixDupFn() 
  source(paste0(RPath, 'fixDup.R'), echo=FALSE)
  print("fix duplicated or repeated stmulus event names")
  
  ### this is done somewhere else in the sigProc.R script
  
  dataNames <- fixDuplicatesFn(x=uniqueExams, 
                               makeDF=makeDF, 
                               showNames=showNames, 
                               output=output)
  
  print(dataNames)
  
  print (paste0("checked for duplicate event labels in ",length(uniqueExams), " exams"))
  
  fixDuplicateTags <- FALSE
  
}



######## SIGNAL PROCESSING ######## 



if(!exists("processData")) processData <- TRUE
# processData <- FALSE



if(processData == TRUE) {
  
  # processData is initialized in the workFlow_init.R script
  
  # use EDAFilt="resp" to extract the respiration signal from the EDA data
  # EDAFilt="resp"
  # EDAFilt="laf"
  # EDAFilt="test"
  
  # source the scripts with signal processing functions for the sensors
  # source('pneumoSigProc.R', echo=FALSE)
  # source('EDASigProc.R', echo=FALSE)
  # source('cardioSigProc.R', echo=FALSE)
  # source('FCSigProc.R', echo=FALSE)
  # source('eCardioSigProc.R', echo=FALSE)
  # source('PLESigProc.R', echo=FALSE)
  # source('activitySigProc.R', echo=FALSE)
  
  # source('EDARespFilters.R', echo=FALSE)
  
  # source the sigProcHelper script to load the helper functions for signal processing
  # source('sigProcHelper.R', echo=FALSE)
  # source('sigProc_extra.R', echo=FALSE)
  
  source(paste0(RPath, 'sigProc.R'), echo=FALSE)
  
  # lowPass <- lowPass8th2hz_resp
  # highPass <- highPass8th1hz_resp
  
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
          scaleOffset=FALSE,
          makeDF=TRUE)
  # use makeDF=TRUE to save the _Data data frame to the global env
  
  # use scaleOffset=TRUE TO scale and offset the data using the init parameters
  
}



######## remove annotations ######## 



if(!exists("removeAnnotations")) removeAnnotations <- FALSE
# removeAnnotations <- TRUE



if(isTRUE(removeAnnotations)) {
  
  # removeAnnotations is initialized in the workFlow_init.R script
  
  # source("~/Dropbox/R/NCCA_ASCII_Parse/removeAnnotations.R", echo=FALSE)
  
  source(paste0(RPath, 'removeAnnotations.R'), echo=FALSE)
  
  # load the getFirstLastEventsFn()
  if(!exists("getFirstLastEventFn")) {
    source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  }
  
  removeAnnotationsFn(uniqueExams=uniqueExams)
  
}



######## fix DLST/TES question labels ######## 



if(!exists("fixDLSTLabels")) fixDLSTLabels <- FALSE
# fixDLSTLabels <- TRUE



if(isTRUE(fixDLSTLabels)) {
  
  # June 20, 2025
  # Stoelting software does not accept the correct DLST/TES question labels
  
  # call a function to fix the DLST/TES question labele
  # 1C1, 1R1, 1R2, 1C2, 2R1, 2R2, 2C1, 3R1, 3R2, 2C2, 4R1, 4R2, 3C1
  # necessary when examiners use simple question labels 
  # C1, R1, R1, C2, R1, R2, C1, R1, R2, C2, R1, R2, C1
  
  
  source(paste0(RPath, 'fixDLSTLabels.R'), echo=FALSE)
  
  
  fixDLSTLabelsFn(x="*_Data$", y="_Stimuli$")
  
  fixDLSTLabels <- FALSE
  
}



######## trim excess time from data ######## 



if(!exists("trimExcessTime")) trimExcessTime <- FALSE
# trimExcessTime <- TRUE



if(isTRUE(trimExcessTime)) {
  
  # trimExcessTime is initialized in the workFlow_init.R script
  
  # must be after signal processing to avoid problems
  # Aug 24, 2020
  
  # source(paste0(RPath, 'toMinSec.R'), echo=FALSE)
  source(paste0(RPath, 'dataTrim.R'), echo=FALSE)
  
  dataTrimFn(x="*_Data$", y="_Stimuli$", pre=8, post=20)
  
}



######## save the RDA1 data for each exam in the cwd ######## 



if(!exists("saveRDA1")) saveRDA1 <- FALSE
# saveRDA1 <- TRUE



if(saveRDA1==TRUE) {
  
  # save the data for each exam in .Rda .RData format
  
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
  
  saveRDA1 <- FALSE
  
  # if(exists("saveRDA1")) rm(saveRDA1)
  
} # end if saveRDA1 == TRUE



######## loadRDA1 the Rda data from the current working directory ######## 



if(!exists("loadRDA1")) loadRDA1 <- FALSE
# loadRDA1 <- TRUE



# myRdaNames <- list.files(pattern="*_1.Rda$")

# myRdaNames <- gsub("\\$", "\\\\$", myRdaNames)
# myRdaNames <- myRdaNames[2
# myRdaNames <- gsub("\\.Rda", ".Rda", myRdaNames)



if(loadRDA1==TRUE) {
  
  print("load the exams from the current directory")
  
  # rm(list=ls())
  if(!exists("selectExams")) { selectExams <- "ALL" }
  rmVector <- ls()[-(which(ls() == "selectExams"))]
  if(exists("uniqueExamNames")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames")] }
  if(exists("uniqueExamNames2")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames2")] }
  
  if(exists("myDF")) { rmVector <- rmVector[-which(rmVector=="myDF")] }
  
  rm(list=rmVector)
  
  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
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
  
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  {
    
    # make a function to make a list of unique exams in the global environment
    getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
    
    # get exam names from the _Data data frames
    print("make a list of unique exams in the global environment")
    uniqueExams <- getUniqueExams(x="*_Data$")
    
    print(uniqueExams)
    
  }
  
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  # source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  loadRDA1 <- FALSE
  
  # rm(loadRDA1)
  # rm(searchPattern)

} # end if loadRDA1 <- TRUE



######## get unique exams from the CWD ######## 



{
  
  # load a library of string functions (str_sub) used by the getUniqueExams()
  library(stringr)
  
  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  # this is sourced by the workFlow_init.R script
  
  source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)
  
  # make a function to make a list of unique exams in the global environment
  if(!exists("getUniqueExams")) {
    getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
    # July 6, 2025 a function to make it easier to recreate the uniqueExams vector
    uniqueExamsFn <- function(x="*_Data$") { 
      assign("uniqueExams", unique(str_sub(ls(pattern=x, pos=1),1, -6)), envir=.GlobalEnv)
    }
  }
  
  print("make a list of unique exams in the global environment")
  
  # get exam names from the _Data data frames
  uniqueExams <- getUniqueExams(x="*_Data$")
  # uniqueExams <- uniqueExams[1]
  
  # print(paste(length(uniqueExams), "exams in the global environment"))
  
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



######## SCALE and offset the data ######## 



if(!exists("scaleOffsetData")) scaleOffsetData <- TRUE
# scaleOffsetData <- FALSE



if(scaleOffsetData) { 
  
  # scaleOffsetData is initialized in the workFlow_init.R script

  # scaling was previously called during signal processing
  
  # but is now called separately 
  # to enable the study of potential differences in feature extraction, R/C ratios, and scores
  # that might occur at different scaling aesthetics
  # ideally there will be no differences
  # but differences may occur at extreme (near zero) scaling values 
  
  # RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"

  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)

  source(paste0(RPath, 'scaleOffsetData.R'), echo=FALSE)

  ScaleOffsetDataFn(x=uniqueExams, makeDF=TRUE, output=FALSE, saveChartDF=FALSE)

}



######## save the RDA2 data for each exam in the cwd ######## 



if(!exists("#saveRDA2")) saveRDA2 <- TRUE
# saveRDA2 <- FALSE



if(saveRDA2==TRUE) {
  
  # saveRDA2 is initialized in the workFlow_init.R script

  ### save the data for each exam in .Rda .RData format
  
  # library(stringr)
  
  # # fix the file names
  # # uniqueExamNames <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  # uniqueExamNames2 <- gsub("\\$", "\\\\$", uniqueExams)
  # uniqueExamNames2 <- gsub("D\\$_", "", uniqueExamNames2)
  # uniqueExamNames2 <- gsub("D&_", "", uniqueExamNames2)
  # uniqueExamNames2 <- gsub("D%_", "", uniqueExamNames2)
  # uniqueExamNames2 <- gsub("D#_", "", uniqueExamNames2)
  # uniqueExamNames2 <- gsub("D&_", "", uniqueExamNames2)
  # # use of [:punct:] seems to cause a problem with some file names
  # # uniqueExamNames2 <- gsub("[:punct:]", "", uniqueExamNames2)
  # #
  # uniqueExams <- gsub("D&_", "", uniqueExamNames2)
  # uniqueExams <- gsub("D\\$_", "", uniqueExams)
  # uniqueExams <- gsub("D%_", "", uniqueExams)
  # uniqueExams <- gsub("D#_", "", uniqueExams)
  # uniqueExams <- gsub("\\$", "", uniqueExams)
  # uniqueExams <- gsub("\\.", "_", uniqueExams)
  # uniqueExams <- gsub("#", "", uniqueExams)
  # uniqueExams <- gsub("%", "", uniqueExams)
  # # uniqueExams <- gsub("[:punct:]", "", uniqueExams)
  # # uniqueExams <- uniqueExams[c(3,5)]
  # 
  # # then iterate over the unique exams in the global environment
  # i=1
  # for (i in 1:length(uniqueExams)) {
  #   # Dec 31, 2021 modified to restrict similar names
  #   objList <- ls(pattern=uniqueExamNames2[i], pos=1)
  #   j=1
  #   for(j in 1:length(objList)) {
  #     # get the name before the "_"
  #     thisObj <- 
  #       str_sub(objList[j], 1, which(strsplit(objList[j], "")[[1]] == "_")[1] - 1)
  #     if(thisObj != uniqueExams[i]) {
  #       # coerce the object name to NA if not a match
  #       objList[j] <- NA
  #     }
  #   }    
  #   objList <- objList[!is.na(objList)]
  #   # save the objects for this exam
  #   save(list=objList, file=paste0(uniqueExams[i], "_2.Rda"))
  # }
  # 
  # # rm(uniqueExams)
  # rm(i)
  # 
  # # rm(saveRDA2)

}



######## EXPORT to Lafayette NCCA ASCII format ######## 



if(!exists("writeNCCAASCII_LAF")) writeNCCAASCII_LAF <- FALSE
# writeNCCAASCII_LAF <- TRUE



if(isTRUE(writeNCCAASCII_LAF)) {
  
  # writeNCCAASCII_LAF is intitialized in the workFlow_init.R script
  
  # sourcing this file will export the data to Lafayette NCCA ASCII format
  # without calling a function
  source(paste0(RPath, 'NCCAASCII_OUTPUT.R'), echo=FALSE)
  
  writeNCCAASCII_LAF <- FALSE

}



######## ARTIFACT processing ######## 



# processArtifacts is set in the workFlow_init.R script
if(!exists("processArtifacts")) processArtifacts <- TRUE
# processArtifacts <- FALSE



if(processArtifacts == TRUE) {
  
  # processArtifacts is set in the workFlow_init.R script
  
  # print("artifact extraction")

  source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  # source('~/Dropbox/NCCAASCII_init.R', echo=FALSE)

  # source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)
  
  # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
  # digital filters
  # source(paste0(RPath, 'sigProc_extra.R'), echo=FALSE)

  # a function to iterate over exams and sensors
  source(paste0(RPath, 'artifactProc.R'), echo=FALSE)

  # call a function to iterate over the exams in the global environment
  artifactProcFn(x=uniqueExams)
  
  # chartDF$AutoEDA_a[which(chartDF$AutoEDA_a != "0")]

  # which(chartDF$Artifacts_a != 0)

  # source(paste0(RPath, 'chartPlot_gg.R'), echo=FALSE)

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



######## check for physical activity ######## 



if(processArtifacts == TRUE && activityArtifacts == TRUE) {

  # source(paste0(RPath, 'getSegment.R'), echo=FALSE)
  # source(paste0(RPath, 'activityCheck.R'), echo=FALSE)
  #
  # print("check for activity sensor artifacts")
  # chartFUN <- activityCheck
  # getExamFn(x=uniqueExams)
  #
  # # source(paste0(RPath, 'chartPlot.R'), echo=FALSE)

}



######## EDA artifacts ######## 



if(processArtifacts == TRUE && edaArtifacts == TRUE) {

  # source(paste0(RPath, 'getSegment.R'), echo=FALSE)
  # 
  # source(paste0(RPath, 'dataCheck.R'), echo=FALSE)
  # 
  # print("check for unresponsive EDA data")
  # chartFUN <- dataCheckFn
  # getExamFn(x=uniqueExams)
  # 
  # # reset the warnings
  # # assign("last.warning", NULL, envir = baseenv())
  # 
  # source(paste0(RPath, 'nonstimArtifacts.R'), echo=FALSE)
  # 
  # print("check for non-specific EDA responses")
  # chartFUN=nonStimArtifactFn
  # getExamFn(x=uniqueExams)
  # 
  # source(paste0(RPath, 'EDAMvtArtifact.R'), echo=FALSE)
  # 
  # print("check for finger movement artifacts in the EDA data")
  # chartFUN=EDAMvtArtifactFn
  # getExamFn(x=uniqueExams)

}



######## pneumo artifacts ######## 



if(processArtifacts == TRUE && pneumoArtifacts == TRUE) {

  # sec <- .5
  # cutProp <- .25
  # # sec=3 cutProp=.5 works well 3-18-2017
  # # sec=1 cutProb=.25 also works ok
  # 
  # 
  # # sec2=4 cutProp2=.5 works well 3-19-2017
  # sec2 <- 3
  # cutProp2 <- .01
  # 
  # source(paste0(RPath, 'getSegment.R'), echo=FALSE)
  # 
  # source(paste0(RPath, 'pneumoCheck.R'), echo=FALSE)
  # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  # source(paste0(RPath, 'tukeyFences.R'), echo=FALSE)
  # 
  # print("check for unresponsive respiration data")
  # chartFUN <- pneumoCheckFn
  # getExamFn(x=uniqueExams)
  # 
  # source(paste0(RPath, 'pneumoArtifact.R'), echo=FALSE)
  # 
  # print("respiration artifacts")
  # chartFUN=pneumoArtifactFn
  # getExamFn(x=uniqueExams)

}



######## cardio artifacts ######## 



if(processArtifacts == TRUE && cardioArtifacts == TRUE) {
  # 
  #   source(paste0(RPath, 'getSegment.R'), echo=FALSE)
  #   source(paste0(RPath, 'cardioArtifact.R'), echo=FALSE)
  # 
  #   # print("check for artifacts in the cardio data")
  #   # chartFUN=cardioArtifactFn
  #   # getExamFn(x=uniqueExams)
  # 
}



######## PLE artifacts ######## 



if(processArtifacts == TRUE && pleArtifacts == TRUE) {
  # 
  # source(paste0(RPath, 'getSegment.R'), echo=FALSE)
  # source(paste0(RPath, 'pleArtifact.R'), echo=FALSE)
  # 
  # # print("PLE artifacts")
  # # chartFUN=pleArtifactFn
  # # getExamFn(x=uniqueExams)
  # 
}



######## FEATURE EXTRACTION ######## 



if(!exists("extractFeatures")) extractFeatures <- TRUE
# extractFeatures <- FALSE



if(extractFeatures == TRUE) {

  # extractFeatures is initialized in the workFlow_init.R script
  
  # source the feature extraction function
  source(paste0(RPath, 'featureExtraction.R'), echo=FALSE)

  print("feature extraction")
  
  if(!exists("uniqueExams")) {
    # uniqueExams <- getUniqueExams(x="*_Data$")
    uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
  }
  
  featureExtraction(x=uniqueExams, 
                    extractPneumo=extractPneumo, 
                    extractEDA=extractEDA, 
                    extractCardio=extractCardio, 
                    extractPLE=extractPLE, 
                    extractPneumoPatterns=extractPneumoPatterns, 
                    writeCSV=FALSE)
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

} # end feature extraction 



######## integrate artifacts ######## 



if(!exists("integrateArtifacts")) integrateArtifacts <- FALSE
# integrateArtifacts <- TRUE



if(integrateArtifacts == TRUE) {
  
  # integrateArtifacts is initialized in the workFlow_init.R script
  
  # # check the Artifacts_a channel 
  # # remove response onset and response end 
  # # if artifacts have occurred from 3 sec prestim seconds to .5 sec after response end
  # 
  # source(paste0(RPath, 'integrateArtifacts.R'), echo=FALSE)
  # 
  # 
  # 
  # integrateArtifactsFn(x=uniqueExams)
  
}


  
######## initialize a _MEASUREMENTS data frame ######## 



if(!exists("getScores")) getScores<- TRUE
# getScores <- FALSE



if(getScores == TRUE) { 
  
  # getScores is initialized in the workFlow_init.R script
  
  ### initialize a _Measurements data frame for each exam
  
  # _Measurements data frame is needed by the scoring algorithms
  
  # source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  
  print("make a data frame of _Measurments for each exam")
  
  # source the extractMeasurements.R script to get the Kircher measurements
  
  source(paste0(RPath, 'extractMeasurements.R'), echo=FALSE)
  
  extractMeasurementsFn(x=uniqueExams)
  # writeCSV=true will save the _Measurements.CSV data frame
  
  # _Measurements.csv and _Measurements data frame have no scores
  # but does have the feature extraction values 
  
} # end measurements # 



######## SCORES ######## 



if(!exists("getScores")) getScores <- TRUE
# getScores <- FALSE



if(getScores == TRUE) {

  # getScores is initialized in the workFlow_init.R script
  
  print("score the data")
  
  # source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  source(paste0(RPath, 'newScores.R'), echo=TRUE)
  
  # uniqueExams <- getUniqueExams(x="*_Data$")
  
  newScoresFn(uniqueExams=uniqueExams, 
              showNames=TRUE, 
              makeDF=FALSE, 
              saveCSV=FALSE, 
              output=FALSE )
  
  # source the script to load the getScoresFn() to calculate all numerical scores
  # source(paste0(RPath, 'scores.R'), echo=FALSE)
  
  # load the getExamFn
  # source(paste0(RPath, 'getSegment.R'), echo=FALSE)

  # declare this after sourciong the getSegment.R script  
  # seriesFUN <- getScoresFn
  # this will be called using do.call() which can take a function or character string as the first arg
  
  # i=1 
  # length(uniqueExams)
  # for(i in 1:length(uniqueExams)) {
  #   getExamFn(x=uniqueExams[i])
  # }

  # write.csv(measurementDF, file = "QC_MeasurementDF.csv", row.names = TRUE)

  #### PCASS Scores
  
  # source(paste0(RPath, 'PCASS_feature_extraction.R'))
  # 
  # if(isTRUE(PCASSFormat)) {
  #   PCASSAlgorithmFn(x=uniqueExams)
  # }
  
  #### load and aggregate the seriesTotals.csv 
  
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



######## save measurements ######## 



if(!exists("saveMeasurements")) saveMeasurements <- TRUE
# saveMeasurements <- FALSE



if(getScores == TRUE && saveMeasurements == TRUE) {
  
  # saveMeasurements is initialized in the workFlow_init.R script
  
  ### save the _Measurements data frame to a .csv for each chart for each exam
  
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  
  library(stringr)
  
  print("save the _Measurments to a .CSV file")
  
  # source the outputScores.R script for the measurementTableFn
  source(paste0(RPath, 'outputScores.R'), echo=FALSE)
  
  source(paste0(RPath, 'newMeasurementsScores.R'), echo=FALSE)
  
  newMeasurementsScoresFn(uniqueExams=uniqueExams,
                          makeDF=FALSE,
                          saveCSV=TRUE,
                          MeasurementsDF=TRUE,
                          measurementTable=TRUE,
                          transpose=FALSE )
  
  # *_measurementTable.csv is now in the cwd with feature extraction values
  
  # source the measurementsScores.R script to get the Kircher measurements
  # source(paste0(RPath, 'measurementsScores.R'))
  
  # examFUN <- measurementsScoresFn
  
  # print("save the measurements and score to a .csv file")
  # getExamFn(x=uniqueExams)
  
}



######## save the _Measurements data frames to .csv ######## 



{
  
  # fileList <- ls(pattern="*_Measurements")
  # 
  # for(i in 1:length(fileList)) {
  #   thisMeasurementDF <- get(fileList[i])
  #   fileName <- paste0(fileList[i], ".csv")
  #   write_csv(thisMeasurementDF, fileName)
  # }
  
}



######## aggregate the measurements for all sample cases ######## 



{
  
  # January 19, 2025
  
  
  
  
  
  
}



######## save the RDA2 data for each exam in the cwd ######## 



if(!exists("saveRDA2")) saveRDA2 <- TRUE
# saveRDA2 <- FALSE



if(saveRDA2==TRUE) {
  
  # saveRDA2 is initialized in the workFlow_init.R script
  
  # save the data for each exam in .Rda .RData format
  
  # library(stringr)
  
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
    # Dec 31, 2021 modified to restrict similar names
    objList <- ls(pattern=uniqueExamNames2[i], pos=1)
    j=1
    for(j in 1:length(objList)) {
      # get the name before the "_"
      thisObj <- 
        str_sub(objList[j], 1, which(strsplit(objList[j], "")[[1]] == "_")[1] - 1)
      if(thisObj != uniqueExams[i]) {
        # coerce the object name to NA if not a match
        objList[j] <- NA
      }
    }    
    objList <- objList[!is.na(objList)]
    # save the objects for this exam
    save(list=objList, file=paste0(uniqueExams[i], "_2.Rda"))
  }
  
  # rm(uniqueExams)
  rm(i)

  # rm(saveRDA2)
  
}



######## LOAD RDA2 the data from the current working directory  ######### 



{
  
  if(!exists("RPath")) {
    # mac
    RPath <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    # windows
    # RPath <- "C://Users/raymo/Dropbox/R/NCCA_ASCII_Parse/"
  }
  
  library(stringr)
  
  # source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  
  # make a function to make a list of unique exams in the global environment
  if(!exists("getUniqueExams")) {
    getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  }
  
  if(length(getUniqueExams(x="*_Data$")) == 0) loadRDA2 <- TRUE
  
  if(!exists("loadRDA2")) loadRDA2 <- TRUE
  # loadRDA2 <- FALSE
  
} 



if(loadRDA2==TRUE) {
  
  # loadRDA2 is initialized in the workFlow_init.R script
  
  # load the str_sub() function used by getUniqueExams()
  library(stringr)
  
  # if(!exists("getUniqueExams")) {
  #   getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  # }
  
  # if(!exists("RPath")) RPath <- "~/Dropbox/"
  # RPath <- "~/Dropbox/"
  
  if(!exists("selectExams")) { selectExams <- "ALL" }
  # a vector of items to remove
  rmVector <- ls()[-(which(ls() %in% c("RPath", "selectExams")))]  
  
  if(exists("uniqueExamNames")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames")] }
  if(exists("uniqueExamNames2")) { rmVector <- rmVector[-which(rmVector=="uniqueExamNames2")] }

  if(exists("myDF")) { rmVector <- rmVector[-which(rmVector=="myDF")] }
  
  rm(list=rmVector)
  
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
  
  # source(paste0(RPath, 'NCCAASCII_init.R'), echo=FALSE)
  # source(paste0(RPath, 'excludedEvents.R'), echo=FALSE)
  # source(paste0(RPath, 'getSegment.R'), echo=FALSE)
  
  # source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
  ### get exam names from the _Data data frames
  
  if(!exists("getUniqueExams")) {
    getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
  }
  
  print("make a list of unique exams in the global environment")
  uniqueExams <- getUniqueExams(x="*_Data$")
  # uniqueExams <- uniqueExams[1]
  # print(paste(length(uniqueExams), "exams in the global environment"))
  
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
  
  # source(paste0(RPath, 'workFlow_init.R'), echo=FALSE)
  
  print(paste(length(uniqueExams), "exams selected"))
  print(uniqueExams)
  
  loadRDA2 <- FALSE
  
  # rm(loadRDA2)
  
} # end if(loadRDA2 == TRUE)



######## save / load .RData ######## 



{
  
  # save and load the workspace 
  
  # save.image()
  # load(".RData")
  
}



######## PRINTING ######## 



if(!exists("printCharts")) printCharts <- TRUE
# printCharts <- FALSE



# getOption("warn")



if(isTRUE(printCharts)) {
  # source the chartPlot.R script to print the plots
  source(paste0(RPath, 'chartPlot_gg.R'), echo=FALSE)
}
 


if(!exists("printSegments")) printSegments <- FALSE
# printSegments <- TRUE



# plot the segments
# source(paste0(RPath, 'segmentPlot_init.R'))
if(isTRUE(printSegments)) {
  source(paste0(RPath, 'segmentPlot_gg.R'), echo=FALSE)
}



######## save algorithm output to .txt ######## 



# set in the workFlow_init.R script
if(!exists("saveAlgorithmTXT")) saveAlgorithmTXT<- FALSE
# saveAlgorithmTXT <- TRUE



if(isTRUE(saveAlgorithmTXT)) {
  
  source(paste0(RPath, 'saveAnalysisToTXT.R'), echo=FALSE)
  # saveAlgorithmTXT <- FALSE
  
  # call the function
  saveAnalysistoTXTFn(x=".ANALYSIS")
  
  saveAlgorithmTXT <- FALSE
  
}



######## save analysis .txt reports to pdf ######## 



if(!exists("saveResultsToPDF")) saveResultsToPDF <- FALSE
# saveResultsToPDF <- TRUE



# install.packages("rmarkdown")
# library(rmarkdown)



if(isTRUE(saveResultsToPDF)) {
  
  # saveResultsToPDF is initialized in the NCCAASCII_init.R script
  
  # install.packages("rmarkdown")
  # library(rmarkdown)
  
  # define the txtToPDF() function
  source(paste0(RPath, 'saveAnalysisToPDF.R'), echo=FALSE)
  
  analysisOutputFiles <- list.files(pattern="Output")
  # exclude file that are not .txt
  analysisOutputFiles <- analysisOutputFiles[grep(".txt", analysisOutputFiles)]
  
  # need to remove .pdf files from the vector
  analysisOutputFiles <- 
    analysisOutputFiles[str_sub(analysisOutputFiles, -4, -1) != ".pdf"]
  
  # call the txtToPDF() function
  txtToPDF(x=analysisOutputFiles)
  
  saveResultsToPDF <- FALSE
  
}



######## copy problem exams ######## 



if(!exists("copyProblems")) copyProblems<- FALSE
# copyProblems <- TRUE



if(isTRUE(copyProblems)) {
  
  library(readr)
  
  examList <- str_sub(list.files(pattern="chartPlot.pdf"), 2, -15)
  # examList <- str_sub(examList, 2, -15)
  
  # numbered  cases
  # examList <- str_sub(examList, -17, -15)
  # axciton names
  
  # sensorTotalsDF <- read_csv("ALL_CASES_100_sensorTotals.csv")
  # seriesTotalsDF <- read_csv("ALL_CASES_60_seriesTotals.csv")
  # OSS3SummarysDF <- read_csv("ALL_CASES_60_OSS3Summary.csv")
  
  # View(OSS3SummaryDF)
  # View(sensorTotalsDF)
  
  # theseProblems <- c(3)
  # theseProblems <- c(5, 6)
  # theseProblems <- c(4)
  # theseProblems <- c(5)
  # theseProblems <- c(6)
  theseProblems <- c(3:6)
  
  OSS3SummaryDF$examName[OSS3SummaryDF$correctCode %in% theseProblems]
  
  # thisSubDir <- "INC_G"
  # thisSubDir <- "INC_I"
  # thisSubDir <- "FN"
  # thisSubDir <- "FP"
  thisSubDir <- "problems"
  
  # ESS-M
  problemCaseList <- OSS3SummaryDF$examName[OSS3SummaryDF$correctCode %in% theseProblems]
  
  # seriesTotalsDF$correctCode
  
  # PCASS2SummaryDF <- read_csv("~/Dropbox/R/NCCA_ASCII_Parse/data/Ohio_PPG_data/Ohio_NCCAASCII/ALL_CASES_40_PCASS2Summary.csv")
  
  # PCASS-2
  # problemCaseList <- 
  #   PCASS2SummaryDF$examName[PCASS2SummaryDF$correctCode %in% c(4)]
  
  # problemCaseList %in% examList
  
  # caseList <- list(files())
  
  # if(!dir.exists("problems")) dir.create("problems")
  
  # lafayette cases
  # allCases <- list.files(path="..", pattern="D\\&-", full.names=FALSE)
  allCases <- list.files(pattern="D\\&", full.names=FALSE)

  # axciton cases
  # allCases <- list.files(pattern="D\\$", full.names=FALSE)
  
  # numbered cases
  # theseFiles <- allCases[str_sub(allCases, -9, -7) %in% examList]
  # axciton cases
  # theseFiles <- allCases[str_replace_all(str_sub(allCases, 4, -7), "\\$", "") %in% problemCaseList]
  
  # change the wortking directory # 
  # setwd("./FP")
  # setwd("./FN")
  # setwd("./FN")
  # setwd(paste0("./", thisSubDir))
  # setwd("./FP_INC")
  
  # file.copy(paste0("../", theseFiles), theseFiles)
  
  # examList <- str_sub(list.files(pattern="chartPlot.pdf"), 2, -15)
  
  # use the chartPlot.pdf files in the cwd
  problemCaseList <- examList
  
  problemCaseList %in% examList
  
  # if(!dir.exists("problems")) dir.create("problems")
  
  i=1
  for(i in 1:length(problemCaseList)) {
    
    theseFiles <- list.files(pattern=problemCaseList[i])
    
    file.copy(theseFiles, paste0("./", thisSubDir, "/", theseFiles))
    
  }
  
  copyProblems <- FALSE
  
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



######## end work flow ######## 



# source(paste0(RPath, 'summarizeAccuracy.R'), echo=FALSE)



