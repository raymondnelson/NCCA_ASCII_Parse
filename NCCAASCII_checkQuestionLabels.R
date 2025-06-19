# R script to locate NCCA ASCII charts that do or do not conform to a question template
# February 18, 2025
# Raymond Nelson
####



library(stringr)

source("C:/Users/raymo/Dropbox/DATASETS_BACKUP/OSSN300/annotations.R", echo=FALSE)
# annotationList

# questionSequence <- c("X", "1", "1A", "1B", "2", "SR", "SA", "SA2", "SR2", "2SA", "2SR", "3", "3E", "E3", "C4", "4C", "R5", "5R", "C6", "6C", "R7", "7R", "8", "E8", "8E", "C9", "9C", "R10", "10R", "XX")
questionSequence <- c("X", "1", "2", "3", "C4", "R5", "C6", "R7", "8", "C9", "R10", "XX")


NCCAPrefix <- "D$-"


{
  outputVc <- NULL
  missingQuestions <- NULL
  missingXXXAnnouncements <- NULL
  repeatedQuestions <- NULL
  insertedQuestions <- NULL
  insertedAnnotations <- NULL
  SKYQuestions <- NULL
  punctQuestions <- NULL
  beforeXAfterXX <- NULL
  blankLabels <- NULL
  question1ABCDE <- NULL
  unlabeledQuestions <- NULL
  problemTags <- NULL
}


{
  
  searchString <- "^D\\$+"
  
  # Stoelting #
  # 2010 changed to !
  # searchPattern <- "^D#+"
  # Limestone %
  # searchPattern <- "^D%+"
  # 2010 changed to @
  # Axciton 
  # searchPattern <- "^D\\$"
  # $ character needs to be double escaped because it is a special character for grep
  
  # fix problem characters in the AXCITON file names
  # special characters must be escaped twice
  # once for R and once for the system
  # multiple special characters can be used after a double escape
  # searchString <- gsub("\\$", "\\\\$", searchString)
  
  # make a vector of the names of exam charts in the current working directory
  NCCAASCII_names <- list.files(path = ".", 
                                pattern = searchString, 
                                all.files = FALSE,
                                full.names = FALSE, 
                                recursive = FALSE,
                                ignore.case = FALSE, 
                                include.dirs = FALSE,
                                no.. = FALSE)
  
  # exclude partial name matches
  # examCharts <- examCharts[which( substr(examCharts, 1, nchar(examCharts)-6) == 
  #                                   unique(substr(examCharts, 1, nchar(examCharts)-6))[1] )]
  
  
  # fix problem characters in the AXCITON file names
  # special characters must be escaped twice
  # once for R and once for the system
  # multiple special characters can be used after a double escape
  # examCharts <- gsub("\\$", "\\\\$", examCharts)
  
}


  
i=1
for(i in 1:length(NCCAASCII_names)) {
  
  {
    
    thisNCCAASCIIName <- NCCAASCII_names[i]
    print(paste(thisNCCAASCIIName, i))
    
    examName <- str_sub(thisNCCAASCIIName, 4, -7)
    
    seriesName <- as.character(str_sub(thisNCCAASCIIName, -5, -5))
    
    chartName <- as.character(str_sub(thisNCCAASCIIName, -3, -1))
    
    # read the NCCA ASCII text file
    textLines <- readLines(paste0(NCCAPrefix, examName, "-", seriesName, ".", chartName))
  }
  
  ## locate the time series data rows ##
  
  {
    # nchar("Sample     Time    Label")
    tsHeaderTextFragment <- "Sample     Time    Label"
    tsStartRow <- which(str_sub(textLines[1:150], 1, 24) == tsHeaderTextFragment) + 1
    tsEndRow <- length(textLines)
    tsHeaderRow <- tsStartRow - 1
  }
  
  ## locate the events table ##
  
  {
    # nchar("Event    Label      Begin        End     Answer")
    eventsHeaderTextFragment <- "Event    Label      Begin        End     Answer"
    eventsStartRow <- which(str_sub(textLines[1:150], 1, 47) == eventsHeaderTextFragment) + 1
    eventsHeaderRow <- eventsStartRow - 1
    # slice the events table
    eventsTableText <- textLines[c(eventsHeaderRow:(tsHeaderRow-2))]
    numberOfEvents <- length(eventsTableText) - 1
  }
  
  ## locate the stimulus question list ##
  
  {
    questionListHeaderTextFragment <- "Event    Label Statement"
    questionListStartRow <- which(str_sub(textLines[1:150], 1, 24) == questionListHeaderTextFragment) + 1
    questionListHeaderRow <- questionListStartRow - 1
    questionListEndRow <- eventsHeaderRow - 2
    # slice the question list
    questionListText <- textLines[c(questionListHeaderRow:questionListEndRow)]
    numberOfQuestions <- numberOfEvents
  }
  
  ## get the question labels and stimulus onset indices ##
  
  {
    eventLabelsVc <- NULL
    for(j in 2:length(eventsTableText)) {
      # 7:14 character columns for the event labels
      eventLabelsVc <- c(eventLabelsVc, str_trim(str_sub(eventsTableText[j], 7, 14)))
    }
    eventBeginVc <- NULL
    for(j in 2:length(eventsTableText)) {
      # 16:25 character columns for stimulus onset indices
      eventBeginVc <- c(eventBeginVc, as.numeric(str_trim(str_sub(eventsTableText[j], 16, 25))))
    }
    
  }
  
  ############################################
  
  ## check for X and XX announcements ##
  
  if( any(!(c("X", "XX") %in% eventLabelsVc)) ) {
    missingXXXAnnouncements <- c(missingXXXAnnouncements, i)
  }
  
  ## check for events prior to XX or after XX
  
  if( any(eventLabelsVc[1] != "X", eventLabelsVc[length(eventLabelsVc)] != "XX") ) {
    beforeXAfterXX <- c(beforeXAfterXX, i)
  }
  
  ## check for questions c("1A", "1B", "1C", "1D", "1E") ##
  
  if( any(c("1A", "1B", "1C", "1D", "1E") %in% eventLabelsVc) ) {
    question1ABCDE <- c(question1ABCDE, i)
  }
  
  ## check for question labels with punctuation [.] ##
  
  if( any(grepl(pattern="[[:punct:]]", x=eventLabelsVc)) ) {
    punctQuestions <- c(punctQuestions, i)
  }
  
  ## check for blank event Labels and 0 event labels ##
  
  if( any(eventLabelsVc == "") || any(eventLabelsVc == "0")) {
    blankLabels <- c(blankLabels, i)
  } 
  
  ## check for unlabeled CQs and RQs ##
  
  if(length(which(grepl(pattern="[CR]", eventLabelsVc))) < 6) {
    unlabeledQuestions <- c(unlabeledQuestions, i)
  }
  
  ## check if all questions (per the test format) are present in the exam ##
  
  if( any(!(questionSequence %in% eventLabelsVc)) ) {
    missingQuestions <- c(missingQuestions, i)
  }
  
  ## check for SKY questions ##
  
  if( any(c("S", "K", "Y", "U") %in% eventLabelsVc) ) {
    SKYQuestions <- c(SKYQuestions, i)
  }
  
  ## check for inserted/other questions ##
  
  if( any(!(eventLabelsVc %in% questionSequence)) ) {
    insertedQuestions <- c(insertedQuestions, i)
  }
  
  ## check for annotations ##
  
  if( any(anotationList %in% toupper(eventLabelsVc)) ) {
    insertedAnnotations <- c(insertedAnnotations, i)
  }
  
  ## check for repeated questions ##
  
  if( any(duplicated(eventLabelsVc)) ) {
    repeatedQuestions <- c(repeatedQuestions, i)
  }
  
  ## check for specific question tags ##
  
  if( any(toupper(eventLabelsVc) %in% c("c", "ct", "wr", "wrq", "cl", "i", "t", "bi", "ai", "tt", "mm", "m", "ttmm", "sn", "sw", "db", "bd", "ei", "i", "c", "ct", "ee") ) ) {
    problemTags <- c(problemTags, i)
  }
  
} # end loop


locateEventsFn(NCCAASCII_names[1])


# NCCAASCII_names[c(203, 204, 318)]
# NCCAASCII_names[171]

NCCAASCIIName <- NCCAASCII_names[1]

NCCAASCII_names[blankLabels]



# copyThese <- NCCAASCII_names[c(17, 40, 41, 42, 43, 45, 50, 53, 55, 57, 58, 59, 64, 65)]
copyThese <- NCCAASCII_names[c(17, 40, 41, 42, 43, 45, 50, 53, 55, 57, 58, 59, 64, 65)]

# file.copy(from=paste0("../", copyThese),
#           to=paste0("./", copyThese),
#           overwrite=TRUE)


# list.files()



