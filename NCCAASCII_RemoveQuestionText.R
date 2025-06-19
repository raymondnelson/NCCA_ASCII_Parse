# R script to remove question text from NCCA ASCII files in the current working directory
# April 9, 2020
# Raymond Nelson

# call this script from the R console
# first set the current working directory where the NCCA ASCII files are located
# this script will iterate over the NCCA ASCII files
# import each file and overwrite each file without the question text

# WARNING: This process is destructive. Backup the NCCA ASCII files before proceeding.

####



# use setwd() to set the current working directory
# setwd("directory name")





# import the stringr library for the str_trm and str_sub functions
library("stringr")

# use search() to check installed packages
# use install.package("stringr") to install the package if necessary


print("search the working directory for NCCA ASCII text output")
# Lafayette uses the &
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
examCharts <- list.files(path = ".", 
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


removeText <- TRUE
removeText <- FALSE

if(removeText) {
  
  # iterate over the names of examCharts and read the files
  i=1 # needed for testing only
  for (i in 1:length(examCharts)) {
    
    print(paste("reading:", examCharts[i]))
    
    thisChartName <- examCharts[i]
      
    # thisChartName <- gsub("\\\\$", "\$", thisChartName)
    
    # read the lines from each file
    dataLines <- readLines(thisChartName, n = -1L, ok = TRUE, warn = FALSE, encoding = "UTF-8")
    
    # fix invalid multibyte strings
    dataLines <- iconv(dataLines, from = "UTF-8", to = "UTF-8", sub = "")
    
    # head(dataLines, 50)
    
    {
      # locate the beginning of the question text header section
      matchText1 <- "Event    Label Statement"
      questionTextHeaderStartLine <- pmatch(matchText1, str_trim(strtrim(dataLines, nchar(matchText1)), side = "both")) + 1
      
      # locate the end of the question text header section
      matchText2 <- "Event    Label      Begin"
      questionTextHeaderEndLine <- pmatch(matchText2, str_trim(strtrim(dataLines, nchar(matchText2)), side = "both")) - 2
    }
    
    {
      # locate the beginning of the events header section
      matchText3 <- "Event    Label      Begin"
      eventsHeaderStartLine <- pmatch(matchText3, str_trim(strtrim(dataLines, nchar(matchText3)), side = "both")) + 1
      
      # locate the end of the events header section
      matchText4 <- "Sample     Time"
      eventsHeaderEndLine <- pmatch(matchText4, str_trim(strtrim(dataLines, nchar(matchText4)), side = "both")) - 2
    }
    
    # get the number of events
    numberEvents <- as.numeric(strtrim(dataLines[eventsHeaderEndLine], width=2))
    
    # get the replacement text for the text header section
    # this method will remove extra lines that when the question text is line-wrapped
    replaceText <- strtrim(dataLines[eventsHeaderStartLine:eventsHeaderEndLine], width=14)
    
    # use the question ID as the question text
    newText <- str_sub(strtrim(dataLines[eventsHeaderStartLine:eventsHeaderEndLine], width=14), start=10, end=14)
    newText <- str_trim(newText, side="both")
    replaceText <- paste0(replaceText, " ", newText)
    
    # replace the question text
    # dataLines[questionTextHeaderStartLine:questionTextHeaderEndLine] <- replaceText
    # do it this way to avoid problems when text lines are wrapped
    newDataLines <- c(dataLines[1:(questionTextHeaderStartLine-1)], 
                      replaceText,
                      dataLines[(questionTextHeaderEndLine+1):length(dataLines)])
    
    print(paste("writing:", thisChartName))
    
    # write the info to the text file
    # cat(dataLines, file=thisChartName, sep="\n")
    cat(newDataLines, file=thisChartName, sep="\n")
    
    print(i)
    
  }
  
  print(paste("Finished:", "question text removed from", i, "charts"))
  
  removeText <- FALSE
  
}



