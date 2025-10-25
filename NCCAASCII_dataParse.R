# R function to read the NCCA ASCII time series data to a data frame
# August 25, 2025
# Raymond Nelson


# was previously in the NCCAASCII_ParseHelperFunctions.R script
# will be easier to work with and maintain here 





dataParse <- function(x=dataNames, y=thisExamName, saveCSV=FALSE, makeDF=TRUE) {
  # R function to read the NCCA ASCII time series data to a data frame
  # also create csv version of the time series data
  #
  # called by the parseUniqueExams() function in NCCAASCII_Parse.R script
  #
  # x input is a vector of exam names from the output of the dataFile function
  #
  # this function will resample exams that are not Lafayette
  # output is a vector of exam names 
  #
  # this function is not vectorized for multiple exams
  #
  ####
  
  {
    if(!exists("dataNames")) dataNames <- x
    if(!exists("thisExamName")) thisExamName <- y
  }
  
  {
    # intialize some empty objects
    outDF <- NULL # for the output
    chartDF <- NULL # for the working chart in the i loop
  }
  
  #### loop over the vector of chart data files ####
  
  i = 1
  for(i in 1:length(dataNames)) {
    
    {
      # first make a variable to hold the name of the chart data
      currentChartName <- str_sub(dataNames[i], 1, -1)
      
      # may be necessary with Axciton charts
      currentChartName <- gsub("\\$", "\\\\$", currentChartName)
      
      # reset this
      useAlternate <- FALSE
      
      # get the first text row, with all column names
      headerRow <- get(currentChartName)[1]
      # nchar(headerRow)
      
      # check which NCCA ASCII format using the header row
      # the NCCA ASCI spec uses "UPneumo" and LPnuemo"
      # while the NCCA pReview application seems to use "Upneumo" and "Lpneumo"
      if(str_sub(headerRow, 25, 35) != "    UPneumo") {
        useAlternate <- TRUE
      }
    }
    
    #### initialize the chart DF from the NCCA ASCII data ####
    
    #### alterate process for Don Krapohl's 2019 data ####
    
    if(isTRUE(useAlternate)) {
      
      # alternate for Don Krapohl's 2019 data
      
      # set the number of data columns
      # use the first row of the chart data vector
      # and divide by 12 to determine the number of columns 
      # first two columns are Sample and time and are 12 chars each
      # last column is Event and is 6 chars
      dataCols <- nchar(str_sub(get(currentChartName)[1], 25, -7L)) / 12
      
      # cols <- dataCols
      colWidths <- rep(12, dataCols)
      
      # read the column names from the data vector
      cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                              widths = c(12, 12, colWidths, 6), 
                                              header = FALSE, 
                                              skip = 0,
                                              n = 1))),
                         side = "both" )
      
      # read the data from the data vector for this chart
      # skip the first (column labels) row
      chartDF <- read.fwf(textConnection(get(currentChartName), open = "r"), 
                          widths = c(12, 12, colWidths, 6), 
                          header = FALSE, 
                          skip = 1,
                          col.names = cNames,
                          n = -1L,
                          stringsAsFactors=FALSE ) 
      
      # chartDF$Label <- as.character(chartDF$Label)
      
      # re-order the colums to the normal
      chartDF <- chartDF[,c(1,2, ncol(chartDF), 3:(ncol(chartDF)-1))]
      
      # reset the column names
      names(chartDF) <- c("Sample", "Time", "Label", "UPneumo", "LPneumo", "EDA1", "Cardio1")
      
      # fix the answers and Labels
      chartDF$Label <- str_trim(chartDF$Label, side="both")
      chartDF$Label[which(chartDF$Label == ".")] <- ""
      chartDF$Label[which(chartDF$Label == "Ans")] <- "No"
      # all answers are No at this point, regardless of the actual answer"
      # chartDF$Label[which(chartDF$Label != "")]
      chartDF$Label <- toupper(chartDF$Label)
      
      # 2025 June 19 # fix the question labels in the chartDF
      chartDF$Label <- fixTagsFn(x=chartDF$Label)
      
      chartDF$Time <- str_trim(chartDF$Time, side="both")
      chartDF$Time <- str_sub(paste0("0", chartDF$Time), -8, -1)
      
    } # end if isTRUE for useAlternate
    
    #### normal process - for normal NCCA ASCII data ####
    
    if(!isTRUE(useAlternate)) {
      
      # for standard NCCA ASCII output
      
      # set the number of data columns
      # use the first row of the chart data vector
      # and divide by 11 to determine the number of columns 
      dataCols <- nchar(str_sub(get(currentChartName)[1], 25, -1L)) / 11
      
      # cols <- dataCols
      colWidths <- rep(11, dataCols)
      
      # read the column names from the data vector
      cNames <- str_trim(as.vector(t(read.fwf(textConnection(get(currentChartName), open = "r"), 
                                              widths = c(6, 9, 9, colWidths), 
                                              header = FALSE, 
                                              skip = 0,
                                              n = 1))),
                         side = "both" )
      
      # read the data from the data vector for this chart
      # skip the first (column labels) row
      chartDF <- read.fwf(textConnection(get(currentChartName), open = "r"), 
                          widths = c(6, 9, 9, colWidths), 
                          header = FALSE, 
                          skip = 1,
                          col.names = cNames,
                          n = -1L,
                          stringsAsFactors=FALSE ) 
      
      chartDF <- chartDF[which(!is.na(chartDF$Sample)),]
      
      # chartDF$Label <- as.character(chartDF$Label)
      
      # 2025 June 19 # fix the question labels in the chartDF
      chartDF$Label <- fixTagsFn(x=chartDF$Label)
      # unique(chartDF$Label)
      
    } # end if is FALSE  useAlternate
    
    # we now have a data frame for the chart #
    
    #### always include the Move1 column (required sensor) if it is missing ####
    
    if(!("Move1" %in% names(chartDF))) {
      
      # June 10, 2025 
      # always include a Move1 column (required sensor) even when it is missing
      
      chartDF$Move1 <- "-9.9"
      
      # dataCols <- dataCols + 1
      dataCols <- ncol(chartDF) - 3
      # include the width for the added activity sensor column
      colWidths <- c(colWidths, colWidths[length(colWidths)])
      cNames <- c(cNames, "Move1")      
      cNames <- unique(cNames)
      
      # assign("chartDF", chartDF, pos=1)
      
    }
    
    #### always include the PPG1 column (not a required sensor) if it is missing ####
    
    if(!("PPG1" %in% names(chartDF)) && isTRUE(includePLEData)) {
      
      # June 10, 2025
      # always include a PPG1 column (not a required sensor) even when it is missing
      
      chartDF$PPG1 <- "-9.9"
      
      # dataCols <- dataCols + 1
      dataCols <- ncol(chartDF) - 3
      # include the width for the added activity sensor column
      colWidths <- c(colWidths, colWidths[length(colWidths)])
      cNames <- c(cNames, "PPG1")
      cNames <- unique(cNames)
      
      # assign("chartDF", chartDF, pos=1)
      
    }
    
    #### check some conditions ####
    
    {
      # increment the i loop if there is no data
      if(length(which(!is.na(chartDF$Sample))) == 0) next()
      
      # increment the i loop if the chart is a short or aborted fragment
      if(length(which(!is.na(chartDF$Sample))) <= 300) next()
      
      # make the labels all upper case and remove the "-" character
      chartLabels <- toupper(str_replace_all(chartDF$Label, "[ -]", ""))
      
      # Feb 3, 2023
      # increment the i loop for any chart that is missing the X or XX announcement
      if(stopXXX) {
        if(any(!(c("X", "XX") %in% chartLabels))) {
          print("missing X and/or XX")
          # stop()
          next()
        }
      }
    }
    
    #### add some columns ####
    
    {
      # add a column for the chart name
      chartDF <- cbind(rep(str_sub(currentChartName, -8, -6), nrow(chartDF)), chartDF)
      # add a column for the series name
      chartDF <- cbind(rep(str_sub(currentChartName, -9, -9), nrow(chartDF)), chartDF)
      # add a column for the exam name
      chartDF <- cbind(rep(str_sub(currentChartName, 1, -10), nrow(chartDF)), chartDF)
      
      names(chartDF)[1:3] <- c("examName", "seriesName", "chartName")
      
      # assign("outDF", outDF, pos=1)
      # assign("chartDF", chartDF, pos=1)
      # View(chartDF)
    }
    
    #### check for columns that differ for the test charts ####
    
    {
      missingCols <- names(outDF)[which(!(names(outDF) %in% names(chartDF)))]
      if(length(missingCols)>0) {for(j in 1:length(missingCols)) {chartDF[missingCols[j]] <- numeric()}}
      
      missingCols <- names(chartDF)[which(!(names(chartDF) %in% names(outDF)))]
      if(length(missingCols)>0) {for(j in 1:length(missingCols)) {outDF[missingCols[j]] <- numeric()}}
    }
    
    #### resample the data to 30 cps if necessary ####
    
    {
      # source("~/Dropbox/dataReduce.R", echo=TRUE)
      
      # check if non Lafayette chart
      # if(!grepl("D&", thisExamName[1])) {
      #   # set reSample to TRUE for non-lafayette charts
      #   reSample <- TRUE
      # } else {
      #   # reduceSampleRate is a setting in the NCCAASCII_init.R script
      #   reSample <- reduceSampleRate
      # }
      
      reSample <- reduceSampleRate
      
      # call the dataReduceFn() from this script
      if(reSample==TRUE) {
        # cps is a parameter from the NCCAASCII_init.R script
        # dataReduceFn is a function in the dataReduce.R script
        
        # source(paste0(RPath, 'dataReduce.R'), echo=FALSE)
        chartDF <- dataReduceFn(chartDF=chartDF, newRate=cps)
        # View(chartDF)
      } 
      
      # at this point the chartDF exists at 30cps
    }
    
    #### fix NA row ####
    
    {
      
      # 2025Aug12 NA rows can occur with LX Edge charts b
      # ecause of differences in the time scale
      
      # NARows <- which(is.na(chartDF$Label))
      
      # if(length(NARows) > 0) {
      #   chartDF <- chartDF[-NARows,]
      # }
      
      # 2025Aug16 replace NA rows with the preceeding value
      
      # <>
      
      # number of data columns was computed earlier
      dataColNumbers <- c(7:(dataCols+6))
      # 8 
      
      # iterate over the data columns 
      # look for -9.9 values and replace these with the preceeding value 
      
      j=1
      for(j in 1:length(dataColNumbers)) {
        
        thisColNumber <- dataColNumbers[j]
        
        thisColDAT <- chartDF[,thisColNumber] 
        
        errCount <- 0
        
        k=1
        for(k in 2:length(thisColDAT)) {
          
          if(thisColDAT[k] == -9.9 || thisColDAT[k] == "NA") {
            errCount <- errCount + 1
            thisColDAT[k] <- thisColDAT[(k-1)]
          }
          
        }
        
        if(errCount > 0) chartDF[,thisColNumber] <- thisColDAT
        
      }
      
      
    }
    
    #### Sep 23, 2025 use a Tukey fence to replace outlier values ####
    
    {
      
      # number of data columns was computed earlier
      dataColNumbers <- c(7:(dataCols+6))
      
      # iterate over the data columns 
      
      # compute the 25th and 75th percentiles and the upper and lower inner fence
      # replace outliers with the median value
      
      j=1
      for(j in 1:length(dataColNumbers)) {
        
        thisColNumber <- dataColNumbers[j]
        
        # View(chartDF)
        
        thisColDAT <- as.numeric(chartDF[,thisColNumber])
        
        # next column if there is no data (all values are -9.9)
        if(sd(thisColDAT) == 0) next()
        
        p25 <- quantile(thisColDAT, .25)
        p50 <- quantile(thisColDAT, .5)
        p75 <- quantile(thisColDAT, .75)
        IQRange <- p75 - p25
        
        # tukey fences
        # 2025Oct25 inner fences cause a lot o of problems
        inUp <- p75 + (1.5 * IQRange)
        inLw <- p25 - (1.5 * IQRange)
        
        # 2025Oct25 changed to outer fence to avoid inducing EDA artifacts
        outUp <- p75 + (9 * IQRange)
        outLw <- p25 - (9 * IQRange)
        
        errCount <- 0
        
        k=1
        for(k in 1:length(thisColDAT)) {
          
          if(thisColDAT[k] >= outUp) {
            # upper fence
            errCount <- errCount + 1
            thisColDAT[k] <- p75
          }
          
          if(thisColDAT[k] <= outLw) {
            # lower fence
            errCount <- errCount + 1
            thisColDAT[k] <- p25
          }
          
        }
        
        if(errCount > 0) chartDF[,thisColNumber] <- thisColDAT
        
      } # end j loop over data columns
      
    }
    
    #### check for overlapping events or overlapping annotations ####
    
    {
      
      thisExamName
      
    }
    
    #### rbind the DFs for all charts ####
    
    outDF <- rbind(outDF, chartDF)
    
    # View(chartDF)
    # View(outDF)
    
  } # end loop over i chart data files
  
  ####  fix some potential problems   ####
  
  {
    
    outDF$Label <- as.character(outDF$Label)
    outDF$Label <- toupper(str_replace_all(outDF$Label, "[- ]", ""))
    # outDF$Label <- toupper(outDF$Label)
    
    # outDF$Move1 <- as.numeric(outDF$Move1)
    
    # fix some potential problems with Limestone question labels
    # outDF$Label <- gsub("S S", "S", outDF$Label)
    # outDF$Label <- gsub("SS", "S", outDF$Label)
    # outDF$Label <- gsub("SY SY", "SY", outDF$Label)
    # outDF$Label <- gsub("R R", "R", outDF$Label)
    # outDF$Label <- gsub("RR", "R", outDF$Label)
    # outDF$Label <- gsub("C C", "C", outDF$Label)
    # outDF$Label <- gsub("CC", "C", outDF$Label)
    # outDF$Label <- gsub("SR SR", "S", outDF$Label)
    # outDF$Label <- gsub("RS RS", "S", outDF$Label)
    # outDF$Label <- gsub("SRSR", "S", outDF$Label)
    # outDF$Label <- gsub("RSRS", "S", outDF$Label)
    # outDF$Label <- gsub("N N", "N", outDF$Label)
    # outDF$Label <- gsub("I I", "I", outDF$Label)
    # outDF$Label <- gsub("II", "I", outDF$Label)
    # outDF$Label <- gsub("NN", "N", outDF$Label)
    
    # remove period characters 
    # for examiners who are silly enough to use them in question tags
    outDF$Label <- gsub("\\.", "", outDF$Label)
    
  }
  
  #### add columns for eventLabel, Events, stimText, and Answer ####
  
  {
    
    eventLabel <- "" # used to hold unique Labels when events are repeaated
    Events <- ""# 
    stimText <- "" # stimulus text statement"
    Answer <- "" # 
    
    outDF <- cbind.data.frame(outDF[,(1:6)],
                              eventLabel,
                              Events,
                              stimText,
                              Answer,
                              outDF[,(7:ncol(outDF))] )
    
    outDF$eventLabel <- as.character(outDF$eventLabel)
    outDF$Events <- as.character(outDF$Events)
    outDF$stimText <- as.character(outDF$stimText)
    outDF$Answer <- as.character(outDF$Answer)
    
  }
  
  #### add the centered data columns ####
  
  {
    
    # make a vector of names for some new columns for the centered data
    newColNames <- paste0("c_", names(outDF[11:ncol(outDF)]))
    
    # add the names to the output data frame
    for (m in 1:length(newColNames)) {
      outDF <- cbind(outDF, rep(0, times=nrow(outDF)))
      # name each new column
      names(outDF)[ncol(outDF)] <- newColNames[m]
    } 
    
  }
  
  ### call a function to add columns for signal processing ###
  # source(paste0(RPath, 'addColumns.R'), echo=FALSE)
  outDF <- addColumnsFn(x=outDF)
  
  #### construct the output ####
  
  {
    
    # create the output file name
    outName <- paste(str_sub(currentChartName, 1, -10), "_Data", sep = "")
    
    # save the data as a CSV
    if (saveCSV==TRUE) write.csv(outDF, 
                                 file=paste(outName, ".csv", sep = ""), 
                                 row.names = FALSE)
    
    # save the data as a data frame 
    if(makeDF==TRUE) assign(x=outName, value=outDF, pos = 1)
    
    
  }
  
  # visible output is the name of the last input exam
  return(str_sub(outName, 1, -1)) 
  
} # end dataParse() function




# dataParse(x=dataNames, makeDF=TRUE, saveCSV=TRUE) # called by the parseUniqueExams function in the NCCAASCIIParse.r script







