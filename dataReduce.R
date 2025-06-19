dataReduceFn <- function(chartDF=chartDF, newRate=30) {
  # function to reduce the sampling rate to 30 cps if necessary
  # 9-29-2016 raymond.nelson@gmail.com
  # input x is a data frame with the time series data for a chart
  # output is a data frame with the data set to 30 cps sampling rate
  #
  # called by the dataParse() function 
  # in the NCCAASCIIParseHelperFunctions.R script
  ####
  
  {
    if(!exists("newRate")) newRate <- cps
    
    saveTime <- as.character(chartDF$Time)
    
    # reset
    # chartDF$Time <- as.character(as.character(saveTime))
    
    # change the time column to character and remove trailing spaces
    chartDF$Time <- str_replace_all(as.character(chartDF$Time), " ", "")
  } 
  
  {
    # check for events on only 1 row, in case they get lost
    LabelVector <- chartDF$Label[2:(nrow(chartDF)-1)]
    # preceding row
    LabelVectorA <- chartDF$Label[1:(nrow(chartDF)-2)]
    # subsequent row
    LabelVectorB <- chartDF$Label[3:nrow(chartDF)]
    # keep only the labels that are not equal to the preceeding or subsequent label
    theseEvents <- which(LabelVector != LabelVectorA & LabelVector != LabelVectorB) + 1
    
    # these events occur on only one row
    theseLabels <- chartDF$Label[theseEvents]

    # check for events on subsampled (-9.9) rows and adjust them
    
    # get the subsampled missing rows
    # missingRows <- which(chartDF$UPneumo == -9.9)
    # Feb 3, 2023 using EDA1 instead because LXCAT has no pneumo data
    missingRows <- which(chartDF$EDA1 == -9.9)
    # May 12, 2024 to work with PPT rows that have missing values
    if(!PCATFormat) {
      missingRows <- c(missingRows, which(chartDF$UPneumo == -9.9))
      missingRows <- c(missingRows, which(chartDF$LPneumo == -9.9))
      missingRows <- c(missingRows, which(chartDF$PPG1 == -9.9))
      missingRows <- c(missingRows, which(chartDF$Move1 == -9.9))
      missingRows <- c(missingRows, which(chartDF$PTTPTT == -9.9))
      missingRows <- sort(unique(missingRows))
    }
    
    # check for single row events on subsampled rows
    adjustThese <- which(theseEvents %in% missingRows)
    # theseEvents[adjustThese]
    
    # a vector of rows for which the sensor value is not -9.9
    # -9.9 indicates an unused required sensor (older exams, before 2007, may not include Move1)
    # -9.9 also indicates a subsampled sensor (cardio has a higher sampling rate for some instruments)
    # keepTheseRows <- which(chartDF$UPneumo != -9.9)
    # Feb 3, 2023 using EDA1 instead because LXCAT has no pneumo data
    # keepTheseRows <- which(chartDF$EDA1 != -9.9)
    # May 12, 2024 to work with PPT rows that have missing values
    keepTheseRows <- which(!(c(1:nrow(chartDF) %in% missingRows)))
    
    useTheseSamples <- chartDF$Sample[keepTheseRows]
    
    # use a loop 
    if(length(adjustThese) > 0 && length(useTheseSamples) > 0) {
      i=1
      for(i in 1:length(adjustThese)) {
        # select the min sample larger than each i
        newRow <- 
          useTheseSamples[min(which(keepTheseRows > theseEvents[adjustThese[i]]))]
        # add the event to the next extant data row
        chartDF$Label[newRow] <- theseLabels[adjustThese[i]]
      }
    }
    
    # rows are not yet adjusted in the event table
    
    # then reduce the data frame to the column with the slowest sampling rate
    if(length(useTheseSamples) > 0) {
      # chartDF <- chartDF[which(chartDF[,'UPneumo'] != -9.9),]
      # Feb 3, 2023
      chartDF <- chartDF[useTheseSamples,]
    }
    
    # the chart DF is now decimated of -9.9 rows and single row events are retained
  }
  
  # which(chartDF$Label %in% unique(theseLabels))
  
  # # feb 21, 2025 to fix problems when decimating and trimming before the X and after XX
  # chartDF <- chartDF[which(chartDF$examName==chartDF$examName[1]),]
  # 
  # # length(which(chartDF$examName!=chartDF$examName[1]))
  # # chartDF <- chartDF_SAVE
  
  #### call the fromMinSecFn() from the  toMinSec.R script ####
  
  {
    # get the time scale in seconds 
    chartTimeScale <- fromMinSecFn(x=chartDF$Time)
    # get the length of the chart in seconds
    chartTime <- chartTimeScale[length(chartTimeScale)]
    # head(chartTimeScale)
    # tail(chartTimeScale, 50)
    # tail(chartDF$Time, 1000)
    # toMinSecFn(chartTime)
    
    # stop if short chart
    if(chartTime < 4.0) stop("short chart cannot be reduced")
  }
  
  ####  calculate the data rate and new time scale  ####
  
  {
    dataLength <- length(chartTimeScale)
    
    # for decimation
    dataRate <- round(1 / (chartTime / dataLength))
    
    ## new time vector and new data rate ##
    
    # make a new time vector at the new data rate
    secondsVc <- round(seq(from=0.0, to=chartTime+1, by = .03333333), 2)
    # head(secondsVc)
    # tail(secondsVc)
    
    dataRate <- round(1 / (chartTime / length(secondsVc)))
    
    newDataLength  <- length(secondsVc)
    # max(secondsVc)
    
    newDataRate <- round(1 / (chartTime / newDataLength))
    
    #  make a new time scale at the new data rate
    #  call the toMinSecFn() from the  toMinSec.R script
    newTimeScale <- toMinSecFn(secondsVc)
  }
  
  #### preserve verbal answers and annotations ####
  
  {
    # also preserve annotation events that occur on a single sample
    # such as some X and XX announcements and annotations
    
    # Feb 3, 2023
    oldLabels <- chartDF$Label
    
    LabelVector <- chartDF$Label[2:(nrow(chartDF)-1)]
    # preceding row
    LabelVectorA <- chartDF$Label[1:(nrow(chartDF)-2)]
    # subsequent row
    LabelVectorB <- chartDF$Label[3:nrow(chartDF)]
    # keep only the labels that are not equal to the preceding or subsequent label
    saveTheseIndices <- which(LabelVector != LabelVectorA & LabelVector != LabelVectorB) + 1
    
    # which(LabelVector != LabelVectorA)
    # which(LabelVector != LabelVectorB)
    # chartDF$Label[saveTheseIndices]
    
    # chartDF$Label[(saveTheseIndices-1)]
    
    # save the label
    saveEventTxt <- chartDF$Label[saveTheseIndices]
    # May include c("YES", "Yes", "yes", "NO", "No", "no", "ANS", "Ans", "ans")
    # may also include annotations unless the next line
    
    # remove "-" and " " characters
    saveEventTxt2 <- str_replace_all(saveEventTxt, "[ -]", "")
    
    # keep on the answers, not the annotations
    # saveEventTxt <- saveEventTxt[which(saveEventTxt2 %in% c("YES", "Yes", "yes", "NO", "No", "no", "ANS", "Ans", "ans"))]
    
    # make a vector of seconds at the old data rate
    # oldSecondsVc <- 
    #   round(seq(from=0.0, to=chartTime+.01, by = 1/dataRate), 2)
    
    # get the old seconds vector using the fromMinSecFn()
    oldSecondsVc <- fromMinSecFn(chartDF$Time)
    
    # get the time of the answers and annotations from the old time scale
    saveEventTimes <- oldSecondsVc[saveTheseIndices]
    
    # initialize a vector of new sampling rows 
    # of the same length as saveEventTimes
    newSampleIndices <- rep(NA, length=length(saveEventTimes))
    # and iterate
    if(length(saveEventTimes) > 0 ) {
      # s=1
      for(s in 1:length(newSampleIndices)) {
        newSampleIndices[s] <- 
          # secondsVc is the new time scale that was created earlier
          min(which(secondsVc >= saveEventTimes[s]))
          # use the old secondsVc time scale
          # min(which(oldSecondsVc >= saveEventTimes[s]))
          # secondsVc[newSampleIndices[s]]
          # oldSecondsVc[saveTheseIndices[s]]
          # oldSecondsVc[newSampleIndices[s]]
      }
    }
    # newSampleIndices is the new location for answers and annotations
    # secondsVc[newSampleIndices]
  }
  
  #### select decimation or interpolation ####
  
  resampleType <- ifelse(dataRate == newDataRate, 
                         "NONE",
                         ifelse(dataLength < newDataLength,
                                "INTERPOLATE",
                                "DECIMATE" ))
  
  #### INTERPOLATION ####
  
  if(resampleType == "INTERPOLATE") {
    
    # initialize a new data frame
    {
      newChartDF <- as.data.frame(matrix(nrow=newDataLength, ncol=ncol(chartDF)))
      names(newChartDF) <- names(chartDF)
      newChartDF$examName <- chartDF$examName[1]
      newChartDF$seriesName <- chartDF$seriesName[1]
      newChartDF$chartName <- chartDF$chartName[1]
      newChartDF$Sample <- c(1:newDataLength)
      newChartDF$Time <- toMinSecFn(secondsVc)
      # View(newChartDF)
    }
    
    # set the first and last row
    {
      newChartDF[1,] <- chartDF[1,]
      newChartDF[nrow(chartDF),] <- chartDF[nrow(chartDF),]
      newChartDF[nrow(newChartDF),] <- chartDF[nrow(chartDF),]

    }
    
    # clear answer events or annotations that occur on a single row
    {
      chartDF2 <- chartDF
      chartDF2$Label[saveTheseIndices] <- chartDF2$Label[(saveTheseIndices-1)]
    }
    
    # iterate over old data 
    i=1
    # i=newDataLength
    for(i in 1:nrow(chartDF2)) {
      # locate the new time that corresponds to the old time
      # assign each old row to a new row
      # at the max of <= time 
      newChartDF[max(which(secondsVc <= chartTimeScale[i])),] <- chartDF2[i,]
    }
    # View(newChartDF)
    
    # reset the sample numbers because they have been disrupted
    newChartDF$Sample <- c(1:newDataLength)
    
    # reset the time scale
    newChartDF$Time <- toMinSecFn(secondsVc)
    
    # fix the labels
    {
      newChartDF$Label <- toupper(newChartDF$Label)
      newChartDF$Label <- str_replace_all(newChartDF$Label, " ", "")
      newChartDF$Label <- str_replace_all(newChartDF$Label, "-", "")
    }
    
    # fix the single line events
    {
      newChartDF$Label[newSampleIndices] <- saveEventTxt
    }
    
    # fix the length of the new data frame
    {
      oldTime <- fromMinSecFn(chartDF$Time[nrow(chartDF)])
      newTime <- fromMinSecFn(newChartDF$Time)
      
      # which(newTime >= oldTime)
    
      if(nrow(newChartDF) > which(newTime >= oldTime)[1]) {
        newChartDF <- newChartDF[1:which(newTime >= oldTime)[1],]
      }
    }
    
    # make sure that data columns are numeric
    {
      j=7
      for(j in 7:ncol(newChartDF)) {
        newChartDF[,j] <- as.numeric(newChartDF[,j])
      }
      # which(is.na(newChartDF[,11]))
    }
    
    # now iterate over the new data frame columns to interpolate
    j=7
    for(j in 7:ncol(newChartDF)) {
      thisCol <- newChartDF[,j]
      # locate NA values and set them to the preceding value
      # iterate backward
      k=length(thisCol)-1
      for(k in (length(thisCol)-1):2) {
        thisValue <- thisCol[k]
        
        if(is.na(thisValue)) {
          # locate the subsequent value
          nextValue <- thisCol[(k+1)]
          # then locate the preceding value that is not NA
          precedingValue <- NA
          l <- k
          while(is.na(precedingValue)) {
            l <- l-1
            precedingValue <- thisCol[l]
          }
          # get the increment
          thisDistance <- k - l
          diffValue <- nextValue - precedingValue
          thisIncrement <- diffValue / (thisDistance + 1)
          # calculate the missing values
          missingValues <- 
            thisCol[l] + cumsum(rep(thisIncrement, length=thisDistance))
          # set the NA values
          thisCol[(l+1):((l+1)+(thisDistance-1))] <- missingValues
          # set k to l to reduce the interation 
          # k <- l      
        } # end if
        
      } # end k loop over the values in each column
      
      newChartDF[,j] <- thisCol
      
    } # end j loop over the columns
    
    ## iterate  over the labels to fill missing values
    labelsCol <- newChartDF$Label
    m=1557
    for(m in 2:(length(labelsCol)-1)) {
      if(is.na(labelsCol[m]) && 
         !(labelsCol[m-1] %in% c("YES", "NO", "ANS"))) {
        labelsCol[m] <- labelsCol[m-1]
      } else if(is.na(labelsCol[m])) {
        labelsCol[m] <- ""
      }
    }
    
    # submit the new labelsCol to the new  chart  data frame
    newChartDF$Label <- labelsCol
    
    # fix the time scale
    newChartDF$Time <- newTimeScale[1:nrow(newChartDF)]
    
  } # end interpolate
  
  #### DECIMATION ####
  
  if(resampleType == "DECIMATE") {
    
    # compute the data decimation rate for 10 seconds and divide by 30 cps
    # dataDecVal <- ( (which(chartDF$Time=="00:10.00")[1]-1) / 10 ) / cps
    # this will work better with inconsistent sampling times
    dataDecVal <- dataRate / cps
    # dataDecVal is the sample retention interval 
    
    # dataDecVal cannot be less than 1 for decimation
    if(dataDecVal < 1) dataDecVal <- 1
    
    # use the dataDecVal to identify rows to keep 
    keepRows <- round(1:(nrow(chartDF) / dataDecVal) * dataDecVal, 0)
    
    # clear answer events or annotations on a single row
    {
      chartDF2 <- chartDF
      chartDF2$Label[saveTheseIndices] <- chartDF2$Label[(saveTheseIndices-1)]
    }
    
    # and finally decimate the data frame 
    newChartDF <- chartDF2[keepRows,]
    # View(newChartDF)
    
    # check the length of the new time scale and new data frame
    if(length(newTimeScale) > nrow(newChartDF)) {
      newTimeScale <- newTimeScale[1:nrow(newChartDF)]
    }
    if(nrow(newChartDF) > length(newTimeScale)) {
      newChartDF <- newChartDF[(1:length(newTimeScale)),]
    }
    
    
    # replace the time old time scale with the new time scale
    newChartDF$Time <- newTimeScale[1:nrow(newChartDF)]
    # str(newTimeScale)
    # str(chartTimeScale)
    # str(secondsVc)
    
    # fix the single line events (answers and annotations)
    {
      newChartDF$Label[newSampleIndices] <- saveEventTxt
    }
    
    
    # # salvage the answers after decimation
    # # use the chartDF to do this
    # # chartDF$Label <- toupper(chartDF$Label)
    # answerLabels <- toupper(str_replace_all(chartDF$Label, "[ -]", ""))
    # answerRows <- 
    #   which(answerLabels %in% c("YES", "Yes", "yes", "NO", "No", "no", "ANS", "Ans", "ans"))
    # 
    # if(length(answerRows) > 0) {
    #   newAnswerRows <- rep(NA, length(answerRows))
    #   # use a loop to set the answer row using the 
    #   i=1
    #   for (i in 1:length(answerRows)) {
    #     newAnswerRows[i] <- 
    #       max(which(secondsVc <= chartTimeScale[answerRows[i]]))
    #   }
    #   newChartDF$Label[newAnswerRows] <- chartDF$Label[answerRows]
    # }
    
  } # end decimate
  
  #### NONE ####
  
  if(resampleType == "NONE") {
    newChartDF <- chartDF
  }
  
  #### fix  the answers and annotations ####
  
  if(resampleType != "NONE") {
    
    # these are the events and locations in the chartDF at this point
    # saveEventTimes
    # saveEventTxt # taken from the chart data frame
    # saveEventTxt2 # stripped " " and "-" characters
    
    # in the newChartDF
    # newSampleIndices # the new location for answers and annotations
    newEventTimes <- newChartDF$Time[newSampleIndices]
    
    # this is what remains after decimation
    newEventTxt <- newChartDF$Label[(newSampleIndices)]
    newEventTxt2 <- str_replace_all(newEventTxt, "[ -]", "")
    
    # sometimes the answer can shift around and can use these to check  
    # newChartDF$Label[(newSampleIndices - 1)]
    # newChartDF$Label[(newSampleIndices - 2)]
    # newChartDF$Label[(newSampleIndices + 1)]
    # newChartDF$Label[(newSampleIndices + 2)]
    # newChartDF$Label[(newSampleIndices + 3)]
    
    # look for and clear the extant answers and annotations before replacing them
    {
      newSampleIndices <- c(newSampleIndices, newSampleIndices + c(1:3))
      
      # use the outer join function to select rows after the selected sample indices
      postRows <- sort(unique((outer(newSampleIndices, c(1:3), "+"))))
      # use the outer join to function so 
      preRows <- sort(unique((outer(newSampleIndices, c(1:3), "-"))))
      
      clearThese <- sort(unique(c(newSampleIndices, postRows, preRows)))
      clearThese <- clearThese[clearThese > 0]
      clearThese <- clearThese[clearThese <= nrow(newChartDF)]
             
      # newChartDF$Label[(newSampleIndices)] <- ""
      # newChartDF$Label[(newSampleIndices - 1)] <- ""
      # newChartDF$Label[(newSampleIndices - 2)] <- ""
      # newChartDF$Label[(newSampleIndices - 3)] <- ""
      # newChartDF$Label[(newSampleIndices + 1)] <- ""
      # newChartDF$Label[(newSampleIndices + 2)] <- ""
      # newChartDF$Label[(newSampleIndices + 3)] <- ""
      
      newChartDF$Label[(clearThese)] <- ""
    }
    
    # replace them all at once 
    newChartDF$Label[newSampleIndices] <- saveEventTxt
    
    # fix the sample indices 
    newChart$DF$Sample <- c(1:nrow(newChartDF))
    
  } # end resample type != "NONE"
  
  #### output the result ####
  
  # View(newChartDF)
  
  return(newChartDF)
} # end dataReduceFn()



