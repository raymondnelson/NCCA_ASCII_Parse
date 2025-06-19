# R function to to iterate over exams, series and charts
# 9/30/2021
# Raymond Nelson
####


# library(stringr)


# getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
# uniqueExams <- getUniqueExams(x="*_Data$")
# uniqueExams <- uniqueExams[2]


####




examSeriesChartsFn <- function(x=uniqueExams, 
                              makeDF=TRUE, 
                              saveCSV=FALSE,
                              showNames=TRUE, 
                              output=FALSE) {
  # R function to to iterate over exams, series and charts
  # 9/30/2021
  # Raymond Nelson
  ####
  #
  # unique exams is a vector of exams in the global envir
  #
  # makeDF=TRUE will save the examDF to the global environment
  # using examName, "_Data"
  # output=TRUE will output the time series data frame for the last exam in the input vector of exam names
  #
  ####
  
  # set.seed(1234567890)
  
  uniqueExams <- x
  
  #### iterate On the exam names in the input vector ####
  
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      searchString <- paste0("*", examName, "_Data", "*")
      
      dataName <- glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE)
      
      examDF <- get(dataName, pos=1)
      
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      # for testing
      # assign("examDF", examDF, pos=1)
      # assign("examName", examName, pos=1)
      
      if(showNames==TRUE) print(paste("exam", i, examName))
      
      # get the names of each unique series in the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
    }
    
    #### iterate on the the series within each exam  ####
    
    j=1
    for(j in 1:length(uniqueSeries)) {
      
      {
        
        seriesName <- uniqueSeries[j]
        
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
        seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
        
        # for testing
        # assign("seriesDF", seriesDF, pos=1)
        # assign("seriesName", seriesName, pos=1)
        
        if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
        
        uniqueCharts <- as.character(unique(seriesDF$chartName))
        
      }
      
      #### iterate on the charts in each series ####
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          chartName <- uniqueCharts[k]
          
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          
          # skip charts less than 15 seconds
          if(nrow(chartDF)<900) next()
          
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          # for testing
          # assign("chartDF", chartDF, pos=1)
          # assign("chartName", chartName, pos=1)
          
          if(showNames==TRUE) print(uniqueCharts[k])
          
          uniqueEvents <- unique(chartDF$eventLabel)
          uniqueEvents <- uniqueEvents[uniqueEvents != ""]
          
        }
        
        ####   get the first and last events   ####
        
        # call the eventNamesFn from the sigProcHelper.R script
        # firstLastEvents <- eventNamesFn(x=chartDF)
        
        {
          
          firstLastEvents <- getFirstLastEventFn(x=chartDF)
          firstEvent <- firstLastEvents[1]
          lastEventEnd <- firstLastEvents[2] # - 450 
          
          # 2-6-2021 stop at the onset of the last event
          # because some examiners blow the cuff before they stop recording
          # assign("firstLastEvents", firstLastEvents, pos=1)
          
        }
        
        ############### call a function here #################
        
        {
        
          # DF9J0D 
          
          # DCIVSL
          
          uniqueEvents
          
          which(chartDF$eventLabel != "")
          
          View(chartDF)
          
          which(chartDF$Cardio1 == 1750)
          
          chartDF$Cardio1[1130:1178]
          
          chartDF$Cardio1[1200:(1200+48)] 
          
          chartDF$Cardio1[c(1200, (1200+48))]
          # [1] 1882 1869
          
          mean(chartDF$Cardio1[c(1129,1179)])
          # [1] 1839 1873
          # 1859
          
          mean(chartDF$Cardio1[c(1200, (1200+48))] - chartDF$Cardio1[c(1129,1179)])
          # 20
          
          chartDF$Cardio1[1130:1178] <- (chartDF$Cardio1[c(1200:(1200+48))] - 20)
          
          
          chartDF$PPG1[1130:1178]
          
          1178-1130
          48
          
          1130+48
          
          chartDF$PPG1[1130:1178] <- chartDF$PPG1[1200:(1200+48)]
          
        }
        
        
        ######################
        
        # save the chartDF to the seriesDF
        # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        # save directly to the examDF instead
        examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF
        
      } # end for loop over each k chart in each series
      
      # save the seriesDF to the examDF
      # not needed because the chartDF is saved directly to the examDF in the inner loop
      # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over j unique series
    
    # save the examDF to the global environment
    if(makeDF==TRUE) { assign(paste0(examName, "_Data"), examDF, pos=1) }
    
    # write a .csv to the current directory
    if(saveCSV==TRUE) {
      outputDFName <- paste0(examName, "_Data", sep="_")
      write.csv(examDF,
                file=paste0(examName, "_Data", ".csv"),
                row.names=FALSE)
    }
    
    assign(dataName, examDF, envir=.GlobalEnv)
    
  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) { return(examDF) } else {
    return(uniqueExams)
  }
  
} # end examSeriesChartsFn()


# call the function
# examSeriesChartsFn(x=uniqueExams, output=FALSE, showNames=TRUE)



