# RC Parser to aggregate RQ and CQ measurements to a score for each RQ
# 10-23-2015
# Raymond Nelson
#


# RCParseFn 
# function to iterate over a vector of data frame names for all exams in the cwd
# and create a data frame of scores for each exam


# scoresDataSet
# function to combine all score data frames in the cwd to to a single data set


# combinePneumos
# function to combine the upper and lower pneumo sores to a single score




################################



library(stringr)



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Measurements$", pos=1),1, -14))
# uniqueExams <- uniqueExams[1]



# cps <- 30
# prestimSeg <- 5
# EDALat <- .5
# CardioLat <- .5
# ROWEnd <- 5
# measuredSeg <- 15
# addSeg <- 5



##############

x=uniqueExams
showNames=TRUE
output=FALSE



# get a segment instead of iterating through all exams 
getChart <- FALSE
examNum <- 1
seriesNum <- 1
chartNum <- 2
# use sensorNumNA 
sensorNum <- 4



# function to iterate over a vector of data frame names for all exams in the cwd
# and create a data frame of scores for each exam


RCParseFn<- function(x=uniqueExams, showNames=TRUE, output=FALSE, getChart=getChart, RCMethod="LEFT") {
  # function to iterate over a vector of data frame names for all exams in the cwd
  # and create a data frame of scores for each exam
  # 
  # x is a vector of names of data frames that contain the
  # time series data fro all charts for each exam
  # 
  # showNames=TRUE will print the exam, series and chart names to the console
  # output=TRUE will return a data frame for the last input exam
  # 
  ##########################
  
  uniqueExams <- x
  
  # print(getChart)
  
  if(getChart == TRUE) {
    assign("examName", uniqueExams[examNum])
    uniqueExams <- uniqueExams[examNum] 
  } 
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Measurements", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    # examDF <- get(examName)
    
    if(getChart==TRUE) {
      assign("examDF", examDF, pos=1)
      # break
    }
    
    # examName <- str_sub(examName, 1, -14)
    
    if(showNames==TRUE) print(examName)
    
    examStartRow <- 1
    examEndRow <- nrow(examDF)
    
    ### add additional columns here
    
    ###
    
    # get the names of all unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    if(getChart == TRUE) { 
      uniqueSeries <- uniqueSeries[seriesNum] 
      assign("uniqueSeries", uniqueSeries, pos=1)
      assign("seriesName", uniqueSeries, pos=1) 
    }
    
    # make a new
    
    # make the output data frame
    outputDF <- NULL
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      seriesName <- uniqueSeries[j]
      
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      if(getChart==TRUE) {
        assign("seriesDF", seriesDF, pos=1)
        # break
      }
      
      if(showNames==TRUE) print(paste("series", seriesName))
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # uniqueCharts <- names(seriesDF)
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      if(getChart == TRUE) {
        uniqueCharts <- uniqueCharts[chartNum]
        assign("uniqueCharts", uniqueCharts[chartNum], pos=1)
        assign("chartName", uniqueCharts[chartNum], pos=1) 
      }
      
      # make a new series name, to correct for inconsistencies in data collection
      newSeriesName <- paste0("Series_", j)
      
      # loop over each chart in the series 
      for(k in 1:length(uniqueCharts)) {
        # k=1
        chartName <- uniqueCharts[k]
        
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        # chartDF[,1:5] <- as.character(chartDF[,1:5])
        
        if(getChart==TRUE) {
          assign("chartDF", chartDF, pos=1)
          # break
        }
        
        if(showNames==TRUE) print(chartName)
        
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ### process the stimulus segments
        
        # a vector of event onset rows
        eventNames <- unique(as.character(chartDF$eventName[chartDF$eventName!=""]))
        # exclude non-scored stimulus events
        # excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB")
        eventNames <- eventNames[!(eventNames %in% excludeEvents)]
        # exclude the first iteration of repeated stimuli, indicated with an "a" appended to the event name
        eventNames <- eventNames[grep(".*a$", eventNames, invert=TRUE)]
        
        if(getChart == TRUE) { 
          eventNames <- eventNames[segmentNum] 
        }
        
        # make a new chart name, to  correct for inconsistencies in data collection
        newChartName <- paste0("Chart_", k)
        
        sensorNames <- as.character(unique(chartDF$sensorName))

        # loop over all the sensors and calculate the R/C ratios
        for (l in 1:length(sensorNames)) {
          # l=1
          sensorName <- sensorNames[l]
          # get the onset row  for the chart data frame
          sensorRows <- which(chartDF$sensorName==sensorName)
          
          # select the measurements for the sensor data frame
          sensorDF <- chartDF[sensorRows,]
          
          if(showNames==TRUE) print(sensorName)
          
          # keep only the scored questions
          if(length(grep(pattern=".*SR.*", toupper(eventNames)))>0) { 
            # to exclude sacrifice questions from the analysis
            eventNames <- eventNames[-grep(pattern=".*SR.*", toupper(eventNames))]
          }
          eventNames <- eventNames[grep(pattern=".*[R C].*", eventNames)]
          
          # get the sensor measurements for RQs and CQs
          sensorMeasurements <- sensorDF$sensorMeasurement[sensorDF$eventName %in% eventNames]
          
          # get the RQ and CQ indices in the eventNames vector
          RQs <- grep(pattern=".*R.*", eventNames)
          CQs <- grep(pattern=".*C.*", eventNames)
          
          # then get the RQ and CQ names for later use
          RQNames <- eventNames[RQs]
          CQNames <- eventNames[CQs]
          
          # exit the loop if no RQs or no CQs
          if(length(RQs)==0) next
          if(length(CQs)==0) next
          
          # get the RQ and CQ values from the sensorMeasurements vector
          RQVals <- sensorMeasurements[RQs]
          CQVals <- sensorMeasurements[CQs]
          
          # Laplace add 1 smooth
          RQVals <- RQVals + 1
          CQVals <- CQVals + 1
          
          
          ###### compute the R/C ratios
          
          # 1. score left == each RQ compared to the preceeding CQ
          RCLeft <- rep(NA, times=length(RQs))
          CQNameLEFT <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            for (m in 1:length(RQVals)) {
              # identify the CQ without throwing a warming if there is none
              useCQ <- ifelse(length(which(CQs < RQs[m]))!=0, 
                              CQs[max(which(CQs < RQs[m]), na.rm=TRUE)],
                              NA )
              # if no left CQ then use the right CQ
              if(is.na(useCQ)) useCQ <- CQs[min(which(CQs > RQs[m]), na.rm=TRUE)]
              # calculate the R/C ratio
              RCLeft[m] <- log( sensorMeasurements[RQs[m]] / sensorMeasurements[useCQ] )
              # set the CQName
              CQNameLEFT[m] <- eventNames[useCQ]
            } # end for loop
          } # end if
          
          # 2. score right
          RCRight <- rep(NA, times=length(RQs))
          CQNameRIGHT <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            for (m in 1:length(RQVals)) {
              # identify the CQ without throwing a warming if there is none
              useCQ <- ifelse(length(which(CQs > RQs[m]))!=0, 
                              CQs[min(which(CQs > RQs[m]), na.rm=TRUE)],
                              NA )
              # if no right CQ then use the left CQ
              if(is.na(useCQ)) useCQ <- CQs[max(which(CQs < RQs[m]), na.rm=TRUE)]
              # calculate the R/C ratio
              RCRight[m] <- log( sensorMeasurements[RQs[m]] / sensorMeasurements[useCQ] )
              # set the CQName
              CQNameRIGHT[m] <- eventNames[useCQ]
            } # end for loop
          } # end if
          
          # 3. score each RQ to the greather change in physiology for the preceeding or subsequent CQ
          RCGreater <- rep(NA, times=length(RQs))
          CQNameGREATER <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            for (m in 1:length(RQVals)) {
              # select the preceding and subsequent CQ without throwing a warning
              precedingCQ <- ifelse(length(which(CQs < RQs[m]))!=0, 
                     CQs[max(which(CQs < RQs[m]), na.rm=TRUE)], 
                     NA )
              subsequentCQ <- ifelse(length(which(CQs > RQs[m]))!=0,
                     CQs[min(which(CQs > RQs[m]), na.rm=TRUE)], 
                     NA )
              CQChoice <- c(precedingCQ, subsequentCQ)
              # proceed to the next iteration or exit if there is no CQ measurement
              if(all(is.na(sensorMeasurements[CQChoice]))) next
              useCQ <- CQChoice[which.max(sensorMeasurements[CQChoice])]
              # exit the for loop if there is no CQ to use
              if(is.na(useCQ)) next
              RCGreater[m] <- log( sensorMeasurements[RQs[m]] / sensorMeasurements[useCQ] )
              CQNameGREATER[m] <- eventNames[useCQ]
            } # end for loop
          } # end if
          
          # 4. score each RQ to the smaller change in physiology for the preceeding or subsequent CQ
          RCSmaller <- rep(NA, times=length(RQs))
          CQNameSMALLER <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            for (m in 1:length(RQVals)) {
              # select the preceding and subsequent CQ without throwing a warning
              precedingCQ <- ifelse(length(which(CQs < RQs[m]))!=0, 
                     CQs[max(which(CQs < RQs[m]), na.rm=TRUE)], 
                     NA )
              subsequentCQ <- ifelse(length(which(CQs > RQs[m]))!=0,
                     CQs[min(which(CQs > RQs[m]), na.rm=TRUE)], 
                     NA )
              CQChoice <- c(precedingCQ, subsequentCQ)
              # proceed to the next iteration or exit if there is no CQ measurement
              if(all(is.na(sensorMeasurements[CQChoice]))) next
              useCQ <- CQChoice[which.min(sensorMeasurements[CQChoice])]
              # exit the for loop if there is no CQ to use
              if(is.na(useCQ)) next
              RCSmaller[m] <- log( sensorMeasurements[RQs[m]] / sensorMeasurements[useCQ] )
              CQNameSMALLER[m] <- eventNames[useCQ]
            } # end for loop
          } # end if
          
          # 5. score mean
          RCMean <- rep(NA, times=length(RQs))
          CQNameMEAN <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            RCMean <- log( RQVals / mean(CQVals[which(!is.na(CQVals))], na.rm=TRUE) )
            CQNameMEAN <- rep("CQMean", times=length(RQs))
          } # end if
          
          # 6. score to CQ max change in physiological activity
          RCMax <- rep(NA, times=length(RQs))
          CQNameMAX <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            RCMax <- log( RQVals / max(CQVals, na.rm=TRUE) )
            CQNameMAX <- rep("CQMax", times=length(RQs))
          } # end if
          
          # 7. score to CQ min change in physiological activity
          RCMin <- rep(NA, times=length(RQs))
          CQNameMIN <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            RCMin <- log( RQVals / min(CQVals, na.rm=TRUE) )
            CQNameMIN <- rep("CQMin", times=length(RQs))
          } # end if
          
          # 8. score to mean CQ after removing the min CQ
          RCTrimMaxMean <- rep(NA, times=length(RQs))
          CQNameTRIMMAXMEAN <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            ifelse(length(CQVals[which(!is.na(CQVals))]) < 3,
                   # ifelse is vectorized if the assigniment is completed within the ifelse statement
                   RCTrimMaxMean <- log( RQVals / mean(CQVals[which(!is.na(CQVals))]) ),
                   RCTrimMaxMean <- log( RQVals / ( ( sum(CQVals[which(!is.na(CQVals))], na.rm=TRUE) - max(CQVals, na.rm=TRUE) ) / 
                                                 (length(CQVals[which(!is.na(CQVals))])-1) ) ) ) # end ifelse
            CQNameTRIMMAXMEAN <- rep("CQTrimMaxMean", times=length(RQs))
          } # end if
          
          # 9. score the mean CQ after removing the max CQ
          RCTrimMinMean <- rep(NA, times=length(RQs))
          CQNameTRIMMINMEAN <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            ifelse(length(CQVals[which(!is.na(CQVals))]) < 3,
                   # ifelse is vectorized if the assigniment is completed within the ifelse statement
                   RCTrimMinMean <- log( RQVals / mean(CQVals[which(!is.na(CQVals))]) ),
                   RCTrimMinMean <- log( RQVals / ( ( sum(CQVals[which(!is.na(CQVals))], na.rm=TRUE) - min(CQVals[which(!is.na(CQVals))], na.rm=TRUE) ) / 
                                                 (length(CQVals[which(!is.na(CQVals))])-1) ) ) ) # end ifelse
            CQNameTRIMMINMEAN <- rep("CQTrimMinMean", times=length(RQs))
          } # end if
          
          # 10 score each RQ to the median of the CQ measurements
          RCMedian <- rep(NA, times=length(RQs))
          CQNameMEDIAN <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            RCMedian <- log( RQVals / median(CQVals[which(!is.na(CQVals))], na.rm=TRUE) )
            CQNameMEDIAN <- rep("CQMedian", times=length(RQs))
          } # end if
          
          # 11 score each RQ to the closest preceding or subsequent CQ in the sequence
          RCClosest <- rep(NA, times=length(RQs))
          CQNameCLOSEST <- rep(NA, times=length(RQs))
          # score only if 2 or more CQ measurements 
          if(length(CQVals[which(!is.na(CQVals))])>=2) { 
            for (m in 1:length(RQVals)) {
              # select the preceding and subsequent CQ without throwing a warning
              precedingCQ <- ifelse(length(which(CQs < RQs[m]))!=0, 
                                    CQs[max(which(CQs < RQs[m]), na.rm=TRUE)], 
                                    NA )
              subsequentCQ <- ifelse(length(which(CQs > RQs[m]))!=0,
                                     CQs[min(which(CQs > RQs[m]), na.rm=TRUE)], 
                                     NA )
              CQChoice <- c(precedingCQ, subsequentCQ)
              # proceed to the next iteration or exit if there is no CQ measurement
              if(all(is.na(sensorMeasurements[CQChoice]))) next
              # select the closest CQ
              useCQ <- CQChoice[which.min(abs(RQs[m]- CQChoice))]
              # exit the for loop if there is no CQ to use
              if(is.na(useCQ)) next
              RCClosest[m] <- log( sensorMeasurements[RQs[m]] / sensorMeasurements[useCQ] )
              CQNameCLOSEST[m] <- eventNames[useCQ]
            } # end for loop
          } # end ifelse
          
          ######
          
          # use mean replacement for missing CQs
          # meanReplace <- TRUE
          
          # choose the R/C method
          # "LEFT", "RIGHT", "GREATER", "SMALLER", "MEAN", "MAX", "MIN", "TRIMMAXMEAN", "TRIMMINMEAN", "MEDIAN"
          RCChoice <- "LEFT"
          switch(RCChoice, 
                 GREATER = { CQSelect = RCGreater
                 CQSelectName = CQNameGREATER },
                 LEFT= { CQSelect=RCLeft 
                CQSelectName = CQNameLEFT } 
          )

#           switch(sensorName, ## this works well 13-11-2015 ##
#                  "UPneumo" = {cutRatioUL = 1.5; cutRatioLL = 1.05},
#                  "LPneumo" = {cutRatioUL = 1.5; cutRatioLL = 1.05},
#                  "EDA" = {cutRatioUL = 1.05; cutRatioLL = 1.05},
#                  "Cardio" = {cutRatioUL = 1.5; cutRatioLL = 1.25},
#                  "PLE" = {cutRatioUL = 1.01; cutRatioLL = 1.01} 
#                  ) 
          
#           switch(sensorName,
#                  "UPneumo" = {cutRatioUL = 1.5; cutRatioLL = 1.05}, # 1.05
#                  "LPneumo" = {cutRatioUL = 1.5; cutRatioLL = 1.05}, 
#                  "EDA" = {cutRatioUL = 1.025; cutRatioLL = 1.025}, # 1.05
#                  "Cardio" = {cutRatioUL = 1.5; cutRatioLL = 1.25}, # 1.2
#                  "PLE" = {cutRatioUL = 1.025; cutRatioLL = 1.025} # 1.02
#           ) 

          switch(sensorName,
                 "UPneumo" = {cutRatioUL = 2; cutRatioLL = 1.05}, # 1.05
                 "LPneumo" = {cutRatioUL = 2; cutRatioLL = 1.05}, 
                 "EDA" = {cutRatioUL = 1.05; cutRatioLL = 1.05}, # 1.05
                 "Cardio" = {cutRatioUL = 1.5; cutRatioLL = 1.25}, # 1.2
                 "PLE" = {cutRatioUL = 1.025; cutRatioLL = 1.025} # 1.02
          ) 
          # uniqueExams[c(18, 32, 33, 37, 2,  6,  8, 10, 12, 16, 21, 26, 29, 39, 40)] # errors 14-11-2015
          # errors <- c(6, 12, 16, 29, 37) # 15-11-2015
          # # Dave_Tests_Greg, Doug_tests_Joe, Greg_tests_Mike_Wyatt, Mike_A_Tests_Orlando, Rivera_09Dec14_Orlando_test_Joe)
          # inconclusives <- c(2, 2340) # 15-11-2015
          # BELL2014-DEC4-RIVERA, TedTestsOrlando
          
          switch(sensorName,
                 "UPneumo" = {cutMax = 3}, # best at 1.75
                 "LPneumo" = {cutMax = 3}, 
                 "EDA" = {cutMax = 48}, # mayb be no advantage to any constraint
                 "Cardio" = {cutMax = 6}, # max r at 6 but concordance peaks at 8
                 "PLE" = {cutMax = 2.5} # 2.5 seems ok
          ) 
          
          # 0.6931472 log(2)
          # 1.648721 exp(.5)
          # 2.718282 exp(1)
          # 4.481689 exp(1.5)
          # 7.098617 exp(2)
          # 12.18249 exp(2.5)
          # 20.08554 exp(3)

          RCScore <- rep(NA, times=length(RQs))

          # input CQSelect is a logged R/C ratio
          ifelse(sensorName %in% c("UPneumo", "LPneumo", "PLE"),
                 integerScores <- c(1, -1),
                 integerScores <- c(-1, 1) )
                 for (p in 1:length(RCLeft)) {
                   RCScore[p] <- ifelse(is.na(CQSelect[p]),
                                        NA,
                                        ifelse(CQSelect[p] >= log(cutRatioUL) & CQSelect[p] <= log(cutMax),
                                               integerScores[1],
                                               ifelse(CQSelect[p] <= -log(cutRatioLL) & CQSelect[p] >= -log(cutMax),
                                                      integerScores[2],
                                                      0) ) ) # end ifelse 
                 } # end for loop
                 

          
          

          # construct the output data frame for the chart
          RCRatioDF <- as.data.frame(matrix(data=NA,nrow=length(RQs), ncol=41 ))
          for(o in 1:length(RQs)) {
            RCRatioDF[o,] <- c(examName, 
                               newSeriesName, 
                               newChartName, 
                               sensorName,
                               RQNames[o], 
                               RQVals[o], 
                               "LEFT", 
                               CQNameLEFT[o], 
                               RCLeft[o], 
                               "RIGHT", 
                               CQNameRIGHT[o], 
                               RCRight[o], 
                               "GREATER", 
                               CQNameGREATER[o], 
                               RCGreater[o], 
                               "SMALLER", 
                               CQNameSMALLER[o],
                               RCSmaller[o], 
                               "MEAN",
                               CQNameMEAN[o], 
                               RCMean[o], 
                               "MAX",
                               CQNameMAX[o], 
                               RCMax[o], 
                               "MIN",
                               CQNameMIN[o], 
                               RCMin[o], 
                               "TRIMMAXMEAN",
                               CQNameTRIMMAXMEAN[o], 
                               RCTrimMaxMean[o], 
                               "TRIMMINMEAN",
                               CQNameTRIMMINMEAN[o], 
                               RCTrimMinMean[o], 
                               "MEDIAN",
                               CQNameMEDIAN[o], 
                               RCMedian[o],
                               "CLOSEST",
                               CQNameCLOSEST[o],
                               RCClosest[o],
                               RCMethod,
                               RCScore[o]
                               )
          }
          
          if(getChart==TRUE) {
            names(RCRatioDF) <- c("examName", 
                                  "seriesName", 
                                  "chartName", 
                                  "sensorName",          
                                  "RQNames",
                                  "RQValues",
                                  "Method1",
                                  "CQMethod1",
                                  "RCMethod1",
                                  "Method2",
                                  "CQMethod2",
                                  "RCMethod2",
                                  "Method3",
                                  "CQMethod3",
                                  "RCMethod3",
                                  "Method4",
                                  "CQMethod4",
                                  "RCMethod4",
                                  "Method5",
                                  "CQMethod5",
                                  "RCMethod5",
                                  "Method6",
                                  "CQMethod6",
                                  "RCMethod6",
                                  "Method7",
                                  "CQMethod7",
                                  "RCMethod7",
                                  "Method8",
                                  "CQMethod8",
                                  "RCMethod8",
                                  "Method9",
                                  "CQMethod9",
                                  "RCMethod9",
                                  "Method10",
                                  "CQMethod10",
                                  "RCMethod10",
                                  "Method11",
                                  "CQMethod11",
                                  "RCMethod11",
                                  "RCMethod",
                                  "RCScore") 
          }
          
          ### output
          
          if(getChart==TRUE) {
            if(is.na(sensorNum)) {
              assign(paste0("RCRatioDF_", sensorName), RCRatioDF, pos=1)
              assign("sesnsorDF", sensorDF, pos=1)
              break
            }
          }
          
          outputDF <- rbind.data.frame(outputDF, RCRatioDF)
          
        } # end loop over l sensors 
        
        if(getChart==TRUE) {
          assign("chartDF", chartDF, pos=1)
          break
        }
        
      } # end iteration over k chart data frames 
      
      if(getChart==TRUE) {
        assign("seriesDF", seriesDF, pos=1)
        break
      }
      
    } # end iteration over j series data frames
    
    names(outputDF) <- c("examName", 
                          "seriesName", 
                          "chartName", 
                          "sensorName",
                          "RQNames",
                          "RQValues",
                          "Method1",
                          "CQMethod1",
                          "RCMethod1",
                          "Method2",
                          "CQMethod2",
                          "RCMethod2",
                          "Method3",
                          "CQMethod3",
                          "RCMethod3",
                          "Method4",
                          "CQMethod4",
                          "RCMethod4",
                          "Method5",
                          "CQMethod5",
                          "RCMethod5",
                          "Method6",
                          "CQMethod6",
                          "RCMethod6",
                          "Method7",
                          "CQMethod7",
                          "RCMethod7",
                          "Method8",
                          "CQMethod8",
                          "RCMethod8",
                          "Method9",
                          "CQMethod9",
                          "RCMethod9",
                          "Method10",
                          "CQMethod10",
                          "RCMethod10",
                          "Method11",
                          "CQMethod11",
                          "RCMethod11",
                          "RCMethod",
                          "RCScore")
    
#     ### fix the pneumo scores before proceeding
#     outputDF$RCScore[outputDF$sensorName=="UPneumo" & !is.na(outputDF$RCScore)] <- outputDF$RCScore[outputDF$sensorName=="UPneumo" & !is.na(outputDF$RCScore)] * -1
#     outputDF$RCScore[outputDF$sensorName=="LPneumo" & !is.na(outputDF$RCScore)] <- outputDF$RCScore[outputDF$sensorName=="LPneumo" & !is.na(outputDF$RCScore)] * -1
#     
#     ### fix the PLE scores
#     outputDF$RCScore[outputDF$sensorName=="PLE" & !is.na(outputDF$RCScore)] <- outputDF$RCScore[outputDF$sensorName=="PLE"] * -1
    
    
    
    # save the outputDF to the global environment 
    assign(paste0(examName, "_scores"), outputDF, pos=1) 
    
    if(getChart==TRUE) {
      assign("examDF", examDF, pos=1)
      break
    }
    
    # write the output data frame to a csv text file
    write.csv(outputDF, file=paste0(examName, "_scores", ".csv"), row.names=FALSE)
    
  } # end iteration over i exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  if(output==TRUE) return(examDF)
  
} # end RCParseFn()

###########



# fix some problems with data collection and the series names
# BELL2014_DEC8_ANDERSON_measurements$seriesName <- 1
# Greg_tests_Ted_measurements$seriesName <- 1


RCParseFn(x=uniqueExams, showNames=TRUE, output=FALSE, getChart=FALSE)



################



# function to combine all score data frames in the cwd to to a single data set

# scoreDFsVector <- ls(pattern=".*_scores")
scoresDataSet <- function(x=scoreDFsVector) {
  # function to combine all score data frames in the cwd to to a single data set
  DFsVector <- x
  numberColumns <- ncol(get(DFsVector[1], pos=1))
  outputDF <- as.data.frame(matrix(data=NA, nrow=0, ncol=numberColumns))
  for (i in 1:length(DFsVector)) {
    # loop over the score data frames and combine them
  outputDF <- rbind.data.frame(outputDF, get(DFsVector[i], pos=1))
  }
  # change the RCScore column to numeric
  outputDF$RCScore <- as.numeric(outputDF$RCScore)
  # weight the EDA scores
  outputDF$RCScore[outputDF$sensorName=="EDA"] <- outputDF$RCScore[outputDF$sensorName=="EDA"] * 2
  # add the confirmation column to each case in the scoresDf data frame
  outputDF$criterionState <- NA
  # assign(outputDF, "scoresDF", pos=1)
  return(outputDF)
} # end scoresDataSet function


scoresDF <- scoresDataSet(x=ls(pattern=".*_scores"))



# scoresDFSAVE <- scoresDF



####################################

#### combine the upper and lower pneumo scores to a single score
combinePneumos <- function(x=scoresDF) {
  # function to combine the upper and lower pneumo sores to a single score
  # input is a skinny data frame of scores with a column named "RCScore"
  # output is a skinny data frame of the same information with combined pneumo scores
  inputDF <- x
  # first make an index vector for the upper and lower pneumo scores in the scoresDF data frame
  pneumoIndices <- which(inputDF$sensorName ==  "UPneumo" | inputDF$sensorName=="LPneumo" )
  # then make a separate data frame to work with the pneumo scores
  pneumoDF <- inputDF[pneumoIndices,]
  # loop over the exams
  uniqueExams <- unique(pneumoDF$examName)
  for (i in 1:length(uniqueExams)) {
    # i=1
    examIndices <- which(pneumoDF$examName==uniqueExams[i])
    examDF <- pneumoDF[examIndices,]
    # make a vector for unique series within each exam
    uniqueSeries <- unique(examDF$seriesName)
    # loop over each series
    for (j in 1:length(uniqueSeries)) {
      # j=1
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      # make a vector of unique charts for each series
      uniqueCharts <- unique(seriesDF$chartName)
      # loop over each chart
      for (k in 1:length(uniqueCharts)) {
        # k=1
        chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
        # make a vector of unique RQs
        uniqueRQs <- unique(chartDF$RQNames)
        # loop over each RQ
        for (l in 1:length(uniqueRQs)) {
          # l=1
          RQDF <- chartDF[chartDF$RQNames==uniqueRQs[l],]
          # make a vector of unique sensor names
          uniqueSensors <- unique(RQDF$sensorName)
          # should now be only 2 rows in the data frame
          P1Score <- RQDF$RCScore[1] # at this point there may be confusion as to which is P1 and P2
          P2Score <- RQDF$RCScore[2]
          ########## now combine the upper and lower #######
          # this will keep and use a single extant pneumo score
          if (is.na(P1Score)) P1Score<-P2Score
          if (is.na(P2Score)) P2Score<-P1Score
          singlePneumoScore <- ifelse(P1Score + P2Score > 0,
                                      1,
                                      ifelse(P1Score + P2Score <0,
                                             -1,
                                             0 ) )
          ### assign the single pneumo score to the upper pneumo and set the lower pneumo to NA
          examDF$RCScore[examDF$examName==uniqueExams[i] & 
                           examDF$seriesName==uniqueSeries[j] &
                           examDF$chartName==uniqueCharts[k] &
                           examDF$RQNames==uniqueRQs[l] &
                           examDF$sensorName==uniqueSensors[1]] <- singlePneumoScore
          examDF$RCScore[examDF$examName==uniqueExams[i] & 
                           examDF$seriesName==uniqueSeries[j] &
                           examDF$chartName==uniqueCharts[k] &
                           examDF$RQNames==uniqueRQs[l] &
                           examDF$sensorName==uniqueSensors[2]] <- NA
          #########
        } # end loop over l RQs
      } # end loop over k charts
    } # end loop over j series
    # write the examDF information to the pneumoDF
    pneumoDF[examIndices,] <- examDF[,]
  } # end loop over i exams
  # and finally write the pneumoDF back to the inputDF
  inputDF[pneumoIndices,] <- pneumoDF[,]
  # rename the UPneumo column
  inputDF$sensorName[inputDF$sensorName=="UPneumo"] <- "Pneumo"
  # and remove the LPneumo column
  inputDF <- inputDF[inputDF$sensorName!="LPneumo",]
  # return the input data frame
  return(inputDF)
} # end combinePneumos() function

scoresDF2 <- combinePneumos(x=scoresDF)

# save the scores data frame
# write.csv(scoresDF2, file="OhioPLE_scores.csv", row.names=FALSE)

########################


source('~/Documents/R_programming/NCCA_ASCII_Parse/getCriterionState.R', echo=TRUE)
