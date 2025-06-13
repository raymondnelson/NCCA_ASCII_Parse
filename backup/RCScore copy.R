# R function to get RQ and CQ measurements from the measurment DF
# Raymond Nelson
# 10-25-2016

##########################



source('~/Dropbox/R/NCCA_ASCII_Parse/getSegment.R', echo=FALSE)

# RCScoreArgs <- list(x="examName", y="seriesName", z="chartName", makeDF=TRUE, output=FALSE)

RCScoreFn <- function(x=examName, y=seriesName, z=chartName, makeDF=TRUE, output=FALSE) {
  # function to compute the R/C Ratio and scores from the measurement data frome
  # 
  # x input is a scalar with a unique exam name
  # this function does not iterate over a vector of exam names
  # y input is a scalar with the series name
  # z input is a scalar with the chart name
  # makeDF=TRUE will assign the funtion output to a data frame in the global env as a side effect
  # output=TRUE will output the measurement data frame in the normal manner
  #
  ######
  
  # print a message
  # myChartFUN()

  examName <- x
  seriesName <- y
  chartName <- z
  
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Measurements", "*")
  
  # initialize the measurementDF for the exam
  measurementDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  # measurmentDF includes these colums 
  # [1] "examName"          "seriesName"        "chartName"         "eventName"        
  # [5] "sensorName"        "sensorMeasurement" "rankScore"         "score"            
  # [9] "integerScore" 
  
  assign("measurementDF", measurementDF, pos=1)
  
  # keep only the measurements for the selected series and chart
  selectRows <- which(measurementDF$seriesName==seriesName & measurementDF$chartName==chartName)
  # print(paste("rows", selectRows))
  chartMeasurementDF <- measurementDF[selectRows,]
  
  assign("chartMeasurementDF", chartMeasurementDF, pos=1)
  
  # make a vector of unique sensor names
  # uniqueSensors <- unique(str_sub(names(RQs), (str_locate(names(myEvents), "_")[,1]+1), -1))
  uniqueSensors <- as.character(unique(chartMeasurementDF$sensorName))
  
  ####### initiate the workingDF data frame for RQ and CQ scores ######
  
  # make a vector of RQs and CQs
  workingRows <- grep("[CR]+", chartMeasurementDF$eventName)
  # use all events except first and last if a chart has no RQs and CQs
  if(length(workingRows) == 0) {
    workingRows <- which(!(chartMeasurementDF$eventName %in% excludeEvents))
    # exclude the first and last event
    # commented out 11-8-2016 to retain repeated stimulus events
    workingEvents <- chartMeasurementDF$eventName[workingRows]
    workingEvents <- unique(workingEvents)
    workingEvents <- workingEvents[-c(1,length(workingEvents))]
    workingRows <- which(chartMeasurementDF$eventName %in% workingEvents)
  }
  
  ####################################
  
  # only work with charts that have some stimulus events 
  # if(length(workingRows) != 0) {
    
  if(length(workingRows) == 0) {
    if(output == TRUE) return(measurementDF)
  }
    
    
    
    
    # initiate the working data frame
    workingDF <- chartMeasurementDF[workingRows,]
    # the workingDF includes RQs and CQs
    # if no RQs and CQs the workingDF includes all events except the first and last
    
    ###### calculate the rank scores ######
    
    # iterate over the sensor names to calculate the rank scores
    for (m in 1:length(uniqueSensors)) {
      rowSelect <- workingDF$sensorName==uniqueSensors[m]
      workingDF[rowSelect,'rankScore'] <- rank(workingDF[rowSelect,'sensorMeasurement'], 
                                               ties.method="average", 
                                               na.last="keep")
    }
    
    ### 11-9-2016 combine the ranks for upper and lower pnuemo and re-rank the pneumo 
    
    ##########  cacluate the RRM relative response magnitude scores   #############
    
    # iterate over the sensor names to calculate the RRM score
    for (m in 1:length(uniqueSensors)) {
      # get all the rows for each m sensor
      rowSelect <- which(workingDF$sensorName==uniqueSensors[m])
      # next iteration if there are no measurements
      if(length(which(!is.na(workingDF$sensorMeasurement[rowSelect]))) == 0) next()
      # get the max min and range for the sensor scores
      maxResponse <- max(workingDF$sensorMeasurement[rowSelect], na.rm=TRUE)
      minResponse <- min(workingDF$sensorMeasurement[rowSelect], na.rm=TRUE)
      responseRange <- maxResponse - minResponse
      # iterate over the stimulus events to calculate the RRM for each event
      for(n in 1:length(rowSelect)) {
        thisResponse <- workingDF$sensorMeasurement[rowSelect][n]
        thisRRM <- (thisResponse - minResponse) / responseRange
        workingDF$RRMScore[rowSelect][n] <- thisRRM
      }
    }
    
    ### 11/6/2016 need to combine the RRM scores for upper and lower pneumo into a single score
    
    # pass the workingDF back to the measurement DF and save to the global environment
    chartMeasurementDF[workingRows,] <- workingDF
    
    ###########   calculate the ipsative Z scores   #############
    
    
    
    ###########  calculate the R/C scores   #############
    
    # make a data frame of RQs and CQs
    rqRows <- grep("R", chartMeasurementDF$eventName)
    rqDF <- chartMeasurementDF[rqRows,]
    cqRows <- grep("C", chartMeasurementDF$eventName)
    cqDF <- chartMeasurementDF[cqRows,]
    
    # make a vector of unique RQs
    uniqueRQs <- unique(rqDF$eventName)
    
    # and another vector of unique CQs
    uniqueCQs <- unique(cqDF$eventName)
    
    ##############   proceed only if there are at least 2 RQs and at least 2 CQs   ##########
    
    if(length(uniqueRQs) >= 2 & length(uniqueCQs) >= 2) {
      
      # get all the scores for an RQ
      # with(measurementDF, measurementDF[eventName=="R1",])
      
      # calculate the average CQ measurement for each sensor
      cqMeans <- rep("", times=length(uniqueSensors))
      for (m in 1:length(uniqueSensors)) {
        # need to manage NA values in case there is no EDA measurement for a stim event
        cqMeans[m] <- mean(cqDF[cqDF$sensorName==uniqueSensors[m],"sensorMeasurement"], na.rm=TRUE)
      }
      cqMeans <- as.numeric((cqMeans))
      names(cqMeans) <- uniqueSensors
      print(cqMeans)
      
      # rqScores <- rep("", times=length(uniqueRQs))
      
      # loop over the RQs and calculate the logged RQ/CQ ratio
      for (m in 1:length(uniqueRQs)) {
        # and also loop over the sensors
        for (n in 1:length(cqMeans)) {
          # first get the row indices for the RQ and sensor
          rowSelect <- rqDF$eventName==uniqueRQs[m] & rqDF$sensorName==uniqueSensors[n]
          # then calculate the RQ / CQmean ratio
          rqDF$score[rowSelect] <- log(rqDF$sensorMeasurement[rowSelect] / cqMeans[n])
        }
      }
      
      # may need to further work the PLE scores
      
      # invert the EDA and Cardio so the + log scores correspond to truth-telling
      reSelectRows <- which(rqDF$sensorName=="EDA" | rqDF$sensorName =="Cardio")
      rqDF$score[reSelectRows] <- -as.numeric(rqDF$score[reSelectRows])
      
      ############   calculate the integer score from the R/C score   #############
      
      # set the constraints
      posPneumoLow <- log(1.25) # 0.2231436
      posPneumoHigh <- log(1.5) # 0.4054651
      negPneumoLow <- -log(1.1) # -0.04879016 -.0953
      negPneumoHigh <- -log(1.5) # -0.4054651
      posEDAHigh <- log(exp(100)) # 1.098612
      posEDALow <- log(1.05) # 0.04879016
      negEDAHigh <- -log(exp(100)) # -1.098612
      negEDALow <- -log(1.05) # -0.04879016
      posCardioHigh <- log(exp(100)) # 1.098612
      posCardioLow <- log(1.1) # 0.04879016
      negCardioHigh <- -log(exp(100)) # -1.098612
      negCardioLow <- -log(1.1) # -0.04879016
      posPLEHigh <- log(exp(100)) # 1.098612
      posPLELow <- .0993
      negPLEHigh <- -log(exp(100)) # -1.098612
      negPLELow <- -0.0993
      
      # assign("rqDF", rqDF, pos=1)
      # stop()
      
      ####  iterate over the RQ score and calculate the integer score  ####
      
      # selectRQs <- which(!is.na(rqDF$sensorMeasurement))
      selectRQs <- which(!is.na(rqDF$score))
      
      for (i in 1:length(selectRQs)) {
        # first get the sensor and score
        thisStimulusName <- rqDF$eventName[selectRQs[i]]
        thisSensor <- rqDF$sensorName[selectRQs[i]]
        thisScore <- as.numeric(rqDF$score[selectRQs[i]])
        # print(paste("this score:", thisScore))
        print(paste(thisStimulusName, thisSensor, thisScore))
        # then calculate the integer scores
        if(thisSensor == "UPneumo" | thisSensor == "LPneumo") {
          integerScore <- if(thisScore >= posPneumoLow & thisScore <= posPneumoHigh) {
            1
          } else if(thisScore <= negPneumoLow & thisScore >= negPneumoHigh) {
            -1
          } else 0
        } else if(thisSensor == "EDA") {
          integerScore <- if(thisScore >= posEDALow) {
            2
          } else if(thisScore <= negEDALow) {
            -2
          } else 0
        } else if(thisSensor == "Cardio") {
          integerScore <- if(thisScore >= posCardioLow) {
            1
          } else if(thisScore <= negCardioLow) {
            -1
          } else 0
        } else if(thisSensor == "PLE") {
          integerScore <- if(thisScore >= posPLELow) {
            1
          } else if(thisScore <= negPLELow) {
            -1
          } else 0
        } 
        
        print(paste("integer score", integerScore))
        
        # assign the sensor integer score to the rqDF data frame
        rqDF$integerScore[selectRQs[i]] <- integerScore
        
      } # end for loop over selectRQs
      
      ####  combine the upper and lower pneumo scores into a single pneumo score  ####
      
      # iterate over the RQs and select the upper and lower pneumo scores
      
      for (j in 1:length(uniqueRQs)) {
        P2 <- rqDF$integerScore[which(rqDF$eventName == uniqueRQs[j] & (rqDF$sensorName == "UPneumo"))]
        P2 <- as.numeric(P2)
        P1 <- rqDF$integerScore[which(rqDF$eventName == uniqueRQs[j] & (rqDF$sensorName == "LPneumo"))]
        P1 <- as.numeric(P1)
        # combine the upper and lower pneumo scores 
        Pneumo <- ifelse(P1 * P2 < 0, 0, ifelse(P1 * P2 > 0, P2, P1 + P2))
        names(Pneumo) <- "Pneumo"
        # assign the value to the rqDF
        pneumoRow <- which(rqDF$eventName == uniqueRQs[j] & (rqDF$sensorName == "Pneumo"))
        rqDF$integerScore[pneumoRow] <- Pneumo
      }
      
      # pass the rqDF back to the measurement DF and save to the global environment
      chartMeasurementDF[rqRows,] <- rqDF
      
    } # end if for 2 RQs and 2 CQs
    
    ######
    
    # save to the global environment for inspection
    # assign(paste0("chartMeasurementDF",i), chartMeasurementDF,pos=1)
    
    # pass the chartMeasurementDF back to the measurementDF
    measurementDF[selectRows,] <- chartMeasurementDF
    
  # } # end if length(workingRows) != 0
  
  ######
  
  # assign the measurement DF to the original object name in the global invironment
  if(makeDF==TRUE) {
    assign(paste0(examName, "_Measurements"), measurementDF, pos = 1)
  }
  
  # function output
  if(output == TRUE) return(measurementDF)

} # end RCScoreFn function


###############################



# RCScoreFn(x=examName, y=seriesName, z=chartName, makeDF=TRUE, output=FALSE)



# myExamFUN <- function() {print("ray")}
# mySeriesFUN <- function() {print("irv")}
# # myChartFUN <- function() {print("nel")}
# mySegmentFUN <- function() {print("rin")}



# this will be called using do.call() which can take function or character string as the first arg
# chartFUN <- RCScoreFn
# 
# getExamFn(x=uniqueExams)


# calculate the sum
# sum(as.numeric(a09N1214_Measurements$integerScore), na.rm=TRUE)

# rqRows <- grep("R", a09N1214_Measurements$eventName)

# RQs <- unique(a09N1214_Measurements$eventName[rqRows])

# names(RQs) <- RQs

# length(RQs)

# loop over the RQs to caculate the the RQ scores
# for (i in 1:length(RQs)) {
#   selectRows <- which(a09N1214_Measurements$eventName == RQs[i])
#   # assign(RQs[i], sum(as.numeric(a09N1214_Measurements$integerScore[selectRows]), na.rm = TRUE))
#   RQs[i] <- sum(as.numeric(a09N1214_Measurements$integerScore[selectRows]), na.rm = TRUE)
# }



# RQs


