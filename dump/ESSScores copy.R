# R function to compute the ESS scores from the RC score in the measurment DF
# called by the RCScoreFn function in the RCScore.R script

ESSScoresFn <- function(x=RqCqDF) {
  # R function to compute the ESS scores from the measurements data frame
  # Raymond Nelson
  # 11-27-2017
  # called by the getScoresFn() in the scores.R script
  #
  ###
  # x input is a data frame of measurements for the RQs and CQs for a chart
  # output is the RqCqDF data frame with the integerScore column populated 
  # with ESS scores
  ##################
  
  RqCqDF <- x
  
  uniqueEvents <- unique(RqCqDF$eventLabel)
  
  # make a data frame of RQs for this chart
  # rqRows <- grep("R", chartMeasurementDF$eventLabel)
  rqRows <- grep("R", RqCqDF$eventLabel)
  # use the RqCqDF to avoid capturing annotations or other events
  rqDF <- RqCqDF[rqRows,]
  # View(rqDF)
  
  # and another data frame of CQs for this chart
  # cqRows <- grep("C", chartMeasurementDF$eventLabel)
  cqRows <- grep("C", RqCqDF$eventLabel)
  # use the RqCqDF to avoid capturing annotations or other events
  cqDF <- RqCqDF[cqRows,]
  # View(cqDF)
  
  # make a vector of unique RQs
  uniqueRQs <- unique(rqDF$eventLabel)
  
  # and another vector of unique CQs
  uniqueCQs <- unique(cqDF$eventLabel)
  
  assign("rqDF", rqDF, pos=1)
  assign("cqDF", cqDF, pos=1)
  # stop()
  
  if(length(uniqueEvents) != 0) {
    
    if(nrow(RqCqDF) > 1) {
      
      # also uses the R/C score
      
      # set the constraints
      
      posPneumoLow <- log(1.25) # 0.2231436
      posPneumoHigh <- log(1.5) # 0.4054651
      # negPneumoLow <- -log(1.05) # -0.04879016 
      negPneumoLow <- -log(1.1) # -.0953
      negPneumoHigh <- -log(1.5) # -0.4054651
      
      posEDAHigh <- log(500) # 1.098612
      posEDALow <- log(1.1) # 0.04879016
      negEDAHigh <- -log(500) # -1.098612
      negEDALow <- -log(1.1) # -0.04879016
      
      posCardioHigh <- log(500) # 1.098612
      posCardioLow <- log(1.1) # 0.04879016
      negCardioHigh <- -log(500) # -1.098612
      negCardioLow <- -log(1.1) # -0.04879016
      
      posPLEHigh <- log(100) # 4.60517
      posPLELow <- log(1.1) # .0953 # was .0993
      negPLEHigh <- -log(100) # -4.60517
      negPLELow <- -log(1.1) # .0953 # was -0.0993
      
      # proceed to calculate integer scores only if there are at least 2 RQs and at least 2 CQs
      if( length(uniqueRQs) >= 2 & length(uniqueCQs) >= 2 ) {
        
        # integer score is NA if the score is NA
        rqDF[is.na(as.numeric(rqDF$score)),"integerScore"] <- NA
        # View(rqDF)
        
        # select RQs for which the score is not NA
        selectRQs <- which(!is.na(as.numeric(rqDF$score)))
        
        # initialize the integerScore to NA to avoid errors if there is no score
        integerScore <- NA
        # iterate over the RQs
        i=4
        for (i in 1:length(selectRQs)) {
          # first get the stimulus name, sensor, and score
          thisStimulusName <- rqDF$eventLabel[selectRQs[i]]
          thisSensor <- rqDF$sensorName[selectRQs[i]]
          # thisScore is the R/C score
          thisScore <- as.numeric(rqDF$score[selectRQs[i]])
          # print the info to the console
          print(paste(thisStimulusName, thisSensor, thisScore))
          
          # then calculate the ESS scores
          
          if(thisSensor == "UPneumo" | thisSensor == "LPneumo") {
            integerScore <- if(thisScore >= posPneumoLow & thisScore <= posPneumoHigh) {
              as.character("+1")
            } else if(thisScore <= negPneumoLow & thisScore >= negPneumoHigh) {
              as.character("-1")
            } else 0
          } else if(any(thisSensor == "EDA", thisSensor == "AutoEDA", thisSensor == "ManualEDA")) {
            integerScore <- if(thisScore >= posEDALow) {
              as.character("-2")
            } else if(thisScore <= negEDALow) {
              as.character("+2")
            } else 0
          } else if(thisSensor == "Cardio") {
            integerScore <- if(thisScore >= posCardioLow) {
              as.character("-1")
            } else if(thisScore <= negCardioLow) {
              as.character("+1")
            } else 0
          } else if(thisSensor == "PLE") {
            integerScore <- if(thisScore >= posPLELow) {
              as.character("-1")
            } else if(thisScore <= negPLELow) {
              as.character("+1")
            } else 0
          }
          
          # assign the sensor integer score to the rqDF data frame
          if(!is.na(integerScore)) rqDF$integerScore[selectRQs[i]] <- integerScore
          
          # print the integer score to the console
          if(!is.na(integerScore)) print(paste("integer score", integerScore))
          
        } # end for loop over selectRQs
        
        ### combine the upper and lower pneumo scores
        
        for (j in 1:length(uniqueRQs)) {
          P2 <- rqDF$integerScore[which(rqDF$eventLabel == uniqueRQs[j] & 
                                          (rqDF$sensorName == "UPneumo"))]
          P2 <- as.numeric(P2)
          P1 <- rqDF$integerScore[which(rqDF$eventLabel == uniqueRQs[j] & 
                                          (rqDF$sensorName == "LPneumo"))]
          P1 <- as.numeric(P1)
          # combine the upper and lower pneumo scores 
          # 3-2-2017 this code should work for 3-position, ESS and 7-position
          Pneumo <- ifelse(P1 * P2 < 0, 
                           0, 
                           ifelse(P1 + P2 > 0, 
                                  max(c(P1, P2)), 
                                  ifelse(P1 + P2 < 0, 
                                         min(c(P1, P2)), 
                                         0
                                  ) 
                           ) 
          )
          
          names(Pneumo) <- "Pneumo"
          # assign the value to the rqDF
          pneumoRow <- which(rqDF$eventLabel == uniqueRQs[j] & (rqDF$sensorName == "Pneumo"))
          rqDF$integerScore[pneumoRow] <- Pneumo
        }
        
        # pass the rqDF back to the working DF
        RqCqDF[rqRows,] <- rqDF
        
        assign("RqCqDF", RqCqDF, pos=1)
        
      } # end if for 2 RQs and 2 CQs
      
    } # end if nrow(RqCqDF) > 1
    
  } # end if for length(uniqueEvents) != 0
  
  return(RqCqDF)

} # end ESSScoresFn



