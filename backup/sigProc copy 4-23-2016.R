# function to process the time series data
# including pneumo, EDA, cardio, PLE and activity sensors
# 4/23/2016
# Raymond Nelson

###########################


# library(stringr)


# get exam names from the _Data data frames
# uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[2]


###


# source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=TRUE)


######################################

sigProc <- function(x=uniqueExams, 
                        output=FALSE, 
                        showNames=TRUE) {
  # function to process the NCCA ASCII time series data
  # including pneumo, EDA, cardio, PLE and activity sensors
  # x input is a list of unique exams in the working directory
  # the input data is the output from the function in the preProc.R script and preProc() function
  # output=TRUE will output the time series for the last exam in the input vector of exam names
  # showNames=TRUE will print the exam names, series names, and chart names to the console
  
  ###
  
  uniqueExams <- x
  
  # loop over each exam in the list 
  for(i in 1:length(uniqueExams)) {
    # i=1
    examName <- uniqueExams[i]
    
    
    # get the names of time series lists for all unique series in each exam
    searchString <- paste0("*", examName, "_Data", "*")
    
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    if(showNames==TRUE) print(examName)
    
    examOnsetRow <- 1
    examEndRow <- nrow(examDF)
    
    ###################
    
    # add new columns for the processed pneumo Data
    examDF$c_UPneumoInh <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoInh <- rep(0, times=nrow(examDF))
    examDF$c_UPneumoExh <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoExh <- rep(0, times=nrow(examDF))
    examDF$c_UPneumoMid <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoMid <- rep(0, times=nrow(examDF))
    examDF$c_UPneumoDiff <- rep(0, times=nrow(examDF))
    examDF$c_LPneumoDiff <- rep(0, times=nrow(examDF))
    examDF$c_PneumoDiff <- rep(0, times=nrow(examDF))
    
    # add some EDA columns
    examDF$c_AutoEDA <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDADiff <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDABase <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDAPeak <- rep(0, times=nrow(examDF))
    examDF$c_AutoEDAMid <- rep(0, times=nrow(examDF))
    
    # add some new columns for the processed cardio data
    examDF$c_CardioSystolic <- rep(0, times=nrow(examDF))
    examDF$c_CardioDiastolic <- rep(0, times=nrow(examDF))
    examDF$c_CardioMinMax <- rep(0, times=nrow(examDF))
    examDF$c_CardioMid <- rep(0, times=nrow(examDF))
    # CardioMA is a very slow smoothed cardio
    examDF$c_CardioMA <- rep(0, times=nrow(examDF))
    examDF$c_CardioAmp <- rep(0, times=nrow(examDF)) 
    
    # add some columns for the PLE data if present
    if(sum(pmatch(names(examDF), "c_PL", nomatch=0))!=0) {
      examDF$c_PLMax <- rep(0, times=nrow(examDF))
      examDF$c_PLMin <- rep(0, times=nrow(examDF))
      examDF$c_PLMA <- rep(0, times=nrow(examDF))
      examDF$c_PLAmp <- rep(0, times=nrow(examDF))
    }
    
    # add some columns for the activity data if present
    if(sum(pmatch(names(examDF), "c_SE", nomatch=0))!=0) {
      examDF$c_SEMax <- rep(0, times=nrow(examDF))
      examDF$c_SEMin <- rep(0, times=nrow(examDF))
      examDF$c_SEMA <- rep(0, times=nrow(examDF))
      examDF$c_SEAmp <- rep(0, times=nrow(examDF))
    }
    
    #################
    
    # get the names of each unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    for(j in 1:length(uniqueSeries)) {
      # j=1
      # get the list of time series data for the charts in the exam
      seriesDF <- examDF[examDF$seriesName==uniqueSeries[j],]
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      seriesOnsetRow <- which(examDF$seriesName==uniqueSeries[j])[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      # k=1
      for(k in 1:length(uniqueCharts)) {
        # get the data frame with the time series data for each chart in the series
        chartDF <- seriesDF[seriesDF$chartName==uniqueCharts[k],]
        
        if(nrow(chartDF)<300) next()
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        ####################
        
        # make a vector of event names
        eventNames <- toupper(chartDF$eventLabel[chartDF$eventLabel!=""])
        # and a vector of event onset rows
        eventRows <- which(chartDF$eventLabel!="")
        # get the first and last events for scaling and centering
        firstEvent <- eventRows[!(eventNames %in% excludeEvents)][1]
        lastEvent <- eventRows[!(eventNames %in% excludeEvents)][length(eventRows[!(eventNames %in% excludeEvents)])]
        lastEventEnd <- lastEvent + measuredSeg * cps
        # fix problem when lastEventEnd exceeds the data frame rows for the chart
        if(lastEventEnd > nrow(chartDF)) { lastEventEnd <- nrow(chartDF) }
        
        # set the y offset value for the plot
        # yOffset <- c(130, 75, 10, -45, -110, -145)
        # names(yOffset) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity")
        
        ################  Pneumo signal processing ###################
        
        chartDF <- newPneumoSigProcFn(x=chartDF)
              
        ##################### EDA signal processing #####################
        
        chartDF <- newEDASigProcFn(x-chartDF)
        
        #################### Cardio signal processing #####################
        
        # first ensure that all peaks are recorded on a single sample
        # because there are rare times in which the time series reduction 
        # can result in a peak value over 2 adjacent samples
        # interpolate and eliminate duplicated peak points
        chartDF$c_Cardio1 <- fixPeak(x=chartDF$c_Cardio1)
        
        # scale the cardio data 
        chartDF$c_Cardio1 <- scaleDataFn(chartDF$c_Cardio1, sec=5, times=20, xRange=scaleVals[4], maxY=165, minY=-165)
        
        # offset the cardio data
        chartDF$c_Cardio1 <- offsetDataFn(x=chartDF$c_Cardio1, y=yOffset[4], yMax=165, yMin=-165, firstRow=firstEvent, lastRow=lastEventEnd)
        
        # calculate the cardio rate
        cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=3,peaks="upper",dataRate=cps,lowPass=TRUE)
        # cardioRate <- ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="upper",dataRate=cps)
        # ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="lower",dataRate=cps)
        
        # ts.plot(lowPass1.7hz.2nd(chartDF$c_Cardio1)[1000:1500])
        # ts.plot(chartDF$c_Cardio1[1000:1500])
        
        # calculate the buffer length for the cardio rate
        # bufferLen <- floor(1/cardioRate*60*cps*.60) - 1  # round down
        # bufferLen <- bufferLenFn(x = ratePerMin(lowPass1.7hz.2nd(chartDF$c_Cardio1),buffer=3,peaks="upper",dataRate=cps))
        bufferLen <- bufferLenFn(cardioRate)
        
        ###
        
        # get the max peak row indices
        maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
        
        # ts.plot(lowPass2hz(chartDF$c_Cardio1[1000:3000]))
        # ts.plot(chartDF$c_Cardio1[1000:3000])
        
        # 1/60*60*30/2
        # 1/80*60*30/2
        # 1/100*60*30/2
        
        # get the max peak values
        maxVal <- chartDF$c_Cardio1[maxOut]
        
        # interpolate between max peak values
        systolicInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
        
        # plot.ts(systolicInterp, ylim=c(-3,10))
        # myCardioData2$CardioSyst <- systolicInterp
        # ts.plot(myCardioData2[1:3000,c(1,2,6)])
        
        # add the systolic time series to the data frame
        chartDF$c_CardioSystolic <- systolicInterp
        
        ####
        
        # use a function to get the min peak row indices
        minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
        # minOut <- minPeak(x=chartDF$c_Cardio1, y=4)
        
        # get the min peak values for the min peak rows
        minVal <- chartDF$c_Cardio1[na.omit(minOut)]
        
        # interpolate between the min peak values
        diastolicInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))
        
        # a test plot 
        # plot.ts(diastolicInterp, ylim=c(-3,10))
        
        # add the vector to the diastolic cardio column 
        chartDF$c_CardioDiastolic <- diastolicInterp[1:nrow(chartDF)]
        
        # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        ####
        
        # compute a min-Max of the cardio time series data
        
        # get the minMax row indices
        minMaxOut <- minMaxPeakFn(x=chartDF$c_Cardio1, y=bufferLen)
        
        # get the minMax values
        minMaxVal <- chartDF$c_Cardio1[minMaxOut]
        
        # interpolate the minMax values
        minMaxInterp <- na.omit(c(interpolatePeaks(x=minMaxOut, y=minMaxVal), 0))[1:nrow(chartDF)]
        
        # plot.ts(minMaxInterp, ylim=c(-3,10))
        
        # add the time series to the data frame
        chartDF$c_CardioMinMax <- minMaxInterp
        
        #### 
        
        # compute the smoothed cardio dta
        # smoothedCardio <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
        
        # add the smoothed cardio to the time series data frame
        # chartDF$c_CardioMid <- smoothedCardio[1:nrow(chartDF)]
        chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
        
        # a test plot
        # plot.ts(chartDF$c_CardioMid[1:3000], ylim=c(-3,10))
        # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
        # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
        
        # compute a more stable moving average for the cardio data
        # to help with evaluation of stability of the data
        # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y = round(.5*cps,0), times=12)
        chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMid, y=round(5*cps,0), times=1)
        
        # add the cardio pulse amplitude to the time series data frame
        chartDF$c_CardioAmp <- chartDF$c_CardioSystolic - chartDF$c_CardioDiastolic
        
        ###################### PLE signal processing ####################
        
        if(sum(pmatch(names(examDF), "c_PL", nomatch=0)) != 0) {
          
          inclPLE <- TRUE
          
          chartDF$c_PL <- highPass.338628(chartDF$c_PL)
          
          # ts.plot(chartDF$c_PL)
          # ts.plot(highPass.338628(chartDF$c_PL))
          
          # scale the PLE data 
          chartDF$c_PL <- scaleDataFn(chartDF$c_PL, sec=10, times=20, xRange=scaleVals[5], maxY=165, minY=-165)
          
          # offset the PLE data
          chartDF$c_PL <- offsetDataFn(x=chartDF$c_PL, y=yOffset[5], yMax=165, yMin=-165, firstRow=firstEvent, lastRow=lastEventEnd)
          
          ####
          
          # use a function to get the min peak rows
          minOut <- minPeak(x=chartDF$c_PL, y=8)
          # change to y=12 for slow pulse 
          
          # get the min peak values for the min peak rows
          minVal <- chartDF$c_PL[na.omit(minOut)]
          
          ### 10-6-2015 use the tukey fence to remove 
          
          # keep only those min values that exceed the Tukey lower inner fence
          #         minOut <- tukeyFence1(x=chartDF$c_PL, y=minOut, z=5)
          #         minVal <- chartDF$c_PL[na.omit(minOut)]        
          
          ###
          
          # interpolate between the min peak values
          PLMinInterp <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))
          # plot.ts(diastolicInterp, ylim=c(-3,10))
          
          # add the vector to the diastolic cardio column 
          chartDF$c_PLMin <- PLMinInterp[1:nrow(chartDF)]
          # myCardioData2$Diast <- diastolicInterp[1:nrow(myCardioData)]
          # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
          
          ####
          
          # get the max peak indices
          maxOut <- maxPeak(x=chartDF$c_PL, y=8)
          
          # get the max peak values
          maxVal <- chartDF$c_PL[maxOut]
          
          ### use the tukey fence to remove problems
          
          # keep only those min values that exceed the Tukey lower inner fence
          #         maxOut <- tukeyFence1(x=chartDF$c_PL, y=maxOut, z=5)
          #         maxVal <- chartDF$c_PL[na.omit(maxOut)]
          
          ###
          
          # interpolate between max peak values
          PLMaxInterp <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
          # plot.ts(systolicInterp, ylim=c(-3,10))
          # myCardioData2$CardioSyst <- systolicInterp
          # ts.plot(myCardioData2[1:3000,c(1,2,6)])
          
          # add the systolic time series to the data frame
          chartDF$c_PLMax <- PLMaxInterp
          
          #### 
          
          # compute the smoothed PLE data
          smoothedPL <- MASmooth(x=chartDF$c_PL, y=30, times=4) # y=15, times=3 will show the respiration
          # plot.ts(smoothedCardio[1:3000], ylim=c(-3,10))
          
          # add the smoothed PLE to the time series data frame
          chartDF$c_PLMA <- smoothedPL[1:nrow(chartDF)]
          # myCardioData2$CardioMA <- smoothedCardio[1:nrow(myCardioData)]
          # ts.plot(myCardioData2[1:3000,c(1,2,6, 7)])
          
          # add the PLE pulse amplitude data to the daeta frame
          chartDF$c_PLAmp <- chartDF$c_PLMax - chartDF$c_PLMin
          
        } else inclPLE <- FALSE  # end if for PLE
        
        ###################### Activity sensor signal processing ################
      
          # check to see if activity sensor data are present in the examDF
        if(sum(pmatch(names(examDF), "c_SE", nomatch=0))!=0) {
          
          inclActivity <- TRUE
          
          # first make sure that all peaks are recorded on a single sample
          
          chartDF$c_SE <- fixPeak(x=chartDF$c_SE)
          
          # scale the activity sensor data 
          chartDF$c_SE <- scaleDataFn(chartDF$c_SE, sec=12, times=20, xRange=scaleVals[6], maxY=165, minY=-165)
          
          # offset the activity sensor data
          chartDF$c_SE <- offsetDataFn(x=chartDF$c_SE, y=yOffset[6], yMax=165, yMin=-165, firstRow=firstEvent, lastRow=lastEventEnd)
          
          myData <- chartDF$c_SE
          
          #         # use a helper functon to make a vector of slope values
          #         mySlope <- slopeDir(x=myData)  
          #         
          #         # use a helper function to smooth the slope by removing slope changes of small duration
          #         mySlope1 <- smoothSlope(x=mySlope, n=1)
          #         
          #         # fill the zero slope segments
          #         mySlope2 <- fillSlope(x=mySlope1)
          #         
          #         mySlope3 <- positiveSlope(x=mySlope2)
          #         
          #         # locate the indices of all positive slope onset rows
          #         exhMin <- positiveOnset(x=mySlope3)
          #         
          #         exhMin[1] <- 1
          #         exhMin[length(myData)] <- 1
          #         
          #         exhMinVal <- myData[which(exhMin!=0)] 
          #         
          #         # interpolate between the exhalation min indices
          #         exhMin2 <- interpolatePeaks(x=which(exhMin!=0), y=exhMinVal)
          
          chartDF$c_SEMin <- interpolatePeaks(x=minPeak(x=myData, y=40), y=myData[minPeak(x=myData, y=40)])
          
          ###
          
          # locate the idices of all nebaetive slope onset rows
          #         exhMax2 <- interpolatePeaks(x=maxPeak(x=myData, y=40), y=myData[maxPeak(x=myData, y=40)])
          
          chartDF$c_SEMax <- interpolatePeaks(x=maxPeak(x=myData, y=40), y=myData[maxPeak(x=myData, y=40)])
          
          # process the min cycle
          
          # compute the inhalation max for each respiration cycle
          
          ###
          
          # compute the moving average of the activity sensor data
          chartDF$c_SEMA <- MASmooth(x=myData, y=150, times=8)
          # plot.ts(chartDF$c_SEMA)
          # plot.ts(chartDF$c_SE)
          
        } else inclActivity <- FALSE # end if for activity sensor data
        
        
        
        
        
        
        
        ############## offset the time series data for plotting #################
        
        # get the offset values for the first event for all data vectors
        
        # UPneumoOffset <- chartDF$c_UPneumo[firstEvent]
        # LPneumoOffset <- chartDF$c_LPneumo[firstEvent]
        
        # AutoEDAOffset <- chartDF$c_AutoEDA[firstEvent]
        # Cardio1Offset <- chartDF$c_CardioMid[firstEvent]
        # if(inclPLE==TRUE) PLOffset <- chartDF$c_PLMA[firstEvent]
        # if(inclActivity==TRUE) ActivityOffset <- chartDF$c_SEMA[firstEvent]
        # 
        # reset the offset values
        
        # pneumo offset
        # chartDF$c_UPneumo <- chartDF$c_UPneumo - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumo <- chartDF$c_LPneumo - LPneumoOffset + yOffset[2]
        
        
        # chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid - LPneumoOffset + yOffset[2]
        # chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh - LPneumoOffset + yOffset[2]
        # chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh - UPneumoOffset + yOffset[1]
        # chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh - LPneumoOffset + yOffset[2]
        
        # EDA offset
        # chartDF$c_AutoEDA <- chartDF$c_AutoEDA - AutoEDAOffset + yOffset[3]
        # chartDF$c_AutoEDAMid <- chartDF$c_AutoEDAMid - AutoEDAOffset + yOffset[3]
        # chartDF$c_AutoEDAPeak <- chartDF$c_AutoEDAPeak - AutoEDAOffset + yOffset[3]
        # chartDF$c_AutoEDABase <- chartDF$c_AutoEDABase - AutoEDAOffset + yOffset[3]
        
        # cardio offset
        # chartDF$c_Cardio1 <- chartDF$c_Cardio1 - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioMid <- chartDF$c_CardioMid - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioMA <- chartDF$c_CardioMA - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic - Cardio1Offset + yOffset[4]
        # chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic - Cardio1Offset + yOffset[4]
        
        # PLE offset
        # if(inclPLE==TRUE) {
        #   chartDF$c_PL <- chartDF$c_PL - PLOffset + yOffset[5]
        #   chartDF$c_PLMA <- chartDF$c_PLMA - PLOffset + yOffset[5]
        #   chartDF$c_PLMax <- chartDF$c_PLMax - PLOffset + yOffset[5]
        #   chartDF$c_PLMin <- chartDF$c_PLMin - PLOffset + yOffset[5]
        # } # end if for PLE
        
        # activity offset
        # if(inclActivity==TRUE) {
        #   chartDF$c_SE <- chartDF$c_SE - ActivityOffset + yOffset[6]
        #   chartDF$c_SEMA <- chartDF$c_SEMA - ActivityOffset + yOffset[6]
        #   chartDF$c_SEMin <- chartDF$c_SEMin - ActivityOffset + yOffset[6]
        #   chartDF$c_SEMax <- chartDF$c_SEMax - ActivityOffset + yOffset[6]
        # }
        
        ###########################################
        
        # save the chartDF to the seriesDF
        # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
        examDF[(chartOnsetRow+seriesOnsetRow-1):(chartEndRow+seriesOnsetRow-1),]  <- chartDF

      } # end for loop over each k chart in each series
      
      # save the seriesDF to the examDF
      # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
      
    } # end loop over j unique series

    # save the examDF to the global environment
    assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) return(examDF) 
  
} # end sigProc function

####

# sigProc(x=uniqueExams, output=FALSE, showNames=TRUE)



