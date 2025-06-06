# archived copies of the scaleDataFn and offsetDataFn
# 1-25-2019



ScaleOffsetDataFn <- function(x=uniqueExams, 
                              makeDF=TRUE, 
                              showNames=TRUE, 
                              output=FALSE) {
  # function to process the NCCA ASCII time series data
  # including pneumo, EDA, cardio, PLE and activity sensors
  # 5/30/2016
  # Raymond Nelson
  #
  # this function will call the scaleDataFn() and offsetDataFn() from the sigProcHelper.R script
  # x input is a vector of unique exam names in the working directory
  # loop over the time series data frames from the global environment
  # the time series data are after signal processing (AutoEDA, pneumo smoothing, etc.)
  # and after feature extraction
  # showNames=TRUE will print the exam names, series names, and chart names to the console
  
  # data frames are saved to the global environment as a side effect
  
  # output=TRUE will output the time series data frame for the last exam in the input vector of exam names
  #
  ###
  
  uniqueExams <- x
  
  pneumoScale=TRUE
  EDAScale=TRUE
  cardioScale=TRUE
  PLEScale=TRUE
  activityScale=TRUE
  
  # loop over each exam in the list 
  i=1
  for(i in 1:length(uniqueExams)) {
    examName <- uniqueExams[i]
    searchString <- paste0("*", examName, "_Data", "*")
    examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
    
    examOnsetRow <- 1
    examEndRow <- nrow(examDF)
    
    # for testing
    assign("examDF", examDF, pos=1)
    assign("examName", examName, pos=1)
    
    if(showNames==TRUE) print(examName)
    
    # choose the EDA filter
    if(exists("uniqueExamNames")) {
      if(grepl("^D&.*$", uniqueExamNames[i])) {
        # lafayette
        # EDAFilter <- "laf" 
        EDAFilter <- EDAFilt
      } else if(grepl("^D\\$.*$", uniqueExamNames[i])) {
        # axciton
        EDAFilter <- "none"
      } else if(grepl("^D#.*$", uniqueExamNames[i])) {
        # Stoelting
        EDAFilter <- "none"
      } else if(grepl("^D%.*$", uniqueExamNames[i])) {
        # Limestone
        EDAFilter <- "none"
      } else EDAFilter <- EDAFilt # variable in the global environment
    } else {
      EDAFilter <- EDAFilt # variable in the global environment
    }
    
    # get the names of each unique series in the exam
    uniqueSeries <- as.character(unique(examDF$seriesName))
    
    # loop over each unique series
    j=1
    for(j in 1:length(uniqueSeries)) {
      seriesName <- uniqueSeries[j]
      
      seriesDF <- examDF[examDF$seriesName==seriesName,]
      
      seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
      seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
      
      # for testing
      assign("seriesDF", seriesDF, pos=1)
      assign("seriesName", seriesName, pos=1)
      
      if(showNames==TRUE) print(paste("series", uniqueSeries[j]))
      
      uniqueCharts <- as.character(unique(seriesDF$chartName))
      
      # loop over each chart in the series 
      k=1
      for(k in 1:length(uniqueCharts)) {
        chartName <- uniqueCharts[k]
        
        chartDF <- seriesDF[seriesDF$chartName==chartName,]
        
        # skip charts less than 15 seconds
        if(nrow(chartDF)<900) next()
        
        chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
        chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
        
        # for testing
        assign("chartDF", chartDF, pos=1)
        assign("chartName", chartName, pos=1)
        
        if(showNames==TRUE) print(uniqueCharts[k])
        
        ####################   get the first and last events   #####################
        
        # call the eventyNamesFn from the sigProcHelper.R script
        # firstLastEvents <- eventNamesFn(x=chartDF)
        firstLastEvents <- getFirstLastEventFn(x=chartDF)
        firstEvent <- firstLastEvents[1]
        lastEventEnd <- firstLastEvents[2]
        assign("firstLastEvents", firstLastEvents, pos=1)
        
        ####################################################
        
        # scaleVal and newScaleVal are variables in the global environment,
        # created by the scaleDataFn() and offsetDataFn()
        
				#################  Pneumo ###################
        
        if(isTRUE(pneumoScale)) {
          
          if(showNames==TRUE) print("  pneumo scale and offset - upper and lower")
          
          # scale the pneumo data 
          chartDF$c_UPneumoSm <- scaleDataFn(x=chartDF$c_UPneumoSm, sec=18, times=20, ignore=2, xRange=scaleVals['uPneumo'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          chartDF$c_LPneumoSm <- scaleDataFn(x=chartDF$c_LPneumoSm, sec=18, times=20, ignore=2, xRange=scaleVals['lPneumo'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          # offset the pneumo data
          chartDF$c_UPneumoSm <- offsetDataFn(x=chartDF$c_UPneumoSm, y=yOffset['uPneumo'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          chartDF$c_LPneumoSm <- offsetDataFn(x=chartDF$c_LPneumoSm, y=yOffset['lPneumo'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          ### make sure the pneumo channels are adequately separated at the first event
          if( (chartDF$c_UPneumoSm[firstEvent] - chartDF$c_LPneumoSm[firstEvent]) < (.15 * yRange)) {
            # calculate the lowerOffset
            lowerOffset1 <- (chartDF$c_UPneumoSm[firstEvent] - (.15 * yRange)) - chartDF$c_LPneumoSm[firstEvent]
            # then add the offset value to the vector
            chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm + lowerOffset1
          }
          
          ### make sure the pneumo channels are adequately separated at the last event
          if( (chartDF$c_UPneumoSm[lastEventEnd] - chartDF$c_LPneumoSm[lastEventEnd]) < (.15*yRange)) {
            # calculate the lowerOffset
            lowerOffset2 <- (chartDF$c_UPneumoSm[lastEventEnd] - (.15*yRange)) - chartDF$c_LPneumoSm[lastEventEnd]
            # add the second offset to the vector
            chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm + lowerOffset2
          }
          
          # compute the additional pneumo channels
          chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumoSm, y=round(3*cps,0), times=5) # was y=40, times=8 11/19/2015
          chartDF$c_LPneumoMid <- MASmooth(x=chartDF$c_LPneumoSm, y=round(3*cps,0), times=5)
          
          chartDF$c_UPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0)),
                                                   y=chartDF$c_UPneumoSm[maxPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0))])
          chartDF$c_UPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0)),
                                                   y=chartDF$c_UPneumoSm[minPeak(x=chartDF$c_UPneumoSm, y=round(1.333*cps,0))])
          
          chartDF$c_LPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0)),
                                                   y=chartDF$c_LPneumoSm[maxPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0))])
          chartDF$c_LPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0)),
                                                   y=chartDF$c_LPneumoSm[minPeak(x=chartDF$c_LPneumoSm, y=round(1.333*cps,0))])
          
        }
        
        ##################### EDA #####################
        
        if(isTRUE(EDAScale)) {
          
          if(showNames==TRUE) print("  EDA scale and offset")
          
          # first source the sigProcHelper.R script to load the scaleDataFn and offsetDataFn
          
          # 8-7-2017 for easier testing of the EDA filters
          # normally done by the sigProcFn in the sigProc.R script
          # chartDF <- EDASigProcFn(x=chartDF,
          #                         EDAHigh=TRUE,
          #                         EDALow=TRUE,
          #                         EDAFilter=EDAFilter )
          
          # scale the EDA data
          chartDF$c_AutoEDA <- scaleDataFn(x=chartDF$c_AutoEDA, sec=40, times=20, ignore=2, xRange=scaleVals['eda'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          # offset the EDA data
          chartDF$c_AutoEDA <- offsetDataFn(x=chartDF$c_AutoEDA, y=yOffset['eda'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          # scale and offset the additional EDA channels
          chartDF$c_AutoEDAMid <- MASmooth(x=chartDF$c_AutoEDA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
          chartDF$c_AutoEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0)),
                                                    y=chartDF$c_AutoEDA[maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0))])
          chartDF$c_AutoEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_AutoEDA, y=round(1.5*cps,0)),
                                                    y=chartDF$c_AutoEDA[minPeak(x=chartDF$c_AutoEDA, y=round(1.5*cps,0))])
          
          ###
          
          # scale the EDA data
          chartDF$c_ManualEDA <- scaleDataFn(x=chartDF$c_ManualEDA, sec=40, times=20, ignore=2, xRange=scaleVals['eda'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          # offset the EDA data
          chartDF$c_ManualEDA <- offsetDataFn(x=chartDF$c_ManualEDA, y=(yOffset['eda']-250), maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          # scale and offset the additional EDA channels
          chartDF$c_ManualEDAMid <- MASmooth(x=chartDF$c_ManualEDA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
          chartDF$c_ManualEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_ManualEDA, y=round(1*cps,0)),
                                                      y=chartDF$c_ManualEDA[maxPeak(x=chartDF$c_ManualEDA, y=round(1*cps,0))])
          chartDF$c_ManualEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_ManualEDA, y=round(1.5*cps,0)),
                                                      y=chartDF$c_ManualEDA[minPeak(x=chartDF$c_ManualEDA, y=round(1.5*cps,0))])
          
          ###
           
          if( "c_EA" %in% colnames(chartDF) &
              length(which(is.na(chartDF$c_EA))) != nrow(chartDF) ) {
            
            # scale the EDA data
            chartDF$c_EA <- scaleDataFn(x=chartDF$c_EA, sec=40, times=20, ignore=2, xRange=scaleVals['eda'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # offset the EDA data
            chartDF$c_EA <- offsetDataFn(x=chartDF$c_EA, y=(yOffset['eda']-250), maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # scale and offset the additional EDA channels
            # chartDF$c_EAMid <- MASmooth(x=chartDF$c_EA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
            # chartDF$c_EAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_EA, y=round(1*cps,0)),
            #                                      y=chartDF$c_EA[maxPeak(x=chartDF$c_EA, y=round(1*cps,0))])
            # chartDF$c_EABase <- interpolatePeaks(x=minPeak(x=chartDF$c_EA, y=round(1.5*cps,0)),
            #                                      y=chartDF$c_EA[minPeak(x=chartDF$c_EA, y=round(1.5*cps,0))])
            
          }
          
          ### also scale and offset the filtered EDA data
          
          # # scale the filtered EDA data
          # chartDF$c_EDAFilt <- scaleDataFn(x=chartDF$c_EDAFilt, sec=40, times=10, ignore=2, xRange=scaleVals[3], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          # 
          # # offset the filtered EDA data
          # chartDF$c_EDAFilt <- offsetDataFn(x=chartDF$c_EDAFilt, y=yOffset[3], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          # 
          # # scale and offset the additional filtered EDA channels
          # chartDF$c_EDAFiltMid <- MASmooth(x=chartDF$c_EDAFilt, y=round(5*cps,0), times=6)
          # chartDF$c_EDAFiltMax <- interpolatePeaks(x=maxPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0)),
          #                                          y=chartDF$c_EDAFilt[maxPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0))])
          # chartDF$c_EDAFiltMin <- interpolatePeaks(x=minPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0)),
          #                                          y=chartDF$c_EDAFilt[minPeak(x=chartDF$c_EDAFilt, y=round(1*cps,0))])
          # 
          # # compute the difference between Max and Min
          # chartDF$c_EDAFiltDiff <- chartDF$c_EDAFiltMax - chartDF$c_EDAFiltMin
          
        } 
        
        #################### Cardio #####################
        
        if(isTRUE(cardioScale)) {
          
          if(showNames==TRUE) print("  cardio scale and offset")
          
          # make sure that every cardio peak exists on a single sample
          # this was also done in the cardioSigProc function and is done again here
          # for additional peak correction
          chartDF$c_Cardio1 <- fixPeak(x=chartDF$c_Cardio1, times=2)
          
          # scale the cardio data 
          chartDF$c_Cardio1 <- scaleDataFn(chartDF$c_Cardio1, sec=5, times=50, ignore=30, xRange=scaleVals['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          # offset the cardio data
          chartDF$c_Cardio1 <- offsetDataFn(x=chartDF$c_Cardio1, y=yOffset['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          # scale and offset the additional cardio channels
          
          # compute the cardio mid line 
          chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
          
          # then compute a slower moving average
          # was 1.5 sec and times=2 3/17/2017
          chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=3)
          # and smooth it some more
          # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(1*cps,0), times=1)
          
          # calculate the cardio rate and buffer length
          # was buffer=3 4-14-2017
          # commented out 4-25-2017 for a better version
          # cardioRate <- ratePerMin(chartDF$c_Cardio1,buffer=9,peaks="upper",lowPass=TRUE)
          # bufferLen <- bufferLenFn(x=cardioRate, y=.6)
          # 
          # # locate the systolic peaks
          # maxOut <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
          # 
          # # compute the systolic line
          # chartDF$c_CardioSystolic <- interpolatePeaks(x=maxOut, y=chartDF$c_Cardio1[maxOut])[1:nrow(chartDF)]
          # 
          # # locate the diastolic peaks
          # minOut <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
          # 
          # # compute the diastolic line
          # chartDF$c_CardioDiastolic <- interpolatePeaks(x=minOut, y=chartDF$c_Cardio1[minOut])[1:nrow(chartDF)]
          
          
          # compute the cardio rate to get the buffer to get the peaks
          cardioRate <- ratePerMin(chartDF$c_Cardio1,
                                   buffer=9,
                                   peaks="upper",
                                   lowPass=TRUE)
          bufferLen <- bufferLenFn(x=cardioRate, y=.6)
          
          
          
          # locate the systolic peaks
          systPeaks <- maxPeak(x=chartDF$c_Cardio1, y=bufferLen)
          # compute the systolic line
          chartDF$c_CardioSystolic <- 
            interpolatePeaks(x=systPeaks, 
                             y=chartDF$c_Cardio1[systPeaks])[1:nrow(chartDF)]
          
          # locate the diastolic peaks
          diastPeaks <- minPeak(x=chartDF$c_Cardio1, y=bufferLen)
          # compute the diastolic line
          chartDF$c_CardioDiastolic <- 
            interpolatePeaks(x=diastPeaks, 
                             y=chartDF$c_Cardio1[diastPeaks])[1:nrow(chartDF)]
          
          # add the systolic and diastolic peaks to the data from
          # to use for peak to peak information processing
          chartDF$CardioPeak[systPeaks] <- "systolic"
          chartDF$CardioPeak[diastPeaks] <- "diastolic"
          
          # compute the beat to beat intervals for the systPeaks
          # (60 / (diff(systPeaks) / 30))
          chartDF$c_cardioRateSystolic <- 
            interpolatePeaks(x=systPeaks, 
                             y=(60 / (diff(systPeaks) / 30)))[1:nrow(chartDF)]
          
          # commented out 5/5/2018 RN not sure what it is used for
          # # scale the cardio rate
          # chartDF$c_cardioRateSystolic <- 
          #   scaleDataFn(chartDF$c_cardioRateSystolic, sec=5, times=30, ignore=20, xRange=scaleVals['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          # 
          # # offset the cardio rate
          # chartDF$c_cardioRateSystolic <-
          #   offsetDataFn(x=chartDF$c_cardioRateSystolic, y=yOffset['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
          
          #### electronic cardio cuff ####
          
          # check to see if electronic cardio data are present in the exam data frame
          if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0)) != 0) {
            
            if(showNames==TRUE) print("  electronic cardio scale and offset")
            
            # scale the electronic cardio cuff data
            chartDF$c_eCardio <- scaleDataFn(chartDF$c_eCardio, sec=5, times=40, ignore=1, xRange=scaleVals['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # offset the electronic cardio cuff data
            chartDF$c_eCardio <- offsetDataFn(x=chartDF$c_eCardio, y=yOffset['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # scale and offset the additional electronic cardio cuff channels
            chartDF$c_eCardioMid <- MASmooth(x=chartDF$c_eCardio, y=round(.5*cps,0), times=3)
            chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardio, y=round(1.5*cps,0), times=2)
            chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardioMA, y=round(.5*cps,0), times=2)
            
            cardioRate <- ratePerMin(chartDF$c_eCardio,buffer=3,peaks="upper",lowPass=TRUE)
            bufferLen <- bufferLenFn(cardioRate)
            
            maxOut <- maxPeak(x=chartDF$c_eCardio, y=bufferLen)
            maxVal <- chartDF$c_eCardio[maxOut]
            chartDF$c_eCardioSystolic <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
            minOut <- minPeak(x=chartDF$c_eCardio, y=bufferLen)
            minVal <- chartDF$c_eCardio[na.omit(minOut)]
            chartDF$c_eCardioDiastolic <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))[1:nrow(chartDF)]
            
          } # end if for electronic cardio
          
          #### finger cuff ####
          
          # check to see if PLE data are present in the exam data frame
          if(sum(pmatch(names(examDF), "c_FC", nomatch=0)) != 0) {
            
            if(showNames==TRUE) print("  finger cuff scale and offset")
            
            # scale the finger cuff data
            chartDF$c_FC <- scaleDataFn(chartDF$c_FC, sec=5, times=40, ignore=1, xRange=scaleVals['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # offset the finger cuff data
            chartDF$c_FC <- offsetDataFn(x=chartDF$c_FC, y=yOffset['cardio'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # scale and offset the additional finger cuff channels
            chartDF$c_FCMid <- MASmooth(x=chartDF$c_FC, y=round(.5*cps,0), times=3)
            chartDF$c_FCMA <- MASmooth(x=chartDF$c_FC, y=round(1.5*cps,0), times=2)
            chartDF$c_FCMA <- MASmooth(x=chartDF$c_FCMA, y=round(.5*cps,0), times=2)
            
            cardioRate <- ratePerMin(chartDF$c_FC,buffer=3,peaks="upper",lowPass=TRUE)
            bufferLen <- bufferLenFn(cardioRate)
            
            maxOut <- maxPeak(x=chartDF$c_FC, y=bufferLen)
            maxVal <- chartDF$c_FC[maxOut]
            chartDF$c_FCSystolic <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
            minOut <- minPeak(x=chartDF$c_FC, y=bufferLen)
            minVal <- chartDF$c_FC[na.omit(minOut)]
            chartDF$c_FCDiastolic <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))[1:nrow(chartDF)]
            
          } # end if for finger cuff
          
        }
        
        ###################### PLE ####################
        
        if(isTRUE(PLEScale)) {
          
          # check to see if PLE data are present in the exam data frame
          if(sum(pmatch(names(examDF), "c_PL", nomatch=0)) != 0) {
            
            # make sure that every PLE peak exists on a single sample
            # this was also done in the PLESigProc function and is done again here
            # for additional peak correction
            chartDF$c_PL <- fixPeak(x=chartDF$c_PL, times=2)
            
            if(showNames==TRUE) print("  PLE scale and offset")
            
            # scale the PLE data 
            chartDF$c_PL <- scaleDataFn(chartDF$c_PL, sec=10, times=40, ignore=4, xRange=scaleVals['ple'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # offset the PLE data
            chartDF$c_PL <- offsetDataFn(x=chartDF$c_PL, y=yOffset['ple'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # scale and offset the additional PLE channels
            chartDF$c_PLMA <- MASmooth(x=chartDF$c_PL, y=30, times=4) # y=15, times=3 will show the respiration
            # chartDF$c_PLMA <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
            
            # compute the cardio rate to get the buffer to get the peaks
            # 7-29-2017 to fix the bd systolic line in PLE data
            cardioRate <- ratePerMin(chartDF$c_PL,buffer=9,peaks="upper",lowPass=TRUE)
            bufferLen <- bufferLenFn(cardioRate, y=.75)
            # cardioRate <- ratePerMin(chartDF$c_PL,buffer=3,peaks="upper",lowPass=TRUE)
            # bufferLen <- bufferLenFn(cardioRate)
            
            # get the systolic peaks
            maxOut <- maxPeak(x=chartDF$c_PL, y=bufferLen)
            # interpolate between systolic peaks
            chartDF$c_PLMax <- interpolatePeaks(x=maxOut, y=chartDF$c_PL[maxOut])[1:nrow(chartDF)]
            # get the diastolic peaks
            minOut <- minPeak(x=chartDF$c_PL, y=bufferLen)
            # interpolate between diastolic peaks
            chartDF$c_PLMin <- interpolatePeaks(x=minOut, y=chartDF$c_PL[minOut])[1:nrow(chartDF)]
            
          } # end if for PLE
          
        } # end if for extant PLE sensor data
        
        ###################### Activity sensor ################
        
        if(isTRUE(activityScale)) {
          
          # check to see if activity sensor data are present in the examDF
          if(sum(pmatch(names(examDF), "c_SE", nomatch=0))!=0) {
            
            if(showNames==TRUE) print("  activity scale and offset")
            
            # scale and offset the activity sensor data 
            chartDF$c_SE <- scaleDataFn(x=chartDF$c_SE, sec=12, times=20, xRange=scaleVals['activity'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            chartDF$c_SE <- offsetDataFn(x=chartDF$c_SE, y=yOffset['activity'], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # scale and offset the additions activity channels
            chartDF$c_SEMA <- MASmooth(x=chartDF$c_SE, y=round(3*cps,0), times=5)
            
            # scale and offset the processed activity sensor data
            # chartDF$c_SEProc <- scaleDataFn(chartDF$c_SEProc, sec=12, times=20, xRange=scaleVals[6], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            # chartDF$c_SEProc <- offsetDataFn(x=chartDF$c_SEProc, y=(yOffset[6]), yMax=yMax, yMin=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=lastEventEnd)
            
            # make a new processed activity data
            chartDF$c_SEProc <- MASmooth(x=chartDF$c_SE, y=round(.0625*cps,0), times=1)
            chartDF$c_SEProcMA <- MASmooth(x=chartDF$c_SEProc, y=round(3*cps,0), times=5)
            
            # scale and offset the additions activity channels
            cardioRate <- ratePerMin(chartDF$c_SEProc,buffer=3,peaks="upper",lowPass=TRUE)
            bufferLen <- bufferLenFn(cardioRate)
            
            maxOut <- maxPeak(x=chartDF$c_SEProc, y=bufferLen)
            chartDF$c_SEMax <- interpolatePeaks(x=maxOut, y=chartDF$c_SEProc[maxOut])[1:nrow(chartDF)]
            minOut <- minPeak(x=chartDF$c_SEProc, y=bufferLen)
            chartDF$c_SEMin <- interpolatePeaks(x=minOut, y=chartDF$c_SEProc[minOut])[1:nrow(chartDF)]
            
            # commented out 4-25-2017 not sure if this is needed or used now
            # chartDF$c_SEResult <- (chartDF$c_SEResult - chartDF$c_SEResult[1]) * (scaleVal * newScaleVal) + chartDF$c_SEProc[1] - (.05*yRange)
            
          } else print("  WARNING: missing activity sensor data") # end if for activity sensor data
          
        } # end if for extant activity sensor data
        
        ##############################################################
        
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
    if(makeDF==TRUE) assign(paste0(examName, "_Data"), examDF, pos=1)

  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) { return(examDF) } else {
    return(uniqueExams)
  }
  
} # end ScaleOffsetDataFn()


# call the function
# ScaleOffsetDataFn(x=uniqueExams, output=FALSE, showNames=TRUE)



scaleDataFn <- function(x=chartDF$c_UPneumoSm, sec=6, times=20, ignore=2, xRange=(.1*yRange), maxY=(yMax-.05*yRange), minY=(yMin+.05*yRange),  firstRow=firstEvent, lastRow=lastEventEnd) {
  # helper function to scale the time series data 
  # this function is located in the sigProcHelper.R script
  # 4-17-2016
  # Raymond Nelson
  # this is called by the ScaleOffsetDataFn() function
  ### input
  # x input is the time series vector for a a single time series channel,
  #  from a single chart
  # sec is the number of seconds to sample
  # times is the number of random samples to get
  # ignore is an integer that specifies the number of largest samples,
  # to ignore in the scaling calcluation
  # xRange is the range to scale the data within
  # maxY is the max y value for the data on the printed chart
  # minY is the min y value for the data on the printed chart
  # firstRow is the index of the onset of the first stimulus event
  # lastRow is the endex of the end of the scoring window for the last stimulus event
  ### output
  # output is a the rescaled time series vector vector
  # also assigns a scalar scaleVal to the global environment,
  # for use when scaling the additional channels
  ##########
  dataLength <- length(x)
  # exit if the vector is NA
  if(length(which(is.na(x)))==length(x)) return(x)
  # x[which(!is.na(x))]
  # chartDF$c_SE[which(!is.na(chartDF$c_SE))]
  # first set the number of indices to include in each sample
  sampleLength <- sec * cps 
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength-sampleLength + 1
  # calculate the scaled data
  if(dataLength <= (sec * 2 * cps)) {
    # handle short charts less than 2 seconds
    if(max(x) == min(x)) {
      dataRange <- 1
    } else {
      dataRange <- max(x) - min(x)
    } # end else
    scaleVal <- xRange / dataRange
    # initialize the default rescale value for this if condition
    rescaleVal <- 1
    xOut <- x * scaleVal
  } else {
    # for charts > 2 seconds
    # sample the data
    # get the sample onset row indices using random numbers
    # use an if else condition for short charts
    if(firstRow < (lastRow-sampleLength)) {
      # get the sample onset indices
      sampleOnset <- sample(c(firstRow:(lastRow-sampleLength)), size=times, replace=TRUE)
      # then get the sample offset indices
      sampleOffset <- sampleOnset + sampleLength - 1
      # make a data frame for the sample segments
      DF <- as.data.frame(matrix(NA, nrow=times, ncol=sampleLength))
      # each row is a sample from the time series input 
      # populate the data frame
      for (i in 1:times) {
        DF[i,] <- x[sampleOnset[i]:sampleOffset[i]]
      }
      # View(DF)
      # make a function to get the range from the DF rows
      dfRangeFn <- function(x) { max(x)-min(x) }
      # apply the range function to each row of the DF to get the range of each row
      # apply is vectorized and needs no loop
      dfRange <- apply(DF, 1, dfRangeFn)
      # sort and remove the largest changes using the ignore parameter 
      dfRange <- sort(dfRange, decreasing = TRUE)[(ignore+1):length(dfRange)]
      # get the scaling value from the dfRange
      # summary(dfRange)
      if(median(dfRange)!=0) {
        scaleVal <- xRange/median(dfRange)
      } else {
        scaleVal <- 1
      } # end else
    } else { 
      # another condition for short charts
      if(max(x[firstRow:lastRow]) != min(x[firstRow:lastRow])) {
        scaleVal <- xRange / abs( max(x[firstRow:lastRow]) - min(x[firstRow:lastRow]) )
      } else scaleVal <- 1
    } # end else
    # scale the input vector
    xOut <- x * scaleVal
    # get the offset max value and offset the data if necessary
    offsetVal <- maxY - max(xOut[firstRow:lastRow])
    # 12-31-2016 change this to + offsetVal
    if(offsetVal < 0) { xOut <- xOut + offsetVal }
    # get the offset min value and offset the data if necessary
    offsetVal <- minY - min(xOut[firstRow:lastRow])
    if(offsetVal > 0) { xOut <- xOut + offsetVal }
    # check the range 
    newRange <- max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow])
    # initialize the default rescale value for this else condition
    rescaleVal <- 1
    # rescale the data if the range exceeds the maxY and minY values
    if(newRange > (maxY - (minY + (.4 * yRange)))) { 
      rescaleVal <- (maxY - (minY + (.4 * yRange))) / newRange
      xOut <- xOut * rescaleVal
    } # end if
  } # end else
  # assign the scale value to the global environment
  # the scale value needs to be used to scale additional channels for each sensor
  assign("scaleVal", (scaleVal * rescaleVal), pos=1)
  return(xOut)
} # end scaleDataFn()  


#####


offsetDataFn <- function(x=chartDF$c_Cardio1, y=0, maxY=(yMax-.05*yRange), minY=(yMin+.05*yRange), firstRow=firstEvent, lastRow=lastEventEnd) {
  # function to offset the time series data for plotting
  # 4-17-2016
  # Raymond Nelson
  ###
  # x input is the time series vector
  # y is the offset value to locate the onset of the first event
  # yMax is the maximum y value to display the data (less than the max y index on the chart)
  # yMin is the minimum y value to display the data (more than the min y index on the chart)
  # firstRow is the row index of the onset of the first stimulus event in the time series data
  # lastRow is the row index of the end of evaluation window for the last stimulus event in the time series data 
  # firstRow and lastRow should ignore the X and XX announcements
  # output is the offset time series data
  # also places 2 scalars newScaleVal and offsetVal in the global environment
  #####
  dataLength <- length(x)
  # exit if NA
  if(length(which(is.na(x)))==length(x)) return(x)
  # continue
  if(is.null(firstRow)) firstRow <- 1
  if(is.null(lastRow)) lastRow <- dataLength
  newOffsetAdjust <- 0
  # set the new scale value
  newScaleVal <- 1
  # offset the data
  if(dataLength > (20*cps)) {
    # get the offset value for the first event
    offsetVal <- x[firstRow]
    # compute the offset adjustment
    offsetAdjust <- y - offsetVal
    # offset the data
    xOut <- x + offsetAdjust
    # set the new scale value
    # newScaleVal <- 1
    # check to ensure that the data to not exceed the difference between yMax and yMin
    if(max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]) > yMax-yMin) {
      # compute a new scale value from the difference between max and min values
      newScaleVal <- (maxY-minY) / (max(xOut[firstRow:lastRow]) - min(xOut[firstRow:lastRow]))
      # rescale the data
      xOut <- xOut * newScaleVal
    } # end if
    # check to ensure that the data do not exceed yMax or yMin
    newOffsetAdjust <- 0
    if(max(xOut[firstRow:lastRow]) > maxY) {
      newOffsetAdjust <- maxY - max(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    } else if(min(xOut[firstRow:lastRow]) < minY) {
      newOffsetAdjust <- minY - min(xOut[firstRow:lastRow])
      xOut <- xOut + newOffsetAdjust
    } # end else if
  } else { 
    # for shart charts less than 20 seconds
    offsetVal <- median(x)
    offsetAdjust <- y - offsetVal
    xOut <- x + offsetVal
  } # end else
  assign("newScaleVal", newScaleVal, pos=1)
  assign("offsetVal", offsetVal + newOffsetAdjust, pos=1)
  return(xOut)
} # end offsetDataFn() function




