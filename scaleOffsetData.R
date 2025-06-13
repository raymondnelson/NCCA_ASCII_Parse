# function to scale and offset the time series data
# including pneumo, EDA, cardio, PLE and activity sensors
# 5/30/2016
# Raymond Nelson

####


# library(stringr)


# getUniqueExams <- function(x="*_Data$") { unique(str_sub(ls(pattern=x, pos=1),1, -6)) }
# uniqueExams <- getUniqueExams(x="*_Data$")
# uniqueExams <- uniqueExams[2]


####


# some input parameters

# these are set in the init file

# measuredSeg <- 15
# cps <- 30
# yMax <- 1000
# yMin <- -1000
# yRange <- yMax - yMin

# these are also set in the init file

# scaleVals 
# names(scaleVals) 
# yOffset 
# names(yOffset) 


####


{
  # load the scaleDataFn() and offsetDataFn()
  source(paste0(RPath, 'sigProcHelper.R'), echo=FALSE)
  
  # digital filters
  source(paste0(RPath, 'sigProc_extra.R'), echo=FALSE)
  
  source(paste0(RPath, "SBPFromPTT.R"), echo=FALSE)
  
}


# x=uniqueExams
# output=FALSE
# showNames=TRUE
# i=1
# j=1
# k=1


# July 31, 2023
# yOffset <- c(725, 475, 100, -375, -740, -880, -400, -400)
# yOffset <- c(725, 475, 100, -375, -740, -880, -400, -400)



# first source the sigProcHelper.R script to load the scaleDataFn and offsetDataFn
# 11/20/2107 these 2 functions are now included at the end of this script


ScaleOffsetDataFn <- function(x=uniqueExams, 
                              makeDF=TRUE, 
                              saveCSV=FALSE,
                              showNames=TRUE, 
                              output=FALSE,
                              saveChartDF=FALSE ) {
  # function to process the NCCA ASCII time series data
  # including pneumo, EDA, cardio, PLE and activity sensors
  # 5/30/2016
  # modified 1/25/2019
  # Raymond Nelson
  #
  # this function will call the scaleDataFn() and offsetDataFn() from the sigProcHelper.R script
  # x input is a vector of unique exam names in the working directory
  # loop over the time series data frames from the global environment
  # the time series data are after signal processing (AutoEDA, pneumo smoothing, etc.)
  # and after feature extraction
  # showNames=TRUE will print the exam names, series names, and chart names to the console
  
  # data frames are saved to the global environment as a side effect
  
  # makeDF=TRUE will save the examDF to the global environment
  # using examName, "_Data"
  # output=TRUE will output the time series data frame for the last exam in the input vector of exam names
  #
  # saveChartDF=TRUE will save the chartDF and chart name to the global envir for inspetion 2024Jul29
  #
  ####

  # set.seed(1234567890)
  
  uniqueExams <- x
  
  {
    if(!exists("makeDF")) makeDF=TRUE
    if(!exists("saveCSV")) saveCSV=FALSE
    if(!exists("showNames")) showNames=TRUE
    if(!exists("output")) output=FALSE
    if(!exists("saveChartDF")) saveChartDF=FALSE
  }
  
  {
    
    pneumoScale=TRUE
    EDAScale=TRUE
    cardioScale=TRUE
    PLEScale=TRUE
    activityScale=TRUE
    PTTScale=TRUE
    
  }
  
  # loop over each exam in the list 
  i=1
  for(i in 1:length(uniqueExams)) {
    
    {
      
      examName <- uniqueExams[i]
      searchString <- paste0("*", examName, "_Data", "*")
      examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
      
      # View(examDF)
      
      examOnsetRow <- 1
      examEndRow <- nrow(examDF)
      
      # for testing
      # assign("examDF", examDF, pos=1)
      # assign("examName", examName, pos=1)
      
      if(showNames==TRUE) print(paste("exam", i, examName))
      
      # get the names of each unique series in the exam
      uniqueSeries <- as.character(unique(examDF$seriesName))
      
    }
    
    # loop over each unique series
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
      
      # loop over each chart in the series 
      k=1
      for(k in 1:length(uniqueCharts)) {
        
        {
          
          chartName <- uniqueCharts[k]
          
          chartDF <- seriesDF[seriesDF$chartName==chartName,]
          
          # View(chartDF)
          
          # skip charts less than 45 seconds
          if(nrow(chartDF)<1350) next()
          # July 16 2023 was 900 (30 seconds)
          
          chartOnsetRow <- which(seriesDF$chartName==chartName)[1]
          chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
          
          # for testing 2024Jul29
          if(saveChartDF) {
            assign("chartDF", chartDF, pos=1)
            assign("chartName", chartName, pos=1)
          }
          
          if(showNames==TRUE) print(uniqueCharts[k])
          
        }
        
        ############ set the number of sampling times for this chart #############
        
        {
          
          # trunc(nrow(chartDF) / (40 * cps))
          
        }
        
        ####################   get the first and last events   #####################
        
        # call the eventNamesFn from the sigProcHelper.R script
        # firstLastEvents <- eventNamesFn(x=chartDF)
        
        {
          
          firstLastEvents <- getFirstLastEventFn(x=chartDF)
          firstEvent <- firstLastEvents[1] - 600
          lastEventEnd <- firstLastEvents[2] +600
          
          # Nov, 30, 2024
          if(firstEvent < 1) firstEvent <- 1
          if(lastEventEnd > nrow(chartDF)) lastEventEnd <- nrow(chartDF)
          
          # 2-6-2021 stop at the onset of the last event
          # because some examiners blow the cuff before they stop recording
          # assign("firstLastEvents", firstLastEvents, pos=1)
          
        }
        
        ###########################################################
        
        ############### scale and center the data #################
        
        {
          
          # scaleVal and newScaleVal are variables in the global environment,
          # created by the scaleDataFn() and offsetDataFn()
          
          #####  Pneumo #####
          
          if(isTRUE(pneumoScale) && chartDF$UPneumo[1] != -9.9) {
            
            if(showNames==TRUE) print("  pneumo scale and offset - upper and lower")
            
            # scale the pneumo data 
            # chartDF$c_UPneumoSm <- scaleDataFn(x=chartDF$c_UPneumoSm, sec=40, times=6, ignore=3, yRange=scaleVals['uPneumo'], maxY=yMaxVals['uPneumo'], minY=yMinVals['uPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            # chartDF$c_LPneumoSm <- scaleDataFn(x=chartDF$c_LPneumoSm, sec=40, times=6, ignore=3, yRange=scaleVals['lPneumo'], maxY=yMaxVals['lPneumo'], minY=yMinVals['lPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            chartDF$c_UPneumoSm <- scaleDataFn(x=chartDF$c_UPneumoSm, sec=30, times=6, ignore=4, yRange=scaleVals['uPneumo'], maxY=yMaxVals['uPneumo'], minY=yMinVals['uPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            chartDF$c_LPneumoSm <- scaleDataFn(x=chartDF$c_LPneumoSm, sec=30, times=6, ignore=4, yRange=scaleVals['lPneumo'], maxY=yMaxVals['lPneumo'], minY=yMinVals['lPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
            # offset the pneumo data
            chartDF$c_UPneumoSm <- offsetDataFn(x=chartDF$c_UPneumoSm, y=yOffset['uPneumo'], maxY=yMaxVals['uPneumo'], minY=yMinVals['uPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            chartDF$c_LPneumoSm <- offsetDataFn(x=chartDF$c_LPneumoSm, y=yOffset['lPneumo'], maxY=yMaxVals['lPneumo'], minY=yMinVals['lPneumo'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
            ### make sure the pneumo channels are adequately separated at the first event
            # pneumoSepVal is initialized in the NCCAASCII_init.R script
            if( (chartDF$c_UPneumoSm[firstEvent] - chartDF$c_LPneumoSm[firstEvent]) < pneumoSepVal) {
              # calculate the lowerOffset
              lowerOffset1 <- (chartDF$c_UPneumoSm[firstEvent] - pneumoSepVal) - chartDF$c_LPneumoSm[firstEvent]
              # then add the offset value to the vector
              chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm + lowerOffset1
            }
            
            ### make sure the pneumo channels are adequately separated at the last event
            if( (chartDF$c_UPneumoSm[(lastEventEnd)] - chartDF$c_LPneumoSm[(lastEventEnd)]) < pneumoSepVal) {
              # calculate the lowerOffset
              lowerOffset2 <- (chartDF$c_UPneumoSm[(lastEventEnd)] - pneumoSepVal) - chartDF$c_LPneumoSm[(lastEventEnd)]
              # add the second offset to the vector
              chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm + lowerOffset2
            }
            
            # compute the additional pneumo channels
            # chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumoSm, y=round(7.5*cps,0), times=4) # was y=40, times=8 11/19/2015
            # chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumoSm, y=round(9*cps,0), times=12) # was y=40, times=8 11/19/2015
            chartDF$c_UPneumoMid <- MASmooth(x=chartDF$c_UPneumoSm, y=round(25*cps), times=4) # was y=40, times=8 11/19/2015
            
            # chartDF$c_LPneumoMid <- MASmooth(x=chartDF$c_LPneumoSm, y=round(7.5*cps,0), times=4)
            chartDF$c_LPneumoMid <- MASmooth(x=chartDF$c_LPneumoSm, y=round(25*cps), times=4)
            
            # calculate the inhalation and exhalation lines by calling the maxPeak() and mixPeak() functions
            # from the sigProcHelper.R script
            chartDF$c_UPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_UPneumoSm, y=round(.25*cps)),
                                                     y=chartDF$c_UPneumoSm[maxPeak(x=chartDF$c_UPneumoSm, y=round(.25*cps))])
            chartDF$c_UPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_UPneumoSm, y=round(.25*cps)),
                                                     y=chartDF$c_UPneumoSm[minPeak(x=chartDF$c_UPneumoSm, y=round(.25*cps))])
            
            chartDF$c_LPneumoInh <- interpolatePeaks(x=maxPeak(x=chartDF$c_LPneumoSm, y=round(.25*cps)),
                                                     y=chartDF$c_LPneumoSm[maxPeak(x=chartDF$c_LPneumoSm, y=round(.25*cps))])
            chartDF$c_LPneumoExh <- interpolatePeaks(x=minPeak(x=chartDF$c_LPneumoSm, y=round(.25*cps)),
                                                     y=chartDF$c_LPneumoSm[minPeak(x=chartDF$c_LPneumoSm, y=round(.25*cps))])
            
            # compute the amplitude or difference for the inhalation and exhalation lines
            chartDF$c_UpneumoAmp <- round(chartDF$c_UPneumoInh - chartDF$c_UPneumoExh, 2)
            chartDF$c_LpneumoAmp <- round(chartDF$c_LPneumoInh - chartDF$c_LPneumoExh, 2)
            
            chartDF$c_UPneumoExcursion <- respirationLineExcursionFn(x=chartDF$c_UPneumoSm)
            chartDF$c_LPneumoExcursion <- respirationLineExcursionFn(x=chartDF$c_LPneumoSm)
            
            # initialize the excursion vectors without anser distortion buffers
            chartDF$c_UPneumoExcursion0 <- chartDF$c_UPneumoExcursion
            chartDF$c_LPneumoExcursion0 <- chartDF$c_LPneumoExcursion
            
            # source the scipt to load the getAnswerIndicesFn()
            # source("~/Dropbox/R/NCCA_ASCII_Parse/pneumoCaliperFunctions.R", echo=FALSE)
            answerBufferIndices <- getAnswerIndicesFn(x=chartDF$Label, ansBuffLen=pneumoAnsBuff, buffer=TRUE)
            
            # set the answer distortion buffers to NA
            chartDF$c_UPneumoExcursion0[answerBufferIndices] <- NA
            chartDF$c_LPneumoExcursion0[answerBufferIndices] <- NA
            
            
            # # offset the respiration excursion data
            # UPOffset <- 950 - max(chartDF$c_UPneumoExcursion)
            # LPOffset <- 700 - max(chartDF$c_LPneumoExcursion)
            # # min(chartDF$c_UPneumoExcursion)
            # # min(chartDF$c_LPneumoExcursion)
            # chartDF$c_UPneumoExcursion <- chartDF$c_UPneumoExcursion + UPOffset
            # chartDF$c_LPneumoExcursion <- chartDF$c_LPneumoExcursion + LPOffset
            
            # Oct 13, 2024 calculate the 25th 50th and 75th quantiles for the upper pneumo
            chartDF$c_UPneumo_Q25 <- quantile(chartDF$c_UPneumoSm, .25)
            chartDF$c_UPneumo_Q50 <- quantile(chartDF$c_UPneumoSm, .50)
            chartDF$c_UPneumo_Q75 <- quantile(chartDF$c_UPneumoSm, .75)
            # Oct 13, 2024 calculate the 25th 50th and 75th quantiles for the lower pneumo
            chartDF$c_LPneumo_Q25 <- quantile(chartDF$c_LPneumoSm, .25)
            chartDF$c_LPneumo_Q50 <- quantile(chartDF$c_LPneumoSm, .50)
            chartDF$c_LPneumo_Q75 <- quantile(chartDF$c_LPneumoSm, .75)
          }
          
          ##### EDA #####
          
          if(isTRUE(EDAScale)) {
            
            if(showNames==TRUE) print("  EDA scale and offset")
            
            # first source the sigProcHelper.R script to load the scaleDataFn and offsetDataFn
            
            # if(all(i==1, j==2, k==1)) {
            #   assign("chartDF", chartDF, envir=.GlobalEnv)
            #   stop()
            # }
            
            ### Auto EDA
            
            # November 19, 2023
            # set the rangen where the data will be scaled 
            # use the scaleVals and gainVals parameters from the NCCAASCII_init.R script
            gainRangeEDA <- scaleVals['eda'] * (gainVals['eda'] / 100)
            
            # scale the EDA data
            # chartDF$c_AutoEDA <- scaleDataFn(x=chartDF$c_AutoEDA, sec=60, times=6, ignore=1, yRange=scaleVals['eda'], maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
            chartDF$c_AutoEDA <- scaleDataFn(x=chartDF$c_AutoEDA, sec=30, times=6, ignore=3, yRange=gainRangeEDA, maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
            # Aug 2, 2023 adjust the max EDA range
            {
              
                # new values to constrain the auto EDA
                chartRange <- abs(diff(range(chartDF$c_AutoEDA[firstEvent:lastEventEnd])))
                limRange <- yRange * .67
                adjRange <- limRange / chartRange
                lowLim <- yMaxVals['eda'] - limRange
              
              if(chartRange > limRange) {
                # calculate the data length
                useLen <- round(length(chartDF$c_AutoEDA[firstEvent:lastEventEnd]) / cps)
                # use the data length to scale the data using a single sample
                chartDF$c_AutoEDA <- 
                  scaleDataFn(x=chartDF$c_AutoEDA, sec=useLen, times=1, ignore=0, yRange=limRange, maxY=yMaxVals['eda'], minY=lowLim, firstRow=firstEvent, lastRow=(lastEventEnd))
                # set the max value
                # maxOffset <- trunc(max(chartDF$c_AutoEDA[firstEvent:lastEventEnd]) - yMaxVals['eda'])
                # chartDF$c_AutoEDA <- chartDF$c_AutoEDA + maxOffset
              }
              
            }
            
            # offset the EDA data
            chartDF$c_AutoEDA <- offsetDataFn(x=chartDF$c_AutoEDA, y=yOffset['eda'], maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
            # process the additional EDA channels
            chartDF$c_AutoEDAMid <- MASmooth(x=chartDF$c_AutoEDA, y=round(15*cps,0), times=5) # was y=150, times=6 11/26/2015
            chartDF$c_AutoEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0)),
                                                      y=chartDF$c_AutoEDA[maxPeak(x=chartDF$c_AutoEDA, y=round(1*cps,0))])
            chartDF$c_AutoEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_AutoEDA, y=round(1.5*cps,0)),
                                                      y=chartDF$c_AutoEDA[minPeak(x=chartDF$c_AutoEDA, y=round(1.5*cps,0))])
            
            ### Manual EDA
            
            # November 19, 2023
            # set the range where the data will be scaled 
            # use the scaleVals and gainVals parameters from the NCCAASCII_init.R script
            gainRange <- scaleVals['eda'] * (gainVals['eda'] / 100)
            
            # scale the EDA data
            chartDF$c_ManualEDA <- scaleDataFn(x=chartDF$c_ManualEDA, sec=60, times=6, ignore=1, yRange=gainRangeEDA, maxY=(yMaxVals['eda']), minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
            # offset the EDA data
            chartDF$c_ManualEDA <- offsetDataFn(x=chartDF$c_ManualEDA, y=(yOffset['eda']-100), maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
            # process the additional EDA channels
            chartDF$c_ManualEDAMid <- MASmooth(x=chartDF$c_ManualEDA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
            chartDF$c_ManualEDAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_ManualEDA, y=round(1*cps,0)),
                                                        y=chartDF$c_ManualEDA[maxPeak(x=chartDF$c_ManualEDA, y=round(1*cps,0))])
            chartDF$c_ManualEDABase <- interpolatePeaks(x=minPeak(x=chartDF$c_ManualEDA, y=round(1.5*cps,0)),
                                                        y=chartDF$c_ManualEDA[minPeak(x=chartDF$c_ManualEDA, y=round(1.5*cps,0))])
            
            ### LX auto eda output
            
            if( "c_EA" %in% colnames(chartDF) &
                length(which(is.na(chartDF$c_EA))) != nrow(chartDF) ) {
              
              # set the rangen where the data will be scaled 
              # use the scaleVals and gainVals parameters from the NCCAASCII_init.R script
              gainRange <- scaleVals['eda'] * (gainVals['eda'] / 100)
              
              # scale the EDA data
              chartDF$c_EA <- scaleDataFn(x=chartDF$c_EA, sec=30, times=8, ignore=1, yRange=gainRange, maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # offset the EDA data
              chartDF$c_EA <- offsetDataFn(x=chartDF$c_EA, y=(yOffset['eda']-25), maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # scale and offset the additional EDA channels
              # chartDF$c_EAMid <- MASmooth(x=chartDF$c_EA, y=round(5*cps,0), times=6) # was y=150, times=6 11/26/2015
              # chartDF$c_EAPeak <- interpolatePeaks(x=maxPeak(x=chartDF$c_EA, y=round(1*cps,0)),
              #                                      y=chartDF$c_EA[maxPeak(x=chartDF$c_EA, y=round(1*cps,0))])
              # chartDF$c_EABase <- interpolatePeaks(x=minPeak(x=chartDF$c_EA, y=round(1.5*cps,0)),
              #                                      y=chartDF$c_EA[minPeak(x=chartDF$c_EA, y=round(1.5*cps,0))])
              
            }
            
            ### also scale and offset the filtered EDA data
            
            # # scale the filtered EDA data
            # chartDF$c_EDAFilt <- scaleDataFn(x=chartDF$c_EDAFilt, sec=40, times=10, ignore=2, yRange=scaleVals[3], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd-450))
            # 
            # # offset the filtered EDA data
            # chartDF$c_EDAFilt <- offsetDataFn(x=chartDF$c_EDAFilt, y=yOffset[3], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd-450))
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
            
            ### second EDA 2 sensor ###
            
            # Jan 17, 2023
            if(sum(pmatch(names(chartDF), "c_EDA2", nomatch=0)) != 0) {
              
              # chartDF$c_ManualEDA2 <- scaleDataFn(x=chartDF$c_ManualEDA2, sec=60, times=6, ignore=1, yRange=scaleVals['eda'], maxY=(yMaxVals['eda']+250), minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
              # chartDF$c_ManualEDA2 <- scaleDataFn(x=chartDF$c_ManualEDA2, sec=30, times=6, ignore=3, yRange=scaleVals['eda'], maxY=(yMaxVals['eda']+250), minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # offset the EDA2 data
              # chartDF$c_ManualEDA2 <- offsetDataFn(x=chartDF$c_ManualEDA2, y=(yOffset['eda']-200), maxY=yMaxVals['eda'], minY=yMinVals['eda'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              
            }
            
          } 
          
          ##### Cardio #####
          
          if(isTRUE(cardioScale) && chartDF$Cardio1[1] != -9.9) {
            
            if(showNames==TRUE) print("  cardio scale and offset")
            
            # make sure that every cardio peak exists on a single sample
            # this was also done in the cardioSigProc function and is done again here
            # for additional peak correction
            chartDF$c_Cardio1 <- fixPeak(x=chartDF$c_Cardio1, times=2)
            
            # scale the cardio data 
            # chartDF$c_Cardio1 <- scaleDataFn(chartDF$c_Cardio1, sec=30, times=8, ignore=4, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd-450))
            chartDF$c_Cardio1 <- scaleDataFn(chartDF$c_Cardio1, sec=30, times=8, ignore=6, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd-450))
            
            # offset the cardio data
            chartDF$c_Cardio1 <- offsetDataFn(x=chartDF$c_Cardio1, y=yOffset['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))
            
            # scale and offset the additional cardio channels
            
            # compute the cardio mid line 
            chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
            # chartDF$c_CardioMid <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
            
            
            
            # {
            # 
            #   ts <- c(1:nrow(chartDF))
            # 
            #   DAT <- cbind.data.frame(tScale=ts,
            #                           mid=chartDF$c_CardioMid[ts],
            #                           ma=chartDF$c_CardioMA[ts],
            #                           dat=chartDF$c_Cardio1[ts] )
            # 
            #   # pPoints <- sort(unique(c(maxPeak(DAT$mid, 5), minPeak(DAT$mid, 5))))
            # 
            #   offsetVal <- 30
            # 
            #   sPoints <- maxPeak(DAT$mid, 5)
            #   sPoints <- sPoints[which(DAT$mid[sPoints] + offsetVal <= DAT$ma[sPoints])]
            # 
            #   dPoints <- minPeak(DAT$mid, 5)
            #   dPoints <- dPoints[which(DAT$mid[dPoints]- offsetVal >= DAT$ma[dPoints])]
            # 
            #   scPoints <- maxPeak(DAT$dat, 10)
            #   scPoints <- scPoints[which(DAT$dat[scPoints] - offsetVal <= DAT$ma[scPoints])]
            # 
            #   dcPoints <- minPeak(DAT$dat, 10)
            #   dcPoints <- dcPoints[which(DAT$dat[dcPoints] + offsetVal >= DAT$ma[dcPoints])]
            # 
            #   length(sort(unique(c(sPoints, dPoints, scPoints, dcPoints))))
            # 
            #   ggplot() +
            #     geom_path(DAT, mapping=aes(x=tScale, y=dat), color="red", linewidth=.2, alpha=.75) +
            #     geom_path(DAT, mapping=aes(x=tScale, y=ma), color="brown", linewidth=.2, alpha=.67) +
            #     geom_path(DAT, mapping=aes(x=tScale, y=mid), color="black", linewidth=.2, alpha=.77) +
            #     annotate("point", x=DAT$tScale[sPoints], y=DAT$mid[sPoints], shape=8, size=4, color="blue", alpha=.75) +
            #     annotate("point", x=DAT$tScale[dPoints], y=DAT$mid[dPoints], shape=8, size=4, color="blue", alpha=.55) +
            #     annotate("point", x=DAT$tScale[scPoints], y=DAT$dat[scPoints], shape=8, size=4, color="blue", alpha=.55) +
            #     annotate("point", x=DAT$tScale[dcPoints], y=DAT$dat[dcPoints], shape=8, size=4, color="blue", alpha=.55)
            # 
            # }
            
            
            
            # then compute a slower moving average for cardio feature extraction
            # was 1.5 sec and times=2 3/17/2017
            # was 1.2 sec and times=3 6/9/2020
            # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=1)
            # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(1*cps,0), times=1)
            # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(.5*cps,0), times=1)
            
            # May 19, 2024 - improve the correlation coef
            chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1*cps,0), times=3)
            chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(.5*cps,0), times=3)
            chartDF$c_CardioMA <- MASmooth(x=chartDF$c_CardioMA, y=round(.25*cps,0), times=3)
            
            
            # 
            chartDF$c_CardioSRQ1 <- SRQ2001_cardioSquareWaveFn(x=chartDF$c_Cardio1, segLen=15)
            chartDF$c_CardioSRQ2 <- cardioSRQFn(x=chartDF$c_Cardio1, segLen=30)
            
            
            # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=2)
            
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
            chartDF$c_cardioRateSystolic <- 
              interpolatePeaks(x=systPeaks, 
                               y=(60 / (diff(systPeaks) / 30)))[1:nrow(chartDF)]
            
            ### electronic cardio cuff ###
            
            # check to see if electronic cardio data are present in the exam data frame
            if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0)) != 0) {
              
              if(showNames==TRUE) print("  electronic cardio scale and offset")
              
              # make sure that every cardio peak exists on a single sample
              # this was also done in the cardioSigProc function and is done again here
              # for additional peak correction
              chartDF$c_eCardio <- fixPeak(x=chartDF$c_eCardio, times=2)
              
              # scale the electronic cardio cuff data
              # chartDF$c_eCardio <- scaleDataFn(chartDF$c_eCardio, sec=30, times=12, ignore=2, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))
              chartDF$c_eCardio <- scaleDataFn(chartDF$c_eCardio, sec=30, times=8, ignore=6, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # offset the electronic cardio cuff data
              chartDF$c_eCardio <- offsetDataFn(x=chartDF$c_eCardio, y=yOffset['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # scale and offset the additional electronic cardio cuff channels
              
              chartDF$c_eCardioMid <- MASmooth(x=chartDF$c_eCardio, y=round(.5*cps,0), times=3)
              chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardio, y=round(1.5*cps,0), times=3)
              
              # chartDF$c_eCardioMA <- MASmooth(x=chartDF$c_eCardioMA, y=round(.5*cps,0), times=2)
              
              cardioRate <- ratePerMin(chartDF$c_eCardio,buffer=9,peaks="upper",lowPass=TRUE)
              bufferLen <- bufferLenFn(cardioRate, y=.6)
              
              # calculate the systolic and diastolic lines
              maxOut <- maxPeak(x=chartDF$c_eCardio, y=bufferLen)
              maxVal <- chartDF$c_eCardio[maxOut]
              chartDF$c_eCardioSystolic <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
              minOut <- minPeak(x=chartDF$c_eCardio, y=bufferLen)
              minVal <- chartDF$c_eCardio[na.omit(minOut)]
              chartDF$c_eCardioDiastolic <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))[1:nrow(chartDF)]
              
            } # end if for electronic cardio
            
            ### forearm cuff or finger cuff ###
            
            # check to see if PLE data are present in the exam data frame
            if(sum(pmatch(names(examDF), "c_FC", nomatch=0)) != 0) {
              
              if(showNames==TRUE) print("  forearm cuff scale and offset")
              
              # make sure that every cardio peak exists on a single sample
              # this was also done in the cardioSigProc function and is done again here
              # for additional peak correction
              chartDF$c_FC <- fixPeak(x=chartDF$c_FC, times=2)
              
              # scale the finger cuff data
              # chartDF$c_FC <- scaleDataFn(chartDF$c_FC, sec=30, times=12, ignore=2, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))
              chartDF$c_FC <- scaleDataFn(chartDF$c_FC, sec=30, times=8, ignore=6, yRange=scaleVals['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # offset the finger cuff data
              chartDF$c_FC <- offsetDataFn(x=chartDF$c_FC, y=yOffset['cardio'], maxY=yMaxVals['cardio'], minY=yMinVals['cardio'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # scale and offset the additional finger cuff channels
              # chartDF$c_CardioMA <- MASmooth(x=chartDF$c_Cardio1, y=round(1.5*cps,0), times=3)
              
              chartDF$c_FCMid <- MASmooth(x=chartDF$c_FC, y=round(.5*cps,0), times=3)
              
              chartDF$c_FCMA <- MASmooth(x=chartDF$c_FC, y=round(1.5*cps,0), times=3)
              
              # chartDF$c_FCMA <- MASmooth(x=chartDF$c_FCMA, y=round(.5*cps,0), times=2)
              
              chartDF$c_FCMA <- MASmooth(x=chartDF$c_FC, y=round(1.5*cps,0), times=1)
              chartDF$c_FCMA <- MASmooth(x=chartDF$c_FCMA, y=round(1*cps,0), times=1)
              chartDF$c_FCMA <- MASmooth(x=chartDF$c_FCMA, y=round(.5*cps,0), times=1)
              
              
              cardioRate <- ratePerMin(chartDF$c_FC,buffer=9,peaks="upper",lowPass=TRUE)
              bufferLen <- bufferLenFn(cardioRate, y=.6)
              
              maxOut <- maxPeak(x=chartDF$c_FC, y=bufferLen)
              maxVal <- chartDF$c_FC[maxOut]
              chartDF$c_FCSystolic <- interpolatePeaks(x=maxOut, y=maxVal)[1:nrow(chartDF)]
              minOut <- minPeak(x=chartDF$c_FC, y=bufferLen)
              minVal <- chartDF$c_FC[na.omit(minOut)]
              chartDF$c_FCDiastolic <- interpolatePeaks(x=na.omit(minOut), y=na.omit(minVal))[1:nrow(chartDF)]
              
            } # end if for finger cuff
            
          } # if cardioScale
          
          #####   PLE   #####
          
          # check to see if PLE data are present in the exam data frame
          
          if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0)) != 0) {
          
            # Jan 17, 2023 changed the order of this and the preceding if statement  
            if(isTRUE(PLEScale)  && chartDF$PPG1[1] != -9.9) {
            
              # make sure that every PLE peak exists on a single sample
              # this was also done in the PLESigProc function and is done again here
              # for additional peak correction
              chartDF$c_PPG1 <- fixPeak(x=chartDF$c_PPG1, times=2)
              
              if(showNames==TRUE) print("  PLE scale and offset")
              
              # November 19, 2023
              # set the rangen where the data will be scaled 
              # use the scaleVals and gainVals parameters from the NCCAASCII_init.R script
              gainRangePLE <- scaleVals['ple'] * (gainVals['ple'] / 100)
              
              # scale the PLE data 
              # chartDF$c_PPG1 <- scaleDataFn(chartDF$c_PPG1, sec=30, times=8, ignore=2, yRange=scaleVals['ple'], maxY=yMaxVals['ple'], minY=yMinVals['ple'], firstRow=firstEvent, lastRow=(lastEventEnd))
              # chartDF$c_PPG1 <- scaleDataFn(chartDF$c_PPG1, sec=30, times=8, ignore=3, yRange=scaleVals['ple'], maxY=yMaxVals['ple'], minY=yMinVals['ple'], firstRow=firstEvent, lastRow=(lastEventEnd))
              chartDF$c_PPG1 <- scaleDataFn(chartDF$c_PPG1, sec=30, times=8, ignore=3, yRange=gainRangePLE, maxY=yMaxVals['ple'], minY=yMinVals['ple'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # offset the PLE data
              chartDF$c_PPG1 <- offsetDataFn(x=chartDF$c_PPG1, y=yOffset['ple'], maxY=yMaxVals['ple'], minY=yMinVals['ple'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # scale and offset the additional PLE channels
              chartDF$c_PPG1MA <- MASmooth(x=chartDF$c_PPG1, y=30, times=4) # y=15, times=3 will show the respiration
              # chartDF$c_PPG1MA <- MASmooth(x=chartDF$c_Cardio1, y=round(.5*cps,0), times=3)
              
              # compute the cardio rate to get the buffer to get the peaks
              # 7-29-2017 to fix the bd systolic line in PLE data
              cardioRate <- ratePerMin(chartDF$c_PPG1,buffer=9,peaks="upper",lowPass=TRUE)
              bufferLen <- bufferLenFn(cardioRate, y=.75)
              # cardioRate <- ratePerMin(chartDF$c_PPG1,buffer=3,peaks="upper",lowPass=TRUE)
              # bufferLen <- bufferLenFn(cardioRate)
              
              # get the systolic peaks
              maxOut <- maxPeak(x=chartDF$c_PPG1, y=bufferLen)
              # interpolate between systolic peaks
              chartDF$c_PPG1Max <- interpolatePeaks(x=maxOut, y=chartDF$c_PPG1[maxOut])[1:nrow(chartDF)]
              # get the diastolic peaks
              minOut <- minPeak(x=chartDF$c_PPG1, y=bufferLen)
              # interpolate between diastolic peaks
              chartDF$c_PPG1Min <- interpolatePeaks(x=minOut, y=chartDF$c_PPG1[minOut])[1:nrow(chartDF)]
              
            } # end if for extant PLE sensor data
          
          } # end if for PLE
          
          ##### Activity sensor #####
          
          # names(chartDF)
          if(all("Move1" %in% names(chartDF), isTRUE(activityScale), chartDF$Move1[1] != -9.9)) {
            
            # check to see if activity sensor data are present in the examDF
            if(sum(pmatch(names(examDF), "c_Move1", nomatch=0))!=0) {
              
              if(showNames==TRUE) print("  activity scale and offset")
              
              # scale the activity sensor data 
              # chartDF$c_Move1 <- scaleDataFn(x=chartDF$c_Move1, sec=30, times=12, ignore=2, yRange=scaleVals['activity'], maxY=yMaxVals['activity'], minY=yMinVals['activity'], firstRow=firstEvent, lastRow=(lastEventEnd))
              chartDF$c_Move1 <- scaleDataFn(x=chartDF$c_Move1, sec=30, times=12, ignore=4, yRange=scaleVals['activity'], maxY=yMaxVals['activity'], minY=yMinVals['activity'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # offset the activity sensor data 
              chartDF$c_Move1 <- offsetDataFn(x=chartDF$c_Move1, y=yOffset['activity'], maxY=yMaxVals['activity'], minY=yMinVals['activity'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # scale and offset the additions activity channels
              chartDF$c_Move1MA <- MASmooth(x=chartDF$c_Move1, y=round(7.5*cps,0), times=1)
              
              # scale and offset the processed activity sensor data
              # chartDF$c_Move1Proc <- scaleDataFn(chartDF$c_Move1Proc, sec=12, times=20, yRange=scaleVals[6], maxY=yMax, minY=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd-450))
              # chartDF$c_Move1Proc <- offsetDataFn(x=chartDF$c_Move1Proc, y=(yOffset[6]), yMax=yMax, yMin=(yMin+(.05*yRange)), firstRow=firstEvent, lastRow=(lastEventEnd-450))
              
              # make a new processed activity data
              # was y=round(.0625*cps,0) 2020-06-6
              chartDF$c_Move1Proc <- MASmooth(x=chartDF$c_Move1, y=round(.5*cps,0), times=1)
              chartDF$c_Move1ProcMA <- MASmooth(x=chartDF$c_Move1Proc, y=round(15*cps,0), times=1)
              
              # scale and offset the additional activity channels
              # 2025Apr22 adjusted the buffer and cyclic portion for these functions
              activityRate <- ratePerMin(chartDF$c_Move1Proc,buffer=8,peaks="upper",lowPass=TRUE)
              activityBufferLen <- bufferLenFn(activityRate, y=.3)
              
              maxOut <- maxPeak(x=chartDF$c_Move1Proc, y=activityBufferLen)
              chartDF$c_Move1Max <- interpolatePeaks(x=maxOut, y=chartDF$c_Move1Proc[maxOut])[1:nrow(chartDF)]
              minOut <- minPeak(x=chartDF$c_Move1Proc, y=activityBufferLen)
              chartDF$c_Move1Min <- interpolatePeaks(x=minOut, y=chartDF$c_Move1Proc[minOut])[1:nrow(chartDF)]
              
              # commented out 4-25-2017 not sure if this is needed or used now
              # chartDF$c_Move1Result <- (chartDF$c_Move1Result - chartDF$c_Move1Result[1]) * (scaleVal * newScaleVal) + chartDF$c_Move1Proc[1] - (.05*yRange)
              
            } else print("  WARNING: missing activity sensor data") # end if for activity sensor data
            
          } # end if for extant activity sensor data
          
          ######## PTT sensor########
          
          # names(chartDF)
          if(all("PTTPTT" %in% names(chartDF), isTRUE(PTTScale))) {
            
            # check to see if activity sensor data are present in the examDF
            if(sum(pmatch(names(examDF), "c_PTTPTT", nomatch=0))!=0) {
              
              if(showNames==TRUE) print("  PTT scale and offset")
              
              # May 13, 2024
              PTTPPGNa <- sort(unique(which(c(chartDF$PTTPTT == -9.9), which(is.na(chartDF$PTTPTT)))))
              PTTECGNa <- sort(unique(which(c(chartDF$PTTPPG == -9.9), which(is.na(chartDF$PTTPPG)))))
              PTTPTTNa <- sort(unique(which(c(chartDF$PTTECG == -9.9), which(is.na(chartDF$PTTECG)))))
              
              # scale the PTT sensor data 
              chartDF$c_PTTPTT <- scaleDataFn(x=chartDF$c_PTTPTT, sec=30, times=12, ignore=4, yRange=scaleVals['PTTPTT'], maxY=yMaxVals['PTTPTT'], minY=yMinVals['PTTPTT'], firstRow=firstEvent, lastRow=(lastEventEnd)) 
              chartDF$c_PTTPPG <- scaleDataFn(x=chartDF$c_PTTPPG, sec=30, times=12, ignore=4, yRange=scaleVals['PTTPPG'], maxY=yMaxVals['PTTPPG'], minY=yMinVals['PTTPPG'], firstRow=firstEvent, lastRow=(lastEventEnd)) 
              chartDF$c_PTTECG <- scaleDataFn(x=chartDF$c_PTTECG, sec=30, times=12, ignore=4, yRange=scaleVals['PTTECG'], maxY=yMaxVals['PTTECG'], minY=yMinVals['PTTECG'], firstRow=firstEvent, lastRow=(lastEventEnd)) 
              
              # offset the PTT sensor data 
              chartDF$c_PTTPTT <- offsetDataFn(x=chartDF$c_PTTPTT, y=yOffset['PTTPTT'], maxY=yMaxVals['PTTPTT'], minY=yMinVals['PTTPTT'], firstRow=firstEvent, lastRow=(lastEventEnd))
              # May 12, 2024 to prevent data from plotting off screen
              chartDF$c_PTTPTT[which(chartDF$c_PTTPTT < yMinVals['PTTPTT'])] <- yMinVals['PTTPTT']
              chartDF$c_PTTPPG <- offsetDataFn(x=chartDF$c_PTTPPG, y=yOffset['PTTPPG'], maxY=yMaxVals['PTTPPG'], minY=yMinVals['PTTPPG'], firstRow=firstEvent, lastRow=(lastEventEnd))
              chartDF$c_PTTECG <- offsetDataFn(x=chartDF$c_PTTECG, y=yOffset['PTTECG'], maxY=yMaxVals['PTTECG'], minY=yMinVals['PTTECG'], firstRow=firstEvent, lastRow=(lastEventEnd))
              
              # slow moving average
              chartDF$c_PTTPTT_MA <- MASmooth(x=chartDF$c_PTTPTT, y=round(7.5*cps,0), times=3)
              
              # source("~/Dropbox/R/NCCA_ASCII_Parse/SBPFromPTT.R")
              chartDF$c_PTTPTT_abs <- SBP_from_PTT_Fn(PTT=chartDF$c_PTTPTT)
              
            } 
            
          } # end if for extant PTT sensor
          
        }
        
        ########
        
        assign("chartDF", chartDF, envir=.GlobalEnv)
        
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

  } # end loop over i unique exams
  
  if(showNames==TRUE) print(paste(i, "exams processed"))
  
  # return the last
  if(output==TRUE) { return(examDF) } else {
    return(uniqueExams)
  }
  
} # end ScaleOffsetDataFn()


# call the function
# ScaleOffsetDataFn(x=uniqueExams, output=FALSE, showNames=TRUE)



