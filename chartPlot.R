# plot an entire chart in one graphic
#
# sampling rate is 30cps
# 
#
################################



library(stringr)
library(ggplot2)
library(grid)

# source the list of excluded events so that measurements are not plotted for these
source('~/Dropbox/R/NCCA_ASCII_Parse/excludedEvents.R')



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))
# uniqueExams <- uniqueExams[1:3]




# parameters to describe the data
cps <- 30
prestimSeg <- 5
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15
addSeg <- 5



# to control the print output
showNames <- TRUE
output <- FALSE
showMeasurements <- TRUE
outputFileName <- "_chartsPlot.pdf"
separateCharts <- FALSE
printPlot <- FALSE



# plot a single segment instead of iterating through all exams in the global environment
getSegment <- FALSE
examNum <- 1
seriesNum <- 1
chartNum <- 1
segmentNum <- "ALL"




# to re-initialize the graphics device
# graphics.off()
# dev.new()

if(getSegment == TRUE) {
  if(examNum!="ALL") uniqueExams <- uniqueExams[examNum] 
  # assign("examName", uniqueExams[examNum])
} 

# loop over each exam in the list and plot the stimulus segments
for(i in 1:length(uniqueExams)) {
  # i=1
  examName <- uniqueExams[i]
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Data", "*")
  examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  
  if(showNames==TRUE) print(examName)
  
  graphics.off()
  dev.new()
  
  # if(printPlot == TRUE) {
  #   if(separateCharts==FALSE) {
  #     pdf(paste(examName, outputFileName, sep="_"), height=5, width=8)  
  #   } else { 
  #     pdf(paste(examName, chartName, outputFileName, sep="_"), height=5, width=8)
  #   }
  # } 
  
  examStartRow <- 1
  examEndRow <- nrow(examDF)
  
  ### add additional columns here
  
  # get the names of all unique series in the exam
  uniqueSeries <- as.character(unique(examDF$seriesName))
  
  if(getSegment == TRUE) {
    if(seriesNum!="ALL") uniqueSeries <- uniqueSeries[seriesNum]
  }
  
  # loop over each unique series
  for(j in 1:length(uniqueSeries)) {
    # j=1
    seriesName <- uniqueSeries[j]
    
    # get the list of time series data for the charts in the exam
    seriesDF <- examDF[examDF$seriesName==seriesName,]
    
    if(showNames==TRUE) print(paste("series", seriesName))
    
    seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
    seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
    
    # uniqueCharts <- names(seriesDF)
    uniqueCharts <- as.character(unique(seriesDF$chartName))
    
    if(getSegment == TRUE) {
      if(chartNum!="ALL") uniqueCharts <- uniqueCharts[chartNum]
    }
    
    # loop over each chart in the series 
    for(k in 1:length(uniqueCharts)) {
      # k=1
      chartName <- uniqueCharts[k]
      
      # get the data frame with the time series data for each chart in the series
      chartDF <- seriesDF[seriesDF$chartName==chartName,]
      
      if(showNames==TRUE) print(chartName)
      
      chartOnsetRow <- which(seriesDF$chartName==uniqueCharts[k])[1]
      chartEndRow <- chartOnsetRow + nrow(chartDF) - 1
      
      ####################################### scale the data for the chart
      
      # scale the pneumo from 10 to 20 seconds
      UPneumoScale <- ifelse(nrow(chartDF) < 600,
                             35 / (max(chartDF$c_UPneumo[(1:nrow(chartDF))]) - min(chartDF$c_UPneumo[(1:nrow(chartDF))])),
                             35 / (max(chartDF$c_UPneumo[301:600]) - min(chartDF$c_UPneumo[301:600])))
      LPneumoScale <- ifelse(nrow(chartDF) < 600,
                             35 / (max(chartDF$c_LPneumo[(1:nrow(chartDF))]) - min(chartDF$c_LPneumo[(1:nrow(chartDF))])),
                             35 / (max(chartDF$c_LPneumo[301:600]) - min(chartDF$c_LPneumo[301:600]))) 
      EDAScale <- 170 / (max(na.omit(chartDF$c_AutoEDA)) - min(na.omit(chartDF$c_AutoEDA)))

      # scale the cardio pulse amplitude for 5 seconds at the first stimulus event
      firstEvent <- which(chartDF$eventLabel!="")[2]
      outDiff <- NULL
      for (m in firstEvent:(firstEvent+149)) {
        outDiff <- c(outDiff, diff(range(chartDF$c_Cardio1[m:(m+29)])))
      }
      cardioScale <- 35 / mean(outDiff)

      # set a variable to determine if PLE data exist in the current chart
      inclPLE <- ifelse(sum(pmatch(names(chartDF), "c_PL", nomatch=0))>0, 
                        TRUE, 
                        FALSE) 

      # scale the PLE from 10 to 20 seconds
      if(inclPLE==TRUE) PLScale <- 35 / (max(chartDF$c_PL[301:600]) - min(chartDF$c_PL[301:600]))

      # scale the activity sensor data from 10 to 20 seconds
      activitySEScale <- 20 / (max(chartDF$c_SE[301:600]) - min(chartDF$c_SE[301:600]))
      
      ### scale the data for plotting
      
      chartDF$c_UPneumo <- chartDF$c_UPneumo * UPneumoScale
      chartDF$c_LPneumo <- chartDF$c_LPneumo * LPneumoScale
      chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid * UPneumoScale
      chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid * LPneumoScale
      chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh * UPneumoScale
      chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh * LPneumoScale
      chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh * UPneumoScale
      chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh * LPneumoScale
      
      chartDF$c_AutoEDA <- chartDF$c_AutoEDA * EDAScale
      chartDF$c_AutoEDAMid <- chartDF$c_AutoEDAMid * EDAScale
      chartDF$c_AutoEDAPeak <- chartDF$c_AutoEDAPeak * EDAScale
      chartDF$c_AutoEDABase <- chartDF$c_AutoEDABase * EDAScale
      
      chartDF$c_Cardio1 <- chartDF$c_Cardio1 * cardioScale
      chartDF$c_CardioMid <- chartDF$c_CardioMid * cardioScale
      chartDF$c_CardioMA <- chartDF$c_CardioMA * cardioScale
      chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic * cardioScale
      chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic * cardioScale
      
      if(inclPLE==TRUE) { 
        chartDF$c_PL <- chartDF$c_PL * PLScale
        chartDF$c_PLMA <- chartDF$c_PLMA * PLScale
        chartDF$c_PLMax <- chartDF$c_PLMax * PLScale
        chartDF$c_PLMin <- chartDF$c_PLMin * PLScale
        # chartDF$PLEMeans <- as.numeric(chartDF$PLEMeans) * PLScale
      } # end if to include PLE data
      
      chartDF$c_SE <- chartDF$c_SE * activitySEScale
      chartDF$c_SEMA <- chartDF$c_SEMA * activitySEScale
      chartDF$c_SEMin <- chartDF$c_SEMin * activitySEScale
      chartDF$c_SEMax <- chartDF$c_SEMax * activitySEScale
      
      ####################################### process the stimulus segments
      
      # make a vector of event names
      eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]

      # and a vector of event onset rows
      eventRows <- which(chartDF$eventLabel!="")
      
      if(getSegment == TRUE) { 
        if(segmentNum!="ALL") eventNames <- eventNames[segmentNum] 
      }
      
        # make the plot title to include the exam and chart name
        plotTitle <- paste(examName, chartName)
        
        ######
        
        ### Get the offset values 
        
        UPneumoOffset <- chartDF$c_UPneumo[firstEvent]
        LPneumoOffset <- chartDF$c_LPneumo[firstEvent]
        AutoEDAOffset <- chartDF$c_AutoEDA[firstEvent]
        # CardioMidOffset <- chartDF$c_CardioMid[firstEvent]
        # CardioDiastolicOffset <- chartDF$c_CardioDiastolicOffset[1]
        Cardio1Offset <- chartDF$c_CardioMid[firstEvent]
        if(inclPLE==TRUE) PLOffset <- chartDF$c_PLMA[firstEvent]
        ActivityOffset <- chartDF$c_SEMA[firstEvent]
        
        # set the y offset value for the plot
        yOffset <- c(135, 75, 10, -45, -110, -145)
        names(yOffset) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity")
        
        ### offset the data for plotting
        
        chartDF$c_UPneumo <- chartDF$c_UPneumo - UPneumoOffset + yOffset[1]
        chartDF$c_LPneumo <- chartDF$c_LPneumo - LPneumoOffset + yOffset[2]
        chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid - UPneumoOffset + yOffset[1]
        chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid - LPneumoOffset + yOffset[2]
        chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh - UPneumoOffset + yOffset[1]
        chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh - LPneumoOffset + yOffset[2]
        chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh - UPneumoOffset + yOffset[1]
        chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh - LPneumoOffset + yOffset[2]
        
        chartDF$c_AutoEDA <- chartDF$c_AutoEDA - AutoEDAOffset + yOffset[3]
        chartDF$c_AutoEDAMid <- chartDF$c_AutoEDAMid - AutoEDAOffset + yOffset[3]
        chartDF$c_AutoEDAPeak <- chartDF$c_AutoEDAPeak - AutoEDAOffset + yOffset[3]
        chartDF$c_AutoEDABase <- chartDF$c_AutoEDABase - AutoEDAOffset + yOffset[3]
        
        chartDF$c_Cardio1 <- chartDF$c_Cardio1 - Cardio1Offset + yOffset[4]
        chartDF$c_CardioMid <- chartDF$c_CardioMid - Cardio1Offset + yOffset[4]
        # use the Cardio1Offset to center the CardioMA data on the Cardio1 data
        chartDF$c_CardioMA <- chartDF$c_CardioMA - Cardio1Offset + yOffset[4]
        chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic - Cardio1Offset + yOffset[4]
        chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic - Cardio1Offset + yOffset[4]
        
        if(inclPLE==TRUE) { 
          chartDF$c_PL <- chartDF$c_PL - PLOffset + yOffset[5]
          chartDF$c_PLMA <- chartDF$c_PLMA - PLOffset + yOffset[5]
          chartDF$c_PLMax <- chartDF$c_PLMax - PLOffset + yOffset[5]
          chartDF$c_PLMin <- chartDF$c_PLMin - PLOffset + yOffset[5]
        } # end if for PLE
        
        chartDF$c_SE <- chartDF$c_SE - ActivityOffset + yOffset[6]
        chartDF$c_SEMA <- chartDF$c_SEMA - ActivityOffset + yOffset[6]
        chartDF$c_SEMin <- chartDF$c_SEMin - ActivityOffset + yOffset[6]
        chartDF$c_SEMax <- chartDF$c_SEMax - ActivityOffset + yOffset[6]
        
        # not used 4-2016 because auto recentering was added
        # re-offset the cardio data to keep the data on the plot
        # if(min(chartDF$c_Cardio1[firstEvent:eventRows[length(eventRows)]]) < -175) {
        #   newCardio1Offset <- abs(min(chartDF$c_Cardio1[firstEvent:nrow(chartDF)]) + 165)
        #   chartDF$c_Cardio1 <- chartDF$c_Cardio1 + newCardio1Offset
        #   chartDF$c_CardioMid <- chartDF$c_CardioMid + newCardio1Offset
        #   chartDF$c_CardioMA <- chartDF$c_CardioMA + newCardio1Offset
        #   chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic + newCardio1Offset
        #   chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic + newCardio1Offset
        # }
        
        # recenter the cardio data if it goes off the top or the bottom of the plot
        
        # start by setting the row number to recenter at the first
        reCenter <- 1
        # make a vector of reCentering events
        reCenterEvents <- NULL
        reCenterEventsDn <- NULL
        reCenterEventsUp <- NULL
        
        # use a while loop to recenter when data go off the top of chart
        # 175 is the max y value for the plot
        # -175 is the min y value 
        
        while(length(which(chartDF$c_Cardio1 >= 165 | chartDF$c_Cardio1 <= -165)) > 0) {
          reCenter <- min(which(chartDF$c_Cardio1 >= 165 | chartDF$c_Cardio1 <= -165))
          centerVal <- chartDF$c_Cardio1[min(which(chartDF$c_Cardio1 >= 165 | chartDF$c_Cardio1 <= -165))] - -45
          if(centerVal > 0) {
            reCenterEventsDn <- c(reCenterEventsDn, reCenter)
          } else reCenterEventsUp <- c(reCenterEventsUp, reCenter)
          chartDF$c_Cardio1[reCenter:length(chartDF$c_Cardio1)] <- chartDF$c_Cardio1[reCenter:length(chartDF$c_Cardio1)] - centerVal
          chartDF$c_CardioMA[reCenter:length(chartDF$c_CardioMA)] <- chartDF$c_CardioMA[reCenter:length(chartDF$c_CardioMA)] - centerVal
          chartDF$c_CardioMid[reCenter:length(chartDF$c_CardioMid)] <- chartDF$c_CardioMid[reCenter:length(chartDF$c_CardioMid)] - centerVal
          chartDF$c_CardioSystolic[reCenter:length(chartDF$c_CardioSystolic)] <- chartDF$c_CardioSystolic[reCenter:length(chartDF$c_CardioSystolic)] - centerVal
          chartDF$c_CardioDiastolic[reCenter:length(chartDF$c_CardioDiastolic)] <- chartDF$c_CardioDiastolic[reCenter:length(chartDF$c_CardioDiastolic)] - centerVal
          reCenterEvents <- c(reCenterEvents, reCenter)
        } # end while loop
        
        
        
        # cardioReCenterUp <- function(x=chartDF$c_Cardio1) {
        #   # function to recenter the cardio data if it goes off the bottom of the plot
        #   while(length(which(x <= -165))>0) {
        #     reCenter <- max(eventRows[(eventRows <= min(which(x <= -165)))])
        #     centerVal <- x[min(which(x <= -165))]
        #     reCenter <- reCenter - 0
        #     if(reCenter<1) reCenter <- 1
        #     x[reCenter:length(x)] <- x[reCenter:length(x)] - centerVal
        #     reCenterEventsUp <- c(reCenterEventsUp, reCenter)
        #     if(length(which(x >= 165))>0) {
        #       cardioReCenterDn(x=x)
        #     } # end if
        #   }
        #   return(x)
        # } # end cardioReCenterUp() function
        # 
        # chartDF$c_Cardio1 <- cardioReCenterDn()
        
        
        
        # while(length(which(chartDF$c_Cardio1 >= 165))>0) {
        #   # get the data row for the last stimulus onset prior to the data going off the chart
        #   reCenter <- max(eventRows[(eventRows <= min(which(chartDF$c_Cardio1 >= 165)))])
        #   # calculate the centering value to put the stim onset at the baseline
        #   centerVal <- chartDF$c_Cardio1[min(which(chartDF$c_Cardio1 >= 165))] - 0
        #   # offset the centering row to 4 seconds before stimulus onset
        #   reCenter <- reCenter - 120
        #   # fix centering prior to the first row
        #   if(reCenter<1) reCenter <- 1
        #   # and center the data from the centering row to end of chart
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] - centerVal
        #   # add the recenter event to the reCenterEvents vector for plotting
        #   reCenterEventsDn = c(reCenterEventsDn, reCenter)
        # }
        
        # while(length(which(chartDF$c_Cardio1 <= -175))>0) {
        #   reCenter <- max(eventRows[(eventRows <= min(which(chartDF$c_Cardio1 < -175)))])
        #   centerVal <- -45 - chartDF$c_Cardio1[reCenter]
        #   reCenter <- reCenter - 120
        #   if(reCenter<1) reCenter <- 1
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
        #   reCenterEventsUp = c(reCenterEventsUp, reCenter)
        # }
        
        
        
        # another while loop to recenter when data go off the bottom of chart
        # while(length(which(chartDF$c_Cardio1 <= -165))>0) {
        #   reCenter <- min(which(chartDF$c_Cardio1 < -165))
        #   centerVal <- 0 - chartDF$c_Cardio1[reCenter]
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
        #   reCenterEventsUp = c(reCenterEventsUp, reCenter)
        # }

        # and another while loop to recenter when data go off the top
        # while(length(which(chartDF$c_Cardio1 >= 175))>0) {
        #   reCenter <- min(which(chartDF$c_Cardio1 > 175))
        #   centerVal <- 0 - chartDF$c_Cardio1[reCenter]
        #   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
        #   reCenterEventsDn = c(reCenterEventsDn, reCenter)
        # }
          
        
       
        
        
   
        
        
        
        # re-offset the pneumo data to keep the data on the plot
        if(max(chartDF$c_UPneumo) > 175) {
          newPneumoOffset <- max(chartDF$c_UPneumo[1:(length(chartDF$c_UPneumo)-150)]) - 160
          chartDF$c_UPneumo <- chartDF$c_UPneumo - newPneumoOffset
          chartDF$c_LPneumo <- chartDF$c_LPneumo - newPneumoOffset
          # include the mid inhalation and exhalation trend lines
          chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid - newPneumoOffset
          chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid - newPneumoOffset
          chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh - newPneumoOffset
          chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh - newPneumoOffset
          chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh - newPneumoOffset
          chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh - newPneumoOffset
        }
        
        #########
        
        # remove NA rows if necessary for XX segments using times series data columns
        # chartDF <- chartDF[complete.cases(chartDF[,17:21]),]
        
        # save the data frame for analysis 
        # assign("chartDF", chartDF, pos=1)
        
        ##############################################################
        
        ##############################################################
        
        # make the plot
        g <- ggplot()
        # ggplot normally executes in the global environment
        
        ############ tracing baselines ##################
        
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c(yOffset)), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_hline(aes(yintercept=yOffset), color="brown", size=.15)
        
        #### Cardio recentering Events ####
          
          if(!is.null(reCenterEventsDn)) {
            g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsDn)), color="yellow")
          }
          if(!is.null(reCenterEventsUp)) {
            g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEventsUp)), color="green")
          }
          # if(!is.null(reCenterEvents)) {
          #   g <- g + geom_vline(aes(xintercept=as.numeric(reCenterEvents)), color="orange")
          # }
        
        ############ vertical lines for stimulus events
        
        eventIndices <- which(chartDF$Events!="")
        # for (l in 1:length(eventIndices)) {
          g <- g + geom_vline(aes(xintercept=as.numeric(eventIndices)))
        # }
        
#         # another way to make the stimulus event lines 
#         onsetRow <- which(chartDF$Events=="onsetRow")[1]
#         onsetRow <- segOnsetRow
#         g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
#         # EDA latency
#         # EDALatRow <- which(chartDF$Events=="onsetRow")[1]+(EDALat*cps)
#         EDALatRow <- onsetRow+(EDALat*cps)
#         g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="grey80")
#         # offset line
#         offsetRow <- which(chartDF$Events[onsetRow:nrow(chartDF)]=="offsetRow")[1] + onsetRow - 1
#         g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
#         # answer line
#         answerRow <- which(chartDF$Events[onsetRow:nrow(chartDF)]=="answerRow")[1] + onsetRow - 1
#         g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="black")
#         # end of response onset window
#         # ROWEndRow <- which(chartDF$Events[answerRow:nrow(chartDF)]=="answerRow")[1] + answerRow - 1 + (ROWEnd*cps) 
#         ROWEndRow <- answerRow + (ROWEnd*cps)
#         g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
#         # end of scoring window
#         # endRow <- which(chartDF$Events=="onsetRow")[1]+(measuredSeg*cps)
#         segEndRow <- onsetRow+(measuredSeg*cps) - 1
#         if(segEndRow > nrow(chartDF)) segEndRow <- (nrow(chartDF))
#         g <- g + geom_vline(aes(xintercept=as.numeric(segEndRow)), color="grey70")
        
        ############ shaded areas ####################
        
        stimOnset <- which(chartDF$Events=="onsetRow")
        stimOffset <- which(chartDF$Events=="offsetRow")
        answerRow <- which(chartDF$Events=="answerRow")
        
        # stimulus question shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(stimOnset),
                          xmax=as.numeric(stimOffset),
                          ymin=-175, 
                          ymax=175, 
                          alpha=.10, 
                          fill="grey10")
        
        # scoring window shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(stimOffset), 
                          xmax=as.numeric(stimOnset+(measuredSeg*cps)), 
                          ymin=-175, 
                          ymax=175, 
                          alpha=.10, 
                          fill="blue")

        # latency shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(stimOnset),
                          xmax=as.numeric(stimOnset+(cps*EDALat)),
                          ymin=-175,
                          ymax=175,
                          alpha=.125,
                          fill="red")
        
        # response onset window shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(stimOnset),
                          xmax=as.numeric(answerRow+(ROWEnd*cps)),
                          ymin=-150,
                          ymax=150,
                          alpha=.10,
                          fill="blue")
        
        ############ question labels###################
        
        questionLabels <- chartDF$eventLabel[which(chartDF$eventLabel!="")]
        g <- g + annotate(geom="text", 
                          x=which(chartDF$eventLabel!=""), 
                          y=-165, 
                          label=questionLabels,
                          color="black", 
                          size=4)

        ############ data segments ######################
        
        # upper pnuemo
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumo), color="blue1", size=.3) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # lower pneumo
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumo), color="blue4", size=.3) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # cardio
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Cardio1), color="red", size=.3) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMA), color="blue", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # eda
#         g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAPeak), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDABase), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))        
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDA), color="green4", size=.6) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAMid), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # photoplethysmograph
        if(inclPLE==TRUE) { 
#           g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#           g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PL), color="brown", size=.25) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        } # end if for PLE
        # seat activity sensor
#         g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SE), color="grey35", size=.5) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        
        # un-used sensor columns
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1), color="grey20")
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Aux02), color="grey20")
        
        ### Penumo artifacts
        
        ## uppper pneumo artifacts
        
        g <- g + annotate("point", x=which(chartDF$c_UPneumo_a=="Artifact"),
                          y=chartDF$c_UPneumo[which(chartDF$c_UPneumo_a=="Artifact")],
                          shape=4, size=3)
        # g <- g + annotate("point", x=which(chartDF$c_UPneumoInh_a=="Artifact"),
        #                   y=chartDF$c_UPneumo[which(chartDF$c_UPneumoInh_a=="Artifact")],
        #                   shape=4, size=3)
        # g <- g + annotate("point", x=which(chartDF$c_UPneumoExh_a=="Artifact"),
        #                   y=chartDF$c_UPneumo[which(chartDF$c_UPneumoExh_a=="Artifact")],
        #                   shape=4, size=2, color="red")
        # g <- g + annotate("point", x=which(chartDF$c_UPneumoMid_a=="Artifact"),
        #                   y=chartDF$c_UPneumo[which(chartDF$c_UPneumoMid_a=="Artifact")],
        #                   shape=4, size=.5, color="yellow")
        
        ## lower pneumo artifacts
        
        g <- g + annotate("point", x=which(chartDF$c_LPneumo_a=="Artifact"),
                          y=chartDF$c_LPneumo[which(chartDF$c_LPneumo_a=="Artifact")],
                          shape=4, size=3)
#         g <- g + annotate("point", x=which(chartDF$c_LPneumoInh_a=="Artifact"),
#                           y=chartDF$c_LPneumo[which(chartDF$c_LPneumoInh_a=="Artifact")],
#                           shape=4, size=2)
#         g <- g + annotate("point", x=which(chartDF$c_LPneumoExh_a=="Artifact"),
#                           y=chartDF$c_LPneumo[which(chartDF$c_LPneumoExh_a=="Artifact")],
#                           shape=4, size=1, color="red")
#         g <- g + annotate("point", x=which(chartDF$c_LPneumoMid_a=="Artifact"),
#                           y=chartDF$c_LPneumo[which(chartDF$c_LPneumoMid_a=="Artifact")],
#                           shape=4, size=.5, color="yellow")
        
  
  
        ### EDA artifacts
        
        g <- g + annotate("point", x=which(chartDF$c_AutoEDA_a=="Artifact"),
                          y=chartDF$c_AutoEDA[which(chartDF$c_AutoEDA_a=="Artifact")],
                          shape=4, size=1)
        
        
        
        ### cardio artifacts
        
#         g <- g + annotate("point", x=which(chartDF$c_CardioMid_a=="Artifact"),
#                               y=chartDF$c_CardioMid[which(chartDF$c_CardioMid_a=="Artifact")],
#                               shape=4, size=3)
#         g <- g + annotate("point", x=which(chartDF$c_Cardio1_a=="Artifact"),
#                           y=chartDF$c_Cardio1[which(chartDF$c_Cardio1_a=="Artifact")],
#                           shape=4, size=3)
        
        g <- g + annotate("point", x=which(chartDF$c_CardioDiastolic_a=="Artifact"),
                              y=chartDF$c_CardioDiastolic[which(chartDF$c_CardioDiastolic_a=="Artifact")],
                              shape=4, size=4)
#         g <- g + annotate("point", x=which(chartDF$c_CardioSystonlic_a=="Artifact"),
#                               y=chartDF$c_CardioSystolic[which(chartDF$c_CardioSystolic_a=="Artifact")],
#                               shape=4, size=3)
        
        
        
        ### activity sensor artifacts
        
        g <- g + annotate("point", x=which(chartDF$c_SEMA_a=="Artifact"),
                          y=chartDF$c_SE[which(chartDF$c_SEMA_a=="Artifact")],
                          shape=4, size=3, color="red") 
#         g <- g + annotate("point", x=which(chartDF$c_SEMax_a=="Artifact"),
#                           y=chartDF$c_SE[which(chartDF$c_SEMax_a=="Artifact")],
#                           shape=4, size=3 color="red") 
#         g <- g + annotate("point", x=which(chartDF$c_SEMin_a=="Artifact"),
#                           y=chartDF$c_SE[which(chartDF$c_SEMin_a=="Artifact")],
#                           shape=4, size=3 color="red") 
#         g <- g + annotate("point", x=which(chartDF$c_SE_a=="Artifact"),
#                           y=chartDF$c_SE[which(chartDF$c_SE_a=="Artifact")],
#                           shape=4, size=3, color="red") 
        
        
        
        
#         ############    measurements     #############

#         #        excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB", "OSN", "ISN")
#         # excludeEvents <- c(excludeEvents, "I1", "INT", "Int", "N1", "N2", "N3", "N4", "N7", "S", "SR", "Sy", "Sa", "SA", "SAC", "O")
#         # excludeEvents <- c(excludeEvents, "I1", "I2", "I3", "I4", "I7", "SY3", "SY8", "Sy3", "Sy8", "S3", "S8", "SR2", "S2")

#          giant if statement to plot measurements only for the measured segments
#         if(!(segmentName %in% excludeEvents)) {

          ### Pneumo measurement lines ###
        
        if(showMeasurements==TRUE) {
          
          # make a vector of response onset and end rows for upper adn lower pnuemo sensors
          responseOnsetPnUpper <- which(chartDF$UPneumoExtract=="responseOnsetRow")
          responseEndPnUpper <- responseOnsetPnUpper + measuredSeg*cps -1
          
          responseOnsetPnLower <- which(chartDF$LPneumoExtract=="responseOnsetRow")
          responseEndPnLower <- responseOnsetPnLower + measuredSeg*cps -1
          
          ### upper pneumo
          
          # make a bunch of data frames for each segment
          for (l in 1:length(responseOnsetPnUpper)) {
            DF <- as.data.frame(matrix(NA, nrow=450, ncol=2,dimnames=list(NULL,c("UPn", "Idx"))))
            DF$UPn <- chartDF$c_UPneumo[responseOnsetPnUpper[l]:responseEndPnUpper[l]]
            DF$Idx <- c(responseOnsetPnUpper[l]:responseEndPnUpper[l])
            assign(paste0("UPnDF", l), DF, pos=1)
          }
          
          # loop over the data frames
          # use the list in a loop
          # this is done using separate data frames for each segement
          # because ggplot has lazy evaluation and will evaluate only the last x indices
          UPneumoDFs <- ls(pattern="UPnDF", pos=1)
          for (m in 1:length(UPneumoDFs)) {
            g <- g + geom_line(data=get(UPneumoDFs[m], pos=1), aes(x=Idx, y=UPn), color="blue1", size=.75)
          }
          
          ### lower pneumo
          
          for (l in 1:length(responseOnsetPnLower)) {
            DF <- as.data.frame(matrix(NA, nrow=450, ncol=2,dimnames=list(NULL,c("LPn", "Idx"))))
            DF$LPn <- chartDF$c_LPneumo[responseOnsetPnLower[l]:responseEndPnLower[l]]
            DF$Idx <- c(responseOnsetPnLower[l]:responseEndPnLower[l])
            assign(paste0("LPnDF", l), DF, pos=1)
          }
          
          # loop over the data frames
          # use the list in a loop
          # this is done using separate data frames for each segement
          # because ggplot has lazy evaluation and will evaluate only the last x indices
          LPneumoDFs <- ls(pattern="LPnDF", pos=1)
          for (m in 1:length(LPneumoDFs)) {
            g <- g + geom_line(data=get(LPneumoDFs[m], pos=1), aes(x=Idx, y=LPn), color="blue4", size=.75)
          }
          
          ### clean up
          rm(list=ls(pattern="UPnDF"))
          rm(list=ls(pattern="LPnDF"))
          
        } # end if showMeasurements for Pneumo
          
        ### EDA measurement lines ###

        if(showMeasurements==TRUE) {
          
          responseOnsetXEDA <- which(chartDF$AutoEDAExtract == "responseOnsetRow")
          responseOnsetYEDA <- chartDF$c_AutoEDA[responseOnsetXEDA]
          responseEndXEDA <- which(chartDF$AutoEDAExtract == "responseEndRow")
          responseEndYEDA <- chartDF$c_AutoEDA[responseEndXEDA]
          # horizontal line begins at the end and points to the onset
          g <- g + annotate("segment",
                            x=responseEndXEDA,
                            xend=responseOnsetXEDA,
                            y=responseOnsetYEDA, 
                            yend=responseOnsetYEDA,
                            color="purple",
                            size=.6,
                            arrow=arrow(length=unit(0.25, "cm")))
          # vertical line
          g <- g + annotate("segment",
                            x=responseEndXEDA,
                            xend=responseEndXEDA,
                            y=responseOnsetYEDA, 
                            yend=responseEndYEDA,
                            color="purple",
                            size=.6,
                            arrow=arrow(length=unit(0.25, "cm")))
          
        } # end if showMeasurements for EDA
        
          ### Cardio measurement Lines ###

        if(showMeasurements==TRUE) {
          
          responseOnsetXCardio <- which(chartDF$CardioExtract == "responseOnsetRow")
          responseOnsetYCardio <- chartDF$c_CardioMid[responseOnsetXCardio]
          responseEndXCardio <- which(chartDF$CardioExtract == "responseEndRow")
          responseEndYCardio <- chartDF$c_CardioMid[responseEndXCardio]
          
          g <- g + annotate("segment",
                            x=responseEndXCardio,
                            xend=responseOnsetXCardio,
                            y=responseOnsetYCardio, 
                            yend=responseOnsetYCardio,
                            color="blue",
                            size=.6,
                            arrow=arrow(length=unit(0.25, "cm")))
          g <- g + annotate("segment",
                            x=responseEndXCardio,
                            xend=responseEndXCardio,
                            y=responseOnsetYCardio, 
                            yend=responseEndYCardio,
                            color="blue",
                            size=.6,
                            arrow=arrow(length=unit(0.25, "cm")))
          
        } # end if showMeasurements for Cardio
            
          ### PLE measurment ###

              if(inclPLE==TRUE) {

                if(showMeasurements==TRUE) {

                  if(!is.na(which(chartDF$PLEExtract=="prestimSegOnset")[1])) {
                    # prestim measurements
                    preOnX <- which(chartDF$PLEExtract=="prestimSegOnset")[1]
                    preOffX <- which(chartDF$PLEExtract=="prestimSegOffset")[1]
                    # preOnY <- chartDF$c_PLMA[(preOnX+45)] + ((as.numeric(chartDF$PLEMeans[preOnX]) * PLScale) / 2)
                    preOnY <- mean(chartDF$c_PLMax[preOnX:preOffX])
                    # preOffY <- chartDF$c_PLMA[(preOnX+45)] - ((as.numeric(chartDF$PLEMeans[preOffX]) * PLScale) / 2)
                    preOffY <- mean(chartDF$c_PLMin[preOnX:preOffX])
                    # poststim measurements
                    postOnX <- which(chartDF$PLEExtract=="poststimSegOnset")[1]
                    postOffX <- which(chartDF$PLEExtract=="poststimSegOffset")[1]
                    # postOnY <- chartDF$c_PLMA[(postOnX+45)] + ((as.numeric(chartDF$PLEMeans[postOnX]) * PLScale) / 2)
                    postOnY <- mean(chartDF$c_PLMax[postOnX:postOffX])
                    # postOffY <- chartDF$c_PLMA[(postOnX+45)] - ((as.numeric(chartDF$PLEMeans[postOffX])* PLScale) / 2)
                    postOffY <- mean(chartDF$c_PLMin[postOnX:postOffX])
                    # prestim shaded area
                    g <- g + annotate("rect",
                                      xmin=as.numeric(preOnX),
                                      xmax=as.numeric(preOffX),
                                      ymin=preOnY,
                                      ymax=preOffY,
                                      color="black",
                                      size=.3,
                                      alpha=.2,
                                      fill="green")
                    # stimulus shaded area
                    g <- g + annotate("rect",
                                      xmin=as.numeric(postOnX),
                                      xmax=as.numeric(postOffX),
                                      ymin=postOnY,
                                      ymax=postOffY,
                                      color="black",
                                      size=.3,
                                      alpha=.2,
                                      fill="green")

                  } # end if !is.na

                } # end if showMeasurements for PLE

              } # end if inclPLE

#         } # end giant if to plot measurements for measured segments
        
        ########### plot appearance
        
        g <- g + ylab("y-change")
        g <- g + xlab("x-time")
        
        g <- g + ggtitle(plotTitle)
        
        g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,nrow(chartDF),300))
        
        # print the chart to the graphics device
        print(g)
        
        #######################
        
    } # end iteration over k charts
    
  } # end iteration over j series 
  
  if(printPlot == TRUE) { 
    graphics.off()
    dev.new() } 
  
} # end iteration over i exams

if(showNames==TRUE) print(paste(length(uniqueExams), "exams processed"))

if(output==TRUE) return(examDF)




