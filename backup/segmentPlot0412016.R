# plot a stimulus segment recursively
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
# uniqueExams <- uniqueExams[11]



# parameters to describe the 
cps <- 30
prestimSeg <- 10
EDALat <- .5
CardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15
addSeg <- 10



# to control the print output
showNames <- TRUE
output <- FALSE
showMeasurements <- TRUE
outputFileName <- "_segmentPlot.pdf"
separateCharts <- FALSE
printPlot <- FALSE



# plot a single segment instead of iterating through all exams in the global environment
getSegment <- FALSE
examNum <- 1
seriesNum <- 2
chartNum <- 1
segmentNum <- 5


# to re-initialize the graphics device
# graphics.off()
# dev.new()



if(getSegment == TRUE) {
  assign("examName", uniqueExams[examNum])
  uniqueExams <- uniqueExams[examNum] 
} 



# loop over each exam in the list and plot the stimulus segments
for(i in 1:length(uniqueExams)) {
  # i=1
  examName <- uniqueExams[i]
  # get the names of time series lists for all unique series in each exam
  searchString <- paste0("*", examName, "_Data", "*")
  examDF <- get(glob2rx(searchString, trim.head=TRUE, trim.tail=TRUE), pos=1)
  
  if(getSegment==TRUE) {
    assign("examDF", examDF, pos=1)
    # break
  }
  
  if(showNames==TRUE) print(examName)
  
  if(printPlot == TRUE) {
    if(separateCharts==FALSE) {
      graphics.off()
      dev.new()
      pdf(paste(examName, outputFileName, sep="_"), height=5, width=3.5)  
    } else { 
      pdf(paste(examName, chartName, outputFileName, sep="_"), height=5, width=3.5)
    }
  }
  if(printPlot == FALSE) { 
    graphics.off()
    dev.new() 
  }
  
  examStartRow <- 1
  examEndRow <- nrow(examDF)
  
  ### add additional columns here
  
  # get the names of all unique series in the exam
  uniqueSeries <- as.character(unique(examDF$seriesName))
  
  if(getSegment == TRUE) { 
    if(seriesNum!="ALL") uniqueSeries <- uniqueSeries[seriesNum] 
    assign("uniqueSeries", uniqueSeries, pos=1)
    assign("seriesName", uniqueSeries, pos=1) 
  }
  
  # loop over each unique series
  for(j in 1:length(uniqueSeries)) {
    # j=1
    seriesName <- uniqueSeries[j]
    
    # get the list of time series data for the charts in the exam
    seriesDF <- examDF[examDF$seriesName==seriesName,]
    
    if(getSegment==TRUE) {
      assign("seriesDF", seriesDF, pos=1)
      # break
    }
    
    if(showNames==TRUE) print(paste("series", seriesName))
    
    seriesOnsetRow <- which(examDF$seriesName==seriesName)[1]
    seriesEndRow <- seriesOnsetRow + nrow(seriesDF) - 1
    
    # uniqueCharts <- names(seriesDF)
    uniqueCharts <- as.character(unique(seriesDF$chartName))
    
    if(getSegment == TRUE) {
      if(chartNum!="ALL") uniqueCharts <- uniqueCharts[chartNum]
      assign("uniqueCharts", uniqueCharts, pos=1)
      assign("chartName", uniqueCharts, pos=1) 
    }
    
    # loop over each chart in the series 
    for(k in 1:length(uniqueCharts)) {
      # k=1
      chartName <- uniqueCharts[k]
      
      # get the data frame with the time series data for each chart in the series
      chartDF <- seriesDF[seriesDF$chartName==chartName,]
      
      if(getSegment==TRUE) {
        assign("chartDF", chartDF, pos=1)
        # break
      }
      
      if(showNames==TRUE) print(chartName)
      
      #         if(printPlot == TRUE) {
      #           if(separateCharts==TRUE) {
      #           graphics.off()
      #           dev.new()
      #             pdf(paste(examName, chartName, "9-28-2015.pdf",sep="_"), height=5, width=7)  
      #           }
      #           
      #         } else {
      #           graphics.off()
      #           dev.new()
      #         }
      #         }
      
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

      # scale the EDA data
      
      EDAScale <- 170 / (max(na.omit(chartDF$c_AutoEDA)) - min(na.omit(chartDF$c_AutoEDA)))

      # scale the cardio pulse amplitude
      
      firstEvent <- which(chartDF$eventLabel!="")[2]
      outDiff <- NULL
      for (i in firstEvent:(firstEvent+149)) {
        outDiff <- c(outDiff, diff(range(chartDF$c_Cardio1[i:(i+29)])))
      }
      cardioScale <- 35 / mean(outDiff)

      # scale the PLE from 10 to 20 seconds
      
      inclPLE <- ifelse(sum(pmatch(names(chartDF), "c_PL", nomatch=0))>0, 
                        TRUE, 
                        FALSE) 
      if(inclPLE==TRUE) PLScale <- 35 / (max(chartDF$c_PL[301:600]) - min(chartDF$c_PL[301:600]))

      # scale the activity sensor data
      
      activitySEScale <- 30 / (max(chartDF$c_SE) - min(chartDF$c_SE))
      
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
      
      # a vector of event onset rows
      eventNames <- chartDF$eventLabel[chartDF$eventLabel!=""]
      eventRows <- which(chartDF$eventLabel!="")
      
      if(getSegment == TRUE) { 
        if(segmentNum!="ALL") eventNames <- eventNames[segmentNum] 
      }
      
      # loop over all the events in the chart data frame
      for (l in 1:length(eventNames)) {
        # l=1
        segmentName <- eventNames[l]
        # get the onset row  for the chart DF so that events during the prestimSegment are ignored
        segOnsetRow <- which(chartDF$eventLabel==segmentName)[1]
        # segOnsetRow <- eventRows[l] # does not work when getting a single segment
        # get the segment prestim row
        prestimRow <- segOnsetRow - prestimSeg*cps
        if(prestimRow < 1) prestimRow <- 1
        # set the end row so that the data frame
        segEndRow <- segOnsetRow + measuredSeg*cps - 1
        if(segEndRow > nrow(chartDF)) segEndRow <- nrow(chartDF)
        # set the row number for the end of the stimulus segment
        endRow <- segEndRow + addSeg*cps
        if(endRow > nrow(chartDF)) endRow <- nrow(chartDF)
        
        # get the segment data frame
        segmentDF <- chartDF[prestimRow:endRow,]
        
        if(getSegment==TRUE) {
          assign("segmentName", eventNames[l], pos=1) 
          assign("segmentDF", segmentDF, pos=1)
          # break
        }
      
        if(nrow(segmentDF) < 180) next
        
        if(showNames==TRUE) print(segmentName)
        
        #####
        
        # get the segment start row to save it for later
        startRow <- prestimRow
        
        # adjust the rows for the segmentDF
        prestimRow <- prestimRow - startRow + 1 # this will set the prestim row to 1
        segOnsetRow <- segOnsetRow - startRow + 1 # will normally set to 151
        segEndRow <- segEndRow - startRow + 1 # will normally set to 600
        
        # end of of the data frame segment
        endRow <- endRow - startRow + 1 # will normally be 750
        
        ##### now get the segment
        
        stimSegmentDF <- segmentDF
        
        ####################################
        
        #################################### 
        
        # make the plot using ggplot2
        
        # plotTitle <- paste(examName, chartName, segmentName)
        plotTitle <- paste(chartName, segmentName)
        
        ######
        
        ### Get the offset values 
        
        UPneumoOffset <- stimSegmentDF$c_UPneumo[1]
        LPneumoOffset <- stimSegmentDF$c_LPneumo[1]
        AutoEDAOffset <- stimSegmentDF$c_AutoEDA[1]
        # CardioMidOffset <- stimSegmentDF$c_CardioMid[1] # not used 1-23-2016
        # CardioDiastolicOffset <- stimSegmentDF$c_CardioDiastolicOffset[1]
        Cardio1Offset <- stimSegmentDF$c_CardioMid[1]
        if(inclPLE==TRUE) PLOffset <- stimSegmentDF$c_PLMA[1]
        ActivityOffset <- stimSegmentDF$c_SEMA[1]
        
        # set the y offset value for the plot
        yOffset <- c(135, 75, 10, -45, -110, -145)
        names(yOffset) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity")
        
        ### offset the data for plotting
        
        stimSegmentDF$c_UPneumo <- stimSegmentDF$c_UPneumo - UPneumoOffset + yOffset[1]
        stimSegmentDF$c_LPneumo <- stimSegmentDF$c_LPneumo - LPneumoOffset + yOffset[2]
        stimSegmentDF$c_UPneumoMid <- stimSegmentDF$c_UPneumoMid - UPneumoOffset + yOffset[1]
        stimSegmentDF$c_LPneumoMid <- stimSegmentDF$c_LPneumoMid - LPneumoOffset + yOffset[2]
        stimSegmentDF$c_UPneumoInh <- stimSegmentDF$c_UPneumoInh - UPneumoOffset + yOffset[1]
        stimSegmentDF$c_LPneumoInh <- stimSegmentDF$c_LPneumoInh - LPneumoOffset + yOffset[2]
        stimSegmentDF$c_UPneumoExh <- stimSegmentDF$c_UPneumoExh - UPneumoOffset + yOffset[1]
        stimSegmentDF$c_LPneumoExh <- stimSegmentDF$c_LPneumoExh - LPneumoOffset + yOffset[2]
        
        stimSegmentDF$c_AutoEDA <- stimSegmentDF$c_AutoEDA - AutoEDAOffset + yOffset[3]
        stimSegmentDF$c_AutoEDAMid <- stimSegmentDF$c_AutoEDAMid - AutoEDAOffset + yOffset[3]
        stimSegmentDF$c_AutoEDAPeak <- stimSegmentDF$c_AutoEDAPeak - AutoEDAOffset + yOffset[3]
        stimSegmentDF$c_AutoEDABase <- stimSegmentDF$c_AutoEDABase - AutoEDAOffset + yOffset[3]
        
        stimSegmentDF$c_Cardio1 <- stimSegmentDF$c_Cardio1 - Cardio1Offset + yOffset[4]
        # use the Cardio1Offset to center the CardioMA data on the Cardio1 data
        stimSegmentDF$c_CardioMid <- stimSegmentDF$c_CardioMid - Cardio1Offset + yOffset[4]
        stimSegmentDF$c_CardioMA <- stimSegmentDF$c_CardioMA - Cardio1Offset + yOffset[4]
        stimSegmentDF$c_CardioDiastolic <- stimSegmentDF$c_CardioDiastolic - Cardio1Offset + yOffset[4]
        stimSegmentDF$c_CardioSystolic <- stimSegmentDF$c_CardioSystolic - Cardio1Offset + yOffset[4]

        if(inclPLE==TRUE) { 
          stimSegmentDF$c_PL <- stimSegmentDF$c_PL - PLOffset + yOffset[5]
          stimSegmentDF$c_PLMA <- stimSegmentDF$c_PLMA - PLOffset + yOffset[5]
          stimSegmentDF$c_PLMax <- stimSegmentDF$c_PLMax - PLOffset + yOffset[5]
          stimSegmentDF$c_PLMin <- stimSegmentDF$c_PLMin - PLOffset + yOffset[5]
        } # end if for PLE

        stimSegmentDF$c_SE <- stimSegmentDF$c_SE - ActivityOffset + yOffset[6]
        stimSegmentDF$c_SEMA <- stimSegmentDF$c_SEMA - ActivityOffset + yOffset[6]
        stimSegmentDF$c_SEMin <- stimSegmentDF$c_SEMin - ActivityOffset + yOffset[6]
        stimSegmentDF$c_SEMax <- stimSegmentDF$c_SEMax - ActivityOffset + yOffset[6]
        
        # re-offset the cardio data to keep the data on the plot
        if(min(stimSegmentDF$c_Cardio1) < -175) {
          newCardio1Offset <- abs(min(stimSegmentDF$c_Cardio1[1:(length(stimSegmentDF$c_Cardio1)-150)]) + 655)
          stimSegmentDF$c_Cardio1 <- stimSegmentDF$c_Cardio1 + newCardio1Offset
          stimSegmentDF$c_CardioMid <- stimSegmentDF$c_CardioMid + newCardio1Offset
          stimSegmentDF$c_CardioMA <- stimSegmentDF$c_CardioMA + newCardio1Offset
          stimSegmentDF$c_CardioDiastolic <- stimSegmentDF$c_CardioDiastolic + newCardio1Offset
          stimSegmentDF$c_CardioSystolic <- stimSegmentDF$c_CardioSystolic + newCardio1Offset
        }
        
        # re-offset the pneumo data to keep the data on the plot
        if(max(stimSegmentDF$c_UPneumo) > 175) {
          newPneumoOffset <- max(stimSegmentDF$c_UPneumo[1:(length(stimSegmentDF$c_UPneumo)-150)]) - 165
          stimSegmentDF$c_UPneumo <- stimSegmentDF$c_UPneumo - newPneumoOffset
          stimSegmentDF$c_LPneumo <- stimSegmentDF$c_LPneumo - newPneumoOffset
          # include the mid inhalation and exhalation trend lines
          stimSegmentDF$c_UPneumoMid <- stimSegmentDF$c_UPneumoMid - newPneumoOffset
          stimSegmentDF$c_LPneumoMid <- stimSegmentDF$c_LPneumoMid - newPneumoOffset
          stimSegmentDF$c_UPneumoInh <- stimSegmentDF$c_UPneumoInh - newPneumoOffset
          stimSegmentDF$c_LPneumoInh <- stimSegmentDF$c_LPneumoInh - newPneumoOffset
          stimSegmentDF$c_UPneumoExh <- stimSegmentDF$c_UPneumoExh - newPneumoOffset
          stimSegmentDF$c_LPneumoExh <- stimSegmentDF$c_LPneumoExh - newPneumoOffset
        }
        
        # re-offset the activity sensor data to keep the data on the plot
#         if(min(stimSegmentDF$c_SE) < -175) {
#           newActivityOffset <- abs(min(stimSegmentDF$c_SE[1:(length(stimSegmentDF$c_SE)-150)]) + 175)
#           stimSegmentDF$c_SE <- stimSegmentDF$c_SE + newActivityOffset
#         }
        
        #########
        
        # remove NA rows if necessary for XX segments using times series data columns
        # stimSegmentDF <- stimSegmentDF[complete.cases(stimSegmentDF[,17:21]),]
        
        # save the data frame for analysis 
        # assign("stimSegmentDF", stimSegmentDF, pos=1)
        
        ###############################
        ###############################
        
        # make the plot
        g <- ggplot()
        # ggplot normally executes in the global environment
        
        ############ vertical lines
        
        # stimulus onset line
        # onsetRow <- which(stimSegmentDF$Events=="onsetRow")[1]
        onsetRow <- segOnsetRow
        g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
        # EDA latency
        # EDALatRow <- which(stimSegmentDF$Events=="onsetRow")[1]+(EDALat*cps)
        EDALatRow <- onsetRow+(EDALat*cps)
        g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="grey80")
        # offset line
        offsetRow <- which(stimSegmentDF$Events[onsetRow:nrow(stimSegmentDF)]=="offsetRow")[1] + onsetRow - 1
        g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
        # answer line
        answerRow <- which(stimSegmentDF$Events[onsetRow:nrow(stimSegmentDF)]=="answerRow")[1] + onsetRow - 1
        g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="black")
        # end of response onset window
        # ROWEndRow <- which(stimSegmentDF$Events[answerRow:nrow(stimSegmentDF)]=="answerRow")[1] + answerRow - 1 + (ROWEnd*cps) 
        ROWEndRow <- answerRow + (ROWEnd*cps)
        g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
        # end of scoring window
        # endRow <- which(stimSegmentDF$Events=="onsetRow")[1]+(measuredSeg*cps)
        segEndRow <- onsetRow+(measuredSeg*cps) - 1
        if(segEndRow > nrow(stimSegmentDF)) segEndRow <- (nrow(stimSegmentDF))
        g <- g + geom_vline(aes(xintercept=as.numeric(segEndRow)), color="grey70")
        
        ############ shaded areas
        
        # scoring window shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(onsetRow), 
                          xmax=as.numeric(segEndRow), 
                          ymin=-175, 
                          ymax=175, 
                          alpha=.12, 
                          fill="blue")
        # stimulus question shaded area
        g <- g + annotate("rect", 
                          xmin=as.numeric(onsetRow),
                          xmax=as.numeric(offsetRow),
                          ymin=-135, 
                          ymax=135, 
                          alpha=.3, 
                          fill="grey20")
        # latency shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(onsetRow),
                          xmax=as.numeric(onsetRow+15),
                          ymin=-175,
                          ymax=175,
                          alpha=.125,
                          fill="red")
        # response onset window shaded area
        g <- g + annotate("rect",
                          xmin=as.numeric(offsetRow),
                          xmax=as.numeric(ROWEndRow),
                          ymin=-150,
                          ymax=150,
                          alpha=.125,
                          fill="blue")
        
        ############
        
        # data segments
        
        # upper pnuemo
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_UPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_UPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_UPneumo), color="blue1", size=.25) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_UPneumoMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # lower pneumo
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_LPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_LPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_LPneumo), color="blue4", size=.25) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_LPneumoMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # eda
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_AutoEDAPeak), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_AutoEDABase), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))        
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_AutoEDA), color="green4", size=.33) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_AutoEDAMid), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # cardio
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_CardioDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_CardioSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_Cardio1), color="red", size=.25) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_CardioMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        # g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_CardioMA), color="blue", size=.15) + coord_cartesian(ylim=c(-175, 175))
        
        # photoplethysmograph
        if(inclPLE==TRUE) { 
#           g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_PLMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#           g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_PLMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_PL), color="brown", size=.25) + coord_cartesian(ylim=c(-175, 175))
          g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_PLMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        } # end if for PLE
        # seat activity sensor
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_SEMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_SEMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_SE), color="grey35", size=.25) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_SEMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        
        # un-used sensor columns
        # g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_Move1), color="grey20")
        # g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_Aux02), color="grey20")
        
        ########### artifacts
        
        ### Penumo artifacts

        g <- g + annotate("point", x=which(stimSegmentDF$c_UPneumo_a=="Artifact"),
                          y=stimSegmentDF$c_UPneumo[which(stimSegmentDF$c_UPneumo_a=="Artifact")],
                          shape=4, size=3)
#         g <- g + annotate("point", x=which(stimSegmentDF$c_UPneumoInh_a=="Artifact"),
#                           y=stimSegmentDF$c_UPneumo[which(stimSegmentDF$c_UPneumoInh_a=="Artifact")],
#                           shape=4, size=3)
#         g <- g + annotate("point", x=which(stimSegmentDF$c_UPneumoExh_a=="Artifact"),
#                           y=stimSegmentDF$c_UPneumo[which(stimSegmentDF$c_UPneumoExh_a=="Artifact")],
#                           shape=4, size=3)
#         g <- g + annotate("point", x=which(stimSegmentDF$c_UPneumoMid_a=="Artifact"),
#                           y=stimSegmentDF$c_UPneumo[which(stimSegmentDF$c_UPneumoMid_a=="Artifact")],
#                           shape=4, size=3)
        
        g <- g + annotate("point", x=which(stimSegmentDF$c_LPneumo_a=="Artifact"),
                          y=stimSegmentDF$c_LPneumo[which(stimSegmentDF$c_LPneumo_a=="Artifact")],
                          shape=4, size=3)
#         g <- g + annotate("point", x=which(stimSegmentDF$c_LPneumoInh_a=="Artifact"),
#                           y=stimSegmentDF$c_LPneumo[which(stimSegmentDF$c_LPneumoInh_a=="Artifact")],
#                           shape=4, size=3)
#         g <- g + annotate("point", x=which(stimSegmentDF$c_LPneumoExh_a=="Artifact"),
#                           y=stimSegmentDF$c_LPneumo[which(stimSegmentDF$c_LPneumoExh_a=="Artifact")],
#                           shape=4, size=3)
#         g <- g + annotate("point", x=which(stimSegmentDF$c_LPneumoMid_a=="Artifact"),
#                           y=stimSegmentDF$c_LPneumo[which(stimSegmentDF$c_LPneumoMid_a=="Artifact")],
#                           shape=4, size=3)
        
        #         g <- g + annotate("point", x=which(stimSegmentDF$c_UPneumo_a=="Artifact"),
        #                           y=stimSegmentDF$c_UPneumo[which(stimSegmentDF$c_UPneumo_a=="Artifact")],
        #                           shape=4, size=1)
        #         g <- g+ annotate("point", x=which(stimSegmentDF$c_Pneumo_a=="Artifact"),
        #                          y=stimSegmentDF$c_LPneumo[which(stimSegmentDF$c_LPneumo_a=="Artifact")],
        #                          shape=4, size=1)
        
        # buffer around the pneumo artifacts
        
        halfBuffer <- round(1.5*cps,0)
        
        UPneumoArtifacts <- which(stimSegmentDF$c_UPneumo_a=="Artifact")
        UPArtifactBuffXOn <- UPneumoArtifacts[which(UPneumoArtifacts>halfBuffer & UPneumoArtifacts<nrow(stimSegmentDF)-halfBuffer)] - halfBuffer
        
        UPArtifactBuffXOff <- UPneumoArtifacts[which(UPneumoArtifacts>halfBuffer & UPneumoArtifacts<nrow(stimSegmentDF)-halfBuffer)] + halfBuffer
        UPArtifactBuffYOn <- stimSegmentDF$c_UPneumo[UPArtifactBuffXOn]
        UPArtifactBuffYOff <- stimSegmentDF$c_UPneumo[UPArtifactBuffXOff]
        g <- g + annotate("segment",
                          x=UPArtifactBuffXOn,
                          xend=UPArtifactBuffXOff,
                          y=UPArtifactBuffYOn,
                          yend=UPArtifactBuffYOff,
                          color="black",
                          linetype="solid",
                          size=.75)
        
        LPneumoArtifacts <- which(stimSegmentDF$c_LPneumo_a=="Artifact")
        LPArtifactBuffXOn <- LPneumoArtifacts[which(LPneumoArtifacts>halfBuffer & LPneumoArtifacts<nrow(stimSegmentDF)-halfBuffer)] - halfBuffer
        
        LPArtifactBuffXOff <- LPneumoArtifacts[which(LPneumoArtifacts>halfBuffer & LPneumoArtifacts<nrow(stimSegmentDF)-halfBuffer)] + halfBuffer
        LPArtifactBuffYOn <- stimSegmentDF$c_LPneumo[LPArtifactBuffXOn]
        LPArtifactBuffYOff <- stimSegmentDF$c_LPneumo[LPArtifactBuffXOff]
        g <- g + annotate("segment",
                          x=LPArtifactBuffXOn,
                          xend=LPArtifactBuffXOff,
                          y=LPArtifactBuffYOn,
                          yend=LPArtifactBuffYOff,
                          color="black",
                          linetype="solid",
                          size=.75)
        
        
        
        # EDA artifacts
        
        g <- g + annotate("point", x=which(stimSegmentDF$c_AutoEDA_a=="Artifact"),
                          y=stimSegmentDF$c_AutoEDA[which(stimSegmentDF$c_AutoEDA_a=="Artifact")],
                          shape=4, size=1)
        
        
        
        
        
        
        # cardio artifacts
        
        #         g <- g + annotate("point", x=which(stimSegmentDF$c_CardioMid_a=="Artifact"),
        #                               y=stimSegmentDF$c_CardioMid[which(stimSegmentDF$c_CardioMid_a=="Artifact")],
        #                               shape=4, size=3)
        #         g <- g + annotate("point", x=which(stimSegmentDF$c_Cardio1_a=="Artifact"),
        #                           y=stimSegmentDF$c_Cardio1[which(stimSegmentDF$c_Cardio1_a=="Artifact")],
        #                           shape=4, size=3)
        
        g <- g + annotate("point", x=which(stimSegmentDF$c_CardioDiastolic_a=="Artifact"),
                          y=stimSegmentDF$c_CardioDiastolic[which(stimSegmentDF$c_CardioDiastolic_a=="Artifact")],
                          shape=4, size=4)
        #         g <- g + annotate("point", x=which(stimSegmentDF$c_CardioSystonlic_a=="Artifact"),
        #                               y=stimSegmentDF$c_CardioSystolic[which(stimSegmentDF$c_CardioSystolic_a=="Artifact")],
        #                               shape=4, size=3)
        
        
        # PLE artifacts
        
        
        
        # chair activitiy sensor artifacts
        
                g <- g + annotate("point", x=which(stimSegmentDF$c_SEMA_a=="Artifact"),
                                  y=stimSegmentDF$c_SE[which(stimSegmentDF$c_SEMA_a=="Artifact")],
                                  shape=4, size=3, color="red") 
        #         g <- g + annotate("point", x=which(stimSegmentDF$c_SEMax_a=="Artifact"),
        #                           y=stimSegmentDF$c_SE[which(stimSegmentDF$c_SEMax_a=="Artifact")],
        #                           shape=4, size=3, color="red") 
        #         g <- g + annotate("point", x=which(stimSegmentDF$c_SEMin_a=="Artifact"),
        #                           y=stimSegmentDF$c_SE[which(stimSegmentDF$c_SEMin_a=="Artifact")],
        #                           shape=4, size=3, color="red") 
        #         g <- g + annotate("point", x=which(stimSegmentDF$c_SE_a=="Artifact"),
        #                           y=stimSegmentDF$c_SE[which(stimSegmentDF$c_SE_a=="Artifact")],
        #                           shape=4, size=3, color="red") 
        
        
        

        ############    measurements     ######
        
       excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB", "OSN", "ISN")
#        excludeEvents <- c(excludeEvents, "I1", "INT", "Int", "INT1", "Int1", "N1", "N2", "N3", "N4", "N7", "S", "SR", "Sy", "Sa", "SA", "SAC", "O")
#        excludeEvents <- c(excludeEvents, "I1", "I2", "I3", "I4", "I7", "SY3", "SY8", "Sy3", "Sy8", "S3", "S8", "SR2", "S2")
        
        # giant if statement to plot measurements only for the measured segments
        if(!(segmentName %in% excludeEvents)) {
          
          # Pneumo measurement lines
          
          if(showMeasurements==TRUE) {
            if(!is.na(which(stimSegmentDF$UPneumoExtract=="responseOnsetRow")[1])) {
              g <- g + geom_line(data=stimSegmentDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_UPneumo), color="blue1", size=1.25)
              g <- g + geom_line(data=stimSegmentDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_LPneumo), color="blue3", size=1.25)
              
          # commented out 4/5/2016 not needed when artifact buffers are used    
          #     #### buffer around the verbal response is not included in the measurement
          #     # upper pneumo answer buffer
          #     aBuffXOnU <- which(stimSegmentDF$UPneumoExtract[onsetRow:nrow(stimSegmentDF)]=="aBuffOn")[1] + onsetRow - 1
          #     aBuffXOffU <- which(stimSegmentDF$UPneumoExtract[onsetRow:nrow(stimSegmentDF)]=="aBuffOff")[1] + onsetRow - 1
          #     aBuffYOnU <- stimSegmentDF$c_UPneumo[aBuffXOnU][1]
          #     aBuffYOffU <- stimSegmentDF$c_UPneumo[aBuffXOffU][1]
          #     g <- g + annotate("segment",
          #                       x=aBuffXOnU,
          #                       xend=aBuffXOffU,
          #                       y=aBuffYOnU,
          #                       yend=aBuffYOffU,
          #                       color="black",
          #                       linetype="solid",
          #                       size=1.25)
          #     # lower pneumo answer buffer
          #     aBuffXOnL <- which(stimSegmentDF$LPneumoExtract[onsetRow:nrow(stimSegmentDF)]=="aBuffOn")[1] + onsetRow - 1
          #     aBuffXOffL <- which(stimSegmentDF$LPneumoExtract[onsetRow:nrow(stimSegmentDF)]=="aBuffOff")[1] + onsetRow - 1
          #     aBuffYOnL <- stimSegmentDF$c_LPneumo[aBuffXOnU][1]
          #     aBuffYOffL <- stimSegmentDF$c_LPneumo[aBuffXOffU][1]
          #     g <- g + annotate("segment",
          #                       x=aBuffXOnL,
          #                       xend=aBuffXOffL,
          #                       y=aBuffYOnL,
          #                       yend=aBuffYOffL,
          #                       color="black",
          #                       linetype="solid",
          #                       size=1.25)
          #     
          #   } # end if !is.na for pneumo response onset
          # } # end if showMeasurements for Pneumo
          
          ### EDA measurement lines
          
          if(showMeasurements==TRUE) {
            if(!is.na(which(stimSegmentDF$AutoEDAExtract[segOnsetRow:nrow(stimSegmentDF)]=="responseOnsetRow")[1])) {
              EDAxOn <- segOnsetRow - 1 + which(stimSegmentDF$AutoEDAExtract[segOnsetRow:nrow(stimSegmentDF)]=="responseOnsetRow")[1]
              if(!is.na(EDAxOn)) { if(EDAxOn > ROWEndRow) EDAxOn <- NA }
              if(is.na(EDAxOn)) { EDAyOn <- NA; EDAxOff <- NA; EDAyOff<- NA }
              if(!is.na(EDAxOn)) EDAxOff <- EDAxOn - 1 + which(stimSegmentDF$AutoEDAExtract[EDAxOn:nrow(stimSegmentDF)]=="responseEndRow")[1]
              if(!is.na(EDAxOn)) EDAyOn <- stimSegmentDF$c_AutoEDA[EDAxOn]
              if(!is.na(EDAxOn)) EDAyOff <- stimSegmentDF$c_AutoEDA[EDAxOff]
              if(!is.na(EDAxOn)) {
                # horzontal EDA segment measurement
                g <- g + annotate("segment",
                                  x=EDAxOff,
                                  xend=EDAxOn,
                                  y=EDAyOn,
                                  yend=EDAyOn,
                                  color="purple",
                                  size=.6,
                                  arrow=arrow(length=unit(0.4, "cm")))
                # vertical EDA segment
                # geom_segment and annotate are two different ways of adding the lines
                # annotate is probably better
                g <- g + annotate("segment",
                                  x=EDAxOff,
                                  xend=EDAxOff,
                                  y=EDAyOn,
                                  yend=EDAyOff,
                                  color="purple",
                                  size=.6,
                                  arrow=arrow(length=unit(0.4, "cm")))
              }
            } # end if !is.na for EDA response onset
          } # end if showMeasurements for EDA 
          
          ## EDA descent stop
          
          # g <- g + geom_point()
          
          ### Cardio measurement Lines
          
          if(showMeasurements==TRUE) {
            if(!is.na(which(stimSegmentDF$CardioExtract=="responseOnsetRow")[1])) {
              cardioXOn <- segOnsetRow - 1 + which(stimSegmentDF$CardioExtract[segOnsetRow:nrow(stimSegmentDF)]=="responseOnsetRow")[1]
              if(!is.na(cardioXOn)) { if(cardioXOn > ROWEndRow) cardioXOn <- NA }
              # if(is.na(cardioXOn)) { cardioYOn <- NA; cardioXOff <- NA; cardioYOff <- NA }
              if(!is.na(cardioXOn)) cardioXOff <- cardioXOn - 1 + which(stimSegmentDF$CardioExtract[cardioXOn:nrow(stimSegmentDF)]=="responseEndRow")[1]
              if(!is.na(cardioXOn)) cardioYOn <- stimSegmentDF$c_CardioMid[cardioXOn]
              if(!is.na(cardioXOn)) cardioYOff <- stimSegmentDF$c_CardioMid[cardioXOff]
              if(!is.na(cardioXOn)) {
                # if(cardioXOn <= ROWEndRow) {
                # horzontal Cardio segment measurement
                g <- g + annotate("segment",
                                  x=cardioXOff,
                                  xend=cardioXOn,
                                  y=cardioYOn,
                                  yend=cardioYOn,
                                  color="blue",
                                  size=.6,
                                  arrow=arrow(length=unit(0.4, "cm")))
                # vertical Cardio segment
                # geom_segment and annotate are two different ways of adding the lines
                # annotate is probably better
                g <- g + annotate("segment",
                                  x=cardioXOff,
                                  xend=cardioXOff,
                                  y=cardioYOn,
                                  yend=cardioYOff,
                                  color="blue",
                                  size=.6,
                                  arrow=arrow(length=unit(0.4, "cm")))
                # } # end if
              } # end cardio segment measurements
            } # end if !is.na
          } # end if showMeasurements for Cardio
          
          ### PLE measurment
          
              if(inclPLE==TRUE) { 
                if(showMeasurements==TRUE) {
                  if(!is.na(which(stimSegmentDF$PLEExtract=="prestimSegOnset")[1])) {
                    # prestim measurements
                    preOnX <- which(stimSegmentDF$PLEExtract=="prestimSegOnset")[1]
                    preOffX <- which(stimSegmentDF$PLEExtract=="prestimSegOffset")[1]
                    # preOnY <- stimSegmentDF$c_PLMA[(preOnX+45)] + ((as.numeric(stimSegmentDF$PLEMeans[preOnX]) * PLScale) / 2)
                    preOnY <- mean(stimSegmentDF$c_PLMax[preOnX:preOffX])
                    # preOffY <- stimSegmentDF$c_PLMA[(preOnX+45)] - ((as.numeric(stimSegmentDF$PLEMeans[preOffX]) * PLScale) / 2)
                    preOffY <- mean(stimSegmentDF$c_PLMin[preOnX:preOffX])
                    # poststim measurements
                    postOnX <- which(stimSegmentDF$PLEExtract=="poststimSegOnset")[1]
                    postOffX <- which(stimSegmentDF$PLEExtract=="poststimSegOffset")[1]
                    # postOnY <- stimSegmentDF$c_PLMA[(postOnX+45)] + ((as.numeric(stimSegmentDF$PLEMeans[postOnX]) * PLScale) / 2)
                    postOnY <- mean(stimSegmentDF$c_PLMax[postOnX:postOffX])
                    # postOffY <- stimSegmentDF$c_PLMA[(postOnX+45)] - ((as.numeric(stimSegmentDF$PLEMeans[postOffX])* PLScale) / 2)
                    postOffY <- mean(stimSegmentDF$c_PLMin[postOnX:postOffX])
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
          
        } # end giant if to plot measurements for measured segments
        
        ########### plot appearance
        
        g <- g + ylab("y-change")
        g <- g + xlab("x-time")
        
        g <- g + ggtitle(plotTitle) + 
         theme(plot.title = element_text(size=8, color="#666666", face="bold", hjust=0))
        
        g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,nrow(stimSegmentDF),150))
        
        # save the chart to a list
        # outputList[[j]] <- g
        
        # print the chart to the graphics device
        print(g)
        
        #######################
        
      } # end loop over l event segments in each chart
      
      # save the chartDF to the seriesDF
      # seriesDF[chartOnsetRow:(nrow(chartDF)+chartOnsetRow-1),] <- chartDF
      
    } # end iteration over k charts
    
    # save the seriesDF to the examDF
    # examDF[seriesOnsetRow:(seriesOnsetRow+nrow(seriesDF)-1),] <- seriesDF 
    
  } # end iteration over j series 
  
  # save the examDF to the global environment 
  # assign(paste0(examName, "_Data"), examDF, pos=1) 
  
  if(printPlot == TRUE) { 
    graphics.off()
    dev.new() } 
  
} # end iteration over i exams

if(showNames==TRUE) print(paste(length(uniqueExams), "exams processed"))

if(output==TRUE) return(examDF)




