# plot a stimulus segment recursively
#
# sampling rate is 30cps
# x input is a vector of time series data
# x length is from stimulus onset to 15 seconds after stimulus onset
# measurement is the sum of absolute differences between successive samples 
#
################################



library(stringr)
library(ggplot2)
library(grid)

# source the list of excluded events so that measurements are not plotted for these
source('~/Documents/R_programming/NCCA_ASCII_Parse/excludedEvents.R')



# get exam names from the _Data data frames
uniqueExams <- unique(str_sub(ls(pattern="*_Data$", pos=1),1, -6))

uniqueExams <- uniqueExams[1]



# parameters to describe the 
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
examNum <- 23
seriesNum <- 1
chartNum <- 1
segmentNum <- 3





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
      pdf(paste(examName, outputFileName, sep="_"), height=5, width=8)  
    } else { 
      pdf(paste(examName, chartName, outputFileName, sep="_"), height=5, width=8)
    }
  }
  if(printPlot == FALSE) { 
    graphics.off()
    dev.new(height=5, width=9) 
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
      # scale the cardio pulse amplitude
      firstEvent <- which(chartDF$eventLabel!="")[2]
      outDiff <- NULL
      for (m in firstEvent:(firstEvent+149)) {
        outDiff <- c(outDiff, diff(range(chartDF$c_Cardio1[m:(m+29)])))
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
      
        #################################### taken from the segmentPlot.R script
        
        # make the plot using ggplot2
        
        plotTitle <- paste(examName, chartName)
        
        ######
        
        ### Get the offset values 
        
        UPneumoOffset <- chartDF$c_UPneumo[firstEvent]
        LPneumoOffset <- chartDF$c_LPneumo[firstEvent]
        AutoEDAOffset <- chartDF$c_AutoEDA[firstEvent]
        CardioMAOffset <- chartDF$c_CardioMA[firstEvent]
        # CardioDiastolicOffset <- chartDF$c_CardioDiastolicOffset[1]
        Cardio1Offset <- chartDF$c_CardioMA[firstEvent]
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
        
        
        # re-offset the cardio data to keep the data on the plot
        if(min(chartDF$c_Cardio1[firstEvent:nrow(chartDF)]) < -175) {
          newCardio1Offset <- abs(min(chartDF$c_Cardio1[firstEvent:nrow(chartDF)]) + 175)
          chartDF$c_Cardio1 <- chartDF$c_Cardio1 + newCardio1Offset
          chartDF$c_CardioMA <- chartDF$c_CardioMA + newCardio1Offset
          chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic + newCardio1Offset
          chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic + newCardio1Offset
        }
        
        # re-offset the pneumo data to keep the data on the plot
        if(max(chartDF$c_UPneumo) > 175) {
          newPneumoOffset <- max(chartDF$c_UPneumo[1:(length(chartDF$c_UPneumo)-150)]) - 170
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
        
        # re-offset the activity sensor data to keep the data on the plot
        #         if(min(chartDF$c_SE) < -175) {
        #           newActivityOffset <- abs(min(chartDF$c_SE[1:(length(chartDF$c_SE)-150)]) + 175)
        #           chartDF$c_SE <- chartDF$c_SE + newActivityOffset
        #         }
        
        #########
        
        # remove NA rows if necessary for XX segments using times series data columns
        chartDF <- chartDF[complete.cases(chartDF[,17:21]),]
        
        # save the data frame for analysis 
        # assign("chartDF", chartDF, pos=1)
        
        ##############################################################
        
        ##############################################################
        
        # make the plot
        g <- ggplot()
        # ggplot normally executes in the global environment
        
        ############ vertical lines for stimulus events
        
        eventIndices <- which(chartDF$Events!="")
        # for (l in 1:length(eventIndices)) {
          g <- g + geom_vline(aes(xintercept=as.numeric(eventIndices)))
        # }
        
        
        # stimulus onset line
        # onsetRow <- which(chartDF$Events=="onsetRow")[1]
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
        
        ############ shaded areas
        
        stimOnset <- which(chartDF$Events=="onsetRow")
        stimOffset <- which(chartDF$Events=="offsetRow")
        g <- g + annotate("rect", 
                          xmin=as.numeric(stimOnset),
                          xmax=as.numeric(stimOffset),
                          ymin=-175, 
                          ymax=175, 
                          alpha=.10, 
                          fill="grey10")

        
#         # scoring window shaded area
#         g <- g + annotate("rect", 
#                           xmin=as.numeric(onsetRow), 
#                           xmax=as.numeric(segEndRow), 
#                           ymin=-175, 
#                           ymax=175, 
#                           alpha=.12, 
#                           fill="blue")
#         # stimulus question shaded area
#         g <- g + annotate("rect", 
#                           xmin=as.numeric(onsetRow),
#                           xmax=as.numeric(offsetRow),
#                           ymin=-135, 
#                           ymax=135, 
#                           alpha=.3, 
#                           fill="grey20")
#         # latency shaded area
#         g <- g + annotate("rect",
#                           xmin=as.numeric(onsetRow),
#                           xmax=as.numeric(onsetRow+15),
#                           ymin=-175,
#                           ymax=175,
#                           alpha=.125,
#                           fill="red")
#         # response onset window shaded area
#         g <- g + annotate("rect",
#                           xmin=as.numeric(offsetRow),
#                           xmax=as.numeric(ROWEndRow),
#                           ymin=-150,
#                           ymax=150,
#                           alpha=.125,
#                           fill="blue")
        
        ############
        
        # add the question labels
        
        g <- g + annotate(geom="text", x=which(chartDF$eventLabel!=""), y=-165, label=chartDF$eventLabel[which(chartDF$eventLabel!="")],
                 color="black", size=4)

        #         chartDF$eventLabel[which(chartDF$eventLabel!="")]
        
        ############
        
        # data segments
        
        # upper pnuemo
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumoMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_UPneumo), color="blue1", size=.3) + coord_cartesian(ylim=c(-175, 175))
        # lower pneumo
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoInh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoExh), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumoMid), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_LPneumo), color="blue4", size=.3) + coord_cartesian(ylim=c(-175, 175))
        # cardio
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioDiastolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioSystolic), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_CardioMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Cardio1), color="red", size=.3) + coord_cartesian(ylim=c(-175, 175))
        # eda
#         g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAPeak), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#         g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDABase), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))        
#         g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDAMid), color="brown", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_AutoEDA), color="green4", size=.6) + coord_cartesian(ylim=c(-175, 175))
#         # photoplethysmograph
#         if(inclPLE==TRUE) { 
#           g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#           g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
#           g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PLMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
#           g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_PL), color="brown", size=.25) + coord_cartesian(ylim=c(-175, 175))
#         } # end if for PLE
        # seat activity sensor
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMin), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMax), color="grey60", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SEMA), color="black", size=.15) + coord_cartesian(ylim=c(-175, 175))
        g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_SE), color="grey35", size=.5) + coord_cartesian(ylim=c(-175, 175))
        
        # un-used sensor columns
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Move1), color="grey20")
        # g <- g + geom_line(data=chartDF, aes(x=(1:nrow(chartDF)), y=c_Aux02), color="grey20")
        
        ### Penumo artifacts
        
        g <- g + annotate("point", x=which(chartDF$c_UPneumoInh_a=="Artifact"),
                          y=chartDF$c_UPneumo[which(chartDF$c_UPneumoInh_a=="Artifact")],
                          shape=4, size=3)
        g <- g + annotate("point", x=which(chartDF$c_UPneumoExh_a=="Artifact"),
                          y=chartDF$c_UPneumo[which(chartDF$c_UPneumoExh_a=="Artifact")],
                          shape=4, size=3)
        g <- g + annotate("point", x=which(chartDF$c_UPneumoMid_a=="Artifact"),
                          y=chartDF$c_UPneumoMid[which(chartDF$c_UPneumoMid_a=="Artifact")],
                          shape=4, size=3)
        
        g <- g + annotate("point", x=which(chartDF$c_LPneumoInh_a=="Artifact"),
                          y=chartDF$c_LPneumo[which(chartDF$c_LPneumoInh_a=="Artifact")],
                          shape=4, size=3)
        g <- g + annotate("point", x=which(chartDF$c_LPneumoExh_a=="Artifact"),
                          y=chartDF$c_LPneumo[which(chartDF$c_LPneumoExh_a=="Artifact")],
                          shape=4, size=3)
        g <- g + annotate("point", x=which(chartDF$c_LPneumoMid_a=="Artifact"),
                          y=chartDF$c_LPneumoMid[which(chartDF$c_LPneumoMid_a=="Artifact")],
                          shape=4, size=3)
        
        
        
        ### cardio artifacts
        
#         g <- g + annotate("point", x=which(chartDF$c_CardioMA_a=="Artifact"),
#                               y=chartDF$c_CardioMA[which(chartDF$c_CardioMA_a=="Artifact")],
#                               shape=4, size=3)
#         g <- g + annotate("point", x=which(chartDF$c_Cardio1_a=="Artifact"),
#                           y=chartDF$c_Cardio1[which(chartDF$c_Cardio1_a=="Artifact")],
#                           shape=4, size=3)
        
        g <- g + annotate("point", x=which(chartDF$c_CardioDiastolic_a=="Artifact"),
                              y=chartDF$c_CardioDiastolic[which(chartDF$c_CardioDiastolic_a=="Artifact")],
                              shape=4, size=3)
#         g <- g + annotate("point", x=which(chartDF$c_CardioSystonlic_a=="Artifact"),
#                               y=chartDF$c_CardioSystolic[which(chartDF$c_CardioSystolic_a=="Artifact")],
#                               shape=4, size=3)
        
        
        ### activity sensor artifacts
        
        g <- g + annotate("point", x=which(chartDF$c_SEMin_a=="Artifact"),
                          y=chartDF$c_SEMin[which(chartDF$c_SEMin_a=="Artifact")],
                          shape=4, size=3) 
        
        
        
        
        
#         ############    measurements     ######
#         
#         #        excludeEvents <- c("BI", "SW", "X", "XX", "WRQ", "RS", "TI", "EI", "EE", "MV", "MVT", "MI", "CA", "AI", "TDB", "SLP", "WU", "CA", "OS", "OTH", "B", "T", "C", "Y", "BN", "SNF", "CT", "LGH", "DB", "OSN", "ISN")
#         # excludeEvents <- c(excludeEvents, "I1", "INT", "Int", "N1", "N2", "N3", "N4", "N7", "S", "SR", "Sy", "Sa", "SA", "SAC", "O")
#         # excludeEvents <- c(excludeEvents, "I1", "I2", "I3", "I4", "I7", "SY3", "SY8", "Sy3", "Sy8", "S3", "S8", "SR2", "S2")
#         
#         # giant if statement to plot measurements only for the measured segments
#         if(!(segmentName %in% excludeEvents)) {
#           
#           # Pneumo measurement lines
#           
#           if(showMeasurements==TRUE) {
#             if(!is.na(which(chartDF$UPneumoExtract=="responseOnsetRow")[1])) {
#               g <- g + geom_line(data=chartDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_UPneumo), color="blue1", size=1.25)
#               g <- g + geom_line(data=chartDF[onsetRow:segEndRow,], aes(x=onsetRow:segEndRow, y=c_LPneumo), color="blue3", size=1.25)
#               
#               # buffer around the verbal response is not included in the measurement
#               # upper pneumo answer buffer
#               aBuffXOnU <- which(chartDF$UPneumoExtract[onsetRow:nrow(chartDF)]=="aBuffOn")[1] + onsetRow - 1
#               aBuffXOffU <- which(chartDF$UPneumoExtract[onsetRow:nrow(chartDF)]=="aBuffOff")[1] + onsetRow - 1
#               aBuffYOnU <- chartDF$c_UPneumo[aBuffXOnU][1]
#               aBuffYOffU <- chartDF$c_UPneumo[aBuffXOffU][1] 
#               g <- g + annotate("segment",
#                                 x=aBuffXOnU,
#                                 xend=aBuffXOffU,
#                                 y=aBuffYOnU,
#                                 yend=aBuffYOffU,
#                                 color="black",
#                                 linetype="solid",
#                                 size=1.25)
#               # lower pneumo answer buffer
#               aBuffXOnL <- which(chartDF$LPneumoExtract[onsetRow:nrow(chartDF)]=="aBuffOn")[1] + onsetRow - 1
#               aBuffXOffL <- which(chartDF$LPneumoExtract[onsetRow:nrow(chartDF)]=="aBuffOff")[1] + onsetRow - 1
#               aBuffYOnL <- chartDF$c_LPneumo[aBuffXOnU][1]
#               aBuffYOffL <- chartDF$c_LPneumo[aBuffXOffU][1] 
#               g <- g + annotate("segment",
#                                 x=aBuffXOnL,
#                                 xend=aBuffXOffL,
#                                 y=aBuffYOnL,
#                                 yend=aBuffYOffL,
#                                 color="black",
#                                 linetype="solid",
#                                 size=1.25)
#             } # end if !is.na for pneumo response onset
#           } # end if showMeasurements for Pneumo
#           
#           ### EDA measurement lines

        responseOnsetXEDA <- which(chartDF$AutoEDAExtract == "responseOnsetRow")
        responseOnsetYEDA <- chartDF$c_AutoEDA[responseOnsetXEDA]
        responseEndXEDA <- which(chartDF$AutoEDAExtract == "responseEndRow")
        responseEndYEDA <- chartDF$c_AutoEDA[responseEndXEDA]
        g <- g + annotate("segment",
                          x=responseEndXEDA,
                          xend=responseOnsetXEDA,
                          y=responseOnsetYEDA, 
                          yend=responseOnsetYEDA,
                          color="purple",
                          size=.6,
                          arrow=arrow(length=unit(0.25, "cm")))
        g <- g + annotate("segment",
                          x=responseEndXEDA,
                          xend=responseEndXEDA,
                          y=responseOnsetYEDA, 
                          yend=responseEndYEDA,
                          color="purple",
                          size=.6,
                          arrow=arrow(length=unit(0.25, "cm")))
        
        
        
        #           
#           if(showMeasurements==TRUE) {
#             if(!is.na(which(chartDF$AutoEDAExtract[segOnsetRow:nrow(chartDF)]=="responseOnsetRow")[1])) {
#               EDAxOn <- segOnsetRow - 1 + which(chartDF$AutoEDAExtract[segOnsetRow:nrow(chartDF)]=="responseOnsetRow")[1]
#               if(!is.na(EDAxOn)) { if(EDAxOn > ROWEndRow) EDAxOn <- NA }
#               if(is.na(EDAxOn)) { EDAyOn <- NA; EDAxOff <- NA; EDAyOff<- NA }
#               if(!is.na(EDAxOn)) EDAxOff <- EDAxOn - 1 + which(chartDF$AutoEDAExtract[EDAxOn:nrow(chartDF)]=="responseEndRow")[1]
#               if(!is.na(EDAxOn)) EDAyOn <- chartDF$c_AutoEDA[EDAxOn]
#               if(!is.na(EDAxOn)) EDAyOff <- chartDF$c_AutoEDA[EDAxOff]
#               if(!is.na(EDAxOn)) {

#                 # horzontal EDA segment measurement
#                 g <- g + annotate("segment",
#                                   x=EDAxOff,
#                                   xend=EDAxOn,
#                                   y=EDAyOn,
#                                   yend=EDAyOn,
#                                   color="purple",
#                                   size=.6,
#                                   arrow=arrow(length=unit(0.4, "cm")))

#                 # vertical EDA segment
#                 # geom_segment and annotate are two different ways of adding the lines
#                 # annotate is probably better
#                 g <- g + annotate("segment",
#                                   x=EDAxOff,
#                                   xend=EDAxOff,
#                                   y=EDAyOn,
#                                   yend=EDAyOff,
#                                   color="purple",
#                                   size=.6,
#                                   arrow=arrow(length=unit(0.4, "cm")))

        
#               }
#             } # end if !is.na for EDA response onset
#           } # end if showMeasurements for EDA 
#           
#           ## EDA descent stop
#           
#           # g <- g + geom_point()
#           
        
        
        
          ### Cardio measurement Lines

        

  
              responseOnsetXCardio <- which(chartDF$CardioExtract == "responseOnsetRow")
              responseOnsetYCardio <- chartDF$c_CardioMA[responseOnsetXCardio]
              responseEndXCardio <- which(chartDF$CardioExtract == "responseEndRow")
              responseEndYCardio <- chartDF$c_CardioMA[responseEndXCardio]

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
  

            
              
#           
#           if(showMeasurements==TRUE) {
#             if(!is.na(which(chartDF$CardioExtract=="responseOnsetRow")[1])) {
#               cardioXOn <- segOnsetRow - 1 + which(chartDF$CardioExtract[segOnsetRow:nrow(chartDF)]=="responseOnsetRow")[1]
#               if(!is.na(cardioXOn)) { if(cardioXOn > ROWEndRow) cardioXOn <- NA }
#               # if(is.na(cardioXOn)) { cardioYOn <- NA; cardioXOff <- NA; cardioYOff <- NA }
#               if(!is.na(cardioXOn)) cardioXOff <- cardioXOn - 1 + which(chartDF$CardioExtract[cardioXOn:nrow(chartDF)]=="responseEndRow")[1]
#               if(!is.na(cardioXOn)) cardioYOn <- chartDF$c_CardioMA[cardioXOn]
#               if(!is.na(cardioXOn)) cardioYOff <- chartDF$c_CardioMA[cardioXOff]
#               if(!is.na(cardioXOn)) {
#                 # if(cardioXOn <= ROWEndRow) {
#                 # horzontal Cardio segment measurement
#                 g <- g + annotate("segment",
#                                   x=cardioXOff,
#                                   xend=cardioXOn,
#                                   y=cardioYOn,
#                                   yend=cardioYOn,
#                                   color="blue",
#                                   size=.6,
#                                   arrow=arrow(length=unit(0.4, "cm")))
#                 # vertical Cardio segment
#                 # geom_segment and annotate are two different ways of adding the lines
#                 # annotate is probably better
#                 g <- g + annotate("segment",
#                                   x=cardioXOff,
#                                   xend=cardioXOff,
#                                   y=cardioYOn,
#                                   yend=cardioYOff,
#                                   color="blue",
#                                   size=.6,
#                                   arrow=arrow(length=unit(0.4, "cm")))
#                 # } # end if
#               } # end cardio segment measurements
#             } # end if !is.na
#           } # end if showMeasurements for Cardio
#           
#           ### PLE measurment
#           
#           if(inclPLE==TRUE) { 
#             if(showMeasurements==TRUE) {
#               if(!is.na(which(chartDF$PLEExtract=="prestimSegOnset")[1])) {
#                 # prestim measurements
#                 preOnX <- which(chartDF$PLEExtract=="prestimSegOnset")[1]
#                 preOffX <- which(chartDF$PLEExtract=="prestimSegOffset")[1]
#                 # preOnY <- chartDF$c_PLMA[(preOnX+45)] + ((as.numeric(chartDF$PLEMeans[preOnX]) * PLScale) / 2)
#                 preOnY <- mean(chartDF$c_PLMax[preOnX:preOffX])
#                 # preOffY <- chartDF$c_PLMA[(preOnX+45)] - ((as.numeric(chartDF$PLEMeans[preOffX]) * PLScale) / 2)
#                 preOffY <- mean(chartDF$c_PLMin[preOnX:preOffX])
#                 # poststim measurements
#                 postOnX <- which(chartDF$PLEExtract=="poststimSegOnset")[1]
#                 postOffX <- which(chartDF$PLEExtract=="poststimSegOffset")[1]
#                 # postOnY <- chartDF$c_PLMA[(postOnX+45)] + ((as.numeric(chartDF$PLEMeans[postOnX]) * PLScale) / 2)
#                 postOnY <- mean(chartDF$c_PLMax[postOnX:postOffX])
#                 # postOffY <- chartDF$c_PLMA[(postOnX+45)] - ((as.numeric(chartDF$PLEMeans[postOffX])* PLScale) / 2)
#                 postOffY <- mean(chartDF$c_PLMin[postOnX:postOffX])
#                 # prestim shaded area
#                 g <- g + annotate("rect", 
#                                   xmin=as.numeric(preOnX), 
#                                   xmax=as.numeric(preOffX), 
#                                   ymin=preOnY, 
#                                   ymax=preOffY, 
#                                   color="black",
#                                   size=.3,
#                                   alpha=.2, 
#                                   fill="green")
#                 # stimulus shaded area
#                 g <- g + annotate("rect", 
#                                   xmin=as.numeric(postOnX), 
#                                   xmax=as.numeric(postOffX), 
#                                   ymin=postOnY, 
#                                   ymax=postOffY, 
#                                   color="black",
#                                   size=.3,
#                                   alpha=.2, 
#                                   fill="green")
#                 
#               } # end if !is.na
#             } # end if showMeasurements for PLE
#           } # end if inclPLE
#           
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




