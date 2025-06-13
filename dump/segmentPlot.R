# R script to plot a chart segment using ggplot2
#
# will iterate over a vector of names of lists that contain
# the data frames for the time series segment for each stimulus question
#
# all information for the plot is contained in the data frame
# 
# cps is a scalar in the global environment because ggplot executes in the global environment
# ROWEnd is also a scalar in the global env
# EDAlatency is also a scalar
# prestimSeg is also a scalar
# ROWEnd is a scalar
#
#################################


library(ggplot2) # need to load grid library for arrows
library(grid) # for ggplot2 arrows


cps <- 30
prestimSeg <- 5
EDALat <- .5
cardioLat <- .5
ROWEnd <- 5
measuredSeg <- 15


##############

# iterate and make polots over a list 
# of exams, series, charts, and stimulus segments

mySegmentLists <- ls(pattern="*_dataSegmentList$")

mySegmentLists <- mySegmentLists[1]

####

printPlot <- FALSE

# iterate over the names of data segment lists
# i=2
for (i in 1:length(mySegmentLists)) {
  # get the name of the segment list
  segmentListName <- mySegmentLists[i]
  
  # get the data segment list
  segmentList <- get(segmentListName, pos=1)
  
  # save the names of the stimulus segments
  segmentNames <- names(segmentList)
  
  # make an empty output list to save the chart
  outputList <- list()
  
  # iterate over the data frames in each data segment list
  # j=3
  for (j in 1:length(segmentList)) {  
    
    stimSegmentDF <- segmentList[[j]]
    
    ##########################
    
    # make the plot using ggplot2
    
    # assign the stim segment to the global environment so that ggplot can find it
    # assign("stimSegmentDF", stimSegmentDF, pos=1)
    
    examName <- unique(as.character(stimSegmentDF$examName))[1]
    seriesName <- unique(as.character(stimSegmentDF$seriesName))[1]
    chartName <- unique(as.character(stimSegmentDF$chartName))[1]
    stimulusName <- stimSegmentDF$Label[stimSegmentDF$Events=="onsetRow"][1]
    
    # will get the wrong lable if started within 5 seconds of the previous stimulus
    # stimSegmentDF$Label[which(stimSegmentDF$Events=="offsetRow")]
    
    plotTitle <- paste(examName, chartName, stimulusName)

    ###
    
    # get the scaling values
    UPneumoScale <- 40 / (max(stimSegmentDF$c_UPneumoS[1:300]) - min(stimSegmentDF$c_UPneumoS[1:150]))
    LPneumoScale <- 40 / (max(stimSegmentDF$c_LPneumoS[1:300]) - min(stimSegmentDF$c_LPneumoS[1:150]))
    EDAScale <- 100 / (max(na.omit(stimSegmentDF$c_AutoEDA)) - min(na.omit(stimSegmentDF$c_AutoEDA)))
    # scale the cardio pulse amplitude using the first 4 seconds
    outDiff <- NULL
    for (i in 1:121) {
      outDiff <- c(outDiff, diff(range(stimSegmentDF$c_Cardio1[i:(i+29)])))
    }
    cardioScale <- 50 / mean(outDiff)
    
    # scale the data for plotting
    stimSegmentDF$c_UPneumoS <- stimSegmentDF$c_UPneumoS * UPneumoScale
    stimSegmentDF$c_LPneumoS <- stimSegmentDF$c_LPneumoS * LPneumoScale
    stimSegmentDF$c_AutoEDA <- stimSegmentDF$c_AutoEDA * EDAScale
    stimSegmentDF$c_Cardio1 <- stimSegmentDF$c_Cardio1 * cardioScale
    stimSegmentDF$c_CardioMA <- stimSegmentDF$c_CardioMA * cardioScale
    stimSegmentDF$c_CardioDiastolic <- stimSegmentDF$c_CardioDiastolic * cardioScale
    stimSegmentDF$c_CardioSystolic <- stimSegmentDF$c_CardioSystolic * cardioScale
    
    # offset the data for plotting
    yOffset <- c(125, 50, 0, -75)
    UPneumoOffset <- stimSegmentDF$c_UPneumoS[1]
    LPneumoOffset <- stimSegmentDF$c_LPneumoS[1]
    AutoEDAOffset <- stimSegmentDF$c_AutoEDA[1]
    CardioMAOffset <- stimSegmentDF$c_CardioMA[1]
#    CardioDiastolicOffset <- stimSegmentDF$c_CardioDiastolicOffset[1]
    Cardio1Offset <- stimSegmentDF$c_Cardio1[1]
    stimSegmentDF$c_UPneumoS <- stimSegmentDF$c_UPneumoS - UPneumoOffset + yOffset[1]
    stimSegmentDF$c_LPneumoS <- stimSegmentDF$c_LPneumoS - LPneumoOffset + yOffset[2]
    stimSegmentDF$c_AutoEDA <- stimSegmentDF$c_AutoEDA - AutoEDAOffset + yOffset[3]
    stimSegmentDF$c_Cardio1 <- stimSegmentDF$c_Cardio1 - Cardio1Offset + yOffset[4]
    # use the Cardio1Offset to center the CardioMA data on the Cardio1 data
    stimSegmentDF$c_CardioMA <- stimSegmentDF$c_CardioMA - Cardio1Offset + yOffset[4]
    stimSegmentDF$c_CardioDiastolic <- stimSegmentDF$c_CardioDiastolic - Cardio1Offset + yOffset[4]
    stimSegmentDF$c_CardioSystolic <- stimSegmentDF$c_CardioSystolic - Cardio1Offset + yOffset[4]
    
    # remove NA rows if necessary for XX segments using times series data columns
    stimSegmentDF <- stimSegmentDF[complete.cases(stimSegmentDF[,17:21]),]
    
    # make the plot
    g <- ggplot()
    # ggplot normally executes in the global environment

    ### vertical lines 
    # onset line
    onsetRow <- which(stimSegmentDF$Events=="onsetRow")[1]
    g <- g + geom_vline(aes(xintercept=as.numeric(onsetRow)))
    # EDA latency
    EDALatRow <- which(stimSegmentDF$Events=="onsetRow")[1]+(EDALat*cps)
    g <- g + geom_vline(aes(xintercept=as.numeric(EDALatRow)), color="grey80")
    # offset line
    offsetRow <- which(stimSegmentDF$Events=="offsetRow")[1]
    g <- g + geom_vline(aes(xintercept=as.numeric(offsetRow)))
    # answer line
    answerRow <- which(stimSegmentDF$Events=="answerRow")[1]
    g <- g + geom_vline(aes(xintercept=as.numeric(answerRow)), color="black")
    # end of response onset window
    ROWEndRow <- which(stimSegmentDF$Events=="answerRow")[1]+(ROWEnd*cps)
    g <- g + geom_vline(aes(xintercept=as.numeric(ROWEndRow)), color="grey80")
    # end of scoring window
    endRow <- which(stimSegmentDF$Events=="onsetRow")[1]+(measuredSeg*cps)
    if(endRow > nrow(stimSegmentDF)) endRow <- (nrow(stimSegmentDF))
    g <- g + geom_vline(aes(xintercept=as.numeric(endRow)), color="grey70")
    
    ### shaded areas
    # scoring window shaded area
    g <- g + annotate("rect", 
                      xmin=as.numeric(onsetRow), 
                      xmax=as.numeric(endRow), 
                      ymin=-150, 
                      ymax=150, 
                      alpha=.1, 
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
                   ymin=-135,
                   ymax=135,
                   alpha=.125,
                   fill="red")
    # response onset window shaded area
    g <- g + annotate("rect",
                      xmin=as.numeric(offsetRow),
                      xmax=as.numeric(ROWEndRow),
                      ymin=-135,
                      ymax=135,
                      alpha=.11,
                      fill="blue")

    # data segments
    g <- g + geom_line(data=na.omit(stimSegmentDF), aes(x=(1:nrow(na.omit(stimSegmentDF))), y=c_AutoEDA), color="green4", size=.75) + coord_cartesian(ylim=c(-175, 175))
    g <- g + geom_line(data=na.omit(stimSegmentDF), aes(x=(1:nrow(na.omit(stimSegmentDF))), y=c_UPneumoS), color="blue1") + coord_cartesian(ylim=c(-175, 175))
    g <- g + geom_line(data=na.omit(stimSegmentDF), aes(x=(1:nrow(na.omit(stimSegmentDF))), y=c_LPneumoS), color="blue4") + coord_cartesian(ylim=c(-175, 175))
    g <- g + geom_line(data=na.omit(stimSegmentDF), aes(x=(1:nrow(na.omit(stimSegmentDF))), y=c_CardioMA), color="black") + coord_cartesian(ylim=c(-175, 175))
    g <- g + geom_line(data=na.omit(stimSegmentDF), aes(x=(1:nrow(na.omit(stimSegmentDF))), y=c_CardioDiastolic), color="grey80") + coord_cartesian(ylim=c(-175, 175))
    g <- g + geom_line(data=na.omit(stimSegmentDF), aes(x=(1:nrow(na.omit(stimSegmentDF))), y=c_CardioSystolic), color="grey80") + coord_cartesian(ylim=c(-175, 175))
    g <- g + geom_line(data=na.omit(stimSegmentDF), aes(x=(1:nrow(na.omit(stimSegmentDF))), y=c_Cardio1), color="red") + coord_cartesian(ylim=c(-175, 175))
    
    # g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_Move1), color="grey20")
    # g <- g + geom_line(data=stimSegmentDF, aes(x=(1:nrow(stimSegmentDF)), y=c_Aux02), color="grey20")
    
    # Pneumo measurement lines
    
    g <- g + geom_line(data=stimSegmentDF[onsetRow:endRow,], aes(x=onsetRow:endRow, y=c_UPneumoS), color="blue1", size=1.25)
    g <- g + geom_line(data=stimSegmentDF[onsetRow:endRow,], aes(x=onsetRow:endRow, y=c_LPneumoS), color="blue3", size=1.25)
    # buffer around the verbal response is not included in the measurement
    # upper pneumo answer buffer
    aBuffXOnU <- which(stimSegmentDF$UPneumoExtract=="aBuffOn")[1]
    aBuffXOffU <- which(stimSegmentDF$UPneumoExtract=="aBuffOff")[1]
    aBuffYOnU <- stimSegmentDF$c_UPneumoS[aBuffXOnU][1]
    aBuffYOffU <- stimSegmentDF$c_UPneumoS[aBuffXOffU][1] 
    g <- g + annotate("segment",
                      x=aBuffXOnU,
                      xend=aBuffXOffU,
                      y=aBuffYOnU,
                      yend=aBuffYOffU,
                      color="black",
                      linetype="solid",
                      size=1.25)
    # lower pneumo answer buffer
    aBuffXOnL <- which(stimSegmentDF$LPneumoExtract=="aBuffOn")[1]
    aBuffXOffL <- which(stimSegmentDF$LPneumoExtract=="aBuffOff")[1]
    aBuffYOnL <- stimSegmentDF$c_LPneumoS[aBuffXOnU][1]
    aBuffYOffL <- stimSegmentDF$c_LPneumoS[aBuffXOffU][1] 
    g <- g + annotate("segment",
                      x=aBuffXOnL,
                      xend=aBuffXOffL,
                      y=aBuffYOnL,
                      yend=aBuffYOffL,
                      color="black",
                      linetype="solid",
                      size=1.25)

    ### Penumo artifacts

    g <- g + annotate("point", x=which(stimSegmentDF$UPneumoArtifacts=="Artifact"),
                                    y=stimSegmentDF$c_UPneumoS[which(stimSegmentDF$UPneumoArtifacts=="Artifact")],
                                    shape=4, size=2)
    g <- g+ annotate("point", x=which(stimSegmentDF$LPneumoArtifacts=="Artifact"),
                      y=stimSegmentDF$c_LPneumoS[which(stimSegmentDF$LPneumoArtifacts=="Artifact")],
                      shape=4, size=2)

    ### EDA measurement lines
    
    EDAxOn <- which(stimSegmentDF$AutoEDAExtract=="responseOnsetRow")
    EDAxOff <- which(stimSegmentDF$AutoEDAExtract=="responseEndRow")
    EDAyOn <- stimSegmentDF$c_AutoEDA[EDAxOn]
    EDAyOff <- stimSegmentDF$c_AutoEDA[EDAxOff]

    # if(EDAxOn <= ROWEndRow) {
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
    # } 

    ## EDA descent stop

    # g <- g + geom_point()

#     ### Cardio measurement Lines
    
    cardioXOn <- which(stimSegmentDF$CardioExtract=="responseOnsetRow")
    cardioXOff <- which(stimSegmentDF$CardioExtract=="responseEndRow")
    cardioYOn <- stimSegmentDF$c_CardioMA[cardioXOn]
    cardioYOff <- stimSegmentDF$c_CardioMA[cardioXOff]
    
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
    # }
    
    ### plot appearance

    g <- g + ylab("y-change")
    g <- g + xlab("x-time")
    
    g <- g + ggtitle(plotTitle)
    
    g <- g + theme_bw() + scale_x_continuous(breaks=seq(0,900,150))
    
    # save the chart to a list
    # outputList[[j]] <- g
    
    # print the chart to the graphics device
    if(printPlot == TRUE) pdf(paste(examName, "1 8-19-2015.pdf"), height=5, width=7)  
    print(g)

    
    
    # dev.off()
    
    ###########################
    
  } # end iteration over data frames in the segment list
  
  # names(outputList) <- segmentNames
  
} # end iteration over names of data segment lists

if(printPlot == TRUE) { dev.off() }


########


# # print the ouput to a pdf
# library(gridExtra)
# 
# pdf("plots.pdf", onefile = TRUE)
# for (i in seq(length(outputList))) {
#   do.call("print", outputList[[i]])  
# }
# dev.off()
# 
# 
# print(outputList)

####

# to print a list of ggplot objects from a list onto a page 
# require(gridExtra); do.call("grid.arrange",p[[i]])

