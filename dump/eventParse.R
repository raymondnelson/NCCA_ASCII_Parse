############# R Function to parse NCCA ASCII header information ##############
# 6-12-2015 Raymond Nelson 7-28-2015
# 
#
#
# this script contains the following functions
#
# eventParse()
# to identify charts with min 2 CQs and min 2 RQs
# will also make a warning message if each event is not present in all charts
#
#########################################


# library(stringr)


#########################################

# function to identify and keep charts that have 2 CQs and 2 RQs 
# and verify that all stimulus events 


# x="_Stimuli$"
# saveCSV=FALSE
# makeDF=TRUE
# type="CQT"


eventParse <- function(x="_Stimuli$") {
  # function to identify and keep charts that have 2 CQs and 2 RQs 
  # and verify that all stimulus events 
  # x is a regex string to identify the stimuli data frames
  # with 1 data frame with all event stimili for all charts for each exam
  #
  # will also make a warning message if each event is not present in all charts
  #
  # type can be CQT, CIT, SPOT, RI, ACQT or NULL
  #
  # output is a data frame, assigned to the global env with all event labels for all charts
  ####
  
  # first make a vector of names of event stimuli data frames
  stimDFs <- ls(pattern=x, pos=1)
  
  # proceed only if there are any event stimuli
  if(length(stimDFs) == 0) stop("no stimulus data frame")
    
    #### loop over the events ("*_Stimuli") for each exam
    # i=1
    for (i in 1:length(stimDFs)) {
      print(stimDFs[i])
      # create an empty warning message
      
      # work with one of the exam stimuli data frames
      examDF <- get(stimDFs[i])
      # str(workingDF)
      
      # get the exam name
      # examName <- substr(stimDFs[i], start=1, stop=nchar(stimDFs[i])-8)
      examName <- unique(examDF$examName)[1]
      
      # get the chart names
      # uniqueCharts <- unique(as.character(examDF[,3]))
      # uniqueCharts <- unique(as.character(examDF[,'chartName']))
      # length(uniqueCharts)
      
      # get the numbers of the unique series
      # uniqueSeriesNames <- unique(substr(uniqueCharts, start=1, stop=1))
      # uniqueSeriesNames <- unique(as.character(examDF[,2]))
      uniqueSeriesNames <- unique(as.character(examDF[,'seriesName']))
      
      ########################
      
      # insert some operation here
      # not used at theis point
      
      ########################
      
      #### make a loop for each unique series
      # j=1
      for (j in 1:length(uniqueSeriesNames)) {
        seriesName <- uniqueSeriesNames[j]
        
        # set the warningMessage to NULl
        warningMessage <- NULL
        
        # make a separate data frames for the series
        seriesDF <- examDF[examDF$seriesName==seriesName,]
        
        # get the chart names
        # uniqueCharts <- unique(as.character(seriesDF[,3]))
        uniqueCharts <- unique(as.character(seriesDF[,'chartName']))
        
        # length(uniqueCharts)
        
        # initialize some variables 
        eventVectorNames <- NULL
        numberEvents <- NULL
        
        ### use a loop to make a vector for event labels for each unique chart in the series
        # k=1
        for (k in 1:length(uniqueCharts)) {
          chartName <- uniqueCharts[k]
          # get the vector of event lables for each chart
          # eventVector <- seriesDF[seriesDF$chartName==uniqueCharts[k],]$Label
          eventVector <- seriesDF[seriesDF$chartName==chartName,"Label"]
          # remove "" elements
          eventVector <- eventVector[which(eventVector != "")]
          # determine the number of events and concatenate it to a vector numberEvents
          numberEvents <- as.character(c(numberEvents, length(eventVector)))
          # set the vector name and assign the lables to the vector
          dfName <- paste0("x_", uniqueCharts[k], "_labels") # may need to append "x_."
          # assign it to a data frame
          assign(dfName, eventVector)
          # add the name of the event vector to a vector of names
          eventVectorNames <- c(eventVectorNames, dfName)
        
        } # end loop for k unique charts
        
        ### create a warning message if the number of events not the same for all charts
        if (length(unique(numberEvents))!=1) {
          warningMessage <- c(warningMessage, paste("01 Some charts in series",
                                                    seriesName,
                                                    chartName,
                                                    "have different numbers of stimulus events."))
        }
        
        ### a loop to fix the length of all event vectors to the max length for each exam
        maxEvents <- max(as.integer(numberEvents))
        numberEventVectors <- length(eventVectorNames)
        tempVector <- NULL
        # l=1
        for (l in 1:numberEventVectors) {
          tempVector <- get(eventVectorNames[l])
          # adjust the length
          tempVector <- c(tempVector, rep("-", maxEvents - length(tempVector)))
          # assign(paste("eventVector", l, sep = ""), tempVector)
          assign(eventVectorNames[l], tempVector)
          # maybe write the csv to include the padded events
          #
        } # end loop l to fix the length of all event vectors to the max for each exam
        
        if (type=="CQT") {
          
          ### select charts with 2 or more CQs and RQs
          # also make a vector of all event labels for all charts with 2 CQs and 2 RQs
          
          retainEventVectorNames <- NULL # vector for the names of charts with 2 CQs and 2 RQs
          allEventNames <- NULL
          # loop over the eventVectorNames 
          # m=1
          for (m in 1:length(eventVectorNames)) {
            eventVectorName <- eventVectorNames[m] 
            # make a logical scalar if RQs >= 2
            RQs <- length(grep("R", get(eventVectorName), 
                               ignore.case = TRUE, 
                               perl = FALSE, 
                               value = FALSE,
                               fixed = FALSE, 
                               useBytes = FALSE, 
                               invert = FALSE) ) >= 2
            # make a logical scalar if CQs >= 2
            CQs <- length(grep("C", get(eventVectorName), 
                               ignore.case = TRUE, 
                               perl = FALSE, 
                               value = FALSE,
                               fixed = FALSE, 
                               useBytes = FALSE, 
                               invert = FALSE)) >= 2 
            ## keep the chart if >= 2 RQs and >= 2 CQs
            if (CQs && RQs) {
              retainEventVectorNames <- c(retainEventVectorNames, eventVectorNames[m])    
              # make a long vector of all events from all charts with 2 CQs and RQs
              allEventNames <- c(allEventNames, get(eventVectorNames[m]))
            } # end if
            ## warning if some charts in the series do not have >=2 CQs and >=2 RQs
            if(!CQs || !RQs) {
              warningMessage <- c(warningMessage, paste("02 Some charts in series", 
                                                        seriesName,
                                                        chartName,
                                                        "are excluded because they do not include >=2 CQs and >=2 RQs."))
            }
            
          } # end for loop over m eventVectorNames
          
        } # end if type == CQT
        
        # if (type==NULL) retainEventVectorNames <- tempVector # 6-14-15 need to fix this
        
        # make a vector of unique event names
        # unique events may be less than max events because of repetion
        uniqueEventNames <- unique(allEventNames)
        
        ### make an output data frame 
        tempOutVector <- NULL
        outputDF <- NULL
        if(!is.null(retainEventVectorNames)) {
          # n=1
          for (n in 1:length(retainEventVectorNames)) {
            tempOutVector <- get(retainEventVectorNames[n])
            # add the chart name
            tempOutVector <- c(examName, 
                               substr(retainEventVectorNames[n], 
                                      start=3, 
                                      stop=nchar(retainEventVectorNames[n])-7), 
                               tempOutVector)
            # add the exam name
            outputDF <- rbind(outputDF, tempOutVector)
          } # end for loop n to make the output data frame
          row.names(outputDF) <- NULL
          outputDF <- as.data.frame(outputDF)
        } # end if
        
        if(!is.null(outputDF)) {
          names(outputDF) <- c("examName", 
                               "chartName", 
                               paste0("E", 1:(ncol(outputDF)-2)))
        }
        
        #### ouput 
        
        # set the output name
        outName <- paste0(examName, "_", seriesName, "_eventMatrix")
        
        if (makeDF==TRUE) {
          # if(!is.null(outputDF)) {
            assign(outName, outputDF, pos=1)
          # }
        }
        
        if(!is.null(outputDF)) {
          if(saveCSV==TRUE) {
            write.table(outputDF, file = paste0(outName, ".csv"), 
                        append = FALSE, 
                        quote = TRUE, 
                        sep = ",", 
                        eol = "\n", 
                        na = "NA", 
                        dec = ".", 
                        row.names = FALSE, 
                        col.names = TRUE, 
                        qmethod = "double", 
                        fileEncoding = "UTF-8")
          }
        }
        
        # save the warning message
        if(length(warningMessage) > 0) {
          # name of the warning message
          warnName <- paste0(examName, "_", seriesName, "_event_warnings")
          # make a vector in the global env
          assign(warnName, as.data.frame(warningMessage), pos=1)
          if (saveCSV==TRUE) {
            # save the warning message as txt
            fileName <- file(paste0(warnName, ".txt"), "w")
            writeLines(warningMessage, con=fileName)
            close(fileName)
          }
        } 
        
      } # end loop j for each unique series
      
    } # end loop i to parse the events for each exam
    
  print(paste("events parsed for", i, "exams"))
  
  # return(retainEventVectorNames)
  
} # end eventParse() function

# eventParse(x="_Stimuli$")

# need to modify this to make a data frame of all events for all charts and series regardless 
# and specify the selection of CQT RI CIT or ACQT



