# R script to save ANALYSIS output to .txt
# July 29, 2020
# Raymond Nelson

####


library(stringr)


# save the algorithm output to .txt

# {
#   OSS3Output <- ls(pattern=c(".OSS3OutputList$"))
#   OSS2Output <- ls(pattern=c(".OSS2OutputList$"))
#   ESSMOutput <- ls(pattern=c(".ESSMOutputList$"))
#   PAOutput <- ls(pattern=c(".PAOutputList$"))
#   PSSOutput <- ls(pattern=c(".PSSOutputList$"))
#   ROSSOutput <- ls(pattern=c(".ROSSOutputList$"))
#   bootstrapOutput <- ls(pattern=c(".bootstrapOutputList$"))
# }

# # iterate over the unique Exams
# i=1
# for(i in 1:length(ESSMOutput)){
#   # get the exam name
#   outputName <- str_sub(ESSMOutput[i], 1, -18)
#   
#   # construct the output file names
#   {
#     ESSMOutputName <- paste0("ESSMOutput_", outputName, ".txt")
#     OSS2OutputName <- paste0("OSS2Output_", outputName, ".txt")
#     OSS3OutputName <- paste0("OSS3Output_", outputName, ".txt")
#     PAOutputName <- paste0("PAOutput_", outputName, ".txt")
#     PSSOutputName <- paste0("PSSOutput_", outputName, ".txt")
#     ROSSOutputName <- paste0("ROSSOutput_", outputName, ".txt")
#     bootstrapOutputName <- paste0("bootstrapOutput_", outputName, ".txt")
#   }
#   
#   # use sink("filename.txt") to redirect console output to a file
#   # use sink(file=NULL) to stop redirection
#   
#   ### sink does not seem to work in a loop ###
#   # {
#   
#     options("width"=78)
#     
#     # sink(ESSMOutputName); get(ESSMOutput[i], envir=.GlobalEnv); sink(file=NULL)
#     # sink(OSS2OutputName); get(OSS2Output[i], envir=.GlobalEnv); sink(file=NULL)
#     # sink(OSS3OutputName); get(OSS3Output[i], envir=.GlobalEnv); sink(file=NULL)
#     # sink(PAOutputName); get(PAOutput[i], envir=.GlobalEnv); sink(file=NULL)
#     # sink(PSSOutputName); get(PSSOutput[i], envir=.GlobalEnv); sink(file=NULL)
#     # sink(ROSSOutputName); get(ROSSOutput[i], envir=.GlobalEnv); sink(file=NULL)
#     # sink(bootstrapOutputName); get(bootstrapOutput[i], envir=.GlobalEnv); sink(file=NULL)
#     
#   # }    
#   # end output to file
#   # sink(file=NULL)
#     
#     {
#       capture.output(get(ESSMOutput[i], envir=.GlobalEnv), file = ESSMOutputName)
#       capture.output(get(OSS2Output[i], envir=.GlobalEnv), file = OSS2OutputName)
#       capture.output(get(OSS3Output[i], envir=.GlobalEnv), file = OSS3OutputName)
#       capture.output(get(PAOutput[i], envir=.GlobalEnv), file = PAOutputName)
#       capture.output(get(PSSOutput[i], envir=.GlobalEnv), file = PSSOutputName)
#       capture.output(get(ROSSOutput[i], envir=.GlobalEnv), file = ROSSOutputName)
#       capture.output(get(bootstrapOutput[i], envir=.GlobalEnv), file = bootstrapOutputName)
#     }
#     
# } # end i loop

saveAnalysistoTXTFn <- function(x=".ANALYSIS") {
  
  {
    
    if(!exists("x")) x <- ".ANALYSIS"
  
    analysisLists <- ls(pattern=x, pos=1)
    
    options("width"=72)
    
    # to save the results of each algorithm
    # set in the workFlow_init.R script
    # saveALL <- TRUE
    
    # to save the ESSM series totals and sensor totals
    # saveESSM <- TRUE
    
  } 
  
  # iterate on the exams in the global env
  i=1
  for(i in 1:length(analysisLists)) {
    
    {
      #length will be equal to the number of series
      thisAnalysis <-  get(analysisLists[i], pos=1)
      
      examName <- str_sub(analysisLists[i], 1, -10)
    }
    
    # iterate on the series
    j=1
    for(j in 1:length(thisAnalysis)) {
      
      {
        seriesName <- names(thisAnalysis)[j]
        
        seriesNameB <- str_sub(seriesName, -1, -1)
        
        # thisAnalysis[[j]][[1]]
        
        # length(thisAnalysis[[seriesName]])
        
        # names(thisAnalysis[[seriesName]])
      }
      
      # iterate on the different analyses
      k=2
      for(k in 2:length(names(thisAnalysis[[seriesName]])) ) {
        
        # item 1 is the series name
        
        # item 2 is the question list
        
        # will save the question list first 
        
        analysisName <- names(thisAnalysis[[seriesName]])[k]
        
        # select which analyses to save
        
        if(analysisName == "questionList" && saveQuestionList == FALSE) next()
        
        # rank type analyses
        if(analysisName == "rankAnalysisOutput" && saveRank == FALSE) next()
        if(analysisName == "RRMAnalysisOutput" && saveRRM == FALSE) next()
        if(analysisName == "miritelloRankAnalysisOutput" && saveMiritelloRank == FALSE) next()
        if(analysisName == "ipsativeZAnalysisOutput" && saveIPZ == FALSE) next()
        
        # CQT analysis methods
        if(analysisName == "ESSMOutput" && saveESSM == FALSE) next()
        if(analysisName == "PAOutput" && savePA == FALSE) next()
        if(analysisName == "OSS2Output" && saveOSS2 == FALSE) next()
        if(analysisName == "OSS3Output" && saveOSS3 == FALSE) next()
        if(analysisName == "ROSSOutput" && saveROSS == FALSE) next()
        if(analysisName == "PSSMOutput" && savePSS == FALSE) next()
        if(analysisName == "bootstrapOutput" && saveBootstrap == FALSE) next()
        if(analysisName == "PCASS2Output" && savePCAT == FALSE) next()
                
        outputName <- paste0(analysisName,
                             "_",
                             examName,
                             "_",
                             str_sub(seriesName, -1, -1),
                             ".txt" )
        
        # use capture.output to create the text file
        if(isTRUE(saveALL)) {
          capture.output(thisAnalysis[[seriesName]][[analysisName]], 
                         file = outputName )
        }
        
        ### export the ESSM series totals and sensor totals
        
        if(analysisName == "ESSMOutput" && isTRUE(saveESSM)) {
          
          ESSMAnalysisOutput <- thisAnalysis[[seriesName]][[analysisName]]
          
          ESSMSeriesTotalsDF <- ESSMAnalysisOutput$ESSMSeriesTotalsDF
          write.csv(ESSMSeriesTotalsDF, row.names=FALSE, 
                    file=paste0(examName,
                                "_",
                                str_sub(seriesName, -1, -1),
                                "_",
                                "ESSMSeriesTotals",
                                ".csv" ) )
          
          # capture.output(ESSMSeriesTotalsDF,
          #                file=paste0(examName,
          #                            "_",
          #                            str_sub(seriesName, -1, -1),
          #                            "_",
          #                            "ESSMSeriesTotals",
          #                            ".csv" ) )
          
          ESSMSensorTotalsDF <- ESSMAnalysisOutput$ESSMSensorTotalsDF
          write.csv(ESSMSensorTotalsDF, row.names=FALSE, 
                    file=paste0(examName,
                                "_",
                                str_sub(seriesName, -1, -1),
                                "_",
                                "ESSMSensorTotals",
                                ".csv" ) )
          
          # capture.output(ESSMSensorTotalsDF,
          #                file=paste0(examName,
          #                            "_",
          #                            str_sub(seriesName, -1, -1),
          #                            "_",
          #                            "ESSMSensorTotals",
          #                            ".csv" ) )
          
        }
        
      } # end k loop over different analyses
      
    } # end j loop over series
    
  } # end i loop over exams
  
}


