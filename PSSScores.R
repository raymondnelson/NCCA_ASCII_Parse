# MacLaren and Krapohl Permutation Scoring System 
# published in 2003



{
  
  source(paste0(RPath, 'PSSModel.R'), echo=FALSE)
  
  murphyFn <- function(pFD, pFT, prior) {
    # a private function for Murphy's (1978) formula
    # equivalent to the odds form of Bayes theorem
    # inputs are in decimal probability form
    # Posterior Odds = Likelihood Ratio X Prior Odds
    # likelihood ratio is true positive rate / false positive rate
    # using the observed test score as the cutscore 
    # pFD is the sensitivity rate or prob of failing if guilty
    # pfT is the false positive rate or the prob of failing if innocent
    (pFD / pFT) * (prior / (1-prior))
  }
  
}



############ main function ####################



PermutationTestScoresFn <- function(RqCqDFSeries=RqCqDFSeries,
                                    PSSDecisionRule="GTR",
                                    forced=FALSE,
                                    priorProb=.5,
                                    PSSCutProbT=.1,
                                    PSSCutProbD=.9,
                                    forceINC=TRUE,
                                    makeDF=makeDF,
                                    saveCSV=saveCSV,
                                    analysisListName="analysisResultList" ) {
  # R function to compute the permutation scoring system
  # published by MacLaren & Krapohl (2003)
  # 
  # Raymond Nelson
  # May 20, 2019
  # 
  # PSS scores are calculated using within-test z-scores
  # 
  # PSS results are calculated using all measurements from all charts
  # this is different from other algorithms that evaluate one chart at a time
  # before aggregating the scores for the charts
  # 
  # input is a data frame of RQ and CQ measurements for a series (all charts)
  # including all test charts with RQs and CQs
  # and all sensors (upper and lower respiration, electrodermal, cardio)
  #
  # requires the PSS reference model
  # source(paste0(RPath, 'PSSModel.R'))
  # 
  # PSS is for 3 RQs 3CQs and three charts
  # forced=TRUE will coerce the PSS to other exam formats
  # 
  # makeDF 
  # saveCSV
  # 
  # output is the RqCqDFSeries data frame 
  # with the PSSScore column populated with the RC ratios
  # additional output is via side effect
  # 
  ####
  # 
  # Murphy, (1987) Detecting infrequent deception. Journal of Applied Psychology
  # 
  # P(D|F) / P(T|F) = 
  # P(F|D) / P(F|T) * P(D) / P(T)
  # 
  # MacLaren "One way of comparing an individual test result against a population distribution 
  # is to consider the test performance observed in the examinee
  # as the minimum level of performance 
  # that would be considered a positive test outcome.
  # Using this point as a cutoff 
  # the proportion of other test takers in the population 
  # who would be classified as deceptive may be calculated.
  #
  # PSS reference model for FZCT
  # each RQ response is divided by the adjacent CQ response
  # 
  # Two sets of numerical scores are calculated for each case
  # one set of scores using the distribution for guilty scores
  # the other set of scores using the distribution for innocent scores
  # 
  # reduce 2 respiration scores to 1 using the common procedure
  # sum the scores for each set
  # these two sets express similarity of the case with each distribution
  # 
  # to obtain the likelihood score for both total scores
  # use MacLaren table 3
  # which appears to be a permutation of all possible combinations
  # of 7 position scores
  #
  # then use the formula from Murphy (1987)
  #
  ####
  
  {
    
    examName <- RqCqDFSeries$examName[1]
    seriesName <- RqCqDFSeries$seriesName[1]
    
    print("calculate the Permutation scores")
    
    # assign("RqCqDFSeries", RqCqDFSeries, envir=.GlobalEnv)
    # View(RqCqDFSeries)
    # stop()
    
  }
  
  #### set up ####
  
  {
    
    # environment paramenters  
    
    {
      if(!exists("forced")) forced <-TRUE
      if(!exists("priorProb")) priorProb <- .5
      if(!exists("PSSCutProbT")) PSSCutProbT <- .1
      if(!exists("PSSCutProbD")) PSSCutProbD <- .9
      if(!exists("PSSDecisionRule")) PSSDecisionRule="GTR"
      if(!exists("makeDF")) makeDF <- TRUE
      if(!exists("saveCSV")) saveCSV <- FALSE
    }
    
    PSSSensors <- c("UPneumo", 
                    "LPneumo", 
                    "Pneumo", 
                    "AutoEDA", 
                    # "ManualEDA", 
                    # "FC",
                    "Cardio" )
    
    uniqueSensors <- as.character(unique(RqCqDFSeries$sensorName))
    
    uniqueQuestions <- unique(RqCqDFSeries$eventLabel)
    
    # PSS does not work with data at the chart level
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
    rqRows <- grep("R", RqCqDFSeries$eventLabel)
    cqRows <- grep("C", RqCqDFSeries$eventLabel)
    
    uniqueRQs <- unique(RqCqDFSeries[rqRows,'eventLabel'])
    uniqueCQs <- unique(RqCqDFSeries[cqRows,'eventLabel'])
    
    rqDFSeries <- RqCqDFSeries[rqRows,]
    cqDFSeries <- RqCqDFSeries[cqRows,]
    
    # View(rqDFSeries)
    # View(cqDFSeries)
    
    # assign("rqDFSeries", rqDFSeries, pos=1)
    # assign("cqDFSeries", cqDFSeries, pos=1)
    
    uniqueCharts <- unique(RqCqDFSeries$chartName)
    
  }
  
  ############ check some boundary conditions ##############
  
  {
    
    # exit if more than 2 PSS Sensors are missing
    if(length(which(!(PSSSensors %in% uniqueSensors))) > 2) {
      return(RqCqDFSeries)
    }
    
    # exit if there are no unique events
    if(length(uniqueQuestions) == 0) { 
      return(RqCqDFSeries) 
    }
    
    # PSS does not work with the data at the individual chart level
    if( (length(uniqueCharts) < 3 || length(uniqueCharts) > 5) && 
        forced==FALSE ) { 
      "exiting - requires 3 to 5 charts"
      return(RqCqDFSeries) 
    }
    
    # exit if there are no RQs or no CQs
    if(length(rqRows) == 0 || length(cqRows) == 0) { 
      return(RqCqDFSeries) 
    }
    
    # exit if not 3 RQs and 3 CQs
    # PSS assumes 3 RQs and 3 CQs
    if( (length(uniqueRQs) != 3 || length(uniqueCQs) != 3) &&
        forced == FALSE ) {
      "PSS reference model requires 3 RQs and 3 CQs"
      return(RqCqDFSeries)
    }

    if( length(uniqueCharts) != 3 && forced==FALSE ) { 
      "Permutation reference model is for 3 charts"
      return(RqCqDFSeries) 
    }
    
    # exit if not at least 2 RQs and 2 CQs 
    # because there is no point to scoring with this method
    if(length(uniqueRQs) < 2 || length(uniqueCQs) < 2) {
      "incorrect number of RQs and CQs"
      stop()
      return(RqCqDFSeries) 
    }
    
  }
  
  ##############   PSS reference model   ###############
  
  {
    # source(paste0(RPath, 'PSSModel.R'), echo=FALSE)
  }
    
  ######## initialize three data frames for the scores ########
    
  {
    
    guiltyScoresDF <- cbind(rep(uniqueCharts, each=length(PSSSensors)),
                          rep(PSSSensors, times=length(uniqueCharts)) )
    guiltyScoresDF <- cbind(guiltyScoresDF,
                          data.frame(matrix(ncol=length(uniqueRQs), 
                                            nrow=length(PSSSensors) * 
                                              length(uniqueCharts))) )
    names(guiltyScoresDF) <- c("chartName", 
                             "sensorName",
                             uniqueRQs)
    
    # copy it for the innocent scores
    innocentScoresDF <- guiltyScoresDF  
    
    # copy another data frame for the R/C ratios
    RCRatioDF <- guiltyScoresDF
    
    # these three data frames now contain NA values
    
  }
  
  ##### iterate over the charts to compute the septile ratios and scores ####
  
  i=1
  for (i in 1:min(c(3, length(uniqueCharts)))) {
    
    {
    
      # uses only 3 charts
      
      # first get the RcQc data frame for this chart
      thisChart <- uniqueCharts[i]
      thisChartRows <- which(RqCqDFSeries$chartName == thisChart)
      
      RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      # View(RqCqDFChart)
      
    }
    
    {
      
      uniqueRQsChart <- 
        unique(RqCqDFChart$eventLabel[grep("R", RqCqDFChart$eventLabel)])
      
      ## limit it to 3 RQs ##
      # commented out 6-19-2022 for LXCAT charts
      # if(length(uniqueRQsChart) > 3) {
      #   ignoreTheseRQs <- uniqueRQsChart[c(4:length(uniqueRQsChart))]
      #   thisChartRows <- 
      #     thisChartRows[!(RqCqDFChart$eventLabel %in% ignoreTheseRQs)]
      #   RqCqDFChart <- RqCqDFSeries[thisChartRows,]
      # }
      
    }
    
    ######## get the data for the RQs and CQs ########
    
    {
      
      rqRowsChart <- grep("R", RqCqDFChart$eventLabel)
      cqRowsChart <- grep("C", RqCqDFChart$eventLabel)
      
      # exit if there are no RQs or no CQs
      if(length(rqRowsChart) == 0 || length(cqRowsChart) == 0) {
        next()
      }
      
      # get the unique questions for this chaert
      uniqueQuestionsChart <- unique(RqCqDFChart$eventLabel)
      
      # get the RQ and CQ names
      uniqueRQsChart <- unique(RqCqDFChart[rqRowsChart,'eventLabel'])
      uniqueCQsChart <- unique(RqCqDFChart[cqRowsChart,'eventLabel'])
      
      # get the RQ and CQ indices
      # RQIndices <- which(uniqueQuestions %in% uniqueRQsChart)
      # CQIndices <- which(uniqueQuestions %in% uniqueCQsChart)
      
      # Nov 13, 2020
      # need the correct order for the RQIndices and CQ Indices for this chart
      RQIndices <- match(uniqueRQsChart, uniqueQuestionsChart)
      CQIndices <- match(uniqueCQsChart, uniqueQuestionsChart)
      
      # check if all CQs are preceding the RQs
      if( length(CQIndices) != length(RQIndices) ||
         !all( CQIndices[1:length(RQIndices)] < RQIndices, na.rm=TRUE) ) {
        "PSS requires a ZCT format with 3 CQs preceding 3 RQs"
        if(forced==FALSE) { return(RqCqDFSeries) }
      }
      
    }
    
    ######## iterate on the RQs and calculate the PSS score ########
    
    j=1
    # for(j in 1:min(c(3, length(RQIndices)))) {
    for(j in 1:length(RQIndices)) {
        
      {
        
        # get each RQ
        
        # use only 3 RQs per chart
        
        # PSS score is 
        
        thisRQ <- uniqueQuestionsChart[RQIndices[j]]
        thisRQRows <- which(RqCqDFChart$eventLabel==thisRQ &
                              RqCqDFChart$sensorName %in% PSSSensors)
       
        # make a data frame for the RQ
        rqDF <- RqCqDFChart[thisRQRows,]
        # View(rqDF)
        
      }
      
      {
       
        # get the CQs 
        
        # get the CQ preceding each  RQ
        if(length(which(CQIndices < RQIndices[j])) == 0) next()
        thisCQ <- 
          uniqueCQsChart[CQIndices==max(CQIndices[CQIndices < RQIndices[j]])]
        # next RQ if there is no preceding CQ
        if(is.na(thisCQ)) next()
        thisCQRows <- which(RqCqDFChart$eventLabel==thisCQ &
                              RqCqDFChart$sensorName %in% PSSSensors)
        
        # make a data frame for the CQ
        cqDF <- RqCqDFChart[thisCQRows,]
        # View(cqDF)
        
        # we now have all sensor scores for a single RQ and CQ pair
        
      }
      
      {
        
        #### calculate the R/C sensor ratios for this RQ ####
        
        # missing measurements will give NA
        RQMeasurements <- rqDF$sensorMeasurement
        CQMeasurements <- cqDF$sensorMeasurement
        
        RQMeasurements[which(RQMeasurements <= 0)] <- NA
        CQMeasurements[which(CQMeasurements <= 0)] <- NA
        
        RCRatios <- RQMeasurements / CQMeasurements
        names(RCRatios) <- rqDF$sensorName
        # print(RCRatios)
        
        # invert the sign of the respiration values
        # so that smaller values are greater changes in physiology
        # RCRatios[1:2] <- exp(log(RCRatios[1:2]) * -1)
        
        # fix some missing values
        RCRatios[which(is.infinite(RCRatios))] <- NA
        RCRatios[which(RCRatios == 0)] <- NA
        RCRatios[which(is.na(RCRatios))] <- NA
        
      }
      
      #### combine the upper and lower pneumo R/C ratios ####
        
      if(!all(is.na(RCRatios[1:2]))) {
        # check that the upper and lower pneumo scores are not NA
        RCRatios['Pneumo'] <- 
          # use the log to check if signs are opposite
          ifelse(log(RCRatios[1]) * log(RCRatios[2]) < 0,
                 # NA if the logRCRatios have opposite signs
                 0,
                 # otherwise choose the abs(max()) R/C ratio without the log
                 RCRatios[1:2][which.max(c(abs(RCRatios[1:2])))] )
      }
        
      #### submit the rqDF to the RqCqDFChart and RqCqDFSeries ####
        
      {
        
        # submit the ratios to the rqDF
        rqDF$PSSScore <- RCRatios
        # View(rqDF)
        
        RqCqDFChart[thisRQRows,] <- rqDF
        # View(RqCqDFChart)
        
        RqCqDFSeries[thisChartRows,] <- RqCqDFChart
        
      } 
      
      ## still working on RCRatios for each unique RQ
      
      #### get the PSS septile scores using the PSS tables ####
      
      # PSS tables are for 3 charts 3 RQs and 3 CQs 
      # with Pneumo, EDA and Cardio
      
      ## there are 2 PSS septile scores, for guilty and innocent ##
      
      ## septile scores are uniform
      ## with an expected equal number of 1s 2s and 3s
      
      ### guilty scores 
      
      {
        
        # initialize a vector to hold the guilty scores for this RQ
        guiltyPSSScores <- rep(NA, times=length(PSSSensors))
        names(guiltyPSSScores) <- PSSSensors
        
        if(!is.na(RCRatios['UPneumo'])) {
          guiltyPSSScores['UPneumo'] <- 
            PSS_RLL_Ratios$RLLScore[max(
              which(PSS_RLL_Ratios$RLLThRatioG <= RCRatios['UPneumo']) )]
        }
        
        if(!is.na(RCRatios['LPneumo'])) {
          guiltyPSSScores['LPneumo'] <- 
            PSS_RLL_Ratios$RLLScore[max(
              which(PSS_RLL_Ratios$RLLAbRatioG <= RCRatios['LPneumo']) )]
        }
        
        # combine the 2 pneumo scores
        if(!is.na(RCRatios['Pneumo'])) {
          guiltyPSSScores['Pneumo'] <- 
            # guilty scores are integer at this point
            # log is not needed
            ifelse(guiltyPSSScores[1] * guiltyPSSScores[2] < 0,
                   # integer score is 0 if signs are opposite for Th and Ab resp
                   0,
                   # select the greater abs value if signs are not opposite
                   guiltyPSSScores[1:2][which.max(c(abs(guiltyPSSScores[1:2])))] 
            )
        }
        
        if(!is.na(RCRatios['AutoEDA'])) {
          guiltyPSSScores['AutoEDA'] <- 
            PSS_EDA_Ratios$EDAScore[max(
              which(PSS_EDA_Ratios$EDARatioG <= RCRatios['AutoEDA']) )]
        }
        
        # if(!is.na(RCRatios['ManualEDA'])) {
        #   guiltyPSSScores['ManualEDA'] <- 
        #     PSS_EDA_Ratios$EDAScore[max(
        #       which(PSS_EDA_Ratios$EDARatioG <= RCRatios['ManualEDA']) )]
        # }
        
        if(!is.na(RCRatios['Cardio'])) {
          guiltyPSSScores['Cardio'] <- 
            PSS_Cardio_Ratios$CardioScore[max(
              which(PSS_Cardio_Ratios$CardioRatioG <= RCRatios['Cardio']) )]
        }
        
        # guiltyPSSScores
        
      }
      
      ### innocent scores 
      
      {
       
        # initialize a vector to hold the innocent scores
        innocentPSSScores <- rep(NA, times=length(PSSSensors))
        names(innocentPSSScores) <- PSSSensors
        
        if(!is.na(RCRatios['UPneumo'])) {
          innocentPSSScores['UPneumo'] <- 
            PSS_RLL_Ratios$RLLScore[max(
              which(PSS_RLL_Ratios$RLLThRatioI <= RCRatios['UPneumo']) )]
        }
        
        if(!is.na(RCRatios['LPneumo'])) {
          innocentPSSScores['LPneumo'] <- 
            PSS_RLL_Ratios$RLLScore[max(
              which(PSS_RLL_Ratios$RLLAbRatioI <= RCRatios['LPneumo']) )]
        }
        
        # combine the 2 pneumo scores
        if(!is.na(RCRatios['Pneumo'])) {
          innocentPSSScores['Pneumo'] <- 
            ifelse(innocentPSSScores[1] * innocentPSSScores[2] < 0,
                   0,
                   innocentPSSScores[1:2][which.max(c(abs(innocentPSSScores[1:2])))] 
            )
        }
        
        if(!is.na(RCRatios['AutoEDA'])) {
          innocentPSSScores['AutoEDA'] <- 
            PSS_EDA_Ratios$EDAScore[max(
              which(PSS_EDA_Ratios$EDARatioI <= RCRatios['AutoEDA']) )]
        }
        
        # if(!is.na(RCRatios['ManualEDA'])) {
        #   innocentPSSScores['ManualEDA'] <- 
        #     -PSS_EDA_Ratios$EDAScore[max(
        #       which(PSS_EDA_Ratios$EDARatioI <= RCRatios['ManualEDA']) )]
        # }
        
        if(!is.na(RCRatios['Cardio'])) {
          innocentPSSScores['Cardio'] <- 
            PSS_Cardio_Ratios$CardioScore[max(
              which(PSS_Cardio_Ratios$CardioRatioI <= RCRatios['Cardio']) )]
        }
        
        # innocentPSSScores
        
      }
      
      #### submit the scores to the guilty and innocent data frames ####
      
      {
        
        guiltyScoresDF[guiltyScoresDF$chartName==uniqueCharts[i],
                     names(guiltyScoresDF)==uniqueRQs[j]] <- guiltyPSSScores
        
        innocentScoresDF[innocentScoresDF$chartName==uniqueCharts[i],
                       names(innocentScoresDF)==uniqueRQs[j]] <- innocentPSSScores
        
        RCRatioDF[RCRatioDF$chartName==uniqueCharts[i],
                   names(RCRatioDF)==uniqueRQs[j]] <- round(RCRatios, 3)
        
      }
      
    } # end j loop over RQIndices
    
    # already submitted earlier
    # RqCqDFSeries[thisChartRows,] <- RqCqDFChart
    
    # PSS was develope for only 3  charts and 3 RQs
    
  } # end i loop over charts
  
  # View(guiltyScoresDF)
  # View(innocentScoresDF)
  # View(RCRatioDF)
  
  # View(RqCqDFChart)
  
  ######## reduce the PSS sensors ########

  {
    
    PSSSensors2 <- c("Pneumo",
                     "AutoEDA",
                     "Cardio" )
    
    # useRows <- which(guiltyScores$sensorName %in% PSSSensors2)
    
    # keep only the PSSSensors2
    guiltyScoresDF <- 
      guiltyScoresDF[which(guiltyScoresDF$sensorName %in% PSSSensors2),]
    # keep only 3 charts
    guiltyScoresDF <- 
      guiltyScoresDF[guiltyScoresDF$chartName %in% uniqueCharts[1:3],]
    
    # keep only the PSSSensors2
    innocentScoresDF <- 
      innocentScoresDF[which(innocentScoresDF$sensorName %in% PSSSensors2),]
    # keep only 3 charts
    innocentScoresDF <- 
      innocentScoresDF[innocentScoresDF$chartName %in% uniqueCharts[1:3],]
    
    # now get the totals
    
    guiltyTotal <- 
      sum(colSums(guiltyScoresDF[,3:ncol(guiltyScoresDF)], na.rm=TRUE))
    
    innocentTotal <- 
      sum(colSums(innocentScoresDF[,3:ncol(innocentScoresDF)], na.rm=TRUE))
    
  }
  
  ######## get the likelihood scores ########
      
  {
    
    # from the permutation distribution of 7 position scores
    # use Table 3 or PSS_Model_7x27
    
    # PSS_Model_7x27 is used instead of Table 3
    # because extreme score values are often outside the limits of Table3
    
    # View(PSS_Model_7x27)
    # View(PSS_Table3)
    
    permutationScore <- PSS_Model_7x27$permutationScore
    # permutationScore <- PSS_Table3$Score
    
    lf <- PSS_Model_7x27$cdfContCor
    # lf <- PSS_Table3$Pair9cc
    # lf <- PSS_Table3$Pair9
    
    
    # View(PSS_Model_7x27)
    
    # compute the likelihood scores
    
    
    guiltyLS <- ifelse(guiltyTotal > max(permutationScore),
                       .000000001,
                       ifelse(guiltyTotal < min(permutationScore),
                              .000000001,
                              lf[which(permutationScore == guiltyTotal)]))
    
    
    # guiltyLS <- ifelse(guiltyTotal > max(permutationScore),
    #                     .999999999,
    #                     ifelse(guiltyTotal < min(permutationScore),
    #                            .000000001,
    #                            NA))
    # 
    # # then calculate the value if NA
    # if(is.na(guiltyLS)) {
    #   guiltyLS <- lf[which(permutationScore == guiltyTotal)]
    #   guiltyLS <- ifelse(guiltyLS > .999999999,
    #                       .999999999,
    #                       ifelse(guiltyLS < .000000001,
    #                              .000000001,
    #                              guiltyLS))
    # }
    
    innocentLS <- ifelse(innocentTotal > max(permutationScore),
                         .000000001,
                         ifelse(innocentTotal < min(permutationScore),
                                .000000001,
                                lf[which(permutationScore == innocentTotal)] ) )
    
    
    # # initialize the value to NA
    # innocentLS <- ifelse(innocentTotal > max(permutationScore),
    #                      .999999999,
    #                       ifelse(innocentTotal < min(permutationScore),
    #                              .000000001,
    #                              NA) )
    # 
    # # then calculate the value if NA
    # if(is.na(innocentLS)) {
    #   innocentLS <- lf[which(permutationScore == innocentTotal)]
    #   innocentLS <- ifelse(innocentLS > .999999999,
    #                        .999999999,
    #                         ifelse(innocentLS < .000000001,
    #                                .000000001,
    #                                innocentLS))
    # }
    
    # guiltyLS <- guiltyLS / (1-guiltyLS)
    # innocentLS <- innocentLS / (1-innocentLS)
    
  }
  
  # guiltyLS
  # innocentLS
  
  ######## then call Murphy's formula ########
  
  # equivant to the odds form of Bayes theorem
  # Posterior Odds = Likelihood Ratio X Prior Odds
  
  # Murphy, (1987) Detecting infrequent deception. Journal of Applied Psychology
  # 
  # D = deceptive criterion state
  # T = truthful criterion state
  # F = false statement indicated
  # 
  # P(D) and P(T) are population base rates
  # to be conditioned on the test likelihood statistic
  # P(F|D) is true positive rate 
  # P(F|T) is the false positive rate
  #
  # P(D|F) / P(T|F) = P(F|D) / P(F|T) * P(D) / P(T)
  
  
  # P(F|D) and P(F|T) are estimated using the likelihood function
  # (guiltyCDF / innocentCDF) * (priorProb / (1-priorProb))
  
  {
    
    # fix the inversion of sign vals if total scores are opposite
    # if(guiltyTotal * innocentTotal < 0) innocentLS <- 1 -  innocentLS
    
    if(!exists("priorProb")) priorProb <- .5
      
    # call the murphy Fn to calculate the posterior probability of deception
    postOdds <-  
      round(murphyFn(pFD=guiltyLS, pFT=innocentLS, prior=priorProb), 9)
    
    # convert the posterior probability to posterior odds of deception
    postP <- round(postOdds / (1+postOdds), 3)
    
  }
  
  #### calculate the categorical result using the probability cutpoints ####
  
  {
    
    cutScores <- c(GTDI=PSSCutProbD, GTNDI=PSSCutProbT)
    
    if(!exists("uniuqeRQs")) {
      rqRows <- grep("R", RqCqDFSeries$eventLabel)
      cqRows <- grep("C", RqCqDFSeries$eventLabel)
      uniqueRQs <- unique(RqCqDFSeries[rqRows,'eventLabel'])
      uniqueCQs <- unique(RqCqDFSeries[cqRows,'eventLabel'])
    }
    
    # source(paste0(RPath, 'decisionRules.R'), echo=FALSE)
    
    # call the private function to use the grand total rule
    # use flip=TRUE to invert the probability score and probability cutscores
    GTRResult <- GTRFn(totalScore=postP, 
                       RQNames=uniqueRQs,
                       cutScores=cutScores,
                       flip=TRUE )
    
    PSSCategoricalResult <- GTRResult$testResult
    
    PSSQuestionResults <- GTRResult$subtotalResults
    
    resultUsing <- GTRResult$resultUsing
    
  }
  
  ######## attempt to resolve INC results from extreme scores ##########
  
  if( forceINC && PSSCategoricalResult=="INC/NO" ) {
    # forceINC <- TRUE
    
    # first check the classification and pvalue
    
    thisMax <- which.max(abs(c(guiltyTotal, innocentTotal)))
    
    if(thisMax == 2) {
      
      # deceptive classification
      
      # check the p-value 
      
      # if(1-guiltyLS >= cutScores[1]) {
      if(1-guiltyLS >= .9999) {
        
        thisResult<- list(testResult="DI/SR")
        
        thisResult$subtotalResults <- rep("DI/SR", times=length(uniqueRQs))
        names(thisResult$subtotalResults) <- uniqueRQs
        
        thisResult$resultUsing <- "grand total"
        
        thisResult$totalScore <- guiltyLS # get the greater p value
        
        thisResult$cutScore <- cutScores[1] # the the cutscore
        names(thisResult$cutScore) <- "GTR"
        
        thisResult$lowestRQName <- "none"
        
        thisResult$lowestSubtotalScore <- "none"
        
        PSSCategoricalResult <- thisResult$testResult
        
        PSSQuestionResults <- thisResult$subtotalResults
        
        resultUsing <- thisResult$resultUsing
        
      }
      
    } 
    
    if(thisMax == 1) {
      
      # innocent classification
      
      # check the p-value 
      
      # if(innocentLS <= cutScores[2]) {
      if(innocentLS <= .0001) {
        
        thisResult<- list(testResult="NDI/NSR")
        
        thisResult$subtotalResults <- rep("NDI/NSR", times=length(uniqueRQs))
        names(thisResult$subtotalResults) <- uniqueRQs
        
        thisResult$resultUsing <- "grand total"
        
        thisResult$totalScore <- innocentLS # get the greater p value
        
        thisResult$cutScore <- cutScores[2] # the the cutscore
        names(thisResult$cutScore) <- "GTR"
        
        thisResult$lowestRQName <- "none"
        
        thisResult$lowestSubtotalScore <- "none"
        
        PSSCategoricalResult <- thisResult$testResult
        
        PSSQuestionResults <- thisResult$subtotalResults
        
        resultUsing <- thisResult$resultUsing
        
      }
      
    } 
    
  }
  
  ############################### output ###############################
  
  {
    
    # source(paste0(RPath, 'outputScores.R'), echo=FALSE)
    
    PSSMeasurementSensors <- c("UPneumo", 
                               "LPneumo", 
                               "AutoEDA", 
                               # "ManualEDA", 
                               "Cardio" )
    
    # combine the guilty and innocent scores 
    # PSSScores <- guiltyScores[useRows,3:ncol(guiltyScores)] - 
    #   innocentScoresDF[useRows,3:ncol(innocentScores)]
    
    # then calculate a total
    # PSSTotal <- sum(PSSScores)
    
    PSSMeasurementsDF <- measurementTableFn(RqCqDFSeries=RqCqDFSeries, 
                                            useSensors=PSSMeasurementSensors,
                                            decimals=2,
                                            makeDF=makeDF,
                                            saveCSV=saveCSV )
    
    # RCRatioDF <- RCRatioDF[which(RCRatioDF$sensorName %in% PSSSensors2),]
    
    ## need the reduced sensor means
    
    {
      
      PSSSensorMeansDF <- 
        as.data.frame(matrix(nrow=length(PSSSensors2), n=ncol(RCRatioDF)))
      names(PSSSensorMeansDF) <- names(RCRatioDF)
      names(PSSSensorMeansDF)[1] <- "seriesName"
      PSSSensorMeansDF <- cbind(examName=examName, PSSSensorMeansDF)
      PSSSensorMeansDF$seriesName <- seriesName
      PSSSensorMeansDF$sensorName <- PSSSensors2
      # str(PSSSensorMeansDF)
      
      # iterate on the sensors
      i=j
      for(j in 1:length(PSSSensors2)) {
        theseRows <- which(RCRatioDF$sensorName == PSSSensors2[j])
        PSSSensorMeansDF[j,4:ncol(PSSSensorMeansDF)] <- 
          colMeans(RCRatioDF[theseRows,3:ncol(RCRatioDF)], na.rm=TRUE)
      }
      
    }
    
    outputDFNamePSS <- paste(examName, seriesName, "PSSRatiosDF", sep="_")
    
    
    if(isTRUE(saveCSV)) {
      write.csv(RCRatioDF,
                file=paste0(str_sub(outputDFNamePSS, 1, -3), ".csv"),
                row.names=FALSE)
    }
    
    # View(RCRatioDF)
    
  }
  
  ############ construct a list to hold the PSS result #############
  
  {
    
    outputListName <- 
      paste(examName, seriesName, "PSSOutputList", sep="_")
    
    PSSOutputList <- list(PSS="Permutation Scoring System (MacLaren & Krapohl 2003)",
                          examName=examName,
                          seriesName=seriesName,
                          PSSResult=PSSCategoricalResult, #
                          PSSQuestionResults=PSSQuestionResults, #
                          PSSDecisionRule=PSSDecisionRule,
                          PSSResultUsing=resultUsing, #
                          PSSGuiltyTotal=guiltyTotal,
                          PSSInnocentTotal=innocentTotal,
                          PSSGuiltyLS=guiltyLS,
                          PSSInnocentLS=innocentLS,
                          PSSQuestions=uniqueQuestions,
                          PSSRQNames=uniqueRQs,
                          PSSPostOdds=postOdds,
                          PSSPostProb=postP,
                          PSSCutProbD=PSSCutProbD,
                          PSSCutProbT=PSSCutProbT,
                          priorProb=priorProb, 
                          PSSSensors=PSSMeasurementSensors,
                          PSSGuiltyScores=guiltyScoresDF,
                          PSSInnocentScores=innocentScoresDF,
                          PSSRCRatios=RCRatioDF,
                          PSSMeasurements=PSSMeasurementsDF,
                          PSSSensorMeans=PSSSensorMeansDF )
    
  }
  
  #### save the list to the global env as a side effect ####
  
  # use this to save the output list directly to the global env
  # save the list to the globad env as a side effect
  # assign(outputListName, PSSOutputList, env=.GlobalEnv)
  
  {
    
    analysisResultList <- get(analysisListName, envir=.GlobalEnv)
    seriesListName <- paste("series", seriesName, sep="_")
    outputListName <- "PSSMOutput"
    analysisResultList[[seriesListName]][[outputListName]] <- 
      PSSOutputList
    assign(analysisListName, analysisResultList, envir=.GlobalEnv)
    
  }
  
  ######## output ########
  
  return(RqCqDFSeries)
  
}


