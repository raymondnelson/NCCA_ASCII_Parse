# add columns for signal processing and data analysis

addColumnsFn <- function(x=examDF) {
  # x input is the examDF
  # called by the dataParse() function in the NCCAASCIIParseHelperFunctions.R script
  
  examDF <- x
  
  # add new columns for the processed pneumo Data
  examDF$c_UPneumoSm <- 0
  examDF$c_UPneumoInh <- 0
  examDF$c_UPneumoExh <- 0
  examDF$c_UPneumoMid <- 0
  examDF$c_UPneumoInhDiff <- 0
  examDF$c_UPneumoExhDiff <- 0
  examDF$c_UPneumoDiff <- 0
  examDF$c_UPneumoRate <- 0
  examDF$c_UPneumoExcursion <- ""
  
  examDF$c_LPneumoSm <- 0
  examDF$c_LPneumoInh <- 0
  examDF$c_LPneumoExh <- 0
  examDF$c_LPneumoMid <- 0
  examDF$c_LPneumoInhDiff <- 0
  examDF$c_LPneumoExhDiff <- 0
  examDF$c_LPneumoDiff <- 0
  examDF$c_LPneumoRate <- 0
  examDF$c_UPneumoExcursion <- ""
  
  # add a column for the difference between upper and lower pneumo chanels
  examDF$c_PneumoDiff <- 0
  
  # add some columns for the Manual EDA data
  examDF$c_ManualEDA <- 0
  examDF$c_ManualEDAPeak <- 0
  examDF$c_ManualEDAMid <- 0
  examDF$c_ManualEDAPeakDiff <- 0
  examDF$c_ManualEDABaseDiff <- 0
  examDF$c_ManualEDADiff <- 0
  
  examDF$c_ManualEDABase <- 0
  
  # add some EDA columns
  examDF$c_AutoEDA <- 0
  examDF$c_AutoEDAPeak <- 0
  examDF$c_AutoEDAMid <- 0
  examDF$c_AutoEDAPeakDiff <- 0
  examDF$c_AutoEDABaseDiff <- 0
  examDF$c_AutoEDADiff <- 0
  
  ### c_AutoEDABase is a very slow EDA baseline
  examDF$c_AutoEDABase <- 0
  
  # add some columns for the processed EDA data
	# examDF$c_EDAFilt <- 0
  # examDF$c_EDAFiltMid <- 0
  # examDF$c_EDAFiltMax <- 0
  # examDF$c_EDAFiltMin <- 0
  # examDF$c_EDAFiltDiff <- 0
  
  # add some new columns for the processed cardio data
  examDF$c_CardioSystolic <- 0
  examDF$c_CardioDiastolic <- 0
  examDF$c_CardioMinMax <- 0
  examDF$c_CardioMid <- 0
  examDF$c_CardioSystDiff <- 0
  examDF$c_CardioDiastDiff <- 0
  examDF$c_CardioAmp <- 0
  examDF$c_CardioRate <- 0
  examDF$c_cardioRateSystolic <- ""
  examDF$c_cardioRateDystolic <- ""
  
  ### CardioMA is a very slow smoothed cardio
  examDF$c_CardioMA <- 0
  
  # add some new columns for the eCardio channel
  if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0))!=0) {
  	examDF$c_eCardioSystolic <- 0
  	examDF$c_eCardioDiastolic <- 0
  	examDF$c_eCardioMinMax <- 0
  	examDF$c_eCardioMid <- 0
  	examDF$c_eCardioSystDiff <- 0
  	examDF$c_eCardioDiastDiff <- 0
  	examDF$c_eCardioAmp <- 0
  	examDF$c_eCardioRate <- 0
  	examDF$c_eCardioRateSystolic <- 0
  	examDF$c_eCardioRateDiastolic <- 0
  	examDF$c_eCardioMA <- 0
  }
  
  # add some new columns for the finger cuff cardio channel
  if(sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0) {
  	examDF$c_FCSystolic <- 0
  	examDF$c_FCDiastolic <- 0
  	examDF$c_FCMinMax <- 0
  	examDF$c_FCMid <- 0
  	examDF$c_FCSystDiff <- 0
  	examDF$c_FCDiastDiff <- 0
  	examDF$c_FCAmp <- 0
  	examDF$c_FCRate <- 0
  	examDF$c_FCRateSystolic <- 0
  	examDF$c_FCRateDiastolic
  	examDF$c_FCMA <- 0
  }
  
  # add some columns for the PLE data if present
  if(sum(pmatch(names(examDF), "c_PL", nomatch=0))!=0) {
    examDF$c_PLMax <- 0
    examDF$c_PLMin <- 0
    examDF$c_PLSystAmp <- 0
    examDF$c_PLDiastAmp <- 0
    examDF$c_PLAmp <- 0
    examDF$c_PLRateSystolic <- 0
    examDF$c_PLRateDiastolic <- 0
    examDF$c_PLMA <- 0
  }
  
  # add some columns for the activity data if present
  if(sum(pmatch(names(examDF), "c_SE", nomatch=0))!=0) {
    examDF$c_SEMA <- 0
    examDF$c_SEMax <- 0
    examDF$c_SEMin <- 0
    examDF$c_SEProc <- 0
    examDF$c_SEProcMA <- 0
    examDF$c_SEMaxDiff <- 0
    examDF$c_SEMinDiff <- 0
    examDF$c_SEAmp <- 0
    examDF$c_SEResult <- 0
    examDF$c_SEAbstrct1 <- 0
    examDF$c_SEAbstrct2 <- 0
  }
  
  ########################### artifact channels ###########
  
  examDF$Artifacts_a <- 0
  
  # add the pneumo artifact channels to the data frame
  
  examDF$UPneumo_a <- ""
  examDF$UPneumoInh_a <- ""
  examDF$UPneumoMid_a <- ""
  examDF$UPneumoExh_a <- ""
  examDF$UPneumoDiff_a <- ""
  examDF$UPneumoUnresponse_a <- ""
  
  examDF$LPneumo_a <- ""
  examDF$LPneumoInh_a <- ""
  examDF$LPneumoMid_a <- ""
  examDF$LPneumoExh_a <- ""
  examDF$LPneumoDiff_a <- ""
  examDF$LPneumoDiff_a <- ""
  examDF$LPneumoUnresponse_a <- ""
  
  examDF$Pneumo_a <- ""
  
  # add the EDA artifact channels to the data frame
  examDF$AutoEDA_a <- 0
  examDF$AutoEDAMid_a <- 0
  examDF$AutoEDAPeak_a <- 0
  examDF$AutoEDADiff_a <- 0
  examDF$AutoEDABase_a <- 0
  examDF$AutoEDAUnresponse_a <- 0
  
  # add the Manual EDA artifact channels to the data frame
  examDF$ManualEDA_a <- 0
  examDF$ManualEDAMid_a <- 0
  examDF$ManualEDAPeak_a <- 0
  examDF$ManualEDADiff_a <- 0
  examDF$ManualEDABase_a <- 0
  examDF$ManualEDAUnresponse_a <- 0
  
  # filtered EDA artifact channels
  # examDF$EDAFilt_a <- 0
  # examDF$EDAFiltMax_a <- 0
  # examDF$EDAFiltMin_a <- 0
  # examDF$EDAFiltMid_a <- 0
  # examDF$EDAFiltDiff_a <- 0
  
  
  # add the cardio artifact channels to the data frame
  examDF$Cardio1_a <- 0
  examDF$CardioSystolic_a <- 0
  examDF$CardioDiastolic_a <- 0
  examDF$CardioMid_a <- 0
  examDF$CardioAmp_a <- 0
  examDF$CardioMA_a <- 0
  
  ### add artifact chanels for RBPF, unresponse, and arrhythmia
  examDF$CardioRBPF_a <- 0
  examDF$CardioRBPFMessage_a <- 0
  examDF$CardioArrhythmia_a <- 0
  examDF$CardioArrhythmiaMessage_a <- 0
  examDF$CardioUnresponse_a <- 0
  examDF$CardioUnresponseMessage_a <- 0
  examDF$CardioRateMessage_a <- 0
  
  # add the eCardio artifact channels to the data frame
  if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0))!=0) {
  	examDF$eCardio_a <- 0
  	examDF$eCardioSystolic_a <- 0
  	examDF$eCardioDiastolic_a <- 0
  	examDF$eCardioMid_a <- 0
  	examDF$eCardioAmp_a <- 0
  	examDF$eCardioMA_a <- 0
  	examDF$eCardioRBPF_a <- 0
  	examDF$eCardioArrhythmia_a <- 0
  	examDF$eCardioUnresponse_a <- 0
  	examDF$eCardioUnresponse_a <- 0
  
  }
  
  # add the FC cardio artifact channels to the data frame
  if(sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0) {
  	examDF$FC_a <- 0
  	examDF$FCSystolic_a <- 0
  	examDF$FCDiastolic_a <- 0
  	examDF$FCMid_a <- 0
  	examDF$FCAmp_a <- 0
  	examDF$FCMA_a <- 0
  	examDF$FCRBPF_a <- 0
  	examDF$FCArrhythmia_a <- 0
  	examDF$FCUnresponse_a <- 0
  	examDF$FCUnresponseMessage_a <- 0
  
  }
  
  # add the PLE artifact channels
  if(sum(pmatch(names(examDF), "c_PL", nomatch=0))!=0) {
  	examDF$PL_a <- 0
  	examDF$PLUnresponse_a <- 0
  	examDF$PLUnresponseMessage_a <- 0
  	examDF$PLArrhythmia_a <- 0
  	examDF$PLArrhythmiaMessage_a <- 0
  }
  
  # add the activity artifact channels to the data frame
  if(sum(pmatch(names(examDF), "c_SE", nomatch=0))!=0) {
  	examDF$SE_a <- 0
  	examDF$SEMA_a <- 0
  	examDF$SEMax_a <- 0
  	examDF$SEMin_a <- 0
  	examDF$SEAmp_a <- 0
  	examDF$SEresult_a <- 0
  	examDF$SEAbstrct1_a <- 0
  	examDF$SEAbstrct2_a <- 0
  	examDF$SEUnresponse_a <- 0
  	examDF$SEUnresponseMessage_a <- 0
  }
  
  # add the answer artifact channel to the data frame
  examDF$Answer_a <- 0
  
  ###########################  feature extraction  #################
  
  # add the UPneumoExtract and LPneumoExtract columns to the data frame
  examDF$UPneumoExtract <- 0
  examDF$LPneumoExtract <- 0
  examDF$UPneumoMeasure <- 0
  examDF$LPneumoMeasure <- 0
  examDF$UPneumoRank <- 0
  examDF$LPneumoRank <- 0
  examDF$UPneumoScore <- 0
  examDF$LPneumoScore <- 0
  examDF$UPneumoIntegerScore <- 0
  examDF$LPneumoIntegerScore <- 0
  examDF$UPneumoCQ <- 0
  examDF$LPneumoCQ <- 0
  examDF$UPneumo_RRM <- 0
  examDF$LPneumo_RRM <- 0
  examDF$UPneumo_miritelloRank <- 0
  examDF$LPneumo_miritelloRank <- 0
  examDF$UPneumo_ipZ <- 0
  examDF$LPneumo_ipZ <- 0
  
  ### add additional Auto EDA columns
  examDF$AutoEDAExtract <- 0
  examDF$AutoEDAMeasure <- 0
  examDF$AutoEDADuration <- 0
  examDF$AutoEDAComplexity <- 0
  examDF$AutoEDARank <- 0
  examDF$AutoEDAScore <- 0
  examDF$AutoEDACQ <- 0
  examDF$AutoEDAIntegerScore <- 0
  examDF$AutoEDA_RRM <- 0
  examDF$AutoEDA_miritelloRank <- 0
  examDF$AutoEDA__ipZ <- 0
  
  ### add additional Manual EDA columns
  examDF$ManualEDAExtract <- 0
  examDF$ManualEDAMeasure <- 0
  examDF$ManualEDADuration <- 0
  examDF$ManualEDAComplexity <- 0
  examDF$ManualEDARank <- 0
  examDF$ManualEDAScore <- 0
  examDF$ManualEDACQ <- 0
  examDF$ManualEDAIntegerScore <- 0
  examDF$ManualEDA_RRM <- 0
  examDF$ManualEDA_miritelloRank <- 0
  examDF$ManualEDA__ipZ <- 0
  
  ### add additional cardio columns here
  examDF$CardioExtract <- 0
  examDF$CardioMeasure <- 0
  examDF$CardioDuration <- 0
  examDF$CardioComplexity <- 0
  examDF$CardioPeak <- ""
  examDF$CardioRate <- 0
  examDF$CardioRank <- 0
  examDF$CardioScore <- 0
	examDF$Cardio <- 0
  examDF$CardioIntegerScore <- 0
  examDF$Cardio_RRM <- 0
  examDF$Cardio_miritelloRank <- 0
  examDF$Cardio_ipZ <- 0
  
  ### add additional eCardio columns here
  if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0))!=0) {
  	examDF$eCardioExtract <- 0
  	examDF$eCardioMeasure <- 0
  	examDF$eCardioRank <- 0
  	examDF$eCardioScore <- 0
  	examDF$eCardioCQ
  	examDF$eCardioIntegerScore <- 0
  	examDF$eCardio_RRM <- 0
  	examDF$eCardio_miritelloRank <- 0
  	examDF$eCardio_ipZ <- 0
  }
  
  ### add additional finger cuff columns here
  if(sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0) {
  	examDF$FCExtract <- 0
  	examDF$FCMeasure <- 0
  	examDF$FCRank <- 0
  	examDF$FCScore <- 0
  	examDF$FCScore <- 0
  	examDF$FCIntegerScore <- 0
  	examDF$FC_RRM <- 0
  	examDF$FC_miritelloRank <- 0
  	examDF$FC_ipZ <- 0
  }
  
  ### add additional PLE columns here
  if(sum(pmatch(names(examDF), "c_PLE", nomatch=0))!=0) {
  	examDF$PLEExtract <- 0
  	examDF$PLEMeans <- 0
  	examDF$PLEMeasure <- 0
  	examDF$PLERank <- 0
  	examDF$PLEScore <- 0
  	examDF$PLEScore
  	examDF$PLEIntegerScore <- 0
  	examDF$PLE_RRM <- 0
  	examDF$PLE_miritelloRank <- 0
  	examDF$PLE_ipZ <- 0
  }
  
  ###
  
  return(examDF)
  
} # end addColumnsFn function

