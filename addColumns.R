# add columns for signal processing and data analysis

addColumnsFn <- function(x=examDF) {
  # R function to add useful columns to the _Data data frame 
  # x input is the examDF (obtained from the _MeasurementsDF)
  # called by the dataParse() function in the NCCAASCIIParseHelperFunctions.R script
  
  # Aug 7 2023
  # added columns to capture the prestim activity measurement for UP LP AutoEDA Manual EDA Cardio PLE and Activity 
  
  examDF <- x
  
  ############### add the centered and processed data columns ################
  
  #### processed pneumo Data ####
  
  {
    # columns that start with c_ are centered at 0 for the start value
    
    examDF$c_UPneumoSm <- 0 # smoothed pneumo data
    examDF$c_UPneumoInh <- 0 # peak to peak inhalation line
    examDF$c_UPneumoExh <- 0 # peak to peak exhaltion line
    examDF$c_UPneumoMid <- 0 # Pneumo mid line
    examDF$c_UPneumoInhDiff <- 0 # distance from mid line to inhalation line
    examDF$c_UPneumoExhDiff <- 0 # distance from mid line to exhalation line
    examDF$c_UPneumoDiff <- 0 # difference between inhalation and exhalation line
    examDF$c_UPneumoRate <- 0 # respiration rate at each cycle
    examDF$c_UPneumoExcursion <- 0 # respiration line excursion
    
    # Oct 13, 2024
    examDF$c_UPneumo_Q25 <- 0  # 25th quantile
    examDF$c_UPneumo_Q50 <- 0  # median
    examDF$c_UPneumo_Q75 <- 0  # 75th quantile
    
    # May 14, 2025
    examDF$c_UPneumoRate0 <- 0 # respiration rate excluding the answer distortion buffer
    examDF$c_UPneumoExcursion0 <- 0 # respiration excursion excluding the answer  buffer
    examDF$c_UpneumoAmp <- 0 # respiration amplitude caliper
    examDF$c_UpneumoAmp0 <- 0 # respiration amplitude caliper excluding answer buffer
    
    
    examDF$c_LPneumoSm <- 0
    examDF$c_LPneumoInh <- 0
    examDF$c_LPneumoExh <- 0
    examDF$c_LPneumoMid <- 0
    examDF$c_LPneumoInhDiff <- 0
    examDF$c_LPneumoExhDiff <- 0
    examDF$c_LPneumoDiff <- 0
    examDF$c_LPneumoRate <- 0
    examDF$c_LPneumoExcursion <- 0
    
    # Oct 13, 2024
    examDF$c_LPneumo_Q25 <- 0 # 25th quantile
    examDF$c_LPneumo_Q50 <- 0  # median
    examDF$c_LPneumo_Q75 <- 0  # 75th quantile
    
    # May 14, 2025
    examDF$c_LPneumoRate0 <- 0 # respiration rate excluding the answer distortion buffer
    examDF$c_LPneumoExcursion0 <- 0 # respiration excursion excluding the answer  buffer
    examDF$c_LpneumoAmp <- 0 # respiration amplitude caliper
    examDF$c_LpneumoAmp0 <- 0 # respiration amplitude caliper excluding answer buffer
    
    # column for the difference between upper and lower pneumo
    examDF$c_PneumoDiff <- 0
  }
  
  #### Manual EDA data ####
  
  {
    examDF$c_ManualEDA <- 0 # some smoothing to to improve feature extraction
    examDF$c_ManualEDAPeak <- 0 # peak to peak line
    examDF$c_ManualEDAMid <- 0 # eda mid line
    examDF$c_ManualEDAPeakDiff <- 0 # eda mid line to peak line distance
    examDF$c_ManualEDABaseDiff <- 0 # eda mid line to baseline
    examDF$c_ManualEDADiff <- 0
    
    examDF$c_ManualEDA_Excursion <- 0 
    examDF$c_ManualEDA_Amplitude <- 0
    
    # c_ManualEDATonicity is a vector to indicate if data are tonic
    examDF$c_ManualEDATonicity <- 0
    
    # c_ManualEDABase is a very slow EDA baseline
    examDF$c_ManualEDABase <- 0
    
    # Oct 13, 2024
    examDF$c_ManualEDA_Q25 <- 0  # 25th quantile
    examDF$c_ManualEDA_Q50 <- 0  # median
    examDF$c_ManualEDA_Q75 <- 0  # 75th quantile
  }
  
  #### Manual EDA 2 data ####
  
  examDF$c_ManualEDA2 <- 0
  
  #### Auto EDA columns ####
  
  {
    examDF$c_AutoEDA <- 0 # auto centering and smoothed
    examDF$c_AutoEDAPeak <- 0 # peak to peak line
    examDF$c_AutoEDAMid <- 0 # eda mid line
    examDF$c_AutoEDAPeakDiff <- 0 # eda mid line to peak line distance
    examDF$c_AutoEDABaseDiff <- 0 # eda mid line to baseline
    examDF$c_AutoEDADiff <- 0
    
    examDF$c_AutoEDA_Excursion <- 0
    examDF$c_AutoEDA_Amplitude <- 0
    
    # c_AutoEDATonicity is a vector to indicate if data are tonic
    examDF$c_AutoEDATonicity <- 0
    
    # c_AutoEDABase is a very slow EDA baseline
    examDF$c_AutoEDABase <- 0
    
    # Oct 13, 2024
    examDF$c_AutoEDA_Q25 <- 0  # 25th quantile
    examDF$c_AutoEDA_Q50 <- 0  # median
    examDF$c_AutoEDA_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the PCAT-2 EDA ####
  
  {
    # PCAT EDA is slightly different than polygraph 
    # works best with the legacy
    # stronger centering characteristics 
    # with strong diagnostic coef
    # less management required from the user
    
    examDF$c_PCAT_EDA <- 0 # auto centering and smoothed
    examDF$c_PCAT_EDAPeak <- 0 # peak to peak line
    examDF$c_PCAT_EDAMid <- 0 # eda mid line
    examDF$c_PCAT_EDAPeakDiff <- 0 # eda mid line to peak line distance
    examDF$c_PCAT_EDABaseDiff <- 0 # eda mid line to baseline
    examDF$c_PCAT_EDADiff <- 0
    
    examDF$c_PCAT_EDA_Excursion <- 0
    examDF$c_PCAT_EDA_Amplitude <- 0
    
    # c_PCATEDATonicity is a vector to indicate if data are tonic
    examDF$c_PCAT_EDATonicity <- 0
    
    # c_PCATEDABase is a very slow EDA baseline
    examDF$c_PCAT_EDABase <- 0
    
    # Oct 13, 2024
    examDF$c_PCAT_EDA_Q25 <- 0  # 25th quantile
    examDF$c_PCAT_EDA_Q50 <- 0  # median
    examDF$c_PCAT_EDA_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the processed EDA data ####
  
  {
    # examDF$c_EDAFilt <- 0
    # examDF$c_EDAFiltMid <- 0
    # examDF$c_EDAFiltMax <- 0
    # examDF$c_EDAFiltMin <- 0
    # examDF$c_EDAFiltDiff <- 0
  }
  
  #### columns for the processed cardio data ####
  
  {
    examDF$c_CardioSystolic <- 0 # upper peak to peak line
    examDF$c_CardioDiastolic <- 0 # lower peak to peak line
    examDF$c_CardioMinMax <- 0 # not sure what this was used for
    examDF$c_CardioMid <- 0 # cardio mid line
    examDF$c_CardioSystDiff <- 0 # distance from mid to systolic lines
    examDF$c_CardioDiastDiff <- 0 # distance between mid to diastolic lines
    examDF$c_CardioAmp <- 0 # distance between the diastolic and systolic lines
    examDF$c_CardioRate <- 0 # moving cardio rate
    examDF$c_cardioRateSystolic <- 0 # systolic cardio rate (to ID arrhythmia)
    examDF$c_cardioRateDystolic <- 0 # diastolic cardio rate  (to ID arrhythmia)
    examDF$c_CardioMA <- 0 # slower moveing average for artifacts and feature extraction 
    
    examDF$c_Cardio_excursion <- 0
    
    examDF$c_CardioSRQ1 <- 0
    examDF$c_CardioSRQ2 <- 0
    examDF$c_CardioSRQ3 <- 0
    examDF$c_CardioSRQ4 <- 0
    
    # Oct 13, 2024
    examDF$c_Cardio_Q25 <- 0  # 25th quantile
    examDF$c_Cardio_Q50 <- 0  # median
    examDF$c_Cardio_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the eCardio channel ####
  
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
  	
  	examDF$c_eCardio_excursion <- 0
  	
  	# Oct 13, 2024
  	examDF$c_eCardio_Q25 <- 0  # 25th quantile
  	examDF$c_eCardio_Q50 <- 0  # median
  	examDF$c_eCardio_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the finger/forearm cuff cardio channel ####
  
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
  	
  	examDF$c_FC_excursion <- 0
  	
  	# Oct 13, 2024
  	examDF$c_FC_Q25 <- 0  # 25th quantile
  	examDF$c_FC_Q50 <- 0  # median
  	examDF$c_FC_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the PLE data if present ####
  
  if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0))!=0) {
    examDF$c_PPG1Max <- 0 # upper peak to peak line
    examDF$c_PPG1Min <- 0 # lower peak to peak line
    examDF$c_PPG1SystAmp <- 0 # difference between upper peak and mid line
    examDF$c_PPG1DiastAmp <- 0 # difference between lower peak and mid line
    examDF$c_PPG1Amp <- 0 # difference between upper and lower peak lines
    examDF$c_PPG1Rate <- 0 # cardio rate at the PLE 
    examDF$c_PPG1RateSystolic <- 0 # systolic rate may differ from diast
    examDF$c_PPG1RateDiastolic <- 0 # used for arrhythmia detection
    examDF$c_PPG1MA <- 0 # PLE/PPG moving average
    
    examDF$c_PPG1_excursion <- 0
    
    # Oct 13, 2024
    examDF$c_PPG1_Q25 <- 0  # 25th quantile
    examDF$c_PPG1_Q50 <- 0  # median
    examDF$c_PPG1_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the PCAT Vasomotor ####
  
  {
    examDF$c_PCATCardio <- 0
    examDF$c_PCATCardio_Systolic <- 0
    examDF$c_PCATCardio_Diastolic <- 0
    examDF$c_PCATCardio_MinMax <- 0
    examDF$c_PCATCardio_Mid <- 0
    examDF$c_PCATCardio_SystDiff <- 0
    examDF$c_PCATCardio_DiastDiff <- 0
    examDF$c_PCATCardio_Amp <- 0
    examDF$c_PCATCardio_Rate <- 0
    examDF$c_PCATCardio_RateSystolic <- 0
    examDF$c_PCATCardio_RateDystolic <- 0
    examDF$c_PCATCardio_MA <- 0 # a slower moving average 
    
    examDF$c_PCATCardio_excursion <- 0
    
    # Oct 13, 2024
    examDF$c_PCATCardio_Q25 <- 0  # 25th quantile
    examDF$c_PCATCardio_Q50 <- 0  # median
    examDF$c_PCATCardio_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the activity data if present ####
  
  if(sum(pmatch(names(examDF), "c_Move1", nomatch=0))!=0) {
    examDF$c_Move1MA <- 0
    examDF$c_Move1Max <- 0
    examDF$c_Move1Min <- 0
    examDF$c_Move1Proc <- 0
    examDF$c_Move1ProcMA <- 0
    examDF$c_Move1MaxDiff <- 0
    examDF$c_Move1MinDiff <- 0
    examDF$c_Move1Amp <- 0
    examDF$c_Move1Result <- 0
    examDF$c_Move1Abstrct1 <- 0
    examDF$c_Move1Abstrct2 <- 0
    
    # Oct 13, 2024
    examDF$c_Move1_Q25 <- 0  # 25th quantile
    examDF$c_Move1_Q50 <- 0  # median
    examDF$c_Move1_Q75 <- 0  # 75th quantile
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move2", nomatch=0))!=0) {
    examDF$c_Move2MA <- 0
    examDF$c_Move2Max <- 0
    examDF$c_Move2Min <- 0
    examDF$c_Move2Proc <- 0
    examDF$c_Move22ProcMA <- 0
    examDF$c_Move2MaxDiff <- 0
    examDF$c_Move2MinDiff <- 0
    examDF$c_Move2Amp <- 0
    examDF$c_Move2Result <- 0
    examDF$c_Move2Abstrct1 <- 0
    examDF$c_Move2Abstrct2 <- 0
    
    # Oct 13, 2024
    examDF$c_Move2_Q25 <- 0  # 25th quantile
    examDF$c_Move2_Q50 <- 0  # median
    examDF$c_Move2_Q75 <- 0  # 75th quantile
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move3", nomatch=0))!=0) {
    examDF$c_Move3MA <- 0
    examDF$c_Move3Max <- 0
    examDF$c_Move3Min <- 0
    examDF$c_Move3Proc <- 0
    examDF$c_Move3ProcMA <- 0
    examDF$c_Move3MaxDiff <- 0
    examDF$c_Move3MinDiff <- 0
    examDF$c_Move3Amp <- 0
    examDF$c_Move3Result <- 0
    examDF$c_Move3Abstrct1 <- 0
    examDF$c_Move3Abstrct2 <- 0
    
    # Oct 13, 2024
    examDF$c_Move3_Q25 <- 0  # 25th quantile
    examDF$c_Move3_Q50 <- 0  # median
    examDF$c_Move3_Q75 <- 0  # 75th quantile
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move4", nomatch=0))!=0) {
    examDF$c_Move4MA <- 0
    examDF$c_Move4Max <- 0
    examDF$c_Move4Min <- 0
    examDF$c_Move4Proc <- 0
    examDF$c_Move4ProcMA <- 0
    examDF$c_Move4MaxDiff <- 0
    examDF$c_Move4MinDiff <- 0
    examDF$c_Move4Amp <- 0
    examDF$c_Move4Result <- 0
    examDF$c_Move4Abstrct1 <- 0
    examDF$c_Move4Abstrct2 <- 0
    
    # Oct 13, 2024
    examDF$c_Move4_Q25 <- 0  # 25th quantile
    examDF$c_Move4_Q50 <- 0  # median
    examDF$c_Move4_Q75 <- 0  # 75th quantile
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move5", nomatch=0))!=0) {
    examDF$c_Move5MA <- 0
    examDF$c_Move5Max <- 0
    examDF$c_Move5Min <- 0
    examDF$c_Move5Proc <- 0
    examDF$c_Move5ProcMA <- 0
    examDF$c_Move5MaxDiff <- 0
    examDF$c_Move5MinDiff <- 0
    examDF$c_Move5Amp <- 0
    examDF$c_Move5Result <- 0
    examDF$c_Move5Abstrct1 <- 0
    examDF$c_Move5Abstrct2 <- 0
    
    # Oct 13, 2024
    examDF$c_Move5_Q25 <- 0  # 25th quantile
    examDF$c_Move5_Q50 <- 0  # median
    examDF$c_Move5_Q75 <- 0  # 75th quantile
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move6", nomatch=0))!=0) {
    examDF$c_Move6MA <- 0
    examDF$c_Move6Max <- 0
    examDF$c_Move6Min <- 0
    examDF$c_Move6Proc <- 0
    examDF$c_Move6ProcMA <- 0
    examDF$c_Move6MaxDiff <- 0
    examDF$c_Move6MinDiff <- 0
    examDF$c_Move6Amp <- 0
    examDF$c_Move6Result <- 0
    examDF$c_Move6Abstrct1 <- 0
    examDF$c_Move6Abstrct2 <- 0
    
    # Oct 13, 2024
    examDF$c_Move6_Q25 <- 0  # 25th quantile
    examDF$c_Move6_Q50 <- 0  # median
    examDF$c_Move6_Q75 <- 0  # 75th quantile
  }
  
  #### columns for the PTT sensor data if present ####
  # added May 8, 2024

  # added May 8, 2024  
  if(sum(pmatch(names(examDF), "PTTPPG", nomatch=0))!=0) {
    examDF$c_PTTPPG  <- 0
    examDF$c_PTTPPG_MA <- 0
    
    # Oct 13, 2024
    examDF$c_PTTPPG_Q25 <- 0  # 25th quantile
    examDF$c_PTTPPG_Q50 <- 0  # median
    examDF$c_PTTPPG_Q75 <- 0  # 75th quantile
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTECG", nomatch=0))!=0) {
    examDF$c_PTTECG <- 0
    examDF$c_PTTECG_MA <- 0
    
    # Oct 13, 2024
    examDF$c_PTTECG_Q25 <- 0  # 25th quantile
    examDF$c_PTTECG_Q50 <- 0  # median
    examDF$c_PTTECG_Q75 <- 0  # 75th quantile
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTPTT", nomatch=0))!=0) {
    examDF$c_PTTPTT <- 0
    examDF$c_PTTPTT_abs <- 0
    examDF$c_PTTPTT_MA <- 0
    
    # Oct 13, 2024
    examDF$c_PTTPTT_Q25 <- 0  # 25th quantile
    examDF$c_PTTPTT_Q50 <- 0  # median
    examDF$c_PTTPTT_Q75 <- 0  # 75th quantile
  }
  
  ################################################################
  
  ###################### artifact channels #######################
  
  # artifact column names end in _a
  
  examDF$Artifacts_a <- 0
  
  # not sure why iArtifacts
  # examDF$iArtifacts_a <- 0
  
  #### pneumo artifact channels ####
  
  {
    examDF$UPneumo_a <- 0
    examDF$UPneumoInh_a <- 0
    examDF$UPneumoMid_a <- 0
    examDF$UPneumoExh_a <- 0
    examDF$UPneumoDiff_a <- 0
    examDF$UPneumoUnresponse_a <- 0
    
    examDF$LPneumo_a <- 0
    examDF$LPneumoInh_a <- 0
    examDF$LPneumoMid_a <- 0
    examDF$LPneumoExh_a <- 0
    examDF$LPneumoDiff_a <- 0
    examDF$LPneumoDiff_a <- 0
    examDF$LPneumoUnresponse_a <- 0
    
    # parent variable for all pneumo artifacts
    examDF$Pneumo_a <- 0
  }
  
  #### Auto EDA artifact channels ####
  
  {
    examDF$AutoEDA_a <- 0 # parent
    examDF$AutoEDAMid_a <- 0
    examDF$AutoEDAPeak_a <- 0
    examDF$AutoEDADiff_a <- 0
    examDF$AutoEDABase_a <- 0
    examDF$AutoEDAUnresponse_a <- 0
  }
  
  #### Manual EDA artifact channels ####
  
  {
    examDF$ManualEDA_a <- 0 # parent
    examDF$ManualEDAMid_a <- 0
    examDF$ManualEDAPeak_a <- 0
    examDF$ManualEDADiff_a <- 0
    examDF$ManualEDABase_a <- 0
    examDF$ManualEDAUnresponse_a <- 0
  }
  
  #### filtered EDA artifact channels ####
  
  {
    # examDF$EDAFilt_a <- 0
    # examDF$EDAFiltMax_a <- 0
    # examDF$EDAFiltMin_a <- 0
    # examDF$EDAFiltMid_a <- 0
    # examDF$EDAFiltDiff_a <- 0
  }
  
  #### cardio artifact channels ####
  
  {
    examDF$Cardio1_a <- 0 # parent
    examDF$CardioSystolic_a <- 0
    examDF$CardioDiastolic_a <- 0
    examDF$CardioMid_a <- 0
    examDF$CardioAmp_a <- 0
    examDF$CardioMA_a <- 0
  }
  
  #### artifact chanels for RBPF, unresponse, and arrhythmia ####
  
  {
    examDF$CardioRBPF_a <- 0 # parent 
    examDF$CardioRBPFMessage_a <- 0
    examDF$CardioArrhythmia_a <- 0
    examDF$CardioArrhythmiaMessage_a <- 0
    examDF$CardioUnresponse_a <- 0
    examDF$CardioUnresponseMessage_a <- 0
    examDF$CardioRateMessage_a <- 0
  }
  
  #### electronic Cardio artifact channels ####
  
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
  
  #### FC cardio artifact channels - forearm or finger cuff ####
  
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
  if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0))!=0) {
  	examDF$PPG1_a <- 0 # parent 
  	examDF$PPG1Unresponse_a <- 0
  	examDF$PPG1UnresponseMessage_a <- 0
  	examDF$PPG1Arrhythmia_a <- 0
  	examDF$PPG1ArrhythmiaMessage_a <- 0
  }
  
  #### activity artifact channels ####
  
  if(sum(pmatch(names(examDF), "c_Move1", nomatch=0))!=0) {
  	examDF$Move1_a <- 0 # main artifact channell for seat activity 
  	examDF$Move1MA_a <- 0
  	examDF$Move1Max_a <- 0
  	examDF$Move1Min_a <- 0
  	examDF$Move1Amp_a <- 0
  	examDF$Move1result_a <- 0
  	examDF$Move1Abstrct1_a <- 0
  	examDF$Move1Abstrct2_a <- 0
  	examDF$Move1Unresponse_a <- 0
  	examDF$Move1UnresponseMessage_a <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move2", nomatch=0))!=0) {
    examDF$Move2_a <- 0 
    examDF$Move2MA_a <- 0
    examDF$Move2Max_a <- 0
    examDF$Move2Min_a <- 0
    examDF$Move2Amp_a <- 0
    examDF$Move2result_a <- 0
    examDF$Move2Abstrct1_a <- 0
    examDF$Move2Abstrct2_a <- 0
    examDF$Move2Unresponse_a <- 0
    examDF$Move2UnresponseMessage_a <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move3", nomatch=0))!=0) {
    examDF$Move3_a <- 0 
    examDF$Move3MA_a <- 0
    examDF$Move3Max_a <- 0
    examDF$Move3Min_a <- 0
    examDF$Move3Amp_a <- 0
    examDF$Move3result_a <- 0
    examDF$Move3Abstrct1_a <- 0
    examDF$Move3Abstrct2_a <- 0
    examDF$Move3Unresponse_a <- 0
    examDF$Move3UnresponseMessage_a <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move4", nomatch=0))!=0) {
    examDF$Move4_a <- 0 
    examDF$Move4MA_a <- 0
    examDF$Move4Max_a <- 0
    examDF$Move4Min_a <- 0
    examDF$Move4Amp_a <- 0
    examDF$Move4result_a <- 0
    examDF$Move4Abstrct1_a <- 0
    examDF$Move4Abstrct2_a <- 0
    examDF$Move4Unresponse_a <- 0
    examDF$Move4UnresponseMessage_a <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move5", nomatch=0))!=0) {
    examDF$Move5_a <- 0 
    examDF$Move5MA_a <- 0
    examDF$Move5Max_a <- 0
    examDF$Move5Min_a <- 0
    examDF$Move5Amp_a <- 0
    examDF$Move5result_a <- 0
    examDF$Move5Abstrct1_a <- 0
    examDF$Move5Abstrct2_a <- 0
    examDF$Move5Unresponse_a <- 0
    examDF$Move5UnresponseMessage_a <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "c_Move6", nomatch=0))!=0) {
    examDF$Move6_a <- 0 
    examDF$Move6MA_a <- 0
    examDF$Move6Max_a <- 0
    examDF$Move6Min_a <- 0
    examDF$Move6Amp_a <- 0
    examDF$Move6result_a <- 0
    examDF$Move6Abstrct1_a <- 0
    examDF$Move6Abstrct2_a <- 0
    examDF$Move6Unresponse_a <- 0
    examDF$Move6UnresponseMessage_a <- 0
  }
  
  #### answer artifact channel ####
  examDF$Answer_a <- 0
  
  #### PTT artifact channels ####
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTPPG", nomatch=0))!=0) {
    examDF$PTTPPG_a <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTECG", nomatch=0))!=0) {
    examDF$PTTECG_a <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTPTT", nomatch=0))!=0) {
    examDF$PTTPTT_a <- 0
  }
  
  ################  feature extraction columns  #################
  
  #### UPneumoExtract and LPneumoExtract columns ####
  
  {
    examDF$UPneumoPrestim <- 0
    examDF$UPneumoRate <- 0
    examDF$UPneumoExtract <- 0
    examDF$UPneumoMeasure <- 0
    examDF$UPneumoRank <- 0
    examDF$UPneumoScore <- 0
    
    examDF$LPneumoPrestim <- 0
    examDF$LPneumoRate <- 0
    examDF$LPneumoExtract <- 0
    examDF$LPneumoMeasure <- 0
    examDF$LPneumoRank <- 0
    examDF$LPneumoScore <- 0
    
    examDF$UPneumoIntegerScore <- 0
    examDF$UPneumoCQ <- 0
    examDF$UPneumo_RRM <- 0
    examDF$UPneumo_miritelloRank <- 0
    examDF$UPneumo_ipZ <- 0
    
    examDF$LPneumoIntegerScore <- 0
    examDF$LPneumoCQ <- 0
    examDF$LPneumo_RRM <- 0
    examDF$LPneumo_miritelloRank <- 0
    examDF$LPneumo_ipZ <- 0
    
    examDF$PneumoRate <- 0
    examDF$PnuemoRank <- 0
    examDF$PneumoScore <- 0
    
    examDF$PneumoIntegerScore <- 0
  }
  
  #### Auto EDA feature extraction columns ####
  
  {
    examDF$AutoEDAPrestim <- 0
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
    examDF$AutoEDA_ipZ <- 0
  }
  
  #### Manual EDA feature extraction columns ####
  
  {
    examDF$ManualEDAPrestim <- 0
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
    examDF$ManualEDA_ipZ <- 0
  }
  
  #### PCAT EDA feature extraction columns ####
  
  {
    examDF$PCATEDAPrestim <- 0
    examDF$PCATEDAExtract <- 0
    examDF$PCATEDAMeasure <- 0
    examDF$PCATEDADuration <- 0
    examDF$PCATEDAComplexity <- 0
    examDF$PCATEDARank <- 0
    examDF$PCATEDAScore <- 0
    examDF$PCATEDACQ <- 0
    examDF$PCATEDAIntegerScore <- 0
    examDF$PCATEDA_RRM <- 0
    examDF$PCATEDA_miritelloRank <- 0
    examDF$PCATEDA_ipZ <- 0
  }
  
  #### Cardiofeature extraction columns ####
  
  {
    examDF$CardioPrestim <- 0
    examDF$CardioRate <- 0
    examDF$CardioExtract <- 0
    examDF$CardioMeasure <- 0
    examDF$CardioDuration <- 0
    examDF$CardioComplexity <- 0
    examDF$CardioPeak <- 0
    examDF$CardioRank <- 0
    examDF$CardioScore <- 0
    examDF$CardioCQ <- 0
    examDF$CardioIntegerScore <- 0
    examDF$Cardio_RRM <- 0
    examDF$Cardio_miritelloRank <- 0
    examDF$Cardio_ipZ <- 0
    #	examDF$Cardio <- 0 # this may be an error 2019-02-12
    
    # diastolic
    examDF$CardioPrestim_d <- 0
    examDF$CardioRate_d <- 0
    examDF$CardioExtract_d <- 0
    examDF$CardioMeasure_d <- 0
    examDF$CardioDuration_d <- 0
    examDF$CardioComplexity_d <- 0
    examDF$CardioPeak_d <- 0
    examDF$CardioRank_d <- 0
    examDF$CardioScore_d <- 0
    examDF$CardioCQ_d <- 0
    examDF$CardioIntegerScore_d <- 0
    examDF$Cardio_RRM_d <- 0
    examDF$Cardio_miritelloRank_d <- 0
    examDF$Cardio_ipZ_d <- 0
    
    # systolic
    examDF$CardioPrestim_s <- 0
    examDF$CardioRate_s <- 0
    examDF$CardioExtract_s <- 0
    examDF$CardioMeasure_s <- 0
    examDF$CardioDuration_s <- 0
    examDF$CardioComplexity_s <- 0
    examDF$CardioPeak_s <- 0
    examDF$CardioRank_s <- 0
    examDF$CardioScore_s <- 0
    examDF$CardioCQ_s <- 0
    examDF$CardioIntegerScore_s <- 0
    examDF$Cardio_RRM_s <- 0
    examDF$Cardio_miritelloRank_s <- 0
    examDF$Cardio_ipZ_s <- 0
    
    # mid
    examDF$CardioPrestim_m <- 0
    examDF$CardioRate_m <- 0
    examDF$CardioExtract_m <- 0
    examDF$CardioMeasure_m <- 0
    examDF$CardioDuration_m <- 0
    examDF$CardioComplexity_m <- 0
    examDF$CardioPeak_m <- 0
    examDF$CardioRank_m <- 0
    examDF$CardioScore_m <- 0
    examDF$CardioCQ_m <- 0
    examDF$CardioIntegerScore_m <- 0
    examDF$Cardio_RRM_m <- 0
    examDF$Cardio_miritelloRank_m <- 0
    examDF$Cardio_ipZ_m <- 0
    
    # slow moving average
    examDF$CardioPrestim_v <- 0
    examDF$CardioRate_v <- 0
    examDF$CardioExtract_v <- 0
    examDF$CardioMeasure_v <- 0
    examDF$CardioDuration_v <- 0
    examDF$CardioComplexity_v <- 0
    examDF$CardioPeak_v <- 0
    examDF$CardioRank_v <- 0
    examDF$CardioScore_v <- 0
    examDF$CardioCQ_v <- 0
    examDF$CardioIntegerScore_v <- 0
    examDF$Cardio_RRM_v <- 0
    examDF$Cardio_miritelloRank_v <- 0
    examDF$Cardio_ipZ_v <- 0
  }
  
  #### FC (forearm/finger) cuff feature extraction columns ####
  
  if(sum(pmatch(names(examDF), "c_FC", nomatch=0))!=0) {
    
    examDF$FCPrestim <- 0
    examDF$FCRate <- 0
    examDF$FCExtract <- 0
    examDF$FCMeasure <- 0
    examDF$FCDuration <- 0
    examDF$FCComplexity <- 0
    examDF$FCPeak_d <- 0  
    examDF$FCRank <- 0
    examDF$FCScore <- 0
    examDF$FCScoreCQ <- 0
    examDF$FCIntegerScore <- 0
    examDF$FC_RRM <- 0
    examDF$FC_miritelloRank <- 0
    examDF$FC_ipZ <- 0
    
    # diastolic
    examDF$FCPrestim <- 0
    examDF$FCRate_d <- 0
    examDF$FCExtract_d <- 0
    examDF$FCMeasure_d <- 0
    examDF$FCDuration_d <- 0
    examDF$FCComplexity_d <- 0
    examDF$FCPeak_d <- 0
    examDF$FCRank_d <- 0
    examDF$FCScore_d <- 0
    examDF$FCCQ_d <- 0
    examDF$FCIntegerScore_d <- 0
    examDF$FC_RRM_d <- 0
    examDF$FC_miritelloRank_d <- 0
    examDF$FC_ipZ_d <- 0
    
    # systolic
    examDF$FCPrestim <- 0
    examDF$FCRate_s <- 0
    examDF$FCExtract_s <- 0
    examDF$FCMeasure_s <- 0
    examDF$FCDuration_s <- 0
    examDF$FCComplexity_s <- 0
    examDF$FCPeak_s <- 0
    examDF$FCRank_s <- 0
    examDF$FCScore_s <- 0
    examDF$FCCQ_s <- 0
    examDF$FCIntegerScore_s <- 0
    examDF$FC_RRM_s <- 0
    examDF$FC_miritelloRank_s <- 0
    examDF$FC_ipZ_s <- 0
    
    # mid
    examDF$FCPrestim <- 0
    examDF$FCRate_m <- 0
    examDF$FCExtract_m <- 0
    examDF$FCMeasure_m <- 0
    examDF$FCDuration_m <- 0
    examDF$FCComplexity_m <- 0
    examDF$FCPeak_m <- 0
    examDF$FCRank_m <- 0
    examDF$FCScore_m <- 0
    examDF$FCCQ_m <- 0
    examDF$FCIntegerScore_m <- 0
    examDF$FC_RRM_m <- 0
    examDF$FC_miritelloRank_m <- 0
    examDF$FC_ipZ_m <- 0
    
    # slow moving average
    examDF$FCPrestim <- 0
    examDF$FCRate_v <- 0
    examDF$FCExtract_v <- 0
    examDF$FCMeasure_v <- 0
    examDF$FCDuration_v <- 0
    examDF$FCComplexity_v <- 0
    examDF$FCPeak_v <- 0
    examDF$FCRank_v <- 0
    examDF$FCScore_v <- 0
    examDF$FCCQ_v <- 0
    examDF$FCIntegerScore_v <- 0
    examDF$FC_RRM_v <- 0
    examDF$FC_miritelloRank_v <- 0
    examDF$FC_ipZ_v <- 0
    
  }

  #### eCardio feature extraction columns ####
  
  if(sum(pmatch(names(examDF), "c_eCardio", nomatch=0))!=0) {
  
    examDF$eCardioPrestim <- 0
  	examDF$eCardioRate <- 0
  	examDF$eCardioExtract <- 0
  	examDF$eCardioMeasure <- 0
  	examDF$eCardioDuration <- 0
  	examDF$eCardioComplexity <- 0
  	examDF$eCardioPeak <- 0
  	examDF$eCardioRank <- 0
  	examDF$eCardioScore <- 0
  	examDF$eCardioCQ
  	examDF$eCardioIntegerScore <- 0
  	examDF$eCardio_RRM <- 0
  	examDF$eCardio_miritelloRank <- 0
  	examDF$eCardio_ipZ <- 0
  	
  }
  
  #### PLE feature extraction columns ####
  
  if(sum(pmatch(names(examDF), "c_PPG1", nomatch=0))!=0) {
    examDF$PPG1Prestim <- 0
  	examDF$PPG1Rate <- 0
  	examDF$PPG1Extract <- 0
  	examDF$PPG1Means <- 0
  	examDF$PPG1Measure <- 0
  	examDF$PPG1Rank <- 0
  	examDF$PPG1Score <- 0
  	examDF$PPG1Score
  	examDF$PPG1ScoreCQ <- 0
  	examDF$PPG1IntegerScore <- 0
  	examDF$PPG1_RRM <- 0
  	examDF$PPG1_miritelloRank <- 0
  	examDF$PPG1_ipZ <- 0
  }
  
  #### PCAT vasomotor extraction columns ####
  
  {
    examDF$PCATCardoPrestim <- 0
    examDF$PCATCardioRate <- 0
    examDF$PCATCardioExtract <- 0
    examDF$PCATCardioMeans <- 0
    examDF$PCATCardioMeasure <- 0
    examDF$PCATCardioRank <- 0
    examDF$PCATCardioScore <- 0
    examDF$PCATCardioScore
    examDF$PCATCardioScoreCQ <- 0
    examDF$PCATCardioIntegerScore <- 0
    examDF$PCATCardio_RRM <- 0
    examDF$PCATCardio_miritelloRank <- 0
    examDF$PCATCardio_ipZ <- 0
  }
  
  #### PTT feature extraction columns ####
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTPPG", nomatch=0))!=0) {
    examDF$PTTPPGPrestim <- 0
    examDF$PTTPPGRate <- 0
    examDF$PTTPPGExtract <- 0
    examDF$PTTPPGMeans <- 0
    examDF$PTTPPGMeasure <- 0
    examDF$PTTPPGRank <- 0
    examDF$PTTPPGScore <- 0
    examDF$PTTPPGScore
    examDF$PTTPPGScoreCQ <- 0
    examDF$PTTPPGIntegerScore <- 0
    examDF$PTTPPG_RRM <- 0
    examDF$PTTPPG_miritelloRank <- 0
    examDF$PTTPPG_ipZ <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTECG", nomatch=0))!=0) {
    examDF$PTTECGPrestim <- 0
    examDF$PTTECGRate <- 0
    examDF$PTTECGExtract <- 0
    examDF$PTTECGMeans <- 0
    examDF$PTTECGMeasure <- 0
    examDF$PTTECGRank <- 0
    examDF$PTTECGScore <- 0
    examDF$PTTECGScore
    examDF$PTTECGScoreCQ <- 0
    examDF$PTTECGIntegerScore <- 0
    examDF$PTTECG_RRM <- 0
    examDF$PTTECG_miritelloRank <- 0
    examDF$PTTECG_ipZ <- 0
  }
  
  # added May 8, 2024
  if(sum(pmatch(names(examDF), "PTTPTT", nomatch=0))!=0) {
    examDF$PTTPTTPrestim <- 0
    examDF$PTTPTTRate <- 0
    examDF$PTTPTTExtract <- 0
    examDF$PTTPTTMeans <- 0
    examDF$PTTPTTMeasure <- 0
    examDF$PTTPTTRank <- 0
    examDF$PTTPTTScore <- 0
    examDF$PTTPTTScore
    examDF$PTTPTTScoreCQ <- 0
    examDF$PTTPTTIntegerScore <- 0
    examDF$PTTPTT_RRM <- 0
    examDF$PTTPTT_miritelloRank <- 0
    examDF$PTTPTT_ipZ <- 0
  }
  
  #### return ####
  
  return(examDF)
  
} # end addColumnsFn function

