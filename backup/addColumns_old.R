# add columns to the examDF
# 5-4-2016
# Raymond Nelson

# this script is called iteratively by a loop in the sigProc() function

addColsFn <- function(x=examDF) {
  
  examDF <- x
  # function to add data columns to the examDF 
  # these colums will be populated during signal processing
  
  # rep(0, times=nrow(examDF))
  
  # add new columns for the processed pneumo Data
  examDF$c_UPneumoSm <- 0
  examDF$c_UPneumoInh <- 0
  examDF$c_UPneumoExh <- 0
  examDF$c_UPneumoMid <- 0
  examDF$c_UPneumoInhDiff <- 0
  examDF$c_UPneumoExhDiff <- 0
  examDF$c_UPneumoDiff <- 0
  
  examDF$c_LPneumoSm <- 0
  examDF$c_LPneumoInh <- 0
  examDF$c_LPneumoExh <- 0
  examDF$c_LPneumoMid <- 0
  examDF$c_LPneumoInhDiff <- 0
  examDF$c_LPneumoExhDiff <- 0
  examDF$c_LPneumoDiff <- 0
  
  examDF$c_PneumoDiff <- 0
  
  # add some EDA columns
  examDF$c_AutoEDA <- 0
  examDF$c_AutoEDABase <- 0
  examDF$c_AutoEDAPeak <- 0
  examDF$c_AutoEDAMid <- 0
  examDF$c_AutoEDAPeakDiff <- 0
  examDF$c_AutoEDABaseDiff <- 0
  
  # add some new columns for the processed cardio data
  examDF$c_CardioSystolic <- 0
  examDF$c_CardioDiastolic <- 0
  examDF$c_CardioMinMax <- 0
  examDF$c_CardioMid <- 0
  # CardioMA is a very slow smoothed cardio
  examDF$c_CardioMA <- 0
  examDF$c_CardioSystDiff <- 0
  examDF$c_CardioDyastDiff <- 0
  examDF$c_CardioAmp <- 0
  
  # add some columns for the PLE data if present
  if(sum(pmatch(names(examDF), "c_PL", nomatch=0))!=0) {
    examDF$c_PLMax <- 0
    examDF$c_PLMin <- 0
    examDF$c_PLMA <- 0
    examDF$c_PLAmp <- 0
  }
  
  # add some columns for the activity data if present
  if(sum(pmatch(names(examDF), "c_SE", nomatch=0))!=0) {
    examDF$c_SEMax <- 0
    examDF$c_SEMin <- 0
    examDF$c_SEMA <- 0
    examDF$c_SEMaxDiff <- 0
    examDF$c_SEMinDiff <- 0
    examDF$c_SEAmp <- 0
  }
  
  return(examDF)
  
} # end addColsFn()


