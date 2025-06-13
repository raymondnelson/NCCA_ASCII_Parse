
# use EDAFilt="resp" to extract the respiration signal from the EDA data
# EDAFilt="resp"
EDAFilt="laf"

# source the sigProcHelper script to load the helper functions for signal processing
source('~/Dropbox/R/NCCA_ASCII_Parse/pneumoSigProc.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/EDASigProc.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/cardioSigProc.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/FCSigProc.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/eCardioSigProc.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/PLESigProc.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/activitySigProc.R', echo=FALSE)

# source('~/Dropbox/R/NCCA_ASCII_Parse/EDARespFilters.R', echo=FALSE)

source('~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R', echo=FALSE)
source('~/Dropbox/R/NCCA_ASCII_Parse/sigProc.R', echo=FALSE)

# use 1hz to 2hz 8th order to create a ringing EDA
# .5hz to 2hz works OK
lowPass <- lowPass8th2hz_resp
highPass <- highPass8th1hz_resp

# EDABandPassFilter <- bandPass8th.5hz2hz_resp

# lowPass_resp <- lowPass5th.5_resp
# highPass_resp <- highPass5th.1_resp

# lowPass_resp <- lowPass8th1hz_resp
# highPass_resp <- highPass8th.5_resp

# lowPass_resp <- lowPass8th2hz_resp
# highPass_resp <- highPass8th.13_resp

# use 1hz to 2hz 8th order to create a ringing EDA
# .5hz to 2hz works OK
# lowPass_resp <- lowPass8th2hz_resp
# highPass_resp <- highPass8th1hz_resp

# turn off the EDA and pneumo filters for Axciton data
# may need to use this switch if the global env is cleared or the init is reloaded
# useFilters=FALSE
if(useFilters==FALSE) {
  EDAHighPass=FALSE
  EDALowPass=FALSE
  PneumoLowPass=FALSE
}

sigProc(x=uniqueExams, EDAH=EDAHighPass, EDAL=EDALowPass, output=FALSE, showNames=TRUE)


source('~/Dropbox/R/NCCA_ASCII_Parse/scaleOffsetData.R')
ScaleOffsetDataFn(x=uniqueExams, output=FALSE, showNames=TRUE)


source('~/Dropbox/R/NCCA_ASCII_Parse/nonstimArtifacts.R', echo=FALSE)

getExamFn(x=uniqueExams,
          examNum="ALL",
          seriesNum="ALL",
          chartNum="ALL",
          segmentNum=NULL,
          showNames=TRUE,
          output=FALSE,
          makeDF=TRUE,
          examFUN=NULL,
          seriesFUN=NULL,
          chartFUN="nonStimArtifactFn",
          segmentFUN=NULL )


source('~/Dropbox/R/NCCA_ASCII_Parse/chartPlot.R', echo=TRUE)

source('~/Dropbox/R/NCCA_ASCII_Parse/segmentPlot.R', echo=TRUE)

EDAFilt="laf"


