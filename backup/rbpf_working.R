# RBPF
# 5-3-2016
# Raymond Nelson

setwd("~/Dropbox/R/NCCA_ASCII_Parse/data/RBPF/markCMs")

load("WorkingData7.RDA")

getSegment=TRUE
examNum=1
seriesNum=2
chartNum=1

printPlot=FALSE

myCardio <- NEWFIELD__MARCUS__PEAD_7_9_2015_Data$c_Cardio1[NEWFIELD__MARCUS__PEAD_7_9_2015_Data$seriesName==1]

myDF <- NEWFIELD__MARCUS__PEAD_7_9_2015_Data

unique(myDF$seriesName)

seriesDF <- myDF[myDF$seriesName==1,]
seriesDF <- myDF[myDF$seriesName==2,]
seriesDF <- myDF[myDF$seriesName==3,]

unique(seriesDF$chartName)

chartDF <- seriesDF[seriesDF$chartName=="1_01A",]
chartDF <- seriesDF[seriesDF$chartName=="2_01A",]
chartDF <- seriesDF[seriesDF$chartName=="3_01A",]

UP <- ratePerMin(chartDF$c_UPneumoSm, buffer=30, lowPass=FALSE)
LP <- ratePerMin(chartDF$c_LPneumoSm, buffer=30, lowPass=FALSE)

1-exp(-abs(log(UP/LP)))

PRate <- mean(c(UP, LP))

rbpfRate <- ratePerMin(chartDF$c_CardioMid, buffer=8, lowPass = TRUE)

1-exp(-abs(log(PRate/rbpfRate)))



cor(chartDF$c_UPneumoSm, chartDF$c_CardioMid)

cor(chartDF$c_UPneumoSm, chartDF$c_LPneumoSm)


ratePerMin(chartDF$c_CardioMid, buffer=15, lowPass = TRUE)
ratePerMin(chartDF$c_CardioMid, buffer=15, lowPass = FALSE)
ratePerMin(chartDF$c_CardioSystolic, buffer=15, lowPass = TRUE)
ratePerMin(chartDF$c_CardioSystolic, buffer=15, lowPass = FALSE)


LP

# chartDF$c_CardioSystolic
rachartDF$c_UPneumoSm
# chartDF$c_LPneumoSm

ratePerMin(x=chartDF$c_CardioSystolic, buffer=40, peaks="upper", dataRate=cps, lowPass=TRUE)
ratePerMin(x=chartDF$c_UPneumoSm, buffer=40, peaks="upper", dataRate=cps, lowPass=TRUE)
ratePerMin(x=chartDF$c_LPneumoSm, buffer=40, peaks="upper", dataRate=cps, lowPass=TRUE)


myDF <- NEWFIELD__MARCUS__PEAD_7_9_2015_Data


myCardio <- myDF[myDF$seriesName==2,'c_Cardio1']
ratePerMin(myCardio, buffer=3, lowPass = TRUE)

myPneumo <- myDF[myDF$seriesName==2,'c_UPneumo']
ratePerMin(myPneumo, buffer=40, lowPass = TRUE)

myCardioMid <- myDF[myDF$seriesName==2,'c_CardioMid']
ratePerMin(myCardioMid, buffer=15, lowPass = TRUE)


ts.plot(myDF)

# these two give the same result - bounded by 0 and 1
1-exp(-abs(log(9.99/10)))
1-exp(-abs(log(10/9.99)))


