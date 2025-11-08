

# "Sat Oct 25 12:57:50 2025"

chartDF <- D16C0Q_Data[D16C0Q_Data$chartName == "03A",]

# View(chartDF)

seg <- c(2000:2500)

DAT_raw <- chartDF$EDA1[seg]

plot.ts(DAT_raw)


DAT_man <- chartDF$c_ManualEDA[seg]

plot.ts(DAT_man, col="red")

source("~/Dropbox/R/NCCA_ASCII_Parse/sigProcHelper.R")

DAT_test <- boxcarSmoothFn(DAT_raw, mult=.125)
DAT_test2 <- rev(boxcarSmoothFn(rev(DAT_test), mult=.125))

plot.ts(DAT_test)
plot.ts(DAT_test2)

DAT_autoEDA <- chartDF$c_AutoEDA[seg]

plot.ts(DAT_autoEDA)

DAT_EDA2 <- chartDF$EDA1[seg]

plot.ts(DAT_EDA2)

DAT_EDA1 <- chartDF$c_EDA1[seg]

plot.ts(DAT_EDA1, col="blue")



        