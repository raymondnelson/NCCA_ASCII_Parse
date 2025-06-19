########################    init parameters for scaling and plotting    #############################

# all of these are in the NCCAASCII_init.R script

# print("init parameters for plotting")

# y axis scale for plotting 
yMax <- 1000
yMin <- -1000
yRange <- yMax - yMin

# scale sizes for plotting each channel
# scaleVals <- c(30, 30, 150, 35, 25, 12, 35)
scaleVals <- c(.1*yRange, .1*yRange, .4*yRange, .20*yRange, .075*yRange, .035*yRange, .10*yRange, .12*yRange)
names(scaleVals) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio")

# scale offset values for each channel
# yOffset <- c(130, 70, 10, -45, -110, -145, -75)
yOffset <- c(yMax-(.125*yRange), yMax-(.3*yRange), yMax-(.48*yRange), yMin+(.37*yRange), yMin+(.19*yRange), yMin+(.085*yRange), yMin+(.29*yRange), yMin+(.29*yRange))
names(yOffset) <- c("uPneumo", "lPneumo", "eda", "cardio", "ple", "activity", "FC", "eCardio")

