myCardioData <- cbind.data.frame(chartDF$c_CardioDiastolic,chartDF$c_CardioSystolic,chartDF$c_Cardio1)
names(myCardioData) <- c("c_CardioDiastolic", "c_CardioSystolic", "c_Cardio1")

myCardioData <- myCardioData[1:750,]

g <- ggplot()

g <- g + geom_line(data=myCardioData, aes(x=(1:nrow(myCardioData)), y=c_Cardio1), color="blue", size=.75) + coord_cartesian(ylim=c(-10, 10))
g <- g + geom_line(data=myCardioData, aes(x=(1:nrow(myCardioData)), y=c_CardioDiastolic), size=.75, color="green4") + coord_cartesian(ylim=c(-10, 10))
g <- g + geom_line(data=myCardioData, aes(x=(1:nrow(myCardioData)), y=c_CardioSystolic), color="red", size=.75) + coord_cartesian(ylim=c(-10, 10))

print(g)
