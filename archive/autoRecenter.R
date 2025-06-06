# recenter
# 5-3-2016

# removed from the chartPlot.R script




###### keep the data on the plot ######

# # re-offset the pneumo data to keep the data on the plot
# if(max(chartDF$c_UPneumoSm) > 165) {
#   newPneumoOffset <- max(chartDF$c_UPneumoSm[1:(length(chartDF$c_UPneumoSm)-150)]) - 166
#   chartDF$c_UPneumoSm <- chartDF$c_UPneumoSm - newPneumoOffset
#   chartDF$c_LPneumoSm <- chartDF$c_LPneumoSm - newPneumoOffset
#   # include the mid inhalation and exhalation trend lines
#   chartDF$c_UPneumoMid <- chartDF$c_UPneumoMid - newPneumoOffset
#   chartDF$c_LPneumoMid <- chartDF$c_LPneumoMid - newPneumoOffset
#   chartDF$c_UPneumoInh <- chartDF$c_UPneumoInh - newPneumoOffset
#   chartDF$c_LPneumoInh <- chartDF$c_LPneumoInh - newPneumoOffset
#   chartDF$c_UPneumoExh <- chartDF$c_UPneumoExh - newPneumoOffset
#   chartDF$c_LPneumoExh <- chartDF$c_LPneumoExh - newPneumoOffset
#   yOffset[1] <- yOffset[1] - newPneumoOffset
#   yOffset[1] <- yOffset[2] - newPneumoOffset
#   pneumoWarning <- "UNSTABLE PNEUMO DATA1"
# } else {
#   pneumoWarning <- "none"
#   newPneumoOffset=0
#   }






### rescale the cardio data if necessary

# check the cardio range
# cardioWarning <- ifelse( diff(range(chartDF$c_Cardio1[firstEvent:lastEventEnd]))>=330,
#                          "UNSTABLE CARDIO DATA1",
#                          "none"
# )

# if(cardioWarning=="UNSTABLE CARDIO DATA") {
#   # rescale the unstable cardio
#   reScaleCardio <- 330/diff(range(chartDF$c_Cardio1[firstEvent:lastEventEnd]))
#   chartDF$c_Cardio1 <- chartDF$c_Cardio1 * reScaleCardio
#   chartDF$c_CardioMid <- chartDF$c_CardioMid * reScaleCardio
#   chartDF$c_CardioMA <- chartDF$c_CardioMA * reScaleCardio
#   chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic * reScaleCardio
#   chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic * reScaleCardio
#   # re offset the unstable cardio
#   reOffsetCardio <- min(chartDF$c_Cardio1[firstEvent:lastEventEnd]) - -165
#   chartDF$c_Cardio1 <- chartDF$c_Cardio1 - reOffsetCardio
#   chartDF$c_CardioMid <- chartDF$c_CardioMid - reOffsetCardio
#   chartDF$c_CardioMA <- chartDF$c_CardioMA - reOffsetCardio
#   chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic - reOffsetCardio
#   chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic - reOffsetCardio
#   yOffset[4] <- yOffset[4] - reOffsetCardio
# } else reOffsetCardio=0






# re-offset the stable cardio data to keep the data on the plot
# if(cardioWarning=="none") {
# 
#   if(min(chartDF$c_Cardio1[firstEvent:lastEventEnd]) <= -165) {
#     newCardio1Offset <- min(chartDF$c_Cardio1[firstEvent:lastEventEnd]) - -165
#     cardioWarning <- "UNSTABLE CARDIO DATA2"
#   } else if(max(chartDF$c_Cardio1[firstEvent:lastEventEnd]) >= 165) {
#     newCardio1Offset <- max(chartDF$c_Cardio1[firstEvent:lastEventEnd]) - 165
#     cardioWarning <- "UNSTABLE CARDIO DATA3"
#   } else newCardio1Offset <- 0
# 
#   if(newCardio1Offset!=0) {
#     chartDF$c_Cardio1 <- chartDF$c_Cardio1 - newCardio1Offset
#     chartDF$c_CardioMid <- chartDF$c_CardioMid - newCardio1Offset
#     chartDF$c_CardioMA <- chartDF$c_CardioMA - newCardio1Offset
#     chartDF$c_CardioDiastolic <- chartDF$c_CardioDiastolic - newCardio1Offset
#     chartDF$c_CardioSystolic <- chartDF$c_CardioSystolic - newCardio1Offset
#     yOffset[4] <- yOffset[4] - newCardio1Offset
#   }
# 
# } # end if to reoffset stable cardio if necessary






# recenter the cardio data if it goes off the top or the bottom of the plot

# # start by setting the row number to recenter at the first
# reCenter <- 1
# # make a vector of reCentering events
# reCenterEvents <- NULL
# reCenterEventsDn <- NULL
# reCenterEventsUp <- NULL

# # recenter both high and low values
# while(length(which(chartDF$c_Cardio1[firstEvent:lastEventEnd] >= 165 | chartDF$c_Cardio1[firstEvent:lastEventEnd] <= -165)) > 0) {
#   # make a vector of indices that are at the top or bottom of the graphic
#   reCenter <- min(which(chartDF$c_Cardio1 >= 165 | chartDF$c_Cardio1 <= -165))
#   # make a vector of offset values for indices that are at the top or bottom of the graphic
#   centerVal <- chartDF$c_Cardio1[min(which(chartDF$c_Cardio1 >= 165 | chartDF$c_Cardio1 <= -165))] - yOffset[4]
#   # # add the reCenter event to the Up or Dn vector
#   # if(centerVal > 0) {
#   #   reCenterEventsDn <- c(reCenterEventsDn, reCenter)
#   # } else reCenterEventsUp <- c(reCenterEventsUp, reCenter)
#   # recenter from the reCenter index to the end of chart
#   chartDF$c_Cardio1[reCenter:length(chartDF$c_Cardio1)] <- chartDF$c_Cardio1[reCenter:length(chartDF$c_Cardio1)] - centerVal
#   chartDF$c_CardioMA[reCenter:length(chartDF$c_CardioMA)] <- chartDF$c_CardioMA[reCenter:length(chartDF$c_CardioMA)] - centerVal
#   chartDF$c_CardioMid[reCenter:length(chartDF$c_CardioMid)] <- chartDF$c_CardioMid[reCenter:length(chartDF$c_CardioMid)] - centerVal
#   chartDF$c_CardioSystolic[reCenter:length(chartDF$c_CardioSystolic)] <- chartDF$c_CardioSystolic[reCenter:length(chartDF$c_CardioSystolic)] - centerVal
#   chartDF$c_CardioDiastolic[reCenter:length(chartDF$c_CardioDiastolic)] <- chartDF$c_CardioDiastolic[reCenter:length(chartDF$c_CardioDiastolic)] - centerVal
#   reCenterEvents <- c(reCenterEvents, reCenter)
# # } # end while loop



# # cardioReCenterUp <- function(x=chartDF$c_Cardio1) {
#   # function to recenter the cardio data if it goes off the bottom of the plot
#   while(length(which(x <= -165))>0) {
#     # get the last stimulus event before the data go off the bottom of the plot
#     reCenter <- max(eventRows[(eventRows <= min(which(x <= -165)))])
#     # relocate the index to recenter
#     reCenter <- reCenter - 120
#     if(reCenter<1) reCenter <- 1
#     # get the offset value
#     # centerVal <- x[min(which(x <= -165))] - yOffset[4]
#     centerVal <- x[reCenter] - yOffset[4]
#     # recenter from the recenter index to the end of chart
#     x[reCenter:length(x)] <- x[reCenter:length(x)] - centerVal
#     # add the reCenter event to the reCenterEventsUp vector
#     reCenterEventsUp <- c(reCenterEventsUp, reCenter)
#     # # chec for values off the top of the chart
#     # if(length(which(x >= 165))>0) {
#     #   cardioReCenterDn(x=x)
#     # } # end if
#   }
#   return(x)
# } # end cardioReCenterUp() function

# chartDF$c_Cardio1 <- cardioReCenterUp(x=chartDF$c_Cardio1)

# # cardioReCenterDn <- function(x=chartDF$c_Cardio1) {
#   # function to recenter the cardio data if it goes off the top of the plot
#   while(length(which(x >= 165))>0) {
#     # get the last stimulus event before the data go off the bottom of the plot
#     reCenter <- max(eventRows[(eventRows <= min(which(x >= 165)))])
#     # relocate the index to recenter
#     reCenter <- reCenter - 120
#     if(reCenter<1) reCenter <- 1
#     # get the offset value
#     # centerVal <- x[min(which(x >= 165))] - yOffset[4]
#     centerVal <- x[reCenter] - yOffset[4]
#     # recenter from the recenter index to the end of chart
#     x[reCenter:length(x)] <- x[reCenter:length(x)] - centerVal
#     # add the reCenter event to the reCenterEventsUp vector
#     reCenterEventsDn <- c(reCenterEventsDn, reCenter)

#     # # chec for values off the bottom of the chart
#     # if(length(which(x <= -165))>0) {
#     #   cardioReCenterUp(x=x)
#     # } # end if

#     # check to see if the data still go off the top
#     while(length(which(x >= 165))>0) {
#       while( (max(eventRows[(eventRows <= min(which(x >= 165)))])-120) == reCenter ) {
#         # get the recenter2 index
#         reCenter2 <- min(which(x >= 165))
#         # get the recenter2 offset value
#         centerVal2 <- x[reCenter2] - yOffset[4]
#         # recenter the data again
#         x[reCenter2:nrow(chartDF)] <- x[reCenter2:nrow(chartDF)] - centerVal2
#         reCenterEventsDn = c(reCenterEventsDn, reCenter2)
#       }
#     }
# 
#   }
#   return(x)
# } # end cardioReCenterDn() function

# chartDF$c_Cardio1 <- cardioReCenterDn(x=chartDF$c_Cardio1)



# while(length(which(chartDF$c_Cardio1 >= 165))>0) {
#   # get the data row for the last stimulus onset prior to the data going off the chart
#   reCenter <- max(eventRows[(eventRows <= min(which(chartDF$c_Cardio1 >= 165)))])
#   # calculate the centering value to put the stim onset at the baseline
#   centerVal <- chartDF$c_Cardio1[min(which(chartDF$c_Cardio1 >= 165))] - 0
#   # offset the centering row to 4 seconds before stimulus onset
#   reCenter <- reCenter - 120
#   # fix centering prior to the first row
#   if(reCenter<1) reCenter <- 1
#   # and center the data from the centering row to end of chart
#   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] - centerVal
#   # add the recenter event to the reCenterEvents vector for plotting
#   reCenterEventsDn = c(reCenterEventsDn, reCenter)
# }

# while(length(which(chartDF$c_Cardio1 <= -175))>0) {
#   reCenter <- max(eventRows[(eventRows <= min(which(chartDF$c_Cardio1 < -175)))])
#   centerVal <- -45 - chartDF$c_Cardio1[reCenter]
#   reCenter <- reCenter - 120
#   if(reCenter<1) reCenter <- 1
#   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
#   reCenterEventsUp = c(reCenterEventsUp, reCenter)
# }



# another while loop to recenter when data go off the bottom of chart
# while(length(which(chartDF$c_Cardio1 <= -165))>0) {
#   reCenter <- min(which(chartDF$c_Cardio1 < -165))
#   centerVal <- 0 - chartDF$c_Cardio1[reCenter]
#   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
#   reCenterEventsUp = c(reCenterEventsUp, reCenter)
# }

# and another while loop to recenter when data go off the top
# while(length(which(chartDF$c_Cardio1 >= 165))>0) {
#   reCenter <- min(which(chartDF$c_Cardio1 > 165))
#   centerVal <- 0 - chartDF$c_Cardio1[reCenter]
#   chartDF$c_Cardio1[reCenter:nrow(chartDF)] <- chartDF$c_Cardio1[reCenter:nrow(chartDF)] + centerVal
#   reCenterEventsDn = c(reCenterEventsDn, reCenter)
# }

#### end of auto recenter cardio 




