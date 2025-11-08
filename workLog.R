# work log for NCCA ASCII parse 
# Raymond Nelson
#
####



todayStampFn <- function() {
  b <- "####"
  paste(b, date(), b)
}



todayStampFn()


# [1] "#### Sat Nov  1 17:33:44 2025 ####"
# modified NCCAASCII_dataParse function
# changed IQR * 9 to IQR * 18 to avoid inducing EDA artifacts



# [1] "Sun Oct 26 17:50:31 2025"

# NCCAASCII_Output now uses un-smoothed data cols to avoid over-smoothing 
# 
# cleanup to the maxSlopeChange function



#### Sat Oct 25 12:42:49 2025 ####

# changed the ROW graphic to a red lihe
# 
# changed to MRL graphic to a red line
# 
# fixed a problem with the NCCA_dataParse function





# November 19, 2022

# changes to support LXCAT exams which do not have respiration sensors 
# or respiration scores.
# Need to prevent the calculation and display of P1 and P2 scores



