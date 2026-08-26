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



# [1] "#### Sun Aug 23 19:29:39 2026 ####"

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



# [1] "#### Sun Aug 23 07:51:36 2026 ####"
# 
# resuming work after changing computer
# 2018 macbook pro (intel) replaced with 2021 macbook pro M1
# AWOW desktop i7 16GB Win11 replaced with AMD Ryzen 7 32GB
#
# working with the Ohio n=40 laboratory sample
# setwd("~/Dropbox/DATASETS/datasets2026/UTAH3_N40_Ohio/2026/NCCAASCII_LAF_bslnPPG/NCCAASCII_LAF_smPneumos/NCCAASCII_LAF_bslnEDA/NCCAASCII_LAF_qLen")
# 




