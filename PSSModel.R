# MacLaren and Krapohl 2003 Permutation Scoring System Reference Model
# 

# import the Permutation Scoring Sytem ratio tables
PSS_RLL_Ratios <- read.csv(paste0(RPath, "PSS_RLL_Ratios.csv"), stringsAsFactors=FALSE)
PSS_EDA_Ratios <- read.csv(paste0(RPath, "PSS_EDA_Ratios.csv"), stringsAsFactors=FALSE)
PSS_Cardio_Ratios <- read.csv(paste0(RPath, "PSS_Cardio_Ratios.csv"), stringsAsFactors=FALSE)

# import the permutation Table 3 from MacLaren and Krapohl (2003)

PSS_Table3 <- read.csv(paste0(RPath, "PSS_Table3.csv"), stringsAsFactors=FALSE)

# a simulation of Table 3
PSS_Model_7x27 <- read.csv(paste0(RPath, "PSSModel7x27.csv"), stringsAsFactors=FALSE)

# PSS_Model_7x27 <- PSS_Model_7x27[PSS_Model_7x27$pmf >= .001,]



