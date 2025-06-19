# R script to import the OSS-2 reference tables
# 12-8-2017
# Raymond Nelson



# import the OSS-2 reference tables
OSS2_reference_table <- read.csv(paste0(RPath, "OSS2_probability_reference_table.csv"), stringsAsFactors=FALSE)

# import the OSS-2 alpha lookup tables
OSS2_alfa_deception <- read.csv(paste0(RPath, "OSS2_alfa_lookup_deception.csv"), stringsAsFactors=FALSE)
OSS2_alfa_truthtelling <- read.csv(paste0(RPath, "OSS2_alfa_lookup_truthtelling.csv"), stringsAsFactors=FALSE)

# import the OSS-2 ratio tables
OSS2_RLL_Ratios <- read.csv(paste0(RPath, "OSS2_RLL_Ratios.csv"), stringsAsFactors=FALSE)
OSS2_EDA_Ratios <- read.csv(paste0(RPath, "OSS2_EDA_Ratios.csv"), stringsAsFactors=FALSE)
OSS2_Cardio_Ratios <- read.csv(paste0(RPath, "OSS2_Cardio_Ratios.csv"), stringsAsFactors=FALSE)


              
