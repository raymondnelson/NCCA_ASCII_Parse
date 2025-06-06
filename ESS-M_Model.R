# ESS-Multinomial reference tables

   # set the directory location for the ESS-M reference tables 
    ESSMRefDir <- "~/Dropbox/R/NCCA_ASCII_Parse/"
    
    # import the ESS-M reference tables
    ESSM_TableI_2RQs <- read.csv(paste0(ESSMRefDir, "ESSM_TableI_2RQs.csv"), 
                                stringsAsFactors=FALSE)
    ESSM_TableJ_3RQs <- read.csv(paste0(ESSMRefDir, "ESSM_TableJ_3RQs.csv"), 
                                stringsAsFactors=FALSE)
    ESSM_TableK_4RQs <- read.csv(paste0(ESSMRefDir, "ESSM_TableK_4RQs.csv"), 
                                stringsAsFactors=FALSE)
    ESSM_TableL_Subtotalss <- read.csv(paste0(ESSMRefDir, 
                                             "ESSM_TableL_Subtotals.csv"), 
                                      stringsAsFactors=FALSE)
    