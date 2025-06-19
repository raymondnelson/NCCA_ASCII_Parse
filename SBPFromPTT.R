# R function to compute Systolic Blood Pressure from PTT
# June 2024
# Raymond Nelson
# using the formula from
# Jingyu Choi, Younghwan Kang, Jaesoon Park, Yeunho Joung and Chiwan Koo (2023)
# Development of Real-Time Cuffless Blood Pressure Measurement Systems 
# with ECG Electrodes and a Microphone Using Pulse Transit Time (PTT)
####

SBP_from_PTT_Fn <- function(PTT, B=40, p=1035 , height=1600) {
  # R function to compute Systolic Blood Pressure from PTT
  # June 2024
  # Raymond Nelson
  # using the formula from
  # Jingyu Choi, Younghwan Kang, Jaesoon Park, Yeunho Joung and Chiwan Koo (2023)
  # Development of Real-Time Cuffless Blood Pressure Measurement Systems 
  # with ECG Electrodes and a Microphone Using Pulse Transit Time (PTT)
  ####
  # inputs
  # PTT = the pulse transit time, can be a vector of time series values 
  # B = is an offset
  # p = average blood density ρ is 1035 kg/m3
  # height is in mm
  ####
  # output is the SBP
  ####
  A <- (.48 * height)^2 * (p / 1.4)
  SBP <- A / PTT^2
  SBP <- SBP - SBP[1]
  return(SBP + B)
}

# segmentDF$c_PTTPTT_abs <- SBP_from_PTT_Fn(PTT=segmentDF$c_PTTPTT)
# 
# # segmentDF$c_PTTPTT_abs <- segmentDF$c_PTTPTT_abs - segmentDF$c_PTTPTT_abs[1]
# 
# plot.ts(segmentDF$c_PTTPTT_abs)
# plot.ts(segmentDF$c_PTTPTT)

