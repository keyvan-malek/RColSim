# Little_Goose_Dam.R
# Reservoir operations functions for Little Goose
# Handles inflow/outflow calculations and reservoir management operations

#----------------- LITTLE GOOSE DAM ------------------#
#######################################################

LittleGooseFlowData <- function() {
  return(max(FlowLIG))
}
LIGInc <- function() {
  LIGInc_o <- LittleGooseFlowData() - LowerGraniteFlowData()
  return(LIGInc_o)
}
LIGNetHead <- function() {
  LIGNetHead_o <- 98 # https://www.nww.usace.army.mil/Missions/Water-Management/
  return(LIGNetHead_o)
}
LIGPenLimit <- function() {
  LIGPenCap <- 130000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Little-Goose/#top
  LIGPenLimit_o <- LIGPenCap * cfsTOafw
  return(LIGPenLimit_o)
}
LIGIn <- function() {
  LIGIn_o <- LGOut() + LIGInc()
  return(LIGIn_o)
}
LIGPrelim <- function() {
  LIGPrelim_o <- LGPrelim() + LIGInc()
  return(LIGPrelim_o)
}
LIGPreEnergy <- function() {
  LIGPreEnergy_o <- MWhr_per_ftAcFt * min(LIGPrelim(), LIGPenLimit()) * LIGNetHead() * LIGCombEfficiency
  return(LIGPreEnergy_o)
}
LIGOut <- function() {
  LIGOut_o <- LIGIn()
  return(LIGOut_o)
}
LGOIn <- function() {
  LGOIn_o <- LGOut() + LIGInc()
  return(LGOIn_o)
}
LGOOut <- function() {
  LGOOut_o <- LGOIn()
  return(LGOOut_o)
}

#######################################################
