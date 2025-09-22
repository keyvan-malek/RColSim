# Lower_Monumental_Dam.R
# Reservoir operations functions for Lower Monumental
# Handles inflow/outflow calculations and reservoir management operations

#---------------- LOWER MONUMENTAL DAM ---------------#
#######################################################

LowerMonuFlowData <- function() {
  return(max(FlowLM))
}
LMInc <- function() {
  LMInc_o <- LowerMonuFlowData() - LittleGooseFlowData()
  return(LMInc_o)
}
LMNetHead <- function() {
  LMNetHead_o <- 100 # https://www.nww.usace.army.mil/Missions/Water-Management/
  return(LMNetHead_o)
}
LMPenLimit <- function() {
  LMPenCap <- 130000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Lower-Monumental/#top
  LMPenLimit_o <- LMPenCap * cfsTOafw
  return(LMPenLimit_o)
}
LMIn <- function() {
  LMIn_o <- LIGOut() + LMInc()
  return(LMIn_o)
}
LMPrelim <- function() {
  LMPrelim_o <- LIGPrelim() + LMInc()
  return(LMPrelim_o)
}
LMPreEnergy <- function() {
  LMPreEnergy_o <- MWhr_per_ftAcFt * min(LMPrelim(), LMPenLimit()) * LMNetHead() * LMCombEfficiency
  return(LMPreEnergy_o)
}
LMOut <- function() {
  LMOut_o <- LMIn()
  return(LMOut_o)
}

#######################################################
