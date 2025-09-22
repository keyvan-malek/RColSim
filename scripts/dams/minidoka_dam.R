# Minidoka_Dam.R
# Reservoir operations functions for Minidoka
# Handles inflow/outflow calculations and reservoir management operations

#----------------- MINIDOKA DAM ----------------------#
#######################################################

MINFullPoolVol <- 95180
MINBotVol <- 0
MINAvgMin <- 300 # minimum average monthly flow at Neely, ID gauge near American Falls from historical data

InitMIN <- function() {
  InitMIN_o <- ResInitFractionFull * MINFullPoolVol
  return(InitMIN_o)
}
Minidoka <- function() {
  if (week_counter == 1) {
    Minidoka_o <- InitMIN()
  } else {
    Minidoka_o <- reservoir_vol_df$MINAD[week_counter - 1]
  }
  return(Minidoka_o)
}
MinidokaFlowData <- function() {
  return(max(FlowMIN))
}
MINInc <- function() {
  MINInc_o <- MinidokaFlowData() - AmericanFallsFlowData()
  return(MINInc_o)
}
MINIn <- function() {
  MINIn_o <- AMPrelim() + MINInc()
  return(MINIn_o)
}
MINInflow <- function() {
  MINInflow_o <- AMRelease() + MINInc()
  return(MINInflow_o)
}
MINAgReq <- function() {
  MINAgReq_o <- 0
  return(MINAgReq_o)
}
MINMinReq <- function() {
  MINMinReq_o <- max(MINAgReq(), MINAvgMin * cfsTOafw)
  return(MINMinReq_o)
}
MINAvailWater <- function() {
  MINAvailWater_o <- max(0, Minidoka() + MINIn() - MINBotVol)
  return(MINAvailWater_o)
}
MINPrelim <- function() {
  MINPrelim_o <- min(MINAvailWater(), MINMinReq())
  return(MINPrelim_o)
}
MINRelLimit <- function() {
  MINRelLimit_o <- max(max(Minidoka() + MINInflow() - MINBotVol, 0))
  return(MINRelLimit_o)
}
MINDamProtectRel <- function() {
  MINDamProtectRel_o <- max(0, Minidoka() + MINInflow() - MINFullPoolVol)
  return(MINDamProtectRel_o)
}
MINCombUpProtect <- function() {
  MINCombUpProtect_o <- AMDamProtectExcess() + AMCombUpProtect_c
  MINCombUpProtect_c <<- MINCombUpProtect_o
  return(MINCombUpProtect_o)
}
MINDamProtectExcess <- function() {
  MINDamProtectExcess_o <- max(0, MINDamProtectRel() - MINPrelim() - MINCombUpProtect_c)
  return(MINDamProtectExcess_o)
}
MINRelease <- function() {
  MINRelease_o <- max(MINDamProtectRel(), min(MINPrelim() + MINCombUpProtect(), MINRelLimit()))
  return(MINRelease_o)
}
MINOutflow <- function() {
  MINOutflow_o <- MINRelease_c
  return(MINOutflow_o)
}
MINOut <- function() {
  MINOut_o <- MINIn()
  return(MINOut_o)
}

#######################################################
