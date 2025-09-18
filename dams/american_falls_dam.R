#---------------- AMERICAN FALLS DAM -----------------#
#######################################################

AMFullPoolVol <- 1672590
AMBotVol <- 0
AMAvgMin <- 300 # minimum monthly average gauge flow, cfs

InitAM <- function() {
  InitAM_o <- ResInitFractionFull * AMFullPoolVol
  return(InitAM_o)
}
AmericanFalls <- function() {
  if (week_counter == 1) {
    AmericanFalls_o <- InitAM()
  } else {
    AmericanFalls_o <- reservoir_vol_df$AMERI[week_counter - 1]
  }
  return(AmericanFalls_o)
}
AmericanFallsFlowData <- function() {
  return(max(FlowAM))
}
AMInc <- function() {
  AMInc_o <- AmericanFallsFlowData() - IslandParkFlowData() - PalisadesFlowData() - RirieFlowData()
  return(AMInc_o)
}
AMIn <- function() {
  AMIn_o <- IPPrelim() + PALPrelim() + RIRPrelim() + AMInc()
  return(AMIn_o)
}
AMInflow <- function() {
  AMInflow_o <- IPRelease() + PALRelease() + RIRRelease() + AMInc()
  return(AMInflow_o)
}
MinidokaAgReq <- function() { ## Total withdrawals for Minidoka Irrigation Project
ConveyanceLossMID <- 1
IrrigationDemand <- DemMinidoka
MinidokaAgReq_o <- IrrigationDemand * ConveyanceLossMID
return(MinidokaAgReq_o)
}
AMAgReq <- function() {
  if (week_in_year %in% c(45:52, 1:14)) {
    AMAgReq_o <- MinidokaAgReq() + AMIn()
  } else {
    AMAgReq_o <- 0
  }
  return(AMAgReq_o)
}
AMMinReq <- function() {
  AMMinReq_o <- max(AMAgReq(), AMAvgMin * cfsTOafw)
  return(AMMinReq_o)
}
AMAvailWater <- function() {
  AMAvailWater_o <- max(0, AmericanFalls() + AMIn() - AMBotVol)
  return(AMAvailWater_o)
}
AMPrelim <- function() {
  AMPrelim_o <- min(AMAvailWater(), AMMinReq())
  return(AMPrelim_o)
}
AMRelLimit <- function() {
  AMRelLimit_o <- max(0, AmericanFalls() + AMInflow() - AMBotVol)
  return(AMRelLimit_o)
}
AMDamProtectRel <- function() {
  AMDamProtectRel_o <- max(0, AmericanFalls() + AMInflow() - AMFullPoolVol)
  return(AMDamProtectRel_o)
}
AMCombUpProtect <- function() {
  AMCombUpProtect_o <- IPDamProtectExcess() + PALDamProtectExcess() + RIRDamProtectExcess() + PALCombUpProtect_c
  AMCombUpProtect_c <<- AMCombUpProtect_o
  return(AMCombUpProtect_o)
}
AMDamProtectExcess <- function() {
  AMDamProtectExcess_o <- max(0, AMDamProtectRel() - AMPrelim() - AMCombUpProtect_c)
  return(AMDamProtectExcess_o)
}
AMRelease <- function() {
  AMRelease_o <- max(min(AMPrelim() + AMCombUpProtect(), AMRelLimit()), AMDamProtectRel())
  return(AMRelease_o)
}
AMOutflow <- function() {
  AMOutflow_o <- AMRelease_c
  return(AMOutflow_o)
}
AFOut <- function() {
  AFOut_o <- AFIn()
  return(AFOut_o)
}

#######################################################
