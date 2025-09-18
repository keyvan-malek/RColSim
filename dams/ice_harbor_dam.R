#------------------- ICE HARBOR DAM ------------------#
#######################################################

IceHarborFlowData <- function() {
  return(max(FlowIH))
}
IHInc <- function() {
  IHInc_o <- IceHarborFlowData() - LowerMonuFlowData()
  return(IHInc_o)
}
IHNetHead <- function() {
  IHNetHead_o <- 98 # https://www.nww.usace.army.mil/Missions/Water-Management/
  return(IHNetHead_o)
}
IHPenLimit <- function() {
  IHPenCap <- 106000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Ice-Harbor/#top
  IHPenLimit_o <- IHPenCap * cfsTOafw
  return(IHPenLimit_o)
}
IHPrelim <- function() {
  IHPrelim_o <- LMPrelim() + IHInc()
  return(IHPrelim_o)
}
IHPreEnergy <- function() {
  IHPreEnergy_o <- MWhr_per_ftAcFt * min(IHPrelim(), IHPenLimit()) * IHNetHead() * IHCombEfficiency
  return(IHPreEnergy_o)
}
IHIn <- function() {
  IHIn_o <- LMOut() + IHInc()
  return(IHIn_o)
}
IHOut <- function() {
  IHOut_o <- IHIn()
  return(IHOut_o)
}

#######################################################
