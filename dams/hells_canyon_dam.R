#----------------- HELLS CANYON DAM ------------------#
#######################################################

HellsCanyonFlowData <- function() {
  return(max(FlowHC))
}
HCInc <- function() {
  HCInc_o <- HellsCanyonFlowData() - OxbowFlowData()
  return(HCInc_o)
}
HCPenLimit <- function() {
  HCPenCap <- 9000 # FERC No. 1971 Licese Application
  HCPenLimit_o <- HCPenCap * cfsTOafw
  return(HCPenLimit_o)
}
HCNetHead <- function() {
  HCNetHead_o <- 210 # FERC No. 1971 Licese Application
  return(HCNetHead_o)
}
HCPrelim <- function() {
  HCPrelim_o <- OXPrelim() + HCInc()
  return(HCPrelim_o)
}
HCPreEnergy <- function() {
  HCPreEnergy_o <- MWhr_per_ftAcFt * min(HCPrelim(), HCPenLimit()) * HCNetHead() * HCCombEfficiency
  return(HCPreEnergy_o)
}
HCIn <- function() {
  HCIn_o <- OXOut() + HCInc()
  return(HCIn_o)
}
HCOut <- function() {
  HCOut_o <- HCIn()
  return(HCOut_o)
}

#######################################################
