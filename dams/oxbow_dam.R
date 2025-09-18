#--------------------- OXBOW DAM ---------------------#
#######################################################

OxbowFlowData <- function() {
  return(max(FlowOX))
}
OXNetHead <- function() {
  OXNetHead_o <- 117 # FERC No. 1971 Licese Application
  return(OXNetHead_o)
}
OXPenLimit <- function() {
  OXPenCap <- 25000 # https://www.cbr.washington.edu/hydro/oxbow
  OXPenLimit_o <- OXPenCap * cfsTOafw
  return(OXPenLimit_o)
}
OXIn <- function() {
  OXIn_o <- BROutflow()
  return(OXIn_o)
}
OXPrelim <- function() {
  OXPrelim_o <- BRPrelim_c
  return(OXPrelim_o)
}
OXPreEnergy <- function() {
  OXPreEnergy_o <- MWhr_per_ftAcFt * min(OXPrelim(), OXPenLimit()) * OXNetHead() * OXCombEfficiency
  return(OXPreEnergy_o)
}
OXOut <- function() {
  OXOut_o <- OXIn()
  return(OXOut_o)
}

#######################################################
