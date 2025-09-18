#----------------- BOUNDARY DAM ----------------------#
#######################################################

BoundaryFlowData <- function() {
  return(max(FlowBD))
}
BDInc <- function() {
  BDInc_o <- BoundaryFlowData() - BoxCanyonFlowData()
  return(BDInc_o)
}
BDPenLimit <- function() {
  BDPenCap <- 55000 # https://www.seattle.gov/light/Boundary/Relicense/docs/Study_03_TDG_Final_Report_03_09.pdf. (FERC No. 2144)
  BDPenLimit_o <- BDPenCap * cfsTOafw
  return(BDPenLimit_o)
}
BDNetHead <- function() {
  BDNetHead_o <- 261 # FERC No. 2144
  return(BDNetHead_o)
}
BDIn <- function() {
  BDIn_o <- BCOut() + BDInc()
  return(BDIn_o)
}
BDPrelim <- function() {
  BDPrelim_o <- BCPrelim() + BDInc()
  return(BDPrelim_o)
}
BDPreEnergy <- function() {
  BDPreEnergy_o <- MWhr_per_ftAcFt * min(BDPrelim(), BDPenLimit()) * BDNetHead() * BDCombEfficiency
  return(BDPreEnergy_o)
}
BDOut <- function() {
  BDOut_o <- BDIn()
  return(BDOut_o)
}

#######################################################
