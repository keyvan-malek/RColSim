# Cabinet_Gorge_Dam.R
# Reservoir operations functions for Cabinet Gorge
# Handles inflow/outflow calculations and reservoir management operations

#-------------- CABINET GORGE DAM --------------------#
#######################################################

CabinetFlowData <- function() {
  return(max(FlowCB))
}
CBInc <- function() {
  CBInc_o <- CabinetFlowData() - NoxonFlowData()
  return(CBInc_o)
}
CBNetHead <- function() {
  CBNetHead_o <- 97.2  # forebay range: 2168 - 2175 ft. Average tailwater elevation: 2073.5 ft. Meeting with Avista (Klint Kalich, 9/19/2022)
  return(CBNetHead_o)
}
CBPenLimit <- function() {
  CBPenCap <- 39000 # Meeting with Avista (Klint Kalich, 9/19/2022), changed 5/25/2023 from 35700
  CBPenLimit_o <- CBPenCap * cfsTOafw
  return(CBPenLimit_o)
}
CBIn <- function() {
  CBIn_o <- NOXOut() + CBInc()
  return(CBIn_o)
}
CBPrelim <- function() {
  CBPrelim_o <- NOXPrelim() + CBInc()
  return(CBPrelim_o)
}
CBPreEnergy <- function() {
  CBPreEnergy_o <- MWhr_per_ftAcFt * min(CBPrelim(), CBPenLimit()) * CBNetHead() * CBCombEfficiency
  return(CBPreEnergy_o)
}
CBOut <- function() {
  CBOut_o <- CBIn()
  return(CBOut_o)
}

#######################################################
