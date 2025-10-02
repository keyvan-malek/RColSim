# Mcnary_Dam.R
# Reservoir operations functions for Mcnary
# Handles inflow/outflow calculations and reservoir management operations

#--------------------- MCNARY DAM --------------------#
#######################################################

McNaryFlowData <- function() {
  flow_o <- FlowMCN #+ RetMCN
  return(flow_o)
}
MCNInc <- function() {
  MCNInc_o <- McNaryFlowData() - IceHarborFlowData() - PriestRapidsFlowData()
  return(MCNInc_o)
}
MCNetHead <- function() {
  MCNetHead_o <- 74 # https://www.nww.usace.army.mil/Missions/Water-Management/
  return(MCNetHead_o)
}
MCNPenLimit <- function() {
  MCNPenCap <- 232000  # https://www.cbr.washington.edu/hydro/mcnary
  MCNPenLimit_o <- MCNPenCap * cfsTOafw
  return(MCNPenLimit_o)
}
MCNIn <- function() {
  MCNIn_o <- IHOut() + PROut() + MCNInc()
  return(MCNIn_o)
}
MCNCurtail <- function() {
  MCNCurtail_0 <- min(DemMCN, max(max(IflowMCN - MCNIn(), 0)))
  if (curtail_option == 1) {
    if (MCNCurtail_0 > 0) {
      MCNCurtail_o <- CurtMCN
    } else {
      MCNCurtail_o <- 0
    }
  } else if (curtail_option == 2) {
    MCNCurtail_o <- MCNCurtail_0
  } else if (curtail_option == 3) {
    MCNCurtail_o <- 0
  }
  if (DARunoffAprSep > mainstem_rule) {
    MCNCurtail_o <- 0
  } else {
    MCNCurtail_o <- MCNCurtail_o
  }
  return(MCNCurtail_o)
}
MCNInstreamShortfall <- function() {
  MCNInstreamShortfall_o <- max(max(IflowMCN - MCNIn(), 0))
  return(MCNInstreamShortfall_o)
}
MCNPrelim <- function() {
  MCNPrelim_o <- IHPrelim() + PRPrelim() + MCNInc()
  return(MCNPrelim_o)
}
MCNPreEnergy <- function() {
  MCNPreEnergy_o <- MWhr_per_ftAcFt * min(MCNPrelim(), MCNPenLimit()) * MCNetHead() * MCNCombEfficiency
  return(MCNPreEnergy_o)
}
MCNOut <- function() {
  MCNOut_o <- MCNIn()
  return(MCNOut_o)
}

#######################################################
