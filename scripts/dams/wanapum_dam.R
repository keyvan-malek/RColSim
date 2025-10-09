# Wanapum_Dam.R
# Reservoir operations functions for Wanapum
# Handles inflow/outflow calculations and reservoir management operations

#------------------ WANAPUM DAM ----------------------#
#######################################################

WanapumFlowData <- function() {
  flow_o <- FlowWA #+ RetWA
  return(max(FlowWA))
}
WAInc <- function() {
  WAInc_o <- WanapumFlowData() - RockIslandFlowData()
  return(WAInc_o)
}
WACurtail <- function() {
  WACurtail_0 <- min(DemWA, max(max(max(IflowWA - WAIn(), 0))))
  if (curtail_option == 1) {
    if (WACurtail_0 > 0) {
      WACurtail_o <- CurtWA
    } else {
      WACurtail_o <- 0
    }
  } else if (curtail_option == 2) {
    WACurtail_o <- WACurtail_0
  } else if (curtail_option == 3) {
    WACurtail_o <- 0
  }
  if (DARunoffAprSep > mainstem_rule) {
    WACurtail_o <- 0
  } else {
    WACurtail_o <- WACurtail_o
  }
  return(WACurtail_o)
}
WAInstreamShortfall <- function() {
  WAInstreamShortfall_o = max(max(max(IflowWA - WAIn(), 0)))
  return(WAInstreamShortfall_o)
}
WAPenLimit <- function() {
  WAPenCap <- 178000 # https://www.eesi.org/files/PUDpdf.pdf
  WAPenLimit_o <- WAPenCap * cfsTOafw
  return(WAPenLimit_o)
}
WAPrelim <- function() {
  WAPrelim_o <- RIPrelim() + WAInc()
  return(WAPrelim_o)
}
WANetHead <- function() {
  WANetHead_o <- 77.8 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=wanapum
  return(WANetHead_o)
}
WAPreEnergy <- function() {
  WAPreEnergy_o <- MWhr_per_ftAcFt * min(WAPrelim(), WAPenLimit()) * WANetHead() * WACombEfficiency
  return(WAPreEnergy_o)
}
WAIn <- function() {
  WAIn_o <- RIOut() + WAInc()
  return(WAIn_o)
}
WAOut <- function() {
  WAOut_o <- WAIn()
  return(WAOut_o)
}

#######################################################
