#------------------- WELLS DAM -----------------------#
#######################################################

WellsFlowData <- function() {
  return(max(FlowWE))
}
WEInc <- function() {
  WEInc_o <- WellsFlowData() - ChiefJosephFlowData()
  return(WEInc_o)
}
WECurtail <- function() {
  WECurtail_0 <- min(DemWE, max(max(IflowWE - CJOut() - WEInc(), 0)))
  if (curtail_option == 1) {
    if (WECurtail_0 > 0) {
      WECurtail_o <- CurtWE
    } else {
      WECurtail_o <- 0
    }
  } else if (curtail_option == 2) {
    WECurtail_o <- WECurtail_0
  } else if (curtail_option == 3) {
    WECurtail_o <- 0
  }
  if (DARunoffAprSep > mainstem_rule) {
    WECurtail_o <- 0
  } else {
    WECurtail_o <- WECurtail_o
  }
  return(WECurtail_o)
}
WEInstreamShortfall <- function() {
  WEInstreamShortfall_o = max(max(IflowWE - CJOut() - WEInc(), 0))
  return(WEInstreamShortfall_o)
}
WEPenLimit <- function() {
  WEPenCap <- 220000 # FERC Project No. 2149-152
  WEPenLimit_o <- WEPenCap * cfsTOafw
  return(WEPenLimit_o)
}
WEPrelim <- function() {
  WEPrelim_o <- CJPrelim() + WEInc()
  return(WEPrelim_o)
}
WENetHead <- function() {
  WENetHead_o <- 66.9 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=wells
  return(WENetHead_o)
}
WEPreEnergy <- function() {
  WEPreEnergy_o <- MWhr_per_ftAcFt * min(WEPrelim(), WEPenLimit()) * WENetHead() * WECombEfficiency
  return(WEPreEnergy_o)
}
WEIn <- function() {
  WEIn_o <- CJOut() + WEInc()
  return(WEIn_o)
}
WEOut <- function() {
  WEOut_o <- WEIn()
  return(WEOut_o)
}

#######################################################
