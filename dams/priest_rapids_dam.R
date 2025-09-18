#-------------- PRIEST RAPIDS DAM --------------------#
#######################################################

PriestRapidsFlowData <- function() {
  flow_o <- FlowPR #+ RetPR
  return(max(FlowPR))
}
PRInc <- function() {
  PRInc_o <- PriestRapidsFlowData() - WanapumFlowData()
  return(PRInc_o)
}
PRCurtail <- function() {
  PRCurtail_0 <- min(DemPR, max(IflowPR - PRIn(), 0))

  if (curtail_option == 1) {
    PRCurtail_o <- ifelse(PRCurtail_0 > 0, CurtPR, 0)
  } else if (curtail_option == 2) {
    PRCurtail_o <- PRCurtail_0
  } else if (curtail_option == 1) {
    PRCurtail_o <- 0
  }

  if (DARunoffAprSep > mainstem_rule) {
    PRCurtail_o <- 0
  } else {
    PRCurtail_o <- PRCurtail_o
  }

  return(PRCurtail_o)
}
PRInstreamShortfall <- function() {
  PRInstreamShortfall_o = max(IflowPR - PRIn(), 0)
  return(PRInstreamShortfall_o)
}
PRPenLimit <- function() {
  PRPenCap <- 178000 # https://www.ezview.wa.gov/Portals/_1962/images/FERC%20401s/priest_rapids-final_cert040307.pdf
  PRPenLimit_o <- PRPenCap * cfsTOafw
  return(PRPenLimit_o)
}
PRPrelim <- function() {
  PRPrelim_o <- WAPrelim() + PRInc()
  return(PRPrelim_o)
}
PRNetHead <- function() {
  PRNetHead_o <- 76.5 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=priest%20rapids
  return(PRNetHead_o)
}
PRPreEnergy <- function() {
  PRPreEnergy_o <- MWhr_per_ftAcFt * min(PRPrelim(), PRPenLimit()) * PRNetHead() * PRCombEfficiency
  return(PRPreEnergy_o)
}
PRIn <- function() {
  PRIn_o <- WAOut() + PRInc()
  return(PRIn_o)
}
PROut <- function() {
  PROut_o <- PRIn()
  return(PROut_o)
}

############ UPPER SNAKE SYSTEM ##########################

#####################################################
