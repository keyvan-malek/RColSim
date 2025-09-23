# Rocky_Reach_Dam.R
# Reservoir operations functions for Rocky Reach
# Handles inflow/outflow calculations and reservoir management operations

#--------------- ROCKY REACH DAM ---------------------#
#######################################################

RockyReachFlowData <- function() {
  return(max(FlowRR))
}
RRInc <- function() {
  RRInc_o <- RockyReachFlowData() - WellsFlowData() - ChelanFlowData()
  return(RRInc_o)
}
RRCurtail <- function() {
  RRCurtail_0 <- min(DemRR, max(max(IflowRR - WEOut() - CHOutflow() - RRInc(), 0)))
  if (curtail_option==1) {
    if (RRCurtail_0 > 0) {
      RRCurtail_o <- CurtRR
    } else {
      RRCurtail_o <- 0
    }
  } else if (curtail_option == 2) {
    RRCurtail_o <- RRCurtail_0
  } else if (curtail_option == 3) {
    RRCurtail_o <- 0
  }
  if (DARunoffAprSep > mainstem_rule) {
    RRCurtail_o <- 0
  } else {
    RRCurtail_o <- RRCurtail_o
  }
  return(RRCurtail_o)
}
RRInstreamShortfall <- function() {
  RRInstreamShortfall_o = max(max(IflowRR - WEOut() - RRInc(), 0))
  return(RRInstreamShortfall_o)
}
RRPenLimit <- function() {
  RRPenCap <- 201000 # email contact Lindsey Files from Chelan PUD (8/26/2022), changed from 220000 on 5/25/23
  RRPenLimit_o <- RRPenCap * cfsTOafw
  return(RRPenLimit_o)
}
RRPrelim <- function() {
  RRPrelim_o <- WEPrelim() + CHPrelim() + RRInc()
  return(RRPrelim_o)
}
RRNetHead <- function() {
  RRNetHead_o <- 88.6 ## Varies from 96.8 to 73.4 ft, email contact Lindsey Files from Chelan PUD (8/26/2022), changed 5/25/23 from 86.5
  return(RRNetHead_o)
}
RRPreEnergy <- function() {
  RRPreEnergy_o <- MWhr_per_ftAcFt * min(RRPrelim(), RRPenLimit()) * RRNetHead() * RRCombEfficiency
  return(RRPreEnergy_o)
}
RRIn <- function() {
  RRIn_o <- WEOut() + CHRelease() + RRInc()
  return(RRIn_o)
}
RROut <- function() {
  RROut_o <- RRIn()
  return(RROut_o)
}

#######################################################
