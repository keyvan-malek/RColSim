#--------------- ROCK ISLAND DAM ---------------------#
#######################################################

RockIslandFlowData <- function() {
  return(max(FlowRI))
}
RIInc <- function() {
  RIInc_o <- RockIslandFlowData() - RockyReachFlowData()
  return(RIInc_o)
}
RICurtail <- function() {
  RICurtail_0 <- min(DemRI, max(max(IflowRI - RROut() - RIInc(), 0)))
  if (curtail_option==1) {
    if (RICurtail_0 > 0) {
      RICurtail_o <- CurtRI
    } else {
      RICurtail_o <- 0
    }
  } else if (curtail_option==2) {
    RICurtail_o <- RICurtail_0
  } else if (curtail_option==3) {
    RICurtail_o <- 0
  }
  if (DARunoffAprSep > mainstem_rule) {
    RICurtail_o <- 0
  } else {
    RICurtail_o <- RICurtail_o
  }
  return(RICurtail_o)
}
RIInstreamShortfall <- function() {
  RIInstreamShortfall_o = max(max(IflowRI - RROut() - RIInc(), 0))
  return(RIInstreamShortfall_o)
}
RIPenLimit <- function() {
  RIPenCap <- 217000 #email contact Lindsey Files from Chelan PUD (8/26/2022), changed from 220000 on 5/25/23
  RIPenLimit_o <- RIPenCap * cfsTOafw
  return(RIPenLimit_o)
}
RINetHead <- function() {
  RINetHead_o <- 41.5 # Varies from 29.3 to 50.4 ft. Email contact Lindsey Files from Chelan PUD (8/26/2022). Changed from 34.4 5/25/23
  return(RINetHead_o)
}
RIPrelim <- function() {
  RIPrelim_o <- RRPrelim() + RIInc()
  return(RIPrelim_o)
}
RIPreEnergy <- function() {
  RIPreEnergy_o <- MWhr_per_ftAcFt * min(RIPrelim(), RIPenLimit()) * RINetHead() * RICombEfficiency
  return(RIPreEnergy_o)
}
RIIn <- function() {
  RIIn_o <- RROut() + RIInc()
  return(RIIn_o)
}
RIOut <- function() {
  RIOut_o <- RIIn()
  return(RIOut_o)
}

#######################################################
