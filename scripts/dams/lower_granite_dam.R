# Lower_Granite_Dam.R
# Reservoir operations functions for Lower Granite
# Handles inflow/outflow calculations and reservoir management operations

#---------------- LOWER GRANITE DAM ------------------#
#######################################################

LowerGraniteFlowData <- function() {
  return(max(FlowLG))
}
LGInc <- function() {
  LGInc_o <- LowerGraniteFlowData() - HellsCanyonFlowData() - DworshakFlowData()
  return(LGInc_o)
}
LGNetHead <- function() {
  LGNetHead_o <- 100 #https://www.nww.usace.army.mil/Missions/Water-Management/
  return(LGNetHead_o)
}
LGPenLimit <- function() {
  LGPenCap <- 130000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Lower-Granite/#top
  LGPenLimit_o <- LGPenCap * cfsTOafw
  return(LGPenLimit_o)
}
LGIn <- function() {
  LGIn_o <- DWOutflow() + HCOut() + LGInc()
  return(LGIn_o)
}
LGPrelim <- function() {
  LGPrelim_o <- HCPrelim() + DWPrelim() + LGInc()
  return(LGPrelim_o)
}
LGPreEnergy <- function() {
  LGPreEnergy_o <- MWhr_per_ftAcFt * min(LGPrelim(), LGPenLimit()) * LGNetHead() * LGCombEfficiency
  return(LGPreEnergy_o)
}
LGOut <- function() {
  LGOut_o <- LGIn()
  return(LGOut_o)
}

#######################################################
