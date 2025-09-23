# Box_Canyon_Dam.R
# Reservoir operations functions for Box Canyon
# Handles inflow/outflow calculations and reservoir management operations

#----------------- BOX CANYON DAM --------------------#
#######################################################

BoxCanyonFlowData <- function() {
  return(max(FlowBC))
}
BCInc <- function() {
  BCInc_o <- BoxCanyonFlowData() - AlbeniFallsFlowData()
  return(BCInc_o)
}
BCPenLimit <- function() {
  BoxCanyonPenCap <- 27400 # FERC License no. 2042-013
  BCPenLimit_o <- BoxCanyonPenCap * cfsTOafw
  return(BCPenLimit_o)
}
BCNetHead <- function() {
  BCNetHead_o <- 46 # https://popud.org/projects/box-canyon-dam/
  return(BCNetHead_o)
}
BCIn <- function() {
  BCIn_o <- AFRelease_c + BCInc()
  return(BCIn_o)
}
BCPrelim <- function() {
  BCPrelim_o <- AFPrelim() + BCInc()
  return(BCPrelim_o)
}
BCPreEnergy <- function() {
  BCPreEnergy_o <- min(BCPrelim(), BCPenLimit()) * BCNetHead() * BCCombEfficiency * MWhr_per_ftAcFt
  return(BCPreEnergy_o)
}
BCOut <- function(){
  BCOut_o <- BCIn()
  return(BCOut_o)
}

#######################################################
