#--------------- THOMPSON FALLS DAM ------------------#
#######################################################

ThompsonFlowData <- function() {
  return(max(FlowTF))
}
TFInc <- function() {
  TFInc_o <- ThompsonFlowData() - KerrFlowData()
  return(TFInc_o)
}
TFNetHead <- function() {
  TFNetHead_o <- 55 # FERC No. 1869 (docket P-1869)
  return(TFNetHead_o)
}
TFPenLimit <- function() {
  TFPenCap <- 23252 # FERC License no. 2042-013
  TFPenLimit_o <- TFPenCap * cfsTOafw
  return(TFPenLimit_o)
}
TFIn <- function() {
  TFIn_o <- KERelease_c + TFInc()
  return(TFIn_o)
}
TFPrelim <- function() {
  TFPrelim_o <- KEPrelim() + TFInc()
  return(TFPrelim_o)
}
TFPreEnergy <- function() {
  TFPreEnergy_o <- min(TFPrelim(), TFPenLimit()) * TFNetHead() * TFCombEfficiency * MWhr_per_ftAcFt
  return(TFPreEnergy_o)
}
TFOut <- function(){
  TFOut_o <- TFIn()
  return(TFOut_o)
}

#######################################################
