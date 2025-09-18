#-------------- NOXON RAPIDS DAM ---------------------#
#######################################################

NoxonFlowData <- function() {
  return(max(FlowNOX))
}
NOXInc <- function() {
  NOXInc_o <- NoxonFlowData() - ThompsonFlowData()
  return(NOXInc_o)
}
NOXNetHead <- function() {
  NOXNetHead_o <- 152  # forebay range: 2321 - 2331 ft. Average tailwater elveation: 2177 ft. Meeting with Avista (Klint Kalich, 9/19/2022)
  return(NOXNetHead_o)
}
NOXPenLimit <- function() {
  NOXPenCap <- 51000 # Meeting with Avista (Klint Kalich, 9/19/2022), changed 5/25/2023 from 50000
  NOXPenLimit_o <- NOXPenCap * cfsTOafw
  return(NOXPenLimit_o)
}
NOXIn <- function() {
  NOXIn_o <- TFOut() + NOXInc()
  return(NOXIn_o)
}
NOXPrelim <- function() {
  NOXPrelim_o <- TFPrelim() + NOXInc()
  return(NOXPrelim_o)
}
NOXPreEnergy <- function() {
  NOXPreEnergy_o <-  min(NOXPrelim(), NOXPenLimit()) * NOXNetHead() * NOXCombEfficiency * MWhr_per_ftAcFt
  return(NOXPreEnergy_o)
}
NOXOut <- function() {
  NOXOut_o <- NOXIn()
  return(NOXOut_o)
}

#######################################################
