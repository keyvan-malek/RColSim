#------------------ CHELAN DAM -----------------------#
#######################################################

CHFullPoolVol <- 1676000
CHBotVol <- 998600

InitCH <- function() {
  InitCH_o <- ResInitFractionFull * CHFullPoolVol
  return(InitCH_o)
}
Chelan <- function() {
  if (week_counter == 1) {
    Chelan_o <- InitCH()
  } else {
    Chelan_o <- reservoir_vol_df$CHELA[week_counter - 1]
  }
  return(Chelan_o)
}
ChelanFlowData <- function() {
  return(max(FlowCH))
}
CHPenLimit <- function() {
  CHPenCap <- 2200 # Chelan settlement agreement, FERC P-637-022, 2003
  CHPenLimit_o <- CHPenCap * cfsTOafw
  return(CHPenLimit_o)
}
CHNetHead <- function() {
  CHNetHead_o <- 400 # https://hydroreform.org/resource/lake-chelan-project/
  return(CHNetHead_o)
}
CHIn <- function() {
  CHIn_o <- ChelanFlowData()
  return(CHIn_o)
}
CHInflow <- function() {
  CHInflow_o <- CHIn()
  return(CHInflow_o)
}

##################### Chelan max and min releases ##########

CHMinReq <- function() { # Chelan settlement agreement, FERC P-637-022, 2003
if (ChelanFlowData() <= CHFlow_percentiles$Q20[week_in_year]) {
  CHMin <- 80
} else if (ChelanFlowData() > CHFlow_percentiles$Q80[week_in_year]) {
  if (week_in_year %in% c(51,52,1:40)) {
    CHMin <- 80
  } else {
    CHMin <- 320
  }
} else {
  if (week_in_year %in% c(51,52,1:40)) {
    CHMin <- 80
  } else {
    CHMin <- 200
  }
}
CHMinReq_o <- CHMin * cfsTOafw
return(CHMinReq_o)
}
CHDamProtectRel <- function() {
  CHDamProtectRel_o <- max(0, ChelanFlowData() + CHInflow() - CHFullPoolVol)
  return(CHDamProtectRel_o)
}
CHRelLimit <- function() {
  CHRelLimit_o <- max(max(Chelan() + CHInflow() - CHBotVol, 0))
  return(CHRelLimit_o)
}
CHAvailWater <- function() {
  CHAvailWater_o <- max(0, Chelan() + CHIn() - CHBotVol)
  return(CHAvailWater_o)
}

############## Chelan rule curve ##################

CHFloodCurve <- function() {
  CHFloodCurve_o <- CHFlood_input$CHFlood[week_in_year]
  return(CHFloodCurve_o)
}
CHTopVol <- function() {
  if (TopRuleSw() == 0) {
    CHTopVol_o <- CHFloodCurve()
  } else if (TopRuleSw() == 1) {
    CHTopVol_o <- CHFullPoolVol
  } else if (TopRuleSw() == 2) {
    CHTopVol_o <- CHFlood_input$CHFlood[week_in_year]
  }
  return(CHTopVol_o)
}
CHRuleReq <- function() {
  CHRuleReq_o <- max(max(Chelan() + CHIn() - CHTopVol(), 0))
  return(CHRuleReq_o)
}
CHPrelim <- function() {
  CHPrelim_o <- min(CHAvailWater(), max(CHRuleReq(), CHMinReq()))
  return(CHPrelim_o)
}

################## Chelan energy ######################

CHPreEnergy <- function() {
  CHPreEnergy_o <- MWhr_per_ftAcFt * min(CHPrelim(), CHPenLimit()) * CHNetHead() * CHCombEfficiency
  return(CHPreEnergy_o)
}

################## Chelan final release ###############

CHRelease <- function() {
  CHRelease_o <- max(CHDamProtectRel(), min(CHPrelim(), CHRelLimit()))
  return(CHRelease_o)
}
CHOutflow <- function() {
  CHOutflow_o <- CHRelease_c
  return(CHOutflow_o)
}

#######################################################
