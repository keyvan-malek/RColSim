#---------------------- KERR DAM ---------------------#
#######################################################

KEFullPoolVol <- 1792000 # Storage at 2893 ft. of pool elevation
KEBotVol <- 572800 # Storage at 2883 ft. of pool elevation
KEAvgMin <- 3000

InitKE <- function() {
  InitKE_o <- ResInitFractionFull * KEFullPoolVol
  return(InitKE_o)
}
Kerr <- function() {
  if (week_counter == 1) {
    Kerr_o <- InitKE()
  } else {
    Kerr_o <- reservoir_vol_df$FLAPO[week_counter - 1]
  }
  return(Kerr_o)
}
KerrFlowData <- function() {
  return(max(FlowKE))
}
KEInc <- function() {
  KEInc_o <- KerrFlowData() - ColumbiaFallsFlowData()
  return(KEInc_o)
}
KerrElev_ft <- function() {
  upper_vol <- KE_elev_input$Volume[which(KE_elev_input$Volume >= Kerr())[1]]
  lower_vol <- KE_elev_input$Volume[tail(which(KE_elev_input$Volume <= Kerr())[1],1)]
  upper_el <- KE_elev_input$Elevation[which(KE_elev_input$Volume >= Kerr())[1]]
  lower_el <- KE_elev_input$Elevation[tail(which(KE_elev_input$Volume <= Kerr())[1],1)]
  if (is.na(lower_el)) {
    KerrElev_ft_o <- min(KE_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    KerrElev_ft_o <- max(KE_elev_input$Elevation)
  } else if (upper_el == lower_el) {
    KerrElev_ft_o <- lower_el
  } else {
    KerrElev_ft_o <- lower_el + (Kerr() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(KerrElev_ft_o)
}
KENetHead <- function() {
  KETailElev <- 2706 # Average tailwater elevation.  Units ft.
  KENetHead_o <- KerrElev_ft() - KETailElev
  return(KENetHead_o)
}
KEPenLimit <- function() {
  KEPenCap <- 14350 # cfs
  KEPenLimit_o <- KEPenCap * cfsTOafw
  return(KEPenLimit_o)
}
KEIn <- function() {
  KEIn_o <- HHPrelim() + (KerrFlowData() - HungryHorseFlowData())
  return(KEIn_o)
}
KEPreInflow <- function() {
  KEPreInflow_o <- max(min(HHPrelim() + HHCombSup_c, HHRelLimit()), HHDamProtectRel()) + KerrFlowData() - HungryHorseFlowData()
  KEPreInflow_c <<- KEPreInflow_o
  return(KEPreInflow_o)
}
KEInflow <- function() {
  KEInflow_o <- COLOut() + KEInc()
  return(KEInflow_o)
}

################# Kerr max and min releases ####################################

KEMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      KEMinRefill_o <- max(KEAssuredRefill(), KECriticalCurveMin())
    } else {
      KEMinRefill_o <- KEMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    KEMinRefill_o <- KEBotVol
  }
  return(KEMinRefill_o)
}
KEMinReq <- function() {
  KEMinReq_1 <- Article_56() * cfsTOafw
  KEMinReq_o <- min(max(Kerr() + KEIn() - KEMinRefill(), KEAvgMin * cfsTOafw), KEMinReq_1)
  return(KEMinReq_o)
}
KEDamProtectRel <- function() {
  KEDamProtectRel_o <- max(0, KerrFlowData() + KEPreInflow() - KEFullPoolVol)
  return(KEDamProtectRel_o)
}
KERelLimit <- function() {
  KERelLimit_o <- max(max(Kerr() + KEPreInflow_c - KEBotVol, 0))
  return(KERelLimit_o)
}
KEAvailWater <- function() {
  KEAvailWater_o <- max(max(Kerr() + KEIn() - KEBotVol, 0))
  return(KEAvailWater_o)
}

################## Kerr rule curves ####################################

KEFloodCurve <- function() {
  KEFlood_o <- KEFlood_input[week_in_year, 2] # 1965 Memorandum of Understanding between Montana Power Company and U.S.A.C.E. (Montana Power Co., 35 F.P.C. 250 (1966))
  return(KEFlood_o)
}
KETopVol <- function() {
  KETopVol_o <- KEFloodCurve()
  return(KETopVol_o)
}
KERuleReq <- function() {
  KERuleReq_o <- max(max(Kerr() + KEIn() - KETopVol(), 0))
  return(KERuleReq_o)
}
KEPrelim <- function() {
  KEPrelim_o <- min(KEAvailWater(), max(KERuleReq(), KEMinReq()))
  return(KEPrelim_o)
}
KECriticalCurve <- function() {
  KECriticalCurve_o <- KECriticalCurve_input$CRC1[week_in_year]
  return(KECriticalCurve_o)
}
KECriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    KECriticalCurveMin_o <- KECriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    KECriticalCurveMin_o <- KECriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    KECriticalCurveMin_o <- KECriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    KECriticalCurveMin_o <- KECriticalCurve_input$CRC4[week_in_year]
  }
  return(KECriticalCurveMin_o)
}
KEAssuredRefill <- function() {
  KEAssuredRefill_o <- KEAssuredRefill_input[week_in_year, 2]
  return(KEAssuredRefill_o)
}
Kerr_ORC <- function() {
  Kerr_ORC_o <- min(max(KEAssuredRefill(), KECriticalCurve()), KEFloodCurve())
  return(Kerr_ORC_o)
}

################## Kerr fish flow ####################################

######  Article_56
# License Requirement for Kerr - FERC License No. 5, Article 56

Article_56 <- function() {
  Article_56_o <- Article_56_input[week_in_year, 2]
  return(Article_56_o)
}

################### Kerr energy ######################################

KEPreEnergy <- function() {
  KEPreEnergy_o <- MWhr_per_ftAcFt * min(KEPrelim(), KEPenLimit()) * KENetHead() * KECombEfficiency
  return(KEPreEnergy_o)
}
KerrGrPreEnergy <- function() {
  KerrGrPreEnergy_o <- CBPreEnergy() + KEPreEnergy() + NOXPreEnergy()
  return(KerrGrPreEnergy_o)
}
KESharedWater <- function() {
  KESharedWater_o <- max(Kerr() + KEIn() - KEPrelim() - max(KEMinRefill(), KECriticalCurveMin()), 0)
  return(KESharedWater_o)
}
KEDownStreamHead <- function() {
  KEDownStreamHead_o <- AFNetHead() + BCNetHead() + BDNetHead() + CBNetHead() + NOXNetHead() + TotalGCHead()
  return(KEDownStreamHead_o)
}
KerrEnergyContent <- function() {
  KerrEnergyContent_o <- KESharedWater() * (KENetHead() + KEDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(KerrEnergyContent_o)
}
Kerr_ORCSharedWater <- function() {
  Kerr_ORCSharedWater_o <- max(0, Kerr() + KEIn() - KEPrelim() - Kerr_ORC())
  return(Kerr_ORCSharedWater_o)
}
Kerr_ORCEnergyContent <- function() {
  Kerr_ORCEnergyContent_o <- Kerr_ORCSharedWater() * (KENetHead() + KEDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(Kerr_ORCEnergyContent_o)
}
KEFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
    if (TotalEnergyContent_c == 0) {
      KEFirmEngSup_o <- 0
    } else {
      KEFirmEngSup_o <- (KerrEnergyContent() + Kerr_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
    }
  } else if (Total_ORCEnergyContent_c == 0) {
    KEFirmEngSup_o <- 0
  } else {
    KEFirmEngSup_o <- Kerr_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
  }
  return(KEFirmEngSup_o)
}
KENFEnergyContent <- function() {
  KENFEnergyContent_o <- max(0, Kerr_ORCEnergyContent() - KEFirmEngSup())
  return(KENFEnergyContent_o)
}
KENonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    KENonFirmEngSup_o <- 0
  } else {
    KENonFirmEngSup_o <- KENFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
  }
  return(KENonFirmEngSup_o)
}
KEFirmEngSupReq <- function() {
  KEFirmEngSupReq_o <- min(KEPenLimit(), KEFirmEngSup() / (MWhr_per_ftAcFt * (KENetHead() + KEDownStreamHead()) * KECombEfficiency))
  return(KEFirmEngSupReq_o)
}
KENonFirmSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    KENonFirmSupReq_o <- min(KEPenLimit(), (KEFirmEngSup() + KENonFirmEngSup()) / (MWhr_per_ftAcFt * (KENetHead() + KEDownStreamHead()) * KECombEfficiency))
  } else {
    KENonFirmSupReq_o <- 0
  }
  return(KENonFirmSupReq_o)
}
KerrEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    KerrEnergySup_o <- max(min(KEFirmEngSupReq(), KESharedWater()), min(KENonFirmSupReq(), Kerr_ORCSharedWater()))
  } else {
    KerrEnergySup_o <- max(min(KEFirmEngSupReq(), Kerr_ORCSharedWater()), min(KENonFirmSupReq(), Kerr_ORCSharedWater()))
  }
  return(KerrEnergySup_o)
}
KECombUpSup <- function() {
  if (HHCombSup_c == -9999) {
    KECombUpSup_o <- HHCombSup()
  } else {
    KECombUpSup_o <- HHCombSup_c
  }
  return(KECombUpSup_o)
}
KECombSup <- function() {
  KECombSup_o <- KerrEnergySup() + KECombUpSup()
  KECombSup_c <<- KECombSup_o
  return(KECombSup_o)
}
KECombUpProtect <- function() {
  outflow <- KEPrelim() + KECombSup_c
  KECombUpProtect_o <- max(max(min(Kerr() + KEPreInflow() - outflow - Kerr_ORC()), 0))
  KECombUpProtect_c <<- KECombUpProtect_o
  return(KECombUpProtect_o)
}
KEDamProtectExcess <- function() {
  KEDamProtectExcess_o <- max(0, KEDamProtectRel() - KEPrelim() - KECombSup_c - KECombUpProtect())
  return(KEDamProtectExcess_o)
}

################ Kerr final release #####################

KERelease <- function() {
  KERelease_o <- max(min(KEPrelim() + KECombSup() + KECombUpProtect(), KERelLimit()), KEDamProtectRel())
  KERelease_c <<- KERelease_o
  return(KERelease_o)
}
KEOutflow <- function() {
  KEOutflow_o <- KERelease_c
  return(KEOutflow_o)
}

#######################################################
