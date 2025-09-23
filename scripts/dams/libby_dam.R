# Libby_Dam.R
# Reservoir operations functions for Libby
# Handles inflow/outflow calculations and reservoir management operations

#--------------------- LIBBY DAM ---------------------#
#######################################################

LBFullPoolVol <- 5869400 # Gross storage at 2459 ft. pool elevation
LBBotVol <- 889900 # Minimum operating limit, 2287 ft. pool elevation
LBAvgMin <- 4000 # FCOP, 2003

InitLB <- function() {
  InitLB_o <- ResInitFractionFull * LBFullPoolVol
  return(InitLB_o)
}
Libby <- function() {
  if (week_counter == 1) {
    Libby_o <- InitLB()
  } else {
    Libby_o <- reservoir_vol_df$LIBBY[week_counter - 1]
  }
  return(Libby_o)
}
LibbyFlowData <- function() {
  return(max(FlowLB))
}
LBElev_ft <- function() {
  upper_vol <- LB_elev_input$Volume[which(LB_elev_input$Volume >= Libby())[1]]
  lower_vol <- LB_elev_input$Volume[tail(which(LB_elev_input$Volume <= Libby())[1],1)]
  upper_el <- LB_elev_input$Elevation[which(LB_elev_input$Volume >= Libby())[1]]
  lower_el <- LB_elev_input$Elevation[tail(which(LB_elev_input$Volume <= Libby())[1],1)]
  if (is.na(lower_el)) {
    LBElev_ft_o <- min(LB_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    LBElev_ft_o <- max(LB_elev_input$Elevation)
  } else if (lower_el == upper_el) {
    LBElev_ft_o <- lower_el
  } else {
    LBElev_ft_o <- lower_el + (Libby() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(LBElev_ft_o)
}
LBNetHead <- function() {
  LBTailElev <- 2121 # Libby Dam Master Plan 1997
  LBNetHead_o <- LBElev_ft() - LBTailElev
  return(LBNetHead_o)
}
LBPenLimit <- function() {
  LBPenCap <- 24100 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Libby/#top
  LBPenLimit_o <- LBPenCap * cfsTOafw
  return(LBPenLimit_o)
}
LBIn <- function() {
  LBIn_o <- LibbyFlowData()
  return(LBIn_o)
}
LBInflow <- function() {
  LBInflow_o <- LBIn()
  return(LBInflow_o)
}

############ Libby max and min releases ###################

LBMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      LBMinRefill_o <- max(LBAssuredRefill(), LBCriticalCurveMin())
    } else {
      LBMinRefill_o <- LBMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    LBMinRefill_o <- LBBotVol
  }
  return(LBMinRefill_o)
}
LBMinReq <- function() {
  LBMinReq_1 <- max(LB_trout_flow(), LB_sturgeon_flow(), LBAvgMin) * cfsTOafw
  if (FishOverRefillSw() == 1) {
    LBMinReq_o <- LBMinReq_1
  } else {
    LBMinReq_o <- min(max(Libby() + LBIn() - max(LBMinRefill(), LBCriticalCurveMin()), LBAvgMin * cfsTOafw), LBMinReq_1)
  }
  return(LBMinReq_o)
}
LBDamProtectRel <- function() {
  LBDamProtectRel_o <- max(0, Libby() + LBInflow() - LBFullPoolVol)
  return(LBDamProtectRel_o)
}
LBRelLimit <- function() {
  LBRelLimit_o <- max(max(Libby() + LBInflow() - LBBotVol, 0))
  return(LBRelLimit_o)
}
LBAvailWater <- function() {
  LBAvailWater_o <- max(0, Libby() + LBIn() - LBBotVol)
  return(LBAvailWater_o)
}

########### Libby rule curves ##############

## 1998 Revised flood control storage curve ("The effects of VARQ at Libby and Hungry Horse on Columbia River System Flood Control". U.S.A.C.E. 1998, Fig. 3).
LBFloodCurve <- function() {
  if (LBRunoffAprAug < 4.5E6) {
    LB_CurFC_o <- LBFlood_input$LBFlood1[week_in_year]
  } else if (LBRunoffAprAug < 5.5E6) {
    LB_CurFC_o <- LBFlood_input$LBFlood1[week_in_year] - (LBFlood_input$LBFlood1[week_in_year] - LBFlood_input$LBFlood2[week_in_year]) / (5.5E6 - 4.5E6) * (LBRunoffAprAug - 4.5E6)
  } else if (LBRunoffAprAug < 6.5E6) {
    LB_CurFC_o <- LBFlood_input$LBFlood2[week_in_year] - (LBFlood_input$LBFlood2[week_in_year] - LBFlood_input$LBFlood3[week_in_year]) / (6.5E6 - 5.5E6) * (LBRunoffAprAug - 5.5E6)
  } else if (LBRunoffAprAug < 7.5E6) {
    LB_CurFC_o <- LBFlood_input$LBFlood3[week_in_year] - (LBFlood_input$LBFlood3[week_in_year] - LBFlood_input$LBFlood4[week_in_year]) / (7.5E6 - 6.5E6) * (LBRunoffAprAug - 6.5E6)
  } else if (LBRunoffAprAug < 8.0E6) {
    LB_CurFC_o <- LBFlood_input$LBFlood4[week_in_year] - (LBFlood_input$LBFlood4[week_in_year] - LBFlood_input$LBFlood5[week_in_year]) / (8.0E6 - 7.5E6) * (LBRunoffAprAug - 7.5E6)
  } else {
    LB_CurFC_o <- LBFlood_input$LBFlood5[week_in_year]
  }
  return(LB_CurFC_o)
}
LBTopVol <- function() {
  if (TopRuleSw() == 0) {
    LBTopVol_o <- min(LibbyBiOpDraftLimit(), LBFloodCurve())
  } else if (TopRuleSw() == 1) {
    LBTopVol_o <- LBFullPoolVol
  } else {
    LBTopVol_o <- LBFlood_input$LBFlood_1[week_in_year]
  }
  return(LBTopVol_o)
}
LBRuleReq <- function() {
  LBRuleReq_o <- max(max(Libby() + LBIn() - LBTopVol(), 0))
  return(LBRuleReq_o)
}
LBPrelim <- function() {
  LBPrelim_o <- min(LBAvailWater(), max(LBRuleReq(), LBMinReq()))
  return(LBPrelim_o)
}
LBLowerLimit <- function() {
  LBLL_o <- lower_limit_input$Libby[week_in_year]
  return(LBLL_o)
}
LBCriticalCurve <- function() {
  LBCriticalCurve_o <- LBCriticalCurve_input[week_in_year, 2]
  return(LBCriticalCurve_o)
}
LBCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC4[week_in_year]
  }
  return(LBCriticalCurveMin_o)
}
LBAssuredRefill <- function() {
  LBAssuredRefill_o <- LBAssuredRefill_input[week_in_year, 2]
  return(LBAssuredRefill_o)
}
LBVariableRefill <- function() {
  RefillCurve_o <- LBVariableRefillCurve
  return(RefillCurve_o)
}
LB_ORC <- function() {
  LB_ORC_o <- min(max(min(max(LBAssuredRefill(), LBCriticalCurve()), LBVariableRefill()), LBLowerLimit(), LBMinRefill()), LBFloodCurve(), LibbyBiOpDraftLimit())
  return(LB_ORC_o)
}

########### Libby fish flow ############################

# 2006 NWFS Libby BiOp
# A base minimum flow of 6000 cfs is required from May 15 through September 30 and 4000 cfs October 1 to May 14 to support trout and sturgeon
LB_trout_flow <- function() {# 2022 water management plan
if (week_in_year %in% c(49:52, 1:5)) {
  if (LBRunoffAprAug < 4.8e6) {
    LB_bull_trout_o <- 6000
  } else if (LBRunoffAprAug < 6.0e6) {
    LB_bull_trout_o <- 7000
  } else if (LBRunoffAprAug < 6.7e6) {
    LB_bull_trout_o <- 8000
  } else {
    LB_bull_trout_o <- 9000
  }
} else if (week_in_year %in% c(42:48, 6:9)) {
  LB_bull_trout_o <- 6000
} else {
  LB_bull_trout_o <- 4000
}
return(LB_bull_trout_o)
}
LB_sturgeon_flow <- function() { # 2006 USFWS Libby BiOp
if (week_in_year %in% 42:52) {
  if (LBRunoffAprAug < 4.8e6) {
    tier_volume <- 0
  } else if (LBRunoffAprAug < 5.4e6) {
    tier_volume <- 0.8e6
  } else if (LBRunoffAprAug < 6.35e6) {
    tier_volume <- 0.8e6 + (LBRunoffAprAug - 5.4e6) / (6.35e6 - 5.4e6) * (1.12e6 - 0.8e6)
  } else if (LBRunoffAprAug < 7.4e6) {
    tier_volume <- 1.12e6 + (LBRunoffAprAug - 6.35e6) / (7.4e6 - 6.35e6) * (1.2e6 - 1.12e6)
  } else if (LBRunoffAprAug < 8.5e6) {
    tier_volume <- 1.2e6
  } else if (LBRunoffAprAug < 8.9e6) {
    tier_volume <- 1.2e6 + (LBRunoffAprAug - 8.5e6) / (8.9e6 - 8.5e6) * (1.6e6 - 1.2e6)
  } else {
    tier_volume <- 1.6e6
  }
} else {
  tier_volume <- 0
}
LB_sturgeon_o <- 4000 + tier_volume / 11 / cfsTOafw
return(LB_sturgeon_o)
}
# According to 2008 BiOp, Libby should be drafted to 10 ft from full by end of Sept. unless forecasted flow at the Dalles is less than the 20th percentile,
# in which case the target elevation is 20 ft from full. The 2022 water management plan gives variable target elevations based on the Libby forecasted inflows.
LibbyBiOpDraftLimit <- function() {
  if (LBRunoffAprAug <= 4.66e6) {
    LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_15th"[week_in_year]
  } else if (LBRunoffAprAug <= 5.01e6) {
    LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_15th"[week_in_year] + (LBRunoffAprAug - 4.66e6) / (5.01e6 - 4.66e6) * (LibbyBiOpDraftLimit_input$"perc_25th"[week_in_year] - LibbyBiOpDraftLimit_input$"perc_15th"[week_in_year])
  } else if (LBRunoffAprAug <= 6.78e6) {
    LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_75th"[week_in_year]
  } else if (LBRunoffAprAug <= 7.33e6) {
    LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_75th"[week_in_year] + (LBRunoffAprAug - 6.78e6) / (7.33e6 - 6.78e6) * (LibbyBiOpDraftLimit_input$"perc_85th"[week_in_year] - LibbyBiOpDraftLimit_input$"perc_75th"[week_in_year])
  } else {
    LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_85th"[week_in_year]
  }
  return(LibbyBiOpDraftLimit_o)
}
LBMcNaryDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) {
      LBMcNaryDraftLimit_o <- LBBotVol
    } else if (Fish_Pool_Alternative == 1) {
      LBMcNaryDraftLimit_o <- 4.975e6
    } else if (Fish_Pool_Alternative == 2) {
      LBMcNaryDraftLimit_o <- (LBFullPoolVol - 1.258E6)
    } else if (Fish_Pool_Alternative == 3) {
      LBMcNaryDraftLimit_o <- (LBFullPoolVol - 1.643E6)
    } else if (Fish_Pool_Alternative == 4) {
      LBMcNaryDraftLimit_o <- (LBFullPoolVol - 2.277E6)
    } else if (Fish_Pool_Alternative == 5) {
      LBMcNaryDraftLimit_o <- (LBFullPoolVol - 3.161E6)
    } else {
      LBMcNaryDraftLimit_o <- 4.975E6
    }
  } else if (FishOverRefillSw() == 0) {
    LBMcNaryDraftLimit_o <- LB_ORC()
  }
  return(LBMcNaryDraftLimit_o)
}
LBMcNarySharedWater <- function() {
  LBMcNarySharedWater_o <- max(0, Libby() + LBIn() - LBPrelim() - LBMcNaryDraftLimit())
  return(LBMcNarySharedWater_o)
}
LBMcNarySup <- function() {
  if (TotalMcNarySharedWater_c == 0) {
    LBMcNarySup_o <- 0
  } else {
    LBMcNarySup_o <- min(LBMcNarySharedWater(), McNaryFlowDeficit() * LBMcNarySharedWater() / TotalMcNarySharedWater_c)
  }
  return(LBMcNarySup_o)
}

################### Libby energy ########################

LibbyPreEnergy <- function() {
  LibbyPreEnergy_o <- MWhr_per_ftAcFt * min(LBPrelim(), LBPenLimit()) * LBNetHead() * LBCombEfficiency
  return(LibbyPreEnergy_o)
}
LBSharedWater <- function() {
  LBSharedWater_o <- max(Libby() + LBIn() - LBPrelim() - LBMcNarySup() - max(LBMinRefill(), LBCriticalCurveMin()), 0)
  return(LBSharedWater_o)
}
LBDownStreamHead <- function() {
  LBDownStreamHead_o <- TotalGCHead()
  return(LBDownStreamHead_o)
}
LBEnergyContent <- function() {
  LBEnergyContent_o <- LBSharedWater() * (LBNetHead() + LBDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(LBEnergyContent_o)
}
LB_ORCSharedWater <- function() {
  LB_ORCSharedWater_o <- max(0, Libby() + LBIn() - LBPrelim() - LBMcNarySup() - LB_ORC())
  return(LB_ORCSharedWater_o)
}
LB_ORCEnergyContent <- function() {
  LB_ORCEnergyContent_o <- LB_ORCSharedWater() * (LBNetHead() + LBDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(LB_ORCEnergyContent_o)
}
LBFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    if (TotalEnergyContent_c == 0) {
      LBFirmEngSup_o <- 0
    } else {
      LBFirmEngSup_o <- (LBEnergyContent() + LB_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
    }
  } else if (Total_ORCEnergyContent_c == 0) {
    LBFirmEngSup_o <- 0
  } else {
    LBFirmEngSup_o <- LB_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
  }
  return(LBFirmEngSup_o)
}
LBNFEnergyContent <- function() {
  LBNFEnergyContent_o <- max(0, LB_ORCEnergyContent() - LBFirmEngSup())
  return(LBNFEnergyContent_o)
}
LBNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    LBNonFirmEngSup_o <- 0
  } else {
    LBNonFirmEngSup_o <- LBNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
  }
  return(LBNonFirmEngSup_o)
}
LBFirmEngSupReq <- function() {
  LBFirmEngSupReq_o <- min(LBPenLimit(), LBFirmEngSup() / (MWhr_per_ftAcFt * (LBNetHead() + LBDownStreamHead()) * LBCombEfficiency))
  return(LBFirmEngSupReq_o)
}
LBNonFirmEngSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    LBNonFirmEngSupReq_o <- min(LBPenLimit(), (LBFirmEngSup() + LBNonFirmEngSup()) / (MWhr_per_ftAcFt * (LBNetHead() + LBDownStreamHead()) * LBCombEfficiency))
  } else {
    LBNonFirmEngSupReq_o <- 0
  }
  return(LBNonFirmEngSupReq_o)
}
LBEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    LBEnergySup_o <- max(min(LBFirmEngSupReq(), LBSharedWater()), min(LBNonFirmEngSupReq(), LB_ORCSharedWater()))
  } else {
    LBEnergySup_o <- max(min(LBFirmEngSupReq(), LB_ORCSharedWater()), min(LBNonFirmEngSupReq(), LB_ORCSharedWater()))
  }
  return(LBEnergySup_o)
}
LBCombSup <- function() {
  LBCombSup_o <- LBMcNarySup() + LBEnergySup()
  LBCombSup_c <<- LBCombSup_o
  return(LBCombSup_o)
}
LBMcNarySupEnergy <- function() { # Hydropower generated by releasing water from Libby to meet McNary fish flow target
LBMcNarySupEnergy_o <- LBMcNarySup() * (LBNetHead() + LBDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
return(LBMcNarySupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ###############################

LBFloodSpace <- function() {
  LBFloodSpace_o <- min(LBPrelim() + LBCombSup(), max(0, LBFullPoolVol - (Libby() + LBIn() - LBPrelim() - LBCombSup())))
  LBFloodSpace_c <<- LBFloodSpace_o
  return(LBFloodSpace_o)
}
LBFloodFrac <- function() {
  if (TotalFloodSpace_c == 0) {
    LBFloodFrac_o <- 0
  } else {
    LBFloodFrac_o <- LBFloodSpace_c / TotalFloodSpace_c
  }
  return(LBFloodFrac_o)
}
LBRelReducReq <- function() {
  LBRelReducReq_o <- TotalRelReducReq_c * LBFloodFrac()
  return(LBRelReducReq_o)
}
LBDamProtectExcess <- function() {
  LBDamProtectExcess_o <- max(0, LBDamProtectRel() - LBPrelim() - LBCombSup_c)
  return(LBDamProtectExcess_o)
}

############# Libby final release ############################

LBRelease <- function() {
  LBRelease_o <- max(min(LBPrelim() + LBCombSup_c - LBRelReducReq(), LBRelLimit()), LBDamProtectRel(), LBAvgMin * cfsTOafw)
  return(LBRelease_o)
}
LBOutflow <- function() {
  LBOutflow_o <- LBRelease_c
  return(LBOutflow_o)
}

#######################################################
#------------------- BONNERS FERRY -------------------#
#######################################################

BonnersFerryFlowData <- function() {
  return(max(FlowBONF))
}
BONFInc <- function() {
  BONFInc_o <- BonnersFerryFlowData() - LibbyFlowData()
  return(BONFInc_o)
}
BONFIn <- function() {
  BONFIn_o <- LBRelease() + BONFInc()
  return(BONFIn_o)
}
BONFOut <- function() {
  BONDFOut_o <- BONFIn()
  return(BONDFOut_o)
}

#######################################################
