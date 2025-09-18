#------------------- DWORSHAK DAM --------------------#
#######################################################

DWFullPoolVol <- 3468000 # Storage at pool elevation of 1600 ft.
DWBotVol <- 1452800 # Volume cooresponding to the bottom of conservation pool elevation of 1445 ft.  Units acre-ft.
DWAvgMin <- 1300

InitDW <- function() {
  InitDW_o <- ResInitFractionFull * DWFullPoolVol
  return(InitDW_o)
}
Dworshak <- function() {
  if (week_counter == 1) {
    Dworshak_o <- InitDW()
  } else {
    Dworshak_o <- reservoir_vol_df$DWORS[week_counter - 1]
  }
  return(Dworshak_o)
}
DworshakFlowData <- function() {
  return(max(FlowDW))
}
DWElev_ft <- function() {
  upper_vol <- DW_elev_input$Volume[which(DW_elev_input$Volume >= Dworshak())[1]]
  lower_vol <- DW_elev_input$Volume[tail(which(DW_elev_input$Volume <= Dworshak())[1],1)]
  upper_el <- DW_elev_input$Elevation[which(DW_elev_input$Volume >= Dworshak())[1]]
  lower_el <- DW_elev_input$Elevation[tail(which(DW_elev_input$Volume <= Dworshak())[1],1)]
  if (is.na(lower_el)) {
    DWElev_ft_o <- min(DW_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    DWElev_ft_o <- max(DW_elev_input$Elevation)
  } else if (lower_el == upper_el) {
    DWElev_ft_o <- lower_el
  } else {
    DWElev_ft_o <- lower_el + (Dworshak() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(DWElev_ft_o)
}
DWNetHead <- function() {
  DWTailElev <- 975 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=dworshak; changed from 980 5/25/23
  DWNetHead_o <- DWElev_ft() - DWTailElev
  return(DWNetHead_o)
}
DWPenLimit <- function() {
  DWPenCap <- 10500 # Changed from 24000 on 5/25/23. Source: https://www.nwd.usace.army.mil/CRSO/Project-Locations/Dworshak/#top
  DWPenLimit_o <- DWPenCap * cfsTOafw
  return(DWPenLimit_o)
}
DWIn <- function() {
  DWIn_o <- DworshakFlowData()
  return(DWIn_o)
}
DWInflow <- function() {
  DWInflow_o <- DWIn()
  return(DWInflow_o)
}

############ Dworshak max and min releases ###################

DWMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      DWMinRefill_o <- max(DWAssuredRefill(), DWCriticalCurveMin())
    } else {
      DWMinRefill_o <- DWMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    DWMinRefill_o <- DWBotVol
  }
  return(DWMinRefill_o)
}
DWMinReq <- function() {
  DWMinReq_o <- DWAvgMin * cfsTOafw
  return(DWMinReq_o)
}
DWDamProtectRel <- function() {
  DWDamProt <- max(0, Dworshak() + DWInflow() - DWFullPoolVol)
  return(DWDamProt)
}
DWRelLimit <- function() {
  DWRelLimit_o <- max(max(Dworshak() + DWInflow() - DWBotVol, 0))
  return(DWRelLimit_o)
}
DWAvailWater <- function() {
  DWAvailWater_o <- max(0, Dworshak() + DWIn() - DWBotVol)
  return(DWAvailWater_o)
}

################### Dworshak rule curves ##################################

# 1992 Local flood control curve
DWFloodCurve <- function() {
  if (DWRunoffAprJul < 1.2E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood1[week_in_year]
  } else if (DWRunoffAprJul < 1.4E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood1[week_in_year] - (DWFlood_input$DWFlood1[week_in_year] - DWFlood_input$DWFlood2[week_in_year]) / (1.4E6 - 1.2E6) * (DWRunoffAprJul - 1.2E6)
  } else if (DWRunoffAprJul < 1.8E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood2[week_in_year] - (DWFlood_input$DWFlood2[week_in_year] - DWFlood_input$DWFlood3[week_in_year]) / (1.8E6 - 1.4E6) * (DWRunoffAprJul - 1.4E6)
  } else if (DWRunoffAprJul < 2.2E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood3[week_in_year] - (DWFlood_input$DWFlood3[week_in_year] - DWFlood_input$DWFlood4[week_in_year]) / (2.2E6 - 1.8E6) * (DWRunoffAprJul - 1.8E6)
  } else if (DWRunoffAprJul < 2.6E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood4[week_in_year] - (DWFlood_input$DWFlood4[week_in_year] - DWFlood_input$DWFlood5[week_in_year]) / (2.6E6 - 2.2E6) * (DWRunoffAprJul - 2.2E6)
  } else if (DWRunoffAprJul < 3.0E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood5[week_in_year] - (DWFlood_input$DWFlood5[week_in_year] - DWFlood_input$DWFlood6[week_in_year]) / (3.0E6 - 2.6E6) * (DWRunoffAprJul - 2.6E6)
  } else if (DWRunoffAprJul < 3.2E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood6[week_in_year] - (DWFlood_input$DWFlood6[week_in_year] - DWFlood_input$DWFlood7[week_in_year]) / (3.2E6 - 3.0E6) * (DWRunoffAprJul - 3.0E6)
  } else if (DWRunoffAprJul < 3.4E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood7[week_in_year] - (DWFlood_input$DWFlood7[week_in_year] - DWFlood_input$DWFlood8[week_in_year]) / (3.4E6 - 3.2E6) * (DWRunoffAprJul - 3.2E6)
  } else if (DWRunoffAprJul < 3.6E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood8[week_in_year] - (DWFlood_input$DWFlood8[week_in_year] - DWFlood_input$DWFlood9[week_in_year]) / (3.6E6 - 3.4E6) * (DWRunoffAprJul - 3.4E6)
  } else if (DWRunoffAprJul < 3.8E6) {
    DW_CurFC_o <- DWFlood_input$DWFlood9[week_in_year] - (DWFlood_input$DWFlood9[week_in_year] - DWFlood_input$DWFlood10[week_in_year]) / (3.8E6 - 3.6E6) * (DWRunoffAprJul - 3.6E6)
  } else {
    DW_CurFC_o <- DWFlood_input$DWFlood10[week_in_year]
  }
  return(DW_CurFC_o)
}
DWTopVol <- function() {
  if (TopRuleSw() == 0) {
    DWTopVol_o <- min(DWBiOpDraftLimit(), DWFloodCurve())
  } else if (TopRuleSw() == 1) {
    DWTopVol_o <- DWFullPoolVol
  } else if (TopRuleSw() == 2) {
    DWTopVol_o <- DWFlood_input$DWFlood1[week_in_year]
  }
  return(DWTopVol_o)
}
DWRuleReq <- function() {
  DWRuleReq_o <- max(max(Dworshak() + DWIn() - DWTopVol(), 0))
  return(DWRuleReq_o)
}
DWPrelim <- function() {
  DWPrelim_o <- min(DWAvailWater(), max(DWRuleReq(), DWMinReq()))
  return(DWPrelim_o)
}
DWLowerLimit <- function() {
  DWLL_o <- lower_limit_input$Dworshak[week_in_year]
  return(DWLL_o)
}
DWCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    DWCriticalCurve_o <- DWCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    DWCriticalCurve_o <- DWCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    DWCriticalCurve_o <- DWCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    DWCriticalCurve_o <- DWCriticalCurve_input$CRC4[week_in_year]
  }
  return(DWCriticalCurve_o)
}
DWCriticalCurve <- function() {
  DWCriticalCurve_o <- DWCriticalCurve_input$CRC1[week_in_year]
  return(DWCriticalCurve_o)
}
DWAssuredRefill <- function() {
  DWAssuredRefill_o <- DWAssuredRefill_input[week_in_year, 2]
  return(DWAssuredRefill_o)
}
DWVariableRefill <- function() {
  if (RefillSwitch() == 1) {
    DWRefillCurve_o <- DWAssuredRefill()
  } else if (RefillSwitch() == 2) {
    DWRefillCurve_o <- DWVariableRefillCurve
  }
  return(DWRefillCurve_o)
}
DW_ORC <- function() {
  DW_ORC_o <- min(max(min(max(DWAssuredRefill(), DWCriticalCurve()), DWVariableRefill()), DWLowerLimit()), DWFloodCurve(), DWBiOpDraftLimit())
  return(DW_ORC_o)
}

##################### Dworshak fish flows ##############################

DWLGDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) {
      DWLGDraftLimit_o <- DWBotVol
    } else {
      DWLGDraftLimit_o <- 2.238e6
    }
  } else if (FishOverRefillSw() == 0) {
    DWLGDraftLimit_o <- DW_ORC()
  }
  return(DWLGDraftLimit_o)
}

## According to 2008 BiOp, Dworshak is to be drafted to elevation 1535 ft by end of Aug and 1520 ft by end of September.
DWBiOpDraftLimit <- function() {
  DWBiOpDraftLimit_o <- DWBiOpDraftLimit_input[week_in_year, 2]
  return(DWBiOpDraftLimit_o)
}
DWLGAvailWater <- function() {
  DWLGAvailWater_o <- max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
  return(DWLGAvailWater_o)
}
DWLGSup <- function() { # Release from Dworshak for meeting Lower Graninte fish flow target
# Handle NA values in global variables
BRIn_c_safe <- ifelse(is.na(BRIn_c), 0, BRIn_c)
BRPrelim_c_safe <- ifelse(is.na(BRPrelim_c), 0, BRPrelim_c)

denominator <- max(0, Brownlee() + BRIn_c_safe - BRPrelim_c_safe - BRLGDraftLimit()) +
max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
numerator <- max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
if (denominator == 0) {
  DWRelForLG_1 <- 0
} else {
  DWRelForLG_1 <- TotalRelForLowerGranite() * numerator / denominator
}
DWRelForLG_o <- min(DWLGAvailWater(), DWRelForLG_1)
water_df$DWLGSup[week_counter] <<- DWRelForLG_o
return(DWRelForLG_o)
}

##################### Dworshak energy ########################################

DWPreEnergy <- function() {
  DWPreEnergy_o <- MWhr_per_ftAcFt * min(DWPrelim(), DWPenLimit()) * DWNetHead() * DWCombEfficiency
  return(DWPreEnergy_o)
}
DWSharedWater <- function() {
  DWSharedWater_o <- max(Dworshak() + DWIn() - DWPrelim() - DWLGSup() - max(DWMinRefill(), DWCriticalCurveMin()), 0)
  return(DWSharedWater_o)
}
DWDownStreamHead <- function() {
  DWDownStreamHead_o <- BONNetHead() + DANetHead() + IHNetHead() + JDNetHead() + LGNetHead() + LIGNetHead() + LMNetHead() + MCNetHead()
  return(DWDownStreamHead_o)
}
DWEnergyContent <- function() {
  DWEnergyContent_o <- DWSharedWater() * (DWNetHead() + DWDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(DWEnergyContent_o)
}
DW_ORCSharedWater <- function() {
  DW_ORCSharedWater_o <- max(0, Dworshak() + DWIn() - DWPrelim() - DWLGSup() - DW_ORC())
  return(DW_ORCSharedWater_o)
}
DW_ORCEnergyContent <- function() {
  DW_ORCEnergyContent_o <- DW_ORCSharedWater() * (DWNetHead() + DWDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(DW_ORCEnergyContent_o)
}
DWFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    # Handle NA values in global variables
    TotalEnergyContent_c_safe <- ifelse(is.na(TotalEnergyContent_c), 0, TotalEnergyContent_c)
    Total_ORCEnergyContent_c_safe <- ifelse(is.na(Total_ORCEnergyContent_c), 0, Total_ORCEnergyContent_c)
    FirmEnergyDeficit_c_safe <- ifelse(is.na(FirmEnergyDeficit_c), 0, FirmEnergyDeficit_c)

    if (TotalEnergyContent_c_safe == 0) {
      DWFirmEngSup_o <- 0
    } else {
      DWFirmEngSup_o <- (DWEnergyContent() + DW_ORCEnergyContent()) / (TotalEnergyContent_c_safe + Total_ORCEnergyContent_c_safe) * FirmEnergyDeficit_c_safe
    }
  } else if (Total_ORCEnergyContent_c_safe == 0) {
    DWFirmEngSup_o <- 0
  } else {
    DWFirmEngSup_o <- DW_ORCEnergyContent() / Total_ORCEnergyContent_c_safe * FirmEnergyDeficit_c_safe
  }
  if (is.na(water_df$DWFirmEngSup[week_in_year])) {
    water_df$DWFirmEngSup[week_in_year] <<- DWFirmEngSup_o
  }
  return(DWFirmEngSup_o)
}
DWNFEnergyContent <- function() {
  DWNFEnergyContent_o <- max(0, DW_ORCEnergyContent() - DWFirmEngSup())
  return(DWNFEnergyContent_o)
}
DWNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    DWNonFirmEngSup_o <- 0
  } else {
    DWNonFirmEngSup_o <- DWNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
  }
  return(DWNonFirmEngSup_o)
}
DWFirmEngSupReq <- function() {
  DWFirmEngSupReq_o <- min(DWPenLimit(), DWFirmEngSup() / (MWhr_per_ftAcFt * (DWNetHead() + DWDownStreamHead()) * DWCombEfficiency))
  return(DWFirmEngSupReq_o)
}
DWNonFirmEngSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    DWNonFirmEngSupReq_o <- min(DWPenLimit(), (DWFirmEngSup() + DWNonFirmEngSup()) / (MWhr_per_ftAcFt * (DWNetHead() + DWDownStreamHead()) * DWCombEfficiency))
  } else {
    DWNonFirmEngSupReq_o <- 0
  }
  return(DWNonFirmEngSupReq_o)
}
DWEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DWSharedWater()), min(DWNonFirmEngSupReq(), DW_ORCSharedWater())))
  } else {
    DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DW_ORCSharedWater()), min(DWNonFirmEngSupReq(), DW_ORCSharedWater())))
  }
  return(DWEnergySup_o)
}
DWCombSup <- function() {
  DWCombSup_o <- DWLGSup() + DWEnergySup()
  return(DWCombSup_o)
}
DWLGSupEnergy <- function() {
  DWLGSupEnergy_o <- DWLGSup() * (DWNetHead() + DWDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(DWLGSupEnergy_o)
}
DWDamProtectExcess <- function() {
  DWDamProtectExcess_o <- max(max(DWDamProtectRel() - DWPrelim() - DWCombSup(), 0))
  return(DWDamProtectExcess_o)
}

############# Dworshak final release ############################

DWRelease <- function() {
  DWRelease_o <- max(min(DWPrelim() + DWCombSup(), DWRelLimit()), DWDamProtectRel())
  return(DWRelease_o)
}
DWOutflow <- function() {
  DWOutflow_o <- DWRelease_c
  return(DWOutflow_o)
}

#######################################################
