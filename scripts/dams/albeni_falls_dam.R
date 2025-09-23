# Albeni_Falls_Dam.R
# Reservoir operations functions for Albeni Falls
# Handles inflow/outflow calculations and reservoir management operations

#--------------- ALBENI FALLS DAM --------------------#
#######################################################

AFFullPoolVol <- 1540000 # Volume corresponding to 2062.5 ft of elevation.  Normal full pool.  Units acre-ft.
AFBotVol <- 384800 # Volume corresponding to 2049.7 ft elevation.
AFAvgMin <- 4000 # Minimum project outflow, cfs

InitAF <- function() {
  InitAF_o <- ResInitFractionFull * AFFullPoolVol
  return(InitAF_o)
}
AlbeniFalls <- function() {
  if (week_counter == 1) {
    AlbeniFalls_o <- InitAF()
  } else {
    AlbeniFalls_o <- reservoir_vol_df$ALBEN[week_counter - 1]
  }
  return(AlbeniFalls_o)
}
AlbeniFallsFlowData <- function() {
  return(max(FlowAF))
}
AFInc <- function() {
  AFInc_o <- AlbeniFallsFlowData() - CabinetFlowData()
  return(AFInc_o)
}
AFElev_ft <- function() {
  upper_vol <- AF_elev_input$Volume[which(AF_elev_input$Volume >= AlbeniFalls())[1]]
  lower_vol <- AF_elev_input$Volume[tail(which(AF_elev_input$Volume <= AlbeniFalls())[1],1)]
  upper_el <- AF_elev_input$Elevation[which(AF_elev_input$Volume >= AlbeniFalls())[1]]
  lower_el <- AF_elev_input$Elevation[tail(which(AF_elev_input$Volume <= AlbeniFalls())[1],1)]
  if (is.na(lower_el)) {
    AFElev_ft_o <- min(AF_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    AFElev_ft_o <- max(AF_elev_input$Elevation)
  } else if (upper_el == lower_el) {
    AFElev_ft_o <- lower_el
  } else {
    AFElev_ft_o <- lower_el + (AlbeniFalls() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(AFElev_ft_o)
}
AFNetHead <- function() {
  AFTailElev <- 2036 # https://pweb.crohms.org/dd/common/dataquery/www/?k=albeni%20fals. Changed from 2042.2 on 5/25/23
  AFNetHead_o <- AFElev_ft() - AFTailElev
  return(AFNetHead_o)
}
AFPenLimit <- function() {
  AFPenCap <- 33000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Albeni-Falls/#top
  AFPenLimit_o <- AFPenCap * cfsTOafw
  return(AFPenLimit_o)
}
AFIn <- function() {
  AFIn_o <- CBPrelim() + AFInc()
  return(AFIn_o)
}
AFPreInflow <- function() {
  AFPreInflow_o <- KERelease() + AlbeniFallsFlowData() - KerrFlowData()
  AFPreInflow_c <<- AFPreInflow_o
  return(AFPreInflow_o)
}
AFInflow <- function() {
  AFInflow_o <- CBOut() + AFInc()
  return(AFInflow_o)
}

############ Albeni Falls max and min releases ###################

AFMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      AFMinRefill_o <- max(AFAssuredRefill(), AFCriticalCurveMin())
    } else {
      AFMinRefill_o <- AFMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    AFMinRefill_o <- AFBotVol
  }
  return(AFMinRefill_o)
}
AFMinReq <- function() {
  AFMinReq_o <- AFAvgMin * cfsTOafw
  return(AFMinReq_o)
}
AFDamProtectRel <- function() {
  AFDamProtectRel_o <- max(max(AlbeniFalls() + AFPreInflow_c - AFFullPoolVol, 0))
  return(AFDamProtectRel_o)
}
AFRelLimit <- function() {
  AFRelLimit_o <- max(max(AlbeniFalls() + AFPreInflow() - AFBotVol, 0))
  return(AFRelLimit_o)
}
AFAvailWater <- function() {
  AFAvailWater_o <- max(max(AlbeniFalls() + AFIn() - AFBotVol, 0))
  return(AFAvailWater_o)
}

########### Albeni Falls rule curves ##############

AFFloodCurve <- function() {
  AFFloodCurve_o <- AFFlood_input$Max[week_in_year]
  return(AFFloodCurve_o)
}
AFTopVol <- function() {
  if (TopRuleSw() == 0) {
    AFTopVol_o <- AFFloodCurve()
  } else if (TopRuleSw() == 1) {
    AFTopVol_o <- AFFullPoolVol
  } else if (TopRuleSw() == 1) {
    AFTopVol_o <- AFFlood_input$Max[week_in_year]
  }
  return(AFTopVol_o)
}
AFRuleReq <- function() {
  AFRuleReq_o <- max(max(AlbeniFalls() + AFIn() - AFTopVol(), 0))
  return(AFRuleReq_o)
}
AFPrelim <- function() {
  AFPrelim_o <- min(AFAvailWater(), max(AFRuleReq(), AFMinReq()))
  AFPrelim_c <<- AFPrelim_o
  return(AFPrelim_o)
}
AFCriticalCurve <- function() {
  AFCriticalCurve_o <- AFCriticalCurve_input$CRC1[week_in_year]
  return(AFCriticalCurve_o)
}
AFCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC4[week_in_year]
  }
  return(AFCriticalCurveMin_o)
}
AFAssuredRefill <- function() {
  AFAssuredRefill_o <- AFAssuredRefill_input[week_in_year, 2]
  return(AFAssuredRefill_o)
}
AF_ORC <- function() {
  AF_ORC_o <- min(max(AFAssuredRefill(), AFCriticalCurve()), AFFloodCurve())
  return(AF_ORC_o)
}

###### Albeni Falls energy ####################

AFPreEnergy <- function() {
  AFPreEnergy_o <- MWhr_per_ftAcFt * min(AFPrelim(), AFPenLimit()) * AFNetHead() * AFCombEfficiency
  return(AFPreEnergy_o)
}
AFSharedWater <- function() {
  AFSharedWater_o <- max(AlbeniFalls() + AFIn() - AFPrelim() - max(AFMinRefill(), AFCriticalCurveMin()), 0)
  return(AFSharedWater_o)
}
AFDownStreamHead <- function() {
  AFDownStreamHead_o <- BCNetHead() + BDNetHead() + TotalGCHead()
  return(AFDownStreamHead_o)
}
AFEnergyContent <- function() {
  AFEnergyContent_o <- AFSharedWater() * (AFNetHead() + AFDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(AFEnergyContent_o)
}
AF_ORCSharedWater <- function() {
  AF_ORCSharedWater_o <- max(0, AlbeniFalls() + AFIn() - AFPrelim() - AF_ORC())
  return(AF_ORCSharedWater_o)
}
AF_ORCEnergyContent <- function() {
  AF_ORCEnergyContent_o <- AF_ORCSharedWater() * (AFNetHead() + AFDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(AF_ORCEnergyContent_o)
}
AFFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
    if (TotalEnergyContent_c == 0) {
      AFFirmEngSup_o <- 0
    } else {
      AFFirmEngSup_o <- (AFEnergyContent() + AF_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
    }
  } else if (Total_ORCEnergyContent_c == 0) {
    AFFirmEngSup_o <- 0
  } else {
    AFFirmEngSup_o <- AF_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
  }
  return(AFFirmEngSup_o)
}
AFNFEnergyContent <- function() {
  AFNFEnergyContent_o <- max(0, AF_ORCEnergyContent() - AFFirmEngSup())
  return(AFNFEnergyContent_o)
}
AFNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    AFNonFirmEngSup_o <- 0
  } else {
    AFNonFirmEngSup_o <- AFNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
  }
  return(AFNonFirmEngSup_o)
}
AFFirmEngSupReq <- function() {
  AFFirmEngSupReq_o <- min(AFPenLimit(), AFFirmEngSup() / (MWhr_per_ftAcFt * (AFNetHead() + AFDownStreamHead()) * AFCombEfficiency))
  return(AFFirmEngSupReq_o)
}
AFNonFirmSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    AFNonFirmSupReq_o <- min(AFPenLimit(), (AFFirmEngSup() + AFNonFirmEngSup()) / (MWhr_per_ftAcFt * (AFNetHead() + AFDownStreamHead()) * AFCombEfficiency))
  } else {
    AFNonFirmSupReq_o <- 0
  }
  return(AFNonFirmSupReq_o)
}
AFEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    AFEnergySup_o <- max(min(AFFirmEngSupReq(), AFSharedWater()), min(AFNonFirmSupReq(), AF_ORCSharedWater()))
  } else {
    AFEnergySup_o <- max(min(AFFirmEngSupReq(), AF_ORCSharedWater()), min(AFNonFirmSupReq(), AF_ORCSharedWater()))
  }
  return(AFEnergySup_o)
}
AFCombUpSup <- function() {
  if (KECombSup_c == -9999) {
    AFCombUpSup_o <- KECombSup()
  } else {
    AFCombUpSup_o <- KECombSup_c
  }
  return(AFCombUpSup_o)
}
AFCombSup <- function() {
  AFCombSup_o <- AFEnergySup() + AFCombUpSup()
  AFCombSup_c <<- AFCombSup_o
  return(AFCombSup_o)
}
AFCombUpProtect <- function() {
  outflow <- AFPrelim() + AFCombSup_c
  AFCombUpProtect_o <- max(min(AlbeniFalls() + AFPreInflow() - outflow - AF_ORC(), KEDamProtectExcess()), 0)
  AFCombUpProtect_c <<- AFCombUpProtect_o
  return(AFCombUpProtect_o)
}
AFDamProtectExcess <- function() {
  AFDamProtectExcess_o <- max(0, AFDamProtectRel() - AFPrelim() - AFCombSup_c - AFCombUpProtect_c)
  return(AFDamProtectExcess_o)
}

#### Albeni Falls final release ########

AFRelease <- function() {
  AFRelease_o <- max(min(AFPrelim() + AFCombSup() + AFCombUpProtect(), AFRelLimit()), AFDamProtectRel())
  AFRelease_c <<- AFRelease_o
  return(AFRelease_o)
}
AFOutflow <- function() {
  AFOutflow_o <- AFRelease_c
  return(AFOutflow_o)
}
#######################################################
