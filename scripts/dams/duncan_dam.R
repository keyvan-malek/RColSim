# Duncan_Dam.R
# Reservoir operations functions for Duncan
# Handles inflow/outflow calculations and reservoir management operations

#-------------------- DUNCAN DAM ---------------------#
#######################################################

DUFullPoolVol <- 1424900 # Storage volume at 1892 ft. pool elevation
DUBotVol <- 25000 # Storage at 1794.2 ft. pool elevation (1.4 MAF active storage)
DUAvgMin <- 100 # FCOP 2003

InitDU <- function() {
  InitDU_o <- ResInitFractionFull * DUFullPoolVol
  return(InitDU_o)
}
Duncan <- function() {
  if (week_counter == 1) {
    Duncan_o <- InitDU()
  } else {
    Duncan_o <- reservoir_vol_df$DUNCA[week_counter - 1]
  }
  return(Duncan_o)
}
DuncanFlowData <- function() {
  return(max(FlowDU))
}
DUIn <- function() {
  DUIn_o <- DuncanFlowData()
  return(DUIn_o)
}
DUInflow <- function() {
  DUInflow_o <- DUIn()
  return(DUInflow_o)
}

################## Duncan max and min releases ##################################

DUMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      DUMinRefill_o <- max(DUAssuredRefill(), DUCriticalCurveMin())
    } else {
      DUMinRefill_o <- DUMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    DUMinRefill_o <- DUBotVol
  }
  return(DUMinRefill_o)
}
DUMinReq <- function() {
  DURefillMin_o <- DUAvgMin * cfsTOafw
  return(DURefillMin_o)
}
DUDamProtectRel <- function() {
  DUDamProtectRel_o <- max(max(Duncan() + DUInflow() - DUFullPoolVol, 0))
  return(DUDamProtectRel_o)
}
DURelLimit <- function() {
  DURelLimit_o <- max(max(Duncan() + DUInflow() - DUBotVol, 0))
  return(DURelLimit_o)
}
DUAvailWater <- function() {
  DUAvailWater_o <- max(max(Duncan() + DUIn() - DUBotVol, 0))
  return(DUAvailWater_o)
}

################### Duncan rule curves #########################

# Flood evacuation period is 1 December to 29 February
DUFloodCurve <- function() {
  if (DARunoffAprAug > 111E6) {
    DU_CurFC_o <- DUFlood_input$DUFlood5[week_in_year]
  } else if (DURunoffAprAug < 1.4E6) {
    DU_CurFC_o <- DUFlood_input$DUFlood1[week_in_year]
  } else if (DURunoffAprAug < 1.6E6) {
    DU_CurFC_o <- DUFlood_input$DUFlood1[week_in_year] - (DUFlood_input$DUFlood1[week_in_year] - DUFlood_input$DUFlood2[week_in_year]) / (1.6E6 - 1.4E6) * (DURunoffAprAug - 1.4E6)
  } else if (DURunoffAprAug < 1.8E6) {
    DU_CurFC_o <- DUFlood_input$DUFlood2[week_in_year] - (DUFlood_input$DUFlood2[week_in_year] - DUFlood_input$DUFlood3[week_in_year]) / (1.8E6 - 1.6E6) * (DURunoffAprAug - 1.6E6)
  } else if (DURunoffAprAug < 2.0E6) {
    DU_CurFC_o <- DUFlood_input$DUFlood3[week_in_year] - (DUFlood_input$DUFlood3[week_in_year] - DUFlood_input$DUFlood4[week_in_year]) / (2.0E6 - 1.8E6) * (DURunoffAprAug - 1.8E6)
  } else {
    DU_CurFC_o <- DUFlood_input$DUFlood4[week_in_year]
  }
  return(DU_CurFC_o)
}
DUTopVol <- function() {
  if (TopRuleSw() == 0) {
    DUTopVol_o <- DUFloodCurve()
  } else if (TopRuleSw() == 1) {
    DUTopVol_o <- DUFullPoolVol
  } else if (TopRuleSw() == 2) {
    DUTopVol_o <- DUFlood_input$DUFlood1[week_in_year]
  } else if (TopRuleSw() == 3) {
    if (is.na(start_refill_wk)) {
      if (week_in_year < 35) {
        DUTopVol_o <- DUFloodCurve()
      } else {
        DUTopVol_o <- DUFullPoolVol
      }
    } else if (week_in_year < start_refill_wk) {
      DUTopVol_o <- DUFloodCurve()
    } else {
      DUTopVol_o <- DUFullPoolVol()
    }
  }
  return(DUTopVol_o)
}
DURuleReq <- function() {
  DURuleReq_o <- max(max(Duncan() + DUIn() - DUTopVol(), 0))
  return(DURuleReq_o)
}
DUPrelim <- function() {
  DUPrelim_o <- min(DUAvailWater(), max(DURuleReq(), DUMinReq()))
  return(DUPrelim_o)
}
DULowerLimit <- function() {
  DULL_o <- lower_limit_input$Duncan[week_in_year]
  return(DULL_o)
}
DUCriticalCurve <- function() {
  DUCriticalCurve_o <- DUCriticalCurve_input$CRC1[week_in_year]
  return(DUCriticalCurve_o)
}
DUCriticalCurveMin <- function() {
  if (DUCriticalCurveSw == 1) {
    DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC1[week_in_year]
  } else if (DUCriticalCurveSw == 2) {
    DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC2[week_in_year]
  } else if (DUCriticalCurveSw == 3) {
    DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC3[week_in_year]
  } else if (DUCriticalCurveSw == 4) {
    DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC4[week_in_year]
  }
  return(DUCriticalCurveMin_o)
}
DUAssuredRefill <- function() {
  DUAssuredRefill_o <- DUAssuredRefill_input[week_in_year, 2]
  return(DUAssuredRefill_o)
}
DUVariableRefill <- function() {
  if (RefillSwitch() == 1) {
    DURefillCurve_o <- DUAssuredRefill()
  } else if (RefillSwitch() == 2) {
    DURefillCurve_o <- DUVariableRefillCurve
  }
  return(DURefillCurve_o)
}
DU_ORC <- function() {
  DU_ORC_o <- min(max(min(max(DUAssuredRefill(), DUCriticalCurve()), DUVariableRefill()), DULowerLimit()), DUFloodCurve())
  return(DU_ORC_o)
}

##################### Duncan fish flow ########################################################

DUMcNaryDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) { # Default = 0
      DUMcNaryDraftLimit_o <- DUBotVol
    } else if (Fish_Pool_Alternative == 1 || Fish_Pool_Alternative == 2) {
      DUMcNaryDraftLimit_o <- DUFullPoolVol
    } else if (Fish_Pool_Alternative == 3) {
      DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.232E6
    } else if (Fish_Pool_Alternative == 4) {
      DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.464E6
    } else if (Fish_Pool_Alternative == 5) {
      DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.695E6
    } else {
      DUMcNaryDraftLimit_o <- DUFullPoolVol
    }
  } else {
    DUMcNaryDraftLimit_o <- DU_ORC()
  }
  return(DUMcNaryDraftLimit_o)
}
DUMcNarySharedWater <- function() {
  DUMcNarySharedWater_o <- max(0, Duncan() + DUIn() - DUPrelim() - DUMcNaryDraftLimit())
  return(DUMcNarySharedWater_o)
}
DUMcNarySup <- function() {
  if (TotalMcNarySharedWater_c == 0) {
    DUMcNarySup_o <- 0
  } else {
    DUMcNarySup_o <- min(DUMcNarySharedWater(), McNaryFlowDeficit() * DUMcNarySharedWater() / TotalMcNarySharedWater_c)
  }
  return(DUMcNarySup_o)
}

########### Duncan energy ##################################

# Duncan does not have any generators, but it still releases water for hydropower generation at downstream dams

DUMcNarySupEnergy <- function() {
  DUMcNarySupEnergy_o <- DUMcNarySup() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
  return(DUMcNarySupEnergy_o)
}
DUSharedWater <- function() {
  DUSharedWater_o <- max(Duncan() + DUIn() - DUPrelim() - DUMcNarySup() - max(DUMinRefill(), DUCriticalCurveMin()), 0)
  return(DUSharedWater_o)
}
DUEnergyContent <- function() {
  DUEnergyContent_o <- DUSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
  return(DUEnergyContent_o)
}
DU_ORCSharedWater <- function() {
  DU_ORCSharedWater_o <- max(0, Duncan() + DUIn() - DUPrelim() - DUMcNarySup() - DU_ORC())
  return(DU_ORCSharedWater_o)
}
DU_ORCEnergyContent <- function() {
  DU_ORCEnergyContent_o <- DU_ORCSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
  return(DU_ORCEnergyContent_o)
}
DUFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    # Handle NA values in global variables
    TotalEnergyContent_c_safe <- ifelse(is.na(TotalEnergyContent_c), 0, TotalEnergyContent_c)
    Total_ORCEnergyContent_c_safe <- ifelse(is.na(Total_ORCEnergyContent_c), 0, Total_ORCEnergyContent_c)
    FirmEnergyDeficit_c_safe <- ifelse(is.na(FirmEnergyDeficit_c), 0, FirmEnergyDeficit_c)

    if (TotalEnergyContent_c_safe == 0) {
      DUFirmEngSup_o <- 0
    } else {
      DUFirmEngSup_o <- (DUEnergyContent() + DU_ORCEnergyContent()) / (TotalEnergyContent_c_safe + Total_ORCEnergyContent_c_safe) * FirmEnergyDeficit_c_safe
    }
  } else if (Total_ORCEnergyContent_c_safe == 0) {
    DUFirmEngSup_o <- 0
  } else {
    DUFirmEngSup_o <- DU_ORCEnergyContent() / Total_ORCEnergyContent_c_safe * FirmEnergyDeficit_c_safe
  }
  return(DUFirmEngSup_o)
}
DUNFEnergyContent <- function() {
  DUNFEnergyContent_o <- max(0, DU_ORCEnergyContent() - DUFirmEngSup())
  return(DUNFEnergyContent_o)
}
DUNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    DUNonFirmEngSup_o <- 0
  } else {
    DUNonFirmEngSup_o <- DUNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
  }
  return(DUNonFirmEngSup_o)
}
DUFirmEngSupReq <- function() {
  DUFirmEngSupReq_o <- DUFirmEngSup() / (MWhr_per_ftAcFt * TotalGCHead() * DUCombEfficiency)
  return(DUFirmEngSupReq_o)
}
DUNonFirmSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    DUNonFirmSupReq_o <- (DUFirmEngSup() + DUNonFirmEngSup()) / (MWhr_per_ftAcFt * TotalGCHead() * DUCombEfficiency)
  } else {
    DUNonFirmSupReq_o <- 0
  }
  return(DUNonFirmSupReq_o)
}
DUEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    DUEnergySup_o <- max(min(DUFirmEngSupReq(), DUSharedWater()), min(DUNonFirmSupReq(), DU_ORCSharedWater()))
  } else {
    DUEnergySup_o <- max(min(DUFirmEngSupReq(), DU_ORCSharedWater()), min(DUNonFirmSupReq(), DU_ORCSharedWater()))
  }
  return(DUEnergySup_o)
}
DUCombSup <- function() {
  DUCombSup_o <- DUEnergySup() + DUMcNarySup()
  DUCombSup_c <<- DUCombSup_o
  return(DUCombSup_o)
}
DUDamProtectExcess <- function() {
  DUDamProtectExcess_o <- max(0, DUDamProtectRel() - DUPrelim() - DUCombSup_c)
  return(DUDamProtectExcess_o)
}

################### Duncan final release ###############################

DURelease <- function() {
  DURelease_o <- max(min(DUPrelim() + DUCombSup(), DURelLimit()), DUDamProtectRel())
  DURelease_c <<- DURelease_o
  return(DURelease_o)
}
DUOutflow <- function() {
  DUOutflow_o <- DURelease_c
  return(DUOutflow_o)
}

#######################################################
