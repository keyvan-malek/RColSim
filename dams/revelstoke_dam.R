#---------------- REVELSTOKE DAM ---------------------#
#######################################################

RevFlowData <- function() {
  return(max(FlowREV))
}
REVInc <- function() {
  REVInc_o <- RevFlowData() - MicaFlowData()
  return(REVInc_o)
}
REVPrelim <- function() {
  REVPrelim_o <- MIPrelim_c + REVInc()
  return(REVPrelim_o)
}
REVPenLimit <- function() {
  REVPenCap <- 90000 # (Walker-Larsen, 2017)
  REVPenLimit_o <- REVPenCap * cfsTOafw
  return(REVPenLimit_o)
}
REVNetHead <- function() {
  REVNetHead_o <- 436 # R2 Resource Consultants, Inc.
  return(REVNetHead_o)
}
REVPreEnergy <- function() {
  REVPreEnergy_o <- min(REVPrelim(), REVPenLimit()) * REVNetHead() * RevCombEfficiency * MWhr_per_ftAcFt
  return(REVPreEnergy_o)
}
REVIn <- function() {
  REVIn_o <- MIOutflow() + REVInc()
  return(REVIn_o)
}
REVOut <- function() {
  REVOut_o <- REVIn()
  return(REVOut_o)
}

#######################################################
#------------- ARROW DAM (KEENLYSIDE) ----------------#
#######################################################

ARFullPoolVol <- 7327300 # Volume corresponding to 1444 ft of elevation.  This is normal full pool.  Units acre-ft.
ARBotVol <- 227300 # Volume in AF, corresponding to 1377.9 ft elevation
ARAvgMin <- 5000 # FCOP 2003

InitAR <- function() {
  InitAR_o <- ResInitFractionFull * ARFullPoolVol
  return(InitAR_o)
}
Arrow <- function() {
  if (week_counter == 1) {
    Arrow_o <- InitAR()
  } else {
    Arrow_o <- reservoir_vol_df$ARROW[week_counter - 1]
  }
  return(Arrow_o)
}
ArrowFlowData <- function() {
  return(max(FlowAR))
}
ARInc <- function() {
  ARInc_o <- ArrowFlowData() - RevFlowData()
  return(ARInc_o)
}
ARElev_ft <- function() {
  upper_vol <- AR_elev_input$Volume[which(AR_elev_input$Volume >= Arrow())[1]]
  lower_vol <- AR_elev_input$Volume[tail(which(AR_elev_input$Volume <= Arrow())[1],1)]
  upper_el <- AR_elev_input$Elevation[which(AR_elev_input$Volume >= Arrow())[1]]
  lower_el <- AR_elev_input$Elevation[tail(which(AR_elev_input$Volume <= Arrow())[1],1)]
  if (is.na(lower_el)) {
    ARElev_ft_o <- min(AR_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    ARElev_ft_o <- max(AR_elev_input$Elevation)
  } else if (lower_el == upper_el) {
    ARElev_ft_o <- lower_el
  } else {
    ARElev_ft_o <- lower_el + (Arrow() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(ARElev_ft_o)
}
ARNetHead <- function() { # Depth of water in the reservoir above tailwater (ft)
ARTailElev <- 1367 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621041/keenlyside-dam/
ARNetHead_o <- ARElev_ft() - ARTailElev
return(ARNetHead_o)
}
ARPenLimit <- function() { # Maximum flow rate through turbines (AF/wk)
ARPenCap <- 42400 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621041/keenlyside-dam/
ARPenLimit_o <- ARPenCap * cfsTOafw
return(ARPenLimit_o)
}
ARIn <- function() {
  ARIn_o <- REVPrelim() + ARInc()
  return(ARIn_o)
}
ARPreInflow <- function() { ## Inflow to Arrow before accounting for controlled flow at The Dalles
ARPreInflow_o <- max(min(MIPrelim() + MICombSup_c, MIRelLimit()), MIDamProtectRel()) + (ArrowFlowData() - MicaFlowData())
ARPreInflow_c <<- ARPreInflow_o
return(ARPreInflow_o)
}
ARInflow <- function() {
  ARInflow_o <- REVOut() + ARInc()
  return(ARInflow_o)
}

################## Arrow max and min releases ####################################

ARTargetMin <- function() {
  ARTargetMin_o <- ARAssuredRelease_input$target[week_in_year]
  return(ARTargetMin_o)
}
ARMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      ARMinRefill_o <- max(ARAssuredRefill(), ARCriticalCurveMin())
    } else {
      ARMinRefill_o <- ARMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    ARMinRefill_o <- ARBotVol
  }
  return(ARMinRefill_o)
}
ARMinReq <- function() {
  ARMinReq_o <- min(max(Arrow() + ARIn() - max(ARMinRefill(), ARCriticalCurveMin()), ARAvgMin * cfsTOafw), ARTargetMin() * cfsTOafw)
  return(ARMinReq_o)
}
ARDamProtectRel <- function() {
  ARDamProtectRel_o <- max(0, Arrow() + ARPreInflow() - ARFullPoolVol)
  return(ARDamProtectRel_o)
}
ARRelLimit <- function() {
  ARRelLimit_o <- max(max(Arrow() + ARPreInflow_c - ARBotVol, 0))
  return(ARRelLimit_o)
}
ARAvailWater <- function() {
  ARAvailWater_o <- max(max(Arrow() + ARIn() - ARBotVol, 0))
  return(ARAvailWater_o)
}

################## Arrow rule curves ####################################

# Arrow Flood Control Curves are modified according to January 1995 SRDs which are showed in
# 2003 Columbia River Treaty Flood Control Operating Plan
# Flood evacuation is 1 October to 30 March. Refill is from 1 April to 31 July.
# Arrow has 3.6 MAF of treaty storage plus an additional 4.5 MAF of on-call storage
ARFloodCurve <- function() {
  if (DARunoffAprAug >= 111E6) { ## In case of on-call storage
    AR_CurFC_o <- ARFlood_input$ARFlood6[week_in_year]
  } else if (DARunoffAprAug >= 80E6) {
    AR_CurFC_o <- ARFlood_input$ARFlood5[week_in_year]
  } else if (DARunoffAprAug >= 75E6) {
    AR_CurFC_o <- ARFlood_input$ARFlood4[week_in_year] - (ARFlood_input$ARFlood4[week_in_year] - ARFlood_input$ARFlood5[week_in_year]) / (80E6 - 75E6) * (DARunoffAprAug - 75E6)
  } else if (DARunoffAprAug >= 70E6) {
    AR_CurFC_o <- ARFlood_input$ARFlood3[week_in_year] - (ARFlood_input$ARFlood3[week_in_year] - ARFlood_input$ARFlood4[week_in_year]) / (75E6 - 70E6) * (DARunoffAprAug - 70E6)
  } else if (DARunoffAprAug >= 65E6) {
    AR_CurFC_o <- ARFlood_input$ARFlood2[week_in_year] - (ARFlood_input$ARFlood2[week_in_year] - ARFlood_input$ARFlood3[week_in_year]) / (70E6 - 65E6) * (DARunoffAprAug - 65E6)
  } else if (DARunoffAprAug >= 64E6) {
    AR_CurFC_o <- ARFlood_input$ARFlood1[week_in_year] - (ARFlood_input$ARFlood1[week_in_year] - ARFlood_input$ARFlood2[week_in_year]) / (65E6 - 64E6) * (DARunoffAprAug - 64E6)
  } else {
    AR_CurFC_o <- ARFlood_input$ARFlood1[week_in_year]
  }
  return(AR_CurFC_o)
}
ARTopVol <- function() {
  if (TopRuleSw() == 0) {
    ARTopVol_o <- ARFloodCurve()
  } else if (TopRuleSw() == 1) {
    ARTopVol_o <- ARFullPoolVol
  } else if (TopRuleSw() == 2) {
    ARTopVol_o <- ARFlood_input$ARFlood1[week_in_year]
  } else if (TopRuleSw() == 3) {
    if (is.na(start_refill_wk)) {
      if (week_in_year < 35) {
        ARTopVol_o <- ARFloodCurve()
      } else {
        ARTopVol_o <- ARFullPoolVol
      }
    } else if (week_in_year < start_refill_wk) {
      ARTopVol_o <- ARFloodCurve()
    } else {
      ARTopVol_o <- ARFullPoolVol()
    }
  }
  return(ARTopVol_o)
}
ARRuleReq <- function() {
  ARRuleReq_o <- max(max(Arrow() + ARIn() - ARTopVol(), 0))
  return(ARRuleReq_o)
}
ARPrelim <- function() {
  ARPrelim_o <- min(ARAvailWater(), max(ARRuleReq(), ARMinReq()))
  return(ARPrelim_o)
}
ARLowerLimit <- function() {
  ARLL_o <- lower_limit_input$Arrow[week_in_year]
  return(ARLL_o)
}
ARCriticalCurve <- function() {
  ARCriticalCurve_o <- ARCriticalCurve_input$CRC1[week_in_year]
  return(ARCriticalCurve_o)
}
ARCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC4[week_in_year]
  }
  return(ARCriticalCurveMin_o)
}
ARAssuredRefill <- function() {
  ARAssuredRefill_o <- ARAssuredRefill_input[week_in_year,2]
  return(ARAssuredRefill_o)
}
ARVariableRefill <- function() {
  if (RefillSwitch() == 1) {
    ARRefillCurve_o <- ARAssuredRefill()
  } else if (RefillSwitch() == 2) {
    if (week_in_year < 28) {
      ARRefillCurve_o <- ARFullPoolVol
    } else {
      ARRefillCurve_o <- ARVariableRefillCurve
    }
  }
  return(ARRefillCurve_o)
}
AR_ORC <- function() {
  AR_ORC_o <- min(max(min(max(ARAssuredRefill(), ARCriticalCurve()), ARVariableRefill()), ARLowerLimit()), ARFloodCurve())
  return(AR_ORC_o)
}

################## Arrow fish flows ###################

ARMcNaryDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) {
      ARMcNaryDraftLimit_o <- ARBotVol
    } else if (Fish_Pool_Alternative == 1) {
      ARMcNaryDraftLimit_o <- ARFullPoolVol
    } else if (Fish_Pool_Alternative == 2) {
      ARMcNaryDraftLimit_o <- ARFullPoolVol
    } else if (Fish_Pool_Alternative == 3) {
      ARMcNaryDraftLimit_o <- ARFullPoolVol - 0.386E6
    } else if (Fish_Pool_Alternative == 4) {
      ARMcNaryDraftLimit_o <- ARFullPoolVol - 0.773E6
    } else if (Fish_Pool_Alternative == 5) {
      ARMcNaryDraftLimit_o <- ARFullPoolVol - 1.159E6
    } else {
      ARMcNaryDraftLimit_o <- ARFullPoolVol
    }
  } else if (FishOverRefillSw() == 0) {
    ARMcNaryDraftLimit_o <- AR_ORC()
  }
  return(ARMcNaryDraftLimit_o)
}
ARBONDraftLimit <- function() { ## Draft limit to meet Bonneville chum target
if (FishOverRefillSw() == 1) {
  ARBONDraftLimit_o <- ARBotVol
} else if (FishOverRefillSw() == 0) {
  if (week_in_year < 14) {
    ARBONDraftLimit_o <- ARFullPoolVol
  } else if (week_in_year < 19) {
    ARBONDraftLimit_o <- 6E6
  } else if (week_in_year < 23) {
    ARBONDraftLimit_o <- 4E6
  } else {
    ARBONDraftLimit_o <- AR_ORC()
  }
}
return(ARBONDraftLimit_o)
}
ARMcNarySharedWater <- function() {
  ARMcNarySharedWater_o <- max(0, Arrow() + ARIn() - ARPrelim() - ARMcNaryDraftLimit())
  return(ARMcNarySharedWater_o)
}
ARMcNarySup <- function() {
  if (TotalMcNarySharedWater_c == 0) {
    ARMcNarySup_o <- 0
  } else {
    ARMcNarySup_o <- min(ARMcNarySharedWater(), McNaryFlowDeficit() * ARMcNarySharedWater() / TotalMcNarySharedWater_c)
  }
  return(ARMcNarySup_o)
}
ARBONSharedWater <- function() {
  ARBONSharedWater_o <- max(0, Arrow() + ARIn() - ARPrelim() - ARBONDraftLimit())
  return(ARBONSharedWater_o)
}
ARBONSup <- function() {
  if (TotalBONSharedWater_c == 0) {
    ARBONSup_o <- 0
  } else {
    ARBONSup_o <- min(ARBONSharedWater(), BONFlowDeficit() * ARBONSharedWater() / TotalBONSharedWater_c)
  }
  return(ARBONSup_o)
}
ARFishSup <- function() { # Additional release (above preliminary release) for meeting fish flow targets
ARFishSup_o <- max(ARBONSup(), ARMcNarySup())
return(ARFishSup_o)
}

####################### Arrow energy ##############################################

ARPreEnergy <- function() {
  ARPreEnergy_o <- min(ARPrelim(), ARPenLimit()) * ARNetHead() * ARCombEfficiency * MWhr_per_ftAcFt
  return(ARPreEnergy_o)
}
ARSharedWater <- function() {
  ARSharedWater_o <- max(Arrow() + ARIn() - ARPrelim() - ARFishSup() - max(ARMinRefill(), ARCriticalCurveMin()), 0)
  return(ARSharedWater_o)
}
AREnergyContent <- function() {
  AREnergyContent_o <- ARSharedWater() * (ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(AREnergyContent_o)
}
AR_ORCSharedWater <- function() {
  AR_ORCSharedWater_o <- max(0, Arrow() + ARIn() - ARFishSup() - ARPrelim() - AR_ORC())
  return(AR_ORCSharedWater_o)
}
AR_ORCEnergyContent <- function() {
  AR_ORCEnergyContent_o <- AR_ORCSharedWater() * (ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(AR_ORCEnergyContent_o)
}
ARFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    # Handle NA values in global variables
    TotalEnergyContent_c_safe <- ifelse(is.na(TotalEnergyContent_c), 0, TotalEnergyContent_c)
    Total_ORCEnergyContent_c_safe <- ifelse(is.na(Total_ORCEnergyContent_c), 0, Total_ORCEnergyContent_c)
    FirmEnergyDeficit_c_safe <- ifelse(is.na(FirmEnergyDeficit_c), 0, FirmEnergyDeficit_c)

    if (TotalEnergyContent_c_safe == 0) {
      ARFirmEngSup_o <- 0
    } else {
      ARFirmEngSup_o <- (AREnergyContent() + AR_ORCEnergyContent()) / (TotalEnergyContent_c_safe + Total_ORCEnergyContent_c_safe) * FirmEnergyDeficit_c_safe
    }
  } else if (Total_ORCEnergyContent_c_safe == 0) {
    ARFirmEngSup_o <- 0
  } else {
    ARFirmEngSup_o <- AR_ORCEnergyContent() / Total_ORCEnergyContent_c_safe * FirmEnergyDeficit_c_safe
  }
  return(ARFirmEngSup_o)
}
ARNFEnergyContent <- function() {
  ARNFEnergyContent_o <- max(0, AR_ORCEnergyContent() - ARFirmEngSup())
  return(ARNFEnergyContent_o)
}
ARNonFirmEngSup <- function() { # MWhr
if (TotalNFEnergyContent_c == 0) {
  ARNonFirmEngSup_o <- 0
} else {
  ARNonFirmEngSup_o <- ARNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
}
return(ARNonFirmEngSup_o)
}
ARFirmEngSupReq <- function() {
  ARFirmEngSupReq_o <- min(ARPenLimit(), ARFirmEngSup() / (MWhr_per_ftAcFt * (ARNetHead() + TotalGCHead()) * ARCombEfficiency))
  return(ARFirmEngSupReq_o)
}
ARNonFirmSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    ARNonFirmSupReq_o <- min(ARPenLimit(), (ARFirmEngSup() + ARNonFirmEngSup()) / (MWhr_per_ftAcFt * TotalGCHead() * ARCombEfficiency))
  } else {
    ARNonFirmSupReq_o <- 0
  }
  return(ARNonFirmSupReq_o)
}
AREnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    AREnergySup_o <- max(min(ARFirmEngSupReq(), ARSharedWater()), min(ARNonFirmSupReq(), AR_ORCSharedWater()))
  } else {
    AREnergySup_o <- max(min(ARFirmEngSupReq(), AR_ORCSharedWater()), min(ARNonFirmSupReq(), AR_ORCSharedWater()))
  }
  return(AREnergySup_o)
}
ARCombUpSup <- function() { # Release of water upstream of Arrow to meet fish and energy objectives
if (MICombSup_c == -9999) {
  ARCombUpSup_1 <- MICombSup()
} else {
  ARCombUpSup_1 <- MICombSup_c
}
if (refill_cat4 == 0) { # Do not use any of the water released upstream of Arrow to refill to lower rule curve
  ARCombUpSup_o <- ARCombUpSup_1
} else if (refill_cat4 == 1) { # Allow some of the water to be used to refill reservoir to the minimum draft point
inflow <- ARIn() + ARCombUpSup_1
outflow <- ARPrelim() + AREnergySup() + ARFishSup()
min_draft <- min(max(ARMinRefill(), ARCriticalCurveMin()), ARBONDraftLimit(), AR_ORC())
ARCombUpSup_o <- min(max(0, Arrow() + inflow - outflow - min_draft), ARCombUpSup_1)
} else if (refill_cat4 == 2) { # Allow some of the water to be used to refill reservoir to the ORC
inflow <- ARIn() + ARCombUpSup_1
outflow <- ARPrelim() + AREnergySup() + ARFishSup()
ARCombUpSup_o <- min(max(0, Arrow() + inflow - outflow - AR_ORC()), ARCombUpSup_1)
}
return(ARCombUpSup_o)
}
ARCombSup <- function() {
  ARCombSup_o <- ARCombUpSup() + AREnergySup() + ARFishSup()
  ARCombSup_c <<- ARCombSup_o
  return(ARCombSup_o)
}
ARFishSupEnergy <- function() {
  ARFishSupEnergy_o <- ARFishSup() * (ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(ARFishSupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ###############################

ARFloodSpace <- function() {
  ARFloodSpace_o <- min(ARPrelim() + ARCombSup(), max(0, ARFullPoolVol - (Arrow() + ARIn() - ARPrelim() - ARCombSup())))
  ARFloodSpace_c <<- ARFloodSpace_o
  return(ARFloodSpace_o)
}
ARFloodRelSharedWater <- function() { # Water that can be released to prevent the dam from refilling too quickly during refill period
ARFloodRelSharedWater_o <- max(0, Arrow() + ARPreInflow_c - ARPrelim() - ARCombSup() - AR_ORC())
return(ARFloodRelSharedWater_o)
}
ARFloodFrac <- function() {
  if (TotalFloodSpace_c == 0) {
    ARFloodFrac_o <- 0
  } else {
    ARFloodFrac_o <- (MIFloodMult * MIFloodSpace_c + ARFloodSpace_c) / TotalFloodSpace_c
  }
  return(ARFloodFrac_o)
}
ARFloodRelFrac <- function() {
  if (TotalFloodRelSharedWater_c == -9999) {
    TotalFloodRelSharedWater_c <<- TotalFloodRelSharedWater()
    water_df$TotalFloodRelSharedWater[week_counter] <<- TotalFloodRelSharedWater_c
  }
  if (TotalFloodRelSharedWater_c == 0) {
    ARFloodRelFrac_o <- 0
  } else {
    ARFloodRelFrac_o <- ARFloodRelSharedWater() / TotalFloodRelSharedWater_c
  }
  return(ARFloodRelFrac_o)
}
ARRelReducReq <- function() {
  ARRelReducReq_o <- TotalRelReducReq_c * ARFloodFrac()
  ARRelReducReq_c <<- ARRelReducReq_o
  return(ARRelReducReq_o)
}
ARMinFloodRelReq <- function() { ## Released during refill period to ensure the reservoir does not refill too quickly, based on initial controlled flow
ARMinFloodRelReq_o <- min(ARFloodRelSharedWater(), MinFloodRelReq() * ARFloodRelFrac())
ARMinFloodRelReq_c <<- ARMinFloodRelReq_o
if (is.na(water_df$ARMinFloodRelReq[week_counter])) {
  water_df$ARMinFloodRelReq[week_counter] <<- ARMinFloodRelReq_c
}
return(ARMinFloodRelReq_o)
}
ARCombUpProtect <- function() { ## Release of water from upstream dams to prevent overflow
outflow <- ARPrelim() + ARCombSup_c
ARCombUpProtect_o <- max(min(MIDamProtectExcess(), Arrow() + ARPreInflow() - outflow - AR_ORC()), 0)
ARCombUpProtect_c <<- ARCombUpProtect_o
return(ARCombUpProtect_o)
}
ARDamProtectExcess <- function() {
  ARDamProtectExcess_o <- max(0, ARDamProtectRel() - ARPrelim() - ARCombSup_c - ARCombUpProtect())
  return(ARDamProtectExcess_o)
}

################### Arrow final release ###############################

ARRelease <- function() {
  ARRelease_o <- max(min(ARPrelim() + ARCombSup_c - ARRelReducReq() + ARMinFloodRelReq() + ARCombUpProtect_c, ARRelLimit()), ARDamProtectRel(), ARAvgMin * cfsTOafw)
  return(ARRelease_o)
}
AROutflow <- function() {
  AROutflow_o <- ARRelease_c
  return(AROutflow_o)
}

#######################################################
