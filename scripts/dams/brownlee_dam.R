# Brownlee_Dam.R
# Reservoir operations functions for Brownlee
# Handles inflow/outflow calculations and reservoir management operations

#------------------- BROWNLEE DAM --------------------#
#######################################################

BRFullPoolVol <- 1420000 # Volume corresponding to 2077 ft of elevation.  Normal full pool.  Units acre-ft.
BRBotVol <- 444700 # Volume cooresponding to the bottom of conservation pool elevation of 1976 ft.  Units acre-ft.
BRAvgMin <- 5850 # 2016-2017 assured operating plan

InitBR <- function() {
  InitBR_o <- ResInitFractionFull * BRFullPoolVol
  return(InitBR_o)
}
Brownlee <- function() {
  if (week_counter == 1) {
    Brownlee_o <- InitBR()
  } else {
    Brownlee_o <- reservoir_vol_df$BROWN[week_counter-1]
  }
  return(Brownlee_o)
}
BrownleeFlowData <- function() {
  return(max(FlowBR))
}
BRInc <- function() {
  BRInc_o <- BrownleeFlowData() - MilnerFlowData() - LuckyPeakFlowData() - PayetteFlowData() - OwyheeFlowData()
  return(BRInc_o)
}
BRElev_ft <- function() {
  upper_vol <- BR_elev_input$Volume[which(BR_elev_input$Volume >= Brownlee())[1]]
  lower_vol <- BR_elev_input$Volume[tail(which(BR_elev_input$Volume <= Brownlee())[1],1)]
  upper_el <- BR_elev_input$Elevation[which(BR_elev_input$Volume >= Brownlee())[1]]
  lower_el <- BR_elev_input$Elevation[tail(which(BR_elev_input$Volume <= Brownlee())[1],1)]
  if (is.na(lower_el)) {
    BRElev_ft_o <- min(BR_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    BRElev_ft_o <- max(BR_elev_input$Elevation)
  } else if (lower_el == upper_el) {
    BRElev_ft_o <- lower_el
  } else {
    BRElev_ft_o <- lower_el + (Brownlee() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(BRElev_ft_o)
}
BRNetHead <- function() {
  BRTailElev <- 1805 # https://storymaps.arcgis.com/stories/eae485d9d4244bd4bf93c537bd449ae7
  BRNetHead_o <- BRElev_ft() - BRTailElev
  return(BRNetHead_o)
}
BRPenLimit <- function() {
  BRPenCap <- 34500 # FERC No. 1971 License application
  BRPenLimit_o <- BRPenCap * cfsTOafw
  return(BRPenLimit_o)
}
BRIn <- function() {
  BRIn_o <- PayettePrelim() + OWYPrelim() + BoisePrelim() + MILPrelim() + BRInc()
  return(BRIn_o)
}
BRInflow <- function() {
  BRInflow_o <- PayetteRelease() + OWYRelease() + BoiseRelease() + MILOut() + BRInc()
  return(BRInflow_o)
}

################## Brownlee max and min releases ####################################

BRMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      BRMinRefill_o <- max(BRAssuredRefill(), BRCriticalCurveMin())
    } else {
      BRMinRefill_o <- BRMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    BRMinRefill_o <- BRBotVol
  }
  return(BRMinRefill_o)
}
BRMinReq <- function() {
  BRMinReq_1 <- max(BRRelForJBandLP(), BRAvgMin * cfsTOafw)
  if (FishOverRefillSw() == 1) {
    BRMinReq_o <- BRMinReq_1
  } else {
    BRMinReq_o <- min(max(Brownlee() + BRIn_c - BRMinRefill(), BRAvgMin * cfsTOafw), BRMinReq_1)
  }
  return(BRMinReq_o)
}
BRDamProtectRel <- function() {
  BRDamProt <- max(0, Brownlee() + BRInflow() - BRFullPoolVol)
  return(BRDamProt)
}
BRRelLimit <- function() { # Max allowable release
BRRelLimit_o <- max(0, Brownlee() + BRInflow() - BRBotVol)
return(BRRelLimit_o)
}
BRAvailWater <- function() {
  BRAvailWater_o <- max(0, Brownlee() + BRIn_c - BRBotVol)
  return(BRAvailWater_o)
}

################### Brownlee rule curves ##################################

### Flood curve for Browlee reservoir is based on the April--August forecast at The Dalles and the April--July forecast at Brownlee
### (Hells Canyon Complex FERC No. 1971 License Application)
BRFloodCurve <- function() {
  if (DARunoffAprAug <= 75E6) {
    if (BRRunoffAprJul <= 3E6) {
      BRFlood_o <- BRFlood_input$BR75_3[week_in_year]
    } else if (BRRunoffAprJul <= 4E6) {
      BRFlood_o <- BRFlood_input$BR75_3[week_in_year] - (BRFlood_input$BR75_3[week_in_year] - BRFlood_input$BR75_4[week_in_year]) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
    } else if (BRRunoffAprJul <= 5E6) {
      BRFlood_o <- BRFlood_input$BR75_4[week_in_year] - (BRFlood_input$BR75_4[week_in_year] - BRFlood_input$BR75_5[week_in_year]) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
    } else if (BRRunoffAprJul <= 6E6) {
      BRFlood_o <- BRFlood_input$BR75_5[week_in_year] - (BRFlood_input$BR75_5[week_in_year] - BRFlood_input$BR75_6[week_in_year]) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
    } else {
      BRFlood_o <- BRFlood_input$BR75_6[week_in_year]
    }
  } else if (DARunoffAprAug <= 85E6) {
    if (BRRunoffAprJul <= 3E6) {
      BRFlood_o <- BRFlood_input$BR75_3[week_in_year] - (BRFlood_input$BR75_3[week_in_year] - BRFlood_input$BR85_3[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
    } else if (BRRunoffAprJul <= 4E6) {
      BRFlood_1 <- BRFlood_input$BR75_3[week_in_year] - (BRFlood_input$BR75_3[week_in_year] - BRFlood_input$BR85_3[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
      BRFlood_2 <- BRFlood_input$BR75_4[week_in_year] - (BRFlood_input$BR75_4[week_in_year] - BRFlood_input$BR85_4[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
    } else if (BRRunoffAprJul <= 5E6) {
      BRFlood_1 <- BRFlood_input$BR75_4[week_in_year] - (BRFlood_input$BR75_4[week_in_year] - BRFlood_input$BR85_4[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
      BRFlood_2 <- BRFlood_input$BR75_5[week_in_year] - (BRFlood_input$BR75_5[week_in_year] - BRFlood_input$BR85_5[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
    } else if (BRRunoffAprJul <= 6E6) {
      BRFlood_1 <- BRFlood_input$BR75_5[week_in_year] - (BRFlood_input$BR75_5[week_in_year] - BRFlood_input$BR85_5[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
      BRFlood_2 <- BRFlood_input$BR75_6[week_in_year] - (BRFlood_input$BR75_6[week_in_year] - BRFlood_input$BR85_6[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
    } else {
      BRFlood_o <- BRFlood_input$BR75_6[week_in_year] - (BRFlood_input$BR75_6[week_in_year] - BRFlood_input$BR85_6[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
    }
  } else if (DARunoffAprAug <= 95E6) {
    if (BRRunoffAprJul <= 3E6) {
      BRFlood_o <- BRFlood_input$BR85_3[week_in_year] - (BRFlood_input$BR85_3[week_in_year] - BRFlood_input$BR95_3[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
    } else if (BRRunoffAprJul <= 4E6) {
      BRFlood_1 <- BRFlood_input$BR85_3[week_in_year] - (BRFlood_input$BR85_3[week_in_year] - BRFlood_input$BR95_3[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
      BRFlood_2 <- BRFlood_input$BR85_4[week_in_year] - (BRFlood_input$BR85_4[week_in_year] - BRFlood_input$BR95_4[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
    } else if (BRRunoffAprJul <= 5E6) {
      BRFlood_1 <- BRFlood_input$BR85_4[week_in_year] - (BRFlood_input$BR85_4[week_in_year] - BRFlood_input$BR95_4[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
      BRFlood_2 <- BRFlood_input$BR85_5[week_in_year] - (BRFlood_input$BR85_5[week_in_year] - BRFlood_input$BR95_5[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
    } else if (BRRunoffAprJul <= 6E6) {
      BRFlood_1 <- BRFlood_input$BR85_5[week_in_year] - (BRFlood_input$BR85_5[week_in_year] - BRFlood_input$BR95_5[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
      BRFlood_2 <- BRFlood_input$BR85_6[week_in_year] - (BRFlood_input$BR85_6[week_in_year] - BRFlood_input$BR95_6[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
    } else {
      BRFlood_o <- BRFlood_input$BR85_6[week_in_year] - (BRFlood_input$BR85_6[week_in_year] - BRFlood_input$BR95_6[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
    }
  } else if (DARunoffAprAug <= 105E6) {
    if (BRRunoffAprJul <= 3E6) {
      BRFlood_o <- BRFlood_input$BR95_3[week_in_year] - (BRFlood_input$BR95_3[week_in_year] - BRFlood_input$BR105_3[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
    } else if (BRRunoffAprJul <= 4E6) {
      BRFlood_1 <- BRFlood_input$BR95_3[week_in_year] - (BRFlood_input$BR95_3[week_in_year] - BRFlood_input$BR105_3[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
      BRFlood_2 <- BRFlood_input$BR95_4[week_in_year] - (BRFlood_input$BR95_4[week_in_year] - BRFlood_input$BR105_4[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
    } else if (BRRunoffAprJul <= 5E6) {
      BRFlood_1 <- BRFlood_input$BR95_4[week_in_year] - (BRFlood_input$BR95_4[week_in_year] - BRFlood_input$BR105_4[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
      BRFlood_2 <- BRFlood_input$BR95_5[week_in_year] - (BRFlood_input$BR95_5[week_in_year] - BRFlood_input$BR105_5[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
    } else if (BRRunoffAprJul <= 6E6) {
      BRFlood_1 <- BRFlood_input$BR95_5[week_in_year] - (BRFlood_input$BR95_5[week_in_year] - BRFlood_input$BR105_5[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
      BRFlood_2 <- BRFlood_input$BR95_6[week_in_year] - (BRFlood_input$BR95_6[week_in_year] - BRFlood_input$BR105_6[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
    } else {
      BRFlood_o <- BRFlood_input$BR95_6[week_in_year] - (BRFlood_input$BR95_6[week_in_year] - BRFlood_input$BR105_6[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
    }
  } else if (DARunoffAprAug <= 115E6) {
    if (BRRunoffAprJul <= 3E6) {
      BRFlood_o <- BRFlood_input$BR105_3[week_in_year] - (BRFlood_input$BR105_3[week_in_year] - BRFlood_input$BR115_3[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
    } else if (BRRunoffAprJul <= 4E6) {
      BRFlood_1 <- BRFlood_input$BR105_3[week_in_year] - (BRFlood_input$BR105_3[week_in_year] - BRFlood_input$BR115_3[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
      BRFlood_2 <- BRFlood_input$BR105_4[week_in_year] - (BRFlood_input$BR105_4[week_in_year] - BRFlood_input$BR115_4[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
    } else if (BRRunoffAprJul <= 5E6) {
      BRFlood_1 <- BRFlood_input$BR105_4[week_in_year] - (BRFlood_input$BR105_4[week_in_year] - BRFlood_input$BR115_4[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
      BRFlood_2 <- BRFlood_input$BR105_5[week_in_year] - (BRFlood_input$BR105_5[week_in_year] - BRFlood_input$BR115_5[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
    } else if (BRRunoffAprJul <= 6E6) {
      BRFlood_1 <- BRFlood_input$BR105_5[week_in_year] - (BRFlood_input$BR105_5[week_in_year] - BRFlood_input$BR115_5[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
      BRFlood_2 <- BRFlood_input$BR105_6[week_in_year] - (BRFlood_input$BR105_6[week_in_year] - BRFlood_input$BR115_6[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
      BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
    } else {
      BRFlood_o <- BRFlood_input$BR105_6[week_in_year] - (BRFlood_input$BR105_6[week_in_year] - BRFlood_input$BR115_6[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
    }
  } else {
    if (BRRunoffAprJul <= 3E6) {
      BRFlood_o <- BRFlood_input$BR115_3[week_in_year]
    } else if (BRRunoffAprJul <= 4E6) {
      BRFlood_o <- BRFlood_input$BR115_3[week_in_year] - (BRFlood_input$BR115_3[week_in_year] - BRFlood_input$BR115_4[week_in_year]) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
    } else if (BRRunoffAprJul <= 5E6) {
      BRFlood_o <- BRFlood_input$BR115_4[week_in_year] - (BRFlood_input$BR115_4[week_in_year] - BRFlood_input$BR115_5[week_in_year]) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
    } else if (BRRunoffAprJul <= 6E6) {
      BRFlood_o <- BRFlood_input$BR115_5[week_in_year] - (BRFlood_input$BR115_5[week_in_year] - BRFlood_input$BR115_6[week_in_year]) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
    } else {
      BRFlood_o <- BRFlood_input$BR115_6[week_in_year]
    }
  }
  return(BRFlood_o)
}
BRTopVol <- function() {
  if (TopRuleSw() == 0) {
    BRTopVol_o <- min(BRFloodCurve(), BRFallChinookDraft())
  } else if (TopRuleSw() == 1) {
    BRTopVol_o <- BRFullPoolVol
  } else if (TopRuleSw() == 2) {
    BRTopVol_o <- BRFlood_input$BR75_3[week_in_year]
  }
  return(BRTopVol_o)
}
BRRuleReq <- function() {
  BRRuleReq_o <- max(max(Brownlee() + BRIn() - BRTopVol(), 0))
  return(BRRuleReq_o)
}
BRPrelim <- function() {
  BRPrelim_o <- min(BRAvailWater(), max(BRRuleReq(), BRMinReq()))
  return(BRPrelim_o)
}
BRCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    BRCriticalCurve_o <- BRCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    BRCriticalCurve_o <- BRCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    BRCriticalCurve_o <- BRCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    BRCriticalCurve_o <- BRCriticalCurve_input$CRC4[week_in_year]
  }
  return(BRCriticalCurve_o)
}
BRCriticalCurve <- function() { # Minimum storage to meet firm hydropower in a dry year (1928-1932 critical water period)
BRCriticalCurve_o <- BRCriticalCurve_input$CRC1[week_in_year] # year 1 critical curve
return(BRCriticalCurve_o)
}
BRAssuredRefill <- function() { # Assured refill curve based on historical Assured refill
BRAssuredRefill_o <- BRAssuredRefill_input[week_in_year,2]
return(BRAssuredRefill_o)
}
BRVariableRefill <- function() { # Required refill to ensure dam is full by end of year (AF)
if (RefillSwitch() == 1) { # Default = 2
  BRRefillCurve_o <- BRAssuredRefill()
} else if (RefillSwitch() == 2) {
  BRRefillCurve_o <- BRVariableRefillCurve
}
return(BRRefillCurve_o)
}
BR_ORC <- function() {
  BR_ORC_o <- min(max(BRAssuredRefill(), BRCriticalCurve()), BRFloodCurve(), BRFallChinookDraft())
  return(BR_ORC_o)
}

########################### Brownlee fish flows ############################################

BRLGDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) { # Default = 0
      BRLGDraftLimit_o <- BRBotVol
    } else {
      BRLGDraftLimit_o <- 1.183e6
    }
  } else if (FishOverRefillSw() == 0) {
    BRLGDraftLimit_o <- BR_ORC()
  }
  return(BRLGDraftLimit_o)
}
BRJBDraftLimit <- function() {
  #if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) { # Default = 0
      BRJBDraftLimit_o <- BRBotVol
    } else {
      BRJBDraftLimit_o <- 1.0e6
    }
    #} else if (FishOverRefillSw() == 0) {
      #  BRJBDraftLimit_o <- BR_ORC()
      #}
      return(BRJBDraftLimit_o)
    }
    BRLGAvailWater <- function() {
      BRLGAvailWater_o <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGDraftLimit())
      return(BRLGAvailWater_o)
    }
    BRJBAvailWater <- function() {
      BRJBAvailWater_o <- max(0, Brownlee() + BRIn_c - BRJBDraftLimit())
      return(BRJBAvailWater_o)
    }
    BRLGSup <- function() { # Release of water from Brownlee to meet Lower Granite fish flow target
    denominator <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGDraftLimit()) +
    max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
    numerator <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGDraftLimit())
    if (denominator == 0) {
      BRRelForLG_1 <- 0
    } else {
      BRRelForLG_1 <- TotalRelForLowerGranite() * numerator / denominator
    }
    BRRelForLG_o <- min(BRLGAvailWater(), BRRelForLG_1)
    water_df$BRLGSup[week_counter] <<- BRRelForLG_o
    return(BRRelForLG_o)
  }

  ##### LimePointFlowTarget
  # Minimum Flows at the Lime Point (75 miles below Hells Canyon) are required to be at least 13000 cfs 95% of the time (FERC License No. 1971, Article 43) July-Sept.
  # The model assumes these flows must be maintained 100% of the time.
  # In addition a minimum flow of 5000 cfs at Johnson's Bar (17 miles downstream of Hells Canyon) must be maintained.
  # While in actual practice some storage is available for support of these flows from run-of-river projects,
  # Brownlee must generally make sufficient average releases to support these flows on a monthly time frame.
  # The model assumes that Brownlee supplies all supplements to natural inflow required to meet the targets.
  JohnsonBarFlowTarget <- function() {
    JohnsonBarFlowTarget_o <- 5000 # cfs
    return(JohnsonBarFlowTarget_o)
  }
  LimePointFlowTarget <- function() {
    LimePointFlowTarget_o <- LimePointTarget_input[week_in_year,2]
    return(LimePointFlowTarget_o)
  }
  BRRelForJohnsonsBar <- function() { # Release of water from Brownlee to meet Johnson Bar fish flow target
  IncFlow <- HellsCanyonFlowData() - BrownleeFlowData()
  BRRelForJohnsonsBar_o <- max(0, JohnsonBarFlowTarget() * cfsTOafw - (BRPrelim_c + IncFlow))
  return(BRRelForJohnsonsBar_o)
}
LimePointFlowData <- function() {
  return(FlowLimePoint)
}
LPInc <- function() {
  LPInc_o <- LimePointFlowData() - HellsCanyonFlowData()
  return(LPInc_o)
}
BRRelForLimePoint <- function() {
  IncFlow <- LimePointFlowData() - BrownleeFlowData()
  BRRelForLimePoint_o <- max(0, LimePointFlowTarget() * cfsTOafw - (BRPrelim_c + IncFlow))
  return(BRRelForLimePoint_o)
}
BRRelForJBandLP <- function() {
  BRRelForJBandLP_o <- min(BRJBAvailWater(), max(BRRelForJohnsonsBar(), BRRelForLimePoint()))
  if (is.na(water_df$BRRelForJBandLP[week_counter])) {
    water_df$BRRelForJBandLP[week_counter] <<- BRRelForJBandLP_o
  }
  return(BRRelForJBandLP_o)
}
## Brownlee is drafted Sept - December to support spawning of fall Chinook (FERC No. 1971 Licence Application)
BRFallChinookDraft <- function() {
  if (BRRunoffAprJul <= 4.26e6) {  # 20th percentile Apr--Jul inflow forecast for the period 1979-2015
    BRFallChinookDraft_o <- BRFallChinookDraft_input$Low[week_in_year]
  } else if (BRRunoffAprJul <= 7.32e6) {
    BRFallChinookDraft_o <- BRFallChinookDraft_input$Low[week_in_year] + (BRFallChinookDraft_input$Medium[week_in_year] - BRFallChinookDraft_input$Low[week_in_year]) / (7.32e6 - 4.26e6)  * (BRRunoffAprJul - 4.26e6)
  } else if (BRRunoffAprJul <= 13.2e6) {
    BRFallChinookDraft_o <- BRFallChinookDraft_input$Medium[week_in_year] + (BRFallChinookDraft_input$High[week_in_year] - BRFallChinookDraft_input$Medium[week_in_year]) / (13.2e6 - 7.32e6)  * (BRRunoffAprJul - 7.32e6)
  } else {
    BRFallChinookDraft_o <- BRFallChinookDraft_input$High[week_in_year]
  }
  return(BRFallChinookDraft_o)
}

############################# Brownlee energy ##########################

BRPreEnergy <- function() {
  BRPreEnergy_o <- MWhr_per_ftAcFt * min(BRPrelim_c, BRPenLimit()) * BRNetHead() * BRCombEfficiency
  return(BRPreEnergy_o)
}
BRSharedWater <- function() {
  BRSharedWater_o <- max(Brownlee() + BRIn_c - BRPrelim_c - BRLGSup() - max(BRMinRefill(), BRCriticalCurveMin()), 0)
  return(BRSharedWater_o)
}
BRDownStreamHead <- function() {
  BRDownStreamHead_o <- BONNetHead() + DANetHead() + JDNetHead() + MCNetHead() + IHNetHead() +
  LMNetHead() + LIGNetHead() + LGNetHead() + HCNetHead() + OXNetHead()
  return(BRDownStreamHead_o)
}
BREnergyContent <- function() {
  BREnergyContent_o <- BRSharedWater() * (BRNetHead() + BRDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(BREnergyContent_o)
}
BR_ORCSharedWater <- function() {
  BR_ORCSharedWater_o <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGSup() - BR_ORC())
  return(BR_ORCSharedWater_o)
}
BR_ORCEnergyContent <- function() {
  BR_ORCEnergyContent_o <- BR_ORCSharedWater() * (BRNetHead() + BRDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(BR_ORCEnergyContent_o)
}
BRFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    if (TotalEnergyContent_c == 0) {
      BRFirmEngSup_o <- 0
    } else {
      BRFirmEngSup_o <- (BREnergyContent() + BR_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
    }
  } else if (Total_ORCEnergyContent_c == 0) {
    BRFirmEngSup_o <- 0
  } else {
    BRFirmEngSup_o <- BR_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
  }
  return(BRFirmEngSup_o)
}
BRNFEnergyContent <- function() {
  BRNFEnergyContent_o <- max(0, BR_ORCEnergyContent() - BRFirmEngSup())
  return(BRNFEnergyContent_o)
}
BRNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    BRNonFirmEngSup_o <- 0
  } else {
    BRNonFirmEngSup_o <- BRNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
  }
  return(BRNonFirmEngSup_o)
}
BRFirmEngSupReq <- function() {
  BRFirmEngSupReq_o <- min(BRPenLimit(), BRFirmEngSup() / (MWhr_per_ftAcFt * (BRNetHead() + BRDownStreamHead()) * BRCombEfficiency))
  return(BRFirmEngSupReq_o)
}
BRNonFirmEngSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    BRNonFirmEngSupReq_o <- min(BRPenLimit(), (BRFirmEngSup() + BRNonFirmEngSup()) / (MWhr_per_ftAcFt * (BRNetHead() + BRDownStreamHead()) * BRCombEfficiency))
  } else {
    BRNonFirmEngSupReq_o <- 0
  }
  return(BRNonFirmEngSupReq_o)
}
BREnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    BREnergySup_o <- max(min(BRFirmEngSupReq(), BRSharedWater()), min(BRNonFirmEngSupReq(), BR_ORCSharedWater()))
  } else {
    BREnergySup_o <- max(min(BRFirmEngSupReq(), BR_ORCSharedWater()), min(BRNonFirmEngSupReq(), BR_ORCSharedWater()))
  }
  return(BREnergySup_o)
}
BRCombSup <- function() {
  BRCombSup_o <- BRLGSup() + BREnergySup()
  BRCombSup_c <<- BRCombSup_o
  return(BRCombSup_o)
}
BRLGSup <- function() { # Release of water from Brownlee to meet Lower Granite fish flow target
# Handle NA values in global variables
BRIn_c_safe <- ifelse(is.na(BRIn_c), 0, BRIn_c)
BRPrelim_c_safe <- ifelse(is.na(BRPrelim_c), 0, BRPrelim_c)

denominator <- max(0, Brownlee() + BRIn_c_safe - BRPrelim_c_safe - BRLGDraftLimit()) +
max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
numerator <- max(0, Brownlee() + BRIn_c_safe - BRPrelim_c_safe - BRLGDraftLimit())
if (denominator == 0) {
  BRRelForLG_1 <- 0
} else {
  BRRelForLG_1 <- TotalRelForLowerGranite() * numerator / denominator
}
BRRelForLG_o <- min(BRLGAvailWater(), BRRelForLG_1)
water_df$BRLGSup[week_counter] <<- BRRelForLG_o
return(BRRelForLG_o)
}
BRLGSupEnergy <- function() {
  BRLGSupEnergy_o <- BRLGSup() * (BRNetHead() + BRDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(BRLGSupEnergy_o)
}

############# Brownlee final release ############################

BRCombUpProtect <- function() {
  excess <- PayetteDamProtectExcess() + BoiseDamProtectExcess() + OWYDamProtectExcess() + MINDamProtectExcess() + MINCombUpProtect_c
  inflow <- BRIn_c + excess
  outflow <- BRPrelim_c + BRCombSup_c
  BRCombUpProtect_o <- max(min(Brownlee() + inflow - outflow - BR_ORC(), excess), 0)
  BRCombUpProtect_c <<- BRCombUpProtect_o
  return(BRCombUpProtect_o)
}
BRDamProtectExcess <- function() {
  BRDamProtectExcess_o <- max(0, BRDamProtectRel() - BRPrelim_c - BRCombSup_c - BRCombUpProtect())
  return(BRDamProtectExcess_o)
}
BRRelease <- function() {
  BRRelease_o <- max(min(BRPrelim_c + BRCombSup() + BRCombUpProtect(), BRRelLimit()), BRDamProtectRel())
  return(BRRelease_o)
}
BROutflow <- function() {
  BROutflow_o <- BRRelease_c
  return(BROutflow_o)
}
BROut <- function() {
  BROut_o <- BRIn()
  return(BROut_o)
}

#######################################################
