#------------------ HUNGRY HORSE DAM -----------------#
#######################################################

HHFullPoolVol <- 3071500 # Storage corresponding with 3560 ft. pool elevation
HHBotVol <- 0 # Storage at 3336 ft. pool elevation
HHAvgMin <- 400 # FCOP, 2003

InitHH <- function() {
  InitHH_o <- ResInitFractionFull * HHFullPoolVol
  return(InitHH_o)
}
HungryHorse <- function() {
  if (week_counter == 1) {
    HungryHorse_o <- InitHH()
  } else {
    HungryHorse_o <- reservoir_vol_df$FLASF[week_counter - 1]
  }
  return(HungryHorse_o)
}
HungryHorseFlowData <- function() {
  return(max(FlowHH))
}
HHElev_ft <- function() {
  upper_vol <- HH_elev_input$Volume[which(HH_elev_input$Volume >= HungryHorse())[1]]
  lower_vol <- HH_elev_input$Volume[tail(which(HH_elev_input$Volume <= HungryHorse())[1],1)]
  upper_el <- HH_elev_input$Elevation[which(HH_elev_input$Volume >= HungryHorse())[1]]
  lower_el <- HH_elev_input$Elevation[tail(which(HH_elev_input$Volume <= HungryHorse())[1],1)]
  if (is.na(lower_el)) {
    HHElev_ft_o <- min(HH_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    HHElev_ft_o <- max(HH_elev_input$Elevation)
  } else if (lower_el == upper_el) {
    HHElev_ft_o <- lower_el
  } else {
    HHElev_ft_o <- lower_el + (HungryHorse() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(HHElev_ft_o)
}
HHNetHead <- function() {
  HHTailElev <- 3077 # Changed from 3082.6 on 5/25/23, average from 1/1/1980 to 1/1/2023. https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=hungry%20horse
  HHNetHead_o <- HHElev_ft() - HHTailElev
  return(HHNetHead_o)
}
HHPenLimit <- function() {
  HHpen <- 12048 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Hungry-Horse/
  HHPenLimit_o <- HHpen * cfsTOafw
  return(HHPenLimit_o)
}
HHIn <- function() {
  HHIn_o <- HungryHorseFlowData()
  return(HHIn_o)
}
HHInflow <- function() {
  HHInflow_o <- HHIn()
  return(HHInflow_o)
}

############ Hungry Horse max and min releases ###################

HHMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      HHMinRefill_o <- max(HHAssuredRefill(), HHCriticalCurveMin())
    } else {
      HHMinRefill_o <- HHMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    HHMinRefill_o <- HHBotVol
  }
  return(HHMinRefill_o)
}
HHMinReq <- function() {
  HHMinReq_1 <- max(HHFishMin() * cfsTOafw, HHRelForColFalls())
  if (FishOverRefillSw() == 1) {
    HHMinReq_o <- HHMinReq_1
  } else {
    HHMinReq_o <- min(max(HungryHorse() + HHIn() - max(HHCriticalCurveMin(), HHMinRefill()), HHAvgMin * cfsTOafw), HHMinReq_1)
  }
  return(HHMinReq_o)
}
HHDamProtectRel <- function() {
  HHDamProtectRel_o <- max(0, HungryHorse() + HHInflow() - HHFullPoolVol)
  return(HHDamProtectRel_o)
}
# Maximum flow at Columbia Falls, stipulated for Oct - Dec.
ColFallsMaxFlow <- function() {
  ColFallsMaxFlow_o <- 51000 # Flood stage at Columbia Falls is about 14 ft. or 51000 cfs (2009 Water Management Plan)
  return(ColFallsMaxFlow_o)
}
HHColFallsmax <- function() {
  HHColFallsmax_o <- max(0, ColFallsMaxFlow() * cfsTOafw - (ColumbiaFallsFlowData() - HungryHorseFlowData()))
  return(HHColFallsmax_o)
}
HHRelLimit <- function() {
  HHRelLimit_o <- max(max(HungryHorse() + HHInflow() - HHBotVol, 0))
  return(HHRelLimit_o)
}
HHAvailWater <- function() {
  HHAvailWater_o <- max(0, HungryHorse() + HHIn() - HHBotVol)
  return(HHAvailWater_o)
}

########### Hungry Horse rule curves ##############

## 1998 VARQ flood curve (current as of 5/30/2023)
# Oct - Apr flood evacuation; May - Jun 30 refill
HHFloodCurve <- function() {
  if (HHRunoffMaySep < 1.0E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood1[week_in_year]
  } else if (HHRunoffMaySep < 1.4E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood1[week_in_year] - (HHFlood_input$HHFlood1[week_in_year] - HHFlood_input$HHFlood2[week_in_year]) / (1.4E6 - 1.0E6) * (HHRunoffMaySep - 1.0E6)
  } else if (HHRunoffMaySep < 1.6E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood2[week_in_year] - (HHFlood_input$HHFlood2[week_in_year] - HHFlood_input$HHFlood3[week_in_year]) / (1.6E6 - 1.4E6) * (HHRunoffMaySep - 1.4E6)
  } else if (HHRunoffMaySep < 2.0E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood3[week_in_year] - (HHFlood_input$HHFlood3[week_in_year] - HHFlood_input$HHFlood4[week_in_year]) / (2.0E6 - 1.6E6) * (HHRunoffMaySep - 1.6E6)
  } else if (HHRunoffMaySep < 2.2E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood4[week_in_year] - (HHFlood_input$HHFlood4[week_in_year] - HHFlood_input$HHFlood5[week_in_year]) / (2.2E6 - 2.0E6) * (HHRunoffMaySep - 2.0E6)
  } else if (HHRunoffMaySep < 2.5E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood5[week_in_year] - (HHFlood_input$HHFlood5[week_in_year] - HHFlood_input$HHFlood6[week_in_year]) / (2.5E6 - 2.2E6) * (HHRunoffMaySep - 2.2E6)
  } else if (HHRunoffMaySep < 2.8E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood6[week_in_year] - (HHFlood_input$HHFlood6[week_in_year] - HHFlood_input$HHFlood7[week_in_year]) / (2.8E6 - 2.5E6) * (HHRunoffMaySep - 2.5E6)
  } else if (HHRunoffMaySep < 3.6E6) {
    HH_CurFC_o <- HHFlood_input$HHFlood7[week_in_year] - (HHFlood_input$HHFlood7[week_in_year] - HHFlood_input$HHFlood8[week_in_year]) / (3.6E6 - 2.8E6) * (HHRunoffMaySep - 2.8E6)
  } else {
    HH_CurFC_o <- HHFlood_input$HHFlood8[week_in_year]
  }
  return(HH_CurFC_o)
}
HHTopVol <- function() {
  if (TopRuleSw() == 0) {
    HHTopVol_o <- min(HHBiOpDraftLimit(), HHFloodCurve())
  } else if (TopRuleSw() == 1) {
    HHTopVol_o <- HHFullPoolVol
  } else if (TopRuleSw() == 2) {
    HHTopVol_o <- HHFlood$HHFlood1[week_in_year]
  }
  return(HHTopVol_o)
}
HHRuleReq <- function() {
  HHRuleReq_o <- max(max(HungryHorse() + HHIn() - HHTopVol(), 0))
  return(HHRuleReq_o)
}
HHPrelim <- function() {
  HHPrelim_o <- min(HHAvailWater(), max(HHRuleReq(), HHMinReq()))
  return(HHPrelim_o)
}
HHLowerLimit <- function() {
  HHLL_o <- lower_limit_input$HungryHorse[week_in_year]
  return(HHLL_o)
}
HHCriticalCurve <- function() {
  HHCriticalCurve_o <- HHCriticalCurve_input[week_in_year, 2]
  return(HHCriticalCurve_o)
}
HHCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC4[week_in_year]
  }
  return(HHCriticalCurveMin_o)
}
HHAssuredRefill <- function() {
  HHAssuredRefill_o <- HHAssuredRefill_input[week_in_year, 2]
  return(HHAssuredRefill_o)
}
HHVariableRefill <- function() {
  if (RefillSwitch() == 1) {
    HHRefillCurve_o <- HHAssuredRefill()
  } else if (RefillSwitch() == 2) {
    HHRefillCurve_o <- HHVariableRefillCurve
  }
  return(HHRefillCurve_o)
}
HH_ORC <- function() {
  HH_ORC_o <- min(max(min(max(HHAssuredRefill(), HHCriticalCurve()), HHVariableRefill()), HHLowerLimit(), HHMinRefill()), HHFloodCurve(), HHBiOpDraftLimit())
  return(HH_ORC_o)
}

########### Hungry Horse fish flow ###########################

####### HHMin (2000 FCRPS BiOp Section 3.A.1 Page 6)
# Minimum release from Hungry Horse
# 400 to 900 cfs
# variable based upon predicted inflows between april and august
HHFishMin <- function() {
  if (HHRunoffAprAug / 1000 < 1190) {
    HHMin_o <- 400
  } else if (HHRunoffAprAug / 1000 <= 1790) {
    HHMin_o <- 400 + (HHRunoffAprAug / 1000 - 1190) / (1790 - 1190) * (900 - 400)
  } else {
    HHMin_o <- 900
  }
  return(HHMin_o)
}
#### ColFallsTarget from 2000 FCRS BiOp Section 3.A.1 Page 7
# Columbia falls obligation to Columbia Falls Flow Target is based upon forecasted inflows from April through August.  The target is as follows (units cfs)
# Forecast       -- target
# Above 1790 taf -- 3500 cfs
# Below 1190 taf -- 3200 cfs
# between two values is a linear interpolation.
ColFallsTarget <- function() {
  if (HHRunoffAprAug / 1000 < 1190) {
    ColFallsTarget_o <- 3200
  } else if (HHRunoffAprAug / 1000 <= 1790) {
    ColFallsTarget_o <- 3200 + (HHRunoffAprAug / 1000 - 1190) / (1790 - 1190) * (3500 - 3200)
  } else {
    ColFallsTarget_o <- 3500
  }
  return(ColFallsTarget_o)
}
HHRelForColFalls <- function() {
  HHRelForColFalls_o <- max(0, ColFallsTarget() * cfsTOafw - (ColumbiaFallsFlowData() - HungryHorseFlowData()))
  return(HHRelForColFalls_o)
}
## 2008 BiOp requires HH be drafted to 3540-3550 ft. pool elevation from July - Sept.
## The variable target is based on May-Sep forecasted runoff volume at Hungry Horse (2022 water management plan).
HHBiOpDraftLimit <- function() {
  if (HHRunoffMaySep < 1.41e6) {
    HHBiOpDraftLimit_o <- HHBiOpDraftLimit_input$Low[week_in_year]
  } else if (HHRunoffMaySep < 1.58e6) {
    HHBiOpDraftLimit_o <- HHBiOpDraftLimit_input$Low[week_in_year] + (HHRunoffMaySep - 1.41e6) / (1.58e6 - 1.41e6) * (HHBiOpDraftLimit_input$High[week_in_year] - HHBiOpDraftLimit_input$Low[week_in_year])
  } else {
    HHBiOpDraftLimit_o <- HHBiOpDraftLimit_input$High[week_in_year]
  }
  return(HHBiOpDraftLimit_o)
}
HHMcNaryDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) {
      HHMcNaryDraftLimit_o <- HHBotVol
    } else if (Fish_Pool_Alternative == 1) {
      HHMcNaryDraftLimit_o <- 3.166e6
    } else if (Fish_Pool_Alternative == 2) {
      HHMcNaryDraftLimit_o <- HHFullPoolVol - 0.827E6
    } else if (Fish_Pool_Alternative == 3) {
      HHMcNaryDraftLimit_o <- HHFullPoolVol - 1.051E6
    } else if (Fish_Pool_Alternative == 4) {
      HHMcNaryDraftLimit_o <- HHFullPoolVol - 1.419E6
    } else if (Fish_Pool_Alternative == 5) {
      HHMcNaryDraftLimit_o <- HHFullPoolVol - 1.932E6
    } else {
      HHMcNaryDraftLimit_o <- 3.166e6
    }
  } else if (FishOverRefillSw() == 0) {
    HHMcNaryDraftLimit_o <- HH_ORC()
  }
  return(HHMcNaryDraftLimit_o)
}
HHMcNarySharedWater <- function() {
  HHMcNarySharedWater_o <- max(0, HungryHorse() + HHIn() - HHPrelim() - HHMcNaryDraftLimit())
  return(HHMcNarySharedWater_o)
}
HHMcNarySup <- function() {
  if (TotalMcNarySharedWater_c == 0) {
    HHMcNarySup_o <- 0
  } else {
    HHMcNarySup_o <- min(HHMcNarySharedWater(), McNaryFlowDeficit() * HHMcNarySharedWater() / TotalMcNarySharedWater_c)
  }
  return(HHMcNarySup_o)
}

########## Hungry Horse energy #####################################

HungryHorsePreEnergy <- function() {
  HungryHorsePreEnergy_o <- MWhr_per_ftAcFt * min(HHPrelim(), HHPenLimit()) * HHNetHead() * HHCombEfficiency
  return(HungryHorsePreEnergy_o)
}
HHSharedWater <- function() {
  HHSharedWater_o <- max(HungryHorse() + HHIn() - HHPrelim() - HHMcNarySup() - max(HHMinRefill(), HHCriticalCurveMin()), 0)
  return(HHSharedWater_o)
}
HHDownStreamHead <- function() {
  HHDownStreamHead_o <- AFNetHead() + BCNetHead() + BDNetHead() + CBNetHead() + KENetHead() + NOXNetHead() + TotalGCHead()
  return(HHDownStreamHead_o)
}
HHEnergyContent <- function() {
  HHEnergyContent_o <- HHSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(HHEnergyContent_o)
}
HH_ORCSharedWater <- function() {
  HH_ORCSharedWater_o <- max(0, HungryHorse() + HHIn() - HHPrelim() - HHMcNarySup() - HH_ORC())
  return(HH_ORCSharedWater_o)
}
HH_ORCEnergyContent <- function() {
  HH_ORCEnergyContent_o <- (HH_ORCSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt)
  return(HH_ORCEnergyContent_o)
}
HHFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    if (TotalEnergyContent_c == 0) {
      HHFirmEngSup_o <- 0
    } else {
      HHFirmEngSup_o <- (HHEnergyContent() + HH_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
    }
  } else if (Total_ORCEnergyContent_c == 0) {
    HHFirmEngSup_o <- 0
  } else {
    HHFirmEngSup_o <- HH_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
  }
  return(HHFirmEngSup_o)
}
HHNFEnergyContent <- function() {
  HHNFEnergyContent_o <- max(0, HH_ORCEnergyContent() - HHFirmEngSup())
  return(HHNFEnergyContent_o)
}
HHNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    HHNonFirmEngSup_o <- 0
  } else {
    HHNonFirmEngSup_o <- HHNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
  }
  return(HHNonFirmEngSup_o)
}
HHFirmEngSupReq <- function() {
  HHFirmEngSupReq_o <- min(HHPenLimit(), HHFirmEngSup() / (MWhr_per_ftAcFt * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency))
  return(HHFirmEngSupReq_o)
}
HHNonFirmEngSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    HHNonFirmEngSupReq_o <- min(HHPenLimit(), (HHFirmEngSup() + HHNonFirmEngSup()) / (MWhr_per_ftAcFt * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency))
  } else {
    HHNonFirmEngSupReq_o <- 0
  }
  return(HHNonFirmEngSupReq_o)
}
HHEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    HHEngSup_o <- max(min(HHFirmEngSupReq(), HHSharedWater()), min(HHNonFirmEngSupReq(), HH_ORCSharedWater()))
  } else {
    HHEngSup_o <- max(min(HHFirmEngSupReq(), HH_ORCSharedWater()), min(HHNonFirmEngSupReq(), HH_ORCSharedWater()))
  }
  return(HHEngSup_o)
}
HHEnergySupAllow <- function() { # Maximum water that can be released to meet energy requirement and flow objective at Columbia Falls
HHEnergySupAllow_o <- max(0, HHColFallsmax() - HHPrelim())
return(HHEnergySupAllow_o)
}
HHCombSup <- function() {
  HHCombSup_o <- min(HHEnergySup(), HHEnergySupAllow()) + HHMcNarySup()
  HHCombSup_c <<- HHCombSup_o
  return(HHCombSup_o)
}
HHMcNarySupEnergy <- function() {
  HHMcNarySupEnergy_o <- HHMcNarySup() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(HHMcNarySupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ###############################

HHFloodSpace <- function() {
  HHFloodSpace_o <- min(HHPrelim() + HHCombSup(), max(0, HHFullPoolVol - (HungryHorse() + HHIn() - HHPrelim() - HHCombSup())))
  HHFloodSpace_c <<- HHFloodSpace_o
  return(HHFloodSpace_o)
}
HHFloodFrac <- function() {
  if (TotalFloodSpace_c == 0) {
    HHFloodFrac_o <- 0
  } else {
    HHFloodFrac_o <- HHFloodSpace_c / TotalFloodSpace_c
  }
  return(HHFloodFrac_o)
}
HHRelReducReq <- function() {
  HHRelReducReq_o <- TotalRelReducReq_c * HHFloodFrac()
  return(HHRelReducReq_o)
}
HHDamProtectExcess <- function() {
  HHDamProtectExcess_o <- max(0, HHDamProtectRel() - HHPrelim() - HHCombSup_c)
  return(HHDamProtectExcess_o)
}

############# Hungry Horse final release ############################

HHRelease <- function() {
  HHRelease_o <- max(min(HHPrelim() + HHCombSup_c - HHRelReducReq(), HHRelLimit()), HHDamProtectRel(), HHAvgMin * cfsTOafw)
  return(HHRelease_o)
}
HHOutflow <- function() {
  HHOutflow_o <- HHRelease_c
  return(HHOutflow_o)
}

## There is not actually a dam at Columbia Falls, just a stream gauge

ColumbiaFallsFlowData <- function() {
  return(max(FlowCOL))
}
COLInc <- function() {
  COLInc_o <- ColumbiaFallsFlowData() - HungryHorseFlowData()
  return(COLInc_o)
}
COLIn <- function() {
  COLIn_o <- HHOutflow() + COLInc()
  return(COLIn_o)
}
COLOut <- function() {
  COLOut_o <- COLIn()
  return(COLOut_o)
}

#######################################################
