#-------------------- MICA DAM -----------------------#
#######################################################

MIFullPoolVol <- 20075000 # Storage at 2,475 ft of elevation
MIBotVol <- 8032200 # Storage at 2,320 ft of elevation
MIAvgMin <- 3000 # Minimum project outflow from Mica (cfs), FCOP 2003

InitMI <- function() { ## Initialize reservoir storage
InitMI_o <- ResInitFractionFull * MIFullPoolVol
return(InitMI_o)
}
Mica <- function() { ### Storage volume in most upstream dam
if (week_counter == 1) {
  Mica_o <- InitMI()
} else {
  Mica_o <- reservoir_vol_df$MICAA[week_counter - 1]
}
return(Mica_o)
}
MicaFlowData <- function() {
  return(max(FlowMI))
}
MIElev_ft <- function() { # Water level in dam (ft)
upper_vol <- MI_elev_input$Volume[which(MI_elev_input$Volume >= Mica())[1]]
lower_vol <- MI_elev_input$Volume[tail(which(MI_elev_input$Volume <= Mica())[1],1)]
upper_el <- MI_elev_input$Elevation[which(MI_elev_input$Volume >= Mica())[1]]
lower_el <- MI_elev_input$Elevation[tail(which(MI_elev_input$Volume <= Mica())[1],1)]
if (is.na(lower_el)) {
  MIElev_ft_o <- min(MI_elev_input$Elevation)
} else if (is.na(upper_el)) {
  MIElev_ft_o <- max(MI_elev_input$Elevation)
} else if (lower_el == upper_el) {
  MIElev_ft_o <- lower_el
} else {
  MIElev_ft_o <- lower_el + (Mica() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
}
return(MIElev_ft_o)
}
MINetHead <- function() { # Depth of water in the reservoir above tailwater (ft)
MITailElev <- 1875 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621046/mica-dam/
MINetHead_o <- MIElev_ft() - MITailElev
return(MINetHead_o)
}
MIPenLimit <- function() { # Maximum flow rate through turbines (AF/wk)
MIPenCap <- 38140 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621046/mica-dam/
MIPenLimit_o <- MIPenCap * cfsTOafw
return(MIPenLimit_o)
}
MIIn <- function() {
  MIIn_o = MicaFlowData()
  return(MIIn_o)
}
MIInflow <- function() { # Inflow to the dam
MIInflow_o <- MIIn()
return(MIIn())
}

################## Mica max and min releases ####################################

MITargetMin <- function() { ## target minimum release, FCOP 2003
MITargetMin_o <- MIAssuredRelease_input$target[week_in_year]
return(MITargetMin_o)
}
MITargetRelease <- function() { ## target average release, FCOP 2003
if (week_in_year %in% 1:3) {
  if (Arrow() >= 6772749) {
    #MITargetRelease_o <- (Mica() + MIInflow() - 19747728) / cfsTOafw
    MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
  } else if (Arrow() >= 4531429) {
    MITargetRelease_o <- 25000
  } else if (Arrow() >= 3202504) {
    MITargetRelease_o <- 20000
  } else {
    MITargetRelease_o <- 32000
  }
} else if (week_in_year %in% 4:5) {
  if (Arrow() >= 5602502) {
    #MITargetRelease_o <- (Mica() + MIInflow() - 20075000) / cfsTOafw
    MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
  } else if (Arrow() >= 4095065) {
    MITargetRelease_o <- 25000
  } else {
    MITargetRelease_o <- 32000
  }
} else if (week_in_year %in% 6:9) {
  if (Arrow() >= 7228947) {
    #MITargetRelease_o <- (Mica() + MIInflow() - 20075000) / cfsTOafw
    MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
  } else if (Arrow() >= 6971096) {
    MITargetRelease_o <- 24000
  } else if (Arrow() >= 5781014) {
    MITargetRelease_o <- 27000
  } else {
    MITargetRelease_o <- 32000
  }
} else if (week_in_year %in% 10:14) {
  if (Arrow() >= 7050435) {
    #MITargetRelease_o <- (Mica() + MIInflow() - 19875066) / cfsTOafw
    MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
  } else if (Arrow() >= 5384321) {
    MITargetRelease_o <- 19000
  } else if (Arrow() >= 4194239) {
    MITargetRelease_o <- 22000
  } else {
    MITargetRelease_o <- 32000
  }
} else if (week_in_year %in% 15:18) {
  if (Arrow() >= 6852088) {
    MITargetRelease_o <- 21000
  } else if (Arrow() >= 6435559) {
    MITargetRelease_o <- 19000
  } else if (Arrow() >= 1060357) {
    MITargetRelease_o <- 25000
  } else {
    MITargetRelease_o <- 32000
  }
} else if (week_in_year %in% 19:22) {
  if (Arrow() >= 5662006) {
    MITargetRelease_o <- 25000
  } else if (Arrow() >= 3797545) {
    MITargetRelease_o <- 22000
  } else if (Arrow() >= 822341) {
    MITargetRelease_o <- 27000
  } else {
    MITargetRelease_o <- 32000
  }
} else if (week_in_year %in% 23:27) {
  if (Arrow() >= 5463659) {
    MITargetRelease_o <- 24000
  } else if (Arrow() >= 4551263) {
    MITargetRelease_o <- 27000
  } else if (Arrow() >= 2904984) {
    MITargetRelease_o <- 25000
  } else {
    MITargetRelease_o <- 29000
  }
} else if (week_in_year %in% 28:31) {
  if (Arrow() >= 2944653) {
    MITargetRelease_o <- 21000
  } else if (Arrow() >= 2012422) {
    MITargetRelease_o <- 26000
  } else if (Arrow() >= 1219035) {
    MITargetRelease_o <- 21000
  } else {
    MITargetRelease_o <- 26000
  }
} else if (week_in_year %in% 32:35) {
  if (Arrow() >= 1814076) {
    MITargetRelease_o <- 17000
  } else if (Arrow() >= 1754571) {
    MITargetRelease_o <- 26000
  } else if (Arrow() >= 1238869) {
    MITargetRelease_o <- 22000
  } else {
    MITargetRelease_o <- 25000
  }
} else if (week_in_year %in% 36:37) {
  if (Arrow() >= 1992588) {
    MITargetRelease_o <- 20000
  } else if (Arrow() >= 921514) {
    MITargetRelease_o <- 10000
  } else if (Arrow() >= 663663) {
    MITargetRelease_o <- 12000
  } else {
    MITargetRelease_o <- 22000
  }
} else if (week_in_year %in% 38:39) {
  if (Arrow() >= 1357878) {
    MITargetRelease_o <- 10000
  } else if (Arrow() >= 445482) {
    MITargetRelease_o <- 15000
  } else if (Arrow() >= 266969) {
    MITargetRelease_o <- 10000
  } else {
    MITargetRelease_o <- 15000
  }
} else if (week_in_year %in% 40:44) {
  if (Arrow() >= 1496720) {
    MITargetRelease_o <- 8000
  } else if (Arrow() >= 1258704) {
    MITargetRelease_o <- 12000
  } else if (Arrow() >= 663663) {
    MITargetRelease_o <- 8000
  } else {
    MITargetRelease_o <- 10000
  }
} else if (week_in_year %in% 45:48) {
  if (Arrow() >= 3420686) {
    MITargetRelease_o <- 8000
  } else if (Arrow() >= 2250439) {
    MITargetRelease_o <- 10000
  } else if (Arrow() >= 1833910) {
    MITargetRelease_o <- 14000
  } else {
    MITargetRelease_o <- 18000
  }
} else if (week_in_year %in% 49:52) {
  if (Arrow() >= 6534733) {
    #MITargetRelease_o <- (Mica() + MIInflow() - 19952025) / cfsTOafw
    MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
  } else if (Arrow() >= 5523163) {
    #MITargetRelease_o <- (Mica() + MIInflow() - 19829050) / cfsTOafw
    MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
  } else if (Arrow() >= 2528125) {
    MITargetRelease_o <- 14000
  } else {
    MITargetRelease_o <- 18000
  }
}
return(MITargetRelease_o)
}


MIMinRefill <- function() { ## Minimum storage that will allow dam to refill and meet minimum project outflow criteria
if (GuranteeRefillSw() == 1) {
  if (week_in_year < 23) {
    MIMinRefill_o <- max(MIAssuredRefill(), MICriticalCurveMin())
  } else {
    MIMinRefill_o <- MIMinRefillCurve
  }
} else if (GuranteeRefillSw() == 2){
  MIMinRefill_o <- MIBotVol
}
return(MIMinRefill_o)
}
MIMinReq <- function() {
  MIMinReq_1 <- min(max(Mica() + MIIn() - max(MIMinRefill(), MICriticalCurveMin()), MIAvgMin * cfsTOafw), MITargetMin() * cfsTOafw)
  MIMinReq_2 <- min(max(Mica() + MIIn() - max(MIMinRefill(), MICriticalCurve()), MIAvgMin * cfsTOafw), MITargetMin() * cfsTOafw)
  MIMinReq_o <- max(MIMinReq_1, MIMinReq_2)
  return(MIMinReq_o)
}
MIDamProtectRel <- function() { # The release required to keep the dam from filling above the full pool volume.  Units acre-ft.
MIDamProt <- max(0, Mica() + MIInflow() - MIFullPoolVol)
return(MIDamProt)
}
MIRelLimit <- function() { # Max allowable release
MIRelLimit_o <- max(max(Mica() + MIInflow() - MIBotVol, 0))
return(MIRelLimit_o)
}
MIAvailWater <- function() { # Active storage
MIAvailWater_o <- max(0, Mica() + MIIn() - MIBotVol)
return(MIAvailWater_o)
}

################### Mica rule curves #########################

# Mica Flood Control Curves are modified according to January 1995 SRDs which are showed in USCOE Website
# (http://www.nwd-wc.usace.army.mil/cafe/forecast/SRD/MCDBChart8.pdf)
# Flood storage curve selected is based on the April -- August runoff forecast at The Dalles during the flood control evacuation period (Oct. -- Mar). Units acre-ft.
# When The Dalles forecast is > 111 MAF, on-call storage is requested up to 8 MAF. This is in addition to the 4 MAF of treaty storage at Mica
# Flood curve values are given in terms of the total volume of water held in the reservoir.

# Mica Dam flood evacuation period is 1 October to 30 March. Refill period is 1 April to 31 July.
MIFloodCurve <- function() {
  if (DARunoffAprAug >= 111E6 && MIRunoffAprAug > 19.3E6) {
    MI_CurFC_o <- MIFlood_input$MIFlood12[week_in_year]
  } else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 18E6 || MIRunoffMayAug > 17.5E6)) {
    MI_CurFC_o <- MIFlood_input$MIFlood11[week_in_year] - (MIFlood_input$MIFlood11[week_in_year] - MIFlood_input$MIFlood12[week_in_year]) / (19.3E6 - 18E6) * (MIRunoffAprAug - 18E6)
  } else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 16E6 || MIRunoffMayAug > 15.50E6)) {
    MI_CurFC_o <- MIFlood_input$MIFlood10[week_in_year] - (MIFlood_input$MIFlood10[week_in_year] - MIFlood_input$MIFlood11[week_in_year]) / (18E6 - 16E6) * (MIRunoffAprAug - 16E6)
  } else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 14E6 || MIRunoffMayAug > 13.50E6)) {
    MI_CurFC_o <- MIFlood_input$MIFlood9[week_in_year] - (MIFlood_input$MIFlood9[week_in_year] - MIFlood_input$MIFlood10[week_in_year]) / (16E6 - 14E6) * (MIRunoffAprAug - 14E6)
  } else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 12E6 || MIRunoffMayAug > 11.50E6)) {
    MI_CurFC_o <- MIFlood_input$MIFlood8[week_in_year] - (MIFlood_input$MIFlood8[week_in_year] - MIFlood_input$MIFlood9[week_in_year]) / (14E6 - 12E6) * (MIRunoffAprAug - 12E6)
  } else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 10E6 || MIRunoffMayAug > 9.5E6)) {
    MI_CurFC_o <- MIFlood_input$MIFlood7[week_in_year] - (MIFlood_input$MIFlood7[week_in_year] - MIFlood_input$MIFlood8[week_in_year]) / (12E6 - 10E6) * (MIRunoffAprAug - 10E6)
  } else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 8E6 || MIRunoffMayAug > 7.5E6)) {
    MI_CurFC_o <- MIFlood_input$MIFlood6[week_in_year] - (MIFlood_input$MIFlood6[week_in_year] - MIFlood_input$MIFlood7[week_in_year]) / (10E6 - 8E6) * (MIRunoffAprAug - 8E6)
  } else if (DARunoffAprAug >= 80.0E6) {
    MI_CurFC_o <- MIFlood_input$MIFlood5[week_in_year]
  } else if (DARunoffAprAug >= 75.0E6) {
    MI_CurFC_o <- MIFlood_input$MIFlood4[week_in_year] - (MIFlood_input$MIFlood4[week_in_year] - MIFlood_input$MIFlood5[week_in_year]) / (80E6 - 75E6) * (DARunoffAprAug - 75E6)
  } else if (DARunoffAprAug >= 70.0E6) {
    MI_CurFC_o <- MIFlood_input$MIFlood3[week_in_year] - (MIFlood_input$MIFlood3[week_in_year] - MIFlood_input$MIFlood4[week_in_year]) / (75E6 - 70E6) * (DARunoffAprAug - 70E6)
  } else if (DARunoffAprAug >= 65.0E6) {
    MI_CurFC_o <- MIFlood_input$MIFlood2[week_in_year] - (MIFlood_input$MIFlood2[week_in_year] - MIFlood_input$MIFlood3[week_in_year]) / (70E6 - 65E6) * (DARunoffAprAug - 65E6)
  } else if (DARunoffAprAug >= 62.0E6) {
    MI_CurFC_o <- MIFlood_input$MIFlood1[week_in_year] - (MIFlood_input$MIFlood1[week_in_year] - MIFlood_input$MIFlood2[week_in_year]) / (65E6 - 62E6) * (DARunoffAprAug - 62E6)
  } else {
    MI_CurFC_o <- MIFlood_input$MIFlood1[week_in_year]
  }
  return(MI_CurFC_o)
}
MITopVol <- function() {  ## Upper rule curve
if (TopRuleSw() == 0) { # Default = 0
  MITopVol_o <- MIFloodCurve()
} else if (TopRuleSw() == 1) {
  MITopVol_o <- MIFullPoolVol
} else if (TopRuleSw() == 2) {
  MITopVol_o <- MIFlood_input$MIFlood1[week_in_year]
} else if (TopRuleSw() == 3) {
  if (is.na(start_refill_wk)) {
    if (week_in_year < 35) {
      MITopVol_o <- MIFloodCurve()
    } else {
      MITopVol_o <- MIFullPoolVol
    }
  } else if (week_in_year < start_refill_wk) {
    MITopVol_o <- MIFloodCurve()
  } else {
    MITopVol_o <- MIFullPoolVol
  }
}
return(MITopVol_o)
}
MIRuleReq <- function() { ## Release required to meet upper rule curve (flood curve)
MIRuleReq_1 <- max(max(Mica() + MIIn() - MITopVol(), 0))
return(MIRuleReq_1)
}
MIPrelim <- function() { # Preliminary release based on required minimum release
MIpre <- min(MIAvailWater(), max(MIRuleReq(), MIMinReq()))
MIPrelim_c <<- MIpre
return(MIpre)
}
MILowerLimit <- function() { ## Minimum storage during the period January -- April
MILL_o <- lower_limit_input$Mica[week_in_year]
return(MILL_o)
}
MICriticalCurve <- function() { ## Minimum storage to meet firm hydropower in a dry year (1928-1932 critical water period)
MICriticalCurve_o <- MICriticalCurve_input$CRC1[week_in_year] # year 1 critical curve
return(MICriticalCurve_o)
}
## Draft limit in years when the reservoir needs to be drafted below the critical curve to meet the firm hydropower load.
## This is discussed in "Principles and Procedures", pg. 21.
MICriticalCurveMin <- function() {
  if (MICriticalCurveSw == 1) {
    MICriticalCurveMin_o <- MICriticalCurve_input$CRC1[week_in_year]
  } else if (MICriticalCurveSw == 2) {
    MICriticalCurveMin_o <- MICriticalCurve_input$CRC2[week_in_year] # year 2 critical curve
  } else if (MICriticalCurveSw == 3) {
    MICriticalCurveMin_o <- MICriticalCurve_input$CRC3[week_in_year] # year 3 critical curve
  } else if (MICriticalCurveSw == 4) {
    MICriticalCurveMin_o <- MICriticalCurve_input$CRC4[week_in_year] # year 4 critical curve
  }
  return(MICriticalCurveMin_o)
}
MIAssuredRefill <- function() { # Assured refill curve based on 1931 historical inflows
MIAssuredRefill_o <- MIAssuredRefill_input[week_in_year,2]
return(MIAssuredRefill_o)
}
###### Variable refill
# Refill rule curve based on Assured inflows and forecasts. The variable refill curve August -- December is equal
# to the assured refill curve. From January through July, the refill curve depends on inflow forecasts.
MIVariableRefill <- function() { # Required refill to ensure dam is full by end of year
if (RefillSwitch() == 1) { ## Default = 2
  MIRefillCurve_o <- MIAssuredRefill()
} else if (RefillSwitch() == 2) {
  MIRefillCurve_o <- MIVariableRefillCurve
}
return(MIRefillCurve_o)
}

##### Operating Rule Curve
# During the fixed period from August-December before the forecast, the BECC (Base Energy Content Curve) is the greater of the critical curve and the refill curve based on 1931 inflows (assured refill curve).
# During the variable period from January--July, the VECC (Variable Energy Content Curve) can be lower, but not higher than the BECC (Base Energy Content Curve).
# This value is compared to the flood storage curve, and operating rule curve lower limit to calculate the operating rule curve.
# "Principles and Procedures", pg. 19
MI_ORC <- function() {
  MI_ORC_o <- min(max(min(max(MIAssuredRefill(), MICriticalCurve()), MIVariableRefill()), MILowerLimit(), MIMinRefill()), MIFloodCurve())
  return(MI_ORC_o)
}

##################### Mica fish flow ########################################################

MIMcNaryDraftLimit <- function() { # Minumum allowable storage volume after accounting for release to meet McNary fish flow target
if (FishOverRefillSw() == 1) { # Default is 0 (don't sacrifice refill to meet fish flow target)
  MIMcNaryDraftLimit_o <- if (UseAllStorForMCNLG == 1) {
    MIMcNaryDraftLimit_o <- MIBotVol
  } else if (Fish_Pool_Alternative == 1) {
    MIMcNaryDraftLimit_o <- MIFullPoolVol
  } else if (Fish_Pool_Alternative == 2) {
    MIMcNaryDraftLimit_o <- MIFullPoolVol - 0.402E6
  } else if (Fish_Pool_Alternative == 3) {
    MIMcNaryDraftLimit_o <- MIFullPoolVol - 0.804E6
  } else if (Fish_Pool_Alternative == 4) {
    MIMcNaryDraftLimit_o <- MIFullPoolVol - 1.205E6
  } else {
    MIMcNaryDraftLimit_o <- MIFullPoolVol
  }
} else if (FishOverRefillSw() == 0) {
  MIMcNaryDraftLimit_o <- MI_ORC()
}
return(MIMcNaryDraftLimit_o)
}
MIMcNarySharedWater <- function() { # Volume of water that can be drafted below flood curve elevation to meet McNary flow target
MIMcNarySharedWater_o <- max(0, Mica() + MIIn() - MIPrelim() - MIMcNaryDraftLimit())
return(MIMcNarySharedWater_o)
}
MIMcNarySup <- function() { # Water released to meet McNary fish target (AF/wk)
if (TotalMcNarySharedWater_c == 0) {
  MIMcNarySup_o <- 0
} else {
  MIMcNarySup_o <- min(MIMcNarySharedWater(), McNaryFlowDeficit() * MIMcNarySharedWater() / TotalMcNarySharedWater_c)
}
return(MIMcNarySup_o)
}

############################### Mica Energy #######################

MIPreEnergy <- function() { # Preliminary energy production (MW-hr), considering only water released for flood protection and to meet minimum flow requirement
MIPreEnergy_o <- MWhr_per_ftAcFt * min(MIPrelim(), MIPenLimit()) * MINetHead() * MICombEfficiency
return(MIPreEnergy_o)
}
MISharedWater <- function() { # Maximum storage (above that already allocated for fish flow and flood protection) available to meet firm energy load
MISharedWater_o <- max(Mica() + MIIn() - MIPrelim() - MIMcNarySup() - max(MIMinRefill(), MICriticalCurveMin()), 0)
return(MISharedWater_o)
}
MIDownStreamHead <- function() { # Combined hydraulic head of reservoirs downstream of Mica
MIDownStreamHead_o <- REVNetHead() + ARNetHead() + TotalGCHead()
return(MIDownStreamHead_o)
}
MIEnergyContent <- function() { # Hydropower that could be generated by releasing all of MISharedWater
MIEnergyContent_o <- MISharedWater() * (MINetHead() + MIDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
return(MIEnergyContent_o)
}
MI_ORCSharedWater <- function() { # After meeting flood control and fish flow objectives, the additional water that can be released to generate primary (firm) or secondary (non firm) energy.
MI_ORCSharedWater_o <- max(0, Mica() + MIIn() - MIMcNarySup() - MIPrelim() - MI_ORC())
return(MI_ORCSharedWater_o)
}
MI_ORCEnergyContent <- function() { # Hydropower that could be generated by releasing all of MI_ORCSharedWater
MI_ORCEnergyContent_o <- MI_ORCSharedWater() * (MINetHead() + REVNetHead() + ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
return(MI_ORCEnergyContent_o)
}
MIFirmEngSup <- function() { # Power generation from Mica required to meet firm energy target (MW-hr)
if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
  if (TotalEnergyContent_c == 0) {
    MIFirmEngSup_o <- 0
  } else {
    MIFirmEngSup_o <- (MIEnergyContent() + MI_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
  }
} else if (Total_ORCEnergyContent_c == 0) {
  MIFirmEngSup_o <- 0
} else {
  MIFirmEngSup_o <- MI_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
}
return(MIFirmEngSup_o)
}
MINFEnergyContent <- function() { # Energy content of water stored in Mica that could be used for generating non-firm hydropower (MW-hr)
MINFEnergyContent_o <- max(0, MI_ORCEnergyContent() - MIFirmEngSup())
return(MINFEnergyContent_o)
}
MINonFirmEngSup <- function() { # Power generation from Mica required to meet non-firm energy target (MW-hr)
if (TotalNFEnergyContent_c == 0) {
  MINonFirmEngSup_o <- 0
} else {
  MINonFirmEngSup_o <- MINFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
}
return(MINonFirmEngSup_o)
}
MIFirmEngSupReq <- function() { # Required release from Mica to meet firm energy target (AF/wk)
MIFirmEngSupReq_o <- min(MIPenLimit(), MIFirmEngSup() / (MWhr_per_ftAcFt * (MINetHead() + REVNetHead() + TotalGCHead()) * MICombEfficiency))
return(MIFirmEngSupReq_o)
}
MINonFirmSupReq <- function() { # Required release from Mica to meet non-firm energy target (AF/wk)
if (NonFirmEnergySw() == 1) { # Default = 1
  MINonFirmSupReq_o <- min(MIPenLimit(), (MIFirmEngSup() + MINonFirmEngSup()) / (MWhr_per_ftAcFt * (MINetHead() + REVNetHead() + TotalGCHead()) * MICombEfficiency))
} else {
  MINonFirmSupReq_o <- 0
}
return(MINonFirmSupReq_o)
}
MIEnergySup <- function() { # Water released to satisfy firm and non-firm power requirements (AF/wk)
if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
  MIEnergySup_o <- max(min(MIFirmEngSupReq(), MISharedWater()), min(MINonFirmSupReq(), MI_ORCSharedWater()))
} else {
  MIEnergySup_o <- max(min(MIFirmEngSupReq(), MI_ORCSharedWater()), min(MINonFirmSupReq(), MI_ORCSharedWater()))
}
return(MIEnergySup_o)
}
# Total water to be released to meet fish and energy objectives
# When this function is called, it also calculates common variables that are used by many functions.
# This greatly increases computational efficiency.
MICombSup <- function() {
  if (BRIn_c == -9999) {
    BRIn_c <<- BRIn()
    water_df$BRIn[week_counter] <<- BRIn_c
  }
  if (BRPrelim_c == -9999) {
    BRPrelim_c <<- BRPrelim()
  }
  if (GCIn_c == -9999) {
    GCIn_c <<- GCIn()
    water_df$GCIn[week_counter] <<- GCIn_c
  }
  if (TotalEnergyContent_c == -9999) {
    TotalEnergyContent_c <<- TotalEnergyContent()
    energy_df$TotalEnergyContent[week_counter] <<- TotalEnergyContent_c
  }
  if (TotalMcNarySharedWater_c == -9999) {
    TotalMcNarySharedWater_c <<- TotalMcNarySharedWater()
    water_df$TotalMcNarySharedWater[week_counter] <<- TotalMcNarySharedWater_c
  }
  if (TotalBONSharedWater_c == -9999) {
    TotalBONSharedWater_c <<- TotalBONSharedWater()
    water_df$TotalBONSharedWater[week_counter] <<- TotalBONSharedWater_c
  }
  if (Total_ORCEnergyContent_c == -9999) {
    Total_ORCEnergyContent_c <<- Total_ORCEnergyContent()
    energy_df$Total_ORCEnergyContent[week_counter] <<- Total_ORCEnergyContent_c
  }
  if (TotalCoordPreEnergy_c == -9999) {
    TotalCoordPreEnergy_c <<- TotalCoordPreEnergy()
    energy_df$TotalCoordPreEnergy[week_counter] <<- TotalCoordPreEnergy_c
  }
  if (FirmEnergyDeficit_c == -9999) {
    FirmEnergyDeficit_c <<- FirmEnergyDeficit()
    energy_df$FirmEnergyDeficit[week_counter] <<- FirmEnergyDeficit_c
  }
  if (NonFirmEnergyDeficit_c == -9999) {
    NonFirmEnergyDeficit_c <<- NonFirmEnergyDeficit()
    energy_df$NonFirmEnergyDeficit[week_counter] <<- NonFirmEnergyDeficit_c
  }
  if (TotalNFEnergyContent_c == -9999) {
    TotalNFEnergyContent_c <<- TotalNFEnergyContent()
    energy_df$TotalNFEnergyContent[week_counter] <<- TotalNFEnergyContent_c
  }
  MICombSup_o <- MIEnergySup() + MIMcNarySup()
  MICombSup_c <<- MICombSup_o
  return(MICombSup_o)
}
MIMcNarySupEnergy <- function() { # Hydropower generated by releasing water to meet McNary fish target (MW-hr)
MIMcNarySupEnergy_o <- MIMcNarySup() * (MINetHead() + TotalGCHead() + REVNetHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
return(MIMcNarySupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ###############################

MIFloodSpace <- function() { ## Available flood space in Mica dam
MIFloodSpace_o <- min(MIPrelim() + MICombSup(), max(0, MIFullPoolVol - (Mica() + MIIn() - MIPrelim() - MICombSup())))
MIFloodSpace_c <<- MIFloodSpace_o
return(MIFloodSpace_o)
}
MIFloodFrac <- function() { ## Flood space in Mica as a fraction of the total flood space in Mica, Arrow, Libby, Hungry Horse, and Grand Coulee reservoirs
if (TotalFloodSpace_c == -9999) {
  TotalFloodSpace_c <<- TotalFloodSpace()
  water_df$TotalFloodSpace[week_counter] <<- TotalFloodSpace_c
}
if (TotalFloodSpace_c == 0) {
  MIFloodFrac_o <- 0
} else {
  MIFloodFrac_o <- MIFloodSpace_c / TotalFloodSpace_c
}
return(MIFloodFrac_o)
}
MIRelReducReq <- function() { ## Reduction in project outflow to meet controlled flow at The Dalles
MIRelReducReq_o <- TotalRelReducReq() * MIFloodFrac()
MIRelReducReq_c <<- MIRelReducReq_o
return(MIRelReducReq_o)
}
MIDamProtectExcess <- function() { ## release of water to prevent the dam from overflowing, in excess of that required to meet all other objectives.
MIDamProtectExcess_o <- max(0, MIDamProtectRel() - MIPrelim() - MICombSup_c)
return(MIDamProtectExcess_o)
}

################ Mica final release #######################################################

MIRelease <- function() { # Water released from Mica after accounting for all objectives
MIRelease_o <- max(min(MIPrelim() + MICombSup() - MIRelReducReq(), MIRelLimit()), MIDamProtectRel(), MIAvgMin * cfsTOafw)
return(MIRelease_o)
}
MIOutflow <- function() { # Same as MIRelease, but called when calculating inflow to downstream dams
MIOutflow_o <- MIRelease_c
return(MIOutflow_o)
}

#######################################################
