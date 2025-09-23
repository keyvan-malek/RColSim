# Grand_Coulee_Dam.R
# Reservoir operations functions for Grand Coulee Dam
# Handles inflow/outflow calculations, energy production, and flood control operations

#-------------- GRAND COULEE DAM ---------------------#
#######################################################

GCFullPoolVol <- 9107400 ## Volume at 1290 ft.
GCBotVol <- 3921900 ## Volume at 1208 ft.
GCAvgMin <- 30000 # Reported in Assured Operating Plans (AOPs)

InitGC <- function() {
  InitGC_o <- ResInitFractionFull * GCFullPoolVol
  return(InitGC_o)
}
GrandCoulee <- function() {
  if (week_counter == 1) {
    GrandCoulee_o <- InitGC()
  } else {
    GrandCoulee_o <- reservoir_vol_df$GCOUL[week_counter - 1]
  }
  return(GrandCoulee_o)
}
GrandCouleeFlowData <- function() {
  return(max(FlowGC))
}
GCInc <- function() {
  GCInc_o <- GrandCouleeFlowData() - BoundaryFlowData() - ArrowFlowData() - CorraLinnFlowData()
  return(GCInc_o)
}
GCElev_ft <- function() {
  upper_vol <- GC_elev_input$Volume[which(GC_elev_input$Volume >= GrandCoulee())[1]]
  lower_vol <- GC_elev_input$Volume[tail(which(GC_elev_input$Volume <= GrandCoulee())[1],1)]
  upper_el <- GC_elev_input$Elevation[which(GC_elev_input$Volume >= GrandCoulee())[1]]
  lower_el <- GC_elev_input$Elevation[tail(which(GC_elev_input$Volume <= GrandCoulee())[1],1)]
  if (is.na(lower_el)) {
    GCElev_ft_o <- min(GC_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    GCElev_ft_o <- max(GC_elev_input$Elevation)
  } else if (lower_el == upper_el) {
    GCElev_ft_o <- lower_el
  } else {
    GCElev_ft_o <- lower_el + (GrandCoulee() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(GCElev_ft_o)
}
GCNetHead <- function() {
  GCTailElev <- 961 # Changed 5/25/23 from 947. Source: https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=grand%20coulee
  GCNetHead_o <- GCElev_ft() - GCTailElev
  return(GCNetHead_o)
}
GCPenLimit <- function() {
  GCPenCap <- 292000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Grand-Coulee/
  GCPenLimit_o <- GCPenCap * cfsTOafw
  return(GCPenLimit_o)
}
GCIn <- function() { # We do not calculate Prelim() for the immediately upstream dam, Boundary, so we need to subtract the Boundary demands
GCIn_o <- BDPrelim() + ARPrelim() + CLPrelim() + GCInc()
return(GCIn_o)
}
GCPreInflow <- function() {
  GCPreInflow_o <- AFRelease() + max(min(ARPrelim() + ARCombSup_c + ARCombUpProtect(), ARRelLimit()), ARDamProtectRel()) +
  CLRelease() + (GrandCouleeFlowData() - ArrowFlowData() - CorraLinnFlowData() - AlbeniFallsFlowData())
  GCPreInflow_c <<- GCPreInflow_o
  return(GCPreInflow_o)
}
GCInflow <- function() {
  GCInflow_o <- BDOut() + AROutflow() + CLRelease_c + GCInc()
  return(GCInflow_o)
}

################## Grand Coulee max and min releases ####################################

GCMinRefill <- function() {
  if (GuranteeRefillSw() == 1) {
    if (week_in_year < 23) {
      GCMinRefill_o <- max(GCAssuredRefill(), GCCriticalCurveMin())
    } else {
      GCMinRefill_o <- GCMinRefillCurve
    }
  } else if (GuranteeRefillSw() == 2) {
    GCMinRefill_o <- GCBotVol
  }
  return(GCMinRefill_o)
}
GCMinReq <- function() {
  GCMinReq_o <- GCAvgMin * cfsTOafw
  return(GCMinReq_o)
}
GCDamProtectRel <- function() {
  GCDamProtectRel_o <- max(0, GrandCoulee() + GCPreInflow() - GCFullPoolVol)
  return(GCDamProtectRel_o)
}
GCRelLimit <- function() {
  GCRelLimit_o <- max(max(GrandCoulee() + GCPreInflow_c - GCBotVol, 0))
  return(GCRelLimit_o)
}
GCAvailWater <- function() {
  GCAvailWater_o <- max(0, GrandCoulee() + GCIn_c - GCBotVol)
  return(GCAvailWater_o)
}

################## Grand Coulee rule curves ####################################

# 2015 flood control curve. The Grand Coulee flood curve depends forecasted inflow at The Dalles,
# corrected for upstream refill
GCFloodCurve <- function() {
  if (CorrectedDARunoffAprAug < 57E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood1[week_in_year]
  } else if (CorrectedDARunoffAprAug < 60E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood1[week_in_year] - (GCFlood_input$GCFlood1[week_in_year] - GCFlood_input$GCFlood2[week_in_year]) / (60E6 - 57E6) * (CorrectedDARunoffAprAug - 57E6)
  } else if (CorrectedDARunoffAprAug < 63.25E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood2[week_in_year] - (GCFlood_input$GCFlood2[week_in_year] - GCFlood_input$GCFlood3[week_in_year]) / (63.25E6 - 60E6) * (CorrectedDARunoffAprAug - 60E6)
  } else if (CorrectedDARunoffAprAug < 65E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood3[week_in_year] - (GCFlood_input$GCFlood3[week_in_year] - GCFlood_input$GCFlood4[week_in_year]) / (65E6 - 63.25E6) * (CorrectedDARunoffAprAug - 63.25E6)
  } else if (CorrectedDARunoffAprAug < 67.66E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood4[week_in_year] - (GCFlood_input$GCFlood4[week_in_year] - GCFlood_input$GCFlood5[week_in_year]) / (67.66E6 - 65E6) * (CorrectedDARunoffAprAug - 65E6)
  } else if (CorrectedDARunoffAprAug < 71.0E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood5[week_in_year] - (GCFlood_input$GCFlood5[week_in_year] - GCFlood_input$GCFlood6[week_in_year]) / (71.0E6 - 67.66E6) * (CorrectedDARunoffAprAug - 67.66E6)
  } else if (CorrectedDARunoffAprAug < 75.0E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood6[week_in_year] - (GCFlood_input$GCFlood6[week_in_year] - GCFlood_input$GCFlood7[week_in_year]) / (75.0E6 - 71.0E6) * (CorrectedDARunoffAprAug - 71.0E6)
  } else if (CorrectedDARunoffAprAug < 80.0E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood7[week_in_year] - (GCFlood_input$GCFlood7[week_in_year] - GCFlood_input$GCFlood8[week_in_year]) / (80.0E6 - 75.0E6) * (CorrectedDARunoffAprAug - 75.0E6)
  } else if (CorrectedDARunoffAprAug <= 95.0E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood8[week_in_year]
  } else if (CorrectedDARunoffAprAug < 100.0E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood8[week_in_year] - (GCFlood_input$GCFlood8[week_in_year] - GCFlood_input$GCFlood9[week_in_year]) / (100.0E6 - 95.0E6) * (CorrectedDARunoffAprAug - 95.0E6)
  } else if (CorrectedDARunoffAprAug <= 115.0E6) {
    GC_CurFC_o <- GCFlood_input$GCFlood9[week_in_year] - (GCFlood_input$GCFlood9[week_in_year] - GCFlood_input$GCFlood10[week_in_year]) / (115.0E6 - 100.0E6) * (CorrectedDARunoffAprAug - 100.0E6)
  } else {
    GC_CurFC_o <- GCFlood_input$GCFlood10[week_in_year]
  }
  return(GC_CurFC_o)
}
GCBeginRefill <- function() { # the beginning of the flood control refill period, (2022 Draft Water Management, pg. 32)
if (is.na(start_refill_wk_GC)) {
  GCBeginRefill_o <- 40
} else {
  GCBeginRefill_o <- max(40, start_refill_wk_GC)
}
return(GCBeginRefill_o)
}
GCTopVol <- function() {
  if (TopRuleSw() == 0) {
    if (week_in_year >= GCBeginRefill()) {
      GCTopVol_o <- GCFullPoolVol
    } else {
      GCTopVol_o <- min(GCSummerDraftLimit(), GCFloodCurve())
    }
  } else if (TopRuleSw() == 1) {
    GCTopVol_o <- GCFullPoolVol
  } else if (TopRuleSw() == 2) {
    GCTopVol_o <- GCFlood_input$GCFlood1[week_in_year]
  }
  return(GCTopVol_o)
}
GC_April_Target <- function() {
  if (CorrectedDARunoffAprAug < 57E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood1[36]
  } else if (CorrectedDARunoffAprAug < 60E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood1[36] - (GCFlood_input$GCFlood1[36] - GCFlood_input$GCFlood2[36]) / (60E6 - 57E6) * (CorrectedDARunoffAprAug - 57E6)
  } else if (CorrectedDARunoffAprAug < 63.25E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood2[36] - (GCFlood_input$GCFlood2[36] - GCFlood_input$GCFlood3[36]) / (63.25E6 - 60E6) * (CorrectedDARunoffAprAug - 60E6)
  } else if (CorrectedDARunoffAprAug < 65E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood3[36] - (GCFlood_input$GCFlood3[36] - GCFlood_input$GCFlood4[36]) / (65E6 - 63.25E6) * (CorrectedDARunoffAprAug - 63.25E6)
  } else if (CorrectedDARunoffAprAug < 67.66E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood4[36] - (GCFlood_input$GCFlood4[36] - GCFlood_input$GCFlood5[36]) / (67.66E6 - 65E6) * (CorrectedDARunoffAprAug - 65E6)
  } else if (CorrectedDARunoffAprAug < 71.0E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood5[36] - (GCFlood_input$GCFlood5[36] - GCFlood_input$GCFlood6[36]) / (71.0E6 - 67.66E6) * (CorrectedDARunoffAprAug - 67.66E6)
  } else if (CorrectedDARunoffAprAug < 75.0E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood6[36] - (GCFlood_input$GCFlood6[36] - GCFlood_input$GCFlood7[36]) / (75.0E6 - 71.0E6) * (CorrectedDARunoffAprAug - 71.0E6)
  } else if (CorrectedDARunoffAprAug < 80.0E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood7[36] - (GCFlood_input$GCFlood7[36] - GCFlood_input$GCFlood8[36]) / (80.0E6 - 75.0E6) * (CorrectedDARunoffAprAug - 75.0E6)
  } else if (CorrectedDARunoffAprAug <= 95.0E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood8[36]
  } else if (CorrectedDARunoffAprAug < 100.0E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood8[36] - (GCFlood_input$GCFlood8[36] - GCFlood_input$GCFlood9[36]) / (100.0E6 - 95.0E6) * (CorrectedDARunoffAprAug - 95.0E6)
  } else if (CorrectedDARunoffAprAug <= 115.0E6) {
    GC_AprilFC_o <- GCFlood_input$GCFlood9[36] - (GCFlood_input$GCFlood9[36] - GCFlood_input$GCFlood10[36]) / (115.0E6 - 100.0E6) * (CorrectedDARunoffAprAug - 100.0E6)
  } else {
    GC_AprilFC_o <- GCFlood_input$GCFlood10[36]
  }
  GC_April_Target_o <- GC_AprilFC_o
  return(GC_April_Target_o)
}
GCRuleReq <- function() {
  GCRuleReq_o <- max(max(GrandCoulee() + GCIn_c - GCTopVol(), 0))
  return(GCRuleReq_o)
}
GCPrelim <- function() {
  GCPrelim_o <- min(GCAvailWater(), max(GCRuleReq(), GCMinReq()))
  return(GCPrelim_o)
}
GCLowerLimit <- function() {
  GCLL_o <- lower_limit_input$GrandCoulee[week_in_year]
  return(GCLL_o)
}
GCCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC4[week_in_year]
  }
  return(GCCriticalCurveMin_o)
}
GCCriticalCurve <- function() {
  GCCriticalCurve_o <- GCCriticalCurve_input$CRC1[week_in_year]
  return(GCCriticalCurve_o)
}
GCAssuredRefill <- function() {
  GCAssuredRefill_o <- GCAssuredRefill_input$GrandCoulee[week_in_year]
  return(GCAssuredRefill_o)
}
GCVariableRefill <- function() {
  if (week_in_year >= GCBeginRefill()) {
    GCRefillCurve_o <- GCVariableRefillCurve
  } else {
    GCRefillCurve_o <- GCFullPoolVol
  }
  return(GCRefillCurve_o)
}

# The Gifford-Inchelium ferry cannot operate if the reservoir is below 1232 ft of elevation
# This storage is used as a lower bound for the BECC and VECC curve (https://www.scenicwa.com/poi/inchelium-gifford-ferry).  Units acre-ft.
GCFerryLimit <- function() {
  GCFerryLimit_o <- GC_elev_input$Volume[GC_elev_input$Elevation == 1232] # Storage at 1232 ft.
  return(GCFerryLimit_o)
}
GCResidualInflow <- function() { # 85% exceedence flow for reguluated inflow to Grand Coulee
GCResidualInflow_o <- GCResidualInflow_input[week_in_year,2]
return(GCResidualInflow_o)
}
PRIncInflow <- function() { # 75% exceedence incremental flow between Priest Rapids and Grand Coulee
PRIncInflow_o <- PRInc_inflow_input[week_in_year,2]
return(PRIncInflow_o)
}

## The variable draft limit is the lower rule curve for the period 1 Jan to 10 April. It is the minimum storage
## required to meet the April 10 flood target. It for the regulated inflow and discharge to meet the Vernita Bar flow target.
## Described in 2022 Draft Water Management Plan, Section 7.4
GCVariableDraftLimit <- function() {
  if (week_in_year>=23 && week_in_year<=36) {
    GCVariableLimit_o <- min(GCFloodCurve(), GC_April_Target() - GCResidualInflow() - PRIncInflow() + GCBdgtForVB())
  } else {
    GCVariableLimit_o <- GCFullPoolVol ## The variable draft limit does not apply after April 10
  }
  return(GCVariableLimit_o)
}

## The variable draft limit cannot fall below the lower limit unless required for flood control
GC_VDL <- function() {
  GC_VDL_o <- min(max(GCVariableDraftLimit(), GCLowerLimit()), GCFloodCurve())
  return(GC_VDL_o)
}

## Recreation requirements at Grand Coulee require that the VECC is limited to 1280 ft of elevation (storage=8.712e6 acre ft) from June 30 to Labor Day.
## This model uses end-of-month storage targets to drive the model
## calculations so the draft limit is in essence a lower bound for the end of month VECC for June, July, and August. Units acre-ft.
GCRecLimit <- function() {
  if (week_in_year %in% c(49:52, 1:6)) {
    GCRecLimit_o <- GC_elev_input$Volume[GC_elev_input$Elevation == 1280] # https://www.lrf.org/lake-roosevelt/operations/recreation
  } else {
    GCRecLimit_o <- 0
  }
  return(GCRecLimit_o)
}
GC_ORC <- function() {
  GC_ORC_o <- min(max(min(max(GCAssuredRefill(), GCCriticalCurve()), GCVariableRefill()), GCLowerLimit(), GCRecLimit(), GCFerryLimit()), GCSummerDraftLimit(), GC_VDL())
  return(GC_ORC_o)
}

################## Grand Coulee fish flows ###################

## Grand Coulee is drafted in July and August to support salmon migration. If The Dalles runoff forecast is >= 92 MAF, the target elevation of
## Lake Roosevelt is 1280 ft, otherwise it is 1278 ft (2009 Water Management Plan)
GCSummerDraftLimit <- function() {
  if (DARunoffAprAug >= 92.0E6) {
    GCSummerDraft_o <- GCSummerDraft_input$Draft1[week_in_year]
  } else {
    GCSummerDraft_o <- GCSummerDraft_input$Draft2[week_in_year]
  }
  return(GCSummerDraft_o)
}

## The Vernita Bar Agreement sets minimum flow during spawning of fall Chinook (approximately August through November) of 70 kcfs. The required flow
## is 50 kcsf during hatching and emergence (December through March). The Spring flow BiOp at Priest rapids is 135 kcfs from April 10 to June 30
## VernitaBarFlowTarget combines 135 kcfs spring flow objectives at Priest Rapids (1998 NMFS Supplemental Opinion, see 2000 FCRPS BiOp) and the Vernita Bar Agreement ("Hanford Reach Fall Chinook Protection Program")
GCBdgtForVB <- function() {
  GCBdgtForVB_o <- GCBdgtForVB_input[week_in_year, 2] ## Date to April 10 discharge volume (Hanford Reach fall Chinook protection program)
  return(GCBdgtForVB_o)
}
VernitaBarFlowTarget <- function() {
  VernitaBarFlowTarget_o <- VernitaBarFlowTarget_input[week_in_year, 2]
  return(VernitaBarFlowTarget_o)
}
GCVernitaBarAvailWater <- function() {
  GCVernitaBarAvailWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GC_ORC())
  return(GCVernitaBarAvailWater_o)
}
GCSupForVernitaBar <- function() {
  GCSupForVernitaBar_o <- max(min(GCVernitaBarAvailWater(), VernitaBarFlowTarget() * cfsTOafw - (GCPrelim() + PriestRapidsFlowData() - GrandCouleeFlowData())), 0)
  if (is.na(water_df$GCSupForVernitaBar[week_counter])) {
    water_df$GCSupForVernitaBar[week_counter] <<- GCSupForVernitaBar_o
  }
  return(GCSupForVernitaBar_o)
}
GCMcNaryDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    if (UseAllStorForMCNLG == 1) {
      GCMcNaryDraftLimit_o <- GCBotVol
    } else if (Fish_Pool_Alternative == 1) {
      GCMcNaryDraftLimit_o <- 8.316e6
    } else if (Fish_Pool_Alternative == 2) {
      GCMcNaryDraftLimit_o <- GCFullPoolVol - 1.203e6
    } else if (Fish_Pool_Alternative == 3) {
      GCMcNaryDraftLimit_o <- GCFullPoolVol - 1.615e6
    } else if (Fish_Pool_Alternative == 4) {
      GCMcNaryDraftLimit_o <- GCFullPoolVol - 2.292e6
    } else if (Fish_Pool_Alternative == 5) {
      GCMcNaryDraftLimit_o <- GCFullPoolVol - 3.235e6
    } else {
      GCMcNaryDraftLimit_o <- 8.316e6
    }
  } else if (FishOverRefillSw()==0) {
    GCMcNaryDraftLimit_o <- GC_ORC()
  }
  return(GCMcNaryDraftLimit_o)
}
GCMcNarySharedWater <- function() {
  GCMcNarySharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCMcNaryDraftLimit())
  return(GCMcNarySharedWater_o)
}
GCMcNarySup <- function() {
  if (TotalMcNarySharedWater_c == 0) {
    GCMcNarySup_o <- 0
  } else {
    GCMcNarySup_o <- min(GCMcNarySharedWater(), McNaryFlowDeficit() * GCMcNarySharedWater() / TotalMcNarySharedWater_c)
  }
  return(GCMcNarySup_o)
}
GCBONDraftLimit <- function() {
  if (FishOverRefillSw() == 1) {
    GCBONDraftLimit_o <- GCBotVol
  } else if (week_in_year < 28) {
    GCBONDraftLimit_o <- BONDraftLimit_input[week_in_year,2]
  } else {
    GCBONDraftLimit_o <- GC_ORC()
  }
  return(GCBONDraftLimit_o)
}
GCBONSharedWater <- function() {
  GCBONSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCBONDraftLimit())
  return(GCBONSharedWater_o)
}
GCBONSup <- function() {
  if (TotalBONSharedWater_c == 0) {
    GCBONSup_o <- 0
  } else {
    GCBONSup_o <- min(GCBONSharedWater(), BONFlowDeficit() * GCBONSharedWater() / TotalBONSharedWater_c)
  }
  return(GCBONSup_o)
}
GCFishSup <- function() {
  GCFishSup_o <- max(GCBONSup(), GCSupForVernitaBar(), GCMcNarySup())
  return(GCFishSup_o)
}

############## Grand Coulee energy #################

GCPreEnergy <- function() {
  GCPreEnergy_o <- MWhr_per_ftAcFt * min(GCPrelim(), GCPenLimit()) * GCNetHead() * GCCombEfficiency
  return(GCPreEnergy_o)
}
GCSharedWater <- function() {
  GCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCFishSup() - min(max(GCMinRefill(), GCCriticalCurveMin()), GC_ORC()))
  return(GCSharedWater_o)
}
GCDownStreamHead <- function() {
  GCDownStreamHead_o <- BONNetHead() + CJNetHead() + DANetHead() + JDNetHead() + MCNetHead() + PRNetHead() + RINetHead() + RRNetHead() + WANetHead() + WENetHead()
  return(GCDownStreamHead_o)
}
TotalGCHead = function() {
  TotalGCHead_o <- GCNetHead() + GCDownStreamHead()
  return(TotalGCHead_o)
}
GCEnergyContent <- function() {
  GCEnergyContent_o <- GCSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
  return(GCEnergyContent_o)
}
GC_ORCSharedWater <- function() {
  GC_ORCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCFishSup() - GC_ORC())
  return(GC_ORCSharedWater_o)
}
GC_ORCEnergyContent <- function() {
  GC_ORCEnergyContent_o <- GC_ORCSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
  return(GC_ORCEnergyContent_o)
}
GCFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    # Handle NA values in global variables
    TotalEnergyContent_c_safe <- ifelse(is.na(TotalEnergyContent_c), 0, TotalEnergyContent_c)
    Total_ORCEnergyContent_c_safe <- ifelse(is.na(Total_ORCEnergyContent_c), 0, Total_ORCEnergyContent_c)
    FirmEnergyDeficit_c_safe <- ifelse(is.na(FirmEnergyDeficit_c), 0, FirmEnergyDeficit_c)

    if (TotalEnergyContent_c_safe == 0) {
      GCFirmEngSup_o <- 0
    } else {
      GCFirmEngSup_o <- (GCEnergyContent() + GC_ORCEnergyContent()) / (TotalEnergyContent_c_safe + Total_ORCEnergyContent_c_safe) * FirmEnergyDeficit_c_safe
    }
  } else if (Total_ORCEnergyContent_c_safe == 0) {
    GCFirmEngSup_o <- 0
  } else {
    GCFirmEngSup_o <- GC_ORCEnergyContent() / Total_ORCEnergyContent_c_safe * FirmEnergyDeficit_c_safe
  }
  if (is.na(water_df$GCFirmEngSup[week_counter])) {
    water_df$GCFirmEngSup[week_counter] <<- GCFirmEngSup_o
  }
  return(GCFirmEngSup_o)
}
GCNFEnergyContent <- function() {
  GCNFEnergyContent_o <- max(0, GC_ORCEnergyContent() - GCFirmEngSup())
  return(GCNFEnergyContent_o)
}
GCNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    GCNonFirmEngSup_o <- 0
  } else {
    GCNonFirmEngSup_o <- GCNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
  }
  return(GCNonFirmEngSup_o)
}
GCFirmEngSupReq <- function() {
  GCFirmEngSupReq_o <- min(GCPenLimit(), GCFirmEngSup() / (MWhr_per_ftAcFt * TotalGCHead() * GCCombEfficiency))
  return(GCFirmEngSupReq_o)
}
GCNonFirmEngSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    GCNonFirmEngSupReq_o <- min(GCPenLimit(), (GCFirmEngSup() + GCNonFirmEngSup()) / (MWhr_per_ftAcFt * (GCNetHead() + GCDownStreamHead()) * GCCombEfficiency))
  } else {
    GCNonFirmEngSupReq_o <- 0
  }
  return(GCNonFirmEngSupReq_o)
}
GCEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    GCEnergySup_o <- max(min(GCFirmEngSupReq(), GCSharedWater()), min(GCNonFirmEngSupReq(), GC_ORCSharedWater()))
  } else {
    GCEnergySup_o <- max(min(GCFirmEngSupReq(), GC_ORCSharedWater()), min(GCNonFirmEngSupReq(), GC_ORCSharedWater()))
  }
  return(GCEnergySup_o)
}
GCCombUpSup <- function() {
  if (AFCombSup_c == -9999) {
    GCCombUpSup_1 <- AFCombSup() + ARCombSup() + CLCombSup()
  } else {
    GCCombUpSup_1 <- AFCombSup_c + ARCombSup_c + CLCombSup_c
  }
  if (refill_cat4 == 0) {
    if (week_in_year > 14) {
      GCCombUpSup_o <- GCCombUpSup_1
    } else {
      inflow <- GCIn_c + GCCombUpSup_1
      outflow <- GCPrelim() + GCEnergySup() + GCFishSup()
      min_draft <- min(max(GCMinRefill(), GCCriticalCurveMin()), GCBONDraftLimit(), GC_ORC())
      GCCombUpSup_o <- min(max(max(GrandCoulee() + inflow - outflow - min_draft, 0)), GCCombUpSup_1)
    }
  } else if (refill_cat4 == 1) {
    inflow <- GCIn_c + GCCombUpSup_1
    outflow <- GCPrelim() + GCEnergySup() + GCFishSup()
    min_draft <- min(max(GCMinRefill(), GCCriticalCurveMin()), GCBONDraftLimit(), GC_ORC())
    GCCombUpSup_o <- min(max(max(GrandCoulee() + inflow - outflow - min_draft, 0)), GCCombUpSup_1)
  } else if (refill_cat4 == 2) {
    inflow <- GCIn_c + GCCombUpSup_1
    outflow <- GCPrelim() + GCEnergySup() + GCFishSup()
    GCCombUpSup_o <- min(max(max(GrandCoulee() + inflow - outflow - GC_ORC(), 0)), GCCombUpSup_1)
  }
  return(GCCombUpSup_o)
}
GCCombSup <- function() {
  GCCombSup_o <- GCFishSup() + GCCombUpSup() + GCEnergySup()
  GCCombSup_c <<- GCCombSup_o
  return(GCCombSup_o)
}
GCFishSupEnergy <- function() { # Energy produced by releasing water to meet fish targets at McNary, Vernita Bar, and Bonneville
GCFishSupEnergy_o <- GCFishSup() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
return(GCFishSupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ###############################

GCFloodSpace <- function() {
  GCFloodSpace_o <- min(GCPrelim() + GCCombSup_c, max(0, GCFullPoolVol - (GrandCoulee() + GCIn_c - GCPrelim() - GCCombSup_c)))
  return(GCFloodSpace_o)
}
GCFloodRelSharedWater <- function() {
  GCFloodRelSharedWater_o <- max(0, GrandCoulee() + GCPreInflow_c - GCPrelim() - GCCombSup() - GC_ORC())
  GCFloodRelSharedWater_c <<- GCFloodRelSharedWater_o
  return(GCFloodRelSharedWater_o)
}
GCMinFloodRelReq <- function() {
  GCMinFloodRelReq_o <- min(GCFloodRelSharedWater(), MinFloodRelReq())
  GCMinFloodRelReq_c <<- GCMinFloodRelReq_o
  water_df$GCMinFloodRelReq[week_counter] <<- GCMinFloodRelReq_c
  return(GCMinFloodRelReq_o)
}
GCCombUpProtect <- function() {
  excess <- AFDamProtectExcess() + ARDamProtectExcess() + CLDamProtectExcess() + AFCombUpProtect_c + ARCombUpProtect_c + CLCombUpProtect_c
  outflow <- GCPrelim() + GCCombSup_c
  GCCombUpProtect_o <- max(min(GrandCoulee() + GCPreInflow_c - outflow - GC_ORC(), excess), 0)
  GCCombUpProtect_c <<- GCCombUpProtect_o
  return(GCCombUpProtect_o)
}
GCDamProtectExcess <- function() {
  GCDamProtectExcess_o <- max(0, GCDamProtectRel() - GCPrelim() - GCCombSup_c - GCCombUpProtect())
  return(GCDamProtectExcess_o)
}

##################### Grand Coulee final release ###################################

GCRelease <- function() {
  GCRelease_o <- max(min(GCPrelim() + GCCombSup_c - TotalRelReducReq_c + GCMinFloodRelReq_c + GCCombUpProtect(), GCRelLimit()), GCDamProtectRel() + (GCInflow() - GCPreInflow()), GCMinReq())
  return(GCRelease_o)
}
GCOutflow <- function() {
  GCOutflow_o <- GCRelease_c
  return(GCOutflow_o)
}

#################### Controlled flow at The Dalles ###################################


## The initial controlled flow guides refill during the flood control refill period, which begins 3 weeks before the
## unregulated flow at The Dalles is forecasted to exceed 450 cfs (FCOP, 2003).
ICF <- function() {
  if (week_in_year == 1 || is.na(start_refill_wk)) {
    InitialControlledFlow_previous <<- 1e10
    ICF_o <- InitialControlledFlow_previous
  } else if (week_in_year %in% start_refill_wk) {
    ICF_o <- InitialControlledFlow
    InitialControlledFlow_previous <<- ICF_o
    if (week_in_year == start_refill_wk) {
      control_wk <<- week_counter
    }
  } else {
    ICF_o <- InitialControlledFlow_previous
  }
  return(ICF_o)
}

## The controlled flow is adusted based on the fraction of storage space that has been refilled in the category 4 reservoirs
Cat4FilledStorPerc <- function() {
  if (control_wk == -9999 || control_wk == week_counter) {
    storage_o <- 0
  } else {
    storage_o <- 100 * max(0, 1.0 - (((GCFullPoolVol - GrandCoulee()) + (ARFullPoolVol - Arrow())) /
    ((GCFullPoolVol - reservoir_vol_df$GCOUL[control_wk]) + (ARFullPoolVol - reservoir_vol_df$ARROW[control_wk]))))
  }
  return(storage_o)
}

## Residual inflow calculation made at the start of flood control refill period
DACorrectedResidualInflow <- function() {
  if (week_in_year == 1 || is.na(start_refill_wk)) {
    DACorrectedResidualInflow_previous <<- DACorrectedResidualInflowAprAug
    DACorrectedResidualInflow_o <- DACorrectedResidualInflow_previous
  } else if (week_in_year == start_refill_wk) {
    DACorrectedResidualInflow_o <- DACorrectedResidualInflowAprAug
    DACorrectedResidualInflow_previous <<- DACorrectedResidualInflow_o
  } else {
    DACorrectedResidualInflow_o <- DACorrectedResidualInflow_previous
  }
  return(DACorrectedResidualInflow_o)
}
ICF_adj <- function() { ## Adjustment to initial controlled flow
if (DACorrectedResidualInflow() < 40e6) {
  colnum <- 2
} else if (DACorrectedResidualInflow() < 50e6) {
  colnum <- 3
} else {
  colnum <- 4
}
upper_row <- tail(which(ICF_adj_input$filled_perc >= Cat4FilledStorPerc()), 1)
lower_row <- which(ICF_adj_input$filled_perc <= Cat4FilledStorPerc())[1]
upper_adj <- ICF_adj_input[upper_row,colnum]
lower_adj <- ICF_adj_input[lower_row,colnum]
upper_perc <- ICF_adj_input$filled_perc[upper_row]
lower_perc <- ICF_adj_input$filled_perc[lower_row]
if (Cat4FilledStorPerc() == 100) {
  ICF_adj_o <- ICF_adj_input[1,colnum]
} else if (Cat4FilledStorPerc() == 0) {
  ICF_adj_o <- 0
} else {
  ICF_adj_o <- lower_adj + (upper_adj - lower_adj) / (upper_perc - lower_perc) * (Cat4FilledStorPerc() - lower_perc)
}
return(ICF_adj_o)
}
ControlledFlow <- function() { #cfs
if (is.na(start_refill_wk)) { ## For years in which the unregulated flow is never forecasted to exceed 450 kcfs
  ControlledFlow_o <- 350000
} else {
  ControlledFlow_o <- ICF() + ICF_adj()
  ControlledFlow_o <- ControlledFlow_o
}
water_df$ControlledFlow[week_counter] <<- ControlledFlow_o
return(ControlledFlow_o)
}
################### CHIEF JOSEPH DAM ##############################

ChiefJosephFlowData <- function() {
  return(max(FlowCJ))
}
CJInc <- function() {
  CJInc_o <- ChiefJosephFlowData() - GrandCouleeFlowData()
  return(CJInc_o)
}
CJCurtail <- function() { # Curtailment of water rights provisioned on flow measured at Chief Joseph
CJCurtail_0 <-  min(DemCJ, max(max(IflowCJ - GCOutflow() - CJInc(), 0)))
if (curtail_option == 1) {
  if (CJCurtail_0 > 0) {
    CJCurtail_o <- CurtCJ
  } else {
    CJCurtail_o <- 0
  }
} else if (curtail_option == 2) {
  CJCurtail_o <- CJCurtail_0
} else if (curtail_option==3) {
  CJCurtail_o <- 0
}
if (DARunoffAprSep > mainstem_rule) {
  CJCurtail_o <- 0
} else {
  CJCurtail_o <- CJCurtail_o
}
return(CJCurtail_o)
}
CJInstreamShortfall <- function() { # Columbia River flow deficit measured at Chief Joseph
CJInstreamShortfall_o = max(max(IflowCJ - GCOutflow() - CJInc(), 0))
return(CJInstreamShortfall_o)
}
CJPenLimit <- function() {
  CJPenCap <- 213000 # https://www.nws.usace.army.mil/Missions/Civil-Works/Locks-and-Dams/Chief-Joseph-Dam/-Hydropower/
  CJPenLimit_o <- CJPenCap * cfsTOafw
  return(CJPenLimit_o)
}
CJPrelim <- function() {
  CJPrelim_o <- max(0, GCPrelim() + CJInc())
  return(CJPrelim_o)
}
CJNetHead <- function() {
  CJNetHead_o <- 178 # https://www.nws.usace.army.mil/Missions/Civil-Works/Locks-and-Dams/Chief-Joseph-Dam/-Hydropower/
  return(CJNetHead_o)
}
CJPreEnergy <- function() {
  CJPreEnergy_o <- MWhr_per_ftAcFt * min(CJPrelim(), CJPenLimit()) * CJNetHead() * CJCombEfficiency
  return(CJPreEnergy_o)
}
CJIn <- function() {
  CJIn_o <- GCRelease_c + CJInc()
  return(CJIn_o)
}
CJOut <- function() {
  CJOut_o <- CJIn()
  return(CJOut_o)
}

#######################################################
