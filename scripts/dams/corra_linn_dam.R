# Corra_Linn_Dam.R
# Reservoir operations functions for Corra Linn
# Handles inflow/outflow calculations and reservoir management operations

#------------------ CORRA LINN DAM -------------------#
#######################################################

CLFullPoolVol <- 817000 # Volume corresponding to 1745.3 ft. pool elevation.
CLBotVol <- 145200 # Volume corresponding to 1739.1 ft. pool elevation.
CLAvgMin <- 5000

InitCL <- function() {
  InitCL_o <- ResInitFractionFull * CLFullPoolVol
  return(InitCL_o)
}
CorraLinn <- function() {
  if (week_counter == 1) {
    CorraLinn_o <- InitCL()
  } else {
    CorraLinn_o <- reservoir_vol_df$CORRA[week_counter - 1]
  }
  return(CorraLinn_o)
}
CorraLinnFlowData <- function() {
  return(max(FlowCL))
}
CLInc <- function() {
  CLInc_o <- CorraLinnFlowData() - DuncanFlowData() - BonnersFerryFlowData()
  return(CLInc_o)
}
CLElev_ft <- function() {
  upper_vol <- CL_elev_input$Volume[which(CL_elev_input$Volume >= CorraLinn())[1]]
  lower_vol <- CL_elev_input$Volume[tail(which(CL_elev_input$Volume <= CorraLinn())[1],1)]
  upper_el <- CL_elev_input$Elevation[which(CL_elev_input$Volume >= CorraLinn())[1]]
  lower_el <- CL_elev_input$Elevation[tail(which(CL_elev_input$Volume <= CorraLinn())[1],1)]
  if (is.na(lower_el)) {
    CLElev_ft_o <- min(CL_elev_input$Elevation)
  } else if (is.na(upper_el)) {
    CLElev_ft_o <- max(CL_elev_input$Elevation)
  } else if (lower_el == upper_el) {
    CLElev_ft_o <- lower_el
  } else {
    CLElev_ft_o <- lower_el + (CorraLinn() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
  }
  return(CLElev_ft_o)
}
CLNetHead <- function() {
  CLTailElev <- 1693 # head of 16 m (Fortis BC Inc., 2013)
  CLNetHead_o <- CLElev_ft() - CLTailElev
  return(CLNetHead_o)
}
CLPenLimit <- function() {
  CLPenCap <- 12600 # (Fortis BC Inc., 2013)
  CLPenLimit_o <- CLPenCap * cfsTOafw
  return(CLPenLimit_o)
}
CLIn <- function() {
  CLIn_o <- DUPrelim() + LBPrelim() + (CorraLinnFlowData() - DuncanFlowData() - LibbyFlowData())
  return(CLIn_o)
}
CLPreInflow <- function() {
  CLPreInflow_o <- DURelease() + max(min(LBPrelim() + LBCombSup_c, LBRelLimit()), LBDamProtectRel()) +
  (CorraLinnFlowData() - LibbyFlowData() - DuncanFlowData())
  CLPreInflow_c <<- CLPreInflow_o
  return(CLPreInflow_o)
}
CLInflow <- function() {
  CLInflow_o <- DURelease_c + BONFOut() + CLInc()
  return(CLInflow_o)
}

################# Corra Linn max and min releases ####################################

CLMinReq <- function() {
  CLMinReq_o <- CLAvgMin * cfsTOafw
  return(CLMinReq_o)
}
CLDamProtectRel <- function() {
  CLDamProtectRel_o <- max(0, CorraLinn() + CLPreInflow() - CLFullPoolVol)
  return(CLDamProtectRel_o)
}
CLRelLimit <- function() {
  CLRelLimit_o <- max(max(CorraLinn() + CLPreInflow_c - CLBotVol, 0))
  return(CLRelLimit_o)
}
CLAvailWater <- function() {
  CLAvailWater_o <- max(0, CorraLinn() + CLIn() - CLBotVol)
  return(CLAvailWater_o)
}

################## Corra Linn rule curves ####################################

#### CLRuleVol
# Corra Linn operates to International Joint Commission, 1938 Kootenay Lake Order.  This agreement specifies reservoir elevations at five dates throughout the year.
# To include this curve in the model,  intermediate data points have been added for other months based on a linear interpolation between points.
# The January 7 storage value was assumed to occur on Dec 31 for simplicity.  Aug 31 corresponds to month 1.

CLFloodCurve <- function() {
  CLIJCRuleCurve_o <- CLFlood_input[week_in_year, 2]
  return(CLIJCRuleCurve_o)
}
CLTopVol <- function() {
  if (TopRuleSw() == 0) {
    CLTopVol_o <- CLFloodCurve()
  } else if (TopRuleSw() == 1) {
    CLTopVol_o <- CLFullPoolVol
  } else if (TopRuleSw() == 2) {
    CLTopVol_o <- CLFullPoolVol
  }
  return(CLTopVol_o)
}
CLRuleReq <- function() {
  CLRuleReq_o <- max(max(CorraLinn() + CLIn() - CLTopVol(), 0))
  return(CLRuleReq_o)
}
CLPrelim <- function() {
  CLPrelim_o <- min(CLAvailWater(), max(CLRuleReq(), CLMinReq()))
  return(CLPrelim_o)
}
CLCriticalCurveMin <- function() {
  if (CriticalCurveSw == 1) {
    CLCriticalCurveMin_o <- CLCriticalCurve_input$CRC1[week_in_year]
  } else if (CriticalCurveSw == 2) {
    CLCriticalCurveMin_o <- CLCriticalCurve_input$CRC2[week_in_year]
  } else if (CriticalCurveSw == 3) {
    CLCriticalCurveMin_o <- CLCriticalCurve_input$CRC3[week_in_year]
  } else if (CriticalCurveSw == 4) {
    CLCriticalCurveMin_o <- CLCriticalCurve_input$CRC4[week_in_year]
  }
  return(CLCriticalCurveMin_o)
}
CLCriticalCurve <- function() {
  CLCriticalCurve_o <- CLCriticalCurve_input$CRC1[week_in_year]
  return(CLCriticalCurve_o)
}
CL_ORC <- function() {
  CL_ORC_o <- min(CLCriticalCurve(), CLFloodCurve())
  return(CL_ORC_o)
}

########## Corra Linn energy #########################

CLPreEnergy <- function() {
  CLPreEnergy_o <- MWhr_per_ftAcFt * min(CLPrelim(), CLPenLimit()) * CLNetHead() * CLCombEfficiency
  return(CLPreEnergy_o)
}
CLSharedWater <- function() {
  CLSharedWater_o <- max(0, CorraLinn() + CLIn() - CLPrelim() - CLCriticalCurveMin())
  return(CLSharedWater_o)
}
CLDownStreamHead <- function() {
  CLDownStreamHead_o <- TotalGCHead()
  return(CLDownStreamHead_o)
}
CLEnergyContent <- function() {
  CLEnergyContent_o <- CLSharedWater() * (CLNetHead() + CLDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(CLEnergyContent_o)
}
CL_ORCSharedWater <- function() {
  CL_ORCSharedWater_o <- max(0, CorraLinn() + CLIn() - CLPrelim() - CL_ORC())
  return(CL_ORCSharedWater_o)
}
CL_ORCEnergyContent <- function() {
  CL_ORCEnergyContent_o <- CL_ORCSharedWater() * (CLNetHead() + CLDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
  return(CL_ORCEnergyContent_o)
}
CLFirmEngSup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
    if (TotalEnergyContent_c == 0) {
      CLFirmEngSup_o <- 0
    } else {
      CLFirmEngSup_o <- (CLEnergyContent() + CL_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
    }
  } else if (Total_ORCEnergyContent_c == 0) {
    CLFirmEngSup_o <- 0
  } else {
    CLFirmEngSup_o <- CL_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
  }
  return(CLFirmEngSup_o)
}
CLNFEnergyContent <- function() {
  CLNFEnergyContent_o <- max(0, CL_ORCEnergyContent() - CLFirmEngSup())
  return(CLNFEnergyContent_o)
}
CLNonFirmEngSup <- function() {
  if (TotalNFEnergyContent_c == 0) {
    CLNonFirmEngSup_o <- 0
  } else {
    CLNonFirmEngSup_o <- CLNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
  }
  return(CLNonFirmEngSup_o)
}
CLFirmEngSupReq <- function() {
  CLFirmEngSupReq_o <- min(CLPenLimit(), CLFirmEngSup() / (MWhr_per_ftAcFt * (CLNetHead() + CLDownStreamHead()) * CLCombEfficiency))
  return(CLFirmEngSupReq_o)
}
CLNonFirmSupReq <- function() {
  if (NonFirmEnergySw() == 1) {
    CLNonFirmSupReq_o <- min(CLPenLimit(), (CLFirmEngSup() + CLNonFirmEngSup()) / (MWhr_per_ftAcFt * (CLNetHead() + CLDownStreamHead()) * CLCombEfficiency))
  } else {
    CLNonFirmSupReq_o <- 0
  }
  return(CLNonFirmSupReq_o)
}
CLEnergySup <- function() {
  if (UseTotalEnergyContentForFirm() == 1) {
    CLEnergySup_o <- max(min(CLFirmEngSupReq(), CLSharedWater()), min(CLNonFirmSupReq(), CL_ORCSharedWater()))
  } else {
    CLEnergySup_o <- max(min(CLFirmEngSupReq(), CL_ORCSharedWater()), min(CLNonFirmSupReq(), CL_ORCSharedWater()))
  }
  return(CLEnergySup_o)
}
CLCombUpSup <- function() {
  if (DUCombSup_c == -9999) {
    CLCombUpSup_o <- DUCombSup() + LBCombSup()
  } else {
    CLCombUpSup_o <- DUCombSup_c + LBCombSup_c
  }
  return(CLCombUpSup_o)
}
CLCombSup <- function() {
  CLCombSup_o <- CLEnergySup() + CLCombUpSup()
  CLCombSup_c <<- CLCombSup_o
  return(CLCombSup_o)
}
CLCombUpProtect <- function() {
  outflow <- CLPrelim() + CLCombSup_c
  CLCombUpProtect_o <- max(min(DUDamProtectExcess() + LBDamProtectExcess(),
  CorraLinn() + CLPreInflow() - outflow - CL_ORC()), 0)
  CLCombUpProtect_c <<- CLCombUpProtect_o
  return(CLCombUpProtect_o)
}
CLDamProtectExcess <- function() {
  CLDamProtectExcess_o <- max(0, CLDamProtectRel() - CLPrelim() - CLCombSup_c - CLCombUpProtect_c)
  return(CLDamProtectExcess_o)
}

########### Corra Linn final release #################

CLRelease <- function() {
  CLRelease_o <- max(min(CLPrelim() + CLCombSup() + CLCombUpProtect(), CLRelLimit()), CLDamProtectRel())
  CLRelease_c <<- CLRelease_o
  return(CLRelease_o)
}
CLOutflow <- function() {
  CLOutflow <- CLRelease_c
  return(CLOutflow)
}

#######################################################
