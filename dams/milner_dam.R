#------------------- MILNER DAM ----------------------#
#######################################################

MilnerFlowData <- function() {
  return(max(FlowMIL))
}
MILInc <- function() {
  MILInc_o <- MilnerFlowData() - MinidokaFlowData()
  return(MILInc_o)
}
MILPrelim <- function() {
  MILPrelim_o <- MINPrelim() + MILInc()
  return(MILPrelim_o)
}
MILIn <- function() {
  MILIn_o <- MINRelease() + MILInc()
  return(MILIn_o)
}
MILOut <- function() {
  MILOut_o <- MILIn()
  return(MILOut_o)
}

############## Middle Snake #################################

#######################################################
#------------------- BOISE SYSTEM --------------------#
#------ (ARROW ROCK, ANDERSON RANCH, LUCKY PEAK) -----#
#######################################################

BoiseFullPoolVol <- 1011000
BoiseBotVol <- 61300

InitBoise <- function() {
  InitBoise_o <- ResInitFractionFull * BoiseFullPoolVol
  return(InitBoise_o)
}
Boise <- function() {
  if (week_counter == 1) {
    Boise_o <- InitBoise()
  } else {
    Boise_o <- reservoir_vol_df$BOISE[week_counter - 1]
  }
  return(Boise_o)
}
LuckyPeakFlowData <- function() {
  return(max(FlowLUC))
}
BoiseIn <- function() {
  BoiseIn_o <- LuckyPeakFlowData()
  return(BoiseIn_o)
}
BoiseInflow <- function() {
  BoiseInflow_o <- BoiseIn()
  return(BoiseInflow_o)
}
BoiseFloodCurve <- function() {
  if (BoiseResidualInflowJanJul <= 0.5E6) {
    BoiseFloodCurve_o <- BoiseFlood_input$BoiseFlood1[week_in_year] + (BoiseResidualInflowJanJul - 0) / (0.5E6 - 0) * (BoiseFlood_input$BoiseFlood2[week_in_year] - BoiseFlood_input$BoiseFlood1[week_in_year])
  } else if (BoiseResidualInflowJanJul <= 1.0E6) {
    BoiseFloodCurve_o <- BoiseFlood_input$BoiseFlood2[week_in_year] + (BoiseResidualInflowJanJul - 0.5E6) / (1.0E6 - 0.5E6) * (BoiseFlood_input$BoiseFlood3[week_in_year] - BoiseFlood_input$BoiseFlood2[week_in_year])
  } else if (BoiseResidualInflowJanJul <= 1.5E6) {
    BoiseFloodCurve_o <- BoiseFlood_input$BoiseFlood3[week_in_year] + (BoiseResidualInflowJanJul - 1.0E6) / (1.5E6 - 1.0E6) * (BoiseFlood_input$BoiseFlood4[week_in_year] - BoiseFlood_input$BoiseFlood3[week_in_year])
  } else if (BoiseResidualInflowJanJul <= 2.0E6) {
    BoiseFloodCurve_o <- BoiseFlood_input$BoiseFlood4[week_in_year] + (BoiseResidualInflowJanJul - 1.5E6) / (2.0E6 - 1.5E6) * (BoiseFlood_input$BoiseFlood5[week_in_year] - BoiseFlood_input$BoiseFlood4[week_in_year])
  } else if (BoiseResidualInflowJanJul <= 2.5E6) {
    BoiseFloodCurve_o <- BoiseFlood_input$BoiseFlood5[week_in_year] + (BoiseResidualInflowJanJul - 2.0E6) / (2.5E6 - 2.0E6) * (BoiseFlood_input$BoiseFlood6[week_in_year] - BoiseFlood_input$BoiseFlood5[week_in_year])
  } else if (BoiseResidualInflowJanJul <= 3.0E6) {
    BoiseFloodCurve_o <- BoiseFlood_input$BoiseFlood6[week_in_year] + (BoiseResidualInflowJanJul - 2.5E6) / (3.0E6 - 2.5E6) * (BoiseFlood_input$BoiseFlood7[week_in_year] - BoiseFlood_input$BoiseFlood6[week_in_year])
  } else {
    BoiseFloodCurve_o <- BoiseFlood_input$BoiseFlood7
  }
  return(BoiseFloodCurve_o)
}
BoiseAgReq <- function() { # Total withdrawals for Boise irrigation project
IrrigationDemand <- DemBoiseSys
ConveyanceLossBoise <- 1
if (week_in_year %in% c(50:52,1:14)) {
  BoiseAgReq_o <- BoiseInflow() + IrrigationDemand * ConveyanceLossBoise
} else {
  BoiseAgReq_o <- 0
}
return(BoiseAgReq_o)
}
BoiseTopVol <- function() {
  if (TopRuleSw() == 0) {
    BoiseTopVol_o <- BoiseFloodCurve()
  } else if (TopRuleSw() == 1) {
    BoiseTopVol_o <- BoiseFullPoolVol
  } else if (TopRuleSw() == 2) {
    BoiseTopVol_o <- BoiseFlood_input$BoiseFlood1[week_in_year]
  }
  return(BoiseTopVol_o)
}
BoiseMinReq <- function() {
  LUCAvgMin <- 100
  BoiseMinReq_o <- max(BoiseAgReq(), LUCAvgMin * cfsTOafw)
  return(BoiseMinReq_o)
}
BoiseRuleReq <- function() {
  BoiseRuleReq_o <- max(max(Boise() + BoiseIn() - BoiseTopVol(), 0))
  return(BoiseRuleReq_o)
}
BoiseAvailWater <- function() {
  BoiseAvailWater_o <- max(max(Boise() + BoiseIn() - BoiseBotVol, 0))
  return(BoiseAvailWater_o)
}
BoisePrelim <- function() {
  BoisePrelim_o <- min(BoiseAvailWater(), max(BoiseRuleReq(), BoiseMinReq()))
  return(BoisePrelim_o)
}
BoiseRelLimit <- function() {
  BoiseRelLimit_o <- max(max(Boise() + BoiseInflow() - BoiseBotVol, 0))
  return(BoiseRelLimit_o)
}
BoiseDamProtectRel <- function() {
  BoiseDamProtectRel_o <- max(0, Boise() + BoiseInflow() - BoiseFullPoolVol)
  return(BoiseDamProtectRel_o)
}
BoiseDamProtectExcess <- function() {
  BoiseDamProtectExcess_o <- max(0, BoiseDamProtectRel() - BoisePrelim())
  return(BoiseDamProtectExcess_o)
}
BoiseRelease <- function() {
  BoiseRelease_o <- max(BoiseDamProtectRel(), min(BoisePrelim(), BoiseRelLimit()))
  return(BoiseRelease_o)
}
BoiseOutflow <- function() {
  BoiseOutflow_o <- BoiseRelease_c
  return(BoiseOutflow_o)
}

#######################################################
#------------------ PAYETTE SYSTEM -------------------#
#-------------- (DEADWOOD AND CASCADE) ---------------#
#######################################################

PayetteFullPoolVol <- 847192
PayetteBotVol <- 46740
PayetteAvgMin <- 700

InitPayette <- function() {
  InitPayette_o <- ResInitFractionFull * PayetteFullPoolVol
  return(InitPayette_o)
}
Payette <- function() {
  if (week_counter == 1) {
    Payette_o <- InitPayette()
  } else {
    Payette_o <- reservoir_vol_df$PAYHS[week_counter - 1]
  }
  return(Payette_o)
}
PayetteFlowData <- function() {
  return(max(FlowPAY))
}
PayetteIn <- function() {
  PayetteIn_o <- PayetteFlowData()
  return(PayetteIn_o)
}
PayetteInflow <- function() {
  PayetteInflow_o <- PayetteIn()
  return(PayetteInflow_o)
}
PayetteFloodCurve <- function() {
  if (PayetteResidualInflowJanJun <= 0.5E6) {
    PayetteFloodCurve_o <- PayetteFlood_input$PAYFlood1[week_in_year] + (PayetteResidualInflowJanJun - 0) / (0.5E6 - 0) * (PayetteFlood_input$PAYFlood2[week_in_year] - PayetteFlood_input$PAYFlood1[week_in_year])
  } else if (PayetteResidualInflowJanJun <= 1.0E6) {
    PayetteFloodCurve_o <- PayetteFlood_input$PAYFlood2[week_in_year] + (PayetteResidualInflowJanJun - 0.5E6) / (1.0E6 - 0.5E6) * (PayetteFlood_input$PAYFlood3[week_in_year] - PayetteFlood_input$PAYFlood2[week_in_year])
  } else if (PayetteResidualInflowJanJun <= 1.5E6) {
    PayetteFloodCurve_o <- PayetteFlood_input$PAYFlood3[week_in_year] + (PayetteResidualInflowJanJun - 1.0E6) / (1.5E6 - 1.0E6) * (PayetteFlood_input$PAYFlood4[week_in_year] - PayetteFlood_input$PAYFlood3[week_in_year])
  } else if (PayetteResidualInflowJanJun <= 2.0E6) {
    PayetteFloodCurve_o <- PayetteFlood_input$PAYFlood4[week_in_year] + (PayetteResidualInflowJanJun - 1.5E6) / (2.0E6 - 1.5E6) * (PayetteFlood_input$PAYFlood5[week_in_year] - PayetteFlood_input$PAYFlood4[week_in_year])
  } else if (PayetteResidualInflowJanJun <= 3.0E6) {
    PayetteFloodCurve_o <- PayetteFlood_input$PAYFlood5[week_in_year] + (PayetteResidualInflowJanJun - 2.0E6) / (3.0E6 - 2.0E6) * (PayetteFlood_input$PAYFlood6[week_in_year] - PayetteFlood_input$PAYFlood5[week_in_year])
  } else {
    PayetteFloodCurve_o <- PayetteFlood_input$PAYFlood6
  }
  return(PayetteFloodCurve_o)
}
PayetteAgReq <- function() { ## Total irrigation withdrawals for Payette system
IrrigationDemand <- DemPayette
ConveyanceLossPayette <- 1
if (week_in_year %in% c(47:52, 1:14)) {
  PayetteAgReq_o <- PayetteInflow() + IrrigationDemand * ConveyanceLossPayette
} else {
  PayetteAgReq_o <- 0
}
return(PayetteAgReq_o)
}
PayetteTopVol <- function() {
  if (TopRuleSw() == 0) {
    PayetteTopVol_o <- PayetteFloodCurve()
  } else if (TopRuleSw() == 1) {
    PayetteTopVol_o <- PayetteFullPoolVol
  } else if (TopRuleSw() == 2) {
    PayetteTopVol_o <- PayetteFlood_input$PAYFlood1[week_in_year]
  }
  return(PayetteTopVol_o)
}
PayetteMinReq <- function() {
  PayetteMinReq_o <- max(PayetteAgReq(), PayetteAvgMin * cfsTOafw)
  return(PayetteMinReq_o)
}
PayetteRuleReq <- function() {
  PayetteRuleReq_o <- max(max(Payette() + PayetteIn() - PayetteTopVol(), 0))
  return(PayetteRuleReq_o)
}
PayetteAvailWater <- function() {
  PayetteAvailWater_o <- max(max(Payette() + PayetteIn() - PayetteBotVol, 0))
  return(PayetteAvailWater_o)
}
PayettePrelim <- function() {
  PayettePrelim_o <- min(PayetteAvailWater(), max(PayetteRuleReq(), PayetteMinReq()))
  return(PayettePrelim_o)
}
PayetteRelLimit <- function() {
  PayetteRelLimit_o <- max(max(Payette() + PayetteInflow() - PayetteBotVol, 0))
  return(PayetteRelLimit_o)
}
PayetteDamProtectRel <- function() {
  PayetteDamProtectRel_o <- max(0, Payette() + PayetteInflow() - PayetteFullPoolVol)
  return(PayetteDamProtectRel_o)
}
PayetteDamProtectExcess <- function() {
  PayetteDamProtectExcess_o <- max(0, PayetteDamProtectRel() - PayettePrelim())
  return(PayetteDamProtectExcess_o)
}
PayetteRelease <- function() {
  PayetteRelease_o <- max(min(PayettePrelim(), PayetteRelLimit()), PayetteDamProtectRel())
  return(PayetteRelease_o)
}
PayetteOutflow <- function() {
  PayetteOutflow_o <- PayetteRelease_c
  return(PayetteOutflow_o)
}

#######################################################
