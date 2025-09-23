# Ririe_Dam.R
# Reservoir operations functions for Ririe
# Handles inflow/outflow calculations and reservoir management operations

#--------------------- RIRIE DAM ---------------------#
#######################################################

RIRFullPoolVol <- 100500
RIRBotVol <- 19960

InitRIR <- function() {
  InitRIR_o <- ResInitFractionFull * RIRFullPoolVol
  return(InitRIR_o)
}
Ririe <- function() {
  if (week_counter == 1) {
    Ririe_o <- InitRIR()
  } else {
    Ririe_o <- reservoir_vol_df$RIRDM[week_counter - 1]
  }
  return(Ririe_o)
}
RirieFlowData <- function() {
  return(max(FlowRIR))
}
RIRIn <- function() {
  RIRIn_o <- RirieFlowData()
  return(RIRIn_o)
}
RIRInflow <- function() {
  RIRInflow_o <- RIRIn()
  return(RIRInflow_o)
}
RIRFloodCurve <- function() {
  if (RirieResidualInflowJanJun <= 0.05E6) {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood1[week_in_year] + (RirieResidualInflowJanJun - 0) / (0.05E6 - 0) * (RIRFlood_input$RIRFlood2[week_in_year] - RIRFlood_input$RIRFlood1[week_in_year])
  } else if (RirieResidualInflowJanJun <= 0.08E6) {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood2[week_in_year] + (RirieResidualInflowJanJun - 0.05E6) / (0.08E6 - 0.05E6) * (RIRFlood_input$RIRFlood3[week_in_year] - RIRFlood_input$RIRFlood2[week_in_year])
  } else if (RirieResidualInflowJanJun <= 0.1E6) {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood3[week_in_year] + (RirieResidualInflowJanJun - 0.08E6) / (0.1E6 - 0.08E6) * (RIRFlood_input$RIRFlood4[week_in_year] - RIRFlood_input$RIRFlood3[week_in_year])
  } else if (RirieResidualInflowJanJun <= 0.15E6) {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood4[week_in_year] + (RirieResidualInflowJanJun - 0.1E6) / (0.15E6 - 0.1E6) * (RIRFlood_input$RIRFlood5[week_in_year] - RIRFlood_input$RIRFlood4[week_in_year])
  } else if (RirieResidualInflowJanJun <= 0.2E6) {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood5[week_in_year] + (RirieResidualInflowJanJun - 0.15E6) / (0.2E6 - 0.15E6) * (RIRFlood_input$RIRFlood6[week_in_year] - RIRFlood_input$RIRFlood5[week_in_year])
  } else if (RirieResidualInflowJanJun <= 0.25E6) {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood6[week_in_year] + (RirieResidualInflowJanJun - 0.2E6) / (0.25E6 - 0.2E6) * (RIRFlood_input$RIRFlood7[week_in_year] - RIRFlood_input$RIRFlood6[week_in_year])
  } else if (RirieResidualInflowJanJun <= 0.3E6) {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood7[week_in_year] + (RirieResidualInflowJanJun - 0.25E6) / (0.3E6 - 0.25E6) * (RIRFlood_input$RIRFlood8[week_in_year] - RIRFlood_input$RIRFlood7[week_in_year])
  } else {
    RIRFloodCurve_o <- RIRFlood_input$RIRFlood8
  }
  return(RIRFloodCurve_o)
}
RIRAgReq <- function() {
  if (week_in_year %in% c(45:52, 1:14)) {
    RIRAgReq_o <- RIRInflow() + UpSnakeAgReq() * Ririe() / (Palisades() + Jackson() + IslandPark() + Ririe())
  } else {
    RIRAgReq_o <- 0
  }
  return(RIRAgReq_o)
}
RIRTopVol <- function() {
  if (TopRuleSw() == 0) {
    RIRTopVol_o <- RIRFloodCurve()
  } else if (TopRuleSw() == 1) {
    RIRTopVol_o <- RIRFullPoolVol
  } else if (TopRuleSw() == 2) {
    RIRTopVol_o <- RIRFlood_input$RIRFlood1[week_in_year]
  }
  return(RIRTopVol_o)
}
RIRMinReq <- function() {
  RIRAvgMin <- 0
  RIRMinReq_o <- max(RIRAgReq(), RIRAvgMin * cfsTOafw)
  return(RIRMinReq_o)
}
RIRRuleReq <- function() {
  RIRRuleReq_o <- max(max(Ririe() + RIRIn() - RIRTopVol(), 0))
  return(RIRRuleReq_o)
}
RIRAvailWater <- function() {
  RIRAvailWater_o <- max(0, Ririe() + RIRIn() - RIRBotVol)
  return(RIRAvailWater_o)
}
RIRPrelim <- function() {
  RIRPrelim_o <- min(RIRAvailWater(), max(RIRRuleReq(), RIRMinReq()))
  return(RIRPrelim_o)
}
RIRRelLimit <- function() {
  RIRRelLimit_o <- max(max(Ririe() + RIRInflow() - RIRBotVol, 0))
  return(RIRRelLimit_o)
}
RIRDamProtectRel <- function() {
  RIRDamProtectRel_o <- max(0, Ririe() + RIRInflow() - RIRFullPoolVol)
  return(RIRDamProtectRel_o)
}
RIRDamProtectExcess <- function() {
  RIRDamProtectExcess_o <- max(0, RIRDamProtectRel() - RIRPrelim())
  return(RIRDamProtectExcess_o)
}
RIRRelease <- function() {
  RIRRelease_o <- max(min(RIRPrelim(), RIRRelLimit()), RIRDamProtectRel())
  return(RIRRelease_o)
}
RIROutflow <- function() {
  RIROutflow_o <- RIRRelease_c
  return(RIROutflow_o)
}
RIROut <- function() {
  RIROut_o <- RIRIn()
  return(RIROut_o)
}

#######################################################
