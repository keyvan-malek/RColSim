#--------------------- OWYHEE DAM --------------------#
#######################################################

OWYFullPoolVol <- 1120000
OWYBotVol <- 405000

InitOWY <- function() {
  InitOWY_o <- ResInitFractionFull * OWYFullPoolVol
  return(InitOWY_o)
}
Owyhee <- function() {
  if (week_counter == 1) {
    OWY_o <- InitOWY()
  } else {
    OWY_o <- reservoir_vol_df$OWYHE[week_counter - 1]
  }
  return(OWY_o)
}
OwyheeFlowData <- function() {
  return(max(FlowOWY))
}
OWYIn <- function() {
  OWYIn_o <- OwyheeFlowData()
  return(OWYIn_o)
}
OWYInflow <- function() {
  OWYInflow_o <- OWYIn()
  return(OWYInflow_o)
}
OWYFloodCurve <- function() {
  if (OwyheeResidualInflowJanMay <= 0.5E6) {
    OWYFloodCurve_o <- OWYFlood_input$OWYFlood1[week_in_year] + (OwyheeResidualInflowJanMay - 0) / (0.5E6 - 0) * (OWYFlood_input$OWYFlood2[week_in_year] - OWYFlood_input$OWYFlood1[week_in_year])
  } else if (OwyheeResidualInflowJanMay <= 1.0E6) {
    OWYFloodCurve_o <- OWYFlood_input$OWYFlood2[week_in_year] + (OwyheeResidualInflowJanMay - 0.5E6) / (1.0E6 - 0.5E6) * (OWYFlood_input$OWYFlood3[week_in_year] - OWYFlood_input$OWYFlood2[week_in_year])
  } else if (OwyheeResidualInflowJanMay <= 2.0E6) {
    OWYFloodCurve_o <- OWYFlood_input$OWYFlood3[week_in_year] + (OwyheeResidualInflowJanMay - 1.0E6) / (2.0E6 - 1.0E6) * (OWYFlood_input$OWYFlood4[week_in_year] - OWYFlood_input$OWYFlood3[week_in_year])
  } else if (OwyheeResidualInflowJanMay <= 3.0E6) {
    OWYFloodCurve_o <- OWYFlood_input$OWYFlood4[week_in_year] + (OwyheeResidualInflowJanMay - 2.0E6) / (3.0E6 - 2.0E6) * (OWYFlood_input$OWYFlood5[week_in_year] - OWYFlood_input$OWYFlood4[week_in_year])
  } else {
    OWYFloodCurve_o <- OWYFlood_input$OWYFlood5
  }
  return(OWYFloodCurve_o)
}
OWYAgReq <- function() { ## Total withdrawals by Owyhee Irrigation District
IrrigationDemand <- DemOwyhee
ConveyanceLossOWY <- 1
if (week_in_year %in% c(45:52, 1:14)) {
  OWYAgReq_o <- OWYInflow() + IrrigationDemand * ConveyanceLossOWY
} else {
  OWYAgReq_o <- 0
}
return(OWYAgReq_o)
}
OWYTopVol <- function() {
  if (TopRuleSw() == 0) {
    OWYTopVol_o <- OWYFloodCurve()
  } else if (TopRuleSw() == 1) {
    OWYTopVol_o <- OWYFullPoolVol
  } else if (TopRuleSw() == 2) {
    OWYTopVol_o <- OWYFlood_input$OWYFlood1[week_in_year]
  }
  return(OWYTopVol_o)
}
OWYMinReq <- function() {
  OWYAvgMin <- 0
  OWYMinReq_o <- max(OWYAgReq(), OWYAvgMin * cfsTOafw)
  return(OWYMinReq_o)
}
OWYRuleReq <- function() {
  OWYRuleReq_o <- max(max(Owyhee() + OWYIn() - OWYTopVol(), 0))
  return(OWYRuleReq_o)
}
OWYAvailWater <- function() {
  OWYAvailWater_o <- max(max(Owyhee() + OWYIn() - OWYBotVol, 0))
  return(OWYAvailWater_o)
}
OWYPrelim <- function() {
  OWYPrelim_o <- min(OWYAvailWater(), max(OWYRuleReq(), OWYMinReq()))
  return(OWYPrelim_o)
}
OWYRelLimit <- function() {
  OWYRelLimit_o <- max(max(Owyhee() + OWYInflow() - OWYBotVol, 0))
  return(OWYRelLimit_o)
}
OWYDamProtectRel <- function() {
  OWYDamProtectRel_o <- max(0, Owyhee() + OWYInflow() - OWYFullPoolVol)
  return(OWYDamProtectRel_o)
}
OWYDamProtectExcess <- function() {
  OWYDamProtectExcess_o <- max(0, OWYDamProtectRel() - OWYPrelim())
  return(OWYDamProtectExcess_o)
}
OWYRelease <- function() {
  OWYRelease_o <- max(min(OWYPrelim(), OWYRelLimit()), OWYDamProtectRel())
  return(OWYRelease_o)
}
OWYOutflow <- function() {
  OWYOutflow_o <- OWYRelease_c
  return(OWYOutflow_o)
}
OWYOut <- function() {
  OWYOut_o <- OWYIn()
  return(OWYOut_o)
}

#######################################################
