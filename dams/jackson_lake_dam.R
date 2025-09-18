#---------------- JACKSON LAKE DAM -----------------#
#####################################################

JLFullPoolVol <- 850000
JLBotVol <- 0
JLAvgMin <- 100 # minimum monthly average gauge flow

InitJL <- function() {
  InitJL_o <- ResInitFractionFull * JLFullPoolVol
  return(InitJL_o)
}
Jackson <- function() {
  if (week_counter == 1) {
    Jackson_o <- InitJL()
  } else {
    Jackson_o <- reservoir_vol_df$JLAKE[week_counter - 1]
  }
  return(Jackson_o)
}
JacksonFlowData <- function() {
  return(max(FlowJL))
}
JLIn <- function() {
  JLIn_o <- JacksonFlowData()
  return(JLIn_o)
}
JLInflow <- function() {
  JLInflow_o <- JLIn()
  return(JLInflow_o)
}
JLFloodCurve <- function() {
  if (HeiseResidualInflowJanJul <= 1.0E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood1[week_in_year] + (HeiseResidualInflowJanJul - 0) / (1.0E6 - 0) * (JLFlood_input$JLFlood2[week_in_year] - JLFlood_input$JLFlood1[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 1.5E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood2[week_in_year] + (HeiseResidualInflowJanJul - 1.0E6) / (1.5E6 - 1.0E6) * (JLFlood_input$JLFlood3[week_in_year] - JLFlood_input$JLFlood2[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 2.0E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood3[week_in_year] + (HeiseResidualInflowJanJul - 1.5E6) / (2.0E6 - 1.5E6) * (JLFlood_input$JLFlood4[week_in_year] - JLFlood_input$JLFlood3[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 2.5E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood4[week_in_year] + (HeiseResidualInflowJanJul - 2.0E6) / (2.5E6 - 2.0E6) * (JLFlood_input$JLFlood5[week_in_year] - JLFlood_input$JLFlood4[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 3.0E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood5[week_in_year] + (HeiseResidualInflowJanJul - 2.5E6) / (3.0E6 - 2.5E6) * (JLFlood_input$JLFlood6[week_in_year] - JLFlood_input$JLFlood5[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 3.5E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood6[week_in_year] + (HeiseResidualInflowJanJul - 3.0E6) / (3.5E6 - 3.0E6) * (JLFlood_input$JLFlood7[week_in_year] - JLFlood_input$JLFlood6[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 4.0E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood7[week_in_year] + (HeiseResidualInflowJanJul - 3.5E6) / (4.0E6 - 3.5E6) * (JLFlood_input$JLFlood8[week_in_year] - JLFlood_input$JLFlood7[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 5.0E6) {
    JLFloodCurve_o <- JLFlood_input$JLFlood8[week_in_year] + (HeiseResidualInflowJanJul - 4.0E6) / (5.0E6 - 4.0E6) * (JLFlood_input$JLFlood9[week_in_year] - JLFlood_input$JLFlood8[week_in_year])
  } else {
    JLFloodCurve_o <- JLFlood_input$JLFlood9[week_in_year]
  }
  return(JLFloodCurve_o)
}
UpSnakeAgReq <- function() { ## Total irrigation withdrawals upstream of American Falls Dam
CLUpSnake <- 1
IrrigationDemand <- DemUpSnake
UpSnakeAgReq_o = IrrigationDemand * CLUpSnake
return(UpSnakeAgReq_o)
}
JLAgReq <- function() {
  if (week_in_year %in% c(45:52, 1:14)) {
    JLAgReq_o <- JLInflow() + UpSnakeAgReq() * Jackson() / (Palisades() + Jackson() + IslandPark() + Ririe())
  } else {
    JLAgReq_o <- 0
  }
  return(JLAgReq_o)
}
JLTopVol <- function() {
  if (TopRuleSw() == 0) {
    JLTopVol_o <- JLFloodCurve()
  } else if (TopRuleSw() == 1) {
    JLTopVol_o <- JLFullPoolVol
  } else if (TopRuleSw() == 2) {
    JLTopVol_o <- JLFlood_input$JLFlood1[week_in_year]
  }
  return(JLTopVol_o)
}
JLMinReq <- function() {
  JLMinReq_o <- max(JLAgReq(), JLAvgMin * cfsTOafw)
  return(JLMinReq_o)
}
JLRuleReq <- function() {
  JLRuleReq_o <- max(0, Jackson() + JLIn() - JLTopVol())
  return(JLRuleReq_o)
}
JLAvailWater <- function() {
  JLAvailWater_o <- max(0, Jackson() + JLIn() - JLBotVol)
  return(JLAvailWater_o)
}
JLPrelim <- function() {
  JLPrelim_o <- min(JLAvailWater(), max(JLRuleReq(), JLMinReq()))
  return(JLPrelim_o)
}
JLRelLimit <- function() {
  JLRelLimit_o <- max(0, Jackson() + JLInflow() - JLBotVol)
  return(JLRelLimit_o)
}
JLDamProtectRel <- function() {
  JLDamProtectRel_o <- max(0, Jackson() + JLInflow() - JLFullPoolVol)
  return(JLDamProtectRel_o)
}
JLDamProtectExcess <- function() {
  JLDamProtectExcess_o <- max(0, JLDamProtectRel() - JLPrelim())
  return(JLDamProtectExcess_o)
}
JLRelease <- function() {
  JLRelease_o <- max(min(JLPrelim(), JLRelLimit()), JLDamProtectRel())
  return(JLRelease_o)
}
JLOutflow <- function() {
  JLOutflow_o <- JLRelease()
  return(JLOutflow_o)
}
JLOut <- function() {
  JLOut_o <- JLIn()
  return(JLOut_o)
}

#####################################################
