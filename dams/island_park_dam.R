#---------------- ISLAND PARK DAM --------------------#
#######################################################

IPFullPoolVol <- 135205
IPBotVol <- 0
IPAvgMin <- 30

InitIP <- function() {
  InitIP_o <- ResInitFractionFull * IPFullPoolVol
  return(InitIP_o)
}
IslandPark <- function() {
  if (week_counter == 1) {
    IslandPark_o <- InitIP()
  } else {
    IslandPark_o <- reservoir_vol_df$IPARK[week_counter - 1]
  }
  return(IslandPark_o)
}
IslandParkFlowData <- function() {
  return(max(FlowIP))
}
IPIn <- function() {
  IPIn_o <- IslandParkFlowData()
  return(IPIn_o)
}
IPInflow <- function() {
  IPInflow_o <- IPIn()
  return(IPInflow_o)
}
IPFloodCurve <- function() {
  if (HenryResidualInflowJanJun <= 0.1E6) {
    IPFloodCurve_o <- IPFlood_input$IPFlood1[week_in_year] + (HenryResidualInflowJanJun - 0) / (0.1E6 - 0) * (IPFlood_input$IPFlood2[week_in_year] - IPFlood_input$IPFlood1[week_in_year])
  } else if (HenryResidualInflowJanJun <= 0.15E6) {
    IPFloodCurve_o <- IPFlood_input$IPFlood2[week_in_year] + (HenryResidualInflowJanJun - 0.1E6) / (0.15E6 - 0.1E6) * (IPFlood_input$IPFlood3[week_in_year] - IPFlood_input$IPFlood2[week_in_year])
  } else if (HenryResidualInflowJanJun <= 0.2E6) {
    IPFloodCurve_o <- IPFlood_input$IPFlood3[week_in_year] + (HenryResidualInflowJanJun - 0.15E6) / (0.2E6 - 0.15E6) * (IPFlood_input$IPFlood4[week_in_year] - IPFlood_input$IPFlood3[week_in_year])
  } else if (HenryResidualInflowJanJun <= 0.25E6) {
    IPFloodCurve_o <- IPFlood_input$IPFlood4[week_in_year] + (HenryResidualInflowJanJun - 0.2E6) / (0.25E6 - 0.2E6) * (IPFlood_input$IPFlood5[week_in_year] - IPFlood_input$IPFlood4[week_in_year])
  } else if (HenryResidualInflowJanJun <= 0.28E6) {
    IPFloodCurve_o <- IPFlood_input$IPFlood5[week_in_year] + (HenryResidualInflowJanJun - 0.25E6) / (0.28E6 - 0.25E6) * (IPFlood_input$IPFlood6[week_in_year] - IPFlood_input$IPFlood5[week_in_year])
  } else if (HenryResidualInflowJanJun <= 0.31E6) {
    IPFloodCurve_o <- IPFlood_input$IPFlood6[week_in_year] + (HenryResidualInflowJanJun - 0.28E6) / (0.31E6 - 0.28E6) * (IPFlood_input$IPFlood7[week_in_year] - IPFlood_input$IPFlood6[week_in_year])
  } else {
    IPFloodCurve_o <- IPFlood_input$IPFlood7[week_in_year]
  }
  return(IPFloodCurve_o)
}
IPAgReq <- function() {
  if (week_in_year %in% c(45:52, 1:14)) {
    IPAgReq_o <- IPInflow() + UpSnakeAgReq() * IslandPark() / (Palisades() + Jackson() + IslandPark() + Ririe())
  } else {
    IPAgReq_o <- 0
  }
  return(IPAgReq_o)
}
IPTopVol <- function() {
  if (TopRuleSw() == 0) {
    IPTopVol_o <- IPFloodCurve()
  } else if (TopRuleSw() == 1) {
    IPTopVol_o <- IPFullPoolVol
  } else if (TopRuleSw() == 2) {
    IPTopVol_o <- IPFlood_input$IPFlood1[week_in_year]
  }
  return(IPTopVol_o)
}
IPMinReq <- function() {
  IPMinReq_o <- max(IPAgReq(), IPAvgMin * cfsTOafw)
  return(IPMinReq_o)
}
IPRuleReq <- function() {
  IPRuleReq_o <- max(max(IslandPark() + IPIn() - IPTopVol(), 0))
  return(IPRuleReq_o)
}
IPAvailWater <- function() {
  IPAvailWater_o <- max(0, IslandPark() + IPIn() - IPBotVol)
  return(IPAvailWater_o)
}
IPPrelim <- function() {
  IPPrelim_o <- min(IPAvailWater(), max(IPRuleReq(), IPMinReq()))
  return(IPPrelim_o)
}
IPRelLimit <- function() {
  IPRelLimit_o <- max(max(IslandPark() + IPInflow() - IPBotVol, 0))
  return(IPRelLimit_o)
}
IPDamProtectRel <- function() {
  IPDamProtectRel_o <- max(0, IslandPark() + IPInflow() - IPFullPoolVol)
  return(IPDamProtectRel_o)
}
IPDamProtectExcess <- function() {
  IPDamProtectExcess_o <- max(0, IPDamProtectRel() - IPPrelim())
  return(IPDamProtectExcess_o)
}
IPRelease <- function() {
  IPRelease_o <- max(min(IPPrelim(), IPRelLimit()), IPDamProtectRel())
  return(IPRelease_o)
}
IPOutflow <- function() {
  IPOutflow_o <- IPRelease_c
  return(IPOutflow_o)
}
IPOut <- function() {
  IPOut_o <- IPIn()
  return(IPOut_o)
}

#######################################################
