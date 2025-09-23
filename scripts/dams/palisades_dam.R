# Palisades_Dam.R
# Reservoir operations functions for Palisades
# Handles inflow/outflow calculations and reservoir management operations

#------------------ PALISADES DAM ------------------#
#####################################################

PALFullPoolVol <- 1401000
PALBotVol <- 201000
PALAvgMin <- 700 # minimum average monthly flow at Neely, ID gauge near American Falls from historical data

InitPAL <- function() {
  InitPAL_o <- ResInitFractionFull * PALFullPoolVol
  return(InitPAL_o)
}
Palisades <- function() {
  if (week_counter == 1) {
    Palisades_o <- InitPAL()
  } else {
    Palisades_o <- reservoir_vol_df$PALIS[week_counter - 1]
  }
  return(Palisades_o)
}
PalisadesFlowData <- function() {
  return(max(FlowPAL))
}
PALInc <- function() {
  PALInc_o <- PalisadesFlowData() - JacksonFlowData()
  return(PALInc_o)
}
PALIn <- function() {
  PALIn_o <- JLPrelim() + PALInc()
  return(PALIn_o)
}
PALInflow <- function() {
  PALInflow_o <- JLRelease() + PALInc()
  return(PALInflow_o)
}
PALFloodCurve <- function() {
  if (HeiseResidualInflowJanJul <= 1.0E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood1[week_in_year] + (HeiseResidualInflowJanJul - 0) / (1.0E6 - 0) * (PALFlood_input$PALFlood2[week_in_year] - PALFlood_input$PALFlood1[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 1.5E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood2[week_in_year] + (HeiseResidualInflowJanJul - 1.0E6) / (1.5E6 - 1.0E6) * (PALFlood_input$PALFlood3[week_in_year] - PALFlood_input$PALFlood2[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 2.0E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood3[week_in_year] + (HeiseResidualInflowJanJul - 1.5E6) / (2.0E6 - 1.5E6) * (PALFlood_input$PALFlood4[week_in_year] - PALFlood_input$PALFlood3[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 2.5E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood4[week_in_year] + (HeiseResidualInflowJanJul - 2.0E6) / (2.5E6 - 2.0E6) * (PALFlood_input$PALFlood5[week_in_year] - PALFlood_input$PALFlood4[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 3.0E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood5[week_in_year] + (HeiseResidualInflowJanJul - 2.5E6) / (3.0E6 - 2.5E6) * (PALFlood_input$PALFlood6[week_in_year] - PALFlood_input$PALFlood5[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 3.5E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood6[week_in_year] + (HeiseResidualInflowJanJul - 3.0E6) / (3.5E6 - 3.0E6) * (PALFlood_input$PALFlood7[week_in_year] - PALFlood_input$PALFlood6[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 4.0E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood7[week_in_year] + (HeiseResidualInflowJanJul - 3.5E6) / (4.0E6 - 3.5E6) * (PALFlood_input$PALFlood8[week_in_year] - PALFlood_input$PALFlood7[week_in_year])
  } else if (HeiseResidualInflowJanJul <= 5.0E6) {
    PALFloodCurve_o <- PALFlood_input$PALFlood8[week_in_year] + (HeiseResidualInflowJanJul - 4.0E6) / (5.0E6 - 4.0E6) * (PALFlood_input$PALFlood9[week_in_year] - PALFlood_input$PALFlood8[week_in_year])
  } else {
    PALFloodCurve_o <- PALFlood_input$PALFlood9[week_in_year]
  }
  return(PALFloodCurve_o)
}
PALAgReq <- function() {
  if (week_in_year %in% c(45:52,1:14)) {
    PALAgReq_o <- PALInflow() + UpSnakeAgReq() * Palisades() / (Palisades() + Jackson() + IslandPark() + Ririe())
  } else {
    PALAgReq_o <- 0
  }
  return(PALAgReq_o)
}
PALTopVol <- function() {
  if (TopRuleSw() == 0) {
    PALTopVol_o <- PALFloodCurve()
  } else if (TopRuleSw() == 1) {
    PALTopVol_o <- PALFullPoolVol
  } else if (TopRuleSw() == 2) {
    PALTopVol_o <- PALFlood_input$PALFlood1[week_in_year]
  }
  return(PALTopVol_o)
}
PALMinReq <- function() {
  PALMinReq_o <- max(PALAgReq(), PALAvgMin * cfsTOafw)
  return(PALMinReq_o)
}
PALRuleReq <- function() {
  PALRuleReq_o <- max(max(Palisades() + PALIn() - PALTopVol(), 0))
  return(PALRuleReq_o)
}
PALAvailWater <- function() {
  PALAvailWater_o <- max(0, Palisades() + PALIn() - PALBotVol)
  return(PALAvailWater_o)
}
PALPrelim <- function() {
  PALPrelim_o <- min(PALAvailWater(), max(PALRuleReq(), PALMinReq()))
  return(PALPrelim_o)
}
PALRelLimit <- function() {
  PALRelLimit_o <- max(max(Palisades() + PALInflow() - PALBotVol, 0))
  return(PALRelLimit_o)
}
PALDamProtectRel <- function() {
  PALDamProtectRel_o <- max(0, Palisades() + PALInflow() - PALFullPoolVol)
  return(PALDamProtectRel_o)
}
PALCombUpProtect <- function() {
  inflow <- PALIn() + JLDamProtectExcess()
  outflow <- PALPrelim()
  PALCombUpProtect_o <- max(min(Palisades() + inflow - outflow - PALTopVol(), JLDamProtectExcess()), 0)
  PALCombUpProtect_c <<- PALCombUpProtect_o
  return(PALCombUpProtect_o)
}
PALDamProtectExcess <- function() {
  PALDamProtectExcess_o <- max(0, PALDamProtectRel() - PALPrelim() - PALCombUpProtect())
  return(PALDamProtectExcess_o)
}
PALRelease <- function() {
  PALRelease_o <- max(min(PALPrelim() + PALCombUpProtect(), PALRelLimit()), PALDamProtectRel())
  return(PALRelease_o)
}
PALOutflow <- function() {
  PALOutflow_o <- PALRelease_c
  return(PALOutflow_o)
}
PALOut <- function() {
  PALOut_o <- PALIn()
  return(PALOut_o)
}

#######################################################
