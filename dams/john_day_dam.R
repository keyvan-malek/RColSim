#------------------ JOHN DAY DAM ---------------------#
#######################################################

JohnDayFlowData <- function() {
  return(max(FlowJD))
}
JDInc <- function() {
  JDInc_o <- JohnDayFlowData() - McNaryFlowData()
  return(JDInc_o)
}
JDCurtail <- function() {
  JDCurtail_0 <- min(DemJD, max(max(IflowJD - MCNOut() - JDInc(), 0)))
  if (curtail_option == 1) {
    if (JDCurtail_0 > 0) {
      JDCurtail_o <- CurtJD
    } else {
      JDCurtail_o <- 0
    }
  } else if (curtail_option == 2) {
    JDCurtail_o <- JDCurtail_0
  } else if (curtail_option == 3) {
    JDCurtail_o <- 0
  }
  if (DARunoffAprSep > mainstem_rule) {
    JDCurtail_o <- 0
  } else {
    JDCurtail_o <- JDCurtail_o
  }
  return(JDCurtail_o)
}
JDInstreamShortfall <- function() {
  JDInstreamShortfall_o = max(max(IflowJD - MCNOut() - JDInc(), 0))
  return(JDInstreamShortfall_o)
}
JDPenLimit <- function() {
  JDPenCap <- 322000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/John-Day/#top
  JDPenLimit_o <- JDPenCap * cfsTOafw
  return(JDPenLimit_o)
}
JDPrelim <- function() {
  JDPrelim_o <- MCNPrelim() + JDInc()
  return(JDPrelim_o)
}
JDNetHead <- function() {
  JDNetHead_o <- 100 # forebay operating range: 260 - 268 ft. Tailwater ~ 165 ft.
  return(JDNetHead_o)
}
JDPreEnergy <- function() {
  JDPreEnergy_o <- MWhr_per_ftAcFt * min(JDPrelim(), JDPenLimit()) * JDNetHead() * JDCombEfficiency
  return(JDPreEnergy_o)
}
JDIn <- function() {
  JDIn_o <- MCNOut() + JDInc()
  return(JDIn_o)
}
JDOut <- function() {
  JDOut_o <- JDIn()
  return(JDOut_o)
}

#######################################################
#---------------- PELTON COMPLEX ---------------------#
#---- (PELTON, ROUND BUTTE, AND REREGULATING DAM) ----#
#######################################################

PELFullPoolVol <- 558270
PELBotVol <- 249700

InitPEL <- function() {
  InitPEL_o <- ResInitFractionFull * PELFullPoolVol
  return(InitPEL_o)
}
Pelton <- function() {
  if (week_counter == 1) {
    Pelton_o <- InitPEL()
  } else {
    Pelton_o <- reservoir_vol_df$PELTO[week_counter - 1]
  }
  return(Pelton_o)
}
PeltonFlowData <- function() {
  return(max(FlowPEL))
}
PELPenLimit <- function() {
  PELPenCap <- 14000 # 2005 BiOp for Pelton Round Butte Hydroelectric Project (FERC No. 2030)
  PELPenLimit_o <- PELPenCap * cfsTOafw
  return(PELPenLimit_o)
}
PELNetHead <- function() {
  PELNetHead_o <- 385 # back-calculated using installed capacity of 367 MW, flow of 14000 cfs and efficiency of 0.8
  return(PELNetHead_o)
}
PELIn <- function() {
  PELIn_o <- PeltonFlowData()
  return(PELIn_o)
}
PELInflow <- function() {
  PELInflow_o <- PELIn()
  return(PELInflow_o)
}

##################### Pelton Round Butte max and min releases ##########

PELMinReq <- function() {
  PELMin <- PEL_target_min$flow[week_in_year] # FERC P-2030
  PELMinReq_o <- PELMin * cfsTOafw
  return(PELMinReq_o)
}
PELDamProtectRel <- function() {
  PELDamProtectRel_o <- max(0, PeltonFlowData() + PELInflow() - PELFullPoolVol)
  return(PELDamProtectRel_o)
}
PELRelLimit <- function() {
  PELRelLimit_o <- max(max(Pelton() + PELInflow() - PELBotVol, 0))
  return(PELRelLimit_o)
}
PELAvailWater <- function() {
  PELAvailWater_o <- max(0, Pelton() + PELIn() - PELBotVol)
  return(PELAvailWater_o)
}

############## Pelton Round Butte rule curve ##################

PELFloodCurve <- function() {
  PELFloodCurve_o <- PELFlood_input$PELFlood[week_in_year]
  return(PELFloodCurve_o)
}
PELTopVol <- function() {
  if (TopRuleSw() == 0) {
    PELTopVol_o <- PELFloodCurve()
  } else if (TopRuleSw() == 1) {
    PELTopVol_o <- PELFullPoolVol
  } else if (TopRuleSw() == 2) {
    PELTopVol_o <- PELFlood_input$PELFlood[week_in_year]
  }
  return(PELTopVol_o)
}
PELRuleReq <- function() {
  PELRuleReq_o <- max(max(Pelton() + PELIn() - PELTopVol(), 0))
  return(PELRuleReq_o)
}
PELPrelim <- function() {
  PELPrelim_o <- min(PELAvailWater(), max(PELRuleReq(), PELMinReq()))
  return(PELPrelim_o)
}

############ Pelton Round Butte energy ######################

PELPreEnergy <- function() {
  PELPreEnergy_o <- MWhr_per_ftAcFt * min(PELPrelim(), PELPenLimit()) * PELNetHead() * PELCombEfficiency
  return(PELPreEnergy_o)
}

################## Pelton Round Butte final release ###################

PELRelease <- function() {
  PELRelease_o <- max(PELDamProtectRel(), min(PELPrelim(), PELRelLimit()))
  return(PELRelease_o)
}
PELOutflow <- function() {
  PELOutflow_o <- PELRelease_c
  return(PELOutflow_o)
}

#######################################################
#-------------------- THE DALLES ---------------------#
#######################################################

DallesFlowData <- function() {
  return(max(FlowDA))
}
DAInc <- function() {
  DAInc_o <- DallesFlowData() - JohnDayFlowData() - PeltonFlowData()
  return(DAInc_o)
}
DAPenLimit <- function() {
  DAPenCap <- 375000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/The-Dalles/#top
  DAPenLimit_o <- DAPenCap * cfsTOafw
  return(DAPenLimit_o)
}
DANetHead <- function() {
  DANetHead_o <- 80.1 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=the%20dalles
  return(DANetHead_o)
}
DAIn <- function() {
  DAIn_o <- JDOut() + PELRelease() + DAInc()
  return(DAIn_o)
}
DACurtail <- function() {
  DACurtail_0 <- min(DemDA, max(max(IflowDA - JDOut() - PELOutflow() - DAInc(), 0)))
  if (curtail_option == 1) {
    DACurtail_o <- ifelse(DACurtail_0 > 0, CurtDA, 0)
  } else if (curtail_option == 2) {
    DACurtail_o <- DACurtail_0
  } else if (curtail_option == 3) {
    DACurtail_o <- 0
  }
  if (DARunoffAprSep > mainstem_rule) {
    DACurtail_o <- 0
  } else {
    DACurtail_o <- DACurtail_o
  }
  return(DACurtail_o)
}
DAInstreamShortfall <- function() {
  DAInstreamShortfall_o = max(max(IflowDA - JDOut() - PELOutflow() - DAInc(), 0))
  return(DAInstreamShortfall_o)
}
DAPrelim <- function() {
  DAPrelim_o <- JDPrelim() + PELPrelim() + DAInc()
  return(DAPrelim_o)
}
DAPreEnergy <- function() {
  DAPreEnergy_o <- MWhr_per_ftAcFt * min(DAPrelim(), DAPenLimit()) * DANetHead() * DACombEfficiency
  return(DAPreEnergy_o)
}
DAOut <- function() {
  DAOut_o <- DAIn()
  return(DAOut_o)
}

#######################################################
