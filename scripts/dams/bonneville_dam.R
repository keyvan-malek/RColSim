# Bonneville_Dam.R
# Reservoir operations functions for Bonneville
# Handles inflow/outflow calculations and reservoir management operations

#---------------- BONNEVILLE DAM ---------------------#
#######################################################

BonnevilleFlowData <- function() {
  return(max(FlowBON))
}
BONInc <- function() {
  BONInc_o <- BonnevilleFlowData() - DallesFlowData()
  return(BONInc_o)
}
BONPenLimit <- function() {
  BONPenCap <- 288000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Bonneville/#top
  BONPenLimit_o <- BONPenCap * cfsTOafw
  return(BONPenLimit_o)
}
BONNetHead <- function() {
  BONNetHead_o <- BONNetHead_input$BONNetHead[week_in_year]
  return(BONNetHead_o)
}
BONPrelim <- function() {
  BONPrelim_o <- DAPrelim() + BONInc()
  return(BONPrelim_o)
}
BONPreEnergy <- function() {
  BONPreEnergy_o <- MWhr_per_ftAcFt * min(BONPrelim(), BONPenLimit()) * BONNetHead() * BONCombEfficiency
  return(BONPreEnergy_o)
}
BONIn <- function() {
  BONIn_o <- DAOut() + BONInc()
  return(BONIn_o)
}
BONOut <- function() {
  BONOut_o <- BONIn()
  return(BONOut_o)
}

########################### Flow targets #######################

# Biological Opinions ------------------------------------------------------
# The Corps designates Libby, and the USBR designates Grand Coulee and Hungry Horse to contribute to the Federal Columbia River Power System (FCRPS)
# goal of meeting the following BiOp flow objectives:
# Spring (4/10 - 6/30)     Priest Rapids (135 kcfs) (1998 FCRPS BiOp)
# Spring (4/20 - 6/30)**   McNary Dam (220-260 kcfs) (1995 FCRPS BiOp)
# Summer (7/1 - 8/31)      McNary Dam (200 kcfs) (1995 FCRPS BiOp)
# Interpolation between 220 and 260 kcfs is based upon Corps of Engineers data submittal, which bases targets on forecasts of Jan-July flow at The Dalles.

##### McNary Flow target
# Variable minimum flow for McNary based upon forecasted inflow to the Dalles, for the period of April 20 to June 30.
# Flow varies linearly between 220 and 260 kcfs based on inflows of 85 to 105 MAF at the Dalles

McNaryFlowTarget <- function() {
  if (DARunoffJanJul <= 85e6) {
    McNaryFlowTarget_o <- McNaryFlowTarget_input$Low[week_in_year]
  } else if (DARunoffJanJul <= 105e6) {
    McNaryFlowTarget_o <- McNaryFlowTarget_input$Low[week_in_year] + (DARunoffJanJul - 85e6) / (105e6 - 85e6) * (McNaryFlowTarget_input$High[week_in_year] - McNaryFlowTarget_input$Low[week_in_year])
  } else {
    McNaryFlowTarget_o <- McNaryFlowTarget_input$High[week_in_year]
  }
  return(McNaryFlowTarget_o)
}
McNaryFlowDeficit <- function() {
  McNaryFlowDeficit_o <- max(0, McNaryFlowTarget() * cfsTOafw - MCNPrelim())
  return(McNaryFlowDeficit_o)
}

# Grand Coulee is designated to meet the BiOP chum flow requirement at Bonneville Dam of 125-160 kcfs from November 1 to April 10 (2020 NMFS BiOp)
# The variable target is based on water volume forecasts
BonnevilleFlowTarget <- function() {
  if (DARunoffJanJul <= 85e6) {
    BonnevilleFlowTarget_o <- BonnevilleFlowTarget_input$Low[week_in_year]
  } else if (DARunoffJanJul <= 105e6) {
    BonnevilleFlowTarget_o <- BonnevilleFlowTarget_input$Low[week_in_year] + (DARunoffJanJul - 85e6) / (105e6 - 85e6) * (BonnevilleFlowTarget_input$High[week_in_year] - BonnevilleFlowTarget_input$Low[week_in_year])
  } else {
    BonnevilleFlowTarget_o <- BonnevilleFlowTarget_input$High[week_in_year]
  }
  return(BonnevilleFlowTarget_o)
}
BONFlowDeficit <- function() {
  if (Chum_Q_Switch() == 1) {
    BONFlowDeficit_o <- max(0, BonnevilleFlowTarget() * cfsTOafw - BONPrelim())
  } else {
    BONFlowDeficit_o <- 0
  }
  BONFlowDeficit_c <<- BONFlowDeficit_o
  return(BONFlowDeficit_o)
}

# The 2000 BiOp sets a flow objective between 85-100 kcfs from April 2 to June 20 and 50-55 kcfs from June 21 to August 31
# The target varies based on April-July runoff forecast at Lower Granite
LowerGraniteFlowTarget <- function() {
  if (LGRunoffAprJul < 16e6) {
    LowerGraniteFlowTarget_o <- LowerGraniteFlowTarget_input$Low[week_in_year]
  } else if (LGRunoffAprJul <= 20e6 && week_in_year %in% 36:47) { # spring
  LowerGraniteFlowTarget_o <- LowerGraniteFlowTarget_input$Low[week_in_year] + (LGRunoffAprJul - 16e6) / (20e6 - 16e6) * (LowerGraniteFlowTarget_input$High[week_in_year] - LowerGraniteFlowTarget_input$Low[week_in_year])
} else if (LGRunoffAprJul > 20e6 && week_in_year %in% 36:47) {
  LowerGraniteFlowTarget_o <- LowerGraniteFlowTarget_input$High[week_in_year]
} else if (LGRunoffAprJul <= 28e6 && week_in_year %in% c(48:52, 1:5)) { # summer
LowerGraniteFlowTarget_o <- LowerGraniteFlowTarget_input$Low[week_in_year] + (LGRunoffAprJul - 16e6) / (28e6 - 16e6) * (LowerGraniteFlowTarget_input$High[week_in_year] - LowerGraniteFlowTarget_input$Low[week_in_year])
} else if (LGRunoffAprJul > 28e6) {
  LowerGraniteFlowTarget_o <- LowerGraniteFlowTarget_input$High[week_in_year]
} else {
  LowerGraniteFlowTarget_o <- 0
}
return(LowerGraniteFlowTarget_o)
}
TotalRelForLowerGranite <- function() {
  TotalRelForLowerGranite_o <- max(0, LowerGraniteFlowTarget() * cfsTOafw - LGPrelim())
  return(TotalRelForLowerGranite_o)
}

################################################################################################

TotalMcNarySharedWater <- function() {
  TotalMcNarySharedWater_o <- MIMcNarySharedWater() + DUMcNarySharedWater() + ARMcNarySharedWater() + GCMcNarySharedWater() + HHMcNarySharedWater() + LBMcNarySharedWater()
  return(TotalMcNarySharedWater_o)
}
TotalBONSharedWater <- function() {
  TotalBONSharedWater_o <- ARBONSharedWater() + GCBONSharedWater()
  return(TotalBONSharedWater_o)
}
TotalEnergyFromMcNarySups <- function() { # Total hydropower generated by water released to meet fish flow targets
TotalEnergyFromMcNarySups_o <- MIMcNarySupEnergy() + DUMcNarySupEnergy() + HHMcNarySupEnergy() + LBMcNarySupEnergy()
return(TotalEnergyFromMcNarySups_o)
}
TotalEnergyFromLGSups <- function() {
  TotalEnergyFromLGSups_o <- DWLGSupEnergy() + BRLGSupEnergy()
  return(TotalEnergyFromLGSups_o)
}
TotalFloodSpace <- function() {
  TotalFloodSpace_o <- MIFloodMult * MIFloodSpace() + ARFloodSpace() + GCFloodSpace() + LBFloodSpace() + HHFloodSpace()
  return(TotalFloodSpace_o)
}
TotalFloodRelSharedWater <- function() {
  TotalFloodRelSharedWater_o <- ARFloodRelSharedWater() + GCFloodRelSharedWater()
  if (is.na(water_df$TotalFloodRelSharedWater[week_counter])) {
    water_df$TotalFloodRelSharedWater[week_counter] <<- TotalFloodRelSharedWater_o
  }
  return(TotalFloodRelSharedWater_o)
}
DACombSup <- function() {
  DACombSup_o <- BRCombSup() + DWCombSup() + GCCombSup()
  return(DACombSup_o)
}
DAPreOutflow <- function() {
  DAPreOutflow_o <- DAPrelim() + DACombSup() + TotalDamProtectExcess()
  return(DAPreOutflow_o)
}
TotalRelReducReq <- function() {
  TotalRelReducReq_o <- max(0, DAPreOutflow() + TotalFloodRelease() - ControlledFlow() * cfsTOafw)
  TotalRelReducReq_c <<- TotalRelReducReq_o
  if (is.na(water_df$TotalRelReducReq[week_counter])) {
    water_df$TotalRelReducReq[week_counter] <<- TotalRelReducReq_c
  }
  return(TotalRelReducReq_o)
}
MinFloodRelReq <- function() {
  if (is.na(start_refill_wk)) {
    MinFloodRelReq_o <- 0
  } else if (week_in_year %in% start_refill_wk:52) {
    MinFloodRelReq_o <- max(max(ControlledFlow() * cfsTOafw - DAPreOutflow(), 0))
  } else {
    MinFloodRelReq_o <- 0
  }
  MinFloodRelReq_c <<- MinFloodRelReq_o
  return(MinFloodRelReq_o)
}
TotalDamProtectExcess <- function() {
  TotalDamProtectExcess_o <- GCDamProtectExcess() + GCCombUpProtect() + BRDamProtectExcess() +
  BRCombUpProtect() + DWDamProtectExcess()
  TotalDamProtectExcess_c <<- TotalDamProtectExcess_o
  return(TotalDamProtectExcess_o)
}
TotalFloodRelease <- function() {
  TotalFloodRelease_o <- GCMinFloodRelReq()
  return(TotalFloodRelease_o)
}

########################################### TOTAL ENERGY #########################################

MicaGroupPreEnergy <- function() {
  MicaGroupPreEnergy_o <- MIPreEnergy() + REVPreEnergy() + ARPreEnergy()
  return(MicaGroupPreEnergy_o)
}
DworshakGroupPreEnergy <- function() {
  DworshakGroupPreEnergy_o <- DWPreEnergy() + LGPreEnergy() + LIGPreEnergy() + LMPreEnergy() + IHPreEnergy()
  return(DworshakGroupPreEnergy_o)
}
LowerColGroupPreEnergy <- function() {
  LowerColGroupPreEnergy_o <- MCNPreEnergy() + JDPreEnergy() + DAPreEnergy() + BONPreEnergy()
  return(LowerColGroupPreEnergy_o)
}
AlbeniFallsGroupPreEnergy <- function() {
  AFGroupPreEnergy_o <- AFPreEnergy() + BCPreEnergy() + BDPreEnergy()
  return(AFGroupPreEnergy_o)
}
GrandCouleeGroupPreEnergy <- function() {
  GrandCouleeGroupPreEnergy_o <- GCPreEnergy() + CJPreEnergy() + WEPreEnergy() + RRPreEnergy() + RIPreEnergy() + WAPreEnergy() + PRPreEnergy()
  return(GrandCouleeGroupPreEnergy_o)
}
BrownleeGroupPreEnergy <- function() {
  BrownleeGroupPreEnergy_o <- BRPreEnergy() + OXPreEnergy() + HCPreEnergy()
  return(BrownleeGroupPreEnergy_o)
}
KerrGroupPreEnergy <- function() {
  KerrGroupPreEnergy_o <- KEPreEnergy() + TFPreEnergy() + NOXPreEnergy() + CBPreEnergy()
  return(KerrGroupPreEnergy_o)
}
TotalCoordPreEnergy <- function() { # Hydropower that could be generated from preliminary and fish flow releases at all dams
TotalCoordPreEnergy_o <- TotalEnergyFromMcNarySups() + TotalEnergyFromLGSups() + GCFishSupEnergy() + ARFishSupEnergy() + HungryHorsePreEnergy() + LibbyPreEnergy() + MicaGroupPreEnergy() + KerrGrPreEnergy() +
AlbeniFallsGroupPreEnergy() + GrandCouleeGroupPreEnergy() + DworshakGroupPreEnergy() + LowerColGroupPreEnergy() +
BrownleeGroupPreEnergy() + CHPreEnergy() + PELPreEnergy()
return(TotalCoordPreEnergy_o)
}
TotalEnergyContent <- function() {
  TotalEnergyContent_o <- DWEnergyContent() + GCEnergyContent() + HHEnergyContent() + LBEnergyContent() + MIEnergyContent() +
  AREnergyContent() + DUEnergyContent() + CLEnergyContent() + KerrEnergyContent() + AFEnergyContent() + BREnergyContent()
  return(TotalEnergyContent_o)
}
Total_ORCEnergyContent <- function() {
  Total_ORCEnergyContent_o <- DW_ORCEnergyContent() + GC_ORCEnergyContent() + HH_ORCEnergyContent() + LB_ORCEnergyContent() + MI_ORCEnergyContent() +
  AR_ORCEnergyContent() + DU_ORCEnergyContent() + CL_ORCEnergyContent() + Kerr_ORCEnergyContent() + AF_ORCEnergyContent() + BR_ORCEnergyContent()
  return(Total_ORCEnergyContent_o)
}
FirmFraction <- function() { # Adjustment to the average firm energy load throughout the year.
FirmFraction_o <- FirmFraction_input[week_in_year, 2]
return(FirmFraction_o)
}
AvgFirmLoad <- function() { # Average firm energy requirement
#AvgFirmLoad_o <- 1.12e6 # Step2 studies
AvgFirmLoad_o <- 1984816 # MW-hr/wk, Step1 studies
return(AvgFirmLoad_o)
}

# Average firm energy target. This value is multiplied by the seasonal fraction to yield the firm energy target for each month.  Units MW-hr/week.
FirmEnergyTarget <- function() {
  FirmEnergyTarget_o <- AvgFirmLoad() * (FirmFraction() + 1)
  return(FirmEnergyTarget_o)
}

FirmEnergyDeficit <- function() { # Basin-wide deficit in firm energy production
FirmEnergyDeficit_o <- max(0, FirmEnergyTarget() - TotalCoordPreEnergy_c)
return(FirmEnergyDeficit_o)
}

TotalNFEnergyContent <- function() { # Total non-firm energy content
TotalNFEnergyContent_o <- ARNFEnergyContent() + DUNFEnergyContent() + DWNFEnergyContent() +
GCNFEnergyContent() + HHNFEnergyContent() + LBNFEnergyContent() + MINFEnergyContent()
return(TotalNFEnergyContent_o)
}
NonFirmEnergyDeficit <- function() {
  NonFirmEnergyDeficit_o <- max(0, NonFirmEnergyTarget() - max(0, (TotalCoordPreEnergy_c - FirmEnergyTarget())))
  return(NonFirmEnergyDeficit_o)
}
NonFirmFraction <- function() { # Fraction of the average non-firm energy load.
NonFirmFraction_o <- NonFirmFraction_input[week_in_year, 2]
return(NonFirmFraction_o)
}
AltNonFirmLoad <- function() {
  AltNonFirmLoad_o <- AltNonFirmLoad_input[week_in_year, 2]
  return(AltNonFirmLoad_o)
}
AvgNonFirmLoad <- function() { # Yearly average non-firm energy load.  Units MWhr.
AvgNonFirmLoad_o <- 1.06e6  ## Capacity of PNW-PSW intertie * 0.8 (average load/peak)
return(AvgNonFirmLoad_o)
}
NonFirmEnergyTarget <- function() {
  NonFirmEnergyTarget_o <- AvgNonFirmLoad() * (NonFirmFraction() + 1)
  return(NonFirmEnergyTarget_o)
}

#### Actual Hydropower production

MicaGroupEnergy <- function() {
  MicaGroupEnergy_o <- MIEnergyProduction() + REVEnergyProduction() + AREnergyProduction()
  return(MicaGroupEnergy_o)
}
DworshakGroupEnergy <- function() {
  DworshakGroupEnergy_o <- DWEnergyProduction() + LGEnergyProduction() + LIGEnergyProduction() + LMEnergyProduction() + IHEnergyProduction()
  return(DworshakGroupEnergy_o)
}
LowerColumbiaEnergy <- function() {
  LowerColEnergy_o <- MCNEnergyProduction() + JDEnergyProduction() + DAEnergyProduction() + BONEnergyProduction()
  return(LowerColEnergy_o)
}
AlbeniFallsGroupEnergy <- function() {
  AlbeniFallsGroupEnergy_o <- AFEnergyProduction() + BCEnergyProduction() + BDEnergyProduction()
  return(AlbeniFallsGroupEnergy_o)
}
GrandCouleeGroupEnergy <- function() {
  GrandCouleeGroupEnergy_o <- GCEnergyProduction() + CJEnergyProduction() + WEEnergyProduction() + RREnergyProduction() + RIEnergyProduction() + WAEnergyProduction() + PREnergyProduction()
  return(GrandCouleeGroupEnergy_o)
}
BrownleeGroupEnergy <- function() {
  BrownleeGroupEnergy_o <- BREnergyProduction() + OXEnergyProduction() + HCEnergyProduction()
  return(BrownleeGroupEnergy_o)
}
KerrGroupEnergy <- function() {
  KerrGroupEnergy_o <- KEEnergyProduction() + TFEnergyProduction() + NOXEnergyProduction() + CBEnergyProduction()
  return(KerrGroupEnergy_o)
}


MIEnergyProduction <- function() {
  MIEnergyProduction_o <-  MWhr_per_ftAcFt * min(MIOutflow(), MIPenLimit()) * MINetHead() * MICombEfficiency
  return(MIEnergyProduction_o)
}
REVEnergyProduction <- function() {
  REVEnergyProduction_o <- MWhr_per_ftAcFt * min(REVOut(), REVPenLimit()) * REVNetHead() * RevCombEfficiency
  return(REVEnergyProduction_o)
}
AREnergyProduction <- function() {
  AREnergyProduction_o <- MWhr_per_ftAcFt * min(AROutflow(), ARPenLimit()) * ARNetHead() * ARCombEfficiency
  return(AREnergyProduction_o)
}
DWEnergyProduction <- function() {
  DWEnergyProduction_o <-  MWhr_per_ftAcFt * min(DWOutflow(), DWPenLimit()) * DWNetHead() * DWCombEfficiency
  return(DWEnergyProduction_o)
}
LGEnergyProduction <- function() {
  LGEnergyProduction_o <- MWhr_per_ftAcFt * min(LGOut(), LGPenLimit()) * LGNetHead() * LGCombEfficiency
  return(LGEnergyProduction_o)
}
LIGEnergyProduction <- function() {
  LIGEnergyProduction_o <- MWhr_per_ftAcFt * min(LIGOut(), LIGPenLimit()) * LIGNetHead() * LIGCombEfficiency
  return(LIGEnergyProduction_o)
}
LMEnergyProduction <- function() {
  LMEnergyProduction_o <-  MWhr_per_ftAcFt * min(LMOut(), LMPenLimit()) * LMNetHead() * LMCombEfficiency
  return(LMEnergyProduction_o)
}
IHEnergyProduction <- function() {
  IHEnergyProduction_o <- MWhr_per_ftAcFt * min(IHOut(), IHPenLimit()) * IHNetHead() * IHCombEfficiency
  return(IHEnergyProduction_o)
}
MCNEnergyProduction <- function() {
  MCNEnergyProduction_o <- MWhr_per_ftAcFt * min(MCNOut(), MCNPenLimit()) * MCNetHead() * MCNCombEfficiency
  return(MCNEnergyProduction_o)
}
JDEnergyProduction <- function() {
  JDEnergyProduction_o <- MWhr_per_ftAcFt * min(JDOut(), JDPenLimit()) * JDNetHead() * JDCombEfficiency
  return(JDEnergyProduction_o)
}
DAEnergyProduction <- function() {
  DAEnergyProduction_o <- MWhr_per_ftAcFt * min(DAOut(), DAPenLimit()) * DANetHead() * DACombEfficiency
  return(DAEnergyProduction_o)
}
BONEnergyProduction <- function() {
  BONEnergyProduction_o <- MWhr_per_ftAcFt * min(BONOut(), BONPenLimit()) * BONNetHead() * BONCombEfficiency
  return(BONEnergyProduction_o)
}
AFEnergyProduction <- function() {
  AFEnergyProduction_o <- MWhr_per_ftAcFt * min(AFOutflow(), AFPenLimit()) * AFNetHead() * AFCombEfficiency
  return(AFEnergyProduction_o)
}
BCEnergyProduction <- function() {
  BCEnergyProduction_o <- MWhr_per_ftAcFt * min(BCOut(), BCPenLimit()) * BCNetHead() * BCCombEfficiency
  return(BCEnergyProduction_o)
}
BDEnergyProduction <- function() {
  BDEnergyProduction_o <- MWhr_per_ftAcFt * min(BDOut(), BDPenLimit()) * BDNetHead() * BDCombEfficiency
  return(BDEnergyProduction_o)
}
GCEnergyProduction <- function() {
  GCEnergyProduction_o <- MWhr_per_ftAcFt * min(GCOutflow(), GCPenLimit()) * GCNetHead() * GCCombEfficiency
  return(GCEnergyProduction_o)
}
CJEnergyProduction <- function() {
  CJEnergyProduction_o <- MWhr_per_ftAcFt * min(CJOut(), CJPenLimit()) * CJNetHead() * CJCombEfficiency
  return(CJEnergyProduction_o)
}
WEEnergyProduction <- function() {
  WEEnergyProduction_o <- MWhr_per_ftAcFt * min(WEOut(), WEPenLimit()) * WENetHead() * WECombEfficiency
  return(WEEnergyProduction_o)
}
RREnergyProduction <- function() {
  RREnergyProduction_o <- MWhr_per_ftAcFt * min(RROut(), RRPenLimit()) * RRNetHead() * RRCombEfficiency
  return(RREnergyProduction_o)
}
RIEnergyProduction <- function() {
  RIEnergyProduction_o <- MWhr_per_ftAcFt * min(RIOut(), RIPenLimit()) * RINetHead() * RICombEfficiency
  return(RIEnergyProduction_o)
}
WAEnergyProduction <- function() {
  WAEnergyProduction_o <- MWhr_per_ftAcFt * min(WAOut(), WAPenLimit()) * WANetHead() * WACombEfficiency
  return(WAEnergyProduction_o)
}
PREnergyProduction <- function() {
  PREnergyProduction_o <- MWhr_per_ftAcFt * min(PROut(), PRPenLimit()) * PRNetHead() * PRCombEfficiency
  return(PREnergyProduction_o)
}
BREnergyProduction <- function() {
  BREnergyProduction_o <- MWhr_per_ftAcFt * min(BROutflow(), BRPenLimit()) * BRNetHead() * BRCombEfficiency
  return(BREnergyProduction_o)
}
OXEnergyProduction <- function() {
  OXEnergyProduction_o <- MWhr_per_ftAcFt * min(OXOut(), OXPenLimit()) * OXNetHead() * OXCombEfficiency
  return(OXEnergyProduction_o)
}
HCEnergyProduction <- function() {
  HCEnergyProduction_o <- MWhr_per_ftAcFt * min(HCOut(), HCPenLimit()) * HCNetHead() * HCCombEfficiency
  return(HCEnergyProduction_o)
}
KEEnergyProduction <- function() {
  KEEnergyProduction_o <- MWhr_per_ftAcFt * min(KEOutflow(), KEPenLimit()) * KENetHead() * KECombEfficiency
  return(KEEnergyProduction_o)
}
TFEnergyProduction <- function() {
  TFEnergyProduction_o <- MWhr_per_ftAcFt * min(TFOut(), TFPenLimit()) * TFNetHead() * TFCombEfficiency
  return(TFEnergyProduction_o)
}
NOXEnergyProduction <- function() {
  NOXEnergyProduction_o <- MWhr_per_ftAcFt * min(NOXOut(), NOXPenLimit()) * NOXNetHead() * NOXCombEfficiency
  return(NOXEnergyProduction_o)
}
CBEnergyProduction <- function() {
  CBEnergyProduction_o <- MWhr_per_ftAcFt * min(CBOut(), CBPenLimit()) * CBNetHead() * CBCombEfficiency
  return(CBEnergyProduction_o)
}
HHEnergyProduction <- function() {
  HungryHorseEnergy_o <- MWhr_per_ftAcFt * min(HHOutflow(), HHPenLimit()) * HHNetHead() * HHCombEfficiency
  return(HungryHorseEnergy_o)
}
LBEnergyProduction <- function() {
  LibbyEnergy_o <- MWhr_per_ftAcFt * min(LBOutflow(), LBPenLimit()) * LBNetHead() * LBCombEfficiency
  return(LibbyEnergy_o)
}
CHEnergyProduction <- function() {
  ChelanEnergy_o <- MWhr_per_ftAcFt * min(CHOutflow(), CHPenLimit()) * CHNetHead() * CHCombEfficiency
  return(ChelanEnergy_o)
}
PELEnergyProduction <- function() {
  PeltonEnergy_o <- MWhr_per_ftAcFt * min(PELOutflow(), PELPenLimit()) * PELNetHead() * PELCombEfficiency
  return(PeltonEnergy_o)
}
MaxSystemEnergy <- function() { # Hydropower production for entire CRB, excluding upper Snake River
MaxSystemEnergy_o <- HHEnergyProduction() + LBEnergyProduction() + MicaGroupEnergy() + KerrGroupEnergy() +
AlbeniFallsGroupEnergy() + GrandCouleeGroupEnergy() + DworshakGroupEnergy() + LowerColumbiaEnergy()  +
BrownleeGroupEnergy() + CHEnergyProduction() + PELEnergyProduction()
MaxSystemEnergy_c <<- MaxSystemEnergy_o
return(MaxSystemEnergy_o)
}
######## find which operation year we are in, from weekly data
year_from_weekly <<- function() {
  all_years <- unique(input_file$DamYear)
  year_from_weekly_o <- min(which(all_years == input_file$DamYear[week_counter]), length(all_years) - 1)
  return(year_from_weekly_o)
}
