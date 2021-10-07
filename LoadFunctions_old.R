######################### Constants ##########

cfsTOafw <- 1.9834 * 7 # cfs to acre-ft per week conversion
MWhr_per_ftAcFt <- 1.02246e-3 # Convert head times flow rate (ft-acre-ft) to energy (MW-hr).

########################  MICAA DAM   #########################

InitMILink <- 20075520
ResInitFractionFull <- 0
MIAvgMin <- 3000 # Minimum allowable release from Mica (cfs)
MIBotVol <- 10580000 # Approximate volume cooresponding to 2364.8 ft of elevation (1.058e7 acre ft).  The lower limit of operating range in the Columbia River Treaty.
# Minimum pool value is 2,320 ft of elevation, or 0.80035e7 acre ft
##### MIFullPoolVol # Approximate volume associated with 2475 ft of elevation.  This is the normal full pool elevation.
MIFullPoolVol <- function() {
	MIFullPool <- 20075520
	return(MIFullPool)
}
MicaReservoir <- function() { ### Storage volume in most upstream dam
	if (week_counter == 1) {
		if (InitialConditionSwitch == 0) { # Default = 2
			MicaRes <- InitMILink
		} else if (InitialConditionSwitch == 1) {
  			MicaRes <- ResInitFractionFull * MIFullPoolVol()
		} else if (InitialConditionSwitch == 2) {
			MIHistStor <- HistStor[week_counter, 2]
			MicaRes <- MIHistStor
		} else {
			MicaRes <- MIFullPoolVol()
		}
	} else {
		MicaRes <- reservoir_vol_df[week_counter - 1, 1]
	}
	return(MicaRes)
}
MicaFlowData <- function() { ## Naturalized streamflow 
	return(PriVICMI)
}
MIIn <- function() { # Inflow to the dam = supply - total surface water demand
  MIIn_o = MicaFlowData() - DemVICMI
  return(MIIn_o)
}
MIInflow <- function() {
	return(MIIn())
}
MIDamProtectRel <- function() { # The release required to keep the dam from filling above the full pool volume.  Units acre-ft.
	MIDamProt <- max(0, MicaReservoir() + MIInflow() - MIFullPoolVol())
	return(MIDamProt)
}
MIMaxFloodOutflow <- function() { # Max allowable release for flood prevention
	MIMaxFloodOutflow_o <- MIMaxFloodOutflow_input[week_counter_in_year(), 2]
	return(MIMaxFloodOutflow_o)
}
MIRelLimit <- function() { # Max allowable release
	MIRelLimit_o <- min(MIMaxFloodOutflow(), max(MicaReservoir() + MIInflow() - MIBotVol, 0))
	return(MIRelLimit_o)
}
MIAssuredReleasedMin <- function() {
	MIAssuredReleasedMin_o <- MIAssuredReleasedMin_i[week_counter_in_year(), 2]
	return(MIAssuredReleasedMin_o)
}
MIAugNovMin <- function() {
	if (ARFloodCurve() < 2.64E5 || ARFloodCurve() < 3.53E5) {
		dust_o <- 0
	} else if (ArrowReservoir() < 2.64E5) {
		dust_o <- 32000
	} else if (ArrowReservoir() < 3.53E5) {
		dust_o <- 27000
	} else {
		dust_o <- 0
	}
	return(dust_o)
}
MIDecMin <- function() {
	if (ARFloodCurve() < 1.34E6) {
		dust_o <- 23000
	} else if (ArrowReservoir() < 1.34E6) {
		dust_o <- 32000
	} else {
		dust_o <- 23000
	}
	return(dust_o)
}
MIJanMin <- function() {
	if (ARFloodCurve() < 8.93E5) {
		dust_o <- 23000
	} else if (ArrowReservoir() < 8.93E5) {
		dust_o <- 32000
	} else {
		dust_o <- 23000
	}
	return(dust_o)
}
MIFebMin <- function() {
	if (ARFloodCurve() < 3.48E5) {
		dust_o <- 23000
	} else if (ArrowReservoir() < 3.48E5) {
		dust_o <- 27000
	} else {
		dust_o <- 23000
	}
	return(dust_o)
}
MIMarchMin <- function() {
	if (ARFloodCurve() < 3.48E5) {
		dust_o <- 18000
	} else if (ArrowReservoir() < 1.39E6) {
		dust_o <- 23000
	} else {
		dust_o <- 18000
	}
	return(dust_o)
}
MIApriMin <- function() {
	dust_o <- 18000
	return(dust_o)
}
MIMayMin <- function() {
	dust_o <- 10000
	return(dust_o)
}
MIJuneMin <- function() {
	if (ArrowReservoir() < 248000) {
		dust_o <- 25000
	} else {
		dust_o <- 10000
	}
	return(dust_o)
}
MIJulyMin <- function() {
	if (ArrowReservoir() < 7.45E5) {
		dust_o <- 32000
	} else {
		dust_o <- 0
	}
	return(dust_o)
}
MIMinRelForAR <- function() { # Minimum release from MICAA to ARROW
	if (week_counter_in_year() >= 1 && week_counter_in_year() <= 18) {
		MIMinRelForAR_o <- MIAugNovMin() * cfsTOafw
	} else if (week_counter_in_year() >= 18 && week_counter_in_year() <= 22) {
		MIMinRelForAR_o <- MIDecMin() * cfsTOafw
	} else if (week_counter_in_year() >= 23 && week_counter_in_year() <= 26) {
		MIMinRelForAR_o <- MIJanMin() * cfsTOafw
	} else if (week_counter_in_year() >= 27 && week_counter_in_year() <= 30) {
		MIMinRelForAR_o <- MIFebMin() * cfsTOafw
	} else if (week_counter_in_year() >= 31 && week_counter_in_year() <= 35) {
		MIMinRelForAR_o <- MIMarchMin() * cfsTOafw
	} else if (week_counter_in_year() >= 36 && week_counter_in_year() <= 39) {
		MIMinRelForAR_o <- MIApriMin() * cfsTOafw
	} else if (week_counter_in_year() >= 40 && week_counter_in_year() <= 44) {
		MIMinRelForAR_o <- MIMayMin() * cfsTOafw
	} else if (week_counter_in_year() >= 45 && week_counter_in_year() <= 48) {
		MIMinRelForAR_o <- MIJuneMin() * cfsTOafw
	} else if (week_counter_in_year() >= 49 && week_counter_in_year() <= 52) {
		MIMinRelForAR_o <- MIJulyMin() * cfsTOafw
	} else {
		MIMinRelForAR_o <- 0
	}
	MIMinRelForAR_o <- 0 # it is set to zero in the current version of ColSim
	return(MIMinRelForAR_o)
}

########### MIFloodCurve

# Mica Flood Control Curves are modified according to January 1995 SRDs which are showed in USCOE Website
# (http://www.nwd-wc.usace.army.mil/cafe/forecast/SRD/MCDBChart8.pdf) (by Se-yeun Lee, Dec.05)
# Flood storage curve selected is based on the total run off April-August  for the variable period && is fixed during the rest of the year.
# For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles and/or to the dam itself.  Units acre-ft.
# Flood control curves give the volume of water in the dam.


MI_CurFC <- function() { # Year-round flood curve
	if (DallesRunoffAprAug >= 111E6 && MISumRunoffAprilAug > 19.3E6) {
		MI_CurFC_o <- MIFlood12
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 18E6 || MISumRunoffMayAug > 17.5E6)) {
		MIFlood11 <- MIFlood_712[week_counter_in_year(), 12]
		MI_CurFC_o <- MIFlood11
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 16E6 || MISumRunoffMayAug > 15.50E6)) {
		MIFlood10 <- MIFlood_712[week_counter_in_year(), 11]
		MI_CurFC_o <- MIFlood10
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 14E6 || MISumRunoffMayAug > 13.50E6)) {
		MIFlood9 <- MIFlood_712[week_counter_in_year(), 10]
		MI_CurFC_o <- MIFlood9
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 12E6 || MISumRunoffMayAug > 11.50E6)) {
		MIFlood8 <- MIFlood_712[week_counter_in_year(), 9]
		MI_CurFC_o <- MIFlood8
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 10E6 || MISumRunoffMayAug > 9.5E6)) {
		MIFlood7 <- MIFlood_712[week_counter_in_year(), 8]
		MI_CurFC_o <- MIFlood7
	} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 8E6 || MISumRunoffMayAug > 7.5E6)) {
		MIFlood_6 <- MIFlood_712[week_counter_in_year(), 7]
		MI_CurFC_o <- MIFlood_6
	} else if (DallesRunoffAprAug >= 80.0E6) {
		MIFlood5 <- MIFlood_712[week_counter_in_year(), 6]
		MI_CurFC_o <- MIFlood5
	} else if (DallesRunoffAprAug >= 75.0E6) {
		MIFlood4 <- MIFlood_712[week_counter_in_year(), 5]
		MI_CurFC_o <- MIFlood4
	} else if (DallesRunoffAprAug >= 70.0E6) {
		MIFlood3 <- MIFlood_712[week_counter_in_year(), 4]
		MI_CurFC_o <- MIFlood3
	} else if (DallesRunoffAprAug >= 65.0E6) {
		MIFlood2 <- MIFlood_712[week_counter_in_year(), 3]
		MI_CurFC_o <- MIFlood2
	} else {
		MIFlood1 <- MIFlood_712[week_counter_in_year(), 2]
		MI_CurFC_o <- MIFlood1
	}
	return(MI_CurFC_o)
}

MICurFC_Cont <- function() { # December -- April flood curve
	MIFlood_Range <- c(10000000, 12000000, 14000000, 16000000, 18000000, 20000000)
	if (MIFlood_Range[length(MIFlood_Range)] < MISumRunoffAprilAug) {
		row_no <- length(MIFlood_Range)
	} else {
		row_no <- max(1.0, which(MIFlood_Range > MISumRunoffAprilAug)[1] - 1)
	}
	if (DallesRunoffAprAug >= 111.0E06 && month_in_year %in% 3:9) {
		if (MISumRunoffAprilAug <= 20.0E06) {
			if (month_in_year==5) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 1] # DEC_MI
			} else if (month_in_year==6) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 2] # JAN_MI
			} else if (month_in_year==7) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 3] # FEB_MI
			} else if (month_in_year==8) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 4] # MAR_MI
			} else if (month_in_year==9) {
				MI_CurFC_o <- MIFlood_DecToApr[row_no, 5] # APR_MI
			} else if (month_in_year %in% c(3,4)) { # Oct-Nov
				MI_CurFC_o <- 19875520
			} else { ## outside the December through April timeframe
				MI_CurFC_o <- MIFullPool
			}
		} else { # MISumRunoffAprilAug > 20E6
			MIFlood12 <- MIFlood_712[week_counter_in_year(), 13] 
			MI_CurFC_o <- MIFlood12
		}
	} else if (DallesRunoffAprAug < 111.0E06 && month_in_year %in% 3:9) {
	  if (DallesRunoffAprAug >= 80.0E6) {
	    MI_CurFC_o <- MIFlood_712[week_counter_in_year(), 6]
	  } else if (DallesRunoffAprAug >= 75.0E6) {
	    MI_CurFC_o <- MIFlood_712[week_counter_in_year(), 5]  
	  } else if (DallesRunoffAprAug >= 70.0E6) {
	    MI_CurFC_o <- MIFlood_712[week_counter_in_year(), 4]
	  } else if (DallesRunoffAprAug >= 65.0E6) {
	    MI_CurFC_o <- MIFlood_712[week_counter_in_year(), 3]
	  } else {
	    MI_CurFC_o <- MIFlood_712[week_counter_in_year(), 2] 
	  }
	} else { # Outside Dec through April timeframe
	  MI_CurFC_o <- MIFullPoolVol()
	}
	return(MI_CurFC_o)
}
MIFloodCurve <- function() {
	GlobalFloodEvacMult <- GlobalFloodEvacMult_i[week_counter_in_year(), 2] ## Default = 1
	if (FC_Option == 1) { ## default = 2
		MIFloodCurve_o <- MIFullPoolVol() - GlobalFloodEvacMult * (MIFullPoolVol() - MI_CurFC())
	} else if (FC_Option == 2) {
		MIFloodCurve_o <- MIFullPoolVol() - GlobalFloodEvacMult * (MIFullPoolVol() - MICurFC_Cont())
	}
	return(MIFloodCurve_o)
}
MITopVol <- function() { # max allowable volume of water in dam for given timestep
	if (TopRuleSw() == 0) { # Default = 0
		MITopVol_1 <- MIFloodCurve()
	} else if (TopRuleSw() == 1) {
		MITopVol_1 <- MIFullPoolVol() # reservoir storage capacity
	} else {
		MIFlood1 <- MIFlood_712[week_counter_in_year(), 2]
		MITopVol_1 <- MIFlood1
	}
	return(MITopVol_1)
}
MIRuleReq <- function() { # minimum release based on flood curve
	MIRuleReq_1 <- max(MicaReservoir() + MIIn() - MITopVol(), 0)
	return(MIRuleReq_1)
}
MIReq <- function() { # Minimum release based on historical data
	if (RefillMinSw() == 1) { # Default is 0
		MIReq_o <- MIAssuredReleasedMin() * cfsTOafw
	} else {
		MIReq_o <- max(MIMinRelForAR(), MIAvgMin * cfsTOafw)
	}
	return(MIReq_o)
}
MIAvailAfter <- function() { # Total available volume of water to be allocated 
	MIAvailAfter_o <- max(0, MicaReservoir() + MIIn() - MIBotVol)
	return(MIAvailAfter_o)
}
MIPrelim <- function() { # Preliminary release based on required minimum release
	MIpre <- min(MIAvailAfter(), max(MIRuleReq(), MIReq()))
	MIPrelim_c <<- MIpre
	return(MIpre)
}
MIMcNaryDraftLimit <- function() { # Minumum reservoir volume after accounting for release to meet McNary fish target
	MIMcNaryDraftLimit_o <- if (UseAllStorForMCNLG == 1) { # Default is 0
    MIMcNaryDraftLimit_o <- MIBotVol
	} else if (Fish_Pool_Alternative == 1) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol()
	} else if (Fish_Pool_Alternative == 2) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol() - 0.402E6
	} else if (Fish_Pool_Alternative == 3) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol() - 0.804E6
	} else if (Fish_Pool_Alternative == 4) {
		MIMcNaryDraftLimit_o <- MIFullPoolVol() - 1.205E6
	} else {
		MIMcNaryDraftLimit_o <- MIFullPoolVol()
	}
	return(MIMcNaryDraftLimit_o)
}
MIMcNarySharedWater <- function() { # Water allocated for meeting McNary fish flow target
	MIMcNarySharedWater_o <- max(0, MicaReservoir() + MIIn() - MIPrelim() - MIMcNaryDraftLimit())
	return(MIMcNarySharedWater_o)
}
MIMcNarySup <- function() { # Water released to meet McNary fish target (AF/wk)
	if (TotalMcNarySharedWater_c == 0) {
		MIMcNarySup_o <- 0
	} else {
		MIMcNarySup_o <- min(MIMcNarySharedWater(), McNaryFlowDeficit() * MIMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(MIMcNarySup_o)
}
MISharedWater <- function() { # Water stored in Mica after allocating water to meet minimum release and fish target objectives
	MISharedWater_o <- max(0, MicaReservoir() + MIIn() - MIPrelim() - MIMcNarySup() - MIBotVol)
	return(MISharedWater_o)
}
MIEnergyContent <- function() { # Energy content of water stored in Mica 
	MIEnergyContent_o <- MISharedWater() * (MINetHead() + REVNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIEnergyContent_o)
}
MIFirmEngSup <- function() { # Power generation from Mica required to meet firm energy target (MW-hr)
  if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
    if (TotalEnergyContent_c == 0) {
      MIFirmEngSup_o <- 0
    } else {
      MIFirmEngSup_o <- (MIEnergyContent() + MIECCEnergyContent()) / (TotalEnergyContent_c + MIECCEnergyContent()) * FirmEnergyDeficit_c
    }
  } else if (TotalECCEnergyContent_c == 0) {
    MIFirmEngSup_o <- 0
  } else {
    MIFirmEngSup_o <- MIECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
  }
  return(MIFirmEngSup_o)
}
MINFEnergyContent <- function() { # Energy content of water stored in Mica that could be used for generating non-firm hydropower (MW-hr)
	MINFEnergyContent_o <- max(0, MIECCEnergyContent() - MIFirmEngSup())
	return(MINFEnergyContent_o)
}
MINonFirmEngSup <- function() { # Power generation from Mica required to meet non-firm energy target (MW-hr)
	if (TotalNFEnergyContent_c == 0) {
		MINonFirmEngSup_o <- 0
	} else {
		MINonFirmEngSup_o <- MINFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(MINonFirmEngSup_o)
}
MIFloodSpace <- function() { # Additional storage space that can be used in case of high flow at The Dalle (AF)
  MIFloodSpace_o <- min(MIPrelim() + MIEnergySup(), max(0, MIFullPoolVol() - MicaReservoir() + MIIn() - (MIPrelim() + MIEnergySup())))
  return(MIFloodSpace_o)
}
MIFloodFrac <- function() { # Fraction of the total needed flood space supplied by Mica
  if (TotalFloodSpace_c == -9999) {
    TotalFloodSpace_c <<- TotalFloodSpace()
    water_df[week_counter,2]<<-TotalFloodSpace_c
  }
  if (TotalFloodSpace_c == 0) {
    MIFloodFrac_o <- 0
  } else {
    MIFloodFrac_o <- MIFloodMult * MIFloodSpace() / TotalFloodSpace_c
  }
  return(MIFloodFrac_o)
}
MIRelReducReq <- function() { # Water stored rather than released in case of high flow at The Dalles (AF/wk)
	MIRelReducReq_o <- TotalRelReducReq() * MIFloodFrac()
	return(MIRelReducReq_o)
}
MIElev_ft <- function() { # Water level in dam (ft)
	MIElev_ft_o <- 2.86317977E-33 * MicaReservoir()^5 - 1.60110881E-25 * MicaReservoir()^4 + 3.33032270E-18 * MicaReservoir()^3 -
    3.24050996E-11 * MicaReservoir()^2 + 1.67124652E-04 * MicaReservoir() + 1.90570627E+03
	return(MIElev_ft_o)
}
MINetHead <- function() { # Depth of water above turbines (ft)
	MITailElev <- 1867
	MILoss <- 0
	MINetHead_o <- MIElev_ft() - MITailElev - MILoss
	return(MINetHead_o)
}
MIPenLimit <- function() { # Maximum flow rate through turbines (AF/wk)
	MIPenCap <- 41600
	MIPenLimit_o <- MIPenCap * cfsTOafw
	return(MIPenLimit_o)
}
MIPreEnergy <- function() { # Initial estimate of energy production (MW-hr), considering only water released for flood protection
	MIPreEnergy_o <- MWhr_per_ftAcFt * min(MIPrelim(), MIPenLimit()) * MINetHead() * MICombEfficiency
	return(MIPreEnergy_o)
}
MicaGrPreEnergy <- function() {
	MicaGrPreEnergy_o <- MIPreEnergy() + REVPreEnergy()
	return(MicaGrPreEnergy_o)
}
MIFirmEngSupReq <- function() { # Required release from Mica to meet firm energy target (AF/wk)
	MIFirmEngSupReq_o <- min(MIPenLimit(), MIFirmEngSup() / (MWhr_per_ftAcFt * (MINetHead() + REVNetHead() + TotalGCHead()) * MICombEfficiency))
	return(MIFirmEngSupReq_o)
}
MINonFirmSupReq <- function() { # Required release from Mica to meet non-firm energy target (AF/wk)
	if (NonFirmEnergySw == 1) { # Default = 1
		MINonFirmSupReq_o <- min(MIPenLimit(), (MIFirmEngSup() + MINonFirmEngSup()) / (MWhr_per_ftAcFt * (MINetHead() + REVNetHead() + TotalGCHead()) * MICombEfficiency))
	} else {
		MINonFirmSupReq_o <- 0
	}
	return(MINonFirmSupReq_o)
}
MICriticalCurve <- function() { # Minimum reservoir storage for given week of year to meet energy objectives (AF)
	MICriticalCurve_o <- MICriticalCurve_input[week_counter_in_year(), 1]
	return(MICriticalCurve_o)
}
MIRefillCurve <- function() { # Required refill to ensure dam is full by end of year (AF)
	if (RefillSwitch() == 1) { # Default = 2 (Perfect Forecast)
		MIRefillCurve_o <- MIRefillVol1
	} else if (RefillSwitch() == 2) {
		MIRefillCurve_o <- MIRefillVol2
	} else {
		MIRefillCurve_o <- 0
	}
	return(MIRefillCurve_o)
}
MI1931Refill <- function() { # Refill curve based on historical 1931 refill curve
	MI1931Refill_o <- MI1931Refill_input$MI1931Refill[week_counter_in_year()]
	return(MI1931Refill_o)
}
MIECC <- function() {
  # During the fixed period from August-December before the forecast, the ECC (Energy Content Curve) is the greater of the critical curve or the refill curve based on 1931 inflows.
  # In the variable period from January-July the VEEC can be lower than the ECC due to revised refill curve (based on forecast).
  # The VEEC curve, however, cannot be higher than the original ECC curve.  The value in this icon is compared to the flood storage curve and the minimum value is selected.
  # The model will always release water to evacuate flood storage, and will release to the ECC and VECC if the user selects maximum allowed non-firm releases.
	if (month_in_year <= 5 || month_in_year == 12) { # Fixed period, we include July (month 7) to ensure refill by the end of the year.
		out1 <- max(MICriticalCurve(), MIRefillCurve())
	} else { # Variable period
		out1 <- min(max(MIRefillCurve(), MICriticalCurve()), max(MICriticalCurve(), MI1931Refill()))
	}
	MIECC_o <- min(MIFloodCurve(), out1)
  return(MIECC_o)
}
MIECCSharedWater <- function() { # Water available for generating hydropower after meeting minumum release, fish flow, and energy water storage objectives
	MIECCSharedWater_o <- max(0, MicaReservoir() + MIIn() - MIMcNarySup() - MIPrelim() - MIECC())
	return(MIECCSharedWater_o)
}
MIECCEnergyContent <- function() { # Energy content of water that can be used to generate hydropower while maintaining required ECC levels
	MIECCEnergyContent_o <- MIECCSharedWater() * (MINetHead() + REVNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIECCEnergyContent_o)
}
MIMcNarySupEnergy <- function() { # Hydropower generated by releasing water to meet McNary fish target (MW-hr)
	MIMcNarySupEnergy_o <- MIMcNarySup() * (MINetHead() + TotalGCHead() + REVNetHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIMcNarySupEnergy_o)
}
MIEnergySup <- function() { # Water released to satisfy firm and non-firm power requirements (AF/wk)
	if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
		MIEnergySup_o <- max(min(MIFirmEngSupReq(), MISharedWater()), min(MINonFirmSupReq(), MIECCSharedWater()))
	} else {
		MIEnergySup_o <- max(min(MIFirmEngSupReq(), MIECCSharedWater()), min(MINonFirmSupReq(), MIECCSharedWater()))
	}
	return(MIEnergySup_o)
}
MICombSup <- function() { # Total water to be released to meet fish and energy objectives
# Units Mwhr
	if (TotalEnergyContent_c == -9999) {
		TotalEnergyContent_c <<- TotalEnergyContent()
		energy_df[week_counter,2] <<- TotalEnergyContent_c
	}
	if (TotalMcNarySharedWater_c == -9999) {
		TotalMcNarySharedWater_c <<- TotalMcNarySharedWater()
		water_df[week_counter,3] <<- TotalMcNarySharedWater_c
	}
	if (TotalECCEnergyContent_c == -9999) {
		TotalECCEnergyContent_c <<- TotalECCEnergyContent()
		energy_df[week_counter,3] <<- TotalECCEnergyContent_c
	}
	if (TotalCoordPreEnergy_c == -9999) {
		TotalCoordPreEnergy_c <<- TotalCoordPreEnergy()
		energy_df[week_counter,5] <<- TotalCoordPreEnergy_c   
	}
	if (FirmEnergyDeficit_c == -9999) {
		FirmEnergyDeficit_c <<- FirmEnergyDeficit()
		energy_df[week_counter,4] <<- FirmEnergyDeficit_c
	}
	if (NonFirmEnergyDeficit_c == -9999) {
		NonFirmEnergyDeficit_c <- NonFirmEnergyDeficit()
		energy_df[week_counter,7]<<-NonFirmEnergyDeficit_c
	}
	if (TotalNFEnergyContent_c == -9999) {
		TotalNFEnergyContent_c <<- TotalNFEnergyContent()
		energy_df[week_counter,6]<<-TotalNFEnergyContent_c
	}
	MICombSup_o <- MIEnergySup() + MIMcNarySup()
	return(MICombSup_o)
}
APR_MI <- function() { # April flood curve
	if(which(APR_MI_input > MISumRunoffAprilAug)[1]>1){
		APR_MI_o <- APR_MI_input[which(APR_MI_input > MISumRunoffAprilAug)[1] - 1, 2]
	} else {
		APR_MI_o <- APR_MI_input[1,2]
	}
	return(APR_MI_o)
}
MIAprEvaq_Cont <- function() {
	if (DallesRunoffAprAug >= 111E06) {
		if (MISumRunoffAprilAug <= 20.0E06) {
			MIAprEvaq_Cont_o <- APR_MI()
		} else {
			MIAprEvaq_Cont_o <- 7537058
		}
	} else {
		MIAprEvaq_Cont_o <- 15075520
	}
	return(MIAprEvaq_Cont_o)
}
MI_April_Evac_Target <- function() { # Release to meet April flood target (used for culculating corrected flow at the Dalles)
	if (FC_Option == 1) {
		if (DallesRunoffAprAug >= 111E6 && MISumRunoffAprilAug > 19.3E6) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 13]
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 18E6 || MISumRunoffMayAug > 17.5E6)) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 12]
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 16E6 || MISumRunoffMayAug > 15.50E6)) {
			MIF_o <-MIFlood_712[week_counter_in_year(), 11]
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 14E6 || MISumRunoffMayAug > 13.50E6)) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 10]
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 12E6 || MISumRunoffMayAug > 11.50E6)) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 9]
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 10E6 || MISumRunoffMayAug > 9.5E6)) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 8]
		} else if (DallesRunoffAprAug >= 111E6 && (MISumRunoffAprilAug > 8E6 || MISumRunoffMayAug > 7.5E6)) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 7]
		} else if (DallesRunoffAprAug >= 80.0E6) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 6]
		} else if (DallesRunoffAprAug >= 75.0E6) {
			MIF_o <- MIFlood_712[week_counter_in_year(), 5]
		} else if (DallesRunoffAprAug >= 70.0E6) {
			MIF_o <-MIFlood_712[week_counter_in_year(), 4]
		} else if (DallesRunoffAprAug >= 65.0E6) {
			MIF_o <-MIFlood_712[week_counter_in_year(), 3]
		} else {
			MIF_o <- MIFlood_712[week_counter_in_year(), 2]
		}
		MI_April_Evac_Target_o <- GlobalFloodEvacMult * (MIFullPoolVol() - MIF_o)
  } else {
		MI_April_Evac_Target_o <- GlobalFloodEvacMult * (MIFullPoolVol() - MIAprEvaq_Cont())
  }
  return(MI_April_Evac_Target_o)
}

MIRelease <- function() { # Water released from Mica after accounting for all objectives
	MIRelease_o <- max(MIDamProtectRel(), min(MIPrelim() + MICombSup() - MIRelReducReq(), MIRelLimit()))
	return(MIRelease_o)
}
MIOutflow <- function() { # Final outflow from Mica
  if (ResetStorage() == 1) { # default ResetStorage == 0
    MIOutflow_o <- 0
  } else {
    MIOutflow_o <- MicaRelease_c
  }
  return(MIOutflow_o)
}

################### REVELSTOKE ########################

RevFlowData <- function() {
	return(PriVICREV)
}
REVInc <- function() {
	REVInc_o <- RevFlowData() - MicaFlowData()
	return(REVInc_o)
}
REVPrelim <- function() {
	REVPrelim_o <- MIPrelim_c + REVInc() - DemVICREV
	return(REVPrelim_o)
}
REVPenLimit <- function() {
	REVPenCap <- 56000
	REVPenLimit_o <- REVPenCap * cfsTOafw
	return(REVPenLimit_o)
}
REVNetHead <- function() { # Approximate elevation since we do not have the data to relate Volume to water elevation at Revelstoke
	REVNetHead_o <- 420
	return(REVNetHead_o)
}
REVPreEnergy <- function() {
	REVPreEnergy_o <- min(REVPrelim(), REVPenLimit()) * REVNetHead() * RevCombEfficiency * MWhr_per_ftAcFt
	return(REVPreEnergy_o)
}
REVIn <- function() {
	REVIn_o <- MIOutflow() + REVInc() - DemVICREV
	return(REVIn_o)
}
REVOut <- function() {
	REVOut_o <- REVIn()
	return(REVOut_o)
}

################# ARROW DAM ########################################

InitARLink <- 4886277.48
ARBotVol <- 227340

ARFullPoolVol <- function() {
  ARFullPoolVol_o <- 7327300 # Volume corresponding to 1444 ft of elevation.  This is normal full pool.  Units acre-ft.
  return(ARFullPoolVol_o)
}
ArrowFlowData <- function() {
	return(PriVICAR)
}
InitAR <- function() {
	if (InitialConditionSwitch == 0) { # Default = 2
		InitAR_o <- InitARLink
	} else if (InitialConditionSwitch == 1) {
		InitAR_o <- ResInitFractionFull * ARFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitAR_o <- ARHistStor
	} else {
		ARFullPoolVol
	}
	return(InitAR_o)
}
ArrowReservoir <- function() {
	if (week_counter == 1) {
		if (InitialConditionSwitch == 0) {
			ArrowReservoir_o <- InitARLink
		} else if (InitialConditionSwitch == 1) {
			ArrowReservoir_o <- ResInitFractionFull * ARFullPoolVol()
		} else if (InitialConditionSwitch == 2) {
			ARHistStor <- ARHistStor_i[week_counter, 2]
			ArrowReservoir_o <- ARHistStor
		} else {
			ArrowReservoir_o <- ARFullPoolVol
		}
	} else {
		ArrowReservoir_o <- reservoir_vol_df[week_counter - 1, 2]
	}
	return(ArrowReservoir_o)
}
ARInc <- function() {
	ARInc_o <- ArrowFlowData() - RevFlowData()
	return(ARInc_o)
}
ARSufaceArea <- function() { # Surface Area of dam
	ARSufaceArea_o <- (-1.21281443E-13 * (ArrowReservoir() / 1000)^4 + 1.53692112E-09 * (ArrowReservoir() / 1000)^3 -
    6.75961255E-06 * (ArrowReservoir() / 1000)^2 + 1.87278268E-02 * (ArrowReservoir() / 1000) + 2.30403996) * 1000
	return(ARSufaceArea_o)
}
AREvapData <- function() {
	AREvapData_o <- 0 # Could include open-water evaporation rate
	return(AREvapData_o)
}
AREvap <- function() { # Volumetric evaporation
	AREvap_o <- (ARSufaceArea() * AREvapData()) * 0.5042 / 12
	return(AREvap_o)
}
ARIn <- function() { # Preliminary inflow to Arrow
	ARIn_o <- REVPrelim() + ARInc() - AREvap() - DemVICAR
	return(ARIn_o)
}
ARInflow <- function() {
	if (ResetStorage() == 1) {
		ARInflow_o <- InitAR() - ArrowReservoir()
	} else {
		ARInflow_o <- REVOut() + ARInc() - DemVICAR - AREvap()
	}
	return(ARInflow_o)
}
AROutflow <- function() {
	if (ResetStorage() == 1) {
		AROutflow_o <- 0
	} else {
		AROutflow_o <- ARRelease_c
	}
	return(AROutflow_o)
}
AR_CurFC <- function() {
	ARFlood1 <- ARFlood[week_counter_in_year(), 2]
	ARFlood2 <- ARFlood[week_counter_in_year(), 3]
	ARFlood3 <- ARFlood[week_counter_in_year(), 4]
	ARFlood4 <- ARFlood[week_counter_in_year(), 5]
	ARFlood5 <- ARFlood[week_counter_in_year(), 6]
	ARFlood6 <- ARFlood[week_counter_in_year(), 7]
	if (DallesRunoffAprAug < 64.0E6) {
		AR_CurFC_o <- ARFlood1
	} else if (DallesRunoffAprAug < 65.0E6) {
		AR_CurFC_o <- ARFlood2
	} else if (DallesRunoffAprAug < 70.0E6) {
		AR_CurFC_o <- ARFlood3
	} else if (DallesRunoffAprAug < 75E6) {
		AR_CurFC_o <- ARFlood4
	} else {
		AR_CurFC_o <- ARFlood5
	}
	return(AR_CurFC_o)
}
######## ARCurFC_Cont ##### 
# Refill timing was changed from Jun to Apr (10/24/2007)-Se-yeun Lee

ARCurFC_Cont <- function() {  # Release period OCT-JUN for flood storage   JUL-SEP REFILL
	DAFlood_Range <- c(60000000, 65000000, 70000000, 75000000, 80000000)
	if (DAFlood_Range[length(DAFlood_Range)] < DallesRunoffAprAug) {
		row_no <- length(DAFlood_Range)
	} else {
		row_no <- max(1.0, which(DAFlood_Range > DallesRunoffAprAug)[1] - 1)
	}
	if (month_in_year %in% 3:9) {
  	if (DallesRunoffAprAug <= 64.0E06) {
  		ARCurFC_Cont_o <- ARFlood_1May_input[week_counter_in_year(), 2] # Dry year flood curve
  	} else if (DallesRunoffAprAug <= 80.0E06) {
  		if (month_in_year == 3 || month_in_year == 4) {
  			ARCurFC_Cont_o <- 7077300
  		} else if (month_in_year == 5) {
  			ARCurFC_Cont_o <- 6617300
  		} else if (month_in_year == 6) {
  			ARCurFC_Cont_o <- ARFloodMonth[row_no, 2] # JAN_AR
  		} else if (month_in_year == 7) {
  			ARCurFC_Cont_o <- ARFloodMonth[row_no, 3] # FEB_AR
  		} else if (month_in_year == 8) {
  			ARCurFC_Cont_o <- ARFloodMonth[row_no, 4] # MAR_AR
  		} else {
  			ARCurFC_Cont_o <- ARFloodMonth[row_no, 5] # APR_AR
  		} 
  	} else { # DallesRunoffAprAug > 80.0E06
  		ARCurFC_Cont_o <- ARFlood_May5_i[week_counter_in_year(), 2] # Wet year flood curve
  	}
	} else { # Outside flood evacuation period
	  ARCurFC_Cont_o <- ARFullPoolVol() 
	}
	return(ARCurFC_Cont_o)
}

ARFloodCurve <- function() {
	if (FC_Option == 1) {
		ARFloodCurve_o <- ARFullPoolVol() - GlobalFloodEvacMult * (ARFullPoolVol() - (AR_CurFC()))
	} else if (FC_Option == 2) {
		ARFloodCurve_o <- ARFullPoolVol() - GlobalFloodEvacMult * (ARFullPoolVol() - (ARCurFC_Cont()))
	}
	return(ARFloodCurve_o)
}
ARTopVol <- function() {
	ARFlood1 <- ARFlood[week_counter_in_year(), 2]
	if (TopRuleSw() == 0) {
		ARTopVol_o <- ARFloodCurve()
	} else if (TopRuleSw() == 1) {
		ARTopVol_o <- ARFullPoolVol()
	} else {
		ARTopVol_o <- ARFlood1
	}
	return(ARTopVol_o)
}
ARRuleReq <- function() {
	ARRuleReq_o <- max(ArrowReservoir() + ARIn() - ARTopVol(), 0)
	return(ARRuleReq_o)
}
ARAvailAfter <- function() {
	ARAvailAfter_o <- max(0, ArrowReservoir() + ARIn() - ARBotVol)
	return(ARAvailAfter_o)
}
ARAvgMin <- function() {
	ARAvgMin_o <- 5000
	return(ARAvgMin_o)
}
ARAssuredRelease <- function() {
	ARAssuredRelease_o <- ARAssuredRelease_input[week_counter_in_year(), 2]
	return(ARAssuredRelease_o)
}
ARMinReq <- function() {
	if (RefillMinSw() == 1) {
		ARMinReq_o <- ARAssuredRelease() * cfsTOafw
	} else {
		ARMinReq_o <- ARAvgMin() * cfsTOafw
	}
	return(ARMinReq_o)
}
ARPrelim <- function() {
	ARPrelim_o <- min(ARAvailAfter(), max(ARRuleReq(), ARMinReq()))
	return(ARPrelim_o)
}
ARMcNaryDraftLimit <- function() {
	if (UseAllStorForMCNLG == 1) {
		ARMcNaryDraftLimit_o <- ARBotVol
	} else if (Fish_Pool_Alternative == 1) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol()
	} else if (Fish_Pool_Alternative == 2) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol()
	} else if (Fish_Pool_Alternative == 3) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol() - 0.386E6
	} else if (Fish_Pool_Alternative == 4) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol() - 0.773E6
	} else if (Fish_Pool_Alternative == 5) {
		ARMcNaryDraftLimit_o <- ARFullPoolVol() - 1.159E6
	} else {
		ARMcNaryDraftLimit_o <- ARFullPoolVol()
	}
	return(ARMcNaryDraftLimit_o)
}
ARMcNarySharedWater <- function() {
	ARMcNarySharedWater_o <- max(0, ArrowReservoir() + ARIn() - ARPrelim() - ARMcNaryDraftLimit())
	return(ARMcNarySharedWater_o)
}
ARFirmEngSupReq <- function() {
	ARFirmEngSupReq_o <- ARFirmEngSup_c / (MWhr_per_ftAcFt * TotalGCHead() * ARCombEfficiency)
	return(ARFirmEngSupReq_o)
}
ARFloodSpace <- function() {
  ARFloodSpace_o <- min((ARPrelim() + AREnergySup() + ARCombUpSup()), max(0, ARFullPoolVol() - ArrowReservoir() + ARIn() - (ARPrelim() + AREnergySup() + ARCombUpSup())))
  return(ARFloodSpace_o)
}
ARFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			ARFirmEngSup_o <- 0
		} else {
			ARFirmEngSup_o <- (AREnergyContent() + ARECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		ARFirmEngSup_o <- 0
	} else {
		ARFirmEngSup_o <- ARECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(ARFirmEngSup_o)
}
ARSharedWater <- function() {
	ARSharedWater_o <- max(0, ArrowReservoir() + ARIn() - ARPrelim() - ARMcNarySup() - ARBotVol)
	return(ARSharedWater_o)
}
AREnergySup <- function() { 
	if (ARFirmEngSup_c == -9999) {
		ARFirmEngSup_c <<- ARFirmEngSup()
	}
	if (ARFirmEngSupReq_c == -9999) {
		ARFirmEngSupReq_c <<- ARFirmEngSupReq()
		energy_df[week_counter,1] <<- ARFirmEngSupReq_c
    }
	if (UseTotalEnergyContentForFirm() == 1) {
		AREnergySup_o <- max(min(ARFirmEngSupReq_c, ARSharedWater()), min(ARNonFirmSupReq(), ARECCSharedWater()))
	} else {
		AREnergySup_o <- max(min(ARFirmEngSupReq_c, ARECCSharedWater()), min(ARNonFirmSupReq(), ARECCSharedWater()))
	}
	return(AREnergySup_o)
}
AREnergyContent <- function() {
	AREnergyContent_o <- ARSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(AREnergyContent_o)
}
ARMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		ARMcNarySup_o <- 0
	} else {
		ARMcNarySup_o <- min(ARMcNarySharedWater(), McNaryFlowDeficit() * ARMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(ARMcNarySup_o)
}
ARMcNarySupEnergy <- function() {
	ARMcNarySupEnergy_o <- ARMcNarySup() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(ARMcNarySupEnergy_o)
}
ARCombUpSup <- function() {
	ARCombUpSup_o <- MICombSup()
	return(ARCombUpSup_o)
}
ARCombSup <- function() {
	ARCombSup_o <- ARCombUpSup() + AREnergySup() + ARMcNarySup()
	return(ARCombSup_o)
}
ARCriticalCurve <- function() {
	ARCriticalCurve_o <- ARCriticalCurve_input[week_counter_in_year(), 2]
	return(ARCriticalCurve_o)
}
ARRefillCurve <- function() {
	if (RefillSwitch() == 1) {
		ARRefillCurve_o <- ARRefillVol1
	} else if (RefillSwitch() == 2) {
		ARRefillCurve_o <- ARRefillVol2
	}
	return(ARRefillCurve_o)
}
AR1931Refill <- function() {
	AR1931Refill_o <- AR1931Refill_input[week_counter_in_year(), 2]
	return(AR1931Refill_o)
}
ARECC <- function() {
	if (month_in_year <= 5 || month_in_year == 12) {
		out1 <- max(ARCriticalCurve(), ARRefillCurve())
	} else {
		out1 <- min(ARRefillCurve(), max(ARCriticalCurve(), AR1931Refill()))
	}
	ARECC_o <- min(ARFloodCurve(), out1)
	return(ARECC_o)
}
ARECCSharedWater <- function() {
	ARECCSharedWater_o <- max(0, ArrowReservoir() + ARIn() - ARMcNarySup() - ARPrelim() - ARECC())
	return(ARECCSharedWater_o)
}
ARECCEnergyContent <- function() {
	ARECCEnergyContent_o <- ARECCSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(ARECCEnergyContent_o)
}
ARNFEnergyContent <- function() {
	ARNFEnergyContent_o <- max(0, ARECCEnergyContent() - ARFirmEngSup_c)
	return(ARNFEnergyContent_o)
}
ARNonFirmEngSup <- function() { # MWhr
	if (NonFirmEnergyDeficit_c == -9999) {
		NonFirmEnergyDeficit_c <<- NonFirmEnergyDeficit()
		energy_df[week_counter,7] <<- NonFirmEnergyDeficit_c
	}
	if (TotalNFEnergyContent_c == 0) {
		ARNonFirmEngSup_o <- 0
	} else {
		ARNonFirmEngSup_o <- ARNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(ARNonFirmEngSup_o)
}
ARNonFirmSupReq <- function() { # Acre-ft/wk
	if (NonFirmEnergySw == 1) { # Default = 1
		ARNonFirmSupReq_o <- (ARFirmEngSup_c + ARNonFirmEngSup()) / (MWhr_per_ftAcFt * TotalGCHead() * ARCombEfficiency)
	} else {
		ARNonFirmSupReq_o <- 0
	}
	return(ARNonFirmSupReq_o)
}
###### AR_April_Evac_Target
# Expected Arrow flood evacuation at the end of April.
# Arrow Flood Control Curves are modified according to January 1995 SRDs which are showed in 2003 Columbia River Treaty Flood Control Operating Plan (by Se-yeun Lee, Dec.05)
# Flood storage curve selected is based on the total run off April-August
# for the variable period and is fixed during the rest of the year.  For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles.
ARAprEvaq_Cont <- function() {
	if (DallesRunoffAprAug <= 64.0E06) {
		ARAprEvaq_Cont_o <- 6617300
	} else if (DallesRunoffAprAug <= 80.0E06) {
		DAFlood_Range <- c(60000000, 65000000, 70000000, 75000000, 80000000)
		if (DAFlood_Range[length(DAFlood_Range)] < DallesRunoffAprAug) {
			row_no <- length(DAFlood_Range)
		} else {
			row_no <- max(1.0, which(DAFlood_Range > DallesRunoffAprAug)[1] - 1)
		}
	ARAprEvaq_Cont_o <- ARFloodMonth[row_no, 5] # APR_AR
	} else {
		ARAprEvaq_Cont_o <- 3727300
	}
	return(ARAprEvaq_Cont_o)
}
AR_April_Evac_Target <- function() {
	if (DallesRunoffAprAug < 64.0E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 2] # ARFlood1_2
	} else if (DallesRunoffAprAug < 65.0E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 3] # ARFlood2_2
	} else if (DallesRunoffAprAug < 70.0E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 4] # ARFlood3_2
	} else if (DallesRunoffAprAug < 75E6) {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 5] # ARFlood4_2
	} else {
		ARF_o <- ARFlood_2_input[week_counter_in_year(), 6]
	} # ARFlood5_2
	if (FC_Option == 2) {
		AR_April_Evac_Target_o <- (GlobalFloodEvacMult * (ARFullPoolVol() - ARAprEvaq_Cont()))
	} else {
		AR_April_Evac_Target_o <- (GlobalFloodEvacMult * (ARFullPoolVol() - ARF_o))
	}
	return(AR_April_Evac_Target_o)
}
ARDamProtectRel <- function() {
	ARDamProtectRel_o <- max(0, ArrowReservoir() + ARInflow() - ARFullPoolVol())
	return(ARDamProtectRel_o)
}
ARFloodFrac <- function() { # Includes dam plus upstream storage space
	if (TotalFloodSpace_c == 0) {
		ARFloodFrac_o <- 0
	} else {
		ARFloodFrac_o <- (MIFloodSpace() + ARFloodSpace()) / TotalFloodSpace_c
	}
	return(ARFloodFrac_o)
}
ARRelReducReq <- function() {
	ARRelReducReq_o <- min(TotalRelReducReq() * ARFloodFrac(), ARPrelim() + ARCombSup())
	return(ARRelReducReq_o)
}
ARRelLimit <- function() {
	ARRelLimit_o <- max(ArrowReservoir() + ARInflow() - ARBotVol, 0)
	return(ARRelLimit_o)
}
ARRelease <- function() { 
	ARRelease_o <- min(ARPrelim() + ARCombSup() - ARRelReducReq(), ARRelLimit())
	return(ARRelease_o)
}

##########################  BOX CANYON  ###################################################

BCNetHead <- function() {  
	BCNetHead_o <- 152
	return(BCNetHead_o)
}
BCPrelim <- function() {
	BCPrelim_o <- AFPrelim_c
	return(BCPrelim_o)
}
BCPenLimit <- function() {
	BoxCanyonPenCap <- 29000	
	BCPenLimit_o <- BoxCanyonPenCap * cfsTOafw
	return(BCPenLimit_o)
}
BCPreEnergy <- function() {
	BCPreEnergy_o <- min(BCPrelim(), BCPenLimit()) * BCNetHead() * BCCombEfficiency * MWhr_per_ftAcFt
	return(BCPreEnergy_o)
}
BCRelease <- function(){
  BCRelease_o <- AFRelease()
  return(BCRelease_o)
}

##################### DUNCAN DAM ##########################################

InitDULink <- 1423400
ResInitFractionFull <- 0
DUFullPoolVol <- 1.4234E+06
DUBotVol <- 2.568e4

DUHistStor <- function(week_counter) {
	DUHistStor_o <- HistStor[week_counter, 4]
	return(DUHistStor_o)
}
InitDU <- function() {
	if (InitialConditionSwitch == 0) {
		InitDU_o <- InitDULink
	} else if (InitialConditionSwitch == 1) {
		InitDU_o <- ResInitFractionFull * DUFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitDU_o <- DUHistStor(week_counter)
	} else {
		InitDU_o <- DUFullPoolVol
	}
	return(InitDU_o)
}
Duncan <- function() {
	if (week_counter == 1) {
		Duncan_o <- InitDU()
	} else {
		Duncan_o <- reservoir_vol_df[week_counter - 1, 3]
	}
	return(Duncan_o)
}
DuncanFlowData <- function() {
	return(PriVICDU)
}
DuncanIn <- function() {
	DuncanIn_o <- DuncanFlowData() - DUEvap() - DemVICDU
	return(DuncanIn_o)
}
DURelLimit <- function() {
	DURelLimit_o <- max(Duncan() + DUInflow() - DUBotVol, 0)
	return(DURelLimit_o)
}
DUEvap <- function() {
	DUEvapData <- 0
	DUEvap_o <- DUSufaceArea() * DUEvapData * 0.5042 / 12
	return(DUEvap_o)
}
DUSufaceArea <- function() {
	DUSufaceArea_o <- (-1.21281443E-13 * (Duncan() / 1000)^4 + 1.53692112E-09 * (Duncan() / 1000)^3 - 6.75961255E-06 * (Duncan() / 1000)^2 + 1.87278268E-02 * (Duncan() / 1000) + 2.30403996) * 1000
	return(DUSufaceArea_o)
}
DUAvailAfter <- function() {
	DUAvailAfter_o <- max(0, Duncan() + DuncanIn() - DUBotVol)
	return(DUAvailAfter_o)
}
DURuleReq <- function() {
	DURuleReq_o <- max(Duncan() + DuncanIn() - DUTopVol(), 0)
	return(DURuleReq_o)
}
DUMinReq <- function() {
  DUAvgMin <- 100 
	if (RefillMinSw() == 1) {
		DURefillMin_o <- DURefillMin_input[week_counter_in_year(), 2]
	} else {
		DURefillMin_o <- DUAvgMin * cfsTOafw
	}
	return(DURefillMin_o)
}
DUPrelim <- function() {
	DUPrelim_o <- min(DUAvailAfter(), max(DURuleReq(), DUMinReq()))
	return(DUPrelim_o)
}
DUInflow <- function() {
	if (ResetStorage() == 1) {
		DUInflow_o <- InitDU() - Duncan()
	} else {
		DUInflow_o <- DuncanIn()
	}
	return(DUInflow_o)
}
DUMcNaryDraftLimit <- function() {
  if (UseAllStorForMCNLG == 1) { # Default = 0
    DUMcNaryDraftLimit_o <- DUBotVol
  } else if (Fish_Pool_Alternative == 1 || Fish_Pool_Alternative == 2) {
    DUMcNaryDraftLimit_o <- DUFullPoolVol
  } else if (Fish_Pool_Alternative == 3) {
    DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.232E6
  } else if (Fish_Pool_Alternative == 4) {
    DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.464E6
  } else if (Fish_Pool_Alternative == 5) {
    DUMcNaryDraftLimit_o <- DUFullPoolVol - 0.695E6
  } else {
    DUMcNaryDraftLimit_o <- DUFullPoolVol
  }	
  return(DUMcNaryDraftLimit_o)
}
DUMcNarySharedWater <- function() {
	DUMcNarySharedWater_o <- max(0, Duncan() + DuncanIn() - DUPrelim() - DUMcNaryDraftLimit())	
	return(DUMcNarySharedWater_o)
}
DU_CurFC <- function() {
	if (DURunoffAprAug < 1.4E6) {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 2] # DUFlood1
	} else if (DURunoffAprAug < 1.6E6) {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 3] # DUFlood2
	} else if (DURunoffAprAug < 1.8E6) {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 4] # DUFlood3
	} else {
		DU_CurFC_o <- DUFlood_input[week_counter_in_year(), 5]
	} # DUFlood4}
	return(DU_CurFC_o)
}
DUCurFC_Cont <- function() {
	DURunof_Range <- c(1400000, 1600000, 1800000, 2000000)
	if (DURunof_Range[length(DURunof_Range)] < DURunoffAprAug) {
		row_no <- length(DURunof_Range)
	} else {
		row_no <- max(1.0, which(DURunof_Range > DURunoffAprAug)[1] - 1)
	}
	if (DURunoffAprAug <= 2.0E06) {
		if (month_in_year == 5) {
			DUCurFC_Cont_o <- 1023400
		} else if (month_in_year == 6) {
			DUCurFC_Cont_o <- DU_JAN_FAMAJ[row_no, 2] # JAN_DU
		} else if (month_in_year %in% 7:11) {
			DUCurFC_Cont_o <- DU_JAN_FAMAJ[row_no, 3] # FMAMJ_DU 
		} else {
			DUCurFC_Cont_o <- DUFullPoolVol
		}
	} else if (DURunoffAprAug > 2.0E6 && month_in_year %in% 5:11) {
		DUCurFC_Cont_o <- DUFlood_input[week_counter_in_year(), 5] # DUFlood4
	} else {
	  DUCurFC_Cont_o <- DUFullPoolVol
	}
	return(DUCurFC_Cont_o)
}
DUFloodCurve <- function() {
	if (FC_Option == 1) {
		DUFloodCurve_o <- DUFullPoolVol - GlobalFloodEvacMult * (DUFullPoolVol - DU_CurFC())
	} else if (FC_Option == 2) {
		DUFloodCurve_o <- DUFullPoolVol - GlobalFloodEvacMult * (DUFullPoolVol - DUCurFC_Cont())
	}
	return(DUFloodCurve_o)
}
DUTopVol <- function() {
	if (TopRuleSw() == 0) { # Default = 0
		DUTopVol_o <- DUFloodCurve()
	} else if (TopRuleSw() == 1) {
		DUTopVol_o <- DUFullPoolVol
	} else {
		DUTopVol_o <- DUFlood_input[week_counter_in_year(), 2]
	} # DUFlood1
	return(DUTopVol_o)
}

######## Duncun_April_Evac_Target
# Expected Duncan flood evacuation at the end of April.
# Flood storage curve selected is based on the total run off April-August  for the variable period and is fixed during the rest of the year.
# For some dams the decision is based on the forecast of  total inflow to dam itself.
# For other dams the flood storage decision is based on the forecast of  total runoff anticipated at the Dalles.
FMAMJ_DU <- function() { # April flood curve
	if (DURunoffAprAug < 1600000) {
		FMAMJ_DU_o <- FMAMJ_DU_input[1, 2]
	} else if (DURunoffAprAug > 1600000 && DURunoffAprAug < 1800000) {
		FMAMJ_DU_o <- FMAMJ_DU_input[2, 2]
	} else if (DURunoffAprAug > 1800000 && DURunoffAprAug < 2000000) {
		FMAMJ_DU_o <- FMAMJ_DU_input[3, 2]
	} else {
		FMAMJ_DU_o <- FMAMJ_DU_input[4, 2]
	}
	return(FMAMJ_DU_o)
}
DUAprEvaq_Cont <- function() {
	if (DURunoffAprAug <= 2.0E06) {
		DUAprEvaq_Cont_o <- FMAMJ_DU()
	} else {
		DUAprEvaq_Cont_o <- 153400
	}
	return(DUAprEvaq_Cont_o)
}
Duncun_April_Evac_Target <- function() {
	if (DURunoffAprAug < 1.4E6) {
		DUF_o <- DUF_2_input[month_in_year, 2] # DUFlood1_2
	} else if (DURunoffAprAug < 1.6E6) {
		DUF_o <- DUF_2_input[month_in_year, 3] # DUFlood2_2
	} else if (DURunoffAprAug < 1.8E6) {
		DUF_o <- DUF_2_input[month_in_year, 4] # DUFlood3_2
	} else {
		DUF_o <- DUF_2_input[month_in_year, 5]
	} # DUFlood4_2
	if (FC_Option == 2) {
		Duncun_April_Evac_Target_o <- GlobalFloodEvacMult * (DUFullPoolVol - (DUAprEvaq_Cont()))
	} else {
		Duncun_April_Evac_Target_o <- GlobalFloodEvacMult * (DUFullPoolVol - (DUF_o))
	}
	return(Duncun_April_Evac_Target_o)
}
DUMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		DUMcNarySup_o <- 0
	} else {
		DUMcNarySup_o <- min(DUMcNarySharedWater(), McNaryFlowDeficit() * DUMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(DUMcNarySup_o)
}
DUCombSup <- function() {
	DUCombSup_o <- DUEnergySup() + DUMcNarySup()
	return(DUCombSup_o)
}
DUMcNarySupEnergy <- function() {
	DUMcNarySupEnergy_o <- DUMcNarySup() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt	
	return(DUMcNarySupEnergy_o)
}
DUSharedWater <- function() {
	DUSharedWater_o <- max(0, Duncan() + DuncanIn() - DUPrelim() - DUMcNarySup() - DUBotVol)
	return(DUSharedWater_o)
}
DUEnergyContent <- function() {
	DUEnergyContent_o <- DUSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DUEnergyContent_o)
}
DUCriticalCurve <- function() {
	DUCriticalCurve_o <- DUCriticalCurve_input[week_counter_in_year(), 2]
	return(DUCriticalCurve_o)
}
DURefillCurve <- function() {
	if (RefillSwitch() == 1) {
		DURefillCurve_o <- DURefillVol1
	} else if (RefillSwitch() == 2) {
		DURefillCurve_o <- DURefillVol
	}
	return(DURefillCurve_o)
}
DU1931Refill <- function() {
	DU1931Refill_o <- DU1931Refill_input[week_counter_in_year(), 2]
	return(DU1931Refill_o)
}
DUECC <- function() {
	if (month_in_year <= 5 || month_in_year == 12) {
		out1 <- max(DUCriticalCurve(), DURefillCurve())
	} else {
		out1 <- min(max(DURefillCurve(), DUCriticalCurve()), max(DUCriticalCurve(), DU1931Refill()))
	}
	DUECC_o <- min(DUFloodCurve(), out1)
	return(DUECC_o)
}
DUECCSharedWater <- function() {
	DUECCSharedWater_o <- max(0, Duncan() + DuncanIn() - DUPrelim() - DUMcNarySup() - DUECC())
	return(DUECCSharedWater_o)
}
DUECCEnergyContent <- function() {
	DUECCEnergyContent_o <- DUECCSharedWater() * (TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DUECCEnergyContent_o)
}
DUFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			DUFirmEngSup_o <- 0
		} else {
			DUFirmEngSup_o <- (DUEnergyContent() + DUECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		DUFirmEngSup_o <- 0
	} else {
		DUFirmEngSup_o <- DUECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(DUFirmEngSup_o)
}
DUNFEnergyContent <- function() {
	DUNFEnergyContent_o <- max(0, DUECCEnergyContent() - DUFirmEngSup())
	return(DUNFEnergyContent_o)
}
DUNonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		DUNonFirmEngSup_o <- 0
	} else {
		DUNonFirmEngSup_o <- DUNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(DUNonFirmEngSup_o)
}
DUNonFirmSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		DUNonFirmSupReq_o <- (DUFirmEngSup() + DUNonFirmEngSup()) / (MWhr_per_ftAcFt * TotalGCHead() * DUCombEfficiency)	
	} else {
		DUNonFirmSupReq_o <- 0
	}
	return(DUNonFirmSupReq_o)
}
DUFirmEngSupReq <- function() {
	DUFirmEngSupReq_o <- DUFirmEngSup() / (MWhr_per_ftAcFt * TotalGCHead() * DUCombEfficiency)
	return(DUFirmEngSupReq_o)
}
DUEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		DUEnergySup_o <- max(min(DUFirmEngSupReq(), DUSharedWater()), min(DUNonFirmSupReq(), DUECCSharedWater()))
	} else {
		DUEnergySup_o <- max(min(DUFirmEngSupReq(), DUECCSharedWater()), min(DUNonFirmSupReq(), DUECCSharedWater()))
	}
	return(DUEnergySup_o)
}
DUDamProtectRel <- function() {
	DUDamProtectRel_o <- max(0, Duncan() + DUInflow() - DUFullPoolVol)
	return(DUDamProtectRel_o)
}
DURelease <- function() {
	DURelease_o <- max(DUDamProtectRel(), min(DUPrelim() + DUCombSup(), DURelLimit()))
	return(DURelease_o)
}
DUOutflow <- function() {
	if (ResetStorage() == 1) {
		DUOutflow_o <- 0
	} else {
		DUOutflow_o <- DURelease()
	}
	return(DUOutflow_o)
}
########################### LIBBY DAM ####################################

LBFullPoolVol <- 5855100
InitLBLink <- 5654926.179
LBBotVol <- 900300

LBHistStor <- function() {
	LBHistStor_o <- LBHistStor_input[week_counter, 2]
	return(LBHistStor_o)
}
InitLB <- function() {
	if (InitialConditionSwitch == 0) {
		InitLB_o <- InitLBLink
	} else if (InitialConditionSwitch == 1) {
		InitLB_o <- ResInitFractionFull * LBFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitLB_o <- LBHistStor()
	} else {
		InitLB_o <- LBFullPoolVol
	}
	return(InitLB_o)
}
Libby <- function() {
	if (week_counter == 1) {
		Libby_o <- InitLB()
	} else {
		Libby_o <- reservoir_vol_df[week_counter - 1, 5]
	}
	return(Libby_o)
}
LibbyFlowData <- function() {
	return(PriVICLB)
}
LBSufaceArea <- function() {
	LBSufaceArea_o <- (-1.21281443E-13 * (Libby() / 1000)^4 + 1.53692112E-09 * (Libby() / 1000)^3 - 6.75961255E-06 * (Libby() / 1000)^2 + 1.87278268E-02 * (Libby() / 1000) + 2.30403996) * 1000
	return(LBSufaceArea_o)
}
LibbyEvapData <- function() {
	LibbyEvapData_o <- 0
	return(LibbyEvapData_o)
}
LBEvap <- function() {
	LBEvap_o <- (LBSufaceArea() * LibbyEvapData()) * 0.5042 / 12
	return(LBEvap_o)
}
LBIn <- function() {
	LBIn_o <- LibbyFlowData() - LBEvap() - DemVICLB
	return(LBIn_o)
}
LBInflow <- function() {
	if (ResetStorage() == 1) {
		LBInflow_o <- InitLB() - Libby()
	} else {
		LBInflow_o <- LBIn()
	}
	return(LBInflow_o)
}
LBRuleReq <- function() {
	LBRuleReq_o <- max(Libby() + LBIn() - LBTopVol(), 0)
	return(LBRuleReq_o)
}
LBAvgMin <- function() {
	LBAvgMin_o <- 2000 
	return(LBAvgMin_o)
}
LBMinReq <- function() {
	LBMinReq_o <- LBAvgMin()
	return(LBMinReq_o)
}
LBAvailAfter <- function() {
	LBAvailAfter_o <- max(0, Libby() + LBIn() - LBBotVol)
	return(LBAvailAfter_o)
}
LBPrelim <- function() {
	LBPrelim_o <- min(LBAvailAfter(), max(LBRuleReq(), LBMinReq()))
	return(LBPrelim_o)
}
LBDamProtectRel <- function() {
	LBDamProtectRel_o <- max(0, Libby() + LBInflow() - LBFullPoolVol)
	return(LBDamProtectRel_o)
}
LBRelLimit <- function() {
	LBRelLimit_o <- max(Libby() + LBInflow() - LBBotVol, 0)
	return(LBRelLimit_o)
}
LBNetHead <- function() {
	LBTailElev <- 2118
	LBLoss <- 0
	LBNetHead_o <- LBElev_ft() - LBTailElev - LBLoss
	return(LBNetHead_o)
}
LBPenLimit <- function() {
	LBGenPenCap <- 24100
	LBPenLimit_o <- LBGenPenCap * cfsTOafw
	return(LBPenLimit_o)
}
LibbyPreEnergy <- function() {
	LibbyPreEnergy_o <- MWhr_per_ftAcFt * min(LBPrelim(), LBPenLimit()) * LBNetHead() * LBCombEfficiency
	return(LibbyPreEnergy_o)
}
LBCriticalCurve <- function() {
	LBCriticalCurve_o <- LBCriticalCurve_input[week_counter_in_year(), 2]
	return(LBCriticalCurve_o)
}
LB1931Refill <- function() {
	LB1931Refill_o <- LB1931Refill_input[week_counter_in_year(), 2]
	return(LB1931Refill_o)
}
RefillCurve <- function() {
	# Refill rule curve based on 1931 inflows and forecasts.  If the refill switch is set to the status quo rules,
	# the model selects the 1931 based refill curve August-December and then selects the forecast refill curve for the remainder of the year.
	# If the refill switch is set to alternate rules, the forecast refill curve is used for the entire year.
	# The second option may allow more non-firm energy releases Aug-December than the status quo rules. Units af.
	if (RefillSwitch() == 1) {
		RefillCurve_o <- LBRefillVol1
	} else if (RefillSwitch() == 2) {
		RefillCurve_o <- LBRefillVol2
	} 
	return(RefillCurve_o)
}
LB_CurFC <- function() {
	if (LBSumAprAug < 4.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 2] # LBFlood1
	} else if (LBSumAprAug < 5.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 3] # LBFlood2
	} else if (LBSumAprAug < 6.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 4] # LBFlood3
	} else if (LBSumAprAug < 7.5E6) {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 5] # LBFlood4
	} else {
		LB_CurFC_o <- LBFlood_input[week_counter_in_year(), 6] # LBFlood5
	} 
	return(LB_CurFC_o)
}
LBCurFC_Cont <- function() { 
	LB_FloodM_Range <- c(4500000, 5000000, 5500000, 6000000, 6500000, 7000000, 7500000, 8000000)
	if (LB_FloodM_Range[length(LB_FloodM_Range)] < LBSumAprAug) {
		row_no <- length(LB_FloodM_Range)
	} else {
		row_no <- max(1.0, which(LB_FloodM_Range > LBSumAprAug)[1] - 1)
	}
	if (LBSumAprAug <= 8.0E06) {
		if (month_in_year==4) { # Nov
			LBCurFC_Cont_o <- 5355100
		} else if (month_in_year==5) { # Dec
			LBCurFC_Cont_o <- 3855100
		} else if (month_in_year==6) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 2] # JAN_LB
		} else if (month_in_year==7) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 3] # FEB_LB
		} else if (month_in_year==8) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 4] #  MAR_LB
		} else if (month_in_year==9) {
			LBCurFC_Cont_o <- MFlood_input[row_no, 5] #  APR_LB
		} else {
			LBCurFC_Cont_o <- LBFullPoolVol
		}
	} else if (LBSumAprAug > 8.0E06 && month_in_year %in% 4:9) {
		LBCurFC_Cont_o <- LBFlood_input[week_counter_in_year(), 5]
	} else { # Outside flood evacuation period
	  LBCurFC_Cont_o <- LBFullPoolVol
	}
	return(LBCurFC_Cont_o)
}
LibbyFloodCurve <- function() {
	if (FC_Option == 1) {
		LibbyFloodCurve_o <- LBFullPoolVol - GlobalFloodEvacMult * (LBFullPoolVol - LB_CurFC())
	} else if (FC_Option == 2) {
		LibbyFloodCurve_o <- LBFullPoolVol - GlobalFloodEvacMult * (LBFullPoolVol - LBCurFC_Cont())
	}
	return(LibbyFloodCurve_o)
}
LBTopVol <- function() {
	if (TopRuleSw() == 0) {
		LBTopVol_o <- LibbyFloodCurve()
	} else if (TopRuleSw() == 1) {
		LBTopVol_o <- LBFullPoolVol
	} else {
		LBTopVol_o <- LBFlood_input[week_counter_in_year(), 2]
	}
	return(LBTopVol_o)
}
APR_LB <- function() {
	if (which(APR_LB_input > LBSumAprAug)[1] > 1) {
		APR_LB_o <- APR_LB_input[which(APR_LB_input > LBSumAprAug)[1] - 1, 2]
	} else {
		APR_LB_o <- APR_LB_input[which(APR_LB_input > LBSumAprAug)[1], 2]
	}
	return(APR_LB_o)
}
LBApREVaq_Cont <- function() {
	if (LBSumAprAug <= 8.0E06) {
		LBApREVaq_Cont_o <- APR_LB()
	} else {
		LBApREVaq_Cont_o <- 875100
	}
	return(LBApREVaq_Cont_o)
}
Libby_April_Evac_Target <- function() {
	if (LBSumAprAug < 4.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 2] # LBFlood1_2
	} else if (LBSumAprAug < 5.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 3] # LBFlood2_2
	} else if (LBSumAprAug < 6.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 4] # LBFlood3_2
	} else if (LBSumAprAug < 7.5E6) {
		LBF_o <- LBF_input[week_counter_in_year(), 5] # LBFlood4_2
	} else {
		LBF_o <- LBF_input[week_counter_in_year(), 6]
	} 
	if (FC_Option == 2) {
		Libby_April_Evac_Target_o <- GlobalFloodEvacMult * (LBFullPoolVol - LBApREVaq_Cont())
	} else {
		Libby_April_Evac_Target_o <- GlobalFloodEvacMult * (LBFullPoolVol - LBF_o)
	}
	return(Libby_April_Evac_Target_o)
}
LBMcNaryDraftLimit <- function() {
	if (UseAllStorForMCNLG == 1) {
		LBMcNaryDraftLimit_o <- LBBotVol
	} else if (Fish_Pool_Alternative == 1) {
		LBMcNaryDraftLimit_o <- 4.975e6
	} else if (Fish_Pool_Alternative == 2) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 1.258E6)
	} else if (Fish_Pool_Alternative == 3) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 1.643E6)
	} else if (Fish_Pool_Alternative == 4) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 2.277E6)
	} else if (Fish_Pool_Alternative == 5) {
		LBMcNaryDraftLimit_o <- (LBFullPoolVol - 3.161E6)
	} else {
		LBMcNaryDraftLimit_o <- 4.975E6
	}
	return(LBMcNaryDraftLimit_o)
}
LBMcNarySharedWater <- function() {
	LBMcNarySharedWater_o <- max(0, Libby() + LBIn() - LBPrelim() - LBMcNaryDraftLimit())
	return(LBMcNarySharedWater_o)
}
LBMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		LBMcNarySup_o <- 0
	} else {
		LBMcNarySup_o <- min(LBMcNarySharedWater(), McNaryFlowDeficit() * LBMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(LBMcNarySup_o)
}
LBSharedWater <- function() {
	LBSharedWater_o <- max(0, Libby() + LBIn() - LBPrelim() - LBMcNarySup() - LBBotVol)
	return(LBSharedWater_o)
}
LBSupForVernitaBar <- function() {
	if (SumGCLBHHSharedWater() > 0) {
		LBSupForVernitaBar_o <- GCSupForVernitaBar() * LBVBFact() * LBSharedWater() / SumGCLBHHSharedWater()
	} else {
		LBSupForVernitaBar_o <- 0
	}
	return(LBSupForVernitaBar_o)
}
LBCombFlowSup <- function() {
	LBCombFlowSup_o <- max(LBMcNarySup(), LBSupForVernitaBar())
	return(LBCombFlowSup_o)
}
LBMcNarySupEnergy <- function() { # Hydropower generated by releasing water from Libby to meet McNary fish flow target
	LBMcNarySupEnergy_o <- LBCombFlowSup() * (LBNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBMcNarySupEnergy_o)
}
LBEnergyContent <- function() {
	LBEnergyContent_o <- LBSharedWater() * (LBNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBEnergyContent_o)
}
LBElev_ft <- function() {
	LBElev_ft_o <- 2.50182927E-31 * Libby()^5 - 4.25316189E-24 * Libby()^4 + 2.73998760E-17 * Libby()^3 - 8.64412557E-11 * Libby()^2 + 1.73222635E-04 * Libby() + 2.18520663E+03
	return(LBElev_ft_o)
}
LBECC <- function() {
	if (month_in_year <= 5 || month_in_year == 12) {
		out1 <- max(LBCriticalCurve(), RefillCurve())
	} else {
		out1 <- min(max(RefillCurve(), LBCriticalCurve()), max(LBCriticalCurve(), LB1931Refill()))
	}	
	LBECC_o <- min(LibbyFloodCurve(), out1)
	return(LBECC_o)
}
LBECCSharedWater <- function() {
	LBECCSharedWater_o <- max(0, Libby() + LBIn() - LBPrelim() - LBMcNarySup() - LBECC())
	return(LBECCSharedWater_o)
}
LBECCEnergyContent <- function() {
	LBECCEnergyContent_o <- LBECCSharedWater() * (LBNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBECCEnergyContent_o)
}
LBFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			LBFirmEngSup_o <- 0
		} else {
			LBFirmEngSup_o <- (LBEnergyContent() + LBECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		LBFirmEngSup_o <- 0
	} else {
		LBFirmEngSup_o <- LBECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(LBFirmEngSup_o)
}
LBNFEnergyContent <- function() {
	LBNFEnergyContent_o <- max(0, LBECCEnergyContent() - LBFirmEngSup())
	return(LBNFEnergyContent_o)
}
LBNonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		LBNonFirmEngSup_o <- 0
	} else {
		LBNonFirmEngSup_o <- LBNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
	return(LBNonFirmEngSup_o)
}
LBFirmEngSupReq <- function() {
	LBFirmEngSupReq_o <- min(LBPenLimit(), LBFirmEngSup() / (MWhr_per_ftAcFt * (LBNetHead() + TotalGCHead()) * LBCombEfficiency))
	return(LBFirmEngSupReq_o)
}
LBNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		LBNonFirmEngSupReq_o <- min(LBPenLimit(), (LBFirmEngSup() + LBNonFirmEngSup()) / (MWhr_per_ftAcFt * (LBNetHead() + TotalGCHead()) * LBCombEfficiency))
	} else {
		LBNonFirmEngSupReq_o <- 0
	}
	return(LBNonFirmEngSupReq_o)
}
LBEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		LBEnergySup_o <- max(min(LBFirmEngSupReq(), LBSharedWater()), min(LBNonFirmEngSupReq(), LBECCSharedWater()))
	} else {
		LBEnergySup_o <- max(min(LBFirmEngSupReq(), LBECCSharedWater()), min(LBNonFirmEngSupReq(), LBECCSharedWater()))
	}
	return(LBEnergySup_o)
}
LBFloodSpace <- function() {
	LBFloodSpace_o <- min((LBPrelim() + LBMcNarySup() + LBEnergySup()), max(0, LBFullPoolVol - Libby() + LBIn() - LBPrelim() - LBEnergySup() - LBMcNarySup()))
	return(LBFloodSpace_o)
}
LBFloodFrac <- function() {
	if (TotalFloodSpace_c == 0) {
		LBFloodFrac_o <- 0
	} else {
		LBFloodFrac_o <- (LBFloodMult * LBFloodSpace()) / TotalFloodSpace_c
	}	
	return(LBFloodFrac_o)
}
LBRelReducReq <- function() {
	LBRelReducReq_o <- TotalRelReducReq() * LBFloodFrac()
	return(LBRelReducReq_o)
}
LBCombSup <- function() {
	LBCombSup_o <- LBCombFlowSup() + LBEnergySup()
	return(LBCombSup_o)
}
LBRelease <- function() {
	LBRelease_o <- max(LBDamProtectRel(), min(LBPrelim() + LBCombSup() - LBRelReducReq(), LBRelLimit()))
	return(LBRelease_o)
}
LBOutflow <- function() {
	if (ResetStorage() == 1) {
		LBOutflow_o <- 0
	} else {
		LBOutflow_o <- LBRelease()
	}
	return(LBOutflow_o)
}
########################### BONNERS FERRY ##############################

BonnersFerryFlowData <- function() {
	return(PriVICBONF)
}
BONFInc <- function() {
	BONFInc_o <- BonnersFerryFlowData() - LibbyFlowData()
	return(BONFInc_o)
}
BonnersFerry <- function() {
	BonnersFerry_o <- LBOutflow() + BONFInc() - DemVICBONF
	return(BonnersFerry_o)
}
######################################### CORRA LINN #####################################

CLBotVol <- 1.44e5
CLFullPool <- 816730
CL_April_Target <- 245000

#### CLRuleVol
# Corra Linn operates to International Joint Commission guidelines.  This agreement specifies reservoir elevations at five dates throughout the year.
# To include this curve in the model,  intermediate data points have been added for other months based on a linear interpolation between points.
# The January 7 storage value was assumed to occur on Dec 31 for simplicity.  Aug 31 corresponds to month 1.

CLIJCRuleCurve <- function() { 
	CLIJCRuleCurve_o <- CLIJCRuleCurve_input[week_counter_in_year(), 2]
	return(CLIJCRuleCurve_o)
}
CLRuleVol <- function() {
	CLRuleVol_o <- CLIJCRuleCurve()
	return(CLRuleVol_o)
}
CLHistStor <- function() {
	CLHistStor_o <- CLHistStor_input[week_counter, 2]
	return(CLHistStor_o)
}
InitCLLink <- function() {
	InitCLLink_o <- 310978.75
	return(InitCLLink_o)
}
InitCL <- function() {
	if (InitialConditionSwitch == 0) {
		InitCL_o <- InitCLLink()
	} else if (InitialConditionSwitch == 1) {
		InitCL_o <- ResInitFractionFull * CLRuleVol()
	} else if (InitialConditionSwitch == 2) {
		InitCL_o <- CLHistStor()
	} else {
		InitCL_o <- CLRuleVol()
	}
	return(InitCL_o)
}
CorraLinnReservoir <- function() {
	if (week_counter == 1) {
		CorraLinnReservoir_o <- InitCL()
		# reservoir_vol_df[week_counter,4]=CorraLinnReservoir_o
	} else {
		CorraLinnReservoir_o <- reservoir_vol_df[week_counter - 1, 4]
	}
	return(CorraLinnReservoir_o)
}
CorraLinnFlowData <- function() {
	return(PriVICCL)
}
CLInc <- function() {
	CLInc_o <- CorraLinnFlowData() - DuncanFlowData() - BonnersFerryFlowData()
	return(CLInc_o)
}
CLSufaceArea <- function() {
	CLSufaceArea_o <- (-1.21281443E-13 * (CorraLinnReservoir() / 1000)^4 + 1.53692112E-09 * (CorraLinnReservoir() / 1000)^3 -
    6.75961255E-06 * (CorraLinnReservoir() / 1000)^2 + 1.87278268E-02 * (CorraLinnReservoir() / 1000) + 2.30403996) * 1000
	return(CLSufaceArea_o)
}
CLEvapData <- function() {
	CLEvapData_o <- 0
	return(CLEvapData_o)
}
CLEvap <- function() {
	CLEvap_o <- (CLSufaceArea() * CLEvapData()) * 0.5042 / 12
	return(CLEvap_o)
}
CLIn <- function() {
	CLIn_o <- DUPrelim() + LBPrelim() + (CorraLinnFlowData() - DuncanFlowData() - LibbyFlowData()) - CLEvap() - DemVICCL - DemVICBONF
	return(CLIn_o)
}
CLAvailAfter <- function() {
	CLAvailAfter_o <- max(0, CorraLinnReservoir() + CLIn() - CLBotVol)
	return(CLAvailAfter_o)
}
CLCombEfficiency <- function() {
	CLCombEfficiency_o <- 0.8
	return(CLCombEfficiency_o)
}
CLInflow <- function() {
	if (ResetStorage() == 1) {
		CLInflow_o <- InitCL() - CorraLinnReservoir()
	} else {
		CLInflow_o <- DUOutflow() + BonnersFerry() + CLInc() - DemVICCL - CLEvap()
	}
	return(CLInflow_o)
}
CLRuleReq <- function() {
	CLRuleReq_o <- max(CorraLinnReservoir() + CLIn() - CLRuleVol(), 0)
	return(CLRuleReq_o)
}
CLMin <- function() {
  CLMin_o <- 5000 # the actual is a time series that includes 52, 5000 one for each week
  return(CLMin_o)
}
CLPenLimit <- function() {
	CLPenCap <- 12600
	CLPenLimit_o <- CLPenCap * cfsTOafw
	return(CLPenLimit_o)
}
CLFirmEngTarget <- function() {
	CLFirmEngTarget_o <- 0 # the actual data is a time-series of zeros for each week
	return(CLFirmEngTarget_o)
}
CLNonFirmTarget <- function() {
	CLNonFirmTarget_o <- 0
	return(CLNonFirmTarget_o)
}
CLCombEngRelReq <- function() {
	CLCombEngRelReq_o <- min(CLPenLimit(), (CLFirmEngTarget() + CLNonFirmTarget()) / (MWhr_per_ftAcFt * CLNetHead() * CLCombEfficiency()))
	return(CLCombEngRelReq_o)
}
CLMinReq <- function() {
	CLMinReq_o <- max(min(CLAvailAfter(), CLCombEngRelReq()), CLMin() * cfsTOafw)
	return(CLMinReq_o)
}
CLPrelim <- function() {
	CLPrelim_o <- min(CLAvailAfter(), max(CLRuleReq(), CLMinReq()))
	return(CLPrelim_o)
}
CLOutflow <- function() {
	if (ResetStorage() == 1) {
		CLOutflow <- 0
	} else {
		CLOutflow <- CLRelease_c
	}
	return(CLOutflow)
}
CLElev_ft <- function() {
	CLElev_ft_o <- -4.15160146E-13 * CorraLinnReservoir()^2 + 9.39564626E-06 * CorraLinnReservoir() + 1.73792103E+03
	return(CLElev_ft_o)
}
CLNetHead <- function() {
	CLTailElev <- 1686
	CLLoss <- 0
	CLNetHead_o <- CLElev_ft() - CLTailElev - CLLoss
	return(CLNetHead_o)
}
CLCombSup <- function() {
	CLCombSup_o <- DUCombSup() + LBCombSup()
	return(CLCombSup_o)
}
CLRelLimit <- function() {
	CLRelLimit_o <- max(CorraLinnReservoir() + CLInflow() - CLBotVol, 0)
	return(CLRelLimit_o)
}
CLRelease <- function() {
	CLRelease_o <- min(CLPrelim() + CLCombSup(), CLRelLimit())
	return(CLRelease_o)
}
############### HUNGRY HORSE ################## 

InitHHLink <- 3286250
HHFullPoolVol <- 3647e3
HHBotVol <- 4.86e5

InitHH <- function() {
	HHHistStor <- HHStor[week_counter, 2]
	if (InitialConditionSwitch == 0) {
		InitHH_o <- InitHHLink
	} else if (InitialConditionSwitch == 1) {
		InitHH_o <- ResInitFractionFull * HHFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitHH_o <- HHHistStor
	} else {
		HHFullPoolVol
	}
	return(InitHH_o)
}
HungryHorse <- function() {
	if (week_counter == 1) {
		HungryHorse_o <- InitHH()
	} else {
		HungryHorse_o <- reservoir_vol_df[week_counter - 1, 6]
	}	
	return(HungryHorse_o)
}
HungryHorseFlowData <- function() {
	return(PriVICHH)
}
HHEvap <- function() {
	HHEvapData <- 0
	HHEvap_o <- HHSufaceArea() * HHEvapData * 0.5042 / 12
	return(HHEvap_o)
}
HHSufaceArea <- function() {
	HHSufaceArea_o <- (-1.21281443E-13 * (HungryHorse() / 1000)^4 + 1.53692112E-09 * (HungryHorse() / 1000)^3 -
    6.75961255E-06 * (HungryHorse() / 1000)^2 + 1.87278268E-02 * (HungryHorse() / 1000) + 2.30403996) * 1000
	return(HHSufaceArea_o)
}
HHIn <- function() {
	HHIn_o <- HungryHorseFlowData() - HHEvap() - DemVICHH
	return(HHIn_o)
}
HH_CurFC <- function() {
	if (HHSumMaySept < 1.0E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 2] # HHFlood1
	} else if (HHSumMaySept < 1.4E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 3] # HHFlood2
	} else if (HHSumMaySept < 1.6E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 4] # HHFlood3
	} else if (HHSumMaySept < 2.0E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 5] # HHFlood4
	} else if (HHSumMaySept < 2.2E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 6] # HHFlood5
	} else if (HHSumMaySept < 2.5E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 7] # HHFlood6
	} else if (HHSumMaySept < 2.8E6) {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 8] # HHFlood7
	} else {
		HH_CurFC_o <- HHFlood[week_counter_in_year(), 9]
	} # HHFlood
	return(HH_CurFC_o)
}
HHCurFC_Cont <- function() {
	JanToAprFlood_Range <- c(1000000, 1500000, 2000000, 2500000, 3000000, 3500000)
	if (JanToAprFlood_Range[length(JanToAprFlood_Range)] < HHSumMaySept) {
		row_no <- length(JanToAprFlood_Range)
	} else {
		row_no <- max(1.0, which(JanToAprFlood_Range > HHSumMaySept)[1] - 1)
	}
	if (HHSumMaySept <= 3.68E06) {
		if (month_in_year == 3 || month_in_year == 4) { # Oct or Nov
			HHCurFC_Cont_o <- 3547000
		} else if (month_in_year == 5) {
			HHCurFC_Cont_o <- 3397000
		} else if (month_in_year == 6) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 2] # JAN_HH
		} else if (month_in_year == 7) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 3] # FEB_HH
		} else if (month_in_year == 8) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 4] # MAR_HH
		} else if (month_in_year == 9) {
			HHCurFC_Cont_o <- JanToAprFlood[row_no, 5] # APR_HH
		} else {
			HHCurFC_Cont_o <- HHFullPoolVol
		}
	} else if (HHSumMaySept > 3.68E06 && month_in_year %in% 3:9) {
		HHCurFC_Cont_o <- HHFlood[week_counter_in_year(), 9]
	} else {
	  HHCurFC_Cont_o <- HHFullPoolVol
	}
	return(HHCurFC_Cont_o)
}
HHFloodCurve <- function() {
	if (FC_Option == 1) {
		HHFloodCurve_o <- HHFullPoolVol - GlobalFloodEvacMult * (HHFullPoolVol - (HH_CurFC()))
	} else if (FC_Option == 2) {
		HHFloodCurve_o <- HHFullPoolVol - GlobalFloodEvacMult * (HHFullPoolVol - (HHCurFC_Cont()))
	}
	return(HHFloodCurve_o)
}
HHTopVol <- function() {
	if (TopRuleSw() == 0) {
		HHTopVol_o <- HHFloodCurve()
	} else if (TopRuleSw() == 1) {
		HHTopVol_o <- HHFullPoolVol
	} else {
		HHTopVol_o <- HHFlood[week_counter_in_year(), 2]
	} # HHFlood1
	return(HHTopVol_o)
}
HHRuleReq <- function() {
	HHRuleReq_o <- max(HungryHorse() + HHIn() - HHTopVol(), 0)
	return(HHRuleReq_o)
}

#### ColFallsTarget 
# Columbia falls obligation to Columbia Falls Flow Target is based upon forecasted inflows from April through August.  The target is as follows (units cfs)
# Forecast - target
# Above 1790 taf - 3500 cfs
# Below 1190 taf - 3200 cfs
# between two values is a linear interpolation.

ColFallsTarget <- function() {
	# not sure how these values are calculated and what the actual time step is
	if (ColFall_Target[length(ColFall_Target[, 3]), 2] < HHInQ_AprAug / 1000) {
		row_no <- length(ColFall_Target[,3])
	} else {
		row_no <- which(ColFall_Target[,2] > HHInQ_AprAug / 1000)[1]
	}
	ColFallsTarget_o <- ColFall_Target[row_no,3]
	return(ColFallsTarget_o)
}
####### HHMin
# Minimum release from Hungry Horse for Columbia Falls.  Units cfs.
# Jan to Aug 400 to 900 cfs
# variable based upon predicted inflows between april and august

HHMin <- function() {
	HHMin_o <- 400 # prescribed to be 400 cfs in original ColSim
	return(HHMin_o)
}
###### HHRelForColFalls # this code is a part of "Col Falls Target" calculations
# Minimum Flows at the Columbia Falls are required to be at least 3,500 cfs, with maximum values  of 4500 cfs Oct-Dec.
# Hungry Horse must make sufficient average releases to support these flows on a monthly time frame.
# The model assumes that Hungry Horse supplies all supplements to natural inflow required to meet the Columbia Falls Target.

HHRelForColFalls <- function() {
	HHRelForColFalls_o <- max(0, ColFallsTarget() * cfsTOafw - (ColumbiaFallsFlowData() - HungryHorseFlowData() - DemVICCOL))
	return(HHRelForColFalls_o)
}
HHMinReq <- function() {
	if (RefillMinSw() == 1) {
    HHMinReq_o <- 0
	} else {
		HHMinReq_o <- max(HHMin() * cfsTOafw, HHRelForColFalls())
	}
	return(HHMinReq_o)
}
HHAvailAfter <- function() {
	HHAvailAfter_o <- max(0, HungryHorse() + HHIn() - HHBotVol)
	return(HHAvailAfter_o)
}
HHFloodSpace <- function() {
	HHFloodSpace_o <- 0 
	return(HHFloodSpace_o)
}
HHPrelim <- function() {
	HHPrelim_o <- min(HHAvailAfter(), max(HHRuleReq(), HHMinReq()))
	return(HHPrelim_o)
}
HHInflow <- function() {
	if (ResetStorage() == 1) {
		HHInflow_o <- InitHH() - HungryHorse()
	} else {
		HHInflow_o <- HHIn()
	}
	return(HHInflow_o)
}
HHOutflow <- function() {
	if (ResetStorage() == 1) {
		HHOutflow_o <- 0
	} else {
		HHOutflow_o <- HHRelease_c
	}
	return(HHOutflow_o)
}
HHPenLimit <- function() {
	HHpen <- 12048
	HHPenLimit_o <- HHpen * cfsTOafw
	return(HHPenLimit_o)
}
HHNetHead <- function() {
	HHTailElev <- 3082.6
	HHLoss <- 0
	HHNetHead_o <- HHElev_ft() - HHTailElev - HHLoss
	return(HHNetHead_o)
}
HungryHorsePreEnergy <- function() {
	HungryHorsePreEnergy_o <- MWhr_per_ftAcFt * min(HHPrelim(), HHPenLimit()) * HHNetHead() * HHCombEfficiency
	return(HungryHorsePreEnergy_o)
}
HHVBFract <- function() {
	HHVBFract <- 0
	return(HHVBFract)
}
HHSupForVernitaBar <- function() {
  # Minimum Flows at the Vernita Bar are required to be at least 36,000 cfs at all times and approximately 70,000 cfs Dec-May.
  # In actual operations, the Dec-May flow value is determined in part by the pREVious years flows, but this aspect of the system is not modeled at this time.
  # While in actual practice some storage is available for support of these flows from run-of-river projects,
  # Grand Coulee must generally make sufficient average releases to support these flows on a monthly time frame.
  # The model assumes that Grand Coulee supplies all supplements to natural inflow required to meet the Vernita Bar Target.
	if (SumGCLBHHSharedWater() > 0) {
		HHSupForVernitaBar_o <- GCSupForVernitaBar() * HHVBFrat() * HHSharedWater() / SumGCLBHHSharedWater()
	} else {
		HHSupForVernitaBar_o <- 0
	}
	return(HHSupForVernitaBar_o)
}
APR_HH <- function() {
	if (HHSumMaySept < APR_HH_input[1, 1]) {
		APR_HH_o <- APR_HH_input[1, 2]
	} else if (HHSumMaySept < APR_HH_input[1, 1]) {
		APR_HH_o <- APR_HH_input[1, 2]
	} else if (HHSumMaySept < APR_HH_input[2, 1]) {
		APR_HH_o <- APR_HH_input[2, 2]
	} else if (HHSumMaySept < APR_HH_input[3, 1]) {
		APR_HH_o <- APR_HH_input[3, 2]
	} else if (HHSumMaySept < APR_HH_input[4, 1]) {
		APR_HH_o <- APR_HH_input[4, 2]
	} else if (HHSumMaySept < APR_HH_input[5, 1]) {
		APR_HH_o <- APR_HH_input[5, 2]
	} else {
		APR_HH_o <- APR_HH_input[6, 2]
	}
	return(APR_HH_o)
}
HHApREVaq_Cont <- function() {
	if (HHSumMaySept <= 3.68E06) {
		HHApREVaq_Cont_o <- APR_HH()
	} else {
		HHApREVaq_Cont_o <- 665000
	}
	return(HHApREVaq_Cont_o)
}
HHorse_April_Evac_Target <- function() {
	if (HHSumMaySept < 1.0E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 2] # HHFlood1_2
	} else if (HHSumMaySept < 1.4E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 3] # HHFlood2_2
	} else if (HHSumMaySept < 1.6E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 4] # HHFlood3_2
	} else if (HHSumMaySept < 2.0E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 5] # HHFlood4_2
	} else if (HHSumMaySept < 2.2E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 6] # HHFlood5_2
	} else if (HHSumMaySept < 2.5E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 7] # HHFlood6_2
	} else if (HHSumMaySept < 2.8E6) {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 8] # HHFlood7_2
	} else {
		HHF_o <- HHFlood_2_input[week_counter_in_year(), 9] # HHFlood8_2
	} 
	if (FC_Option == 2) {
		HHorse_April_Evac_Target_o <- (GlobalFloodEvacMult * (HHFullPoolVol - HHApREVaq_Cont()))
	} else {
		HHorse_April_Evac_Target_o <- GlobalFloodEvacMult * (HHFullPoolVol - HHF_o)
	}
	return(HHorse_April_Evac_Target_o)
}
HHCriticalCurve <- function() {
	HHCriticalCurve_o <- HHCriticalCurve_input[week_counter_in_year(), 2]
	return(HHCriticalCurve_o)
}
###### HHRefillCurve
# Refill rule curve based on 1931 inflows and forecasts.  If the refill switch is set to the status quo rules,
# the model selects the 1931 based refill curve August-December and then selects the forecast refill curve for the remainder of the year.
# If the refill switch is set to alternate rules, the forecast refill curve is used for the entire year.
# The second option may allow more non-firm energy releases Aug-December than the status quo rules. Units af.

HHRefillCurve <- function() {
	if (RefillSwitch() == 1) {
		HHRefillCurve_o <- HHRefillVol1
	} else if (RefillSwitch() == 2) {
		HHRefillCurve_o <- HHRefillVol
	}
	return(HHRefillCurve_o)
}
HHMcNaryDraftLimit <- function() {
	if (UseAllStorForMCNLG == 1) {
		HHMcNaryDraftLimit_o <- HHBotVol
	} else if (Fish_Pool_Alternative == 1) {
		HHMcNaryDraftLimit_o <- 3.166e6
	} else if (Fish_Pool_Alternative == 2) {
		HHMcNaryDraftLimit_o <- HHFullPoolVol - 0.827E6
	} else if (Fish_Pool_Alternative == 3) {
		HHMcNaryDraftLimit_o <- HHFullPoolVol - 1.051E6
	} else if (Fish_Pool_Alternative == 4) {
		HHMcNaryDraftLimit_o <- HHFullPoolVol - 1.419E6
	} else if (Fish_Pool_Alternative == 5) {
		HHMcNaryDraftLimit_o <- HHFullPoolVol - 1.932E6
	} else {
		HHMcNaryDraftLimit_o <- 3.166e6
	}
	return(HHMcNaryDraftLimit_o)
}
HHMcNarySharedWater <- function() {
	HHMcNarySharedWater_o <- max(0, HungryHorse() + HHIn() - HHPrelim() - HHMcNaryDraftLimit())
	return(HHMcNarySharedWater_o)
}
HHMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		HHMcNarySup_o <- 0
	} else {
		HHMcNarySup_o <- min(HHMcNarySharedWater(), McNaryFlowDeficit() * HHMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(HHMcNarySup_o)
}
HHCombFlowSup <- function() {
	HHCombFlowSup_o <- max(HHMcNarySup(), HHSupForVernitaBar())
	return(HHCombFlowSup_o)
}
HHDownStreamHead <- function() {
	HHDownStreamHead_o <- AFNetHead() + BCNetHead() + BDNetHead() + CBNetHead() + KENetHead() + NOXNetHead() + TotalGCHead()
	return(HHDownStreamHead_o)
}
HHMcNarySupEnergy <- function() {
	HHMcNarySupEnergy_o <- HHCombFlowSup() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(HHMcNarySupEnergy_o)
}
HHSharedWater <- function() {
	HHSharedWater_o <- max(0, HungryHorse() + HHIn() - HHPrelim() - HHMcNarySup() - HHBotVol)
	return(HHSharedWater_o)
}
LBVBFract <- function() {
  LBVBFact_o <- 0
  return(LBVBFact_o)
}
SumGCLBHHSharedWater <- function() {
  SumGCLBHHSharedWater_o <- HHVBFract() * HHSharedWater() + LBVBFract() * LBSharedWater()
  return(SumGCLBHHSharedWater_o)
}
HHAssuredRefill <- function() {
	HHAssuredRefill_o <- HHAssuredRefill_input[month_in_year, 2]
	return(HHAssuredRefill_o)
}
HHBaseDraftLimit <- function() {
	HHBaseDraftLimit_o <- HHBaseDraftLimit_input[week_counter_in_year(), 2]
	return(HHBaseDraftLimit_o)
}
HHBiOpDraftLimit <- function() {
	if (month_in_year >= 6 && month_in_year <= 8) {
		HHBiOpDraftLimit_o <- HHRefillCurve()
	} else {
		HHBiOpDraftLimit_o <- HHBaseDraftLimit()
	}
	return(HHBiOpDraftLimit_o)
}
HHECC <- function() {
  # During the fixed period from August-December before the forecast, the ECC is the greater of the critical curve or the refill curve based on 1931 inflows.
  # In the variable period from January-July the VEEC can be lower than the ECC due to REVised refill curve (based on forecast).
  # The VEEC curve, however, cannot be higher than the original ECC curve.
  # The value is compared to the flood storage curve and the minimum value is selected.
  # The model will always release water to evacuate flood storage, and will release to the ECC and VECC if the user selects maximum allowed non-firm releases.
	if (month_in_year <= 5 || month_in_year == 12) {
		out1 <- max(HHCriticalCurve(), max(HHRefillCurve(), HHBiOpDraftLimit()))
	} else {
		out1 <- min(max(HHRefillCurve(), max(HHCriticalCurve(), HHBiOpDraftLimit())), max(HHCriticalCurve(), max(HHAssuredRefill(), HHBiOpDraftLimit())))
	}
	HHECC_o <- min(HHFloodCurve(), out1)
	return(HHECC_o)
}
HHEnergyContent <- function() {
	if (month_in_year >= 3 && month_in_year <= 5) {
		HHEnergyContent_o <- 0
	} else {
		HHEnergyContent_o <- HHSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	}
	return(HHEnergyContent_o)
}
HHECCSharedWater <- function() {
	HHECCSharedWater_o <- max(0, HungryHorse() + HHIn() - HHPrelim() - HHMcNarySup() - HHECC())
	return(HHECCSharedWater_o)
}
HHECCEnergyContent <- function() {
	if (month_in_year >= 3 && month_in_year <= 5) {
		HHECCEnergyContent_o <- 0
	} else {
		HHECCEnergyContent_o <- (HHECCSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt)
	}
	return(HHECCEnergyContent_o)
}
HHFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			HHFirmEngSup_o <- 0
		} else {
			HHFirmEngSup_o <- (HHEnergyContent() + HHECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		HHFirmEngSup_o <- 0
	} else {
		HHFirmEngSup_o <- HHECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(HHFirmEngSup_o)
}
HHFirmEngSupReq <- function() {
	HHFirmEngSupReq_o <- min(HHPenLimit(), HHFirmEngSup() / (MWhr_per_ftAcFt * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency))
	return(HHFirmEngSupReq_o)
}
HHNonFirmEngSup <- function() {
	if (NonFirmEnergyDeficit_c == -9999) {
		NonFirmEnergyDeficit_c <- NonFirmEnergyDeficit()
		energy_df[week_counter,7] <<- NonFirmEnergyDeficit_c
	}
	if (TotalNFEnergyContent_c == 0) {
		HHNonFirmEngSup_o <- 0
	} else {
		HHNonFirmEngSup_o <- HHNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
  return(HHNonFirmEngSup_o)
}
HHNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		HHNonFirmEngSupReq_o <- min(HHPenLimit(), (HHFirmEngSup() + HHNonFirmEngSup()) / (MWhr_per_ftAcFt * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency))
	} else {
		HHNonFirmEngSupReq_o <- 0
	}
	return(HHNonFirmEngSupReq_o)
}
HHNFEnergyContent <- function() {
	HHNFEnergyContent_o <- max(0, HHECCEnergyContent() - HHFirmEngSup())
	return(HHNFEnergyContent_o)
}
HHElev_ft <- function() {
	HHElev_ft_o <- 4.84974275E-18 * HungryHorse()^3 - 4.48065506E-11 * HungryHorse()^2 + 1.81102983E-04 * HungryHorse() + 3.26045812E+03
	return(HHElev_ft_o)
}
HHEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		HHEngSup_o <- max(min(HHFirmEngSupReq(), HHSharedWater()), min(HHNonFirmEngSupReq(), HHECCSharedWater()))
	} else {
		HHEngSup_o <- max(min(HHFirmEngSupReq(), HHECCSharedWater()), min(HHNonFirmEngSupReq(), HHECCSharedWater()))
	}
	return(HHEngSup_o)
}
ColFallsMaxFlow <- function() {
# Maximum flow at Columbia Falls.  This flow is maintained unless it conflicts with flood storage evacuation requirements.  Units cfs.
	ColFallsMaxFlow_o <- ColFallsMaxFlow_input[month_in_year, 2]
	return(ColFallsMaxFlow_o)
}
HHColFallsmax <- function() {
	HHColFallsmax_o <- max(0, ColFallsMaxFlow() * cfsTOafw - (ColumbiaFallsFlowData() - HungryHorseFlowData() - DemVICCOL))
	return(HHColFallsmax_o)
}
##### HH_USBRmax
# Maximum outflow limited to turbine capacity 12,048 cfs
# Limited June-Labor Day by 6,800, unless this interferes with ESA reqs.
HH_USBRmax <- function() {
	HH_USBRmax_o <- HH_USBRmax_input[month_in_year, 2]
	return(HH_USBRmax_o)
}
HHEnergySupAllow <- function() { # Maximum water that can be released to meet energy requirement and flow objective at Columbia Falls
	HHEnergySupAllow_o <- max(0, min(HHColFallsmax() - HHPrelim(), HH_USBRmax() * cfsTOafw - HHPrelim()))
	return(HHEnergySupAllow_o)
}
HHCombSup <- function() {
	HHCombSup_o <- min(HHEngSup(), HHEnergySupAllow()) + HHCombFlowSup()
	return(HHCombSup_o)
}
HHRelease <- function() {
	HHRelease_o <- min(HHAvailAfter(), HHPrelim() + HHCombSup())
	return(HHRelease_o)
}

######### COLUMBIA FALLS ###################

ColumbiaFallsFlowData <- function() {
	return(PriVICCOL)
}
COLInc <- function() {
	COLInc_o <- ColumbiaFallsFlowData() - HungryHorseFlowData()
	return(COLInc_o)
}
ColumbiaFalls <- function() {
	ColumbiaFalls_o <- HHPrelim() + COLInc() - DemVICCOL
	return(ColumbiaFalls_o)
}
###################### KERR DAM ##################

KerrTopVolSw <- 1
KEFullPoolVol <- 1792e3
KEBotVol <- 572.8e3

# Kerr --------------------------------------------------------------------
# Cummulative flow data with agricultural withdrawals (1990 estimates)
# and estimated seasonal evaporation with existing dams included for the entire period of record at a constant level . Units acre-ft/month.

KerrFlowData <- function() {
	return(PriVICKE)
}
KETopVol <- function() {
	# Kerr is currently undergoing experimental changes in operation  that essentially hold the pool level at a constant value.
	# This switch allows the user to select the pREVious rule curves for flood storage, or the fixed full pool volume as the top of the conservation pool.  Options:
	# 0-Fixed full pool
	# 1-PREVious flood storage rule curves
	KerrFloodCurve <- KerrFloodC[week_counter_in_year(), 2]
	if (KerrTopVolSw == 0) {
		KETopVol_o <- KEFullPoolVol
	} else {
		KETopVol_o <- KerrFloodCurve
	}
	return(KETopVol_o)
}
Kerr_Reservoir <- function() {
	Kerr_Reservoir_o <- KETopVol()
	return(Kerr_Reservoir_o)
}
KEInc <- function() {
	KEInc_o <- KerrFlowData() - ColumbiaFallsFlowData()
	return(KEInc_o)
}
KEIn <- function() {
	KEIn_o <- HHPrelim() + (KerrFlowData() - HungryHorseFlowData()) - KEEvap() - DemVICKE - DemVICCOL
	return(KEIn_o)
}
KEEvap <- function() { 
	KEEvapData <- 0
	KEEvap_o <- KESufaceArea() * KEEvapData * 0.5042 / 12
	return(KEEvap_o)
}
KESufaceArea <- function() {
	KESufaceArea_o <- Kerr_Reservoir() * 0
	return(KESufaceArea_o)
}
KEFirmEngTarget <- function() {
	KEFirmEngTarget_o <- 0 
	return(KEFirmEngTarget_o)
}
KENonFirmTarget <- function() {
	KENonFirmTarget_o <- 0 
	return(KENonFirmTarget_o)
}
KEPenLimit <- function() {
	KEPenCap <- 14350 # cfs
	KEPenLimit_o <- KEPenCap * cfsTOafw
	return(KEPenLimit_o)
}
KEElev_ft <- function() {
	KEElev_ft_o <- -3.41281327E-13 * Kerr_Reservoir()^2 + 9.04645500E-06 * Kerr_Reservoir() + 2.87787155E+03
	return(KEElev_ft_o)
}
KENetHead <- function() {
	KETailElev <- 2706 # Tailwater elevation.  Units ft.
	KELoss <- 0 # Piping head losses.  Units ft.
	KENetHead_o <- KEElev_ft() - KETailElev - KELoss
	return(KENetHead_o)
}
KECombEngRelReq <- function() {
	KECombEfficiency <- 0.8
	KECombEngRelReq_o <- min(KEPenLimit(), 4.2603e7 * (KEFirmEngTarget() + KENonFirmTarget()) / (43560 * KENetHead() * KECombEfficiency))
	return(KECombEngRelReq_o)
}
KERuleReq <- function() {
	KERuleReq_o <- max(Kerr_Reservoir() + KEIn() - KETopVol(), 0)
	return(KERuleReq_o)
}
KEAvailAfter <- function() {
	KEAvailAfter_o <- max(0, Kerr_Reservoir() + KEIn() - KEBotVol)
	return(KEAvailAfter_o)
}
######  Article_56
# License Requirement for Kerr - Article 56
# Min Flows for fish (cfs)

Article_56 <- function() {
	Article_56_o <- Article_56_input[week_counter_in_year(), 2]
	return(Article_56_o)
}
KEMinReq <- function() {
	KEMinReq_o <- max(min(KEAvailAfter(), KECombEngRelReq()), Article_56() * cfsTOafw)
	return(KEMinReq_o)
}
KEPrelim <- function() {
	KEPrelim_o <- min(KEAvailAfter(), max(KERuleReq(), KEMinReq()))
	return(KEPrelim_o)
}
KECombSup <- function() {
	KECombSup_o <- HHCombSup()
	return(KECombSup_o)
}
KEInflow <- function() {
	KEInflow_o <- ColumbiaFalls() + KEInc() - DemVICKE - KEEvap()
	return(KEInflow_o)
}
KEOutflow <- function() {
	KEOutflow_o <- KERelease_c
	return(KEOutflow_o)
}
KEPreEnergy <- function() {
	KEPreEnergy_o <- MWhr_per_ftAcFt * min(KEPrelim(), KEPenLimit()) * KENetHead() * KECombEfficiency
	return(KEPreEnergy_o)
}
KerrGrPreEnergy <- function() {
	KerrGrPreEnergy_o <- CBPreEnergy() + KEPreEnergy() + NOXPreEnergy()
	return(KerrGrPreEnergy_o)
}
KRFloodSpace <- function() {
	KRFloodSpace_o <- 0
	return(KRFloodSpace_o)
}
Kerr_April_Target <- function() {
	Kerr_April_Target_o <- 579583
	return(Kerr_April_Target_o)
}
KERelLimit <- function() {
	KERelLimit_o <- max(Kerr_Reservoir() + KEInflow() - KEBotVol, 0)
	return(KERelLimit_o)
}
KERelease <- function() {
	KERelease_o <- min(KERelLimit(), KEPrelim() + KECombSup())
	return(KERelease_o)
}
######################## NOXON ##################

NoxonFlowData <- function() {
	return(PriVICNOX)
}
NOXInc <- function() {
	NOXInc_o <- NoxonFlowData() - KerrFlowData()
	return(NOXInc_o)
}
NOXPenLimit <- function() {
	NOXPenCap <- 50000
	NOXPenLimit_o <- NOXPenCap * cfsTOafw
	return(NOXPenLimit_o)
}
NOXPrelim <- function() {
	NOXPrelim_o <- KEPrelim() + NOXInc() - DemVICNOX
	return(NOXPrelim_o)
}
NOXNetHead <- function() {
	NOXNetHead_o <- 152
	return(NOXNetHead_o)
}
NOXPreEnergy <- function() {
	NOXPreEnergy_o <- MWhr_per_ftAcFt * min(NOXPrelim(), NOXPenLimit()) * NOXNetHead() * NOXCombEfficiency
	return(NOXPreEnergy_o)
}
NOXIn <- function() {
	NOXIn_o <- KEPrelim() + NOXInc() - DemVICNOX
	return(NOXIn_o)
}
NOXOut <- function() {
	NOXOut_o <- NOXIn()
	return(NOXOut_o)
}
################### CABINET #################################

CabinetFlowData <- function() {
	return(PriVICCB)
}
CBIn <- function() {
	CBIn_o <- NOXPrelim() + CBInc() - DemVICCB
	return(CBIn_o)
}
CBInc <- function() {
	CBInc_o <- CabinetFlowData() - NoxonFlowData()
	return(CBInc_o)
}
CBOut <- function() {
	CBOut_o <- CBIn()
	return(CBOut_o)
}
CBPenLimit <- function() {
	CBPenCap <- 35700
	CBPenLimit_o <- CBPenCap * cfsTOafw
	return(CBPenLimit_o)
}
CBPrelim <- function() {
	UpDemand <- DemVICNOX + DemVICCB
	IncFlow <- CabinetFlowData() - KerrFlowData()
	CBPrelim_o <- KEPrelim() + IncFlow - UpDemand
	return(CBPrelim_o)
}
CBNetHead <- function() {
	CBNetHead_o <- 97.2
	return(CBNetHead_o)
}
CBPreEnergy <- function() {
	CBPreEnergy_o <- MWhr_per_ftAcFt * min(CBPrelim(), CBPenLimit()) * CBNetHead() * CBCombEfficiency
	return(CBPreEnergy_o)
}
############### ALBENI FALLS #################################

AFBotVol <- 479.4e3
AFFullPoolVol <- 1519e3 # Volume corresponding to 2062.5 ft of elevation.  Normal full pool.  Units acre-ft.
AFalls_April_Target <- 829390
# AFPrelim ----------------------------------------------------------------
# Preliminary release based upon rule requirements and minimum releases.
# The release cannot be larger than the available water after withdrawals.  Units cfs-days/m

AFFloodCurve <- function() {
	AFFloodCurve_o <- AFFlood[week_counter_in_year(), 2]
	return(AFFloodCurve_o)
}
AFTopVol <- function() {
	AFTopVol_o <- AFFloodCurve()
	return(AFTopVol_o)
}
AlbeniFalls <- function() {
	AlbeniFalls_o <- AFTopVol()
	return(AlbeniFalls_o)
}
AlbeniFallFlowData <- function() {
	return(PriVICAF)
}
AFInc <- function() {
	AFInc_o <- AlbeniFallFlowData() - CabinetFlowData()
	return(AFInc_o)
}
AFCombEfficiency <- function() {
	AFCombEfficiency_o <- 0.8
	return(AFCombEfficiency_o)
}
AFAvgMin <- function() {
	AFAvgMin_o <- AFAvgMin_input[week_counter_in_year(), 2]
	return(AFAvgMin_o)
}
AFEvap <- function() {
	AFEvapData <- 0
	AFEvap_o <- AFSufacearea() * AFEvapData * 0.5042 / 12
	return(AFEvap_o)
}
AFSufacearea <- function() {
	AFSufacearea_o <- (-1.21281443E-13 * (AlbeniFalls() / 1000)^4 + 1.53692112E-09 * (AlbeniFalls() / 1000)^3 -
    6.75961255E-06 * (AlbeniFalls() / 1000)^2 + 1.87278268E-02 * (AlbeniFalls() / 1000) + 2.30403996) * 1000
	return(AFSufacearea_o)
}
AFIn <- function() {
	AFIn_o <- CBPrelim() + AFInc() - AFEvap() - DemVICAF
	return(AFIn_o)
}
AFInflow <- function() {
	AFInflow_o <- CBOut() + AFInc() - AFEvap() - DemVICAF
	return(AFInflow_o)
}
AFRelLimit <- function() {
	AFRelLimit_o <- max(AlbeniFalls() + AFInflow() - AFBotVol, 0)	
	return(AFRelLimit_o)
}
AFAvailAfter <- function() {
	AFAvailAfter_o <- max(AlbeniFalls() + AFIn() - AFBotVol, 0)
	return(AFAvailAfter_o)
}
AFFirmEngTarget <- function() {
	AFFirmEngTarget_o <- 0 
	return(AFFirmEngTarget_o)
}
AFNonFirmTarget <- function() {
	AFNonFirmTarget_o <- 0 
	return(AFNonFirmTarget_o)
}
AFCombEngRelReq <- function() {
	AFCombEngRelReq_o <- min(AFPenLimit(), (AFFirmEngTarget() + AFNonFirmTarget()) / (MWhr_per_ftAcFt * AFNetHead() * AFCombEfficiency()))
	return(AFCombEngRelReq_o)
}
AFMinReq <- function() {
	AFMinReq_o <- max(min(AFAvailAfter(), AFCombEngRelReq()), AFAvgMin() * cfsTOafw)
	return(AFMinReq_o)
}
AFPenLimit <- function() {
	AFPenCap <- 33000
	AFPenLimit_o <- AFPenCap * cfsTOafw
	return(AFPenLimit_o)
}
AFElev_ft <- function() {
	AFElev_ft_o <- -4.55570216E-13 * AlbeniFalls()^2 + 1.20161922E-05 * AlbeniFalls() + 2.04508013E+03
	return(AFElev_ft_o)
}
AFCombSup <- function() {
	AFCombSup_o <- KECombSup()
	return(AFCombSup_o)
}
AFRuleReq <- function() {
	AFRuleReq_o <- max(AlbeniFalls() + AFIn() - AFTopVol(), 0)
	return(AFRuleReq_o)
}
AFPrelim <- function() {
	AFPrelim_o <- min(AFAvailAfter(), max(AFRuleReq(), AFMinReq()))
	AFPrelim_c <<- AFPrelim_o
	return(AFPrelim_o)
}
AFNetHead <- function() {
	AFTailElev <- 2042.2 # Tailwater elevation.  Units ft.
	AFLoss <- 0
	AFNetHead_o <- AFElev_ft() - AFTailElev - AFLoss
	return(AFNetHead_o)
}
AFPreEnergy <- function() {
	AFPreEnergy_o <- MWhr_per_ftAcFt * min(AFPrelim(), AFPenLimit()) * AFNetHead() * AFCombEfficiency()
	return(AFPreEnergy_o)
}
AlbeniFallsGroupPreEnergy <- function() {
	AlbeniFallsGroupPreEnergy_o <- AFPreEnergy() + BCPreEnergy() + BDPreEnergy()
	return(AlbeniFallsGroupPreEnergy_o)
}
AFRelease <- function() {
  AFRelease_o <- min(AFPrelim() + AFCombSup(), AFRelLimit())
  return(AFRelease_o)
}
AFOutflow <- function() {
  AFOutflow_o <- AFRelease_c
  return(AFOutflow_o)
}

#################### BOUNDARY #########################

BoundaryFlowData <- function() {
	return(PriVICBD)
}
BDInc <- function() {
	BDInc_o <- BoundaryFlowData() - AlbeniFallFlowData()
	return(BDInc_o)
}
BDIn <- function() {
	BDIn_o <- AFOutflow() + BDInc() - DemVICBD
	return(BDIn_o)
}
BDOut <- function() {
	BDOut_o <- BDIn()
	return(BDOut_o)
}
BDNetHead <- function() {
	BDNetHead_o <- 97.2
	return(BDNetHead_o)
}
BDPenLimit <- function() {
	BDPenCap <- 53000
	BDPenLimit_o <- BDPenCap * cfsTOafw
	return(BDPenLimit_o)
}
BDPrelim <- function() {
	BDPrelim_o <- AFPrelim() + BDInc() - DemVICBD
	return(BDPrelim_o)
}
BDPreEnergy <- function() {
	BDPreEnergy_o <- MWhr_per_ftAcFt * min(BDPrelim(), BDPenLimit()) * BDNetHead() * BDCombEfficiency
	return(BDPreEnergy_o)
}

######################### GRAND COULEE ########################################

GCFullPoolVol <- 9.1073000E+06
InitGCLink <- 8332155.071
GCBotVol <- 3.9219E+06
GCTailElev <- 947 # Tailwater elevation.  Units ft.

GCMcNaryDraftLimit <- function() {
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
	return(GCMcNaryDraftLimit_o)
}
GrandCoulee <- function() {
	if (week_counter == 1) {
		GrandCoulee_o <- InitGC()
		# reservoir_vol_df[week_counter,7]=GrandCoulee_o
	} else {
		GrandCoulee_o <- reservoir_vol_df[week_counter - 1, 7]
	}
	return(GrandCoulee_o)
}
######### GCHistStor
GCHistStor <- function() {
	GCHistStor_o <- HistStor[week_counter, 8]
	return(GCHistStor_o)
}
InitGC <- function() {
	if (InitialConditionSwitch == 0) {
		InitGC_o <- InitGCLink
	} else if (InitialConditionSwitch == 1) {
		InitGC_o <- ResInitFractionFull * GCFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitGC_o <- GCHistStor()
	} else {
		InitGC_o <- GCFullPoolVol
	}
	return(InitGC_o)
}
GrandCouleeFlowData <- function() {
	return(PriVICGC)
}
GCInc <- function() {
	GCInc_o <- GrandCouleeFlowData() - BoundaryFlowData() - ArrowFlowData() - CorraLinnFlowData()
	return(GCInc_o)
}
GCEvap <- function() {
	GCEvapData <- 0
	GCEvap_o <- GCSufaceArea() * GCEvapData * 0.5042 / 12
	return(GCEvap_o)
}
GCSufaceArea <- function() {
	GCSufaceArea_o <- (-1.21281443E-13 * (GrandCoulee() / 1000)^4 + 1.53692112E-09 * (GrandCoulee() / 1000)^3 -
    6.75961255E-06 * (GrandCoulee() / 1000)^2 + 1.87278268E-02 * (GrandCoulee() / 1000) + 2.30403996) * 1000
	return(GCSufaceArea_o)
}
GCRefillMin <- function() {
	GCRefillMin_o <- GCRefillMin_input[week_counter_in_year(), 2]
	return(GCRefillMin_o)
}
GCIn <- function() { # We do not calculate Prelim() for the immediately upstream dam, Boundary, so we need to subtract the Boundary demands
	GCIn_o <- AFPrelim() + ARPrelim() + CLPrelim() + (GrandCouleeFlowData() - AlbeniFallFlowData() - ArrowFlowData() - CorraLinnFlowData()) - GCEvap() - DemVICGC - DemVICBD
	return(GCIn_o)
}
Canada_Outflows <- function() {
  CanadaOutflows_o <- AROutflow() + CLOutflow()
  return(CanadaOutflows_o)
}
GCInflow <- function() {
	if (ResetStorage() == 1) {
		GCInflow_o <- InitGC() - GrandCoulee()
	} else {
		GCInflow_o <- GCInc() + Canada_Outflows() + BDOut() - DemVICGC - GCEvap()
	}
	return(GCInflow_o)
}
GCAvgMin <- function() {
	GCAvgMin_o <- GCAvgMin_input[week_counter_in_year(), 2]
	return(GCAvgMin_o)
}
GCRuleReq <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c=water_df[week_counter,5]
	GCRuleReq_o <- max(GrandCoulee() + GCIn_c - GCTopVol(), 0)
	return(GCRuleReq_o)
}
GCMinReq <- function() {
	if (RefillMinSw() == 1) {
		GCMinReq_o <- GCRefillMin()
	} else {
		GCMinReq_o <- GCAvgMin() * cfsTOafw
	}
	return(GCMinReq_o)
}
GCAvailAfter <- function() {
	if(is.na(water_df[week_counter,5])){
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c=water_df[week_counter,5]
	GCAvailAfter_o <- max(0, GrandCoulee() + GCIn_c - GCBotVol)
	return(GCAvailAfter_o)
}
GCPrelim <- function() {
	GCPrelim_o <- min(GCAvailAfter(), max(GCRuleReq(), GCMinReq()))
	return(GCPrelim_o)
}
GCOutflow <- function() {
	if (ResetStorage() == 1) {
		GCOutflow_o <- 0
	} else {
		GCOutflow_o <- GCRelease_c # Called from GCRelease() in main program
	}
	return(GCOutflow_o)
}
GCMcNarySharedWater <- function() {
	if(is.na(water_df[week_counter,5])){
		water_df[week_counter,5] <<- GCIn() # Save GCIn() and write to file
	}
	GCIn_c = water_df[week_counter,5]
	GCMcNarySharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCMcNaryDraftLimit())
	return(GCMcNarySharedWater_o)
}
GCPreEnergy <- function() {
  GCPreEnergy_o <- MWhr_per_ftAcFt * min(GCPrelim(), GCPenLimit()) * GCNetHead() * GCCombEfficiency
  return(GCPreEnergy_o)
}
GrandCouleePreEnergy <- function() {
	GrandCouleePreEnergy_o <- CJPreEnergy() + GCPreEnergy() + PRPreEnergy() + RIPreEnergy() + RRPreEnergy() + WAPreEnergy() + WEPreEnergy()
	return(GrandCouleePreEnergy_o)
}	
GCFloodEvacMult <- function() { # check # this function seems to have different values for different flood level but I only see one value (1)
	# for all the values of runoff
	# check this function
	GCFloodEvacMult_o <- 1 # this is actually a time series
	return(GCFloodEvacMult_o)
}
AprilUpstreamFloodEvacGC <- function() {
	AprilUpstreamFloodEvacGC_o <- (AFFullPoolVol - AFalls_April_Target) + AR_April_Evac_Target() + (CLFullPool - CL_April_Target) + Duncun_April_Evac_Target() + HHorse_April_Evac_Target() +
    (KEFullPoolVol - Kerr_April_Target()) + Libby_April_Evac_Target() + MI_April_Evac_Target()
	return(AprilUpstreamFloodEvacGC_o)
}
CorrectedDallesRunoff <- function() { # Dalles runoff minus the total storage space made available by April
	CorrectedDallesRunoff_o <- DallesRunoffAprAug - AprilUpstreamFloodEvacGC()
	return(CorrectedDallesRunoff_o)
}
GC_CurFC <- function() {
	if (CorrectedDallesRunoff() < 57E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood1
	} else if (CorrectedDallesRunoff() < 60E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 3] # GCFlood2
	} else if (CorrectedDallesRunoff() < 63.25E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 4] # GCFlood3
	} else if (CorrectedDallesRunoff() < 65E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 5] # GCFlood4
	} else if (CorrectedDallesRunoff() < 67.66E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 6] # GCFlood5
	} else if (CorrectedDallesRunoff() < 71.0E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 7] # GCFlood6
	} else if (CorrectedDallesRunoff() < 80.0E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 8] # GCFlood7
	} else if (CorrectedDallesRunoff() < 90.0E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 9] # GCFlood8
	} else if (CorrectedDallesRunoff() < 100E6) {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 10] # GCFlood9
	} else {
		GC_CurFC_o <- GCFlood1_input[week_counter_in_year(), 11]
	}
	return(GC_CurFC_o)
}
GCCurFC_Cont <- function() {
	CorrectedDallesRunoff_Range <<- c(60000000, 65000000, 70000000, 75000000, 80000000, 85000000, 90000000, 95000000, 100000000, 105000000, 110000000, 115000000)
	if (CorrectedDallesRunoff_Range[length(CorrectedDallesRunoff_Range)] < CorrectedDallesRunoff()) {
		row_no <- length(CorrectedDallesRunoff_Range)
	} else {
		row_no <- max(1.0, which(CorrectedDallesRunoff_Range > CorrectedDallesRunoff())[1] - 1)
	}
	if (CorrectedDallesRunoff() <= 115.0E06) {
		if (month_in_year == 6) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 2] # JAN_GC
		} else if (month_in_year == 7) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 3] # FEB_GC
		} else if (month_in_year == 8) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 4] # MAR_GC
		} else if (month_in_year == 9) {
			GCCurFC_Cont_o <- GCF_Month_input[row_no, 5] # APR_GC
		} else {
			GCCurFC_Cont_o <- GCFullPoolVol
		}
	} else if (CorrectedDallesRunoff() > 115.0E06 && month_in_year %in% 6:9) {
		GCCurFC_Cont_o <- GCFlood1_input[week_counter_in_year(), 11]
	} else {
	  GCCurFC_Cont_o <- GCFullPoolVol
	}
	return(GCCurFC_Cont_o)
}
TotalGCHead = function() {
	TotalGCHead_o <- GCNetHead() + GCDownStreamHead()
	return(TotalGCHead_o)
}
GCElev_ft <- function() {
  GCElev_ft_o <- -8.86464496E-13 * GrandCoulee()^2 + 2.72969963E-05 * GrandCoulee() + 1.11465576E+03
  return(GCElev_ft_o)
}
GCNetHead <- function() {
	GCLoss <- 0
	GCNetHead_o <- GCElev_ft() - GCTailElev - GCLoss
	return(GCNetHead_o)
}
GCPenLimit <- function() {
	GCPenCap <- 280000
	GCPenLimit_o <- GCPenCap * cfsTOafw
	return(GCPenLimit_o)
}
GCMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		GCMcNarySup_o <- 0
	} else {
		GCMcNarySup_o <- min(GCMcNarySharedWater(), McNaryFlowDeficit() * GCMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(GCMcNarySup_o)
}
GCSharedWater <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c = water_df[week_counter,5]
	GCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCMcNarySup() - GCBotVol)
	return(GCSharedWater_o)
}
GCEnergyContent <- function() {
	GCEnergyContent_o <- GCSharedWater() * (GCNetHead() + GCDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCEnergyContent_o)
}
GCRefillCurve <- function() {
	if (RefillSwitch() == 1) {
		GCRefillCurve_o <- GCRefillVol1
	} else if (RefillSwitch() == 2) {
		GCRefillCurve_o <- GCRefillVol2
	}
	return(GCRefillCurve_o)
}
GCCriticalCurve <- function() {
	GCCriticalCurve_o <- GCCriticalCurve_input[week_counter_in_year(), 2]
	return(GCCriticalCurve_o)
}
######## GCFloodCurve
# Grand Coulee Flood Control Curves are modified according to March 1997 SRDs which are showed in USCOE Website
# (http://www.nwd-wc.usace.army.mil/cafe/forecast/SRD/GCL1997.pdf) (by Se-yeun Lee, Dec.05)
# Draft valuses at end of each month are given Carolyn in COE on Dec. 05

GCFloodCurve <- function() {
	if (FC_Option == 1) {
		GCFloodCurve_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - (GC_CurFC()))
	} else if (FC_Option == 2) {
		GCFloodCurve_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - (GCCurFC_Cont()))
	}
	return(GCFloodCurve_o)
}
GCTopVol <- function() {
	if (TopRuleSw() == 0) {
		GCTopVol_o <- GCFloodCurve()
	} else if (TopRuleSw() == 1) {
		GCTopVol_o <- GCFullPoolVol
	} else {
		GCTopVol_o <- GCFlood1_input[week_counter_in_year(), 2] # GCFlood1
	} 
	return(GCTopVol_o)
}
GCRecLimit <- function() {
	# Recreation requirements at Grand Coulee require that the ECC and VECC are limited to 1285 ft of elevation (storage=8.712e6 acre ft) from June 30 to Labor Day.
	# This model uses end-of-month storage targets to drive the model
	# calculations so the draft limit is in essence a lower bound for the end of month ECC and VECC for June, July, and August. Units acre-ft.
	GCRecLimit_o <- GCRecLimit_input[month_in_year, 2]
	return(GCRecLimit_o)
}
PreGCDraftLimit <- function() {
	PreGCDraftLimit_o <- PreGCDraftLimit_input[week_counter_in_year(), 2]
	return(PreGCDraftLimit_o)
}
GCStaticLimit <- function() {
	if (month_in_year == 8) {
		GCStaticLimit_o <- GCFloodCurve()
	} else if (month_in_year == 9 | month_in_year == 10) {
		GCStaticLimit_o <- min(GCFloodCurve(), PreGCDraftLimit())
	} else if (month_in_year == 1 & DallesRunoffAprAug >= 92.0E6) {
		GCStaticLimit_o <- 8166925
	} else if (month_in_year == 1 & DallesRunoffAprAug < 92.0E6) {
		GCStaticLimit_o <- 8319471
	} else {
		GCStaticLimit_o <- PreGCDraftLimit()
	}
	return(GCStaticLimit_o)
}
GCAprEvaq_Cont <- function() {
	if (CorrectedDallesRunoff() <= 115.0E06) {
		APR_GC <- GCF_month[which(GCF_month[, 1] > CorrectedDallesRunoff())[1], 5]
		GCAprEvaq_Cont_o <- APR_GC
	} else {
		GCAprEvaq_Cont_o <- 3921300
	}	
	return(GCAprEvaq_Cont_o)
}
GCCur_Apr <- function() {
	if (CorrectedDallesRunoff() < 57E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 2] # GCFlood1_2
	} else if (CorrectedDallesRunoff() < 60E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 3] # GCFlood2_2
	} else if (CorrectedDallesRunoff() < 63.25E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 4] # GCFlood3_2
	} else if (CorrectedDallesRunoff() < 65E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 5] # GCFlood4_2
	} else if (CorrectedDallesRunoff() < 67.66E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 6] # GCFlood5_2
	} else if (CorrectedDallesRunoff() < 71.0E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 7] # GCFlood6_2
	} else if (CorrectedDallesRunoff() < 80.0E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 8] # GCFlood7_2
	} else if (CorrectedDallesRunoff() < 90.0E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 9] # GCFlood8_2
	} else if (CorrectedDallesRunoff() < 100E6) {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 10] # GCFlood9_2
	} else {
		GCCur_Apr_o <- GCFlood2[week_counter_in_year(), 11]
	} # GCFlood10_2
	return(GCCur_Apr_o)
}
GC_April_Target <- function() {
	if (FC_Option == 1) {
		GC_April_Target_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - GCCur_Apr())
	} else if (FC_Option == 2) {
		GC_April_Target_o <- GCFullPoolVol - GCFloodEvacMult() * GlobalFloodEvacMult * (GCFullPoolVol - GCAprEvaq_Cont())
	}
	return(GC_April_Target_o)
}
GCAbsMinQ <- function() { # Absolute minimum release from Grand Coulee to support fish flow objects (AF/wk)
	GCAbsMinQ_o <- GCAbsMinQ_input[week_counter_in_year(), 2]
	return(GCAbsMinQ_o)
}
GCBdgtForVB <- function() { 
	GCBdgtForVB_o <- GCBdgtForVB_input[week_counter_in_year(), 2]
	return(GCBdgtForVB_o)
}
GCEnvirQBdgt <- function() { # Release from Grand Coulee to meet fish flow requirement at Vernita Bar (aF/wk)
	GCEnvirQBdgt_o <- max(GCAbsMinQ(), max(GCBdgtForVB(), 0))
	return(GCEnvirQBdgt_o)
}
GCVariableLimit <- function() {
	#   Pre Variable Draft Limit
	GCVariableLimit_o <- min(GCFloodCurve(), GC_April_Target() - GCRunoffJanApr + GCEnvirQBdgt() - AprilUpstreamFloodEvacGC())
	return(GCVariableLimit_o)
}
GCFerryLimit <- function() {
	# The Gifford-Inchelium ferry cannot operate if the reservoir is below 1225 ft of elevation (storage=4,775,542 acre ft).
	# This storage is used as a lower bound for the ECC and VECC curve.  Units acre-ft.
	GCFerryLimit_o <- 4.775542e6
	return(GCFerryLimit_o)
}
GC_VDL <- function() {
	GC_VDL_o <- min(GCTopVol(), max(GCStaticLimit(), max(GCVariableLimit(), max(GCBotVol, GCFerryLimit()))))
	return(GC_VDL_o)
}
GCECC <- function() {
	GCECC_o <- min(GCFloodCurve(), max(GCRefillCurve(), max(GCCriticalCurve(), max(GCRecLimit(), GC_VDL()))))
	return(GCECC_o)
}
GCECCSharedWater <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c = water_df[week_counter,5]
	GCECCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCMcNarySup() - GCECC())
	return(GCECCSharedWater_o)
}
GCECCEnergyContent <- function() {
	GCECCEnergyContent_o <- GCECCSharedWater() * (GCNetHead() + GCDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCECCEnergyContent_o)
}
GCUpMcNarySup <- function() {
	GCUpMcNarySup_o <- MIMcNarySup() + ARMcNarySup() + DUMcNarySup() + HHMcNarySup() + LBMcNarySup()
	return(GCUpMcNarySup_o)
}
GCFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			GCFirmEngSup_o <- 0
		} else {
			GCFirmEngSup_o <- GCEngContMult * (GCEnergyContent() + GCECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		GCFirmEngSup_o <- 0
	} else {
		GCFirmEngSup_o <- GCEngContMult * GCECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(GCFirmEngSup_o)
}
GCFirmEngSupReq <- function() {
	GCFirmEngSupReq_o <- min(GCPenLimit(), GCFirmEngSup() / (MWhr_per_ftAcFt * (GCNetHead() + GCDownStreamHead()) * GCCombEfficiency))
	return(GCFirmEngSupReq_o)
}
GCNonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		GCNonFirmEngSup_o <- 0
	} else {
		GCNonFirmEngSup_o <- GCEngContMult * GCNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
	return(GCNonFirmEngSup_o)
}
GCNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		GCNonFirmEngSupReq_o <- min(GCPenLimit(), (GCFirmEngSup() + GCNonFirmEngSup()) / (MWhr_per_ftAcFt * (GCNetHead() + GCDownStreamHead()) * GCCombEfficiency))
	} else {
		GCNonFirmEngSupReq_o <- 0
	}
	return(GCNonFirmEngSupReq_o)
}
GCNFEnergyContent <- function() {
	GCNFEnergyContent_o <- max(0, GCECCEnergyContent() - GCFirmEngSup())
	return(GCNFEnergyContent_o)
}
GCEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		GCEnergySup_o <- max(min(GCFirmEngSupReq(), GCSharedWater()), min(GCNonFirmEngSupReq(), GCECCSharedWater()))
	} else {
		GCEnergySup_o <- max(min(GCFirmEngSupReq(), GCECCSharedWater()), min(GCNonFirmEngSupReq(), GCECCSharedWater()))
	}
	return(GCEnergySup_o)
}
GCLimitedStorage <- function() {
	GCLimitedStorage_o <- max(0, GrandCoulee() - GC_VDL())
	return(GCLimitedStorage_o)
}
TotalGCHead <- function() {
	TotalGCHead_o <- GCNetHead() + GCDownStreamHead()
	return(TotalGCHead_o)
}
VernitaBarFlowTarget <- function() {
	VernitaBarFlowTarget_o <- VernitaBarFlowTarget_input[month_in_year, 2]
	return(VernitaBarFlowTarget_o)
}
GCSupForVernitaBar <- function() { 
	Updemand <- DemVICCJ + DemVICWE + DemVICRR + DemVICRI + DemVICWA + DemVICPR
	Upsupply <- GCPrelim() + PriestRapidsFlowData() - GrandCouleeFlowData()
	GCSupForVernitaBar_o <- max(0, VernitaBarFlowTarget() * cfsTOafw - (Upsupply - Updemand))
	return(GCSupForVernitaBar_o)
}
GCCombFlowSup <- function() { # Water allocated for meeting fish targets at McNary and Verita Bar
	GCCombFlowSup_o <- max(GCMcNarySup(), GCSupForVernitaBar())
	return(GCCombFlowSup_o)
}
GCMcNaryBONSupEnergy <- function() { # Energy produced by releasing water to meet fish targets at McNary, Vernita Bar, and Bonneville
	GCMcNaryBONSupEnergy_o <- max(GCCombFlowSup(), BONFlowDeficit()) * (GCNetHead() + GCDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCMcNaryBONSupEnergy_o)
}
GCDamProtectRel <- function() {
	GCDamProtectRel_o <- max(0, GrandCoulee() + GCInflow() - GCFullPoolVol)
	return(GCDamProtectRel_o)
}
GCCombUpSup <- function() { # Water allocated for hydropower upstream of Grand Coulee
	GCCombUpSup_o <- AFCombSup() + ARCombSup() + CLCombSup()
	return(GCCombUpSup_o)
}
GCRelLimit <- function() {
	GCRelLimit_o <- max(GrandCoulee() + GCInflow() - GCBotVol, 0)
	return(GCRelLimit_o)
}
GCCombSup <- function() {
	GCCombSup_o <- GCCombFlowSup() + GCCombUpSup() + GCEnergySup()
	return(GCCombSup_o)
}
GCFloodSpace <- function() {
	if (is.na(water_df[week_counter,5])) {
		water_df[week_counter,5] <<- GCIn()
	}
	GCIn_c = water_df[week_counter,5]
	GCFloodSpace_o <- min(
    (GCPrelim() + GCCombUpSup() + GCUpMcNarySup() + GCMcNarySup() + GCEnergySup()),
    max(0, GCFullPoolVol - GrandCoulee() + GCIn_c - (GCPrelim() + GCEnergySup() + GCMcNarySup() + GCCombUpSup() + GCUpMcNarySup()))
	)
	return(GCFloodSpace_o)
}
GCRelease <- function() {
	GCRelease_o <- max(GCDamProtectRel(), min(max(min(BONFlowDeficit(), GCLimitedStorage()), GCPrelim() + GCCombSup() - TotalRelReducReq()), GCRelLimit()))
	return(GCRelease_o)
}

################### CHIEF JOSEPH DAM ##############################

ChiefJosephFlowData <- function() {
	return(PriVICCJ)
}
CJInc <- function() {
	CJInc_o <- ChiefJosephFlowData() - GrandCouleeFlowData()
	return(CJInc_o)
}
CJCurtail <- function() { # Curtailment of water rights provisioned on flow measured at Chief Joseph
	CJCurtail_0 <-  min(DemVICCJ, max(IflowCJ + DemVICCJ - GCOutflow() - CJInc(), 0))
	if (curtail_option == 1) {
		CJCurtail_o <- ifelse(CJCurtail_0 > 0, CurtVICCJ, 0)
	} else if (curtail_option == 2) {	
		CJCurtail_o <- CJCurtail_0
	} else if (curtail_option==3) {
		CJCurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		CJCurtail_o <- 0
	} else {
		CJCurtail_o <- CJCurtail_o
	}		
	return(CJCurtail_o)
}
CJInstreamShortfall <- function() { # Columbia River flow deficit measured at Chief Joseph
	CJInstreamShortfall_o = max(IflowCJ + DemVICCJ - GCOutflow() - CJInc(), 0)
	return(CJInstreamShortfall_o)
}
CJPenLimit <- function() {
	CJPenCap <- 219000
	CJPenLimit_o <- CJPenCap * cfsTOafw
	return(CJPenLimit_o)
}
CJPrelim <- function() {
	CJPrelim_o <- max(0, GCPrelim() + CJInc() - DemVICCJ)
	return(CJPrelim_o)
}
CJNetHead <- function() {
	CJNetHead_o <- 167
	return(CJNetHead_o)
}
CJPreEnergy <- function() {
	CJPreEnergy_o <- MWhr_per_ftAcFt * min(CJPrelim(), CJPenLimit()) * CJNetHead() * CJCombEfficiency
	return(CJPreEnergy_o)
}
CJIn <- function() {
	CJIn_o <- GCOutflow() + CJInc() - DemVICCJ
	return(CJIn_o)
}
CJOut <- function() {
	CJOut_o <- CJIn()
	return(CJOut_o)
}

############################# ROCK ISLAND DAM ################

RockIslandFlowData <- function() {
	return(PriVICRI)
}
RIInc <- function() {
	RIInc_o <- RockIslandFlowData() - RockyReachFlowData()
	return(RIInc_o)
}
RICurtail <- function() {
	RICurtail_0 <- min(DemVICRI, max(IflowRI + DemVICRI - RROut() - RIInc(), 0))
	if (curtail_option==1) {
		RICurtail_o <- ifelse(RICurtail_0 > 0, CurtVICRI, 0)
	} else if (curtail_option==2) {
	  RICurtail_o <- RICurtail_0
	} else if (curtail_option==3) {
	  RICurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		RICurtail_o <- 0
	} else {
		RICurtail_o <- RICurtail_o
	}		
	return(RICurtail_o)
}
RIInstreamShortfall <- function() {
	RIInstreamShortfall_o = max(IflowRI + DemVICRI - RROut() - RIInc(), 0)
	return(RIInstreamShortfall_o)
}
RIPenLimit <- function() {
	RIPenCap <- 220000
	RIPenLimit_o <- RIPenCap * cfsTOafw
	return(RIPenLimit_o)
}
RINetHead <- function() {
	RINetHead_o <- 34.4
	return(RINetHead_o)
}
RIPrelim <- function() {
	UpDemand <- DemVICRI + DemVICRR + DemVICWE + DemVICCJ
	IncFlow <- RockIslandFlowData() - GrandCouleeFlowData()
	RIPrelim_o <- GCPrelim() + IncFlow - UpDemand
	return(RIPrelim_o)
}
RIPreEnergy <- function() {
	RIPreEnergy_o <- MWhr_per_ftAcFt * min(RIPrelim(), RIPenLimit()) * RINetHead() * RICombEfficiency
	return(RIPreEnergy_o)
}
RIIn <- function() {
	RIIn_o <- RROut() + RIInc() - DemVICRI
	return(RIIn_o)
}
RIOut <- function() {
	RIOut_o <- RIIn()
	return(RIOut_o)
}

####################### ROCKY REACH DAM ########################################

RockyReachFlowData <- function() {
	return(PriVICRR)
}
RRInc <- function() {
	RRInc_o <- RockyReachFlowData() - WellsFlowData()
	return(RRInc_o)
}
RRCurtail <- function() {
	RRCurtail_0 <- min(DemVICRR, max(IflowRR + DemVICRR - WEOut() - RRInc(), 0))
	if (curtail_option==1) {
		RRCurtail_o <- ifelse(RRCurtail_0 > 0, CurtVICRR, 0)
	} else if (curtail_option == 2) {
		RRCurtail_o <- RRCurtail_0
	} else if (curtail_option == 3) {
		RRCurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		RRCurtail_o <- 0
	} else {
		RRCurtail_o <- RRCurtail_o
	}		
	return(RRCurtail_o)
}
RRInstreamShortfall <- function() {
	RRInstreamShortfall_o = max(IflowRR + DemVICRR - WEOut() - RRInc(), 0)
	return(RRInstreamShortfall_o)
}
RRPenLimit <- function() {
	RRPenCap <- 220000
	RRPenLimit_o <- RRPenCap * cfsTOafw
	return(RRPenLimit_o)
}
RRPrelim <- function() {
	UpDemand <- DemVICRR + DemVICWE + DemVICCJ
	IncFlow <- RockyReachFlowData() - GrandCouleeFlowData()
	RRPrelim_o <- GCPrelim() + IncFlow - UpDemand
	return(RRPrelim_o)
}
RRNetHead <- function() {
	RRNetHead_o <- 86.5
	return(RRNetHead_o)
}
RRPreEnergy <- function() {
	RRPreEnergy_o <- MWhr_per_ftAcFt * min(RRPrelim(), RRPenLimit()) * RRNetHead() * RRCombEfficiency
	return(RRPreEnergy_o)
}
RRIn <- function() {
	RRIn_o <- WEOut() + RRInc() - DemVICRR
	return(RRIn_o)
}
RROut <- function() {
	RROut_o <- RRIn()
	return(RROut_o)
}

################# WANAPUM DAM ##############################

WanapumFlowData <- function() {
	return(PriVICWA)
}
WAInc <- function() {
	WAInc_o <- WanapumFlowData() - RockIslandFlowData()
	return(WAInc_o)
}
WACurtail <- function() {
	WACurtail_0 <- min(DemVICWA, max(IflowWA + DemVICWA - RIOut() - WAInc(), 0))
	if (curtail_option == 1) {
		WACurtail_o <- ifelse(WACurtail_0 > 0, CurtVICWA, 0)
	} else if (curtail_option == 2) {
		WACurtail_o <- WACurtail_0
	} else if (curtail_option == 3) {
		WACurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		WACurtail_o <- 0
	} else {
		WACurtail_o <- WACurtail_o
	}		
	return(WACurtail_o)
}
WAInstreamShortfall <- function() {
	WAInstreamShortfall_o = max(IflowWA + DemVICWA - RIOut() - WAInc(), 0)
	return(WAInstreamShortfall_o)
}
WAPenLimit <- function() {
	WAPenCap <- 178000
	WAPenLimit_o <- WAPenCap * cfsTOafw
	return(WAPenLimit_o)
}
WAPrelim <- function() {
	UpDemand <- DemVICCJ + DemVICWE + DemVICRR + DemVICRI + DemVICWA # Demand between Wanapum and Grand Coulee
	IncFlow <- WanapumFlowData() - GrandCouleeFlowData()
	WAPrelim_o <- GCPrelim() + IncFlow - UpDemand
	return(WAPrelim_o)
}
WANetHead <- function() {
	WANetHead_o <- 77.8
	return(WANetHead_o)
}
WAPreEnergy <- function() {
	WAPreEnergy_o <- MWhr_per_ftAcFt * min(WAPrelim(), WAPenLimit()) * WANetHead() * WACombEfficiency
	return(WAPreEnergy_o)
}
WAIn <- function() {
	WAIn_o <- RIOut() + WAInc() - DemVICWA
	return(WAIn_o)
}
WAOut <- function() {
	WAOut_o <- WAIn()
	return(WAOut_o)
}

######################## WELLS DAM #############################

WellsFlowData <- function() {
	return(PriVICWE)
}
WEInc <- function() {
	WEInc_o <- WellsFlowData() - ChiefJosephFlowData()
	return(WEInc_o)
}
WECurtail <- function() {
	WECurtail_0 <- min(DemVICWE, max(IflowWE + DemVICWE - CJOut() - WEInc(), 0))
	if (curtail_option == 1) {
		WECurtail_o <- ifelse(WECurtail_0 > 0, CurtVICWE, 0)
	} else if (curtail_option == 2) {
		WECurtail_o <- WECurtail_0
	} else if (curtail_option == 3) {
		WECurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		WECurtail_o <- 0
	} else {
		WECurtail_o <- WECurtail_o
	}		
	return(WECurtail_o)
}
WEInstreamShortfall <- function() {
	WEInstreamShortfall_o = max(IflowWE + DemVICWE - CJOut() - WEInc(), 0)
	return(WEInstreamShortfall_o)
}
WEPenLimit <- function() {
	WEPenCap <- 220000
	WEPenLimit_o <- WEPenCap * cfsTOafw
	return(WEPenLimit_o)
}
WEPrelim <- function() {
	UpDemand <- DemVICWE + DemVICCJ # Demand between Wells and Grand Coulee
	IncFlow <- WellsFlowData() - GrandCouleeFlowData()
	WEPrelim_o <- GCPrelim() + IncFlow - UpDemand
	return(WEPrelim_o)
}
WENetHead <- function() {
	WENetHead_o <- 66.9
	return(WENetHead_o)
}
WEPreEnergy <- function() {
	WEPreEnergy_o <- MWhr_per_ftAcFt * min(WEPrelim(), WEPenLimit()) * WENetHead() * WECombEfficiency
	return(WEPreEnergy_o)
}
WEIn <- function() {
	WEIn_o <- CJOut() + WEInc() - DemVICWE
	return(WEIn_o)
}
WEOut <- function() {
	WEOut_o <- WEIn()
	return(WEOut_o)
}
############### PRIEST RAPIDS ############

PriestRapidsFlowData <- function() {
	return(PriVICPR)
}
PRInc <- function() {
	PRInc_o <- PriestRapidsFlowData() - WanapumFlowData()
	return(PRInc_o)
}
PRCurtail <- function()	{
	PRCurtail_0 <- min(DemVICPR, max(IflowPR + DemVICPR - WAOut() - PRInc(), 0))
	if (curtail_option == 1) {
		PRCurtail_o <- ifelse(PRCurtail_0 > 0, CurtVICPR, 0)
	} else if (curtail_option == 2) {
		PRCurtail_o <- PRCurtail_0 
	} else if (curtail_option == 1) {
		PRCurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		PRCurtail_o <- 0
	} else {
		PRCurtail_o <- PRCurtail_o
	}		
	return(PRCurtail_o)
}
PRInstreamShortfall <- function() {
	PRInstreamShortfall_o = max(IflowPR + DemVICPR - WAOut() - PRInc(), 0)
	return(PRInstreamShortfall_o)
}
PRIn <- function() {
	PRIn_o <- WAOut() + PRInc() - DemVICPR
	return(PRIn_o)
}
PROut <- function() {
	PROut_o <- PRIn()
	return(PROut_o)
}
PRPenLimit <- function() {
	PRPenCap <- 187000
	PRPenLimit_o <- PRPenCap * cfsTOafw
	return(PRPenLimit_o)
}
PRPrelim <- function() {
	UpDemand <- DemVICCJ + DemVICWE + DemVICRR + DemVICRI + DemVICWA + DemVICPR # Water demand between Priest Rapids and Grand Coulee
	IncFlow <- PriestRapidsFlowData() - GrandCouleeFlowData()
	PRPrelim_o <- GCPrelim() + IncFlow - UpDemand
	return(PRPrelim_o)
}
PRNetHead <- function() {
	PRNetHead_o <- 76.5
	return(PRNetHead_o)
}
PRPreEnergy <- function() {
	PRPreEnergy_o <- 43560 * (998 * min(PRPrelim(), PRPenLimit()) * 0.028317 * 9.81 * PRNetHead() * 0.3048 * PRCombEfficiency) / 3.6E9
	return(PRPreEnergy_o)
}
######## UPPER SNAKE COMPOSITE RESERVOIR ################################

USBotVol <- 201000

#### USPrelim
# Preliminary release based upon rule requirements and minimum releases.
# The release cannot be larger than the available water after withdrawals.  

PalisadesFlowData <- function() {
	return(PriVICPAL)
}
USFloodRuleCurve <- function() {
	USFloodRuleCurve_o <- USFloodRuleCurve_input[week_counter_in_year(), 2]
	return(USFloodRuleCurve_o)
}
USTopVol <- function() {
	USTopVol_o <- USFloodRuleCurve()
	return(USTopVol_o)
}
UpSnakeComb <- function() {
	if (week_counter == 1) {
		UpSnakeComb_o <- USTopVol()
	} else {
		UpSnakeComb_o <- reservoir_vol_df[week_counter - 1, 10]
	}
	return(UpSnakeComb_o)
}
USIn <- function() {
	USIn_o <- PalisadesFlowData() - DemVICPAL
	return(USIn_o)
}
USInflow <- function() {
  USInflow_o <- USIn()
  return(USInflow_o)
}
USRuleReq <- function() {
	USRuleReq_o <- max(UpSnakeComb() + USIn() - USTopVol(), 0)
	return(USRuleReq_o)
}
USPrelim <- function() {
	USPrelim_o <- min(USAvailAfter(), USRuleReq())
	return(USPrelim_o)
}
USAvailAfter <- function() {
	USAvailAfter_o <- max(0, UpSnakeComb() + USIn() - USBotVol)
	return(USAvailAfter_o)
}
USSharedWater <- function() {
	USSharedWater_o <- max(0, UpSnakeComb() + USIn() - USPrelim() - USBotVol)
	return(USSharedWater_o)
}
USRelLimit <- function() {
	USRelLimit_o <- max(UpSnakeComb() + USInflow() - USBotVol, 0)
	return(USRelLimit_o)
}
UpperSnakeRelease <- function() {
  UpperSnakeRelease_o <- min(USPrelim(), USRelLimit())
  return(UpperSnakeRelease_o)
}
USOutflow <- function() {
  USOutflow_o <- UpperSnakeRelease()
  return(USOutflow_o)
}

################### MIDDLE SNAKE COMPOSITE RESERVOIR ################################################

MSBotVol <- 119800 # Volume cooresponding to sum of bottom pool volume at all reservoirs.  Units acre-ft.
MSFullPoolVol <- 4242400 # Volume cooresponding to sum of full pool volume at all reservoirs.  Units acre-ft.

MilnerFlowData <- function() {
	return(PriVICMIL)
}
MSInc <- function() {
	MSInc_o <- MilnerFlowData() - PalisadesFlowData()
	return(MSInc_o)
}
MSSufaceArea <- function() {
	MSSufaceArea_o <- MdlSnakeComb() * 0
	return(MSSufaceArea_o)
}
MSEvap <- function() {
	MSEvapData <- 0
	MSEvap_o <- MSSufaceArea() * MSEvapData * 0.5042 / 12
	return(MSEvap_o)
}
MSIn <- function() {
	MSIn_o <- USPrelim() + MSInc() - MSEvap() - DemVICMIL
	return(MSIn_o)
}
MSFloodRuleCurve <- function() {
	MSFloodRuleCurve_o <- MSFloodRuleCurve_input[week_counter_in_year(), 2]
	return(MSFloodRuleCurve_o)
}
MSTopVol <- function() {
	MSTopVol_o <- MSFloodRuleCurve()
	return(MSTopVol_o)
}
MdlSnakeComb <- function() {
	if (week_counter == 1) {
		MdlSnakeComb_o <- MSTopVol()
	} else {
		MdlSnakeComb_o <- reservoir_vol_df[week_counter - 1, 11]
	}
	return(MdlSnakeComb_o)
}
MSRuleReq <- function() {
	MSRuleReq_o <- max(MdlSnakeComb() + MSIn() - MSTopVol(), 0)
	return(MSRuleReq_o)
}
MSAvailAfter <- function() {
	MSAvailAfter_o <- max(0, MdlSnakeComb() + MSIn() - MSBotVol)
	return(MSAvailAfter_o)
}
MSPrelim <- function() {
	MSPrelim_o <- min(MSAvailAfter(), MSRuleReq())
	return(MSPrelim_o)
}
MSSup <- function() {
	MSSup_o <- 0
	return(MSSup_o)
}
MSInflow <- function() {
	MSInflow_o <- USPrelim() + MSInc() - MSEvap() - DemVICMIL
	return(MSInflow_o)
}
MSRelLimit <- function() {
	MSRelLimit_o <- MdlSnakeComb() + MSInflow() - MSBotVol
	return(MSRelLimit_o)
}
MSRelease <- function() {
	MSRelease_o <- min(MSPrelim() + MSSup(), MSRelLimit())
	return(MSRelease_o)
}
MSOutflow <- function() {
	MSOutflow_o <- MSRelease()
	return(MSOutflow_o)
}
################################## BROWNLEE #################################################

BRFullPoolVol <- 1.420e6 # Volume corresponding to 2077 ft of elevation.  Normal full pool.  Units acre-ft.
BRBotVol <- 443000 # Volume cooresponding to the bottom of conservation pool elevation of 1976 ft.  Units acre-ft.
BRTailElev <- 1805 # Tailwater elevation.  Units ft.
BRCombEfficiency <- 0.8

BRHistStor <- function() {
	BRHistStor_o <- BRHistStor_input[week_counter, 2]
	return(BRHistStor_o)
}
InitBRLink <- function() {
	InitBRLink_o <- 1295476.748
	return(InitBRLink_o)
}
InitBR <- function() {
	if (InitialConditionSwitch == 0) {
		InitBR_o <- InitBRLink()
	} else if (InitialConditionSwitch == 1) {
		InitBR_o <- ResInitFractionFull * BRFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitBR_o <- BRHistStor()
	} else {
		InitBR_o <- BRFullPoolVol
	}
	return(InitBR_o)
}
Brownlee <- function() {
	if (week_counter == 1) {
		Brownlee_o <- InitBR()
		#  reservoir_vol_df[week_counter,9]=Brownlee_o
	} else {
		Brownlee_o <- reservoir_vol_df[week_counter - 1, 9]
	}
	return(Brownlee_o)
}
BrownleeFlowData <- function() {
	return(PriVICBR)
}
BRInc <- function() {
	BRInc_o <- BrownleeFlowData() - MilnerFlowData()
	return(BRInc_o)
}
BREvap <- function() {
	BREvapData <- 0
	BREvap_o <- BRSufaceArea() * BREvapData * 0.5042 / 12
	return(BREvap_o)
}
BRSufaceArea <- function() {
	BRSufaceArea_o <- Brownlee() * 0
	return(BRSufaceArea_o)
}
BRInflow <- function() {
	if (ResetStorage() == 1) {
		BRInflow_o <- InitBR() - Brownlee()
	} else {
		BRInflow_o <- MSOutflow() + BRInc() - DemVICBR - BREvap()
	}
	return(BRInflow_o)
}
BRBaseFloodCurve <- function() {
	if (BRFloodCond() == 1) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 2] # BRFlood1
	} else if (BRFloodCond() == 2) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 3] # BRFlood2
	} else if (BRFloodCond() == 3) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 4] # BRFlood3
	} else if (BRFloodCond() == 4) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 5] # BRFlood4
	} else if (BRFloodCond() == 5) {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 6] # BRFlood5
	} else {
		BRBaseFloodCurve_o <- BRFlood[week_counter_in_year(), 7]
	} # BRFlood6
	return(BRBaseFloodCurve_o)
}
BRFloodCond <- function() {
	if (BRRunoffAprJuly < 2.5e6) {
		BRFloodCond_o <- 1
	} else if (BRRunoffAprJuly < 3.0e6) {
		BRFloodCond_o <- 2
	} else if (BRRunoffAprJuly < 4.0e6) {
		BRFloodCond_o <- 3
	} else if (BRRunoffAprJuly < 5.0e6) {
		BRFloodCond_o <- 4
	} else if (BRRunoffAprJuly < 6.0e6) {
		BRFloodCond_o <- 5
	} else {
		BRFloodCond_o <- 6
	}
	return(BRFloodCond_o)
}
DallesFloodCond <- function() {
	if (DallesRunoffAprAug < 60e6) {
		DallesFloodCond_o <- 1
	} else if (DallesRunoffAprAug < 70e6) {
		DallesFloodCond_o <- 2
	} else if (DallesRunoffAprAug < 80e6) {
		DallesFloodCond_o <- 3
	} else if (DallesRunoffAprAug < 90e6) {
		DallesFloodCond_o <- 4
	} else if (DallesRunoffAprAug < 100e6) {
		DallesFloodCond_o <- 5
	} else if (DallesRunoffAprAug < 110e6) {
		DallesFloodCond_o <- 6
	} else if (DallesRunoffAprAug < 120e6) {
		DallesFloodCond_o <- 7
	} else if (DallesRunoffAprAug < 130e6) {
		DallesFloodCond_o <- 8
	} else if (DallesRunoffAprAug < 140e6) {
		DallesFloodCond_o <- 9
	} else if (DallesRunoffAprAug < 160e6) {
		DallesFloodCond_o <- 10
	} else {
		DallesFloodCond_o <- 11
	}
	return(DallesFloodCond_o)
}
Add_Space <- function() { # Additional flood evacuation based on Forecasted Runoff at The Dalles
	row_num <- DallesFloodCond()
	Add_Space_o <- AddSp_input[row_num, 2]
	return(Add_Space_o)
}
BRCurFC_Cont75 <- function() {
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {# JAN_BR75
			BRCurFC_Cont75_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont75_o <- BRForecastFloodStorage[row_no, 2] # FEB_BR75
		} else if (month_in_year==8) {
			BRCurFC_Cont75_o <- BRForecastFloodStorage[row_no, 3] # MAR_BR75
		} else if (month_in_year==9) {
			BRCurFC_Cont75_o <- BRForecastFloodStorage[row_no, 4] # APR_BR75
		} else {
			BRCurFC_Cont75_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont75_o <- BRCurFC_Cont[week_counter_in_year(), 2]
	} # BRFlood6_75
	return(BRCurFC_Cont75_o)
}
BRCurFC_Cont80 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont80_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont80_o <- BRForecastFloodStorage[row_no, 5] # FEB_BR80
		} else if (month_in_year==8) {
			BRCurFC_Cont80_o <- BRForecastFloodStorage[row_no, 6] # MAR_BR80
		} else if (month_in_year==9) {
			BRCurFC_Cont80_o <- BRForecastFloodStorage[row_no, 7] # APR_BR80
		} else {
			BRCurFC_Cont80_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont80_o <- BRCurFC_Cont[week_counter_in_year(), 3]
	} # BRFlood6_80
	return(BRCurFC_Cont80_o)
}
BRCurFC_Cont85 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont85_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont85_o <- BRForecastFloodStorage[row_no, 8] # FEB_BR85
		} else if (month_in_year==8) {
			BRCurFC_Cont85_o <- BRForecastFloodStorage[row_no, 9] # MAR_BR85
		} else if (month_in_year==9) {
			BRCurFC_Cont85_o <- BRForecastFloodStorage[row_no, 10] # APR_BR85
		} else {
		BRCurFC_Cont85_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont85_o <- BRCurFC_Cont[week_counter_in_year(), 4]
	} # BRFlood6_85
	return(BRCurFC_Cont85_o)
}
BRCurFC_Cont90 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont90_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont90_o <- BRForecastFloodStorage[row_no, 11] # FEB_BR90
		} else if (month_in_year==8) {
			BRCurFC_Cont90_o <- BRForecastFloodStorage[row_no, 12] # MAR_BR90
		} else if (month_in_year==9) {
			BRCurFC_Cont90_o <- BRForecastFloodStorage[row_no, 13] # APR_BR90
		} else {
			BRCurFC_Cont90_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont90_o <- BRCurFC_Cont[week_counter_in_year(), 5]
	}	 # BRFlood6_90
	return(BRCurFC_Cont90_o)
}
BRCurFC_Cont95 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont95_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont95_o <- BRForecastFloodStorage[row_no, 14] # FEB_BR95
		} else if (month_in_year==8) {
			BRCurFC_Cont95_o <- BRForecastFloodStorage[row_no, 15] # MAR_BR95
		} else if (month_in_year==9) {
			BRCurFC_Cont95_o <- BRForecastFloodStorage[row_no, 16] # APR_BR95
		} else {
			BRCurFC_Cont95_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont95_o <- BRCurFC_Cont[week_counter_in_year(), 6]
	} # BRFlood6_95
	return(BRCurFC_Cont95_o)
}
BRCurFC_Cont100 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont100_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont100_o <- BRForecastFloodStorage[row_no, 17] # FEB_BR100
		} else if (month_in_year==8) {
			BRCurFC_Cont100_o <- BRForecastFloodStorage[row_no, 18] # MAR_BR100
		} else if (month_in_year==9) {
			BRCurFC_Cont100_o <- BRForecastFloodStorage[row_no, 19] # APR_BR100
		} else {
			BRCurFC_Cont100_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont100_o <- BRCurFC_Cont[week_counter_in_year(), 7]
	} # BRFlood6_100
	return(BRCurFC_Cont100_o)
}
BRCurFC_Cont105 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont105_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont105_o <- BRForecastFloodStorage[row_no, 20] # FEB_BR105
		} else if (month_in_year==8) {
			BRCurFC_Cont105_o <- BRForecastFloodStorage[row_no, 21] # MAR_BR105
		} else if (month_in_year==9) {
			BRCurFC_Cont105_o <- BRForecastFloodStorage[row_no, 22] # APR_BR105
		} else {
			BRCurFC_Cont105_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont105_o <- BRCurFC_Cont[week_counter_in_year(), 8]
	} # BRFlood6_105
	return(BRCurFC_Cont105_o)
}
BRCurFC_Cont110 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont110_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont110_o <- BRForecastFloodStorage[row_no, 23] # FEB_BR110
		} else if (month_in_year==8) {
			BRCurFC_Cont110_o <- BRForecastFloodStorage[row_no, 24] # MAR_BR110
		} else if (month_in_year==9) {
			BRCurFC_Cont110_o <- BRForecastFloodStorage[row_no, 25] # APR_BR110
		} else {
			BRCurFC_Cont110_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont110_o <- BRCurFC_Cont[week_counter_in_year(), 9]
	}	 # BRFlood6_110
	return(BRCurFC_Cont110_o)
}
BRCurFC_Cont115 <- function() {
	# row_num1=max(1.0,which(BRForecastFloodStorage[,1]>BRRunoffAprJuly)[1]-1)
	BR_FloodM_Range <- c(3000000, 4000000, 5000000, 6000000)
	if (BR_FloodM_Range[length(BR_FloodM_Range)] < BRRunoffAprJuly) {
		row_no <- length(BR_FloodM_Range)
	} else {
		row_no <- max(1.0, which(BR_FloodM_Range > BRRunoffAprJuly)[1] - 1)
	}
	if (BRRunoffAprJuly <= 6.0E06) {
		if (month_in_year==6) {
			BRCurFC_Cont115_o <- 1170000
		} else if (month_in_year==7) {
			BRCurFC_Cont115_o <- BRForecastFloodStorage[row_no, 26] # FEB_BR115
		} else if (month_in_year==8) {
			BRCurFC_Cont115_o <- BRForecastFloodStorage[row_no, 27] # MAR_BR115
		} else if (month_in_year==9) {
			BRCurFC_Cont115_o <- BRForecastFloodStorage[row_no, 28] # APR_BR115
		} else {
			BRCurFC_Cont115_o <- BRFullPoolVol
		}
	} else {
		BRCurFC_Cont115_o <- BRCurFC_Cont[week_counter_in_year(), 10]
	} # BRFlood6_115
	return(BRCurFC_Cont115_o)
}
BRFC_Cont <- function() {
  if (month_in_year %in% 6:9) {
  	if (DallesRunoffAprAug <= 75e06) {
  		BRFC_Cont_o <- BRCurFC_Cont75()
  	} else if (DallesRunoffAprAug <= 80e06) {
  		BRFC_Cont_o <- BRCurFC_Cont80()
  	} else if (DallesRunoffAprAug <= 85e06) {
  		BRFC_Cont_o <- BRCurFC_Cont85()
  	} else if (DallesRunoffAprAug <= 90e06) {
  		BRFC_Cont_o <- BRCurFC_Cont90()
  	} else if (DallesRunoffAprAug <= 95e06) {
  		BRFC_Cont_o <- BRCurFC_Cont95()
  	} else if (DallesRunoffAprAug <= 100e06) {
  		BRFC_Cont_o <- BRCurFC_Cont100()
  	} else if (DallesRunoffAprAug <= 105e06) {
  		BRFC_Cont_o <- BRCurFC_Cont105()
  	} else if (DallesRunoffAprAug <= 110e06) {
  		BRFC_Cont_o <- BRCurFC_Cont110()
  	} else {
  		BRFC_Cont_o <- BRCurFC_Cont115()
  	}
  } else {
    BRFC_Cont_o <- BRFullPoolVol
  }
	return(BRFC_Cont_o)
}
BRFloodVolume <- function() {
	if (FC_Option == 1) {
		if (month_in_year == 8 || month_in_year == 9) {
			BRFV <- max(0, BRBaseFloodCurve() - Add_Space())
		} else {
			BRFV <- BRBaseFloodCurve()
		}
		BRFloodVolume_o <- (BRFullPoolVol - GlobalFloodEvacMult * (BRFullPoolVol - (min(BRFullPoolVol, BRFV))))
	} else {
		BRFloodVolume_o <- (BRFullPoolVol - GlobalFloodEvacMult * (BRFullPoolVol - BRFC_Cont()))
	}
	return(BRFloodVolume_o)
}
BRTopVol <- function() {
	BRTopVol_o <- BRFloodVolume()
	return(BRTopVol_o)
}
BRRuleReq <- function() {
	BRRuleReq_o <- max(Brownlee() + BRInflow() - BRTopVol(), 0)
	return(BRRuleReq_o)
}
BRElev_ft <- function() {
	BRElev_ft_o <- 4.98417182E-28 * Brownlee()^5 - 2.02560727E-21 * Brownlee()^4 + 3.05422588E-15 * Brownlee()^3 - 0.00000000215214417 * Brownlee()^2
	+ 0.000843642399 * Brownlee() + 1827.36033
	return(BRElev_ft_o)
}
BRNetHead <- function() {
	BRLoss <- 0 # Piping head losses.  Units ft.
	BRNetHead_o <- BRElev_ft() - BRTailElev - BRLoss
	return(BRNetHead_o)
}
BRPenLimit <- function() {
	BRPenCap <- 34500
	BRPenLimit_o <- BRPenCap * cfsTOafw
	return(BRPenLimit_o)
}
BRIn <- function() {
	BRIn_o <- MSRelease() + BRInc() - BREvap() - DemVICBR
	return(BRIn_o)
}
BRAvailAfter <- function() {
	if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4] <<- BRIn() # Write BRIn output to water.txt
	}
	BRIn_c = water_df[week_counter,4] # Read value from water.txt
	BRAvailAfter_o <- max(0, Brownlee() + BRIn_c - BRBotVol)
	return(BRAvailAfter_o)
}
BRDraftLimit <- function() {
	if (UseAllStorForMCNLG == 1) { # Default = 0
		BRDraftLimit_o <- BRBotVol
	} else {
		BRDraftLimit_o <- 1.183e6
	}
	return(BRDraftLimit_o)
}
BRLGAvailAfter <- function() {
	if (is.na(water_df[week_counter,4])) { 
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c=water_df[week_counter,4]
	BRLGAvailAfter_o <- max(0, Brownlee() + BRIn_c - BRDraftLimit())
	return(BRLGAvailAfter_o)
}
BRRelForLG <- function() { # Release of water from Brownlee to meet Lower Granite fish flow target 
	if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c = water_df[week_counter,4]
	if ((StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()) == 0) {
		BRRelForLG_o <- 0
	} else {
		BRRelForLG_o <- TotalRelForLowerGranite() * ((StorFrac * Brownlee() + InflowFrac * BRIn_c - StorFrac * BRDraftLimit())
		/ (StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()))
	}
	return(BRRelForLG_o)
}
BRNonFirmTarget <- function() {
	BRNonFirmTarget_o <- 0 
	return(BRNonFirmTarget_o)
}
BRFirmEngTarget <- function() {
	BRFirmEngTarget_o <- 0
	return(BRFirmEngTarget_o)
}
BRFirmEngRelReq <- function() {
	BRFirmEngRelReq_o <- min(BRPenLimit(), BRFirmEngTarget() / (MWhr_per_ftAcFt * BRNetHead() * BRCombEfficiency))
	return(BRFirmEngRelReq_o)
}
JohnsonBarFlowTarget <- function() {
	JohnsonBarFlowTarget_o <- 5000 # monthly time-series
	return(JohnsonBarFlowTarget_o)
}
BRRelForJohnsonsBar <- function() { # Release of water from Brownlee to meet Johnson Bar fish flow target
	BRRelForJohnsonsBar_o <- max(0, JohnsonBarFlowTarget() * cfsTOafw - HCInc() - DemVICHC)
	return(BRRelForJohnsonsBar_o)
}
##### LimePointFlowTarget
# Minimum Flows at the Lime Point (75 miles below Hells Canyon) are required to be at least 13000 cfs 95% of the time,
# with any deviations from these flows occuring July-Sept.  The model assumes these flows must be maintained 100% of the time.
# In addition a minimum flow of 5000 cfs at Johnson's Bar (17 miles downstream of Hells Canyon) must be maintained.
# While in actual practice some storage is available for support of these flows from run-of-river projects,
# Brownlee must generally make sufficient average releases to support these flows on a monthly time frame.
# The model assumes that Brownlee supplies all supplements to natural inflow required to meet the targets.

LimePointFlowTarget <- function() {
	LimePointFlowTarget_o <- 13000 # monthly time-series
	return(LimePointFlowTarget_o)
}
LimePointFlowData <- function() {
	LimePointFlowData_o <- HellsCanyonFlowData() * 1.3
	return(LimePointFlowData_o)
}
LPInc <- function() {
	LPInc_o <- LimePointFlowData() - HellsCanyonFlowData()
	return(LPInc_o)
}
BRRelForLimePoint <- function() {
	BRRelForLimePoint_o <- max(0, LimePointFlowTarget() * cfsTOafw - HCInc() - LPInc() - DemVICHC)
	return(BRRelForLimePoint_o)
}
BRRelForJBandLP <- function() {
	BRRelForJBandLP_o <- max(BRRelForJohnsonsBar(), BRRelForLimePoint())
	return(BRRelForJBandLP_o)
}
BRAvgMin <- function() {
	BRAvgMin_o <- 0
	return(BRAvgMin_o)
}
BRNonFirmEngRelReq <- function() {
	if (NonFirmEnergySw == 1) {
		BRNonFirmEngRelReq_o <- min(BRPenLimit(), (BRFirmEngTarget() + BRNonFirmTarget()) / (MWhr_per_ftAcFt * BRNetHead() * BRCombEfficiency))
	} else {
		BRNonFirmEngRelReq_o <- 0
	}
	return(BRNonFirmEngRelReq_o)
}
BRMaxNonFirmRel <- function() { # Maximum amount of water that can be released for hydropower generation
	if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c = water_df[week_counter,4]
	BRMaxNonFirmRel_o <- max(0, Brownlee() + BRIn_c - BRRefillCurve)
	return(BRMaxNonFirmRel_o)
}
BRMinReq <- function() {
	BRMinReq_o <- max(min(BRAvailAfter(), BRFirmEngRelReq()), 
	  max(min(BRMaxNonFirmRel(), BRNonFirmEngRelReq()),
    max(min(BRLGAvailAfter(), BRRelForLG()), max(BRRelForJBandLP(), BRAvgMin() * cfsTOafw))))
	return(BRMinReq_o)
}
BRPrelim <- function() {
	BRPrelim_o <- min(BRAvailAfter(), max(BRRuleReq(), BRMinReq()))
	return(BRPrelim_o)
}
BRCombSup <- function() {
	BRCombSup_o <- 0
	return(BRCombSup_o)
}
BRRelLimit <- function() {
	BRRelLimit_o <- max(Brownlee() + BRInflow() - BRBotVol, 0)
	return(BRRelLimit_o)
}
BRRelease <- function() {
	BRRelease_o <- min(BRPrelim() + BRCombSup(), BRRelLimit())
	return(BRRelease_o)
}
BROutflow <- function() {
	if (ResetStorage() == 1) {
		BROutflow_o <- 0
	} else {
		BROutflow_o <- BRRelease_c
	}
	return(BROutflow_o)
}

############### OXBOW #####################

OxbowFlowData <- function() {
	return(PriVICOX)
}
OXInc <- function() {
	OXInc_o <- OxbowFlowData() - BrownleeFlowData()
	return(OXInc_o)
}
OXIn <- function() {
	OXIn_o <- BRPrelim_c + OXInc() - DemVICOX
	return(OXIn_o)
}
OXOut <- function() {
	OXOut_o <- OXIn()
	return(OXOut_o)
}

################ HELLS CANYON ################

HellsCanyonFlowData <- function() {
	return(PriVICHC)
}
HCInc <- function() {
	HCInc_o <- HellsCanyonFlowData() - OxbowFlowData()
	return(HCInc_o)
}
HCIn <- function() {
	HCIn_o <- OXOut() + HCInc() - DemVICHC
	return(HCIn_o)
}
HCOut <- function() {
	HCOut_o <- HCIn()
	return(HCOut_o)
}
################ DWORSHAK DAM ##########################

InitDWLink <- 2776338.073
DWFullPoolVol <- 3.4679500E+06
DWBotVol <- 1.4522E+06 # Volume cooresponding to the bottom of conservation pool elevation of 1445 ft.  Units acre-ft.

Dworshak <- function() {
	if (week_counter == 1) {
		Dworshak_o <- InitDW()
	} else {
		Dworshak_o <- reservoir_vol_df[week_counter - 1, 8]
	}
	return(Dworshak_o)
}
DWHistStor <- function() {
	DWHistStor_o <- DWHistStor_input[week_counter_in_year(), 2]
	return(DWHistStor_o)
}
InitDW <- function() {
	if (InitialConditionSwitch == 0) {
		InitDW_o <- InitDWLink
	} else if (InitialConditionSwitch == 1) {
		InitDW_o <- ResInitFractionFull * DWFullPoolVol
	} else if (InitialConditionSwitch == 2) {
		InitDW_o <- DWHistStor()
	} else {
		InitDW_o <- DWFullPoolVol
	}
	return(InitDW_o)
}
DworshakFlowData <- function() {
	return(PriVICDW)
}
DWSufaceArea <- function() {
	DWSufaceArea_o <- (-1.21281443E-13 * (Dworshak() / 1000)^4 + 1.53692112E-09 * (Dworshak() / 1000)^3 -
    6.75961255E-06 * (Dworshak() / 1000)^2 + 1.87278268E-02 * (Dworshak() / 1000) + 2.30403996) * 1000
	return(DWSufaceArea_o)
}
DWEvap <- function() {
	DWEvapData <- 0
	DWEvap_o <- DWSufaceArea() * DWEvapData * 0.5042 / 12
	return(DWEvap_o)
}
DWIn <- function() {
	DWIn_o <- DworshakFlowData() - DWEvap() - DemVICDW
	return(DWIn_o)
}
DWInflow <- function() {
	if (ResetStorage() == 1) {
		DWInflow_o <- InitDW() - Dworshak()
	} else {
		DWInflow_o <- DWIn()
	}
	return(DWInflow_o)
}
DWDraftLimit <- function() {
	if (UseAllStorForMCNLG == 1) {
		DWDraftLimit_o <- DWBotVol
	} else {
		DWDraftLimit_o <- 2.238e6
	}
	return(DWDraftLimit_o)
}
DWPenLimit <- function() {
	DWGenPenCap <- 24000
	DWPenLimit_o <- DWGenPenCap * cfsTOafw
	return(DWPenLimit_o)
}
DW_CurFC <- function() {
	if (DWRunoffAprJuly < 1.2E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 2] # DWFlood1
	} else if (DWRunoffAprJuly < 1.4E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 3] # DWFlood2
	} else if (DWRunoffAprJuly < 1.8E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 4] # DWFlood3
	} else if (DWRunoffAprJuly < 2.2E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 5] # DWFlood4
	} else if (DWRunoffAprJuly < 2.6E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 6] # DWFlood5
	} else if (DWRunoffAprJuly < 3.0E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 7] # DWFlood6
	} else if (DWRunoffAprJuly < 3.2E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 8] # DWFlood7
	} else if (DWRunoffAprJuly < 3.4E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 9] # DWFlood8
	} else if (DWRunoffAprJuly < 3.6E6) {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 10] # DWFlood9
	} else {
		DW_CurFC_o <- DWFlood_input[week_counter_in_year(), 10]
	} 
	return(DW_CurFC_o)
}
DWCurFC_Cont <- function() {
	DW_FloodM_Range <- c(1200000, 1400000, 1600000, 1800000, 2000000, 2200000, 2400000, 2600000, 2800000, 3000000, 3200000, 3400000, 3600000, 3800000)
	if (DW_FloodM_Range[length(DW_FloodM_Range)] < DWRunoffAprJuly) {
		row_no <- length(DW_FloodM_Range)
	} else {
		row_no <- max(1.0, which(DW_FloodM_Range > DWRunoffAprJuly)[1] - 1)
	}
	if (DWRunoffAprJuly <= 3.8E06) {
		if (month_in_year == 3 || month_in_year == 4 || month_in_year == 5) {
			DWCurFC_Cont_o <- 2767950
		} else if (month_in_year == 6) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 2] # JAN_DW
		} else if (month_in_year == 7) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 3] # FEB_DW
		} else if (month_in_year == 8) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 4] # MAR_DW
		} else if (month_in_year == 9) {
			DWCurFC_Cont_o <- DW_FloodM[row_no, 5] # APR_DW
		} else {
			DWCurFC_Cont_o <- DWFullPoolVol
		}
	} else if (DWRunoffAprJuly > 3.8E06 && month_in_year %in% 3:9) {
		DWCurFC_Cont_o <- DWFlood_input[week_counter_in_year(), 10] # DWFlood10
	} else {
	  DWCurFC_Cont_o <- DWFullPoolVol
	}
	return(DWCurFC_Cont_o)
}
DWFloodCurve <- function() {
	if (FC_Option == 1) {
		DWFloodCurve_o <- DWFullPoolVol - DWFloodEvacMult() * GlobalFloodEvacMult * (DWFullPoolVol - DW_CurFC())
	} else if (FC_Option == 2) {
		DWFloodCurve_o <- DWFullPoolVol - DWFloodEvacMult() * GlobalFloodEvacMult * (DWFullPoolVol - DWCurFC_Cont())
	}
	return(DWFloodCurve_o)
}
DWTopVol <- function() {
	if (TopRuleSw() == 0) {
		DWTopVol_o <- DWFloodCurve()
	} else if (TopRuleSw() == 1) {
		DWTopVol_o <- DWFullPoolVol
	} else {
		DWTopVol_o <- DWFlood_input[week_counter_in_year(), 2]
	}
	return(DWTopVol_o)
}
DWRuleReq <- function() {
	DWRuleReq_o <- max(Dworshak() + DWIn() - DWTopVol(), 0)
	return(DWRuleReq_o)
}
DWAvailAfter <- function() {
	DWAvailAfter_o <- max(0, Dworshak() + DWIn() - DWBotVol)
	return(DWAvailAfter_o)
}
TotalRelForLowerGranite <- function() { # Total release from Dworshak and Brownlee for meeting Lower Granite fish flow target
	Upsupply <- LowerGraniteFlowData() - DworshakFlowData() - BrownleeFlowData()
	Updemand <- DemVICLG + DemVICOX + DemVICHC
	TotalRelForLowerGranite_o <- max(0, LowerGraniteTarget() * cfsTOafw - (Upsupply - Updemand))
	return(TotalRelForLowerGranite_o)
}
DWRelForLG <- function() { # Release from Dworshak for meeting Lower Graninte fish flow target
    if(is.na(water_df[week_counter,4])){
		water_df[week_counter,4] <<- BRIn()
	}
	BRIn_c = water_df[week_counter,4]
	if ((StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()) == 0) {
		DWRelForLG_o <- 0
	} else {
		DWRelForLG_o <- TotalRelForLowerGranite() *
		((StorFrac * Dworshak() + InflowFrac * DWIn() - StorFrac * DWDraftLimit()) /
        (StorFrac * Brownlee() + StorFrac * Dworshak() + InflowFrac * BRIn_c + InflowFrac * DWIn() - StorFrac * BRDraftLimit() - StorFrac * DWDraftLimit()))
	}
	return(DWRelForLG_o)
}
DWMinReq <- function() {
	if (RefillMinSw() == 1) {
		DWMinReq_o <- DWRefillMin()
	} else {
		DWMinReq_o <- max(DWAvgMin() * cfsTOafw, min(DWLGAvailWater(), DWRelForLG()))
	}
	return(DWMinReq_o)
}
DWPrelim <- function() {
	DWPrelim_o <- min(DWAvailAfter(), max(DWRuleReq(), DWMinReq()))
	return(DWPrelim_o)
}
DWNetHead <- function() {
	DWTailElev <- 980
	DWLoss <- 0
	DWNetHead_o <- DWElev_ft() - DWTailElev - DWLoss
	return(DWNetHead_o)
}
DWElev_ft <- function() {
	DWElev_ft_o <- -1.47637889E-11 * Dworshak()^2 + 1.49196994E-04 * Dworshak() + 1.25967539E+03
	return(DWElev_ft_o)
}
DWPreEnergy <- function() {
	DWPreEnergy_o <- MWhr_per_ftAcFt * min(DWPrelim(), DWPenLimit()) * DWNetHead() * DWCombEfficiency
	return(DWPreEnergy_o)
}
DWFloodSpace <- function() {
	DWFloodSpace_o <- 0 
	return(DWFloodSpace_o)
}
DWFloodEvacMult <- function() {
	DWFloodEvacMult_o <- 1
	return(DWFloodEvacMult_o)
}
DWRefillMin <- function() {
	DWRefillMin_o <- DWRefillMin_input[week_counter_in_year(), 2]
	return(DWRefillMin_o)
}
DWAvgMin <- function() {
	DWAvgMin_o <- DWAvgMin_input[week_counter_in_year(), 2]
	return(DWAvgMin_o)
}
DWLGAvailWater <- function() {
	DWLGAvailWater_o <- max(0, Dworshak() + DWIn() - DWDraftLimit())
	return(DWLGAvailWater_o)
}
DWSharedWater <- function() {
	DWSharedWater_o <- max(0, Dworshak() + DWIn() - DWPrelim() - DWBotVol)
	return(DWSharedWater_o)
}
DWEnergyContent <- function() {
	DWEnergyContent_o <- DWSharedWater() * (DWNetHead() + DWDownstreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DWEnergyContent_o)
}
DWDownstreamHead <- function() {
	DWDownstreamHead_o <- BONNetHead() + DANetHead() + IHNetHead() + JDNetHead() + LGNetHead() + LIGNetHead() + LMNetHead() + MCNetHead()
	return(DWDownstreamHead_o)
}
DWCriticalCurve <- function() {
	DWCriticalCurve_o <- DWCriticalCurve_input[week_counter_in_year(), 2]
	return(DWCriticalCurve_o)
}
DWRefillCurve <- function() {
	if (RefillSwitch() == 1) {
		DWRefillCurve_o <- DWRefillVol1
	} else if (RefillSwitch() == 2) {
		DWRefillCurve_o <- DWRefillVol2
	} 
	return(DWRefillCurve_o)
}
DW1931Refill <- function() {
	DW1931Refill_o <- DW1931Refill_input[week_counter_in_year(), 2]
	return(DW1931Refill_o)
}
DWECC <- function() {
	if (month_in_year <= 5 || month_in_year == 12) {
		out1 <- max(DWCriticalCurve(), DWRefillCurve())
	} else {
		out1 <- min(max(DWRefillCurve(), DWCriticalCurve()), max(DWCriticalCurve(), DW1931Refill()))
	}
	DWECC_o <- min(DWFloodCurve(), out1)
	return(DWECC_o)
}
DWRelLimit <- function() {
	DWRelLimit_o <- max(Dworshak() + DWInflow() - DWBotVol, 0)
	return(DWRelLimit_o)
}
DWECCSharedWater <- function() {
	DWECCSharedWater_o <- max(0, Dworshak() + DWIn() - DWPrelim() - DWECC())
	return(DWECCSharedWater_o)
}
DWECCEnergyContent <- function() {
	DWECCEnergyContent_o <- DWECCSharedWater() * (DWNetHead() + DWDownstreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DWECCEnergyContent_o)
}
DWFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			DWFirmEngSup_o <- 0
		} else {
			DWFirmEngSup_o <- (DWEnergyContent() + DWECCEnergyContent()) / (TotalEnergyContent_c + TotalECCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (TotalECCEnergyContent_c == 0) {
		DWFirmEngSup_o <- 0
	} else {
		DWFirmEngSup_o <- DWECCEnergyContent() / TotalECCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(DWFirmEngSup_o)
}
DWNFEnergyContent <- function() {
	DWNFEnergyContent_o <- max(0, DWECCEnergyContent() - DWFirmEngSup())
	return(DWNFEnergyContent_o)
}
DWNonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		DWNonFirmEngSup_o <- 0
	} else {
		DWNonFirmEngSup_o <- DWNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}	
	return(DWNonFirmEngSup_o)
}
DWNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw == 1) {
		DWNonFirmEngSupReq_o <- min(DWPenLimit(), (DWFirmEngSup() + DWNonFirmEngSup()) / (MWhr_per_ftAcFt * (DWNetHead() + DWDownstreamHead()) * DWCombEfficiency))
	} else {
		DWNonFirmEngSupReq_o <- 0
	}
	return(DWNonFirmEngSupReq_o)
}
DWFirmEngSupReq <- function() {
	DWFirmEngSupReq_o <- min(DWPenLimit(), DWFirmEngSup() / (MWhr_per_ftAcFt * (DWNetHead() + DWDownstreamHead()) * DWCombEfficiency))
	return(DWFirmEngSupReq_o)
}
DWEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DWSharedWater()), min(DWNonFirmEngSupReq(), DWECCSharedWater())))
	} else {
		DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DWECCSharedWater()), min(DWNonFirmEngSupReq(), DWECCSharedWater())))
	}
	return(DWEnergySup_o)
}
DWCombSup <- function() {
	DWCombSup_o <- DWEnergySup()
	return(DWCombSup_o)
}
DWRelease <- function() {
	DWRelease_o <- min(DWPrelim() + DWCombSup(), DWRelLimit())
	return(DWRelease_o)
}
DWOutflow <- function() {
	if (ResetStorage() == 1) {
		DWOutflow_o <- 0
	} else {
		DWOutflow_o <- DWRelease()
	}
	return(DWOutflow_o)
}
################# Lower Granite Dam ######################################

LowerGraniteFlowData <- function() {
	return(PriVICLG)
}
LGInc <- function() {
	LGInc_o <- LowerGraniteFlowData() - HellsCanyonFlowData() - DworshakFlowData()
	return(LGInc_o)
}
LowerGraniteTarget <- function() {
	LowerGraniteTarget_o <- LowerGraniteTarget_input[month_in_year, 2]
	return(LowerGraniteTarget_o)
}
LGPenLimit <- function() {
	LGPenCap <- 130000
	LGPenLimit_o <- LGPenCap * cfsTOafw
	return(LGPenLimit_o)
}
LGPrelim <- function() {
	UpDemand <- DemVICOX + DemVICHC + DemVICLG
	IncFlow <- LowerGraniteFlowData() - DworshakFlowData() - BrownleeFlowData()
	LGPrelim_o <- BRPrelim_c + DWPrelim() + IncFlow - UpDemand
	return(LGPrelim_o)
}
LGNetHead <- function() {
	LGNetHead_o <- 100
	return(LGNetHead_o)
}
LGPreEnergy <- function() {
	LGPreEnergy_o <- MWhr_per_ftAcFt * min(LGPrelim(), LGPenLimit()) * LGNetHead() * LGCombEfficiency
	return(LGPreEnergy_o)
}
LGIn <- function() {
	LGIn_o <- DWOutflow() + HCOut() + LGInc() - DemVICLG
	return(LGIn_o)
}
LGOut <- function() {
	LGOut_o <- LGIn()
	return(LGOut_o)
}
############################ LITTLE GOOSE #####################

LittleGooseFlowData <- function() {
	return(PriVICLIG)
}
LIGInc <- function() {
	LIGInc_o <- LittleGooseFlowData() - LowerGraniteFlowData()
	return(LIGInc_o)
}
LIGPrelim <- function() {
	UpDemand <- DemVICOX + DemVICHC + DemVICLG + DemVICLIG # Water demand between Little Goose Falls and Brownlee
	IncFlow <- LittleGooseFlowData() - DworshakFlowData() - BrownleeFlowData()
	LIGPrelim_o <- BRPrelim_c + IncFlow - UpDemand
	return(LIGPrelim_o)
}
LIGNetHead <- function() {
	LIGNetHead_o <- 98
	return(LIGNetHead_o)
}
LIGPenLimit <- function() {
	LIGPenCap <- 130000
	LIGPenLimit_o <- LIGPenCap * cfsTOafw
	return(LIGPenLimit_o)
}
LIGPreEnergy <- function() {
	LIGPreEnergy_o <- MWhr_per_ftAcFt * min(LIGPrelim(), LIGPenLimit()) * LIGNetHead() * LIGCombEfficiency
	return(LIGPreEnergy_o)
}
LIGIn <- function() {
	LIGIn_o <- LGOut() + LIGInc() - DemVICLIG
	return(LIGIn_o)
}
LIGOut <- function() {
	LIGOut_o <- LIGIn()
	return(LIGOut_o)
}

##################### LOWER MONUMENT ##################################################### 

LowerMonuFlowData <- function() {
	return(PriVICLM)
}
LMInc <- function() {
	LMInc_o <- LowerMonuFlowData() - LittleGooseFlowData()
	return(LMInc_o)
}
LMIn <- function() {
	LMIn_o <- LIGPrelim() + LMInc() - DemVICLM
	return(LMIn_o)
}
LMOut <- function() {
	LMOut_o <- LMIn()
	return(LMOut_o)
}
LMPenLimit <- function() {
	LMPenCap <- 130000
	LMPenLimit_o <- LMPenCap * cfsTOafw
	return(LMPenLimit_o)
}
LMNetHead <- function() {
	LMNetHead_o <- 100
	return(LMNetHead_o)
}
LMPrelim <- function() {
	UpDemand <- DemVICOX + DemVICHC + DemVICLG + DemVICLIG + DemVICLM # Water demand between Lower Monumental and Brownlee
	IncFlow <- LowerMonuFlowData() - DworshakFlowData() - BrownleeFlowData()
	LMPrelim_o <- BRPrelim_c + IncFlow - UpDemand
	return(LMPrelim_o)
}
LMPreEnergy <- function() {
	LMPreEnergy_o <- MWhr_per_ftAcFt * min(LMPrelim(), LMPenLimit()) * LMNetHead() * LMCombEfficiency
	return(LMPreEnergy_o)
}

###################### ICE HARBOR ###############################

IceHarborFlowData <- function() {
	return(PriVICIH)
}
IHInc <- function() {
	IHInc_o <- IceHarborFlowData() - LowerMonuFlowData()
	return(IHInc_o)
}
IHIn <- function() {
	IHIn_o <- LMOut() + IHInc() - DemVICIH
	return(IHIn_o)
}
IHOut <- function() {
	IHOut_o <- IHIn()
	return(IHOut_o)
}
IHPrelim <- function() {
	UpDemand <- DemVICOX+ DemVICHC + DemVICLG + DemVICLIG + DemVICLM + DemVICIH # Water demand between Ice Harbor and Brownlee
	IncFlow <- IceHarborFlowData() - DworshakFlowData() - BrownleeFlowData()
	IHPrelim_o <- BRPrelim_c + IncFlow - UpDemand
	return(IHPrelim_o)
}
IHPenLimit <- function() {
	IHPenCap <- 106000
	IHPenLimit_o <- IHPenCap * cfsTOafw
	return(IHPenLimit_o)
}
IHNetHead <- function() {
	IHNetHead_o <- 98
	return(IHNetHead_o)
}
IHPreEnergy <- function() {
	IHPreEnergy_o <- MWhr_per_ftAcFt * min(IHPrelim(), IHPenLimit()) * IHNetHead() * IHCombEfficiency
	return(IHPreEnergy_o)
}

############################### MCNARY DAM #####################

McNaryFlowData <- function() {
	return(PriVICMCN)
}
MCNInc <- function() {
	MCNInc_o <- McNaryFlowData() - IceHarborFlowData() - PriestRapidsFlowData()
	return(MCNInc_o)
}
MCNCurtail <- function() {
	MCNCurtail_0 <- min(DemVICMCN, max(IflowMCN + DemVICMCN - IHOut() - PROut() - MCNInc(), 0))
	if (curtail_option == 1) {
		MCNCurtail_o <- ifelse(MCNCurtail_0 > 0, CurtVICMCN, 0)
	} else if (curtail_option == 2) {
		MCNCurtail_o <- MCNCurtail_0
	} else if (curtail_option == 3) {
		MCNCurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		MCNCurtail_o <- 0
	} else {
		MCNCurtail_o <- MCNCurtail_o
	}		
	return(MCNCurtail_o)
}
MCNInstreamShortfall <- function() {
	MCNInstreamShortfall_o <- max(IflowMCN + DemVICMCN - IHOut() - PROut() - MCNInc(), 0)
	return(MCNInstreamShortfall_o)
}
MCNPrelim <- function() {
	MCNPrelim_o <- IHPrelim() + PRPrelim() + MCNInc() - DemVICMCN
	return(MCNPrelim_o)
}
MCNetHead <- function() {
	MCNetHead_o <- 74
	return(MCNetHead_o)
}
MCNPenLimit <- function() {
	MCNPenCap <- 232000
	MCNPenLimit_o <- MCNPenCap * cfsTOafw
	return(MCNPenLimit_o)
}
MCNPreEnergy <- function() {
	MCNPreEnergy_o <- MWhr_per_ftAcFt * min(MCNPrelim(), MCNPenLimit()) * MCNetHead() * MCCombEfficiency
	return(MCNPreEnergy_o)
}
MCNIn <- function() {
	MCNIn_o <- IHOut() + PROut() + MCNInc() - DemVICMCN
	return(MCNIn_o)
}
MCNOut <- function() {
	MCNOut_o <- MCNIn()
	return(MCNOut_o)
}
# Biological Opinions ------------------------------------------------------
# The Corps designates Libby, and the USBR designates Grand Coulee and Hungry Horse to contribute to the Federal Columbia River Power System (FCRPS)
# goal of meeting the following BiOp flow objectives:
# Spring (4/10 - 6/30)     #Priest Rapids (135 kcfs)
# Spring (4/20 - 6/30)**   #McNary Dam (220-260 kcfs)
# Summer (7/1 - 8/31)      #McNary Dam (200 kcfs)
# Interpolation between 220 and 260 kcfs is based upon Corps of Engineers data submittal, which bases targets on forecasts of Jan-July flow at The Dalles.

##### McNary Flow target
# Variable minflow for McNary based upon forecasted inflow to the Dalles, for the period of April 20 to June 30.
# units cfs
# Flow varies linearly between 220 and 260 kcfs based on inflows of 85 to 105 MAF at the Dalles
# McNary flow target = base target + variable target
McNaryBaseTarget <- function() {
  McNaryBaseTarget_o <- MNB_input[month_in_year, 2]
  return(McNaryBaseTarget_o)
}
MCN_variable_1 <- function() {
	if (DallesJanJul <= 85e6) {
		MCNV9 <- 0
	} else if (DallesJanJul >= 105E6) {
		MCNV9 <- 40000
	} else {
		MCNV9 <- (DallesJanJul - 85E6) / 20E6 * 40000
	}
	if (month_in_year == 10 || month_in_year == 11) {
		if (DallesJanJul <= 85e6) {
			MCN_variable_1_o <- 0
		} else if (DallesJanJul >= 105E6) {
			MCN_variable_1_o <- 40000
		} else {
			MCN_variable_1_o <- (DallesJanJul - 85E6) / 20E6 * 40000
		}
	} else if (month_in_year == 9) {
		MCN_variable_1_o <- 10 / 30 * (MCNV9)
	} else {
		MCN_variable_1_o <- 0
	}
	return(MCN_variable_1_o)
}
McNaryFlowTarget <- function() {
	McNaryFlowTarget_o <- (McNaryBaseTarget() + MCN_variable_1()) * cfsTOafw
	return(McNaryFlowTarget_o)
}
McNaryFlowDeficit <- function() {
	if (BRPrelim_c == -9999) {
		BRPrelim_c <<- BRPrelim()
		water_df[week_counter,1] <<- BRPrelim_c
	}
	McNaryFlowDeficit_o <- max(0, ((McNaryBaseTarget() + MCN_variable_1()) * cfsTOafw) - MCNPrelim())
	return(McNaryFlowDeficit_o)
}

################# JOHN DAY #############################

JohnDayFlowData <- function() {
	return(PriVICJD)
}
JDInc <- function() {
	JDInc_o <- JohnDayFlowData() - McNaryFlowData()
	return(JDInc_o)
}
JDCurtail <- function() {
	JDCurtail_0 <- min(DemVICJD, max(IflowJD + DemVICJD - MCNOut() - JDInc(), 0))
	if (curtail_option == 1) {
		JDCurtail_o <- ifelse(JDCurtail_0 > 0, CurtVICJD, 0)
	} else if (curtail_option == 2) {
		JDCurtail_o <- JDCurtail_0
	} else if (curtail_option == 3) {
		JDCurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		JDCurtail_o <- 0
	} else {
		JDCurtail_o <- JDCurtail_o
	}		
	return(JDCurtail_o)
}
JDInstreamShortfall <- function() {
	JDInstreamShortfall_o = max(IflowJD + DemVICJD - MCNOut() - JDInc(), 0)
	return(JDInstreamShortfall_o)
}
JDPenLimit <- function() {
	JDPenCap <- 322000
	JDPenLimit_o <- JDPenCap * cfsTOafw
	return(JDPenLimit_o)
}
JDPrelim <- function() {
	JDPrelim_o <- MCNPrelim() + JDInc() - DemVICJD
	return(JDPrelim_o)
}
JDNetHead <- function() {
	JDNetHead_o <- 100
	return(JDNetHead_o)
}
JDPreEnergy <- function() {
	JDPreEnergy_o <- MWhr_per_ftAcFt * min(JDPrelim(), JDPenLimit()) * JDNetHead() * JDCombEfficiency
	return(JDPreEnergy_o)
}
JDIn <- function() {
	JDIn_o <- MCNOut() + JDInc() - DemVICJD
	return(JDIn_o)
}
JDOut <- function() {
	JDOut_o <- JDIn()
	return(JDOut_o)
}

###################### DALLE ######################################

DallesFlowData <- function() {
	return(PriVICDA)
}
DAInc <- function() {
	DAInc_o <- DallesFlowData() - JohnDayFlowData()
	return(DAInc_o)
}
DACurtail <- function() {
	DACurtail_0 <- min(DemVICDA, max(IflowDA + DemVICDA - JDOut() - DAInc(), 0))
	if (curtail_option == 1) {
		DACurtail_o <- ifelse(DACurtail_0 > 0, CurtVICDA, 0)
	} else if (curtail_option == 2) {
		DACurtail_o <- DACurtail_0
	} else if (curtail_option == 3) {
		DACurtail_o <- 0
	}
	if (DallesRunoffAprSep > mainstem_rule) {
		DACurtail_o <- 0
	} else {
		DACurtail_o <- DACurtail_o
	}		
	return(DACurtail_o)
}
DAInstreamShortfall <- function() {
	DAInstreamShortfall_o = max(IflowDA + DemVICDA - JDOut() - DAInc(), 0)
	return(DAInstreamShortfall_o)
}
DALowFloodTarget <- function() {
	DALowFloodTarget_o <- DALowFloodTarget_input[month_in_year, 2]
	return(DALowFloodTarget_o)
}
DAHighFloodTarget <- function() {
	DAHighFloodTarget_o <- DAHighFloodTarget_input[month_in_year, 2]
	return(DAHighFloodTarget_o)
}
DAFloodTarget <- function() {
	DAFloodTarget_o <- max(400000, if (DallesRunoffAprAug > 120E6) {
		DAHighFloodTarget()
	} else {
		DALowFloodTarget()
	})
	return(DAFloodTarget_o)
}
DAPenLimit <- function() {
	DAPenCap <- 375000
	DAPenLimit_o <- DAPenCap * cfsTOafw
	return(DAPenLimit_o)
}
DAPrelim <- function() {
	DAPrelim_o <- JDPrelim() + DAInc() - DemVICDA
	return(DAPrelim_o)
}
DANetHead <- function() {
	DANetHead_o <- 80.14
	return(DANetHead_o)
}
DAPreEnergy <- function() {
	DAPreEnergy_o <- MWhr_per_ftAcFt * min(DAPrelim(), DAPenLimit()) * DANetHead() * DACombEfficiency
	return(DAPreEnergy_o)
}
DAIn <- function() {
	DAIn_o <- JDOut() + DAInc() - DemVICDA
	return(DAIn_o)
}
DAOut <- function() {
	DAOut_o <- DAIn()
	return(DAOut_o)
}

################# BONNEVILLE DAM #############################
BonnevilleTarget <- function() {
	# Grand Coulee is designated to meet the BiOP chum flow requirement at Bonneville Dam
	# 125-160 kcfs
	# November to April 9.
	# The USBR did not specify a method for designating the variable flow target, other than that the critical flow periods will assume the minimum 125 kcfs as a target.
	# The algorithm below uses the same method of ranking critical years as that of McNary flow targets and linearly
	# interpolates the flow targets between forecasted inflows at The Dalles of 85 and 105 MAF between January and July.
	BonnevilleTarget_o <- (Chum_variable_2() + Chum_variable_1())
	return(BonnevilleTarget_o)
}
BONTarget_AcFt <- function() {
	BONTarget_AcFt_o <- BonnevilleTarget() * cfsTOafw
	return(BONTarget_AcFt_o)
}
BONFlowDeficit <- function() {
  if (Chum_Q_Switch()==1) {
	  BONFlowDeficit_o <- max(0, BONTarget_AcFt() - BONPrelim()) * Chum_Q_Switch()
  } else {
    BONFlowDeficit_o <- max(0, BONTarget_AcFt() - BONPrelim()) * 0
  }
	return(BONFlowDeficit_o)
}
Chum_variable_2 <- function() {
	Chum_variable_2_o <- Chum_variable_2_input[month_in_year, 2]
	return(Chum_variable_2_o)
}
Chum_variable_1 <- function() {
	# Variable minimum flow for Bonneville chum based upon forecasted inflow to the Dalles, for the period of November to April 9.
	# units cfs
	# Flow varies linearly between 125 and 160 kcfs based on inflows of 85 to 105 MAF at the Dalles
	if (month_in_year >= 4 && month_in_year <= 8) {
		if (DallesJanJul <= 85e6) {
			Chum_variable_1_o <- 0
		} else if (DallesJanJul >= 105E6) {
			Chum_variable_1_o <- 35000
		} else {
			Chum_variable_1_o <- (DallesJanJul - 85E6) / 20E6 * 35000
		}	
	} else if (month_in_year == 9) {
		if (DallesJanJul <= 85e6) {
			out2 <- 125000
		} else if (DallesJanJul >= 105E6) {
			out2 <- 160000
		} else {
			out2 <- (DallesJanJul - 85E6) / 20E6 * 35000
		}
		Chum_variable_1_o <- 9 / 30 * out2 + 125000
	} else {
		Chum_variable_1_o <- 0
	}
	return(Chum_variable_1_o)
}
BonnevilleFlowData <- function() {
	return(PriVICBON)
}
BONInc <- function() {
	BONInc_o <- BonnevilleFlowData() - DallesFlowData()
	return(BONInc_o)
}
BONPenLimit <- function() {
	BONPenCap <- 288000
	BONPenLimit_o <- BONPenCap * cfsTOafw
	return(BONPenLimit_o)
}
BONNetHead <- function() {
	BONNetHead_o <- BONNetHead_input[month_in_year, 2]
	return(BONNetHead_o)
}
BONPrelim <- function() {
	BONPrelim_o <- DAPrelim() + BONInc() - DemVICBON
	return(BONPrelim_o)
}
BONFlowDeficit <- function() {
	BONFlowDeficit_o <- max(0, BONTarget_AcFt() - BONPrelim()) * Chum_Q_Switch()
	return(BONFlowDeficit_o)
}
BONPreEnergy <- function() {
	BONPreEnergy_o <- MWhr_per_ftAcFt * min(BONPrelim(), BONPenLimit()) * BONNetHead() * BONCombEfficiency 
	return(BONPreEnergy_o)
}
BONIn <- function() {
	BONIn_o <- DAOut() + BONInc() - DemVICBON
	return(BONIn_o)
}
BONOut <- function() {
	BONOut_o <- BONIn()
	return(BONOut_o)
}

########################################### TOTAL ENERGY #########################################

DworshakGroupPreEnergy <- function() {
	DworshakGroupPreEnergy_o <- DWPreEnergy() + IHPreEnergy() + LGPreEnergy() + LIGPreEnergy() + LMPreEnergy()
	return(DworshakGroupPreEnergy_o)
}
LowerColPreEnergy <- function() {
	LowerColPreEnergy_o <- BONPreEnergy() + DAPreEnergy() + JDPreEnergy() + MCNPreEnergy()
	return(LowerColPreEnergy_o)
}
TotalMcNarySharedWater <- function() {
	TotalMcNarySharedWater_o <- MIMcNarySharedWater() + DUMcNarySharedWater() + ARMcNarySharedWater() + GCMcNarySharedWater() + HHMcNarySharedWater() + LBMcNarySharedWater()
	return(TotalMcNarySharedWater_o)
}
TotalFloodSpace <- function() {
	TotalFloodSpace_o <- MIFloodMult * MIFloodSpace() + ARFloodSpace() + GCFloodSpace() + LBFloodMult * LBFloodSpace() + HHFloodSpace() + KRFloodSpace() + DWFloodSpace()
	return(TotalFloodSpace_o)
}
TotalRelReducReq <- function() {
	TotalRelReducReq_o <- max(0, (DAPrelim() - (DAFloodTarget() * cfsTOafw)))
	return(TotalRelReducReq_o)
}
GCDownStreamHead <- function() {
	GCDownStreamHead_o <- BONNetHead() + CJNetHead() + DANetHead() + JDNetHead() + MCNetHead() + PRNetHead() + RINetHead() + RRNetHead() + WANetHead() + WENetHead()
	return(GCDownStreamHead_o)
}
TotalEnergyContent <- function() {
	TotalEnergyContent_o <- DWEnergyContent() + GCEngContMult * GCEnergyContent() + HHEnergyContent() + LBEnergyContent() + MIEnergyContent() + AREnergyContent() + DUEnergyContent()
	return(TotalEnergyContent_o)
}
TotalECCEnergyContent <- function() {
	TotalECCEnergyContent_o <-
    HHECCEnergyContent() + LBECCEnergyContent() + MIECCEnergyContent() + GCEngContMult * GCECCEnergyContent() + DWECCEnergyContent() + ARECCEnergyContent() + DUECCEnergyContent()
	return(TotalECCEnergyContent_o)
}
FirmFraction <- function() { # Adjustment to the average firm energy load throughout the year. 
	FirmFraction_o <- FirmFraction_input[week_counter_in_year(), 2]
	return(FirmFraction_o)
}
AvgFirmLoad <- function() { # Average firm energy requirement
	AvgFirmLoad_o <- 1.20E+06
	return(AvgFirmLoad_o)
}
# Average firm energy target. This value is multiplied by the seasonal fraction to yield the firm energy target for each month.  Units MW-hr/month.
FirmEnergyTarget <- function() {
	FirmEnergyTarget_o <- AvgFirmLoad() * (Deviation__From_Normal_Curve * FirmFraction() + 1)
	return(FirmEnergyTarget_o)
}
TotalCoordPreEnergy <- function() { # Hydropower that could be generated from prelinary releases at all dams
  TotalCoordPreEnergy_o <- TotalEnergyFromMcNarySups() + HungryHorsePreEnergy() + LibbyPreEnergy() + MicaGrPreEnergy() + KerrGrPreEnergy() +
    AlbeniFallsGroupPreEnergy() + GrandCouleePreEnergy() + DworshakGroupPreEnergy() + LowerColPreEnergy()
  return(TotalCoordPreEnergy_o)
}
FirmEnergyDeficit <- function() { # Basin-wide deficit in firm energy production 
	FirmEnergyDeficit_o <- max(0, EnergyAllocSafeFactor * FirmEnergyTarget() - TotalCoordPreEnergy_c)
	return(FirmEnergyDeficit_o)
}
TotalEnergyFromMcNarySups <- function() { # Total hydropower generated by water released to meet fish flow targets
	TotalEnergyFromMcNarySups_o <- MIMcNarySupEnergy() + ARMcNarySupEnergy() + DUMcNarySupEnergy() + GCMcNaryBONSupEnergy() + HHMcNarySupEnergy() + LBMcNarySupEnergy()
	return(TotalEnergyFromMcNarySups_o)
}
TotalNFEnergyContent <- function() { # Total non-firm energy content
	TotalNFEnergyContent_o <- ARNFEnergyContent() + DUNFEnergyContent() + DWNFEnergyContent() +
    GCEngContMult * GCNFEnergyContent() + HHNFEnergyContent() + LBNFEnergyContent() + MINFEnergyContent()
	return(TotalNFEnergyContent_o)
}
NonFirmEnergyDeficit <- function() {
	NonFirmEnergyDeficit_c <- max(0, EnergyAllocSafeFactor * NonFirmEnergyTarget() - max(0, (TotalCoordPreEnergy_c - FirmEnergyTarget())))
	return(NonFirmEnergyDeficit_c)
}
NonFirmFraction <- function() { # Fraction of the average non-firm energy load.
  NonFirmFraction_o <- NonFirmFraction_input[week_counter_in_year(), 2]
  return(NonFirmFraction_o)
}
AltNonFirmLoad <- function() {
  AltNonFirmLoad_o <- AltNonFirmLoad_input[week_counter_in_year(), 2]
  return(AltNonFirmLoad_o)
}
AvgNonFirmLoad <- function() {
  # Yearly average non-firm energy load.  Units MWhr.
  # Status quo average non-firm load.
  # Use this control when UseAlternateNonFirmTarget is set to a value of 0.
  #AvgNonFirmLoad_o <- 0.2617 * AvgFirmLoad()
  # for some reason the output of this function is different in the original ColSim the value is always
  # 2.9 * 10^6
  #AvgNonFirmLoad_o <- 2.9 * 10^6
  AvgNonFirmLoad_o <- AvgFirmLoad()
  return(AvgNonFirmLoad_o)
}
NonFirmEnergyTarget <- function() {
	# Non-firm energy target by month.  Units MW-hr/month.
	# IF UseAlternateNonFirmTarget = 0 THEN NonFirmFraction*ClimateForecastEnergyMult
	# ELSE
	# IF month<=6 THEN ClimateForecastEnergyMult*AltAvgNonFirmLoad*AltNonFirmFraction
	# ELSE NonFirmFraction*AvgNonFirmLoad
	if (UseAlternateNonFirmTarget == 0) {
		NonFirmEnergyTarget_o <- NonFirmFraction() * AvgNonFirmLoad()
	} else {
		NonFirmEnergyTarget_o <- AltNonFirmLoad()
	}
	return(NonFirmEnergyTarget_o)
}

#### Actual Hydropower production

AlbeniFallsGroupEnergy <- function() { # Actual hydropower generation from Boundary, Albeni Falls, and Box Canyon dams
  AlbeniFallsGroupEnergy_o <- BDEnergyProduction() + AFEnergyProduction() + BCEnergyProduction()
  return(AlbeniFallsGroupEnergy_o)
}
BDEnergyProduction <- function() {
  BDEnergyProduction_o <- MWhr_per_ftAcFt * min(BDOut(), BDPenLimit()) * BDNetHead() * BDCombEfficiency
  return(BDEnergyProduction_o)
}
AFEnergyProduction <- function() { 
  AFEnergyProduction_o <- MWhr_per_ftAcFt * min(AFRelease(), AFPenLimit()) * AFNetHead() * AFCombEfficiency()
  return(AFEnergyProduction_o)
}
BCEnergyProduction <- function() {
  BCEnergyProduction_o <- MWhr_per_ftAcFt * min(BCRelease(), BCPenLimit()) * BCNetHead() * BCCombEfficiency
  return(BCEnergyProduction_o)
}

DworshakGroupEnergy <- function() {
  DworshakGroupEnergy_o <- LIGEnergyProduction() + DWEnergyProduction() + LMEnergyProduction() + LGEnergyProduction() + IHEnergyProduction()
  return(DworshakGroupEnergy_o)
}
LIGEnergyProduction <- function() {
  LIGEnergyProduction_o <- MWhr_per_ftAcFt * min(LIGOut(), LIGPenLimit()) * LIGNetHead() * LIGCombEfficiency
  return(LIGEnergyProduction_o)
}
DWEnergyProduction <- function() {
  DWEnergyProduction_o <-  MWhr_per_ftAcFt * min(DWRelease(), DWPenLimit()) * DWNetHead() * DWCombEfficiency
  return(DWEnergyProduction_o)
}
LMEnergyProduction <- function() {
  LMEnergyProduction_o <-  MWhr_per_ftAcFt * min(LMOut(), LMPenLimit()) * LMNetHead() * LMCombEfficiency
  return(LMEnergyProduction_o)
}
LGEnergyProduction <- function() {
  LGEnergyProduction_o <- MWhr_per_ftAcFt * min(LGOut(), LGPenLimit()) * LGNetHead() * LGCombEfficiency
  return(LGEnergyProduction_o)
}
IHEnergyProduction <- function() {
  IHEnergyProduction_o <- MWhr_per_ftAcFt * min(IHOut(), IHPenLimit()) * IHNetHead() * IHCombEfficiency
  return(IHEnergyProduction_o)
}
GrandCouleeGroupEnergy <- function() {
  GrandCouleeGroupEnergy_o <- CJEnergyProduction() + GCEnergyProduction() + PREnergyProduction() + RIEnergyProduction()
  + RREnergyProduction() + WAEnergyProduction() + WEEnergyProduction()
  return(GrandCouleeGroupEnergy_o)
}
CJEnergyProduction <- function() {
  CJEnergyProduction_o <- MWhr_per_ftAcFt * min(CJOut(), CJPenLimit()) * CJNetHead() * CJCombEfficiency
  return(CJEnergyProduction_o)
}
GCEnergyProduction <- function() {
  GCEnergyProduction_o <- MWhr_per_ftAcFt * min(GCRelease(), GCPenLimit()) * GCNetHead() * GCCombEfficiency
  return(GCEnergyProduction_o)
}
PREnergyProduction <- function() {
  PREnergyProduction_o <- MWhr_per_ftAcFt * min(PROut(), PRPenLimit()) * PRNetHead() * PRCombEfficiency
  return(PREnergyProduction_o)
}
RIEnergyProduction <- function() {
  RIEnergyProduction_o <- MWhr_per_ftAcFt * min(RIOut(), RIPenLimit()) * RINetHead() * RICombEfficiency
  return(RIEnergyProduction_o)
}
RREnergyProduction <- function() {
  RREnergyProduction_o <- MWhr_per_ftAcFt * min(RROut(), RRPenLimit()) * RRNetHead() * RRCombEfficiency
  return(RREnergyProduction_o)
}
WAEnergyProduction <- function() {
  WAEnergyProduction_o <- MWhr_per_ftAcFt * min(WAOut(), WAPenLimit()) * WANetHead() * WACombEfficiency
  return(WAEnergyProduction_o)
}
WEEnergyProduction <- function() {
  WEEnergyProduction_o <- MWhr_per_ftAcFt * min(WEOut(), WEPenLimit()) * WENetHead() * WECombEfficiency
  return(WEEnergyProduction_o)
}
HungryHorseEnergy <- function() {
  HungryHorseEnergy_o <- MWhr_per_ftAcFt * min(HHRelease(), HHPenLimit()) * HHNetHead() * HHCombEfficiency
  return(HungryHorseEnergy_o)
}
KerrGroupEnergy <- function() {
  KerrGroupEnergy_o <- CBEnergyProduction() + KEEnergyProduction() + NoxEnergyProduction()
  return(KerrGroupEnergy_o)
}
CBEnergyProduction <- function() {
  CBEnergyProduction_o <- MWhr_per_ftAcFt * min(CBOut(), CBPenLimit()) * CBNetHead() * CBCombEfficiency
  return(CBEnergyProduction_o)
}
KEEnergyProduction <- function() {
  KEEnergyProduction_o <- MWhr_per_ftAcFt * min(KERelease(), KEPenLimit()) * KENetHead() * KECombEfficiency
  return(KEEnergyProduction_o)
}
NoxEnergyProduction <- function() {
  NoxEnergyProduction_o <- MWhr_per_ftAcFt * min(NOXOut(), NOXPenLimit()) * NOXNetHead() * NOXCombEfficiency
  return(NoxEnergyProduction_o)
}
LibbyEnergy <- function() {
  LibbyEnergy_o <- MWhr_per_ftAcFt * min(LBRelease(), LBPenLimit()) * LBNetHead() * LBCombEfficiency
  return(LibbyEnergy_o)
}
LowerColumbiaEnergy <- function() {
  LowerColumbiaEnergy_o <- BONEnergyProduction() + DAEnergyProduction() + JDEnergyProduction() + McNaryEnergyProduction()
  return(LowerColumbiaEnergy_o)
}
BONEnergyProduction <- function() {
  BONEnergyProduction_o <- MWhr_per_ftAcFt * min(BONOut(), BONPenLimit()) * BONNetHead() * BONCombEfficiency
  return(BONEnergyProduction_o)
}
DAEnergyProduction <- function() {
  DAEnergyProduction_o <- MWhr_per_ftAcFt * min(DAOut(), DAPenLimit()) * DANetHead() * DACombEfficiency
  return(DAEnergyProduction_o)
}
JDEnergyProduction <- function() {
  JDEnergyProduction_o <- MWhr_per_ftAcFt * min(JDOut(), JDPenLimit()) * JDNetHead() * JDCombEfficiency
  return(JDEnergyProduction_o)
}
McNaryEnergyProduction <- function() {
  McNaryEnergyProduction_o <- MWhr_per_ftAcFt * min(MCNOut(), MCNPenLimit()) * MCNetHead() * MCCombEfficiency
  return(McNaryEnergyProduction_o)
}
MicaGroupEnergy <- function() {
  MicaGroupEnergy_o <- MIEnergyProduction() + RevEnergyProduction()
  return(MicaGroupEnergy_o)
}
MIEnergyProduction <- function() {
  MIEnergyProduction_o <-  MWhr_per_ftAcFt * min(MIRelease(), MIPenLimit()) * MINetHead() * MICombEfficiency
  return(MIEnergyProduction_o)
}
RevEnergyProduction <- function() {
  RevEnergyProduction_o <- MWhr_per_ftAcFt * min(REVOut(), REVPenLimit()) * REVNetHead() * RevCombEfficiency
  return(RevEnergyProduction_o)
}
MaxSystemEnergy <- function() { # Hydropower production for entire CRB
  MaxSystemEnergy_o <- AlbeniFallsGroupEnergy() + DworshakGroupEnergy() + GrandCouleeGroupEnergy()
  + HungryHorseEnergy() + KerrGroupEnergy() + LibbyEnergy() + LowerColumbiaEnergy() + MicaGroupEnergy()
  return(MaxSystemEnergy_o)
}
