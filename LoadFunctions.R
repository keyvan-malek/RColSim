######################### Constants ##########

cfsTOafw <- 1.9834 * 7 # cfs to acre-ft per week conversion
MWhr_per_ftAcFt <- 1.02246e-3 # Convert head times flow rate (ft-acre-ft) to energy (MW-hr).

#######################################################
#-------------------- MICA DAM -----------------------#
#######################################################

MIFullPoolVol <- 20075000 # Storage at 2,475 ft of elevation
MIBotVol <- 8032200 # Storage at 2,320 ft of elevation
MIAvgMin <- 3000 # Minimum project outflow from Mica (cfs), FCOP 2003

InitMI <- function() { ## Initialize reservoir storage 
	InitMI_o <- ResInitFractionFull * MIFullPoolVol
	return(InitMI_o)
}
Mica <- function() { ### Storage volume in most upstream dam
	if (week_counter == 1) {	
		Mica_o <- InitMI()
	} else {
		Mica_o <- reservoir_vol_df$MICAA[week_counter - 1]
	}
	return(Mica_o)
}
MicaFlowData <- function() {
	return(FlowMI)
}
MIElev_ft <- function() { # Water level in dam (ft)
	upper_vol <- MI_elev_input$Volume[which(MI_elev_input$Volume >= Mica())[1]]
	lower_vol <- MI_elev_input$Volume[tail(which(MI_elev_input$Volume <= Mica())[1],1)]
	upper_el <- MI_elev_input$Elevation[which(MI_elev_input$Volume >= Mica())[1]]
	lower_el <- MI_elev_input$Elevation[tail(which(MI_elev_input$Volume <= Mica())[1],1)]
	if (is.na(lower_el)) {
		MIElev_ft_o <- min(MI_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		MIElev_ft_o <- max(MI_elev_input$Elevation)	
	} else if (lower_el == upper_el) {
		MIElev_ft_o <- lower_el
	} else {
		MIElev_ft_o <- lower_el + (Mica() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(MIElev_ft_o)
}
MINetHead <- function() { # Depth of water in the reservoir above tailwater (ft)
	MITailElev <- 1875 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621046/mica-dam/
	MINetHead_o <- MIElev_ft() - MITailElev
	return(MINetHead_o)
}
MIPenLimit <- function() { # Maximum flow rate through turbines (AF/wk)
	MIPenCap <- 38140 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621046/mica-dam/
	MIPenLimit_o <- MIPenCap * cfsTOafw
	return(MIPenLimit_o)
}
MIIn <- function() {  
	MIIn_o = MicaFlowData()
	return(MIIn_o)
}
MIInflow <- function() { # Inflow to the dam
	MIInflow_o <- MIIn()
	return(MIIn())
}

################## Mica max and min releases ####################################

MITargetMin <- function() { ## target minimum release, FCOP 2003
	MITargetMin_o <- MIAssuredRelease_input$target[week_in_year]
	return(MITargetMin_o)
}
MITargetRelease <- function() { ## target average release, FCOP 2003
	if (week_in_year %in% 1:3) {
		if (Arrow() >= 6772749) {
			#MITargetRelease_o <- (Mica() + MIInflow() - 19747728) / cfsTOafw
			MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
		} else if (Arrow() >= 4531429) {
			MITargetRelease_o <- 25000
		} else if (Arrow() >= 3202504) {
			MITargetRelease_o <- 20000
		} else {
			MITargetRelease_o <- 32000
		}
	} else if (week_in_year %in% 4:5) {
		if (Arrow() >= 5602502) {
			#MITargetRelease_o <- (Mica() + MIInflow() - 20075000) / cfsTOafw
			MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
		} else if (Arrow() >= 4095065) {
			MITargetRelease_o <- 25000
		} else {
			MITargetRelease_o <- 32000
		}
	} else if (week_in_year %in% 6:9) {
		if (Arrow() >= 7228947) {
			#MITargetRelease_o <- (Mica() + MIInflow() - 20075000) / cfsTOafw
			MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
		} else if (Arrow() >= 6971096) {
			MITargetRelease_o <- 24000
		} else if (Arrow() >= 5781014) {
			MITargetRelease_o <- 27000
		} else {
			MITargetRelease_o <- 32000
		}
	} else if (week_in_year %in% 10:14) {
		if (Arrow() >= 7050435) {
			#MITargetRelease_o <- (Mica() + MIInflow() - 19875066) / cfsTOafw
			MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
		} else if (Arrow() >= 5384321) {
			MITargetRelease_o <- 19000
		} else if (Arrow() >= 4194239) {
			MITargetRelease_o <- 22000
		} else {
			MITargetRelease_o <- 32000
		}
	} else if (week_in_year %in% 15:18) {
		if (Arrow() >= 6852088) {
			MITargetRelease_o <- 21000
		} else if (Arrow() >= 6435559) {
			MITargetRelease_o <- 19000
		} else if (Arrow() >= 1060357) {
			MITargetRelease_o <- 25000
		} else {
			MITargetRelease_o <- 32000
		}		
	} else if (week_in_year %in% 19:22) {
		if (Arrow() >= 5662006) {
			MITargetRelease_o <- 25000
		} else if (Arrow() >= 3797545) {
			MITargetRelease_o <- 22000
		} else if (Arrow() >= 822341) {
			MITargetRelease_o <- 27000
		} else {
			MITargetRelease_o <- 32000
		}	
	} else if (week_in_year %in% 23:27) {
		if (Arrow() >= 5463659) {
			MITargetRelease_o <- 24000
		} else if (Arrow() >= 4551263) {
			MITargetRelease_o <- 27000
		} else if (Arrow() >= 2904984) {
			MITargetRelease_o <- 25000
		} else {
			MITargetRelease_o <- 29000
		}
	} else if (week_in_year %in% 28:31) {
		if (Arrow() >= 2944653) {
			MITargetRelease_o <- 21000
		} else if (Arrow() >= 2012422) {
			MITargetRelease_o <- 26000
		} else if (Arrow() >= 1219035) {
			MITargetRelease_o <- 21000
		} else {
			MITargetRelease_o <- 26000
		}
	} else if (week_in_year %in% 32:35) {
		if (Arrow() >= 1814076) {
			MITargetRelease_o <- 17000
		} else if (Arrow() >= 1754571) {
			MITargetRelease_o <- 26000
		} else if (Arrow() >= 1238869) {
			MITargetRelease_o <- 22000
		} else {
			MITargetRelease_o <- 25000
		}
	} else if (week_in_year %in% 36:37) {
		if (Arrow() >= 1992588) {
			MITargetRelease_o <- 20000
		} else if (Arrow() >= 921514) {
			MITargetRelease_o <- 10000
		} else if (Arrow() >= 663663) {
			MITargetRelease_o <- 12000
		} else {
			MITargetRelease_o <- 22000
		}
	} else if (week_in_year %in% 38:39) {
		if (Arrow() >= 1357878) {
			MITargetRelease_o <- 10000
		} else if (Arrow() >= 445482) {
			MITargetRelease_o <- 15000
		} else if (Arrow() >= 266969) {
			MITargetRelease_o <- 10000
		} else {
			MITargetRelease_o <- 15000
		}
	} else if (week_in_year %in% 40:44) {
		if (Arrow() >= 1496720) {
			MITargetRelease_o <- 8000
		} else if (Arrow() >= 1258704) {
			MITargetRelease_o <- 12000
		} else if (Arrow() >= 663663) {
			MITargetRelease_o <- 8000
		} else {
			MITargetRelease_o <- 10000
		}		
	} else if (week_in_year %in% 45:48) {
		if (Arrow() >= 3420686) {
			MITargetRelease_o <- 8000
		} else if (Arrow() >= 2250439) {
			MITargetRelease_o <- 10000
		} else if (Arrow() >= 1833910) {
			MITargetRelease_o <- 14000
		} else {
			MITargetRelease_o <- 18000
		}
	} else if (week_in_year %in% 49:52) {
		if (Arrow() >= 6534733) {
			#MITargetRelease_o <- (Mica() + MIInflow() - 19952025) / cfsTOafw
			MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
		} else if (Arrow() >= 5523163) {
			#MITargetRelease_o <- (Mica() + MIInflow() - 19829050) / cfsTOafw
			MITargetRelease_o <- (Mica() + MIInflow() - MI_ORC()) / cfsTOafw
		} else if (Arrow() >= 2528125) {
			MITargetRelease_o <- 14000
		} else {
			MITargetRelease_o <- 18000
		}
	}
	return(MITargetRelease_o)
}
		
		
MIMinRefill <- function() { ## Minimum storage that will allow dam to refill and meet minimum project outflow criteria
	if (GuranteeRefillSw() == 1) {
		if (week_in_year < 23) {
			MIMinRefill_o <- max(MIAssuredRefill(), MICriticalCurveMin())
		} else {
			MIMinRefill_o <- MIMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2){
		MIMinRefill_o <- MIBotVol
	}
	return(MIMinRefill_o)
}
MIMinReq <- function() {
	MIMinReq_1 <- min(max(Mica() + MIIn() - max(MIMinRefill(), MICriticalCurveMin()), MIAvgMin * cfsTOafw), MITargetMin() * cfsTOafw)
	MIMinReq_2 <- min(max(Mica() + MIIn() - max(MIMinRefill(), MICriticalCurve()), MIAvgMin * cfsTOafw), MITargetMin() * cfsTOafw)
	MIMinReq_o <- max(MIMinReq_1, MIMinReq_2)
	return(MIMinReq_o)
}
MIDamProtectRel <- function() { # The release required to keep the dam from filling above the full pool volume.  Units acre-ft.
	MIDamProt <- max(0, Mica() + MIInflow() - MIFullPoolVol)
	return(MIDamProt)
}
MIRelLimit <- function() { # Max allowable release
	MIRelLimit_o <- max(Mica() + MIInflow() - MIBotVol, 0)
	return(MIRelLimit_o)
}
MIAvailWater <- function() { # Active storage 
	MIAvailWater_o <- max(0, Mica() + MIIn() - MIBotVol)
	return(MIAvailWater_o)
}

################### Mica rule curves #########################

# Mica Flood Control Curves are modified according to January 1995 SRDs which are showed in USCOE Website
# (http://www.nwd-wc.usace.army.mil/cafe/forecast/SRD/MCDBChart8.pdf)
# Flood storage curve selected is based on the April -- August runoff forecast at The Dalles during the flood control evacuation period (Oct. -- Mar). Units acre-ft.
# When The Dalles forecast is > 111 MAF, on-call storage is requested up to 8 MAF. This is in addition to the 4 MAF of treaty storage at Mica
# Flood curve values are given in terms of the total volume of water held in the reservoir.

# Mica Dam flood evacuation period is 1 October to 30 March. Refill period is 1 April to 31 July.
MIFloodCurve <- function() {
	if (DARunoffAprAug >= 111E6 && MIRunoffAprAug > 19.3E6) {
		MI_CurFC_o <- MIFlood_input$MIFlood12[week_in_year]
	} else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 18E6 || MIRunoffMayAug > 17.5E6)) {
		MI_CurFC_o <- MIFlood_input$MIFlood11[week_in_year] - (MIFlood_input$MIFlood11[week_in_year] - MIFlood_input$MIFlood12[week_in_year]) / (19.3E6 - 18E6) * (MIRunoffAprAug - 18E6)
	} else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 16E6 || MIRunoffMayAug > 15.50E6)) {
		MI_CurFC_o <- MIFlood_input$MIFlood10[week_in_year] - (MIFlood_input$MIFlood10[week_in_year] - MIFlood_input$MIFlood11[week_in_year]) / (18E6 - 16E6) * (MIRunoffAprAug - 16E6)
	} else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 14E6 || MIRunoffMayAug > 13.50E6)) {
		MI_CurFC_o <- MIFlood_input$MIFlood9[week_in_year] - (MIFlood_input$MIFlood9[week_in_year] - MIFlood_input$MIFlood10[week_in_year]) / (16E6 - 14E6) * (MIRunoffAprAug - 14E6) 
	} else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 12E6 || MIRunoffMayAug > 11.50E6)) {
		MI_CurFC_o <- MIFlood_input$MIFlood8[week_in_year] - (MIFlood_input$MIFlood8[week_in_year] - MIFlood_input$MIFlood9[week_in_year]) / (14E6 - 12E6) * (MIRunoffAprAug - 12E6) 
	} else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 10E6 || MIRunoffMayAug > 9.5E6)) {
		MI_CurFC_o <- MIFlood_input$MIFlood7[week_in_year] - (MIFlood_input$MIFlood7[week_in_year] - MIFlood_input$MIFlood8[week_in_year]) / (12E6 - 10E6) * (MIRunoffAprAug - 10E6)
	} else if (DARunoffAprAug >= 111E6 && (MIRunoffAprAug > 8E6 || MIRunoffMayAug > 7.5E6)) {
		MI_CurFC_o <- MIFlood_input$MIFlood6[week_in_year] - (MIFlood_input$MIFlood6[week_in_year] - MIFlood_input$MIFlood7[week_in_year]) / (10E6 - 8E6) * (MIRunoffAprAug - 8E6)
	} else if (DARunoffAprAug >= 80.0E6) {
		MI_CurFC_o <- MIFlood_input$MIFlood5[week_in_year]
	} else if (DARunoffAprAug >= 75.0E6) {
		MI_CurFC_o <- MIFlood_input$MIFlood4[week_in_year] - (MIFlood_input$MIFlood4[week_in_year] - MIFlood_input$MIFlood5[week_in_year]) / (80E6 - 75E6) * (DARunoffAprAug - 75E6)
	} else if (DARunoffAprAug >= 70.0E6) {
		MI_CurFC_o <- MIFlood_input$MIFlood3[week_in_year] - (MIFlood_input$MIFlood3[week_in_year] - MIFlood_input$MIFlood4[week_in_year]) / (75E6 - 70E6) * (DARunoffAprAug - 70E6)
	} else if (DARunoffAprAug >= 65.0E6) {
		MI_CurFC_o <- MIFlood_input$MIFlood2[week_in_year] - (MIFlood_input$MIFlood2[week_in_year] - MIFlood_input$MIFlood3[week_in_year]) / (70E6 - 65E6) * (DARunoffAprAug - 65E6)
	} else if (DARunoffAprAug >= 62.0E6) {
		MI_CurFC_o <- MIFlood_input$MIFlood1[week_in_year] - (MIFlood_input$MIFlood1[week_in_year] - MIFlood_input$MIFlood2[week_in_year]) / (65E6 - 62E6) * (DARunoffAprAug - 62E6)
	} else {
		MI_CurFC_o <- MIFlood_input$MIFlood1[week_in_year]
	}
	return(MI_CurFC_o)
}
MITopVol <- function() {  ## Upper rule curve
	if (TopRuleSw() == 0) { # Default = 0
		MITopVol_o <- MIFloodCurve()
	} else if (TopRuleSw() == 1) {
		MITopVol_o <- MIFullPoolVol
	} else if (TopRuleSw() == 2) {
		MITopVol_o <- MIFlood_input$MIFlood1[week_in_year]
	} else if (TopRuleSw() == 3) {
		if (is.na(start_refill_wk)) {
			if (week_in_year < 35) {
				MITopVol_o <- MIFloodCurve()
			} else {
				MITopVol_o <- MIFullPoolVol
			}
		} else if (week_in_year < start_refill_wk) {
			MITopVol_o <- MIFloodCurve()
		} else {
			MITopVol_o <- MIFullPoolVol
		}
	}
	return(MITopVol_o)
}
MIRuleReq <- function() { ## Release required to meet upper rule curve (flood curve)
	MIRuleReq_1 <- max(Mica() + MIIn() - MITopVol(), 0)
	return(MIRuleReq_1)
}
MIPrelim <- function() { # Preliminary release based on required minimum release
	MIpre <- min(MIAvailWater(), max(MIRuleReq(), MIMinReq()))
	MIPrelim_c <<- MIpre
	return(MIpre)
}
MILowerLimit <- function() { ## Minimum storage during the period January -- April
	MILL_o <- lower_limit_input$Mica[week_in_year]
	return(MILL_o)
}
MICriticalCurve <- function() { ## Minimum storage to meet firm hydropower in a dry year (1928-1932 critical water period)
	MICriticalCurve_o <- MICriticalCurve_input$CRC1[week_in_year] # year 1 critical curve
	return(MICriticalCurve_o)
}
## Draft limit in years when the reservoir needs to be drafted below the critical curve to meet the firm hydropower load.
## This is discussed in "Principles and Procedures", pg. 21.
MICriticalCurveMin <- function() {
	if (MICriticalCurveSw == 1) {
		MICriticalCurveMin_o <- MICriticalCurve_input$CRC1[week_in_year]
	} else if (MICriticalCurveSw == 2) {
		MICriticalCurveMin_o <- MICriticalCurve_input$CRC2[week_in_year] # year 2 critical curve
	} else if (MICriticalCurveSw == 3) {
		MICriticalCurveMin_o <- MICriticalCurve_input$CRC3[week_in_year] # year 3 critical curve
	} else if (MICriticalCurveSw == 4) {
		MICriticalCurveMin_o <- MICriticalCurve_input$CRC4[week_in_year] # year 4 critical curve
	}
	return(MICriticalCurveMin_o)
}
MIAssuredRefill <- function() { # Assured refill curve based on 1931 historical inflows
	MIAssuredRefill_o <- MIAssuredRefill_input[week_in_year,2]
	return(MIAssuredRefill_o)
}
###### Variable refill
# Refill rule curve based on Assured inflows and forecasts. The variable refill curve August -- December is equal
# to the assured refill curve. From January through July, the refill curve depends on inflow forecasts.
MIVariableRefill <- function() { # Required refill to ensure dam is full by end of year
	if (RefillSwitch() == 1) { ## Default = 2
		MIRefillCurve_o <- MIAssuredRefill()
	} else if (RefillSwitch() == 2) {
		MIRefillCurve_o <- MIVariableRefillCurve
	}
	return(MIRefillCurve_o)
}

##### Operating Rule Curve
# During the fixed period from August-December before the forecast, the BECC (Base Energy Content Curve) is the greater of the critical curve and the refill curve based on 1931 inflows (assured refill curve).
# During the variable period from January--July, the VECC (Variable Energy Content Curve) can be lower, but not higher than the BECC (Base Energy Content Curve).
# This value is compared to the flood storage curve, and operating rule curve lower limit to calculate the operating rule curve.
# "Principles and Procedures", pg. 19
MI_ORC <- function() {
	MI_ORC_o <- min(max(min(max(MIAssuredRefill(), MICriticalCurve()), MIVariableRefill()), MILowerLimit(), MIMinRefill()), MIFloodCurve())
	return(MI_ORC_o)
}

##################### Mica fish flow ########################################################

MIMcNaryDraftLimit <- function() { # Minumum allowable storage volume after accounting for release to meet McNary fish flow target
	if (FishOverRefillSw() == 1) { # Default is 0 (don't sacrifice refill to meet fish flow target)
		MIMcNaryDraftLimit_o <- if (UseAllStorForMCNLG == 1) { 
			MIMcNaryDraftLimit_o <- MIBotVol
		} else if (Fish_Pool_Alternative == 1) {
			MIMcNaryDraftLimit_o <- MIFullPoolVol
		} else if (Fish_Pool_Alternative == 2) {
			MIMcNaryDraftLimit_o <- MIFullPoolVol - 0.402E6
		} else if (Fish_Pool_Alternative == 3) {
			MIMcNaryDraftLimit_o <- MIFullPoolVol - 0.804E6
		} else if (Fish_Pool_Alternative == 4) {
			MIMcNaryDraftLimit_o <- MIFullPoolVol - 1.205E6
		} else {
			MIMcNaryDraftLimit_o <- MIFullPoolVol
		}
	} else if (FishOverRefillSw() == 0) {
		MIMcNaryDraftLimit_o <- MI_ORC()
	}
	return(MIMcNaryDraftLimit_o)
}
MIMcNarySharedWater <- function() { # Volume of water that can be drafted below flood curve elevation to meet McNary flow target
	MIMcNarySharedWater_o <- max(0, Mica() + MIIn() - MIPrelim() - MIMcNaryDraftLimit())
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

############################### Mica Energy #######################

MIPreEnergy <- function() { # Preliminary energy production (MW-hr), considering only water released for flood protection and to meet minimum flow requirement
	MIPreEnergy_o <- MWhr_per_ftAcFt * min(MIPrelim(), MIPenLimit()) * MINetHead() * MICombEfficiency
	return(MIPreEnergy_o)
}
MISharedWater <- function() { # Maximum storage (above that already allocated for fish flow and flood protection) available to meet firm energy load
	MISharedWater_o <- max(Mica() + MIIn() - MIPrelim() - MIMcNarySup() - max(MIMinRefill(), MICriticalCurveMin()), 0) 
	return(MISharedWater_o)
}
MIDownStreamHead <- function() { # Combined hydraulic head of reservoirs downstream of Mica
	MIDownStreamHead_o <- REVNetHead() + ARNetHead() + TotalGCHead()
	return(MIDownStreamHead_o)
}
MIEnergyContent <- function() { # Hydropower that could be generated by releasing all of MISharedWater
	MIEnergyContent_o <- MISharedWater() * (MINetHead() + MIDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIEnergyContent_o)
}
MI_ORCSharedWater <- function() { # After meeting flood control and fish flow objectives, the additional water that can be released to generate primary (firm) or secondary (non firm) energy.
	MI_ORCSharedWater_o <- max(0, Mica() + MIIn() - MIMcNarySup() - MIPrelim() - MI_ORC())
	return(MI_ORCSharedWater_o)
}
MI_ORCEnergyContent <- function() { # Hydropower that could be generated by releasing all of MI_ORCSharedWater
	MI_ORCEnergyContent_o <- MI_ORCSharedWater() * (MINetHead() + REVNetHead() + ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MI_ORCEnergyContent_o)
}
MIFirmEngSup <- function() { # Power generation from Mica required to meet firm energy target (MW-hr)
	if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
		if (TotalEnergyContent_c == 0) {
			MIFirmEngSup_o <- 0
		} else {
			MIFirmEngSup_o <- (MIEnergyContent() + MI_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c			
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		MIFirmEngSup_o <- 0
	} else {
		MIFirmEngSup_o <- MI_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(MIFirmEngSup_o)
}
MINFEnergyContent <- function() { # Energy content of water stored in Mica that could be used for generating non-firm hydropower (MW-hr)
	MINFEnergyContent_o <- max(0, MI_ORCEnergyContent() - MIFirmEngSup())
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
MIFirmEngSupReq <- function() { # Required release from Mica to meet firm energy target (AF/wk)
	MIFirmEngSupReq_o <- min(MIPenLimit(), MIFirmEngSup() / (MWhr_per_ftAcFt * (MINetHead() + REVNetHead() + TotalGCHead()) * MICombEfficiency))
	return(MIFirmEngSupReq_o)
}
MINonFirmSupReq <- function() { # Required release from Mica to meet non-firm energy target (AF/wk)
	if (NonFirmEnergySw() == 1) { # Default = 1
		MINonFirmSupReq_o <- min(MIPenLimit(), (MIFirmEngSup() + MINonFirmEngSup()) / (MWhr_per_ftAcFt * (MINetHead() + REVNetHead() + TotalGCHead()) * MICombEfficiency))
	} else {
		MINonFirmSupReq_o <- 0
	}
	return(MINonFirmSupReq_o)
}
MIEnergySup <- function() { # Water released to satisfy firm and non-firm power requirements (AF/wk)
	if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
		MIEnergySup_o <- max(min(MIFirmEngSupReq(), MISharedWater()), min(MINonFirmSupReq(), MI_ORCSharedWater()))
	} else {
		MIEnergySup_o <- max(min(MIFirmEngSupReq(), MI_ORCSharedWater()), min(MINonFirmSupReq(), MI_ORCSharedWater()))
	}
	return(MIEnergySup_o)
}
# Total water to be released to meet fish and energy objectives
# When this function is called, it also calculates common variables that are used by many functions.
# This greatly increases computational efficiency.
MICombSup <- function() { 
	if (BRIn_c == -9999) {
		BRIn_c <<- BRIn()
		water_df$BRIn[week_counter] <<- BRIn_c
	}
	if (BRPrelim_c == -9999) {
		BRPrelim_c <<- BRPrelim()
	}
	if (GCIn_c == -9999) {
		GCIn_c <<- GCIn()
		water_df$GCIn[week_counter] <<- GCIn_c
	}
	if (TotalEnergyContent_c == -9999) {
		TotalEnergyContent_c <<- TotalEnergyContent()
		energy_df$TotalEnergyContent[week_counter] <<- TotalEnergyContent_c
	}
	if (TotalMcNarySharedWater_c == -9999) {
		TotalMcNarySharedWater_c <<- TotalMcNarySharedWater()
		water_df$TotalMcNarySharedWater[week_counter] <<- TotalMcNarySharedWater_c
	}
	if (TotalBONSharedWater_c == -9999) {
		TotalBONSharedWater_c <<- TotalBONSharedWater()
		water_df$TotalBONSharedWater[week_counter] <<- TotalBONSharedWater_c
	}
	if (Total_ORCEnergyContent_c == -9999) {
		Total_ORCEnergyContent_c <<- Total_ORCEnergyContent()
		energy_df$Total_ORCEnergyContent[week_counter] <<- Total_ORCEnergyContent_c
	}
	if (TotalCoordPreEnergy_c == -9999) {
		TotalCoordPreEnergy_c <<- TotalCoordPreEnergy()
		energy_df$TotalCoordPreEnergy[week_counter] <<- TotalCoordPreEnergy_c   
	}
	if (FirmEnergyDeficit_c == -9999) {
		FirmEnergyDeficit_c <<- FirmEnergyDeficit()
		energy_df$FirmEnergyDeficit[week_counter] <<- FirmEnergyDeficit_c
	}
	if (NonFirmEnergyDeficit_c == -9999) {
		NonFirmEnergyDeficit_c <<- NonFirmEnergyDeficit()
		energy_df$NonFirmEnergyDeficit[week_counter] <<- NonFirmEnergyDeficit_c
	}
	if (TotalNFEnergyContent_c == -9999) {
		TotalNFEnergyContent_c <<- TotalNFEnergyContent()
		energy_df$TotalNFEnergyContent[week_counter] <<- TotalNFEnergyContent_c
	}
	MICombSup_o <- MIEnergySup() + MIMcNarySup()
	MICombSup_c <<- MICombSup_o
	return(MICombSup_o)
}
MIMcNarySupEnergy <- function() { # Hydropower generated by releasing water to meet McNary fish target (MW-hr)
	MIMcNarySupEnergy_o <- MIMcNarySup() * (MINetHead() + TotalGCHead() + REVNetHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(MIMcNarySupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ############################### 

MIFloodSpace <- function() { ## Available flood space in Mica dam 
	MIFloodSpace_o <- min(MIPrelim() + MICombSup(), max(0, MIFullPoolVol - (Mica() + MIIn() - MIPrelim() - MICombSup())))
	MIFloodSpace_c <<- MIFloodSpace_o
	return(MIFloodSpace_o)
}
MIFloodFrac <- function() { ## Flood space in Mica as a fraction of the total flood space in Mica, Arrow, Libby, Hungry Horse, and Grand Coulee reservoirs
	if (TotalFloodSpace_c == -9999) {
		TotalFloodSpace_c <<- TotalFloodSpace()
		water_df$TotalFloodSpace[week_counter] <<- TotalFloodSpace_c
	}
	if (TotalFloodSpace_c == 0) {
		MIFloodFrac_o <- 0
	} else {
		MIFloodFrac_o <- MIFloodSpace_c / TotalFloodSpace_c
	}
	return(MIFloodFrac_o)
}
MIRelReducReq <- function() { ## Reduction in project outflow to meet controlled flow at The Dalles
	MIRelReducReq_o <- TotalRelReducReq() * MIFloodFrac()
	MIRelReducReq_c <<- MIRelReducReq_o
	return(MIRelReducReq_o)
}
MIDamProtectExcess <- function() { ## release of water to prevent the dam from overflowing, in excess of that required to meet all other objectives.
	MIDamProtectExcess_o <- max(0, MIDamProtectRel() - MIPrelim() - MICombSup_c)
	return(MIDamProtectExcess_o)
}

################ Mica final release #######################################################

MIRelease <- function() { # Water released from Mica after accounting for all objectives
	MIRelease_o <- max(min(MIPrelim() + MICombSup() - MIRelReducReq(), MIRelLimit()), MIDamProtectRel(), MIAvgMin * cfsTOafw)
	return(MIRelease_o)
}
MIOutflow <- function() { # Same as MIRelease, but called when calculating inflow to downstream dams
	MIOutflow_o <- MIRelease_c
	return(MIOutflow_o)
}

#######################################################
#---------------- REVELSTOKE DAM ---------------------#
#######################################################

RevFlowData <- function() {
	return(FlowREV)
}
REVInc <- function() {
	REVInc_o <- RevFlowData() - MicaFlowData()
	return(REVInc_o)
}
REVPrelim <- function() {
	REVPrelim_o <- MIPrelim_c + REVInc()
	return(REVPrelim_o)
}
REVPenLimit <- function() {
	REVPenCap <- 90000 # (Walker-Larsen, 2017) 
	REVPenLimit_o <- REVPenCap * cfsTOafw
	return(REVPenLimit_o)
}
REVNetHead <- function() { 
	REVNetHead_o <- 436 # R2 Resource Consultants, Inc. 
	return(REVNetHead_o)
}
REVPreEnergy <- function() {
	REVPreEnergy_o <- min(REVPrelim(), REVPenLimit()) * REVNetHead() * RevCombEfficiency * MWhr_per_ftAcFt
	return(REVPreEnergy_o)
}
REVIn <- function() {
	REVIn_o <- MIOutflow() + REVInc()
	return(REVIn_o)
}
REVOut <- function() {
	REVOut_o <- REVIn()
	return(REVOut_o)
}

#######################################################
#------------- ARROW DAM (KEENLYSIDE) ----------------#
#######################################################

ARFullPoolVol <- 7327300 # Volume corresponding to 1444 ft of elevation.  This is normal full pool.  Units acre-ft.
ARBotVol <- 227300 # Volume in AF, corresponding to 1377.9 ft elevation
ARAvgMin <- 5000 # FCOP 2003

InitAR <- function() {
	InitAR_o <- ResInitFractionFull * ARFullPoolVol
	return(InitAR_o)
}
Arrow <- function() {
	if (week_counter == 1) {
		Arrow_o <- InitAR()
	} else {
		Arrow_o <- reservoir_vol_df$ARROW[week_counter - 1]
	}
	return(Arrow_o)
}
ArrowFlowData <- function() {
	return(FlowAR)
}
ARInc <- function() {
	ARInc_o <- ArrowFlowData() - RevFlowData()
	return(ARInc_o)
}
ARElev_ft <- function() { 
	upper_vol <- AR_elev_input$Volume[which(AR_elev_input$Volume >= Arrow())[1]]
	lower_vol <- AR_elev_input$Volume[tail(which(AR_elev_input$Volume <= Arrow())[1],1)]
	upper_el <- AR_elev_input$Elevation[which(AR_elev_input$Volume >= Arrow())[1]]
	lower_el <- AR_elev_input$Elevation[tail(which(AR_elev_input$Volume <= Arrow())[1],1)]
	if (is.na(lower_el)) {
		ARElev_ft_o <- min(AR_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		ARElev_ft_o <- max(AR_elev_input$Elevation)
	} else if (lower_el == upper_el) {
		ARElev_ft_o <- lower_el
	} else {
		ARElev_ft_o <- lower_el + (Arrow() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(ARElev_ft_o)
}
ARNetHead <- function() { # Depth of water in the reservoir above tailwater (ft)
	ARTailElev <- 1367 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621041/keenlyside-dam/
	ARNetHead_o <- ARElev_ft() - ARTailElev
	return(ARNetHead_o)
}
ARPenLimit <- function() { # Maximum flow rate through turbines (AF/wk)
	ARPenCap <- 42400 # https://www.nwd.usace.army.mil/Media/Fact-Sheets/Fact-Sheet-Article-View/Article/621041/keenlyside-dam/
	ARPenLimit_o <- ARPenCap * cfsTOafw
	return(ARPenLimit_o)
}
ARIn <- function() {
	ARIn_o <- REVPrelim() + ARInc()
	return(ARIn_o)
}
ARPreInflow <- function() { ## Inflow to Arrow before accounting for controlled flow at The Dalles
	ARPreInflow_o <- max(min(MIPrelim() + MICombSup_c, MIRelLimit()), MIDamProtectRel()) + (ArrowFlowData() - MicaFlowData())
	ARPreInflow_c <<- ARPreInflow_o
	return(ARPreInflow_o)
}
ARInflow <- function() {
	ARInflow_o <- REVOut() + ARInc()
	return(ARInflow_o)
}

################## Arrow max and min releases #################################### 

ARTargetMin <- function() {
	ARTargetMin_o <- ARAssuredRelease_input$target[week_in_year]
	return(ARTargetMin_o)
}
ARMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {
		if (week_in_year < 23) {
			ARMinRefill_o <- max(ARAssuredRefill(), ARCriticalCurveMin())
		} else {
			ARMinRefill_o <- ARMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		ARMinRefill_o <- ARBotVol
	}
	return(ARMinRefill_o)
}
ARMinReq <- function() {
	ARMinReq_o <- min(max(Arrow() + ARIn() - max(ARMinRefill(), ARCriticalCurveMin()), ARAvgMin * cfsTOafw), ARTargetMin() * cfsTOafw)
	return(ARMinReq_o)
}
ARDamProtectRel <- function() {
	ARDamProtectRel_o <- max(0, Arrow() + ARPreInflow() - ARFullPoolVol)
	return(ARDamProtectRel_o)
}
ARRelLimit <- function() {
	ARRelLimit_o <- max(Arrow() + ARPreInflow_c - ARBotVol, 0)
	return(ARRelLimit_o)
}
ARAvailWater <- function() {
	ARAvailWater_o <- max(Arrow() + ARIn() - ARBotVol, 0)
	return(ARAvailWater_o)
}

################## Arrow rule curves ####################################

# Arrow Flood Control Curves are modified according to January 1995 SRDs which are showed in 
# 2003 Columbia River Treaty Flood Control Operating Plan
# Flood evacuation is 1 October to 30 March. Refill is from 1 April to 31 July.
# Arrow has 3.6 MAF of treaty storage plus an additional 4.5 MAF of on-call storage
ARFloodCurve <- function() {
	if (DARunoffAprAug >= 111E6) { ## In case of on-call storage
		AR_CurFC_o <- ARFlood_input$ARFlood6[week_in_year]
	} else if (DARunoffAprAug >= 80E6) {
		AR_CurFC_o <- ARFlood_input$ARFlood5[week_in_year]
	} else if (DARunoffAprAug >= 75E6) {
		AR_CurFC_o <- ARFlood_input$ARFlood4[week_in_year] - (ARFlood_input$ARFlood4[week_in_year] - ARFlood_input$ARFlood5[week_in_year]) / (80E6 - 75E6) * (DARunoffAprAug - 75E6)
	} else if (DARunoffAprAug >= 70E6) {
		AR_CurFC_o <- ARFlood_input$ARFlood3[week_in_year] - (ARFlood_input$ARFlood3[week_in_year] - ARFlood_input$ARFlood4[week_in_year]) / (75E6 - 70E6) * (DARunoffAprAug - 70E6)
	} else if (DARunoffAprAug >= 65E6) {
		AR_CurFC_o <- ARFlood_input$ARFlood2[week_in_year] - (ARFlood_input$ARFlood2[week_in_year] - ARFlood_input$ARFlood3[week_in_year]) / (70E6 - 65E6) * (DARunoffAprAug - 65E6)
	} else if (DARunoffAprAug >= 64E6) {
		AR_CurFC_o <- ARFlood_input$ARFlood1[week_in_year] - (ARFlood_input$ARFlood1[week_in_year] - ARFlood_input$ARFlood2[week_in_year]) / (65E6 - 64E6) * (DARunoffAprAug - 64E6)
	} else {
		AR_CurFC_o <- ARFlood_input$ARFlood1[week_in_year]
	}
	return(AR_CurFC_o)
}
ARTopVol <- function() {
	if (TopRuleSw() == 0) {
		ARTopVol_o <- ARFloodCurve()
	} else if (TopRuleSw() == 1) {
		ARTopVol_o <- ARFullPoolVol
	} else if (TopRuleSw() == 2) {
		ARTopVol_o <- ARFlood_input$ARFlood1[week_in_year]
	} else if (TopRuleSw() == 3) {
		if (is.na(start_refill_wk)) {
			if (week_in_year < 35) {
				ARTopVol_o <- ARFloodCurve()
			} else {
				ARTopVol_o <- ARFullPoolVol
			}
		} else if (week_in_year < start_refill_wk) {
			ARTopVol_o <- ARFloodCurve()
		} else {
			ARTopVol_o <- ARFullPoolVol()
		}
	}
	return(ARTopVol_o)
}
ARRuleReq <- function() {
	ARRuleReq_o <- max(Arrow() + ARIn() - ARTopVol(), 0)
	return(ARRuleReq_o)
}
ARPrelim <- function() {
	ARPrelim_o <- min(ARAvailWater(), max(ARRuleReq(), ARMinReq()))
	return(ARPrelim_o)
}
ARLowerLimit <- function() {
	ARLL_o <- lower_limit_input$Arrow[week_in_year]
	return(ARLL_o)
}
ARCriticalCurve <- function() { 
	ARCriticalCurve_o <- ARCriticalCurve_input$CRC1[week_in_year]
	return(ARCriticalCurve_o)
}
ARCriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		ARCriticalCurveMin_o <- ARCriticalCurve_input$CRC4[week_in_year]
	}
	return(ARCriticalCurveMin_o)
}
ARAssuredRefill <- function() { 
	ARAssuredRefill_o <- ARAssuredRefill_input[week_in_year,2]
	return(ARAssuredRefill_o)
}
ARVariableRefill <- function() {
	if (RefillSwitch() == 1) {
		ARRefillCurve_o <- ARAssuredRefill()
	} else if (RefillSwitch() == 2) {
		if (week_in_year < 28) {
			ARRefillCurve_o <- ARFullPoolVol
		} else {
			ARRefillCurve_o <- ARVariableRefillCurve
		}
	}
	return(ARRefillCurve_o)
}
AR_ORC <- function() {
	AR_ORC_o <- min(max(min(max(ARAssuredRefill(), ARCriticalCurve()), ARVariableRefill()), ARLowerLimit()), ARFloodCurve())
	return(AR_ORC_o)
}

################## Arrow fish flows ###################

ARMcNaryDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
		if (UseAllStorForMCNLG == 1) {
			ARMcNaryDraftLimit_o <- ARBotVol
		} else if (Fish_Pool_Alternative == 1) {
			ARMcNaryDraftLimit_o <- ARFullPoolVol
		} else if (Fish_Pool_Alternative == 2) {
			ARMcNaryDraftLimit_o <- ARFullPoolVol
		} else if (Fish_Pool_Alternative == 3) {
			ARMcNaryDraftLimit_o <- ARFullPoolVol - 0.386E6
		} else if (Fish_Pool_Alternative == 4) {
			ARMcNaryDraftLimit_o <- ARFullPoolVol - 0.773E6
		} else if (Fish_Pool_Alternative == 5) {
			ARMcNaryDraftLimit_o <- ARFullPoolVol - 1.159E6
		} else {
			ARMcNaryDraftLimit_o <- ARFullPoolVol
		}
	} else if (FishOverRefillSw() == 0) {
		ARMcNaryDraftLimit_o <- AR_ORC()
	}
	return(ARMcNaryDraftLimit_o)
}
ARBONDraftLimit <- function() { ## Draft limit to meet Bonneville chum target
	if (FishOverRefillSw() == 1) {
		ARBONDraftLimit_o <- ARBotVol
	} else if (FishOverRefillSw() == 0) {
		if (week_in_year < 14) {
			ARBONDraftLimit_o <- ARFullPoolVol
		} else if (week_in_year < 19) {
			ARBONDraftLimit_o <- 6E6
		} else if (week_in_year < 23) {
			ARBONDraftLimit_o <- 4E6
		} else {
			ARBONDraftLimit_o <- AR_ORC()
		}
	}
	return(ARBONDraftLimit_o)
}
ARMcNarySharedWater <- function() {
	ARMcNarySharedWater_o <- max(0, Arrow() + ARIn() - ARPrelim() - ARMcNaryDraftLimit())
	return(ARMcNarySharedWater_o)
}
ARMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		ARMcNarySup_o <- 0
	} else {
		ARMcNarySup_o <- min(ARMcNarySharedWater(), McNaryFlowDeficit() * ARMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(ARMcNarySup_o)
}
ARBONSharedWater <- function() {
	ARBONSharedWater_o <- max(0, Arrow() + ARIn() - ARPrelim() - ARBONDraftLimit())
	return(ARBONSharedWater_o)
}
ARBONSup <- function() {
	if (TotalBONSharedWater_c == 0) {
		ARBONSup_o <- 0
	} else {
		ARBONSup_o <- min(ARBONSharedWater(), BONFlowDeficit() * ARBONSharedWater() / TotalBONSharedWater_c)
	} 
	return(ARBONSup_o)
}
ARFishSup <- function() { # Additional release (above preliminary release) for meeting fish flow targets
	ARFishSup_o <- max(ARBONSup(), ARMcNarySup())
	return(ARFishSup_o)
}

####################### Arrow energy ##############################################

ARPreEnergy <- function() {
	ARPreEnergy_o <- min(ARPrelim(), ARPenLimit()) * ARNetHead() * ARCombEfficiency * MWhr_per_ftAcFt
	return(ARPreEnergy_o)
}
ARSharedWater <- function() {
	ARSharedWater_o <- max(Arrow() + ARIn() - ARPrelim() - ARFishSup() - max(ARMinRefill(), ARCriticalCurveMin()), 0)
	return(ARSharedWater_o)
}
AREnergyContent <- function() {
	AREnergyContent_o <- ARSharedWater() * (ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(AREnergyContent_o)
}
AR_ORCSharedWater <- function() {
	AR_ORCSharedWater_o <- max(0, Arrow() + ARIn() - ARFishSup() - ARPrelim() - AR_ORC())
	return(AR_ORCSharedWater_o)
}
AR_ORCEnergyContent <- function() {
	AR_ORCEnergyContent_o <- AR_ORCSharedWater() * (ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(AR_ORCEnergyContent_o)
}
ARFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			ARFirmEngSup_o <- 0
		} else {
			ARFirmEngSup_o <- (AREnergyContent() + AR_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		ARFirmEngSup_o <- 0
	} else {
		ARFirmEngSup_o <- AR_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(ARFirmEngSup_o)
}
ARNFEnergyContent <- function() {
	ARNFEnergyContent_o <- max(0, AR_ORCEnergyContent() - ARFirmEngSup())
	return(ARNFEnergyContent_o)
}
ARNonFirmEngSup <- function() { # MWhr
	if (TotalNFEnergyContent_c == 0) {
		ARNonFirmEngSup_o <- 0
	} else {
		ARNonFirmEngSup_o <- ARNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(ARNonFirmEngSup_o)
}
ARFirmEngSupReq <- function() {
	ARFirmEngSupReq_o <- min(ARPenLimit(), ARFirmEngSup() / (MWhr_per_ftAcFt * (ARNetHead() + TotalGCHead()) * ARCombEfficiency))
	return(ARFirmEngSupReq_o)
}
ARNonFirmSupReq <- function() {
	if (NonFirmEnergySw() == 1) {
		ARNonFirmSupReq_o <- min(ARPenLimit(), (ARFirmEngSup() + ARNonFirmEngSup()) / (MWhr_per_ftAcFt * TotalGCHead() * ARCombEfficiency))
	} else {
		ARNonFirmSupReq_o <- 0
	}
	return(ARNonFirmSupReq_o)
}
AREnergySup <- function() { 
	if (UseTotalEnergyContentForFirm() == 1) {
		AREnergySup_o <- max(min(ARFirmEngSupReq(), ARSharedWater()), min(ARNonFirmSupReq(), AR_ORCSharedWater()))
	} else {
		AREnergySup_o <- max(min(ARFirmEngSupReq(), AR_ORCSharedWater()), min(ARNonFirmSupReq(), AR_ORCSharedWater()))
	}
	return(AREnergySup_o)
}
ARCombUpSup <- function() { # Release of water upstream of Arrow to meet fish and energy objectives 
	if (MICombSup_c == -9999) {
		ARCombUpSup_1 <- MICombSup()
	} else {
		ARCombUpSup_1 <- MICombSup_c
	}
	if (refill_cat4 == 0) { # Do not use any of the water released upstream of Arrow to refill to lower rule curve
		ARCombUpSup_o <- ARCombUpSup_1
	} else if (refill_cat4 == 1) { # Allow some of the water to be used to refill reservoir to the minimum draft point
		inflow <- ARIn() + ARCombUpSup_1
		outflow <- ARPrelim() + AREnergySup() + ARFishSup()
		min_draft <- min(max(ARMinRefill(), ARCriticalCurveMin()), ARBONDraftLimit(), AR_ORC())
		ARCombUpSup_o <- min(max(0, Arrow() + inflow - outflow - min_draft), ARCombUpSup_1)
	} else if (refill_cat4 == 2) { # Allow some of the water to be used to refill reservoir to the ORC
		inflow <- ARIn() + ARCombUpSup_1
		outflow <- ARPrelim() + AREnergySup() + ARFishSup()
		ARCombUpSup_o <- min(max(0, Arrow() + inflow - outflow - AR_ORC()), ARCombUpSup_1)	
	}
	return(ARCombUpSup_o)
}
ARCombSup <- function() {
	ARCombSup_o <- ARCombUpSup() + AREnergySup() + ARFishSup()
	ARCombSup_c <<- ARCombSup_o
	return(ARCombSup_o)
}
ARFishSupEnergy <- function() {
	ARFishSupEnergy_o <- ARFishSup() * (ARNetHead() + TotalGCHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(ARFishSupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ############################### 

ARFloodSpace <- function() {
	ARFloodSpace_o <- min(ARPrelim() + ARCombSup(), max(0, ARFullPoolVol - (Arrow() + ARIn() - ARPrelim() - ARCombSup())))
	ARFloodSpace_c <<- ARFloodSpace_o
	return(ARFloodSpace_o)
}
ARFloodRelSharedWater <- function() { # Water that can be released to prevent the dam from refilling too quickly during refill period
	ARFloodRelSharedWater_o <- max(0, Arrow() + ARPreInflow_c - ARPrelim() - ARCombSup() - AR_ORC())
	return(ARFloodRelSharedWater_o)
}
ARFloodFrac <- function() { 
	if (TotalFloodSpace_c == 0) {
		ARFloodFrac_o <- 0
	} else {
		ARFloodFrac_o <- (MIFloodMult * MIFloodSpace_c + ARFloodSpace_c) / TotalFloodSpace_c
	}
	return(ARFloodFrac_o)
}
ARFloodRelFrac <- function() {
	if (TotalFloodRelSharedWater_c == -9999) {
		TotalFloodRelSharedWater_c <<- TotalFloodRelSharedWater()
		water_df$TotalFloodRelSharedWater[week_counter] <<- TotalFloodRelSharedWater_c
	}
	if (TotalFloodRelSharedWater_c == 0) {
		ARFloodRelFrac_o <- 0
	} else {
		ARFloodRelFrac_o <- ARFloodRelSharedWater() / TotalFloodRelSharedWater_c
	}
	return(ARFloodRelFrac_o)
}
ARRelReducReq <- function() { 
	ARRelReducReq_o <- TotalRelReducReq_c * ARFloodFrac()
	ARRelReducReq_c <<- ARRelReducReq_o
	return(ARRelReducReq_o)
}
ARMinFloodRelReq <- function() { ## Released during refill period to ensure the reservoir does not refill too quickly, based on initial controlled flow
	ARMinFloodRelReq_o <- min(ARFloodRelSharedWater(), MinFloodRelReq() * ARFloodRelFrac())
	ARMinFloodRelReq_c <<- ARMinFloodRelReq_o
	if (is.na(water_df$ARMinFloodRelReq[week_counter])) {
		water_df$ARMinFloodRelReq[week_counter] <<- ARMinFloodRelReq_c
	}
	return(ARMinFloodRelReq_o)
}
ARCombUpProtect <- function() { ## Release of water from upstream dams to prevent overflow
	outflow <- ARPrelim() + ARCombSup_c
	ARCombUpProtect_o <- max(min(MIDamProtectExcess(), Arrow() + ARPreInflow() - outflow - AR_ORC()), 0)
	ARCombUpProtect_c <<- ARCombUpProtect_o
	return(ARCombUpProtect_o)
}
ARDamProtectExcess <- function() {
	ARDamProtectExcess_o <- max(0, ARDamProtectRel() - ARPrelim() - ARCombSup_c - ARCombUpProtect())
	return(ARDamProtectExcess_o)
}

################### Arrow final release ###############################
 
ARRelease <- function() { 
	ARRelease_o <- max(min(ARPrelim() + ARCombSup_c - ARRelReducReq() + ARMinFloodRelReq() + ARCombUpProtect_c, ARRelLimit()), ARDamProtectRel(), ARAvgMin * cfsTOafw)
	return(ARRelease_o)
}
AROutflow <- function() {
	AROutflow_o <- ARRelease_c
	return(AROutflow_o)
}

#######################################################
#-------------------- DUNCAN DAM ---------------------#
#######################################################

DUFullPoolVol <- 1424900 # Storage volume at 1892 ft. pool elevation
DUBotVol <- 25000 # Storage at 1794.2 ft. pool elevation (1.4 MAF active storage)
DUAvgMin <- 100 # FCOP 2003

InitDU <- function() {
	InitDU_o <- ResInitFractionFull * DUFullPoolVol
	return(InitDU_o)
}
Duncan <- function() {
	if (week_counter == 1) {
		Duncan_o <- InitDU()
	} else {
		Duncan_o <- reservoir_vol_df$DUNCA[week_counter - 1]
	}
	return(Duncan_o)
}
DuncanFlowData <- function() {
	return(FlowDU)
}
DUIn <- function() {
	DUIn_o <- DuncanFlowData()
	return(DUIn_o)
}
DUInflow <- function() {
	DUInflow_o <- DUIn()
	return(DUInflow_o)
}

################## Duncan max and min releases ##################################

DUMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {
		if (week_in_year < 23) {
			DUMinRefill_o <- max(DUAssuredRefill(), DUCriticalCurveMin())
		} else {
			DUMinRefill_o <- DUMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		DUMinRefill_o <- DUBotVol
	}
	return(DUMinRefill_o)
}
DUMinReq <- function() {
	DURefillMin_o <- DUAvgMin * cfsTOafw
	return(DURefillMin_o)
}
DUDamProtectRel <- function() {
	DUDamProtectRel_o <- max(Duncan() + DUInflow() - DUFullPoolVol, 0)
	return(DUDamProtectRel_o)
}
DURelLimit <- function() {
	DURelLimit_o <- max(Duncan() + DUInflow() - DUBotVol, 0)
	return(DURelLimit_o)
}
DUAvailWater <- function() {
	DUAvailWater_o <- max(Duncan() + DUIn() - DUBotVol, 0)
	return(DUAvailWater_o)
}

################### Duncan rule curves #########################

# Flood evacuation period is 1 December to 29 February
DUFloodCurve <- function() {
	if (DARunoffAprAug > 111E6) {
		DU_CurFC_o <- DUFlood_input$DUFlood5[week_in_year]
	} else if (DURunoffAprAug < 1.4E6) {
		DU_CurFC_o <- DUFlood_input$DUFlood1[week_in_year]
	} else if (DURunoffAprAug < 1.6E6) {
		DU_CurFC_o <- DUFlood_input$DUFlood1[week_in_year] - (DUFlood_input$DUFlood1[week_in_year] - DUFlood_input$DUFlood2[week_in_year]) / (1.6E6 - 1.4E6) * (DURunoffAprAug - 1.4E6)
	} else if (DURunoffAprAug < 1.8E6) {
		DU_CurFC_o <- DUFlood_input$DUFlood2[week_in_year] - (DUFlood_input$DUFlood2[week_in_year] - DUFlood_input$DUFlood3[week_in_year]) / (1.8E6 - 1.6E6) * (DURunoffAprAug - 1.6E6)
	} else if (DURunoffAprAug < 2.0E6) {
		DU_CurFC_o <- DUFlood_input$DUFlood3[week_in_year] - (DUFlood_input$DUFlood3[week_in_year] - DUFlood_input$DUFlood4[week_in_year]) / (2.0E6 - 1.8E6) * (DURunoffAprAug - 1.8E6)
	} else {
		DU_CurFC_o <- DUFlood_input$DUFlood4[week_in_year]
	}
	return(DU_CurFC_o)
}
DUTopVol <- function() {
	if (TopRuleSw() == 0) {
		DUTopVol_o <- DUFloodCurve()
	} else if (TopRuleSw() == 1) {
		DUTopVol_o <- DUFullPoolVol
	} else if (TopRuleSw() == 2) {
		DUTopVol_o <- DUFlood_input$DUFlood1[week_in_year]
	} else if (TopRuleSw() == 3) {
		if (is.na(start_refill_wk)) {
			if (week_in_year < 35) {
				DUTopVol_o <- DUFloodCurve()
			} else {
				DUTopVol_o <- DUFullPoolVol
			}
		} else if (week_in_year < start_refill_wk) {
			DUTopVol_o <- DUFloodCurve()
		} else {
			DUTopVol_o <- DUFullPoolVol()
		}
	}
	return(DUTopVol_o)
}
DURuleReq <- function() {
	DURuleReq_o <- max(Duncan() + DUIn() - DUTopVol(), 0)
	return(DURuleReq_o)
}
DUPrelim <- function() {
	DUPrelim_o <- min(DUAvailWater(), max(DURuleReq(), DUMinReq()))
	return(DUPrelim_o)
}
DULowerLimit <- function() {
	DULL_o <- lower_limit_input$Duncan[week_in_year]
	return(DULL_o)
}
DUCriticalCurve <- function() {
	DUCriticalCurve_o <- DUCriticalCurve_input$CRC1[week_in_year]
	return(DUCriticalCurve_o)
}
DUCriticalCurveMin <- function() {
	if (DUCriticalCurveSw == 1) {
		DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC1[week_in_year]
	} else if (DUCriticalCurveSw == 2) {
		DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC2[week_in_year]
	} else if (DUCriticalCurveSw == 3) {
		DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC3[week_in_year]
	} else if (DUCriticalCurveSw == 4) {
		DUCriticalCurveMin_o <- DUCriticalCurve_input$CRC4[week_in_year]
	}
	return(DUCriticalCurveMin_o)
}
DUAssuredRefill <- function() {
	DUAssuredRefill_o <- DUAssuredRefill_input[week_in_year, 2]
	return(DUAssuredRefill_o)
}
DUVariableRefill <- function() {
	if (RefillSwitch() == 1) {
		DURefillCurve_o <- DUAssuredRefill()
	} else if (RefillSwitch() == 2) {
		DURefillCurve_o <- DUVariableRefillCurve
	}
	return(DURefillCurve_o)
}
DU_ORC <- function() {
	DU_ORC_o <- min(max(min(max(DUAssuredRefill(), DUCriticalCurve()), DUVariableRefill()), DULowerLimit()), DUFloodCurve())
	return(DU_ORC_o)
}

##################### Duncan fish flow ########################################################

DUMcNaryDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
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
	} else {
		DUMcNaryDraftLimit_o <- DU_ORC()
	}
	return(DUMcNaryDraftLimit_o)
}
DUMcNarySharedWater <- function() {
	DUMcNarySharedWater_o <- max(0, Duncan() + DUIn() - DUPrelim() - DUMcNaryDraftLimit())
	return(DUMcNarySharedWater_o)
}
DUMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		DUMcNarySup_o <- 0
	} else {
		DUMcNarySup_o <- min(DUMcNarySharedWater(), McNaryFlowDeficit() * DUMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(DUMcNarySup_o)
}

########### Duncan energy ##################################

# Duncan does not have any generators, but it still releases water for hydropower generation at downstream dams

DUMcNarySupEnergy <- function() {
	DUMcNarySupEnergy_o <- DUMcNarySup() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt	
	return(DUMcNarySupEnergy_o)
}
DUSharedWater <- function() {
	DUSharedWater_o <- max(Duncan() + DUIn() - DUPrelim() - DUMcNarySup() - max(DUMinRefill(), DUCriticalCurveMin()), 0)
	return(DUSharedWater_o)
}
DUEnergyContent <- function() {
	DUEnergyContent_o <- DUSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DUEnergyContent_o)
}
DU_ORCSharedWater <- function() {
	DU_ORCSharedWater_o <- max(0, Duncan() + DUIn() - DUPrelim() - DUMcNarySup() - DU_ORC())
	return(DU_ORCSharedWater_o)
}
DU_ORCEnergyContent <- function() {
	DU_ORCEnergyContent_o <- DU_ORCSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DU_ORCEnergyContent_o)
}
DUFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			DUFirmEngSup_o <- 0
		} else {
			DUFirmEngSup_o <- (DUEnergyContent() + DU_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		DUFirmEngSup_o <- 0
	} else {
		DUFirmEngSup_o <- DU_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(DUFirmEngSup_o)
}
DUNFEnergyContent <- function() {
	DUNFEnergyContent_o <- max(0, DU_ORCEnergyContent() - DUFirmEngSup())
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
DUFirmEngSupReq <- function() {
	DUFirmEngSupReq_o <- DUFirmEngSup() / (MWhr_per_ftAcFt * TotalGCHead() * DUCombEfficiency)
	return(DUFirmEngSupReq_o)
}
DUNonFirmSupReq <- function() {
	if (NonFirmEnergySw() == 1) {
		DUNonFirmSupReq_o <- (DUFirmEngSup() + DUNonFirmEngSup()) / (MWhr_per_ftAcFt * TotalGCHead() * DUCombEfficiency)	
	} else {
		DUNonFirmSupReq_o <- 0
	}
	return(DUNonFirmSupReq_o)
}
DUEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		DUEnergySup_o <- max(min(DUFirmEngSupReq(), DUSharedWater()), min(DUNonFirmSupReq(), DU_ORCSharedWater()))
	} else {
		DUEnergySup_o <- max(min(DUFirmEngSupReq(), DU_ORCSharedWater()), min(DUNonFirmSupReq(), DU_ORCSharedWater()))
	}
	return(DUEnergySup_o)
}
DUCombSup <- function() {
	DUCombSup_o <- DUEnergySup() + DUMcNarySup()
	DUCombSup_c <<- DUCombSup_o
	return(DUCombSup_o)
}
DUDamProtectExcess <- function() {
	DUDamProtectExcess_o <- max(0, DUDamProtectRel() - DUPrelim() - DUCombSup_c)
	return(DUDamProtectExcess_o)
}

################### Duncan final release ###############################

DURelease <- function() {
	DURelease_o <- max(min(DUPrelim() + DUCombSup(), DURelLimit()), DUDamProtectRel())
	DURelease_c <<- DURelease_o
	return(DURelease_o)
}
DUOutflow <- function() {
	DUOutflow_o <- DURelease_c
	return(DUOutflow_o)
}

#######################################################
#--------------------- LIBBY DAM ---------------------#
#######################################################

LBFullPoolVol <- 5869400 # Gross storage at 2459 ft. pool elevation
LBBotVol <- 889900 # Minimum operating limit, 2287 ft. pool elevation
LBAvgMin <- 4000 # FCOP, 2003

InitLB <- function() {
	InitLB_o <- ResInitFractionFull * LBFullPoolVol
	return(InitLB_o)
}
Libby <- function() {
	if (week_counter == 1) {
		Libby_o <- InitLB()
	} else {
		Libby_o <- reservoir_vol_df$LIBBY[week_counter - 1]
	}
	return(Libby_o)
}
LibbyFlowData <- function() {
	return(FlowLB)
}
LBElev_ft <- function() {
	upper_vol <- LB_elev_input$Volume[which(LB_elev_input$Volume >= Libby())[1]]
	lower_vol <- LB_elev_input$Volume[tail(which(LB_elev_input$Volume <= Libby())[1],1)]
	upper_el <- LB_elev_input$Elevation[which(LB_elev_input$Volume >= Libby())[1]]
	lower_el <- LB_elev_input$Elevation[tail(which(LB_elev_input$Volume <= Libby())[1],1)]
	if (is.na(lower_el)) {
		LBElev_ft_o <- min(LB_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		LBElev_ft_o <- max(LB_elev_input$Elevation)	
	} else if (lower_el == upper_el) {
		LBElev_ft_o <- lower_el
	} else {
		LBElev_ft_o <- lower_el + (Libby() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(LBElev_ft_o)
}
LBNetHead <- function() {
	LBTailElev <- 2121 # Libby Dam Master Plan 1997
	LBNetHead_o <- LBElev_ft() - LBTailElev
	return(LBNetHead_o)
}
LBPenLimit <- function() {
	LBPenCap <- 24100 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Libby/#top
	LBPenLimit_o <- LBPenCap * cfsTOafw
	return(LBPenLimit_o)
}
LBIn <- function() {
	LBIn_o <- LibbyFlowData()
	return(LBIn_o)
}
LBInflow <- function() {
	LBInflow_o <- LBIn()
	return(LBInflow_o)
}

############ Libby max and min releases ################### 

LBMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {	
		if (week_in_year < 23) {
			LBMinRefill_o <- max(LBAssuredRefill(), LBCriticalCurveMin())
		} else {
			LBMinRefill_o <- LBMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		LBMinRefill_o <- LBBotVol
	}
	return(LBMinRefill_o)
}
LBMinReq <- function() {
	LBMinReq_1 <- max(LB_trout_flow(), LB_sturgeon_flow(), LBAvgMin) * cfsTOafw
	if (FishOverRefillSw() == 1) {
		LBMinReq_o <- LBMinReq_1
	} else {
		LBMinReq_o <- min(max(Libby() + LBIn() - max(LBMinRefill(), LBCriticalCurveMin()), LBAvgMin * cfsTOafw), LBMinReq_1)
	}
	return(LBMinReq_o)
}
LBDamProtectRel <- function() {
	LBDamProtectRel_o <- max(0, Libby() + LBInflow() - LBFullPoolVol)
	return(LBDamProtectRel_o)
}
LBRelLimit <- function() {
	LBRelLimit_o <- max(Libby() + LBInflow() - LBBotVol, 0)
	return(LBRelLimit_o)
}
LBAvailWater <- function() {
	LBAvailWater_o <- max(0, Libby() + LBIn() - LBBotVol)
	return(LBAvailWater_o)
}

########### Libby rule curves ##############

## 1998 Revised flood control storage curve ("The effects of VARQ at Libby and Hungry Horse on Columbia River System Flood Control". U.S.A.C.E. 1998, Fig. 3).
LBFloodCurve <- function() {
	if (LBRunoffAprAug < 4.5E6) {
		LB_CurFC_o <- LBFlood_input$LBFlood1[week_in_year]
	} else if (LBRunoffAprAug < 5.5E6) {
		LB_CurFC_o <- LBFlood_input$LBFlood1[week_in_year] - (LBFlood_input$LBFlood1[week_in_year] - LBFlood_input$LBFlood2[week_in_year]) / (5.5E6 - 4.5E6) * (LBRunoffAprAug - 4.5E6)
	} else if (LBRunoffAprAug < 6.5E6) {
		LB_CurFC_o <- LBFlood_input$LBFlood2[week_in_year] - (LBFlood_input$LBFlood2[week_in_year] - LBFlood_input$LBFlood3[week_in_year]) / (6.5E6 - 5.5E6) * (LBRunoffAprAug - 5.5E6)
	} else if (LBRunoffAprAug < 7.5E6) {
		LB_CurFC_o <- LBFlood_input$LBFlood3[week_in_year] - (LBFlood_input$LBFlood3[week_in_year] - LBFlood_input$LBFlood4[week_in_year]) / (7.5E6 - 6.5E6) * (LBRunoffAprAug - 6.5E6)
	} else if (LBRunoffAprAug < 8.0E6) {
		LB_CurFC_o <- LBFlood_input$LBFlood4[week_in_year] - (LBFlood_input$LBFlood4[week_in_year] - LBFlood_input$LBFlood5[week_in_year]) / (8.0E6 - 7.5E6) * (LBRunoffAprAug - 7.5E6)
	} else {
		LB_CurFC_o <- LBFlood_input$LBFlood5[week_in_year]
	}
	return(LB_CurFC_o)
}
LBTopVol <- function() {
	if (TopRuleSw() == 0) {
		LBTopVol_o <- min(LibbyBiOpDraftLimit(), LBFloodCurve())
	} else if (TopRuleSw() == 1) {
		LBTopVol_o <- LBFullPoolVol
	} else {
		LBTopVol_o <- LBFlood_input$LBFlood_1[week_in_year]
	}
	return(LBTopVol_o)
}
LBRuleReq <- function() {
	LBRuleReq_o <- max(Libby() + LBIn() - LBTopVol(), 0)
	return(LBRuleReq_o)
}
LBPrelim <- function() {
	LBPrelim_o <- min(LBAvailWater(), max(LBRuleReq(), LBMinReq()))
	return(LBPrelim_o)
}
LBLowerLimit <- function() {
	LBLL_o <- lower_limit_input$Libby[week_in_year]
	return(LBLL_o)
}	
LBCriticalCurve <- function() {
	LBCriticalCurve_o <- LBCriticalCurve_input[week_in_year, 2]
	return(LBCriticalCurve_o)
}
LBCriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		LBCriticalCurveMin_o <- LBCriticalCurve_input$CRC4[week_in_year]
	}
	return(LBCriticalCurveMin_o)
}
LBAssuredRefill <- function() {
	LBAssuredRefill_o <- LBAssuredRefill_input[week_in_year, 2]
	return(LBAssuredRefill_o)
}
LBVariableRefill <- function() {
	RefillCurve_o <- LBVariableRefillCurve
	return(RefillCurve_o)
}
LB_ORC <- function() {
	LB_ORC_o <- min(max(min(max(LBAssuredRefill(), LBCriticalCurve()), LBVariableRefill()), LBLowerLimit(), LBMinRefill()), LBFloodCurve(), LibbyBiOpDraftLimit())
	return(LB_ORC_o)
}

########### Libby fish flow ############################

# 2006 NWFS Libby BiOp 
# A base minimum flow of 6000 cfs is required from May 15 through September 30 and 4000 cfs October 1 to May 14 to support trout and sturgeon
LB_trout_flow <- function() {# 2022 water management plan
	if (week_in_year %in% c(49:52, 1:5)) {
		if (LBRunoffAprAug < 4.8e6) {		 
			LB_bull_trout_o <- 6000
		} else if (LBRunoffAprAug < 6.0e6) {
			LB_bull_trout_o <- 7000
		} else if (LBRunoffAprAug < 6.7e6) {
			LB_bull_trout_o <- 8000
		} else {
			LB_bull_trout_o <- 9000
		}
	} else if (week_in_year %in% c(42:48, 6:9)) {
		LB_bull_trout_o <- 6000
	} else {
		LB_bull_trout_o <- 4000
	}
	return(LB_bull_trout_o)
}
LB_sturgeon_flow <- function() { # 2006 USFWS Libby BiOp
	if (week_in_year %in% 42:52) {
		if (LBRunoffAprAug < 4.8e6) {
			tier_volume <- 0
		} else if (LBRunoffAprAug < 5.4e6) {
			tier_volume <- 0.8e6
		} else if (LBRunoffAprAug < 6.35e6) {	
			tier_volume <- 0.8e6 + (LBRunoffAprAug - 5.4e6) / (6.35e6 - 5.4e6) * (1.12e6 - 0.8e6)
		} else if (LBRunoffAprAug < 7.4e6) {
			tier_volume <- 1.12e6 + (LBRunoffAprAug - 6.35e6) / (7.4e6 - 6.35e6) * (1.2e6 - 1.12e6)
		} else if (LBRunoffAprAug < 8.5e6) {	
			tier_volume <- 1.2e6
		} else if (LBRunoffAprAug < 8.9e6) {
			tier_volume <- 1.2e6 + (LBRunoffAprAug - 8.5e6) / (8.9e6 - 8.5e6) * (1.6e6 - 1.2e6)
		} else {
			tier_volume <- 1.6e6
		}
	} else {
		tier_volume <- 0
	}
	LB_sturgeon_o <- 4000 + tier_volume / 11 / cfsTOafw	
	return(LB_sturgeon_o)
}	
# According to 2008 BiOp, Libby should be drafted to 10 ft from full by end of Sept. unless forecasted flow at the Dalles is less than the 20th percentile,
# in which case the target elevation is 20 ft from full. The 2022 water management plan gives variable target elevations based on the Libby forecasted inflows. 
LibbyBiOpDraftLimit <- function() {
	if (LBRunoffAprAug <= 4.66e6) {
		LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_15th"[week_in_year]
	} else if (LBRunoffAprAug <= 5.01e6) {
		LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_15th"[week_in_year] + (LBRunoffAprAug - 4.66e6) / (5.01e6 - 4.66e6) * (LibbyBiOpDraftLimit_input$"perc_25th"[week_in_year] - LibbyBiOpDraftLimit_input$"perc_15th"[week_in_year])   
	} else if (LBRunoffAprAug <= 6.78e6) {	
		LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_75th"[week_in_year]
	} else if (LBRunoffAprAug <= 7.33e6) {
		LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_75th"[week_in_year] + (LBRunoffAprAug - 6.78e6) / (7.33e6 - 6.78e6) * (LibbyBiOpDraftLimit_input$"perc_85th"[week_in_year] - LibbyBiOpDraftLimit_input$"perc_75th"[week_in_year])   
	} else {
		LibbyBiOpDraftLimit_o <- LibbyBiOpDraftLimit_input$"perc_85th"[week_in_year]
	}	
	return(LibbyBiOpDraftLimit_o)
}
LBMcNaryDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
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
	} else if (FishOverRefillSw() == 0) {
		LBMcNaryDraftLimit_o <- LB_ORC()
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

################### Libby energy ########################

LibbyPreEnergy <- function() {
	LibbyPreEnergy_o <- MWhr_per_ftAcFt * min(LBPrelim(), LBPenLimit()) * LBNetHead() * LBCombEfficiency
	return(LibbyPreEnergy_o)
}
LBSharedWater <- function() {
	LBSharedWater_o <- max(Libby() + LBIn() - LBPrelim() - LBMcNarySup() - max(LBMinRefill(), LBCriticalCurveMin()), 0)
	return(LBSharedWater_o)
}
LBDownStreamHead <- function() {
	LBDownStreamHead_o <- TotalGCHead()
	return(LBDownStreamHead_o)
}
LBEnergyContent <- function() {
	LBEnergyContent_o <- LBSharedWater() * (LBNetHead() + LBDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBEnergyContent_o)
}
LB_ORCSharedWater <- function() {
	LB_ORCSharedWater_o <- max(0, Libby() + LBIn() - LBPrelim() - LBMcNarySup() - LB_ORC())
	return(LB_ORCSharedWater_o)
}
LB_ORCEnergyContent <- function() {
	LB_ORCEnergyContent_o <- LB_ORCSharedWater() * (LBNetHead() + LBDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LB_ORCEnergyContent_o)
}
LBFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			LBFirmEngSup_o <- 0
		} else {
			LBFirmEngSup_o <- (LBEnergyContent() + LB_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		LBFirmEngSup_o <- 0
	} else {
		LBFirmEngSup_o <- LB_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(LBFirmEngSup_o)
}
LBNFEnergyContent <- function() {
	LBNFEnergyContent_o <- max(0, LB_ORCEnergyContent() - LBFirmEngSup())
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
	LBFirmEngSupReq_o <- min(LBPenLimit(), LBFirmEngSup() / (MWhr_per_ftAcFt * (LBNetHead() + LBDownStreamHead()) * LBCombEfficiency))
	return(LBFirmEngSupReq_o)
}
LBNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw() == 1) {
		LBNonFirmEngSupReq_o <- min(LBPenLimit(), (LBFirmEngSup() + LBNonFirmEngSup()) / (MWhr_per_ftAcFt * (LBNetHead() + LBDownStreamHead()) * LBCombEfficiency))
	} else {
		LBNonFirmEngSupReq_o <- 0
	}
	return(LBNonFirmEngSupReq_o)
}
LBEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		LBEnergySup_o <- max(min(LBFirmEngSupReq(), LBSharedWater()), min(LBNonFirmEngSupReq(), LB_ORCSharedWater()))
	} else {
		LBEnergySup_o <- max(min(LBFirmEngSupReq(), LB_ORCSharedWater()), min(LBNonFirmEngSupReq(), LB_ORCSharedWater()))
	}
	return(LBEnergySup_o)
}
LBCombSup <- function() {
	LBCombSup_o <- LBMcNarySup() + LBEnergySup()
	LBCombSup_c <<- LBCombSup_o
	return(LBCombSup_o)
}
LBMcNarySupEnergy <- function() { # Hydropower generated by releasing water from Libby to meet McNary fish flow target
	LBMcNarySupEnergy_o <- LBMcNarySup() * (LBNetHead() + LBDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(LBMcNarySupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ############################### 

LBFloodSpace <- function() {
	LBFloodSpace_o <- min(LBPrelim() + LBCombSup(), max(0, LBFullPoolVol - (Libby() + LBIn() - LBPrelim() - LBCombSup())))
	LBFloodSpace_c <<- LBFloodSpace_o
	return(LBFloodSpace_o)
}
LBFloodFrac <- function() {
	if (TotalFloodSpace_c == 0) {
		LBFloodFrac_o <- 0
	} else {
		LBFloodFrac_o <- LBFloodSpace_c / TotalFloodSpace_c
	}	
	return(LBFloodFrac_o)
}
LBRelReducReq <- function() {
	LBRelReducReq_o <- TotalRelReducReq_c * LBFloodFrac()
	return(LBRelReducReq_o)
}
LBDamProtectExcess <- function() {
	LBDamProtectExcess_o <- max(0, LBDamProtectRel() - LBPrelim() - LBCombSup_c)
	return(LBDamProtectExcess_o)
}

############# Libby final release ############################

LBRelease <- function() {
	LBRelease_o <- max(min(LBPrelim() + LBCombSup_c - LBRelReducReq(), LBRelLimit()), LBDamProtectRel(), LBAvgMin * cfsTOafw)
	return(LBRelease_o)
}
LBOutflow <- function() {
	LBOutflow_o <- LBRelease_c
	return(LBOutflow_o)
}

#######################################################
#------------------- BONNERS FERRY -------------------#
#######################################################

BonnersFerryFlowData <- function() {
	return(FlowBONF)
}
BONFInc <- function() {
	BONFInc_o <- BonnersFerryFlowData() - LibbyFlowData()
	return(BONFInc_o)
}
BONFIn <- function() {
	BONFIn_o <- LBRelease() + BONFInc()
	return(BONFIn_o)
}
BONFOut <- function() {
	BONDFOut_o <- BONFIn()
	return(BONDFOut_o)
}

#######################################################
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
	return(FlowCL)
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
	CLRelLimit_o <- max(CorraLinn() + CLPreInflow_c - CLBotVol, 0)
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
	CLRuleReq_o <- max(CorraLinn() + CLIn() - CLTopVol(), 0)
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
#------------------ HUNGRY HORSE DAM -----------------#
####################################################### 

HHFullPoolVol <- 3071500 # Storage corresponding with 3560 ft. pool elevation
HHBotVol <- 0 # Storage at 3336 ft. pool elevation 
HHAvgMin <- 400 # FCOP, 2003

InitHH <- function() {
	InitHH_o <- ResInitFractionFull * HHFullPoolVol
	return(InitHH_o)
}
HungryHorse <- function() {
	if (week_counter == 1) {
		HungryHorse_o <- InitHH()
	} else {
		HungryHorse_o <- reservoir_vol_df$FLASF[week_counter - 1]
	}	
	return(HungryHorse_o)
}
HungryHorseFlowData <- function() {
	return(FlowHH)
}
HHElev_ft <- function() {
	upper_vol <- HH_elev_input$Volume[which(HH_elev_input$Volume >= HungryHorse())[1]]
	lower_vol <- HH_elev_input$Volume[tail(which(HH_elev_input$Volume <= HungryHorse())[1],1)]
	upper_el <- HH_elev_input$Elevation[which(HH_elev_input$Volume >= HungryHorse())[1]]
	lower_el <- HH_elev_input$Elevation[tail(which(HH_elev_input$Volume <= HungryHorse())[1],1)]
	if (is.na(lower_el)) {
		HHElev_ft_o <- min(HH_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		HHElev_ft_o <- max(HH_elev_input$Elevation)	
	} else if (lower_el == upper_el) {
		HHElev_ft_o <- lower_el
	} else {
		HHElev_ft_o <- lower_el + (HungryHorse() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(HHElev_ft_o)
}
HHNetHead <- function() {
	HHTailElev <- 3077 # Changed from 3082.6 on 5/25/23, average from 1/1/1980 to 1/1/2023. https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=hungry%20horse 
	HHNetHead_o <- HHElev_ft() - HHTailElev
	return(HHNetHead_o)
}
HHPenLimit <- function() {
	HHpen <- 12048 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Hungry-Horse/
	HHPenLimit_o <- HHpen * cfsTOafw
	return(HHPenLimit_o)
}
HHIn <- function() {
	HHIn_o <- HungryHorseFlowData()
	return(HHIn_o)
}
HHInflow <- function() {
	HHInflow_o <- HHIn()
	return(HHInflow_o)
}

############ Hungry Horse max and min releases ###################

HHMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {
		if (week_in_year < 23) {
			HHMinRefill_o <- max(HHAssuredRefill(), HHCriticalCurveMin())
		} else {
			HHMinRefill_o <- HHMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		HHMinRefill_o <- HHBotVol
	}
	return(HHMinRefill_o)
}
HHMinReq <- function() {
	HHMinReq_1 <- max(HHFishMin() * cfsTOafw, HHRelForColFalls())
	if (FishOverRefillSw() == 1) {
		HHMinReq_o <- HHMinReq_1
	} else {
		HHMinReq_o <- min(max(HungryHorse() + HHIn() - max(HHCriticalCurveMin(), HHMinRefill()), HHAvgMin * cfsTOafw), HHMinReq_1)
	}
	return(HHMinReq_o)
}
HHDamProtectRel <- function() {
	HHDamProtectRel_o <- max(0, HungryHorse() + HHInflow() - HHFullPoolVol)
	return(HHDamProtectRel_o)
}
# Maximum flow at Columbia Falls, stipulated for Oct - Dec.
ColFallsMaxFlow <- function() {
	ColFallsMaxFlow_o <- 51000 # Flood stage at Columbia Falls is about 14 ft. or 51000 cfs (2009 Water Management Plan)
	return(ColFallsMaxFlow_o)
}
HHColFallsmax <- function() {
	HHColFallsmax_o <- max(0, ColFallsMaxFlow() * cfsTOafw - (ColumbiaFallsFlowData() - HungryHorseFlowData()))
	return(HHColFallsmax_o)
}
HHRelLimit <- function() {
	HHRelLimit_o <- max(HungryHorse() + HHInflow() - HHBotVol, 0)
	return(HHRelLimit_o)
}
HHAvailWater <- function() {
	HHAvailWater_o <- max(0, HungryHorse() + HHIn() - HHBotVol)
	return(HHAvailWater_o)
}

########### Hungry Horse rule curves ##############

## 1998 VARQ flood curve (current as of 5/30/2023)
# Oct - Apr flood evacuation; May - Jun 30 refill
HHFloodCurve <- function() {
	if (HHRunoffMaySep < 1.0E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood1[week_in_year]
	} else if (HHRunoffMaySep < 1.4E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood1[week_in_year] - (HHFlood_input$HHFlood1[week_in_year] - HHFlood_input$HHFlood2[week_in_year]) / (1.4E6 - 1.0E6) * (HHRunoffMaySep - 1.0E6)
	} else if (HHRunoffMaySep < 1.6E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood2[week_in_year] - (HHFlood_input$HHFlood2[week_in_year] - HHFlood_input$HHFlood3[week_in_year]) / (1.6E6 - 1.4E6) * (HHRunoffMaySep - 1.4E6)
	} else if (HHRunoffMaySep < 2.0E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood3[week_in_year] - (HHFlood_input$HHFlood3[week_in_year] - HHFlood_input$HHFlood4[week_in_year]) / (2.0E6 - 1.6E6) * (HHRunoffMaySep - 1.6E6)
	} else if (HHRunoffMaySep < 2.2E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood4[week_in_year] - (HHFlood_input$HHFlood4[week_in_year] - HHFlood_input$HHFlood5[week_in_year]) / (2.2E6 - 2.0E6) * (HHRunoffMaySep - 2.0E6)
	} else if (HHRunoffMaySep < 2.5E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood5[week_in_year] - (HHFlood_input$HHFlood5[week_in_year] - HHFlood_input$HHFlood6[week_in_year]) / (2.5E6 - 2.2E6) * (HHRunoffMaySep - 2.2E6)
	} else if (HHRunoffMaySep < 2.8E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood6[week_in_year] - (HHFlood_input$HHFlood6[week_in_year] - HHFlood_input$HHFlood7[week_in_year]) / (2.8E6 - 2.5E6) * (HHRunoffMaySep - 2.5E6)
	} else if (HHRunoffMaySep < 3.6E6) {
		HH_CurFC_o <- HHFlood_input$HHFlood7[week_in_year] - (HHFlood_input$HHFlood7[week_in_year] - HHFlood_input$HHFlood8[week_in_year]) / (3.6E6 - 2.8E6) * (HHRunoffMaySep - 2.8E6)
	} else {
		HH_CurFC_o <- HHFlood_input$HHFlood8[week_in_year]
	}
	return(HH_CurFC_o)
}
HHTopVol <- function() {
	if (TopRuleSw() == 0) {
		HHTopVol_o <- min(HHBiOpDraftLimit(), HHFloodCurve())
	} else if (TopRuleSw() == 1) {
		HHTopVol_o <- HHFullPoolVol
	} else if (TopRuleSw() == 2) {
		HHTopVol_o <- HHFlood$HHFlood1[week_in_year]
	} 
	return(HHTopVol_o)
}
HHRuleReq <- function() {
	HHRuleReq_o <- max(HungryHorse() + HHIn() - HHTopVol(), 0)
	return(HHRuleReq_o)
}
HHPrelim <- function() {
	HHPrelim_o <- min(HHAvailWater(), max(HHRuleReq(), HHMinReq()))
	return(HHPrelim_o)
}
HHLowerLimit <- function() {
	HHLL_o <- lower_limit_input$HungryHorse[week_in_year]
	return(HHLL_o)
}	
HHCriticalCurve <- function() {
	HHCriticalCurve_o <- HHCriticalCurve_input[week_in_year, 2]
	return(HHCriticalCurve_o)
}
HHCriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		HHCriticalCurveMin_o <- HHCriticalCurve_input$CRC4[week_in_year]
	}
	return(HHCriticalCurveMin_o)
}
HHAssuredRefill <- function() { 
	HHAssuredRefill_o <- HHAssuredRefill_input[week_in_year, 2]
	return(HHAssuredRefill_o)
}
HHVariableRefill <- function() {
	if (RefillSwitch() == 1) {
		HHRefillCurve_o <- HHAssuredRefill()
	} else if (RefillSwitch() == 2) {
		HHRefillCurve_o <- HHVariableRefillCurve
	}
	return(HHRefillCurve_o)
}
HH_ORC <- function() {
	HH_ORC_o <- min(max(min(max(HHAssuredRefill(), HHCriticalCurve()), HHVariableRefill()), HHLowerLimit(), HHMinRefill()), HHFloodCurve(), HHBiOpDraftLimit())
	return(HH_ORC_o)
}

########### Hungry Horse fish flow ###########################

####### HHMin (2000 PALFWS BiOp Section 3.A.1 Page 6)
# Minimum release from Hungry Horse
# 400 to 900 cfs
# variable based upon predicted inflows between april and august
HHFishMin <- function() {
	if (HHRunoffAprAug / 1000 < 1190) {
		HHMin_o <- 400
	} else if (HHRunoffAprAug / 1000 <= 1790) {
		HHMin_o <- 400 + (HHRunoffAprAug / 1000 - 1190) / (1790 - 1190) * (900 - 400)
	} else {
		HHMin_o <- 900
	}
	return(HHMin_o)
}
#### ColFallsTarget from 2000 FCRS BiOp Section 3.A.1 Page 7
# Columbia falls obligation to Columbia Falls Flow Target is based upon forecasted inflows from April through August.  The target is as follows (units cfs)
# Forecast       -- target
# Above 1790 taf -- 3500 cfs
# Below 1190 taf -- 3200 cfs
# between two values is a linear interpolation.
ColFallsTarget <- function() {
	if (HHRunoffAprAug / 1000 < 1190) {
		ColFallsTarget_o <- 3200
	} else if (HHRunoffAprAug / 1000 <= 1790) {
		ColFallsTarget_o <- 3200 + (HHRunoffAprAug / 1000 - 1190) / (1790 - 1190) * (3500 - 3200)
	} else {
		ColFallsTarget_o <- 3500
	}
	return(ColFallsTarget_o)
}
HHRelForColFalls <- function() {
	HHRelForColFalls_o <- max(0, ColFallsTarget() * cfsTOafw - (ColumbiaFallsFlowData() - HungryHorseFlowData()))
	return(HHRelForColFalls_o)
}
## 2008 BiOp requires HH be drafted to 3540-3550 ft. pool elevation from July - Sept. 
## The variable target is based on May-Sep forecasted runoff volume at Hungry Horse (2022 water management plan).
HHBiOpDraftLimit <- function() {
	if (HHRunoffMaySep < 1.41e6) {
		HHBiOpDraftLimit_o <- HHBiOpDraftLimit_input$Low[week_in_year]
	} else if (HHRunoffMaySep < 1.58e6) {
		HHBiOpDraftLimit_o <- HHBiOpDraftLimit_input$Low[week_in_year] + (HHRunoffMaySep - 1.41e6) / (1.58e6 - 1.41e6) * (HHBiOpDraftLimit_input$High[week_in_year] - HHBiOpDraftLimit_input$Low[week_in_year])
	} else {
		HHBiOpDraftLimit_o <- HHBiOpDraftLimit_input$High[week_in_year]
	}	
	return(HHBiOpDraftLimit_o)
}
HHMcNaryDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
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
	} else if (FishOverRefillSw() == 0) {
		HHMcNaryDraftLimit_o <- HH_ORC()
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

########## Hungry Horse energy #####################################

HungryHorsePreEnergy <- function() {
	HungryHorsePreEnergy_o <- MWhr_per_ftAcFt * min(HHPrelim(), HHPenLimit()) * HHNetHead() * HHCombEfficiency
	return(HungryHorsePreEnergy_o)
}
HHSharedWater <- function() {
	HHSharedWater_o <- max(HungryHorse() + HHIn() - HHPrelim() - HHMcNarySup() - max(HHMinRefill(), HHCriticalCurveMin()), 0)
	return(HHSharedWater_o)
}
HHDownStreamHead <- function() {
	HHDownStreamHead_o <- AFNetHead() + BCNetHead() + BDNetHead() + CBNetHead() + KENetHead() + NOXNetHead() + TotalGCHead()
	return(HHDownStreamHead_o)
}
HHEnergyContent <- function() {
	HHEnergyContent_o <- HHSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(HHEnergyContent_o)
}
HH_ORCSharedWater <- function() {
	HH_ORCSharedWater_o <- max(0, HungryHorse() + HHIn() - HHPrelim() - HHMcNarySup() - HH_ORC())
	return(HH_ORCSharedWater_o)
}
HH_ORCEnergyContent <- function() {
	HH_ORCEnergyContent_o <- (HH_ORCSharedWater() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt)
	return(HH_ORCEnergyContent_o)
}
HHFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			HHFirmEngSup_o <- 0
		} else {
			HHFirmEngSup_o <- (HHEnergyContent() + HH_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		HHFirmEngSup_o <- 0
	} else {
		HHFirmEngSup_o <- HH_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(HHFirmEngSup_o)
}
HHNFEnergyContent <- function() {
	HHNFEnergyContent_o <- max(0, HH_ORCEnergyContent() - HHFirmEngSup())
	return(HHNFEnergyContent_o)
}
HHNonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		HHNonFirmEngSup_o <- 0
	} else {
		HHNonFirmEngSup_o <- HHNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
	return(HHNonFirmEngSup_o)
}
HHFirmEngSupReq <- function() {
	HHFirmEngSupReq_o <- min(HHPenLimit(), HHFirmEngSup() / (MWhr_per_ftAcFt * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency))
	return(HHFirmEngSupReq_o)
}
HHNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw() == 1) {
		HHNonFirmEngSupReq_o <- min(HHPenLimit(), (HHFirmEngSup() + HHNonFirmEngSup()) / (MWhr_per_ftAcFt * (HHNetHead() + HHDownStreamHead()) * HHCombEfficiency))
	} else {
		HHNonFirmEngSupReq_o <- 0
	}
	return(HHNonFirmEngSupReq_o)
}
HHEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		HHEngSup_o <- max(min(HHFirmEngSupReq(), HHSharedWater()), min(HHNonFirmEngSupReq(), HH_ORCSharedWater()))
	} else {
		HHEngSup_o <- max(min(HHFirmEngSupReq(), HH_ORCSharedWater()), min(HHNonFirmEngSupReq(), HH_ORCSharedWater()))
	}
	return(HHEngSup_o)
}
HHEnergySupAllow <- function() { # Maximum water that can be released to meet energy requirement and flow objective at Columbia Falls
	HHEnergySupAllow_o <- max(0, HHColFallsmax() - HHPrelim())
	return(HHEnergySupAllow_o)
}
HHCombSup <- function() {
	HHCombSup_o <- min(HHEnergySup(), HHEnergySupAllow()) + HHMcNarySup()
	HHCombSup_c <<- HHCombSup_o
	return(HHCombSup_o)
}
HHMcNarySupEnergy <- function() {
	HHMcNarySupEnergy_o <- HHMcNarySup() * (HHNetHead() + HHDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(HHMcNarySupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ############################### 

HHFloodSpace <- function() {
	HHFloodSpace_o <- min(HHPrelim() + HHCombSup(), max(0, HHFullPoolVol - (HungryHorse() + HHIn() - HHPrelim() - HHCombSup())))
	HHFloodSpace_c <<- HHFloodSpace_o
	return(HHFloodSpace_o)
}
HHFloodFrac <- function() {
	if (TotalFloodSpace_c == 0) {
		HHFloodFrac_o <- 0
	} else {
		HHFloodFrac_o <- HHFloodSpace_c / TotalFloodSpace_c
	}	
	return(HHFloodFrac_o)
}
HHRelReducReq <- function() {
	HHRelReducReq_o <- TotalRelReducReq_c * HHFloodFrac()
	return(HHRelReducReq_o)
}
HHDamProtectExcess <- function() {
	HHDamProtectExcess_o <- max(0, HHDamProtectRel() - HHPrelim() - HHCombSup_c)
	return(HHDamProtectExcess_o)
}

############# Hungry Horse final release ############################

HHRelease <- function() {
	HHRelease_o <- max(min(HHPrelim() + HHCombSup_c - HHRelReducReq(), HHRelLimit()), HHDamProtectRel(), HHAvgMin * cfsTOafw)
	return(HHRelease_o)
}
HHOutflow <- function() {
	HHOutflow_o <- HHRelease_c
	return(HHOutflow_o)
}

## There is not actually a dam at Columbia Falls, just a stream gauge

ColumbiaFallsFlowData <- function() {
	return(FlowCOL)
}
COLInc <- function() {
	COLInc_o <- ColumbiaFallsFlowData() - HungryHorseFlowData()
	return(COLInc_o)
}
COLIn <- function() {
	COLIn_o <- HHOutflow() + COLInc()
	return(COLIn_o)
}
COLOut <- function() {
	COLOut_o <- COLIn()
	return(COLOut_o)
}

#######################################################
#---------------------- KERR DAM ---------------------#
#######################################################

KEFullPoolVol <- 1792000 # Storage at 2893 ft. of pool elevation
KEBotVol <- 572800 # Storage at 2883 ft. of pool elevation
KEAvgMin <- 3000

InitKE <- function() {
	InitKE_o <- ResInitFractionFull * KEFullPoolVol
	return(InitKE_o)
}
Kerr <- function() {
	if (week_counter == 1) {
		Kerr_o <- InitKE()
	} else {
		Kerr_o <- reservoir_vol_df$FLAPO[week_counter - 1]
	}
	return(Kerr_o)
}
KerrFlowData <- function() {
	return(FlowKE)
}
KEInc <- function() {
	KEInc_o <- KerrFlowData() - ColumbiaFallsFlowData()
	return(KEInc_o)
}
KerrElev_ft <- function() {
	upper_vol <- KE_elev_input$Volume[which(KE_elev_input$Volume >= Kerr())[1]]
	lower_vol <- KE_elev_input$Volume[tail(which(KE_elev_input$Volume <= Kerr())[1],1)]
	upper_el <- KE_elev_input$Elevation[which(KE_elev_input$Volume >= Kerr())[1]]
	lower_el <- KE_elev_input$Elevation[tail(which(KE_elev_input$Volume <= Kerr())[1],1)]
	if (is.na(lower_el)) {
		KerrElev_ft_o <- min(KE_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		KerrElev_ft_o <- max(KE_elev_input$Elevation)
	} else if (upper_el == lower_el) {
		KerrElev_ft_o <- lower_el
	} else {
		KerrElev_ft_o <- lower_el + (Kerr() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(KerrElev_ft_o)
}
KENetHead <- function() {
	KETailElev <- 2706 # Average tailwater elevation.  Units ft.
	KENetHead_o <- KerrElev_ft() - KETailElev
	return(KENetHead_o)
}
KEPenLimit <- function() {
	KEPenCap <- 14350 # cfs
	KEPenLimit_o <- KEPenCap * cfsTOafw
	return(KEPenLimit_o)
}
KEIn <- function() {
	KEIn_o <- HHPrelim() + (KerrFlowData() - HungryHorseFlowData())
	return(KEIn_o)
}
KEPreInflow <- function() {
	KEPreInflow_o <- max(min(HHPrelim() + HHCombSup_c, HHRelLimit()), HHDamProtectRel()) + KerrFlowData() - HungryHorseFlowData()
	KEPreInflow_c <<- KEPreInflow_o
	return(KEPreInflow_o)
}
KEInflow <- function() {
	KEInflow_o <- COLOut() + KEInc()
	return(KEInflow_o)
}

################# Kerr max and min releases ####################################

KEMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {	
		if (week_in_year < 23) {
			KEMinRefill_o <- max(KEAssuredRefill(), KECriticalCurveMin())
		} else {
			KEMinRefill_o <- KEMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		KEMinRefill_o <- KEBotVol
	}
	return(KEMinRefill_o)
}
KEMinReq <- function() {
	KEMinReq_1 <- Article_56() * cfsTOafw
	KEMinReq_o <- min(max(Kerr() + KEIn() - KEMinRefill(), KEAvgMin * cfsTOafw), KEMinReq_1)
	return(KEMinReq_o)
}
KEDamProtectRel <- function() {
	KEDamProtectRel_o <- max(0, KerrFlowData() + KEPreInflow() - KEFullPoolVol)
	return(KEDamProtectRel_o)
}
KERelLimit <- function() {
	KERelLimit_o <- max(Kerr() + KEPreInflow_c - KEBotVol, 0)
	return(KERelLimit_o)
}
KEAvailWater <- function() {
	KEAvailWater_o <- max(Kerr() + KEIn() - KEBotVol, 0)
	return(KEAvailWater_o)
}

################## Kerr rule curves ####################################

KEFloodCurve <- function() {
	KEFlood_o <- KEFlood_input[week_in_year, 2] # 1965 Memorandum of Understanding between Montana Power Company and U.S.A.C.E. (Montana Power Co., 35 F.P.C. 250 (1966)) 
	return(KEFlood_o)
}
KETopVol <- function() {
	KETopVol_o <- KEFloodCurve()
	return(KETopVol_o)
}
KERuleReq <- function() {
	KERuleReq_o <- max(Kerr() + KEIn() - KETopVol(), 0)
	return(KERuleReq_o)
}
KEPrelim <- function() {
	KEPrelim_o <- min(KEAvailWater(), max(KERuleReq(), KEMinReq()))
	return(KEPrelim_o)
}
KECriticalCurve <- function() { 
	KECriticalCurve_o <- KECriticalCurve_input$CRC1[week_in_year]
	return(KECriticalCurve_o)
}
KECriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		KECriticalCurveMin_o <- KECriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		KECriticalCurveMin_o <- KECriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		KECriticalCurveMin_o <- KECriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		KECriticalCurveMin_o <- KECriticalCurve_input$CRC4[week_in_year]
	}
	return(KECriticalCurveMin_o)
}
KEAssuredRefill <- function() { 
	KEAssuredRefill_o <- KEAssuredRefill_input[week_in_year, 2]
	return(KEAssuredRefill_o)
}
Kerr_ORC <- function() {
	Kerr_ORC_o <- min(max(KEAssuredRefill(), KECriticalCurve()), KEFloodCurve())
	return(Kerr_ORC_o)
}

################## Kerr fish flow ####################################

######  Article_56
# License Requirement for Kerr - FERC License No. 5, Article 56

Article_56 <- function() {
	Article_56_o <- Article_56_input[week_in_year, 2]
	return(Article_56_o)
}

################### Kerr energy ######################################

KEPreEnergy <- function() {
	KEPreEnergy_o <- MWhr_per_ftAcFt * min(KEPrelim(), KEPenLimit()) * KENetHead() * KECombEfficiency
	return(KEPreEnergy_o)
}
KerrGrPreEnergy <- function() {
	KerrGrPreEnergy_o <- CBPreEnergy() + KEPreEnergy() + NOXPreEnergy()
	return(KerrGrPreEnergy_o)
}
KESharedWater <- function() {
	KESharedWater_o <- max(Kerr() + KEIn() - KEPrelim() - max(KEMinRefill(), KECriticalCurveMin()), 0)
	return(KESharedWater_o)
}
KEDownStreamHead <- function() {
	KEDownStreamHead_o <- AFNetHead() + BCNetHead() + BDNetHead() + CBNetHead() + NOXNetHead() + TotalGCHead()
	return(KEDownStreamHead_o)
}
KerrEnergyContent <- function() { 
	KerrEnergyContent_o <- KESharedWater() * (KENetHead() + KEDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(KerrEnergyContent_o)
}
Kerr_ORCSharedWater <- function() { 
	Kerr_ORCSharedWater_o <- max(0, Kerr() + KEIn() - KEPrelim() - Kerr_ORC())
	return(Kerr_ORCSharedWater_o)
}
Kerr_ORCEnergyContent <- function() {
	Kerr_ORCEnergyContent_o <- Kerr_ORCSharedWater() * (KENetHead() + KEDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(Kerr_ORCEnergyContent_o)
}
KEFirmEngSup <- function() { 
	if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
		if (TotalEnergyContent_c == 0) {
			KEFirmEngSup_o <- 0
		} else {
			KEFirmEngSup_o <- (KerrEnergyContent() + Kerr_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c			
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		KEFirmEngSup_o <- 0
	} else {
		KEFirmEngSup_o <- Kerr_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(KEFirmEngSup_o)
}
KENFEnergyContent <- function() { 
	KENFEnergyContent_o <- max(0, Kerr_ORCEnergyContent() - KEFirmEngSup())
	return(KENFEnergyContent_o)
}
KENonFirmEngSup <- function() { 
	if (TotalNFEnergyContent_c == 0) {
		KENonFirmEngSup_o <- 0
	} else {
		KENonFirmEngSup_o <- KENFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(KENonFirmEngSup_o)
}
KEFirmEngSupReq <- function() { 
	KEFirmEngSupReq_o <- min(KEPenLimit(), KEFirmEngSup() / (MWhr_per_ftAcFt * (KENetHead() + KEDownStreamHead()) * KECombEfficiency))
	return(KEFirmEngSupReq_o)
}
KENonFirmSupReq <- function() { 
	if (NonFirmEnergySw() == 1) { 
		KENonFirmSupReq_o <- min(KEPenLimit(), (KEFirmEngSup() + KENonFirmEngSup()) / (MWhr_per_ftAcFt * (KENetHead() + KEDownStreamHead()) * KECombEfficiency))
	} else {
		KENonFirmSupReq_o <- 0
	}
	return(KENonFirmSupReq_o)
}
KerrEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) { 
		KerrEnergySup_o <- max(min(KEFirmEngSupReq(), KESharedWater()), min(KENonFirmSupReq(), Kerr_ORCSharedWater()))
	} else {
		KerrEnergySup_o <- max(min(KEFirmEngSupReq(), Kerr_ORCSharedWater()), min(KENonFirmSupReq(), Kerr_ORCSharedWater()))
	}
	return(KerrEnergySup_o)
}
KECombUpSup <- function() {
	if (HHCombSup_c == -9999) {
		KECombUpSup_o <- HHCombSup()
	} else {
		KECombUpSup_o <- HHCombSup_c
	}
	return(KECombUpSup_o)
}
KECombSup <- function() { 
	KECombSup_o <- KerrEnergySup() + KECombUpSup()
	KECombSup_c <<- KECombSup_o
	return(KECombSup_o)
}
KECombUpProtect <- function() {
	outflow <- KEPrelim() + KECombSup_c
	KECombUpProtect_o <- max(min(Kerr() + KEPreInflow() - outflow - Kerr_ORC()), 0)
	KECombUpProtect_c <<- KECombUpProtect_o
	return(KECombUpProtect_o)
}
KEDamProtectExcess <- function() {
	KEDamProtectExcess_o <- max(0, KEDamProtectRel() - KEPrelim() - KECombSup_c - KECombUpProtect())
	return(KEDamProtectExcess_o)
}

################ Kerr final release #####################

KERelease <- function() {
	KERelease_o <- max(min(KEPrelim() + KECombSup() + KECombUpProtect(), KERelLimit()), KEDamProtectRel())
	KERelease_c <<- KERelease_o
	return(KERelease_o)
}
KEOutflow <- function() {  
	KEOutflow_o <- KERelease_c
	return(KEOutflow_o)
}

#######################################################
#--------------- THOMPSON FALLS DAM ------------------#
#######################################################

ThompsonFlowData <- function() {
	return(FlowTF)
}
TFInc <- function() {
	TFInc_o <- ThompsonFlowData() - KerrFlowData()
	return(TFInc_o)
}
TFNetHead <- function() {
	TFNetHead_o <- 55 # FERC No. 1869 (docket P-1869)
	return(TFNetHead_o)
}
TFPenLimit <- function() {
	TFPenCap <- 23252 # FERC License no. 2042-013	
	TFPenLimit_o <- TFPenCap * cfsTOafw
	return(TFPenLimit_o)
}
TFIn <- function() {
	TFIn_o <- KERelease_c + TFInc()
	return(TFIn_o)
}
TFPrelim <- function() {
	TFPrelim_o <- KEPrelim() + TFInc()
	return(TFPrelim_o)
}
TFPreEnergy <- function() {
	TFPreEnergy_o <- min(TFPrelim(), TFPenLimit()) * TFNetHead() * TFCombEfficiency * MWhr_per_ftAcFt
	return(TFPreEnergy_o)
}
TFOut <- function(){
	TFOut_o <- TFIn()
	return(TFOut_o)
}

#######################################################
#-------------- NOXON RAPIDS DAM ---------------------#
#######################################################

NoxonFlowData <- function() {
	return(FlowNOX)
}
NOXInc <- function() {
	NOXInc_o <- NoxonFlowData() - ThompsonFlowData()
	return(NOXInc_o)
}
NOXNetHead <- function() {
	NOXNetHead_o <- 152  # forebay range: 2321 - 2331 ft. Average tailwater elveation: 2177 ft. Meeting with Avista (Klint Kalich, 9/19/2022)
	return(NOXNetHead_o)
}
NOXPenLimit <- function() {
	NOXPenCap <- 51000 # Meeting with Avista (Klint Kalich, 9/19/2022), changed 5/25/2023 from 50000
	NOXPenLimit_o <- NOXPenCap * cfsTOafw
	return(NOXPenLimit_o)
}
NOXIn <- function() {
	NOXIn_o <- TFOut() + NOXInc()
	return(NOXIn_o)
}
NOXPrelim <- function() {
	NOXPrelim_o <- TFPrelim() + NOXInc()
	return(NOXPrelim_o)
}
NOXPreEnergy <- function() {
	NOXPreEnergy_o <-  min(NOXPrelim(), NOXPenLimit()) * NOXNetHead() * NOXCombEfficiency * MWhr_per_ftAcFt
	return(NOXPreEnergy_o)
}
NOXOut <- function() {
	NOXOut_o <- NOXIn()
	return(NOXOut_o)
}

#######################################################
#-------------- CABINET GORGE DAM --------------------#
#######################################################

CabinetFlowData <- function() {
	return(FlowCB)
}
CBInc <- function() {
	CBInc_o <- CabinetFlowData() - NoxonFlowData()
	return(CBInc_o)
}
CBNetHead <- function() {
	CBNetHead_o <- 97.2  # forebay range: 2168 - 2175 ft. Average tailwater elevation: 2073.5 ft. Meeting with Avista (Klint Kalich, 9/19/2022)
	return(CBNetHead_o)
}
CBPenLimit <- function() {
	CBPenCap <- 39000 # Meeting with Avista (Klint Kalich, 9/19/2022), changed 5/25/2023 from 35700
	CBPenLimit_o <- CBPenCap * cfsTOafw
	return(CBPenLimit_o)
}
CBIn <- function() {
	CBIn_o <- NOXOut() + CBInc()
	return(CBIn_o)
}
CBPrelim <- function() {
	CBPrelim_o <- NOXPrelim() + CBInc()
	return(CBPrelim_o)
}
CBPreEnergy <- function() {
	CBPreEnergy_o <- MWhr_per_ftAcFt * min(CBPrelim(), CBPenLimit()) * CBNetHead() * CBCombEfficiency
	return(CBPreEnergy_o)
}
CBOut <- function() {
	CBOut_o <- CBIn()
	return(CBOut_o)
}

#######################################################
#--------------- ALBENI FALLS DAM --------------------#
#######################################################

AFFullPoolVol <- 1540000 # Volume corresponding to 2062.5 ft of elevation.  Normal full pool.  Units acre-ft.
AFBotVol <- 384800 # Volume corresponding to 2049.7 ft elevation.
AFAvgMin <- 4000 # Minimum project outflow, cfs

InitAF <- function() {
	InitAF_o <- ResInitFractionFull * AFFullPoolVol
	return(InitAF_o)
}
AlbeniFalls <- function() {
	if (week_counter == 1) {
		AlbeniFalls_o <- InitAF()
	} else {
		AlbeniFalls_o <- reservoir_vol_df$ALBEN[week_counter - 1]
	}
	return(AlbeniFalls_o)
}
AlbeniFallsFlowData <- function() {
	return(FlowAF)
}
AFInc <- function() {
	AFInc_o <- AlbeniFallsFlowData() - CabinetFlowData()
	return(AFInc_o)
}
AFElev_ft <- function() {
	upper_vol <- AF_elev_input$Volume[which(AF_elev_input$Volume >= AlbeniFalls())[1]]
	lower_vol <- AF_elev_input$Volume[tail(which(AF_elev_input$Volume <= AlbeniFalls())[1],1)]
	upper_el <- AF_elev_input$Elevation[which(AF_elev_input$Volume >= AlbeniFalls())[1]]
	lower_el <- AF_elev_input$Elevation[tail(which(AF_elev_input$Volume <= AlbeniFalls())[1],1)]
	if (is.na(lower_el)) {
		AFElev_ft_o <- min(AF_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		AFElev_ft_o <- max(AF_elev_input$Elevation)
	} else if (upper_el == lower_el) {
		AFElev_ft_o <- lower_el
	} else {
		AFElev_ft_o <- lower_el + (AlbeniFalls() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(AFElev_ft_o)
}
AFNetHead <- function() {
	AFTailElev <- 2036 # https://pweb.crohms.org/dd/common/dataquery/www/?k=albeni%20fals. Changed from 2042.2 on 5/25/23
	AFNetHead_o <- AFElev_ft() - AFTailElev
	return(AFNetHead_o)
}
AFPenLimit <- function() {
	AFPenCap <- 33000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Albeni-Falls/#top
	AFPenLimit_o <- AFPenCap * cfsTOafw
	return(AFPenLimit_o)
}
AFIn <- function() {
	AFIn_o <- CBPrelim() + AFInc()
	return(AFIn_o)
}
AFPreInflow <- function() {
	AFPreInflow_o <- KERelease() + AlbeniFallsFlowData() - KerrFlowData()
	AFPreInflow_c <<- AFPreInflow_o
	return(AFPreInflow_o)
}
AFInflow <- function() {
	AFInflow_o <- CBOut() + AFInc()
	return(AFInflow_o)
}

############ Albeni Falls max and min releases ################### 

AFMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {	
		if (week_in_year < 23) {
			AFMinRefill_o <- max(AFAssuredRefill(), AFCriticalCurveMin())
		} else {
			AFMinRefill_o <- AFMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		AFMinRefill_o <- AFBotVol
	}
	return(AFMinRefill_o)
}
AFMinReq <- function() {
	AFMinReq_o <- AFAvgMin * cfsTOafw
	return(AFMinReq_o)
}
AFDamProtectRel <- function() {
	AFDamProtectRel_o <- max(AlbeniFalls() + AFPreInflow_c - AFFullPoolVol, 0)
	return(AFDamProtectRel_o)
}
AFRelLimit <- function() {
	AFRelLimit_o <- max(AlbeniFalls() + AFPreInflow() - AFBotVol, 0)	
	return(AFRelLimit_o)
}
AFAvailWater <- function() {
	AFAvailWater_o <- max(AlbeniFalls() + AFIn() - AFBotVol, 0)
	return(AFAvailWater_o)
}

########### Albeni Falls rule curves ##############

AFFloodCurve <- function() {
	AFFloodCurve_o <- AFFlood_input$Max[week_in_year]
	return(AFFloodCurve_o)
}
AFTopVol <- function() {
	if (TopRuleSw() == 0) {
		AFTopVol_o <- AFFloodCurve()
	} else if (TopRuleSw() == 1) {
		AFTopVol_o <- AFFullPoolVol
	} else if (TopRuleSw() == 1) {
		AFTopVol_o <- AFFlood_input$Max[week_in_year]
	}
	return(AFTopVol_o)
}
AFRuleReq <- function() {
	AFRuleReq_o <- max(AlbeniFalls() + AFIn() - AFTopVol(), 0)
	return(AFRuleReq_o)
}
AFPrelim <- function() {
	AFPrelim_o <- min(AFAvailWater(), max(AFRuleReq(), AFMinReq()))
	AFPrelim_c <<- AFPrelim_o
	return(AFPrelim_o)
}
AFCriticalCurve <- function() { 
	AFCriticalCurve_o <- AFCriticalCurve_input$CRC1[week_in_year]
	return(AFCriticalCurve_o)
}
AFCriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		AFCriticalCurveMin_o <- AFCriticalCurve_input$CRC4[week_in_year]
	}
	return(AFCriticalCurveMin_o)
}
AFAssuredRefill <- function() { 
	AFAssuredRefill_o <- AFAssuredRefill_input[week_in_year, 2]
	return(AFAssuredRefill_o)
}
AF_ORC <- function() {
	AF_ORC_o <- min(max(AFAssuredRefill(), AFCriticalCurve()), AFFloodCurve())
	return(AF_ORC_o)
}

###### Albeni Falls energy ####################

AFPreEnergy <- function() {
	AFPreEnergy_o <- MWhr_per_ftAcFt * min(AFPrelim(), AFPenLimit()) * AFNetHead() * AFCombEfficiency
	return(AFPreEnergy_o)
}
AFSharedWater <- function() {
	AFSharedWater_o <- max(AlbeniFalls() + AFIn() - AFPrelim() - max(AFMinRefill(), AFCriticalCurveMin()), 0)
	return(AFSharedWater_o)
}
AFDownStreamHead <- function() {
	AFDownStreamHead_o <- BCNetHead() + BDNetHead() + TotalGCHead()
	return(AFDownStreamHead_o)
}
AFEnergyContent <- function() { 
	AFEnergyContent_o <- AFSharedWater() * (AFNetHead() + AFDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(AFEnergyContent_o)
}
AF_ORCSharedWater <- function() { 
	AF_ORCSharedWater_o <- max(0, AlbeniFalls() + AFIn() - AFPrelim() - AF_ORC())
	return(AF_ORCSharedWater_o)
}
AF_ORCEnergyContent <- function() {
	AF_ORCEnergyContent_o <- AF_ORCSharedWater() * (AFNetHead() + AFDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(AF_ORCEnergyContent_o)
}
AFFirmEngSup <- function() { 
	if (UseTotalEnergyContentForFirm() == 1) { # Default = 1
		if (TotalEnergyContent_c == 0) {
			AFFirmEngSup_o <- 0
		} else {
			AFFirmEngSup_o <- (AFEnergyContent() + AF_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c			
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		AFFirmEngSup_o <- 0
	} else {
		AFFirmEngSup_o <- AF_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(AFFirmEngSup_o)
}
AFNFEnergyContent <- function() { 
	AFNFEnergyContent_o <- max(0, AF_ORCEnergyContent() - AFFirmEngSup())
	return(AFNFEnergyContent_o)
}
AFNonFirmEngSup <- function() { 
	if (TotalNFEnergyContent_c == 0) {
		AFNonFirmEngSup_o <- 0
	} else {
		AFNonFirmEngSup_o <- AFNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit()
	}
	return(AFNonFirmEngSup_o)
}
AFFirmEngSupReq <- function() { 
	AFFirmEngSupReq_o <- min(AFPenLimit(), AFFirmEngSup() / (MWhr_per_ftAcFt * (AFNetHead() + AFDownStreamHead()) * AFCombEfficiency))
	return(AFFirmEngSupReq_o)
}
AFNonFirmSupReq <- function() { 
	if (NonFirmEnergySw() == 1) { 
		AFNonFirmSupReq_o <- min(AFPenLimit(), (AFFirmEngSup() + AFNonFirmEngSup()) / (MWhr_per_ftAcFt * (AFNetHead() + AFDownStreamHead()) * AFCombEfficiency))
	} else {
		AFNonFirmSupReq_o <- 0
	}
	return(AFNonFirmSupReq_o)
}
AFEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) { 
		AFEnergySup_o <- max(min(AFFirmEngSupReq(), AFSharedWater()), min(AFNonFirmSupReq(), AF_ORCSharedWater()))
	} else {
		AFEnergySup_o <- max(min(AFFirmEngSupReq(), AF_ORCSharedWater()), min(AFNonFirmSupReq(), AF_ORCSharedWater()))
	}
	return(AFEnergySup_o)
}
AFCombUpSup <- function() {
	if (KECombSup_c == -9999) {
		AFCombUpSup_o <- KECombSup()
	} else {
		AFCombUpSup_o <- KECombSup_c
	}
	return(AFCombUpSup_o)
}
AFCombSup <- function() { 
	AFCombSup_o <- AFEnergySup() + AFCombUpSup()
	AFCombSup_c <<- AFCombSup_o
	return(AFCombSup_o)
}
AFCombUpProtect <- function() {
	outflow <- AFPrelim() + AFCombSup_c
	AFCombUpProtect_o <- max(min(AlbeniFalls() + AFPreInflow() - outflow - AF_ORC(), KEDamProtectExcess()), 0)
	AFCombUpProtect_c <<- AFCombUpProtect_o
	return(AFCombUpProtect_o)
}
AFDamProtectExcess <- function() {
	AFDamProtectExcess_o <- max(0, AFDamProtectRel() - AFPrelim() - AFCombSup_c - AFCombUpProtect_c)
	return(AFDamProtectExcess_o)
}

#### Albeni Falls final release ########

AFRelease <- function() {
  AFRelease_o <- max(min(AFPrelim() + AFCombSup() + AFCombUpProtect(), AFRelLimit()), AFDamProtectRel())
  AFRelease_c <<- AFRelease_o
  return(AFRelease_o)
}
AFOutflow <- function() {
	AFOutflow_o <- AFRelease_c
	return(AFOutflow_o)
}
#######################################################
#----------------- BOX CANYON DAM --------------------#
#######################################################

BoxCanyonFlowData <- function() {
	return(FlowBC)
}
BCInc <- function() {
	BCInc_o <- BoxCanyonFlowData() - AlbeniFallsFlowData() 
	return(BCInc_o)
}
BCPenLimit <- function() {
	BoxCanyonPenCap <- 27400 # FERC License no. 2042-013	
	BCPenLimit_o <- BoxCanyonPenCap * cfsTOafw
	return(BCPenLimit_o)
}
BCNetHead <- function() {  
	BCNetHead_o <- 46 # https://popud.org/projects/box-canyon-dam/
	return(BCNetHead_o)
}
BCIn <- function() {
	BCIn_o <- AFRelease_c + BCInc()
	return(BCIn_o)
}
BCPrelim <- function() {
	BCPrelim_o <- AFPrelim() + BCInc()
	return(BCPrelim_o)
}
BCPreEnergy <- function() {
	BCPreEnergy_o <- min(BCPrelim(), BCPenLimit()) * BCNetHead() * BCCombEfficiency * MWhr_per_ftAcFt
	return(BCPreEnergy_o)
}
BCOut <- function(){
	BCOut_o <- BCIn()
	return(BCOut_o)
}

#######################################################
#----------------- BOUNDARY DAM ----------------------#
#######################################################

BoundaryFlowData <- function() {
	return(FlowBD)
}
BDInc <- function() {
	BDInc_o <- BoundaryFlowData() - BoxCanyonFlowData()
	return(BDInc_o)
}
BDPenLimit <- function() {
	BDPenCap <- 55000 # https://www.seattle.gov/light/Boundary/Relicense/docs/Study_03_TDG_Final_Report_03_09.pdf. (FERC No. 2144)
	BDPenLimit_o <- BDPenCap * cfsTOafw
	return(BDPenLimit_o)
}
BDNetHead <- function() {
	BDNetHead_o <- 261 # FERC No. 2144 
	return(BDNetHead_o)
}
BDIn <- function() {
	BDIn_o <- BCOut() + BDInc()
	return(BDIn_o)
}
BDPrelim <- function() {
	BDPrelim_o <- BCPrelim() + BDInc() 
	return(BDPrelim_o)
}
BDPreEnergy <- function() {
	BDPreEnergy_o <- MWhr_per_ftAcFt * min(BDPrelim(), BDPenLimit()) * BDNetHead() * BDCombEfficiency
	return(BDPreEnergy_o)
}
BDOut <- function() {
	BDOut_o <- BDIn()
	return(BDOut_o)
}

#######################################################
#-------------- GRAND COULEE DAM ---------------------#
#######################################################

GCFullPoolVol <- 9107400 ## Volume at 1290 ft.
GCBotVol <- 3921900 ## Volume at 1208 ft.
GCAvgMin <- 30000 # Reported in Assured Operating Plans (AOPs)

InitGC <- function() {
	InitGC_o <- ResInitFractionFull * GCFullPoolVol
	return(InitGC_o)
}
GrandCoulee <- function() {
	if (week_counter == 1) {
		GrandCoulee_o <- InitGC()
	} else {
		GrandCoulee_o <- reservoir_vol_df$GCOUL[week_counter - 1]
	}
	return(GrandCoulee_o)
}
GrandCouleeFlowData <- function() {
	return(FlowGC)
}
GCInc <- function() {
	GCInc_o <- GrandCouleeFlowData() - BoundaryFlowData() - ArrowFlowData() - CorraLinnFlowData()
	return(GCInc_o)
}
GCElev_ft <- function() {
	upper_vol <- GC_elev_input$Volume[which(GC_elev_input$Volume >= GrandCoulee())[1]]
	lower_vol <- GC_elev_input$Volume[tail(which(GC_elev_input$Volume <= GrandCoulee())[1],1)]
	upper_el <- GC_elev_input$Elevation[which(GC_elev_input$Volume >= GrandCoulee())[1]]
	lower_el <- GC_elev_input$Elevation[tail(which(GC_elev_input$Volume <= GrandCoulee())[1],1)]
	if (is.na(lower_el)) {
		GCElev_ft_o <- min(GC_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		GCElev_ft_o <- max(GC_elev_input$Elevation)
	} else if (lower_el == upper_el) {
		GCElev_ft_o <- lower_el
	} else {
		GCElev_ft_o <- lower_el + (GrandCoulee() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(GCElev_ft_o)
}
GCNetHead <- function() {
	GCTailElev <- 961 # Changed 5/25/23 from 947. Source: https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=grand%20coulee
	GCNetHead_o <- GCElev_ft() - GCTailElev
	return(GCNetHead_o)
}
GCPenLimit <- function() {
	GCPenCap <- 292000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Grand-Coulee/
	GCPenLimit_o <- GCPenCap * cfsTOafw
	return(GCPenLimit_o)
}
GCIn <- function() { # We do not calculate Prelim() for the immediately upstream dam, Boundary, so we need to subtract the Boundary demands
	GCIn_o <- BDPrelim() + ARPrelim() + CLPrelim() + GCInc()
	return(GCIn_o)
}
GCPreInflow <- function() {
	GCPreInflow_o <- AFRelease() + max(min(ARPrelim() + ARCombSup_c + ARCombUpProtect(), ARRelLimit()), ARDamProtectRel()) +
		CLRelease() + (GrandCouleeFlowData() - ArrowFlowData() - CorraLinnFlowData() - AlbeniFallsFlowData())
	GCPreInflow_c <<- GCPreInflow_o
	return(GCPreInflow_o)
}
GCInflow <- function() {
	GCInflow_o <- BDOut() + AROutflow() + CLRelease_c + GCInc()
	return(GCInflow_o)
}

################## Grand Coulee max and min releases #################################### 

GCMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {
		if (week_in_year < 23) {
			GCMinRefill_o <- max(GCAssuredRefill(), GCCriticalCurveMin())
		} else {
			GCMinRefill_o <- GCMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		GCMinRefill_o <- GCBotVol
	}
	return(GCMinRefill_o)
}
GCMinReq <- function() {
	GCMinReq_o <- GCAvgMin * cfsTOafw
	return(GCMinReq_o)
}
GCDamProtectRel <- function() {
	GCDamProtectRel_o <- max(0, GrandCoulee() + GCPreInflow() - GCFullPoolVol)
	return(GCDamProtectRel_o)
}
GCRelLimit <- function() {
	GCRelLimit_o <- max(GrandCoulee() + GCPreInflow_c - GCBotVol, 0)
	return(GCRelLimit_o)
}
GCAvailWater <- function() {
	GCAvailWater_o <- max(0, GrandCoulee() + GCIn_c - GCBotVol)
	return(GCAvailWater_o)
}

################## Grand Coulee rule curves ####################################

# 2015 flood control curve. The Grand Coulee flood curve depends forecasted inflow at The Dalles, 
# corrected for upstream refill
GCFloodCurve <- function() {
	if (CorrectedDARunoffAprAug < 57E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood1[week_in_year]
	} else if (CorrectedDARunoffAprAug < 60E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood1[week_in_year] - (GCFlood_input$GCFlood1[week_in_year] - GCFlood_input$GCFlood2[week_in_year]) / (60E6 - 57E6) * (CorrectedDARunoffAprAug - 57E6)
	} else if (CorrectedDARunoffAprAug < 63.25E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood2[week_in_year] - (GCFlood_input$GCFlood2[week_in_year] - GCFlood_input$GCFlood3[week_in_year]) / (63.25E6 - 60E6) * (CorrectedDARunoffAprAug - 60E6)
	} else if (CorrectedDARunoffAprAug < 65E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood3[week_in_year] - (GCFlood_input$GCFlood3[week_in_year] - GCFlood_input$GCFlood4[week_in_year]) / (65E6 - 63.25E6) * (CorrectedDARunoffAprAug - 63.25E6)
	} else if (CorrectedDARunoffAprAug < 67.66E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood4[week_in_year] - (GCFlood_input$GCFlood4[week_in_year] - GCFlood_input$GCFlood5[week_in_year]) / (67.66E6 - 65E6) * (CorrectedDARunoffAprAug - 65E6)
	} else if (CorrectedDARunoffAprAug < 71.0E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood5[week_in_year] - (GCFlood_input$GCFlood5[week_in_year] - GCFlood_input$GCFlood6[week_in_year]) / (71.0E6 - 67.66E6) * (CorrectedDARunoffAprAug - 67.66E6)
	} else if (CorrectedDARunoffAprAug < 75.0E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood6[week_in_year] - (GCFlood_input$GCFlood6[week_in_year] - GCFlood_input$GCFlood7[week_in_year]) / (75.0E6 - 71.0E6) * (CorrectedDARunoffAprAug - 71.0E6)
	} else if (CorrectedDARunoffAprAug < 80.0E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood7[week_in_year] - (GCFlood_input$GCFlood7[week_in_year] - GCFlood_input$GCFlood8[week_in_year]) / (80.0E6 - 75.0E6) * (CorrectedDARunoffAprAug - 75.0E6)
	} else if (CorrectedDARunoffAprAug <= 95.0E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood8[week_in_year]
	} else if (CorrectedDARunoffAprAug < 100.0E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood8[week_in_year] - (GCFlood_input$GCFlood8[week_in_year] - GCFlood_input$GCFlood9[week_in_year]) / (100.0E6 - 95.0E6) * (CorrectedDARunoffAprAug - 95.0E6)	
	} else if (CorrectedDARunoffAprAug <= 115.0E6) {
		GC_CurFC_o <- GCFlood_input$GCFlood9[week_in_year] - (GCFlood_input$GCFlood9[week_in_year] - GCFlood_input$GCFlood10[week_in_year]) / (115.0E6 - 100.0E6) * (CorrectedDARunoffAprAug - 100.0E6)			
	} else {
		GC_CurFC_o <- GCFlood_input$GCFlood10[week_in_year]
	}
	return(GC_CurFC_o)
}
GCBeginRefill <- function() { # the beginning of the flood control refill period, (2022 Draft Water Management, pg. 32)
	if (is.na(start_refill_wk_GC)) {
		GCBeginRefill_o <- 40
	} else {
		GCBeginRefill_o <- max(40, start_refill_wk_GC)
	}
	return(GCBeginRefill_o)
}
GCTopVol <- function() {
	if (TopRuleSw() == 0) {
		if (week_in_year >= GCBeginRefill()) {
			GCTopVol_o <- GCFullPoolVol
		} else {
			GCTopVol_o <- min(GCSummerDraftLimit(), GCFloodCurve())
		}
	} else if (TopRuleSw() == 1) {
		GCTopVol_o <- GCFullPoolVol
	} else if (TopRuleSw() == 2) {
		GCTopVol_o <- GCFlood_input$GCFlood1[week_in_year]
	}
	return(GCTopVol_o)
}
GC_April_Target <- function() {
	if (CorrectedDARunoffAprAug < 57E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood1[36]
	} else if (CorrectedDARunoffAprAug < 60E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood1[36] - (GCFlood_input$GCFlood1[36] - GCFlood_input$GCFlood2[36]) / (60E6 - 57E6) * (CorrectedDARunoffAprAug - 57E6)
	} else if (CorrectedDARunoffAprAug < 63.25E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood2[36] - (GCFlood_input$GCFlood2[36] - GCFlood_input$GCFlood3[36]) / (63.25E6 - 60E6) * (CorrectedDARunoffAprAug - 60E6)
	} else if (CorrectedDARunoffAprAug < 65E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood3[36] - (GCFlood_input$GCFlood3[36] - GCFlood_input$GCFlood4[36]) / (65E6 - 63.25E6) * (CorrectedDARunoffAprAug - 63.25E6)
	} else if (CorrectedDARunoffAprAug < 67.66E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood4[36] - (GCFlood_input$GCFlood4[36] - GCFlood_input$GCFlood5[36]) / (67.66E6 - 65E6) * (CorrectedDARunoffAprAug - 65E6)
	} else if (CorrectedDARunoffAprAug < 71.0E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood5[36] - (GCFlood_input$GCFlood5[36] - GCFlood_input$GCFlood6[36]) / (71.0E6 - 67.66E6) * (CorrectedDARunoffAprAug - 67.66E6)
	} else if (CorrectedDARunoffAprAug < 75.0E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood6[36] - (GCFlood_input$GCFlood6[36] - GCFlood_input$GCFlood7[36]) / (75.0E6 - 71.0E6) * (CorrectedDARunoffAprAug - 71.0E6)
	} else if (CorrectedDARunoffAprAug < 80.0E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood7[36] - (GCFlood_input$GCFlood7[36] - GCFlood_input$GCFlood8[36]) / (80.0E6 - 75.0E6) * (CorrectedDARunoffAprAug - 75.0E6)
	} else if (CorrectedDARunoffAprAug <= 95.0E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood8[36]
	} else if (CorrectedDARunoffAprAug < 100.0E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood8[36] - (GCFlood_input$GCFlood8[36] - GCFlood_input$GCFlood9[36]) / (100.0E6 - 95.0E6) * (CorrectedDARunoffAprAug - 95.0E6)	
	} else if (CorrectedDARunoffAprAug <= 115.0E6) {
		GC_AprilFC_o <- GCFlood_input$GCFlood9[36] - (GCFlood_input$GCFlood9[36] - GCFlood_input$GCFlood10[36]) / (115.0E6 - 100.0E6) * (CorrectedDARunoffAprAug - 100.0E6)			
	} else {
		GC_AprilFC_o <- GCFlood_input$GCFlood10[36]
	}
	GC_April_Target_o <- GC_AprilFC_o
	return(GC_April_Target_o)
}
GCRuleReq <- function() {
	GCRuleReq_o <- max(GrandCoulee() + GCIn_c - GCTopVol(), 0)
	return(GCRuleReq_o)
}
GCPrelim <- function() {
	GCPrelim_o <- min(GCAvailWater(), max(GCRuleReq(), GCMinReq()))
	return(GCPrelim_o)
}
GCLowerLimit <- function() {
	GCLL_o <- lower_limit_input$GrandCoulee[week_in_year]
	return(GCLL_o)
}
GCCriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		GCCriticalCurveMin_o <- GCCriticalCurve_input$CRC4[week_in_year]
	}
	return(GCCriticalCurveMin_o)
}
GCCriticalCurve <- function() {
	GCCriticalCurve_o <- GCCriticalCurve_input$CRC1[week_in_year]
	return(GCCriticalCurve_o)
}
GCAssuredRefill <- function() {
	GCAssuredRefill_o <- GCAssuredRefill_input$GrandCoulee[week_in_year]
	return(GCAssuredRefill_o)
}	
GCVariableRefill <- function() {
	if (week_in_year >= GCBeginRefill()) {
		GCRefillCurve_o <- GCVariableRefillCurve
	} else {
		GCRefillCurve_o <- GCFullPoolVol
	}
	return(GCRefillCurve_o)
}

# The Gifford-Inchelium ferry cannot operate if the reservoir is below 1232 ft of elevation
# This storage is used as a lower bound for the BECC and VECC curve (https://www.scenicwa.com/poi/inchelium-gifford-ferry).  Units acre-ft.
GCFerryLimit <- function() {
	GCFerryLimit_o <- GC_elev_input$Volume[GC_elev_input$Elevation == 1232] # Storage at 1232 ft.
	return(GCFerryLimit_o)
}
GCResidualInflow <- function() { # 85% exceedence flow for reguluated inflow to Grand Coulee
	GCResidualInflow_o <- GCResidualInflow_input[week_in_year,2]
	return(GCResidualInflow_o)
}
PRIncInflow <- function() { # 75% exceedence incremental flow between Priest Rapids and Grand Coulee
	PRIncInflow_o <- PRInc_inflow_input[week_in_year,2]
	return(PRIncInflow_o)
}

## The variable draft limit is the lower rule curve for the period 1 Jan to 10 April. It is the minimum storage 
## required to meet the April 10 flood target. It for the regulated inflow and discharge to meet the Vernita Bar flow target.
## Described in 2022 Draft Water Management Plan, Section 7.4
GCVariableDraftLimit <- function() {
	if (week_in_year>=23 && week_in_year<=36) {
		GCVariableLimit_o <- min(GCFloodCurve(), GC_April_Target() - GCResidualInflow() - PRIncInflow() + GCBdgtForVB())
	} else {
		GCVariableLimit_o <- GCFullPoolVol ## The variable draft limit does not apply after April 10
	}
	return(GCVariableLimit_o)
}

## The variable draft limit cannot fall below the lower limit unless required for flood control
GC_VDL <- function() {  
	GC_VDL_o <- min(max(GCVariableDraftLimit(), GCLowerLimit()), GCFloodCurve())
	return(GC_VDL_o)
}

## Recreation requirements at Grand Coulee require that the VECC is limited to 1280 ft of elevation (storage=8.712e6 acre ft) from June 30 to Labor Day.
## This model uses end-of-month storage targets to drive the model
## calculations so the draft limit is in essence a lower bound for the end of month VECC for June, July, and August. Units acre-ft.
GCRecLimit <- function() {
	if (week_in_year %in% c(49:52, 1:6)) {
		GCRecLimit_o <- GC_elev_input$Volume[GC_elev_input$Elevation == 1280] # https://www.lrf.org/lake-roosevelt/operations/recreation
	} else {
		GCRecLimit_o <- 0
	}	
	return(GCRecLimit_o)
}
GC_ORC <- function() {
	GC_ORC_o <- min(max(min(max(GCAssuredRefill(), GCCriticalCurve()), GCVariableRefill()), GCLowerLimit(), GCRecLimit(), GCFerryLimit()), GCSummerDraftLimit(), GC_VDL())
	return(GC_ORC_o)
}

################## Grand Coulee fish flows ###################

## Grand Coulee is drafted in July and August to support salmon migration. If The Dalles runoff forecast is >= 92 MAF, the target elevation of 
## Lake Roosevelt is 1280 ft, otherwise it is 1278 ft (2009 Water Management Plan)
GCSummerDraftLimit <- function() {
	if (DARunoffAprAug >= 92.0E6) {
		GCSummerDraft_o <- GCSummerDraft_input$Draft1[week_in_year]
	} else {
		GCSummerDraft_o <- GCSummerDraft_input$Draft2[week_in_year]
	}
	return(GCSummerDraft_o)
}
	
## The Vernita Bar Agreement sets minimum flow during spawning of fall Chinook (approximately August through November) of 70 kcfs. The required flow
## is 50 kcsf during hatching and emergence (December through March). The Spring flow BiOp at Priest rapids is 135 kcfs from April 10 to June 30
## VernitaBarFlowTarget combines 135 kcfs spring flow objectives at Priest Rapids (1998 NMFS Supplemental Opinion, see 2000 FCRPS BiOp) and the Vernita Bar Agreement ("Hanford Reach Fall Chinook Protection Program")
GCBdgtForVB <- function() { 
	GCBdgtForVB_o <- GCBdgtForVB_input[week_in_year, 2] ## Date to April 10 discharge volume (Hanford Reach fall Chinook protection program) 
	return(GCBdgtForVB_o)
}
VernitaBarFlowTarget <- function() { 
	VernitaBarFlowTarget_o <- VernitaBarFlowTarget_input[week_in_year, 2] 
	return(VernitaBarFlowTarget_o)
}
GCVernitaBarAvailWater <- function() {
	GCVernitaBarAvailWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GC_ORC())
	return(GCVernitaBarAvailWater_o)
}
GCSupForVernitaBar <- function() { 
	GCSupForVernitaBar_o <- max(min(GCVernitaBarAvailWater(), VernitaBarFlowTarget() * cfsTOafw - (GCPrelim() + PriestRapidsFlowData() - GrandCouleeFlowData())), 0)
	if (is.na(water_df$GCSupForVernitaBar[week_counter])) {
		water_df$GCSupForVernitaBar[week_counter] <<- GCSupForVernitaBar_o
	}
	return(GCSupForVernitaBar_o)
}
GCMcNaryDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
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
	} else if (FishOverRefillSw()==0) {
		GCMcNaryDraftLimit_o <- GC_ORC()
	}
	return(GCMcNaryDraftLimit_o)
}
GCMcNarySharedWater <- function() {
	GCMcNarySharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCMcNaryDraftLimit())
	return(GCMcNarySharedWater_o)
}
GCMcNarySup <- function() {
	if (TotalMcNarySharedWater_c == 0) {
		GCMcNarySup_o <- 0
	} else {
		GCMcNarySup_o <- min(GCMcNarySharedWater(), McNaryFlowDeficit() * GCMcNarySharedWater() / TotalMcNarySharedWater_c)
	}
	return(GCMcNarySup_o)
}
GCBONDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
		GCBONDraftLimit_o <- GCBotVol
	} else if (week_in_year < 28) {
		GCBONDraftLimit_o <- BONDraftLimit_input[week_in_year,2]
	} else {
		GCBONDraftLimit_o <- GC_ORC()
	}
	return(GCBONDraftLimit_o)
}
GCBONSharedWater <- function() {
	GCBONSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCBONDraftLimit())
	return(GCBONSharedWater_o)
}
GCBONSup <- function() {
	if (TotalBONSharedWater_c == 0) {
		GCBONSup_o <- 0
	} else {
		GCBONSup_o <- min(GCBONSharedWater(), BONFlowDeficit() * GCBONSharedWater() / TotalBONSharedWater_c)
	} 
	return(GCBONSup_o)
}
GCFishSup <- function() {
	GCFishSup_o <- max(GCBONSup(), GCSupForVernitaBar(), GCMcNarySup())
	return(GCFishSup_o)
}

############## Grand Coulee energy #################

GCPreEnergy <- function() {
	GCPreEnergy_o <- MWhr_per_ftAcFt * min(GCPrelim(), GCPenLimit()) * GCNetHead() * GCCombEfficiency
	return(GCPreEnergy_o)
}
GCSharedWater <- function() {
	GCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCFishSup() - min(max(GCMinRefill(), GCCriticalCurveMin()), GC_ORC()))
	return(GCSharedWater_o)
}
GCDownStreamHead <- function() {
	GCDownStreamHead_o <- BONNetHead() + CJNetHead() + DANetHead() + JDNetHead() + MCNetHead() + PRNetHead() + RINetHead() + RRNetHead() + WANetHead() + WENetHead()
	return(GCDownStreamHead_o)
}
TotalGCHead = function() {
	TotalGCHead_o <- GCNetHead() + GCDownStreamHead()
	return(TotalGCHead_o)
}
GCEnergyContent <- function() {
	GCEnergyContent_o <- GCSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCEnergyContent_o)
}
GC_ORCSharedWater <- function() {
	GC_ORCSharedWater_o <- max(0, GrandCoulee() + GCIn_c - GCPrelim() - GCFishSup() - GC_ORC())
	return(GC_ORCSharedWater_o)
}
GC_ORCEnergyContent <- function() {
	GC_ORCEnergyContent_o <- GC_ORCSharedWater() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GC_ORCEnergyContent_o)
}
GCFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			GCFirmEngSup_o <- 0
		} else {
			GCFirmEngSup_o <- (GCEnergyContent() + GC_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		GCFirmEngSup_o <- 0
	} else {
		GCFirmEngSup_o <- GC_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	if (is.na(water_df$GCFirmEngSup[week_counter])) {
		water_df$GCFirmEngSup[week_counter] <<- GCFirmEngSup_o
	}
	return(GCFirmEngSup_o)
}
GCNFEnergyContent <- function() {
	GCNFEnergyContent_o <- max(0, GC_ORCEnergyContent() - GCFirmEngSup())
	return(GCNFEnergyContent_o)
}
GCNonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		GCNonFirmEngSup_o <- 0
	} else {
		GCNonFirmEngSup_o <- GCNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
	return(GCNonFirmEngSup_o)
}
GCFirmEngSupReq <- function() {
	GCFirmEngSupReq_o <- min(GCPenLimit(), GCFirmEngSup() / (MWhr_per_ftAcFt * TotalGCHead() * GCCombEfficiency))
	return(GCFirmEngSupReq_o)
}
GCNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw() == 1) {
		GCNonFirmEngSupReq_o <- min(GCPenLimit(), (GCFirmEngSup() + GCNonFirmEngSup()) / (MWhr_per_ftAcFt * (GCNetHead() + GCDownStreamHead()) * GCCombEfficiency))
	} else {
		GCNonFirmEngSupReq_o <- 0
	}
	return(GCNonFirmEngSupReq_o)
}
GCEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		GCEnergySup_o <- max(min(GCFirmEngSupReq(), GCSharedWater()), min(GCNonFirmEngSupReq(), GC_ORCSharedWater()))
	} else {
		GCEnergySup_o <- max(min(GCFirmEngSupReq(), GC_ORCSharedWater()), min(GCNonFirmEngSupReq(), GC_ORCSharedWater()))
	}
	return(GCEnergySup_o)
}
GCCombUpSup <- function() {
	if (AFCombSup_c == -9999) {
		GCCombUpSup_1 <- AFCombSup() + ARCombSup() + CLCombSup()
	} else {
		GCCombUpSup_1 <- AFCombSup_c + ARCombSup_c + CLCombSup_c
	}
	if (refill_cat4 == 0) {
		if (week_in_year > 14) {
			GCCombUpSup_o <- GCCombUpSup_1
		} else {
			inflow <- GCIn_c + GCCombUpSup_1
			outflow <- GCPrelim() + GCEnergySup() + GCFishSup()
			min_draft <- min(max(GCMinRefill(), GCCriticalCurveMin()), GCBONDraftLimit(), GC_ORC())
			GCCombUpSup_o <- min(max(GrandCoulee() + inflow - outflow - min_draft, 0), GCCombUpSup_1)
		}
	} else if (refill_cat4 == 1) {
		inflow <- GCIn_c + GCCombUpSup_1
		outflow <- GCPrelim() + GCEnergySup() + GCFishSup()
		min_draft <- min(max(GCMinRefill(), GCCriticalCurveMin()), GCBONDraftLimit(), GC_ORC())
		GCCombUpSup_o <- min(max(GrandCoulee() + inflow - outflow - min_draft, 0), GCCombUpSup_1)
	} else if (refill_cat4 == 2) {
		inflow <- GCIn_c + GCCombUpSup_1
		outflow <- GCPrelim() + GCEnergySup() + GCFishSup()
		GCCombUpSup_o <- min(max(GrandCoulee() + inflow - outflow - GC_ORC(), 0), GCCombUpSup_1)
	}		
	return(GCCombUpSup_o)
}
GCCombSup <- function() {
	GCCombSup_o <- GCFishSup() + GCCombUpSup() + GCEnergySup()
	GCCombSup_c <<- GCCombSup_o
	return(GCCombSup_o)
}
GCFishSupEnergy <- function() { # Energy produced by releasing water to meet fish targets at McNary, Vernita Bar, and Bonneville
	GCFishSupEnergy_o <- GCFishSup() * TotalGCHead() * Estimated_Efficiency * MWhr_per_ftAcFt
	return(GCFishSupEnergy_o)
}

######################## Additional storage in case of high flow at The Dalles ############################### 

GCFloodSpace <- function() {
	GCFloodSpace_o <- min(GCPrelim() + GCCombSup_c, max(0, GCFullPoolVol - (GrandCoulee() + GCIn_c - GCPrelim() - GCCombSup_c)))
	return(GCFloodSpace_o)
}
GCFloodRelSharedWater <- function() {
	GCFloodRelSharedWater_o <- max(0, GrandCoulee() + GCPreInflow_c - GCPrelim() - GCCombSup() - GC_ORC())
	GCFloodRelSharedWater_c <<- GCFloodRelSharedWater_o	
	return(GCFloodRelSharedWater_o)
}
GCMinFloodRelReq <- function() { 
	GCMinFloodRelReq_o <- min(GCFloodRelSharedWater(), MinFloodRelReq())
	GCMinFloodRelReq_c <<- GCMinFloodRelReq_o
	water_df$GCMinFloodRelReq[week_counter] <<- GCMinFloodRelReq_c
	return(GCMinFloodRelReq_o)
}
GCCombUpProtect <- function() {
	excess <- AFDamProtectExcess() + ARDamProtectExcess() + CLDamProtectExcess() + AFCombUpProtect_c + ARCombUpProtect_c + CLCombUpProtect_c
	outflow <- GCPrelim() + GCCombSup_c
	GCCombUpProtect_o <- max(min(GrandCoulee() + GCPreInflow_c - outflow - GC_ORC(), excess), 0)
	GCCombUpProtect_c <<- GCCombUpProtect_o
	return(GCCombUpProtect_o)
}
GCDamProtectExcess <- function() {
	GCDamProtectExcess_o <- max(0, GCDamProtectRel() - GCPrelim() - GCCombSup_c - GCCombUpProtect())
	return(GCDamProtectExcess_o)
}

##################### Grand Coulee final release ###################################

GCRelease <- function() {
	GCRelease_o <- max(min(GCPrelim() + GCCombSup_c - TotalRelReducReq_c + GCMinFloodRelReq_c + GCCombUpProtect(), GCRelLimit()), GCDamProtectRel() + (GCInflow() - GCPreInflow()), GCMinReq())
	return(GCRelease_o)
}
GCOutflow <- function() {
	GCOutflow_o <- GCRelease_c
	return(GCOutflow_o)
}	

#################### Controlled flow at The Dalles ###################################


## The initial controlled flow guides refill during the flood control refill period, which begins 3 weeks before the 
## unregulated flow at The Dalles is forecasted to exceed 450 cfs (FCOP, 2003). 
ICF <- function() {
	if (week_in_year == 1 || is.na(start_refill_wk)) {
		InitialControlledFlow_previous <<- 1e10
		ICF_o <- InitialControlledFlow_previous
	} else if (week_in_year %in% start_refill_wk) {
		ICF_o <- InitialControlledFlow
		InitialControlledFlow_previous <<- ICF_o
		if (week_in_year == start_refill_wk) {
			control_wk <<- week_counter
		}
	} else {
		ICF_o <- InitialControlledFlow_previous
	}
	return(ICF_o)
}

## The controlled flow is adusted based on the fraction of storage space that has been refilled in the category 4 reservoirs
Cat4FilledStorPerc <- function() {
	if (control_wk == -9999 || control_wk == week_counter) {
		storage_o <- 0
	} else {
		storage_o <- 100 * max(0, 1.0 - (((GCFullPoolVol - GrandCoulee()) + (ARFullPoolVol - Arrow())) / 
			((GCFullPoolVol - reservoir_vol_df$GCOUL[control_wk]) + (ARFullPoolVol - reservoir_vol_df$ARROW[control_wk]))))
	}
	return(storage_o)
}

## Residual inflow calculation made at the start of flood control refill period
DACorrectedResidualInflow <- function() { 
	if (week_in_year == 1 || is.na(start_refill_wk)) {
		DACorrectedResidualInflow_previous <<- DACorrectedResidualInflowAprAug
		DACorrectedResidualInflow_o <- DACorrectedResidualInflow_previous
	} else if (week_in_year == start_refill_wk) {
		DACorrectedResidualInflow_o <- DACorrectedResidualInflowAprAug
		DACorrectedResidualInflow_previous <<- DACorrectedResidualInflow_o
	} else {
		DACorrectedResidualInflow_o <- DACorrectedResidualInflow_previous
	} 
	return(DACorrectedResidualInflow_o)
}
ICF_adj <- function() { ## Adjustment to initial controlled flow 
	if (DACorrectedResidualInflow() < 40e6) {
		colnum <- 2
	} else if (DACorrectedResidualInflow() < 50e6) {
		colnum <- 3
	} else {
		colnum <- 4
	}
	upper_row <- tail(which(ICF_adj_input$filled_perc >= Cat4FilledStorPerc()), 1)
	lower_row <- which(ICF_adj_input$filled_perc <= Cat4FilledStorPerc())[1]
	upper_adj <- ICF_adj_input[upper_row,colnum]
	lower_adj <- ICF_adj_input[lower_row,colnum]
	upper_perc <- ICF_adj_input$filled_perc[upper_row]
	lower_perc <- ICF_adj_input$filled_perc[lower_row]
	if (Cat4FilledStorPerc() == 100) {
		ICF_adj_o <- ICF_adj_input[1,colnum]
	} else if (Cat4FilledStorPerc() == 0) {
		ICF_adj_o <- 0
	} else {
		ICF_adj_o <- lower_adj + (upper_adj - lower_adj) / (upper_perc - lower_perc) * (Cat4FilledStorPerc() - lower_perc)
	}
	return(ICF_adj_o)
}
ControlledFlow <- function() { #cfs
	if (is.na(start_refill_wk)) { ## For years in which the unregulated flow is never forecasted to exceed 450 kcfs
		ControlledFlow_o <- 350000
	} else {
		ControlledFlow_o <- ICF() + ICF_adj()
		ControlledFlow_o <- ControlledFlow_o
	}
	water_df$ControlledFlow[week_counter] <<- ControlledFlow_o
	return(ControlledFlow_o)
}
################### CHIEF JOSEPH DAM ##############################

ChiefJosephFlowData <- function() {
	return(FlowCJ)
}
CJInc <- function() {
	CJInc_o <- ChiefJosephFlowData() - GrandCouleeFlowData()
	return(CJInc_o)
}
CJCurtail <- function() { # Curtailment of water rights provisioned on flow measured at Chief Joseph
	CJCurtail_0 <-  min(DemCJ, max(IflowCJ - GCOutflow() - CJInc(), 0))
	if (curtail_option == 1) {
		CJCurtail_o <- ifelse(CJCurtail_0 > 0, CurtCJ, 0)
	} else if (curtail_option == 2) {	
		CJCurtail_o <- CJCurtail_0
	} else if (curtail_option==3) {
		CJCurtail_o <- 0
	}
	if (DARunoffAprSep > mainstem_rule) {
		CJCurtail_o <- 0
	} else {
		CJCurtail_o <- CJCurtail_o
	}		
	return(CJCurtail_o)
}
CJInstreamShortfall <- function() { # Columbia River flow deficit measured at Chief Joseph
	CJInstreamShortfall_o = max(IflowCJ - GCOutflow() - CJInc(), 0)
	return(CJInstreamShortfall_o)
}
CJPenLimit <- function() {
	CJPenCap <- 213000 # https://www.nws.usace.army.mil/Missions/Civil-Works/Locks-and-Dams/Chief-Joseph-Dam/-Hydropower/
	CJPenLimit_o <- CJPenCap * cfsTOafw
	return(CJPenLimit_o)
}
CJPrelim <- function() {
	CJPrelim_o <- max(0, GCPrelim() + CJInc())
	return(CJPrelim_o)
}
CJNetHead <- function() {
	CJNetHead_o <- 178 # https://www.nws.usace.army.mil/Missions/Civil-Works/Locks-and-Dams/Chief-Joseph-Dam/-Hydropower/
	return(CJNetHead_o)
}
CJPreEnergy <- function() {
	CJPreEnergy_o <- MWhr_per_ftAcFt * min(CJPrelim(), CJPenLimit()) * CJNetHead() * CJCombEfficiency
	return(CJPreEnergy_o)
}
CJIn <- function() {
	CJIn_o <- GCRelease_c + CJInc()
	return(CJIn_o)
}
CJOut <- function() {
	CJOut_o <- CJIn()
	return(CJOut_o)
}

#######################################################
#------------------ CHELAN DAM -----------------------#
####################################################### 

CHFullPoolVol <- 1676000
CHBotVol <- 998600

InitCH <- function() {
	InitCH_o <- ResInitFractionFull * CHFullPoolVol
	return(InitCH_o)
}	
Chelan <- function() {
	if (week_counter == 1) {
		Chelan_o <- InitCH()
	} else {
		Chelan_o <- reservoir_vol_df$CHELA[week_counter - 1]
	}
	return(Chelan_o)
}
ChelanFlowData <- function() {
	return(FlowCH)
}
CHPenLimit <- function() {
	CHPenCap <- 2200 # Chelan settlement agreement, FERC P-637-022, 2003
	CHPenLimit_o <- CHPenCap * cfsTOafw
	return(CHPenLimit_o)
}
CHNetHead <- function() {
	CHNetHead_o <- 400 # https://hydroreform.org/resource/lake-chelan-project/ 
	return(CHNetHead_o)
}
CHIn <- function() {
	CHIn_o <- ChelanFlowData()
	return(CHIn_o)
}
CHInflow <- function() {
	CHInflow_o <- CHIn()
	return(CHInflow_o)
}

##################### Chelan max and min releases ########## 
 
CHMinReq <- function() { # Chelan settlement agreement, FERC P-637-022, 2003
	if (ChelanFlowData() <= CHFlow_percentiles$Q20[week_in_year]) {
		CHMin <- 80
	} else if (ChelanFlowData() > CHFlow_percentiles$Q80[week_in_year]) {
		if (week_in_year %in% c(51,52,1:40)) {
			CHMin <- 80
		} else {
			CHMin <- 320
		}
	} else {
		if (week_in_year %in% c(51,52,1:40)) {
			CHMin <- 80
		} else {
			CHMin <- 200
		}
	}
	CHMinReq_o <- CHMin * cfsTOafw
	return(CHMinReq_o)
}
CHDamProtectRel <- function() {
	CHDamProtectRel_o <- max(0, ChelanFlowData() + CHInflow() - CHFullPoolVol)
	return(CHDamProtectRel_o)
}
CHRelLimit <- function() {
	CHRelLimit_o <- max(Chelan() + CHInflow() - CHBotVol, 0)
	return(CHRelLimit_o)
}
CHAvailWater <- function() {
	CHAvailWater_o <- max(0, Chelan() + CHIn() - CHBotVol)
	return(CHAvailWater_o)
} 
 
############## Chelan rule curve ##################

CHFloodCurve <- function() {
	CHFloodCurve_o <- CHFlood_input$CHFlood[week_in_year]
	return(CHFloodCurve_o)
}
CHTopVol <- function() {
	if (TopRuleSw() == 0) {
		CHTopVol_o <- CHFloodCurve()
	} else if (TopRuleSw() == 1) {
		CHTopVol_o <- CHFullPoolVol
	} else if (TopRuleSw() == 2) {
		CHTopVol_o <- CHFlood_input$CHFlood[week_in_year]
	}
	return(CHTopVol_o)
}
CHRuleReq <- function() {
	CHRuleReq_o <- max(Chelan() + CHIn() - CHTopVol(), 0)
	return(CHRuleReq_o)
}
CHPrelim <- function() {
	CHPrelim_o <- min(CHAvailWater(), max(CHRuleReq(), CHMinReq()))
	return(CHPrelim_o)
}

################## Chelan energy ######################

CHPreEnergy <- function() {
	CHPreEnergy_o <- MWhr_per_ftAcFt * min(CHPrelim(), CHPenLimit()) * CHNetHead() * CHCombEfficiency
	return(CHPreEnergy_o)
} 

################## Chelan final release ###############

CHRelease <- function() {
	CHRelease_o <- max(CHDamProtectRel(), min(CHPrelim(), CHRelLimit()))
	return(CHRelease_o)
}
CHOutflow <- function() {  
	CHOutflow_o <- CHRelease_c
	return(CHOutflow_o)
}

#######################################################
#------------------- WELLS DAM -----------------------#
#######################################################

WellsFlowData <- function() {
	return(FlowWE)
}
WEInc <- function() {
	WEInc_o <- WellsFlowData() - ChiefJosephFlowData()
	return(WEInc_o)
}
WECurtail <- function() {
	WECurtail_0 <- min(DemWE, max(IflowWE - CJOut() - WEInc(), 0))
	if (curtail_option == 1) {
		WECurtail_o <- ifelse(WECurtail_0 > 0, CurtWE, 0)
	} else if (curtail_option == 2) {
		WECurtail_o <- WECurtail_0
	} else if (curtail_option == 3) {
		WECurtail_o <- 0
	}
	if (DARunoffAprSep > mainstem_rule) {
		WECurtail_o <- 0
	} else {
		WECurtail_o <- WECurtail_o
	}		
	return(WECurtail_o)
}
WEInstreamShortfall <- function() {
	WEInstreamShortfall_o = max(IflowWE - CJOut() - WEInc(), 0)
	return(WEInstreamShortfall_o)
}
WEPenLimit <- function() {
	WEPenCap <- 220000 # FERC Project No. 2149-152
	WEPenLimit_o <- WEPenCap * cfsTOafw
	return(WEPenLimit_o)
}
WEPrelim <- function() {
	WEPrelim_o <- CJPrelim() + WEInc()
	return(WEPrelim_o)
}
WENetHead <- function() {
	WENetHead_o <- 66.9 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=wells
	return(WENetHead_o)
}
WEPreEnergy <- function() {
	WEPreEnergy_o <- MWhr_per_ftAcFt * min(WEPrelim(), WEPenLimit()) * WENetHead() * WECombEfficiency
	return(WEPreEnergy_o)
}
WEIn <- function() {
	WEIn_o <- CJOut() + WEInc()
	return(WEIn_o)
}
WEOut <- function() {
	WEOut_o <- WEIn()
	return(WEOut_o)
}

#######################################################
#--------------- ROCKY REACH DAM ---------------------#
#######################################################

RockyReachFlowData <- function() {
	return(FlowRR)
}
RRInc <- function() {
	RRInc_o <- RockyReachFlowData() - WellsFlowData() - ChelanFlowData()
	return(RRInc_o)
}
RRCurtail <- function() {
	RRCurtail_0 <- min(DemRR, max(IflowRR - WEOut() - CHOutflow() - RRInc(), 0))
	if (curtail_option==1) {
		RRCurtail_o <- ifelse(RRCurtail_0 > 0, CurtRR, 0)
	} else if (curtail_option == 2) {
		RRCurtail_o <- RRCurtail_0
	} else if (curtail_option == 3) {
		RRCurtail_o <- 0
	}
	if (DARunoffAprSep > mainstem_rule) {
		RRCurtail_o <- 0
	} else {
		RRCurtail_o <- RRCurtail_o
	}		
	return(RRCurtail_o)
}
RRInstreamShortfall <- function() {
	RRInstreamShortfall_o = max(IflowRR - WEOut() - RRInc(), 0)
	return(RRInstreamShortfall_o)
}
RRPenLimit <- function() {
	RRPenCap <- 201000 # email contact Lindsey Files from Chelan PUD (8/26/2022), changed from 220000 on 5/25/23
	RRPenLimit_o <- RRPenCap * cfsTOafw
	return(RRPenLimit_o)
}
RRPrelim <- function() {
	RRPrelim_o <- WEPrelim() + CHPrelim() + RRInc()
	return(RRPrelim_o)
}
RRNetHead <- function() {
	RRNetHead_o <- 88.6 ## Varies from 96.8 to 73.4 ft, email contact Lindsey Files from Chelan PUD (8/26/2022), changed 5/25/23 from 86.5
	return(RRNetHead_o)
}
RRPreEnergy <- function() {
	RRPreEnergy_o <- MWhr_per_ftAcFt * min(RRPrelim(), RRPenLimit()) * RRNetHead() * RRCombEfficiency
	return(RRPreEnergy_o)
}
RRIn <- function() {
	RRIn_o <- WEOut() + CHRelease() + RRInc()
	return(RRIn_o)
}
RROut <- function() {
	RROut_o <- RRIn()
	return(RROut_o)
}

#######################################################
#--------------- ROCK ISLAND DAM ---------------------#
#######################################################

RockIslandFlowData <- function() {
	return(FlowRI)
}
RIInc <- function() {
	RIInc_o <- RockIslandFlowData() - RockyReachFlowData()
	return(RIInc_o)
}
RICurtail <- function() {
	RICurtail_0 <- min(DemRI, max(IflowRI - RROut() - RIInc(), 0))
	if (curtail_option==1) {
		RICurtail_o <- ifelse(RICurtail_0 > 0, CurtRI, 0)
	} else if (curtail_option==2) {
	  RICurtail_o <- RICurtail_0
	} else if (curtail_option==3) {
	  RICurtail_o <- 0
	}
	if (DARunoffAprSep > mainstem_rule) {
		RICurtail_o <- 0
	} else {
		RICurtail_o <- RICurtail_o
	}		
	return(RICurtail_o)
}
RIInstreamShortfall <- function() {
	RIInstreamShortfall_o = max(IflowRI - RROut() - RIInc(), 0)
	return(RIInstreamShortfall_o)
}
RIPenLimit <- function() {
	RIPenCap <- 217000 #email contact Lindsey Files from Chelan PUD (8/26/2022), changed from 220000 on 5/25/23
	RIPenLimit_o <- RIPenCap * cfsTOafw
	return(RIPenLimit_o)
}
RINetHead <- function() {
	RINetHead_o <- 41.5 # Varies from 29.3 to 50.4 ft. Email contact Lindsey Files from Chelan PUD (8/26/2022). Changed from 34.4 5/25/23
	return(RINetHead_o)
}
RIPrelim <- function() {
	RIPrelim_o <- RRPrelim() + RIInc()
	return(RIPrelim_o)
}
RIPreEnergy <- function() {
	RIPreEnergy_o <- MWhr_per_ftAcFt * min(RIPrelim(), RIPenLimit()) * RINetHead() * RICombEfficiency
	return(RIPreEnergy_o)
}
RIIn <- function() {
	RIIn_o <- RROut() + RIInc()
	return(RIIn_o)
}
RIOut <- function() {
	RIOut_o <- RIIn()
	return(RIOut_o)
}

#######################################################
#------------------ WANAPUM DAM ----------------------#
#######################################################

WanapumFlowData <- function() {
	flow_o <- FlowWA + RetVICWA
	return(flow_o)
}
WAInc <- function() {
	WAInc_o <- WanapumFlowData() - RockIslandFlowData()
	return(WAInc_o)
}
WACurtail <- function() {
	WACurtail_0 <- min(DemWA, max(IflowWA - RIOut() - WAInc(), 0))
	if (curtail_option == 1) {
		WACurtail_o <- ifelse(WACurtail_0 > 0, CurtWA, 0)
	} else if (curtail_option == 2) {
		WACurtail_o <- WACurtail_0
	} else if (curtail_option == 3) {
		WACurtail_o <- 0
	}
	if (DARunoffAprSep > mainstem_rule) {
		WACurtail_o <- 0
	} else {
		WACurtail_o <- WACurtail_o
	}		
	return(WACurtail_o)
}
WAInstreamShortfall <- function() {
	WAInstreamShortfall_o = max(IflowWA - RIOut() - WAInc(), 0)
	return(WAInstreamShortfall_o)
}
WAPenLimit <- function() {
	WAPenCap <- 178000 # https://www.eesi.org/files/PUDpdf.pdf
	WAPenLimit_o <- WAPenCap * cfsTOafw
	return(WAPenLimit_o)
}
WAPrelim <- function() {
	WAPrelim_o <- RIPrelim() + WAInc()
	return(WAPrelim_o)
}
WANetHead <- function() {
	WANetHead_o <- 77.8 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=wanapum
	return(WANetHead_o)
}
WAPreEnergy <- function() {
	WAPreEnergy_o <- MWhr_per_ftAcFt * min(WAPrelim(), WAPenLimit()) * WANetHead() * WACombEfficiency
	return(WAPreEnergy_o)
}
WAIn <- function() {
	WAIn_o <- RIOut() + WAInc()
	return(WAIn_o)
}
WAOut <- function() {
	WAOut_o <- WAIn()
	return(WAOut_o)
}

#######################################################
#-------------- PRIEST RAPIDS DAM --------------------#
#######################################################

PriestRapidsFlowData <- function() {
	flow_o <- FlowPR + RetVICPR
	return(FlowPR)
}
PRInc <- function() {
	PRInc_o <- PriestRapidsFlowData() - WanapumFlowData()
	return(PRInc_o)
}
PRCurtail <- function()	{
	PRCurtail_0 <- min(DemPR, max(IflowPR - WAOut() - PRInc(), 0))
	if (curtail_option == 1) {
		PRCurtail_o <- ifelse(PRCurtail_0 > 0, CurtPR, 0)
	} else if (curtail_option == 2) {
		PRCurtail_o <- PRCurtail_0 
	} else if (curtail_option == 1) {
		PRCurtail_o <- 0
	}
	if (DARunoffAprSep > mainstem_rule) {
		PRCurtail_o <- 0
	} else {
		PRCurtail_o <- PRCurtail_o
	}		
	return(PRCurtail_o)
}
PRInstreamShortfall <- function() {
	PRInstreamShortfall_o = max(IflowPR - WAOut() - PRInc(), 0)
	return(PRInstreamShortfall_o)
}
PRPenLimit <- function() {
	PRPenCap <- 178000 # https://www.ezview.wa.gov/Portals/_1962/images/FERC%20401s/priest_rapids-final_cert040307.pdf
	PRPenLimit_o <- PRPenCap * cfsTOafw
	return(PRPenLimit_o)
}
PRPrelim <- function() {
	PRPrelim_o <- WAPrelim() + PRInc()
	return(PRPrelim_o)
}
PRNetHead <- function() {
	PRNetHead_o <- 76.5 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=priest%20rapids
	return(PRNetHead_o)
}
PRPreEnergy <- function() {
	PRPreEnergy_o <- MWhr_per_ftAcFt * min(PRPrelim(), PRPenLimit()) * PRNetHead() * PRCombEfficiency
	return(PRPreEnergy_o)
}
PRIn <- function() {
	PRIn_o <- WAOut() + PRInc()
	return(PRIn_o)
}
PROut <- function() {
	PROut_o <- PRIn()
	return(PROut_o)
}

############ UPPER SNAKE SYSTEM ##########################

#####################################################
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
	return(FlowJL)
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

#####################################################
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
	return(FlowPAL)
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
	PALRuleReq_o <- max(Palisades() + PALIn() - PALTopVol(), 0)
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
	PALRelLimit_o <- max(Palisades() + PALInflow() - PALBotVol, 0)
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

#######################################################
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
	return(FlowIP)
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
	IPRuleReq_o <- max(IslandPark() + IPIn() - IPTopVol(), 0)
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
	IPRelLimit_o <- max(IslandPark() + IPInflow() - IPBotVol, 0)
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

#######################################################
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
	return(FlowRIR)
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
	RIRRuleReq_o <- max(Ririe() + RIRIn() - RIRTopVol(), 0)
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
	RIRRelLimit_o <- max(Ririe() + RIRInflow() - RIRBotVol, 0)
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

#######################################################
#---------------- AMERICAN FALLS DAM -----------------#
#######################################################

AMFullPoolVol <- 1672590
AMBotVol <- 0
AMAvgMin <- 300 # minimum monthly average gauge flow, cfs

InitAM <- function() {
	InitAM_o <- ResInitFractionFull * AMFullPoolVol
	return(InitAM_o)
}	
AmericanFalls <- function() {
	if (week_counter == 1) {
		AmericanFalls_o <- InitAM()
	} else {
		AmericanFalls_o <- reservoir_vol_df$AMERI[week_counter - 1]
	}
	return(AmericanFalls_o)
}
AmericanFallsFlowData <- function() {
	return(FlowAM)
}
AMInc <- function() {
	AMInc_o <- AmericanFallsFlowData() - IslandParkFlowData() - PalisadesFlowData() - RirieFlowData()
	return(AMInc_o)
}
AMIn <- function() {
	AMIn_o <- IPPrelim() + PALPrelim() + RIRPrelim() + AMInc()
	return(AMIn_o)
}
AMInflow <- function() {
	AMInflow_o <- IPRelease() + PALRelease() + RIRRelease() + AMInc()
	return(AMInflow_o)
}
MinidokaAgReq <- function() { ## Total withdrawals for Minidoka Irrigation Project
	ConveyanceLossMID <- 1
	IrrigationDemand <- DemMinidoka 
	MinidokaAgReq_o <- IrrigationDemand * ConveyanceLossMID
	return(MinidokaAgReq_o)
}
AMAgReq <- function() {
	if (week_in_year %in% c(45:52, 1:14)) {
		AMAgReq_o <- MinidokaAgReq() + AMIn() 
	} else {
		AMAgReq_o <- 0
	}
	return(AMAgReq_o)
}
AMMinReq <- function() {
	AMMinReq_o <- max(AMAgReq(), AMAvgMin * cfsTOafw)
	return(AMMinReq_o)
}
AMAvailWater <- function() {
	AMAvailWater_o <- max(0, AmericanFalls() + AMIn() - AMBotVol)
	return(AMAvailWater_o)
}
AMPrelim <- function() {
	AMPrelim_o <- min(AMAvailWater(), AMMinReq())
	return(AMPrelim_o)
}
AMRelLimit <- function() {
	AMRelLimit_o <- max(0, AmericanFalls() + AMInflow() - AMBotVol)
	return(AMRelLimit_o)
}
AMDamProtectRel <- function() {
	AMDamProtectRel_o <- max(0, AmericanFalls() + AMInflow() - AMFullPoolVol)
	return(AMDamProtectRel_o)
}
AMCombUpProtect <- function() {
	AMCombUpProtect_o <- IPDamProtectExcess() + PALDamProtectExcess() + RIRDamProtectExcess() + PALCombUpProtect_c
	AMCombUpProtect_c <<- AMCombUpProtect_o
	return(AMCombUpProtect_o)
}
AMDamProtectExcess <- function() {
	AMDamProtectExcess_o <- max(0, AMDamProtectRel() - AMPrelim() - AMCombUpProtect_c)
	return(AMDamProtectExcess_o)
}
AMRelease <- function() {
  AMRelease_o <- max(min(AMPrelim() + AMCombUpProtect(), AMRelLimit()), AMDamProtectRel())
  return(AMRelease_o)
}
AMOutflow <- function() {
	AMOutflow_o <- AMRelease_c
	return(AMOutflow_o)
}

#######################################################
#----------------- MINIDOKA DAM ----------------------#
#######################################################

MINFullPoolVol <- 95180
MINBotVol <- 0
MINAvgMin <- 300 # minimum average monthly flow at Neely, ID gauge near American Falls from historical data

InitMIN <- function() {
	InitMIN_o <- ResInitFractionFull * MINFullPoolVol
	return(InitMIN_o)
}	
Minidoka <- function() {
	if (week_counter == 1) {
		Minidoka_o <- InitMIN()
	} else {
		Minidoka_o <- reservoir_vol_df$MINAD[week_counter - 1]
	}
	return(Minidoka_o)
}
MinidokaFlowData <- function() {
	return(FlowMIN)
}
MINInc <- function() {
	MINInc_o <- MinidokaFlowData() - AmericanFallsFlowData()
	return(MINInc_o)
}
MINIn <- function() {
	MINIn_o <- AMPrelim() + MINInc()
	return(MINIn_o)
}
MINInflow <- function() {
	MINInflow_o <- AMRelease() + MINInc()
	return(MINInflow_o)
}
MINAgReq <- function() {
	MINAgReq_o <- 0
	return(MINAgReq_o)
}
MINMinReq <- function() {
	MINMinReq_o <- max(MINAgReq(), MINAvgMin * cfsTOafw)
	return(MINMinReq_o)
}
MINAvailWater <- function() {
	MINAvailWater_o <- max(0, Minidoka() + MINIn() - MINBotVol)
	return(MINAvailWater_o)
}
MINPrelim <- function() {
	MINPrelim_o <- min(MINAvailWater(), MINMinReq())
	return(MINPrelim_o)
}
MINRelLimit <- function() {
	MINRelLimit_o <- max(Minidoka() + MINInflow() - MINBotVol, 0)
	return(MINRelLimit_o)
}
MINDamProtectRel <- function() {
	MINDamProtectRel_o <- max(0, Minidoka() + MINInflow() - MINFullPoolVol)
	return(MINDamProtectRel_o)
}
MINCombUpProtect <- function() {
	MINCombUpProtect_o <- AMDamProtectExcess() + AMCombUpProtect_c
	MINCombUpProtect_c <<- MINCombUpProtect_o
	return(MINCombUpProtect_o)
}
MINDamProtectExcess <- function() {
	MINDamProtectExcess_o <- max(0, MINDamProtectRel() - MINPrelim() - MINCombUpProtect_c)
	return(MINDamProtectExcess_o)
}
MINRelease <- function() {
  MINRelease_o <- max(MINDamProtectRel(), min(MINPrelim() + MINCombUpProtect(), MINRelLimit()))
  return(MINRelease_o)
}
MINOutflow <- function() {
	MINOutflow_o <- MINRelease_c
	return(MINOutflow_o)
}

#######################################################
#------------------- MILNER DAM ----------------------#
#######################################################

MilnerFlowData <- function() {
	return(FlowMIL)
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
	return(FlowLUC)
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
	BoiseRuleReq_o <- max(Boise() + BoiseIn() - BoiseTopVol(), 0)
	return(BoiseRuleReq_o)
}
BoiseAvailWater <- function() {
	BoiseAvailWater_o <- max(Boise() + BoiseIn() - BoiseBotVol, 0)
	return(BoiseAvailWater_o)
}
BoisePrelim <- function() {
	BoisePrelim_o <- min(BoiseAvailWater(), max(BoiseRuleReq(), BoiseMinReq()))
	return(BoisePrelim_o)
}
BoiseRelLimit <- function() {
	BoiseRelLimit_o <- max(Boise() + BoiseInflow() - BoiseBotVol, 0)
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
	return(FlowPAY)
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
	PayetteRuleReq_o <- max(Payette() + PayetteIn() - PayetteTopVol(), 0)
	return(PayetteRuleReq_o)
}
PayetteAvailWater <- function() {
	PayetteAvailWater_o <- max(Payette() + PayetteIn() - PayetteBotVol, 0)
	return(PayetteAvailWater_o)
}
PayettePrelim <- function() {
	PayettePrelim_o <- min(PayetteAvailWater(), max(PayetteRuleReq(), PayetteMinReq()))
	return(PayettePrelim_o)
}
PayetteRelLimit <- function() {
	PayetteRelLimit_o <- max(Payette() + PayetteInflow() - PayetteBotVol, 0)
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
	return(FlowOWY)
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
	OWYRuleReq_o <- max(Owyhee() + OWYIn() - OWYTopVol(), 0)
	return(OWYRuleReq_o)
}
OWYAvailWater <- function() {
	OWYAvailWater_o <- max(Owyhee() + OWYIn() - OWYBotVol, 0)
	return(OWYAvailWater_o)
}
OWYPrelim <- function() {
	OWYPrelim_o <- min(OWYAvailWater(), max(OWYRuleReq(), OWYMinReq()))
	return(OWYPrelim_o)
}
OWYRelLimit <- function() {
	OWYRelLimit_o <- max(Owyhee() + OWYInflow() - OWYBotVol, 0)
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

#######################################################
#------------------- BROWNLEE DAM --------------------#
#######################################################

BRFullPoolVol <- 1420000 # Volume corresponding to 2077 ft of elevation.  Normal full pool.  Units acre-ft.
BRBotVol <- 444700 # Volume cooresponding to the bottom of conservation pool elevation of 1976 ft.  Units acre-ft.
BRAvgMin <- 5850 # 2016-2017 assured operating plan

InitBR <- function() {
	InitBR_o <- ResInitFractionFull * BRFullPoolVol
	return(InitBR_o)
}
Brownlee <- function() {
	if (week_counter == 1) {
		Brownlee_o <- InitBR()
	} else {
		Brownlee_o <- reservoir_vol_df$BROWN[week_counter-1]
	}
	return(Brownlee_o)
}
BrownleeFlowData <- function() {
	return(FlowBR)
}
BRInc <- function() {
	BRInc_o <- BrownleeFlowData() - MilnerFlowData() - LuckyPeakFlowData() - PayetteFlowData() - OwyheeFlowData()
	return(BRInc_o)
}
BRElev_ft <- function() {
	upper_vol <- BR_elev_input$Volume[which(BR_elev_input$Volume >= Brownlee())[1]]
	lower_vol <- BR_elev_input$Volume[tail(which(BR_elev_input$Volume <= Brownlee())[1],1)]
	upper_el <- BR_elev_input$Elevation[which(BR_elev_input$Volume >= Brownlee())[1]]
	lower_el <- BR_elev_input$Elevation[tail(which(BR_elev_input$Volume <= Brownlee())[1],1)]
	if (is.na(lower_el)) {
		BRElev_ft_o <- min(BR_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		BRElev_ft_o <- max(BR_elev_input$Elevation)		
	} else if (lower_el == upper_el) {
		BRElev_ft_o <- lower_el
	} else {
		BRElev_ft_o <- lower_el + (Brownlee() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(BRElev_ft_o)
}
BRNetHead <- function() {
	BRTailElev <- 1805 # https://storymaps.arcgis.com/stories/eae485d9d4244bd4bf93c537bd449ae7
	BRNetHead_o <- BRElev_ft() - BRTailElev
	return(BRNetHead_o)
}
BRPenLimit <- function() {
	BRPenCap <- 34500 # FERC No. 1971 License application
	BRPenLimit_o <- BRPenCap * cfsTOafw
	return(BRPenLimit_o)
}
BRIn <- function() {
	BRIn_o <- PayettePrelim() + OWYPrelim() + BoisePrelim() + MILPrelim() + BRInc()
	return(BRIn_o)
}
BRInflow <- function() {
	BRInflow_o <- PayetteRelease() + OWYRelease() + BoiseRelease() + MILOut() + BRInc()
	return(BRInflow_o)
}

################## Brownlee max and min releases ####################################

BRMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {
		if (week_in_year < 23) {
			BRMinRefill_o <- max(BRAssuredRefill(), BRCriticalCurveMin())
		} else {
			BRMinRefill_o <- BRMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		BRMinRefill_o <- BRBotVol
	}
	return(BRMinRefill_o)
}
BRMinReq <- function() {
	BRMinReq_1 <- max(BRRelForJBandLP(), BRAvgMin * cfsTOafw)
	if (FishOverRefillSw() == 1) {
		BRMinReq_o <- BRMinReq_1
	} else {
		BRMinReq_o <- min(max(Brownlee() + BRIn_c - BRMinRefill(), BRAvgMin * cfsTOafw), BRMinReq_1)
	}
	return(BRMinReq_o)
}
BRDamProtectRel <- function() {
	BRDamProt <- max(0, Brownlee() + BRInflow() - BRFullPoolVol)
	return(BRDamProt)
}
BRRelLimit <- function() { # Max allowable release
	BRRelLimit_o <- max(0, Brownlee() + BRInflow() - BRBotVol)
	return(BRRelLimit_o)
}
BRAvailWater <- function() {
	BRAvailWater_o <- max(0, Brownlee() + BRIn_c - BRBotVol)
	return(BRAvailWater_o)
}

################### Brownlee rule curves ##################################

### Flood curve for Browlee reservoir is based on the April--August forecast at The Dalles and the April--July forecast at Brownlee 
### (Hells Canyon Complex FERC No. 1971 License Application)
BRFloodCurve <- function() { 
	if (DARunoffAprAug <= 75E6) {
		if (BRRunoffAprJul <= 3E6) {
			BRFlood_o <- BRFlood_input$BR75_3[week_in_year]
		} else if (BRRunoffAprJul <= 4E6) {
			BRFlood_o <- BRFlood_input$BR75_3[week_in_year] - (BRFlood_input$BR75_3[week_in_year] - BRFlood_input$BR75_4[week_in_year]) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
		} else if (BRRunoffAprJul <= 5E6) {
			BRFlood_o <- BRFlood_input$BR75_4[week_in_year] - (BRFlood_input$BR75_4[week_in_year] - BRFlood_input$BR75_5[week_in_year]) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
		} else if (BRRunoffAprJul <= 6E6) {		
			BRFlood_o <- BRFlood_input$BR75_5[week_in_year] - (BRFlood_input$BR75_5[week_in_year] - BRFlood_input$BR75_6[week_in_year]) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
		} else {
			BRFlood_o <- BRFlood_input$BR75_6[week_in_year]
		}
	} else if (DARunoffAprAug <= 85E6) {
		if (BRRunoffAprJul <= 3E6) {
			BRFlood_o <- BRFlood_input$BR75_3[week_in_year] - (BRFlood_input$BR75_3[week_in_year] - BRFlood_input$BR85_3[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
		} else if (BRRunoffAprJul <= 4E6) {
			BRFlood_1 <- BRFlood_input$BR75_3[week_in_year] - (BRFlood_input$BR75_3[week_in_year] - BRFlood_input$BR85_3[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
			BRFlood_2 <- BRFlood_input$BR75_4[week_in_year] - (BRFlood_input$BR75_4[week_in_year] - BRFlood_input$BR85_4[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
		} else if (BRRunoffAprJul <= 5E6) {
			BRFlood_1 <- BRFlood_input$BR75_4[week_in_year] - (BRFlood_input$BR75_4[week_in_year] - BRFlood_input$BR85_4[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
			BRFlood_2 <- BRFlood_input$BR75_5[week_in_year] - (BRFlood_input$BR75_5[week_in_year] - BRFlood_input$BR85_5[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
		} else if (BRRunoffAprJul <= 6E6) {
			BRFlood_1 <- BRFlood_input$BR75_5[week_in_year] - (BRFlood_input$BR75_5[week_in_year] - BRFlood_input$BR85_5[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
			BRFlood_2 <- BRFlood_input$BR75_6[week_in_year] - (BRFlood_input$BR75_6[week_in_year] - BRFlood_input$BR85_6[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)		
		} else {
			BRFlood_o <- BRFlood_input$BR75_6[week_in_year] - (BRFlood_input$BR75_6[week_in_year] - BRFlood_input$BR85_6[week_in_year]) / (85E6 - 75E6) * (DARunoffAprAug - 75E6)
		}
	} else if (DARunoffAprAug <= 95E6) {
		if (BRRunoffAprJul <= 3E6) {
			BRFlood_o <- BRFlood_input$BR85_3[week_in_year] - (BRFlood_input$BR85_3[week_in_year] - BRFlood_input$BR95_3[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
		} else if (BRRunoffAprJul <= 4E6) {
			BRFlood_1 <- BRFlood_input$BR85_3[week_in_year] - (BRFlood_input$BR85_3[week_in_year] - BRFlood_input$BR95_3[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
			BRFlood_2 <- BRFlood_input$BR85_4[week_in_year] - (BRFlood_input$BR85_4[week_in_year] - BRFlood_input$BR95_4[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
		} else if (BRRunoffAprJul <= 5E6) {
			BRFlood_1 <- BRFlood_input$BR85_4[week_in_year] - (BRFlood_input$BR85_4[week_in_year] - BRFlood_input$BR95_4[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
			BRFlood_2 <- BRFlood_input$BR85_5[week_in_year] - (BRFlood_input$BR85_5[week_in_year] - BRFlood_input$BR95_5[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
		} else if (BRRunoffAprJul <= 6E6) {
			BRFlood_1 <- BRFlood_input$BR85_5[week_in_year] - (BRFlood_input$BR85_5[week_in_year] - BRFlood_input$BR95_5[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
			BRFlood_2 <- BRFlood_input$BR85_6[week_in_year] - (BRFlood_input$BR85_6[week_in_year] - BRFlood_input$BR95_6[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)		
		} else {
			BRFlood_o <- BRFlood_input$BR85_6[week_in_year] - (BRFlood_input$BR85_6[week_in_year] - BRFlood_input$BR95_6[week_in_year]) / (95E6 - 85E6) * (DARunoffAprAug - 85E6)
		}
	} else if (DARunoffAprAug <= 105E6) {
		if (BRRunoffAprJul <= 3E6) {
				BRFlood_o <- BRFlood_input$BR95_3[week_in_year] - (BRFlood_input$BR95_3[week_in_year] - BRFlood_input$BR105_3[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
		} else if (BRRunoffAprJul <= 4E6) {
			BRFlood_1 <- BRFlood_input$BR95_3[week_in_year] - (BRFlood_input$BR95_3[week_in_year] - BRFlood_input$BR105_3[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
			BRFlood_2 <- BRFlood_input$BR95_4[week_in_year] - (BRFlood_input$BR95_4[week_in_year] - BRFlood_input$BR105_4[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
		} else if (BRRunoffAprJul <= 5E6) {
			BRFlood_1 <- BRFlood_input$BR95_4[week_in_year] - (BRFlood_input$BR95_4[week_in_year] - BRFlood_input$BR105_4[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
			BRFlood_2 <- BRFlood_input$BR95_5[week_in_year] - (BRFlood_input$BR95_5[week_in_year] - BRFlood_input$BR105_5[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
		} else if (BRRunoffAprJul <= 6E6) {
			BRFlood_1 <- BRFlood_input$BR95_5[week_in_year] - (BRFlood_input$BR95_5[week_in_year] - BRFlood_input$BR105_5[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
			BRFlood_2 <- BRFlood_input$BR95_6[week_in_year] - (BRFlood_input$BR95_6[week_in_year] - BRFlood_input$BR105_6[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)		
		} else {
			BRFlood_o <- BRFlood_input$BR95_6[week_in_year] - (BRFlood_input$BR95_6[week_in_year] - BRFlood_input$BR105_6[week_in_year]) / (105E6 - 95E6) * (DARunoffAprAug - 95E6)
		}
	} else if (DARunoffAprAug <= 115E6) {
		if (BRRunoffAprJul <= 3E6) {
			BRFlood_o <- BRFlood_input$BR105_3[week_in_year] - (BRFlood_input$BR105_3[week_in_year] - BRFlood_input$BR115_3[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
		} else if (BRRunoffAprJul <= 4E6) {
			BRFlood_1 <- BRFlood_input$BR105_3[week_in_year] - (BRFlood_input$BR105_3[week_in_year] - BRFlood_input$BR115_3[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
			BRFlood_2 <- BRFlood_input$BR105_4[week_in_year] - (BRFlood_input$BR105_4[week_in_year] - BRFlood_input$BR115_4[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
		} else if (BRRunoffAprJul <= 5E6) {
			BRFlood_1 <- BRFlood_input$BR105_4[week_in_year] - (BRFlood_input$BR105_4[week_in_year] - BRFlood_input$BR115_4[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
			BRFlood_2 <- BRFlood_input$BR105_5[week_in_year] - (BRFlood_input$BR105_5[week_in_year] - BRFlood_input$BR115_5[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
		} else if (BRRunoffAprJul <= 6E6) {
			BRFlood_1 <- BRFlood_input$BR105_5[week_in_year] - (BRFlood_input$BR105_5[week_in_year] - BRFlood_input$BR115_5[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
			BRFlood_2 <- BRFlood_input$BR105_6[week_in_year] - (BRFlood_input$BR105_6[week_in_year] - BRFlood_input$BR115_6[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
			BRFlood_o <- BRFlood_1 - (BRFlood_1 - BRFlood_2) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)		
		} else {
			BRFlood_o <- BRFlood_input$BR105_6[week_in_year] - (BRFlood_input$BR105_6[week_in_year] - BRFlood_input$BR115_6[week_in_year]) / (115E6 - 105E6) * (DARunoffAprAug - 105E6)
		}
	} else {
		if (BRRunoffAprJul <= 3E6) {
			BRFlood_o <- BRFlood_input$BR115_3[week_in_year]
		} else if (BRRunoffAprJul <= 4E6) {
			BRFlood_o <- BRFlood_input$BR115_3[week_in_year] - (BRFlood_input$BR115_3[week_in_year] - BRFlood_input$BR115_4[week_in_year]) / (4E6 - 3E6) * (BRRunoffAprJul - 3E6)
		} else if (BRRunoffAprJul <= 5E6) {
			BRFlood_o <- BRFlood_input$BR115_4[week_in_year] - (BRFlood_input$BR115_4[week_in_year] - BRFlood_input$BR115_5[week_in_year]) / (5E6 - 4E6) * (BRRunoffAprJul - 4E6)
		} else if (BRRunoffAprJul <= 6E6) {		
			BRFlood_o <- BRFlood_input$BR115_5[week_in_year] - (BRFlood_input$BR115_5[week_in_year] - BRFlood_input$BR115_6[week_in_year]) / (6E6 - 5E6) * (BRRunoffAprJul - 5E6)
		} else {
			BRFlood_o <- BRFlood_input$BR115_6[week_in_year]
		}
	}
	return(BRFlood_o)
}
BRTopVol <- function() {
	if (TopRuleSw() == 0) {
		BRTopVol_o <- min(BRFloodCurve(), BRFallChinookDraft())
	} else if (TopRuleSw() == 1) {
		BRTopVol_o <- BRFullPoolVol
	} else if (TopRuleSw() == 2) {
		BRTopVol_o <- BRFlood_input$BR75_3[week_in_year]
	}
	return(BRTopVol_o)
}
BRRuleReq <- function() {
	BRRuleReq_o <- max(Brownlee() + BRIn() - BRTopVol(), 0)
	return(BRRuleReq_o)
}
BRPrelim <- function() {
	BRPrelim_o <- min(BRAvailWater(), max(BRRuleReq(), BRMinReq()))
	return(BRPrelim_o)
}
BRCriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		BRCriticalCurve_o <- BRCriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		BRCriticalCurve_o <- BRCriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		BRCriticalCurve_o <- BRCriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		BRCriticalCurve_o <- BRCriticalCurve_input$CRC4[week_in_year]
	}	
	return(BRCriticalCurve_o)
}
BRCriticalCurve <- function() { # Minimum storage to meet firm hydropower in a dry year (1928-1932 critical water period)
	BRCriticalCurve_o <- BRCriticalCurve_input$CRC1[week_in_year] # year 1 critical curve
	return(BRCriticalCurve_o)
}
BRAssuredRefill <- function() { # Assured refill curve based on historical Assured refill
	BRAssuredRefill_o <- BRAssuredRefill_input[week_in_year,2]
	return(BRAssuredRefill_o)
}
BRVariableRefill <- function() { # Required refill to ensure dam is full by end of year (AF)
	if (RefillSwitch() == 1) { # Default = 2 
		BRRefillCurve_o <- BRAssuredRefill()
	} else if (RefillSwitch() == 2) {
		BRRefillCurve_o <- BRVariableRefillCurve
	}
	return(BRRefillCurve_o)
}
BR_ORC <- function() {
	BR_ORC_o <- min(max(BRAssuredRefill(), BRCriticalCurve()), BRFloodCurve(), BRFallChinookDraft())
	return(BR_ORC_o)
}

########################### Brownlee fish flows ############################################

BRLGDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
		if (UseAllStorForMCNLG == 1) { # Default = 0
			BRLGDraftLimit_o <- BRBotVol
		} else {
			BRLGDraftLimit_o <- 1.183e6
		}
	} else if (FishOverRefillSw() == 0) {
		BRLGDraftLimit_o <- BR_ORC()
	}
	return(BRLGDraftLimit_o)
}
BRJBDraftLimit <- function() {
	#if (FishOverRefillSw() == 1) {
		if (UseAllStorForMCNLG == 1) { # Default = 0
			BRJBDraftLimit_o <- BRBotVol
		} else {
			BRJBDraftLimit_o <- 1.0e6
		}
	#} else if (FishOverRefillSw() == 0) {
	#	BRJBDraftLimit_o <- BR_ORC()
	#}
	return(BRJBDraftLimit_o)
}
BRLGAvailWater <- function() {
	BRLGAvailWater_o <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGDraftLimit())
	return(BRLGAvailWater_o)
}
BRJBAvailWater <- function() {
	BRJBAvailWater_o <- max(0, Brownlee() + BRIn_c - BRJBDraftLimit())
	return(BRJBAvailWater_o)
}
BRLGSup <- function() { # Release of water from Brownlee to meet Lower Granite fish flow target 
	denominator <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGDraftLimit()) +
		max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())	
	numerator <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGDraftLimit())
	if (denominator == 0) {
		BRRelForLG_1 <- 0
	} else {
		BRRelForLG_1 <- TotalRelForLowerGranite() * numerator / denominator
	}
	BRRelForLG_o <- min(BRLGAvailWater(), BRRelForLG_1)
	water_df$BRLGSup[week_counter] <<- BRRelForLG_o
	return(BRRelForLG_o)
}

##### LimePointFlowTarget
# Minimum Flows at the Lime Point (75 miles below Hells Canyon) are required to be at least 13000 cfs 95% of the time (FERC License No. 1971, Article 43) July-Sept. 
# The model assumes these flows must be maintained 100% of the time.
# In addition a minimum flow of 5000 cfs at Johnson's Bar (17 miles downstream of Hells Canyon) must be maintained.
# While in actual practice some storage is available for support of these flows from run-of-river projects,
# Brownlee must generally make sufficient average releases to support these flows on a monthly time frame.
# The model assumes that Brownlee supplies all supplements to natural inflow required to meet the targets.
JohnsonBarFlowTarget <- function() {
	JohnsonBarFlowTarget_o <- 5000 # cfs
	return(JohnsonBarFlowTarget_o)
}
LimePointFlowTarget <- function() {
	LimePointFlowTarget_o <- LimePointTarget_input[week_in_year,2] 
	return(LimePointFlowTarget_o)
}
BRRelForJohnsonsBar <- function() { # Release of water from Brownlee to meet Johnson Bar fish flow target
	IncFlow <- HellsCanyonFlowData() - BrownleeFlowData()
	BRRelForJohnsonsBar_o <- max(0, JohnsonBarFlowTarget() * cfsTOafw - (BRPrelim_c + IncFlow))
	return(BRRelForJohnsonsBar_o)
}
LimePointFlowData <- function() {
	return(FlowLimePoint)
}
LPInc <- function() {
	LPInc_o <- LimePointFlowData() - HellsCanyonFlowData()
	return(LPInc_o)
}
BRRelForLimePoint <- function() {
	IncFlow <- LimePointFlowData() - BrownleeFlowData()
	BRRelForLimePoint_o <- max(0, LimePointFlowTarget() * cfsTOafw - (BRPrelim_c + IncFlow))
	return(BRRelForLimePoint_o)
}
BRRelForJBandLP <- function() {
	BRRelForJBandLP_o <- min(BRJBAvailWater(), max(BRRelForJohnsonsBar(), BRRelForLimePoint()))
	if (is.na(water_df$BRRelForJBandLP[week_counter])) {
		water_df$BRRelForJBandLP[week_counter] <<- BRRelForJBandLP_o
	}
	return(BRRelForJBandLP_o)
}
## Brownlee is drafted Sept - December to support spawning of fall Chinook (FERC No. 1971 Licence Application)
BRFallChinookDraft <- function() {
	if (BRRunoffAprJul <= 4.26e6) {	# 20th percentile Apr--Jul inflow forecast for the period 1979-2015
		BRFallChinookDraft_o <- BRFallChinookDraft_input$Low[week_in_year]
	} else if (BRRunoffAprJul <= 7.32e6) {
		BRFallChinookDraft_o <- BRFallChinookDraft_input$Low[week_in_year] + (BRFallChinookDraft_input$Medium[week_in_year] - BRFallChinookDraft_input$Low[week_in_year]) / (7.32e6 - 4.26e6)  * (BRRunoffAprJul - 4.26e6)
	} else if (BRRunoffAprJul <= 13.2e6) {
		BRFallChinookDraft_o <- BRFallChinookDraft_input$Medium[week_in_year] + (BRFallChinookDraft_input$High[week_in_year] - BRFallChinookDraft_input$Medium[week_in_year]) / (13.2e6 - 7.32e6)  * (BRRunoffAprJul - 7.32e6)
	} else {
		BRFallChinookDraft_o <- BRFallChinookDraft_input$High[week_in_year]
	}
	return(BRFallChinookDraft_o)
}

############################# Brownlee energy ##########################

BRPreEnergy <- function() {
	BRPreEnergy_o <- MWhr_per_ftAcFt * min(BRPrelim_c, BRPenLimit()) * BRNetHead() * BRCombEfficiency
	return(BRPreEnergy_o)
}
BRSharedWater <- function() {
	BRSharedWater_o <- max(Brownlee() + BRIn_c - BRPrelim_c - BRLGSup() - max(BRMinRefill(), BRCriticalCurveMin()), 0)
	return(BRSharedWater_o)
}
BRDownStreamHead <- function() {
	BRDownStreamHead_o <- BONNetHead() + DANetHead() + JDNetHead() + MCNetHead() + IHNetHead() + 
		LMNetHead() + LIGNetHead() + LGNetHead() + HCNetHead() + OXNetHead()
	return(BRDownStreamHead_o)
}
BREnergyContent <- function() {
	BREnergyContent_o <- BRSharedWater() * (BRNetHead() + BRDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt 
	return(BREnergyContent_o)
}
BR_ORCSharedWater <- function() {
	BR_ORCSharedWater_o <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGSup() - BR_ORC())
	return(BR_ORCSharedWater_o)
}
BR_ORCEnergyContent <- function() {
	BR_ORCEnergyContent_o <- BR_ORCSharedWater() * (BRNetHead() + BRDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(BR_ORCEnergyContent_o)
}
BRFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			BRFirmEngSup_o <- 0
		} else {
			BRFirmEngSup_o <- (BREnergyContent() + BR_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		BRFirmEngSup_o <- 0
	} else {
		BRFirmEngSup_o <- BR_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	return(BRFirmEngSup_o)
}
BRNFEnergyContent <- function() {
	BRNFEnergyContent_o <- max(0, BR_ORCEnergyContent() - BRFirmEngSup())
	return(BRNFEnergyContent_o)
}
BRNonFirmEngSup <- function() {
	if (TotalNFEnergyContent_c == 0) {
		BRNonFirmEngSup_o <- 0
	} else {
		BRNonFirmEngSup_o <- BRNFEnergyContent() / TotalNFEnergyContent_c * NonFirmEnergyDeficit_c
	}
	return(BRNonFirmEngSup_o)
}
BRFirmEngSupReq <- function() {
	BRFirmEngSupReq_o <- min(BRPenLimit(), BRFirmEngSup() / (MWhr_per_ftAcFt * (BRNetHead() + BRDownStreamHead()) * BRCombEfficiency))
	return(BRFirmEngSupReq_o)
}
BRNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw() == 1) {
		BRNonFirmEngSupReq_o <- min(BRPenLimit(), (BRFirmEngSup() + BRNonFirmEngSup()) / (MWhr_per_ftAcFt * (BRNetHead() + BRDownStreamHead()) * BRCombEfficiency))
	} else {
		BRNonFirmEngSupReq_o <- 0
	}
	return(BRNonFirmEngSupReq_o)
}
BREnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		BREnergySup_o <- max(min(BRFirmEngSupReq(), BRSharedWater()), min(BRNonFirmEngSupReq(), BR_ORCSharedWater()))
	} else {
		BREnergySup_o <- max(min(BRFirmEngSupReq(), BR_ORCSharedWater()), min(BRNonFirmEngSupReq(), BR_ORCSharedWater()))
	}
	return(BREnergySup_o)
}
BRCombSup <- function() {
	BRCombSup_o <- BRLGSup() + BREnergySup()
	BRCombSup_c <<- BRCombSup_o
	return(BRCombSup_o)
}
BRLGSupEnergy <- function() {
	BRLGSupEnergy_o <- BRLGSup() * (BRNetHead() + BRDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(BRLGSupEnergy_o)
}

############# Brownlee final release ############################

BRCombUpProtect <- function() {
	excess <- PayetteDamProtectExcess() + BoiseDamProtectExcess() + OWYDamProtectExcess() + MINDamProtectExcess() + MINCombUpProtect_c	
	inflow <- BRIn_c + excess
	outflow <- BRPrelim_c + BRCombSup_c
	BRCombUpProtect_o <- max(min(Brownlee() + inflow - outflow - BR_ORC(), excess), 0)
	BRCombUpProtect_c <<- BRCombUpProtect_o
	return(BRCombUpProtect_o)
}
BRDamProtectExcess <- function() {
	BRDamProtectExcess_o <- max(0, BRDamProtectRel() - BRPrelim_c - BRCombSup_c - BRCombUpProtect())
	return(BRDamProtectExcess_o)
}
BRRelease <- function() {
	BRRelease_o <- max(min(BRPrelim_c + BRCombSup() + BRCombUpProtect(), BRRelLimit()), BRDamProtectRel())
	return(BRRelease_o)
}
BROutflow <- function() {
	BROutflow_o <- BRRelease_c
	return(BROutflow_o)
}

#######################################################
#--------------------- OXBOW DAM ---------------------#
#######################################################

OxbowFlowData <- function() {
	return(FlowOX)
}
OXNetHead <- function() {
	OXNetHead_o <- 117 # FERC No. 1971 Licese Application
	return(OXNetHead_o)
}
OXPenLimit <- function() {
	OXPenCap <- 25000 # https://www.cbr.washington.edu/hydro/oxbow
	OXPenLimit_o <- OXPenCap * cfsTOafw
	return(OXPenLimit_o)
}
OXIn <- function() {
	OXIn_o <- BROutflow()
	return(OXIn_o)
}
OXPrelim <- function() {
	OXPrelim_o <- BRPrelim_c
	return(OXPrelim_o)
}
OXPreEnergy <- function() {
	OXPreEnergy_o <- MWhr_per_ftAcFt * min(OXPrelim(), OXPenLimit()) * OXNetHead() * OXCombEfficiency
	return(OXPreEnergy_o)
}
OXOut <- function() {
	OXOut_o <- OXIn()
	return(OXOut_o)
}

#######################################################
#----------------- HELLS CANYON DAM ------------------#
#######################################################

HellsCanyonFlowData <- function() {
	return(FlowHC)
}
HCInc <- function() {
	HCInc_o <- HellsCanyonFlowData() - OxbowFlowData()
	return(HCInc_o)
}
HCPenLimit <- function() {
	HCPenCap <- 9000 # FERC No. 1971 Licese Application
	HCPenLimit_o <- HCPenCap * cfsTOafw
	return(HCPenLimit_o)
}
HCNetHead <- function() {
	HCNetHead_o <- 210 # FERC No. 1971 Licese Application
	return(HCNetHead_o)
}
HCPrelim <- function() {
	HCPrelim_o <- OXPrelim() + HCInc()
	return(HCPrelim_o)
}
HCPreEnergy <- function() {
	HCPreEnergy_o <- MWhr_per_ftAcFt * min(HCPrelim(), HCPenLimit()) * HCNetHead() * HCCombEfficiency
	return(HCPreEnergy_o)
}
HCIn <- function() {
	HCIn_o <- OXOut() + HCInc()
	return(HCIn_o)
}
HCOut <- function() {
	HCOut_o <- HCIn()
	return(HCOut_o)
}

#######################################################
#------------------- DWORSHAK DAM --------------------#
#######################################################

DWFullPoolVol <- 3468000 # Storage at pool elevation of 1600 ft.
DWBotVol <- 1452800 # Volume cooresponding to the bottom of conservation pool elevation of 1445 ft.  Units acre-ft.
DWAvgMin <- 1300 

InitDW <- function() {
	InitDW_o <- ResInitFractionFull * DWFullPoolVol
	return(InitDW_o)
}
Dworshak <- function() {
	if (week_counter == 1) {
		Dworshak_o <- InitDW()
	} else {
		Dworshak_o <- reservoir_vol_df$DWORS[week_counter - 1]
	}
	return(Dworshak_o)
}
DworshakFlowData <- function() {
	return(FlowDW)
}
DWElev_ft <- function() {
	upper_vol <- DW_elev_input$Volume[which(DW_elev_input$Volume >= Dworshak())[1]]
	lower_vol <- DW_elev_input$Volume[tail(which(DW_elev_input$Volume <= Dworshak())[1],1)]
	upper_el <- DW_elev_input$Elevation[which(DW_elev_input$Volume >= Dworshak())[1]]
	lower_el <- DW_elev_input$Elevation[tail(which(DW_elev_input$Volume <= Dworshak())[1],1)]
	if (is.na(lower_el)) {
		DWElev_ft_o <- min(DW_elev_input$Elevation)
	} else if (is.na(upper_el)) {
		DWElev_ft_o <- max(DW_elev_input$Elevation)		
	} else if (lower_el == upper_el) {
		DWElev_ft_o <- lower_el				
	} else {
		DWElev_ft_o <- lower_el + (Dworshak() - lower_vol) / (upper_vol - lower_vol) * (upper_el - lower_el)
	}
	return(DWElev_ft_o)
}
DWNetHead <- function() {
	DWTailElev <- 975 # https://www.nwd-wc.usace.army.mil/dd/common/dataquery/www/?k=dworshak; changed from 980 5/25/23
	DWNetHead_o <- DWElev_ft() - DWTailElev
	return(DWNetHead_o)
}
DWPenLimit <- function() {
	DWPenCap <- 10500 # Changed from 24000 on 5/25/23. Source: https://www.nwd.usace.army.mil/CRSO/Project-Locations/Dworshak/#top
	DWPenLimit_o <- DWPenCap * cfsTOafw
	return(DWPenLimit_o)
}
DWIn <- function() {
	DWIn_o <- DworshakFlowData()
	return(DWIn_o)
}
DWInflow <- function() {
	DWInflow_o <- DWIn()
	return(DWInflow_o)
}

############ Dworshak max and min releases ###################

DWMinRefill <- function() {
	if (GuranteeRefillSw() == 1) {	
		if (week_in_year < 23) {
			DWMinRefill_o <- max(DWAssuredRefill(), DWCriticalCurveMin())
		} else {
			DWMinRefill_o <- DWMinRefillCurve
		}
	} else if (GuranteeRefillSw() == 2) {
		DWMinRefill_o <- DWBotVol
	}
	return(DWMinRefill_o)
}
DWMinReq <- function() {
	DWMinReq_o <- DWAvgMin * cfsTOafw
	return(DWMinReq_o)
}
DWDamProtectRel <- function() {
	DWDamProt <- max(0, Dworshak() + DWInflow() - DWFullPoolVol)
	return(DWDamProt)
}
DWRelLimit <- function() {
	DWRelLimit_o <- max(Dworshak() + DWInflow() - DWBotVol, 0)
	return(DWRelLimit_o)
}
DWAvailWater <- function() {
	DWAvailWater_o <- max(0, Dworshak() + DWIn() - DWBotVol)
	return(DWAvailWater_o)
}

################### Dworshak rule curves ##################################

# 1992 Local flood control curve
DWFloodCurve <- function() {
	if (DWRunoffAprJul < 1.2E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood1[week_in_year]
	} else if (DWRunoffAprJul < 1.4E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood1[week_in_year] - (DWFlood_input$DWFlood1[week_in_year] - DWFlood_input$DWFlood2[week_in_year]) / (1.4E6 - 1.2E6) * (DWRunoffAprJul - 1.2E6)
	} else if (DWRunoffAprJul < 1.8E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood2[week_in_year] - (DWFlood_input$DWFlood2[week_in_year] - DWFlood_input$DWFlood3[week_in_year]) / (1.8E6 - 1.4E6) * (DWRunoffAprJul - 1.4E6)
	} else if (DWRunoffAprJul < 2.2E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood3[week_in_year] - (DWFlood_input$DWFlood3[week_in_year] - DWFlood_input$DWFlood4[week_in_year]) / (2.2E6 - 1.8E6) * (DWRunoffAprJul - 1.8E6)
	} else if (DWRunoffAprJul < 2.6E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood4[week_in_year] - (DWFlood_input$DWFlood4[week_in_year] - DWFlood_input$DWFlood5[week_in_year]) / (2.6E6 - 2.2E6) * (DWRunoffAprJul - 2.2E6)
	} else if (DWRunoffAprJul < 3.0E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood5[week_in_year] - (DWFlood_input$DWFlood5[week_in_year] - DWFlood_input$DWFlood6[week_in_year]) / (3.0E6 - 2.6E6) * (DWRunoffAprJul - 2.6E6)
	} else if (DWRunoffAprJul < 3.2E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood6[week_in_year] - (DWFlood_input$DWFlood6[week_in_year] - DWFlood_input$DWFlood7[week_in_year]) / (3.2E6 - 3.0E6) * (DWRunoffAprJul - 3.0E6)
	} else if (DWRunoffAprJul < 3.4E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood7[week_in_year] - (DWFlood_input$DWFlood7[week_in_year] - DWFlood_input$DWFlood8[week_in_year]) / (3.4E6 - 3.2E6) * (DWRunoffAprJul - 3.2E6)
	} else if (DWRunoffAprJul < 3.6E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood8[week_in_year] - (DWFlood_input$DWFlood8[week_in_year] - DWFlood_input$DWFlood9[week_in_year]) / (3.6E6 - 3.4E6) * (DWRunoffAprJul - 3.4E6)
	} else if (DWRunoffAprJul < 3.8E6) {
		DW_CurFC_o <- DWFlood_input$DWFlood9[week_in_year] - (DWFlood_input$DWFlood9[week_in_year] - DWFlood_input$DWFlood10[week_in_year]) / (3.8E6 - 3.6E6) * (DWRunoffAprJul - 3.6E6)
	} else {
		DW_CurFC_o <- DWFlood_input$DWFlood10[week_in_year]
	} 
	return(DW_CurFC_o)
}
DWTopVol <- function() {
	if (TopRuleSw() == 0) {
		DWTopVol_o <- min(DWBiOpDraftLimit(), DWFloodCurve())
	} else if (TopRuleSw() == 1) {
		DWTopVol_o <- DWFullPoolVol
	} else if (TopRuleSw() == 2) {
		DWTopVol_o <- DWFlood_input$DWFlood1[week_in_year]
	}
	return(DWTopVol_o)
}
DWRuleReq <- function() {
	DWRuleReq_o <- max(Dworshak() + DWIn() - DWTopVol(), 0)
	return(DWRuleReq_o)
}
DWPrelim <- function() {
	DWPrelim_o <- min(DWAvailWater(), max(DWRuleReq(), DWMinReq()))
	return(DWPrelim_o)
}
DWLowerLimit <- function() {
	DWLL_o <- lower_limit_input$Dworshak[week_in_year]
	return(DWLL_o)
}
DWCriticalCurveMin <- function() {
	if (CriticalCurveSw == 1) {
		DWCriticalCurve_o <- DWCriticalCurve_input$CRC1[week_in_year]
	} else if (CriticalCurveSw == 2) {
		DWCriticalCurve_o <- DWCriticalCurve_input$CRC2[week_in_year]
	} else if (CriticalCurveSw == 3) {
		DWCriticalCurve_o <- DWCriticalCurve_input$CRC3[week_in_year]
	} else if (CriticalCurveSw == 4) {
		DWCriticalCurve_o <- DWCriticalCurve_input$CRC4[week_in_year]
	}
	return(DWCriticalCurve_o)
}
DWCriticalCurve <- function() {
	DWCriticalCurve_o <- DWCriticalCurve_input$CRC1[week_in_year]
	return(DWCriticalCurve_o)
}
DWAssuredRefill <- function() {
	DWAssuredRefill_o <- DWAssuredRefill_input[week_in_year, 2]
	return(DWAssuredRefill_o)
}
DWVariableRefill <- function() {
	if (RefillSwitch() == 1) {
		DWRefillCurve_o <- DWAssuredRefill()
	} else if (RefillSwitch() == 2) {
		DWRefillCurve_o <- DWVariableRefillCurve
	} 
	return(DWRefillCurve_o)
}
DW_ORC <- function() {
	DW_ORC_o <- min(max(min(max(DWAssuredRefill(), DWCriticalCurve()), DWVariableRefill()), DWLowerLimit()), DWFloodCurve(), DWBiOpDraftLimit())
	return(DW_ORC_o)
}

##################### Dworshak fish flows ##############################

DWLGDraftLimit <- function() {
	if (FishOverRefillSw() == 1) {
		if (UseAllStorForMCNLG == 1) {
			DWLGDraftLimit_o <- DWBotVol
		} else {
			DWLGDraftLimit_o <- 2.238e6
		}
	} else if (FishOverRefillSw() == 0) {
		DWLGDraftLimit_o <- DW_ORC()
	}
	return(DWLGDraftLimit_o)
}

## According to 2008 BiOp, Dworshak is to be drafted to elevation 1535 ft by end of Aug and 1520 ft by end of September.
DWBiOpDraftLimit <- function() { 
	DWBiOpDraftLimit_o <- DWBiOpDraftLimit_input[week_in_year, 2]
	return(DWBiOpDraftLimit_o)
}
DWLGAvailWater <- function() {
	DWLGAvailWater_o <- max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
	return(DWLGAvailWater_o)
}
DWLGSup <- function() { # Release from Dworshak for meeting Lower Graninte fish flow target
	denominator <- max(0, Brownlee() + BRIn_c - BRPrelim_c - BRLGDraftLimit()) +
		max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
	numerator <- max(0, Dworshak() + DWIn() - DWPrelim() - DWLGDraftLimit())
	if (denominator == 0) {
		DWRelForLG_1 <- 0
	} else {
		DWRelForLG_1 <- TotalRelForLowerGranite() * numerator / denominator
	}
	DWRelForLG_o <- min(DWLGAvailWater(), DWRelForLG_1)
	water_df$DWLGSup[week_counter] <<- DWRelForLG_o
	return(DWRelForLG_o)
}

##################### Dworshak energy ########################################

DWPreEnergy <- function() {
	DWPreEnergy_o <- MWhr_per_ftAcFt * min(DWPrelim(), DWPenLimit()) * DWNetHead() * DWCombEfficiency
	return(DWPreEnergy_o)
}
DWSharedWater <- function() {
	DWSharedWater_o <- max(Dworshak() + DWIn() - DWPrelim() - DWLGSup() - max(DWMinRefill(), DWCriticalCurveMin()), 0)
	return(DWSharedWater_o)
}
DWDownStreamHead <- function() {
	DWDownStreamHead_o <- BONNetHead() + DANetHead() + IHNetHead() + JDNetHead() + LGNetHead() + LIGNetHead() + LMNetHead() + MCNetHead()
	return(DWDownStreamHead_o)
}
DWEnergyContent <- function() {
	DWEnergyContent_o <- DWSharedWater() * (DWNetHead() + DWDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DWEnergyContent_o)
}
DW_ORCSharedWater <- function() {
	DW_ORCSharedWater_o <- max(0, Dworshak() + DWIn() - DWPrelim() - DWLGSup() - DW_ORC())
	return(DW_ORCSharedWater_o)
}
DW_ORCEnergyContent <- function() {
	DW_ORCEnergyContent_o <- DW_ORCSharedWater() * (DWNetHead() + DWDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DW_ORCEnergyContent_o)
}
DWFirmEngSup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		if (TotalEnergyContent_c == 0) {
			DWFirmEngSup_o <- 0
		} else {
			DWFirmEngSup_o <- (DWEnergyContent() + DW_ORCEnergyContent()) / (TotalEnergyContent_c + Total_ORCEnergyContent_c) * FirmEnergyDeficit_c
		}
	} else if (Total_ORCEnergyContent_c == 0) {
		DWFirmEngSup_o <- 0
	} else {
		DWFirmEngSup_o <- DW_ORCEnergyContent() / Total_ORCEnergyContent_c * FirmEnergyDeficit_c
	}
	if (is.na(water_df$DWFirmEngSup[week_in_year])) {
		water_df$DWFirmEngSup[week_in_year] <<- DWFirmEngSup_o
	}
	return(DWFirmEngSup_o)
}
DWNFEnergyContent <- function() {
	DWNFEnergyContent_o <- max(0, DW_ORCEnergyContent() - DWFirmEngSup())
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
DWFirmEngSupReq <- function() {
	DWFirmEngSupReq_o <- min(DWPenLimit(), DWFirmEngSup() / (MWhr_per_ftAcFt * (DWNetHead() + DWDownStreamHead()) * DWCombEfficiency))
	return(DWFirmEngSupReq_o)
}
DWNonFirmEngSupReq <- function() {
	if (NonFirmEnergySw() == 1) {
		DWNonFirmEngSupReq_o <- min(DWPenLimit(), (DWFirmEngSup() + DWNonFirmEngSup()) / (MWhr_per_ftAcFt * (DWNetHead() + DWDownStreamHead()) * DWCombEfficiency))
	} else {
		DWNonFirmEngSupReq_o <- 0
	}
	return(DWNonFirmEngSupReq_o)
}
DWEnergySup <- function() {
	if (UseTotalEnergyContentForFirm() == 1) {
		DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DWSharedWater()), min(DWNonFirmEngSupReq(), DW_ORCSharedWater())))
	} else {
		DWEnergySup_o <- max(0, max(min(DWFirmEngSupReq(), DW_ORCSharedWater()), min(DWNonFirmEngSupReq(), DW_ORCSharedWater())))
	}
	return(DWEnergySup_o)
}
DWCombSup <- function() {
	DWCombSup_o <- DWLGSup() + DWEnergySup()
	return(DWCombSup_o)
}
DWLGSupEnergy <- function() {
	DWLGSupEnergy_o <- DWLGSup() * (DWNetHead() + DWDownStreamHead()) * Estimated_Efficiency * MWhr_per_ftAcFt
	return(DWLGSupEnergy_o)
}
DWDamProtectExcess <- function() {
	DWDamProtectExcess_o <- max(DWDamProtectRel() - DWPrelim() - DWCombSup(), 0)
	return(DWDamProtectExcess_o)
}

############# Dworshak final release ############################

DWRelease <- function() {
	DWRelease_o <- max(min(DWPrelim() + DWCombSup(), DWRelLimit()), DWDamProtectRel())
	return(DWRelease_o)
}
DWOutflow <- function() {
	DWOutflow_o <- DWRelease_c
	return(DWOutflow_o)
}

#######################################################
#---------------- LOWER GRANITE DAM ------------------#
#######################################################

LowerGraniteFlowData <- function() {
	return(FlowLG)
}
LGInc <- function() {
	LGInc_o <- LowerGraniteFlowData() - HellsCanyonFlowData() - DworshakFlowData()
	return(LGInc_o)
}
LGNetHead <- function() {
	LGNetHead_o <- 100 #https://www.nww.usace.army.mil/Missions/Water-Management/
	return(LGNetHead_o)
}
LGPenLimit <- function() {
	LGPenCap <- 130000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Lower-Granite/#top
	LGPenLimit_o <- LGPenCap * cfsTOafw
	return(LGPenLimit_o)
}
LGIn <- function() {
	LGIn_o <- DWOutflow() + HCOut() + LGInc()
	return(LGIn_o)
}
LGPrelim <- function() {
	LGPrelim_o <- HCPrelim() + DWPrelim() + LGInc()
	return(LGPrelim_o)
}
LGPreEnergy <- function() {
	LGPreEnergy_o <- MWhr_per_ftAcFt * min(LGPrelim(), LGPenLimit()) * LGNetHead() * LGCombEfficiency
	return(LGPreEnergy_o)
}
LGOut <- function() {
	LGOut_o <- LGIn()
	return(LGOut_o)
}

#######################################################
#----------------- LITTLE GOOSE DAM ------------------#
#######################################################

LittleGooseFlowData <- function() {
	return(FlowLIG)
}
LIGInc <- function() {
	LIGInc_o <- LittleGooseFlowData() - LowerGraniteFlowData()
	return(LIGInc_o)
}
LIGNetHead <- function() {
	LIGNetHead_o <- 98 # https://www.nww.usace.army.mil/Missions/Water-Management/
	return(LIGNetHead_o)
}
LIGPenLimit <- function() {
	LIGPenCap <- 130000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Little-Goose/#top
	LIGPenLimit_o <- LIGPenCap * cfsTOafw
	return(LIGPenLimit_o)
}
LIGIn <- function() {
	LIGIn_o <- LGOut() + LIGInc()
	return(LIGIn_o)
}
LIGPrelim <- function() {
	LIGPrelim_o <- LGPrelim() + LIGInc()
	return(LIGPrelim_o)
}
LIGPreEnergy <- function() {
	LIGPreEnergy_o <- MWhr_per_ftAcFt * min(LIGPrelim(), LIGPenLimit()) * LIGNetHead() * LIGCombEfficiency
	return(LIGPreEnergy_o)
}
LIGOut <- function() {
	LIGOut_o <- LIGIn()
	return(LIGOut_o)
}

#######################################################
#---------------- LOWER MONUMENTAL DAM ---------------#
#######################################################

LowerMonuFlowData <- function() {
	return(FlowLM)
}
LMInc <- function() {
	LMInc_o <- LowerMonuFlowData() - LittleGooseFlowData()
	return(LMInc_o)
}
LMNetHead <- function() {
	LMNetHead_o <- 100 # https://www.nww.usace.army.mil/Missions/Water-Management/
	return(LMNetHead_o)
}
LMPenLimit <- function() {
	LMPenCap <- 130000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Lower-Monumental/#top
	LMPenLimit_o <- LMPenCap * cfsTOafw
	return(LMPenLimit_o)
}
LMIn <- function() {
	LMIn_o <- LIGOut() + LMInc()
	return(LMIn_o)
}
LMPrelim <- function() {
	LMPrelim_o <- LIGPrelim() + LMInc()
	return(LMPrelim_o)
}
LMPreEnergy <- function() {
	LMPreEnergy_o <- MWhr_per_ftAcFt * min(LMPrelim(), LMPenLimit()) * LMNetHead() * LMCombEfficiency
	return(LMPreEnergy_o)
}
LMOut <- function() {
	LMOut_o <- LMIn()
	return(LMOut_o)
}

#######################################################
#------------------- ICE HARBOR DAM ------------------#
#######################################################

IceHarborFlowData <- function() {
	return(FlowIH)
}
IHInc <- function() {
	IHInc_o <- IceHarborFlowData() - LowerMonuFlowData()
	return(IHInc_o)
}
IHNetHead <- function() {
	IHNetHead_o <- 98 # https://www.nww.usace.army.mil/Missions/Water-Management/
	return(IHNetHead_o)
}
IHPenLimit <- function() {
	IHPenCap <- 106000 # https://www.nwd.usace.army.mil/CRSO/Project-Locations/Ice-Harbor/#top
	IHPenLimit_o <- IHPenCap * cfsTOafw
	return(IHPenLimit_o)
}
IHPrelim <- function() {
	IHPrelim_o <- LMPrelim() + IHInc()
	return(IHPrelim_o)
}
IHPreEnergy <- function() {
	IHPreEnergy_o <- MWhr_per_ftAcFt * min(IHPrelim(), IHPenLimit()) * IHNetHead() * IHCombEfficiency
	return(IHPreEnergy_o)
}
IHIn <- function() {
	IHIn_o <- LMOut() + IHInc()
	return(IHIn_o)
}
IHOut <- function() {
	IHOut_o <- IHIn()
	return(IHOut_o)
}

#######################################################
#--------------------- MCNARY DAM --------------------#
#######################################################

McNaryFlowData <- function() {
	flow_o <- FlowMCN + RetVICMCN
	return(flow_o)
}
MCNInc <- function() {
	MCNInc_o <- McNaryFlowData() - IceHarborFlowData() - PriestRapidsFlowData()
	return(MCNInc_o)
}
MCNetHead <- function() {
	MCNetHead_o <- 74 # https://www.nww.usace.army.mil/Missions/Water-Management/
	return(MCNetHead_o)
}
MCNPenLimit <- function() {
	MCNPenCap <- 232000  # https://www.cbr.washington.edu/hydro/mcnary
	MCNPenLimit_o <- MCNPenCap * cfsTOafw
	return(MCNPenLimit_o)
}
MCNIn <- function() {
	MCNIn_o <- IHOut() + PROut() + MCNInc()
	return(MCNIn_o)
}
MCNCurtail <- function() {
	MCNCurtail_0 <- min(DemMCN, max(IflowMCN - IHOut() - PROut() - MCNInc(), 0))
	if (curtail_option == 1) {
		MCNCurtail_o <- ifelse(MCNCurtail_0 > 0, CurtMCN, 0)
	} else if (curtail_option == 2) {
		MCNCurtail_o <- MCNCurtail_0
	} else if (curtail_option == 3) {
		MCNCurtail_o <- 0
	}
	if (DARunoffAprSep > mainstem_rule) {
		MCNCurtail_o <- 0
	} else {
		MCNCurtail_o <- MCNCurtail_o
	}		
	return(MCNCurtail_o)
}
MCNInstreamShortfall <- function() {
	MCNInstreamShortfall_o <- max(IflowMCN - IHOut() - PROut() - MCNInc(), 0)
	return(MCNInstreamShortfall_o)
}
MCNPrelim <- function() {
	MCNPrelim_o <- IHPrelim() + PRPrelim() + MCNInc()
	return(MCNPrelim_o)
}
MCNPreEnergy <- function() {
	MCNPreEnergy_o <- MWhr_per_ftAcFt * min(MCNPrelim(), MCNPenLimit()) * MCNetHead() * MCNCombEfficiency
	return(MCNPreEnergy_o)
}
MCNOut <- function() {
	MCNOut_o <- MCNIn()
	return(MCNOut_o)
}

#######################################################
#------------------ JOHN DAY DAM ---------------------#
#######################################################

JohnDayFlowData <- function() {
	return(FlowJD)
}
JDInc <- function() {
	JDInc_o <- JohnDayFlowData() - McNaryFlowData()
	return(JDInc_o)
}
JDCurtail <- function() {
	JDCurtail_0 <- min(DemJD, max(IflowJD - MCNOut() - JDInc(), 0))
	if (curtail_option == 1) {
		JDCurtail_o <- ifelse(JDCurtail_0 > 0, CurtJD, 0)
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
	JDInstreamShortfall_o = max(IflowJD - MCNOut() - JDInc(), 0)
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
	return(FlowPEL)
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
	PELRelLimit_o <- max(Pelton() + PELInflow() - PELBotVol, 0)
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
	PELRuleReq_o <- max(Pelton() + PELIn() - PELTopVol(), 0)
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
	return(FlowDA)
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
	DACurtail_0 <- min(DemDA, max(IflowDA - JDOut() - PELOutflow() - DAInc(), 0))
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
	DAInstreamShortfall_o = max(IflowDA - JDOut() - PELOutflow() - DAInc(), 0)
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
#---------------- BONNEVILLE DAM ---------------------#
#######################################################

BonnevilleFlowData <- function() {
	return(FlowBON)
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
		MinFloodRelReq_o <- max(ControlledFlow() * cfsTOafw - DAPreOutflow(), 0)
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