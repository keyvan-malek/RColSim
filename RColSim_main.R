                          ##################################################################################################################
                          ##################################################################################################################
                                                            ################################################
                                                            #                                              #
                                                            #                                              #
                                                            #                    R-ColSim                  #
                                                            #                                              #
                                                            #                                              #
                                                            ################################################
                          ##################################################################################################################
                          ##################################################################################################################                                                            
                                                            
                                                                #######################################
                                                                #                                     #
                                                                #         Principle Developers:       #
                                                                #                                     #
                                                                #           Keyvan Malek              #
                                                                #           Matthew Yourek            #
                                                                #                                     #
                                                                #######################################
                                                            
                                                              


# -------------------------------------------------------------------------------------------------------------------------------------------
#
#                                                    Read the global input file
#                                                            
# -------------------------------------------------------------------------------------------------------------------------------------------
                                           
scr = "Historical_baseline" 
print(paste0("Now doing scenario: ", scr))
GlobalFile <- read.table(paste0("~/RColSim_v1/inputs/GIF_", scr), stringsAsFactors=F)


# -------------------------------------------------------------------------------------------------------------------------------------------
#
#                                                    Prepare input files and time series for simulation
#                                                            
# -------------------------------------------------------------------------------------------------------------------------------------------
setwd(GlobalFile[1,2])
input_file <- read.table(GlobalFile[2,2], header=T)
print(paste0("Reading: ", GlobalFile[2,2]))


datetxt_sim <- as.Date(paste0(input_file$Month, "-", input_file$Day, "-", input_file$Year), format="%m-%d-%Y")
input_start_date <- datetxt_sim[1] 
input_end_date <- datetxt_sim[length(datetxt_sim)] 

# Input data time frame
date_hist_sim_0 <- data.frame(date=datetxt_sim,
	month=as.numeric(format(datetxt_sim, format="%m")),
    week=input_file$Week, day=as.numeric(format(datetxt_sim, format="%d")),
    year=as.numeric(format(datetxt_sim, format="%Y")))

# Simulation start and end date
start_year <- GlobalFile[4,2]
simulation_start_date <- datetxt_sim[which(datetxt_sim > paste0(start_year, "-07-31"))[1]]
simulation_end_date <- GlobalFile[5,2] #"2014-09-27"

lines_to_keep <- which(datetxt_sim >= simulation_start_date & datetxt_sim <= simulation_end_date) 
num_lines_to_skip <<- which(datetxt_sim == simulation_start_date) - 1
num_years_to_skip <<- as.numeric(start_year) - as.numeric(format(as.Date(input_start_date), format="%Y"))

date_hist_sim <- date_hist_sim_0[lines_to_keep,]
N_of_TimeSteps <- nrow(date_hist_sim)

# ------- Load functions and rule curves
# 5 files need to be loaded in the following order: 1- LoadFunctions.R 2- Read_Rule_Curves.R 3- Switches.R  4- dataframes.R 5- Measures of performance

# 1- LOAD ALL FUNCTIONS
source("LoadFunctions.R") 
# 2- READ ALL INPUT FILES
source("Read_Rule_Curves.R")
Read_Rule_Curves()
# 3- DEFINE SWITCHES AND DEFAULTS
source("Switches.R")
ReadSwitches()
# 4- CREATE DATAFRAMES
source("dataframes.R")
# 5- LOAD PMFs
source("PMFs.R")
# OUTPUT FILE
OutputFolder<-GlobalFile[3,2]

###### READ INPUT DATA FOR EACH WEEK
source("VIC_Data.R")

# ----------------- New simulation?
#------------------- Attention!!!!!!!!!!!!!!
# If new simulation is TRUE the program removes all file in the output directory

NEW_SIMULATION <- TRUE

##################################################################################################################
##################################################################################################################
                                  #######################################
                                  #                                     #
                                  #                                     #
                                  #          WEEKLY TIME-STEP           #
                                  #                                     #
                                  #                                     #
                                  #######################################
##################################################################################################################
##################################################################################################################
I_Week <- 1
for (I_Week in 1:N_of_TimeSteps){
	if(I_Week == 1) { # Model initialization
		print(paste0("initialization"))
		week_counter <- I_Week
		print(paste0("time step = ", week_counter))
		week_in_year <- input_file$Week[week_counter + num_lines_to_skip]
		year_counter <- year_from_weekly()
		###### READ INPUT DATA FOR WEEK week_counter
		VIC_Data()
		############### Common weekly variables
		reset_variables <- c("BRPrelim_c", "BRIn_c", "BRPrelim_c", "GCIn_c", "CJIn_c", "AFCombSup_c", "MICombSup_c", "ARCombSup_c", "DUCombSup_c",
			"LBCombSup_c", "KECombSup_c", "HHCombSup_c", "BRCombSup_c", "CLCombSup_c", "GCCombSup_c", 
			"TotalFloodSpace_c", "TotalEnergyContent_c", "TotalECCEnergyContent_c", "FirmEnergyDeficit_c", 
			"NonFirmEnergyDeficit_c", "TotalMcNarySharedWater_c", "TotalBONSharedWater_c", "TotalFloodRelSharedWater_c", "TotalCoordPreEnergy_c",
			"TotalNFEnergyContent_c", "control_wk")
		for (var in reset_variables) {
			assign(var, -9999)
		}
		# ---------------- Initialize the model
		source("initialize_model.R")		
	} else {
		# FIND THE RIGHT TIME STEP
		week_counter <- I_Week
		print(paste0("time step = ", week_counter))
		week_in_year <- input_file$Week[week_counter]
		year_counter <- year_from_weekly() ## Year of the current time step
		###### READ INPUT DATA FOR EACH WEEK
		VIC_Data()
		############### COMMON WEEKLY VARIABLES
		reset_variables <- c("BRPrelim_c", "BRIn_c", "GCIn_c", "BRPrelim_c", "CJIn_c", "AFCombSup_c", "MICombSup_c", "ARCombSup_c", "DUCombSup_c",
			"LBCombSup_c", "KECombSup_c", "HHCombSup_c", "BRCombSup_c", "CLCombSup_c", "GCCombSup_c", 
			"TotalFloodSpace_c", "TotalEnergyContent_c", "TotalECCEnergyContent_c", "FirmEnergyDeficit_c", 
			"NonFirmEnergyDeficit_c", "TotalMcNarySharedWater_c", "TotalBONSharedWater_c", "TotalFloodRelSharedWater_c", "TotalCoordPreEnergy_c",
			"TotalNFEnergyContent_c", "control_week")
		for (var in reset_variables) {
			assign(var, -9999)
		}
		
		MIRelease_c <- MIRelease()
		dams_in$MICAA[week_counter] <- MIInflow()
		dams_out$MICAA[week_counter] <- MIOutflow()
		flood_curve_df$MICAA[week_counter] <- MIFloodCurve()
		energy_curve_df$MICAA[week_counter] <- MIECC()

		dams_in$REVEL[week_counter] <- REVIn()
		dams_out$REVEL[week_counter] <- REVOut()

		ARRelease_c <- ARRelease()
		dams_in$ARROW[week_counter] <- ARInflow()
		dams_out$ARROW[week_counter] <- AROutflow()
		flood_curve_df$ARROW[week_counter] <- ARFloodCurve()
		energy_curve_df$ARROW[week_counter] <- ARECC()

		HHRelease_c <- HHRelease()
		dams_in$FLASF[week_counter] <- HHInflow()
		dams_out$FLASF[week_counter] <- HHOutflow()
		flood_curve_df$FLASF[week_counter] <- HHFloodCurve()
		energy_curve_df$FLASF[week_counter] <- HHECC()

		#KERelease_c <- KERelease()
		dams_in$FLAPO[week_counter] <- KEInflow()
		dams_out$FLAPO[week_counter] <- KERelease_c 
		flood_curve_df$FLAPO[week_counter] <- KEFloodCurve()
		energy_curve_df$FLAPO[week_counter] <- KerrECC()

		dams_in$THOMF[week_counter] <- TFIn()
		dams_out$THOMF[week_counter] <- TFOut()

		dams_in$NOXON[week_counter] <- NOXIn()
		dams_out$NOXON[week_counter] <- NOXOut()

		dams_in$CABIN[week_counter] <- CBIn()
		dams_out$CABIN[week_counter] <- CBOut()

		#AFRelease_c <- AFRelease()
		dams_in$ALBEN[week_counter] <- AFInflow()
		dams_out$ALBEN[week_counter] <- AFRelease_c
		flood_curve_df$ALBEN[week_counter] <- AFFloodCurve()
		energy_curve_df$ALBEN[week_counter] <- AFECC()

		dams_in$BOXCA[week_counter] <- BCIn()
		dams_out$BOXCA[week_counter] <- BCOut()

		dams_in$BOUND[week_counter] <- BDIn()
		dams_out$BOUND[week_counter] <- BDOut()

		LBRelease_c <- LBRelease()
		dams_in$LIBBY[week_counter] <- LBInflow()
		dams_out$LIBBY[week_counter] <- LBOutflow()
		flood_curve_df$LIBBY[week_counter] <- LBFloodCurve()
		energy_curve_df$LIBBY[week_counter] <- LBECC()

		dams_in$BONFE[week_counter] <- BONFIn()
		dams_out$BONFE[week_counter] <- BONFOut()

		#DURelease_c <- DURelease() 
		dams_in$DUNCA[week_counter] <- DUInflow()
		dams_out$DUNCA[week_counter] <- DURelease_c
		flood_curve_df$DUNCA[week_counter] <- DUFloodCurve()
		energy_curve_df$DUNCA[week_counter] <- DUECC()

		#CLRelease_c <- CLRelease() 
		dams_in$CORRA[week_counter] <- CLInflow()
		dams_out$CORRA[week_counter] <- CLRelease_c
		flood_curve_df$CORRA[week_counter] <- CLFloodCurve()
		energy_curve_df$CORRA[week_counter] <- CLECC()

		GCRelease_c <- GCRelease()
		dams_in$GCOUL[week_counter] <- GCInflow()
		dams_out$GCOUL[week_counter] <- GCOutflow()
		flood_curve_df$GCOUL[week_counter] <- GCFloodCurve()
		energy_curve_df$GCOUL[week_counter] <- GCECC()
		
		dams_in$CHIEF[week_counter] <- CJIn()
		dams_out$CHIEF[week_counter] <- CJOut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$CHIEF[week_counter] <- CJCurtail()
			mainstem_shortfall$CHIEF[week_counter] <- CJInstreamShortfall()
		}

		dams_in$WELLS[week_counter] <- WEIn()
		dams_out$WELLS[week_counter] <- WEOut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$WELLS[week_counter] <- WECurtail()
			mainstem_shortfall$WELLS[week_counter] <- WEInstreamShortfall()
		}

		CHRelease_c <- CHRelease()
		dams_in$CHELA[week_counter] <- CHInflow()
		dams_out$CHELA[week_counter] <- CHOutflow()
		flood_curve_df$CHELA[week_counter] <- CHFloodCurve()

		dams_in$ROCKY[week_counter] <- RRIn()
		dams_out$ROCKY[week_counter] <- RROut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$ROCKY[week_counter] <- RRCurtail()
			mainstem_shortfall$ROCKY[week_counter] <- RRInstreamShortfall()
		}

		dams_in$RISLA[week_counter] <- RIIn()
		dams_out$RISLA[week_counter] <- RIOut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$RISLA[week_counter] <- RICurtail()
			mainstem_shortfall$RISLA[week_counter] <- RIInstreamShortfall()
		}

		dams_in$WANAP[week_counter] <- WAIn()
		dams_out$WANAP[week_counter] <- WAOut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$WANAP[week_counter] <- WACurtail()
			mainstem_shortfall$WANAP[week_counter] <- WAInstreamShortfall()
		}

		dams_in$PRIRA[week_counter] <- PRIn()
		dams_out$PRIRA[week_counter] <- PROut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$PRIRA[week_counter] <- PRCurtail()
			mainstem_shortfall$PRIRA[week_counter] <- PRInstreamShortfall()
		}

		JLRelease_c <- JLRelease()
		dams_in$JLAKE[week_counter] <- JLInflow()
		dams_out$JLAKE[week_counter] <- JLOutflow()
		#flood_curve_df$JLAKE[week_counter] <- JLFloodCurve()

		PALRelease_c <- PALRelease()
		dams_in$PALIS[week_counter] <- PALInflow()
		dams_out$PALIS[week_counter] <- PALOutflow()
		#flood_curve_df$PALIS[week_counter] <- PALFloodCurve()

		IPRelease_c <- IPRelease()
		dams_in$IPARK[week_counter] <- IPInflow()
		dams_out$IPARK[week_counter] <- IPOutflow()
		#flood_curve_df$IPARK[week_counter] <- IPFloodCurve()

		RIRRelease_c <- RIRRelease()
		dams_in$RIRDM[week_counter] <- RIRInflow()
		dams_out$RIRDM[week_counter] <- RIROutflow()
		#flood_curve_df$RIRDM[week_counter] <- RIRFloodCurve()

		AMRelease_c <- AMRelease()
		dams_in$AMERI[week_counter] <- AMInflow()
		dams_out$AMERI[week_counter] <- AMOutflow()

		MINRelease_c <- MINRelease()
		dams_in$MINAD[week_counter] <- MINInflow()
		dams_out$MINAD[week_counter] <- MINOutflow()

		dams_in$MILNE[week_counter] <- MILIn()
		dams_out$MILNE[week_counter] <- MILOut()

		BoiseRelease_c <- BoiseRelease()
		dams_in$BOISE[week_counter] <- BoiseInflow()
		dams_out$BOISE[week_counter] <- BoiseOutflow()
		#flood_curve_df$BOISE[week_counter] <- BoiseFloodCurve()

		PayetteRelease_c <- PayetteRelease()
		dams_in$PAYHS[week_counter] <- PayetteInflow()
		dams_out$PAYHS[week_counter] <- PayetteOutflow()
		#flood_curve_df$PAYHS[week_counter] <- PayetteFloodCurve()

		OWYRelease_c <- OWYRelease()
		dams_in$OWYHE[week_counter] <- OWYInflow()
		dams_out$OWYHE[week_counter] <- OWYOutflow()
		flood_curve_df$OWYHE[week_counter] <- OWYFloodCurve()

		BRRelease_c <- BRRelease()
		dams_in$BROWN[week_counter] <- BRInflow()
		dams_out$BROWN[week_counter] <- BROutflow()
		#flood_curve_df$BROWN[week_counter] <- BRFloodCurve()
		#energy_curve_df$BROWN[week_counter] <- BRECC()

		dams_in$OXBOW[week_counter] <- OXIn()
		dams_out$OXBOW[week_counter] <- OXOut()

		dams_in$HCANY[week_counter] <- HCIn()
		dams_out$HCANY[week_counter] <- HCOut()

		DWRelease_c <- DWRelease()
		dams_in$DWORS[week_counter] <- DWInflow()
		dams_out$DWORS[week_counter] <- DWOutflow()
		#flood_curve_df$DWORS[week_counter] <- DWFloodCurve()
		#energy_curve_df$DWORS[week_counter] <- DWECC()

		dams_in$LGRAN[week_counter] <- LGIn()
		dams_out$LGRAN[week_counter] <- LGOut()	

		dams_in$LGOOS[week_counter] <- LIGIn()
		dams_out$LGOOS[week_counter] <- LIGOut()

		dams_in$LMONU[week_counter] <- LMIn()
		dams_out$LMONU[week_counter] <- LMOut()

		dams_in$ICEHA[week_counter] <- IHIn()
		dams_out$ICEHA[week_counter] <- IHOut()

		dams_in$MCNAR[week_counter] <- MCNIn()
		dams_out$MCNAR[week_counter] <- MCNOut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$MCNAR[week_counter] <- MCNCurtail()
			mainstem_shortfall$MCNAR[week_counter] <- MCNInstreamShortfall()
		}
		#BiOp$MCNAR[week_counter] <- McNaryFlowTarget()

		dams_in$JDAYY[week_counter] <- JDIn()
		dams_out$JDAYY[week_counter] <- JDOut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$JDAYY[week_counter] <- JDCurtail()
			mainstem_shortfall$JDAYY[week_counter] <- JDInstreamShortfall()
		}

		PELRelease_c <- PELRelease()
		dams_in$PELTO[week_counter] <- PELInflow()
		dams_out$PELTO[week_counter] <- PELOutflow()
		#flood_curve_df$PELTO[week_counter] <- PELFloodCurve()

		dams_in$DALLE[week_counter] <- DAIn()
		dams_out$DALLE[week_counter] <- DAOut()
		if (track_curtailment == week_counter) {
			mainstem_curtailments$DALLE[week_counter] <- DACurtail()
			mainstem_shortfall$DALLE[week_counter] <- DAInstreamShortfall()
		}

		dams_in$BONNE[week_counter] <- BONIn()
		dams_out$BONNE[week_counter] <- BONOut()
		#BiOp$BONNE[week_counter] <- BonnevilleFlowTarget()
		water_df$TotalDamProtectExcess[week_counter] <- TotalDamProtectExcess_c
    
		##################################################################################################################
		##################################################################################################################
									  #######################################
									  #                                     #
									  #                                     #
									  #          Write Model Output         #
									  #                                     #
									  #                                     #
									  #######################################
		##################################################################################################################
		##################################################################################################################
		
		for (res in names(reservoir_vol_df)) {
			reservoir_vol_df[week_counter,res] <- reservoir_vol_df[week_counter-1,res] + (dams_in[week_counter,res] - dams_out[week_counter,res])
		}
		
		###### MOPs Measures Of Performance
		MOP_df$FirmEnergy[week_counter] <- FirmEnergyMOP() # Firm energy shortfall
		MOP_df$NonFirmEnergy[week_counter] <- NonFirmEnergyMOP() # Non-Firm energy shortfall
		MOP_df$ColFallsFlow[week_counter] <- ColFallsFlowMOP() # Columbia Falls flow shortfall
		MOP_df$LowerGraniteFlow[week_counter] <- LowerGraniteFlowMOP() # Lower Granite flow shortfall 
		MOP_df$VernitaBarFlow[week_counter] <- VernitaFlowMOP() # Vernita Bar flow shortfall
		MOP_df$McNaryFlow[week_counter] <- McNaryFlowMOP() # McNary flow shortfall
		MOP_df$GCRec[week_counter] <- GCRecMOP() # Grand Coulee recreation metric
		MOP_df$DallesFlood[week_counter] <- DallesFloodMOP() # The Dalles flood protection metric
		MOP_df$IHNav[week_counter] <- IHNavMOP() # Ice Harbor navigation metric
		MOP_df$BonnevillFlow[week_counter] <- BonnevilleFlowMOP() # Bonneville flow shortfall
		MOP_df$ExtraSpace[week_counter] <- ExtraSpace() # Excess flood storage space
		MOP_df$TotalSysEnergy[week_counter] <- MaxSystemEnergy_c
		
		print(paste0("Simulation date = ", as.Date(date_hist_sim[week_counter,1])))
		print(Sys.time())
		print(year_counter)
		print(paste0("---------------------------------------------------------"))
		
		write.table(cbind(date_hist_sim[week_counter,], dams_out[week_counter,]), paste0(OutputFolder, "/dams_out.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[week_counter,], dams_in[week_counter,]), paste0(OutputFolder, "/dams_in.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[week_counter,], reservoir_vol_df[week_counter,]), paste0(OutputFolder, "/reservoir_volume.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[week_counter,], MOP_df[week_counter,]), paste0(OutputFolder, "/MOP_df.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[week_counter,], water_df[week_counter,]), paste0(OutputFolder, "/water.txt"), row.names=F, col.names=F, append=T)
		write.table(cbind(date_hist_sim[week_counter,], energy_df[week_counter,]), paste0(OutputFolder, "/energy.txt"), row.names=F, col.names=F, append=T)
		
		if (track_curtailment == week_counter) {
			write.table(cbind(date_hist_sim[week_counter,], mainstem_shortfall[week_counter,]), paste0(OutputFolder, "/mainstem_shortfall.txt"), row.names=F, col.names=F, append=T)
			write.table(cbind(date_hist_sim[week_counter,], mainstem_curtailments[week_counter,]), paste0(OutputFolder, "/mainstem_curtailment.txt"), row.names=F, col.names=F, append=T)
		}
	}
}



