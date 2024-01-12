initialize_output <- function() {
	dams_out <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=46))
	names(dams_out) <- c("ALBEN", "AMERI", "ARROW", "BONFE", "BONNE", "BOUND", "BOXCA", "BROWN", "CABIN", "CHELA",
		"CHIEF", "CORRA", "DALLE", "DUNCA", "DWORS", "FLAPO", "FLASF", "GCOUL", "HCANY", "ICEHA", "IPARK", 
		"JDAYY", "JLAKE", "LIBBY", "LGOOS", "LGRAN", "LMONU", "BOISE", "MCNAR", "MICAA", "MILNE", "MINAD", "NOXON",
		"OWYHE", "OXBOW", "PALIS", "PAYHS", "PELTO", "PRIRA", "REVEL", "RIRDM", "RISLA", "ROCKY", "THOMF", "WANAP",
		"WELLS")
		
	dams_in <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=46))
	names(dams_in) <- c("ALBEN", "AMERI", "ARROW", "BONFE", "BONNE", "BOUND", "BOXCA", "BROWN", "CABIN", "CHELA",
		"CHIEF", "CORRA", "DALLE", "DUNCA", "DWORS", "FLAPO", "FLASF", "GCOUL", "HCANY", "ICEHA", "IPARK", 
		"JDAYY", "JLAKE", "LIBBY", "LGOOS", "LGRAN", "LMONU", "BOISE", "MCNAR", "MICAA", "MILNE", "MINAD", "NOXON",
		"OWYHE", "OXBOW", "PALIS", "PAYHS", "PELTO", "PRIRA", "REVEL", "RIRDM", "RISLA", "ROCKY", "THOMF", "WANAP",
		"WELLS")
		
	MOP_df <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=14))
	names(MOP_df) <- c("FirmEnergy", "NonFirmEnergy", "ColFallsFlow", "LowerGraniteFlow", "VernitaBarFlow", "McNaryFlow",
		"GCRec", "DallesFlood", "IHNav", "BonnevillFlow", "ExtraSpace", "FirmEnergySales", "NonFirmSpotSales", "TotalSysEnergy")

	############ DO NOT DELETE THESE DATAFRAMES !!!! #####################
	energy_df <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=6))
	names(energy_df) <- c("TotalEnergyContent", "TotalECCEnergyContent", "FirmEnergyDeficit",
	"TotalCoordPreEnergy", "TotalNFEnergyContent", "NonFirmEnergyDeficit")
	
	water_df <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=18))
	names(water_df) <- c("TotalFloodSpace", "TotalRelReducReq", "TotalMcNarySharedWater", "TotalBONSharedWater", "GCSupForVernitaBar", "BRLGSup", "BRRelForJBandLP",
		"DWLGSup", "DWFirmEngSup", "GCFirmEngSup", "TotalEnergySup", "UpstreamFloodEvacGC", "TotalFloodRelSharedWater", "GCMinFloodRelReq", "ARMinFloodRelReq",
		"ControlledFlow", "BRIn", "GCIn")
		
	mainstem_curtailments <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
	names(mainstem_curtailments) <- c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE")
	
	mainstem_shortfall <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
	names(mainstem_shortfall) <- c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE")
	
	reservoir_vol_df <- data.frame(matrix(nrow=N_of_TimeSteps, ncol=22))
	names(reservoir_vol_df) <- c("MICAA", "ARROW", "DUNCA", "CORRA", "LIBBY", "FLASF", "GCOUL", "DWORS", "BROWN", "FLAPO", 
		"ALBEN", "CHELA", "JLAKE", "PALIS", "IPARK", "RIRDM", "AMERI", "MINAD", "BOISE", "PAYHS", "OWYHE", "PELTO")
		
	out_vars <- ls()
	if (track_curtailment == 0) {
		out_vars <- out_vars[-which(out_vars %in% c("mainstem_shortfall", "mainstem_curtailments"))]
	}
	
	dams_out <<- dams_out
	dams_in <<- dams_in
	MOP_df <<- MOP_df
	energy_df <<- energy_df
	water_df <<- water_df
	mainstem_curtailments <<- mainstem_curtailments
	mainstem_shortfall <<- mainstem_shortfall
	reservoir_vol_df <<- reservoir_vol_df
	out_vars <<- out_vars
		
	####################################################################################
}