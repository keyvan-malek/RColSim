#######################
# OUTPUT DATAFRAMES ###
#######################
dams_out = data.frame(matrix(nrow=N_of_TimeSteps, ncol=34))
names(dams_out) = c("MICAA", "REVEL", "ARROW", "DUNCA", "LIBBY", "BONFE", "CORRA", "FLASF", "FLAPO", "NOXON", "CABIN", "ALBEN", "BOUND",
"GCOUL", "CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "UpSnake", "MdlSnake", "BROWN", "OXBOW", "HCANY", "DWORS", "LGRAN", "LGOOS", 
"LMONU", "ICEHA", "MCNAR", "JDAYY", "DALLE", "BONNE")

dams_in = data.frame(matrix(nrow=N_of_TimeSteps, ncol=34))
names(dams_in) = c("MICAA", "REVEL", "ARROW", "DUNCA", "LIBBY", "BONFE", "CORRA", "FLASF", "FLAPO", "NOXON", "CABIN", "ALBEN", "BOUND",
"GCOUL", "CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "UpSnake", "MdlSnake", "BROWN", "OXBOW", "HCANY", "DWORS", "LGRAN", "LGOOS", 
"LMONU", "ICEHA", "MCNAR", "JDAYY", "DALLES", "BONNE")

mainstem_curtailments = data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
names(mainstem_curtailments) = c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE")

other_curtailments = data.frame(matrix(nrow=N_of_TimeSteps, ncol=25))
names(other_curtailments) = c("MICAA", "REVEL", "ARROW", "DUNCA", "LIBBY", "BONFE", "CORRA", "FLASF", "FLAPO", "NOXON", "CABIN", "ALBEN", "BOUND",
"GCOUL", "UpSnake", "MdlSnake", "BROWN", "OXBOW", "HCANY", "DWORS", "LGRAN", "LGOOS", "LMONU", "ICEHA", "BONNE")

mainstem_shortfall = data.frame(matrix(nrow=N_of_TimeSteps, ncol=9))
names(mainstem_shortfall) = c("CHIEF", "WELLS", "ROCKY", "RISLA", "WANAP", "PRIRA", "MCNAR", "JDAYY", "DALLE") 

MOP_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=14))
names(MOP_df) = c("shortfall_1", "shortfall_2", "shortfall_5", "shortfall_6", "shortfall_7","shortfall_8",
"shortfall_9", "shortfall_10", "shortfall_11", "NumOfLowFlowBonn", "BelowFCC", "FirmEnergySales", "NonFirmSpotSales", "MaxSystemEnergy")

reservoir_vol_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=11))
names(reservoir_vol_df) = c("MICAA", "ARROW", "DUNCA", "CORRA", "LIBBY", "FLASF", "GCOUL", "DWORS", "BROWN", "UpSnake", "MdlSnake")

energy_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=7))
names(energy_df) = c("ARFirmEngSupReq", "TotalEnergyContent", "TotalECCEnergyContent", "FirmEnergyDeficit",
"TotalCoordPreEnergy", "TotalNFEnergyContent", "NonFirmEnergyDeficit")

water_df = data.frame(matrix(nrow=N_of_TimeSteps, ncol=5))
names(water_df) = c("BRPrelim_c", "TotalFloodSpace_c", "TotalMcNarySharedWater_c", "BRIn", "GCIn")
