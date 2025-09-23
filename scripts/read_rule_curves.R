# Read_Rule_Curves.R
# Reads rule curve data from input files for all reservoirs
# Loads flood curves, critical curves, assured refill curves, and other operational parameters

input_dir = "inputs/default_rule_curves/"
Read_Rule_Curves <- function() {
	###### common ##########
	lower_limit_input <<- read.table(paste0(input_dir, "rule_curve_lower_limits.txt"), header=T)
	ICF_adj_input <<- read.table(paste0(input_dir, "ICF_adjustment.txt"), header=T)
	efficiencies <<- read.csv(paste0("inputs/miscellaneous/efficiencies.csv"), header=T)
	
	###### Dworshak Dam ######
	DWFlood_input <<- read.table(paste0(input_dir, "DW/DWFlood.txt"), header=T)
	DWCriticalCurve_input <<- read.table(paste0(input_dir, "DW/DWCriticalCurve.txt"), header=T)
	DWAssuredRefill_input <<- read.table(paste0(input_dir, "DW/DW1931Refill.txt"), header=T)
	DWBiOpDraftLimit_input <<- read.table(paste0(input_dir, "DW/DWBiOpDraftLimit.txt"), header=T)
	DW_elev_input <<- read.table(paste0(input_dir, "DW/DW_storage_elevation.txt"), header=T)

	###### Upper Snake ############
	JLFlood_input <<- read.table(paste0(input_dir, "UpperSnake/JLRuleCurve.txt"), header=T)
	PALFlood_input <<- read.table(paste0(input_dir, "UpperSnake/PALRuleCurve.txt"), header=T)
	IPFlood_input <<- read.table(paste0(input_dir, "UpperSnake/IPRuleCurve.txt"), header=T)
	RIRFlood_input <<- read.table(paste0(input_dir, "UpperSnake/RIRRuleCurve.txt"), header=T)
	
	##### Middle Snake ############
	BoiseFlood_input <<- read.table(paste0(input_dir, "MiddleSnake/BoiseRuleCurve.txt"), header=T)
	OWYFlood_input <<- read.table(paste0(input_dir, "MiddleSnake/OWYRuleCurve.txt"), header=T)
	PayetteFlood_input <<- read.table(paste0(input_dir, "MiddleSnake/PayetteRuleCurve.txt"), header=T)

	#### Brownlee #######
	BRFlood_input <<- read.table(paste0(input_dir, "BRL/BRFlood_1998.txt"), header=T)
	BRCriticalCurve_input <<- read.table(paste0(input_dir, "BRL/BRCriticalCurve.txt"), header=T)
	BRAssuredRefill_input <<- read.table(paste0(input_dir, "BRL/BR1931Refill_2011.txt"), header=T)	
	BR_elev_input <<- read.table(paste0(input_dir, "BRL/BR_storage_elevation.txt"), header=T)
	BRFallChinookDraft_input <<- read.table(paste0(input_dir, "BRL/BRFallChinookDraft.txt"), header=T)

	####### Libby ########
	LBFlood_input <<- read.table(paste0(input_dir, "Libby/LBFlood_1998.txt"), header=T)
	LBCriticalCurve_input <<- read.table(paste0(input_dir, "Libby/LBCriticalCurve.txt"), header=T)
	LBAssuredRefill_input <<- read.table(paste0(input_dir, "Libby/LB1931Refill_med.txt"), header=T)
	LibbyBiOpDraftLimit_input <<- read.table(paste0(input_dir, "Libby/LBBiOpDraftLimit.txt"), header=T)
	LB_elev_input <<- read.table(paste0(input_dir, "Libby/LB_storage_elevation.txt"), header=T)

	##### Corra Linn ###########
	CLFlood_input <<- read.table(paste0(input_dir, "CL/CLIJCRuleCurve.txt"), header=T)
	CLCriticalCurve_input <<- read.table(paste0(input_dir, "CL/CLCriticalCurve.txt"), header=T)
  	CL_elev_input <<- read.table(paste0(input_dir, "CL/CL_storage_elevation.txt"), header=T)
	
	##### Chelan ###########
	CHFlood_input <<- read.table(paste0(input_dir, "Chelan/CHRuleCurve.txt"), header=T)
	CHFlow_percentiles <<- read.table(paste0(input_dir, "Chelan/weekly_flow_quantiles.txt"), header=T)
	
	##### Hungry Horse ########
	HHFlood_input <<- read.table(paste0(input_dir, "HH/HHFlood_1998.txt"), header=T)
	HHCriticalCurve_input <<- read.table(paste0(input_dir, "HH/HHCriticalCurve.txt"), header=T)
	HHAssuredRefill_input <<- read.table(paste0(input_dir, "HH/HH1931Refill_med.txt"), header=T)
	HHBiOpDraftLimit_input <<- read.table(paste0(input_dir, "HH/HHBiOpDraftLimit.txt"), header=T)
	HH_elev_input <<- read.table(paste0(input_dir, "HH/HH_storage_elevation.txt"), header=T)
	
	### Kerr #####
	KEFlood_input <<- read.table(paste0(input_dir, "Kerr/FloodCurve.txt"), header=T)
	KECriticalCurve_input <<- read.table(paste0(input_dir, "Kerr/KECriticalCurve.txt"), header=T)
	KEAssuredRefill_input <<- read.table(paste0(input_dir, "Kerr/KE1931Refill.txt"), header=T)
	KE_elev_input <<- read.table(paste0(input_dir, "Kerr/KE_storage_elevation.txt"), header=T)
  
	### AlbeniFalls ######
	AFFlood_input <<- read.table(paste0(input_dir, "AFECCandFC/AFFC.txt"), header=T)
	AFCriticalCurve_input <<- read.table(paste0(input_dir, "AFECCandFC/AF_critical_curve.txt"), header=T)
	AFAssuredRefill_input <<- read.table(paste0(input_dir, "AFECCandFC/AF1931Refill.txt"), header=T)	
	AF_elev_input <<- read.table(paste0(input_dir, "AFECCandFC/AF_storage_elevation.txt"), header=T)
	
	##### Deschuttes ############
	PELFlood_input <<- read.table(paste0(input_dir, "Deschuttes/PELRuleCurve.txt"), header=T)
	PEL_target_min <<- read.table(paste0(input_dir, "Deschuttes/PEL_target_min.txt"), header=T)
  
	### Grand Coulee #######
	GCFlood_input <<- read.table(paste0(input_dir, "GC/GCFlood_2015.txt"), header=T)
	GCCriticalCurve_input <<- read.table(paste0(input_dir, "GC/GCCriticalCurve.txt"), header=T)
	GCAssuredRefill_input <<- read.table(paste0(input_dir, "GC/GC1931Refill_med.txt"), header=T)
	GC_elev_input <<- read.table(paste0(input_dir, "GC/GC_storage_elevation.txt"), header=T)
	GCSummerDraft_input <<- read.table(paste0(input_dir, "GC/GCSummerDraft.txt"), header=T)
	GCResidualInflow_input <<- read.table(paste0(input_dir, "GC/GCResidual_inflow.txt"), header=T)
	PRInc_inflow_input <<- read.table(paste0(input_dir, "GC/PRInc_inflow.txt"), header=T)
	BONDraftLimit_input <<- read.table(paste0(input_dir, "GC/BonnevilleDraftLimit.txt"), header=T)

	##### Duncan ##########
	DUFlood_input <<- read.table(paste0(input_dir, "DUECCandFC/DUFlood.txt"), header=T)
	DUCriticalCurve_input <<- read.table(paste0(input_dir, "DUECCandFC/DUCriticalCurve.txt"), header=T)
	DUAssuredRefill_input <<- read.table(paste0(input_dir, "DUECCandFC/DU1931Refill_med.txt"), header=T)

	###### Arrow ##########
	ARFlood_input <<- read.table(paste0(input_dir, "ARECCandFC/ARFlood.txt"), header=T)
	ARCriticalCurve_input <<- read.table(paste0(input_dir, "ARECCandFC/ARCriticalCurve.txt"), header=T)
	ARAssuredRefill_input <<- read.table(paste0(input_dir, "ARECCandFC/AR1931Refill_med.txt"), header=T)
	AR_elev_input <<- read.table(paste0(input_dir, "ARECCandFC/AR_storage_elevation.txt"), header=T)
	ARAssuredRelease_input <<- read.table(paste0(input_dir, "ARECCandFC/ARAssuredRelease.txt"), header=T)

	#### Mica ###########
	MIFlood_input <<- read.table(paste0(input_dir, "MicaECCandFC/MIFlood.txt"), header=T)
	MICriticalCurve_input <<- read.table(paste0(input_dir, "MicaECCandFC/MICriticalCurve.txt"), header=T)
	MIAssuredRefill_input <<- read.table(paste0(input_dir, "MicaECCandFC/MI1931Refill_med.txt"), header=T)
 	MI_elev_input <<- read.table(paste0(input_dir, "MicaECCandFC/MI_storage_elevation.txt"), header=T)
	MIAssuredRelease_input <<- read.table(paste0(input_dir, "MicaECCandFC/MIAssuredRelease.txt"), header=T)
 
	##### Energy #######
	NonFirmFraction_input <<- read.table(paste0(input_dir, "Energy/NonFirmFraction.txt"), header=T)
	FirmFraction_input <<- read.table(paste0(input_dir, "Energy/FirmFraction.txt"), header=T)
  
  	#### Flow targets ######
	BonnevilleFlowTarget_input <<- read.table(paste0(input_dir, "BONN/BON_flow_target.txt"), header=T)
	LowerGraniteFlowTarget_input <<- read.table(paste0(input_dir, "LG/LowerGraniteTarget.txt"), header=T)
	VernitaBarFlowTarget_input <<- read.table(paste0(input_dir, "GC/VernitaBarFlowTarget.txt"), header=T)
	GCBdgtForVB_input <<- read.table(paste0(input_dir, "GC/GCBdgtForVB.txt"), header=T)
  	Article_56_input <<- read.table(paste0(input_dir, "Kerr/article_56.txt"), header=T)
	McNaryFlowTarget_input <<- read.table(paste0(input_dir, "McNary/MCN_flow_target.txt"), header=T)
	LimePointTarget_input <<- read.table(paste0(input_dir, "BRL/LimePointFlowTarget.txt"), header=T)

	######### Bonneville ########
	BONNetHead_input <<- read.table(paste0(input_dir, "BONN/BONNetHead.txt"), header=T)
 
}
