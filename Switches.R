
ReadSwitches <<- function() {

# Factors for controlling additional flood storage space  
  
StorFrac <<- 1 # Weight of Brownlee and Dworshak resrevoir storage to use for meeting fish flow target at Lower Granite Dam
InflowFrac <<- 1 # Weight of inflow to Brownlee and Dworshak reservoirs to use for meeting fish flow target at Lower Granite Dam
MIFloodMult <<- 1.25 # Factor to multiply available storage space at Arrow and Mica dams for reducing high flow at The Dalles.
LBFloodMult <<- 1.25 # Factor to multiply available storage space at Libby dam for reducing high flow at The Dalles.

# Toggles the  Bonneville chum target, met by Grand Coulee
# Options: 1--Release water from Grand Coulee to meet the Bonneville fish flow target
#          2--Do not release
Chum_Q_Switch <<- function() {
  Chum_Q_Switch_o <- 1
  return(Chum_Q_Switch_o)
}  
fish_over_refill <<- 0

# gurantee refill switch
# Options: 1 -- Use minimum refill curve and CriticalCurveMin for proportional draft
#		   2 -- Use only CriticalCurveMin for proportional draft
gurantee_refill <<- 2

# refill for category IV dams (Arrow and Grand Coulee).
# Options: 0--Do not use upstream release to fill to ECC curve
#		   1--Use upstream release to fill to minimum ECC
#		   2--Use upstream release to fill to ECC
refill_cat4 <<- 1


# Curtail option allows the user to select how mainstem curtailment should be calculated.
# Options: 1--Calculate mainstem curtailment based on interruptible demand, 
#          2--Calculate the minimum of total demand and instream flow deficit 
#          3--Do not calculate
curtail_option <<- 3 
track_curtailment <<- 0 # If 1, output mainstem curtailment, this slows down the code

# Fraction of full pool storage to initialize reservoirs
ResInitFractionFull <<- 1 

######## find which operation year we are in, from weekly data
year_from_weekly <<- function() {
	all_years <- unique(input_file$DamYear)
	year_from_weekly_o <- min(which(all_years == input_file$DamYear[week_counter]), length(all_years) - 1)
	return(year_from_weekly_o)
}
########## MOPControl
#This control variable is used to track measures of performance only for specific climate conditions.
#If this variable <<-1 in the timestep, then measures of performance are recorded, otherwise they are ignored.

MOPControl <<- 1
SensitivityFraction <<- 0.001 # Factor of safety for meeting flow objectives

# Switch to select how to determine flood rule
TopRuleSw <<- function() { # Default is 0
  # Options:
  # 0-Use flood storage prescribed by forecasts
  # 1-Don't evacuate any flood storage
  # 2-Use the highest flood rule curve at all times
  TopRuleSw_1 <- 0
  return(TopRuleSw_1)
}

UseTotalEnergyContentForFirm <<- function() { # Default is 1 
	#Options:
	#0--Allocate based on ECC for firm energy.  Reservoir may release only to ECC after allocation.
	#1--Allow releases below ECC for firm energy
	UseTotalEnergyContentForFirm_o <- 1
	return(UseTotalEnergyContentForFirm_o)
}

## CriticalCurveSw
## Options:
## 1 -- release to 1st year critical curve for firm energy
## 2 -- release to 2nd year critical curve for firm energy
## 3 -- release to 3rd year critical curve for firm energy
## 4 -- release to 4th year critical curve for firm energy
CriticalCurveSw <<- 1 
MICriticalCurveSw <<- 3
DUCriticalCurveSw <<- 3
Estimated_Efficiency <<- 0.8 # Estimated combined efficiency for all plants.  This efficiency is used for estimating energy content only.


RefillSwitch <<- function(){
	# This switch selects refill rule curves that are based on status quo operations (forecasts only affect Jan-July)
	# or refill rule curves based on higher quality forecasts that specify refill lower values Aug-July.  Settings are:
	#  1--Only use assured refill curve for refill
	#  2--Use variable refill curve for refill
	RefillSwitch_o <- 2
	return(RefillSwitch_o)
}

# Kerr dam top volume switch

# Kerr is currently undergoing experimental changes in operation  that essentially hold the pool level at a constant value.
# This switch allows the user to select the pREVious rule curves for flood storage, or the fixed full pool volume as the top of the conservation pool.  Options:
# 0-Fixed full pool
# 1-Previous flood storage rule curves
# With the current setup, the release required for fish flow always exceeds the flood evacuation, so this switch 
# does not actually change the output.

KerrTopVolSw <<- 0 # Default = 0

########## EnergyAllocSafeFactor
#Due to penstock constraints, the energy allocation algorithm may occasionally request water from an upstream dam that is already spilling water.
#In this case the system wide energy target may be missed by a small amount since no extra energy will be produced at the upstream dam itself.
#This control increases the energy target by a small amount to ensure that the system wide targets are met despite this allocation decision.
#Note that supplemental energy releases are modeled  to pass through the entire system, so spill from upstream dams still has substantial energy content.

EnergyAllocSafeFactor <<- 1.01

######## UseAlternateNonFirmTarget
# Options: 0:  Use status quo energy target and fraction,
# 1: Use alternative forecast for non-firm energy that uses climate forecast multipliers for the period from August-January.

UseAlternateNonFirmTarget <<- 0

NonFirmEnergySw <<- 1 # Options: 1--Release water for non-firm hydropower generation, otherwise do not

### Estimated combined efficiency of turbines

AFCombEfficiency <<- 0.8
ARCombEfficiency <<- 0.8
BONCombEfficiency <<- 0.8
BDCombEfficiency <<- 0.8
BCCombEfficiency <<- 0.8
BRCombEfficiency <<- 0.8
CBCombEfficiency <<- 0.8
CHCombEfficiency <<- 0.8
CJCombEfficiency <<- 0.8
CLCombEfficiency <<- 0.8
DACombEfficiency <<- 0.8
DUCombEfficiency <<- 0.8
DWCombEfficiency <<- 0.8
KECombEfficiency <<- 0.8
HHCombEfficiency <<- 0.8
GCCombEfficiency <<- 0.8
HCCombEfficiency <<- 0.8
IHCombEfficiency <<- 0.8
JDCombEfficiency <<- 0.8
LBCombEfficiency <<- 0.8
LIGCombEfficiency <<- 0.8
LGCombEfficiency <<- 0.8
LMCombEfficiency <<- 0.8
MCNCombEfficiency <<- 0.8
MICombEfficiency <<- 0.8
NOXCombEfficiency <<- 0.8
OXCombEfficiency <<- 0.8
PELCombEfficiency <<- 0.8
PRCombEfficiency <<- 0.8
RevCombEfficiency <<- 0.8
RICombEfficiency <<- 0.8
RRCombEfficiency <<- 0.8
TFCombEfficiency <<- 0.8
WACombEfficiency <<- 0.8
WECombEfficiency <<- 0.8

### Options for using storage to meet fish targets

Fish_Pool_Alternative <<- 1 # Options: 1--Use only water above full pool volume, 2-5--Use water above varying storage levels, depending on the dam.
UseAllStorForMCNLG <<- 0 # Options: 0--Use current draft limits for McNary and Lower Granite, 1--Use all major system storage for McNary and Lower Granite

## Other controls

GCEngContMult <<- 1 # Multiplier for flow from GC allocated for meeting firm energy target
Deviation__From_Normal_Curve <<- 1 # Multiplier to adjust firm energy target, leave as 1 unless experimenting
mainstem_rule <<- 600E6 # Sum of Apr to September runoff at Dalles that triggers mainstem curtailement (actual is 60E6), leave high to force curtailment every year

} ### end of all switches
