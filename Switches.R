
ReadSwitches <<- function() {

# Factors for controlling additional flood storage space  
  
StorFrac <<- 0.2 # Fraction of storage in Brownlee and Dworshak dams to use for meeting fish flow target at Lower Granite Dam
InflowFrac <<- 1 # Fraction of inflow to Brownlee and Dworshak reservoirs to use for meeting fish flow target at Lower Granite Dam
MIFloodMult <<- 1.25 # Factor to multiply available storage space at Arrow and Mica dams for reducing high flow at The Dalles.
LBFloodMult <<- 1.25 # Factor to multiply available storage space at Libby dam for reducing high flow at The Dalles.

# Toggles the  Bonneville chum target, met by Grand Coulee
# Options: 1--Release water from Grand Coulee to meet the Bonneville fish flow target
#          2--Do not release
Chum_Q_Switch <<- function() {
  Chum_Q_Switch_o <- 1
  return(Chum_Q_Switch_o)
}  

# Curtail option allows the user to select how mainstem curtailment should be calculated.
# Options: 1--Calculate mainstem curtailment based on interruptible demand, 
#          2--Calculate instream flow deficit rather than curtailment
#          3--Do not calculate
curtail_option <<- 3 
track_curtailment <<- 0 # If 1, output mainstem curtailment, this slows down the code

##### InitialConditionSwitch
#Options: 0:  Use values imported from spreadsheet. 1:  Use fixed multiplier for whole basin (fraction of full pool)
#2:  Use historic storage value
InitialConditionSwitch <<- 2 # Default = 2
ResInitFractionFull <<- 0.8 # Used with option 1

week_counter_in_year <<- function() { # Find week of year starting on August 1st
	week_counter_in_year_o = date_hist_sim$week[week_counter]
	return(week_counter_in_year_o)
}

######## find which operation year we are in, from weekly data
year_from_weekly <<- function() {
	year_from_weekly_o = week_counter %/% 52 + 1  
	return(year_from_weekly_o)
}

########## MOPControl
#This control variable is used to track measures of performance only for specific climate conditions.
#If this variable <<-1 in the timestep, then measures of performance are recorded, otherwise they are ignored.

MOPControl <<- 1
SensitivityFraction <<- 0.001 # Factor of safety for meeting flow objectives

###### RefillMinSw
# Switch to select alternate minimum release for generating inflow estimates at Grand Coulee and Arrow.
# These flow estimates are used to compute refill curves at Grand Coulee and Arrow.  Options:
# 0--Normal minimum requirements
# 1--Special minimum requirements for estimating inflows for refill curves.
RefillMinSw <<- function(){ # Default is 0
	RefillMinSw_o <- 0
	return (RefillMinSw_o)
}

# Switch to select how to determine flood rule
TopRuleSw <<- function() { # Default is 0
  # Options:
  # 0-Use flood storage prescribed by forecasts
  # 1-Don't evacuate any flood storage
  # 2-Use the highest flood rule curve at all times
  TopRuleSw_1 <- 0
  return(TopRuleSw_1)
}

####### ResetStorageSwitch
# This switch controls a routine that resets the storage at particular dams to preset storage values.
# The month that the storage is reset is specified by ResetStorageMonth.
# Options:
# 0--do not reset storage
# 1--reset the storage to the initial value at the end of the month specified
# 2--reset the storage according to a timeseries

ResetStorageSwitch <<- function() { # Default is 0
	ResetStorageSwitch_o <- 0
	return(ResetStorageSwitch_o)
}

####### ResetStorageMonth
# When the ResetStorageSwitch is set to 1, this function determines the month for which the storage is reset to a preset initial value.
# If this value is 6, for example, the storage at the end of January  will be used.

ResetStorageMonth <<- function() {
  ResetStorageMonth_o <- 12
  return(ResetStorageMonth_o)
}
ResetStorage <<- function() {
  if(ResetStorageSwitch()==1 && month_in_year==ResetStorageMonth()) {
    ResetStorage_o <- 1
  } else {
    ResetStorage_o <- 0
  }
  return(ResetStorage_o)
}

UseTotalEnergyContentForFirm <<- function() { # Default is 1 
	#Options:
	#0--Allocate based on  ECC for Firm Energy.  Reservoir may release only to ECC after allocation.
	#1-Allocate based on volume mid point between ECC and bottom of pool for Firm Energy.  Reservoir may release to bottom of pool after allocation.
	UseTotalEnergyContentForFirm_o = 1
	return(UseTotalEnergyContentForFirm_o)
}

Estimated_Efficiency <<- 0.8 # Estimated combined efficiency for all plants.  This efficiency is used for estimating energy content only.

# ---- Refill curve selection switches
SQuo_Refill_Targ <<- 0 # Use 1931 assured refill curve
PfctForecast_Refill_Targ <<- 1 # Use forecast for the refill 

RefillSwitch <<- function(){
	# This switch selects refill rule curves that are based on status quo operations (forecasts only affect Jan-July)
	# or refill rule curves based on higher quality forecasts that specify refill lower values Aug-July.  Settings are:
	#  1-Refill based on status Quo Operations
	#  2-Refill based on perfect forecast
  
	if (SQuo_Refill_Targ==1 | PfctForecast_Refill_Targ==0 ) {
		RefillSwitch_o = 1
	} else if (PfctForecast_Refill_Targ==1) {
		RefillSwitch_o = 2
	}
	return(RefillSwitch_o)
}

### Options for Flood curve
# Options: 1--Use flood control curve year-round, 2--Use flood control curve only during flood evacuation season.

FC_Option <<- 2 # Default = 2
GlobalFloodEvacMult <<- 1 # Factor to adjust flood evacuation, default is 1.

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
HHCombEfficiency <<- 0.8
LBCombEfficiency <<- 0.8
MICombEfficiency <<- 0.8
RevCombEfficiency <<- 0.8
CBCombEfficiency <<- 0.8
KECombEfficiency <<- 0.8
NOXCombEfficiency <<- 0.8
BCCombEfficiency <<- 0.8
BDCombEfficiency <<- 0.8
CJCombEfficiency <<- 0.8
GCCombEfficiency <<- 0.8
PRCombEfficiency <<- 0.8
RICombEfficiency <<- 0.8
RRCombEfficiency <<- 0.8
WACombEfficiency <<- 0.8
WECombEfficiency <<- 0.8
DWCombEfficiency <<- 0.8
IHCombEfficiency <<- 0.8
LGCombEfficiency <<- 0.8
LIGCombEfficiency <<- 0.8
LMCombEfficiency <<- 0.8
BONCombEfficiency <<- 0.8
DACombEfficiency <<- 0.8
JDCombEfficiency <<- 0.8
MCCombEfficiency <<- 0.8
ARCombEfficiency <<- 0.8
DUCombEfficiency <<- 0.8
DUCombEfficiency_00 <<- -999

### Options for using storage to meet fish targets

Fish_Pool_Alternative <<- 1 # Options: 1--Use only water above full pool volume, 2-5--Use water above varying storage levels, depending on the dam.
UseAllStorForMCNLG <<- 0 # Options: 0--Use current draft limits for McNary and Lower Granite, 1--Use all major system storage for McNary and Lower Granite

## Other controls

GCEngContMult <<- 1 # Multiplier for flow from GC allocated for meeting firm energy target
Deviation__From_Normal_Curve <<- 1 # Multiplier to adjust firm energy target, leave as 1 unless experimenting
mainstem_rule <<- 600E6 # Sum of Apr to September runoff at Dalles that triggers mainstem curtailement (actual is 60E6), leave high to force curtailment every year

} ### end of all switches
