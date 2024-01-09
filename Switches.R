
ReadSwitches <<- function() {

# Factors for controlling additional flood storage space  
  
MIFloodMult <<- 1.25 # Factor to multiply available storage space at Arrow and Mica dams for reducing high flow at The Dalles.

# Toggles the  Bonneville chum target, met by Grand Coulee
# Options: 1--Release water from Grand Coulee to meet the Bonneville fish flow target
#          2--Do not release
Chum_Q_Switch <<- function() {
  Chum_Q_Switch_o <- 1 ## Default is 1
  return(Chum_Q_Switch_o)
}  

# Choose whether meeting fish flow requirements should take priority over refilling the reservoirs
# Options: 0--FALSE
#		   1--TRUE
FishOverRefillSw <<- function() {
	fish_over_refill_o <<- 0
	return(fish_over_refill_o)
}

# gurantee refill switch
# Options: 1 -- Use minimum refill curve for proportional draft
#		   2 -- Use only CriticalCurveMin for proportional draft and set minimum refill curve to BotVol
GuranteeRefillSw <<- function() {
	gurantee_refill_o <- 2 ## Default is 2
	return(gurantee_refill_o)
}

# refill for category IV dams (Arrow and Grand Coulee).
# Options: 0--Do not use upstream release to fill to the ORC 
#		   1--Use upstream release to fill to the minimum draft point
#		   2--Use upstream release to fill to the
refill_cat4 <<- 1


# Curtail option allows the user to select how mainstem curtailment should be calculated.
# Options: 1--Calculate mainstem curtailment based on interruptible demand, 
#          2--Calculate the minimum of total demand and instream flow deficit 
#          3--Do not calculate
curtail_option <<- 3 
track_curtailment <<- 0 # If 1, output mainstem curtailment, this slows down the code

# Fraction of full pool storage to initialize reservoirs
ResInitFractionFull <<- 1 

########## MOPControl
#This control variable is used to track measures of performance only for specific climate conditions.
#If this variable <<-1 in the timestep, then measures of performance are recorded, otherwise they are ignored.

MOPControl <<- 1
SensitivityFraction <<- 0.001 # Factor of safety for meeting flow objectives

# Switch to select how to determine upper rule curve
TopRuleSw <<- function() { # Default is 0
  # Options:
  # 0-Upper rule curve equals the flood rule curve
  # 1-Don't evacuate any storage
  # 2-Use the highest flood rule curve at all times
  TopRuleSw_1 <- 0
  return(TopRuleSw_1)
}

UseTotalEnergyContentForFirm <<- function() { # Default is 1 
	#Options:
	#0--Allocate based on ORC for firm energy.  Reservoir may release only to ORC after allocation.
	#1--Allow releases below ORC for firm energy
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

RefillSwitch <<- function(){
	# This switch selects refill rule curves that are based on status quo operations (forecasts only affect Jan-July)
	# or refill rule curves based on higher quality forecasts that specify refill lower values Aug-July.  Settings are:
	#  1--Only use assured refill curve for refill
	#  2--Use variable refill curve for refill
	RefillSwitch_o <- 2
	return(RefillSwitch_o)
}

NonFirmEnergySw <<- function() {
	NonFirmEnergySw_o <<- 1 # Options: 1--Release water for non-firm hydropower generation, otherwise do not
	return(NonFirmEnergySw_o)
}
### Options for using storage to meet fish targets

Fish_Pool_Alternative <<- 1 # Options: 1--Use only water above full pool volume, 2-5--Use water above varying storage levels, depending on the dam.
UseAllStorForMCNLG <<- 0 # Options: 0--Use current draft limits for McNary and Lower Granite, 1--Use all major system storage for McNary and Lower Granite

## Other controls

mainstem_rule <<- 600E6 # Sum of Apr to September runoff at Dalles that triggers mainstem curtailement (actual is 60E6), leave high to force curtailment every year

} ### end of all switches
