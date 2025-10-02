#!/usr/bin/bash
run_type=supply_and_demand
declare -a scr_list=(Historical_baseline bcc-csm1-1_rcp45 bcc-csm1-1_rcp85 bcc-csm1-1_historical) 

for scr_name in ${scr_list[@]}; do
	export SCR=$scr_name
	export RUN=$run_type
	qsub -V VIC_to_RColSim
done


