setwd("~/papers/RColSim/August 2021/For_Keyvan/")
ListOfDams = c("ARROW", "BROWN", "DALLE", "DUNCA", "DWORS", "GCOUL", "FLASF", "LIBBY", "MICAA")
simulate_demand = 1
curtailment_ext = "with_curtailment"
run_type = "supply_and_demand"

pod_stns = read.table("Files/stn_code_name", stringsAsFactors=F)[,1]
stn_colsim = read.table("Files/RColSim_stations.txt", stringsAsFactors=F)[,1]
stn_abbreviations = read.table("Files/RColSim_stations.txt", stringsAsFactors=F)[,2]
new_pod_names = pod_stns[pod_stns %in% stn_colsim]
mainstem_names = c("CHIEF", "DALLE", "JDAYY", "MCNAR", "PRIRA", "ROCKY", "RISLA", "WANAP", "WELLS")
get_iflow_mainstem = function(station) {
	ifile = read.table(paste('Files/', station, '_iflow_rules', sep=""))
	Months = timeseries$Month
	Days = timeseries$Day
	instream = as.numeric(matrix(nrow=length(Days), ncol=1, 0))
	for (line in 1:length(ifile[,1])) {
		mo = ifile[line,1]
		first_day = ifile[line,2]
		last_day = ifile[line,3]
		instream[Months==mo & Days>=first_day & Days<=last_day] = ifile[line,4] * 13.8838 # converts from cfs to AF/wk
	}
	return(instream)
}

################################ Read in supply and demand data ###################################

Supply_Input = read.table("Step_3/Aggregate_demand_ColSim/to_step_4/Forecast/Historical_baseline/supply_baseline.txt", sep="", header=T)
N = length(Supply_Input[,1])
if (simulate_demand == 1) {
	Demand_Input = read.table(paste("Step_3/Aggregate_demand_ColSim/to_step_4/Forecast/Historical_baseline/demand_baseline_", curtailment_ext, ".txt", sep=""), header=T)
} else {
	Demand_Input = data.frame(matrix(nrow=N, ncol=length(new_pod_names)+4, 0))
}
if (simulate_demand == 1) {
	supply_for_return_flow = data.frame(matrix(nrow=N, ncol=7, 0))
	supply_for_return_flow[,1:4] = Supply_Input[,1:4]
	supply_for_return_flow_locs = c("WANAP", "PRIRA", "MCNAR")
	for (i in 1:3) {
		col = which(names(Supply_Input) == supply_for_return_flow_locs[i])
		supply_for_return_flow[,i+4] = Supply_Input[,col]
	}
	names(supply_for_return_flow) = c("Week", "Month", "Day", "Year", supply_for_return_flow_locs)
	return_fractions = read.csv("Step_4/return_flow_fractions.csv", header=F) # these fractions were derived from the document, "Calculation of 2020 Irrigation Depletions for
	# 2020 Level Modified Streamsflows, Hills et al., prepared for BPA, 2020.
} else {
	return_fractions = data.frame(matrix(nrow=N, ncol=3, 0))
	supply_for_return_flow = data.frame(matrix(nrow=N, ncol=7, 0))
}
if (simulate_demand == 1) {
	interruptible_demand = read.table(paste("Step_3/Aggregate_demand_ColSim/to_step_4/Forecast/Historical_baseline/interruptible_baseline_", curtailment_ext, ".txt", sep=""), header=T)
} else {
	interruptible_demand = data.frame(matrix(nrow=N, ncol=length(new_pod_names)+4, 0))
}
timeseries = Supply_Input[,2:4]
Number_of_ts = length(Supply_Input[,1])
Forecast_coef = 0.2
begin_year = min(timeseries$Year)

########################## Build RColSim input table ###################################################################################

names_Output = c("Week_Number", "month", "day", "year", "ARRefillVol1", "ARRefillVol2", "BRRunoffAprJuly", "BRRefillCurve", "DallesJanJul", "DallesRunoffAprAug", 
"DallesRunoffAprSep", "ModDallesRunoffAprSep", "DARunoffJanApr", "DURefillVol", "DURefillVol1", "DURunoffAprAug", "DWRefillVol1", "DWRefillVol2", "DWRunoffAprJuly", "DWSumRunoffMarJun", 
"GCRefillVol1", "GCRefillVol2", "GCRunoffJanApr", "HHInQAprAug", "HHRefillVol", "HHRefillVol1", "HHSumAprAug", "HHSumAprJul", "HHSumMaySept", "LBRefillVol2", "LBRefillVol1", 
"LBSumAprAug", "LBSumMayAug", "MIRefillVol1", "MIRefillVol2", "MISumRunoffAprilAug", "MISumRunoffMayAug", "PriVICMI", "PriVICREV", "PriVICAR", "PriVICDU", "PriVICLB",
"PriVICBONF", "PriVICCL", "PriVICCOL", "PriVICHH", "PriVICKE", "PriVICNOX", "PriVICCB", "PriVICAF", "PriVICBD", "PriVICGC", "PriVICCJ", "PriVICWE", "PriVICRR", "PriVICRI", 
"PriVICWA", "PriVICPR", "PriVICPAL", "PriVICMIL", "PriVICBR", "PriVICOX", "PriVICHC", "PriVICDW", "PriVICLG", "PriVICLIG", "PriVICLM", "PriVICIH", "PriVICMCN", "PriVICJD", 
"PriVICDA", "PriVICBON", "DemVICAF", "DemVICAR", "DemVICBONF", "DemVICBON", "DemVICBD", "DemVICBR", "DemVICCB", "DemVICCJ", "DemVICCOL", "DemVICCL", "DemVICDA", "DemVICDU", 
"DemVICDW", "DemVICKE", "DemVICHH", "DemVICGC", "DemVICHC", "DemVICIH", "DemVICJD", "DemVICLB", "DemVICLIG", "DemVICLG", "DemVICLM", "DemVICMCN", "DemVICMI", "DemVICMIL", 
"DemVICNOX", "DemVICOX", "DemVICPAL", "DemVICPR", "DemVICREV", "DemVICRI", "DemVICRR", "DemVICWA", "DemVICWE", "RetVICWA", "RetVICPR", "RetVICMCN", "IflowCJ", "IflowDA", "IflowJD", 
"IflowMCN", "IflowPR", "IflowRR", "IflowRI", "IflowWA", "IflowWE", "CurtVICCJ", "CurtVICDA", "CurtVICJD", "CurtVICMCN", "CurtVICPR", "CurtVICRR", "CurtVICRI", "CurtVICWA", "CurtVICWE")

Output_to_ColSim = data.frame(matrix(ncol=length(names_Output), nrow=Number_of_ts, 0))
names(Output_to_ColSim) = names_Output
Output_to_ColSim[,1:4] = Supply_Input[,1:4]
MonthToWeek = read.table("Step_4/MonthToWeek.txt", header=T)
months = data.frame(c(8:12,1:7), 1:12)
month2 = c(1:12)
Output_to_ColSim[,2] = MonthToWeek[match(Output_to_ColSim[,1], MonthToWeek[,1]),2]
Output_to_ColSim[,2] = months[match(Output_to_ColSim[,2], months[,2]),1]
dam_column = match(ListOfDams, names(Supply_Input))

########## Assumed Release
AssumedRelease = read.table("Step_4/AssumedRelease.txt", header=T) # monthly
weekly_assumedRelease = data.frame(matrix(ncol=length(dam_column)+3, nrow=Number_of_ts, 0))
names(weekly_assumedRelease) = c("Week_Number", "month", "year", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
weekly_assumedRelease[1:Number_of_ts,1:3] = Output_to_ColSim[1:Number_of_ts,1:3]
for (i_dam in 1:length(ListOfDams)){
	for (i_m in 1:12){
		weekly_assumedRelease[which(i_m==weekly_assumedRelease[,2]),3+i_dam] = AssumedRelease[which(AssumedRelease[,1]==i_m),1+i_dam]
	}
}
weekly_assumedRelease[,4:12] = weekly_assumedRelease[,4:12] / 0.50417 * 7
############################# Assured Refill
# 1- monthly to weekly (52 weeks in a year)
# this part converts the weekly timeseries to monthly time series

AssuredRefill = read.table("Step_4/AssuredRefill.txt", header=T)
ncols = length(AssuredRefill[1,]) + 2
weekly_AssuredRefill = data.frame(matrix(ncol=ncols, nrow=52, 0))
names(weekly_AssuredRefill) = c("Month", "CalendarMonth", "WeekInMonth", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
weekly_AssuredRefill[,1:3] = MonthToWeek[,2:4]
for (i_ts_week in 1:52){
	N_month = MonthToWeek[i_ts_week,2] # month start from August, therefore month 1 is August, 2 September, and so on
	if(N_month<2){
		weekly_AssuredRefill[i_ts_week,4:ncols] = AssuredRefill[12,2:10] - (AssuredRefill[12,2:10] - AssuredRefill[1,2:10]) / MonthToWeek[i_ts_week,5] * MonthToWeek[i_ts_week,4]
	} else {
		weekly_AssuredRefill[i_ts_week,4:ncols] = AssuredRefill[(N_month-1),2:10] + ((AssuredRefill[N_month,2:10] - AssuredRefill[(N_month-1),2:10]) / MonthToWeek[i_ts_week,5])*MonthToWeek[i_ts_week,4]
	}
} #check
write.table(weekly_AssuredRefill, "Step_4/test_weekly_assured.txt")

# 2- Weekly to each time step
# this part applies the weekly assured refill curves (52 week/year) to all of the years

AssuredRefill_EntireTime = data.frame(matrix(ncol=ncols, nrow=Number_of_ts, 0))
names(AssuredRefill_EntireTime) = c("WeekInYear", "Month","Number_of_Weeks", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
AssuredRefill_EntireTime[,1:2] = Output_to_ColSim[1:Number_of_ts,1:2]
august_firsts = which(AssuredRefill_EntireTime[,2]==8 & AssuredRefill_EntireTime[,1]==1)
Number_of_Years = length(august_firsts)
for(i_year in 1:Number_of_Years){ 
	ind1 = august_firsts[i_year]
	if(i_year==Number_of_Years) {
		ind2 = Number_of_ts
	} else {
		ind2 = august_firsts[i_year + 1] - 1
	}
	L = length(AssuredRefill_EntireTime[ind1:ind2,1])
	if (L==52) {
		AssuredRefill_EntireTime[ind1:ind2,3:ncols] = weekly_AssuredRefill[,3:ncols]
	} else {
		AssuredRefill_EntireTime[ind1:(ind2-1),3:ncols] = weekly_AssuredRefill[1:(L-1),3:ncols]
		AssuredRefill_EntireTime[ind2,3:ncols] = weekly_AssuredRefill[L-1,3:ncols]
	}
}

# assured refil code works fine
####################################### Refill Alternative
# For some of the dams this is RefillVol0 for some of them this is RefillVol2
DamMaxMin = read.table("Step_4/DamMaxMin.txt", header=T)
Refill_Alternative = data.frame(matrix(ncol=ncols , nrow=Number_of_ts, 0))
names(Refill_Alternative) = c("WeekInYear", "Month", "Number_of_Weeks","Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
Refill_Alternative[,1:3] = AssuredRefill_EntireTime[,1:3]
July31s = which(AssuredRefill_EntireTime[,2]==7 & AssuredRefill_EntireTime[,1]==52)
if (July31s[length(July31s)] != nrow(Supply_Input)) {
	July31s = c(July31s, nrow(Supply_Input))
}
Refill_Alternative[July31s,4:ncols] = DamMaxMin[1,1:9]
for(i_week1 in 1:51){
	for (i_dam in 1:9) {
		refill_alt = Refill_Alternative[July31s-i_week1+1,3+i_dam] - (Supply_Input[July31s-i_week1+1,dam_column[i_dam]] - weekly_assumedRelease[July31s-i_week1+1,3+i_dam])   
		refill_alt[refill_alt<DamMaxMin[2,i_dam]] = DamMaxMin[2,i_dam]
		refill_alt[refill_alt>DamMaxMin[1,i_dam]] = DamMaxMin[1,i_dam]
		Refill_Alternative[July31s-i_week1,3+i_dam] = refill_alt
	} 
}
#write.table(Refill_Alternative, "~/Step_4/test_weekly_assured.txt")
#######################################  

RefillStatusQuo = data.frame(matrix(ncol=ncols, nrow=Number_of_ts, 0))
names(RefillStatusQuo) = c("WeekInYear", "Month", "Number_of_Weeks", "Arrow", "Brownlee", "Dalles", "Duncan", "Dworshak", "GrandCoulee", "HungryHorse", "Libby", "Mica")
RefillStatusQuo[,1:3] = AssuredRefill_EntireTime[,1:3]
RefillStatusQuo[,4:ncols] = AssuredRefill_EntireTime[,4:ncols]
write.table(RefillStatusQuo, "Step_4/test_weekly_assured.txt")

##################################################### 
###
###
###               Generating the RColSim input file
###
###
##################################################### 

########################### ARRefillVol1  Column 1
Output_to_ColSim$ARRefillVol1 = RefillStatusQuo[,4]
########################### ARRefillVol2  Column 2
Output_to_ColSim$ARRefillVol2 = Refill_Alternative[,4]
########################### BRRunoffAprJuly # Column 3
tmp = aggregate(Supply_Input$BROWN, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<8), sum)
BRRunoffAprJuly = tmp[apply(tmp[,2:3],1,sum)==2,4]	# The condition, apply(tmp[,2:3],1,sum)==2, selects only the sums for which 3 < month < 8. 
Output_to_ColSim$BRRunoffAprJuly[1:length(BRRunoffAprJuly)] = BRRunoffAprJuly 
##########  BRRefillCurve   # Column 4
Output_to_ColSim$BRRefillCurve = RefillStatusQuo[,5]
######### DallesJanJul  # Column 5
tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month<8), sum)
DallesJanJul = tmp[tmp[,2]==1 & tmp[,1]>begin_year,3]
Output_to_ColSim$DallesJanJul[1:length(DallesJanJul)] = DallesJanJul 
#########  DallesRunoffAprAug  # Column 6
tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum) 
DallesRunoffAprAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$DallesRunoffAprAug[1:length(DallesRunoffAprAug)] = DallesRunoffAprAug 
######### DallesRunoffAprSep  
tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<10), sum)
DallesRunoffAprSep = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$DallesRunoffAprSep[1:length(DallesRunoffAprSep)] = DallesRunoffAprSep 
######### DARunoffJanApr  # Column 8
tmp = aggregate(Supply_Input$DALLE, list(Supply_Input$Year, Supply_Input$Month<5), sum)
DARunoffJanApr = tmp[tmp[,2]==1 & tmp[,1]>begin_year,3]
Output_to_ColSim$DARunoffJanApr[1:length(DARunoffJanApr)] = DARunoffJanApr 
########################### DURefillVol # Column 9
Output_to_ColSim$DURefillVol = Refill_Alternative[,7]
########################### DURefillVol1  # Column 10
Output_to_ColSim$DURefillVol1 = RefillStatusQuo[,7]
########################### DURunoffAprAug   # Column 11
tmp = aggregate(Supply_Input$DUNCA, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum)
DURunoffAprAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$DURunoffAprAug[1:length(DURunoffAprAug)] = DURunoffAprAug
########################### DWRefillVol1  # Column 12
Output_to_ColSim$DWRefillVol1 = RefillStatusQuo[,8]
########################### DWRefillVol2  # Column 13
Output_to_ColSim$DWRefillVol2 = Refill_Alternative[,8]
########################### DWRunoffAprJuly  # Column 14
tmp = aggregate(Supply_Input$DWORS, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=7), sum)
DWRunoffAprJuly = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$DWRunoffAprJuly[1:length(DWRunoffAprJuly)] = DWRunoffAprJuly
########################### DWSumRunoffMarJun  # Column 15
tmp = aggregate(Supply_Input$DWORS, list(Supply_Input$Year, Supply_Input$Month>2, Supply_Input$Month<=6), sum)
DWSumRunoffMarJun = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$DWSumRunoffMarJun[1:length(DWSumRunoffMarJun)] = DWSumRunoffMarJun
########################### GCRefillVol1  # Column 16
Output_to_ColSim$GCRefillVol1 = RefillStatusQuo[,9]
########################### GCRefillVol2  # Column 17
Output_to_ColSim$GCRefillVol2 = Refill_Alternative[,9]  # check
########################### GCRunoffJanApr  # Column 18
tmp = aggregate(Supply_Input$GCOUL, list(Supply_Input$Year, Supply_Input$Month<5), sum)
GCRunoffJanApr = tmp[tmp[,2]==1 & tmp[,1]>begin_year,3]
Output_to_ColSim$GCRunoffJanApr[1:length(GCRunoffJanApr)] = GCRunoffJanApr 
########################### HHInQAprAug  # Column 19 ****
tmp = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum)
HHInQAprAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$HHInQAprAug[1:length(HHInQAprAug)]=HHInQAprAug
########################### HHRefillVol  # Column 20
Output_to_ColSim$HHRefillVol = Refill_Alternative[,10] 
########################### HHRefillVol1  # Column 21
Output_to_ColSim$HHRefillVol1 = RefillStatusQuo[,10] 
########################### HHSumAprAug  # Column 22
tmp = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum) 
HHSumAprAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$HHSumAprAug[1:length(HHSumAprAug)] = HHSumAprAug
########################### HHSumAprJul  # Column 23
tmp = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=7), sum)
HHSumAprJul = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$HHSumAprJul[1:length(HHSumAprJul)] = HHSumAprJul
########################### HHSumMaySept  # Column 24
tmp = aggregate(Supply_Input$FLASF, list(Supply_Input$Year, Supply_Input$Month>4, Supply_Input$Month<=9), sum)
HHSumMaySept = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$HHSumMaySept[1:length(HHSumMaySept)] = HHSumMaySept
########################### LBRefillVol2  # Column 25
Output_to_ColSim$LBRefillVol2 = Refill_Alternative[,11] 
########################### LBRefillVol1  # Column 26
Output_to_ColSim$LBRefillVol1 = RefillStatusQuo[,11] 
########################### LBSumAprAug  # Column 27
tmp = aggregate(Supply_Input$LIBBY, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum)
LBSumAprAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$LBSumAprAug[1:length(LBSumAprAug)] = LBSumAprAug
########################### LBSumMayAug  # Column 28
tmp = aggregate(Supply_Input$LIBBY, list(Supply_Input$Year, Supply_Input$Month>4, Supply_Input$Month<=8), sum)
LBSumMayAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$LBSumMayAug[1:length(LBSumMayAug)] = LBSumMayAug
########################### MIRefillVol1  # Column 29
Output_to_ColSim$MIRefillVol1 = RefillStatusQuo[,12]
########################### MIRefillVol2  # Column 30
Output_to_ColSim$MIRefillVol2 = Refill_Alternative[,12] 
########################### MISumRunoffAprilAug  # Column 28
tmp = aggregate(Supply_Input$MICAA, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<=8), sum) 
MISumRunoffAprilAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$MISumRunoffAprilAug[1:length(MISumRunoffAprilAug)] = MISumRunoffAprilAug
########################### MISumRunoffMayAug  # Column 28
tmp = aggregate(Supply_Input$MICAA, list(Supply_Input$Year, Supply_Input$Month>4, Supply_Input$Month<=8), sum)
MISumRunoffMayAug = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$MISumRunoffMayAug[1:length(MISumRunoffMayAug)] = MISumRunoffMayAug

#------------------------------------------------- FLOW INPUTS ----------------------------------------------

for (i in 1:length(stn_colsim)) {
	var_supply = paste("PriVIC", stn_abbreviations[i], sep="")
	var_demand = paste("DemVIC", stn_abbreviations[i], sep="")
	var_interruptible = paste("CurtVIC", stn_abbreviations[i], sep="")
	Output_to_ColSim[,which(names_Output==var_supply)] = Supply_Input[,names(Supply_Input)==stn_colsim[i]]
	if (simulate_demand == 1) {
		Output_to_ColSim[,which(names_Output==var_demand)] = Demand_Input[,names(Demand_Input)==stn_colsim[i]]
		colnum = which(names(interruptible_demand)==stn_colsim[i])
		if (length(colnum) == 0) {
			Output_to_ColSim[,which(names_Output==var_interruptible)] = rep(0, nrow(Output_to_ColSim))
		} else {
			Output_to_ColSim[,which(names_Output==var_interruptible)] = interruptible_demand[,names(interruptible_demand)==stn_colsim[i]]
		}
	} else {
		Output_to_ColSim[,which(names_Output==var_demand)] = rep(0, nrow(Output_to_ColSim)) 
		Output_to_ColSim[,which(names_Output==var_interruptible)] = rep(0, nrow(Output_to_ColSim)) 
	}
}

ModDA = Supply_Input$DALLE - apply(Output_to_ColSim[,grep("Dem", names_Output)], 1, sum) + Output_to_ColSim$DemVICBON
tmp = aggregate(ModDA, list(Supply_Input$Year, Supply_Input$Month>3, Supply_Input$Month<10), sum)
ModDallesRunoffAprSep = tmp[apply(tmp[,2:3],1,sum)==2 & tmp[,1]>begin_year,4]
Output_to_ColSim$ModDallesRunoffAprSep[1:length(ModDallesRunoffAprSep)] = ModDallesRunoffAprSep 

for (i in 1:length(mainstem_names)) {
	#print(paste(names(Output_to_ColSim)[cols[i]], " = ", mainstem_names[i], sep=""))
	varname = paste("Iflow", stn_abbreviations[stn_colsim==mainstem_names[i]], sep="")
	Output_to_ColSim[,which(names_Output==varname)] = get_iflow_mainstem(mainstem_names[i]) 
}

if (simulate_demand == 1) {
	annual_return = aggregate(supply_for_return_flow[,5], list(supply_for_return_flow$Year), sum)
	Output_to_ColSim$RetVICWA = return_fractions[match(supply_for_return_flow$Week, 1:52),1] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
	Output_to_ColSim$RetVICPR = return_fractions[match(supply_for_return_flow$Week, 1:52),2] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
	Output_to_ColSim$RetVICMCN = return_fractions[match(supply_for_return_flow$Week, 1:52),3] * annual_return[match(supply_for_return_flow$Year, annual_return[,1]),2]
} else {
	Output_to_ColSim$RetVICWA = rep(0, length(Output_to_ColSim[,1])) 
	Output_to_ColSim$RetVICPR = rep(0, length(Output_to_ColSim[,1])) 
	Output_to_ColSim$RetVICMCN = rep(0, length(Output_to_ColSim[,1])) 
}

if (!dir.exists(paste("Step_4/output/Forecast/", run_type, sep=""))) {
	dir.create(paste("Step_4/output/Forecast/", run_type, sep=""))
}
if (!dir.exists(paste("Step_4/output/Forecast/", run_type, "/Historical_baseline", sep=""))) {
	dir.create(paste("Step_4/output/Forecast/", run_type, "/Historical_baseline", sep=""))
}

write.table(Output_to_ColSim, file=paste("Step_4/output/Forecast/", run_type, "/Historical_baseline/ToRColSim_scenario_baseline_", curtailment_ext, ".txt", sep=""), row.names=F)

##################### Make Global Input File #############################################

global_input_file = "Historical_baseline"
GIF = data.frame(matrix(nrow=4, ncol=2))
GIF[,1] = c("RColSim_WD", "Flow_Input_File", "N_of_TimeSteps", "Output_Folder")
GIF[1,2] = paste(getwd(), "/Step_5/RColSim", sep="")
GIF[2,2] = paste(getwd(), "/Step_4/output/Forecast/", run_type, "/Historical_baseline/ToRColSim_scenario_baseline_", curtailment_ext, ".txt", sep="")
GIF[3,2] = length(read.table(GIF[2,2], header=T)[,1])
GIF[4,2] = paste(getwd(), "/Step_5/output/Forecast/", run_type, "/Historical_baseline/", sep="")

if (!dir.exists(paste("Step_5/output/Forecast/", run_type, sep=""))) {
	dir.create(paste("Step_5/output/Forecast/", run_type, sep=""))
}
if (!dir.exists(paste("Step_5/output/Forecast/", run_type, "/Historical_baseline", sep=""))) {
  dir.create(paste("Step_5/output/Forecast/", run_type, "/Historical_baseline", sep=""))
}
if (!dir.exists(paste("Step_5/RColSim/inputs/Forecast/", run_type, sep=""))) {
	dir.create(paste("Step_5/RColSim/inputs/Forecast/", run_type, sep=""))
}
if (!dir.exists(paste("Step_5/RColSim/inputs/Forecast/", run_type, "/Historical_baseline", sep=""))) {
  dir.create(paste("Step_5/RColSim/inputs/Forecast/", run_type, "/Historical_baseline", sep=""))
}

write.table(GIF, paste("Step_5/RColSim/inputs/Forecast/", run_type, "/Historical_baseline/GIF_", global_input_file, sep=""), col.names=F, row.names=F, quote=F)






