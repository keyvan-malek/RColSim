library(xts)
scr <- commandArgs()[6]
run_type <- commandArgs()[7] # must either be "supply_only" or "supply_and_demand"

cfsTOafw <- 13.8838
get_iflow <- function(time_series, station) {
	ifile <- read.table(paste0('inputs/', station, '_iflow_rules'))
	Months <- as.numeric(strftime(time_series,"%m"))
	Days <- as.numeric(strftime(time_series,"%d"))
	instream <- as.numeric(matrix(nrow=length(Days), ncol=1, 0))
	for (line in 1:length(ifile[,1])) {
		mo <- ifile[line,1]
		first_day <- ifile[line,2]
		last_day <- ifile[line,3]
		instream[Months==mo & Days>=first_day & Days<=last_day] <- ifile[line,4]
	}
	return(instream)
}
scr_name <- sub("_", "/", scr)


stn_list <- read.table("inputs/stn_code_name", header=F, stringsAsFactors=F)[,1]
stn_list <- stn_list[-which(stn_list == "LISPO")]
pod_stns <- read.table("inputs/stn_code_name", header=F, stringsAsFactors=F)[,1]
pod_stns <- c(pod_stns, "OWYHE_ID")
stn_colsim <- read.table("inputs/RColSim_stations.txt", header=T, stringsAsFactors=F)[,1]
stn_iflow <- c("SIMNI","METPA","WENMO","WENPE","OKANA","OKANO", "COLKE", "CHIEF", "DALLE", "JDAYY", "MCNAR", "PRIRA", "ROCKY", "RISLA", "WANAP", "WELLS") ## Control points for instream flow rules
trib_names <- c("SIMNI","METPA","WENMO","WENPE","OKANA","OKANO", "COLKE") ## Control points along tributaries to the Columbia R.
mainstem_names <- c("CHIEF", "DALLE", "JDAYY", "MCNAR", "PRIRA", "ROCKY", "RISLA", "WANAP", "WELLS") ## Control points along the Columbia mainstem

if(scr_name == "Historical/baseline") {
	scr_name <- "Historical_baseline/baseline"
	begin_date <- as.Date("1979-01-01", "%Y-%m-%d")
	end_date <- as.Date("2015-12-31", "%Y-%m-%d")
} else if (length(grep("historical", scr_name)) == 1) {
	begin_date <- as.Date("1950-01-01", "%Y-%m-%d")
	end_date <- as.Date("2005-12-31", "%Y-%m-%d")
} else {
	begin_date <- as.Date("2006-01-01", "%Y-%m-%d") 
	end_date <- as.Date("2094-12-31", "%Y-%m-%d")
}

##################################### Natural Supply #######################################################
########## Create weekly bias-corrected flow from bias-corrected monthly flow and raw daily flow ###########

nrows <- as.numeric(end_date - begin_date + 1)
daily_supply <- data.frame(matrix(ncol=(length(stn_list)+3), nrow=nrows, -9999))
routdir <- "Preliminary/routed_flow/"
biasdir <- "Preliminary/bc_flow/"

for (ii_stn in 1:length(stn_list)) {
	print(ii_stn)
	if (scr_name == "Historical_baseline/baseline") {
		flow_raw_month <- read.table(paste0(routdir, strsplit(scr_name, "/")[[1]][1], "/", stn_list[ii_stn], ".month"), header=F) ## Routed monthly flows
		flow_raw_day <- read.table(paste0(routdir, strsplit(scr_name, "/")[[1]][1], "/", stn_list[ii_stn], ".day"), header=F) ## Routed daily flows
		flow_bc_month <- read.table(paste0(biasdir, strsplit(scr_name, "/")[[1]][1], "/", stn_list[ii_stn], ".month.bc.ff"), header=F) ## Bias-corrected monthly flows
	} else {
		flow_bc_month <- read.table(paste0(biasdir, scr_name, "/" , stn_list[ii_stn], ".month.bc.ff"), header=F)
		if (strsplit(scr_name, "/")[[1]][2] == "historical") {
			flow_raw_month <- read.table(paste0(routdir, strsplit(scr_name, "/")[[1]][1], "/", "historical/", stn_list[ii_stn], ".month"), header=F)
			flow_raw_day <- read.table(paste0(routdir, strsplit(scr_name, "/")[[1]][1], "/", "historical/", stn_list[ii_stn], ".day"), header=F)
		} else {
			flow_raw_month <- read.table(paste0(routdir, scr_name, "/", stn_list[ii_stn], ".month"), header=F)				
			flow_raw_day <- read.table(paste0(routdir, scr_name, "/", stn_list[ii_stn], ".day"), header=F)
		}
	}
	flow_bc_day <- flow_raw_day
	number_of_month <- (length(unique(flow_raw_month[,1]))) * 12
	for (ii_mo in 1:number_of_month) {
		month <- flow_raw_month[ii_mo,2]
		year <- flow_raw_month[ii_mo,1]
		diff_month <- flow_raw_month[ii_mo,3] / flow_bc_month[ii_mo,]
		corresponding_month <- which(flow_raw_day[,2]==month & flow_raw_day[,1]==year)			
		flow_bc_day[corresponding_month,4] <- flow_raw_day[corresponding_month,4] / diff_month
	}
	daily_supply[,(ii_stn+3)] <- flow_bc_day[,4]
}
daily_supply[,1:3] <- flow_raw_day[,1:3]
L_col1 <- length(stn_list) + 3
L_col2 <- length(pod_stns)
xts_supply <- xts(daily_supply[1:nrows,4:L_col1], seq(from=begin_date, to=end_date, by=1))
weekly_supply <- apply.weekly(xts_supply, mean) * cfsTOafw ## Calculate weekly flow from daily flow
dates = as.Date(row.names(as.data.frame(weekly_supply)),"%Y-%m-%d")
if (scr_name == "Historical_baseline/baseline") {
	start_index <- which(dates==as.Date("1979-08-05", "%Y-%m-%d"))
	end_index <- which(dates==as.Date("2015-09-27", "%Y-%m-%d"))
} else if (length(grep("hist", scr_name)) == 1) {
	start_index <- which(dates==as.Date("1950-08-06", "%Y-%m-%d"))
	end_index <- which(dates==as.Date("2005-09-25", "%Y-%m-%d"))
} else {
	start_index <- which(dates==as.Date("2006-08-06", "%Y-%m-%d"))
	end_index <- which(dates==as.Date("2094-09-26", "%Y-%m-%d"))
}

weekly_supply$OXBOW <- weekly_supply$BROWN ## The NRNI flow for Brownlee and Oxbow use the same gauge
names(weekly_supply) <- stn_list
weekly_supply <- as.data.frame(weekly_supply)

############################### Irrigation demands ############################################

if (run_type == "supply_and_demand") {
	if (scr_name == "Historical_baseline/baseline") {
		daily_demand <- read.table(paste0("Preliminary/irrigation_demands/Historical_baseline/Historical_baseline_demands.txt"))
		daily_interruptible_demand <- read.table(paste0("Preliminary/irrigation_demands/Historical_baseline/Historical_baseline_interruptible_demands.txt"))
	} else {
		daily_demand <- read.table(paste0("Preliminary/irrigation_demands/", scr_name, "_demands.txt"))
		daily_interruptible_demand <- read.table(paste0("Preliminary/irrigation_demands/", scr_name, "_interruptible_demands.txt"))
	}
	xts_demand <- xts(daily_demand[1:nrows,1:L_col2], seq(from=begin_date, to=end_date, by=1))
	xts_interruptible_demand <- xts(daily_interruptible_demand[1:nrows,1:L_col2], seq(from=begin_date, to=end_date, by=1))
	weekly_demand <- apply.weekly(xts_demand, mean) * cfsTOafw
	interruptible_weekly <- apply.weekly(xts_interruptible_demand, mean) * cfsTOafw
	names(interruptible_weekly) <- pod_stns 
	interruptible_weekly <- as.data.frame(interruptible_weekly)
	names(weekly_demand) <- pod_stns
		
	################# Add Residential Demand ############################

	MGtoAFM <- 3.068893 
	muni_demand <- read.csv("inputs/Final_Muni_Demand.csv", stringsAsFactors=F) ## Consumptive municiple water demand by subbasin in Washington State 
	WRIA_to_pod_areas <- read.csv("inputs/WRIA_pod_areas.csv", stringsAsFactors=F) 
	WRIA_areas <- aggregate(WRIA_to_pod_areas[,5], list(WRIA_to_pod_areas[,1]), sum)
	WRIA_to_pod_areas$WRIA_Area <- WRIA_areas[match(WRIA_to_pod_areas[,1], WRIA_areas[,1]),2]
	WRIA_to_pod_areas$Fraction <- WRIA_to_pod_areas[,5] / WRIA_to_pod_areas[,6]
	if(scr_name == "Historical_baseline/baseline") {
		decade <- 2020
	} else if (length(grep("historical", scr_name)) == 1) {
		decade <- 2020
	} else {
		decade <- c(2020, 2030, 2040)
	}
	muni_demand <- muni_demand[muni_demand$Year %in% decade,]
	monthly_muni <- data.frame(matrix(nrow=0, ncol=5))
		
	################## Split the residential demand by WRIA among pod demand drainage areas ##############################################
		
	for (r in 1:nrow(WRIA_to_pod_areas)) {
		muni_r <- muni_demand[muni_demand[,1]==WRIA_to_pod_areas$WRIA_NR[r],5] * WRIA_to_pod_areas$Fraction[r] * MGtoAFM
		muni.df <- data.frame(rep(WRIA_to_pod_areas$WRIA_NR[r], 12), rep(WRIA_to_pod_areas$StnName[r], 12), rep(decade, each=12), rep(1:12, length(decade)), muni_r)
		monthly_muni <- rbind(monthly_muni, muni.df)
	}
	names(monthly_muni) <- c("WRIA_ID", "StnID", "Decade", "Month", "Demand")
	monthly_muni <- aggregate(monthly_muni$Demand, list(monthly_muni$StnID, monthly_muni$Month, monthly_muni$Decade), sum)
	WRIA_names <- as.character(unique(monthly_muni[,1]))
	monthly_muni <- sapply(WRIA_names, function(x) { monthly_muni[which(monthly_muni[,1]==x),4] })

	############### Generate a weekly time series of residential demands from the monthly time series ####################################

	weekly_muni <- data.frame(matrix(nrow=length(dates), ncol=ncol(monthly_muni), 0))
	create_weekly <- data.frame(Year=as.numeric(strftime(dates,"%Y")), Month=as.numeric(strftime(dates,"%m")), 
		Decade=ifelse(as.numeric(strftime(dates,"%Y")) <= 2020, 2020, ifelse(as.numeric(strftime(dates,"%Y")) <= 2030, 2025, 
		ifelse(as.numeric(strftime(dates,"%Y")) <= 2040, 2035, 2040))))
	tmp <- aggregate(create_weekly$Month, list(create_weekly$Year, create_weekly$Month), function(x) {length(x)})
	create_weekly$NumWeeks <- tmp[match(paste(create_weekly$Year, create_weekly$Month), paste(tmp[,1], tmp[,2])), 3]
	for (i in 1:nrow(create_weekly)) {
		if (create_weekly$Decade[i] == 2020) {
			weekly_muni[i,] <- monthly_muni[create_weekly$Month[i],] / create_weekly$NumWeeks[i] 
		} else if (create_weekly$Decade[i] == 2025) {
			weekly_muni[i,] <- (monthly_muni[create_weekly$Month[i],] + monthly_muni[create_weekly$Month[i]+12,]) / 2 / create_weekly$NumWeeks[i]
		} else if (create_weekly$Decade[i] == 2035) {
			weekly_muni[i,] <- (monthly_muni[create_weekly$Month[i]+12,] + monthly_muni[create_weekly$Month[i]+24,]) / 2 / create_weekly$NumWeeks[i]
		} else {
			weekly_muni[i,] <- monthly_muni[create_weekly$Month[i]+24,] / create_weekly$NumWeeks[i] 
		}
	}
	names(weekly_muni) <- colnames(monthly_muni)
	weekly_muni$WALST <- 0		# municipal water has gw source
	weekly_muni$COLKE <- 0		# municipal water has gw source
	#weekly_muni[weekly_muni>0] = 0
		
	######################### Total demands ######################################

	row.names(weekly_muni) <- dates
	total_demands <- data.frame(matrix(nrow=nrow(weekly_muni), ncol=length(pod_stns), 0))
	for (i in 1:length(pod_stns)) {
		colnum1 <- which(names(weekly_muni)==pod_stns[i])
		colnum2 <- which(names(weekly_demand)==pod_stns[i])
		if (length(colnum1)==0) {
			total_demands[,i] <- weekly_demand[,colnum2]
		} else {
			total_demands[,i] <- weekly_demand[,colnum2] + weekly_muni[,colnum1]
		}
	}
	names(total_demands) <- pod_stns	
		
	######################### Tributary Curtailment ###############################	

	SWFraction <- read.table("inputs/interruptible_sw_fractions.txt", header=T)
	iflows <- data.frame(matrix(nrow=length(dates), ncol=length(pod_stns), 0))
	for(i in 1:length(stn_iflow)) {
		col = which(pod_stns==stn_iflow[i])
		if(length(col) == 0) {
			next
		}
		iflows[,col] <- get_iflow(dates, stn_iflow[i]) * cfsTOafw	
	}
	names(iflows) <- pod_stns

	curtailment <- data.frame(matrix(nrow=length(dates), ncol=length(stn_list), 0))
	instream_shortfall <- data.frame(matrix(nrow=length(dates), ncol=length(stn_list), 0))
	names(curtailment) <- names(instream_shortfall) <- stn_list
	for (st in stn_list) {
		print(st)
		arg <- as.numeric(apply(cbind(iflows[,st] + total_demands[,st] - weekly_supply[,st], rep(0, length(dates))), 1, max))
		curtailment[,st] <- ifelse(arg>0, interruptible_weekly[,st], 0)
		remaining_instream <- weekly_supply[,st] - (total_demands[,st] - curtailment[,st])
		instream_shortfall[,st] <- iflows[,st] - apply(cbind(remaining_instream, iflows[,st]), 1, min)
	}
	curtailment_trib <- curtailment
	curtailment_trib[,which(!(stn_list %in% trib_names))] <- 0

	################## Calculate demands after accounting for curtailment ##############################################
	adj_demand = total_demands
	for (st in pod_stns) {
		if (st %in% stn_list) {
			if (!(st %in% SWFraction$Stn_ID)) {
				adj_demand[,st] <- total_demands[,st]
			} else {
				adj_demand[,st] <- total_demands[,st] - curtailment_trib[,st] * SWFraction$Fraction[which(SWFraction$Stn_ID == st)]  ##### We only want to remove the surface water curtailments for input to RColSim
			}
		} else {
			adj_demand[,st] <- total_demands[,st]
		}
	}
	### We need to make additional adjustments to OKANA and WENMO because each have upstream gauges
	arg1 <- as.numeric(apply(cbind(iflows$OKANO + total_demands$OKANO + adj_demand$SIMNI - weekly_supply$OKANO, rep(0, length(dates))), 1, max))
	curtailment_trib$OKANO <- ifelse(arg1>0, interruptible_weekly$OKANO, 0)
	adj_demand$OKANO <- total_demands$OKANO - curtailment_trib$OKANO * SWFraction$Fraction[SWFraction$Stn_ID=="OKANO"]
	arg1 <- as.numeric(apply(cbind(iflows$OKANA + total_demands$OKANA + adj_demand$OKANO + adj_demand$SIMNI - weekly_supply$OKANA, rep(0, length(dates))), 1, max))
	curtailment_trib$OKANA <- ifelse(arg1>0, interruptible_weekly$OKANA, 0)
	adj_demand$OKANA <- total_demands$OKANA - curtailment_trib$OKANA * SWFraction$Fraction[SWFraction$Stn_ID=="OKANA"]
	arg2 <- as.numeric(apply(cbind(iflows$WENMO + total_demands$WENMO + adj_demand$WENPE - weekly_supply$WENMO, rep(0, length(dates))), 1, max))
	curtailment_trib$WENMO <- ifelse(arg2>0, interruptible_weekly$WENMO, 0)
	adj_demand$WENMO <- total_demands$WENMO - curtailment_trib$WENMO * SWFraction$Fraction[SWFraction$Stn_ID=="WENMO"]
	print ("done with adj_demand")

	######################### Calculate tributary instream flow deficit ##########################################
		
	actual_flow <- data.frame(matrix(nrow=nrow(weekly_supply), ncol=length(stn_list)))
	names(actual_flow) <- stn_list
	for (st in stn_list) {
		if (st == "WENMO") {
			remaining_instream <- weekly_supply$WENMO - total_demands$WENMO - total_demands$WENPE
			instream_shortfall$WENMO <- iflows$WENMO - apply(cbind(remaining_instream, iflows$WENMO), 1, min)
			actual_flow$WENMO <- remaining_instream + curtailment_trib$WENMO * SWFraction$Fraction[SWFraction$Stn_ID=="WENMO"]
		} else if (st == "OKANA") {
			remaining_instream <- weekly_supply$OKANA - total_demands$OKANA - total_demands$OKANO - total_demands$SIMNI
			instream_shortfall$OKANA <- iflows$OKANA - apply(cbind(remaining_instream, iflows$OKANA), 1, min)
			actual_flow$OKANA <- remaining_instream + curtailment_trib$OKANA * SWFraction$Fraction[SWFraction$Stn_ID=="OKANA"]
		} else {	
			remaining_instream <- weekly_supply[,st] - total_demands[,st]
			instream_shortfall[,i] <- iflows[,st] - apply(cbind(remaining_instream, iflows[,st]), 1, min)
			if (st %in% stn_iflow) {
				actual_flow[,st] <- remaining_instream + curtailment_trib[,st] * SWFraction$Fraction[SWFraction$Stn_ID==st]
			}
		}
	}
	actual_flow <- actual_flow[,which(names(actual_flow) %in% trib_names)]

	############################ aggregate the demand aggregation areas to the RColSim drainages ######################## 
	UpSnakeCorrectionFactor <- 1/.65 * 1.25
	UpSnakeCorrectionFactor2 <- 1/.75 * 1.25
	apply_correction <- c("SNKHI", "MILNE", "PALIS", "HFORK", "IPARK", "SNKHE", "OWYHE", "OWYHE_ID", "AMERI", "MINAD", "RIRDM", "BROWN", "OXBOW", "HCANY", "JLAKE")
	apply_correction2 <- c("LUCKY", "LBOIS", "PAYET", "PAYHS")
	adj_demand[,apply_correction] <- UpSnakeCorrectionFactor * adj_demand[,apply_correction]
	adj_demand[,apply_correction2] <- UpSnakeCorrectionFactor2 * adj_demand[,apply_correction2]
	map_stations <- read.csv("inputs/station_mapping", sep="\t", header=F, stringsAsFactors=F) ## maps the demand drainage areas to the RColSim drainage areas
	weekly_demand_agg <- adj_demand[,which(pod_stns %in% stn_colsim)]   
	new_pod_names <- pod_stns[pod_stns %in% stn_colsim]
	for(i in 1:length(new_pod_names)) {
		stations_to_sum <- map_stations[map_stations[,1] == new_pod_names[i],2]
		weekly_demand_agg[,i] <- apply(as.matrix(adj_demand[,match(stations_to_sum, pod_stns)]), 1, sum)
	}
	weekly_demand_agg$BoiseSys <- adj_demand$LBOIS         ## Demand for the Boise River system
	weekly_demand_agg$Minidoka <- adj_demand$MINAD + adj_demand$MILNE + adj_demand$SNKHI   ## Demand for the Minidoka project
	weekly_demand_agg$Owyhee <- adj_demand$OWYHE_ID ## Demand for Owyhee Irrigation District
	weekly_demand_agg$UpSnake <- adj_demand$AMERI + adj_demand$RIRDM ## Demand for UpperSnake system
	weekly_demand_agg$Payette <- adj_demand$PAYET ## Demand for Payette system
	weekly_demand_agg$SNKHE <- adj_demand$SNKHE
	weekly_demand_agg$HFORK <- adj_demand$HFORK
	weekly_demand_agg$LIMEP <- adj_demand$LIMEP
}
#################### Write Output #################################	

final_weekly_supply <- data.frame(matrix(nrow=nrow(weekly_supply), ncol=length(stn_colsim)))
for (ii_s in 1:length(stn_colsim)) {
	col_nu <- which(stn_list==stn_colsim[ii_s])
	final_weekly_supply[,ii_s] <- weekly_supply[,col_nu]		
}
names(final_weekly_supply) <- stn_colsim	
final_weekly_supply$SNKHE <- weekly_supply$SNKHE
final_weekly_supply$OXBOW <- weekly_supply$BROWN
final_weekly_supply$HFORK <- weekly_supply$HFORK
final_weekly_supply$LIMEP <- weekly_supply$LIMEP

if (scr_name == "Historical_baseline/baseline") {
	timeseries <- read.table("inputs/ts_historical.txt", header=T) 
} else if (length(grep("hist", scr_name)) == 1) {
	timeseries <- read.table("inputs/ts_hist.txt", header=T)
} else {
	timeseries <- read.table("inputs/ts_GCM.txt", header=T)
}

outdir <- paste0("Preliminary/output/", run_type, "/", strsplit(scr_name, "/")[[1]][1])
if (!dir.exists(outdir)) { dir.create(outdir, recursive=T) }
setwd(outdir)

supply <- cbind(timeseries, final_weekly_supply[start_index:end_index,])
write.table(supply, file=paste0("supply_", strsplit(scr_name, "/")[[1]][2],  ".txt"), row.names=F)

if (run_type == "supply_and_demand") {
	demand <- cbind(timeseries, data.frame(weekly_demand_agg[start_index:end_index,]))
	iflow <- cbind(timeseries, iflows[start_index:end_index,which(names(iflows) %in% mainstem_names)])
	interruptible <- cbind(timeseries, data.frame(interruptible_weekly[start_index:end_index, which(names(interruptible_weekly) %in% mainstem_names)]))
	names(demand)[1:4] <- names(iflow)[1:4] <- names(interruptible)[1:4] <- c("Week", "Month", "Day", "Year")
	write.table(iflow, file=paste0("iflow_", strsplit(scr_name, "/")[[1]][2], ".txt"), row.names=F)
	write.table(interruptible, file=paste0("interruptible_", strsplit(scr_name, "/")[[1]][2], ".txt"), row.names=F)
	write.table(demand, file=paste0("demand_", strsplit(scr_name, "/")[[1]][2], ".txt"), row.names=F)
}













