
get_iflow_mainstem = function(station) {
	ifile = read.table(paste('~/Bias_correction/flow_weekly_bc/', station, '_iflow_rules', sep=""))
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


get_columns <- function(dam, flow_map) {
	ind = dam
	next_up = flow_map$Up[flow_map$Down==dam]
	while (length(next_up) > 0) {
		ind = c(ind, next_up)
		if (length(next_up) == 1) {
			next_up = flow_map$Up[flow_map$Down==next_up]
		} else {
			for (n in next_up) {
				nnext_up = flow_map$Up[flow_map$Down==n]
				while (length(nnext_up) > 0) {
					ind = c(ind, nnext_up)
					if (length(nnext_up) == 1) {					
						nnext_up = flow_map$Up[flow_map$Down==nnext_up]
					} else { 
						for (nn in nnext_up) {
							nnnext_up = flow_map$Up[flow_map$Down==nn]
							while (length(nnnext_up) > 0) {
								ind = c(ind, nnnext_up)
								if (length(nnnext_up) == 1) {
									nnnext_up = flow_map$Up[flow_map$Down==nnnext_up]
								} else {
									for (nnn in nnnext_up) {
										nnnnext_up = flow_map$Up[flow_map$Down==nnn]
										while (length(nnnnext_up) > 0) {
											ind = c(ind, nnnnext_up)
											if (length(nnnnext_up) == 1) {
												nnnnext_up = flow_map$Up[flow_map$Down==nnnnext_up]
											} else {
												for (nnnn in nnnnext_up) {
													nnnnnext_up = flow_map$Up[flow_map$Down==nnnn]
													while (length(nnnnnext_up) > 0) {
														ind = c(ind, nnnnnext_up)
														if (length(nnnnnext_up) == 1) {
															nnnnnext_up = flow_map$Up[flow_map$Down==nnnnnext_up]
														} else {
															for (nnnnn in nnnnnext_up) {
																nnnnnnext_up = flow_map$Up[flow_map$Down==nnnnn]
																while (length(nnnnnnext_up) > 0) {
																	ind = c(ind, nnnnnnext_up)
																	nnnnnnext_up = flow_map$Up[flow_map$Down==nnnnnnext_up]
																}
															}
															nnnnnext_up = character(0)
														}
													}
												}
												nnnnext_up = character(0)
											}
										}
									}
									nnnext_up = character(0)
								}
							}
						}
						nnext_up = character(0)
					}
				}
			}
			next_up = character(0)
		}
	}
	return(ind)
}

get_rule_curves <- function(res) {
	if (res == "MICAA") {
		flood_o <- MIFloodCurve()
		critical_o <- MICriticalCurve()
		assured_o <- MIAssuredRefill()
		lower_o <- MILowerLimit()
		variable_o <- VariableRefillCurve$MICAA[row_num]
		BiOp_o <- MIFullPoolVol
	} else if (res == "ARROW") {
		flood_o <- ARFloodCurve()
		critical_o <- ARCriticalCurve()
		assured_o <- ARAssuredRefill()
		lower_o <- ARLowerLimit()
		variable_o <- VariableRefillCurve$ARROW[row_num]
		BiOp_o <- ARFullPoolVol		
	} else if (res == "DUNCA") {
		flood_o <- DUFloodCurve()
		critical_o <- DUCriticalCurve()
		assured_o <- DUAssuredRefill()
		lower_o <- DULowerLimit()
		variable_o <- VariableRefillCurve$DUNCA[row_num]
		BiOp_o <- DUFullPoolVol	
	} else if (res == "LIBBY") {
		flood_o <- LBFloodCurve()
		critical_o <- LBCriticalCurve()
		assured_o <- LBAssuredRefill()
		lower_o <- LBLowerLimit()
		variable_o <- VariableRefillCurve$LIBBY[row_num]
		BiOp_o <- LibbyBiOpDraftLimit()
	} else if (res == "FLASF") {
		flood_o <- HHFloodCurve()
		critical_o <- HHCriticalCurve()
		assured_o <- HHAssuredRefill()
		lower_o <- HHLowerLimit()
		variable_o <- VariableRefillCurve$FLASF[row_num]
		BiOp_o <- HHBiOpDraftLimit()	
	} else if (res == "FLAPO") {
		flood_o <- KEFloodCurve()
		critical_o <- KECriticalCurve()
		assured_o <- KEAssuredRefill()
		lower_o <- 0
		variable_o <- KEFullPoolVol
		BiOp_o <- KEFullPoolVol
	} else if (res == "ALBEN") {
		flood_o <- AFFloodCurve()
		critical_o <- AFCriticalCurve()
		assured_o <- AFAssuredRefill()
		lower_o <- 0
		variable_o <- AFFullPoolVol
		BiOp_o <- AFFullPoolVol		
	} else if (res == "CORRA") {
		flood_o <- CLFloodCurve()
		critical_o <- CLCriticalCurve()
		assured_o <- 0
		lower_o <- 0
		variable_o <- AFFullPoolVol
		BiOp_o <- AFFullPoolVol		
	} else if (res == "BROWN") {
		flood_o <- BRFloodCurve()
		critical_o <- BRCriticalCurve()
		assured_o <- BRAssuredRefill()
		lower_o <- 0
		variable_o <- BRFullPoolVol
		BiOp_o <- BRFullPoolVol
	} else if (res == "DWORS") {
		flood_o <- DWFloodCurve()
		critical_o <- DWCriticalCurve()
		assured_o <- DWAssuredRefill()
		lower_o <- DWLowerLimit()
		variable_o <- VariableRefillCurve$DWORS[row_num]
		BiOp_o <- DWBiOpDraftLimit()
	} else if (res == "GCOUL") {
		flood_o <- GCFloodCurve()
		critical_o <- GCCriticalCurve()
		assured_o <- GCAssuredRefill()
		lower_o <- GCLowerLimit()
		variable_o <- VariableRefillCurve$GCOUL[row_num]
		BiOp_o <- GCFullPoolVol
	}
	return(c(flood_o, critical_o, assured_o, lower_o, BiOp_o, variable_o))
}



calc_refill_curve <- function(type) {
	RefillCurve <- data.frame(matrix(ncol=length(ListOfDams)+3 , nrow=N, 0))
	names(RefillCurve) <- c("Week", "Month", "Year", ListOfDams)
	RefillCurve[1:3] <- timeseries[c("Week", "Month", "Year")]
	for (dname in ListOfDams) {
		wk <- target_refill_week$Week[target_refill_week$Dam==dname]
		RefillCurve[July31s[,dname],dname] <- DamMaxMin[1,dname]
		for (y in 1:n_years) {
			if (type == "regular") {
				if (DA_forecast$Q[y] <= 80E6) {
					weekly_assumedRelease <- PDR_80[-1]
				} else if (DA_forecast$Q[y] <= 95E6) {
					weekly_assumedRelease <- (PDR_95[-1] - PDR_80[-1]) / (95E6 - 80E6) * (DA_forecast$Q[y] - 80E6) + PDR_80[-1]
				} else if (DA_forecast$Q[y] <= 110E6) {
					weekly_assumedRelease <- (PDR_110[-1] - PDR_95[-1]) / (110E6 - 95E6) * (DA_forecast$Q[y] - 95E6) + PDR_95[-1]
				} else {
					weekly_assumedRelease <- PDR_110[-1]
				}
			} else if (type == "minimum") {
				weekly_assumedRelease <- PDR_lower[-1]
			}
			names(weekly_assumedRelease) <- as.character(dam_lookup$name1[match(names(weekly_assumedRelease), dam_lookup$name2)])
			nw <- nws[y,dname]
			if (wk < 40) {
				i_week_max <- nw - 1
			} else {
				i_week_max <- ifelse(y==1, wk - 1, nw - 1)
			}
			for(i_week in 1:i_week_max) {
				row_num1 <- July31s[y,dname] - (i_week - 1)
				if (wk == 52) {
					row_num2 <- max(1, wk - (i_week - 1))
				} else {
					if (wk - (i_week - 1) > 0) {
						row_num2 <- wk - (i_week - 1)
					} else {
						row_num2 <- max(wk + 1, wk - (i_week - 1) + 52)
					}
				}
				if (type == "regular") {
					refill <- RefillCurve[row_num1,dname] - modified_flow[row_num1,dname] + forecast_error[row_num2,dname] + weekly_assumedRelease[row_num2,dname] * cfsTOafw 
				} else if (type == "minimum") {
					refill <- RefillCurve[row_num1,dname] - modified_flow[row_num1,dname] + weekly_assumedRelease[row_num2,dname] * cfsTOafw
				}
				refill[refill < DamMaxMin[2,dname]] <- DamMaxMin[2,dname]
				refill[refill > DamMaxMin[1,dname]] <- DamMaxMin[1,dname]
				RefillCurve[row_num1-1,dname] <- refill			
			}
		}
		if (wk > 40) {
			RefillCurve[RefillCurve$Week>=wk | RefillCurve$Week<=22,dname] = DamMaxMin[1,dname]
		} else {
			RefillCurve[RefillCurve$Week %in% wk:22,dname] <- DamMaxMin[1,dname]
			RefillCurve[1:wk,dname] <- RefillCurve[53:(52+wk),dname]
		}
	}
	if (type == "regular") {
		write.table(RefillCurve, "~/Step_4/test_weekly_assured_before_refill.txt", col.names=T, row.names=F, quote=F)
	} else if (type == "minimum") {
		write.table(RefillCurve, "~/Step_4/test_weekly_assured_before_refill_min.txt", col.names=T, row.names=F, quote=F)
	}	
	return(RefillCurve)
}

calc_refill_with_upstream_storage <- function(type, res) {
	RefillCurve <- vector(length=N, mode="numeric")
	RefillCurve[July31s[,res]] <- DamMaxMin[1,res]
	wk <- target_refill_week$Week[target_refill_week$Dam == res]
	for (y in 1:n_years) {
		if (type == "regular") {
			if (DA_forecast$Q[y] <= 80E6) {
				weekly_assumedRelease <- PDR_80[-1]
			} else if (DA_forecast$Q[y] <= 95E6) {
				weekly_assumedRelease <- (PDR_95[-1] - PDR_80[-1]) / (95E6 - 80E6) * (DA_forecast$Q[y] - 80E6) + PDR_80[-1]
			} else if (DA_forecast$Q[y] <= 110E6) {
				weekly_assumedRelease <- (PDR_110[-1] - PDR_95[-1]) / (110E6 - 95E6) * (DA_forecast$Q[y] - 95E6) + PDR_95[-1]
			} else {
				weekly_assumedRelease <- PDR_110[-1]
			}
		} else if (type == "minimum") {
			weekly_assumedRelease <- PDR_lower[-1]
		}
		names(weekly_assumedRelease) <- as.character(dam_lookup$name1[match(names(weekly_assumedRelease), dam_lookup$name2)])
		nw <- nws[y,res]
		i_week_max = ifelse(y==1, wk - 1, nw - 1)
		if (res == "ARROW") {
			if (type == "regular") {
				upstream_refill <- MIRuleCurves.df$RefillReq
			} else if (type == "minimum") {
				upstream_refill <- MIRefill_min
			}
		} else if (res == "GCOUL") {
			if (type == "regular") {
				upstream_refill <- GCUpstreamRefill
			} else if (type == "minimum") {
				upstream_refill <- GCUpstreamRefill_min
			}
		}
		for (i_week in 1:i_week_max) {
			row_num1 <- July31s[y, res] - (i_week - 1)
			if (wk == 52) {
				row_num2 <- max(1, wk - (i_week - 1))
			} else {
				if (wk - (i_week - 1) > 0) {
					row_num2 <- wk - (i_week - 1)
				} else {
					row_num2 <- max(wk + 1, wk - (i_week - 1) + 52)
				}
			}
			if (type == "regular") {
				refill <- RefillCurve[row_num1] - modified_flow[row_num1,res] + forecast_error[row_num2,res] + weekly_assumedRelease[row_num2,res] * cfsTOafw + upstream_refill[row_num1-1]  
			} else if (type == "minimum") {
				refill <- RefillCurve[row_num1] - modified_flow[row_num1,res] + weekly_assumedRelease[row_num2,res] * cfsTOafw + upstream_refill[row_num1-1]
			}
			refill[refill<DamMaxMin[2,res]] <- DamMaxMin[2,res]
			refill[refill>DamMaxMin[1,res]] <- DamMaxMin[1,res]
			RefillCurve[row_num1-1] <- refill				
		}
	}
	if (wk > 40) {
		RefillCurve[timeseries$Week>=wk | timeseries$Week<=22] <- DamMaxMin[1,res]
	} else {
		RefillCurve[timeseries$Week %in% wk:22] <- DamMaxMin[1,res]
		RefillCurve[1:wk] <- RefillCurve[53:(52+wk)]
	}
	return(RefillCurve)
}





RuleCurve_df <- function(res) {
	RuleCurves = data.frame(matrix(nrow=N, ncol=11))
	RuleCurves[,1:4] = Output_to_ColSim[,1:4]
	RuleCurves[,5] = ifelse(RuleCurves[,2]>7, RuleCurves[,4] + 1, RuleCurves[,4])
	names(RuleCurves) = c("Week", "Month", "Day", "Year", "DY", "Flood", "Critical", "AssuredRefill", "LowerLimit", "BiOp", "VariableRefill")
	for (y in 1:length(years)) {
		if (res == "MICAA") {
			DARunoffAprAug <<- Output_to_ColSim$DARunoffAprAug[y]
			MIRunoffMayAug <<- Output_to_ColSim$MIRunoffMayAug[y]
			MIRunoffAprAug <<- Output_to_ColSim$MIRunoffMayAug[y]			
		} else if (res == "ARROW") {
			DARunoffAprAug <<- Output_to_ColSim$DARunoffAprAug[y]
		} else if (res == "DUNCA") {
			DURunoffAprAug <<- Output_to_ColSim$DURunoffAprAug[y]
		} else if (res == "LIBBY") {
			LBRunoffAprAug <<- Output_to_ColSim$LBRunoffAprAug[y]
		} else if (res == "FLASF") {
			HHRunoffMaySep <<- Output_to_ColSim$HHRunoffMaySep[y]
		} else if (res == "BROWN") {
			DARunoffAprAug <<- Output_to_ColSim$DARunoffAprAug[y]
			BRRunoffAprJul <<- Output_to_ColSim$BRRunoffAprJul[y]
		} else if (res == "DWORS") {
			DWRunoffAprJul <<- Output_to_ColSim$DWRunoffAprJul[y]
		} else if (res == "BROWN") {
			DARunoffAprAug <<- Output_to_ColSim$DARunoffAprAug[y]
		} else if (res == "GCOUL") {
			CorrectedDARunoffAprAug <<- Output_to_ColSim$CorrectedDARunoffAprAug[y]
		}
		for (wk in 1:52) {
			week_in_year <<- wk
			row_num <<- which(RuleCurves$Week == week_in_year & RuleCurves$DY == years[y])
			if (length(row_num) == 2) {
				RuleCurves[row_num,6:11] <- c(rep(get_rule_curves(res)[1:5],each=2), get_rule_curves(res)[6:7])
			} else if (length(row_num) == 1) {
				RuleCurves[row_num,6:11] <- get_rule_curves(res)
			}
		}
	}
	RuleCurves$OperatingRuleCurve <- pmin(pmax(pmin(pmax(RuleCurves$AssuredRefill, RuleCurves$Critical), RuleCurves$VariableRefill), RuleCurves$LowerLimit), RuleCurves$Flood, RuleCurves$BiOp)
	RuleCurves$RefillReq <- c(RuleCurves$OperatingRuleCurve[2:N] - RuleCurves$OperatingRuleCurve[1:(N-1)], 0)
	RuleCurves[is.na(RuleCurves)] = DamMaxMin[1,res]
	write.table(RuleCurves, paste0("~/Step_4/", res, "RefillCurves.txt"), row.names=F, col.names=T, quote=F)
	return(RuleCurves)
}
DAResidualInflow <- function(S) {
	mod_flow <- subset(modified_flow, Month %in% 4:8)[c("Week", "Month", "Day", "Year", "DALLE")]
	storage <- subset(S, Month %in% 4:8)[c("Week", "Month", "Day", "Year", "Sum")]
	mod_flow.df <- data.frame(matrix(nrow=nrow(Output_to_ColSim), ncol=3))
	mod_flow.df[1:2] <- Output_to_ColSim[c("Month", "Year")]
	mod_flow.df[3] <- 0
	names(mod_flow.df) <- c("Month", "Year", "Flow")
	for (y in years) {
		year_flow <- subset(mod_flow, Year==y)
		year_storage <- subset(storage, Year==y)
		cumflow <- cumsum(year_flow$DALLE)
		total_inflow <- sum(year_flow$DALLE)
		mod_flow.df[which(mod_flow.df$Year==y & mod_flow.df$Month %in% 4:8),3] <- head(c(total_inflow - year_storage$Sum[1], pmax(0, total_inflow - cumflow - year_storage$Sum)), -1)
	}
	return(mod_flow.df[,3])
}

get_ICF <- function(res_inflow, wk) {
	if (res_inflow <= 30e6) {
		ICF <- 9999
	} else {
		upper_inflow <- flow_inc[which(flow_inc >= res_inflow)[1]]
		lower_inflow <- flow_inc[tail(which(flow_inc <= res_inflow), 1)]
		upper_CF <- ICF_table[wk, which(flow_inc >= res_inflow)[1]+1]
		lower_CF <- ICF_table[wk, tail(which(flow_inc <= res_inflow), 1)+1]
		ICF <- lower_CF + (upper_CF - lower_CF) / (upper_inflow - lower_inflow) * (res_inflow - lower_inflow)
	}
	ICF_o <- 1000 * ICF
	return(ICF_o)
}
runoff_remaining <- function(stn, start_wk, end_wk, flow) {
	mod_flow <- subset(flow, Week %in% start_wk:end_wk)[c("Week", "Month", "Day", "Year", stn)]
	mod_flow.df <- data.frame(matrix(nrow=nrow(Output_to_ColSim), ncol=3))
	mod_flow.df[1:2] <- Output_to_ColSim[c(1,4)]
	mod_flow.df[3] <- 0
	for (y in unique(mod_flow$Year)) {
		year_flow <- subset(mod_flow, Year==y)
		cumflow <- cumsum(year_flow[stn])[,1]
		total_inflow <- sum(year_flow[stn])
		mod_flow.df[which(mod_flow.df[,2]==y & mod_flow.df[,1] %in% start_wk:end_wk),3] <- pmax(0, total_inflow - cumflow)
	}
	return(mod_flow.df[,3])
}