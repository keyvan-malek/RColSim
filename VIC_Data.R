# week_counter= time steps of model does NOT set to zero each year
# The Unit is AF/week
# reading the input file

VIC_Data <- function() {
	####### supply, demands, curtailment ########
	terms <- c("Flow", "Dem", "Iflow", "Curt", "Variable", "Ret", "Residual", "Refill", "OperatingRuleCurve", "ECCurve")
	for (j in terms) {
		cols = grep(j, names(input_file))
		for (i in cols) {
			var <- names(input_file)[i]
			assign(var, pos=1, input_file[week_counter + num_lines_to_skip,var])
		}
	}
	cols = grep("Runoff", names(input_file))
	for (i in cols) {
		var <- names(input_file)[i]
		assign(var, pos=1, input_file[year_counter + num_years_to_skip,var])
	}
	terms <- c("OperatingRuleCurve", "FloodCurve")
	for (j in terms) {
		cols <- grep(j, names(input_file))
		for (i in cols) {
			var <- names(input_file)[i]
			april <- paste0(names(input_file)[i], "_Apr")
			jan <- paste0(names(input_file)[i], "_Jan")
			assign(april, pos=1, input_file[which(input_file$Week == 36)[year_counter + num_years_to_skip],var])
			assign(jan, pos=1, input_file[which(input_file$Week == 23)[year_counter + num_years_to_skip],var])	
		}
	}
	start_refill_wk <<- input_file[year_counter + num_years_to_skip, "start_refill_wk"]
	start_refill_wk_GC <<- input_file[year_counter + num_years_to_skip, "start_refill_wk_GC"]
}


