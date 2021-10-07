# week_counter= time steps of model does NOT set to zero each year
# The Unit is AF/week
# reading the input file

VIC_Data <- function() {
  
  ####### supply, demands, curtailment ########
  terms = c("PriVIC", "DemVIC", "Iflow", "CurtVIC", "Vol")
  for (j in terms) {
    cols = grep(j, names(input_file))
    for (i in cols) {
      var = names(input_file)[i]
      assign(var, pos=1, input_file[week_counter + num_lines_to_skip - 1,names(input_file)==var])
    }
  }
  
  ####### other inputs ##########
  BRRefillCurve <<- input_file$BRRefillCurve[week_counter + num_lines_to_skip - 1]
  BRRunoffAprJuly <<- input_file$BRRunoffAprJuly[year_counter + num_years_to_skip]
  DallesJanJul <<- input_file$DallesJanJul[year_counter + num_years_to_skip]
  DallesRunoffAprAug <<- input_file$DallesRunoffAprAug[year_counter + num_years_to_skip]
  DallesRunoffAprSep <<- input_file$DallesRunoffAprSep[year_counter + num_years_to_skip]
  DURunoffAprAug <<- input_file$DURunoffAprAug[year_counter + num_years_to_skip]
  DWRunoffAprJuly <<- input_file$DWRunoffAprJuly[year_counter + num_years_to_skip]
  GCRunoffJanApr <<- input_file$GCRunoffJanApr[year_counter + num_years_to_skip]
  HHInQ_AprAug <<- input_file$HHInQAprAug[year_counter + num_years_to_skip]
  HHSumMaySept <<- input_file$HHSumMaySept[year_counter + num_years_to_skip]
  LBSumAprAug <<- input_file$LBSumAprAug[year_counter + num_years_to_skip]
  MISumRunoffAprilAug <<- input_file$MISumRunoffAprilAug[year_counter + num_years_to_skip]
  MISumRunoffMayAug <<- input_file$MISumRunoffMayAug[year_counter + num_years_to_skip]

}


