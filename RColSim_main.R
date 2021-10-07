                          ##################################################################################################################
                          ##################################################################################################################
                                                            ################################################
                                                            #                                              #
                                                            #                                              #
                                                            #                    R-ColSim                  #
                                                            #                                              #
                                                            #                                              #
                                                            ################################################
                          ##################################################################################################################
                          ##################################################################################################################                                                            
                                                            
                                                                #######################################
                                                                #                                     #
                                                                #         Principle Developers:       #
                                                                #                                     #
                                                                #           Keyvan Malek              #
                                                                #           Matthew Yourek            #
                                                                #                                     #
                                                                #######################################
                                                            
                                                              


# -------------------------------------------------------------------------------------------------------------------------------------------
#
#                                                    Read the global input file
#                                                            
# -------------------------------------------------------------------------------------------------------------------------------------------
                                                            

scr = "Historical_baseline"

# -------- Read the global input file
print(paste("Now doing scenario:", scr))
GlobalFile = read.table(paste("~/papers/RColSim/August 2021/For_Keyvan/Step_5/RColSim/inputs/Global_Input_File_Historical_baseline.txt", sep=""), stringsAsFactors=F)


# -------------------------------------------------------------------------------------------------------------------------------------------
#
#                                                    Prepare input files and date time series for simulation
#                                                            
# -------------------------------------------------------------------------------------------------------------------------------------------

input_file = read.table(GlobalFile[2,2], header=T)
print(paste("Reading:", GlobalFile[2,2]))
setwd(GlobalFile[1,2])

datetxt_sim <- as.Date(paste(input_file$month, "-", input_file$day, "-", input_file$year, sep=""), format = "%m-%d-%Y")
input_start_date <- datetxt_sim[1] # Current input file starts on "1979-08-05"
input_end_date <- datetxt_sim[length(datetxt_sim)] # Current input file ends on "2015-09-27"

# Input data time frame
date_hist_sim_0 <- data.frame(date = datetxt_sim,
                              month = as.numeric(format(datetxt_sim, format = "%m")),
                              week = input_file$Week_Number,
                              day = as.numeric(format(datetxt_sim, format = "%d")),
                              year = as.numeric(format(datetxt_sim, format = "%Y")))

# Simulation start and end date
start_year = GlobalFile[4,2]
simulation_start_date <- datetxt_sim[which(datetxt_sim > paste(start_year, "-07-31", sep=""))[1]]
simulation_end_date <- GlobalFile[5,2] #"2015-09-27"

lines_to_keep = which(datetxt_sim >= simulation_start_date & datetxt_sim <= simulation_end_date) 
num_lines_to_skip <<- which(datetxt_sim >= simulation_start_date)[1]
num_years_to_skip <<- as.numeric(start_year) - as.numeric(format(as.Date(input_start_date), format = "%Y"))

date_hist_sim = date_hist_sim_0[lines_to_keep,]
N_of_TimeSteps = length(date_hist_sim[,1])

# ------- Load functions and rule curves
# 5 files need to be loaded in the following order: 1- LoadFunctions.R 2- Read_Rule_Curves.R 3- Switches.R  4- dataframes.R 5- Measures of performance

# 1- LOAD ALL FUNCTIONS
source("LoadFunctions.R")
# 2- READ ALL INPUT FILES
source("Read_Rule_Curves.R")
Read_Rule_Curves()
# 3- DEFINE SWITCHES AND DEFAULTS
source("Switches.R")
ReadSwitches()
# 4- CREATE DATAFRAMES
source("dataframes.R")
# 5- LOAD PMFs
source("PMFs.R")
# OUTPUT FILE
OutputFolder=GlobalFile[3,2]

###### READ INPUT DATA FOR EACH WEEK
source("VIC_Data.R")


# ----------------- New simulation?
#------------------- Attention!!!!!!!!!!!!!!
# If new simulation is TRUE the program removes all file in the output directory

NEW_SIMULATION = TRUE

##################################################################################################################
##################################################################################################################
                                  #######################################
                                  #                                     #
                                  #                                     #
                                  #          WEEKLY TIME-STEP           #
                                  #                                     #
                                  #                                     #
                                  #######################################
##################################################################################################################
##################################################################################################################
I_Week = 1
for (I_Week in 1:N_of_TimeSteps){
  
  if(I_Week==1){ # Model initialization
    
    print(paste("initialization"))
    
    week_counter = 1
    no_week_in_year = week_counter_in_year() # Find the week of the year (1-52)
    month_in_year =  ifelse(input_file$month[I_Week]>=8, input_file$month[I_Week] - 7, input_file$month[I_Week] + 5) #Convert from calendar month to reservoir operation month
    year_counter = year_from_weekly()
    
    ###### READ INPUT DATA FOR WEEK 1
    VIC_Data()
    
    ############### Common weekly variables
    BRPrelim_c = -9999
    TotalFloodSpace_c = -9999
    ARFirmEngSupReq_c = -9999
    ARFirmEngSup_c = -9999
    TotalEnergyContent_c = -9999
    TotalECCEnergyContent_c = -9999
    FirmEnergyDeficit_c = -9999
    TotalCoordPreEnergy_c = -9999
    TotalNFEnergyContent_c = -9999
    NonFirmEnergyDeficit_c = -9999
    TotalMcNarySharedWater_c = -9999		
    
    # ---------------- Initialize the model
    source("initialize_model.R")
    
    
  } else{ # Actual simulation begins here
    
    print(paste("time step=", I_Week))
    # FIND THE RIGHT TIME STEP
    week_counter = I_Week
    no_week_in_year = week_counter_in_year()
    month_in_year =  ifelse(input_file$month[I_Week]>=8, input_file$month[I_Week] - 7, input_file$month[I_Week] + 5)
    year_counter = year_from_weekly()
    
    ###### READ INPUT DATA FOR EACH WEEK
    VIC_Data()
    ############### COMMON WEEKLY VARIABLES
    BRPrelim_c = -9999
    TotalFloodSpace_c = -9999
    ARFirmEngSupReq_c = -9999
    ARFirmEngSup_c = -9999
    TotalEnergyContent_c = -9999
    TotalECCEnergyContent_c = -9999
    FirmEnergyDeficit_c = -9999
    TotalCoordPreEnergy_c = -9999
    TotalNFEnergyContent_c = -9999
    NonFirmEnergyDeficit_c = -9999
    TotalMcNarySharedWater_c = -9999
    
    MicaRelease_c = MIRelease()
    dams_in[I_Week,1] = MIInflow()
    dams_out[I_Week,1] = MIOutflow()
    
    dams_in[I_Week,2] = REVIn()
    dams_out[I_Week,2] = REVOut()
    
    ARRelease_c = ARRelease()
    dams_in[I_Week,3] = ARInflow()
    dams_out[I_Week,3] = AROutflow()
    
    DUOutflow_c = DURelease() 
    dams_in[I_Week,4] = DUInflow()
    dams_out[I_Week,4] = DUOutflow_c
    
    dams_in[I_Week,5] = LBInflow()
    dams_out[I_Week,5] = LBOutflow()
    
    BonnersFerry_c = BonnersFerry() 
    dams_in[I_Week,6] = BonnersFerry_c
    dams_out[I_Week,6] = BonnersFerry_c
    
    CLRelease_c = CLRelease() 
    dams_in[I_Week,7] = CLInflow()
    dams_out[I_Week,7] = CLOutflow()
    
    HHRelease_c = HHRelease()
    dams_in[I_Week,8] = HHInflow()
    dams_out[I_Week,8] = HHOutflow()
    
    KERelease_c = KERelease()
    dams_in[I_Week,9] = KEInflow()
    dams_out[I_Week,9] = KEOutflow()
    
    dams_in[I_Week,10] = NOXIn()
    dams_out[I_Week,10] = NOXOut()
    
    dams_in[I_Week,11] = CBIn()
    dams_out[I_Week,11] = CBOut()
    
    AFRelease_c = AFRelease()
    dams_in[I_Week,12] = AFInflow()
    dams_out[I_Week,12] = AFOutflow()
    
    dams_in[I_Week,13] = BDIn()
    dams_out[I_Week,13] = BDOut()
    
    GCRelease_c = GCRelease()
    dams_in[I_Week,14] = GCInflow()
    dams_out[I_Week,14] = GCOutflow()
    
    dams_in[I_Week,15] = CJIn()
    dams_out[I_Week,15] = CJOut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,1] = CJCurtail()        
      mainstem_shortfall[I_Week,1] = CJInstreamShortfall()
    }
    
    dams_in[I_Week,16] = WEIn()
    dams_out[I_Week,16] = WEOut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,2] = WECurtail()
      mainstem_shortfall[I_Week,2] = WEInstreamShortfall()
    }
    
    dams_in[I_Week,17] = RRIn()
    dams_out[I_Week,17] = RROut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,3] = RRCurtail()
      mainstem_shortfall[I_Week,3] = RRInstreamShortfall()
    }
    
    dams_in[I_Week,18] = RIIn()
    dams_out[I_Week,18] = RIOut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,4] = RICurtail()
      mainstem_shortfall[I_Week,4] = RIInstreamShortfall()
    }
    
    dams_in[I_Week,19] = WAIn()
    dams_out[I_Week,19] = WAOut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,5] = WACurtail()
      mainstem_shortfall[I_Week,5] = WAInstreamShortfall()
    }
    
    dams_in[I_Week,20] = PRIn()
    dams_out[I_Week,20] = PROut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,6] = PRCurtail()
      mainstem_shortfall[I_Week,6] = PRInstreamShortfall()
    }
    
    dams_in[I_Week,21] = USInflow()
    dams_out[I_Week,21] = USOutflow()
    
    dams_in[I_Week,22] = MSInflow()
    dams_out[I_Week,22] = MSOutflow()
    
    BRRelease_c = BRRelease()
    dams_in[I_Week,23] = BRInflow()
    dams_out[I_Week,23] = BROutflow()
    
    dams_in[I_Week,24] = OXIn()
    dams_out[I_Week,24] = OXOut()
    
    dams_in[I_Week,25] = HCIn()
    dams_out[I_Week,25] = HCOut()
    
    DWRelease_c = DWRelease()
    dams_in[I_Week,26] = DWInflow()
    dams_out[I_Week,26] = DWOutflow()
    
    dams_in[I_Week,27] = LGIn()
    dams_out[I_Week,27] = LGOut()	
    
    dams_in[I_Week,28] = LIGIn()
    dams_out[I_Week,28] = LIGOut()
    
    dams_in[I_Week,29] = LMIn()
    dams_out[I_Week,29] = LMOut()
    
    dams_in[I_Week,30] = IHIn()
    dams_out[I_Week,30] = IHOut()
    
    dams_in[I_Week,31] = MCNIn()
    dams_out[I_Week,31] = MCNOut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,7] = MCNCurtail()
      mainstem_shortfall[I_Week,7] = MCNInstreamShortfall()
    }
    
    dams_in[I_Week,32] = JDIn()
    dams_out[I_Week,32] = JDOut()
    if (track_curtailment==1) {
      mainstem_curtailments[I_Week,8] = JDCurtail()
      mainstem_shortfall[I_Week,8] = JDInstreamShortfall()
    }
    
    dams_in[I_Week,33] = DAIn()
    dams_out[I_Week,33] = DAOut()
    if (track_curtailment==1) {	
      mainstem_curtailments[I_Week,9] = DACurtail()
      mainstem_shortfall[I_Week,9] = DAInstreamShortfall()
    }
    
    dams_in[I_Week,34] = BONIn()
    dams_out[I_Week,34] = BONOut()
    
    ##################################################################################################################
    ##################################################################################################################
                                  #######################################
                                  #                                     #
                                  #                                     #
                                  #          Write Model Output         #
                                  #                                     #
                                  #                                     #
                                  #######################################
    ##################################################################################################################
    ##################################################################################################################
    
    reservoir_vol_df[I_Week,1] = reservoir_vol_df[I_Week-1,1] + (dams_in[I_Week,1] - dams_out[I_Week,1]) # MICAA RESERVOIR
    reservoir_vol_df[I_Week,2] = reservoir_vol_df[I_Week-1,2] + (dams_in[I_Week,3] - dams_out[I_Week,3]) # ARROW RESERVOIR
    reservoir_vol_df[I_Week,3] = reservoir_vol_df[I_Week-1,3] + (dams_in[I_Week,4] - dams_out[I_Week,4]) # DUNCAN RESERVOIR
    reservoir_vol_df[I_Week,4] = reservoir_vol_df[I_Week-1,4] + (dams_in[I_Week,7] - dams_out[I_Week,7]) # CORRA LINN RESERVOIR 
    reservoir_vol_df[I_Week,5] = reservoir_vol_df[I_Week-1,5] + (dams_in[I_Week,5] - dams_out[I_Week,5]) # LIBBY RESERVOIR
    reservoir_vol_df[I_Week,6] = reservoir_vol_df[I_Week-1,6] + (dams_in[I_Week,8] - dams_out[I_Week,8]) # HUNGRY HORSE RESERVOIR
    reservoir_vol_df[I_Week,7] = reservoir_vol_df[I_Week-1,7] + (dams_in[I_Week,14] - dams_out[I_Week,14]) # GRAND COULEE RESERVOIR
    reservoir_vol_df[I_Week,8] = reservoir_vol_df[I_Week-1,8] + (dams_in[I_Week,26] - dams_out[I_Week,26]) # DWORSHAK RESERVOIR
    reservoir_vol_df[I_Week,9] = reservoir_vol_df[I_Week-1,9] + (dams_in[I_Week,23] - dams_out[I_Week,23]) # BROWNLEE RESERVOIR
    reservoir_vol_df[I_Week,10] = reservoir_vol_df[I_Week-1,10] + (dams_in[I_Week,21] - dams_out[I_Week,21]) # UPPER SNAKE COMPOSITE RESERVOIR
    reservoir_vol_df[I_Week,11] = reservoir_vol_df[I_Week-1,11] + (dams_in[I_Week,22] - dams_out[I_Week,22]) # MIDDLE SNAKE COMPOSITE RESERVOIR

    ###### MOPs Measures Of Performance
    MOP_df[I_Week,1] = shortfall() # Firm Energy Performance Metrics ==> if(shortfall()>0.001) --> does not meet target
    MOP_df[I_Week,2] = shortfall_2() # Non-Firm Energy Performance Metric ==> if(shortfall_2()>0.001) --> does not meet target
    MOP_df[I_Week,3] = shortfall_5() # Columbia Falls Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
    MOP_df[I_Week,4] = shortfall_6() # Lower Granite Flow Metrics ==> if(shortfall_5()>0.001) --> does not meet target
    MOP_df[I_Week,5] = shortfall_7() # Vernita Bar Flow Target Metrics ==> if(shortfall_7()>0.001) --> does not meet target
    MOP_df[I_Week,6] = shortfall_8() # McNary Flow Target Metrics ==> if(shortfall_8()>0.001) --> does not meet target
    MOP_df[I_Week,7] = shortfall_9() # Grand Coulee Recreation Metrics ==> if greater than zero "0" does not meet target flow
    MOP_df[I_Week,8] = shortfall_10() # Dalles Flood Protection Metrics ==> if greater than zero "0" does not meet target flow
    MOP_df[I_Week,9] = shortfall_11() # Ice Harbor Flow Metrics ==> if(shortfall_11()>0.001) --> does not meet target
    MOP_df[I_Week,10] = ChumSF() # Bonneville Winter Chum Flow Metrics ==> if greater than zero "0" does not meet target flow
    MOP_df[I_Week,11] = BelowFCC() # Flood Pool below  curve
    MOP_df[I_Week,12] = FirmEnergySales()
    MOP_df[I_Week,13] = NonFirmSpotSales()
    MOP_df[I_Week,14] = MaxSystemEnergy()
    
    print(paste("Simulation date = ", as.Date(date_hist_sim[I_Week,1])))
    print(Sys.time())
    print(paste("---------------------------------------------------------"))
    
    write.table(cbind(date_hist_sim[I_Week,], dams_out[I_Week,]), paste(OutputFolder, "/dams_out.txt", sep=""), row.names=F, col.names=F, append=T)
    write.table(cbind(date_hist_sim[I_Week,], dams_in[I_Week,]), paste(OutputFolder, "/dams_in.txt", sep=""), row.names=F, col.names=F, append=T)
    write.table(cbind(date_hist_sim[I_Week,], reservoir_vol_df[I_Week,]), paste(OutputFolder, "/reservoir_volume.txt", sep=""), row.names=F, col.names=F, append=T)
    write.table(cbind(date_hist_sim[I_Week,], MOP_df[I_Week,]), paste(OutputFolder, "/MOP_df.txt", sep=""), row.names=F, col.names=F, append=T)
    write.table(cbind(date_hist_sim[I_Week,], water_df[I_Week,]), paste(OutputFolder, "/water.txt", sep = ""), row.names=F, col.names=F, append=T)
    write.table(cbind(date_hist_sim[I_Week,], energy_df[I_Week,]), paste(OutputFolder, "/energy.txt", sep = ""), row.names=F, col.names=F, append=T)
    write.table(cbind(date_hist_sim[I_Week,], mainstem_curtailments[I_Week,]), paste(OutputFolder, "/mainstem_curtailment.txt", sep=""), row.names=F, col.names=F, append=T)
    if (track_curtailment==1) {
      write.table(cbind(date_hist_sim[I_Week,], other_curtailments[I_Week,]), paste(OutputFolder, "/other_curtailment.txt", sep=""), row.names=F, col.names=F, append=T)
      write.table(cbind(date_hist_sim[I_Week,], mainstem_shortfall[I_Week,]), paste(OutputFolder, "/mainstem_shortfall.txt", sep=""), row.names=F, col.names=F, append=T)
    }
  } # I_week > 1
} # Weekly loop



