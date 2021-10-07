
# This script initializes the model and create necessary output files


if(NEW_SIMULATION == TRUE){
  do.call(file.remove, list(list.files(paste(OutputFolder, "/", sep = ""), full.names = TRUE)))
}

if (I_Week==1) {
  ############################ initialize reservoirs
  reservoir_vol_df[1,1] = MicaReservoir()
  reservoir_vol_df[1,2] = ArrowReservoir()
  reservoir_vol_df[1,3] = Duncan()
  reservoir_vol_df[1,4] = CorraLinnReservoir()
  reservoir_vol_df[1,5] = Libby()
  reservoir_vol_df[1,6] = HungryHorse()
  reservoir_vol_df[1,7] = GrandCoulee()
  reservoir_vol_df[1,8] = Dworshak()
  reservoir_vol_df[1,9] = Brownlee()
  reservoir_vol_df[1,10] = UpSnakeComb()
  reservoir_vol_df[1,11] = MdlSnakeComb()
}

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

######### STORAGE FOR THE SECOND TIME STEP
reservoir_vol_df[1,1] = reservoir_vol_df[1,1] + (dams_in[1,1] - dams_out[1,1]) # MICAA RESERVOIR
reservoir_vol_df[1,2] = reservoir_vol_df[1,2] + (dams_in[1,3] - dams_out[1,3]) # ARROW RESERVOIR
reservoir_vol_df[1,3] = reservoir_vol_df[1,3] + (dams_in[1,4] - dams_out[1,4]) # DUNCAN RESERVOIR
reservoir_vol_df[1,4] = reservoir_vol_df[1,4] + (dams_in[1,7] - dams_out[1,7]) # CORRA LINN RESERVOIR 
reservoir_vol_df[1,5] = reservoir_vol_df[1,5] + (dams_in[1,5] - dams_out[1,5]) # LIBBY RESERVOIR
reservoir_vol_df[1,6] = reservoir_vol_df[1,6] + (dams_in[1,8] - dams_out[1,8]) # HUNGRY HORSE RESERVOIR
reservoir_vol_df[1,7] = reservoir_vol_df[1,7] + (dams_in[1,14] - dams_out[1,14]) # GRAND COULEE RESERVOIR
reservoir_vol_df[1,8] = reservoir_vol_df[1,8] + (dams_in[1,26] - dams_out[1,26]) # DWORSHAK RESERVOIR
reservoir_vol_df[1,9] = reservoir_vol_df[1,9] + (dams_in[1,23] - dams_out[1,23]) # BROWNLEE RESERVOIR
reservoir_vol_df[1,10] = reservoir_vol_df[1,10] + (dams_in[1,21] - dams_out[1,21]) # UPPER SNAKE COMPOSITE RESERVOIR
reservoir_vol_df[1,11] = reservoir_vol_df[1,11] + (dams_in[1,22] - dams_out[1,22]) # MIDDLE SNAKE COMPOSITE RESERVOIR

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


write.table(cbind(date_hist_sim[I_Week,], dams_out[1,]), paste(OutputFolder, "/dams_out.txt", sep=""), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[I_Week,], dams_in[1,]), paste(OutputFolder, "/dams_in.txt", sep=""), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[I_Week,], reservoir_vol_df[1,]), paste(OutputFolder, "/reservoir_volume.txt", sep=""), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[I_Week,], MOP_df[1,]), paste(OutputFolder, "/MOP_df.txt", sep=""), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[I_Week,], water_df[1,]), paste(OutputFolder, "/water.txt", sep = ""), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[I_Week,], energy_df[1,]), paste(OutputFolder, "/energy.txt", sep = ""), row.names=F, col.names=T, append=F)
if (track_curtailment==1) {
  write.table(cbind(date_hist_sim[I_Week,], mainstem_shortfall[1,]), paste(OutputFolder, "/mainstem_shortfall.txt", sep=""), row.names=F, col.names=T, append=F)
  write.table(cbind(date_hist_sim[I_Week,], mainstem_curtailments[1,]), paste(OutputFolder, "/mainstem_curtailment.txt", sep=""), row.names=F, col.names=T, append=F)
}

##################################################################################################################
##################################################################################################################
##################################################################################################################
##################################################################################################################
