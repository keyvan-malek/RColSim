
# This script initializes the model and create necessary output files


if(NEW_SIMULATION == TRUE){
	do.call(file.remove, list(list.files(OutputFolder, full.names=TRUE)))
}

reservoir_vol_df$MICAA[1] <- Mica()
reservoir_vol_df$ARROW[1] <- Arrow()
reservoir_vol_df$DUNCA[1] <- Duncan()
reservoir_vol_df$CORRA[1] <- CorraLinn()
reservoir_vol_df$LIBBY[1] <- Libby()
reservoir_vol_df$FLASF[1] <- HungryHorse()
reservoir_vol_df$GCOUL[1] <- GrandCoulee()
reservoir_vol_df$DWORS[1] <- Dworshak()
reservoir_vol_df$BROWN[1] <- Brownlee()
reservoir_vol_df$FLAPO[1] <- Kerr()
reservoir_vol_df$ALBEN[1] <- AlbeniFalls()
reservoir_vol_df$CHELA[1] <- Chelan()
reservoir_vol_df$JLAKE[1] <- Jackson()
reservoir_vol_df$PALIS[1] <- Palisades()
reservoir_vol_df$IPARK[1] <- IslandPark()
reservoir_vol_df$RIRDM[1] <- Ririe()
reservoir_vol_df$AMERI[1] <- AmericanFalls()
reservoir_vol_df$MINAD[1] <- Minidoka()
reservoir_vol_df$BOISE[1] <- Boise()
reservoir_vol_df$PAYHS[1] <- Payette()
reservoir_vol_df$OWYHE[1] <- Owyhee()
reservoir_vol_df$PELTO[1] <- Pelton()

MIRelease_c <- MIRelease()
dams_in$MICAA[1] <- MIInflow()
dams_out$MICAA[1] <- MIOutflow()
flood_curve_df$MICAA[1] <- MIFloodCurve()
energy_curve_df$MICAA[1] <- MIECC()

dams_in$REVEL[1] <- REVIn()
dams_out$REVEL[1] <- REVOut()

ARRelease_c <- ARRelease()
dams_in$ARROW[1] <- ARInflow()
dams_out$ARROW[1] <- AROutflow()
flood_curve_df$ARROW[1] <- ARFloodCurve()
energy_curve_df$ARROW[1] <- ARECC()

HHRelease_c <- HHRelease()
dams_in$FLASF[1] <- HHInflow()
dams_out$FLASF[1] <- HHOutflow()
flood_curve_df$FLASF[1] <- HHFloodCurve()
energy_curve_df$FLASF[1] <- HHECC()

#KERelease_c <- KERelease()
dams_in$FLAPO[1] <- KEInflow()
dams_out$FLAPO[1] <- KERelease_c
flood_curve_df$FLAPO[1] <- KEFloodCurve()
energy_curve_df$FLAPO[1] <- KerrECC()

dams_in$THOMF[1] <- TFIn()
dams_out$THOMF[1] <- TFOut()

dams_in$NOXON[1] <- NOXIn()
dams_out$NOXON[1] <- NOXOut()

dams_in$CABIN[1] <- CBIn()
dams_out$CABIN[1] <- CBOut()

#AFRelease_c <- AFRelease()
dams_in$ALBEN[1] <- AFInflow()
dams_out$ALBEN[1] <- AFRelease_c
flood_curve_df$ALBEN[1] <- AFFloodCurve()
energy_curve_df$ALBEN[1] <- AFECC()

dams_in$BOXCA[1] <- BCIn()
dams_out$BOXCA[1] <- BCOut()

dams_in$BOUND[1] <- BDIn()
dams_out$BOUND[1] <- BDOut()

LBRelease_c <- LBRelease()
dams_in$LIBBY[1] <- LBInflow()
dams_out$LIBBY[1] <- LBOutflow()
flood_curve_df$LIBBY[1] <- LBFloodCurve()
energy_curve_df$LIBBY[1] <- LBECC()

dams_in$BONFE[1] <- BONFIn()
dams_out$BONFE[1] <- BONFOut()

#DURelease_c <- DURelease() 
dams_in$DUNCA[1] <- DUInflow()
dams_out$DUNCA[1] <- DURelease_c
flood_curve_df$DUNCA[1] <- DUFloodCurve()
energy_curve_df$DUNCA[1] <- DUECC()

#CLRelease_c <- CLRelease() 
dams_in$CORRA[1] <- CLInflow()
dams_out$CORRA[1] <- CLRelease_c
flood_curve_df$CORRA[1] <- CLFloodCurve()
energy_curve_df$CORRA[1] <- CLECC()

GCRelease_c <- GCRelease()
dams_in$GCOUL[1] <- GCInflow()
dams_out$GCOUL[1] <- GCOutflow()
flood_curve_df$GCOUL[1] <- GCFloodCurve()
energy_curve_df$GCOUL[1] <- GCECC()


dams_in$CHIEF[1] <- CJIn_c
dams_out$CHIEF[1] <- CJOut()
if (track_curtailment == 1) {
	mainstem_curtailments$CHIEF[1] <- CJCurtail()
	mainstem_shortfall$CHIEF[1] <- CJInstreamShortfall()
}

dams_in$WELLS[1] <- WEIn()
dams_out$WELLS[1] <- WEOut()
if (track_curtailment == 1) {
	mainstem_curtailments$WELLS[1] <- WECurtail()
	mainstem_shortfall$WELLS[1] <- WEInstreamShortfall()
}

CHRelease_c <- CHRelease()
dams_in$CHELA[1] <- CHInflow()
dams_out$CHELA[1] <- CHOutflow()
flood_curve_df$CHELA[1] <- CHFloodCurve()

dams_in$ROCKY[1] <- RRIn()
dams_out$ROCKY[1] <- RROut()
if (track_curtailment == 1) {
	mainstem_curtailments$ROCKY[1] <- RRCurtail()
	mainstem_shortfall$ROCKY[1] <- RRInstreamShortfall()
}

dams_in$RISLA[1] <- RIIn()
dams_out$RISLA[1] <- RIOut()
if (track_curtailment == 1) {
	mainstem_curtailments$RISLA[1] <- RICurtail()
	mainstem_shortfall$RISLA[1] <- RIInstreamShortfall()
}

dams_in$WANAP[1] <- WAIn()
dams_out$WANAP[1] <- WAOut()
if (track_curtailment == 1) {
	mainstem_curtailments$WANAP[1] <- WACurtail()
	mainstem_shortfall$WANAP[1] <- WAInstreamShortfall()
}

dams_in$PRIRA[1] <- PRIn()
dams_out$PRIRA[1] <- PROut()
if (track_curtailment == 1) {
	mainstem_curtailments$PRIRA[1] <- PRCurtail()
	mainstem_shortfall$PRIRA[1] <- PRInstreamShortfall()
}

JLRelease_c <- JLRelease()
dams_in$JLAKE[1] <- JLInflow()
dams_out$JLAKE[1] <- JLOutflow()
flood_curve_df$JLAKE[1] <- JLFloodCurve()

PALRelease_c <- PALRelease()
dams_in$PALIS[1] <- PALInflow()
dams_out$PALIS[1] <- PALOutflow()
flood_curve_df$PALIS[1] <- PALFloodCurve()

IPRelease_c <- IPRelease()
dams_in$IPARK[1] <- IPInflow()
dams_out$IPARK[1] <- IPOutflow()
flood_curve_df$IPARK[1] <- IPFloodCurve()

RIRRelease_c <- RIRRelease()
dams_in$RIRDM[1] <- RIRInflow()
dams_out$RIRDM[1] <- RIROutflow()
flood_curve_df$RIRDM[1] <- RIRFloodCurve()

AMRelease_c <- AMRelease()
dams_in$AMERI[1] <- AMInflow()
dams_out$AMERI[1] <- AMOutflow()

MINRelease_c <- MINRelease()
dams_in$MINAD[1] <- MINInflow()
dams_out$MINAD[1] <- MINOutflow()

dams_in$MILNE[1] <- MILIn()
dams_out$MILNE[1] <- MILOut()

BoiseRelease_c <- BoiseRelease()
dams_in$BOISE[1] <- BoiseInflow()
dams_out$BOISE[1] <- BoiseOutflow()
flood_curve_df$BOISE[1] <- BoiseFloodCurve()

PayetteRelease_c <- PayetteRelease()
dams_in$PAYHS[1] <- PayetteInflow()
dams_out$PAYHS[1] <- PayetteOutflow()
flood_curve_df$PAYHS[1] <- PayetteFloodCurve()

OWYRelease_c <- OWYRelease()
dams_in$OWYHE[1] <- OWYInflow()
dams_out$OWYHE[1] <- OWYOutflow()
flood_curve_df$OWYHE[1] <- OWYFloodCurve()

BRRelease_c <- BRRelease()
dams_in$BROWN[1] <- BRInflow()
dams_out$BROWN[1] <- BROutflow()
flood_curve_df$BROWN[1] <- BRFloodCurve()
energy_curve_df$BROWN[1] <- BRECC()

dams_in$OXBOW[1] <- OXIn()
dams_out$OXBOW[1] <- OXOut()

dams_in$HCANY[1] <- HCIn()
dams_out$HCANY[1] <- HCOut()

DWRelease_c <- DWRelease()
dams_in$DWORS[1] <- DWInflow()
dams_out$DWORS[1] <- DWOutflow()
flood_curve_df$DWORS[1] <- DWFloodCurve()
energy_curve_df$DWORS[1] <- DWECC()

dams_in$LGRAN[1] <- LGIn()
dams_out$LGRAN[1] <- LGOut()	

dams_in$LGOOS[1] <- LIGIn()
dams_out$LGOOS[1] <- LIGOut()

dams_in$LMONU[1] <- LMIn()
dams_out$LMONU[1] <- LMOut()

dams_in$ICEHA[1] <- IHIn()
dams_out$ICEHA[1] <- IHOut()

dams_in$MCNAR[1] <- MCNIn()
dams_out$MCNAR[1] <- MCNOut()
if (track_curtailment == 1) {
	mainstem_curtailments$MCNAR[1] <- MCNCurtail()
	mainstem_shortfall$MCNAR[1] <- MCNInstreamShortfall()
}
BiOp$MCNAR[1] <- McNaryFlowTarget()

dams_in$JDAYY[1] <- JDIn()
dams_out$JDAYY[1] <- JDOut()
if (track_curtailment == 1) {
	mainstem_curtailments$JDAYY[1] <- JDCurtail()
	mainstem_shortfall$JDAYY[1] <- JDInstreamShortfall()
}

PELRelease_c <- PELRelease()
dams_in$PELTO[1] <- PELInflow()
dams_out$PELTO[1] <- PELOutflow()
flood_curve_df$PELTO[1] <- PELFloodCurve()

dams_in$DALLE[1] <- DAIn()
dams_out$DALLE[1] <- DAOut()
if (track_curtailment == 1) {
	mainstem_curtailments$DALLE[1] <- DACurtail()
	mainstem_shortfall$DALLE[1] <- DAInstreamShortfall()
}

dams_in$BONNE[1] <- BONIn()
dams_out$BONNE[1] <- BONOut()
BiOp$BONNE[1] <- BonnevilleFlowTarget()
water_df$TotalDamProtectExcess[1] <- TotalDamProtectExcess()

######### STORAGE FOR THE FIRST TIME STEP
for (res in names(reservoir_vol_df)) {
	reservoir_vol_df[1,res] <- reservoir_vol_df[1,res] + (dams_in[1,res] - dams_out[1,res])
}

###### MOPs Measures Of Performance
MOP_df$FirmEnergy[1] <- FirmEnergyMOP() # Firm energy shortfall
MOP_df$NonFirmEnergy[1] <- NonFirmEnergyMOP() # Non-Firm energy shortfall
MOP_df$ColFallsFlow[1] <- ColFallsFlowMOP() # Columbia Falls flow shortfall
MOP_df$LowerGraniteFlow[1] <- LowerGraniteFlowMOP() # Lower Granite flow shortfall 
MOP_df$VernitaBarFlow[1] <- VernitaFlowMOP() # Vernita Bar flow shortfall
MOP_df$McNaryFlow[1] <- McNaryFlowMOP() # McNary flow shortfall
MOP_df$GCRec[1] <- GCRecMOP() # Grand Coulee recreation metric
MOP_df$DallesFlood[1] <- DallesFloodMOP() # The Dalles flood protection metric
MOP_df$IHNav[1] <- IHNavMOP() # Ice Harbor navigation metric
MOP_df$BonnevilleFlow[1] <- BonnevilleFlowMOP() # Bonneville flow shortfall
MOP_df$BelowFCC[1] <- BelowFCC() # Excess flood storage space
MOP_df$FirmEnergySales[1] <- FirmEnergySales()
#MOP_df$NonFirmSpotSales[1] <- NonFirmSpotSales()
MOP_df$TotalSysEnergy[1] <- MaxSystemEnergy()

write.table(cbind(date_hist_sim[1,], dams_out[1,]), paste0(OutputFolder, "/dams_out.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], dams_in[1,]), paste0(OutputFolder, "/dams_in.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], reservoir_vol_df[1,]), paste0(OutputFolder, "/reservoir_volume.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], MOP_df[1,]), paste0(OutputFolder, "/MOP_df.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], water_df[1,]), paste0(OutputFolder, "/water.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], energy_df[1,]), paste0(OutputFolder, "/energy.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], BiOp[1,]), paste0(OutputFolder, "/BiOp_flow.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], flood_curve_df[1,]), paste0(OutputFolder, "/flood_curve.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], energy_curve_df[1,]), paste0(OutputFolder, "/energy_content_curve.txt"), row.names=F, col.names=T, append=F)
write.table(cbind(date_hist_sim[1,], GC_VDL_df[1,]), paste0(OutputFolder, "/GC_variable_draft.txt"), row.names=F, col.names=T, append=F)


if (track_curtailment == 1) {
	write.table(cbind(date_hist_sim[1,], mainstem_shortfall[1,]), paste0(OutputFolder, "/mainstem_shortfall.txt"), row.names=F, col.names=T, append=F)
	write.table(cbind(date_hist_sim[1,], mainstem_curtailments[1,]), paste0(OutputFolder, "/mainstem_curtailment.txt"), row.names=F, col.names=T, append=F)
}
