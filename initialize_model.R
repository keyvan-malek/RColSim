
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

### Estimated combined efficiency of turbines

for (dam in efficiencies$ShortNM) {
	assign(paste0(dam, "CombEfficiency"), 0.8, pos=1)
}
Estimated_Efficiency <<- 0.8 # Estimated combined efficiency for all plants.  This efficiency is used for estimating energy content only.

MIRelease_c <- MIRelease()
dams_in$MICAA[1] <- MIInflow()
dams_out$MICAA[1] <- MIOutflow()

dams_in$REVEL[1] <- REVIn()
dams_out$REVEL[1] <- REVOut()

ARRelease_c <- ARRelease()
dams_in$ARROW[1] <- ARInflow()
dams_out$ARROW[1] <- AROutflow()

HHRelease_c <- HHRelease()
dams_in$FLASF[1] <- HHInflow()
dams_out$FLASF[1] <- HHOutflow()

dams_in$FLAPO[1] <- KEInflow()
dams_out$FLAPO[1] <- KERelease_c

dams_in$THOMF[1] <- TFIn()
dams_out$THOMF[1] <- TFOut()

dams_in$NOXON[1] <- NOXIn()
dams_out$NOXON[1] <- NOXOut()

dams_in$CABIN[1] <- CBIn()
dams_out$CABIN[1] <- CBOut()

dams_in$ALBEN[1] <- AFInflow()
dams_out$ALBEN[1] <- AFRelease_c

dams_in$BOXCA[1] <- BCIn()
dams_out$BOXCA[1] <- BCOut()

dams_in$BOUND[1] <- BDIn()
dams_out$BOUND[1] <- BDOut()

LBRelease_c <- LBRelease()
dams_in$LIBBY[1] <- LBInflow()
dams_out$LIBBY[1] <- LBOutflow()

dams_in$BONFE[1] <- BONFIn()
dams_out$BONFE[1] <- BONFOut()

dams_in$DUNCA[1] <- DUInflow()
dams_out$DUNCA[1] <- DURelease_c

dams_in$CORRA[1] <- CLInflow()
dams_out$CORRA[1] <- CLRelease_c

GCRelease_c <- GCRelease()
dams_in$GCOUL[1] <- GCInflow()
dams_out$GCOUL[1] <- GCOutflow()

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

PALRelease_c <- PALRelease()
dams_in$PALIS[1] <- PALInflow()
dams_out$PALIS[1] <- PALOutflow()

IPRelease_c <- IPRelease()
dams_in$IPARK[1] <- IPInflow()
dams_out$IPARK[1] <- IPOutflow()

RIRRelease_c <- RIRRelease()
dams_in$RIRDM[1] <- RIRInflow()
dams_out$RIRDM[1] <- RIROutflow()

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

PayetteRelease_c <- PayetteRelease()
dams_in$PAYHS[1] <- PayetteInflow()
dams_out$PAYHS[1] <- PayetteOutflow()

OWYRelease_c <- OWYRelease()
dams_in$OWYHE[1] <- OWYInflow()
dams_out$OWYHE[1] <- OWYOutflow()

BRRelease_c <- BRRelease()
dams_in$BROWN[1] <- BRInflow()
dams_out$BROWN[1] <- BROutflow()

dams_in$OXBOW[1] <- OXIn()
dams_out$OXBOW[1] <- OXOut()

dams_in$HCANY[1] <- HCIn()
dams_out$HCANY[1] <- HCOut()

DWRelease_c <- DWRelease()
dams_in$DWORS[1] <- DWInflow()
dams_out$DWORS[1] <- DWOutflow()

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

dams_in$JDAYY[1] <- JDIn()
dams_out$JDAYY[1] <- JDOut()
if (track_curtailment == 1) {
	mainstem_curtailments$JDAYY[1] <- JDCurtail()
	mainstem_shortfall$JDAYY[1] <- JDInstreamShortfall()
}

PELRelease_c <- PELRelease()
dams_in$PELTO[1] <- PELInflow()
dams_out$PELTO[1] <- PELOutflow()

dams_in$DALLE[1] <- DAIn()
dams_out$DALLE[1] <- DAOut()
if (track_curtailment == 1) {
	mainstem_curtailments$DALLE[1] <- DACurtail()
	mainstem_shortfall$DALLE[1] <- DAInstreamShortfall()
}

dams_in$BONNE[1] <- BONIn()
dams_out$BONNE[1] <- BONOut()

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
MOP_df$BonnevillFlow[1] <- BonnevilleFlowMOP() # Bonneville flow shortfall
MOP_df$ExtraSpace[1] <- ExtraSpace() # Excess flood storage space
MOP_df$TotalSysEnergy[1] <- MaxSystemEnergy_c

######### Write output for first timestep

for (var in out_vars) {
	write.table(cbind(date_hist_sim[1,], get(var)[1,]), paste0(OutputFolder, var, ".txt"), row.names=F, col.names=T, append=F)
}








