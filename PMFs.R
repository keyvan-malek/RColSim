
##############################################################################################################################################################
##############################################################################################################################################################
##############################################################################################################################################################

                                                              #######################################
                                                              #                                     #
                                                              #                                     #
                                                              #        Performance Measures         #
                                                              #                                     #
                                                              #                                     #
                                                              #######################################
                                                          
############################ Bonneville

ChumSF<-function() { # Bonneville flow deficit
  if ((BONTarget_AcFt() - BONOut()) / BONTarget_AcFt() > 0.05) {
    ChumSF_o <- max(BONTarget_AcFt() - BONOut(), 0)
  } else {
    ChumSF_o=0
  }
  return(ChumSF_o)
}

############################# Columbia Falls

shortfall_2<-function() { # Shortfall in non-firm generation
	if (MOPControl==1) {
		Shortfall_2_o <- max(0, NonFirmEnergyTarget() * (1 - SensitivityFraction) - NonFirmEnergyMarketed())
	} else {
		Shortfall_2_o <- 0
	}
	return(Shortfall_2_o)
}
ColFallsFlow_cfs <- function() {
  ColFallsFlow_cfs_o <- ColumbiaFalls() / cfsTOafw
  return(ColFallsFlow_cfs_o)
}
shortfall_5 <- function() { # Shortfall in flow at Columbia Falls for fish
	if(MOPControl==1){
		Shortfall_5_o <- max(0, ColFallsTarget() * (1 - SensitivityFraction) - ColFallsFlow_cfs())
	} else { 
		Shortfall_5_o <- 0
	}
	return(Shortfall_5_o)
}

#### Lower Granite 

LowerGraniteFlow_cfs <- function() {
	LowerGraniteFlow_cfs_o <- LGOut() / cfsTOafw
	return(LowerGraniteFlow_cfs_o)
}
shortfall_6 <- function() {
	if (MOPControl==1) {
		Shortfall_6_o <- max(0, LowerGraniteTarget() * (1 - SensitivityFraction) - LowerGraniteFlow_cfs())
	} else { 
		Shortfall_6_o <- 0
	}
	return(Shortfall_6_o)
}


################# Ice Harbor 

IHFlowcfs <- function() {
  IHFlowcfs_o <- IHOut() / cfsTOafw
  return(IHFlowcfs_o)
}
shortfall_11 <- function() {
	IHNavMaxTarget <- 100000
	if (MOPControl==1) {
		Shortfall_11_o <- max(0, IHNavMaxTarget * (1 - SensitivityFraction) - IHFlowcfs())
	} else { 
		Shortfall_11_o <- 0
	}
	return(Shortfall_11_o)
}

######## Vernita Bar

VernitaBarFlow_cfs <- function() {
	VernitaBarFlow_cfs_o <- PROut() / cfsTOafw
	return(VernitaBarFlow_cfs_o)
}
shortfall_7 <- function() {
	if(MOPControl==1) {
		Shortfall_7_o <- max(0, VernitaBarFlowTarget() * (1 - SensitivityFraction) - VernitaBarFlow_cfs())
	} else { 
		Shortfall_7_o <- 0
	}
	return(Shortfall_7_o)
}

####### McNary

McNaryFlow_cfs <- function() {
	McNaryFlow_cfs_o <- MCNOut() / cfsTOafw
	return(McNaryFlow_cfs_o)
}
shortfall_8<-function(){
	if (MOPControl==1) {
		Shortfall_8_o=max(0, McNaryBaseTarget() * (1 - SensitivityFraction) - McNaryFlow_cfs())
	} else { 
		Shortfall_8_o <- 0
	}
	return(Shortfall_8_o)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############                          Hydropower Measures of Performance
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

########### Firm energy shortfalls

shortfall <- function() {
	if (MOPControl==1) {
		shortfall_o <- max(0, FirmEnergyTarget() * (1 - SensitivityFraction) - MaxSystemEnergy())
	} else {
		shortfall_o <- 0
	}
	return(shortfall_o)
}
FirmEnergyMarketed <- function() {
  FirmEnergyMarketed_o <- min(FirmEnergyTarget(), MaxSystemEnergy())
  return(FirmEnergyMarketed_o)
}
NonFirmEnergyMarketed <- function() {
	NonFirmEnergyMarketed_o <- min(NonFirmEnergyTarget(), (MaxSystemEnergy() - FirmEnergyMarketed()))
	return(NonFirmEnergyMarketed_o)
}

########### Energy Production Metrics #########################

FirmEnergySales <- function() {
	FirmEnergyPrice <- 35
	FirmEnergySales_o <- FirmEnergyMarketed() * FirmEnergyPrice
	return(FirmEnergySales_o)
}
NonFirmSpotSales <- function() {
	SpotNonFirmPrice <- c(39.7,32.5 ,26.2, 32.6, 33.2, 28.3, 27.2, 25.8, 18.3, 16.8, 21.0, 29.0)
    NonFirmSpotSales_o <- NonFirmEnergyMarketed() * SpotNonFirmPrice[month_in_year]
	return(NonFirmSpotSales_o)
}

###############################################################################################
#######################################################################################################################
################################### flood and storage metrics

####### Dalles Flood Protection Metric

DallesFlow_cfs <- function() {
	DallesFlow_cfs_o <- DAOut() / cfsTOafw
	return(DallesFlow_cfs_o)
}
shortfall_10 <- function() {
	if(MOPControl==1)  {
		Shortfall_10_o <- max(0, DallesFlow_cfs() - DAFloodTarget() * (1 + SensitivityFraction)) 
	} else {
		Shortfall_10_o <- 0
	}
	return(Shortfall_10_o)
}

########## Grand Coulee Recreation Metric

shortfall_9 <- function() {
	GCRecStdElev <- 1280 #ft
	if(MOPControl==1) {
		if (month_in_year<12 && month_in_year>3) {
			Shortfall_9_o <- 0
		} else {
			Shortfall_9_o <- max(0, GCRecStdElev - GCElev_ft()) 
		}
    } else {
		Shortfall_9_o <- 0
	}
	return(Shortfall_9_o)
}
TotalSysStorage <- function() {
	TotalSysStorage_o <- ArrowReservoir() + Brownlee() + CorraLinnReservoir() + Duncan() + Dworshak() + GrandCoulee() + HungryHorse() + Libby() + MicaReservoir()
	return(TotalSysStorage_o)
}
SumFloodTarget <- function() { # Target storage for dams to meet flood protection objective
	SumFloodTarget_o <- ARFloodCurve() + DUFloodCurve() + DWFloodCurve() + GCFloodCurve() + HHFloodCurve()
	+ LibbyFloodCurve() + MIFloodCurve() + BRFloodVolume() + CLRuleVol()
	return(SumFloodTarget_o)
}
BelowFCC <- function() {
	if (TotalSysStorage() < SumFloodTarget() * (1 - SensitivityFraction)) {
		BelowFCC_o <- SumFloodTarget() - TotalSysStorage()
	} else {
		BelowFCC_o <- 0
	}
	return(BelowFCC_o)
}