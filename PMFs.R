
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

BonnevilleFlowMOP <- function() { # Bonneville flow deficit
  if ((BonnevilleFlowTarget() - BONOut()) / BonnevilleFlowTarget() > 0.05) {
    BonnevilleFlowMOP_o <- max(BonnevilleFlowTarget() - BONOut(), 0)
  } else {
    BonnevilleFlowMOP_o=0
  }
  return(BonnevilleFlowMOP_o)
}

############################# Columbia Falls

ColFallsFlow_cfs <- function() {
  ColFallsFlow_cfs_o <- COLOut() / cfsTOafw
  return(ColFallsFlow_cfs_o)
}
ColFallsFlowMOP <- function() {
	if(MOPControl==1){
		ColFallsFlowMOP_o <- max(0, ColFallsTarget() * (1 - SensitivityFraction) - ColFallsFlow_cfs())
	} else { 
		ColFallsFlowMOP_o <- 0
	}
	return(ColFallsFlowMOP_o)
}

#### Lower Granite 

LowerGraniteFlow_cfs <- function() {
	LowerGraniteFlow_cfs_o <- LGOut() / cfsTOafw
	return(LowerGraniteFlow_cfs_o)
}
LowerGraniteFlowMOP <- function() {
	if (MOPControl==1) {
		LowerGraniteFlowMOP_o <- max(0, LowerGraniteFlowTarget() * (1 - SensitivityFraction) - LowerGraniteFlow_cfs())
	} else { 
		LowerGraniteFlowMOP_o <- 0
	}
	return(LowerGraniteFlowMOP_o)
}


################# Ice Harbor 

IHFlowcfs <- function() {
  IHFlowcfs_o <- IHOut() / cfsTOafw
  return(IHFlowcfs_o)
}
IHNavMOP <- function() {
	IHNavMaxTarget <- 100000
	if (MOPControl==1) {
		IHNavMOP_o <- max(0, IHNavMaxTarget * (1 - SensitivityFraction) - IHFlowcfs())
	} else { 
		IHNavMOP_o <- 0
	}
	return(IHNavMOP_o)
}

######## Vernita Bar

VernitaBarFlow_cfs <- function() {
	VernitaBarFlow_cfs_o <- PROut() / cfsTOafw
	return(VernitaBarFlow_cfs_o)
}
VernitaFlowMOP <- function() {
	if(MOPControl==1) {
		VernitaFlowMOP_o <- max(0, VernitaBarFlowTarget() * (1 - SensitivityFraction) - VernitaBarFlow_cfs())
	} else { 
		VernitaFlowMOP_o <- 0
	}
	return(VernitaFlowMOP_o)
}

####### McNary

McNaryFlow_cfs <- function() {
	McNaryFlow_cfs_o <- MCNOut() / cfsTOafw
	return(McNaryFlow_cfs_o)
}
McNaryFlowMOP<-function(){
	if (MOPControl==1) {
		McNaryFlowMOP_o=max(0, McNaryFlowTarget() * (1 - SensitivityFraction) - McNaryFlow_cfs())
	} else { 
		McNaryFlowMOP_o <- 0
	}
	return(McNaryFlowMOP_o)
}


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
############                          Hydropower Measures of Performance
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

FirmEnergyMOP <- function() { # Shortfall in firm energy production
	if (MOPControl==1) {
		FirmEnergyMOP_o <- max(0, FirmEnergyTarget() * (1 - SensitivityFraction) - MaxSystemEnergy())
	} else {
		FirmEnergyMOP_o <- 0
	}
	return(FirmEnergyMOP_o)
}
NonFirmEnergyMOP <- function() { # Shortfall in non-firm energy production
	if (MOPControl==1) {
		NonFirmEnergyMOP_o <- max(0, NonFirmEnergyTarget() * (1 - SensitivityFraction) - NonFirmEnergyMarketed())
	} else {
		NonFirmEnergyMOP_o <- 0
	}
	return(NonFirmEnergyMOP_o)
}
FirmEnergyMarketed <- function() {
  FirmEnergyMarketed_o <- min(FirmEnergyTarget(), MaxSystemEnergy())
  return(FirmEnergyMarketed_o)
}
NonFirmEnergyMarketed <- function() {
	NonFirmEnergyMarketed_o <- min(NonFirmEnergyTarget(), (MaxSystemEnergy() - FirmEnergyMarketed()))
	return(NonFirmEnergyMarketed_o)
}
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
DallesFloodMOP <- function() {
	if(MOPControl==1)  {
		DallesFloodMOP_o <- max(0, DallesFlow_cfs() - ControlledFlow() * (1 + SensitivityFraction)) 
	} else {
		DallesFloodMOP_o <- 0
	}
	return(DallesFloodMOP_o)
}

########## Grand Coulee Recreation Metric

GCRecMOP <- function() {
	GCRecStdElev <- 1280 #ft
	if(MOPControl==1) {
		if (week_in_year %in% c(49:52, 1:6)) {
			GCRecMOP_o <- 0
		} else {
			GCRecMOP_o <- max(0, GCRecStdElev - GCElev_ft()) 
		}
    } else {
		GCRecMOP_o <- 0
	}
	return(GCRecMOP_o)
}
TotalSysStorage <- function() {
	TotalSysStorage_o <- Arrow() + Brownlee() + CorraLinn() + Duncan() + Dworshak() + GrandCoulee() + HungryHorse() + Libby() + Mica() + Kerr() + AlbeniFalls()
	return(TotalSysStorage_o)
}
SumFloodTarget <- function() { # Target storage for dams to meet flood protection objective
	SumFloodTarget_o <- ARFloodCurve() + DUFloodCurve() + DWFloodCurve() + GCFloodCurve() + HHFloodCurve() + LBFloodCurve() + MIFloodCurve() + BRFloodCurve() + CLFloodCurve() + KEFloodCurve() + AFFloodCurve()
	return(SumFloodTarget_o)
}
BelowFCC <- function() { # Excess storage space
	if (TotalSysStorage() < SumFloodTarget() * (1 - SensitivityFraction)) {
		BelowFCC_o <- SumFloodTarget() - TotalSysStorage()
	} else {
		BelowFCC_o <- 0
	}
	return(BelowFCC_o)
}