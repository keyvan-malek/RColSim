Read_Rule_Curves <- function() {
  ###### initial ##########
  HistStor <<- read.table("default_rule_curves/HistStor.txt", header=T, nrows=367)
  DUFlood_input <<- read.table("default_rule_curves/DUECCandFC/DUFlood.txt", header=T)
  DU_JAN_FAMAJ <<- read.table("default_rule_curves/DUECCandFC/DU-JAN-FAMAJ.txt", header=T)
  ARFlood <<- read.table("default_rule_curves/ARECCandFC/ARFlood.txt", header=T)
  BRCurFC_Cont <<- read.table("default_rule_curves/BRL/BRCurFC_Cont.txt", header=T)
  BRForecastFloodStorage <<- read.table("default_rule_curves/BRL/BRForecastFloodStorage.txt", header=T)
  
  ###### Dworshak Dam ######
  DWHistStor_input <<- read.table("default_rule_curves/DW/DWHistStor.txt", header=T)
  DWFlood_input <<- read.table("default_rule_curves/DW/DWFlood.txt", header=T)
  DW_FloodM <<-read.table("default_rule_curves/DW/DWFlood_Month.txt", header=T)
  DWRefillMin_input <<- read.table("default_rule_curves/DW/DWRefillMin.txt", header=T)
  DWAvgMin_input <<- read.table("default_rule_curves/DW/DWAvgMin.txt", header=T)
  DWCriticalCurve_input <<- read.table("default_rule_curves/DW/DWCriticalCurve.txt", header=T)
  DW1931Refill_input <<- read.table("default_rule_curves/DW/DW1931Refill.txt", header=T)
  
  ##### Low Granite Dam ######
  LowerGraniteTarget_input <<- read.table("default_rule_curves/LG/LowerGraniteTarget.txt", header=T)
  
  ###### US-CR ############
  USFloodRuleCurve_input <<- read.table("default_rule_curves/USCR/USFloodRuleCurve.txt", header=T)
  
  #### MS-CR ###########
  MSFloodRuleCurve_input <<- read.table("default_rule_curves/MSCR/MSFloodRuleCurve.txt", header=T)

  #### MSAg #############3
  B064X_input <<- read.table("default_rule_curves/MSAg/B065-4567.txt", header=T)
  FCIrQSnke_input <<- read.table("default_rule_curves/MSAg/FCIrQSnke.txt", header=T)
  QSnke_input <<- read.table("default_rule_curves/MSAg/QSnke.txt", header=T)
  BaseMSAgReturn_input <<- read.table("default_rule_curves/BRL/BaseMSAgReturn.txt", header=T)
  BaseMSAgWith_input <<- read.table("default_rule_curves/MSAg/MSNetAgRight.txt", header=T)
  
  #### McNary Dam #############
  MNB_input <<- read.table("default_rule_curves/McNary/MNBaseTarget.txt", header=T)
  ModMcNaryFlowData_input <<- read.table("default_rule_curves/McNary/ModMcNaryFlowData.txt", header=T)
  
  #### Brownlee #######
  BRHistStor_input <<- read.table("default_rule_curves/BRL/BRHistStor.txt", header=T)
  BRFlood <<- read.table("default_rule_curves/BRL/BRFlood.txt", header=T)
  AddSp_input <<- read.table("default_rule_curves/BRL/Add_Space.txt", header=T)

  ####### Libby ########
  LBHistStor_input <<- read.table("default_rule_curves/Libby/LBHistStor.txt", header=T)
  LBFlood_input <<- read.table("default_rule_curves/Libby/LBFlood.txt", header=T)
  MFlood_input <<- read.table("default_rule_curves/Libby/MFlood.txt", header=T)
  LBF_input <<- read.table("default_rule_curves/Libby/LBFlood_2.txt", header=T)
  APR_LB_input <<- read.table("default_rule_curves/Libby/APRLB.txt", header=T)
  LBCriticalCurve_input <<- read.table("default_rule_curves/Libby/LBCriticalCurve.txt", header=T)
  LB1931Refill_input <<- read.table("default_rule_curves/Libby/LB1931Refill.txt", header=T)
  LBMaxFCRel_input <<- read.table("default_rule_curves/Libby/LBMaxFCRel.txt", header=T)

  ##### Corra Linn ###########
  CLHistStor_input <<- read.table("default_rule_curves/CL/CLHistStor.txt", header=T)
  CLIJCRuleCurve_input <<- read.table("default_rule_curves/CL/CLIJCRuleCurve.txt", header=T)
  
  ##### Columbia Falls ######
  ColFall_Target <<- read.table("default_rule_curves/HH/ColFalls_Target.txt",header=T) # check ,
  
  ##### Hungary Horse ########
  HHCriticalCurve_input <<- read.table("default_rule_curves/HH/HHCriticalCurve.txt", header=T)
  HHBaseDraftLimit_input <<- read.table("default_rule_curves/HH/HHBaseDraftLimit.txt", header=T)
  HHAssuredRefill_input <<- read.table("default_rule_curves/HH/HHAssuredRefill.txt", header=T)
  HHStor <<- read.table("default_rule_curves/HH/HHHistStor.txt", header=T)
  HHFlood <<- read.table("default_rule_curves/HH/HHFlood.txt", header=T)
  JanToAprFlood <<- read.table("default_rule_curves/HH/HHFlood_JanApr.txt", header=T)
  HHFlood_2_input <<- read.table("default_rule_curves/HH/HHFlood_2.txt", header=T)
  APR_HH_input <<- read.table("default_rule_curves/HH/AprHH.txt", header=T)
  
  ### Kerr #####
  KerrFloodC <<- read.table("default_rule_curves/Kerr/FloodCurve.txt", header=T)
  Article_56_input <<- read.table("default_rule_curves/Kerr/article_56.txt", header=T)
  
  #### Bonneville ######
  Chum_variable_2_input <<- read.table("default_rule_curves/BONN/Chum_variable_2.txt", header=T)

  ##### PreEnergy ########3
  VernitaBarFlowTarget_input <<- read.table("default_rule_curves/GC/VernitaBarFlowTarget.txt", header=T)
  
  ### AlbeniFalls ######
  AFAvgMin_input <<- read.table("default_rule_curves/AFECCandFC/AFAvgMin.txt", header=T)
  AFFlood <<- read.table("default_rule_curves/AFECCandFC/AFFC.txt", header=T)
  
  ### Grand Coulee #######
  GCFlood1_input <<- read.table("default_rule_curves/GC/GCFlood1.txt", header=T)
  GCF_Month_input <<- read.table("default_rule_curves/GC/GCFlood_Month.txt", header=T)
  GCRefillMin_input <<- read.table("default_rule_curves/GC/GCRefillMin.txt", header=T)
  GCAvgMin_input <<- read.table("default_rule_curves/GC/GCAvgMin.txt", header=T)
  GCCriticalCurve_input <<- read.table("default_rule_curves/GC/GCCriticalCurve.txt", header=T)
  GCRecLimit_input <<- read.table("default_rule_curves/GC/GCRecLimit.txt", header=T)
  PreGCDraftLimit_input <<- read.table("default_rule_curves/GC/PreGCDraftLimit.txt", header=T)
  GCFlood2 <<- read.table("default_rule_curves/GC/GCFlood2.txt", header=T)
  GCF_month <<- read.table("default_rule_curves/GC/GCFlood_Month.txt", header=T)
  GCAbsMinQ_input <<- read.table("default_rule_curves/GC/GCAbsMinQ.txt", header=T)
  GCBdgtForVB_input <<- read.table("default_rule_curves/GC/GCBdgtForVB.txt", header=T)
  
  ##### Duncan ##########
  DUCriticalCurve_input <<- read.table("default_rule_curves/DUECCandFC/DUCriticalCurve.txt", header=T)
  DU1931Refill_input <<- read.table("default_rule_curves/DUECCandFC/DU1931Refill.txt", header=T)
  DURefillMin_input <<- read.table("default_rule_curves/DUECCandFC/DURefillMin.txt", header=T)
  DUF_2_input <<- read.table("default_rule_curves/DUECCandFC/DUFlood_2.txt", header=T)
  FMAMJ_DU_input <<- read.table("default_rule_curves/DUECCandFC/FMAMJ_DU.txt", header=T)

  ###### Arrow ##########
  ARFloodMonth <<- read.table("default_rule_curves/ARECCandFC/ARFloodMonth.txt", header=T)
  ARFlood_May5_i <<- read.table("default_rule_curves/ARECCandFC/ARFlood_May5.txt", header=T)
  ARFlood_1May_input <<- read.table("default_rule_curves/ARECCandFC/ARFlood_1May.txt", header=T)
  ARFlood_2_input <<- read.table("default_rule_curves/ARECCandFC/ARFlood_2.txt", header=T)
  ARCriticalCurve_input <<- read.table("default_rule_curves/ARECCandFC/ARCriticalCurve.txt", header=T)
  AR1931Refill_input <<- read.table("default_rule_curves/ARECCandFC/AR1931Refill.txt", header=T)
  ARHistStor_i <<- read.table("default_rule_curves/ARECCandFC/ARHistStor.txt", header=T)
  ARAssuredRelease_input <<- read.table("default_rule_curves/ARECCandFC/AssuredRelease.txt", header=T)
  
  #### Mica ###########
  MI1931Refill_input <<- read.table("default_rule_curves/MicaECCandFC/MI1931Refill.txt", header=T)
  GlobalFloodEvacMult_i <<- read.table("default_rule_curves/GlobalFloodEvacMult.txt", header=T)
  MIFlood_712 <<- read.table("default_rule_curves/MicaECCandFC/MIFlood7-12.txt",header=T)
  MIFlood_DecToApr <<- read.table("default_rule_curves/MicaECCandFC/MIFlood-DecToApr.txt", header=T)
  APR_MI_input <<- read.table("default_rule_curves/MicaECCandFC/APR_MI.txt", header=T)
  MICriticalCurve_input <<- read.table("default_rule_curves/MicaECCandFC/MICriticalCurve.txt", header=T)
  MIAssuredReleasedMin_i <<- read.table("default_rule_curves/MIAssuredReleasedMin.txt", header=T)
  MIMaxFloodOutflow_input <<- read.table("default_rule_curves/MicaECCandFC/MIMaxFloodOutflow.txt", header=T)
  
  ##### Energy #######
  NonFirmFraction_input <<- read.table("default_rule_curves/Energy/NonFirmFraction.txt", header=T)
  AltNonFirmLoad_input <<- read.table("default_rule_curves/Energy/AltNonFirmLoad.txt", header=T)
  ColFallsMaxFlow_input <<- read.table("default_rule_curves/Energy/ColFallsMaxFlow.txt", header=T)
  HH_USBRmax_input <<- read.table("default_rule_curves/Energy/HH_USBRmax.txt", header=T)
  FirmFraction_input <<- read.table("default_rule_curves/FirmFraction.txt", header=T)
  
  ######### Dalles #####
  DALowFloodTarget_input <<- read.table("default_rule_curves/Dall/DALowFloodTarget.txt", header=T)
  DAHighFloodTarget_input <<- read.table("default_rule_curves/Dall/DAHighFloodTarget.txt", header=T)

  ######### Bonneville ########
  BONNetHead_input <<- read.table("default_rule_curves/BONN/BONNetHead.txt", header=T)
}
