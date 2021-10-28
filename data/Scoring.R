##########################################################################################################################
#### PROMIS Scoring ######################################################################################################
##########################################################################################################################
PROMIS_Variables = list(
    "Depression" = grep("^EDDEP", names(Codebook),value=TRUE),
    "Anxiety" = grep("^EDANX", names(Codebook), value=TRUE),
    "Sleep" = grep("^Sleep", names(Codebook), value=TRUE),
    "Fatigue" = c(grep("^FAT", names(Codebook), value=TRUE), "HI7_PROMIS", "AN3"),
    "PhysicalFunction" = grep("^PFA", names(Codebook), value=TRUE),
    "Pain" = c(grep("^PAININ", names(Codebook), value=TRUE),"Global07"),
    "Social" = grep("^SRP", names(Codebook), value=TRUE))

Item_Parameters=readRDS("Item_Parameters.RDS")
source("ThetaSEeap.R")

#Reverse & recode Global07 0-10 to 1-5

PROMIS_Recode=data.frame(matrix(NA,nrow=nrow(Data),ncol=0))
PROMIS_Recode$Global07=Data$Global07
PROMIS_Recode$Global07[which(Data$Global07==0)]=5
PROMIS_Recode$Global07[which(Data$Global07==1)]=4
PROMIS_Recode$Global07[which(Data$Global07==2)]=4
PROMIS_Recode$Global07[which(Data$Global07==3)]=4
PROMIS_Recode$Global07[which(Data$Global07==4)]=3
PROMIS_Recode$Global07[which(Data$Global07==5)]=3
PROMIS_Recode$Global07[which(Data$Global07==6)]=3
PROMIS_Recode$Global07[which(Data$Global07==7)]=2
PROMIS_Recode$Global07[which(Data$Global07==8)]=2
PROMIS_Recode$Global07[which(Data$Global07==9)]=2
PROMIS_Recode$Global07[which(Data$Global07==10)]=1

PROMIS_Recode_Scored=cbind(PROMIS_Recode,
###Social is scored with v1 Satisfaction (p29 v2 uses Ability) params
"PROMIS_SOC"=thetaSE.eap(Item_Parameters$Satifaction[PROMIS_Variables$Social,c("a",paste0("cb",1:4),"NCAT")], Data[,PROMIS_Variables$Social])$theta*10+50,

#remove the Global07 from list of Pain variables with setdiff for scoring (not IRT scored)
"PROMIS_PN"=thetaSE.eap(Item_Parameters$Pain_Interference[setdiff(PROMIS_Variables$Pain, "Global07"),c("a",paste0("cb",1:4),"NCAT")], Data[,setdiff(PROMIS_Variables$Pain, "Global07")])$theta*10+50,

"PROMIS_DEP"=thetaSE.eap(Item_Parameters$Depression[PROMIS_Variables$Depression,c("a",paste0("cb",1:4),"NCAT")], Data[,PROMIS_Variables$Depression])$theta*10+50,

"PROMIS_ANX"=thetaSE.eap(Item_Parameters$Anxiety[PROMIS_Variables$Anxiety,c("a",paste0("cb",1:4),"NCAT")], Data[,PROMIS_Variables$Anxiety])$theta*10+50,

"PROMIS_SLP"=thetaSE.eap(Item_Parameters$Sleep_Disturbance[PROMIS_Variables$Sleep,c("a",paste0("cb",1:4),"NCAT")], Data[,PROMIS_Variables$Sleep])$theta*10+50,

#use gsub to remove the "_PROMIS" from the HI7 item (it's in both P29 & FKSI)
"PROMIS_FAT"=thetaSE.eap(Item_Parameters$Fatigue[gsub("_PROMIS","",PROMIS_Variables$Fatigue),c("a",paste0("cb",1:4),"NCAT")], Data[,PROMIS_Variables$Fatigue])$theta*10+50,

"PROMIS_PF"=thetaSE.eap(Item_Parameters$Physical_Function[PROMIS_Variables$PhysicalFunction,c("a",paste0("cb",1:4),"NCAT")], Data[,PROMIS_Variables$PhysicalFunction])$theta*10+50)

##########################################################################################################################
#### FACT Scoring ########################################################################################################
##########################################################################################################################
FACT_Variables = list( 
    "NFKSI_19_Variables" = c("gp1","gp4","c2","hi7","b1","brm3","bp1","l2","hi12","rcc2","c6","gf5","ge6","gp2","c5","gp5","gf1","gf3","gf7"),
    "FKSI_15_Variables"=c("gp1","gp4","gp5","c2","bp1","hi7","gf3","b1","ge6","l2","c6", "brm3","gf1","rcc2","gf5"),
    "FKSI_DRS_13_Variables"=c("gp1","gp4","c2","hi7","b1","brm3","bp1","l2","hi12","rcc2","c6","gf5","ge6"),
    "FKSI_DRS_9_Variables"=c("gp1","gp4","c2","hi7","b1","brm3","bp1","l2","rcc2"),
    "DRS_P"=c("gp1","gp4","c2","hi7","b1","brm3","bp1","l2", "hi12","rcc2","c6","gf5"),
    "DRS_E"=c("ge6"),
    "TSE"=c("gp2","c5","gp5"),
    "FWB"=c("gf1","gf3","gf7"))

###unneeded, already reversed in dataset
# FACT_Recode=cbind(  
# "gp1" = c(4 - Data$gp1), 
# "gp4" = c(4 - Data$gp4),
# "c2" = c(4 - Data$c2),
# "hi7" = c(4 - Data$hi7),
# "b1" = c(4 - Data$b1),
# "brm3" = c(4 - Data$brm3),
# "bp1" = c(4 - Data$bp1),
# "l2" = c(4 - Data$l2),
# "hi12" = c(4 - Data$hi12),
# "rcc2" = c(4 - Data$rcc2),
# "c6" = c(0 + Data$c6),
# "gf5" = c(0 + Data$gf5),
# "ge6" = c(4 - Data$ge6),
# "gp2" = c(4 - Data$gp2),
# "c5" = c(4 - Data$c5),
# "gp5" = c(4 - Data$gp5),
# "gf1" = c(0 + Data$gf1),
# "gf3" = c(0 + Data$gf3),
# "gf7" = c(0 + Data$gf7))

#not actually recoded (see above initial recoding, but preserved for script purposes)
FACT_Recode=data.frame(
"gp1"=Data$gp1,
"gp4"=Data$gp4,
"c2"=Data$c2,
"hi7"=Data$hi7,
"b1"=Data$b1,
"brm3"=Data$brm3,
"bp1"=Data$bp1,
"l2"=Data$l2,
"hi12"=Data$hi12,
"rcc2"=Data$rcc2,
"c6"=Data$c6,
"gf5"=Data$gf5,
"ge6"=Data$ge6,
"gp2"=Data$gp2,
"c5"=Data$c5,
"gp5"=Data$gp5,
"gf1"=Data$gf1,
"gf3"=Data$gf3,
"gf7"=Data$gf7)


FACT_Scoring = function(x){ni=length(x); nobs=sum(!is.na(x)); if(nobs/ni >0.5){return(sum(x,na.rm=TRUE)*ni/nobs)} else{return(na)}}

FACT_Recode_Scored=as.data.frame(cbind(FACT_Recode,
    "NFKSI_19_Total"=apply(FACT_Recode[,FACT_Variables$NFKSI_19_Variables],1, FACT_Scoring),
    "FKSI_15_Total"=apply(FACT_Recode[,FACT_Variables$FKSI_15_Variables],1, FACT_Scoring),
    "FKSI_DRS_13_Total"=apply(FACT_Recode[,FACT_Variables$FKSI_DRS_13_Variables],1, FACT_Scoring),
    "FKSI_DRS_9_Total"=apply(FACT_Recode[,FACT_Variables$FKSI_DRS_9_Variables],1, FACT_Scoring),
    "FKSI_DRS_P_Total"=apply(FACT_Recode[,FACT_Variables$DRS_P],1, FACT_Scoring),
    "FKSI_DRS_E_Total"=FACT_Recode[,FACT_Variables$DRS_E],
    "FKSI_TSE_Total"=apply(FACT_Recode[,FACT_Variables$TSE],1, FACT_Scoring),
    "FKSI_FWB_Total"=apply(FACT_Recode[,FACT_Variables$FWB],1, FACT_Scoring)))

##########################################################################################################################
#### SF36 Scoring ########################################################################################################
##########################################################################################################################
SF36_Variables=list(
  "PhysicalFunctioning"=paste0("PF",formatC(1:10, width = 2, format = "d", flag = "0")),
  "RoleLimitations_Phys"=paste0("RP",formatC(1:4, width = 2, format = "d", flag = "0")),
  "RoleLimitations_Emot"=paste0("RE",formatC(1:3, width = 2, format = "d", flag = "0")),
  "EnergyFatigue"=paste0("VT",formatC(1:4, width = 2, format = "d", flag = "0")),
  "EmotWellBeing"=paste0("MH",formatC(1:5, width = 2, format = "d", flag = "0")),
  "SocialFunctioning"=paste0("SF",formatC(1:2, width = 2, format = "d", flag = "0")),
  "Pain"= paste0("BP",formatC(1:2, width = 2, format = "d", flag = "0")),
  "GeneralHealth"=paste0("GH",formatC(1:5, width = 2, format = "d", flag = "0")),
  "HealthChange"="HT")

SF36_Recode=cbind(
    sapply(c("GH01", "HT", "SF01", "BP02", "GH03", "GH05"), function(x){
      recode=Data[,x]
      recode[recode==1]=100
      recode[recode==2]=75
      recode[recode==3]=50
      recode[recode==4]=25
      recode[recode==5]=0
      recode
    }),
    
    sapply(SF36_Variables$PhysicalFunctioning , function(x){
      recode=Data[,x]
      recode[recode==1]=0
      recode[recode==2]=50
      recode[recode==3]=100
      recode
    }),
    
    sapply(c(SF36_Variables$RoleLimitations_Emot,SF36_Variables$RoleLimitations_Phys), function(x){
      recode=Data[,x] #these response options don't match SF36 v2 ( only 1 & 2, 0&100)
      recode[recode==1]=0
      recode[recode==2]=25
      recode[recode==3]=50
      recode[recode==4]=75
      recode[recode==5]=100
      recode
    }),
    
    sapply(c("BP01", "VT01", "MH03", "VT02", "MH05"), function(x){
      recode=Data[,x]
      recode[recode==1]=100
      recode[recode==2]=80
      recode[recode==3]=60
      recode[recode==4]=40
      recode[recode==5]=20
      recode[recode==6]=0
      recode
    }),

    sapply(c("MH01", "MH02", "MH04", "VT03", "VT04"), function(x){
      recode=Data[,x]
      recode[recode==1]=0
      recode[recode==2]=20
      recode[recode==3]=40
      recode[recode==4]=60
      recode[recode==5]=80
      recode[recode==6]=100
      recode
    }),
    
    sapply(c("SF02", "GH02", "GH04"), function(x){
      recode=Data[,x]
      recode[recode==1]=0
      recode[recode==2]=25
      recode[recode==3]=50
      recode[recode==4]=75
      recode[recode==5]=100
      recode
    }))

SF36_Recode_Scored=as.data.frame(cbind(SF36_Recode,
"SF36_PF"=apply(SF36_Recode[,SF36_Variables$PhysicalFunctioning], 1, mean, na.rm=TRUE),
"SF36_RP"=apply(SF36_Recode[,SF36_Variables$RoleLimitations_Phys], 1, mean, na.rm=TRUE),
"SF36_RE"=apply(SF36_Recode[,SF36_Variables$RoleLimitations_Emot], 1, mean, na.rm=TRUE),
"SF36_VT"=apply(SF36_Recode[,SF36_Variables$EnergyFatigue], 1, mean, na.rm=TRUE),
"SF36_EWB"=apply(SF36_Recode[,SF36_Variables$EmotWellBeing], 1, mean, na.rm=TRUE),
"SF36_SF"=apply(SF36_Recode[,SF36_Variables$SocialFunctioning], 1, mean, na.rm=TRUE),
"SF36_PN"=apply(SF36_Recode[,SF36_Variables$Pain], 1, mean, na.rm=TRUE),
"SF36_GH"=apply(SF36_Recode[,SF36_Variables$GeneralHealth], 1, mean, na.rm=TRUE)))

#<!-- summary scores... they look complicated to calculate? and check for reverse scores w/corr-->
