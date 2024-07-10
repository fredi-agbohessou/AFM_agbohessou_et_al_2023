# STEP (*.out) to (*.csv & *.xlsx)
# yelognisse agbohessou 
# 22/02/2021

# convert soil moisture from millimeter (mm) to vol (%) in each soil layer
# compute fsj, Etr, vcft and CO2Soil 
# convert output files of STEP model to a single readable excel or csv file


library(openxlsx)

Output = NULL
for (i in 12:22){
  df =  read.table(paste0("S01001",i,".out"), h=FALSE, dec=".")
  Output <- rbind(Output, df) 
}

#create a sequence of strings for the Titles
title0 <- c("Jour","Mois","Rain", "Etp","Rayglo","Shum_0","Shum_1","Shum_2","Shum_3","Etot","Trtot","PST","BMs","BMv","Bmlita",
            "Psife","Rstoe","REspCv","REspEv","Psi(1)","Psi(2)","Psi(3)","BMr","RespCr","RespEr","a","BMrs","Epsiloni","Photoresp",
            "Bmlits","Bmburn","Bmfec","Bming","Bmsts","Bmsss","BSOM","Temp","Vent","Vapeur","BurntF","chaTotJ","TqCs",
            "Cs(1)",	"Cs(2)",	"Cs(3)",	"Cs(4)",	"Cs(5)",	"Cs(6)",	"Total_N",	"Amonia",	"End_N_(Norga)",	"Sub_N",	"Bn",	"Co2S","Ih(1)",
            "Ih(2)",	"Ih(3)",	"HCr(1)",	"Hlim",	"HFle",	"HCr(2)",	"Hlim",	"HFle",	"HCr(3)",	"Hlim",	"HFle",	"EP",	"TrP",	"PSTP",	"REspCPv",
            "REspEPv",	"BMPv",	"BPMs",	"(BMPlita)",	"PsifP",	"RstoP",	"BMPr",	"RespCPr",	"RespEPr",	"BMPrs",	"EpsiloniP",	"PhotorespP",
            "(BPburn)", "(BPing)",	"BPsts",	"BPsss",	"BLmv",	"BFlita",	"LAILv",	"hca",	"hcp",	"LAIv",	"LAIPv","G","rss","Snvege","SnSolnu","Sn",
            "Ln",	"Rn",	"H",	"LAIs","LAIPs","LAIlita","vcfv","vcfs","vcfl","vcft","NOFLUX_(kgN/ha/an)","Bc","NOFlux_Process",
            "N2OFlux_Process","NO3","NO2","N2O","En20","wfps","En2o_NOE", "Runoff","Subrunoff")
#add the column titles to the new data frame
colnames(Output) <- title0

#create date column
Date <- seq.Date(as.Date("2012-01-01"), length.out = nrow(Output), by = "day", drop=FALSE)
df <- data.frame(Date)
#add date column to Output data frame
Output1 <- cbind(df,Output)
attach(Output1)

Output1$Shum_0_per <- try((100*Shum_0) / 20)   #Shum0: 20 mm depth
Output1$Shum_1_per <- try((100*Shum_1) / 280)   #Shum1: 280 mm depth
Output1$Shum_2_per <- try((100*Shum_2) / 700)   #Shum2: 700 mm depth
Output1$Shum_3_per <- try((100*Shum_3) / 2000)  #Shum3: 2000 mm depth
Output1$Etr <- try(Trtot + Etot)               #actual evapotranspiration (etr)
Output1$vcft <- try(vcfv+vcfs+vcfl)  # total ground cover 
#Output1$BMv_s <- try(BMs+BMv)  
Output1$BMv_t_ha <- try((BMv)*0.01)
#Output1$BMt <- try(BMs+BMv+Bmlita)  
#Output1$BMt_t_ha <- try((BMs+BMv+Bmlita)*0.01)
Output1$SoilResp <- try((Co2S*0.5)+ (RespCr*0.5) + (RespEr*0.5)+(RespCPr*0.5)+(RespEPr*0.5))
Output1$CO2Soil <- try(Co2S*0.5)
Output1$Reco <-  try((Co2S*0.5) + (RespCr*0.5) + (RespEr*0.5)+ (REspCv*0.5) + (REspEv*0.5)+(REspCPv*0.5)+(REspEPv*0.5)+(RespCPr*0.5)+(RespEPr*0.5))

Output1$Reco_t_ha  <- try(((Co2S*0.5) + (RespCr*0.5) + (RespEr*0.5)+ (REspCv*0.5) + (REspEv*0.5)+(REspCPv*0.5)+(REspEPv*0.5)+(RespCPr*0.5)+(RespEPr*0.5))*0.01)
Output1$N2Odenit_kg_ha  <- try((En2o_NOE/1160)-(N2OFlux_Process/365))
Output1$N2Onit_kg_ha  <- try(N2OFlux_Process/365)
Output1$N2O_total_kg_ha  <- try(En2o_NOE/1160)
Output1$N2O_total_t_ha  <- try((En2o_NOE/1160)*0.001)

# 1 kg N2O-N = (44/28)*1 kg N2O = 1.57 kg N2O and 1 kg N2O = 265 kg CO2 equivalents !(IPCC, 2014) 5th assessment
Output1$N2O_total_kg_CO2_equiv_ha  <- try((En2o_NOE/1160)*1.57*265)

# 1kg/ha = 0.001 t/ha
Output1$N2O_total_t_CO2_equiv_ha  <- try((En2o_NOE/1160)*1.57*265*0.001)

#1 kg CO2-C = (44/12)*1 kg CO2 =3.67 kg CO2 ! (IPCC, 2014)
Output1$Reco_t_CO2_ha  <- try(((Co2S*0.5) + (RespCr*0.5) + (RespEr*0.5)+ (REspCv*0.5) + (REspEv*0.5)+(REspCPv*0.5)+(REspEPv*0.5)+(RespCPr*0.5)+(RespEPr*0.5))*0.01*3.67)

# Total budget
Output1$GHG_t_CO2_equiv_ha <- try(((En2o_NOE/1160)*1.57*265*0.001) + (((Co2S*0.5) + (RespCr*0.5) + (RespEr*0.5)+ (REspCv*0.5) + (REspEv*0.5)+(REspCPv*0.5)+(REspEPv*0.5)+(RespCPr*0.5)+(RespEPr*0.5))*0.01*3.67))

# GPP
Output1$GPP  <- try(Output1$PST*0.5)


#Output2$fsjposi<-ifelse(Output1$fsj<0,0.01,Output2$fsj) #if fsj<0 print 0.01 else fsj

# write.xlsx(Output1, 'Output.xlsx')
write.csv(Output1, paste0("Output",isite,".csv",sep=""))

# next ---step_fig.R
