# Run STEP-GENDEC-N2O 
# y. agbohessou
# 05-01-2021

# R.version
rm(list=ls())

# ------------------------------------------------------------------------------------
#Load library
require(rstep)
# Define the workspaces --------------------------------------------------------------
workspace = normalizePath("2-outputs/Output", winslash = "/")
filepath = normalizePath("0-data/step_1d_simu/daily_original", winslash = "/")
step.exe = normalizePath("0-step.exe/step_1D/step_daily_original/", winslash = "/")

# run step
#run_step_1D(step_daily_original = step.exe,daily_original =filepath)

#write output file in csv and xlsx format in the workspace---------------------------
write_step_out("Dahra",workspace,12,20,"csv")

# Define the variables of interest----------------------------------------------------

sim_variables = c("Rain","Shum_1_per","Etr","TqCs","Total_N","EP","TrP","TqCs","REspCPv","NO3",
                  "Reco","En2o_NOE","N2OFlux_Process","REspEPv","BMPv","BMs","PST","BMv","NO2",
                  "BMPv", "RespCr","CO2Soil","RespEr","REspCv","REspEv","SoilResp","BMt","N2O_total_t_ha",
                  "Reco_t_ha","N2Odenit_kg_ha","N2Onit_kg_ha","N2O_total_kg_ha","N2O_total_kg_CO2_equiv_ha",
                  "N2O_total_t_CO2_equiv_ha","Reco_t_CO2_ha","GHG_t_CO2_equiv_ha","Temp","BMt_t_ha","BLmv",
                  "BMv","Bmlita","vcfv","vcfs","vcfl","vcft","Shum_0_per","Shum_1_per","Shum_2_per","Shum_3_per",
                  "BFlita","GPP","LAIv","wfps","Cs(1)","Cs(2)","Shum_0","Shum_1","Shum_2","NOFLUX_(kgN/ha/an)","NOFlux_Process") 
sim_variables2 = c("Rain","Shum_1_per","Etr","TqCs","Reco","Total_N","BMv","vcfv","GPP","LAIv","En2o_NOE")

# create a table for plots ------------------------------------------------------------
df= gen_table(filepath,workspace,"Dahra",sim_variables,obs=F,test0 = F)
df_2= gen_table(filepath,workspace,"Dahra",sim_variables2,obs=T,test0 = F)

# No animal_test-----------------------------------
df_no_ani= gen_table(filepath,workspace,"Dahra_no_animal",sim_variables,obs=F,test0 = F)
df_2_no_ani= gen_table(filepath,workspace,"Dahra_no_animal",sim_variables2,obs=T,test0 = F)

# add Dynacof output-----------------------------------------
#dynacof1 = read.csv("2-outputs/Output/Output_Dynacof_balanites.csv")
dynacof1 = read.csv("2-outputs/Output/Final_Output_B_aegyptiaca.csv")
dynacof1$Date = as.Date(dynacof1$Date)
dyna1 = subset(dynacof1,dynacof1$Date>"2011-12-31"&dynacof1$Date<"2021-01-01")
#dynacof2 = read.csv("2-outputs/Output/Output_Dynacof_raddiana.csv")
dynacof2 = read.csv("2-outputs/Output/Final_Output_A_raddiana.csv")
dynacof2$Date = as.Date(dynacof2$Date)
dyna2 = subset(dynacof2,dynacof2$Date>"2011-12-31"&dynacof2$Date<"2021-01-01")
df$Reco_t_ha = df$Reco *0.01
