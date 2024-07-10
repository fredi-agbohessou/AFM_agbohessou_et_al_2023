# write soil files for STEP
# yelognisse agbohessou
# 2022-03-15
# from the dataset built in Qgis, write soil files for each site
rm(list=ls())

# import required packages
require(rstep)

# import final pre-processed dataset and write sol files for 2D STEP ---------------------------
data3 = read.csv("~/2D_step_sahel_agby/0-data/sol/preprocessed/sahel_soil_data_final4.csv")
data3 = data3[-1]

# import files from rstep
ini=c("bmt","burn","disp","wet") #"veg","cle","dat"
for (j in unique(ini)){
  rstep::gen_step_xlsx(filepath = "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",filename =paste0(j))
}

# format .dat file generated before running the loop

# generate the files for all sites : "dat","bmt","burn","disp","wet" and "sol"
for(i in 1:nrow(data3)){
  for (j in unique(ini)){
    
    # import sol file from rstep
    t = rstep::sol
    t[2,2][t[2,2]=="ZONE SAHELIENNE DU SENEGAL"] <- paste0("ZONE SAHELIENNE")
    
    # format the sol file for each site using the dataset
    t[1,2][t[1,2] =="SITE RSP_six_forage"] <- paste0("SITE_",data3[i,"id"]," [long_lat: ",data3[i,"coord.x"],"]")
    t[3,2][t[3,2]=="0.29648000000000002"] <- paste0(data3[i,"albedo"])
    t[5,2][t[5,2] =="0.6"] <- paste0(data3[i,"Soilwater1"])
    t[6,2][t[6,2] =="17.87"] <- paste0(data3[i,"Tsoil1"])
    t[8,2][t[8,2] == "5"] <-paste0(data3[i,"clay1"])
    t[9,2][t[9,2] == "93"] <-paste0(data3[i,"sand1"])
    t[10,2][t[10,2] == "7.4"] <-paste0(data3[i,"pH1"])
    t[11,2][t[11,2] == "2"] <- paste0(data3[i,"silt1"])
    
    t[5,3][t[5,3] == t[5,3]] <- data3[i,"Soilwater2"]
    t[6,3][t[6,3] == t[6,3]] <- data3[i,"Tsoil2"]
    t[8,3][t[8,3] == t[8,3]] <-data3[i,"clay2"]
    t[9,3][t[9,3] == t[9,3]] <-data3[i,"sand2"]
    t[10,3][t[10,3] == t[10,3]] <-data3[i,"pH2"]
    t[11,3][t[11,3] == t[11,3]] <- data3[i,"silt2"]
    
    t[5,4][t[5,4] == t[5,4]] <- data3[i,"Soilwater3"]
    t[6,4][t[6,4] == t[6,4]] <- data3[i,"Tsoil3"]
    t[8,4][t[8,4] == t[8,4]] <-data3[i,"clay3"]
    t[9,4][t[9,4] == t[9,4]] <-data3[i,"sand3"]
    t[10,4][t[10,4] == t[10,4]] <-data3[i,"pH3"]
    t[11,4][t[11,4] == t[11,4]] <- data3[i,"silt3"]
    
    t[5,5][t[5,5] == t[5,5]] <- data3[i,"Soilwater4"]
    t[6,5][t[6,5] == t[6,5]] <- data3[i,"Tsoil4"]
    t[8,5][t[8,5] == t[8,5]] <-data3[i,"clay4"]
    t[9,5][t[9,5] == t[9,5]] <-data3[i,"sand4"]
    t[10,5][t[10,5] == t[10,5]] <-data3[i,"pH4"]
    t[11,5][t[11,5] == t[11,5]] <- data3[i,"silt4"]
    
    t[6,6][t[6,6] == t[6,6]] <- data3[i,"Tsoil5"]
    
    #new folder for the files
    #dir.create (paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"]))
    
    # write sol file in the new folder
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100110.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100111.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile2 <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                                "/S0100112.sol"))
    write.table(t, file = myfile2,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100113.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100114.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile2 <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                                "/S0100115.sol"))
    write.table(t, file = myfile2,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100116.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100117.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile2 <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                                "/S0100118.sol"))
    write.table(t, file = myfile2,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100119.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                               "/S0100120.sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile2 <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                                "/S0100121.sol"))
    write.table(t, file = myfile2,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    myfile2 <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",data3[i,"id"],
                                "/S0100122.sol"))
    write.table(t, file = myfile2,sep="\t",col.names=FALSE,
                row.names=FALSE,na="",quote=FALSE)
    
    #write "dat","bmt","burn","cle","disp","wet" files in the new folder
    rstep::gen_step_file(filepath = "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",filename =paste0(j),
                         state.date = 10,end.date = 22,
                         isite = paste0(data3[i,"id"]))
    
    rstep::gen_step_file(filepath = "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",filename ="dat",
                         state.date = 10,end.date = 22,
                         isite = paste0(data3[i,"id"]))
  }
  
}

#remove xlsx file from the directory
file = c("bmt","burn","dat","disp","wet")
unlink(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",file,".xlsx"))
