# veg: formatting veg file for STEP simulation
rm(list=ls())

# import required packages -------------------------------------------------------------
require(rstep)
# import preprocessed data -------------------------------------------------------------

data <- read.csv("~/2D_step_sahel_agby/0-data/veg/veg_data.csv")
for(i in 1:nrow(data)){
  t = rstep::veg
  # Ligneux: Biomasse foliaire max (kg/ha)
  t[11,1][t[11,1] == t[11,1]] <- "600" #paste0(data[i,"veg600"]) #  annee precedente 650
  t[11,2][t[11,2] == t[11,2]] <- 400 #data[i,"veg400"] #  annee courante ! 300
  myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",
                             data[i,"id"],"/S0100112.veg"))
  write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  
  # 2022
  t = rstep::veg
  t[11,1][t[11,1] == t[11,1]] <- "400" #paste0(data[i,"veg400"]) #  annee precedente  !400
  t[11,2][t[11,2] == t[11,2]] <- 500 #data[i,"veg500"] #  annee courante !400
  myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",
                             data[i,"id"],"/S0100122.veg"))
  write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  
  # 2013-2021
  for(j in 13:21){
    t = rstep::veg
    t[11,1][t[11,1] == t[11,1]] <-"400" #paste0(data[i,"veg400"])#  annee precedente  !400
    t[11,2][t[11,2] == t[11,2]] <- 400 #data[i,"veg400"] #  annee courante !400
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",
                               data[i,"id"],"/S01001",j,".veg"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  }
  
}
