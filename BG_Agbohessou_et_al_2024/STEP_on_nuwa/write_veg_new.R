# veg: formatting veg file for STEP simulation
rm(list=ls())

# import required packages -------------------------------------------------------------
require(rstep)
# import preprocessed data -------------------------------------------------------------

data <- read.csv("~/2D_step_sahel_agby/0-data/veg/veg_data3.csv")
for(i in 1:nrow(data)){
  for(j in 10:22){
    t = rstep::veg
    t[11,1][t[11,1] == t[11,1]] <-paste0(data[i,"leaf_DM_kg_ha2"])
    t[11,2][t[11,2] == t[11,2]] <- data[i,"leaf_DM_kg_ha2"] 
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",
                               data[i,"id"],"/S01001",j,".veg"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  }
}
