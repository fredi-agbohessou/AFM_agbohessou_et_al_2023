## write cle param-----------------------------------------------------

# import dataset
data = read.csv("~/2D_step_sahel_agby/0-data/cle_param/cle_data.csv")

#cle: formatting cle file for STEP simulation:

# 2022
for(i in 1:nrow(data)){
  t = rstep::cle
  #formatting .cle file
  t[1,1][t[1,1] == t[1,1]] <- 4.5 # old val = 5  efficience de conversion (gMS/MJ)  !2.5 and 7 caroline pierre
  t[1,2][t[1,2] == t[1,2]] <- data[i,"BI_2021"] #  Biomasse verte initiale  (gMS/m2) !0.5 et 2.5
  #t[1,3][t[1,3] == t[1,3]] <- 43 #  % de C3    !30 et 60
  t[1,4][t[1,4] == t[1,4]] <- data[i,"SLA0_2021"] #  Slag0 vert (cm² g-1) !180-280

  # writting .cle file
  myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",
                             data[i,"id"],"/S0100122",".cle"))
  write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
}
