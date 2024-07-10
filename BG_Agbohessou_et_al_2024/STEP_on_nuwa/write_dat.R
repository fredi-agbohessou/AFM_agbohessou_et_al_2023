# write pluri.dat file
# import the updated dat.xlsx file before running the code 
rm(list=ls())
require(rstep)

data3 = read.csv("~/2D_step_sahel_agby/0-data/sol/preprocessed/sahel_soil_data_final3.csv")
data3 = data3[-1]

for(i in 1:nrow(data3)){
    rstep::gen_step_file(filepath = "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",filename ="dat",
                         state.date = 10,end.date = 12,
                         isite = paste0(data3[i,"id"]))
}
