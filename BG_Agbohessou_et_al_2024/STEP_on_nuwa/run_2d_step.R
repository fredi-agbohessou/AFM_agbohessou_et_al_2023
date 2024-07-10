# write Stepatsec.exe
# yelognisse Agbohessou
# 12/06/2021
# compile 2D STEP in the git terminal and write Stepatsec.exe in the folder 1-code

rm(list=ls()) 

#set working dir
setwd("~/2D_step_sahel_agby/0-step.exe/2D_step_daily_original") 

# generate the model executable
cmd = "make"
system(cmd)
cmd1="cp Stepatsec ../../1-code/"
system(cmd1)
cmdd = "rm *.o"
system(cmdd)

