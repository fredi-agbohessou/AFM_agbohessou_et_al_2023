# yelognisse agbohessou
# Run 2D STEP 
# 05/09/2021

rm(list=ls()) #cleans every object
# col.reg = ((brewer.pal(10,"BrBG"))) biomass
# col.reg = ((brewer.pal(10,"OrRd"))) GHG
# I. installation-----------------------------------------------------------------

# 1. copy the folder of the Rproject "2D_step_fredi" on your laptop 
# 2. install R and fortran on windows : find resources in "0-Install Fortran on windows"
# 3. compile 2d STEP model by running in the terminal git the cmd : 'Rsript.exe run_2d_step.R'
# 4. copy Stepatsec.exe from 0-step.exe/2D_step_daily_original/Stepatsec.exe to 1-code/
# file.copy(from = "0-step.exe/2D_step_daily_original/Stepatsec.exe",
#           to = "1-code/", 
#           overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)

# II. prepare input files----------------------------------------------------------------

# Import functions
Sys.setenv(TZ="UTC")
require(rstep)
require(readr)

# Define the workspaces
workspace = "~/2D_step_sahel_agby/0-data/climat/step_mto_from_ERA5"
filepath = "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other"
mto_filepath = "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_mto"
new_outpath ="~/2D_step_sahel_agby/2-outputs/new_output"
old_outpath = "~/2D_step_sahel_agby/2-outputs/old_output"

# import id site
site_id =  read.csv("~/2D_step_sahel_agby/1-code/site_id.csv")
# create directories for output files of each site
sapply(paste0("~/2D_step_sahel_agby/2-outputs/new_output/",unique(site_id$id)),dir.create)

# III. run the model for all sites----------------------------------------------------
# run in the terminal git the cmd : 'Rscript.exe step_almip_fred.R'-------

# moving output files from a new simulation to another dir----------------

clean_out_dir=function(new_outpath,old_outpath){
  #move folders from new dir to old dir
  current_files = list.files(new_outpath, full.names = TRUE)
  file.copy(from = current_files, to = old_outpath, overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  
  t = list.files(path=paste0(current_files), pattern=NULL, all.files=FALSE,
                 full.names=TRUE)
  sapply(paste0(t),file.remove)# remove all file from new_output dir
}

clean_out_dir(new_outpath,old_outpath)

# 4. Delete Outputs
#
# for (i in 1:nrow(site_id)){
#   unlink(paste0("~/2D_step_sahel_agby/1-code/Output",site_id[i,"id"],".csv"))
# }

# 5. re-run the model for all sites--------------------------------------
#run in the terminal git the cmd : 'Rscript.exe step_almip_fred.R'

# 7. sum output --------------------------------------------------------------------
# Rscript plot_map.R in the terminal to make summary of output files 
