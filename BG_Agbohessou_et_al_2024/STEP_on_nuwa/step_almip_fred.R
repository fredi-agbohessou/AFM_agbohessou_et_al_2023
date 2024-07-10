# Run STEP for multiple sites
# yelognisse & Manuela 
# 24/06/2021

# Run step for each site (and the years defined in the input file)

rm(list=ls()) #cleans every object

workdir = "~/2D_step_sahel_agby/1-code"
inputdirmto= "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_mto"
inputdirsol= "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other"
output= "~/2D_step_sahel_agby/2-outputs/new_output"


setwd(workdir) 

# import site id (is id_mto)
############# site_id =  read.csv("sum_all_sites/site_id.csv")

### site_id =  read.csv("sum_all_sites/site_id_132_2979.csv")
### site_id =  read.csv("sum_all_sites/site_id_2980_5479.csv") 
### site_id =  read.csv("sum_all_sites/site_id_5480_7979.csv")
### site_id =  read.csv("sum_all_sites/site_id_7980_10479.csv")
### site_id =  read.csv("sum_all_sites/site_id_10480_12979.csv")
### site_id =  read.csv("sum_all_sites/site_id_12980_15479.csv")
site_id =  read.csv("sum_all_sites/site_id_15480_18750.csv")


### site_id =  read.csv("sum_all_sites/id_site_arg.csv")


for (isite in unique(site_id$id)){
  cmd =c(paste("cp ",inputdirmto,"/",isite,"/*.* .",sep = ""))
  cmdsol =c(paste("cp ",inputdirsol,"/",isite,"/*.* .",sep = ""))
  system(cmd)
  system(cmdsol)
  cmdexe= "./Stepatsec"
  system(cmdexe)
  source("out_to_xlsx_2D.R")
  #cmdR="Rscript step_fig.R"
  #system(cmdR)
  #cmdoutS01 =c(paste("mv *.out *.xlsx ",output,"/",isite,sep = ""))
  #system(cmdoutS01)
  cmdrm= "rm arbre.dat carbone.dat ChAnim.dat *.mto *.burn fort.* *.disp *.sol *.cha *.cle *.wet *.veg *.bmt *.out"
  system(cmdrm)
}



