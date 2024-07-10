# formating cha file using data from Gridded Livestock of the World (GLW)
# https://dataverse.harvard.edu/dataverse/glw

# resolution : 10 km: 0.083333 decimal degrees resolution (10km x 10km = 100km2 = 10000 ha) 
# resolution of my grid 0.1*0.1 = 11.1 x 11.1 = 123.21 km2 = 12321 ha

rm(list=ls())

# import required packages -------------------------------------------------------------
require(rstep)

# import preprocessed data -------------------------------------------------------------
df = read.csv("~/2D_step_sahel_agby/0-data/animal_load/preprocessed/sahel_animal_load_GLW3.csv")

id_site = read.csv("~/2D_step_sahel_agby/1-code/sum_all_sites/site_id.csv")
data = merge(id_site,df,by="id")
df = data
df$lon = df$lon.x
df$lat = df$lat.x

res_grid= 1500 #10000  #  (aires de desserte = px resolution of glw dataset)
# write cha file for all px
for(i in 1:nrow(df)){

  # import cha file
  t = rstep::cha

  # repartition de la charge animale pour chaque mois
  t[5:16,1][t[5:16,1] == t[5:16,1]] <- paste0(df[i,"rep_bovin"]) # Bovins
  t[5:16,2][t[5:16,2] == t[5:16,2]] <- df[i,"rep_caprin"] # Caprins
  t[5:16,3][t[5:16,3] == t[5:16,3]] <- df[i,"rep_ovin"] # Ovins
  t[5:16,4][t[5:16,4] == t[5:16,4]] <- df[i,"rep_asins"] # Asins
  t[5:16,5][t[5:16,5] == t[5:16,5]] <- df[i,"rep_camelins"] # Camelins
  t[5:16,6][t[5:16,6] == t[5:16,6]] <- df[i,"rep_equin"] # Equins

  # 31 lignes: Jour, charge animale totale (nombre de tetes) * 12
  t[17:47,2][t[17:47,2] == t[17:47,2]] <- round(df[i,"total"]*0.6) # jan 60%
  t[17:47,3][t[17:47,3] == t[17:47,3]] <- round(df[i,"total"]*0.4) # feb 40%
  t[17:47,4][t[17:47,4] == t[17:47,4]] <- round(df[i,"total"]*0.2) # mar 20%
  t[17:47,5][t[17:47,5] == t[17:47,5]] <- round(df[i,"total"]*0.2) # apr
  t[17:47,6][t[17:47,6] == t[17:47,6]] <- round(df[i,"total"]*0.2) # may
  t[17:47,7][t[17:47,7] == t[17:47,7]] <- round(df[i,"total"]*0.4) # jun
  t[17:47,8][t[17:47,8] == t[17:47,8]] <- round(df[i,"total"]*0.6) # jul
  t[17:47,9][t[17:47,9] == t[17:47,9]] <- round(df[i,"total"]*0.6) # aug
  t[17:47,10][t[17:47,10] == t[17:47,10]] <- df[i,"total"] # sep 100%
  t[17:47,11][t[17:47,11] == t[17:47,11]] <- df[i,"total"] # oct
  t[17:47,12][t[17:47,12] == t[17:47,12]] <- df[i,"total"] # nov
  t[17:47,13][t[17:47,13] == t[17:47,13]] <- df[i,"total"] # dec

  # aires de desserte
  t[48,1][t[48,1] == t[48,1]] <- paste0(res_grid) # (ha)
  t[48,2:12][t[48,2:12] == t[48,2:12]] <- res_grid # (ha)

  for (yrs in 10:22){
    # write cha file in the new folder 
    myfile <- file.path(paste0("~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_sol_and_other/",df[i,"id"],
                               "/S01001",yrs,".cha")) 
    write.table(t, file = myfile,sep="\t",col.names=FALSE, 
                row.names=FALSE,na="",quote=FALSE)    
  }

}

