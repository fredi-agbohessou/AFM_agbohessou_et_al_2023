# yelognisse agbohessou
# write summary files for outputs
# 05/09/2021
rm(list=ls())
require(dplyr)
require(readr)
require(tibble)
require(lubridate)


id_site = read.csv("sum_all_sites/site_id.csv")##_132_2979.csv")

#id_site = read.csv("sum_all_sites/site_id_2980_5479.csv")
#id_site = read.csv("sum_all_sites/site_id_5480_7979.csv")
#id_site = read.csv("sum_all_sites/site_id_7980_10479.csv")
#id_site = read.csv("sum_all_sites/site_id_10480_12979.csv")
#id_site = read.csv("sum_all_sites/site_id_12980_15479.csv")
#id_site = read.csv("sum_all_sites/site_id_15480_18750.csv")

isite="" #"132_2979_"

start = 2012
end = 2022

Output <- lapply(unique(id_site$id), function(i) {
  file_path <- paste0("result_simu6/Output", i, ".csv")
  data <- read_csv(file_path, col_types = cols(
    Date = col_date(format = "%Y-%m-%d"),
    BMv_t_ha = col_number(),
    N2O_total_kg_ha = col_number(),
    LAIv = col_number(),
    Shum_1_per = col_number(),
    CO2Soil = col_number(),
    TqCs = col_number(),
    Total_N = col_number(),
    Rain = col_number(),
    Temp = col_number(),
    wfps = col_number(),
    Rayglo = col_number(),
    chaTotJ = col_number(),
    Amonia = col_number(),
    SoilResp = col_number()
  ),
  col_select = c("Date", "Jour", "Mois", "Rain", "BMv_t_ha", "N2O_total_kg_ha",
                 "LAIv", "Shum_1_per", "CO2Soil", "TqCs", "Total_N", "Temp",
                 "wfps", "Rayglo", "chaTotJ", "Amonia", "SoilResp"))
  
  data$id <- rep(i, nrow(data))  # add a column "Site" name to each element of the list
  return(data)
})

Output<- do.call("rbind", Output) #combine all element of the list into a data frame
dframe = tibble::add_column(Output, year = format(Output$Date, "%Y"),.after = 1)
dframe = tibble::add_column(dframe, month = format(dframe$Date, "%Y-%m"),.after = 2)

attach(dframe)

step_sum_table = function (varname, id_year,FUN = mean){
  sum1 <- aggregate(varname ~ year+month+id, dframe, paste0(FUN), na.rm=TRUE)
  df = sum1 %>% filter (year == id_year) #select a date
  data = merge(df,id_site, by="id")
  return(data)
}


#1. BMv_t_ha-----------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = BMv_t_ha, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_BMv_t_ha_",yr,".csv"))
}

#2. N2O_total_kg_ha-----------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = N2O_total_kg_ha, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_N2O_total_kg_ha_",yr,".csv"))
}


#7. LAI----------------------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = LAIv, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_LAI_",yr,".csv"))
}

#8. SWC 1----------------------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = Shum_1_per, id_year = yr, FUN="mean")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_Shum_1_per_",yr,".csv"))
}


#10. Microbial respiration ---------------------------------------------------------------------->
for (yr in start:end){
  df=step_sum_table(varname = CO2Soil, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_CO2Soil_g_m2_",yr,".csv"))
}


#1. Total soil C-----------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = TqCs, id_year = yr, FUN="mean")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_mean_Total_soil_C_gC_",yr,".csv"))
}
for (yr in start:end){
  df=step_sum_table(varname = TqCs, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_Total_soil_C_gC_",yr,".csv"))
}

#2. Total soil N-----------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = Total_N, id_year = yr, FUN="mean")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_mean_Total_soil_N_gN_",yr,".csv"))
}
for (yr in start:end){
  df=step_sum_table(varname = Total_N, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_Total_soil_N_gN_",yr,".csv"))
}

#8. SWC 1----------------------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = Shum_1_per, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_SWC_1_per_",yr,".csv"))
}
#4. Rain---------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = Rain, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_Rain_mm_",yr,".csv"))
}

for (yr in start:end){
  df=step_sum_table(varname = Rain, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_Rain_mm_",yr,".csv"))
}

#5. Temperature----------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = Temp, id_year = yr, FUN="mean")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_mean_Temperature_degC_",yr,".csv"))
}
for (yr in start:end){
  df=step_sum_table(varname = Temp, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_Temperature_degC_",yr,".csv"))
}
for (yr in start:end){
  df=step_sum_table(varname = Temp, id_year = yr, FUN="min")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_min_Temperature_degC_",yr,".csv"))
}

#6. Water Filed Pore Space----------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = wfps, id_year = yr, FUN="mean")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_mean_wfps_",yr,".csv"))
}
for (yr in start:end){
  df=step_sum_table(varname = wfps, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_wfps_",yr,".csv"))
}

#7. Rayglo----------------------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = Rayglo, id_year = yr, FUN="max")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_max_Rayglo_MJ_m2_",yr,".csv"))
}
for (yr in start:end){
  df=step_sum_table(varname = Rayglo, id_year = yr, FUN="mean")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_mean_Rayglo_MJ_m2_",yr,".csv"))
}

#9. total animal load for the day ----------------------------------------------------------------------------------------
for (yr in start:end){
  df=step_sum_table(varname = chaTotJ, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_Total_animal_load_",yr,".csv"))
}


#10. mineral nitrogen(gN) ---------------------------------------------------------------------->
for (yr in start:end){
  df=step_sum_table(varname = Amonia, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_amonia_gN_",yr,".csv"))
}

for (yr in start:end){
  df=step_sum_table(varname = Amonia, id_year = yr, FUN="mean")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_mean_amonia_gN_",yr,".csv"))
}

#Soil respiration ---------------------------------------------------------------------->
for (yr in start:end){
  df=step_sum_table(varname = SoilResp, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_SoilResp_gC_m2_",yr,".csv"))
}



