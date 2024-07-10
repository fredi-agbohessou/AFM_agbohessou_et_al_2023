# yelognisse agbohessou
# write summary files for outputs
# 05/09/2021
rm(list=ls())
require(dplyr)
require(readr)
require(tibble)
require(lubridate)

id_site = read.csv("sum_all_sites/site_id.csv")
isite=""

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
    SoilResp = col_number(),
    `NOFLUX_(kgN/ha/an)`=col_number(),
    NOFlux_Process = col_number(),
    Reco_t_ha = col_number(),
    GPP= col_number(),
    GHG_t_CO2_equiv_ha=col_number()
  ),
  col_select = c("Date", "Jour", "Mois", "Rain", "BMv_t_ha", "N2O_total_kg_ha",
                 "LAIv", "Shum_1_per", "CO2Soil", "TqCs", "Total_N", "Temp",
                 "wfps", "Rayglo", "chaTotJ", "Amonia", "SoilResp","NOFLUX_(kgN/ha/an)","NOFlux_Process",
                 "Reco_t_ha","GPP","GHG_t_CO2_equiv_ha"))
  data$NOFLUX_kgN_ha_an = data$`NOFLUX_(kgN/ha/an)`
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

#NOFlux_Process : daily amount of N0 emitted from soil by mineralisation/nitrification  (kgN.ha-1)---------------------------------------------------------------------->
for (yr in start:end){
  df=step_sum_table(varname = NOFlux_Process, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_NOFlux_Process_kgN_ha_",yr,".csv"))
}

#GPP : gross primary production (gC/m2)---------------------------------------------------------------------->
for (yr in start:end){
    df=step_sum_table(varname = GPP, id_year = yr, FUN="sum")
    write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_GPP_gC_m2_",yr,".csv"))
}

# "Reco_t_ha : Ecosystem respiration = Soil respiration + aboveground plant respiration (tC.ha-1)"  --------------------------------------------------------------------->
for (yr in start:end){
    df=step_sum_table(varname = Reco_t_ha, id_year = yr, FUN="sum")
    write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_Reco_t_ha_",yr,".csv"))
}

# "GHG_t_CO2_equiv_ha : Total GHG (N2O & CO2) budget in (tCO2.eq.ha)"   --------------------------------------------------------------------->
for (yr in start:end){
    df=step_sum_table(varname = GHG_t_CO2_equiv_ha, id_year = yr, FUN="sum")
    write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_GHG_t_CO2_equiv_ha_",yr,".csv"))
}

#NOFLUX_(kgN/ha/an) : daily amount of NO emitted from soil (kgN.ha-1) ---------------------------------------------------------------------->
for (yr in start:end){
  df=step_sum_table(varname = NOFLUX_kgN_ha_an, id_year = yr, FUN="sum")
  write.csv(df,paste0("sum_all_sites/new_simu/",isite,"monthly_sum_NOFLUX_kgN_ha_an_",yr,".csv"))
}