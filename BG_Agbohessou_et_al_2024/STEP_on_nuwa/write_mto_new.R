# write meteo files for 2D STEP
# yelognisse agbohessou
# 2020-03-20
# Write .mto file in STEP format for each site using rstep function: gen_step_mto

rm(list=ls())
require(rstep)
require(tibble)
require(lubridate)
#6. Write .mto file in STEP format for each site-----------------------------------------------

# import climate dataset
meteo_2019_2021 = read.csv("~/2D_step_sahel_agby/0-data/climat/preprocessed/meteo_sahel_2022.csv")

# in meteo_2019_2021 :
#Rain (mm) from IMERG: https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGDF_06/summary?keywords=%22IMERG%20final%22
#Temperature2m (min and max) from ERA5: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview
#Solar radiation, vapor pressure and wind speed2m  from era5

sum_data = meteo_2019_2021[-1]
sum_data$temp_min = replace(sum_data$temp_min,sum_data$temp_min<5,5)
# write mto file fo reach grid ------------------------------------------------------------------------

for (isite in unique(sum_data$id)){

  df = sum_data[sum_data$id %in% isite,] # saves to environment
  lati = df[which(df$id==isite)[1],"lat"]
  meteo.y <- df[,c("date","year","rain","RayGlo_MJm","temp_min","temp_max","pvap_2_m_hPa","Wind_Speed")]
  
  df = meteo.y[meteo.y$year %in% 2022,]
  df <- add_column(df, day = day(df$date), .after = 1)
  rstep::gen_step_mto(workspace = "~/2D_step_sahel_agby/0-data/step_2d_simu/step2d_workspace/Input_mto/",dataframe=df,isite=isite,
                        year = substr(unique(df$year),3,4),alt=308,lat=lati,hautmes=2)

}
