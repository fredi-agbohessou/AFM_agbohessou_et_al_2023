# Extracting  AgERA5 data from the Copernicus Climate Data Store (CDS) using the ag5Tools package
#y. agbohessou

# 1- Toolbox for downloading and extracting data from the Copernicus AgERA5 dataset
# https://cds.climate.copernicus.eu/cdsapp#!/dataset/10.24381/cds.6c68c9bb?tab=overview

# 2- Install your API file
# https://confluence.ecmwf.int/display/CKB/How+to+install+and+use+CDS+API+on+Windows


# Install ag5Tools R package
#devtools::install_github("agrdatasci/ag5Tools", build_vignettes = TRUE)
# ?ag5_download
# ??ag5Tools
require(ag5Tools)

start_time <- Sys.time()
doParallel::registerDoParallel() # run faster using multiple cores (processors)

for (i in 2010:2020){

  # solar_radiation_flux------------------------
  ag5_download(variable = "solar_radiation_flux",
               day = "all",
               month = "all",
               year = i,
               path = "0-data/climat/noisy_dataset/ERA5_nc/Solar-Radiation-Flux/")
  
  # temperature min--------------------------------
  ag5_download(variable = "2m_temperature",
               statistic = "24_hour_minimum",
               day = "all",
               month = "all",
               year = i,
               path = "0-data/climat/noisy_dataset/ERA5_nc/Temperature-Air-2m-Min-24h/")
  
  # temperature max---------------------------------
  ag5_download(variable = "2m_temperature",
               statistic = "24_hour_maximum",
               day = "all",
               month = "all",
               year = i,
               path = "0-data/climat/noisy_dataset/ERA5_nc/Temperature-Air-2m-Max-24h/")
  
  # vapour_pressure-----------------------------------
  ag5_download(variable = "vapour_pressure",
               statistic = "24_hour_mean",
               day = "all",
               month = "all",
               year = i,
               path = "0-data/climat/noisy_dataset/ERA5_nc/Vapour-Pressure-Mean/")
  # ......
}

end_time <- Sys.time()
end_time - start_time

