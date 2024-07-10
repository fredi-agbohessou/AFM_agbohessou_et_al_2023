# subsetting netcdf file
# yelognisse agbohessou

# import pkgs
rm(list=ls())
library(rgdal)
library(ncdf4)
library(raster)

# import sahel shp mask
shp = rgdal::readOGR("3-spatialisation/save/sahel_cassecs.shp",verbose = FALSE)
proj4string(shp)<- CRS("+proj=longlat +datum=WGS84")



 # select the variables 
 variables = c("Solar-Radiation-Flux",
                "Temperature-Air-2m-Max-24h","Temperature-Air-2m-Min-24h",
                "Vapour-Pressure-Mean","Wind-Speed-Mean")

start_time <- Sys.time()
doParallel::registerDoParallel() # run faster using multiple cores (processors)

# Extract sahel from the global dataset
for (var in unique(variables)){
  for (yr in 2010:2020){
    # import nc file for 1st jan
    pre0.brick = brick(paste0("0-data/climat/noisy_dataset/ERA5_nc/",var,"/",yr,"/",var,"_C3S-glob-agric_AgERA5_",yr,"0101_final-v1.0.nc"))
    
    # subset data of 1st jan
    pre0.mask = mask(pre0.brick, shp)
    pre0.df = as.data.frame(pre0.mask, xy=TRUE)
    data=pre0.df[complete.cases(pre0.df), ]
    
    # add date col
    Date <- seq.Date(from= as.Date(paste0(yr,"-01-02")), to = as.Date(paste0(yr,"-12-31")), by = "day", drop=FALSE)
    date = data.frame("date"=Date)
    date$year = format(date$date,"%Y")
    date$month = format(date$date,"%m")
    date$day = format(date$date,"%d")
    date$ymd=paste0(date$year,date$month,date$day)  
    
    for(i in unique(date$ymd)){
      pre1.brick = brick(paste0("0-data/climat/noisy_dataset/ERA5_nc/",var,"/",yr,"/",var,"_C3S-glob-agric_AgERA5_",i,"_final-v1.0.nc"))
      pre1.mask = mask(pre1.brick, shp)
      pre1.df = as.data.frame(pre1.mask, xy=TRUE)
      pre1.df=pre1.df[complete.cases(pre1.df), ]
      data = cbind(data,pre1.df[3])    
    }  
    write.csv(data,paste0("0-data/climat/noisy_dataset/ERA5_nc/Sahel_",var,"_",yr,".csv"))
  }
}

end_time <- Sys.time()
end_time - start_time

