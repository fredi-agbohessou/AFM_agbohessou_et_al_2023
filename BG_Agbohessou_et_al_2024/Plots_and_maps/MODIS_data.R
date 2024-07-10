# test MODIS data
# y.agbohessou 
rm(list = ls())

# create a grid of 0.1*0.1 res
bbox = c ("xmin" = -17.6,"ymin" = 12.3,"xmax" = -11.3,"ymax" = 16.7)
grd_template = expand.grid(Long = seq(from = bbox["xmin"], to = bbox["xmax"], by = 0.1), # 0.05 degree
                           Lat = seq(from = bbox["ymin"], to = bbox["ymax"], by = 0.1))

#MODIS data from : https://ladsweb.modaps.eosdis.nasa.gov/missions-and-measurements/modis/
#NASA reprojection tool available at :https://wiki.earthdata.nasa.gov/display/DAS/HEG%3A++HDF-EOS+to+GeoTIFF+Conversion+Tool

# Modis data doc : https://modis.gsfc.nasa.gov/data/dataprod/mod13.php

# Documentation about MOD13A3 available at: https://lpdaac.usgs.gov/products/mod13a3v006/

# For batch run from the GUI read
# chapter 8.2.1 Batch Initiation from GUI 
# of "0-data/MODIS/HEG/EED2-TP-030_Rev01_HEG_UsersGuide_2.15.pdf"
# Provides detailed data on hdf4 files but takes ages

#devtools:::install_github("gearslaboratory/gdalUtils")

library(gdalUtils)
library(raster)
require(tidyverse)
for(pkg in c("raster","sp","sf","gstat","readr","terra","rasterVis",
             "fields","interp","mgcv","automap", "patchwork", "viridis","RColorBrewer",
             "tidyr","rstep"
)){
  library(pkg, character.only = TRUE)
}

# NDVI-----------------------------------------------------------------------------------
files_hdf=list.files(path = "0-data/MODIS/NDVI")[-1]
#files_tif=list.files(path = "0-data/MODIS/NDVI/HEGOUT")[-25]
lst =NULL
lst = list()
for (i in unique(files_hdf)){
  filename = substr(i,1,41) # 41 = nchar("MOD13A3.A2020214.h16v07.061.2020346100416_HEGOUT.tif")-nchar("_HEGOUT.tif")
  info = gdalinfo(paste0("0-data/MODIS/NDVI/",filename,".hdf"))
  date= substr(info[136],22,32) # extract date
  rst = raster(paste0("0-data/MODIS/NDVI/HEGOUT/",filename,"_HEGOUT.tif"))
  NDVI = raster::extract(rst,grd_template)
  lst[[paste0("NDVI_",date)]]=NDVI
  #assign(paste0("NDVI_",date),NDVI)
}
require(dplyr)
df <- do.call("bind_rows",lst) #combine all vectors into a matrix
df= replace(df,df == -3000, NA)
df = df*0.0001 # scale factor = 0.0001
data = cbind(grd_template,df)
data1 = data[complete.cases(data), ]  # drop all rows with NA

write.csv(data1,"0-data/MODIS/sn_MODIS_monthly_NDVI_2019_2020.csv")

consen0 = rgdal::readOGR("3-spatialisation/sn_shp/SEN_adm1.shp",verbose = FALSE)

# 2019
month=names(data1[,3:14])
s=raster()# create an empty raster
for (j in month){
  raster <- rasterFromXYZ(data1[,c("Long","Lat",j)])  #Convert first two columns as lon-lat and third as value 
  data_raster_crop = crop(raster, extent(consen0))  #crop the raster
  data_raster_mask_sen = raster::mask(data_raster_crop,consen0) #Mask rasters using senegal polygon
  s = stack(s,data_raster_mask_sen)# stack raters
}

p1 = levelplot(s, cuts=6, pretty=T, 
               col.regions=(brewer.pal(8,"BrBG")) ,  
               main="MODIS Monthly NDVI 2019")+
  latticeExtra::layer(sp.lines(consen0, col="black", lwd=0.5))
p1

# 2020
month=names(data1[,15:26])
s=raster()# create an empty raster
for (j in month){
  raster <- rasterFromXYZ(data1[,c("Long","Lat",j)])  #Convert first two columns as lon-lat and third as value 
  data_raster_crop = crop(raster, extent(consen0))  #crop the raster
  data_raster_mask_sen = raster::mask(data_raster_crop,consen0) #Mask rasters using senegal polygon
  s = stack(s,data_raster_mask_sen)# stack raters
}

p2 = levelplot(s, cuts=6, pretty=T, 
               col.regions=(brewer.pal(8,"BrBG")) ,  
               main="MODIS Monthly NDVI 2020")+
  latticeExtra::layer(sp.lines(consen0, col="black", lwd=0.5))
p2

pdf("0-data/MODIS/pdf_NDVI.pdf")
print(p1)
print(p2)
dev.off()

# LAI-----------------------------------------------------------------------------------------
filename = substr(files_hdf[8],1,42)
#info = gdalinfo(paste0("0-data/MODIS/LAI/","MOD15A2H.A2019057.h16v07.006.2019084162114",".hdf"))
#date=paste0(substr(info[28],17,19),"_",substr(info[28],21,22),"_",substr(info[28],33,38))

files_hdf=list.files(path = "0-data/MODIS/LAI")[-1]
#files_tif=list.files(path = "0-data/MODIS/LAI/HEGOUT")[-93]
lst2 = list()
for (i in unique(files_hdf)){
  filename = substr(i,1,42) # 42 = nchar("MOD15A2H.A2019001.h16v07.006.2019010204200.hdf")-nchar(".hdf")
  info = gdalinfo(paste0("0-data/MODIS/LAI/",filename,".hdf"))
  date= paste0(substr(info[28],17,19),"_",substr(info[28],21,22),"_",substr(info[28],33,38)) # extract date
  rst = raster(paste0("0-data/MODIS/LAI/HEGOUT/",filename,"_HEGOUT.tif"))
  LAI = raster::extract(rst,grd_template)
  lst2[[paste0("LAI_",date)]]=LAI
  #assign(paste0("LAI_",date),LAI)
}
require(dplyr)
df <- do.call("bind_rows",lst2) #combine all vectors into a matrix
df= replace(df,df > 60, NA)
df = df*0.1 # scale factor = 0.1
#df = replace(df,df >5, NA)
data = cbind(grd_template,df)
data1 = data[complete.cases(data), ]  # drop all rows with NA

require(tidyr)
data2 = pivot_longer(data1, cols=3:ncol(data1),names_to ="Date",values_to = "LAI")
data2$YM=paste0(substr(data2$Date,5,7),"_",substr(data2$Date,12,15))

require(plyr)
data3 <- ddply(data2, .(Long,Lat,YM), summarize,
               LAI_val=mean(LAI)
               )

data4 = pivot_wider(data3,names_from = YM,values_from = LAI_val)

val_name =c("Long","Lat","Jan_2019","Feb_2019","Mar_2019","Apr_2019","May_2019",
            "Jun_2019","Jul_2019","Aug_2019","Sep_2019","Oct_2019","Nov_2019","Dec_2019",
            "Jan_2020","Feb_2020","Mar_2020","Apr_2020","May_2020",
            "Jun_2020","Jul_2020","Aug_2020","Sep_2020","Oct_2020","Nov_2020","Dec_2020")

data5=data4[,val_name]
write.csv(data5,"0-data/MODIS/sn_MODIS_monthly_LAI_2019_2020.csv")

consen0 = rgdal::readOGR("3-spatialisation/sn_shp/SEN_adm1.shp",verbose = FALSE)

# 2019
month=names(data5[,3:14])
s=raster()# create an empty raster
for (j in month){
  raster <- rasterFromXYZ(data5[,c("Long","Lat",j)])  #Convert first two columns as lon-lat and third as value 
  data_raster_crop = crop(raster, extent(consen0))  #crop the raster
  data_raster_mask_sen = raster::mask(data_raster_crop,consen0) #Mask rasters using senegal polygon
  s = stack(s,data_raster_mask_sen)# stack raters
}

p1 = levelplot(s, cuts=6, pretty=T, 
               col.regions=(brewer.pal(8,"BrBG")) ,  
               main="MODIS Monthly LAI 2019")+
  latticeExtra::layer(sp.lines(consen0, col="black", lwd=0.5))
p1

# 2020
month=names(data5[,15:26])
s=raster()# create an empty raster
for (j in month){
  raster <- rasterFromXYZ(data5[,c("Long","Lat",j)])  #Convert first two columns as lon-lat and third as value 
  data_raster_crop = crop(raster, extent(consen0))  #crop the raster
  data_raster_mask_sen = raster::mask(data_raster_crop,consen0) #Mask rasters using senegal polygon
  s = stack(s,data_raster_mask_sen)# stack raters
}

p2 = levelplot(s, cuts=6, pretty=T, 
               col.regions=(brewer.pal(8,"BrBG")) ,  
               main="MODIS Monthly LAI 2020")+
  latticeExtra::layer(sp.lines(consen0, col="black", lwd=0.5))
p2

pdf("0-data/MODIS/pdf_LAI.pdf")
print(p1)
print(p2)
dev.off()
