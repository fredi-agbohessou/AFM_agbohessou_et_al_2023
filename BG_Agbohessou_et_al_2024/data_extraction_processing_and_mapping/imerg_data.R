#map Imerg precip: just a test
# y. agbohessou

rm(list = ls())


library(ncdf4)
library(chron)
library(lattice)
library(RColorBrewer)
library(terra)
library(rasterVis)
library(raster)

#plot as raster
inputfile = paste0("0-data/climat/noisy_dataset/Imerg/precipCal_Imerg/2019/imerg_sn_precipitationCal_2019.csv")

nc = read.csv(inputfile)

names(nc)
nc$lon=round(nc$lon,2)
nc$lat=round(nc$lat,2)

library(tidyr)
data= pivot_wider(nc,names_from = time,values_from = precipitationCal)

Date <- seq.Date(as.Date("2019-01-01"), length.out = 365, by = "day", drop=FALSE)
names(data) <- c("lon","lat",paste0(Date))

data$an_sum <- apply(data[which(names(data)=="2019-01-01"):
                                        which(names(data)=="2019-12-31")],1,sum) # annual (i.e. row) means

consen0 = rgdal::readOGR("3-spatialisation/sn_shp/SEN_adm1.shp",verbose = FALSE)
raster <- rasterFromXYZ(data[,c("lon","lat","an_sum")]) 

bbox = c (
  "xmin" = -17.6,
  "ymin" = 12.3,
  "xmax" = -11.3,
  "ymax" = 16.7
)

grd_template = expand.grid(
  Long = seq(from = bbox["xmin"], to = bbox["xmax"], by = 0.1), # 0.05 degree
  Lat = seq(from = bbox["ymin"], to = bbox["ymax"], by = 0.1)
)

grd = raster::extract(raster,grd_template)
data = cbind(grd_template,grd)


s=raster()# create an empty raster
for (i in Date){
  raster <- rasterFromXYZ(df.long[,c("lon","lat",i)])  #Convert first two columns as lon-lat and third as value 
  data_raster_crop = crop(raster, extent(consen0))  #crop the raster
  data_raster_mask_sen = raster::mask(data_raster_crop,consen0) #Mask rasters using senegal polygon
  #assign(paste0("raster_",i),raster)
  s = stack(s,data_raster_mask_sen)# stack raters
}

plot(data_raster_mask_sen)

data = as.data.frame(as(data_raster_mask_sen, "SpatialPixelsDataFrame")) # Spatial dataframe of SM data
colnames(data) = c("varname","Long", "Lat") 


png("era5_sum_2019.png", width=10, height=6.5, 
    units='in',res = 200)
levelplot(data$varname ~ data$Long * data$Lat, cuts=5, pretty=T, 
          col.regions=((brewer.pal(11,"RdBu"))),
          main="ERA5: Total precipitation 2019 (mm)")

dev.off()



