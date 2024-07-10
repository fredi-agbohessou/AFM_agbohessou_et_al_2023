# y. agbohessou

library(raster)

# replace 0 with na 
dem<-raster("3-spatialisation/save/glc_shrubs_covered_areas_sahel.tif")
values(dem)[values(dem) == 0] = NA
writeRaster(dem,"3-spatialisation/save/glc_shrubs_covered_areas_sahel.tif", overwrite=TRUE)

plot(dem)



