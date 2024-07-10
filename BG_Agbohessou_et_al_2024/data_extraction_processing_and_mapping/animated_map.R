# Mapping STEP outputs
# y. agbohessou
# 05/09/2021

rm(list=ls()) #cleans every object
# col.reg = ((brewer.pal(10,"BrBG"))) biomass

# Import functions
Sys.setenv(TZ="UTC")

# sf pkg to add
#install.packages("gganimate")
require(animation)
require(rgdal)
require(raster)
library("gifski")
library("gganimate")

#-- load needed packages
for(pkg in c("raster","sp","gstat","readr","terra","rasterVis","png","patchwork",
             "fields","interp","mgcv","automap", "patchwork", "viridis","RColorBrewer",
             "tidyr","rstep",'maptools',"tidyverse","ggplot2","dplyr","plyr"
)){
  library(pkg, character.only = TRUE)
}

source("1-code/0_function2D.R")

new_map = normalizePath("3-spatialisation/map/new_map/", winslash = "/")
old_map= normalizePath("3-spatialisation/map/old_map/", winslash = "/")
consen = rgdal::readOGR("3-spatialisation/save/sahel_cassecs.shp",verbose = FALSE)

# set color #choose color here---- https://bookdown.org/hneth/ds4psy/D-3-apx-colors-basics.html
biomass_col <- c("antiquewhite","sandybrown","yellow2","greenyellow","springgreen2","springgreen4")
gas_col  <- c("#ddf1da", "#abdda4", "#e6f598","#fee08b", "#fdae61","#f46d43","#d53e4f")

# Load landcover data
LC = read.csv("0-data/sol/preprocessed/sahel_soil_data_final3.csv")

# N2O_total_kg_ha-----------------------------------------------------------------------------

s_N2O=raster()# create an empty raster
for (i in 2010:2021){
  df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_N2O_total_kg_ha_",i,".csv"))
  df0 = df[,c("lon","lat", "month","varname")]
  df.long = pivot_wider(df0,names_from = month,values_from = varname)
  
    for (j in unique(df$month)){
    raster <- rasterFromXYZ(df.long[,c("lon","lat",j)])
    data_raster_crop = crop(raster, extent(consen))  #crop the raster
    data_raster_mask_sen = raster::mask(data_raster_crop,consen) #Mask rasters using senegal polygon
    #assign(paste0("raster_",i),raster)
    data_raster_mask_sen[is.na(data_raster_mask_sen[])] <- 0 
    s_N2O = stack(s_N2O,data_raster_mask_sen)
  }
}

# plots
a = anim_map_sahel(stack = s_N2O,
                  shape = consen,
                  title = "N2O emissions at month ",
                  var = expression(atop("N"[2]*"O flux", paste(" (kg N"[2]*"O-N ha"^"-1","yr"^"-1",")"))),
                  col.reg =gas_col,
                  label = T)
t=animate(a, duration = 20, height = 6, width = 17,
          units = "in", res = 150)    

anim_save(paste0(new_map,"/5_N2O_flux.gif"),t)

# GHG_t_CO2_equiv_ha----------------------------------------------------------------------------
s_GHG=raster()# create an empty raster
for (i in 2010:2021){
  df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_GHG_t_CO2_equiv_ha_",i,".csv"))#*298 au lieu de 265
  df0 = df[,c("lon","lat", "month","varname")]
  df.long = pivot_wider(df0,names_from = month,values_from = varname)
  for (j in unique(df$month)){
    raster <- rasterFromXYZ(df.long[,c("lon","lat",j)])
    data_raster_crop = crop(raster, extent(consen))  #crop the raster
    data_raster_mask_sen = raster::mask(data_raster_crop,consen) #Mask rasters using senegal polygon
    #assign(paste0("raster_",i),raster)
    data_raster_mask_sen[is.na(data_raster_mask_sen[])] <- 0 
    s_GHG = stack(s_GHG,data_raster_mask_sen)
  }
}

# plots
a= anim_map_sahel(stack = s_GHG,
                  shape = consen,
                  title = "GHG emissions at month ",
                  var = expression(atop("CO"[2]*" & N"[2]*" O", paste(" (t CO"[2]*"eq ha"^"-1","yr"^"-1",")"))),
                  col.reg =gas_col,
                  label = T)
t=animate(a, duration = 40, height = 6, width = 17,
          units = "in", res = 150)    
anim_save(paste0(new_map,"/6_GHG_flux.gif"),t)


