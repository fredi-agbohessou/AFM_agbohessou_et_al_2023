# Mapping 2D STEP-GENDEC-N2O outputs 
# y. agbohessou
# 05/09/2021

rm(list=ls()) #cleans every object
Sys.setenv(TZ="UTC")

#-- load needed packages
for(pkg in c("raster","sp","gstat","readr","terra","rasterVis","png","patchwork","scico","viridisLite",
             "fields","interp","mgcv","automap", "patchwork", "viridis","RColorBrewer",
             "tidyr","rstep",'maptools',"tidyverse","ggplot2","dplyr","plyr","extrafont")){
  library(pkg, character.only = TRUE)
}
# Import functions
source("1-code/0_function2D.R")

new_map = normalizePath("3-spatialisation/map/new_map/", winslash = "/")
old_map= normalizePath("3-spatialisation/map/old_map/", winslash = "/")
consen = rgdal::readOGR("3-spatialisation/save/sahel_cassecs.shp",verbose = FALSE)

biomass_col <- viridis(n=20,direction = -1)
gas_col2  <-  viridis(n=20,direction = -1)

# Load landcover data
LC = read.csv("0-data/sol/preprocessed/sahel_soil_data_final3.csv")

per = c(0,80)
for (t in unique(per)){
  
  #### ### #### ### #### ### #### ####### ####### ####### ###
  #### ### Soil N2O emissions #### ####### ####### ####### ###
  #### ### #### ### #### ### #### ####### ####### ####### ###
  
  if(t==80){sbt = ": SPS pixels only"}
  if(t==0){sbt= ": all pixels"}
  
  s_N2O=raster()# create an empty raster
  for (i in 2012:2022){
    df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_N2O_total_kg_ha_",i,".csv"))
    df2<- aggregate(varname ~ year+id+lon+lat, df, "sum", na.rm=TRUE)    
    df2 = merge(df2,LC,by="id",all=TRUE)
    df2 = df2[complete.cases(df2),]    
    df2 = subset(df2,df2$glc_grass_shrubs_px_per>=t)
    #df2$varname= (df2$glc_grass_shrubs_px_per*df2$varname)/100    
    df_new = df2[,c("lon.x","lat.x","varname")]
    names(df_new)=c("lon","lat",paste0("Nitrous_oxide_emissions_",i))
    assign(paste0("df_",i),df_new)
    raster <- rasterFromXYZ(df_new[,c("lon","lat",paste0("Nitrous_oxide_emissions_",i))])
    data_raster_crop = crop(raster, extent(consen))
    data_raster_mask_sen = raster::mask(data_raster_crop,consen)
    s_N2O = stack(s_N2O,data_raster_mask_sen)
  }
  s_N2O_2 = s_N2O
  
  # plots all sites
  plt_N2O_total_kg_ha = multi_map_sahel(data = s_N2O_2, # s_N2O_2
                                        shape = consen,
                                        var = expression(atop("N"[2]*"O flux", paste(" (kg N"[2]*"O-N ha"^"-1","yr"^"-1",")"))),
                                        col.reg =gas_col2,
                                        label = T)
  png(paste0(new_map,"/LC_new/LC",t,"_N2O_flux.png"),  width=17, height=12, units='in',res = 300)
  print(plt_N2O_total_kg_ha)
  dev.off()
  
  s_N2O$Nitrous_oxide_emissions_Mean = calc(s_N2O, mean, na.rm=T)
  # plot_mean
  s_N2O_2=raster()
  s_N2O_2$Nitrous_oxide_emissions_mean_2010_2021 =s_N2O$Nitrous_oxide_emissions_Mean
  data2 = as.data.frame(as(s_N2O_2$Nitrous_oxide_emissions_mean_2010_2021, "SpatialPixelsDataFrame")) 
  names(data2)
  data3 = data2[,c(2,3,1)]
  
  plt_N2O= map_sahel(data = data3,
                     shape = consen,
                     title = NULL,#expression("Soil N"[2]*"O emissions (mean over 2010-2021)"),
                     subtitle = paste0("Mean over 2012-2022",sbt),
                     var = expression(atop("Soil N"[2]*"O emissions", paste(" (kg N"[2]*"O-N ha"^"-1","yr"^"-1",")"))),
                     col.reg =gas_col2,
                     label = T)
  plt_N2O
  #sd
  s_N2O_2 = s_N2O
  s_N2O_2$Nitrous_oxide_emissions_sd = calc(s_N2O_2, sd, na.rm=T)
  data4 = as.data.frame(as(s_N2O_2$Nitrous_oxide_emissions_sd, "SpatialPixelsDataFrame")) 
  data5 = data4[,c(2,3,1)]
  plt_N2O_sd= map_sahel(data = data5,
                        shape = consen,
                        subtitle = "Standard deviation over 2012-2022",
                        title = NULL,
                        var = expression(atop("Soil N"[2]*"O emissions", paste(" (kg N"[2]*"O-N ha"^"-1","yr"^"-1",")"))),
                        col.reg =gas_col2,
                        label = T)
  
  # gradiant plot latitudinal distribution patterns-------------
  data = ddply(data2,.(y),summarize,
               Mean=mean(Nitrous_oxide_emissions_mean_2010_2021,na.rm=NA),
               SD=sd(Nitrous_oxide_emissions_mean_2010_2021))
  plot_N2O_lat = ggplot(data,aes(x=y,y=Mean))+
    scale_y_continuous(expand = c(0.01, 0.01), 
                       breaks=seq(0,2,0.4))+
    scale_x_continuous(breaks=seq(13,18,1),expand = c(0.01, 0.01),limits = c(13,18),
                       labels = paste0(seq(13, 18, 1), "°"),
                       position = "top", sec.axis = sec_axis(~., labels = NULL))+
    ylab(expression(atop(paste("Soil N"[2]*"O"),paste(" (kg N-N"[2]*"O ha"^"-2","yr"^"-1",")"))))+
    xlab("")+
    coord_flip()+ theme_void()
  tt= (plt_N2O+plot_N2O_lat) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
    plot_layout(widths = c(8,1))
  assign(paste0("tt_N2O_",t),tt)
  
  # plot lat sd
  data_sd = ddply(data4,.(y),summarize,
                  Mean=mean(Nitrous_oxide_emissions_sd,na.rm=NA),
                  SD=sd(Nitrous_oxide_emissions_sd))
  plot_N2O_lat_sd = ggplot(data_sd,aes(x=y,y=Mean))+
    scale_y_continuous(expand = c(0.01, 0.01), 
                       breaks=seq(0,2,0.4))+
    scale_x_continuous(breaks=seq(13,18,1),expand = c(0.01, 0.01),limits = c(13,18),
                       labels = paste0(seq(13, 18, 1), "°"),
                       position = "top", sec.axis = sec_axis(~., labels = NULL))+
    ylab(expression(atop(paste("Soil N"[2]*"O"),paste(" (kg N-N"[2]*"O ha"^"-2","yr"^"-1",")"))))+
    xlab("")+
    coord_flip()+ theme_void()
  tt2= (plt_N2O_sd+plot_N2O_lat_sd) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
    plot_layout(widths = c(8,1))
  
  tt3= (tt/tt2) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
  png(paste0(new_map,"/LC_new/LC",t,"_N2O_flux_mean.png"),  width=12, height=7.2, units='in',res = 300)
  print(tt3)
  dev.off()
  
  
  #### ### #### ### #### ### #### ####### ####### ####### ###
  #### ### Herbaceous biomass #### ####### ####### ####### ###
  #### ### #### ### #### ### #### ####### ####### ####### ###
  
  s_BMv=raster()# create an empty raster
  for (i in 2012:2022){
    df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_max_BMv_t_ha_",i,".csv"))
    df2<- aggregate(varname ~ year+id+lon+lat, df, "max", na.rm=TRUE)
    df2 = merge(df2,LC,by="id",all=TRUE)
    df2 = df2[complete.cases(df2),]
    df2 = subset(df2,df2$glc_grass_shrubs_px_per>=t)
    df_new = df2[,c("lon.x","lat.x","varname")]
    names(df_new)=c("lon","lat",paste0("Max_Herbaceous_Biomass_",i))
    assign(paste0("df_",i),df_new)
    raster <- rasterFromXYZ(df_new[,c("lon","lat",paste0("Max_Herbaceous_Biomass_",i))])
    data_raster_crop = crop(raster, extent(consen))
    data_raster_mask_sen = raster::mask(data_raster_crop,consen)
    s_BMv = stack(s_BMv,data_raster_mask_sen)
  }
  
  # Plot all sites
  plt_Green_Biomass = multi_map_sahel(data = s_BMv,
                                      shape = consen,
                                      var = expression(atop("Herbaceous biomass", paste(" (t DM ha"^"-1","yr"^"-1",")"))),
                                      col.reg =biomass_col,
                                      label = T)
  png(paste0(new_map,"/LC_new/LC",t,"_Green_biomass.png"),  width=17, height=12, units='in',res = 300)
  print(plt_Green_Biomass)
  dev.off()
  
  s_BMv$Herbaceous_Biomass_Mean = calc(s_BMv, mean,na.rm=T)
  s_BMv$Herbaceous_Biomass_sd = calc(s_BMv, sd,na.rm=T)
  # plot mean
  s_BMv_2 = raster()
  s_BMv_2$Herbaceous_Biomass_mean_2010_2021 = s_BMv$Herbaceous_Biomass_Mean
  data2 = as.data.frame(as(s_BMv_2$Herbaceous_Biomass_mean_2010_2021, "SpatialPixelsDataFrame")) 
  data3 = data2[,c(2,3,1)]
  plt_BMv= map_sahel(data = data3,
                     shape = consen,
                     title = NULL,
                     subtitle = paste0("Mean over 2012-2022",sbt), 
                     var = expression(atop("Herbaceous biomass", paste(" (t DM ha"^"-1","yr"^"-1",")"))),
                     col.reg =biomass_col,
                     label = T)
  
  data4 = as.data.frame(as(s_BMv$Herbaceous_Biomass_sd, "SpatialPixelsDataFrame")) 
  data5 = data4[,c(2,3,1)]
  plt_BMv_sd= map_sahel(data = data5,
                        shape = consen,
                        title = NULL,
                        subtitle = "Standard deviation over 2012-2022", 
                        var = expression(atop("Herbaceous biomass", paste(" (t DM ha"^"-1","yr"^"-1",")"))),
                        col.reg =biomass_col,
                        label = T)
  # gradiant plot
  # latitudinal distribution patterns-------------
  data = ddply(data2,.(y),summarize,
               Mean=mean(Herbaceous_Biomass_mean_2010_2021,na.rm=NA),
               SD=sd(Herbaceous_Biomass_mean_2010_2021))
  plot_BMv_lat = ggplot(data,aes(x=y,y=Mean))+
    scale_y_continuous(expand = c(0.01, 0.01)
    )+
    scale_x_continuous(breaks=seq(13,18,1),expand = c(0.01, 0.01),limits = c(13,18),
                       labels = paste0(seq(13, 18, 1), "°"),
                       position = "top", sec.axis = sec_axis(~., labels = NULL))+
    xlab("")+
    ylab(expression(atop("Herbaceous biomass", paste(" (t DM ha"^"-1","yr"^"-1",")"))))+
    coord_flip()+theme_void()
    #theme_lat
  tt_BMv= (plt_BMv+plot_BMv_lat) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
    plot_layout(widths = c(8,1))
  assign(paste0("tt_BMv_",t),tt_BMv)
  
  # plot lat sd 
  data_sd = ddply(data4,.(y),summarize,
                  Mean=mean(Herbaceous_Biomass_sd,na.rm=NA),
                  SD=sd(Herbaceous_Biomass_sd))
  plot_BMv_lat_sd = ggplot(data,aes(x=y,y=Mean))+
    scale_y_continuous(expand = c(0.01, 0.01)
    )+
    scale_x_continuous(breaks=seq(13,18,1),expand = c(0.01, 0.01),limits = c(13,18),
                       labels = paste0(seq(13, 18, 1), "°"),
                       position = "top", sec.axis = sec_axis(~., labels = NULL))+
    xlab("")+
    ylab(expression(atop("Herbaceous biomass", paste(" (t DM ha"^"-1","yr"^"-1",")"))))+
    coord_flip()+ theme_void()
    #theme_lat
  
  tt_BMv2= (plt_BMv_sd+plot_BMv_lat_sd) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
    plot_layout(widths = c(8,1))
  tt_BMv3= (tt_BMv/tt_BMv2) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
  png(paste0(new_map,"/LC_new/LC",t,"_Green_biomass_mean.png"),  width=12, height=7.2, units='in',res = 300)
  print(tt_BMv3)
  dev.off()
  
  
  #### ### #### ### #### ### #### ####### ####### ####### ###
  #### ### Soil CO2 emissions #### ####### ####### ####### ###
  #### ### #### ### #### ### #### ####### ####### ####### ###
  
  s_CO2S=raster()# create an empty raster
  for (i in 2012:2022){
    df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_sum_SoilResp_gC_m2_",i,".csv"))
    df2<- aggregate(varname ~ year+id+lon+lat, df, "sum", na.rm=TRUE)
    df2 = merge(df2,LC,by="id",all=TRUE)
    df2 = df2[complete.cases(df2),]
    df2 = subset(df2,df2$glc_grass_shrubs_px_per>=t)
    df_new = df2[,c("lon.x","lat.x","varname")]
    df_new$varname = replace(df_new$varname,df_new$varname<0,0)
    df_new$varname = df_new$varname*0.01
    names(df_new)=c("lon","lat",paste0("Soil_Respiration_",i))
    assign(paste0("df_",i),df_new)
    raster <- rasterFromXYZ(df_new[,c("lon","lat",paste0("Soil_Respiration_",i))])
    data_raster_crop = crop(raster, extent(consen))
    data_raster_mask_sen = raster::mask(data_raster_crop,consen)
    s_CO2S = stack(s_CO2S,data_raster_mask_sen)
  }
  
  # plot all sites
  plt_Soil_respiration = multi_map_sahel(data = s_CO2S,
                                         shape = consen,
                                         var = expression(atop("Soil CO"[2]*" emissions", paste(" (t CO"[2]*"-C ha"^"-1","yr"^"-1",")"))),
                                         col.reg =gas_col2,
                                         label = T)
  
  png(paste0(new_map,"/LC_new/LC",t,"_Soil_resp_flux.png"),  width=17, height=12, units='in',res = 300)
  print(plt_Soil_respiration)
  dev.off()
  
  s_CO2S$Soil_Respiration_Mean = calc(s_CO2S, mean,na.rm=T)
  s_CO2S$Soil_Respiration_sd = calc(s_CO2S, sd,na.rm=T)
  
  # plot mean
  s_CO2S_2=raster()
  s_CO2S_2$Soil_Respiration_mean_2010_2021 = s_CO2S$Soil_Respiration_Mean
  data2 = as.data.frame(as(s_CO2S_2$Soil_Respiration_mean_2010_2021, "SpatialPixelsDataFrame")) 
  data3 = data2[,c(2,3,1)]
  plt_co2= map_sahel(data = data3,
                     shape = consen,
                     title = NULL,
                     subtitle = paste0("Mean over 2012-2022",sbt),
                     var = expression(atop("Soil CO"[2]*" emissions", paste(" (t CO"[2]*"-C ha"^"-1","yr"^"-1",")"))),
                     col.reg =gas_col2,
                     label = T)
  
  data4 = as.data.frame(as(s_CO2S$Soil_Respiration_sd, "SpatialPixelsDataFrame")) 
  data5 = data4[,c(2,3,1)]
  plt_co2_sd= map_sahel(data = data5,
                        shape = consen,
                        subtitle = "Standard deviation over 2012-2022",
                        title = NULL,
                        var = expression(atop("Soil CO"[2]*" emissions", paste(" (t CO"[2]*"-C ha"^"-1","yr"^"-1",")"))),
                        col.reg =gas_col2,
                        label = T)
  
  # gradiant plot latitudinal distribution patterns-------------
  data = ddply(data2,.(y),summarize,
               Mean=mean(Soil_Respiration_mean_2010_2021,na.rm=NA),
               SD=sd(Soil_Respiration_mean_2010_2021))
  
  plot_co2_lat = ggplot(data,aes(x=y,y=Mean))+
    scale_y_continuous(expand = c(0.01, 0.01)
    )+
    scale_x_continuous(breaks=seq(13,18,1),expand = c(0.01, 0.01),limits = c(13,18),
                       labels = paste0(seq(13, 18, 1), "°"),
                       position = "top", sec.axis = sec_axis(~., labels = NULL))+
    xlab("")+
    ylab(expression(atop("Soil CO"[2]*"", paste(" (t CO"[2]*"-C ha"^"-1","yr"^"-1",")"))))+
    coord_flip()+theme_void()
    #theme_lat
  tt_CO2S= (plt_co2+plot_co2_lat) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
    plot_layout(widths = c(8,1))
  assign(paste0("tt_CO2S_",t),tt_CO2S)
  
  # plot lat sd
  data_sd = ddply(data4,.(y),summarize,
                  Mean=mean(Soil_Respiration_sd,na.rm=NA),
                  SD=sd(Soil_Respiration_sd))
  
  plot_co2_lat_sd = ggplot(data,aes(x=y,y=Mean))+
    scale_y_continuous(expand = c(0.01, 0.01)
    )+
    scale_x_continuous(breaks=seq(13,18,1),expand = c(0.01, 0.01),limits = c(13,18),
                       labels = paste0(seq(13, 18, 1), "°"),
                       position = "top", sec.axis = sec_axis(~., labels = NULL))+
    xlab("")+
    ylab(expression(atop("Soil CO"[2]*"", paste(" (t CO"[2]*"-C ha"^"-1","yr"^"-1",")"))))+
    coord_flip()+theme_void()
    #theme_lat
  tt_CO2S_sd= (plt_co2_sd+plot_co2_lat_sd ) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
    plot_layout(widths = c(8,1))
  
  tt_CO2S_final= (tt_CO2S/tt_CO2S_sd) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
  png(paste0(new_map,"/LC_new/LC",t,"_Soil_resp_flux_mean.png"),  width=12, height=7.2, units='in',res = 300)
  print(tt_CO2S_final)
  dev.off()
  
  #-------------
  tt_CO2S_final= (tt_CO2S/tt) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
  png(paste0(new_map,"/LC_new/LC",t,"_CO2_N2O.png"),  width=12, height=7.2, units='in',res = 300)
  print(tt_CO2S_final)
  dev.off()
  
}



