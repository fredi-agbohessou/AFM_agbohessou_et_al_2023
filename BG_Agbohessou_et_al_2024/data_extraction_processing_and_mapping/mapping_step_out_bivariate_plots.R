# Mapping STEP outputs: bivariate plots
# yelognisse agbohessou
# 05/09/2021

rm(list=ls()) #cleans every object
Sys.setenv(TZ="UTC")

#-- load needed packages
for(pkg in c("raster","sp","gstat","readr","terra","rasterVis","png","data.table",
             "fields","automap", "patchwork", "viridis","RColorBrewer","classInt",
             'maptools',"tidyverse","ggplot2","extrafont")){ #"dplyr",,"plyr"
  library(pkg, character.only = TRUE)
}
# plot 

# Import functions
source("1-code/0_function2D.R")

new_map = normalizePath("3-spatialisation/map/new_map/", winslash = "/")
old_map= normalizePath("3-spatialisation/map/old_map/", winslash = "/")
consen = rgdal::readOGR("3-spatialisation/save/sahel_cassecs.shp",verbose = FALSE)

# Load landcover data
LC = read.csv("0-data/sol/preprocessed/sahel_soil_data_final3.csv")


# Plot N2O----------------------------------------
s_N2O=raster()# create an empty raster
for (i in 2012:2022){
  df_co2=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_sum_SoilResp_gC_m2_",i,".csv"))
  df2_co2<- aggregate(varname ~ year+id+lon+lat, df_co2, "sum", na.rm=TRUE)
  df2_co2<-df2_co2[-5]
  
  df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_N2O_total_kg_ha_",i,".csv"))
  df2<- aggregate(varname ~ year+id+lon+lat, df, "sum", na.rm=TRUE)  
  
  df22 <- merge(df2_co2,df2,by="id",all=TRUE)
  df22<-df22[,c("id","year.x","lon.x","lat.x","varname")]
  names(df22) <-c("id","year","lon","lat","varname")
  df22$varname<-replace(df22$varname,is.na(df22$varname),0)
  
  df2 = merge(df22,LC,by="id",all=TRUE)
  
  df2 = df2[complete.cases(df2),]    
  #df2 = subset(df2,df2$glc_grass_shrubs_px_per>=t)
  df_new = df2[,c("lon.x","lat.x","varname")]
  names(df_new)=c("lon","lat",paste0("Nitrous_oxide_emissions_",i))
  assign(paste0("df_",i),df_new)
  raster <- rasterFromXYZ(df_new[,c("lon","lat",paste0("Nitrous_oxide_emissions_",i))])
  data_raster_crop = crop(raster, extent(consen))
  data_raster_mask_sen = raster::mask(data_raster_crop,consen)
  s_N2O = stack(s_N2O,data_raster_mask_sen)
}
s_N2O$Nitrous_oxide_emissions_Mean = calc(s_N2O, mean, na.rm=T)
plot(s_N2O$Nitrous_oxide_emissions_Mean)


LC2=LC[,c("lon","lat","glc_grass_shrubs_px_per")]
raster_LC2 <- rasterFromXYZ(LC2)
data_raster_crop_LC2 = crop(raster_LC2, extent(consen))
raster_LC = raster::mask(data_raster_crop_LC2,consen)
raster_N2O = s_N2O$Nitrous_oxide_emissions_Mean 
rstack_N2O=stack(raster_LC,raster_N2O)
names(rstack_N2O) <- c("SPS_pixel","Nitrous_oxide_emissions")

a=round(maxValue(rstack_N2O$Nitrous_oxide_emissions),1)

map_N2O=bivariate_map_sh(raster_x = rstack_N2O$Nitrous_oxide_emissions,
                       raster_y=rstack_N2O$SPS_pixel,
                       xlab_elem=round(seq(0, a,a/4),1),
                       xlab_legend=expression(atop(paste("Soil N"[2]*"O"),paste(" (kg N-N"[2]*"O ha"^"-2","yr"^"-1",")"))),
                       ylab_legend="% of SPS pixel",
                       title = expression("Soil N"[2]*"O emissions"),
                       subtitle="Mean over 2012-2022: all pixels"# "mean over 2010-2021 (pixels dominated by SPS (>80%) only)"
                       )

map_N2O[[1]]

# ############
plot_all1= (map_N2O[[1]]/tt) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  plot_layout(heights = c(1.2,1))

png(paste0(new_map,"/LC_new/All_plot_N2O.png"),  width=12, height=6.5, units='in',res = 300)
print(plot_all1)
dev.off()
############


#map CO2
s_CO2S=raster()# create an empty raster
for (i in 2012:2022){
  df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_sum_SoilResp_gC_m2_",i,".csv"))
  df2<- aggregate(varname ~ year+id+lon+lat, df, "sum", na.rm=TRUE)
  df2 = merge(df2,LC,by="id",all=TRUE)
  df2 = df2[complete.cases(df2),]
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

s_CO2S$Soil_Respiration_Mean = calc(s_CO2S, mean,na.rm=T)
raster_CO2= s_CO2S$Soil_Respiration_Mean
rstack_CO2=stack(raster_LC,raster_CO2)
names(rstack_CO2) <- c("SPS_pixel","CO2_emissions")

a=round(maxValue(rstack_CO2$CO2_emissions),1)

map_CO2=bivariate_map_sh(raster_x = rstack_CO2$CO2_emissions,
                         raster_y=rstack_CO2$SPS_pixel,
                         xlab_elem=round(seq(0, a,a/4),1),
                         xlab_legend=expression(atop("Soil CO"[2]*" emissions", paste(" (t CO"[2]*"-C ha"^"-1","yr"^"-1",")"))),
                         ylab_legend="% of SPS pixel",
                         title = expression("Soil CO"[2]*" emissions"),
                         subtitle = "Mean over 2012-2022: all pixels"
)

map_CO2[[1]]
############
plot_all2= (map_CO2[[1]]/tt_CO2S) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+plot_layout(heights = c(1.2,1))
png(paste0(new_map,"/LC_new/All_plot_CO2.png"),  width=12, height=6.5, units='in',res = 300)
print(plot_all2)
dev.off()
############

#####map herbaceous biomass------------
s_BMv=raster()# create an empty raster
for (i in 2012:2022){
  df=read.csv(paste0("3-spatialisation/sum_all_sites/new_simu/monthly_max_BMv_t_ha_",i,".csv"))
  df2<- aggregate(varname ~ year+id+lon+lat, df, "max", na.rm=TRUE)
  df2 = merge(df2,LC,by="id",all=TRUE)
  df2 = df2[complete.cases(df2),]
  df_new = df2[,c("lon.x","lat.x","varname")]
  names(df_new)=c("lon","lat",paste0("Max_Herbaceous_mass_",i))
  assign(paste0("df_",i),df_new)
  raster <- rasterFromXYZ(df_new[,c("lon","lat",paste0("Max_Herbaceous_mass_",i))])
  data_raster_crop = crop(raster, extent(consen))
  data_raster_mask_sen = raster::mask(data_raster_crop,consen)
  s_BMv = stack(s_BMv,data_raster_mask_sen)
}

s_BMv$Herbaceous_mass_Mean = calc(s_BMv, mean,na.rm=T)
raster_BMv= s_BMv$Herbaceous_mass_Mean
rstack_BMv=stack(raster_LC,raster_BMv)
names(rstack_BMv) <- c("SPS_pixel","Herbaceous_mass")

a=round(maxValue(rstack_BMv$Herbaceous_mass),1)
map_BMv=bivariate_map_sh(raster_x = rstack_BMv$Herbaceous_mass,
                         raster_y=rstack_BMv$SPS_pixel,
                         xlab_elem=round(seq(0, a,a/4),1),
                         xlab_legend=expression(atop("Aboveground herbaceous mass", paste(" (t DM ha"^"-1","yr"^"-1",")"))),
                         ylab_legend="% of SPS pixel",
                         title = expression("Aboveground herbaceous mass"),
                         subtitle = "Mean over 2012-2022: all pixels"
)

map_BMv[[1]]

############
plot_all3= (map_BMv[[1]]/tt_BMv) + theme(plot.margin = unit(c(0,0,0,0), "cm"))+plot_layout(heights = c(1.2,1))
png(paste0(new_map,"/LC_new/All_plot_biomass.png"),  width=12, height=6.5, units='in',res = 300)
print(plot_all3)
dev.off()
############



plot_all4= (map_BMv[[1]]/map_CO2[[1]]/map_N2O[[1]]) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
png(paste0(new_map,"/LC_new/Bivariate_map_CO2_N2O.png"),  width=12, height=7.2, units='in',res = 300)
print(plot_all4)
dev.off()


plot_all5= (tt_BMv2/tt_CO2S_sd/tt2) + theme(plot.margin = unit(c(0,0,0,0), "cm"))
png(paste0(new_map,"/LC_new/all_sd.png"),  width=12, height=9.5, units='in',res = 300)
print(plot_all5)
dev.off()

plot_all6= (map_CO2[[1]]/tt_CO2S/map_N2O[[1]]/tt) +theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  plot_layout(heights = c(1.2,1,1.2,1))
png(paste0(new_map,"/LC_new/All_plot_CO2_N2O.png"),  width=12, height=13, units='in',res = 300)
print(plot_all6)
dev.off()


pltt_n2o <- (map_N2O[[2]]+map_N2O[[3]]+plot_N2O_lat)+ plot_layout(widths = c(8,1,1))
png(paste0(new_map,"/LC_new/pp0.png"),  width=12, height=2.7, units='in',res = 300)
print(pltt_n2o)
dev.off()
pltt_co2<- (map_CO2[[2]]+map_CO2[[3]]+plot_co2_lat)+ plot_layout(widths = c(8,1,1))
pltt_BMv<- (map_BMv[[2]]+map_BMv[[3]]+plot_BMv_lat)+ plot_layout(widths = c(8,1,1))

pltt<- (pltt_BMv/pltt_co2/pltt_n2o)

png(paste0(new_map,"/LC_new/Bivariate_map_CO2_N2O_2.png"),  width=12, height=2.7*3, units='in',res = 300)
print(pltt)
dev.off()

