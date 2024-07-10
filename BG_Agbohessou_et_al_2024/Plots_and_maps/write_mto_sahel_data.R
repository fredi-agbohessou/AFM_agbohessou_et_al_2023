# Processing and writing .mto file for 2D STEP simulation
# y. agbohessou
# 2020-03-20


rm(list=ls())
#load needed packages
for(pkg in c("raster","sp","sf","dplyr","readxl",
             "lubridate","ggplot2","tibble","plyr","gstat",
             "fields", "interp", "mgcv", "automap","chron","rgdal",
             "patchwork","rstep","viridis","ncdf4","lattice",
             "RColorBrewer","terra","rasterVis","raster","tidyr"
)){
  library(pkg, character.only = TRUE)
}

shp = rgdal::readOGR("3-spatialisation/save/sahel_cassecs.shp",verbose = FALSE)
proj4string(shp)<- CRS("+proj=longlat +datum=WGS84")

# import id of each grid point from soil dataset  ------------------
id = read.csv("3-spatialisation/sum_all_sites/site_id_from_soil.csv")
id = id[,c("id","coord")]
#length(unique(id$id))

# create 0.1 deg x 0.1 deg grid for simu (using same bbox as for soil dataset)
bbox = c ("xmin" = -18.0, "ymin" = 13.0,"xmax" = 20.0,"ymax" = 18.0)
grd_template = expand.grid(
  lon = seq(from = bbox["xmin"], to = bbox["xmax"], by = 0.1),
  lat = seq(from = bbox["ymin"], to = bbox["ymax"], by = 0.1)
)

# dataset from era5 ------------------------------------------------------
variables = c("pvap_2_m_hPa","RayGlo_MJm","wind_speed","temp_max","temp_min")
# unit of the variables from era5 : 
# RayGlo_MJm (J m-2 d-1)
# temp_max and min (K)
# pvap (hPa) 
# wind_speed (m s-1)

for (var in unique(variables)){
  for (yr in 2010:2021){
    tp=read.csv(paste0("0-data/climat/data_era5_renamed/Sahel_",var,"_",yr,".csv"))
    tp=tp[-1]
    lonlat = tp[,c("x","y")]
    lonlat=round(lonlat,2)
    dt = tp[,which(names(tp)==paste0("X",yr,".01.01")): which(names(tp)==paste0("X",yr,".12.31"))]
    
    if(var=="temp_max" | var =="temp_min"){
      dt <- dt - 273.15      # convert temperature from K to degC    
    }
    if(var=="RayGlo_MJm"){
      dt <- dt/1000000       # convert radiation from J m-2 d-1 to MJ m-2 d-1   
    }else{
      dt = dt                # right unit for pvap and wind speed
    }
    dt = round(dt,2)
    data=cbind(lonlat,dt)
    
    Date <- seq.Date(as.Date(paste0(yr,"-01-01")), length.out = ncol(data)-2, by = "day", drop=FALSE)
    names(data) <- c("lon","lat",paste0("tp_",Date))
    
    #reshaping the dataset from a matrix to a dataframe
    tp_df03 <- pivot_longer(data, cols=3:ncol(data), names_to = "Date", values_to = "var")
    tp_df03$Date= gsub("[tp_]", "",  tp_df03$Date) 
    data2 = na.omit(tp_df03)
    
    # rename cols
    names(data2) <- c("x","y","date","var")
    
    # standardize coordinates
    data2$x = round(data2$x,2)
    data2$y = round(data2$y,2)
    
    # a dataframe containing grid x and y
    df3 = as.data.frame(grd_template)
    for (i in unique(data2$date)){
      # subsetting (each day)
      df1 = subset(data2,data2$date==i)
      #subsetting cols of interest
      df2 = df1[,c("x","y","var")] # variable
      # rasterize data
      raster <- raster::rasterFromXYZ(df2)
      # extract variable values for each grid point 
      tt = raster::extract(raster,grd_template)
      # bind data to grid x, y dataframe
      df3 = cbind(df3,tt)
    }
    df3=df3[complete.cases(df3),]
    
    #3.1- df3 to df.long  with cols names: "coord" "id" "lon" "lat" "Date" "variable"-----------------
    # add coord col to df3
    df3$lon = round(df3$lon,2)
    df3$lat = round(df3$lat,2)
    coord = paste(df3$lon,"_",df3$lat)
    df4 = cbind(coord,df3)
    
    # rename df3 cols with coresponding dates
    Date <- seq.Date(as.Date(paste0(yr,"-01-01")), length.out = ncol(df4)-3, by = "day", drop=FALSE)
    names(df4) <- c("coord","lon","lat",paste0("tp_",Date))
    
    # add the corresponding id (from soil dataset) to each grid point
    new_df = merge(id,df4,by="coord") 
    # reshape the dataset using tidyr
    df.long <- pivot_longer(new_df, cols=5:ncol(new_df), names_to = "Date", values_to = paste0(var))
    
    # edit Date col
    df.long$Date= gsub("[tp_]", "",  df.long$Date) 
    
    # save df.long
    write.csv(df.long,paste0("0-data/climat/mto_sahel_",var,"_",yr,".csv"))
    
    #3.2- plot annual sum or mean from df.long----------------------------------------------------
    # just for checking
    r2= pivot_wider(df.long,names_from = "Date",values_from = paste0(var))
    tp_df02 = r2
    tp_df02$annual_sum <- apply(tp_df02[which(names(tp_df02)==paste0(yr,"-01-01")):
                                          which(names(tp_df02)==paste0(yr,"-12-31"))],1,mean) # sum  mean
    
    plt = levelplot(tp_df02$annual_sum ~ tp_df02$lon * tp_df02$lat, cuts=6, 
                    col.regions=(rev(brewer.pal(8,"RdBu"))),               #(rev(brewer.pal(10,"RdBu")))
                    main=paste0("era5 ",yr,": ",var),
                    xlab="Longitude",ylab="Latitude")+ 
      latticeExtra::layer(sp.lines(shp, col="black", lwd=0.5))
    
    png(paste0("0-data/climat/mto_sahel_",var,"_",yr,".png"), 
        width=15, height=4,units='in',res = 600)
    print(plt)
    dev.off()
  }
}

# rain dataset from Imerg ------------------
for(yr in 2010:2021){
  nc=read.csv(paste0("0-data/climat/data_era5_renamed/imerg_sahel_rain_",yr,".csv"))  
  data2 = na.omit(nc)
  data2 =data2[,c("lon","lat","time","precipitationCal")]
  
  # rename cols
  names(data2) <- c("x","y","date","var")
  
  # standardize coordinates
  data2$x = round(data2$x,2)
  data2$y = round(data2$y,2)
  
  # a dataframe containing grid x and y
  df3 = as.data.frame(grd_template)
  for (i in unique(data2$date)){
    # subsetting (each day)
    df1 = subset(data2,data2$date==i)
    #subsetting cols of interest
    df2 = df1[,c("x","y","var")] # variable
    # rasterize data
    raster <- raster::rasterFromXYZ(df2)
    # extract variable values for each grid point 
    tt = raster::extract(raster,grd_template)
    # bind data to grid x, y dataframe
    df3 = cbind(df3,tt)
  }
  df3=df3[complete.cases(df3),]
  
  #3.1- df3 to df.long  with cols names: "coord" "id" "lon" "lat" "Date" "rain"-----------------
  # add coord col to df3
  df3$lon = round(df3$lon,2)
  df3$lat = round(df3$lat,2)
  coord = paste(df3$lon,"_",df3$lat)
  df4 = cbind(coord,df3)
  
  # rename df3 cols with coresponding dates
  Date <- seq.Date(as.Date(paste0(yr,"-01-01")), length.out = ncol(df4)-3, by = "day", drop=FALSE)
  names(df4) <- c("coord","lon","lat",paste0("tp_",Date))
  
  # add the corresponding id (from soil dataset) to each grid point
  new_df = merge(id,df4,by="coord") 
  # reshape the dataset using tidyr
  df.long <- pivot_longer(new_df, cols=5:ncol(new_df), names_to = "Date", values_to = "rain")
  
  # edit Date col
  df.long$Date= gsub("[tp_]", "",  df.long$Date) 
  
  # save df.long
  write.csv(df.long,paste0("0-data/climat/mto_sahel_rain_",yr,".csv"))
  
  #3.2- plot annual sum  from df.long----------------------------------------------------
  # just for checking
  r2= pivot_wider(df.long,names_from = "Date",values_from = "rain")
  tp_df02 = r2
  tp_df02$annual_sum <- apply(tp_df02[which(names(tp_df02)==paste0(yr,"-01-01")):
                                        which(names(tp_df02)==paste0(yr,"-12-31"))],1,sum) # sum  mean
  
  plt = levelplot(tp_df02$annual_sum ~ tp_df02$lon * tp_df02$lat, cuts=6, 
                  col.regions=((brewer.pal(8,"RdBu"))),               #(rev(brewer.pal(10,"RdBu")))
                  main=paste0("imerg ",yr,": rain (mm)"),
                  xlab="Longitude",ylab="Latitude")+ 
    latticeExtra::layer(sp.lines(shp, col="black", lwd=0.5))
  
  png(paste0("0-data/climat/mto_sahel_rain_",yr,".png"), 
      width=15, height=4,units='in',res = 600)
  print(plt)
  dev.off()
}


#4- bind all variables together in one dataframe-----------------------------------------------
##c("x","y","date","year","rain","RayGlo_MJm","temp_min","temp_max","pvap_2_m_hPa","Wind_Speed")

variables2 = c("rain","RayGlo_MJm","temp_min","temp_max","pvap_2_m_hPa","Wind_Speed")

for (var2 in unique(variables2)){
  for (yr2 in 2010:2021){
    dff = read.csv(paste0("0-data/climat/mto_sahel_",var2,"_",yr2,".csv"))
    assign(paste0(var2,yr2),dff)
  }
}

rain = rbind(rain2019,rain2020,rain2021)
rain$coord_date = paste0(rain$id,"_",rain$Date)
variables <- c("RayGlo_MJm", "temp_min", "temp_max", "pvap_2_m_hPa", "Wind_Speed")
years <- 2010:2021

# Function to combine multiple data frames row-wise
combine_data <- function(df_list) {
  bind_rows(df_list)
}
# Function to combine multiple data frames column-wise
bind_columns <- function(...) {
  bind_cols(...)
}
# Combine variables data using loops
combined_data <- list()

for (var in variables) {
  var_list <- list()
  for (yr in years) {
    var_list[[yr - min(years) + 1]] <- get(paste0(var, yr))
  }
  combined_data[[var]] <- combine_data(var_list)
}
# Combine all data frames into one
df2 <- bind_columns(
  combined_data[["RayGlo_MJm"]],
  combined_data[["temp_min"]],
  combined_data[["temp_max"]],
  combined_data[["pvap_2_m_hPa"]],
  combined_data[["Wind_Speed"]]
)
df2$coord_date = paste0(df2$id,"_",df2$Date)
df3 = merge(rain,df2,by="coord_date")
data = df3[,c("coord.x","id.y","lon.y","lat.y","Date.y","rain","RayGlo_MJm","temp_min",
              "temp_max","pvap_2_m_hPa","wind_speed")]

data<- add_column(data,date = as.Date(data$Date),.after = 5)
data<- add_column(data,year = format(data$date, "%Y"),.after = 6)

meteo_2010_2021 = data[,c("coord.x","id.y","lon.y","lat.y","date","year","rain","RayGlo_MJm","temp_min",
                          "temp_max","pvap_2_m_hPa","wind_speed")]

names(meteo_2010_2021) <- c("coord","id","lon","lat","date","year","rain","RayGlo_MJm","temp_min",
                            "temp_max","pvap_2_m_hPa","Wind_Speed")

names(meteo_2010_2021)

length(which(is.na(meteo_2010_2021)))

write.csv(meteo_2010_2021,"0-data/climat/preprocessed/meteo_sahel_2010_2021.csv")

#6. Write .mto file in STEP format for each site-----------------------------------------------

# import climate dataset 
meteo_2010_2021 = read.csv("0-data/climat/preprocessed/meteo_sahel_2010_2021.csv")

# in meteo_2019_2021 :
#Rain (mm) from IMERG: https://disc.gsfc.nasa.gov/datasets/GPM_3IMERGDF_06/summary?keywords=%22IMERG%20final%22
#Temperature2m (min and max) Solar radiation, vapor pressure and wind speed from ERA5: https://cds.climate.copernicus.eu/cdsapp#!/dataset/reanalysis-era5-land?tab=overview

sum_data = meteo_2010_2021[-1]

# Dataset quality check ------------------------------------------------------------
#sum_data2 = sum_data[,c("coord", "id","lon","lat","date","rain")]

# plot rain to check if everything is good

#r2= pivot_wider(sum_data2,names_from = "date",values_from = "rain")
#tp_df02 = r2
#tp_df02$annual_sum <- apply(tp_df02[which(names(tp_df02)=="2020-01-01"):
#                                      which(names(tp_df02)=="2020-12-31")],1,sum) # sum  mean
#length(which(is.na(tp_df02))) # 0
#raster <- rasterFromXYZ(tp_df02[,c("lon","lat","annual_sum")])  
#plot(raster,col=(brewer.pal(8,"RdBu")))

# extract id_mto
#nrow(r2)# = 18271
#id_mto =  r2[,c("coord", "id","lon","lat")]
#length(unique(id_mto$id)) # = 18271

# export id 
#write.csv(id_mto,"0-data/climat/preprocessed/id_mto.csv")

# write mto file fo reach grid ------------------------------------------------------------------------
names(sum_data)
length(which(is.na(sum_data)))

# if  length(which(is.na(sum_data))) = 0 then run the loop
sum_data$temp_min = replace(sum_data$temp_min,sum_data$temp_min<5,5)



for (isite in unique(sum_data$id)){
  
  df = sum_data[sum_data$id %in% isite,] # saves to environment
  
  meteo.y <- df[,c("date","year","rain","RayGlo_MJm","temp_min","temp_max","pvap_2_m_hPa","Wind_Speed")]
  
  for (i in 2010:2021){
    
    df = meteo.y[meteo.y$year %in% i,]
    
    df <- add_column(df, day = day(df$date), .after = 1) 
    
    gen_step_mto(workspace = "0-data/step_2d_simu/step2d_workspace/Input_mto/",dataframe=df,isite=isite,
                 year = substr(unique(df$year),3,4),alt=308,lat=15.4,hautmes=2)    
  }
  
}





