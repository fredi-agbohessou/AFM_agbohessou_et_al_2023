# Hovmoller plot: CO2, N2O and Biomass vs coord vs time
# y. agbohessou
# 05/09/2021

rm(list=ls()) #cleans every object

#-- load needed packages
for(pkg in c("tidyr","ggplot2","plyr","dplyr","rstep","lubridate","ggthemes","patchwork","extrafont",
             "RColorBrewer","viridis")){
  library(pkg, character.only = TRUE)
}
# Import functions
source("1-code/0_function2D.R")

# site ID
id=read.csv("1-code/important/site_id.csv")

# Load landcover data
LC = read.csv("0-data/sol/preprocessed/sahel_soil_data_final3.csv")
LC=LC[,c("id","glc_grass_shrubs_px_per")]
id_LC = merge(id,LC,by="id")


# plot soil CO2
plot_hvml = function(df,          # dataframe
                     t,   # t = SPS pixel threshold 
                     var="CO2"    # var = variable
                     ){
  df = df
  data = merge(id_LC,df,by="id")
  data = subset(data,data$glc_grass_shrubs_px_per>=t)
  data=data[,-c(1,2,3,6,7)]
  names(data)=c("lon","lat",paste0(2012:2022),"Mean")
  data = pivot_longer(data,cols=c(paste0(2012:2022)),
                      names_to = "time",values_to = "value")
  data$time = as.Date(paste0(data$time, "-01-01"), format = "%Y-%m-%d")
  data<-ddply(data, .(lat,time),summarize,value=mean(value))

  if(var=="CO2"){
    aa=1
    unit=expression(paste("t CO"[2]*"-C ha"^"-1","yr"^"-1"))
    sub_var=expression("Soil CO"[2]*" emissions")
    col = viridis(n=20,direction = -1)
    }
  
  if(var=="N2O"){
    aa=1
    unit=expression(paste("kg N"[2]*"O-N ha"^"-1","yr"^"-1"))
    sub_var=expression("Soil N"[2]*"O emissions")
    col =  viridis(n=20,direction = -1)
  }
  if(var=="Rain"){
    aa=0
    unit = expression(paste("mm"," yr"^"-1"))
    sub_var = expression("Annual Precipitation")
    col =  viridis(n=20,direction = -1)#((brewer.pal(7,"RdBu")))
  }
  if(var=="BMv"){
    aa=1
    unit = expression(paste("t DM ha"^"-1","yr"^"-1"))
    sub_var = expression("Herbaceous mass")
    col = viridis(n=20,direction = -1)
  }
  
  # Plot the Hovmöller representation
  plt= ggplot(data, aes(x=time, y=lat, fill=value)) +
    geom_tile() +
    scale_x_date(limits=c(as.Date("2011-06-01"), as.Date("2022-08-01")),
                 breaks = "1 year",expand = c(0,-1),date_labels = "%Y") +
    scale_y_continuous(limits=c(11.9, 19),expand = c(0, -1)) +
    scale_fill_gradientn(guide = guide_legend(reverse = FALSE),
                         colors=col,
                         breaks = seq(min(data$value),max(data$value), length.out = 6),
                         labels = seq(min(data$value),max(data$value), length.out = 6) %>% round(aa),
                         name = unit,
                         na.value = "gray82") +
    labs(x="", y="Latitude (\u00B0N)", fill="Value",
         subtitle = sub_var) +
    thm2#+theme(axis.title.x = element_blank(),axis.text.x = element_blank())
  return(plt)
}

# plot CO2
df_CO2=read.csv("3-spatialisation/sum_all_sites/new_simu/annual_SoilResp_tC_ha.csv")
plt_CO2=plot_hvml(df=df_CO2,t=80,var="CO2")+theme(axis.title.x = element_blank(),axis.text.x = element_blank())
plt_CO2
# Plot N2O
df_N2O=read.csv("3-spatialisation/sum_all_sites/new_simu/annual_N2O_total_kg_ha.csv")
plt_N2O=plot_hvml(df=df_N2O,t=80,var="N2O") 

# plot CO2 & N2O
ts = (plt_CO2/plt_N2O)+ theme(plot.margin = unit(c(0.1,0,0,0), "cm"))+ 
  plot_layout(heights = c(1,1))
ts

#Plot BMv
df_BMv=read.csv("3-spatialisation/sum_all_sites/new_simu/annual_max_BMv_t_ha.csv")
plt_BMv=plot_hvml(df=df_BMv,t=80,var="BMv") 
plt_BMv
plt_anom  

# Plot  rain 
df_Rain=read.csv("3-spatialisation/sum_all_sites/new_simu/annual_Rain.csv")
plt_Rain=plot_hvml(df_Rain,t=80,var="Rain") 

ts = ((plt_Rain+theme(plot.tag = element_text(size = 9),
                        plot.tag.position  = c(.13, .95)))/
        (plt_BMv+theme(plot.tag = element_text(size = 9),
                         plot.tag.position  = c(.13, .95)))/
        (plt_anom+theme(plot.tag = element_text(size = 9),
                          plot.tag.position  = c(.13, .95))))+ 
  plot_annotation(tag_levels = "a",tag_prefix = '(',tag_suffix = ')')+ 
  plot_layout(heights = c(1,1,0.8))
ts

png("2-outputs/hvml_plot_BMv_Rain_px_80_per_SPS.png",  width=5, height=6,units='in',res = 300)
ts
dev.off()