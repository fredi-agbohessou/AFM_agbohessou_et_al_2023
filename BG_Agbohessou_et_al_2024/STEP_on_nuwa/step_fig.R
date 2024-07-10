# plot outputs
# yelognisse agbohessou 
# 22/02/2021

#- select and plot some variables of interest 

Sys.setenv(TZ='GMT') #To set environment variable 'TZ'

#load the required packages
library("readxl")
library(lubridate)
library(dygraphs)
library(plyr) 
library(ggplot2)                                                                                          
library(gridExtra)                                                                                        
library(tidyr)                                                                                            
library(dplyr) 
library(openxlsx)

Output1 <- read_excel("Output.xlsx")#import new output file written in out_to_xlsx2D.R
new_data <- Output1
attach(new_data)

# Shum_1_per
p1 =  ggplot(data=new_data, aes(x = ymd(Date), y = Shum_1_per)) + 
  geom_line()+ ylab("Soil moisture [2-30cm] (%)")+ xlab("Date")
  
# ETR
p2 = ggplot(data=new_data, aes(x = ymd(Date), y = Etr)) + 
   geom_line()+
   ylab("Actual evapotranspiration (mm)")+
   xlab("Date")


# Reco
p2_1 = ggplot(data=new_data, aes(x = ymd(Date), y = Reco)) + 
  geom_line()+
  ylab("Ecosystem respiration (gC/m2)")+
  xlab("Date")

# BMv_t_ha
p2_2 = ggplot(data=new_data, aes(x = ymd(Date), y = BMv_t_ha)) + 
  geom_line()+
  ylab("Green + Dry biomass (tMS/ha)")+
  xlab("Date")

# BMt
#p2_3 = ggplot(data=new_data, aes(x = ymd(Date), y = BMt)) + 
#  geom_line()+
#  ylab("Total biomass (gMS/m2)")+
#  xlab("Date")


# subsetting
Output2 <- Output1[, c("Date","Rain","BMs","BMv","Bmlita","TqCs",
                       "Total_N","Amonia","CO2Soil","vcfv","vcfs","vcfl","vcft",
                       "NOFLUX_(kgN/ha/an)","NO2","N2O","En2o_NOE","Shum_0_per",
                       "Shum_1_per")]

# TqcS
p3 = ggplot(data=Output2, aes(x = ymd(Date), y = TqCs)) + 
  geom_line()+ ylab("Total soil carbon (gC)")+  xlab("Date")

# CO2
p4 = ggplot(data=Output2, aes(x = ymd(Date), y = CO2Soil)) + 
  geom_line()+ ylab("Microbial Respiration (gC/m2)")+xlab("Date")

# total N
p5 = ggplot(data=Output2, aes(x = ymd(Date), y = Total_N)) + 
  geom_line()+  ylab("Total N (gN) ")+  xlab("Date")

# N2O
p6 = ggplot(data=Output2, aes(x = ymd(Date), y = En2o_NOE)) + 
  geom_line() + ylab("Total N2O emission by soils (ngN/m2/s)")+
  xlab("Date")

# ground cover rate
df3 <- new_data %>%
  select(Date,vcfv,vcfs,vcfl,vcft) %>%
  gather(key = "variable", value = "value", -Date)
p7 = ggplot(df3, aes(x = ymd(Date), y=value)) + 
  geom_line(aes(color = variable), size = 0.5)+
  scale_color_manual(labels = c( "litter","dry biomass","total biomass","green biomass"),
                     values = c("black","Peru","dark grey","green")) +
  ylab("Ground cover rate")+xlab("Date")+
  theme(legend.position = "bottom")

# biomass

df3 <- new_data %>%
  select(Date,BMv,BMs,Bmlita) %>%
  gather(key = "variable", value = "value", -Date)
p8 = ggplot(df3, aes(x = ymd(Date), y=value)) + 
  geom_line(aes(color = variable), size = 0.5)+
  scale_color_manual(labels = c( "litter","dry biomass","green biomass"),
                     values = c("black","Peru","green")) +
  ylab("Biomass (gMs/m2)")+xlab("Date")+
  theme(legend.position = "bottom")

# subsetting
df_in=new_data[, c("Date", "CO2Soil","En2o_NOE","NOFLUX_(kgN/ha/an)")]
attach(df_in)

# rename column where names is "Date"
names(df_in)[names(df_in) == "Date"] <- "date"
names(df_in)[names(df_in) == "CO2Soil"] <- "CO2_gC"
names(df_in)[names(df_in) == "En2o_NOE"] <- "N2O_flux_ngN.m2.s"
names(df_in)[names(df_in) == "NOFLUX_(kgN/ha/an)"] <- "NO_flux_kgN.ha.an"
df_in$date <- as.Date(df_in$date)

df_in0=df_in
dates <- as.Date(df_in0$date, "%d-%b-%y")

df_in0$years <- format(df_in$date, "%Y")
sumYear1 <- ddply(df_in0, .(years), summarize,
                  CO2Soil = mean(CO2Soil),
                  N2O_flux_ngN.m2.s = mean(N2O_flux_ngN.m2.s),
                  NO_flux_kgN.ha.an = mean(NO_flux_kgN.ha.an))

sumYear2 <- ddply(df_in0, .(years), summarize,
                  Sum_CO2Soil = sum(CO2Soil),
                  Sum_N2O_flux_ngN.m2.s = sum(N2O_flux_ngN.m2.s),
                  Sum_NO_flux_kgN.ha.an = sum(NO_flux_kgN.ha.an))

sumYear2<- sumYear2[,c(-1)]
sumYear <- cbind(sumYear1,sumYear2)


# barplot
sumYear=sumYear %>%
  mutate_if(is.numeric,round,digits=2)
attach(sumYear)


# CO2

# annual sum
p10 <- ggplot(data=sumYear, aes(x=years, y=Sum_CO2Soil)) +
  geom_bar(stat="identity", width=0.5,fill ="bisque4")+
  geom_text(aes(label=Sum_CO2Soil), vjust=1.6, color="black",
            position = position_dodge(0.9), size=2)+
  ylab(expression(paste("Sum CO"[2]* " (gC ", m^{-2},yr^{-1},")")))
  


# N2O

# annual sum
p12 <- ggplot(data=sumYear, aes(x=years, y=Sum_N2O_flux_ngN.m2.s)) +
  geom_bar(stat="identity", width=0.5,fill ="bisque4")+
  geom_text(aes(label=Sum_N2O_flux_ngN.m2.s), vjust=1.6, color="black",
            position = position_dodge(0.9), size=2)+
  ylab(expression(paste("Sum N"[2]*"0 (ngN ", m^{-2},yr^{-1},")")))



pdf("plot.pdf")
print(p1)
print(p2)
print(p2_1)
print(p2_2)
#print(p2_3)
print(p3)
print(p4)
print(p5)
print(p6)
print(p7)
print(p8)
print(p10)
print(p12)
dev.off()
