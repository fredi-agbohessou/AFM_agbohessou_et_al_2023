# plots in To what extent are greenhouse-gas emissions offset by trees in a Sahelian silvopastoral system? (Agbohessou et al 2023)
# y. agbohessou

#-- load needed packages------------------------------------------------------------------------
# Load necessary packages efficiently
if (!require("pacman")) install.packages("pacman")
pacman::p_load(ggplot2, dplyr, ggthemes, RColorBrewer, lubridate, tidyr, 
               plyr, cowplot, grid, gridExtra, patchwork, extrafont, CroPlotR, data.table)

# Load data
uncert_data <- fread("uncertainty_res_total.csv")
uncert_data[, `:=`(
  per_incert_co2 = (sd_co2 * 100) / mean_co2,
  per_incert_n2o = (sd_n2o * 100) / mean_n2o
)]
uncert_data_tree <- fread("uncertainty_res_total_tree.csv")

# Define themes
reg_thm <- theme_bw() +
  theme(panel.grid.major = element_line(linetype = 5, size = .2),
        panel.grid.minor = element_line(linetype = 0),
        axis.text = element_text(size = 8, colour = "black"),
        axis.title = element_text(size = 8),
        text = element_text(family = "Candara"))

bar_thm <- theme_classic() + 
  theme(panel.grid.major = element_line(linetype = 5, size = .2),
        text = element_text(family = "Candara"),
        panel.border = element_rect(color = "black", fill = NA),
        axis.title.y = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 5.5, color = "black"),
        axis.text.y = element_text(size = 7, color = "black"),
        legend.text = element_text(size = 6.5),
        legend.key.height = unit(3, "mm"),
        legend.background = element_blank(),
        legend.justification = c(0.2, 0.9),
        legend.position = c(0.2, 0.9),
        legend.direction = "vertical",
        axis.title.x = element_blank(),
        legend.box = "vertical",
        legend.margin = margin(-0.8, 0, -0.1, 0, unit = "cm"))

####-----------------------------------------------------------------------------------------
# Read and process FAO data
fao_data <- fread("0-data/FAO_GHG/FAOSTAT_data_en_7-28-2022.csv") %>%
  filter(Element == "Emissions (CO2eq) from N2O (AR5)") %>%
  mutate(value_t_ha = (Value * 1000) / 19672200)

fao_manure <- fao_data %>%
  filter(Item == "Manure left on Pasture") %>%
  group_by(Year) %>%
  summarize(FAO_manu_N2O_total_t_CO2_equiv_ha = sum(value_t_ha)) %>%
  rename(years = Year)

# Calculate N2O emissions
step_data <- df %>%
  mutate(years = format(Date, "%Y")) %>%
  group_by(years) %>%
  summarize(N2O_total_t_CO2_equiv_ha = sum(N2O_total_t_CO2_equiv_ha)) %>%
  mutate(uncert_n2o_t_CO2_equiv_ha = uncert_data$sd_n2o * 1.57 * 265 * 0.001)

# Merge data
data <- merge(fao_manure, step_data, by = "years", all = TRUE) %>%
  select(years, FAO_manu_N2O_total_t_CO2_equiv_ha, N2O_total_t_CO2_equiv_ha, uncert_n2o_t_CO2_equiv_ha) %>%
  pivot_longer(cols = -years, names_to = "variable", values_to = "value") %>%
  mutate(uncert_n2o_t_CO2_equiv_ha = ifelse(variable == "FAO_manu_N2O_total_t_CO2_equiv_ha", NA, uncert_n2o_t_CO2_equiv_ha))

# Plot N2O emissions
bar_N2O_fao_simu <- ggplot(data, aes(x = years, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7, alpha = 0.7) +
  geom_errorbar(aes(ymin = value - uncert_n2o_t_CO2_equiv_ha, ymax = value + uncert_n2o_t_CO2_equiv_ha), 
                size = .2, width = .18, linetype = "solid", position = position_dodge(.9)) +
  scale_fill_manual(name = '', 
                    values = c("FAO_manu_N2O_total_t_CO2_equiv_ha" = "gray70", "N2O_total_t_CO2_equiv_ha" = "gray40"),
                    labels = c("FAO TIER1: Emissions from Manure", "STEP: Emissions from Dahra SPS")) +
  labs(x = "", y = expression(paste("Emissions [CO"[2]*"eq] from N" [2]*"O (t CO"[2]*"eq ha"^"-1"," yr"^"-1",")"))) +
  bar_thm


# Flux per season -----------------
df_in1 <- read_excel("0-data/step_1d_simu/daily_original/SeasonDahra2012-2020.xlsx")
df1 <- df %>%
  mutate(Season = df_in1[, 2],
         years = format(Date, "%Y"))

sumYear2 <- df1 %>%
  group_by(years, Season) %>%
  summarise(N2O_total_kg_ha = sum(N2O_total_kg_ha, na.rm = TRUE))

smy <- sumYear2 %>%
  pivot_wider(names_from = Season, values_from = N2O_total_kg_ha) %>%
  mutate(total = dry_season + wet_season,
         per_wet_season = (wet_season / total) * 100 / 200,
         uncert_n2o_kg_ha = uncert_n2o$uncert_n2o_kg_ha)

df2 <- smy %>%
  select(years, wet_season, dry_season, per_wet_season) %>%
  pivot_longer(cols = -years, names_to = "group", values_to = "N2O")

sumYear <- df2 %>%
  mutate(across(where(is.numeric), round, digits = 3))

bar_N2O_season <- ggplot(data = filter(sumYear, group %in% c("wet_season", "dry_season")), 
                         aes(x = years, y = N2O, fill = group)) +
  geom_bar(stat = "identity", position = "stack", width = 0.4, alpha = 0.7) + 
  scale_fill_manual(name = '',
                    breaks = c("dry_season", "wet_season"),
                    values = c("dry_season" = "gray70", "wet_season" = "gray40"),
                    labels = c("Dry season", "Wet season")) + 
  scale_y_continuous(breaks = seq(0, 0.55, 0.1), expand = c(0, 0), limits = c(0, NA)) +
  labs(x = "", y = expression(paste("N" [2]*"O emissions (kg N ha"^"-1"," yr"^"-1",")"))) +
  bar_thm

# Contribution Nit and Denit ---------------------------------------
df1 <- df %>%
  select(Date, N2O_total_kg_ha, N2Onit_kg_ha, N2Odenit_kg_ha) %>%
  mutate(years = format(Date, "%Y"))

df1 <- df1 %>%
  group_by(years) %>%
  summarise(across(c(N2Odenit_kg_ha, N2Onit_kg_ha, N2O_total_kg_ha), sum, na.rm = TRUE)) %>%
  mutate(per_N2Onit_kg_ha = (N2Onit_kg_ha / N2O_total_kg_ha) * 100 / 200)

df2 <- df1 %>%
  select(years, N2Odenit_kg_ha, N2Onit_kg_ha, per_N2Onit_kg_ha) %>%
  pivot_longer(cols = -years, names_to = "group", values_to = "N2O")

sumYear <- df2 %>%
  mutate(across(where(is.numeric), round, digits = 2))

bar_N2O_nit_denit <- ggplot(data = filter(sumYear, group %in% c("N2Odenit_kg_ha", "N2Onit_kg_ha")), 
                            aes(x = years, y = N2O, fill = group)) +
  geom_bar(stat = "identity", position = "stack", width = 0.4, alpha = 0.7) + 
  scale_fill_manual(name = '',
                    breaks = c("N2Odenit_kg_ha", "N2Onit_kg_ha"),
                    values = c("N2Odenit_kg_ha" = "gray70", "N2Onit_kg_ha" = "gray40"),
                    labels = c("Denitrification", "Nitrification")) + 
  scale_y_continuous(breaks = seq(0, 0.55, 0.1), expand = c(0, 0), limits = c(0, NA)) +
  labs(x = "", y = expression(paste("N" [2]*"O emissions (kg N ha"^"-1"," yr"^"-1",")"))) +
  bar_thm

# Combine plots
bar_N2O_plt <- ((bar_N2O_season + theme(plot.tag.position  = c(.19, .95))) +
                  (bar_N2O_nit_denit + theme(plot.tag.position  = c(.19, .95)))) / 
  (bar_N2O_fao_simu + theme(plot.tag.position  = c(.095, .93))) + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm")) + 
  plot_annotation(tag_levels = "a", tag_prefix = '(', tag_suffix = ')')

# Save plot
tiff("2-outputs/Output/bar_N2O.tif", width = 16, height = 12, units = 'cm', res = 300)
print(bar_N2O_plt)
dev.off()


# Function to process data for Reco and GPP
process_flux_data <- function(df, flux_vars, conversion_factor = 0.01) {
  df <- df %>%
    mutate(across(all_of(flux_vars), ~ .x * conversion_factor))
  
  df_long <- df %>%
    pivot_longer(cols = all_of(flux_vars), names_to = "group", values_to = "flux") %>%
    mutate(years = format(Date, "%Y"))
  
  df_sum <- df_long %>%
    group_by(years, group) %>%
    summarise(flux = sum(flux, na.rm = TRUE)) %>%
    mutate(across(where(is.numeric), round, digits = 2))
  
  df_sum
}

# Function to add uncertainties
add_uncertainties <- function(df, uncertainties) {
  df <- df %>%
    left_join(uncertainties, by = "group") %>%
    mutate(uncertainty = if_else(is.na(uncertainty), flux * uncertainty_pct / 100, uncertainty),
           uncertainty = round(uncertainty, 2))
  
  df
}

# Function to create bar plots
create_bar_plot <- function(data, groups, colors, labels, y_label) {
  ggplot(data = data, aes(x = years, y = flux, fill = group)) +
    geom_bar(stat = "identity", position = position_dodge(), width = 0.7, alpha = 0.7) +
    geom_errorbar(aes(ymax = flux + uncertainty, ymin = flux - uncertainty), 
                  size = 0.2, width = 0.18, linetype = "solid", 
                  position = position_dodge(0.7), show.legend = FALSE) +
    scale_fill_manual(name = '', breaks = groups, values = colors, labels = labels) +
    scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
    labs(x = "", y = y_label) +
    bar_thm +
    theme(legend.key.height = unit(1, "mm"),
          axis.text.x = element_text(size = 7, color = "black"),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.justification = c(0.72, 0.83),
          legend.position = c(0.72, 0.83),
          legend.margin = margin(-0.8, 0, -0.1, 0, unit = "cm"),
          axis.title.x = element_blank())
}

# Process Reco data
flux_vars_reco <- c("Reco", "Reco.simu")
df_reco <- process_flux_data(df_2, flux_vars_reco)
bar_Reco <- create_bar_plot(df_reco, 
                            c("Reco", "Reco.simu"), 
                            c("Reco" = "gray70", "Reco.simu" = "gray40"), 
                            c("Reco Observed", "Reco Simulated"), 
                            expression(paste("Ecosystem Respiration (t C ha"^"-1"," yr"^"-1",")")))

# Process GPP and Reco data
flux_vars_gpp_reco <- c("GPP", "GPP.simu", "Reco", "Reco.simu")
df_gpp_reco <- process_flux_data(df_2, flux_vars_gpp_reco)

# Add uncertainties
uncertainties <- tibble(
  group = c("GPP.simu", "Reco.simu"),
  uncertainty = c(df_uncert_GPP$uncert_GPP, df_uncert_Reco$uncert_Reco)
)

# Add uncertainty percentages for observed data
df_gpp_reco <- df_gpp_reco %>%
  mutate(uncertainty_pct = case_when(
    group == "GPP" ~ 4.2,
    group == "Reco" ~ 11.2,
    TRUE ~ NA_real_
  ))

df_gpp_reco <- add_uncertainties(df_gpp_reco, uncertainties)

# Create bar plot for GPP and Reco
bar_GPP_Reco <- create_bar_plot(df_gpp_reco, 
                                c("GPP.simu", "GPP", "Reco.simu", "Reco"), 
                                c("GPP.simu" = "seagreen4", "GPP" = "darkseagreen",
                                  "Reco.simu" = "goldenrod4", "Reco" = "goldenrod"), 
                                c("GPP simulated", "GPP observed", "Reco simulated", "Reco observed"), 
                                expression(paste("CO"[2], " fluxes (t C-CO"[2]*" ha"^"-1"," yr"^"-1",")")))

# Save plots
tiff("2-outputs/Output/bar_GPP_Reco.tif", width = 16, height = 12, units = 'cm', res = 300)
print(bar_GPP_Reco)
dev.off()

tiff("2-outputs/Output/bar_Reco.tif", width = 16, height = 12, units = 'cm', res = 300)
print(bar_Reco)
dev.off()

#3. times series ------------------------------------------------------------------------------------------------ 
annotate_rain <- function(a){
    annotate("text", x = as.Date("2012-08-01"), y = a, label = "515 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+ #,
  annotate("text", x = as.Date("2013-08-01"), y = a, label = "355 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+
  annotate("text", x = as.Date("2014-08-01"), y = a, label = "332 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+
  annotate("text", x = as.Date("2015-08-01"), y = a, label = "271 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+
  annotate("text", x = as.Date("2016-08-20"), y = a, label = "368 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+
  annotate("text", x = as.Date("2017-08-01"), y = a, label = "400 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+
  annotate("text", x = as.Date("2018-08-01"), y = a, label = "295 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+
  annotate("text", x = as.Date("2019-08-01"), y = a, label = "355 mm",
           vjust=1,fontface="italic",color="blue",size=2,family="Candara")+
  annotate("text", x = as.Date("2020-08-01"), y = a, label = "529 mm",
           vjust=1,fontface="bold",color="blue",size=2,family="Candara")
}
thm_ts<- theme(legend.text=element_text(size=6,face="italic"), 
        axis.text = element_text(colour = "black"),
        axis.title.y=element_text(size = 8,face="italic"),        
        axis.title = element_text(size = 10),
        legend.position = c(0.22, 0.55),
        legend.background = element_rect(fill="transparent",color = NA),
        legend.title = element_text(size = 6.5, face = "italic"),
        legend.box.background = element_rect(colour = "black"),
        legend.box.margin = margin(-7.2,-1,-7,-3,"mm"),
        panel.grid.major = element_line(linetype=5,size = .2),
        text = element_text(family="Candara"),# 
        legend.direction="horizontal"
  )
#3.2 SWC, ET -------------------------------------------------------------------------------------
df_2$Rainfall <- 35-df_2$Rain/6
df2 <- df_2 %>%
  select(Date,Shum_1_per,Shum_1_per.simu,Rainfall) %>%
  gather(key = "variable", value = "value", -Date)

plt_shum = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  annotate_rain(38.0)+  
  geom_line(data=subset(df2,variable == "Shum_1_per.simu",linetype = "solid"), size = 0.3)+
  geom_point(data=subset(df2,variable == "Shum_1_per"),size = 0.2)+
  geom_line(data=subset(df2,variable == "Rainfall",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c("Rain","SWC_Observed","SWC_Simulated"),
                     values = c("blue","red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","blank","solid"),
                       shape = c(NA,16,NA),size = 1 ))) +  labs(color="")+
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(sec.axis=sec_axis(~(35-.)*6, name= "Rain (mm)",breaks=seq(0,200,25)),
                     breaks=seq(0,35,5), expand = c(0, 0.01), limits = c(2.2, NA))+
  theme_classic()+
  ylab("Soil Water \n Content (%)")+xlab("Date")+thm_ts
  
# reg Shum----------------------------
reg.Shum <- lm(Shum_1_per.simu ~ Shum_1_per, df_2)
slp <- reg.Shum[["coefficients"]][2]
intcpt <- reg.Shum[["coefficients"]][1]
summary(reg.Shum)
EF<-CroPlotR::EF(df_2$Shum_1_per.simu,df_2$Shum_1_per,na.rm = T)
BIAS<-CroPlotR::Bias(df_2$Shum_1_per.simu,df_2$Shum_1_per,na.rm = T)
RMSE<-CroPlotR::RMSE(df_2$Shum_1_per.simu,df_2$Shum_1_per,na.rm = T)

reg_Shum<-ggplot(df_2, aes(y=Shum_1_per.simu, x=Shum_1_per))+
  geom_point(alpha = 0.4,size=1.3,col="grey30")+
  geom_abline(intercept=0, slope=1, linetype = "dashed",size=.5)+
  geom_abline(intercept = intcpt ,slope =slp ,colour = "black", linetype = "solid",size=.5)+
  ylab("Simulated Soil Water Content (%)")+
  xlab("Observed Soil Water Content (%)") +  
  scale_y_continuous(breaks=seq(2,16,2),expand = c(0, 0), limits = c(0, NA))+
  scale_x_continuous(breaks=seq(2,16,2),expand = c(0, 0), limits = c(0, NA))+
  annotate("text", x =11.7, y = 3.8, hjust = 0,
           label = paste0("Y = ",round(slp,2),"X + ",round(intcpt,2),
                          "\nEF = ",round(EF,2),"\nBIAS = ",round(BIAS,2),
                          "\nRMSE = ",round(RMSE,2)),size=2.5,family="Candara")+
  coord_fixed(xlim = c(2, 16),ylim=c(2, 16))+reg_thm


# ET --------------------------
df_2$Etr.simu = replace(df_2$Etr.simu,df_2$Etr.simu<0,0)
df2 <- df_2 %>%
  select(Date,Etr,Etr.simu) %>% #,Rainfall
  gather(key = "variable", value = "value", -Date)

plt_etr = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  geom_line(data=subset(df2,variable == "Etr.simu",linetype = "solid"), size = 0.3)+
  geom_point(data=subset(df2,variable == "Etr"),size = 0.2)+
  scale_color_manual(labels = c("ET_Observed","ET_Simulated"),#,"Rain"
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,NA),size = 1))) + #,"deepskyblue1"
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+ labs(color="")+
  scale_y_continuous(expand = c(0, 0), breaks=seq(0,9,2))+
  theme_classic()+ylab(expression(paste("Evapotranspiration (mm d"^"-1",")")))+xlab("Date")+thm_ts

# reg ET---------------------------------------
reg.etr <- lm(Etr.simu ~ Etr, df_2)
slp <- reg.etr[["coefficients"]][2]
intcpt <- reg.etr[["coefficients"]][1]
summary(reg.etr)
EF<-CroPlotR::EF(df_2$Etr.simu,df_2$Etr,na.rm = T)
BIAS<-CroPlotR::Bias(df_2$Etr.simu,df_2$Etr,na.rm = T)
RMSE<-CroPlotR::RMSE(df_2$Etr.simu,df_2$Etr,na.rm = T)

reg_etr<-ggplot(df_2, aes(y=Etr.simu, x=Etr))+
  geom_point(alpha = 0.4,size=1.3,col="grey30")+
  geom_abline(intercept=0, slope=1, linetype = "dashed",size=.5)+
  geom_abline(intercept = intcpt ,slope =slp ,colour = "black", linetype = "solid",size=.5)+
  ylab(expression(paste("Simulated Evapotranspiration (mm d"^"-1",")")))+
  xlab(expression(paste("Observed Evapotranspiration (mm d"^"-1",")")))+
  theme_classic()+ 
  scale_y_continuous(breaks=seq(0,10,1))+
  scale_x_continuous(breaks=seq(0,10,1))+
  annotate("text", x =6.5, y = 0.8, hjust = 0,
           label = paste0("Y = ",round(slp,2),"X + ",round(intcpt,2),
                          "\nEF = ",round(EF,2),"\nBIAS = ",round(BIAS,2),
                          "\nRMSE = ",round(RMSE,2)), size=2.5,family="Candara")+
  coord_fixed(xlim = c(0, 9),ylim=c(0, 9))+reg_thm

ts_SWC_ET_plt = ((plt_shum+theme(plot.tag.position  = c(.095, .8)))/
                 (plt_etr+theme(plot.tag.position  = c(.095, .9)))/
                 (reg_Shum+theme(plot.tag.position  = c(.2, .96))+
                  reg_etr+theme(plot.tag.position  = c(.15, .96)))
                 )+ theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')+
  plot_layout(heights = c(1.2,1,1.9))
ts_SWC_ET_plt
tiff("2-outputs/Output/ts_SWC_ET_plt.tif",  width=17, height=17,units='cm',res = 300)
ts_SWC_ET_plt
dev.off()

#3.2 Biomass & ground cover-----------------------------------------------------------
df_2$Rainfall <- 600-df_2$Rain/0.4
a = 625
df2 <- df_2 %>%
  select(Date,BMv,BMv.simu,Rainfall) %>% #,Rainfall
  gather(key = "variable", value = "value", -Date)

plt_BMv = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  annotate_rain(a)+  
  geom_line(data=subset(df2,variable == "BMv.simu",linetype = "solid"), size = 0.3)+
  geom_point(data=subset(df2,variable == "BMv"),size = 0.2)+
  geom_line(data=subset(df2,variable == "Rainfall",linetype = "solid"), size = 0.3)+
  geom_line(aes(color = variable), size = 0.3)+
  scale_color_manual(labels = c("Biomass_Observed","Biomass_Simulated","Rain"),
                     values = c("red","black","blue"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid","solid"),
                       shape = c(16,NA,NA),size=1))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(sec.axis=sec_axis(~(600-.)*0.4, name= "Rain (mm)",
                                      breaks=seq(0,250,25)),breaks=seq(0,600,100),
                   expand = c(0, 0), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  theme_classic()+
  ylab(expression(atop("Green herbaceous mass",paste(" (gDM m"^"-2",")"))))+
  xlab("Date")+thm_ts+theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# reg BMv-------------------------
step_reg(df_2,"BMv",target = "metric")
reg.BMv <- lm(BMv.simu ~ BMv, df_2)
slp <- reg.BMv[["coefficients"]][2]
intcpt <- reg.BMv[["coefficients"]][1]
summary(reg.BMv)
EF<-CroPlotR::EF(df_2$BMv.simu,df_2$BMv,na.rm = T)
BIAS<-CroPlotR::Bias(df_2$BMv.simu,df_2$BMv,na.rm = T)
RMSE<-CroPlotR::RMSE(df_2$BMv.simu,df_2$BMv,na.rm = T)

reg_Bmv<-ggplot(df_2, aes(y=BMv.simu, x=BMv))+
  geom_point(alpha = 0.4,size=1.3,col="grey30")+
  geom_abline(intercept=0, slope=1, linetype = "dashed",size=.5)+
  geom_abline(intercept = intcpt ,slope =slp ,colour = "black", linetype = "solid",size=.5)+
  ylab(expression(paste("Simulated green biomass (gMS m"^"-2",")")))+  #
  xlab(expression(paste("Observed green biomass (gMS m"^"-2",")")))+ 
  theme_classic()+ 
  scale_y_continuous(breaks=seq(0,400,50))+
  scale_x_continuous(breaks=seq(0,400,50))+
  annotate("text", x =310, y = 22, hjust = 0,
           label =  paste0("Y = ",round(slp,2),"X + ",round(intcpt,2),
                           "\nEF = ",round(EF,2),"\nBIAS = ",round(BIAS,2),
                           "\nRMSE = ",round(RMSE,2)),  size=2.5,family="Candara")+
  coord_fixed(xlim = c(0, 400),ylim=c(0, 400))+reg_thm

# LAI ----------------------------
df2 <- df_2 %>%
  select(Date,LAIv,LAIv.simu) %>% #,Rainfall
  gather(key = "variable", value = "value", -Date)

plt_LAIv = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  geom_line(data=subset(df2,variable == "LAIv.simu",linetype = "solid"), size = 0.3)+
  geom_point(data=subset(df2,variable == "LAIv"),size = 1,shape=1)+
  scale_color_manual(labels = c("LAI_MODIS","LAI_Simulated"),#,"Rain"
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,1),size = 1))) + #,"deepskyblue1"
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  labs(color = expression(paste("")))+
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  theme_bw()+ylab(expression(paste("LAI (m"^"2", "m"^"-2",")")))+xlab("Date")+thm_ts

step_reg(df_2,"LAIv",target = "plot")
reg_LAIv<-ggplot(df_2, aes(y=LAIv.simu, x=LAIv))+
  geom_abline(intercept=0, slope=1, colour = "gray70", linetype = "solid",size=.5)+
  geom_point(alpha = 200,size=.7)+
  geom_smooth(colour="black", method="lm", fill="white",size=.5) +
  ylab(expression(paste("Simulated LAI (m"^"2", "m"^"-2",")")))+
  xlab(expression(paste("LAI MODIS (m"^"2", "m"^"-2",")"))) +
  theme_minimal()+ 
  scale_y_continuous(breaks=seq(0,3,0.5),expand = c(0.01, 0),
                     position = "right", sec.axis = sec_axis(~., labels = NULL))+
  scale_x_continuous(breaks=seq(0,3,0.5),expand = c(0, 0),limits = c(0, NA))+
  theme(panel.grid.major = element_line(linetype="dashed"),
        text = element_text(family="Candara"),# 
        axis.text=element_text(size=8,colour="black"), 
        axis.title=element_text(size=8, face = 'italic'),
  )+
  annotate("text", x =2.5, y = 0.3, label = "Y = 0.87X - 0.16\n R\u00B2 = 0.43\n  RMSE = 0.51", 
           size=2.5, fontface = 'italic')+
  coord_fixed(xlim = c(0, 3),ylim=c(0, 3))
reg_LAIv

#3.3 TqCs & Total_N -----------------------------------------------------------------------
# convert variables
#SOC(%)= (SOcg/m2 * 10) / (10cm * 1.5g/cm3 * 1000) ==================> 1g/m2 =10kg/ha
df_2$TqCs.simu <- (df_2$TqCs.simu * 10) / (10 * 1.5 * 1000)# convert TqCs to %
df_2$Total_N.simu <- (df_2$Total_N.simu * 10) / (10 * 1.5 * 1000) #convert Total N to %
df_2$Total_N<-replace(df_2$Total_N,df_2$Total_N>0.044,NA)

# Total_N
df2 <- df_2 %>%
  select(Date,Total_N,Total_N.simu) %>% 
  gather(key = "variable", value = "value", -Date)

plt_TotalN = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  geom_point(data=subset(df2,variable == "Total_N"),size = 0.2)+
  geom_line(data=subset(df2,variable == "Total_N.simu",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c("Soil_N_Observed","Soil_N_Simulated"),
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,NA),size=1))) +   
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0,0))+
  scale_y_continuous(breaks=seq(0.001,0.08,0.005))+
  theme_classic()+ylab(paste("Soil total \n N (%)"))+xlab("Date")+thm_ts
## Total C
df_2$Rainfall <- 0.8-df_2$Rain/300
df2 <- df_2 %>%
  select(Date,TqCs,TqCs.simu,Rainfall) %>% 
  gather(key = "variable", value = "value", -Date)

plt_TqCs = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  geom_point(data=subset(df2,variable == "TqCs"),size = 0.2)+
  geom_line(data=subset(df2,variable == "TqCs.simu",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c("Soil_C_Observed","Soil_C_Simulated"),
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,NA),size=1))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0,0))+
  scale_y_continuous(expand = c(0, 0), limits = c(0.16, 0.32),
                     breaks=seq(0.10,0.32,0.03))+
  theme_classic()+ylab(paste("Soil total \n C (%)"))+xlab("Date")+
  thm_ts+theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ts_BMv_C_N_plt = ((plt_BMv + theme(plot.tag.position  = c(.135, .87)))/
                  (plt_TqCs + theme(plot.tag.position  = c(.135, .785)))/
                  (plt_TotalN + theme(plot.tag.position  = c(.135, .80)))
                  )+ theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')+
  plot_layout(heights = c(1.5,1,1))

ts_BMv_C_N_plt
tiff("2-outputs/Output/ts_BMv_C_N_plt.tif",  width=17, height=15,units='cm',res = 300)
ts_BMv_C_N_plt
dev.off()

#3.4 GPP & Reco ----------------------------------------------------------------------------

## GPP
df_2$Rainfall <- 30-df_2$Rain/8
a = 32
df2 <- df_2 %>%
  select(Date,GPP,GPP.simu,Rainfall) %>% 
  gather(key = "variable", value = "value", -Date)

plt_GPP = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  annotate_rain(a)+
  geom_line(data=subset(df2,variable == "GPP.simu",linetype = "solid"), size = 0.4)+
  geom_point(data=subset(df2,variable == "GPP"),size = 0.4)+
  geom_line(data=subset(df2,variable == "Rainfall",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c("GPP_Observed","GPP_Simulated","Rain"),
                     values = c("red","black","blue"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid","solid"),
                       shape = c(16,NA,NA), size = 1))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(sec.axis=sec_axis(~(30-.)*8, name= "Rain (mm)",
                                       breaks=seq(0,250,25)),breaks=seq(0,35,5),
                     expand = c(0, 0), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  theme_classic()+
  ylab(expression(atop("Gross Primary",paste("Productivity (gC m"^"-2"," d "^"-1",")"))))+
  xlab("Date")+thm_ts+theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plt_GPP
# reg GPP-------------------------------------------
step_reg(df_2,"GPP",target = "plot")
reg.GPP <- lm(GPP.simu ~ GPP, df_2)
slp <- reg.GPP[["coefficients"]][2]
intcpt <- reg.GPP[["coefficients"]][1]
summary(reg.GPP)
EF<-CroPlotR::EF(df_2$GPP.simu,df_2$GPP,na.rm = T)
BIAS<-CroPlotR::Bias(df_2$GPP.simu,df_2$GPP,na.rm = T)
RMSE<-CroPlotR::RMSE(df_2$GPP.simu,df_2$GPP,na.rm = T)

reg_GPP<-ggplot(df_2, aes(y=GPP.simu, x=GPP))+
  geom_point(alpha = 0.4,size=1.3,col="grey30")+
  geom_abline(intercept=0, slope=1, linetype = "dashed",size=.5)+
  geom_abline(intercept = intcpt ,slope =slp ,colour = "black", linetype = "solid",size=.5)+
  ylab(expression(paste("Simulated GPP (gC m"^"-2"," d "^"-1",")")))+
  xlab(expression(paste("Observed GPP (gC m"^"-2"," d "^"-1",")"))) +
  scale_y_continuous(breaks=seq(0,18,2))+
  scale_x_continuous(breaks=seq(0,18,2))+
  annotate("text", x =13, y = 1.7,  hjust = 0,
           label = paste0("Y = ",round(slp,2),"X + ",round(intcpt,2),
                          "\nEF = ",round(EF,2),"\nBIAS = ",round(BIAS,2),
                          "\nRMSE = ",round(RMSE,2)),size=2.5,family="Candara")+
  coord_fixed(xlim = c(0, 18),ylim=c(0, 18))+reg_thm
reg_GPP

## Reco---------------------
df2 <- df_2 %>%
  select(Date,Reco,Reco.simu) %>% 
  gather(key = "variable", value = "value", -Date)

plt_Reco = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  
  # plot lines
  geom_point(data=subset(df2,variable == "Reco"),size = 0.4)+
  geom_line(data=subset(df2,variable == "Reco.simu",linetype = "solid"), size = 0.4)+
  scale_color_manual(labels = c("Reco_Observed","Reco_Simulated"),
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,NA),size = 1))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  theme_classic()+
  ylab(expression(atop("Ecosystem",paste("Respiration (gC m"^"-2"," d "^"-1",")"))))+
  xlab("Date")+thm_ts

# reg Reco -----------------------------
step_reg(df_2,"Reco",target = "plot")
reg.Reco <- lm(Reco.simu ~ Reco, df_2)
slp <- reg.Reco[["coefficients"]][2]
intcpt <- reg.Reco[["coefficients"]][1]
summary(reg.Reco)
EF<-CroPlotR::EF(df_2$Reco.simu,df_2$Reco,na.rm = T)
BIAS<-CroPlotR::Bias(df_2$Reco.simu,df_2$Reco,na.rm = T)
RMSE<-CroPlotR::RMSE(df_2$Reco.simu,df_2$Reco,na.rm = T)

reg_Reco<-ggplot(df_2, aes(y=Reco.simu, x=Reco))+
  geom_point(alpha = 0.4,size=1.3,col="grey30")+
  geom_abline(intercept=0, slope=1, linetype = "dashed",size=.5)+
  geom_abline(intercept = intcpt ,slope =slp ,colour = "black", linetype = "solid",size=.5)+
  ylab(expression(paste("Simulated R"[eco]*" (gC m"^"-2"," d "^"-1",")")))+
  xlab(expression(paste("Observed R"[eco]*" (gC m"^"-2"," d "^"-1",")"))) +
  scale_y_continuous(breaks=seq(0,18,2))+
  scale_x_continuous(breaks=seq(0,18,2))+
  annotate("text", x =11.7, y = 2, hjust = 0,
           label = paste0("Y = ",round(slp,2),"X + ",round(intcpt,2),
                          "\nEF = ",round(EF,2),"\nBIAS = ",round(BIAS,2),
                          "\nRMSE = ",round(RMSE,2)),size=2.5,family="Candara")+
  coord_fixed(xlim = c(0, 18),ylim=c(0, 18))+reg_thm

ts_GPP_Reco = ((plt_GPP+theme(plot.tag.position  = c(.11, .85)))/
                 (plt_Reco+theme(plot.tag.position  = c(.115, .9)))/
                 (reg_GPP+theme(plot.tag.position  = c(.24, .96))+
                    reg_Reco+theme(plot.tag.position  = c(.155, .96)))
)+ theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')+
  plot_layout(heights = c(1.2,1,1.9))
ts_GPP_Reco

tiff("2-outputs/Output/ts_GPP_Reco.tif",  width=17, height=17,units='cm',res = 300)
ts_GPP_Reco
dev.off()

#### Microbial resp and autotrophic resp-------
df$Auto_resp = df$Reco - df$CO2Soil
df2 <- df %>%
  select(Date,CO2Soil,Auto_resp) %>% 
  gather(key = "variable", value = "value", -Date)

plt_CO2 = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  geom_line(data=subset(df2,variable == "CO2Soil"),linetype = "solid",size = 0.4)+
  geom_line(data=subset(df2,variable == "Auto_resp"),linetype = "solid", size = 0.4)+
  scale_color_manual(labels = c("Autotrophic resp.","Heterotrophic resp."),
                     values = c("chartreuse4","brown4"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","solid"),
                       shape = c(16,16),size = 1))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  theme_classic()+ylab(expression(paste("Simulated respiration (gC m"^"-2"," d "^"-1",")")))+xlab("Date")+thm_ts


#3.5 N2O ----------------------------------------------------------------------------

df_2$Rainfall <- 42-df_2$Rain/6
a = 44.5
df2 <- df_2 %>%
  select(Date,En2o_NOE,En2o_NOE.simu,Rainfall) %>%
  gather(key = "variable", value = "value", -Date)
#head(df2, 3)

plt_N2O = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  annotate_rain(a) +
  geom_line(data=subset(df2,variable == "En2o_NOE.simu"), size = 0.3)+
  geom_point(data=subset(df2,variable == "En2o_NOE"), size = 1,shape =16)+
  geom_line(data=subset(df2,variable == "Rainfall",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c(expression(paste("Measured N"[2]*"O")),
                                expression(paste("Simulated N"[2]*"O")),"Rain"),
                     values = c("red","black","blue"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid","solid"),
                       shape = c(16,NA,NA),size = 1))) +
  labs(color="")+
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(sec.axis=sec_axis(~(42-.)*6, name= "Rain (mm)",breaks=seq(0,300,25)),
                     breaks=seq(0,42,5), expand = c(0, 0.01), limits = c(0, NA))+
  theme_classic()+
  ylab(expression(atop(paste("N"[2]*"O emissions"),paste(" (ng N-N"[2]*"O m"^"-2","s"^"-1",")"))))+
  xlab("Date")+thm_ts

tiff("2-outputs/Output/plt_N2O.tif",  width=17, height=7,units='cm',res = 300)
plt_N2O
dev.off()

# reg N2O-----------------------------------------------------------
reg.temp <- lm(En2o_NOE.simu ~ En2o_NOE, df_2) 
slp <- reg.temp[["coefficients"]][2]
intcpt <- reg.temp[["coefficients"]][1]
summary(reg.temp)
EF<-CroPlotR::EF(df_2$En2o_NOE.simu,df_2$En2o_NOE,na.rm = T)
BIAS<-CroPlotR::Bias(df_2$En2o_NOE.simu,df_2$En2o_NOE,na.rm = T)
RMSE<-CroPlotR::RMSE(df_2$En2o_NOE.simu,df_2$En2o_NOE,na.rm = T)

reg_co2_1<-ggplot(df_2, aes(y=En2o_NOE.simu, x=En2o_NOE))+
  geom_point(alpha = 0.4,size=2,col="grey30")+
  geom_abline(intercept=0, slope=1, linetype = "dashed",size=.5)+
  geom_abline(intercept = intcpt ,slope =slp ,colour = "black", linetype = "solid",size=.5)+
  ylab(expression(atop(paste("Simulated N"[2]*"O emissions"),paste(" (ng N-N"[2]*"O m"^"-2","s"^"-1",")"))))+
  xlab(expression(atop(paste("Observed N"[2]*"O emissions"),paste(" (ng N-N"[2]*"O m"^"-2","s"^"-1",")"))))+
  theme_classic()+ 
  scale_y_continuous(breaks=seq(0,8,2),
                     position = "right", sec.axis = sec_axis(~., labels = NULL))+
  scale_x_continuous(breaks=seq(0,8,2),expand = c(0, 0))+
  annotate("text", x =1.5, y = 6.5, hjust = 0,
           label = paste0("Y = ",round(slp,2),"X  ",round(intcpt,2),
                          "\nEF = ",round(EF,2),"\nBIAS = ",round(BIAS,2),
                          "\nRMSE = ",round(RMSE,2)), 
           size=2,family="Candara")+
  coord_fixed(xlim = c(0,8),ylim=c(0, 8))+reg_thm
reg_co2_1

plt_co2_final <- ((plt_N2O+theme(plot.tag.position  = c(.18, .75)))+
                  (reg_co2_1+theme(plot.tag.position  = c(.098, .92))))+
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')+
  plot_layout(widths  = c(2.2,1))
plt_co2_final 

tiff("2-outputs/Output/plt_N2O_2.tif",  width=17, height=6.8,units='cm',res = 300)
plt_co2_final
dev.off()

# Zoom 2013 ------------------------------
df_2_n = subset(df_2,df_2$Date>"2013-07-01"&df_2$Date<"2013-11-09")
df_2_n$Rainfall <- 30-df_2_n$Rain/6
df2 <- df_2_n %>%
  select(Date,En2o_NOE,En2o_NOE.simu,Rainfall) %>%
  gather(key = "variable", value = "value", -Date)

plt_N2O_1 = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  geom_line(data=subset(df2,variable == "En2o_NOE.simu"), size = 0.3)+
  geom_point(data=subset(df2,variable == "En2o_NOE"), size = 1,shape =16)+
  geom_line(data=subset(df2,variable == "Rainfall",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c("Measured N2O emissions","Simulated N2O_emissions"),
                     values = c("red","black","blue")) +
  labs(color="")+
  scale_x_date(date_labels="%Y-%m-%d",date_breaks  ="4 week",expand = c(0, 0))+
  scale_y_continuous(sec.axis=sec_axis(~(30-.)*6,breaks=seq(0,200,35)),
                     breaks=seq(0,30,5), expand = c(0, 0.01), limits = c(0, NA))+
  theme_classic()+
  ylab(expression(atop(paste("N"[2]*"O emissions"),
                       paste(" (ng N-N"[2]*"O m"^"-2","s"^"-1",")"))))+
  thm_ts

# Zoom 2017 ------------------------------
df_2_n = subset(df_2,df_2$Date>"2017-09-01"&df_2$Date<"2017-10-13")
df_2_n$Rainfall <- 30-df_2_n$Rain/6
df2 <- df_2_n %>%
  select(Date,En2o_NOE,En2o_NOE.simu,Rainfall) %>%
  gather(key = "variable", value = "value", -Date)

plt_N2O_2 = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  geom_line(data=subset(df2,variable == "En2o_NOE.simu"), size = 0.3)+
  geom_point(data=subset(df2,variable == "En2o_NOE"), size = 1,shape =16)+
  geom_line(data=subset(df2,variable == "Rainfall",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c("Measured N2O emissions",
                                "Simulated N2O_emissions","Rain"),
                     values = c("red","black","blue")) +  labs(color="")+
  scale_x_date(date_labels="%Y-%m-%d",date_breaks  ="2 week",expand = c(0, 0))+
  scale_y_continuous(sec.axis=sec_axis(~(30-.)*6,name= "Rain (mm)",breaks=seq(0,200,35)),
                     breaks=seq(0,30,5), expand = c(0, 0.01), limits = c(0, NA))+  ylab("")+
  theme_classic()+thm_ts

plt_N2O_3 = plt_N2O_1 +plt_N2O_2 + theme(plot.margin = unit(c(0,0,0,0), "cm"))+
  plot_layout(ncol = 2,widths = c(2, 1.4))
ts_plt_N2O = plt_N2O / plt_N2O_3 +theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_layout(heights = c(2,1))

#---------------------------------------------------------------------------------------------
# next 2_plt_no_animal------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
# contribution Nit and Denit ---------------------------------------
df1 = df_no_ani[,c("Date","N2O_total_kg_ha","N2Onit_kg_ha","N2Odenit_kg_ha")]
df1$years <- format(df1$Date, "%Y")
require(plyr)
df1 <- ddply(df1,.(years), summarize,
             N2Odenit_kg_ha = sum(N2Odenit_kg_ha),
             N2Onit_kg_ha= sum(N2Onit_kg_ha),
             N2O_total_kg_ha = sum(N2O_total_kg_ha))
df1$per_N2Onit_kg_ha = (df1$N2Onit_kg_ha/df1$N2O_total_kg_ha)*100
data_no_ani = df1
df1$per_N2Onit_kg_ha <- df1$per_N2Onit_kg_ha/200

df2 <- df1 %>%
  select(years,N2Odenit_kg_ha,N2Onit_kg_ha,per_N2Onit_kg_ha) %>%
  gather(key = "group", value = "N2O", -years)

sumYear=df2 %>%
  mutate_if(is.numeric,round,digits=2)

g_N2O_bar= ggplot(data = subset(sumYear,group == "N2Odenit_kg_ha"|group == "N2Onit_kg_ha"), 
                  aes(x = years, 
                      y = N2O, 
                      fill= group)) +
  geom_bar(stat="identity",position="stack",width = 0.4,alpha=0.7) + # position="fill", position="stack"
  geom_line(data=subset(sumYear,group == "per_N2Onit_kg_ha"),
            aes(x=years,y=N2O,group=1,color="N2O"),size=1,alpha=0.7)+
  scale_colour_manual(" ", values=c("N2O" = "black"),
                      labels=c("Nitrification (Ratio to total emissions)"))+
  scale_fill_manual(name = '',
                    breaks = c("N2Odenit_kg_ha", "N2Onit_kg_ha"),
                    values = c("N2Odenit_kg_ha" = "gray70", 
                               "N2Onit_kg_ha" = "gray40"),
                    labels = c("Denitrification", "Nitrification")) + 
  scale_y_continuous(sec.axis=sec_axis(~.*200,name="% Emissions by nitrification",breaks=seq(0,100,10)),
                     breaks=seq(0,0.55,0.05), expand = c(0, 0), limits = c(0, NA))+
  labs(x="",y= expression(paste("N" [2]*"O emissions (kg N ha"^"-1"," yr"^"-1",")")) ) +
  theme_minimal() +
  theme(text = element_text(family="CenturySch"),
        axis.title.y = element_text(size = 8),
        # adjust X-axis labels
        axis.text.x = element_text(size = 5.5,color="black"),
        # adjust y-axis labels
        axis.text.y = element_text(size = 7,color="black"),
        legend.text = element_text(size = 6, face = "italic"),
        # change spacing between legend items
        legend.key.height = unit(3, "mm"),
        # don't draw legend box (check element_rect() for borders and backgrounds)
        legend.background = element_blank(),
        legend.justification = c(1,1),
        legend.position =c(1,1),
        legend.direction="vertical",
        axis.title.x=element_blank(),
        legend.box = "vertical",
        legend.margin=margin(-0.8,0,-0.1,0, unit="cm") )

# merging data and data_no_ani------------------------------------------------------------------
df1 = df[,c("Date","N2O_total_kg_ha","N2Onit_kg_ha","N2Odenit_kg_ha")]
df1$years <- format(df1$Date, "%Y")
require(plyr)
df1 <- ddply(df1,.(years), summarize,
             N2Odenit_kg_ha = sum(N2Odenit_kg_ha),
             N2Onit_kg_ha= sum(N2Onit_kg_ha),
             N2O_total_kg_ha = sum(N2O_total_kg_ha))
df1$per_N2Onit_kg_ha = (df1$N2Onit_kg_ha/df1$N2O_total_kg_ha)*100
data=df1
data_all = merge(data,data_no_ani,by="years",suffixes = c("","_no_ani"))
data_all$decrease_total = data_all$N2O_total_kg_ha - data_all$N2O_total_kg_ha_no_ani
data_all$per_decrease_total = (data_all$decrease_total/data_all$N2O_total_kg_ha)*100
names(data_all)
newdata = data_all[,c("years","N2O_total_kg_ha","N2O_total_kg_ha_no_ani","decrease_total","per_decrease_total")]
newdata$per_decrease_total2 <- newdata$per_decrease_total/200
df2 <- newdata %>%
  select(years,N2O_total_kg_ha,N2O_total_kg_ha_no_ani,per_decrease_total2) %>%
  gather(key = "group", value = "N2O", -years)
df2$uncertainty<-NA
df2[df2$group=="N2O_total_kg_ha","uncertainty"]<-uncert_data$sd_n2o
df2[df2$group=="N2O_total_kg_ha_no_ani","uncertainty"]<-(uncert_data$per_incert_n2o*df2[df2$group=="N2O_total_kg_ha_no_ani","N2O"])/100
sumYear=df2 %>%
  mutate_if(is.numeric,round,digits=5)
g_N2O_bar= ggplot(data = subset(sumYear,group == "N2O_total_kg_ha"|group == "N2O_total_kg_ha_no_ani"), 
                  aes(x = years,y = N2O,fill= group)) +
  geom_bar(stat="identity",position="dodge",width = 0.7,alpha=0.7) +
  geom_errorbar(aes(ymax=N2O+uncertainty, 
                    ymin=N2O-uncertainty),size=.2, width=.18, linetype="solid", 
                position=position_dodge(.7))+
  scale_fill_manual(name = '',
                    breaks = c("N2O_total_kg_ha", "N2O_total_kg_ha_no_ani"),
                    values = c("N2O_total_kg_ha" = "gray70", 
                               "N2O_total_kg_ha_no_ani" = "gray40"),
                    labels = c("With actual animal load", "Without livestock")) + 
  scale_y_continuous(breaks=seq(0,0.7,0.1), expand = c(0, 0), limits = c(0, 0.55))+
  labs(x="", y= expression(paste("N" [2]*"O emissions (kg N-N"[2]*"O ha"^"-1"," yr"^"-1",")")),
  ) +bar_thm
g_N2O_bar

# Reco---------------------------------------
names(df_no_ani)
df1 = df_no_ani[,c("Date","CO2Soil")]
df1$years <- format(df1$Date, "%Y")
df1_no_ani <- ddply(df1,.(years), summarize,
                    CO2Soil_t_ha = sum(CO2Soil*0.01))
df1 = df[,c("Date","CO2Soil")]
df1$years <- format(df1$Date, "%Y")
df1 <- ddply(df1,.(years), summarize,
             CO2Soil_t_ha = sum(CO2Soil*0.01))
data = merge(df1,df1_no_ani,by="years",suffixes = c("","_no_ani"))
data$decrease_total = data$CO2Soil_t_ha - data$CO2Soil_t_ha_no_ani
data$per_decrease_total = (data$decrease_total/data$CO2Soil_t_ha)*100
data$per_decrease_total2 <- data$per_decrease_total/10
df2 <- data %>%
  select(years,CO2Soil_t_ha,CO2Soil_t_ha_no_ani,per_decrease_total2) %>%
  gather(key = "group", value = "Reco", -years)
df2$uncertainty<-NA
df2[df2$group=="CO2Soil_t_ha","uncertainty"]<-uncert_data$sd_co2
df2[df2$group=="CO2Soil_t_ha_no_ani","uncertainty"]<-(uncert_data$per_incert_co2*df2[df2$group=="CO2Soil_t_ha_no_ani","Reco"])/100
sumYear=df2 %>%
  mutate_if(is.numeric,round,digits=5)
g_CO2_bar= ggplot(data = subset(sumYear,group == "CO2Soil_t_ha"|group == "CO2Soil_t_ha_no_ani"), 
                  aes(x = years, y = Reco,fill= group)) +
  geom_bar(stat="identity",position="dodge",width = 0.7,alpha=0.7) + 
  geom_errorbar(aes(ymax=Reco+uncertainty, 
                    ymin=Reco-uncertainty),size=.2, width=.18, linetype="solid", 
                position=position_dodge(.7))+
  scale_fill_manual(name = '',
                    breaks = c("CO2Soil_t_ha", "CO2Soil_t_ha_no_ani"),
                    values = c("CO2Soil_t_ha" = "gray70", 
                               "CO2Soil_t_ha_no_ani" = "gray40"),
                    labels = c("With actual animal load", "Without livestock")) +
  scale_y_continuous(breaks=seq(0,5,0.4), expand = c(0, 0), limits = c(0, 2.2))+
  labs(x="",y= expression(paste("CO" [2]*" emissions (t C-CO"[2]*" ha"^"-1"," yr"^"-1",")")),
  ) + bar_thm + theme(legend.justification = c(0.95,0.93),
                      legend.position =c(0.95,0.93))


#---------------------------------------------------------------------------------------------
# next 3_plt_Dyna------------------------------------------------------------------------
#---------------------------------------------------------------------------------------------
df_in1 <-readxl::read_excel("0-data/step_1d_simu/daily_original/SeasonDahra2012-2020.xlsx")
df3 <- cbind(df_2,df_in1[,c(2,3)])
df3$Reco.simu = dyna1$Ra_Tree + dyna2$Ra_Tree
df3$GPP.simu = dyna1$GPP_Tree + dyna2$GPP_Tree
df3$Tree_Transpi = dyna1$T_Tree + dyna2$T_Tree
df3$Reco = ifelse(df3$Season2=="dry_season",df3$Reco,NA)
df3$GPP = ifelse(df3$Season2=="dry_season",df3$GPP,NA)
df3$Etr =ifelse(df3$Season2=="dry_season",df3$Etr,NA)
df3$Etr.simu = df3$Tree_Transpi

df3$LAI_Tree = dyna1$LAI_Tree + dyna2$LAI_Tree
df3$LAI_Radi = dyna2$LAI_Tree
df3$LAI_Bala = dyna1$LAI_Tree
df3$Tree_Morta = dyna1$Mortality_Leaf_Tree +dyna1$Mortality_Branch_Tree + dyna1$Mortality_Stem_Tree +
  dyna1$Mortality_CR_Tree+dyna1$Mortality_FRoot_Tree+dyna2$Mortality_Leaf_Tree +dyna2$Mortality_Branch_Tree +
  dyna2$Mortality_Stem_Tree +  dyna2$Mortality_CR_Tree+dyna2$Mortality_FRoot_Tree
df3$Tree_Morta_Brch_Stem = dyna1$Mortality_Branch_Tree + dyna1$Mortality_Stem_Tree +
  dyna2$Mortality_Branch_Tree + dyna2$Mortality_Stem_Tree 
df3$Tree_Morta_leaf_Froot = dyna1$Mortality_Leaf_Tree+ dyna1$Mortality_FRoot_Tree+
  dyna2$Mortality_Leaf_Tree + dyna2$Mortality_FRoot_Tree


high_seas<-function(){
  geom_rect(aes(xmin = as.Date("2012-06-24"),
                xmax = as.Date("2012-10-11"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+ ##B3E5FC
  geom_rect(aes(xmin = as.Date("2013-06-30"),
                xmax = as.Date("2013-10-17"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+
  geom_rect(aes(xmin = as.Date("2014-07-03"),
                xmax = as.Date("2014-10-14"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+
  geom_rect(aes(xmin = as.Date("2015-07-07"),
                xmax = as.Date("2015-10-24"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+
  geom_rect(aes(xmin = as.Date("2016-07-15"),
                xmax = as.Date("2016-09-16"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+
  geom_rect(aes(xmin = as.Date("2017-06-24"),
                xmax = as.Date("2017-10-25"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+
  geom_rect(aes(xmin = as.Date("2018-06-27"),
                xmax = as.Date("2018-09-29"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+
  geom_rect(aes(xmin = as.Date("2019-07-25"),
                xmax = as.Date("2019-10-25"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)+
  geom_rect(aes(xmin = as.Date("2020-07-01"),
                xmax = as.Date("2020-10-05"),
                ymin = -Inf, ymax = Inf),
            fill="#E1F5FE", linetype=0, alpha=1, inherit.aes=FALSE)
}
# Transpiration tree-----------------------
df2 <- df3 %>%
  select(Date,Etr,Etr.simu) %>% 
  gather(key = "variable", value = "value", -Date)
plt_T_Tree = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
            high_seas()+  
  # plot lines
  geom_line(data=subset(df2,variable == "Etr.simu",linetype = "solid"), size = 0.4)+
  geom_point(data=subset(df2,variable == "Etr"),size = 0.4)+
  scale_color_manual(labels = c("Transpiration_Observed","Transpiration_Simulated"),
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,NA), size = 1
                     ))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(breaks=seq(0,2,0.2),
                     expand = c(0, 0), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  ylab(expression(atop("Tree Transpiration",paste("(mm"," d "^"-1",")"))))+
  xlab("Date")+
  theme_classic()+ thm_ts+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plt_T_Tree


## GPP-----------------
df2 <- df3 %>%
  select(Date,GPP,GPP.simu) %>% 
  gather(key = "variable", value = "value", -Date)

plt_GPP = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  high_seas()+
  # plot lines
  geom_line(data=subset(df2,variable == "GPP.simu",linetype = "solid"), size = 0.4)+
  geom_point(data=subset(df2,variable == "GPP"),size = 0.4)+  
  scale_color_manual(labels = c("GPP_Observed","GPP_Simulated"),
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,NA), size = 1     ))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(breaks=seq(0,2,0.5),
                     expand = c(0, 0), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  ylab(expression(atop("Tree Gross Primary",paste("Production (gC m"^"-2"," d "^"-1",")"))))+
  xlab("Date")+  theme_classic()+ thm_ts+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


## LAI--------------------

df3$LAI_Tree = dyna1$LAI_Tree + dyna2$LAI_Tree
plot(df3$LAI_Tree)
df3$Rainfall <- 0.4-df3$Rain/600
a = 0.43
df2 <- df3 %>%
  select(Date,LAI_Radi,LAI_Bala,Rainfall) %>% 
  gather(key = "variable", value = "value", -Date)

plt_LAI = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  # highlight wet seasons
  high_seas()+annotate_rain(a)+  
  # plot lines
  geom_line(data=subset(df2,variable == "LAI_Radi",linetype = "solid"), size = 0.4)+
  geom_line(data=subset(df2,variable == "LAI_Bala",linetype = "solid"), size = 0.4)+
  geom_line(data=subset(df2,variable == "Rainfall",linetype = "solid"), size = 0.3)+
  scale_color_manual(labels = c("LAI B. aegyptiaca","LAI A. raddiana","Rain"),
                     values = c("red","black","blue"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("solid","solid","solid"),
                       shape = c(16,16,16), size = 1))) +
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(sec.axis=sec_axis(~(0.4-.)*600, name= "Rain (mm)",
                                       breaks=seq(0,250,25)),breaks=seq(0,0.4,0.05),
                     expand = c(0, 0), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  theme_classic()+
  ylab(expression(atop("Tree LAI",paste("(m"^"2"," m "^"-2",")"))))+
  xlab("Date")+thm_ts+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

plt_LAI

## Reco---------------------
df2 <- df3 %>%
  select(Date,Reco,Reco.simu) %>% 
  gather(key = "variable", value = "value", -Date)

plt_Reco = ggplot(df2, aes(x=ymd(Date),y=value,colour=variable)) +
  high_seas()+
  # plot lines
  geom_point(data=subset(df2,variable == "Reco"),size = 0.4)+
  geom_line(data=subset(df2,variable == "Reco.simu",linetype = "solid"), size = 0.4)+
  scale_color_manual(labels = c("Reco_Observed","Reco_Simulated"),
                     values = c("red","black"),
                     guide = guide_legend(override.aes = list(
                       linetype = c("blank","solid"),
                       shape = c(16,NA),size = 1))) + 
  scale_x_date(date_labels="%Y",date_breaks  ="1 year",expand = c(0, 0))+
  scale_y_continuous(expand = c(0.01, 0.01), limits = c(0, NA))+
  labs(color = expression(paste("")))+
  theme_classic()+
  ylab(expression(atop("Tree Respiration",paste("(gC m"^"-2"," d "^"-1",")"))))+
  xlab("Date")+thm_ts

ts_GPP_Reco_plt= ((plt_LAI+theme(plot.tag.position  = c(.115, .85)))/
                  (plt_T_Tree+theme(plot.tag.position  = c(.115, .93)))/
                  (plt_GPP+theme(plot.tag.position  = c(.115, .93)))/
                  (plt_Reco+theme(plot.tag.position  = c(.115, .93))))+ 
  theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')+
  plot_layout(heights = c(1.3,1,1,1))
ts_GPP_Reco_plt

tiff("2-outputs/Output/trees_ts_GPP_Reco.tif",  width=17, height=18,units='cm',res = 300)
ts_GPP_Reco_plt
dev.off()

#bar plot---------------------
# mortality--------------
names(df3)
dff =df3[,c("Date","Tree_Morta","Reco.simu","GPP.simu","Tree_Morta_leaf_Froot","Tree_Morta_Brch_Stem")]
dff$years <- format(dff$Date, "%Y")
dff1 <- ddply(dff,.(years), summarize,
             Reco_Tree = sum(Reco.simu*0.01),
             GPP_Tree= sum(GPP.simu*0.01),
             Tree_Mortality = sum(Tree_Morta*0.01),
             Tree_Morta_leaf_Froot =sum(Tree_Morta_leaf_Froot*0.01),
             Tree_Morta_Brch_Stem=sum(Tree_Morta_Brch_Stem*0.01))

bar_tree_morta = ggplot(data = dff1,aes(x = years,y = Tree_Morta_leaf_Froot)) +
  geom_bar(fill="gray40",stat="identity", position=position_dodge(),width = 0.5,alpha=0.7) +
  scale_y_continuous(breaks=seq(0,0.6,0.1), expand = c(0, 0), limits = c(0, NA))+
  labs(x="", y=(expression(atop("Tree organs mortality",paste("(t C ha"^"-1"," yr "^"-1",")")))),
  ) + bar_thm+  theme(axis.text.x = element_text(size = 7,color="black"))

#bar_tree_morta2########################
df2 <- dff1 %>%
  select(years,Tree_Morta_leaf_Froot,Tree_Morta_Brch_Stem) %>%
  gather(key = "group", value = "Morta", -years)
bar_tree_morta2= ggplot(data = df2, aes(x= years, 
                        y= Morta, fill= group)) +
  geom_bar(stat="identity",position="stack",width = 0.5,alpha=0.7) + 
  scale_fill_manual(name = '',
                    breaks = c("Tree_Morta_Brch_Stem", "Tree_Morta_leaf_Froot"),
                    values = c("Tree_Morta_Brch_Stem" = "gray70", 
                               "Tree_Morta_leaf_Froot" = "gray40"),
                    labels = c("Mortality: Branch, Stem & C.root", "Mortality: Leaf & F.root")) + 
  scale_y_continuous(breaks=seq(0,1,0.1), expand = c(0, 0), limits = c(0, NA))+
  labs(x="", y=(expression(atop("Tree organs mortality",paste("(t C ha"^"-1"," yr "^"-1",")")))),
  ) + bar_thm+
  theme(legend.key.height = unit(4, "mm"),
        legend.justification = c(0,0.9),
        legend.position = c(0,0.9))
bar_tree_morta2
tiff("2-outputs/Output/bar_tree_morta2.tif",  width=12, height=6,units='cm',res = 300)
bar_tree_morta2
dev.off()

#plot Reco tree & herbaceous----------------
names(df_2)
ndf1 =df_2[,c("Date","Reco.simu","GPP.simu")]
ndf1$years <- format(ndf1$Date, "%Y")
ndff1 <- ddply(ndf1,.(years), summarize,
              Reco_all = sum(Reco.simu*0.01),
              GPP_all = sum(GPP.simu*0.01))
dff2 = merge(ndff1,dff1,by="years")
dff2$Reco_herba = dff2$Reco_all-dff2$Reco_Tree
dff2$GPP_herba = dff2$GPP_all-dff2$GPP_Tree
df2 <- dff2 %>%
  select(years,Reco_Tree,Reco_herba) %>%
  gather(key = "group", value = "Reco", -years)
bar_TH_Reco= ggplot(data = df2,aes(x= years, y= Reco,fill= group)) +
  geom_bar(stat="identity",position=position_dodge(),width = 0.5,alpha=0.7) + 
  scale_fill_manual(name = '',
                    breaks = c("Reco_herba", "Reco_Tree"),
                    values = c("Reco_herba" = "gray70", 
                               "Reco_Tree" = "gray40"),
                    labels = c("Reco Herbaceous layer", "Reco Tree layer")) + 
  scale_y_continuous(breaks=seq(0,8,1), expand = c(0, 0), limits = c(0, NA))+
  labs(x="",   y= expression(atop("Ecosystem Respiration",paste("(tC ha"^"-1"," yr"^"-1",")"))),
  ) +bar_thm

# GPP tree & herbaceous -------------------
df2 <- dff2 %>%
  select(years,GPP_Tree,GPP_herba) %>%
  gather(key = "group", value = "GPP", -years)
bar_TH_GPP= ggplot(data = df2, aes(x= years,y= GPP,fill= group)) +
  geom_bar(stat="identity",position=position_dodge(),width = 0.5,alpha=0.7) + 
  scale_fill_manual(name = '',
                    breaks = c("GPP_herba", "GPP_Tree"),
                    values = c("GPP_herba" = "gray70", 
                               "GPP_Tree" = "gray40"),
                    labels = c("GPP Herbaceous layer", "GPP Tree layer")) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA),breaks=seq(0,8,1))+
  labs(x="", y= expression(atop("Gross Primary",paste("Production (tC ha"^"-1"," yr"^"-1",")"))),
  ) +bar_thm

#####GPP +Reco------------------
df2 <- dff2 %>%
  select(years,GPP_herba,GPP_Tree,Reco_Tree,Reco_herba) %>%
  gather(key = "group", value ="CO2", -years)
df2$uncertainty<-NA
df2[df2$group=="GPP_herba","uncertainty"]<-uncert_data$sd_GPP
df2[df2$group=="Reco_herba","uncertainty"]<-uncert_data$sd_Reco
df2[df2$group=="Reco_Tree","uncertainty"]<-
  (uncert_data_tree$per_sd_Ra*df2[df2$group=="Reco_Tree","CO2"])/100
df2[df2$group=="GPP_Tree","uncertainty"]<-
  (uncert_data_tree$per_sd_GPP*df2[df2$group=="GPP_Tree","CO2"])/100
g_GPP_Reco_bar_Tree= ggplot(data = df2,aes(x= years,y= CO2,fill= group)) +
  geom_bar(stat="identity",position=position_dodge(),width = 0.7,alpha=0.7) +
  geom_errorbar(aes(ymax=CO2+uncertainty, 
                    ymin=CO2-uncertainty),size=.2, width=.18, linetype="solid", 
                position=position_dodge(.7))+
  scale_fill_manual(breaks = c("GPP_herba","GPP_Tree","Reco_herba","Reco_Tree"),
                    values = c("GPP_herba" = "deepskyblue4","GPP_Tree" = "deepskyblue",
                               "Reco_herba" = "grey30", "Reco_Tree" = "grey70"),
                    labels = c("GPP Herbaceous layer","GPP Tree layer",
                               "Reco Herbaceous layer","Reco Tree layer")) + 
  scale_y_continuous(breaks=seq(0,15,1), expand = c(0, 0), limits = c(0, 9))+
  labs(x="",y= expression(paste("Simulated CO"[2]," fluxes (t C-CO"[2]*" ha"^"-1"," yr"^"-1",")"))
  ) + bar_thm+
  theme(axis.title.y = element_text(size = 8, face = "bold"),
        axis.text.x = element_text(size = 7,color="black"),
        legend.key.height = unit(1, "mm"),
        legend.title = element_blank(),
        legend.justification = c(0.95,0.85),
        legend.position = c(0.95,0.85),
        legend.margin=margin(-0.8,0,-0.1,0, unit="cm")  )

bar_Reco_GPP_T_H =((g_GPP_Reco_bar2+theme(plot.tag.position  = c(.2, .95)))+
                     (g_GPP_Reco_bar_Tree+theme(plot.tag.position  = c(.22, .95)))
                   )+ theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')
bar_Reco_GPP_T_H
tiff("2-outputs/Output/bar_Reco_GPP_T_H.tif",  width=16, height=6,units='cm',res = 300)
bar_Reco_GPP_T_H
dev.off()
# compute NEE Tree--------------------
df_Rh_tree = read.csv("Rh_tree.csv")
dff1$NEP = dff1$GPP_Tree-dff1$Reco_Tree-dff1$Tree_Morta_Brch_Stem-df_Rh_tree$Rh_tree_tC_ha
dff1$N1EP_tCO2eq_ha = dff1$NEP*3.67
df$years <- format(df$Date, "%Y")
step_data <- aggregate(N2O_total_t_CO2_equiv_ha~years, df, "sum")
dff1 = merge(dff1,step_data,by="years")

#-------------------------------------------------------------------------------------------------
# run 2CO2_N2O_increase.R------------------------------------------------------------------------
#-------------------------------------------------------------------------------------------------
data_CO2 = data
data_CO2$decrease_CO2_co2eq=data_CO2$decrease_total*3.67
data_all$decrease_N2O_co2eq = ((data_all$decrease_total)*1.57*265*0.001)
data_live = data_CO2[1]
data_live$increase_CO2_co2eq = data_CO2$decrease_CO2_co2eq
data_live$increase_N2O_co2eq = data_all$decrease_N2O_co2eq
data_live$CO2_N2O =  data_live$increase_N2O_co2eq +data_live$increase_CO2_co2eq
dff1 = merge(dff1,data_live,by="years")
dff1$aN1EP_tCO2eq_ha = dff1$N1EP_tCO2eq_ha
dff1$per_per = (dff1$aN1EP_tCO2eq_ha/dff1$CO2_N2O)*100

df2 <- dff1 %>%
  select(years,aN1EP_tCO2eq_ha,increase_N2O_co2eq,increase_CO2_co2eq) %>%
  gather(key = "group", value = "Var", -years)

fao_data = read.csv("0-data/FAO_GHG/FAOSTAT_data_en_10-19-2022.csv")
fao_data = subset(fao_data,fao_data$Element=="Emissions (CO2eq) from CH4 (AR5)"|
                    fao_data$Element=="Emissions (CO2eq) from N2O (AR5)")
fao_data$value_t_ha = (fao_data$Value*1000)/19672200 # senegal area = 19672200 ha
unique(fao_data$Item)

# Emissions Manure Management, Manure applied to Soils, Manure left on Pasture--------------------------
# fao_manure = subset(fao_data,fao_data$Item=="Manure Management"|
#                       fao_data$Item=="Manure applied to Soils"|
#                       fao_data$Item=="Manure left on Pasture")

fao_manure = subset(fao_data,fao_data$Item=="Enteric Fermentation"|
                    fao_data$Item=="Manure left on Pasture")
names(fao_manure)
# group
fao_manure1=fao_manure[,c("Year","Element","value_t_ha")]
fao_manure1[1,2]
data = fao_manure1
data = replace(data,data=="Emissions (CO2eq) from CH4 (AR5)","CH4")
data = replace(data,data=="Emissions (CO2eq) from N2O (AR5)","N2O")
data2= data%>%
  mutate_if(is.numeric,round,digits=2)
CH4_FAO = subset(data2,data2$Element=="CH4")
data2$Year = as.character(data2$Year)

names(CH4_FAO)=c("years","group","Var")
CH4_FAO = replace(CH4_FAO,CH4_FAO=="CH4","increase_D_CH4")
df22 = rbind(df2,CH4_FAO)
bar_T_NEP_N2O = ggplot(data = df22,aes(x= years,y= Var,fill= group)) +
  geom_bar(stat="identity",position=position_dodge(),width = 0.6,alpha=0.7) + 
  scale_fill_manual(name = '',
                    breaks = c("aN1EP_tCO2eq_ha",
                               "increase_CO2_co2eq",
                               "increase_D_CH4",
                               "increase_N2O_co2eq"),
                    values = c("aN1EP_tCO2eq_ha" = "seagreen4",
                               "increase_CO2_co2eq"="gray50",
                               "increase_D_CH4"="deepskyblue4",
                               "increase_N2O_co2eq" = "peru"),
                    labels = c("Trees’ net productivity ",
                               expression("CO"[2]*" emissions due to livestock"),
                               expression("CH"[4]*" emissions due to livestock"),
                               expression("N"[2]*"O emissions due to livestock"))) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 4.8),breaks=seq(0,8,1))+
  labs(x="", y= expression(atop("Annual CO"[2]*"eq fluxes",paste(" (t CO"[2]*"eq ha"^"-1"," yr"^"-1",")"))),) +
  theme_classic()+ 
  theme(panel.grid.major= element_line(linetype=5,size = .2),
        text = element_text(family="Candara"),# 
        panel.border = element_rect(color = "black",fill = NA),
        axis.title.y = element_text(size = 8, face = "bold"),
        axis.text.y = element_text(size = 7,color="black"),
        legend.text = element_text(size = 6.5),
        legend.key.height = unit(3, "mm"),
        legend.background = element_blank(),
        legend.justification = c(1,0),
        legend.direction="horizontal",
        legend.position ="bottom",#c(0.78,1),
        legend.margin=margin(-0.1,0,-0.1,0, unit="cm"),
        axis.title.x=element_blank())

plt_GHG_NEP<-(((g_CO2_bar+theme(plot.tag.position  = c(.3, .95)))+
                 (g_N2O_bar+theme(plot.tag.position  = c(.2, .95))))/
                (bar_T_NEP_N2O+theme(plot.tag.position  = c(.13, .95)))
              )+ theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')
tiff("2-outputs/Output/plt_GHG_NEP.tif",  width=16, height=12,units='cm',res = 300)
plt_GHG_NEP
dev.off()


