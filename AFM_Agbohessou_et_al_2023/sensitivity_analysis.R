# Sensitivity analysis 
# To what extent are greenhouse-gas emissions offset by trees in a Sahelian silvopastoral system? (Agbohessou et al. 2023)
# https://doi.org/10.1016/j.agrformet.2023.109780
# y. agbohessou


## define the function to run the model for the sensitivity analysis 

thm_tss=  theme_classic()+
  theme(panel.grid.major = element_line(linetype=5,size = .2),
        panel.grid.minor = element_line(linetype=0),
        axis.text=element_text(size=5,colour="black"), 
        text = element_text(family="Candara"),
        axis.title = element_text(size = 6),
        legend.position ="none")

run_step<-function(step_daily_original=step_daily_original,
                      daily_original=daily_original){
  file.copy(from = paste0(step_daily_original,"/Stepatsec.exe"),
            to = paste0(daily_original,"/"),
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  setwd(get(".rs.getProjectDirectory")())
  setwd(daily_original)
  system("./Stepatsec")
  unlink("*.exe")
  setwd(get(".rs.getProjectDirectory")())
  dir.create("2-outputs/Output", showWarnings = FALSE, recursive = FALSE)
  files=Sys.glob(file.path(paste0(daily_original, "/*.out")))
  file.copy(from = files,to = "2-outputs/Output/",
            overwrite = TRUE, recursive = TRUE, copy.mode = TRUE)
  unlink(files)
}

step_sensitivity <- function(x) {
  # Animal load
  t = rstep::cha
  # repartition de la charge animale pour chaque mois
  t[5:16,1][t[5:16,1] == t[5:16,1]] <- "0.53" # Bovins
  t[5:16,2][t[5:16,2] == t[5:16,2]] <- 0.185 # Caprins
  t[5:16,3][t[5:16,3] == t[5:16,3]] <- 0.185 # Ovins
  t[5:16,4][t[5:16,4] == t[5:16,4]] <- 0.0 # Asins
  t[5:16,5][t[5:16,5] == t[5:16,5]] <- 0.0 # Camelins
  t[5:16,6][t[5:16,6] == t[5:16,6]] <- 0.1 # Equins
  # 31 lignes: Jour, charge animale totale (nombre de tetes) * 12
  t[17:47,2][t[17:47,2] == t[17:47,2]] <- 726*x[1] # jan
  t[17:47,3][t[17:47,3] == t[17:47,3]] <- 484*x[1]# feb
  t[17:47,4][t[17:47,4] == t[17:47,4]] <- 242*x[1] # mar
  t[17:47,5][t[17:47,5] == t[17:47,5]] <- 242*x[1] # apr
  t[17:47,6][t[17:47,6] == t[17:47,6]] <- 242*x[1] # may
  t[17:47,7][t[17:47,7] == t[17:47,7]] <- 484*x[1]# jun
  t[17:47,8][t[17:47,8] == t[17:47,8]] <- 726*x[1]# jul
  t[17:47,9][t[17:47,9] == t[17:47,9]] <- 726*x[1] # aug
  t[17:47,10][t[17:47,10] == t[17:47,10]] <- 1210*x[1] # sep
  t[17:47,11][t[17:47,11] == t[17:47,11]] <- 1210*x[1] # oct
  t[17:47,12][t[17:47,12] == t[17:47,12]] <- 1210*x[1] # nov
  t[17:47,13][t[17:47,13] == t[17:47,13]] <- 1210*x[1] # dec
  # aires de desserte
  t[48,1][t[48,1] == t[48,1]] <- "500"
  t[48,2:12][t[48,2:12] == t[48,2:12]] <- 500
  for (iyr in 12:20) {
    myfile <- file.path(paste0("0-data/step_1d_simu/daily_original/S01001", iyr, ".cha"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  }
  
  # paramètre clé---------------------------------------------------------------------------
  t = rstep::cle
  #valeur de référence dans pierre et al 2011
  t[1,1][t[1,1] == t[1,1]] <- x[4] #  efficience de conversion (gMS/MJ)  !2.5 and 7 caroline pierre
  t[1,2][t[1,2] == t[1,2]] <- x[5] #  Biomasse verte initiale  (gMS/m2) !0.1 et 3
  t[1,3][t[1,3] == t[1,3]] <- x[7] #  % de C3	!20 et 60
  t[1,4][t[1,4] == t[1,4]] <- x[6] #  Slag0 vert (cm² g-1) !100-300
  for (i_yr in 12:20) {
    myfile <- file.path(paste0("0-data/step_1d_simu/daily_original/S01001",i_yr,".cle"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  }
  
  # soil files -4 ; 4-----------------------------------------------
  t=sol
  t=t[c(-3,-11),]
  t[1,2][t[1,2] =="SITE RSP_six_forage"] <- "SITE DAHRA"
  t[4,2][t[4,2] =="0.6"] <- paste0(0.4)           # 0.4 stock d'eau initial (mm) #1
  t[5,2][t[5,2] == "17.87"] <-paste0(23.5)        # 23.5 Temp. sol #1
  t[7,2][t[7,2] == "5"] <-paste0(7.9+(x[2]))             # 7.9   % argile #1
  t[8,2][t[8,2] == "93"] <-paste0(89.0-(x[2]))           # 89.0  % sable #1
  t[9,2][t[9,2] == "7.4"] <- paste0(x[3])          # 6.4 soil pH #1
  t[4,3][t[4,3] == t[4,3]] <- 8.0                 # 8.0 stock d'eau initial (mm) #2
  t[5,3][t[5,3] == t[5,3]] <- 23.9                 # 23.9 Temp. sol #2
  t[7,3][t[7,3] == t[7,3]] <-7.9+(x[2])                # 7.9   % argile #2
  t[8,3][t[8,3] == t[8,3]] <- 89.0-(x[2])                 # 89.0  % sable #2
  t[9,3][t[9,3] == t[9,3]] <- x[3]                 # 6.4 soil pH #2
  t[4,4][t[4,4] ==t[4,4]] <- 10.0                 # 10 stock d'eau initial (mm) #3
  t[5,4][t[5,4] == t[5,4]] <-28.0                 # 28.0 Temp. sol #3
  t[7,4][t[7,4] == t[7,4]] <-7.4+(x[2])                # 7.4   % argile #3
  t[8,4][t[8,4] == t[8,4]] <- 91.0-(x[2])                 # 91.0  % sable #3
  t[9,4][t[9,4] == t[9,4]] <- x[3]                 # 6.4 soil pH #3
  t[4,5][t[4,5] ==t[4,5]] <- 38.0                 # 38.0 stock d'eau initial (mm) #4
  t[5,5][t[5,5] == t[5,5]] <-30.0                 # 30.0 Temp. sol #4
  t[7,5][t[7,5] == t[7,5]] <-5.5+(x[2])                 # 5.5   % argile #4
  t[8,5][t[8,5] == t[8,5]] <-91.3 -(x[2])                # 91.3  % sable #4
  t[9,5][t[9,5] == t[9,5]] <- x[3]                 # 6.4 soil pH #4
  t[5,6][t[5,6] == t[5,6]] <- 32.0                 # 32.0 Temp. sol #5
  for (yr_i in 12:20) {
    myfile <- file.path(paste0("0-data/step_1d_simu/daily_original/S01001",yr_i,".sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  }
  # run step----------------------------------------------------------------------
  run_step(step_daily_original = step.exe,daily_original =filepath)
  #write output file in csv and xlsx format in the workspace---------------------------
  write_step_out("Dahra",workspace,12,20,"csv")
  df= gen_table(filepath,workspace,"Dahra",c("N2O_total_kg_ha","SoilResp"),obs=F,test0 = F)
  df$years <- format(df$Date, "%Y")
  df2 =aggregate(SoilResp~years, df, "sum")
  mean_CO2=mean(df2$SoilResp)
  return(mean_CO2)
}

# perform the sensitivity analysis (sobol method  Saltelli et al., 2008)-------------------------------------------------------------

# run Variance Based Sensitivity Analysis script developped by Saltelli et al., 2010
source("sobol_ sensitivity.R")

# set upper and lower bounds for the parameters

# parms1: 0.1-4 value times actual animal load
# parms2: -4,4 change ins oil texture 
# parms3: 4,9 soil pH
# parms4: 4,8 maximum conversion efficiency
# parms5: 0.5,2.5 aboveground mass at germination
# parms6: 160,300 SLA at germination
# parms7: 20,60 % of C3 plants

# load the function "step_sensitivity" below thats runs the model for the sensitivity analysis

set.seed(123) # for reproducible results
sensi<-sobol(fn="step_sensitivity",   
             lower=c(0.1,-4,4,4,0.5,160,20), 
             upper=c(4,4,9,8,2.5,300,60),
             N=10*7, scrambling="Faure-Tezuka", REPORT=5)
# N should be 10*number of parameter (must be even number)

# N2O
saveRDS(sensi,file="sensitivity_res_n2o.RData")
write.csv(sensi$Ranking,"sensitivity_res_n2o.csv")
 
# CO2 
saveRDS(sensi,file="sensitivity_res_co2.RData")
write.csv(sensi$Ranking,"sensitivity_res_co2.csv")

# Plot result N2O---------------------------------------------------------------------
senN2O_SI = readRDS("sensitivity_res_n2o.RData")
rank_parms_n2o = senN2O_SI$Ranking
rank_parms_n2o$Parameter.Name[rank_parms_n2o$Parameter.Name=="Param1       "]<-"Animal load"
rank_parms_n2o$Parameter.Name[rank_parms_n2o$Parameter.Name=="Param2       "]<-"Soil texture"
rank_parms_n2o$Parameter.Name[rank_parms_n2o$Parameter.Name=="Param3       "]<-"Soil pH"
rank_parms_n2o$Parameter.Name[rank_parms_n2o$Parameter.Name=="Param4       "]<-"Max CE"
rank_parms_n2o$Parameter.Name[rank_parms_n2o$Parameter.Name=="Param5       "]<-"Bg0"
rank_parms_n2o$Parameter.Name[rank_parms_n2o$Parameter.Name=="Param6       "]<-"SLAg0"
rank_parms_n2o$Parameter.Name[rank_parms_n2o$Parameter.Name=="Param7       "]<-"%C3"
rank_parms_n2o
rank_parms_n2o$First.Order.Index<-replace(rank_parms_n2o$First.Order.Index,
                                          rank_parms_n2o$First.Order.Index<0,0)
rank_parms_n2o$First.Order.Index<-rank_parms_n2o$First.Order.Index*100
plt_rank1_n2o <- ggplot(rank_parms_n2o,
                       aes(x= reorder(Parameter.Name,+First.Order.Index),y =First.Order.Index)) +
  geom_segment(aes(x=reorder(Parameter.Name,+First.Order.Index), 
                   xend=reorder(Parameter.Name,+First.Order.Index),
                   y=0, yend=First.Order.Index),color="grey50",size=3,alpha=0.6) +
  coord_flip()+
  labs(x="",y="First order sensitivity index (%)",
       subtitle =  expression("Soil N"[2]*"O emissions : most sensitive parameters "))+
  scale_y_continuous(breaks=seq(0,50,10),expand = c(0, 0),limits = c(0,41))+
  theme_classic()+
  theme(legend.position="bottom",
        panel.grid.major = element_line(linetype=5,size = .2),
        panel.grid.minor = element_line(linetype=0),
        text = element_text(size=7.5,family="Candara"),#   Comic Sans MS
        axis.text = element_text(color="black"),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"),
        axis.ticks.y=element_blank()
  )
plt_rank1_n2o

rank_parms_n2o$Total.Order.Index<-rank_parms_n2o$Total.Order.Index*100
plt_rankT_n2o <- ggplot(rank_parms_n2o,
                        aes(x= reorder(Parameter.Name,+Total.Order.Index),
                            y =Total.Order.Index)) +
  geom_segment(aes(x=reorder(Parameter.Name,+Total.Order.Index), 
                   xend=reorder(Parameter.Name,+Total.Order.Index),
                   y=0, yend=Total.Order.Index),color="grey50",size=3,alpha=0.6) +
  coord_flip()+
  labs(x="",y="Total order sensitivity index (%)",
       subtitle =  expression("Soil N"[2]*"O emissions : most sensitive parameters "))+
  scale_y_continuous(breaks=seq(0,50,10),expand = c(0, 0),limits = c(0,45))+
  theme_classic()+
  theme(legend.position="bottom",
        panel.grid.major = element_line(linetype=5,size = .2),
        panel.grid.minor = element_line(linetype=0),
        text = element_text(size=7.5,family="Candara"),#   Comic Sans MS
        axis.text = element_text(color="black"),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"),
        axis.ticks.y=element_blank()
  )
plt_rankT_n2o

# Plot result co2---------------------------------------------------------------------
senCO2_SI = readRDS("sensitivity_res_co2.RData")
rank_parms_co2 = senCO2_SI$Ranking
rank_parms_co2$Parameter.Name[rank_parms_co2$Parameter.Name=="Param1       "]<-"Animal load"
rank_parms_co2$Parameter.Name[rank_parms_co2$Parameter.Name=="Param2       "]<-"Soil texture"
rank_parms_co2$Parameter.Name[rank_parms_co2$Parameter.Name=="Param3       "]<-"Soil pH"
rank_parms_co2$Parameter.Name[rank_parms_co2$Parameter.Name=="Param4       "]<-"Max CE"
rank_parms_co2$Parameter.Name[rank_parms_co2$Parameter.Name=="Param5       "]<-"Bg0"
rank_parms_co2$Parameter.Name[rank_parms_co2$Parameter.Name=="Param6       "]<-"SLAg0"
rank_parms_co2$Parameter.Name[rank_parms_co2$Parameter.Name=="Param7       "]<-"%C3"
rank_parms_co2
rank_parms_co2$First.Order.Index<-replace(rank_parms_co2$First.Order.Index,
                                          rank_parms_co2$First.Order.Index<0,0)
rank_parms_co2$First.Order.Index<-rank_parms_co2$First.Order.Index*100

plt_rank1_co2 <- ggplot(rank_parms_co2,
                        aes(x= reorder(Parameter.Name,+First.Order.Index),y =First.Order.Index)) +
  geom_segment(aes(x=reorder(Parameter.Name,+First.Order.Index), 
                   xend=reorder(Parameter.Name,+First.Order.Index),
                   y=0, yend=First.Order.Index),color="grey50",size=3,alpha=0.6) +
  coord_flip()+
  labs(x="",y="First order sensitivity index (%)",
       subtitle =  expression("Soil CO"[2]*" emissions : most sensitive parameters "))+
  scale_y_continuous(breaks=seq(0,60,10),expand = c(0, 0),limits = c(0,60))+
  theme_classic()+
  theme(legend.position="bottom",
        panel.grid.major = element_line(linetype=5,size = .2),
        panel.grid.minor = element_line(linetype=0),
        text = element_text(size=7.5,family="Candara"), # Comic Sans MS
        axis.text = element_text(color="black"),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"),
        axis.ticks.y=element_blank()
  )
plt_rank1_co2
rank_parms_co2$Total.Order.Index<-rank_parms_co2$Total.Order.Index*100
plt_rankT_co2 <- ggplot(rank_parms_co2,
                        aes(x= reorder(Parameter.Name,+Total.Order.Index),
                            y =Total.Order.Index)) +
  geom_segment(aes(x=reorder(Parameter.Name,+Total.Order.Index), 
                   xend=reorder(Parameter.Name,+Total.Order.Index),
                   y=0, yend=Total.Order.Index),color="grey50",size=3,alpha=0.6) +
  coord_flip()+
  labs(x="",y="Total order sensitivity index (%)",
       subtitle =  expression("Soil CO"[2]*" emissions : most sensitive parameters "))+
  scale_y_continuous(breaks=seq(0,70,10),expand = c(0, 0),limits = c(0,70))+
  theme_classic()+
  theme(legend.position="bottom",
        panel.grid.major = element_line(linetype=5,size = .2),
        panel.grid.minor = element_line(linetype=0),
        text = element_text(size=7.5,family="Candara"),#   Comic Sans MS
        axis.text = element_text(color="black"),
        plot.subtitle=element_text(size=7, hjust=0, face="italic", color="black"),
        axis.ticks.y=element_blank()
  )
plt_rankT_co2

#####################################################
names(rank_parms_co2) <- c("Rank","Parms","CO2_SI","CO2_ST"); rank_parms_co2<-rank_parms_co2[-1]
names(rank_parms_n2o) <- c("Rank","Parms","N2O_SI","N2O_ST"); rank_parms_n2o<-rank_parms_n2o[-1]

data_sens<-merge(rank_parms_co2,rank_parms_n2o, by="Parms")

df2 <- data_sens %>%
  select(Parms,CO2_SI,CO2_ST,N2O_SI,N2O_ST) %>%
  gather(key = "group", value ="index", -Parms)
df2

plt<-ggplot(df2,aes(x=Parms,y =index, fill= group)) +
  
  geom_bar(stat="identity",position=position_dodge(),width = 0.7,alpha=0.7) +
  coord_flip()+
  scale_fill_manual(breaks = c("CO2_ST","CO2_SI",
                               "N2O_ST","N2O_SI"),
                    values = c("CO2_ST" = "seagreen4",
                               "CO2_SI" = "darkseagreen",
                               "N2O_ST" = "goldenrod4",
                               "N2O_SI" = "goldenrod"),
                    labels = c(expression(paste("Total Order indices CO" [2]*"")),
                               expression(paste("First Order indices CO" [2]*"")),
                               expression(paste("Total Order indices N" [2]*"O")),
                               expression(paste("First Order indices N" [2]*"O"))
                               )) + 
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0, 0), limits = c(0, 70))+
  ggtitle(expression("STEP-GENDEC-N"[2]*"O"))+
  labs(x="",y= expression(paste("Sensitivity index (%)")),
  ) + bar_thm+theme(axis.title.x = element_text(size = 8, face = "bold"),
                    axis.text.x = element_text(size = 7,color="black"),
                    plot.title = element_text(size = 8, face = "bold"),
                    legend.key.height = unit(1, "mm"),
                    legend.title = element_blank(),
                    legend.justification = c(0.9,0.1),
                    legend.position = c(0.9,0.1),
                    legend.margin=margin(-0.8,0,-0.1,0, unit="cm")  )
plt

# 
# tiff("2-outputs/Output/sensitivity_plot.tif",  width=14, height=8,units='cm',res = 300)
# plt
# dev.off()

# Tree layer GPP and Reco gsa
tgpp<-read.csv("gsa_GPP_Tree.csv")
treco<-read.csv("gsa_Ra_Tree.csv")
names(tgpp)<-c("GPP_ST","GPP_SI","parms")
names(treco)<-c("Reco_ST","Reco_SI","parms")
df_sens<-merge(tgpp,treco, by="parms")
df_sens$GPP_ST<-df_sens$GPP_ST*100;df_sens$GPP_SI<-df_sens$GPP_SI*100
df_sens$Reco_ST<-df_sens$Reco_ST*100;df_sens$Reco_SI<-df_sens$Reco_SI*100
df_sens$GPP_SI<-replace(df_sens$GPP_SI,df_sens$GPP_SI<0,0)
df_sens$Reco_SI<-replace(df_sens$Reco_SI,df_sens$Reco_SI<0,0)

df2 <- df_sens %>%
  select(parms,GPP_ST,GPP_SI,Reco_ST,Reco_SI) %>%
  gather(key = "group", value ="index", -parms)
df2

plt_tree<-ggplot(df2,aes(x=parms,y =index, fill= group)) +
  
  geom_bar(stat="identity",position=position_dodge(),width = 0.7,alpha=0.7) +
  coord_flip()+
  scale_fill_manual(breaks = c("GPP_ST","GPP_SI",
                               "Reco_ST","Reco_SI"),
                    values = c("GPP_ST" = "deepskyblue4",
                               "GPP_SI" = "deepskyblue",
                               "Reco_ST" = "grey30",
                               "Reco_SI" = "grey70"),
                    labels = c(expression(paste("Total Order indices GPP Tree layer")),
                               expression(paste("First Order indices GPP Tree layer")),
                               expression(paste("Total Order indices Reco Tree layer")),
                               expression(paste("First Order indices Reco Tree layer"))
                    )) + 
  scale_y_continuous(breaks=seq(0,100,10), expand = c(0, 0), limits = c(0, 70))+
  ggtitle(expression("DynACof"))+
  labs(x="",y= expression(paste("Sensitivity index (%)")),
  ) + bar_thm+  theme(axis.title.x = element_text(size = 8, face = "bold"),
                      axis.text.x = element_text(size = 7,color="black"),
                      legend.key.height = unit(1, "mm"),
                      plot.title = element_text(size = 8, face = "bold"),
                      legend.title = element_blank(),
                      legend.justification = c(0.9,0.1),
                      legend.position = c(0.9,0.1),
                      legend.margin=margin(-0.8,0,-0.1,0, unit="cm")  )
plt_tree

tiff("2-outputs/Output/sga_plot_tree.tif",  width=14, height=8,units='cm',res = 300)
plt_tree
dev.off()


plt_gsa =((plt+theme(plot.tag = element_text(size = 8),
                     plot.tag.position  = c(.95, .88)))+
                     (plt_tree+theme(plot.tag = element_text(size = 8),
                                     plot.tag.position  = c(.95, .88)))
                   )+ theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')

plt_gsa
tiff("2-outputs/Output/plt_gsa.tif",  width=16, height=6,units='cm',res = 300)
plt_gsa
dev.off()
