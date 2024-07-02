# Uncertainty analysis 
# To what extent are greenhouse-gas emissions offset by trees in a Sahelian silvopastoral system? (Agbohessou et al. 2023)
# https://doi.org/10.1016/j.agrformet.2023.109780
# y. agbohessou

library(mc2d)      # For Monte Carlo simulation
library(ggplot2)   # For visualization
require(rstep)

## define the function to run the model for the uncertainty analysis ----------------------------------------------------
step_uncertainty <- function(x1,x2,x3,x4,x5,x6,x7) {
  
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
  t[17:47,2][t[17:47,2] == t[17:47,2]] <- 726*x1 # jan
  t[17:47,3][t[17:47,3] == t[17:47,3]] <- 484*x1# feb
  t[17:47,4][t[17:47,4] == t[17:47,4]] <- 242*x1 # mar
  t[17:47,5][t[17:47,5] == t[17:47,5]] <- 242*x1 # apr
  t[17:47,6][t[17:47,6] == t[17:47,6]] <- 242*x1 # may
  t[17:47,7][t[17:47,7] == t[17:47,7]] <- 484*x1# jun
  t[17:47,8][t[17:47,8] == t[17:47,8]] <- 726*x1# jul
  t[17:47,9][t[17:47,9] == t[17:47,9]] <- 726*x1 # aug
  t[17:47,10][t[17:47,10] == t[17:47,10]] <- 1210*x1 # sep
  t[17:47,11][t[17:47,11] == t[17:47,11]] <- 1210*x1 # oct
  t[17:47,12][t[17:47,12] == t[17:47,12]] <- 1210*x1 # nov
  t[17:47,13][t[17:47,13] == t[17:47,13]] <- 1210*x1 # dec
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
  t[1,1][t[1,1] == t[1,1]] <- x4 #  efficience de conversion (gMS/MJ)  !2.5 and 7 caroline pierre
  t[1,2][t[1,2] == t[1,2]] <- x5 #  Biomasse verte initiale  (gMS/m2) !0.1 et 3
  t[1,3][t[1,3] == t[1,3]] <- x7 #  % de C3	!20 et 60
  t[1,4][t[1,4] == t[1,4]] <- x6 #  Slag0 vert (cm² g-1) !100-300
  
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
  t[7,2][t[7,2] == "5"] <-paste0(7.9+(x2))             # 7.9   % argile #1
  t[8,2][t[8,2] == "93"] <-paste0(89.0-(x2))           # 89.0  % sable #1
  t[9,2][t[9,2] == "7.4"] <- paste0(x3)          # 6.4 soil pH #1
  
  t[4,3][t[4,3] == t[4,3]] <- 8.0                 # 8.0 stock d'eau initial (mm) #2
  t[5,3][t[5,3] == t[5,3]] <- 23.9                 # 23.9 Temp. sol #2
  t[7,3][t[7,3] == t[7,3]] <-7.9+(x2)                # 7.9   % argile #2
  t[8,3][t[8,3] == t[8,3]] <- 89.0-(x2)                 # 89.0  % sable #2
  t[9,3][t[9,3] == t[9,3]] <- x3                 # 6.4 soil pH #2
  
  t[4,4][t[4,4] ==t[4,4]] <- 10.0                 # 10 stock d'eau initial (mm) #3
  t[5,4][t[5,4] == t[5,4]] <-28.0                 # 28.0 Temp. sol #3
  t[7,4][t[7,4] == t[7,4]] <-7.4+(x2)                # 7.4   % argile #3
  t[8,4][t[8,4] == t[8,4]] <- 91.0-(x2)                 # 91.0  % sable #3
  t[9,4][t[9,4] == t[9,4]] <- x3                 # 6.4 soil pH #3
  
  t[4,5][t[4,5] ==t[4,5]] <- 38.0                 # 38.0 stock d'eau initial (mm) #4
  t[5,5][t[5,5] == t[5,5]] <-30.0                 # 30.0 Temp. sol #4
  t[7,5][t[7,5] == t[7,5]] <-5.5+(x2)                 # 5.5   % argile #4
  t[8,5][t[8,5] == t[8,5]] <-91.3 -(x2)                # 91.3  % sable #4
  t[9,5][t[9,5] == t[9,5]] <- x3                 # 6.4 soil pH #4
  
  t[5,6][t[5,6] == t[5,6]] <- 32.0                 # 32.0 Temp. sol #5
  
  for (yr_i in 12:20) {
    myfile <- file.path(paste0("0-data/step_1d_simu/daily_original/S01001",yr_i,".sol"))
    write.table(t, file = myfile,sep="\t",col.names=FALSE, row.names=FALSE,na="",quote=FALSE)
  }
  # 
  
  # run step----------------------------------------------------------------------
  run_step(step_daily_original = step.exe,daily_original =filepath)
  #write output file in csv and xlsx format in the workspace---------------------------
  write_step_out("Dahra",workspace,12,20,"csv")
  df= gen_table(filepath,workspace,"Dahra",c("N2O_total_kg_ha","GPP"),obs=F,test0 = F)
  df$GPP_t_ha <- df$GPP *0.01
  df$years <- format(df$Date, "%Y")
  df1 =aggregate(GPP_t_ha~years, df, "sum")
  df2 <- tidyr::pivot_wider(df1,names_from = years,values_from = GPP_t_ha)
  # Return the model output
  return(df2)
}

# Perform uncertainty analysis using Monte Carlo simulation---------------------------------------------------------------------

# The code  below performs a Monte Carlo simulation with an increasing number of runs, starting from 100 and incrementing by 100 up to 10,000. 
# For each number of runs, the code samples parameter values from the defined distributions, runs the process-based model, and stores the model outputs.
# After each simulation, the code calculates the standard error of the mean (SEM) and the coefficient of variation (CV) based on the model outputs. 
# The SEM measures the precision of the estimated mean, while the CV indicates the relative variability of the output distribution.
# The code then checks the convergence criteria. Here the convergence is considered achieved if either the SEM divided by the mean is less than or equal to 0.01 
# or the CV is less than or equal to 0.05. These criteria can be ajusted.
# Once the convergence criteria are met, the code prints the number of runs at which convergence was achieved. 
# You can modify the code to store the output distribution or perform further analysis as needed.

# Define the process-based model parameters 

# parms1: 0.1,2.1 value times actual animal load
# parms2: -2,2change ins oil texture 
# parms3: 6,7 soil pH
# parms4: 4,8 maximum conversion efficiency
# parms5: 0.5,2.5 aboveground mass at germination
# parms6: 160,300 SLA at germination
# parms7: 20,60 % of C3 plants

# compute incertitudes 
# Define uncertainty distributions for input parameters
x<- rnorm(1000)
x1_dis <- (x-min(x))/(max(x)-min(x))*(2.1-0.1)+0.1 ;plot(x1_dis)
x2_dis <- (x-min(x))/(max(x)-min(x))*(2-(-2))+(-2) ;plot(x2_dis)
x3_dis <- (x-min(x))/(max(x)-min(x))*(7-6)+6 ;plot(x3_dis)
x4_dis <- (x-min(x))/(max(x)-min(x))*(8-4)+4;plot(x4_dis)
x5_dis <- (x-min(x))/(max(x)-min(x))*(2.5-0.5)+0.5;plot(x5_dis)
x6_dis <- (x-min(x))/(max(x)-min(x))*(300-160)+160;plot(x6_dis)
x7_dis <- (x-min(x))/(max(x)-min(x))*(60-20)+20;plot(x7_dis)

# Perform the Monte Carlo simulation with increasing number of runs
model_outputs <- data.frame()
for (num_runs in seq(100, 1000, by = 100)) {
  # Run Monte Carlo simulation
  for (i in 1:num_runs) {
    # Sample parameter values from the distributions
    x1 <- x1_dis[i]
    x2 <- x2_dis[i]
    x3 <- x3_dis[i]
    x4 <- x4_dis[i]
    x5 <- x5_dis[i]
    x6 <- x6_dis[i]
    x7 <- x7_dis[i]
    # Run the process-based model  # the ouptput is the sum of GPP for each year (can be modified to get results for other outputs)
    out <- step_uncertainty(x1,x2,x3,x4,x5,x6,x7)
    # Store the model output
    model_outputs<-rbind(model_outputs,out)
  }  
  # Calculate the standard error of the mean (SEM)
  sem <- sd(model_outputs$`2012`) / sqrt(num_runs)
  # Calculate the coefficient of variation (CV)
  cv <- sd(model_outputs$`2012`) / mean(model_outputs$`2012`)
  # Check convergence criteria for 2012
  if (sem / mean(model_outputs$`2012`) <= 0.01 || cv <= 0.05) {
    # Convergence criteria met
    print(paste("Convergence achieved with", num_runs, "runs."))
    break
  }
}
write.csv(model_outputs,"uncertainty_res_GPP_t_ha.csv")

# Analyze the results
summary_stats <- summary(model_outputs$X2012)
sd(model_outputs$X2012)
quantiles <- quantile(model_outputs$X2012, probs = c(0.05, 0.5, 0.95))   # Example quantiles

# Visualize the output distribution
ggplot(data.frame(output = model_outputs$X2012), aes(x = output)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "black") +
  geom_vline(xintercept = quantiles, linetype = "dashed", color = "red") +
  labs(x = "Model Output", y = "Frequency") +
  ggtitle("Monte Carlo Simulation Results")


# compute incertitudes 
uncer_GPP<- read.csv("uncertainty_res_GPP_t_ha.csv")
uncer_Reco<-read.csv("uncertainty_res_Reco_t_CO2_ha.csv")
uncer_n2o<-read.csv("uncertainty_res_n2o.csv")
uncer_co2<-read.csv("uncertainty_res_co2.csv")
mean_GPP<-apply(uncer_GPP, 2, mean)
sd_GPP<- apply(uncer_GPP, 2, sd)
mean_Reco<-apply(uncer_Reco, 2, mean)
sd_Reco<- apply(uncer_Reco, 2, sd)
mean_n2o<-apply(uncer_n2o, 2, mean)
sd_n2o<- apply(uncer_n2o, 2, sd)
mean_co2<-apply(uncer_co2, 2, mean);mean_co2<-mean_co2*0.01# g/m2 to t/ha
sd_co2<- apply(uncer_co2, 2, sd);sd_co2<-sd_co2*0.01
t<-rbind(mean_GPP,sd_GPP,mean_Reco,sd_Reco,mean_n2o,sd_n2o,mean_co2,sd_co2)
uncert_data<-as.data.frame(t);uncert_data<-uncert_data[-1]
uncert_data2<- as.data.frame(t(uncert_data),row.names = T)
uncert_data2$years<-seq(from=2012, to=2020)
write.csv(uncert_data2,"uncertainty_res_total.csv",row.names = F)
read.csv("uncertainty_res_total.csv")


# Dynacof
uncer_Ra<-read.csv("Uncertainty_Reco_Out_BA.csv"); uncer_Ra=uncer_Ra[-1]
uncer_GPP<- read.csv("Uncertainty_GPP_Out_BA.csv");uncer_GPP<-uncer_GPP[-1]
mean_GPP<-apply(uncer_GPP, 2, mean);mean_GPP<-mean_GPP*0.01
sd_GPP<- apply(uncer_GPP, 2, sd);sd_GPP<-sd_GPP*0.01
mean_Ra<-apply(uncer_Ra, 2, mean);mean_Ra<-mean_Ra*0.01
sd_Ra<- apply(uncer_Ra, 2, sd);sd_Ra<- sd_Ra*0.01

t<-rbind(mean_Ra,sd_Ra,mean_GPP,sd_GPP)
uncert_data<-as.data.frame(t)
uncert_data2<- as.data.frame(t(uncert_data),row.names = T)
uncert_data2$years<-seq(from=2012, to=2020)
uncert_data2$per_sd_Ra<-(uncert_data2$sd_Ra*100)/uncert_data2$mean_Ra
uncert_data2$per_sd_GPP<-(uncert_data2$sd_GPP*100)/uncert_data2$mean_GPP
write.csv(uncert_data2,"uncertainty_res_total_tree.csv",row.names = F)
read.csv("uncertainty_res_total_tree.csv")

#-----Combine error
uncert_data=read.csv("uncertainty_res_total.csv")
uncert_data_tree = read.csv("uncertainty_res_total_tree.csv")
library(errors)
ER_GPP = mean(uncert_data$sd_GPP)
ER_GPP_Tree = mean(uncert_data_tree$sd_GPP)

combined_error <- sqrt(ER_GPP^2+ ER_GPP_Tree^2)

