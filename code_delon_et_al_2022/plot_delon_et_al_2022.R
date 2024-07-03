# Plots in Nitrogen budget and critical load determination at a Sahelian grazed grassland site (Delon et al. 2022)
# In Nutr Cycl Agroecosyst https://doi.org/10.1007/s10705-022-10220-6
# y. agbohessou
# 22/07/21
#-----------------------------------------------------------------------

citation()
rm(list=ls())
Sys.setlocale("LC_TIME", "English")#set time


#load needed packages
for(pkg in c("ggplot2","dplyr","png","lubridate","grid","tidyverse","dygraphs",
             "plyr","gridExtra","tidyr","openxlsx","readr","ggpubr","cowplot")){
  library(pkg, character.only = TRUE)
}


#----------------------------------figure1
#import rain data for 2014 & 2017
data <- read_csv("00Rain_artl.csv")
attach(data)

#Plot Rain 2014 & 2017
df2 <- data %>%
  select(Date,Rain_2014,Rain_2017) %>%
  gather(key = "variable", value = "value", -Date)
head(df2, 3)
p <- ggplot(df2, aes(x = ymd(Date), y=value)) + 
  geom_point(shape = 1,aes(color = variable), size = 3)+
  geom_line(aes(color = variable), size = 1)+
  scale_color_manual(labels = c("2014","2017"),
                     values = c("black","peru")) +
  scale_x_date(date_labels="%B",date_breaks  ="1 month")+ #%B to get the whole name of the months
  scale_y_continuous(breaks=seq(0,200,20))+
  theme_classic()+ylab("Rain (mm)")+xlab("Month of the year")+
  theme(legend.title=element_blank(),legend.text=element_text(size=9),
        axis.text = element_text(colour = "black"),
        axis.title=element_text(size=11, face = 'italic'),
        legend.position = c(0.90, 0.90),
        legend.background = element_rect(fill="transparent"))#add colour ="black" to get a frame
p

#import rain 2014 and 2017 separately
Rain2014 <- read_csv("Rain2014.csv")
Rain2017 <- read_csv("Rain2017.csv")
attach (Rain2014)
attach (Rain2017)

#Plot Rain 2014
p1 <- ggplot(data = Rain2014, aes(x = Date, y = Rain)) +
  geom_bar(stat = "identity", fill = "blue",width = 0.5) +
  labs(x = "Date", y = "Rain (mm)")+
  scale_x_date(date_labels="%d/%m/%y",date_breaks  ="2 week")+
  scale_y_continuous(breaks=seq(0,80,10),limits=c(0, 80))+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"),
        axis.title=element_text(size=11, face = 'italic'),
        axis.text.x = element_text(angle=45, hjust = 1))+
  geom_text(aes(x=as.Date("2014-06-02"), label="2014", y=80), colour="black", 
            angle=0, vjust = 1, size=4)
p1

#Plot Rain 2017
p2 <- ggplot(data = Rain2017, aes(x = Date, y = Rain)) +
  geom_bar(stat = "identity", fill = "blue",width = 0.5) +
  labs(x = "Date", y = "")+
  scale_x_date(date_labels="%d/%m/%y",date_breaks  ="2 week")+
  scale_y_continuous(breaks=seq(0,80,10),limits=c(0, 80))+
  theme_classic()+
  theme(axis.text = element_text(colour = "black"),
        axis.title=element_text(size=11, face = 'italic'),
        axis.text.x = element_text(angle=45, hjust = 1))+#+guides( y = "none")
  geom_text(aes(x=as.Date("2017-06-26"), label="2017", y=80), colour="black", 
            angle=0, vjust = 1, size=4)
p2

#Combine p, p1 and p2
pp1<-ggarrange(p,                                                 # First row with scatter plot
               ggarrange(p1, p2, ncol = 2, labels = c("B", "C")), # Second row with box and dot plots
               nrow = 2, 
               labels = "A"                                        # Labels of the scatter plot
) 
pp1
ggsave(file="Figure1.tiff", units="in",width=8, height=6, dpi=600, compression = 'lzw',limitsize = FALSE,pp1)


#----------------------------------figure3
#import N inputs/outputs 2014
data2 <- read_csv("in_outputs_2014.csv")
data2 = subset(data2,data2$Year_2014 !="DF")

attach (data2)

#import image for the legend
img <- readPNG("Capture.png")

#order x axis 
data2$Year_2014 <- as.character(data2$Year_2014)#Turn your 'treatment' column into a character vector
data2$Year_2014 <- factor(data2$Year_2014, levels=unique(data2$Year_2014))#Then turn it back into a factor with the levels in the correct order

pplt4 <- ggplot(data=data2, aes(x=Year_2014, y=Values, group=Estimates,fill=Type_data,alpha=Estimates)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  scale_fill_brewer(palette="Dark2")+
  labs(x = "", y = "")+
  theme_classic()+
  theme(legend.position ="none" )+#c(0.10, 0.67)
  scale_y_continuous(breaks=seq(0,35,5),limits=c(0, 35))+
  theme(axis.text = element_text(colour = "black"))+
  theme(axis.title=element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1))+#,
  theme(legend.title = element_blank(),legend.background = element_rect(fill="transparent")) +
  geom_text(aes(x=1, label="2014", y=35), colour="black", 
            angle=0, vjust = 1, size=4)+
  scale_x_discrete(limits = c("BNF", "Dry_dep", "Wet_dep","Manure", "N2O_emission", 
                              "NO_emission","NH3_vol","BB","Leaching",
                              "Tree_uptake","Animal_ingestion"),
                   labels = c(expression("BNF"),expression("Dry dep"), expression("Wet dep"),expression("Manure"),expression("N"[2]*"O emission"),
                              expression("NO emission"),expression("NH"[3]*" vol"),expression("BB"),
                              expression("Leaching"),expression("Tree uptake"),expression("Animal ingestion")))+
  annotation_custom(rasterGrob(img, width = 1, height = 1),
                    xmin = 1.5, xmax = 6,
                    ymin = 32, ymax = 36)
pplt4

#import sum N inputs/outputs 2014
#require(openxlsx)
#data3 <- read.csv("Sum_in_outputs_2014.csv")
data3 <- openxlsx::read.xlsx("Sum_in_outputs_2014.xlsx")
attach(data3)

data3$data <- as.character(data3$data)
data3$data <- factor(data3$data, levels=unique(data3$data))

pplt5 <- ggplot(data=data3, aes(x=data, y=Estimate, group=Type_data,fill=data,alpha=Type_data)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  scale_fill_brewer(palette="Dark2")+
  labs(x = "", y = "")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(0,50,10),limits=c(0, 50))+
  theme(axis.text = element_text(colour = "black"))+
  theme(axis.title=element_text(size=11, face = 'italic'))+
  theme(legend.title = element_blank())+
  theme(legend.position= "none") 
pplt5

#Combine pplt4 & pplt5
pp2014=ggdraw() +
  draw_plot(pplt4) +
  draw_plot(pplt5, x = 0.60, y = .59, width = .30, height = .35)
pp2014

#import N inputs/outputs 2017
data4 <- read.csv("in_outputs_2017.csv")
data4 = subset(data4,data4$Year_2017 !="DF")
attach(data4)

#order x axis 
data4$Year_2017 <- as.character(data4$Year_2017 )
data4$Year_2017  <- factor(data4$Year_2017 , levels=unique(data4$Year_2017))

pplt6 <- ggplot(data=data4, aes(x=Year_2017, y=Values, group=Estimates,fill=Type_data,alpha=Estimates)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  scale_fill_brewer(palette="Dark2")+
  labs(x = "", y = "")+
  theme_classic()+
  theme(legend.position = "none")+ #c(0.10, 0.78)
  scale_y_continuous(breaks=seq(0,35,5),limits=c(0, 35))+
  theme(axis.text = element_text(colour = "black"))+
  theme(axis.title=element_text(size=11),
        axis.text.x = element_text(angle=45, hjust=1))+
  theme(legend.title = element_blank(),legend.background = element_rect(fill="transparent"))+
  geom_text(aes(x=1, label="2017", y=35), colour="black", 
            angle=0, vjust = 1, size=4)+
  scale_x_discrete(limits = c("BNF", "Dry_dep", "Wet_dep","Manure", "N2O_emission", 
                              "NO_emission","NH3_vol","BB","Leaching",
                              "Tree_uptake","Animal_ingestion"),
                   labels = c(expression("BNF"),expression("Dry dep"), expression("Wet dep"),expression("Manure"),expression("N"[2]*"O emission"),
                              expression("NO emission"),expression("NH"[3]*" vol"),expression("BB"),
                              expression("Leaching"),expression("Tree uptake"),expression("Animal ingestion")))+
  annotation_custom(rasterGrob(img, width = 1, height = 1),
                    xmin = 1.5, xmax = 6,
                    ymin = 32, ymax = 36)

pplt6

#import sum N inputs/outputs 2017
data5 <- openxlsx::read.xlsx("Sum_in_outputs_2017.xlsx")

attach(data5)

#order x axis 
data5$data <- as.character(data5$data )
data5$data  <- factor(data5$data , levels=unique(data5$data ))

pplt7 <- ggplot(data=data5, aes(x=data, y=Estimate, group=Type_data,fill=data,alpha=Type_data)) +
  geom_bar(stat="identity", position=position_dodge(),colour="black")+
  scale_fill_brewer(palette="Dark2")+
  labs(x = "", y = "")+
  theme_classic()+
  theme(legend.position = "none")+
  scale_y_continuous(breaks=seq(0,50,10),limits=c(0, 50))+
  theme(axis.text = element_text(colour = "black"))+
  theme(axis.title=element_text(size=11, face = 'italic'))+
  theme(legend.title = element_blank())+
  theme(legend.position= "none") 
pplt7

#Combine pplt6 and pplt7
pp2017=ggdraw() +
  draw_plot(pplt6) +
  draw_plot(pplt7, x = 0.60, y = .59, width = .30, height = .35)
pp2017

#Combine pp2014 and pp2017
PLOT2=grid.arrange(pp2014, pp2017,
                   ncol=1, nrow=2,
                   left=textGrob(expression(paste("N inputs and outputs (kg N ha"^"-1"," yr "^"-1",")")),
                                 x=1.2,rot=90, hjust=0.38,gp = gpar(fontsize=12, fontface = "italic",col="black")))

                                 
ggsave(file="Figure3.tiff", units="in",width=7, height=7, dpi=600, compression = 'lzw',limitsize = FALSE,PLOT2)


#----------------------------------figure4

#import total N inputs/outputs with sd
data6 <- read.csv("sd_in_output.csv")
data6 = subset(data6,data6$Average !="DF")
attach(data6)

#order x axis 
data6$Average <- as.character(data6$Average)
data6$Average <- factor(data6$Average , levels=unique(data6$Average))

p8 <- ggplot(data=data6, aes(x=Average, y=Mean, fill=Type_data)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Dark2")+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.2,
                position=position_dodge(.9))+
  labs(x = expression(paste("")), y = expression(paste("N inputs and outputs (kg N ",  ha^{-1},yr^{-1},")")))+
  theme_classic()+
  theme(legend.position = c(0.1, 0.89))+
  scale_y_continuous(breaks=seq(0,40,5),limits=c(0, 40))+
  theme(axis.text = element_text(colour = "black"))+
  theme(axis.title=element_text(size=11, face = 'italic'),
        axis.text.x = element_text(angle=45, hjust = 1))+
  theme(legend.title = element_blank())+
  scale_x_discrete(limits = c("BNF", "Dry_dep", "Wet_dep","Manure", "N2O_emission", 
                              "NO_emission","NH3_vol","BB","Leaching",
                              "Tree_uptake","Animal_ingestion"),
                   labels = c(expression("BNF"),expression("Dry dep"), expression("Wet dep"),expression("Manure"),expression("N"[2]*"O emission"),
                              expression("NO emission"),expression("NH"[3]*" vol"),expression("BB"),
                              expression("Leaching"),expression("Tree uptake"),expression("Animal ingestion")))
p8

#import sum total N inputs/outputs with sd
data7 <- read.csv("sum_sd_in_output.csv")
attach(data7)

#order x axis 
data7$Type_data <- as.character(data7$Type_data )
data7$Type_data<- factor(data7$Type_data, levels=unique(data7$Type_data))

p9 <- ggplot(data=data7, aes(x=Type_data, y=Mean, fill=Type_data)) +
  geom_bar(stat="identity", position=position_dodge())+
  scale_fill_brewer(palette="Dark2")+
  geom_errorbar(aes(ymin=Mean-Std, ymax=Mean+Std), width=.2,
                position=position_dodge(.9))+
  labs(x = "", y = "")+
  theme_classic()+
  theme(legend.position = c(0.89, 0.89))+
  scale_y_continuous(breaks=seq(0,60,10),limits=c(0, 60))+
  theme(axis.text = element_text(colour = "black"))+
  theme(axis.title=element_text(size=11, face = 'italic'))+
  theme(legend.title = element_blank())+
  theme(legend.position= "none") 
p9

#Combine p8 and p9
p15=ggdraw() +
  draw_plot(p8) +
  draw_plot(p9, x = 0.45, y = .50, width = .35, height = .45)
p15
ggsave(file="Figure4.tiff", units="in",width=7, height=4, dpi=600, compression = 'lzw',limitsize = FALSE,p15)



###################################################### a function to center the names of i axis elements
addline_format <- function(x,...){
  gsub('\\s','\n',x)
}

myplot + 
  scale_x_discrete(breaks=unique(df_M$variable), 
                   labels=addline_format(c("Ambystoma mexicanum", "Psychrolutes marcidus")))
                   
                   