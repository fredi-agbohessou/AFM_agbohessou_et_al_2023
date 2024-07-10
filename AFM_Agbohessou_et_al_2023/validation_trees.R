# validation tree
# dynacof1 = Balanites aegyptiaca (dynacofcof simulation dataframe)
# dynacof2 = Acacia raddiana     (dynacofcof simulation dataframe)

#### leaf biomass---------------------
# B. aegyptiaca

# leaf biomass
dynacof1$Biomass_leaf_kg_tree = dynacof1$DM_Leaf_Tree*10
plot(dynacof1$Biomass_leaf_kg_tree)

# Measurement biomass 
#meas.mean = 122.97
meas.Hier = 162.45
meas.Ciss = 112.90
meas.Cissb = 93.58

plt_1= ggplot(dynacof1, aes(x = ymd(Date),y= Biomass_leaf_kg_tree))+
  #geom_hline(yintercept = meas.mean ,size=0.4,linetype=1,colour="green")+
  geom_hline(yintercept = meas.Hier ,size=0.3,linetype=1,colour="red")+
  geom_hline(yintercept = meas.Ciss ,size=0.3,linetype=1,colour="blue")+
  geom_hline(yintercept = meas.Cissb ,size=0.3,linetype=2,colour="blue")+
  geom_line(size=0.3,color="black")+
  scale_x_date(date_labels="%Y",date_breaks  ="3 year",expand = c(0, 0))+
  scale_y_continuous(breaks=seq(0,300,50),limits = c(0, NA))+
  ylab(expression(atop(paste("B. aegyptiaca leaf"),paste("biomass (kgDM"," ha"^"-1",")"))))+
  xlab("Date")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.Hier+15, label = "Hiernaux et al., 2022",colour="red",size=2.5,family="Candara")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.Ciss+15, label = "Cissé 1980",colour="blue",size=2.5,family="Candara")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.Cissb+15, label = "Cissé 1980",colour="blue",size=2.5,family="Candara")+
  theme_classic()+
  theme(panel.grid.major= element_line(linetype=5,size = .2),
        text = element_text(family="Candara"),# 
        panel.border = element_rect(color = "black",fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.y=element_text(size = 8,face="italic"),
        axis.text.y=element_text(size = 6),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()
          #axis.ticks.x=element_blank(),
  )
plt_1  

# A. raddiana

# compute DM in kg/tree
dynacof2$Biomass_leaf_kg_tree = (dynacof2$DM_Leaf_Tree*10)
plot(dynacof2$Biomass_leaf_kg_tree)

# Measurement biomass in kg/tree 
meas.Hier2 = 102.80
meas.Ciss2 = 167.16

plt_2= ggplot(dynacof2, aes(x = ymd(Date),y= Biomass_leaf_kg_tree))+
  geom_hline(yintercept = meas.Hier2,size=0.3,linetype=1,colour="red")+
  geom_hline(yintercept = meas.Ciss2,size=0.3,linetype=1,colour="blue")+
  geom_line(size=0.3,color="black")+
  scale_x_date(date_labels="%Y",date_breaks  ="3 year",expand = c(0, 0))+
  scale_y_continuous(breaks=seq(0,380,50), limits = c(0, NA))+
  ylab(expression(atop(paste("A. raddiana leaf"),paste(" biomass (kg DM"," ha"^"-1",")"))))+
  xlab("Date")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.Hier2+12, label = "Hiernaux et al., 2022",colour="red",size=2.5,family="Candara")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.Ciss2+12, label = "Cissé 1980",colour="blue",size=2.5,family="Candara")+
  theme_classic()+
  theme(panel.grid.major= element_line(linetype=5,size = .2),
        text = element_text(family="Candara"),#
        panel.border = element_rect(color = "black",fill = NA),
        axis.text = element_text(colour = "black"),
        #axis.text.x=element_text(size = 6),
        axis.text.y=element_text(size = 6),
        axis.title=element_text(size = 8),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()
  )
plt_2

# ts = (plt_1/plt_2)+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
# ts
# png("2-outputs/Output/Tree_leaf_biomass.png",  width=17, height=10,units='cm',res = 600)
# ts
# dev.off()


####- wood biomass (branch+stem)--------
# B. aegyptiaca
dynacof1$DM_wood = dynacof1$DM_Branch_Tree + dynacof1$DM_Stem_Tree

# compute DM in kg/tree
dynacof1$Biomass_wood_kg_tree = (dynacof1$DM_wood*10)
plot(dynacof1$Biomass_wood_kg_tree,type="l")

# Measurement biomass kg/tree
meas.poupon = 4239.05
meas.Hier3 = 3690.23

plt_3= ggplot(dynacof1, aes(x = ymd(Date),y= Biomass_wood_kg_tree))+
  geom_hline(yintercept = meas.Hier3 ,size=0.3,linetype=1,colour="red")+
  geom_hline(yintercept = meas.poupon ,size=0.3,linetype=1,colour="blue")+
  geom_line(size=0.3,color="black")+
  scale_x_date(date_labels="%Y",date_breaks  ="3 year",expand = c(0, 0))+
  scale_y_continuous(limits = c(0, NA),breaks=seq(0,9000,1500))+
  ylab(expression(atop(paste("B. aegyptiaca wood"),paste("biomass (kg DM"," ha"^"-1",")"))))+
  xlab("Date")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.Hier3-400, label = "Hiernaux et al., 2022",colour="red",size=2.5,family="Candara")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.poupon+400, label = "Poupon 1980",colour="blue",size=2.5,family="Candara")+
  theme_classic()+
  theme(panel.grid.major= element_line(linetype=5,size = .2),
        text = element_text(family="Candara"),#
        panel.border = element_rect(color = "black",fill = NA),
        axis.text = element_text(colour = "black"),
        axis.title.y=element_text(size =8,face="italic"),
        axis.text.y=element_text(size = 6),
        axis.title.x=element_blank(),
        axis.text.x=element_blank()
        #axis.ticks.x=element_blank(),
  )
plt_3

# A. raddiana
dynacof2$DM_wood = dynacof2$DM_Branch_Tree + dynacof2$DM_Stem_Tree

# compute DM in kg/tree
dynacof2$Biomass_wood_kg_tree = (dynacof2$DM_wood*10)
plot(dynacof2$Biomass_wood_kg_tree,type="l")

# Measurement biomass in kg/tree using equation allo
meas.mean4= 1832.23

plt_4= ggplot(dynacof2, aes(x = ymd(Date),y= Biomass_wood_kg_tree))+
  geom_hline(yintercept = meas.mean4 ,size=0.3,linetype=1,colour="red")+
  geom_line(size=0.3,color="black")+
  scale_x_date(date_labels="%Y",date_breaks  ="3 year",expand = c(0, 0))+
  scale_y_continuous(limits = c(0, NA),breaks=seq(0,9000,1000))+
  ylab(expression(atop(paste("A. raddiana wood"),paste("biomass (kg DM"," ha"^"-1",")"))))+
  xlab("Date")+
  annotate("text", x = as.Date("1993-04-01"), y = meas.mean4+300, label = "Hiernaux et al., 2022",colour="red",size=2.5,family="Candara")+
  theme_classic()+
  theme(panel.grid.major= element_line(linetype=5,size = .2),
        text = element_text(family="Candara"),#
        panel.border = element_rect(color = "black",fill = NA),
        axis.text = element_text(colour = "black"),
        #axis.text.x=element_text(size = 6),
        axis.text.y=element_text(size = 6),
        axis.title=element_text(size = 8)
  )
plt_4

# ts = (plt_3/plt_4)+ theme(plot.margin = unit(c(0,0,0,0), "cm"))
# ts
# png("2-outputs/Output/Tree_wood_biomass.png",  width=17, height=10,units='cm',res = 600)
# ts
# dev.off()


ts2= ((plt_1+theme(plot.tag.position  = c(.13, .91)))/
      (plt_2+theme(plot.tag.position  = c(.13, .91)))/
      (plt_3+theme(plot.tag.position  = c(.13, .91)))/
      (plt_4+theme(plot.tag.position  = c(.13, .91)))
      )+ theme(plot.margin = unit(c(0,0,0,0), "cm"))+ 
  plot_annotation(tag_levels = "a", tag_prefix = '(',tag_suffix = ')')+
  plot_layout(heights = c(1,1,1,1))
ts2

tiff("2-outputs/Output/Tree_biomass.tif",  width=17, height=18,units='cm',res = 300)
ts2
dev.off()
