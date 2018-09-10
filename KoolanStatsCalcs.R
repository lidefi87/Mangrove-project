#Clear workspace
rm(list = ls())

#Upload library - if this fails, install it by typing: install.packages("plyr")
library(plyr)
library(ggplot2)
library(png)

#Set working directory - address of the folder where your data is kept. Update Folder!!!!
setwd("Y:/MTG - Mt Gibson/MTG1802_Dewatering monitoring/Reporting/Compliance Reporting/WeeklyReport5_26Aug-1Sep2018") 
#outputs will be saved in this folder

###################################### TURBIDITY DATA #########################################################
#Uploading turbidity data to be analysed - see template for data formatting, must be csv file
#Update this name to whatever the name of your file is
Turb = read.csv("TurbidityData.csv")
Turb = transform(Turb, Date = as.Date(Date)) #format date to exclude time

#Calculating 5th percentile, median and 95th percentiles 
Calcs = ddply(Turb,.(Date,SiteName), summarise, per5th = quantile(Turbidity, probs = c(0.05)), 
              median = median(Turbidity,na.rm = T), per95th = quantile(Turbidity, probs = c(0.95)))

#Save calculations
write.csv(Calcs, "ResultsTurb.csv")

# #Extract SC-1 data
# SC1 = Turb[c(which(Turb$SiteName=="SC1")),]
# SC1$Date = as.factor(SC1$Date)

# #How to create an inset figure explaining different parts of the boxplot
# d1 <- data.frame(x = 1, y = c(1:1000, 1502))
# d2 <- data.frame(
#   y = c(boxplot.stats(d1$y)$stats, 1502),
#   x = 1,
#   label = c('min', '1st quartile', 'median', '3rd quartile', 'max', 'outlier'))
# plot1 <- ggplot(d1, aes(x, y)) + geom_boxplot(width = 0.2) +
#   geom_text(aes(x = 1.15, label = label), d2, hjust = 0) +
#   xlim(0.9, 1.5) +
#   theme_void() + theme(panel.background = element_rect(fill = 'white', color = 1))
# plot1 = plot1 + annotation_custom(ggplotGrob(leg), xmin = 3, xmax = 3.5, ymin = 25, ymax = 35)
# ggsave(filename = "boxplot.png", dpi = 100)
# BP = readPNG('boxplot.png')

# ggplot(SC1,aes(x=Date,y=Turbidity))+geom_boxplot()+theme_bw()+labs(x = "",y="Turbidity (NTU)")+ylim(0,(round(max(SC1$Turbidity),1))+2)+
#   theme(axis.text.x=element_text(angle=90),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
#         axis.text = element_text(size = 14),axis.title = element_text(size = 12))+
#   geom_hline(yintercept=4,linetype="dashed",color="darkorange3")+
#   annotate("text",x=4, y=4.5, label="4 NTU = Trigger for checking impact against reference data",size = 5.5)+
#   geom_hline(yintercept=6,linetype="dashed",color="darkred")+
#   annotate("text", x=4, y=6.5, label="6 NTU = Trigger for diversion to sedimentation pond",size = 5.5)#+
# # annotation_raster(BP, ymin = 6.25,ymax= 12,xmin = 6,xmax = 9)
# ggsave("TurbiditySC1.tiff",plot = last_plot(), width = 333, height = 209, units = "mm")

#################################### DISSOLVED 02 DATA #####################################################
#Uploading dissolved O2 data to be analysed - see template for data formatting, must be csv file
#Update this name to whatever the name of your file is
DO2 = read.csv("DissO2Data.csv")
DO2 = transform(DO2, Date = as.Date(Date)) #format date to exclude time

#Calculating 5th percentile, median and 95th percentiles 
Calcs = ddply(DO2,.(Date,Site), summarise, median = median(DissO2,na.rm = T))

#Save calculations
write.csv(Calcs, "ResultsDO2.csv")

# #Extract SC-1 data
# SC1 = DO2[c(which(DO2$Site=="SC1")),]
# SC1$Date = as.factor(SC1$Date)
# 
# ggplot(SC1,aes(x=Date,y=DissO2))+geom_boxplot()+theme_bw()+labs(x = "",y="Dissolved Oxygen Saturation (%)")+
#   ylim(70,120)+theme(axis.text.x=element_text(angle=90),panel.grid.major=element_blank(),
#                      panel.grid.minor=element_blank(),axis.text = element_text(size = 14),
#                      axis.title = element_text(size = 12))+
#   geom_hline(yintercept=80,linetype="dashed",color="darkorange3")+
#   annotate("text",x=4, y=81.75, label="<80% = trigger",size = 5.5)
# ggsave("DissO2SC1.tiff",plot = last_plot(), width = 333, height = 209, units = "mm")

# #################################### TEMPERATURE DATA #####################################################
# #Uploading temperature data to be analysed - see template for data formatting, must be csv file
# #Update this name to whatever the name of your file is
# Temp = read.csv("TempDataCS1.csv")
# Temp = transform(Temp, Date = as.Date(Date)) #format date to exclude time
# Temp$Date = as.factor(Temp$Date)
# 
# ggplot(Temp,aes(x=Date,y=Temp))+geom_boxplot()+theme_bw()+labs(x = "",y="Temperature (°C)")+
#   ylim((round(min(Temp$Temp),1))-2,(round(max(Temp$Temp),1))+2)+
#   theme(axis.text.x=element_text(angle=90),panel.grid.major=element_blank(),
#                      panel.grid.minor=element_blank(),axis.text = element_text(size = 14),
#                      axis.title = element_text(size = 12))+
#   geom_hline(yintercept=26.88,linetype="dashed",color="darkorange3")+ #y intercept needs to be adjusted to latest value
#   annotate("text",x=4, y=26.70, label="+ 5% change from baseline",size = 5.5)+
#   geom_hline(yintercept=23.028,linetype="dashed",color="darkorange3")+ #y intercept needs to be adjusted to latest value
#   annotate("text",x=4, y=23.30, label="- 5% change from baseline",size = 5.5)+
#   annotate("text",x=1.5, y=22.25, label="Note: trigger is the 5th/95th percentile of reference site",size = 4.5)
# ggsave("TempSC1.tiff",plot = last_plot(), width = 333, height = 209, units = "mm")

# #################################### CONDUCTIVITY DATA #####################################################
# #Uploading conductivity data to be analysed - see template for data formatting, must be csv file
# #Update this name to whatever the name of your file is
# Cond = read.csv("CondDataCS1.csv")
# Cond = transform(Cond, Date = as.Date(Date)) #format date to exclude time
# Cond$Date = as.factor(Cond$Date)
# 
# ggplot(Cond,aes(x=Date,y=Cond))+geom_boxplot()+theme_bw()+labs(x = "",y="Conductivity (µS/cm)")+
#   ylim(48750,53000)+theme(axis.text.x=element_text(angle=90),panel.grid.major=element_blank(),
#                     panel.grid.minor=element_blank(),axis.text = element_text(size = 12),
#                     axis.title = element_text(size = 12))+
#   geom_hline(yintercept=50718.16,linetype="dashed",color="darkorange3")+ #y intercept needs to be adjusted to latest value
#   annotate("text",x=4, y=50950, label="+ 1% change from baseline",size = 5.5)+
#   geom_hline(yintercept=49224.7,linetype="dashed",color="darkorange3")+ #y intercept needs to be adjusted to latest value
#   annotate("text",x=4, y=49125, label="- 1% change from baseline",size = 5.5)+
#   annotate("text",x=1.75, y=48775, label="Note: trigger is the 5th/95th percentile of reference site",size = 4.5)
# ggsave("CondSC1.tiff",plot = last_plot(), width = 333, height = 209, units = "mm")
