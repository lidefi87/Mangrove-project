#Linear model - Turbidity vs Tidal State

#Clear workspace
rm(list=ls())

#Set working directory
setwd("Y:/MTG - Mt Gibson/MTG1802_Dewatering monitoring/MTG - Turbidity/Water Quality")

#Upload libraries
library(ggplot2)

#Upload data
tides=read.csv("MTG_RegCor.csv")

#Remove Flood13 and Ebb13 data points (1 and 2 points only)
tides=tides[-c(which(tides$Tide.State=="Flood13" | tides$Tide.State == "Ebb13")),]

#Create a customised order for Tidal States categories - Ebb/Low/Flood/High
Lvl=c(levels(tides$Tide.State)[1:12],"Low",levels(tides$Tide.State)[14:25],"High")
#Delete factors in Tidal State column
tides$Tide.State=as.vector(tides$Tide.State)
#Organise factors in Tidal State column using the customised categories
tides$Tide.State=factor(tides$Tide.State,Lvl)

#Clear any graphs on workspace
dev.off()

#Calculating 99 percentiles for each tide state
Q99=ddply(tides,.(Tide.State),summarise,q99=quantile(Turbidity,probs=0.99))

#Plot data using a box plot
ggplot(tides,aes(x=Tide.State,y=Turbidity))+geom_boxplot()+theme_bw()+labs(x="Tide State",y="Turbidity (NTU)")+
  theme(axis.text.x=element_text(angle=90),panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  geom_line(data=Q99,mapping=aes(x=as.numeric(ordered(Tide.State)),y=q99),color="red",size=0.9)+
  geom_hline(yintercept=4,linetype="dashed",color="darkred")

#Run linear model
LM=lm(Turbidity~Tide.State,data=tides)
summary(LM)


#How to create an inset figure explaining different parts of the boxplot
# d1 <- data.frame(x = 1, y = c(1:1000, 1502))
# d2 <- data.frame(
#   y = c(boxplot.stats(d1$y)$stats, 1502), 
#   x = 1, 
#   label = c('min', '1st quartile', 'median', '3rd quartile', 'max', 'outlier'))
# leg <- ggplot(d1, aes(x, y)) + geom_boxplot(width = 0.2) + 
#   geom_text(aes(x = 1.15, label = label), d2, hjust = 0) +
#   xlim(0.9, 1.5) +
#   theme_void() + theme(panel.background = element_rect(fill = 'white', color = 1))
# plot1 + annotation_custom(ggplotGrob(leg), xmin = 3, xmax = 3.5, ymin = 25, ymax = 35)


