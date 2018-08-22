################################################################################################################################################
######################################## Analysis of Variance Reflectance Values Landsat images ################################################
#Version: 2
#Date: 2018/07/11
#Author: Denisse Fierro Arcos
#Objectives: 
#Use at-surface-reflectance values from atmospherically corrected Landsat 5, 7 & 8 images to perform non-parametric
#analysis of variance in mean reflectance values over time and between river sections.
################################################################################################################################################

#Clear workspace
rm(list=ls())

#Upload relevant libraries
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)

#Set working directory - User defined (location of main folder containing Landsat images)
setwd("T:/GIS_BHP1704/Doce water colour baseline/Satellite Images Rio Doce basin/")

#Find names for each Landsat scene
Scenes = list.dirs(recursive = F)[c(which(grepl("P21",list.dirs(recursive = F))))]
#Find folders within each Landsat scene
for(i in 1:(length(Scenes)-1)){
  if(i == 1){FilePaths = list.dirs(path = Scenes[i],recursive = F)
  }else{FilePaths = append(FilePaths, list.dirs(path = Scenes[i],recursive = F))}}
FilePaths = FilePaths[-which(grepl("Done",FilePaths))] #removing "Done" folder, which does not contain Landsat images
rm(Scenes)

#Years included in the study
Years = str_pad(c(2015:2018), 5, pad = "*") #Set the range of years to be analysed (e.g., 2015 - 2018)
#Creating strings for each seasons:
#Dry/Winter (Cool & dry) - April to September
DryMonths = str_pad(c(4:9), 2, pad = 0)
#Wet/Summer (Warm & wet) - October to March
WetMonths = str_pad(c(10:12,1:3), 2, pad = 0)
for(i in seq_along(Years)){
  assign(paste0("Dry",str_sub(Years[i],start = 2)), paste0(Years[i],DryMonths))
  assign(paste0("Wet",str_sub(Years[i],start = 2)), paste0(c(rep.int(Years[i],times = 3),rep.int(Years[i+1],times = 3)),WetMonths))}
SeasonsYr = paste0(rep(c("Dry","Wet"), each = length(Years)),str_sub(Years,start = 2))[c(2:7)]
rm(Years,DryMonths,WetMonths,Wet2018,Dry2015) # Removing unnecessary variables

#Ordering sites by location - Choose AllSites if analysing all data points
# AllSites=data.frame(names=c("Imp - Bento Ck","Ref - US RGdN","Imp - RGdN","Ref - RdCarmo",
#                             "Imp - RdCarmo to Piranga","Ref - R Piranga","Imp - RD Piranga to Candonga",
#                             "Imp - Candonga","Imp - RD ds Candonga to R Matipo","Ref - R Matipo",
#                             "Imp - RD Matipo to Piracicaba","Ref - Piracicaba","Imp - RD Piracicaba to SA",
#                             "Ref - Santo Antonio","Imp - RD SA to Saucui Grande","Ref - R Suacui Grande",
#                             "Imp - RD SG to Aimores","Ref - R Guandu","Imp - RD Mascarenhas",
#                             "Imp - RD Mascarenhas to Mouth","Ocean N","Ocean NE","Ocean E","Ocean SE",
#                             "Ocean S"))
# AllSites$order=c(1:nrow(AllSites))

#Classification of areas of interest along Rio Doce - Reference areas has been merged together
SitesGraphs=data.frame(names=c("Reference Sites","Imp - Bento Ck","Imp - RGdN","Imp - RdCarmo to Piranga",
                               "Imp - RD Piranga to Candonga","Imp - Candonga",
                               "Imp - RD ds Candonga to R Matipo","Imp - RD Matipo to Piracicaba",
                               "Imp - RD Piracicaba to SA","Imp - RD SA to Saucui Grande",
                               "Imp - RD SG to Aimores","Imp - RD Mascarenhas",
                               "Imp - RD Mascarenhas to Mouth","Ocean"))
#Order of areas of interest to be used in plots
SitesGraphs$order=c(1:nrow(SitesGraphs))

#Name of bands of interest
BandNames=c("Blue","Green","Red","NIR")

#Describing a function that creates datasets by year and per season
DatasetCreation = function(seasons){
  #Create a list of filepaths of images to be included per season and year
  for(i in seq_along(get(seasons))){if(i == 1){x = FilePaths[which(grepl(get(seasons)[i],FilePaths))]
  }else{x = append(x,FilePaths[which(grepl(get(seasons)[i],FilePaths))])}}
  #Read csv file containing reflectance values extracted from Landsat images - only RGB and NIR bands are uploaded
  for(i in seq_along(x)){if(grepl("L8",x[i])){y = read.csv(paste0(x[i],"/surfref_Water.csv"))[,c(2,4:7)]
  colnames(y)[2:ncol(y)] = BandNames #change column names to merge datasets
  }else{y = read.csv(paste0(x[i],"/surfref_Water.csv"))[,c(2:6)]
  colnames(y)[2:ncol(y)] = BandNames}
    if(i == 1){z = y}else{z = rbind(z, y)}}
  z = z[-which(is.na(rowSums(z[,c(2:ncol(z))]))),] #merging datasets
  levels(z$Name)= c(levels(z$Name),"Reference Sites", "Ocean") #include two extra factor levels to change names of areas of interest
  #Rename areas of interest - pooling Reference and Ocean sites together
  z$Name = replace(z$Name, grepl("Ref*",z$Name),"Reference Sites")
  z$Name = replace(z$Name, grepl("Ocean*",z$Name),"Ocean")
  #Include cell with the order that each AOI should appear in graphs
  z$Order = str_pad(match(z$Name,SitesGraphs$names),2,pad="0")
  #Merge order and name columns
  z = unite(z,Name,Order,Name,sep="_")}

#Apply DatasetCreation function to each season/year group included in the study
for(i in seq_along(SeasonsYr)){assign(paste0(SeasonsYr[i],"pts"), DatasetCreation(SeasonsYr[i]))}

#Boxplots using boxplot (R base function)
par(mfrow = c(2,2))
for(i in 2:ncol(Dry2016pts)){if(i < 4){boxplot(Dry2016pts[,i] ~ Dry2016pts$Name, ylab = "At-surface reflectance", xaxt = "n")
  }else{boxplot(Dry2016pts[,i] ~ Dry2016pts$Name, ylab = "At-surface reflectance", las = 2)}
  mtext(side=3, text=colnames(Dry2016pts)[i], line = 2)}

#Boxplots using ggplot
for(i in 2:ncol(Dry2016pts)){
  ggplot(Dry2016pts,aes(x = Dry2016pts$Name, y = Dry2016pts[,i], fill = Name))+theme_bw()+geom_boxplot(na.rm = T)+
  labs(x = "River section", y = "At-surface reflectance", title = paste0(SeasonsYr[1]," - ",
                                                                         colnames(Dry2016pts)[i], " Band"), 
       caption = "Sites ordered from upstream to downstream")+guides(fill = guide_legend(ncol = 3))+
    ylim(0,round(max(Dry2016pts[,c(2:(ncol(Dry2016pts)-1))]),1)+0.1)+
    theme(axis.text.x = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        legend.text = element_text(size = 9), legend.direction = "horizontal", legend.position = c(1,1),
        legend.justification = c(1,1), legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank())
  ggsave(paste0("Dry2016",colnames(Dry2016pts)[i],".pdf"))}

x = ls(pattern = glob2rx("Dry*pts"))
for(i in seq_along(x)){
  y = get(x[i])
  y$year = str_sub(x[i],start = 4,end = -4)
  if(i == 1){Dry = y}else{Dry = rbind(Dry,y)}}

x = ls(pattern = glob2rx("Wet*pts"))
for(i in seq_along(x)){
  y = get(x[i])
  y$year = str_sub(x[i],start = 4,end = -4)
  if(i == 1){Wet = y}else{Wet = rbind(Wet,y)}}

# BlueDry = aov(Blue ~ year, data = Dry)
# summary(BlueDry)
# BlueDryPH = TukeyHSD(BlueDry)
# boxplot(Blue ~ year, data = Dry)

par(mfrow = c(2,2))
for(i in 2:(ncol(Dry)-1)){boxplot(Dry[,i] ~ Dry$year, data = Dry)
  mtext(side=3, text=colnames(Dry)[i], line = 2)}

par(mfrow = c(2,2))
for(i in 2:(ncol(Wet)-1)){boxplot(Wet[,i] ~ Wet$year)
  mtext(side=3, text=colnames(Wet)[i], line = 2)}



    

