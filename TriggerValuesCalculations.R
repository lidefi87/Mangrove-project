#Clear workspace
rm(list = ls())

#Load relevant libraries
library(plyr)

#Set working directory
setwd("Y:/MTG - Mt Gibson/MTG1803 MMP Revision 2018/Denisse - working/Data")

#Koolan seasons
Wet = c("December", "January", "February", "March", "April")
Dry = c("May", "June", "July", "August", "September", "October", "November")

###################################Water Quality - Physical Parameters#############################################
WQphysall = read.csv("WQPhysical.csv")
WQphysall = transform(WQphysall, Salinity = as.numeric(as.character(Salinity)))

#Extract reference sites only
WQphys = WQphysall[c(which(grepl(glob2rx("*REF*"), WQphysall$Site))),]
WQphys$Site = factor(WQphys$Site)

#Update Seasons column in WQphys
for(i in 1:nrow(WQphys)){
  if(months(as.Date(WQphys$Date[i])) %in% Wet){WQphys$Season[i] = "wet"
  }else if(months(as.Date(WQphys$Date[i])) %in% Dry){WQphys$Season[i] = "dry"
  }else{print("error")}}
WQphys$Season = factor(WQphys$Season)

#Cleaning physical WQ data
#Plot data to identify outliers
for(i in 13:19){plot(WQphys[,i] ~ WQphys$Site, main = colnames(WQphys)[i])}
#Temperature - remove values > 40
for(i in 1:nrow(WQphys)){
  if(is.na(WQphys$Temp[i])){WQphys$Temp[i] = WQphys$Temp[i]
  }else if(WQphys$Temp[i] > 40){WQphys$Temp[i] = NA}}
#Salinity - remove values < 20
for(i in 1:nrow(WQphys)){
  if(is.na(WQphys$Salinity[i])){WQphys$Salinity[i] = WQphys$Salinity[i]
  }else if(WQphys$Salinity[i] < 20){WQphys$Salinity[i] = NA}}
#DO - remove values > 10
for(i in 1:nrow(WQphys)){
  if(is.na(WQphys$DO[i])){WQphys$DO[i] = WQphys$DO[i]
  }else if(WQphys$DO[i] > 10){WQphys$DO[i] = NA}}
#DO% - remove values > 120
for(i in 1:nrow(WQphys)){
  if(is.na(WQphys$DO.[i])){WQphys$DO.[i] = WQphys$DO.[i]
  }else if(WQphys$DO.[i] > 120){WQphys$DO.[i] = NA}}
#Turbidity - change negative values to 0 as negative values represent turbdity is below detection limit
for(i in 1:nrow(WQphys)){
  if(is.na(WQphys$Turbidity[i])){WQphys$Turbidity[i] = WQphys$Turbidity[i]
  }else if(WQphys$Turbidity[i] < 0){WQphys$Turbidity[i] = 0}}
#pH appears stable - no outliers found.

#Calculate minimum daily values for all parameters per site sampled
MinWQP = ddply(WQphys,. (Date, Site_rep, Site, Season, Easting, Northing), summarise, 
               TempMin = min(Temp, na.rm = T), SalMin = min(Salinity, na.rm = T), DOperMin = min(DO., na.rm = T),
               TurbMin = min(Turbidity, na.rm = T), pHMin = min(pH, na.rm = T))
#Change Inf values to NA to facilitate calculations
for(i in 1:nrow(MinWQP)){
  for(j in 7:ncol(MinWQP)){
    if(!is.finite(MinWQP[i,j])){MinWQP[i,j] = NA}}}

#Boxplot of physical WQ parameters
par(mfrow = c(2,2))
par(mar = c(5,5,2,1.5))

#Temperature
boxplot(TempMin ~ Season, data = MinWQP, ylab = "Temperature (°C)",
        ylim = c(round(min(MinWQP$TempMin, na.rm = T), 1)-2, round(max(MinWQP$TempMin, na.rm = T),1)+2))

#Salinity
boxplot(SalMin ~ Season, data = MinWQP, ylab = "Salinity (PSU)",
        ylim = c(round(min(MinWQP$SalMin, na.rm = T),1)-2, round(max(MinWQP$SalMin, na.rm = T),1)+2))

#DO%
boxplot(DOperMin ~ Season, data = MinWQP, ylab = "Dissolved Oxygen (%)",
        ylim = c(round(min(MinWQP$DOperMin, na.rm = T),1)-2, round(max(MinWQP$DOperMin, na.rm = T),1)+2))

#Turbidity
boxplot(TurbMin ~ Season, data = MinWQP, ylab = "Turbidity (NTU)",
        ylim = c(0, round(max(MinWQP$TurbMin, na.rm = T),1)+2))

#pH
boxplot(pHMin ~ Season, data = MinWQP, ylab = "pH", ylim = c(7.5,9.5))

#Calculating 20th and 80th for physical WQ parameters per season
Per20_80WQ = for(i in 13:18){
  aggregate(WQphys[,i] ~ WQphys$Season, FUN = 'quantile', probs = c(0.20, 0.80), na.rm = T)}

# ######################################PER SITE PER SEASON####################################################
# #Divide entire dataset into seasons (Reference sites only)
# DryWQmin = MinWQP[c(which(MinWQP$Season == "dry")),]
# WetWQmin = MinWQP[c(which(MinWQP$Season == "wet")),]
#
# #Temperature
# boxplot(TempMin ~ Site, data = DryWQmin, las = 2, ylab = "Temperature (°C)", main = "Dry season",
#         ylim = c(round(min(MinWQP$TempMin, na.rm = T), 1)-2, round(max(MinWQP$TempMin, na.rm = T),1)+2))
# boxplot(TempMin ~ Site, data = WetWQmin, las = 2, ylab = "Temperature (°C)", main = "Wet season",
#         ylim = c(round(min(MinWQP$TempMin, na.rm = T),1)-2, round(max(MinWQP$TempMin, na.rm = T),1)+2))
# 
# #Salinity
# boxplot(SalMin ~ Site, data = DryWQmin, las = 2, ylab = "Salinity (PSU)", main = "Dry season",
#         ylim = c(round(min(MinWQP$SalMin, na.rm = T),1)-2, round(max(MinWQP$SalMin, na.rm = T),1)+2))
# boxplot(SalMin ~ Site, data = WetWQmin, las = 2, ylab = "Salinity (PSU)", main = "Wet season",
#         ylim = c(round(min(MinWQP$SalMin, na.rm = T),1)-2, round(max(MinWQP$SalMin, na.rm = T),1)+2))
# 
# #DO%
# boxplot(DOperMin ~ Site, data = DryWQmin, las = 2, ylab = "Dissolved Oxygen (%)", main = "Dry season",
#         ylim = c(round(min(MinWQP$DOperMin, na.rm = T),1)-2, round(max(MinWQP$DOperMin, na.rm = T),1)+2))
# boxplot(DOperMin ~ Site, data = WetWQmin, las = 2, ylab = "Dissolved Oxygen (%)", main = "Wet season",
#         ylim = c(round(min(MinWQP$DOperMin, na.rm = T),1)-2, round(max(MinWQP$DOperMin, na.rm = T),1)+2))
# 
# #Turbidity
# boxplot(TurbMin ~ Site, data = DryWQmin, las = 2, ylab = "Turbidity (NTU)", main = "Dry season",
#         ylim = c(round(min(MinWQP$TurbMin, na.rm = T),1)-2, round(max(MinWQP$TurbMin, na.rm = T),1)+2))
# boxplot(TurbMin ~ Site, data = WetWQmin, las = 2, ylab = "Turbidity (NTU)", main = "Wet season",
#         ylim = c(round(min(MinWQP$TurbMin, na.rm = T),1)-2, round(max(MinWQP$TurbMin, na.rm = T),1)+2))
# 
# #pH
# boxplot(pHMin ~ Site, data = DryWQmin, las = 2, ylab = "pH", main = "Dry season",
#         ylim = c(round(min(MinWQP$pHMin, na.rm = T),1)-2, round(max(MinWQP$pHMin, na.rm = T),1)+2))
# boxplot(pHMin ~ Site, data = WetWQmin, las = 2, ylab = "pH", main = "Wet season",
#         ylim = c(round(min(MinWQP$pHMin, na.rm = T),1)-2, round(max(MinWQP$pHMin, na.rm = T),1)+2))
# #############################################################################################################


###################################Water Quality - Chemical Parameters#############################################
library(stringr)

#Upload WQ chemical dataset
WQchemall = read.csv("WQChemical.csv")
WQchemall = WQchemall[1:760,1:17]
WQchemall$Group = str_extract(WQchemall$Site,"[A-Z]+")

#Update Seasons column in WQchem
for(i in 1:nrow(WQchemall)){
  if(months(as.Date(WQchemall$Date[i])) %in% Wet){WQchemall$Season[i] = "wet"
  }else if(months(as.Date(WQchemall$Date[i])) %in% Dry){WQchemall$Season[i] = "dry"}}

#Calculate minimum daily values for all parameters per site sampled
MinWQC = ddply(WQchemall,. (Date, Group, Season), summarise, HgMin = min(Hg, na.rm = T), 
               ZnMin = min(Zn, na.rm = T), AlMin = min(Al, na.rm = T), FeMin = min(Fe, na.rm = T),
               CdMin = min(Cd, na.rm = T), CrMin = min(Cr, na.rm = T), NiMin = min(Ni, na.rm = T), 
               CuMin = min(Cu, na.rm = T), PbMin = min(Pb, na.rm = T), NO2Min = min(NO2, na.rm = T), 
               NO3.NO2Min = min(NO3.NO2, na.rm = T), AmmoniaMin = min(Ammonia, na.rm = T), 
               Ortho.PMin = min(Ortho.P, na.rm = T))

#Change Inf values to NA to facilitate calculations
for(i in 1:nrow(MinWQC)){
  for(j in 7:ncol(MinWQC)){
    if(!is.finite(MinWQC[i,j])){MinWQC[i,j] = NA}}}

