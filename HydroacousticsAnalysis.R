######################################################################################################################
#Statistical analysis of marine hydroacoustics data collected in April 2018 (Doce and Jequitinhonha rivers)
#Version: 1
#Date: 2018/06/19
#Author: Denisse Fierro Arcos
#Objectives: 
#Extract biomass values of Sonar5 output files and pool data together
#Create master dataset with biomass and abiotic factor variables
#Perform a two-way ANOVA (Dependent variable: biomass, Independent variables: river, and geographical location)
#Perform multiple linear regressions between biomass and abotic variables
######################################################################################################################

#Clear all variables from current workspace
rm(list = ls())

#Load relevant libraries
library(plotrix)
library(stringr)
library(plyr)

#Set working directory - Location of Sonar5 outputs
setwd("Y:/BHP - BHP Billiton/BHP1704d Marine Survey/Data/Hydroacoustics/Data Analysis/BiomassOutputs")

#List all datasets (.csv files) in current directory
BioData = list.files(pattern = ".csv")

#Extracting total biomass values and creating new dataset
for(i in seq_along(BioData)){
  x = read.csv(BioData[i])
  if(i == 1){
    TotalBio = x[which(grepl("Biomass*",x$Segment.No.)&grepl("*total",x[,1])),-1]
    MaxDepth = x[which(grepl(glob2rx("Max. range*"),x[,1])),-1]
  }else{TotalBio = rbind.fill(TotalBio,x[which(grepl("Biomass*",x[,1])&grepl("*total",x[,1])),-1])
  MaxDepth = rbind.fill(MaxDepth,x[which(grepl(glob2rx("Average range*"),x[,1])),-1])}}
rownames(TotalBio) = paste(str_sub(BioData,end = -5))
rownames(MaxDepth) = paste(str_sub(BioData,end = -5))
for(i in seq_along(TotalBio)){
  TotalBio[,i] = as.numeric(as.character(TotalBio[,i]))
  MaxDepth[,i] = as.numeric(as.character(MaxDepth[,i]))}
Biom3 = TotalBio/MaxDepth
Biom3$MeanBio = rowMeans(Biom3, na.rm = T)
TotalBio$MeanBiom3 = Biom3$MeanBio
TotalBio$MeanDepth = rowMeans(MaxDepth, na.rm = T)
rm(MaxDepth)

#Including factors for the ANOVA analysis
#River name - Based on start of row name
TotalBio$River = NA
for(i in 1:nrow(TotalBio)){
  if(grepl("^D",rownames(TotalBio)[i])){TotalBio$River[i] = "Doce"
  }else if(grepl("^J",rownames(TotalBio)[i])){TotalBio$River[i] = "Jequitinhonha"}}

#Distance from river mouth - Extracted from row name
TotalBio$Distance = NA
for(i in 1:nrow(TotalBio)){
  TotalBio$Distance[i] = as.numeric(gsub(".*?([0-9]+).*", "\\1", rownames(TotalBio)[i]))}

#Geographic location - North or South based on row name
TotalBio$Location = NA
for(i in 1:nrow(TotalBio)){
  if(grepl(glob2rx("*N*"),rownames(TotalBio)[i])){TotalBio$Location[i] = "North"
  }else if(grepl(glob2rx("*S*"),rownames(TotalBio)[i])){TotalBio$Location[i] = "South"
  }else{TotalBio$Location[i] = "North"}} 
#Both D1E and D5E are unclassified, but they are slighty north from river mouth, so they are classified as "North"

#Calculate SE for each site
TotalBio$BiomassSE = NA
for(i in 1:nrow(TotalBio)){
  TotalBio$BiomassSE[i] = sd(TotalBio[i,1:5],na.rm = T)/sqrt(length(which(!is.na(TotalBio[i,1:5]))))}

TotalBio$Sitename = rownames(TotalBio)

#Ordering dataframe by distance and river
TotalBio = TotalBio[order(TotalBio$River,TotalBio$Distance),]

#############################################Producing graphs#####################################################
#Box plots 
boxplot(MeanBiom3~River, data = TotalBio, ylab = "Mean Biomass (g/m3)")
par(mfrow = c(2,2))
boxplot(MeanBiom3~Distance, data = TotalBio[1:21,], ylab = "Mean Biomass (g/m3)", 
        xlab = "Distance from river mouth (km)", ylim = c(0,4.5e-39), cex.axis = 1.5, cex.lab = 1.5)
mtext(side=3, text="Rio Doce", line = 2, cex = 1.5)
boxplot(MeanBiom3~Distance, data = TotalBio[22:38,], ylab = "Mean Biomass (g/m3)", 
        xlab = "Distance from river mouth (km)", ylim = c(0,4.5e-39), cex.axis = 1.5, cex.lab = 1.5)
mtext(side=3, text="Rio Jequitinhonha", line = 2, cex = 1.5)
boxplot(MeanBiom3~Location, data = TotalBio[1:21,], ylab = "Mean Biomass (g/m3)", 
        xlab = "Sub area (location in relation to river mouth)", ylim = c(0,4.5e-39),cex.axis = 1.5, cex.lab = 1.5)
boxplot(MeanBiom3~Location, data = TotalBio[22:38,], ylab = "Mean Biomass (g/m3)", 
        xlab = "Sub area (location in relation to river mouth)", ylim = c(0,4.5e-39),cex.axis = 1.5, cex.lab = 1.5)
dev.off()

#Bar plots: mean and SE per site (one graph per river)
par(mfrow = c(2,1))
par(mar = c(5,6,3,2)+0.5)
Doce = barplot(TotalBio$MeanBiom3[1:21],names.arg = TotalBio$Sitename[1:21],las=2,ylim = c(0,2.5e-38))
mtext(side=2, text="Biomass (g/m3)", line = 5)
mtext(side=3, text="Rio Doce", line = 2)
arrows(x0 = Doce, y0 = TotalBio$MeanBiom3[1:21], x1 = Doce, 
       y1 = (TotalBio$MeanBiom3[1:21] + TotalBio$BiomassSE[1:21]), angle = 90, code = 3, length = 0.04, lwd = 0.4)
Jequi = barplot(TotalBio$MeanBiom3[22:38],names.arg = TotalBio$Sitename[22:38],las=2,ylim = c(0,2.5e-38))
mtext(side=2, text="Biomass (g/m3)", line = 5)
mtext(side=3, text="Rio Jequitinhonha", line = 2)
arrows(x0 = Jequi, y0 = TotalBio$MeanBiom3[22:38], x1 = Jequi, 
       y1 = (TotalBio$MeanBiom3[22:38] + TotalBio$BiomassSE[22:38]), angle = 90, code = 3, length = 0.04, lwd = 0.4)
dev.off()

#Scatterplot (Mean biomass  vs mean depth)
par(mfrow = c(1,2))
par(mar = c(5,5,4,2))
plot(sqrt(sqrt(MeanBiom3[1:21])) ~ log(MeanDepth[1:21]+1), data = TotalBio, ylab = "Fourth root mean biomass (g/m3)", 
     xlab = "Log(x+1) mean depth (m)", cex.axis = 1.25, cex.lab = 1.5, ylim = c(0, 3e-10))
mtext(side=3, text="Rio Doce", line = 2, cex = 1.5)
plot(sqrt(sqrt(MeanBiom3[22:38])) ~ log(MeanDepth[22:38]+1), data = TotalBio, ylab = "Fourth root mean biomass (g/m3)",
     xlab = "Log(x+1) mean depth (m)", cex.axis = 1.25, cex.lab = 1.5, ylim = c(0, 3e-10))
mtext(side=3, text="Rio Jequitinhonha", line = 2, cex = 1.5)
dev.off()

#########################################Statistical analyses###################################################

####Data transformation####

#Biomass
##Log(x+1) -> Non normally distributed
# qqnorm(log1p(TotalBio$MeanBiom3))
# qqline(log1p(TotalBio$MeanBiom3))
# shapiro.test(log1p(TotalBio$MeanBiom3)) #W = 0.53866, p-value = 9.366e-10

###Square root(x) -> Non normally distributed
# qqnorm(sqrt(TotalBio$MeanBiom3))
# qqline(sqrt(TotalBio$MeanBiom3))
# shapiro.test(sqrt(TotalBio$MeanBiom3)) #W = 0.87903, p-value = 0.0006865

###Fourth root(x) -> Normally distributed
# qqnorm(sqrt(sqrt(TotalBio$MeanBiom3)))
# qqline(sqrt(sqrt(TotalBio$MeanBiom3)))
# shapiro.test(sqrt(sqrt(TotalBio$MeanBiom3))) #W = 0.969, p-value = 0.3655


#Depth
###Log(x+1) -> Normally distributed
# qqnorm(log(TotalBio$MeanDepth+1))
# qqline(log(TotalBio$MeanDepth+1))
# shapiro.test(log(TotalBio$MeanDepth+1)) #W = 0.98081, p-value = 0.7461
###########################

#Two-factor ANOVA: River and location
RivLoc = aov(sqrt(sqrt(MeanBiom3)) ~ River * Location, data = TotalBio)
summary(RivLoc)

#Changing Distance to factor to complete Tukey HSD post-hoc test
TotalBio$Distance = as.factor(TotalBio$Distance)
#Two-factor ANOVA: River and location
RivDist = aov(sqrt(sqrt(MeanBiom3)) ~ River * Distance, data = TotalBio)
summary(RivDist)
#Post-hoc Tukey HSD
PostRivDist = TukeyHSD(RivDist)

#Means and SE
RiverBio = ddply(TotalBio,.(River),summarise, meanBio = mean(MeanBiom3), seBio = std.error(MeanBiom3),
                 minBio = min(MeanBiom3), maxBio = max(MeanBiom3))

DepthDif =  lm(sqrt(sqrt(MeanBiom3)) ~ MeanDepth * River, data = TotalBio)
summary(DepthDif)

# #Pairwise t-tests - Not needed
# pairwise.t.test(TotalBio$TotalBio_gm2,TotalBio$Location,p.adjust.method = "bonferroni")


#Interactions between biomass and abiotic variables (CTD data)
#Set working directory for abiotic data
setwd("Y:/BHP - BHP Billiton/BHP1704d Marine Survey/Data/Hydroacoustics/Data Analysis/R Outputs")

#Upload dataset
Abiotic = read.csv("AbioticVariables.csv")

#Create new dataset for linear model
BioAbi = merge(TotalBio[,c(6:10,12)], Abiotic[,c(1:29)], by.x = "Sitename", by.y = "Site")
#Normalise abiotic data
BioAbi.trans = scale(BioAbi[,10:ncol(BioAbi)])
BioAbi.trans = cbind(BioAbi[1:9],BioAbi.trans)
BioAbi.trans$FourthBio = sqrt(sqrt(BioAbi.trans$MeanBiom3))

#Plots to check for relationship
par(mfrow = c(3,3))
#Doce river
for (i in 11:ncol(BioAbi.trans)-1){
  # fit = lm(BioAbi.trans$FourthBio[1:11] ~ BioAbi.trans[1:11,i])
  plot(BioAbi.trans$FourthBio[1:11] ~ BioAbi[1:11,i], ylab = "Fourth root mean biomass (g/m3)", 
       xlab = colnames(BioAbi.trans)[i])}
  # lines(BioAbi.trans[1:11,i], fitted(fit), col="blue")}
dev.off()

par(mfrow = c(3,3))
for (i in 10:ncol(BioAbi.trans)-1){
  plot(BioAbi.trans$FourthBio[12:17] ~ BioAbi[12:17,i], ylab = "Fourth root mean biomass (g/m3)", 
       xlab = colnames(BioAbi.trans)[i])}
dev.off()

library(corrplot)
DoceCor = cor(BioAbi.trans[1:11,c(3,10:ncol(BioAbi.trans))])
corrplot(DoceCor, type = "upper")

JequiCor = cor(BioAbi.trans[12:17,c(3,10:ncol(BioAbi.trans))])
corrplot(JequiCor, type = "upper")

#Calculating p values of correlations
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }}
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat}

# matrix of the p-value of the correlation
pDoce = cor.mtest(BioAbi.trans[1:11,c(3,10:ncol(BioAbi.trans))])
pJequi = cor.mtest(BioAbi.trans[12:17,c(3,10:ncol(BioAbi.trans))])

#Plotting correlation matrix with p values overlaid
corrplot(DoceCor, type="upper", p.mat = pDoce, sig.level = 0.05)
corrplot(JequiCor, type="upper", p.mat = pJequi, sig.level = 0.05)

####Differences in abiotic variables###
for(i in 10:(ncol(BioAbi.trans)-1)){
  print(colnames(BioAbi.trans)[i])
  print(t.test(BioAbi.trans[,i][BioAbi.trans$River == "Doce"], 
         BioAbi.trans[,i][BioAbi.trans$River == "Jequitinhonha"]))}

mean(BioAbi.trans[,13][BioAbi.trans$River == "Doce"])
mean(BioAbi.trans[,13][BioAbi.trans$River == "Jequitinhonha"])
