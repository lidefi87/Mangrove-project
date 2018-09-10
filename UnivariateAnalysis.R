#Clear workspace
rm(list=ls())

#Set working directory
setwd("Y:/SOS - Seas Offshore/SOS1701 Stybarrow offshore/Data/Infauna")

#Upload dataset
Div=read.csv("Diversity.csv")

#Create bixplots of abundance and richness
par(mfrow = c(1,2))
par(mar = c(7,6.5,3.5,2))
boxplot(TotalTaxonomicGroups~Group,data= Div, las=2, ylim = c(0,5), ylab = "Total Taxonomic Groups")
boxplot(TotalIndividuals~Group, data= Div, las=2, ylim = c(0,5), ylab = "Total Number of Individuals")
dev.off()

#Check normality of distribution
##Log(x+1), square root and fourth root did not achieve normality
qqnorm(log(Div$TotalTaxonomicGroups+1))
qqline(log(Div$TotalTaxonomicGroups+1))
shapiro.test(log(Div$TotalTaxonomicGroups+1))

#Non-parametric test for differences in means in abundance and richness
kruskal.test(TotalTaxonomicGroups~Group,data= Div)
kruskal.test(TotalIndividuals~Group,data= Div)
