#################################################################################################################
########################### Creating a Turbidity Predictive Model for Rio Doce ##################################
# Version: 1
# Date: 2018/07/11
# Author: Denisse Fierro Arcos
# Objectives: 
# Use in-situ turbidity and at-surface reflectance values from pre-processed Landsat 7 & 8 images to develop
# an equation that will allow us to estimate turbidity value using Landsat images.
#################################################################################################################

#Clear workspace
rm(list=ls())

#Upload relevant libraries
library(plyr)

#Set working directory
setwd("C:/DoceTurbidity")

#################################################################################################################
#Data Cleaning - Preparing dataset of in-situ turbidity measurements for further processing
#Upload datasets with in-situ turbidity measurements for cleaning
DoceTurbSites = read.csv("TurbiditySitesDoceONLY.csv")
TurbValues = read.csv("TurbidityValuesDoce.csv")[,1:3]

#Merging datasets with measurements from same time period
DoceTurbValues = merge(DoceTurbSites, TurbValues, by = "Sample.Poi", all = F)

#Extract coordinates for each monitoring sites (filters entire data for unique values)
pooled = read.csv("PooledTurbData.csv")
z = pooled[ ! duplicated( pooled[ c("Longitude" , "Latitude", "Station") ] ) , ]

x = read.csv("Data/TurbDataNov15Feb16.csv")
y = x[ ! duplicated( x[ c("Longitude" , "Latitude", "Station") ] ) , ]

#Calculate mean daily turbidity values for sites with more than one measurement per day
data17 = read.csv("Data/TurbDataMay17Aug17.csv")
dailymean17 = ddply(data17,.(Sample.Poi, Name, Date), summarise, meanTurb = mean(TurbidityNTU))
write.csv(dailymean17,"Data/DailyMeanTurbDataMay17Aug17.csv")
#################################################################################################################


#################################################################################################################
#Exploring relationships between variables
#Clear workspace
rm(list=ls())

#Upload relevant libraries
library(corrplot)

#Set working directory
setwd("C:/DoceTurbidity")

#Uploading pooled turbidity data January 2015 to August 2017
AllTurb = read.csv("Data/CombinedDataJan15Aug17.csv")
#Calculate log(x+1) for in-situ measurements to improve of normality of data distribution
AllTurb$logTurb = log(AllTurb$Turbidity+1)
#Delete information for SWIR bands - low correlation to turbidity data
AllTurb = AllTurb[,-c(which(grepl("SWIR",colnames(AllTurb)) | grepl("UltraBlue", colnames(AllTurb)) | 
                              grepl("UB_", colnames(AllTurb))))] 

# hist(log(AllTurb$Turbidity+1))
# qqnorm(log(AllTurb$Turbidity+1))
# qqline(log(AllTurb$Turbidity+1))
# shapiro.test(log(AllTurb$Turbidity+1))
# 
# hist(sqrt(AllTurb$Turbidity))
# qqnorm(sqrt(AllTurb$Turbidity))
# qqline(sqrt(AllTurb$Turbidity))
# shapiro.test(sqrt(AllTurb$Turbidity))

#Create scatterplots matrix to assess relationships between in-situ turbidity and band reflectance (untransformed,
#transformed and band combinations)
par(mfrow = c(3,3))
for (i in 12:(ncol(AllTurb)-1)){
  plot(AllTurb$logTurb ~ AllTurb[,i], ylab = "Log(Turbidity+1)", xlab = colnames(AllTurb)[i])}
# dev.off() # Turn plots off if needed and no output is required to be saved

#Calculate correlation matrix and significance of these correlations
TurbCor = cor(AllTurb[,c(12:ncol(AllTurb))])
TurbCorMat = cor.mtest(AllTurb[,c(12:ncol(AllTurb))])
#Plot above statistics in a graphical form for further inspection
par(mfrow = c(1,1))
#Upper matrix only
# corrplot(TurbCor, type="upper", p.mat = TurbCorMat$p, sig.level = 0.05)
#Upper matrix color scheme and lower matrix numbers
corrplot.mixed(TurbCor, p.mat = TurbCorMat$p, sig.level = 0.05, tl.cex = 0.7, number.cex = 0.7, tl.pos = "lt",
               lower.col = "black", mar = c(0,0,3,0), tl.col = "black")

#################################################################################################################
#Developing predictive regression model
# Create Training and Test data
# Setting seed to reproduce results of random sampling
set.seed(100)  
# Row indices for training data - 80% of values to be used for training
trainingRowIndex = sample(1:nrow(AllTurb), 0.8*nrow(AllTurb))
#Create training dataset
trainingData = AllTurb[trainingRowIndex,]
trainingData = trainingData[,-c(1:12)] # keep only columns with reflectance and turbidity values
#Create test data
testData = AllTurb[-trainingRowIndex,]
testData = AllTurb[,-c(1:12)]

#Predictive model - pre failure
Null = lm(logTurb ~ 1, data = trainingData)
Full = lm(logTurb ~ ., data = trainingData)
#Stepwise regression both ways (forwards and backwards)
step(Null, scope = list(upper = Full), data = trainingData, direction = "both")
# step(preFailNull, scope=list(lower=preFailNull, upper=preFailFull), direction="forward")
# step(preFailFull, data=preFailtrainingData, direction="backward")
#Best model based on AICc values (Results from step wise regression)
best = lm(logTurb ~ Green_Red + Blue_Green + LogRed + Red + LogGreen + Green + Blue_Red, data = trainingData)
summary(best)
Pred = predict(best, testData)
Actual_predicted = data.frame(cbind(actual = testData$logTurb, predicted = Pred))
CorrAcc = cor(Actual_predicted) #71.71%


#Taking into account multicollinearity
library(car)
Null = lm(logTurb ~ 1, data = trainingData)
Full = lm(logTurb ~ ., data = trainingData)

#Forward stepwise regression
stepreg = step(Null, scope = list(lower = Null, upper = Full), direction="forward")

#Calculate VIF for all variables included in best model identified in previous step
allVIFs = vif(stepreg)

#Get the names of variables included in "best" model
signifvar = names(allVIFs)

#Create a loop to identify variables with high collinearity (VIF > 10)
while(any(allVIFs > 10)){
  #Identify the variable with the highest VIF
  var_with_max_vif = names(which(allVIFs == max(allVIFs)))
  #Remove variable with highest VIF from list
  signifvar = signifvar[!(signifvar) %in% var_with_max_vif]
  #Create a new formula with new list of variables of interest (collapse option in paste function includes a
  #character in between items of a string)
  myForm = as.formula(paste("logTurb ~ ", paste (signifvar, collapse = " + "), sep = ""))
  #Run the model again using the previously defined formula
  selectedMod = lm(myForm, data = trainingData)
  #Recalculate VIFs for variables included in the new model
  allVIFs = vif(selectedMod)}

#Print summary of VIF corrected model and recalculated VIFs for variables in corrected model
summary(selectedMod)
print(allVIFs)

x = lm(logTurb ~ Green_Red + Blue_Green, data = trainingData)

plot(a_p$actual ~ a_p$predicted)



#Dividing data in pre and post accident
preDamFail = AllTurb[c(1:107),] # before Nov 2015
postDamFail = AllTurb[c(271:nrow(AllTurb)),]

#Visual inspection linear relationships: Turbidity and bands
par(mfrow = c(3,3))
for (i in 12:(ncol(preDamFail)-1)){
  plot(preDamFail$logTurb ~ preDamFail[,i], ylab = "Log(Turbidity+1)", xlab = colnames(preDamFail)[i])}
par(mfrow = c(3,3))
for (i in 12:(ncol(postDamFail)-1)){
  plot(postDamFail$logTurb ~ postDamFail[,i], ylab = "Log(Turbidity+1)", xlab = colnames(postDamFail)[i])}

#Correlations
PreDamFailCor = cor(preDamFail[,c(12:ncol(preDamFail))])
PreFailCorMat = cor.mtest(preDamFail[,c(12:ncol(preDamFail))])
par(mfrow = c(1,1))
corrplot(PreDamFailCor, type="upper", p.mat = PreFailCorMat$p, sig.level = 0.05)

PostDamFailCor = cor(postDamFail[,c(12:ncol(postDamFail))])
PostFailCorMat = cor.mtest(postDamFail[,c(12:ncol(postDamFail))])
par(mfrow = c(1,1))
corrplot(PostDamFailCor, type="upper", p.mat = PostFailCorMat$p, sig.level = 0.05)
#Scatterplots with line of best fit
par(mfrow = c(3,3))
for (i in 12:(ncol(postDamFail)-1)){
  scatter.smooth(x = postDamFail[,i], y = postDamFail$logTurb, ylab = "Log(Turbidity+1)", xlab = colnames(postDamFail)[i])}

# Create Training and Test data -
set.seed(100)  # setting seed to reproduce results of random sampling
pretrainingRowIndex = sample(1:nrow(preDamFail), 0.8*nrow(preDamFail))  # row indices for training data
preFailtrainingData = preDamFail[pretrainingRowIndex, ]  # model training data
preFailtrainingData = preFailtrainingData[,-c(1:12,17,22:25)]
preFailtestData = preDamFail[-pretrainingRowIndex, ]   # test data
preFailtestData = preFailtestData[,-c(1:12,17,22:25)]

postTrainingRowIndex = sample(1:nrow(postDamFail), 0.8*nrow(postDamFail))  # row indices for training data
postFailtrainingData = postDamFail[postTrainingRowIndex, ]  # model training data
postFailtrainingData = postFailtrainingData[,-c(1:12,17,22:25)]
postFailtestData = postDamFail[-postTrainingRowIndex, ]   # test data
postFailtestData = postFailtestData[,-c(1:12,17,22:25)]

#Predictive model - pre failure
preFailNull = lm(logTurb ~ 1, data = preFailtrainingData)
preFailFull = lm(logTurb ~ ., data = preFailtrainingData)
step(preFailNull, scope = list(upper = preFailFull), data = preFailtrainingData, direction = "both")
# step(preFailNull, scope=list(lower=preFailNull, upper=preFailFull), direction="forward")
# step(preFailFull, data=preFailtrainingData, direction="backward")
bestPreFail = lm(logTurb ~ Green + LogBlue, data = preFailtrainingData) # model choice based on AICc
summary(bestPreFail)
preFailPred = predict(bestPreFail, preFailtestData)
PreActual_predicted = data.frame(cbind(actuals=preFailtestData$logTurb, predicted=preFailPred))
preCorrAcc = cor(PreActual_predicted) #83.01%

#Predictive model - post failure
postFailNull = lm(logTurb ~ 1, data = postFailtrainingData)
postFailFull = lm(logTurb ~ ., data = postFailtrainingData)
step(postFailNull, scope = list(upper = postFailFull), data = postFailtrainingData, direction = "both")
bestPostFail = lm(formula = logTurb ~ Green_Red, data = postFailtrainingData) # model choice based on AICc
postFailTurbPred = predict(bestPostFail, postFailtestData)
postActual_predicted = data.frame(cbind(actual = postFailtestData$logTurb, predicted = postFailTurbPred))
postCorrAcc = cor(postActual_predicted) #45.22%
