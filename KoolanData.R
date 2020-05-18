#Clear all variables
rm(list = ls())

library(plyr)
library(readxl)
library(stringr)

#Directory where all data you are working is located
setwd("C:/Images")

Filepaths=list.dirs(recursive = F)

for(i in 1:length(Filepaths)){
  setwd(Filepaths[i])
  Folders=list.dirs(recursive = F)
  for(j in 1:length(Folders)){
    setwd(Folders[j])
    temp=list.files(pattern = ".xlsx") #get a list of files with a particular ending
    if(length(temp)>1){
      temp=temp[-grep("~",temp)]}
    assign(str_sub(temp,end = -6),read_excel(temp, sheet = str_sub(temp,end = -6), range = "G15:I51"))
    setwd("..")} #go back one folder
  setwd("..")} #go back one folder

#Get all the names of dataframes in the global environment
datasets = sort(names(which(sapply(.GlobalEnv, is.data.frame))))

#Create new matrix
Labels=c("UNKNOWN (UNK)","BARE SUBSTRATE (BS)","SEDIMENT (S)","MACROALGAE (MA)","SEAGRASS (SG)","TURF ALGAE (TA)",
         "TURF ON HARD SUBSTRATE (THS)","TURF ON SOFT SUBSTRATE (TSS)","Turf Algae (TA)","DEAD STANDING CORAL (DCS)",
         "HARD CORAL (HC)","BLEACHED HARD CORAL (BL)","SOFT CORAL (SC)","SOFT CORAL BLEACHED (SCBL)",
         "OTHER (OTH)","TAPE WAND SHADOW (TWS)")
AllCoralCounts=matrix(data = NA,nrow=length(datasets),ncol=length(Labels))
colnames(AllCoralCounts)=paste(Labels)
rownames(AllCoralCounts)=paste(datasets)

# #Add site name to each dataset
# for(i in seq_along(datasets)){
#   x = get(datasets[i])
#   for(j in 1:nrow(x)){
#     x$Sites = datasets[i]}
#   assign(datasets[i], x[,-2])}

#Transfer values to new matrix
for(i in seq_along(datasets)){
  x = get(datasets[i])
  for(j in 1:ncol(AllCoralCounts)){
    for(k in 1:nrow(x)){
      if(colnames(AllCoralCounts)[j]==x$CATEGORIES[k]){
        AllCoralCounts[i,j]=x$`%`[k]}}}}

#Merging turf algae values
AllCoralCounts[,grepl("Turf",colnames(AllCoralCounts))]=
  rowSums(AllCoralCounts[,c(grepl("TURF",colnames(AllCoralCounts)))],na.rm = T)
AllCoralCounts=AllCoralCounts[,-c(which(grepl("TURF",colnames(AllCoralCounts))))]
AllCoralCounts=as.data.frame(AllCoralCounts)
AllCoralCounts$groups=gsub("[^A-Z]","",toupper(datasets))
# AllCoralCounts$groups=replace(AllCoralCounts$groups,which(AllCoralCounts$groups=="NREF"),"NREFW")
# AllCoralCounts$groups=replace(AllCoralCounts$groups,which(AllCoralCounts$groups=="W"),"WW")
# AllCoralCounts$groups=replace(AllCoralCounts$groups,which(AllCoralCounts$groups=="SOUTHREF"),"EREF")
# AllCoralCounts$groups=replace(AllCoralCounts$groups,grepl("PB2*",row.names(AllCoralCounts)),"PBE")

#Environmental matrix
# groups=as.data.frame(unique(gsub("[^A-Z]","",toupper(datasets))))
# groups$code=c(1:nrow(groups))
EnvCoral=matrix(data = NA,nrow=length(datasets),ncol=1)
colnames(EnvCoral)="groups"
rownames(EnvCoral)=paste(datasets)
y=gsub("[^A-Z]","",toupper(datasets))
EnvCoral[,1]=match(y,groups$`unique(gsub("[^A-Z]", "", toupper(datasets)))`) #extract only letters from datasets

write.csv(AllCoralCounts,file = "../../Data/Working/CoverageMatrix.csv")
write.csv(EnvCoral,file = "../../Data/Working/EnvMatrix.csv")


###############################################################################################
#Creating figures - SELECT THE APPROPRIATE VARIABLES

#Clear all variables
rm(list = ls())

library(dplyr)
library("RColorBrewer")
#Set working directory
setwd("C:/Working")

#Upload file with benthic cover data
# Allsites=read.csv("All sites/CoverageMatrix.csv")#All sites
Allsites=read.csv("All/CoverageMatrix_MScience.csv")#All 
# Allsites=read.csv("North/CoverageMatrixNorth_MS.csv")
# Allsites=read.csv("South/CoverageMatrixSouth_MS.csv")

#Names of benthic groups
substrate=c("Unknown","Bare substrate","Sediment","Macroalgae","Seagrass",
            "Turf algae","Dead standing coral","Hard coral","Bleached hard coral",
            "Soft coral","Soft coral bleached","Other","Tape wand shadow")

#Names of groups formatted for figures
groups=c("AC","Diff","NREFW","NREFE","PBE","EREF","WW","WE","PBW")#All sites
# groups=c("NREFW","NREFE","PBE","PBW")#North sites
# groups=c("AC","Diff","EREF","WW","WE")#South sites

#Calculate mean and SD per monitoring group
ALLmean = Allsites %>%
  group_by(groups) %>%
  summarise_at(.vars = names(.)[2:14],.funs = c(mean="mean"))
ALLmean=t(as.matrix(ALLmean[,-1]))
colnames(ALLmean)=groups
rownames(ALLmean)=substrate
ALLsd = Allsites %>%
  group_by(groups) %>%
  summarise_at(.vars = names(.)[2:14],.funs = c(sd="sd"))
ALLsd=t(as.matrix(ALLsd[,-1]))
colnames(ALLsd)=groups
rownames(ALLsd)=substrate

#Create plots
#Allow legend to show outside figure margins (xpd) and adjust figure margins
par(xpd=T,mar=c(4.1,5.1,4.1,2.1))
#Save plot in x variable to plot error bars
x=barplot(ALLmean,beside = T,col=c(brewer.pal(n=12,name="Set3"),"blue4")
          ,ylim=c(0,100),cex.axis=0.9,names.arg=groups,cex.names=0.7,las=2,
          ylab="Cover (%)")
#beside option plots bars next to each other
#brewer.pal selects color palets - see bookmark for more info on names

#Plotting error bars
arrows(x0=x,y0=(ALLmean),x1=x,y1=(ALLmean+ALLsd),angle = 90,code = 3,
       length = 0.04,lwd = 0.4)
#x & y sets where the bar begins (0) and ends (1), code determines type of arrow
legend("top",legend=substrate,fill=c(brewer.pal(n=12,name="Set3"),"blue4"),
       cex=0.75,ncol=3,inset=c(0,-0.15),x.intersp=0.7,y.intersp=0.7,bty = "n")
#x/y.intersp adjusts the distance between lines in the legend



#####################################################################################################
###Creating plots with all data combined#############

rm(list = ls())
library(tidyr)
library(dplyr)
library("RColorBrewer")
#Set working directory
setwd("C:/SitesAllYears")

#ALL SITES ALL YEARS
alldata=read.csv("AllSitesAllYears.csv")
alldata=unite(alldata,group,group,year,sep=" ")
substrate=c("Unknown","Bare substrate","Sediment","Macroalgae","Seagrass",
            "Turf algae","Dead standing coral","Hard coral","Bleached hard coral",
            "Soft coral","Soft coral bleached","Other")
allmeans= alldata %>%
  group_by(group)%>%
  summarise_at(.vars = names(.)[2:13],.funs = c(mean="mean"))
groupsall=allmeans$group
allmeans=t(as.matrix(allmeans[,-1]))
colnames(allmeans)=groupsall
rownames(allmeans)=substrate

allsd=alldata%>%
  group_by(group)%>%
  summarise_at(.vars = names(.)[2:13],.funs = c(sd="sd"))
allsd=t(as.matrix(allsd[,-1]))
colnames(allsd)=groupsall
rownames(allsd)=substrate

#Create plots
#Allow legend to show outside figure margins (xpd) and adjust figure margins
par(xpd=T,mar=c(3.1,5.1,5.1,2.1))
#Save plot in x variable to plot error bars
x=barplot(allmeans[,21:24],beside = T,col=brewer.pal(n=12,name="Set3"),ylim=c(0,100),cex.axis=0.9,
          names.arg=colnames(allmeans)[21:24],cex.names=0.7,ylab="Cover (%)")
#beside option plots bars next to each other
#brewer.pal selects color palets - see bookmark for more info on names

#Plotting error bars
arrows(x0=x,y0=(allmeans[,21:24]),x1=x,y1=(allmeans[,21:24]+allsd[,21:24]),angle = 90,code = 3,
       length = 0.04,lwd = 0.4)
#x & y sets where the bar begins (0) and ends (1), code determines type of arrow
legend("top",legend=substrate,fill=brewer.pal(n=12,name="Set3"),cex=0.75,ncol=3,inset=c(0,-0.15),
       x.intersp=0.8,y.intersp=0.8,bty = "n")
#x/y.intersp adjusts the distance between lines in the legend


###############################################################################################
#Creating figures - SELECT THE APPROPRIATE VARIABLES

#Clear all variables
rm(list = ls())

library(raster)
#library(ncdf4)
setwd("C:/Working")

#Upload SST data as multilayer raster
SSTraw=stack("KoolanSST_2014-2017.nc")

#Upload sites
sites=shapefile("SiteCoords.shp")

#Match projections
projection(SSTraw)=projection(sites)

#Kimberley seasons:
#Dry: May to October
#Wet: November to April

#Extract dates from raster #From 2014-01-01 to 2017-11-30
dates=as.numeric(gsub("[^0-9]","",toupper(names(SSTraw))))
#Find date range
which(grepl(201710,dates))
#2014 - 1-365 / Dry - 121-304 / Wet - 305-485
#2015 - 366-730 / Dry - 486-669 / Wet - 670-849
#2016 - 731 - 1093 / Dry - 850-1032 / Wet - 1033-1213
#2017 - 1094 - 1427 / Dry - 1214-1397

#Calculate means and max values
SST14Dry=mean(SSTraw[[121:304]],na.rm=T)
SST1415Wet=mean(SSTraw[[305:485]],na.rm=T)
SST15Dry=mean(SSTraw[[486:669]],na.rm=T)
SST1516Wet=mean(SSTraw[[670:849]],na.rm=T)
SST16Dry=mean(SSTraw[[850-1032]],na.rm=T)
SST1617Wet=mean(SSTraw[[1033-1213]],na.rm=T)
SST17Dry=mean(SSTraw[[1214:1397]],na.rm=T)

#Stack bands ready for extraction
SSTstack=stack(SST14Dry,SST1415Wet,SST15Dry,SST1516Wet,SST16Dry,SST1617Wet,SST17Dry)

#Extract SST values from all layers into site shapefile
SSTsites=extract(SSTstack,sites)
SSTsites=cbind(sites,SSTsites)
write.table(SSTsites,file="SSTsites.csv",append=F,sep= ",",row.names=F,col.names=T)
a=as.data.frame(SSTsites)[,6:15]
rownames(a)=paste(a[,1])
a=as.matrix(a[,-c(1:3)])
colnames(a)=c("SST14Dry","SST1415Wet","SST15Dry","SST1516Wet","SST16Dry","SST1617Wet","SST17Dry")
a=a[c(1,10,13,18,20,21,30,32,37),]

#Plot values per column
par(mar=c(6, 3, 3, 10),xpd=T)
plot(a[1,],type="n",ylab="",ylim=c(min(a,na.rm=T),round(max(a,na.rm=T),1)),xlab = "",xaxt="n",cex.axis=0.8,
     cex.lab=0.8)
axis(1,at=1:dim(a)[2],labels = colnames(a),las=2,cex.axis=0.8)
mtext("Mean SST", cex=0.8,side=2,line=2)
for(i in 1:dim(a)[1]){
  lines(as.numeric(paste(a[i,])),col=i,lty=i,pch=i,type = "o")}
legend("topright",legend=rownames(a),cex=0.7,col=c(1:nrow(a)),lty=c(1:nrow(a)),
       inset=c(-0.25,0),pch = c(1:nrow(a)),title="Monitoring sites",bty="n")


########################
#Map
library(ggmap)
library(ggplot2)
library(scatterpie)

setwd("C:/GIS")

acpoints=read.csv("ACPoints.csv")
names(acpoints)[2:15]=c("y","x","Unknown","Bare substrate","Sediment","Macroalgae","Seagrass",
                        "Turf algae","Dead standing coral","Hard coral","Bleached hard coral",
                        "Soft coral","Bleached soft coral","Other")
acpoints$X=acpoints$x+c(0.0006,-0.0010,0.0006,-0.0008,0.0010,-0.0008,0.0010,-0.0004,0.0008)
acpoints$Y=acpoints$y+c(0.00096,0,0.00076,-0.0004,0.0003,-0.0006,0,-0.0008,-0.0003)

mapImageData=get_map(location=c(lon=123.738,lat=-16.1359),source="google",maptype="satellite",zoom=17)
n=ggmap(mapImageData,extent="normal",legend="right")+labs(x="Longitude",y="Latitude")
n+geom_point(aes(x=x,y=y),data=acpoints)+
  geom_scatterpie(aes(x=X,y=Y,group=SITES,r=0.0003),data=acpoints,cols=colnames(acpoints)[4:15])

insetmap=get_map(location=c(lon=123.739,lat=-16.1359),source="google",maptype="satellite",zoom=12)
m=ggmap(insetmap)+geom_point(aes(x=x,y=y),data=acpoints)
