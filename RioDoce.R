###############################################################################################
#Run this code if you only have raw images

#Clear all variables
rm(list=ls())

#Load relevant libraries
library(stringr)
library(raster)
library(rgdal)
library(RStoolbox)

#Set working directory - where all data to be analysed is located
setwd("X:/GIS_BHP1704/Doce water colour baseline/Satellite Images Rio Doce basin/")

#Upload points for Rio Doce Path shapefile
#RioDoce=shapefile("RioDocePoints.shp") # original river points
RioDoce=shapefile("RioDoceEdited20180313.shp") # edited river points - less points in upper river area
# #Create buffer around river to minimize RAM usage
# bufRD=buffer(RioDoce,width=500)

#Get directories of all locations of downloaded Landsat images
FilePaths=list.dirs() #Get all directories
#Select only directories with satellite images
FilePaths=subset(FilePaths,grepl(glob2rx("*/201*"),FilePaths))
FilePaths=FilePaths[-which(grepl("gap_mask",FilePaths)|grepl("Done",FilePaths)
                           |grepl("P218_R74",FilePaths))]

#Preprocessing images and saving outputs
for(j in 1:length(FilePaths)){
  setwd(FilePaths[j])
  #Identify the folders that have not been processed yet and run the code only in new folders
  # if(length(list.files(pattern=".csv"))==0){
    #Identifying metadata file
    MT=list.files(pattern = "MTL.txt$")
    #Upload metadata file
    meta=readMeta(MT)
    #Create multilayered raster based on metadata
    MB=stackMeta(meta)
    #Identifying BQA layer
    BQA=list.files(pattern="BQA.TIF$")
    #Upload quality layer file
    qa=raster(BQA)
    # #Crop stack to river buffer
    # cropMB=crop(MB,bufRD)
    #Creating cloud shadow layer
    #Landsat 4-7 collection have same values, but Landsat 8 does not.
    if(grepl(glob2rx("*L7"),FilePaths[j])==TRUE|grepl(glob2rx("*L5"),FilePaths[j])==TRUE){
      CS=c(928,932,936,940,960,964,968,972,752,756,760,764) #including clouds (high confidence)
      for(i in 1:length(CS)){
        qa[qa==CS[i]]=NA}
    }else if(grepl(glob2rx("*L8"),FilePaths[j])==TRUE){
      CS=c(2976,2980,2984,2988,3008,3012,3016,3020,7072,7076,7080,7084,7104,7108,7112,7116,2800,2804,2808,
           2812,6896,6900,6904,6908) #including clouds (high confidence)
      for(i in 1:length(CS)){
        qa[qa==CS[i]]=NA}}
    rm(MT,BQA,CS)
    MB_noshadow=mask(MB,qa)
    rm(MB,qa)
    #Calculating TOA values for green and NIR layers only as they are the only ones needed to calculate NDWI
    if(grepl(glob2rx("*L7"),FilePaths[j])==TRUE|grepl(glob2rx("*L5"),FilePaths[j])==TRUE){
      corr_GNIR=radCor(MB_noshadow,meta,method = 'apref',bandSet = c(2,4))
    }else if(grepl(glob2rx("*L8"),FilePaths[j])==TRUE){
      corr_GNIR=radCor(MB_noshadow,meta,method = 'apref',bandSet = c(3,5))}
    ndwi=spectralIndices(corr_GNIR,green = 1, nir= 2,indices = "NDWI")
    ndwi[ndwi<=0]=NA #water has an NDWI index >0, so any other values are transformed to 'NA'
    rm(corr_GNIR)
    # ndwi[ndwi>0]=1
    # myfun=function(x) ifelse(x>0,1,NA)
    # mask_ndwi=calc(x=ndwi,fun=myfun)
    #Use NDWI index to mask any non-water areas
    water=mask(MB_noshadow,ndwi)
    rm(ndwi,MB_noshadow)
    # water_TOA=overlay(MB_TOA,ndwi,fun=function(x,y){x*y})
    #Calculate at-surface reflectance for all unmasked values in all layers
    if(grepl(glob2rx("*L7"),FilePaths[j])==TRUE|grepl(glob2rx("*L5"),FilePaths[j])==TRUE){
      haze=estimateHaze(water,hazeBands = 1:3)
      surfref_water=radCor(water,meta,method = 'sdos',hazeValues = haze, hazeBands = 1:3)
      rm(haze)
    }else if(grepl(glob2rx("*L8"),FilePaths[j])==TRUE){
      haze=estimateHaze(water,hazeBands = 1:4)
      surfref_water=radCor(water,meta,method = 'sdos',hazeValues = haze, hazeBands = 1:4)
      rm(haze)}
    rm(water)
    writeRaster(surfref_water,filename = "surfref_Water.tif",options="INTERLEAVE=BAND", overwrite=T)
    #Reproject river shapefile to correct CRS based on satellite image
    ReprojRD=spTransform(RioDoce,crs(surfref_water))
    #Reprojection could also be done as: projection(namerasterlayer)="+init=epsg:32624"
    #Extract TOA values from all layers into river points
    pts=extract(surfref_water,ReprojRD)
    rasValues=cbind(ReprojRD,pts)
    rm(surfref_water,pts,ReprojRD)
    #Save as .csv file
    write.table(rasValues,file="surfref_Water.csv",append=F,sep= ",",row.names=F,col.names=T)
    #Save as shapefile
    shapefile(rasValues, "surfref_Water.shp", overwrite = T)
    rm(rasValues)
    #Remove any temporary files
    removeTmpFiles()#}
    setwd("../..") #Go back to main directory containing all files
}


######################################################################################################
#Run this code to extract values ONLY from already calculated rasters
#Clear all variables
rm(list=ls())

#Load relevant libraries
library(raster)
library(rgdal)

#Set working directory - where all data to be analysed is located
setwd("Y:/BHP - BHP Billiton/BHP1704 ED Consultancy/GIS/Doce water colour baseline/Satellite Images Rio Doce basin/")
RioDoce=shapefile("RioDocePoints.shp")

#Get directories of all locations of downloaded Landsat images
FilePaths=list.dirs() #Get all directories
#Select only directories with satellite images
FilePaths=subset(FilePaths,grepl(glob2rx("*/201*"),FilePaths))
FilePaths=FilePaths[-which(grepl("gap_mask",FilePaths)|grepl("Done",FilePaths))]

for(j in 1:length(FilePaths)){
  setwd(FilePaths[j])
  #Upload water multiband raster
  surfref_Water=stack("surfref_Water.tif")
  #Upload river points
  ReprojRD=spTransform(RioDoce,crs(surfref_Water))
  pts=extract(surfref_Water,ReprojRD)
  rasValues=cbind(ReprojRD[,1:2],pts)
  shapefile(rasValues, "surfref_Water.shp", overwrite = T)
  write.table(rasValues,file="surfref_Water.csv",append=F,sep= ",",row.names=F,col.names=T)
  removeTmpFiles(h=3)
  setwd("../..") #Go back to main directory containing all files
}


######################################################################################################

##Analysing data points
rm(list=ls())
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)

#Set working directory - where all data to be analysed is located
setwd("Y:/BHP - BHP Billiton/BHP1704 ED Consultancy/GIS/Doce water colour baseline/Satellite Images Rio Doce basin/")

#Get directories of all locations of downloaded Landsat images
FilePaths=list.dirs()
#Select only directories containing csv files
FilePaths=subset(FilePaths,grepl(glob2rx("*/201*"),FilePaths))
FilePaths=FilePaths[-which(grepl("gap_mask",FilePaths)|grepl("Done",FilePaths))]

#Select files paths by satellite
L8=subset(FilePaths,grepl(glob2rx("*L8$"),FilePaths))
L7=subset(FilePaths,grepl(glob2rx("*L7$"),FilePaths))

#Seasons:
#Winter (Cool & dry) - April to September
#Summer (Warm & wet) - October to March
S1718=c(paste0("*2017",(c(10:12))),paste0("*2018",(str_pad(c(1:3),2,pad = "0"))))
W17=paste0("*2017",(str_pad(c(4:9),2,pad = "0")))
S1617=c(paste0("*2016",(c(10:12))),paste0("*2017",(str_pad(c(1:3),2,pad = "0"))))
W16=paste0("*2016",(str_pad(c(4:9),2,pad = "0")))
S1516=c(paste0("*2015",(c(10:12))),paste0("*2016",(str_pad(c(1:3),2,pad = "0"))))
W15=paste0("*2015",(str_pad(c(4:9),2,pad = "0")))

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
#SitesGraphs has reference sites merged and ocean areas have been excluded
SitesGraphs=data.frame(names=c("Reference Sites","Imp - Bento Ck","Imp - RGdN","Imp - RdCarmo to Piranga",
                               "Imp - RD Piranga to Candonga","Imp - Candonga",
                               "Imp - RD ds Candonga to R Matipo","Imp - RD Matipo to Piracicaba",
                               "Imp - RD Piracicaba to SA","Imp - RD SA to Saucui Grande",
                               "Imp - RD SG to Aimores","Imp - RD Mascarenhas",
                               "Imp - RD Mascarenhas to Mouth"))
SitesGraphs$order=c(1:nrow(SitesGraphs))

#Landsat 8 bands
L8Bands=data.frame(band=c("B1_sre","B2_sre","B3_sre","B4_sre","B5_sre","B6_sre","B7_sre"))
L8Bands$color=c("Ultra blue","Blue","Green","Red","Near Infrared","Shortwave 1","Shortwave 2")

#Landsat 7 bands
L7Bands=data.frame(band=c("B1_sre","B2_sre","B3_sre","B4_sre","B5_sre","B6_sre"))
L7Bands$color=c("Blue","Green","Red","Near Infrared","Shortwave 1","Shortwave 2")

#Assign variable names
VarNam=data.frame(filenames=paste0(rep(c("S","W")),c(1718,17,1617,16,1516,15)),
                  season=rep(c("Wet","Dry")))
VarNam$L8=paste0(rep("L8_"),VarNam$filenames)
VarNam$L8pts=paste0(VarNam$L8,"pts")
VarNam$L7=paste0(rep("L7_"),VarNam$filenames)
VarNam$L7pts=paste0(VarNam$L7,"pts")
VarNam$years=str_sub(VarNam$filenames,2)


#Select files paths by year, season and satellite
for(i in 1:nrow(VarNam)){
  assign(VarNam$L8[i],unlist(lapply(get(paste(VarNam$filenames[i])),function(x) subset(L8,grepl(x,L8)))))
  assign(VarNam$L8pts[i],do.call(rbind,lapply(get(VarNam$L8[i]),
                                           function(x) read.csv(paste0(x,"/surfref_Water.csv")))))}
datasets=VarNam$L8pts

for(i in seq_along(datasets)){
  y <- get(datasets[i])
  levels(y$Name)=c(levels(y$Name),"Reference Sites")
  y$Name=replace(y$Name,grepl("Ref*",y$Name),"Reference Sites")
  y=y[-c(which(grepl("Ocean",y$Name))),]
  y$Order=str_pad(match(y$Name,SitesGraphs$names),2,pad="0")
  y=unite(y,Name,Order,Name,sep="_")
  assign(datasets[i],y)
  x=melt(y[,2:9])
  for(j in 1:nrow(L8Bands)){
    ggplot(x[c(which(x$variable==L8Bands$band[j])),],aes(x=variable,y=value,fill=Name))+theme_bw()+
      geom_boxplot(notch=T,na.rm=T)+labs(x=L8Bands$color[j],y="At-surface reflectance",
                                         title=paste0(VarNam$season[i]," season ",VarNam$years[i]),
                                         caption="Impacted sites ordered from upstream to downstream")+
      guides(fill=guide_legend(title = "Name river section"))+ylim(0,0.20)+
      theme(axis.text.x=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
            legend.text=element_text(size=8))
    ggsave(paste0("Reflectance plots/",VarNam$years[i]," ",VarNam$season[i]," ",L8Bands$color[j],".pdf"))
  }}
  
  
 


L8_17W=unlist(lapply(W17,function(x) subset(L8,grepl(x,L8))))
L8_17Wpts=do.call(rbind,lapply(L8_17W,function(x) read.csv(paste0(x,"/surfref_Water.csv"))))
levels(L8_17Wpts$Name)=c(levels(L8_17Wpts$Name),"Reference Sites")
L8_17Wpts$Name=replace(L8_17Wpts$Name,grepl("Ref*",L8_17Wpts$Name),"Reference Sites")
L8_17Wpts=L8_17Wpts[-c(which(grepl("Ocean",L8_17Wpts$Name))),]
L8_17Wpts$Order=str_pad(match(L8_17Wpts$Name,SitesGraphs$names),2,pad="0")
L8_17Wpts=unite(L8_17Wpts,Name,Order,Name,sep="_")

x=melt(L8_17Wpts[,2:9])
for(i in 1:nrow(L8Bands)){
  ggplot(x[c(which(x$variable==L8Bands$band[i])),],aes(x=variable,y=value,fill=Name))+theme_bw()+
    geom_boxplot(notch=T,na.rm=T)+labs(x=L8Bands$color[i],y="At-surface reflectance",title="Dry season 2017",
                                       caption="Impacted sites ordered from upstream to downstream")+
    guides(fill=guide_legend(title = "Name river section"))+ylim(0,0.20)+
    theme(axis.text.x=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
          legend.text=element_text(size=8))
  ggsave(paste0("Reflectance plots/2017 Dry ",L8Bands$color[i],".pdf"))
  }

#Select files paths by year, season and satellite
L8_1617S=unlist(lapply(S1617,function(x) subset(L8,grepl(x,L8))))
L8_1617Spts=do.call(rbind,lapply(L8_1617S,function(x) read.csv(paste0(x,"/surfref_Water.csv"))))
levels(L8_1617Spts$Name)=c(levels(L8_1617Spts$Name),"Reference Sites")
L8_1617Spts$Name=replace(L8_1617Spts$Name,grepl("Ref*",L8_1617Spts$Name),"Reference Sites")
L8_1617Spts=L8_1617Spts[-c(which(grepl("Ocean",L8_1617Spts$Name))),]
L8_1617Spts$Order=str_pad(match(L8_1617Spts$Name,SitesGraphs$names),2,pad="0")
L8_1617Spts=unite(L8_1617Spts,Name,Order,Name,sep="_")

x=melt(L8_1617Spts[,2:9])
for(i in 1:nrow(L8Bands)){
  ggplot(x[c(which(x$variable==L8Bands$band[i])),],aes(x=variable,y=value,fill=Name))+theme_bw()+
    geom_boxplot(notch=T,na.rm=T)+labs(x=L8Bands$color[i],y="At-surface reflectance",title="Wet season 2017",
                                       caption="Impacted sites ordered from upstream to downstream")+
    guides(fill=guide_legend(title = "Name river section"))+ylim(0,0.20)+
    theme(axis.text.x=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
          legend.text=element_text(size=8))
  ggsave(paste0("Reflectance plots/2017 Wet ",L8Bands$color[i],".pdf"))
}







assign(Sites[i], do.call(rbind, lapply(temp, function(x) read.delim(x,header = T,skip = 4))))


#Get directories of all scenes
Scenes=list.dirs(recursive = F)
SceneNames=str_sub(Scenes,3)

#Uploading csv files for analysis
for(i in seq_along(Scenes)){
  setwd(Scenes[i])
  #Uploading reflectance values
  folders=list.dirs(recursive = F)
  folders=folders[-which(grepl("Done",folders))]
  for(j in seq_along(folders)){
    setwd(folders[j])
    assign(str_sub(folders,3)[j], read.csv("surfref_Water.csv"))
    setwd("..")}
  
  
  test=list(get(L8[1:2]))
    names(which(sapply(.GlobalEnv, is.data.frame)))
  
  L8=str_sub(subset(folders,grepl(glob2rx("*L8$"),folders)),3)
  if(length(L8)!=0){
    setwd(L8[i])
    dates=as.numeric(gsub("[^0-9]","",toupper(L8)))
  }
  L7=subset(folders,grepl(glob2rx("*L7$"),folders))
  
  assign(str_sub(L8,3)[i], read.csv("surfref_Water.csv"))
  
  MT=list.files(pattern = "MTL.txt$")

  for(i in 1:length(Sites)){
    setwd(FilePath[i])
    temp=list.files(pattern = "_BiomassBasket_Bnr5.txt") #get a list of files with a particular ending
    assign(Sites[i], do.call(rbind, lapply(temp, function(x) read.delim(x,header = T,skip = 4)))) #for each step, create
    #a variable with the name Sites[i] (using assign()), read contents and bind them together
    setwd("..") #go back two folders in directory -> setwd("../..") #to move back one folder -> setwd("..")
  }


  
  
  test = `20150130_L8` %>%
    group_by(Name) %>%
    summarise_at(.vars = names(.)[3:9],mean,na.rm=T)
  test=as.data.frame(test)
  row.names(test)=paste(test$Name)
  test=as.matrix(test[,-1])

  plot(test[1,],type="n",ylab="",ylim=c(min(test,na.rm=T),round(max(test,na.rm=T),1)),xlab = "",xaxt="n",cex.axis=0.8,
       cex.lab=0.8)
  axis(1,at=1:ncol(test),labels = colnames(test),las=2,cex.axis=0.8)
  mtext("Surface Reflectance", cex=0.8,side=2,line=2)
  for(i in 1:nrow(test)){
    lines(as.numeric(paste(test[i,])),col=i,lty=i,pch=i,type = "o")}
  legend("topright",legend=rownames(test),cex=0.7,col=c(1:nrow(test)),lty=c(1:nrow(test)),
         inset=c(-0.25,0),pch = c(1:nrow(test)),title="River section",bty="n")
  
  test1 = `20150420_L8` %>%
    group_by(Name) %>%
    summarise_at(.vars = names(.)[3:9],mean,na.rm=T)
  test1=as.data.frame(test1)
  row.names(test1)=paste(test1$Name)
  test1=as.matrix(test1[,-1])
  
  plot(test1[1,],type="n",ylab="",ylim=c(min(test1,na.rm=T),round(max(test1,na.rm=T),2)),xlab = "",xaxt="n",cex.axis=0.8,
       cex.lab=0.8)
  axis(1,at=1:ncol(test1),labels = colnames(test1),las=2,cex.axis=0.8)
  mtext("Surface Reflectance", cex=0.8,side=2,line=2)
  for(i in 1:nrow(test1)){
    lines(as.numeric(paste(test1[i,])),col=i,lty=i,pch=i,type = "o")}
  legend("topright",legend=rownames(test1),cex=0.7,col=c(1:nrow(test1)),lty=c(1:nrow(test1)),
         inset=c(-0.25,0),pch = c(1:nrow(test1)),title="River section",bty="n")
  
  test1 = `20150420_L8` %>%
    group_by(Name) %>%
    boxplot(.vars=names(.)[,3:9])
  
library(reshape2)
test.m=melt(L8_17Wpts[,2:9])
library(ggplot2)
ggplot(test.m[c(which(test.m$variable=="B1_sre")),],aes(x=variable,y=value,fill=Name))+geom_boxplot()



# object.size(MB)
# nlayers(MB)
# plotRGB(MB,r=7,g=5,b=3) #plots multilayered raster in color. r=red, g=green, b=blue
# plot(MB,1)
# plot(new, 2, add=T)
# new = read.csv("rasValues.csv")
# hist(new$LE07_L1TP_216073_20170102_20170223_01_T1_B1)
