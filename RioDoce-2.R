######################################################################################################

##Analysing data points
rm(list=ls())
library(stringr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(tidyr)

#Set working directory - where all data to be analysed is located
setwd("T:/GIS_BHP1704/Doce water colour baseline/Satellite Images Rio Doce basin/")

#Get directories of all locations of downloaded Landsat images
FilePaths=list.dirs()
#Select only directories containing csv files
FilePaths=subset(FilePaths,grepl(glob2rx("*/201*"),FilePaths))
FilePaths=FilePaths[-which(grepl("gap_mask",FilePaths)|grepl("Done",FilePaths))]

#Select files paths by satellite
L8=subset(FilePaths,grepl(glob2rx("*L8$"),FilePaths))
L7=subset(FilePaths,grepl(glob2rx("*L7$"),FilePaths))
# L5=subset(FilePaths,grepl(glob2rx("*L5$"),FilePaths))
rm(FilePaths)

# #Ensuring all datasets have the same column names
# namesL8=c("ID","Name","B1_sre","B2_sre","B3_sre","B4_sre","B5_sre","B6_sre","B7_sre","B9_sre","B10_bt","B11_bt",
#           "coords.x1","coords.x2","optional")
# namesL7=c("ID","Name","B1_sre","B2_sre","B3_sre","B4_sre","B5_sre","B6_VCID_1_bt","B6_VCID_2_bt","B7_sre",
#           "coords.x1","coords.x2","optional")
# namesL5=c("ID","Name","B1_sre","B2_sre","B3_sre","B4_sre","B5_sre","B6_VCID_1_bt","B6_VCID_2_bt","B7_sre",
#           "coords.x1","coords.x2","optional")
# 
# ChangeColNames=function(m,y){
#   for(j in 1:length(m)){
#     setwd(m[j])
#     if(length(list.files(pattern=".csv"))>0){
#       x=read.csv("surfref_Water.csv")
#       if((sum(names(x)!=y))>0){
#         names(x)=y
#         write.csv(x,file="surfref_Water.csv",row.names=F)}}
#     setwd("../..")}}
# ChangeColNames(L8,namesL8)
# ChangeColNames(L7,namesL7)

#Seasons:
#Dry/Winter (Cool & dry) - April to September
#Wet/Summer (Warm & wet) - October to March
D18=paste0("*2018",(str_pad(c(4:9),2,pad = "0")))
W1718=c(paste0("*2017",(c(10:12))),paste0("*2018",(str_pad(c(1:3),2,pad = "0"))))
D17=paste0("*2017",(str_pad(c(4:9),2,pad = "0")))
W1617=c(paste0("*2016",(c(10:12))),paste0("*2017",(str_pad(c(1:3),2,pad = "0"))))
D16=paste0("*2016",(str_pad(c(4:9),2,pad = "0")))
W1516=c(paste0("*2015",(c(10:12))),paste0("*2016",(str_pad(c(1:3),2,pad = "0"))))
# W15=paste0("*2015",(str_pad(c(4:9),2,pad = "0")))

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
L7Bands=data.frame(band=c("B1_sre","B2_sre","B3_sre","B4_sre","B5_sre","B6_VCID_1_bt","B6_VCID_2_bt","B7_sre"))
L7Bands$color=c("Blue","Green","Red","Near Infrared","Shortwave 1","Thermal1","Thermal2","Shortwave 2")

# #Landsat 5 bands
# L5Bands=data.frame(band=c("B1_sre","B2_sre","B3_sre","B4_sre","B5_sre","B6_bt","B7_sre"))
# L5Bands$color=c("Blue","Green","Red","Near Infrared","Shortwave 1","Thermal","Shortwave 2")

#Assign variable names
VarNam=data.frame(filenames=paste0(rep(c("D","W")),c(18,1718,17,1617,16,1516)),
                  season=rep(c("Dry","Wet")))
#VarNam=data.frame(filenames=paste0(rep(c("W","D")),c(15,1415,14,1314,13,1213,12,1211,11)),
#                  season=rep(c("Wet","Dry")))
VarNam$L8=paste0(rep("L8_"),VarNam$filenames)
VarNam$L8pts=paste0(VarNam$L8,"pts")
VarNam$L7=paste0(rep("L7_"),VarNam$filenames)
VarNam$L7pts=paste0(VarNam$L7,"pts")
# VarNam$L5=paste0(rep("L5_"),VarNam$filenames)
# VarNam$L5pts=paste0(VarNam$L5,"pts")
VarNam$years=str_sub(VarNam$filenames,2)

#Select files paths by year, season and satellite
for(i in 1:nrow(VarNam)){
  assign(VarNam$L8[i],unlist(lapply(get(paste(VarNam$filenames[i])),function(x) subset(L8,grepl(x,L8)))))
  assign(VarNam$L7[i],unlist(lapply(get(paste(VarNam$filenames[i])),function(x) subset(L7,grepl(x,L7)))))
  # assign(VarNam$L5[i],unlist(lapply(get(paste(VarNam$filenames[i])),function(x) subset(L5,grepl(x,L5)))))
  assign(VarNam$L8pts[i],do.call(rbind,lapply(get(VarNam$L8[i]),
                                              function(x) read.csv(paste0(x,"/surfref_Water.csv")))))
  assign(VarNam$L7pts[i],do.call(rbind,lapply(get(VarNam$L7[i]),
                                              function(x) read.csv(paste0(x,"/surfref_Water.csv")))))}
  # assign(VarNam$L5pts[i],do.call(rbind,lapply(get(VarNam$L5[i]),
                                              # function(x) read.csv(paste0(x,"/surfref_Water.csv")))))}

#Define function to create dataset needed for seasonal graphs
CreateData=function(x){
  levels(x$Name)=c(levels(x$Name),"Reference Sites")
  x$Name=replace(x$Name,grepl("Ref*",x$Name),"Reference Sites")
  x=x[-c(which(grepl("Ocean",x$Name))),]
  x$Order=str_pad(match(x$Name,SitesGraphs$names),2,pad="0")
  x=unite(x,Name,Order,Name,sep="_")
  return(x)}

for(i in 1:nrow(VarNam)){
  y=get(VarNam$L8pts[i])
  if(length(y)>0){
    colnames(y)[3:9]=L8Bands$color
    x=CreateData(y)
    y=melt(x[,2:9])
    rm(x)}
  w=get(VarNam$L7pts[i])
  if(length(w)!=0){
    colnames(w)[3:10]=L7Bands$color
    x=CreateData(w)
    w=melt(x[,c(2:7,10)])
    rm(x)}
  z=rbind(y,w)
  rm(y,w)
  if(length(z)!=0){
    for(j in 1:nrow(L8Bands)){
      ggplot(z[c(which(z$variable==L8Bands$color[j])),],aes(x=variable,y=value,fill=Name))+theme_bw()+
        geom_boxplot(notch=T,na.rm=T)+labs(x=L8Bands$color[j],y="At-surface reflectance",
                                           title=paste0(VarNam$season[i]," season ",VarNam$years[i]),
                                           caption="Impacted sites ordered from upstream to downstream")+
        guides(fill=guide_legend(title = "Name river section"))+ylim(0,0.40)+
        theme(axis.text.x=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
              legend.text=element_text(size=8))
      ggsave(paste0("Reflectance plots/",VarNam$years[i]," ",VarNam$season[i]," ",L8Bands$color[j],".pdf"))}
    }}

# assign(Sites[i], do.call(rbind, lapply(temp, function(x) read.delim(x,header = T,skip = 4))))
  # object.size(MB)
  # nlayers(MB)
  # plotRGB(MB,r=7,g=5,b=3) #plots multilayered raster in color. r=red, g=green, b=blue
  # plot(MB,1)
  # plot(new, 2, add=T)
  # new = read.csv("rasValues.csv")
  # hist(new$LE07_L1TP_216073_20170102_20170223_01_T1_B1)