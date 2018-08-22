######### Pre-processing Landsat (5,7,8) images to obtain atmospherically corrected reflectance values ###########
# Author: Denisse Fierro Arcos
# Version: 2
# Purpose: To obtain atmospherically corrected at-surface reflectance values for all 30 meter resolution bands
# using Landsat 5, Landsat 7 and Landsat 8 satellite imagery. Reflectance values are only calculated within a
# polygon of interest, which covers the Doce River and about 1 km either side of the river.
# BQA layer is used to masked out any clouded areas.
# Area of study: Rio Doce, Brazil
# Inputs required: 
#   Working directory (main folder containing images and feature layers of the area of interest)
#   Polygon of area of interest
#   Points of the river to be used to extract reflectance values.
##################################################################################################################

#Clear all variables from workspace
rm(list=ls())

#Uploading relevant libraries
library(stringr)
library(raster)
library(rgdal)
library(RStoolbox)

#Set working directory - main folder containing images and feature layers of the area of interest
setwd("T:/GIS_BHP1704/Doce water colour baseline/Satellite Images Rio Doce basin/") # user defined

#Upload points for Rio Doce Path shapefile
RioDocepts = shapefile("RioDoceEdited20180716.shp") # river points - user defined

#Getting all directories contained within main folder
Scenes = list.dirs(recursive = F)

#Creating loop to process all images contained within main folder
for(j in 1:(length(Scenes)-1)){
  #Set new working directory to scene folder
  setwd(Scenes[j])
  #Get a list of all date folders within scene
  Folders = list.dirs(recursive = F)
  Folders = Folders[c(grepl(glob2rx("*2015*"),Folders))]
  #Set new working directory to date folder
  for(i in 1:(length(Folders)-1)){
    setwd(Folders[i])
    #if(length(list.files(pattern="surfRefRiver.csv"))==0){
      #Find name of metadata file
      MT = list.files(pattern = "MTL.txt$")
      #Upload metadata file
      meta = readMeta(MT)
      #Upload cloud free multilayered raster created by arcpy
      MB1Km = stack("masked1Kmreproj.TIF")
      #Identify water only areas (NDVI - NDWI)
      if(grepl(glob2rx("*L5"),Folders[i]) | grepl(glob2rx("*L7"),Folders[i])){
        #Modified NDWI (SWIR1 band used instead of NIR)
        ndwi = spectralIndices(MB1Km, green = 2, nir = 5, indices = "NDWI") 
        ndvi = spectralIndices(MB1Km, red = 3, nir = 4,indices = "NDVI")
      }else if(grepl(glob2rx("*L8"),Folders[i])){
        #Modified NDWI (SWIR1 band used instead of NIR)
        ndwi = spectralIndices(MB1Km, green = 3, nir = 6, indices = "NDWI")
        ndvi = spectralIndices(MB1Km, red = 4, nir = 5, indices = "NDVI")}
      difND = ndvi - ndwi
      rm(ndvi, ndwi)
      #Create a mask using water only areas
      difND[difND >= 0] = NA
      #Apply mask of water only areas on multiband raster
      MB = mask(MB1Km, difND)
      rm(MB1Km, difND)
      #Change names of MB bands so it can find the names in the metadata file
      if(grepl(glob2rx("*L5"),Folders[i]) | grepl(glob2rx("*L8"),Folders[i])){
        names(MB) = c("B1_dn", "B2_dn", "B3_dn", "B4_dn", "B5_dn", "B6_dn", "B7_dn")
      }else if(grepl(glob2rx("*L7"),Folders[i])){
        names(MB) = c("B1_dn", "B2_dn", "B3_dn", "B4_dn", "B5_dn", "B7_dn")}
      #Calculate at-surface reflectance for all layers in masked raster
      if(grepl(glob2rx("*L7"),Folders[i]) | grepl(glob2rx("*L5"),Folders[i])){
        haze = estimateHaze(MB, hazeBands = 1:3)
        riverArea = radCor(MB, meta, method = 'sdos', hazeValues = haze, hazeBands = 1:3)
      }else if(grepl(glob2rx("*L8"),Folders[i])){
        haze = estimateHaze(MB, hazeBands = 1:4)
        riverArea = radCor(MB, meta, method = 'sdos', hazeValues = haze, hazeBands = 1:4)}
      rm(haze,MB)
      # #Reproject resulting raster to WGS84 and saving it in folder
      # riverAreaReproj = projectRaster(riverArea, crs = "+init=epsg:4326")
      writeRaster(riverArea, filename = "riverAreaMB.tif", options="INTERLEAVE=BAND", overwrite=T)
      #Extract at-surface reflectance values from all layers into river points
      pts = extract(riverArea, RioDocepts)
      rasValues = cbind(RioDocepts, pts)
      rm(riverArea, pts)
      #Save reflectance values as .csv file
      write.table(rasValues, file="surfRefRiver.csv", append=F, sep= ",", row.names=F, col.names=T)
      #Save reflectance values as shapefile
      shapefile(rasValues, "surfRefRiver.shp", overwrite = T)
      rm(rasValues)
      #Remove any temporary files
      dir_to_clean = tempdir()
      unlink(dir_to_clean)
      removeTmpFiles()#}
    setwd("..")} #Go back one folder
  setwd("..")} #Go back to main directory containing all files

