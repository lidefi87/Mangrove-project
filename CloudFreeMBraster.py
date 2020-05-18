########### Creating cloud free multiband rasters using Landsat satellite imagery ##########
# Author: Denisse Fierro Arcos
# Version: 1
# Date: 2018-07-06
# Purpose: This script works with Landsat (5, 7 & 8) Collection 1 Level 1 satellite imagery.
# A cloud-free multiband raster within an area of interest is created as output.
# Main folder containing all satellite files need to be provided as input.
############################################################################################

#Uploading relevant libraries
import os
import arcpy

#Activate Spatial Analyst extension
arcpy.CheckOutExtension("Spatial")

#Authorising outputs to be overwritten. This can be changed, but output names must also be changed
arcpy.env.overwriteOutput = True

#Uploading Rio Doce buffer polygon to define area of interest (AOI)
DoceBuff = r"C:\BufferPts.shp"
arcpy.env.mask = DoceBuff

#Setting working directory to main folder containing landsat images
FP = r"C:/Satellite Images Rio Doce basin/"
#Getting a list of all scenes contained in folder
x = os.listdir(FP)
y = []
for i in x:
        if i.startswith("P21"):
            y.append(i)
scenes = y[:-1]
del x,y

#Creating full filepaths for each scene
pathScenes = []
for i in scenes:
    pathScenes.append(os.path.join(FP,i))
del(scenes)

#Create a variable with Landsat bands of interest
x = ["B" + str(i) for i in xrange(1,8)]
Bands = []
for i in x:
    Bands.append(i+".TIF")
del(x)

#Define function MBstack. This function will do the following:
# - Create a multiband raster with bands of interest only
# - Mask resulting raster using the river buffer
# - Landsat Quality layer is used to create a cloud mask
# - Create final multiband raster without any clouds
def MBstack(x):
    #Get a list of all folder dates included under each scene folder
    z = os.listdir(x)
    for i in z[:-1]:
        #Set environment to date folder
        arcpy.env.workspace = os.path.join(x,i)
        #Get a list of all .tif raster contained in folder
        bandsList = arcpy.ListRasters("","TIF")
        if ("maskedMB.TIF" in bandsList) == False:
                #Identify Landsat quality band and other bands of interest
                qual = []
                BandsNames = []
                for j in bandsList:
                    if j.endswith("BQA.TIF"):
                        qual = j
                    for k in Bands:
                        if j.endswith(k):
                            BandsNames.append(j)
                #Getting information about raster for use in other processes
                rasterBand = arcpy.sa.Raster(BandsNames[0])
                Ext = arcpy.Describe(rasterBand)
                #Create multiband raster with bands of interest
                CompRaster = arcpy.CompositeBands_management(BandsNames, "Composite.TIF")
                ClipComp = arcpy.Clip_management(CompRaster, str(Ext.extent), "ClipComp.TIF","","0","NONE","NO_MAINTAIN_EXTENT")
                #Mask raster with AOI polygon
                MaskClip = arcpy.sa.ExtractByMask(ClipComp, DoceBuff)
                #Create quality band raster
                BQA = arcpy.sa.Raster(qual)
                #Set cloud values to be used as cloud mask
                if i.endswith("L5") or i.endswith("L7"):
                    remap = arcpy.sa.RemapValue([[928,0],[932,0],[936,0],[940,0],[960,0],[964,0],[968,0],[972,0],[752,0],[756,0],[760,0],[764,0]])
                elif i.endswith("L8"):
                    remap = arcpy.sa.RemapValue([[2976,0],[2980,0],[2984,0],[2988,0],[3008,0],[3012,0],[3016,0],[3020,0],[7072,0],[7076,0],[7080,0],[7084,0],[7104,0],[7108,0],[7112,0],[7116,0],[2800,0],[2804,0],[2808,0],[2812,0],[6896,0],[6900,0],[6904,0],[6908,0]])
                #Creating cloud mask
                clouds = arcpy.sa.Reclassify(BQA, "VALUE", remap)
                nullClouds = arcpy.sa.SetNull(clouds, clouds, "VALUE = 0")
                MaskClouds = arcpy.sa.ExtractByMask(nullClouds, DoceBuff)
                MaskClouds.save("MaskClouds.TIF")
                #Masking out clouds in multiband raster
                maskedMB = arcpy.sa.ExtractByMask(MaskClip,MaskClouds)
                #Save final output
                maskedMB.save("maskedMB.TIF")
        else:
                print i

#Apply function to all folder scenes
for scenes in pathScenes:
    MBstack(scenes)

#Deactivate Spatial Analyst extension
arcpy.CheckInExtension("Spatial")
      

