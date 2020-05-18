#Uploading relevant libraries
import os
import arcpy

#Activate Spatial Analyst extension
arcpy.CheckOutExtension("Spatial")

#Authorising outputs to be overwritten. This can be changed, but output names must also be changed
arcpy.env.overwriteOutput = True

#Uploading Rio Doce buffer polygon to define area of interest (AOI)
DoceBuff = r"C:\Buffer1000m.shp"
arcpy.env.mask = DoceBuff

#Uploading river points with desired CRS
riverPts = r"C:/RioDoceEdited20180716.shp"

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

#Define function Mask1Km. This function will do the following:
# - Mask raster using 1 km river buffer
# - Landsat Quality layer is used to create a cloud mask
# - Create final multiband raster without any clouds
def Mask1Km(x):
    #Get a list of all folder dates included under each scene folder
    z = os.listdir(x)
    for i in z[:-1]:
        #Set environment to date folder
        arcpy.env.workspace = os.path.join(x,i)
        #Get a list of all .tif raster contained in folder
        bandsList = arcpy.ListRasters("","TIF")
        if ("masked1Kmreproj.TIF" in bandsList) == False:
                #Identify Landsat quality band and other bands of interest
                qual = []
                ClipComp = []
                for j in bandsList:
                        if j.endswith("BQA.TIF"):
                                qual = j
                        elif j.startswith("ClipComp"):
                                ClipComp = j
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
                #Reproject image and save output
                maskedMBreproj = arcpy.ProjectRaster_management(maskedMB,"masked1Kmreproj.TIF",riverPts,"BILINEAR")
        else:
                print i
        
#Apply function to all folder scenes
for scenes in pathScenes:
    Mask1Km(scenes)

#Deactivate Spatial Analyst extension
arcpy.CheckInExtension("Spatial")
      

