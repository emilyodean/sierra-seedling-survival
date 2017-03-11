# Clips rasters with a polygon featureclass

import arcpy
from arcpy import env
from arcpy.sa import *
arcpy.CheckOutExtension("3D")

# Set Over write
arcpy.env.overwriteOutput = 1

# Set the workspace
env.workspace = r"Z:\temp.gdb"
Dir = env.workspace

# Local variables:
counties = r"Z:\temp.gdb\boundaries\counties"
counties_lyr = arcpy.MakeFeatureLayer_management(counties,"counties_lyr")

# Get the list of rasters to process
raster_list = arcpy.ListRasters("*_clp")
print raster_list

for raster in raster_list:
    # Define name and location for output raster
    name = Dir + "\\" + str(raster) + "_clp"
    
    # Clip Raster
    arcpy.Clip_management(raster, "#", name,counties_lyr, "#", "ClippingGeometry")

    print "processing " + raster + " complete..."

print "All processing is now finished"