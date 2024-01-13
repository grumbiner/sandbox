import os
import sys
import shapefile

sf = shapefile.Reader(sys.argv[1])
print("number of records? ",len(sf.shapeRecords()) )

#first = feature.shape.__geo_interface__  
print(sf)
print(sf.shapeTypeName)
print(sf.bbox)

k = 0
for feature in sf.shapes():
   #NOT THIS: for feature in sf.shapeRecords():
   # feature of the shapefile
   #feature = sf.shapeRecords()[k]
   print(k, feature)
   #print(k, len(feature.points), feature.points)
   print(k, len(feature.points) )
   if (len(feature.points) < 10):
       for i in range(0,len(feature.points)):
           print(k,i,feature.points[i])
   k += 1
