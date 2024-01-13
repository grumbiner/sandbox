import os
import sys

import numpy as np

fin = open(sys.argv[1],"r")

try:
  title_tag = sys.argv[2]
except:
  title_tag = "ref"

markersize = float(sys.argv[3])

i = []
j = []
lon = []
lat = []

for line in fin:
  words = line.split()
  conc = float(words[15])
  if (conc > 0 and conc <= 100.0):
    lon.append(float(words[1]))
    lat.append(float(words[2]))

print("found ",len(lat)," error points")
#debug: 
print(max(lat), min(lat), max(lon), min(lon) )
latmax = max(lat)
latmin = min(lat)
lonmax = max(lon)
lonmin = min(lon)


# i-j plot of error points ----------------------------------
import matplotlib
import matplotlib.pyplot as plt
matplotlib.use('Agg') #batch mode

#Elaborations:
#  title
#  axis labels
#  separate color/symbol per parameter
#  different sizes per parameter

# lat-lon plot of error points ---------------------------------
import cartopy.crs as ccrs
import cartopy.feature as cfeature

#proj = ccrs.LambertConformal(central_longitude=-170., central_latitude = 60., cutoff=25.)
proj = ccrs.PlateCarree()

ax = plt.axes(projection = proj)
fig = plt.figure(figsize = (8,6))
ax = fig.add_subplot(1,1,1,projection = proj)
plt.title(title_tag)

#xlocs = list(range(-180,181,30))
#xlocs = list(range(10*int(lonmin/10)-1, 10*int(lonmax/10), 1))
xlocs = list(range(-60,-45,1))

#if ((latmax - latmin) < 30):
#  mean = (latmax + latmin)/2.
#  ax.set_extent((lonmin, lonmax, latmin, latmax), crs=ccrs.PlateCarree() )
#  ylocs = list(range(int(latmin), int(latmax)+1, 1) )
#else:
#  ylocs = list(range(-90, 91, 15))
ylocs = list(range(40,55,1))
ax.set_extent( (-60,-45, 40, 55), crs=ccrs.PlateCarree() )

ax.gridlines(crs=ccrs.PlateCarree(), xlocs=xlocs, ylocs=ylocs )
# not on hera: ax.coastlines()
ax.add_feature(cfeature.GSHHSFeature(levels=[1], scale="f") )
ax.add_feature(cfeature.GSHHSFeature(levels=[2,3,4], scale="f") )
plt.scatter(lon, lat, transform=ccrs.PlateCarree(), s = markersize)
plt.savefig("ll_errs_"+title_tag+".png")
plt.close()

print(markersize)
