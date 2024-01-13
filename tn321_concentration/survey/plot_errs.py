import os
import sys

import numpy as np

fin = open(sys.argv[1],"r")


lon = []
lat = []
msize = []

for line in fin:
  words = line.split()
  lon.append(float(words[0])-360.)
  lat.append(float(words[1]))
  msize.append(int(words[2]))

print("found ",len(lat)," error points")
#debug: 
print(max(lat), min(lat), max(lon), min(lon) )
latmax = max(lat)
latmin = min(lat)
lonmax = max(lon)
lonmin = min(lon)

markersize = 4
c = np.zeros((len(msize)))

cmax = 0
for i in range(0,len(msize)):
  c[i] = msize[i] 
#14 months ~426d
#10 months ~304d
c = np.minimum(122,c)

cmax = c.max()
print("cmax2 = ",c.max() )

#x,b = np.histogram(c,bins = int(cmax+1),range=(0,int(cmax+1)) )
#for i in range(0,int(cmax+1) ):
#  print(x[i],b[i])

c *= (256./cmax )
print("",flush=True)
#debug: exit(0)


# i-j plot of error points ----------------------------------
import matplotlib
import matplotlib.pyplot as plt
matplotlib.use('Agg') #batch mode

# lat-lon plot of error points ---------------------------------
import cartopy.crs as ccrs
import cartopy.feature as cfeature

#proj = ccrs.LambertConformal(central_longitude=-50., central_latitude = 50., cutoff=25.)
proj = ccrs.PlateCarree()

ax = plt.axes(projection = proj)
fig = plt.figure(figsize = (8,6))
ax = fig.add_subplot(1,1,1,projection = proj)
plt.title("counts")

ax.set_extent( (-75,-42.5, 40, 55), crs=ccrs.PlateCarree() )

#ax.gridlines(crs=ccrs.PlateCarree(), xlocs=xlocs, ylocs=ylocs )
ax.gridlines(crs=ccrs.PlateCarree() )
# not on hera: ax.coastlines()
ax.add_feature(cfeature.GSHHSFeature(levels=[1], scale="f") )
ax.add_feature(cfeature.GSHHSFeature(levels=[2,3,4], scale="f") )
#plt.scatter(lon, lat, transform=ccrs.PlateCarree(), s = markersize, c = msize, cmap='plasma')
plt.scatter(lon, lat, transform=ccrs.PlateCarree(), s = markersize, c = msize, cmap='prism')
plt.savefig("count.png")
plt.close()

