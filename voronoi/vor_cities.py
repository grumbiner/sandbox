import csv
import datetime
from math import *

import numpy as np
import scipy
import matplotlib

#Version to work on a sphere, rather than 2d. 
#  Assumes latitude, longitude are in degrees.
#  Approximates earth as perfect sphere, radius = 6371.2 km

def ll_to_sphere(lat, lon, radius = 6371.2):
  x = radius*cos(lat*np.pi/180.)*cos(lon*np.pi/180.)
  y = radius*cos(lat*np.pi/180.)*sin(lon*np.pi/180.)
  z = radius*sin(lat*np.pi/180.)

  return [x,y,z]


# working with: https:// (geonames)

#declare var to accumulate obs lats, lons
points = []
names  = []
pop    = []
clat   = []
clon   = []

count = 0

fin = open("sorted_cities_by_size.csv","r")
for line in fin:
  words = line.split(",")
  tlat = float(words[1])
  tlon = float(words[2])
  if (tlat > -90. and tlon > -181. and tlon < 181.):
    #regularize lons + add to accumulator
    if (tlon > 180.):
      tlon -= 360.
    if (tlon < -180.):
      tlon += 360.
    tpt = ll_to_sphere(tlat,tlon)
    count += 1
    names.append(words[0])
    clat.append(tlat)
    clon.append(tlon)
    pop.append(int(words[3]))
    points.append(tpt)
    if (count > 18900 or int(words[3]) <= 500000 ):  #err between 18900 and 18920
      break


#debug: 
print("number of cities:",len(points), flush=True)

from scipy.spatial import *

radius = 6371.2
center = np.array([0, 0, 0])
sv = SphericalVoronoi(points, radius, center)

x = sv.calculate_areas()
#debug print(sv.calculate_areas(), flush=True)
#debug print(x.max(), x.min(), x.sum(), flush=True ) 


sumpop = 0
sumar  = 0.
sumlat = 0.
print("area (1000 km^2)  Population Lat Lon  Name")
for i in range(0,len(x)):
  sumpop += pop[i]
  sumar  += x[i]
  sumlat += pop[i]*clat[i]
  print("{:11.5f}".format(x[i]/1e3), "{:10d}".format(pop[i]), 
         clat[i], clon[i], names[i] )

print("total pop, area ",sumpop, sumar /1.e6)
print("pop weighted central lat ",sumlat/sumpop)

exit(0)

import matplotlib.pyplot as plt
fig = voronoi_plot_2d(vor)
plt.show()

