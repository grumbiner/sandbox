import csv
import datetime
from math import *

import numpy as np
import scipy
import matplotlib

#Version to work on a sphere, rather than 2d. 
#  Assumes latitude, longitude are in degrees.
#  Approximates earth as perfect sphere, radius = 6371.2 km

#declare var to accumulate obs lats, lons
points = []

# working with: https://iabp.apl.uw.edu/data.html

#iabp:
# flag = -999

def ll_to_sphere(lat, lon, radius = 6371.2):
  x = radius*cos(lat*np.pi/180.)*cos(lon*np.pi/180.)
  y = radius*cos(lat*np.pi/180.)*sin(lon*np.pi/180.)
  z = radius*sin(lat*np.pi/180.)

  return [x,y,z]

fin = open("ArcticTable_Current.txt","r")
for line in fin:
  words = line.split(";")
  if (len(words) != 12):
    print("missing data in line: ",line,flush=True)
  else:
    tlat = float(words[7])
    tlon = float(words[8])
    has_pressure = (float(words[9]) != -999)
    #all valid data:  
    if (tlat > -90 and tlon > -999 ):        # lat of -90 is also used for unknown
    #if (tlat > 50. and tlon > -100 and tlon < 100):
      #debug: print(tlat, tlon, has_pressure)
      #regularize lons + add to accumulator
      if (tlon > 180.):
        tlon -= 360.
      if (tlon < -180.):
        tlon += 360.
      tpt = ll_to_sphere(tlat,tlon)
      points.append(tpt)
      #debug: print(tpt[0], tpt[1], tpt[2], sqrt(tpt[0]**2 + tpt[1]**2 + tpt[2]**2 ), flush=True )


#debug: print("length of points list:",len(points), flush=True)

from scipy.spatial import *

radius = 6371.2
center = np.array([0, 0, 0])
sv = SphericalVoronoi(points, radius, center)

#note that even for small subset, it is apportioning the globe. RG: Need to crop

x = sv.calculate_areas()
#debug print(sv.calculate_areas(), flush=True)
#debug print(x.max(), x.min(), x.sum(), flush=True ) 


for i in range(0,len(x)):
  print(i,x[i]/1e6)

exit(0)

import matplotlib.pyplot as plt
fig = voronoi_plot_2d(vor)
plt.show()

