import csv
import datetime

import numpy
import scipy
import matplotlib

#---------- Acquiring Data, nothing particular to Voronoi Tesselation ----
#declare var to accumulate obs lats, lons
points = []

# working with: https://iabp.apl.uw.edu/data.html

#iabp:
# flag_phys = -999
# flag_loc  = -9999


fin = open("ArcticTable_Current.txt","r")
for line in fin:
  words = line.split(";")
  if (len(words) != 12):
    print("missing data in line: ",line,flush=True)
  else:
    tlat = float(words[7])
    tlon = float(words[8])
    has_pressure = (float(words[9]) != -999)
    #all valid data:  if (tlat > -90 and tlon > -999 ):        # lat of -90 is also used for unknown
    if (tlat > 50. and tlon > -100 and tlon < 100):
      #debug: print(tlat, tlon, has_pressure)
      #regularize lons + add to accumulator
      if (tlon > 180.):
        tlon -= 360.
      if (tlon < -180.):
        tlon += 360.
      points.append([tlon, tlat])


print("length of points list:",len(points))

#---------- Voronoi Tesselation ------------------------
from scipy.spatial import Voronoi, voronoi_plot_2d
vor = Voronoi(points)

print(vor.vertices)
print("\n")
print(vor.regions)

#Detecting regions which extend to infinity:
for simplex in vor.ridge_vertices:
  simplex = numpy.asarray(simplex)
  if (numpy.any(simplex < 0)):
    print(simplex)

print(len(vor.vertices), len(vor.regions), len(vor.ridge_vertices))
for i in range(0,len(vor.regions)):
  print(i, len(vor.regions[i]), vor.regions[i])

#---------- Plotting a 2d Voronoi Tesselation ------------------------
import matplotlib.pyplot as plt
fig = voronoi_plot_2d(vor)
plt.show()

