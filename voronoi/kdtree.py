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

#------
# set up a sample grid mesh for timing and testing
grid = []
m = 5 #magnification
for j in range (0, int((90-50)*m)):
  for i in range (0, int((100-(-100))*m) ):
    tmp = [-100+float(i)/m, 50+float(j)/m ]
    grid.append( tmp )
#debug: print(grid)

#---------- KDTree nearest neighbor ------------------------
from scipy.spatial import *
kdt = KDTree(points)
#print(kdt)

dub = 25.
x = kdt.query_pairs(dub)
#print(x)
print("there are ",len(x)," pairs of buoys within ",dub," of each other")

#k is up to and including kth nearest
#distance upper bound is more to do with simplifying life for algorithm
#dist,ind = kdt.query(grid, k=1)

dist,ind = kdt.query(grid, k=1, distance_upper_bound = dub)

#print(dist)
#print(ind)

#Masked array is better
for dub in range (1,int(dub+2),3):
  count = 0
  for i in range(0,len(dist)):
    if ((dist[i] <= dub )):
    #if (not (dist[i] <= dub )):
      count += 1
      #print (i,grid[i])
  print(count," grid points of ",len(grid), " closer than ",dub," from observation")


#print(count," grid points of ",len(grid), " farther than ",dub," from observation")

gridtree = KDTree(grid) 
gp = gridtree.query_ball_tree(kdt, 5.)
pg = kdt.query_ball_tree(gridtree, 5.)
#debug: print(len(gp), len(pg))
#debug: print(pg[1])
for i in range(0,len(pg)):
  print(i,len(pg[i]), points[i] )
