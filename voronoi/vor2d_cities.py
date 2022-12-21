import sys

import numpy
import scipy
import matplotlib

#declare var to accumulate obs lats, lons
points = []

# working with: 


fin = open("cities.csv","r")
k = 0
errcount = 0

for line in fin:
  k += 1
  if (k == 1):
    continue
  l2 = line.replace("\",",";")
  l3 =   l2.replace("\"" ,"")
  l4 = l3.strip()

  words = l4.split(";")
  #debug: print(len(words)) #should be 11
  tlat = float(words[2].replace("\"",""))
  #debug: print(k,tlat,flush=True)
  tlon = float(words[3].replace("\"",""))
  #debug: print(k,tlon,flush=True)
  try:
    pop  = int(float(words[9]))
  except:
    errcount += 1
    #print(k, line,end="")
    #print(k, l2,end="")
    #print(k, l3,end="")
    #print(k, l4)
    #print(k, words[9], words[10])
    #print(k, words, len(words) )
    pop = 0
  #debug: print(k, words[0], tlat, tlon, pop,flush=True)
  
  #if (tlat > -90. and tlon > -180 and tlon < 180): #whole globe
  #if (tlat > 20. and tlat < 60 and tlon > -125 and tlon < -60.): #~lower 48 + most Canada, some Mexico
  #if (tlat > 30. and tlat < 50 and tlon > -90. and tlon < -70.): #Eastern NA
  #if (tlat > 38. and tlat < 45. and tlon > -78. and tlon < -70.): #Bosnywash
  #if (tlat > 39.-1. and tlat < 39+1. and tlon > -77.-1 and tlon < -77.+1): #DC area +- 1 degree
  #if (tlat > -90. and tlon > -180 and tlon < 180 and pop > int(sys.argv[1]) ): #whole globe
  #if (tlat > 20. and tlat < 60 and tlon > -125 and tlon < -60. and pop > int(sys.argv[1]) ): #~lower 48 + most Canada, some Mexico
  if (tlat > 25. and tlat < 49. and tlon > -125 and tlon < -70. and pop > int(sys.argv[1]) ): #~lower 48 + most Canada, some Mexico
    points.append([tlon, tlat])
    #print(words[0].replace(" ","_"),";", tlat,";", tlon,";", pop,flush=True)
    print(words[0].replace(" ","_"), tlat, tlon, pop,flush=True)

#debug: print("errcount = ",errcount)
print("length of points list:",len(points))

from scipy.spatial import *
vor = Voronoi(points)

#exit(0)
#import matplotlib
import matplotlib.pyplot as plt
fig = voronoi_plot_2d(vor, show_vertices = False, point_size = 2.5, line_width=0.9, line_alpha=0.5)
plt.grid()
try:
  plt.savefig(sys.argv[2])
except:
  plt.savefig("cities2d.png")

