import sys
import numpy as np

nx = 1440
ny = 1080
count = np.zeros((nx, ny),dtype=int)
lats  = np.zeros((nx, ny),dtype=float)
lons  = np.zeros((nx, ny),dtype=float)

f = open(sys.argv[1],"r")
while f:
  x = f.readline()
  words = x.split()
  if (len(words) < 5):
    break
  i = int(words[1])
  j = int(words[2])
  count[i,j] += 1
  lats[i,j] = float(words[4])
  lons[i,j] = float(words[3])

for j in range(0,ny):
  for i in range(0,nx):
    if (count[i,j] != 0):
      print("{:4d}".format(i),"{:4d}".format(j),"{:4d}".format(count[i,j]),
             lons[i,j],lats[i,j])

