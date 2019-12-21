import time
start = time.time()

import os
import numpy as np
from struct import *

from grid import *
from ijpt import *

#====================================================
toler = 0.15
cmax = 1.0

mapper = global_5min()
nx = mapper.nx
ny = mapper.ny

land = np.zeros((nx, ny))
floatmap = np.zeros((nx,ny),dtype=float) #single precision float

fin = open('testin','rb')
binary = fin.read()

#Read and Parse in to proper place in array:
fmt=str(nx*ny)+'f'
x = unpack(fmt,binary[0:4*nx*ny])
landmax = 0.0

count = 0
for val2 in x:
  j = count / nx
  i = count % nx
  if (val2 > cmax):
    land[i,j] = 1.0
    landmax = 1.0
    val2 = 0.0 #reset flag values to zero
  floatmap[i,j] = val2
  count += 1

fin.close()
print "land max = ",landmax
print "reading time ",time.time() - start

##########################################
# Find ice edge (transitions over/under tolerance
##########################################

loc = ijpt()
z = ijpt()
im = ijpt(5,5)
loc.j = 5
loc.i = 5

for i in range (0,nx):
  for j in range (0,ny):
    if land[i,j] != 1. :  
#This adds ~10 seconds over a scalar if 
#  9.3 million evaluations -- dereferences are the issue
        midpoint(loc, im, z)
