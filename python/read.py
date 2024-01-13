import time
start = time.time()

import os
import numpy as np
from struct import *

from grid import *
from ijpt import *

#====================================================
cmax = 1.0
i = 0
j = 0

mapper = global_5min()
nx = mapper.nx
ny = mapper.ny
fmt=str(nx*ny)+'f'

land     = np.zeros((nx, ny))
floatmap = np.zeros((nx,ny),dtype=float) #single precision float

fin = open('testin','rb')
binary = fin.read()
x = unpack(fmt,binary[0:4*nx*ny])

count = 0
for val2 in x:
  j = count / nx
  i = count % nx
#  if (val2 > cmax):
#    land[i,j] = 1.0
#    val2 = 0.0 #reset flag values to zero
  floatmap[i,j] = val2
  count += 1

fin.close()
print("reading time ",time.time() - start)
