import os
import time

import numpy as np
from struct import *

from grid import *
from ijpt import *

#====================================================

loc = ijpt()

mapper = global_5min()
nx = mapper.nx
ny = mapper.ny

lats = np.zeros((nx, ny))
alpha = np.zeros((nx, ny))
lons = np.zeros((nx, ny))

for i in range (0,nx):
  for j in range (0,ny):
    lats[i,j] = float(i)
    lons[i,j] = float(j)
    alpha[i,j] = float(j)

print 4*nx*ny


count = 0
while (1 != 2):
  count += 1 
