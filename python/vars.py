#!/usr/bin/python

import numpy as np
import domain

k=1
nx=360*1024
ny=180*1024

for j in range(2,1025,2):
  k = j
  print("k = ",k)

  x = np.zeros( (k*nx, k*ny) )
  print(x.shape)
  print(x.size / 1e9)

print("done")
