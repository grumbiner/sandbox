import os
import time

import numpy as np
from struct import *

from grid import *
from ijpt import *
#Robert Grumbine
#1 June 2018

#====================================================

loc = ijpt()
z = ijpt()

toler = 0.15
cmax = 1.0

mapper = global_5min()
nx = mapper.nx
ny = mapper.ny

lats = np.zeros((nx, ny))
lons = np.zeros((nx, ny))
land = np.zeros((nx, ny))
#print "nx = ",lats.shape[0],"ny = ", lats.shape[1]

charmap  = np.zeros((nx,ny),'B') #unsigned char
floatmap = np.zeros((nx,ny),'f') #single precision float

start = time.time()
fin = open('testin','rb')
binary = fin.read()
#print len(binary)   # this is a string

#Read and Parse in to proper place in array:
fmt=str(nx*ny)+'f'
#print fmt,' = fmt'
x = unpack(fmt,binary[0:4*nx*ny])
count = 0
for val2 in x:
  #j = count % ny
  #i = count / ny
  j = count / nx
  i = count % nx
  if (val2 > cmax):
    land[i,j] = 1.0
    val2 = 0.0 #reset flag values to zero
  floatmap[i,j] = val2
  #if (floatmap[i,j] > toler and floatmap[i,j] <= cmax ):
  #  print i,j
  count += 1

fin.close()

#print "reading time ",time.time() - start

##########################################
# Find ice edge (transitions over/under tolerance
##########################################

ll = latpt()

for i in range (0,nx):
  loc.i = i
  for j in range (0,ny):
    loc.j = j
    if (land[i,j] != 1.):
      im = loc.im()
      jm = loc.jm()
      ip = loc.ip()
      jp = loc.jp()
      if (ok(floatmap,im)):
        if (floatmap[loc.i,loc.j] >= toler and floatmap[im.i, im.j] < toler and land[im.i,im.j] != 1.):
          midpoint(loc, im, z)
          mapper.locate(z.i, z.j, ll)
          ll.show()
  
      if (ok(floatmap,ip)):
        if (floatmap[loc.i,loc.j] >= toler and floatmap[ip.i, ip.j] < toler and land[ip.i, ip.j] != 1.):
          midpoint(loc, ip, z)
          mapper.locate(z.i, z.j, ll)
          ll.show()
  
      if (ok(floatmap,jm)):
        if (floatmap[loc.i,loc.j] >= toler and floatmap[jm.i, jm.j] < toler and land[jm.i,jm.j] != 1.):
          midpoint(loc, jm, z)
          mapper.locate(z.i, z.j, ll)
          ll.show()
  
      if (ok(floatmap,jp)):
        if (floatmap[loc.i,loc.j] >= toler and floatmap[jp.i, jp.j] < toler and land[jp.i, jp.j] != 1.):
          midpoint(loc, jp, z)
          mapper.locate(z.i, z.j, ll)
          ll.show()

