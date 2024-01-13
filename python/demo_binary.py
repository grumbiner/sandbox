import os
import numpy as np
from struct import *

from grid import *
from ijpt import *

#====================================================
loc = ijpt()
z = ijpt()

#loc.i = 1
#loc.j = 2
#loc.show()

toler = 3

nx = 4
ny = 9

lats = np.zeros((nx, ny))
lons = np.zeros((nx, ny))
conc = np.zeros((nx, ny))

charmap  = np.zeros((nx,ny),'B') #unsigned char
floatmap = np.zeros((nx,ny),'f') #single precision float

print("nx = ",lats.shape[0],"ny = ", lats.shape[1])

for i in range (0,nx):
  for j in range (0,ny): 
    lats[i,j] = j 
    lons[i,j] = i
    conc[i,j] = i+j
    floatmap[i,j] = float(i+j)

#print(lats[1,2])
#print(lons[1,2])
#print(conc[1,2])

#floatmap = float(conc)
fout = open('testout','wb')
fout.write(floatmap)
fout.close()
print('floatmap out ',floatmap[1,2])

#alpha = "asdf"
#print("alpha", alpha[0:2])

fin = open('testin','rb')
#fin.read(floatmap)
#floatmap = fin.read()

binary = fin.read()
print(len(binary))   # this is a string

#print(float(binary[0:3]))
#alpha=eval(binary[0:4])
#print('new alpha ', float(alpha))
alpha=binary[0:4]

#works x = unpack('f', alpha)
#works x = unpack('36f',binary[0:144])
fmt=str(nx*ny)+'f'
print(fmt,' = fmt')
x = unpack(fmt,binary[0:4*nx*ny])
#works print("x = ", x,' type = ',type(x))
count = 0
for val2 in x:
  j = count % ny
  i = count / ny
  #works print('i,j ',i,j,' ',val2, 'type val2 = ',type(val2))
  floatmap[i,j] = val2
  count += 1

#fails floatmap[0,0] = unpack('f',binary[0:4])
#fails floatmap.fromfile(fin, nx*ny)

fin.close()
print('floatmap in ',floatmap[1,2])


##########################################
# Find ice edge (transitions over/under tolerance
##########################################
for i in range (0,nx):
  loc.i = i
  for j in range (0,ny):
    loc.j = j
    im = loc.im()
    jm = loc.jm()
    ip = loc.ip()
    jp = loc.jp()
#    loc.show()
#    im.show()
#    print(ok(lats,loc), ok(lats,im))
    if (ok(conc,im)):
      if (conc[loc.i,loc.j] >= toler and conc[im.i, im.j] < toler):
        midpoint(loc, im, z)
        print("im")
        z.show()

    if (ok(conc,ip)):
      if (conc[loc.i,loc.j] >= toler and conc[ip.i, ip.j] < toler):
        midpoint(loc, ip, z)
        print("ip")
        z.show()

    if (ok(conc,jm)):
      if (conc[loc.i,loc.j] >= toler and conc[jm.i, jm.j] < toler):
        midpoint(loc, jm, z)
        print("jm")
        z.show()

    if (ok(conc,jp)):
      if (conc[loc.i,loc.j] >= toler and conc[jp.i, jp.j] < toler):
        midpoint(loc, jp, z)
        print("jp")
        z.show()

