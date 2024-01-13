import os
import sys
import time
from math import *
import numpy as np
import numpy.ma as ma
from netCDF4 import Dataset

def scan(x):
  print(x.max(), x.min())

#def scan(x, param):
#  print(param, x.max(), x.min())

def scan(x, param, lats, lons, upper = float(+999.), lower = float(-900.) ):
  y = ma.masked_outside(x, lower, upper)
  j = int(0)
  i = int(0)
  if ( not ((x.count() - y.count()) == 0) ) :
    for j in range (0,y.shape[0]):
      for i in range (0, y.shape[1]):
        if (y.mask[j,i] and not x.mask[j,i]):
          print(i,j,lats[j,i], lons[j,i], x[j,i], param, 'out of range')
  #print('unmasked y, x, x-y ',y.count(), x.count(), x.count() - y.count() )
  #print(param, y.max(), y.min(), 'ny = ',y.shape[0], 'nx = ',y.shape[1], upper, lower)
  print(param, y.max(), y.min())
  


#====================================================================
coupled_ice = Dataset('ice2014070200.01.2014070100.nc', 'r', format='NETCDF4')
nx = len(coupled_ice.dimensions["X"])
ny = len(coupled_ice.dimensions["Y"])
print(nx,' ',ny)

lats = np.zeros((ny, nx))
lons = np.zeros((ny, nx))
lats = coupled_ice.variables["Latitude"][:,:]
lons = coupled_ice.variables["Longitude"][:,:]
print('lat max min ',lats.max(), lats.min() )
print('lon max min ',lons.max(), lons.min() )
i=int(0)
j = int(0)
for j in range (0,lons.shape[0]):
  for i in range (0, lons.shape[1]):
    if (lons[j,i] > 440.):
      lons[j,i] -= 360.


uvel_ocean = np.zeros((ny, nx),dtype="float64")
vvel_ocean = np.zeros((ny, nx),dtype="float64")
o_speed    = np.zeros((ny, nx),dtype="float64")

sst        = np.zeros((ny, nx))
sss        = np.zeros((ny, nx))
rho        = np.zeros((ny, nx))

uvel_ocean = coupled_ice.variables["u_velocity"][0,0,:,:]
vvel_ocean = coupled_ice.variables["v_velocity"][0,0,:,:]
sst        = coupled_ice.variables["sst"][0,:,:]
sss        = coupled_ice.variables["sss"][0,:,:]
rho        = coupled_ice.variables["layer_density"][0,0,:,:]

j=int(ny/2)
i=int(nx/2)
print('i j element ',i,' ',j,' ',sst[j,i],' u ', uvel_ocean[j,i])

scan(sst, 'sst', lats, lons, 38.0, -2.3)
scan(sss, 'sss', lats, lons, 40.0, 10.0)
scan(rho, 'rho', lats, lons)
scan(uvel_ocean, 'u_ocean', lats, lons, 2.0, -2.0)
scan(vvel_ocean, 'v_ocean', lats, lons, 2.0, -2.0)

#Getting an overflow in multiplication -- only from u, not v:
print('start with u')
o_speed    = uvel_ocean*uvel_ocean
print('now for v')
o_speed    += vvel_ocean * vvel_ocean

#Vastly faster to used the numpy masked array than to do the if check
o_speed = np.sqrt(o_speed)
scan(o_speed, 'speed', lats, lons, 2.0, 0.0)
