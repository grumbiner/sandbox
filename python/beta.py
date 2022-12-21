import os
import sys
import time
from math import *
import numpy as np
import numpy.ma as ma
from netCDF4 import Dataset

#====================================================================
from eyeball import *


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
print('lon max min ',lons.max(), lons.min(), flush=True )
i = int(0)
j = int(0)
#for j in range (0, ny):
#  for i in range (0, nx):
#    if (lons[j,i] > 440.):
#      lons[j,i] -= 360.

#print('about to get physical parms', flush=True)
sss        = np.zeros((ny, nx))
sss        = coupled_ice.variables["sss"][0,:,:]
sst_read        = np.zeros((ny, nx))
sst_read        = coupled_ice.variables["sst"][0,:,:]
rho        = np.zeros((ny, nx))
rho        = coupled_ice.variables["layer_density"][0,0,:,:]

#sys.stderr.flush()
#print('starting with eyeball grids ',flush=True)
salinity = eyeball_grid(sss, "salinity")
#salinity_l = eyeball_limits(0., 357.)
#salinity.bounded(lats, lons, salinity_l)
#salinity_l2 = eyeball_limits(5.0, 47.0)
#salinity.bounded(lats, lons, salinity_l2)
salinity_l3 = eyeball_limits(salinity.mean - 5.*sqrt(salinity.var), salinity.mean + 5.*sqrt(salinity.var))
#salinity.bounded(lats, lons, salinity_l3)

#if (salinity.gaussian()):
#  print("true in main")
#else:
#  print("gaussian false in main")

print("salinity gaussian tests ",salinity.gaussian(), salinity.gaussian3(), salinity.gaussian4() )


#sys.stderr.flush()
#print('starting with speed grids ',flush=True)
uvel_ocean = np.zeros((ny, nx),dtype="float64")
vvel_ocean = np.zeros((ny, nx),dtype="float64")
o_speed    = np.zeros((ny, nx),dtype="float64")
uvel_ocean = coupled_ice.variables["u_velocity"][0,0,:,:]
vvel_ocean = coupled_ice.variables["v_velocity"][0,0,:,:]
#print('done initializing speed and velocity',flush = True)

uvel = eyeball_grid(uvel_ocean, "u_ocean")
vvel = eyeball_grid(vvel_ocean, "v_ocean")
sst  = eyeball_grid(sst_read, "sst")

print("uvel gaussian tests ",uvel.gaussian(), uvel.gaussian3(), uvel.gaussian4() )
print("vvel gaussian tests ",vvel.gaussian(), vvel.gaussian3(), vvel.gaussian4() )
print("sst  gaussian tests ",sst.gaussian(), sst.gaussian3(), sst.gaussian4() )


#bounds(sst, 'sst', lats, lons, 38.0, -2.3)
#bounds(salinity, 'salinity', lats, lons, 40.0,  5.0)
#bounds(rho, 'rho', lats, lons)
#bounds(uvel_ocean, 'u_ocean', lats, lons, 2.5, -2.5)
#bounds(vvel_ocean, 'v_ocean', lats, lons, 2.5, -2.5)
#
##Getting an overflow in multiplication -- only from u, not v:
#print('start with u')
#o_speed    = uvel_ocean * uvel_ocean
#print('now for v')
#o_speed    += vvel_ocean * vvel_ocean
#
##Vastly faster to used the numpy masked array than to do the if check
#o_speed = np.sqrt(o_speed)
#bounds(o_speed, 'speed', lats, lons, 2.0, 0.0)
#stats(o_speed, 'o_speed', 14.)
