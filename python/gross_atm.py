import os 
import sys 
import datetime
from math import *
import numpy as np

import netCDF4
#files atmos_4xdaily: 
#slp [time, ny, nx]
#us, u1000, u700, u500, u200, u100, u50, u10
#vs, v1000, v700, v500, v200, v100, v50, v10
#tm, t1000, t700, t500, t200, t100, t50, t10
#q1000, q700, q500, q200, q100, q50, q10

#files phyfHHH.tileN.nc


if (not os.path.exists(sys.argv[1]) ):
  print("failed to find ",sys.argv[1])
  exit(1)
else:
  model = netCDF4.Dataset(sys.argv[1],'r')
  nt = len(model.dimensions['time'])
  print("dimensions: ",nx, ny, nt)
  slp   = model.variables['slp'][:,:,:]
  us   = model.variables['us'][:,:,:]
  vs   = model.variables['vs'][:,:,:]
  u1000   = model.variables['u1000'][:,:,:]
  v1000   = model.variables['v1000'][:,:,:]
  tm   = model.variables['tm'][:,:,:]
  t1000   = model.variables['t1000'][:,:,:]
  q1000   = model.variables['q1000'][:,:,:]
  #for i in range(0,nt):
  #  print("slp ",i,slp[i,:,:].max(), slp[i,:,:].min() )

  #grid_spec, tile3=Arctic, tile1=Antarctic
  static = netCDF4.Dataset(sys.argv[2], 'r')
  nx = len(static.dimensions['grid_xt'])
  ny = len(static.dimensions['grid_yt'])
  tlons = static.variables['grid_lont'][:,:]
  tlats = static.variables['grid_latt'][:,:]
  areas = static.variables['area'][:,:]
  nxc = len(static.dimensions['grid_x'])
  nyc = len(static.dimensions['grid_y'])
  clons = static.variables['grid_lon'][:,:]
  clats = static.variables['grid_lat'][:,:]

  print("lat, lon, max, min: ",tlats.max(), tlats.min(), tlons.max(), tlons.min() )
  print("lat, lon, max, min: ",clats.max(), clats.min(), clons.max(), clons.min() )

  #where(lat > 60)
  #near(lat,lon,tolerance=50km)

