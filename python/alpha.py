#!/Library/Frameworks/Python.framework/Versions/3.7/bin/python3
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

lats = np.zeros((ny, nx), dtype="float64")
lons = np.zeros((ny, nx), dtype="float64")
lats = coupled_ice.variables["Latitude"][:,:]
lons = coupled_ice.variables["Longitude"][:,:]
print('lat max min ',lats.max(), lats.min() )
print('lon max min ',lons.max(), lons.min(), flush=True )

tmp        = np.zeros((ny, nx))
tmp        = coupled_ice.variables["sss"][0,:,:]
salinity = eyeball_grid(tmp, "salinity")
salinity_lim = sss_hard_limits
salinity_lim.lows[0] = 5.0
salinity_lim.highs[0] = 45.0
#salinity_l3 = eyeball_limits(salinity.mean - 15.*sqrt(salinity.var), 
#                             salinity.mean + 15.*sqrt(salinity.var)  )
salinity.bounded(lats, lons, salinity_lim)

print("salinity gaussian tests ",salinity.gaussian(), salinity.gaussian3(), salinity.gaussian4() )

tmp        = coupled_ice.variables["sst"][0,:,:]
sst  = eyeball_grid(tmp, "sst")
sst_lim = eyeball_limits(-2.3, 38.0)
print("sst  gaussian tests ",sst.gaussian(), sst.gaussian3(), sst.gaussian4() )
sst.bounded(lats, lons, sst_lim)


tmp = coupled_ice.variables["u_velocity"][0,0,:,:]
uvel = eyeball_grid(tmp, "u_ocean")

tmp = coupled_ice.variables["v_velocity"][0,0,:,:]
vvel = eyeball_grid(tmp, "v_ocean")

print("uvel gaussian tests ",uvel.gaussian(), uvel.gaussian3(), uvel.gaussian4() )
print("vvel gaussian tests ",vvel.gaussian(), vvel.gaussian3(), vvel.gaussian4() )


##Vastly faster to used the numpy masked array than to do the if check
o_speed    = eyeball_grid(uvel.ma * uvel.ma + vvel.ma*vvel.ma, "ocean_speed")
#o_speed.ma    += vvel.ma * vvel.ma
o_speed.ma = np.sqrt(o_speed.ma)

# Need to recompute statistics after operating on the field:
o_speed.getstats()


