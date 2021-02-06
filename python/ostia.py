import os
import sys
import time

import numpy as np
from netCDF4 import Dataset
ostia = Dataset('20190801-UKMO-L4HRfnd-GLOB-v01-fv02-OSTIA.nc', 'r', format='NETCDF4')
nx = len(ostia.dimensions["lon"])
ny = len(ostia.dimensions["lat"])

print(nx,' ',ny)
lats = np.zeros((ny))
lons = np.zeros((nx))
lats = ostia.variables["lat"][:]
lons = ostia.variables["lon"][:]

sst = np.zeros((ny, nx))
sst = ostia.variables["analysed_sst"][:,:]

analy_error = np.zeros((ny,nx))
analy_error = ostia.variables["analysis_error"][:,:]

icec = np.zeros((ny,nx))
icec = ostia.variables["sea_ice_fraction"][:,:]

mask = np.zeros((ny,nx))
mask = ostia.variables["mask"][:,:]

print(sst.max(),' ',analy_error.max(), ' ',icec.max(), ' ',mask.max() )

