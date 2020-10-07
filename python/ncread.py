import os
import sys
import numpy as np
import netCDF4
from netCDF4 import Dataset

dtsum=np.int16
dtsumsq=np.int64

alpha =  Dataset('201805/avhrr-only-v2.20180501.nc', 'r', format='NETCDF4')
#print(alpha.data_model)
#print("\n")
print(alpha)
nlats = len(alpha.dimensions["lat"])
nlons = len(alpha.dimensions["lon"])
print("nlon, nlat ",nlons, nlats)
#print(alpha.variables["sst"][0,0,:,5] )

beta = np.zeros((nlats,nlons))
sum  = np.zeros((nlats,nlons),dtype=dtsum)
sumsq = np.zeros((nlats,nlons),dtype=dtsumsq)

beta = alpha.variables["err"][0,0,:,:]
beta = alpha.variables["anom"][0,0,:,:]
beta = alpha.variables["ice"][0,0,:,:]
beta = alpha.variables["sst"][0,0,:,:]
beta *= 100.
sum   += beta.astype(int)
sumsq += beta.astype(int)*beta.astype(int)

print(sum[int(nlats/2),int(nlons/2)])
print(sum.max(), sumsq.max())
print(beta.shape)

#gamma =  Dataset('/Volumes/Data/qdoi/201805/avhrr-only-v2.20180501.nc', 'r', format='NETCDF4')
