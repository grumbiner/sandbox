#import os
#import sys
from math import * 
import datetime
import numpy as np
import netCDF4
from netCDF4 import Dataset

tag=datetime.date(1981, 9,1)
ym=tag.strftime("%Y%m")

alpha = Dataset(ym+'/avhrr-only-v2.'+tag.strftime("%Y%m%d")+'.nc', 'r', format='NETCDF4')
nlats = len(alpha.dimensions["lat"])
nlons = len(alpha.dimensions["lon"])

#------------------------------------------------
beta = np.zeros((nlats,nlons),dtype='float64')
tmax = np.zeros((nlats,nlons),dtype='float64')
tmin = np.zeros((nlats,nlons),dtype='float64')
tmin = 3000.
tmax = -999.

sum     = np.zeros((nlats,nlons),dtype='int64')
sumsq   = np.zeros((nlats,nlons),dtype='int64')

avg = sum.astype('float64')

count=np.float64        #For the division in averaging
count = 0
dt=datetime.timedelta(1)

while (tag < datetime.date(2011,9,1)):
#while (tag < datetime.date(1981,9,10)):
  beta = alpha.variables["sst"][0,0,:,:]
  beta *= 100.

  tmax = np.fmax(tmax, beta)
  tmin = np.fmin(tmin, beta)
  sum   += beta.astype('int64')
  sumsq += (beta.astype('int64')*beta.astype('int64'))

  count += 1.0
  alpha.close()
  tag += dt 
  ym=tag.strftime("%Y%m")
  alpha = Dataset(ym+'/avhrr-only-v2.'+tag.strftime("%Y%m%d")+'.nc', 'r', format='NETCDF4')

#---------------- Compute summary stats ----------------------
print(count, tag)
avg = sum.astype('float64')
avg /= count

sq = sumsq.astype('float64') 
sq /= count
var = np.zeros((nlats, nlons),dtype='float64')

print('avg: ',round(avg.max(),2), avg.min() )
print("max sq = ",sumsq.max())
tmp=0

#Works, but very slow
#for j in np.nditer(sq, op_flags=['readwrite'] ):
#  if (j < 0):
#     j[...] = 0
#     tmp += 1

for j in range (0, nlats):
  for i in range (0, nlons):
    if (sq[j,i] < 0.):
      sq[j,i] = 0.
      var[j,i] = 0.
      tmp += 1
    else:
      var[j,i] = sq[j,i] - avg[j,i]*avg[j,i]

print('sq: ',sq.max(), sqrt(sq.max()), sq.min(), sqrt(sq.min()) )
print('sqrt(var): ', sqrt(var.max()), sqrt(var.min()) )

#------------- For grads
f = open('stats','wb')

tmp32 = np.zeros((nlats,nlons),dtype='float32')
tmp32 = avg.astype('float32')
f.write(tmp32)
tmp32 = var.astype('float32')
f.write(tmp32)
tmp32 = tmax.astype('float32')
f.write(tmp32)
tmp32 = tmin.astype('float32')
f.write(tmp32)

f.close()
#------------- End grads

#------------- NetCDF4 outputs:
outfile = Dataset("outtest.nc", "w", format="NETCDF4")
outfile.description = "sample output file description\n"
outfile.description += "  second line of description"
outfile.history     = "Created " + datetime.date(2000,12,31).strftime("%Y%m%d")
outfile.source      = "RG creation"
lat = outfile.createDimension("lat", nlats)
lon = outfile.createDimension("lon", nlons)
tempout = outfile.createVariable("temp","i2",("lat","lon"))
tempout.units = "C"
varout  = outfile.createVariable("var","i4",("lat","lon"))
varout.units  = "C**2"
tmaxout = outfile.createVariable("tmax","i2",("lat","lon"))
tmaxout.units = "C"
tminout = outfile.createVariable("tmin","i2",("lat","lon"))
tminout.units = "C"

tempout[:,:] = avg.astype('int16')
tmaxout[:,:] = tmax.astype('int16')
tminout[:,:] = tmin.astype('int16')
varout[:,:]  = var.astype('int32')

print(outfile)
outfile.close()
#-------- Note that the writing is done on file close

