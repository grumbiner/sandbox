import os
import sys
import numpy as np
import numpy.ma as ma
import netCDF4
from netCDF4 import Dataset

#----------------------------------------------
def oiv2(lat, lon):
  dlat = 0.25
  dlon = 0.25
  firstlat = -89.875
  firstlon = 0.125
  if (lon < 0):
    lon += 360.
  j = round( (lat - firstlat)/dlat )
  i = round( (lon - firstlon)/dlon )
  return (j,i)

def rg12th(lat, lon):
  dlat = -1./12.
  dlon = 1./12.
  firstlat = 90. - dlat/2.
  firstlon = dlon/2.
  if (lon < 0):
    lon += 360.
  j = round( (lat - firstlat)/dlat )
  i = round( (lon - firstlon)/dlon )
  return (j,i)

def delta(x,y):
  return (x-y)/(x+y)

###############################################################
ssmif15 = Dataset('l2out.f248.51.nc', 'r', format='NETCDF4')
nobs = len(ssmif15.dimensions["nobs"])
    #print("nobs = ",nobs)
longitude = np.zeros((nobs)) 
latitude = np.zeros((nobs)) 
icec = np.zeros((nobs)) 
quality = np.zeros((nobs), dtype='int') 
land = np.zeros((nobs)) 
dtg1 = np.zeros((nobs), dtype='int') 
dtg2 = np.zeros((nobs), dtype='int') 
t19v = np.zeros((nobs)) 
t19h = np.zeros((nobs)) 
t22v = np.zeros((nobs)) 
t37v = np.zeros((nobs)) 
t37h = np.zeros((nobs)) 
t85v = np.zeros((nobs)) 
t85h = np.zeros((nobs)) 

longitude = ssmif15.variables["longitude"][:] 
latitude = ssmif15.variables["latitude"][:] 
icec = ssmif15.variables["ice_concentration"][:] 
quality = ssmif15.variables["quality"][:] 
land = ssmif15.variables["land_flag"][:] 
dtg1 = ssmif15.variables["dtg_yyyymmdd"][:] 
dtg2 = ssmif15.variables["dtg_hhmm"][:] 
t19v = ssmif15.variables["tb_19V"][:] 
t19h = ssmif15.variables["tb_19H"][:] 
t22v = ssmif15.variables["tb_22V"][:] 
t37v = ssmif15.variables["tb_37V"][:] 
t37h = ssmif15.variables["tb_37H"][:] 
t85v = ssmif15.variables["tb_85V"][:] 
t85h = ssmif15.variables["tb_85H"][:] 
print("f15 ice, quality, land max min: ",icec.max(), icec.min(), quality.max(), quality.min(), land.max(), land.min(), t37v.max(), t37v.min() )
#exit(0)

###########################################
ssmif17 = Dataset('l2out.f285.51.nc', 'r', format='NETCDF4')
s_nobs = len(ssmif17.dimensions["nobs"])
print("f17 nobs = ",nobs)
s_longitude = np.zeros((nobs)) 
s_latitude = np.zeros((nobs)) 
s_icec = np.zeros((nobs)) 
s_quality = np.zeros((nobs), dtype='int') 
s_land = np.zeros((nobs)) 
s_dtg1 = np.zeros((nobs), dtype='int') 
s_dtg2 = np.zeros((nobs), dtype='int') 
s_t19v = np.zeros((nobs)) 
s_t19h = np.zeros((nobs)) 
s_t22v = np.zeros((nobs)) 
s_t37v = np.zeros((nobs)) 
s_t37h = np.zeros((nobs)) 
s_t92v = np.zeros((nobs)) 
s_t92h = np.zeros((nobs)) 

s_longitude = ssmif17.variables["longitude"][:] 
s_latitude = ssmif17.variables["latitude"][:] 
s_icec = ssmif17.variables["ice_concentration"][:] 
s_quality = ssmif17.variables["quality"][:] 
s_land = ssmif17.variables["land_flag"][:] 
s_dtg1 = ssmif17.variables["dtg_yyyymmdd"][:] 
s_dtg2 = ssmif17.variables["dtg_hhmm"][:] 
s_t19v = ssmif17.variables["tb_19V"][:] 
s_t19h = ssmif17.variables["tb_19H"][:] 
s_t22v = ssmif17.variables["tb_22V"][:] 
s_t37v = ssmif17.variables["tb_37V"][:] 
s_t37h = ssmif17.variables["tb_37H"][:] 
s_t92v = ssmif17.variables["tb_92V"][:] 
s_t92h = ssmif17.variables["tb_92H"][:] 

print('longs ',longitude.max(), longitude.min(), s_longitude.max(), s_longitude.min() )
#exit(0)

#----------------------------------------------
dlat = -1./12.
dlon = 1./12.
firstlat = 90. - dlat/2.
firstlon = dlon/2.
i_f15 = np.zeros((nobs), dtype='int')
j_f15 = np.zeros((nobs), dtype='int')
i_f15 = np.rint( (longitude - firstlon)/dlon)
j_f15 = np.rint( (latitude - firstlat) /dlat) 
print("i max min, j max min: ",i_f15.max(), i_f15.min(), j_f15.max(), j_f15.min() )

#Because of how python wraps negative indices, don't need bound check
#flop = ma.masked_array(i_f15 < 0)
#findices = flop.nonzero()
#for k in range(0, len(findices[0])):
#  i = findices[0][k]
#  i_f15[i] += 4320
#print("fixed i indices for f15", flush=True)

icefix = Dataset('seaice_fixed_fields.nc', 'r', format='NETCDF4')
nlats = len(icefix.dimensions["nlats"])
nlons = len(icefix.dimensions["nlons"])

ice_longitude = np.zeros((nlats, nlons),dtype="double") 
ice_latitude = np.zeros((nlats, nlons),dtype="double") 
ice_distance = np.zeros((nlats, nlons),dtype="float") 

ice_land = np.zeros((nlats, nlons))
ice_land = icefix.variables["land"]     [:,:] 

ice_post = np.zeros((nlats, nlons))
ice_post = icefix.variables["posteriori"][:,:] 

ice_longitude = icefix.variables["longitude"][:,:] 
ice_latitude  = icefix.variables["latitude"] [:,:] 
ice_distance  = icefix.variables["distance_to_land"][:,:] 
ice_distance /= 1000.   #Convert to km

distance = np.zeros((nobs))
land = np.zeros((nobs))
post = np.zeros((nobs))
for k in range(0,nobs):
  distance[k] = ice_distance[int(j_f15[k]), int(i_f15[k]) ]

print("distance max min: ",distance.max(), distance.min(), flush=True)

#--------------------------------------------------------
# Use SST from qdoi v2, including its sea ice cover
sstgrid = Dataset('avhrr-only-v2.20180228.nc', 'r', format='NETCDF4')
sst_nlats = len(sstgrid.dimensions["lat"])
sst_nlons = len(sstgrid.dimensions["lon"])

sst = np.zeros((sst_nlats, sst_nlons))
ice_sst = np.zeros((sst_nlats, sst_nlons))

sst     = sstgrid.variables["sst"][0,0,:,:]
ice_sst = sstgrid.variables["ice"][0,0,:,:]

del i_f15, j_f15

dlat = 0.25
dlon = 0.25
firstlat = -89.875
firstlon = 0.125

i_f15 = np.zeros((nobs), dtype='int')
j_f15 = np.zeros((nobs), dtype='int')
i_f15 = np.rint( (longitude - firstlon)/dlon)
j_f15 = np.rint( (latitude -  firstlat)/dlat) 
sst_f15 = np.zeros((nobs))
ice_f15 = np.zeros((nobs))

i_f17 = np.zeros((s_nobs), dtype='int')
j_f17 = np.zeros((s_nobs), dtype='int')
i_f17 = np.rint( (s_longitude - firstlon)/dlon)
j_f17 = np.rint( (s_latitude -  firstlat)/dlat) 
sst_f17 = np.zeros((s_nobs))
ice_f17 = np.zeros((s_nobs))

fout = open("f15","w")
for k in range (0, nobs):
  sst_f15[k] = sst[int(j_f15[k]), int(i_f15[k]) ]
  ice_f15[k] = ice_sst[int(j_f15[k]), int(i_f15[k]) ]
  print(longitude[k], latitude[k], icec[k], quality[k], sst_f15[k], ice_f15[k], file = fout)
fout.close()
del i_f15, j_f15

fout = open("f17","w")
for k in range (0, s_nobs):
  sst_f17[k] = sst[int(j_f17[k]), int(i_f17[k]) ]
  ice_f17[k] = ice_sst[int(j_f17[k]), int(i_f17[k]) ]
  print(s_longitude[k], s_latitude[k], s_icec[k], s_quality[k], sst_f17[k], ice_f17[k], file=fout)
fout.close()
del i_f17, j_f17


#---------------------------------------------------------------------
