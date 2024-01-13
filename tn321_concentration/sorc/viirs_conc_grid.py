import sys

import numpy as np
import numpy.ma as ma
import netCDF4 

from grid import *

#---------------------------------------------------------------------------
#Loop over input arg list (JRR-IceConcentration*)
#fname = "20220828/JRR-IceConcentration_v2r3_j01_s202208281036198_e202208281037426_c202208281059540.nc"

#For output grid:
target_grid = global_5min()
tsumx  = np.zeros((target_grid.ny,target_grid.nx))
tsumx2 = np.zeros((target_grid.ny,target_grid.nx))
gcount = np.zeros((target_grid.ny,target_grid.nx),dtype=int)

n = 0
nvalid = 0
totnp = 0
# For histogram
counts = np.zeros((10001),dtype=int)
#debug 
print("argc 0 and 1",sys.argv[0], sys.argv[1], flush=True)

for fname in sys.argv[1:]:
  #debug: 
  print(n, fname,flush=True)

  try:
    viirs = netCDF4.Dataset(fname, 'r')
  except:
    print("Could not open fname: ",fname,flush=True)
    continue

  n += 1
  np = viirs.variables['TotIceRetrvls'][:]
  #debug: 
  print("file ",n,"ice pixels ",np,flush=True)
  if (np == 0):
      continue
  nvalid += 1
  totnp += np
  if (totnp > 100e6): break

  #This is a masked array, determined by fill value
  conc = viirs.variables['IceConc'][:,:]
  #debug: print(n,np,"conc ",conc.max(), conc.min(),flush=True )

  #Geography:
  lats = viirs.variables['Latitude'][:,:]
  lons = viirs.variables['Longitude'][:,:]
  #debug: print("lats ",lats.max(), lats.min(), flush=True )
  #debug: print("lons ",lons.max(), lons.min(), flush=True )

  #QC:

  #Start Working:
  mask = ma.masked_array(conc)
  indices = mask.nonzero()
  #debug: print("len indices:",len(indices), len(indices[0]), flush=True)
  for k in range(0,len(indices[0])):
      i = indices[1][k]
      j = indices[0][k]
      #debug2: print(lons[j,i], lats[j,i], conc[j,i], " pt")
      #conc histogram
      counts[int(conc[j,i]*100.+0.5)] += 1
      # for gridding
      iloc = target_grid.inv_locate(lats[j,i],lons[j,i])
      ti = int(iloc[0]+0.5)
      tj = int(iloc[1]+0.5)
      gcount[tj,ti] += 1
      tsumx[tj,ti]  += conc[j,i]
      tsumx2[tj,ti] += conc[j,i]*conc[j,i]
      #debug2: print(j, i, tj, ti, lons[j,i], lats[j,i], conc[j,i], " pt", flush=True)
  #debug if (n > 50) :
  #debug     break

print("number of files with valid ice conc: ",nvalid)
print("total number of ice conc observations: ",totnp)
print("Histogram: conc  , # points")
for k in range(0,len(counts)):
    print(k,counts[k])
print(flush=True)

z = latpt()
cellcount = 0
mask = ma.masked_array(gcount > 0)
indices = mask.nonzero()
for k in range(0,len(indices[0])):
    i = indices[1][k]
    j = indices[0][k]
    tsumx[j,i] /= gcount[j,i]
    tsumx2[j,i] = sqrt(max(0., tsumx2[j,i]/gcount[j,i] - tsumx[j,i]*tsumx[j,i]) )
    target_grid.locate(i,j,z)
    print("grid ",i,j,z.lat, z.lon, tsumx[j,i], tsumx2[j,i], gcount[j,i], flush=True)
    cellcount += 1

print("gcount, avg: ",gcount.max(), gcount.min(), tsumx.max(), tsumx.min(), tsumx2.max(), tsumx2.min()  )
print("cellcount = ",cellcount)

#write out netcdf of file
#open
#  header (grid spec, ..)
#  globals (time, producer, etc.)
#  mean
#  sqrt(var)
#  count
#  quality info
#close
#graphics

exit(0)

# Notes -------------------------------------------------------------

nx = len(viirs.dimensions['Columns'])
ny = len(viirs.dimensions['Rows'])
print("dimensions nx,ny: ",nx, ny)

#Accessing general info for file
print("resolution: ",viirs.resolution,flush=True)
#Accessing the fill value (a per-variable value):
print("fill?",viirs.variables['IceConc']._FillValue)

conc = viirs.variables['IceConc'][:,:]
print("conc ",conc.max(), conc.min() )

