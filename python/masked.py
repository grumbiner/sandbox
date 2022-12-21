import os
import sys
import time

import numpy as np
import numpy.ma as ma

import netCDF4
from netCDF4 import Dataset

##############################################
#concentration family
# reference constants:
conc_epsi     = 0.01
conc_pm_min   = 0.15
conc_ims_edge = 0.40

#   conc in [0.01:1] (values under 0.15 are suspect, under 0.01 are extremely so]
def tiny_conc_extent(nx, ny, area, conc, minimum = conc_epsi):
  tiny_area = np.float64
  tiny_area = 0.0
#  start = time.time()
  for i in range (0, nx):
    for j in range (0, ny):
      if ( conc[j,i] != 0. and conc[j,i] < minimum):
        tiny_area += area[j,i]
#  print("sbr run time ",time.time() - start)
  return tiny_area

##############################################
#thickness family
# reference constants:
thick_epsi  = 0.01
thick_pm_min = 0.05
arctic_max_max = 10.0 #maximum acceptable thickness in meters
aa_max_max     =  4.0
arctic_min_max =  2.0 #minimum maximum acceptable thickness (i.e., arctic should have _some_ point thicker than this)
aa_min_max     =  1.5

#  is maximum thickness acceptably small:
def maxthick_ok(nx, ny, thick, val = arctic_max_max):
  return ((thick.max() < val))

def minmaxthick_ok(nx, ny, thick, val = aa_min_max):
  return ((thick.max() > val))

#   thick in [0.01:X] (1 cm ice is _awfully_ thin)
def tiny_thick_extent(nx, ny, area, thick, minimum = thick_epsi):
  tiny_area = np.float64
  tiny_area = 0.0
  for i in range (0, nx):
    for j in range (0, ny):
      if ( thick[j,i] != 0. and thick[j,i] < minimum):
        tiny_area += area[j,i]
  return tiny_area

##############################################
##############################################

######################################################
#cfsv2 grid file
cfsv2 = Dataset('cfsv2_mom4_latlon.nc', 'r', format='NETCDF4')

nxt = len(cfsv2.dimensions["grid_x_T"])
nyt = len(cfsv2.dimensions["grid_y_T"])
longitude = np.zeros((nyt, nxt))
latitude  = np.zeros((nyt, nxt))
area      = np.zeros((nyt, nxt))

longitude = cfsv2.variables["x_T"][:,:]
latitude  = cfsv2.variables["y_T"][:,:]
area      = cfsv2.variables["area_T"][:,:]

######################################################
ice_thick = Dataset('sit.20161231.nc', 'r', format='NETCDF4')
ice_conc  = Dataset('sic.20161231.nc', 'r', format='NETCDF4')
nx = len(ice_thick.dimensions["xt"])
ny = len(ice_thick.dimensions["yt"])

#note reversed index order
thick = np.zeros((ny, nx))
conc  = np.zeros((ny, nx))
thick = ice_thick.variables["SIT"][:,:]
conc  =  ice_conc.variables["SIC"][:,:]

mthick = ma.masked_values(thick, 0.0)
mconc  = ma.masked_values(conc, 0.0)
######################################################

#   thick in [0:5-10], Arctic max in [2-10], AA max in [2-4]
print("max, minmax ",maxthick_ok(nx, ny, thick), minmaxthick_ok(nx, ny, thick))
print("thick max, min ",thick.max(), thick.min() )
print("ma thick max, min ",mthick.max(), mthick.min() )

marea = ma.masked_array(area, mconc.mask)
start = time.time()
print("extent from ma ",marea.sum()/1.e12)
print("area from ma ",(marea*mconc).sum() / 1.e12)
print("vol from ma ",(marea*mconc*mthick).sum() / 1.e12)

#print(np.where(mconc.mask != mthick.mask))
#for j in range (0, ny):
#  for i in range (0, nx):
#    if (mconc.mask[j,i] != mthick.mask[j,i]):
#      print("i,j conc thick ",i,j,mconc.mask[j,i], mthick.mask[j,i], mconc[j,i], mthick[j,i])
#
#print("ma run time ",time.time() - start)
#   conc 0 when thick != 0, or vice versa
conflict_conc_thick = np.float64
conflict_conc_thick = 0.0
start = time.time()
for i in range (0, nx):
  for j in range (0, ny):
    #if ( (thick[j,i] != 0. and conc[j,i] == 0.) or (conc[j,i] != 0. and thick[j,i] == 0.) ):
    if (mconc.mask[j,i] != mthick.mask[j,i]):
      conflict_conc_thick += area[j,i]
      print("conflict ",i,j,(conc[j,i]), thick[j,i], mconc.mask[j,i], mthick.mask[j,i],
round(latitude[j,i],3), round(longitude[j,i],3), round(area[j,i]/1.e6,3) )

print("loop4 run time ",time.time() - start)
print("area of conc v thick conflict Kkm^2 ", round(conflict_conc_thick/1e6, 3) )


extent     = np.float64
tarea      = np.float64
vol        = np.float64
tiny_conc  = np.float64
tiny_thick = np.float64
extent     = 0.0
tarea      = 0.0
vol        = 0.0
tiny_conc  = 0.0
tiny_thick = 0.0

#Global integrals
start = time.time()
for i in range (0, nx):
  for j in range (0, ny):
    if (conc[j,i] != 0. and thick[j,i] != 0.) :
      extent += area[j,i]
      tarea  += area[j,i]*conc[j,i]
      vol    += area[j,i]*conc[j,i]*thick[j,i]      
      if (conc[j,i] < conc_epsi):
        tiny_conc  += area[j,i]
      if (thick[j,i] < thick_epsi):
        tiny_thick += area[j,i]
# huge acceleration for embedding here after the logical test has assured 
#    that conc, thick aren't zero --> masked arrays
print("loop3 run time ",time.time() - start)



#General diagnostics
print("total extent, area, volume = ",round(extent/1e12,3), round(tarea/1e12,3), round(vol/1e12,3) )
print("global effective concentration, thickness ",round(tarea / extent,6), round(vol / tarea,6) )

#error diagnostics
print("area of tiny concentration Mkm^2 ", round(tiny_conc /1e12,3), round(tiny_conc / extent, 3)  )
print("area of tiny thickness     Mkm^2 ", round(tiny_thick/1e12,3), round(tiny_thick / extent, 3)  )
