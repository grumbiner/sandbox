import os
import sys
import datetime
import numpy as np
import netCDF4
from netCDF4 import Dataset

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
#print("long max min ",longitude.max(), longitude.min() )
#print("lat  max min ",latitude.max(), latitude.min() )
#print("area max min km^2 ",area.max()/1.e6, area.min()/1.e6 )


######################################################
tag=datetime.date(2017, 6, 10)
dt = datetime.timedelta(1)
extent     = np.float64
tarea      = np.float64
vol        = np.float64
tiny_conc  = np.float64
tiny_thick = np.float64
conflict_conc_thick = np.float64
epsi_conc  = 0.01
epsi_thick = 0.01
thick_minmax = 2.0

#note reversed index order
thick = np.zeros((nyt, nxt))
conc  = np.zeros((nyt, nxt))

while (tag <= datetime.date(2017, 12, 31)):
  print(tag.strftime("%Y%m%d"),flush=True)
  ice_thick = Dataset('sit.'+tag.strftime("%Y%m%d")+'.nc', 'r', format='NETCDF4')
  ice_conc  = Dataset('sic.'+tag.strftime("%Y%m%d")+'.nc', 'r', format='NETCDF4')
  nx = len(ice_thick.dimensions["xt"])
  ny = len(ice_thick.dimensions["yt"])
  
  thick = ice_thick.variables["SIT"][:,:]
  conc  =  ice_conc.variables["SIC"][:,:]
  #   thick in [0:5-10], Arctic max in [2-10], AA max in [1-3]
  if (thick.max() < thick_minmax):
    print("Global maximum in thickness is too small on ",tag.strftime("%Y%m%d")," at ",thick.max() )
  
  extent = 0.0
  tarea  = 0.0
  vol    = 0.0
  tiny_conc  = 0.0
  tiny_thick = 0.0
  conflict_conc_thick = 0.0
  
  #   conc in [0.01:1] (values under 0.15 are suspect, under 0.01 are extremely so]
  #   thick in [0.01:X] (1 cm ice is _awfully_ thin)
  #   conc 0 when thick != 0, or vice versa
  for i in range (0, nx):
    for j in range (0, ny):
      if ( conc[j,i] > 0. and conc[j,i] < epsi_conc):
        tiny_conc += area[j,i]
  
      if (thick[j,i] > 0. and thick[j,i] < epsi_thick):
        tiny_thick += area[j,i]
  
      if (conc[j,i] != 0. and thick[j,i] != 0.) :
        #print(i,j,round(conc[j,i],3), round(thick[j,i],3), 
        #round(latitude[j,i],3), round(longitude[j,i],3), round(area[j,i]/1.e6,3) )
        extent += area[j,i]
        tarea  += area[j,i]*conc[j,i]
        vol    += area[j,i]*conc[j,i]*thick[j,i]      
  
      if ( (thick[j,i] > 0. and conc[j,i] == 0.) or (conc[j,i] > 0. and thick[j,i] == 0.) ):
        conflict_conc_thick += area[j,i]
        print("conflict ",i,j,round(conc[j,i],3), round(thick[j,i],3), round(latitude[j,i],3), round(longitude[j,i],3), round(area[j,i]/1.e6,3) )
  
  
  #General diagnostics
  print(tag.strftime("%Y%m%d")," total extent = ",round(extent/1e12,3) )
  print(tag.strftime("%Y%m%d")," total area   = ",round(tarea/1e12,3) )
  print(tag.strftime("%Y%m%d")," total vol    = ",round(vol/1e12,3) )
  print(tag.strftime("%Y%m%d")," global effective concentration ",round(tarea / extent,6) )
  print(tag.strftime("%Y%m%d")," global effective thickness     ",round(vol / tarea,6) )
  
  #error diagnostics
  print(tag.strftime("%Y%m%d")," area of tiny concentration Mkm^2 ", round(tiny_conc /1e12,3), round(tiny_conc / extent, 3)  )
  print(tag.strftime("%Y%m%d")," area of tiny thickness     Mkm^2 ", round(tiny_thick/1e12,3), round(tiny_thick / extent, 3)  )
  print(tag.strftime("%Y%m%d")," area of conc v thick conflict Kkm^2 ", round(conflict_conc_thick/1e6, 3) )

  tag += dt
#endwhile on dates

