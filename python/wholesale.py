import os
import sys
import datetime
from math import *
import numpy as np

import netCDF4
#---------------------------------------------------
from bounders import *

#---------------------------------------------------
#Gross bound checks on .nc files, developed primarily from the sea ice (CICE5) output
#Robert Grumbine
#30 January 2020
#
#data file = argv[1] (input)
#control dictionary = argv[2] (input)

tbound = []

#---------------------------------------------------

if (not os.path.exists(sys.argv[1]) ):
  print("failure to find ",sys.argv[1])
  exit(1)
else:
  model = netCDF4.Dataset(sys.argv[1], 'r')
  nx = len(model.dimensions['xh'])
  ny = len(model.dimensions['yh'])
  tlons = model.variables["geolon"][:,:]
  tlats = model.variables["geolat"][:,:]
  try:
    tmask = model.variables["tmask"][:,:]
  except :
    tmask = np.zeros((ny, nx))
    tmask = 1.
  try:
    tarea = model.variables["tarea"][:,:]
  except : 
    tarea = np.zeros((ny, nx))
    tarea = 1.

  #bootstrapping -- read in dictionary of names, write back out name/max/min in dictionary format
  #  next round -- estimate minmax and maxmin by 1% end points of histogram
  # want to specify T pts vs. U pts
  fdic = open(sys.argv[2])
  try: 
    flying_dictionary = open(sys.argv[3],"w")
    flyout = True
  except:
    print("cannot write out to bootstrap dictionary file")
    flyout = False

  k = 0
  for line in fdic:
    words = line.split()
    parm = words[0]
    try: 
      temporary_grid = model.variables[parm][0,:,:]
    except:
      print(parm," not in data file")
      continue

    # Bootstrap the bounds if needed -------------------
    if (len(words) >= 3):
      pmin = float(words[1])
      pmax = float(words[2])
    else:
      pmin = temporary_grid.min()
      pmax = temporary_grid.max()
      #do the multiplier to avoid roundoff issues with printout values
      if (pmin < 0):
         pmin *= 1.001
      else:
         pmin *= 0.999
      if (pmax < 0):
         pmax *= 0.999
      else:
         pmax *= 1.001
  
    if (len(words) >= 5):
      pmaxmin = float(words[3])
      pminmax = float(words[4])
    else:
      pmaxmin = pmin + 0.1*(pmax - pmin)
      pminmax = pmax - 0.1*(pmax - pmin)

    tmp = bounds(parm, pmin, pmax, pmaxmin, pminmax)
    tbound.append(tmp)
    if (flyout):
      tbound[k].show(flying_dictionary)
    else:
      tbound[k].show(sys.stdout)

    # End reading or bootstrapping bounds -----------------
    gfail = tbound[k].inbounds(temporary_grid)
    #Show where (and which) test failed:
    if (gfail):
      print("calling where", flush=True)
      tbound[k].where(temporary_grid, tlats, tlons, tmask, tarea)

    k += 1

#print(len(tbound), " parameter bounds found ")
#-------------------------- Finished with bootstrap and/or first pass

#Now carry on for the forecasts
#  
from_date = datetime.datetime(int(2012),int(1),int(1), int(0) )

dt     = datetime.timedelta(seconds=6*3600)
length = datetime.timedelta(days=35)

valid_date = from_date + dt

base="/scratch2/NCEPDEV/climate/Robert.Grumbine/data/gfs.20120101/00"
while ( (valid_date - from_date) <= length):
  fname=base+"/ocn"+valid_date.strftime("%Y%m%d%H")+".01."+from_date.strftime("%Y%m%d%H")+".nc"
  if (not os.path.exists(fname)):
    print("couldn't get ",fname)
    valid_date += dt
    continue

  model = netCDF4.Dataset(fname, 'r')
  print("valid date = ",valid_date.strftime("%Y%m%d%H"))
  sys.stdout.flush()
  for k in range(0,len(tbound)):
    temporary_grid = model.variables[tbound[k].param][0,:,:]
    gfail = tbound[k].inbounds(temporary_grid)
    if (gfail):
      #print("calling where", flush=True)
      tbound[k].where(temporary_grid, tlats, tlons, tmask, tarea)
    
  valid_date += dt
