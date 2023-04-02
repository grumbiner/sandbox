import os
import sys
import datetime
from math import *
import numpy as np

import netCDF4
#---------------------------------------------------
import bounders 

#---------------------------------------------------
#Gross bound checks on .nc files, developed primarily from the sea ice (CICE6) output
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
  try:
    fdic = open(sys.argv[2])
  except:
    print("could not find a dictionary file ",sys.argv[2])
    exit(1)

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
    tmp = bounders.bounds(param=parm)
    try: 
      temporary_grid = model.variables[parm][0,:,:]
    except:
      print(parm," not in data file")
      continue

    # Bootstrap the bounds if needed -------------------
    if (len(words) >= 3):
      tmp.pmin = float(words[1])
      tmp.pmax = float(words[2])
    else:
      tmp.findbounds(temporary_grid)
  
    if (len(words) >= 5):
      tmp.pmaxmin = float(words[3])
      tmp.pminmax = float(words[4])
    else:
      tmp.findbounds(temporary_grid)

    tbound.append(tmp)
    if (flyout):
      tbound[k].show(flying_dictionary)
    else:
      tbound[k].show(sys.stdout)

    # End reading or bootstrapping bounds -----------------
    gfail = tbound[k].inbounds(temporary_grid)
    #Show where (and which) test failed:
    if (gfail):
      #debug print("calling where", flush=True)
      tbound[k].where(temporary_grid, tlats, tlons, tmask, tarea)

    k += 1

#-------------------------- Finished with bootstrap and/or first pass
#Now carry on for the forecasts
#  

dt     = datetime.timedelta(seconds=6*3600)
length = datetime.timedelta(days=35)

#for yy in (2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018):
#  for mm in range (1,13):
#    for dd in (1,15):
#for tag in ( 20140715, 20161015, 20161101, 20161115, 20161215, 20180115 ):
for tag in ( 20170115, 20170215, 20170315, 20170415, 20170515, 20170615, 20170715, 20170815, 20170915, 20171015, 20171101, 20171115, 20171215, 20180115, 20180215, 20180315 ):
      yy = int(tag / 10000)
      mm = int((tag - yy*10000) / 100)
      dd = (tag - yy*10000 - mm*100) 
      from_date = datetime.datetime(int(yy),int(mm),int(dd), int(0) )
      #if (from_date <= datetime.datetime(2014, 10, 1, 0) ):
      #  continue
      valid_date = from_date + dt
      tag = from_date.strftime("%Y%m%d")
      fout = open("ocn."+tag,"w")

      base = "modelout/gfs."+tag+"/00"
      while ( (valid_date - from_date) <= length):
        fname=base+"/ocn_2D_"+valid_date.strftime("%Y%m%d%H")+".01."+from_date.strftime("%Y%m%d%H")+".nc"
        if (not os.path.exists(fname)):
          print("couldn't get ",fname, file=fout)
          valid_date += dt
          continue
      
        model = netCDF4.Dataset(fname, 'r')
        print("valid date = ",valid_date.strftime("%Y%m%d%H"), file=fout)
        sys.stdout.flush()
        for k in range(0,len(tbound)):
          temporary_grid = model.variables[tbound[k].param][0,:,:]
          gfail = tbound[k].inbounds(temporary_grid, fout)
          if (gfail):
            tbound[k].where(temporary_grid, tlats, tlons, tmask, tarea, fout)
          
        valid_date += dt
      fout.close()
