import os
import sys
import datetime
from math import *
import numpy as np
import numpy.ma as ma

import netCDF4

#---------------------------------------------------
#Gross bound checks on .nc files, developed primarily from the sea ice (CICE5) output
#Robert Grumbine
#30 January 2020
#
#data file = argv[1] (input)
#control dictionary = argv[2] (input)
#bootstrapped dictionary = argv[3] (optional, may be written to if needed and present)
#---------------------------------------------------

errcount = int(0)

if (not os.path.exists(sys.argv[1]) ):
  print("failure to find ",sys.argv[1])
  exit(1)
else:
  model = netCDF4.Dataset(sys.argv[1], 'r')
  nx = len(model.dimensions['ni'])
  ny = len(model.dimensions['nj'])
  #print("nx, ny = ",nx," ",ny)
  #rg q: is this universal across UFS? -- a: no
  tlons = model.variables["TLON"][:,:]
  tlats = model.variables["TLAT"][:,:]
  #print("max, min lons lats masks ",tlons.max(), tlons.min(), tlats.max(), tlats.min() )
  #LAND = 0, #Ocean = 1
  try:
    tmask = model.variables["tmask"][:,:]
  except :
    tmask = np.zeros((ny, nx))
    tmask = 1.
  tarea = model.variables["tarea"][:,:]
  #print("max min mask area ",tmask.max(), tmask.min(), tarea.max(), tarea.min(), sqrt(tarea.max()), sqrt(tarea.min() )  )

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

  parmno = 0
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

    #print("k = ",k,parm, pmin, pmax, pmaxmin, pminmax)
    #RG: need to do something different in formatting small numbers (fsalt, for ex)
    if (len(words) < 5 and flyout) :
      print("{:10s}".format(parm), 
        "{:.5f}".format(pmin),      
        "{:.5f}".format(pmax),
        "{:.5f}".format(pmaxmin),
        "{:.5f}".format(pminmax),
        file=flying_dictionary)
    if (len(words) < 5 and not flyout) : #if no flying dictionary file, write to stdout
      print("bootstrap ","{:10s}".format(parm), 
        "{:.5f}".format(pmin),      
        "{:.5f}".format(pmax),
        "{:.5f}".format(pmaxmin),
        "{:.5f}".format(pminmax) )
    # End finding or bootstrapping bounds -----------------

    #Global tests:
    gmin = temporary_grid.min()
    gmax = temporary_grid.max()
    gfail = False
    if (gmin < pmin):
      print("{:10s}".format(parm)," excessively low minimum ",gmin," versus ",pmin," allowed")
      gfail = True
    if (gmin > pmaxmin):
      print("{:10s}".format(parm)," excessively high minimum ",gmin," versus ",pmaxmin," allowed")
      gfail = True
    if (gmax > pmax):
      print("{:10s}".format(parm)," excessively high maximum ",gmax," versus ",pmax," allowed")
      gfail = True
    if (gmax < pminmax ):
      print("{:10s}".format(parm)," excessively low maximum ",gmax," versus ",pminmax," allowed")
      gfail = True

    #Pointwise checks -- Show where (and which) test failed:
    #  numpy masked arrays are vastly more efficient than manual iteration over indices
    #  0.5 seconds for masked arrays, 5 minutes for manual
    #where(tmp, tlons, tlats, pmin, pmax)
    if (gfail):
      maskhigh = ma.masked_array(temporary_grid > pmax)
      high = maskhigh.nonzero()
      #debug print("len(high): ", len(high[0]),len(high) )
      errcount += len(high[0])

      masklow  = ma.masked_array(temporary_grid < pmin)
      low  = masklow.nonzero()
      #debug print("len(low): ", len(low[0]),len(low) )
      errcount += len(low[0])

      print("parameter i j longitude latitude model_value test_checked test_value")
      for k in range (0,len(high[0])):
        i = high[1][k]
        j = high[0][k]
        print(parm,i,j,tlons[j,i], tlats[j,i], temporary_grid[j,i], " vs pmax ",pmax)

      for k in range (0,len(low[0])):
        i = low[1][k]
        j = low[0][k]
        print(parm,i,j,tlons[j,i], tlats[j,i], temporary_grid[j,i], " vs pmin ",pmin)

    parmno += 1

#exit codes are bounded, while error counts are not
print("errcount = ",errcount)
if (errcount == 0):
  exit(0)
else:
  exit(1)
