import sys
import numpy as np
import numpy.ma as ma

"""
Start on untilities towards mask management
"""

#----------------------------------------------------------
def mask_stats(nobs, landmask, icemask, watermask, known, fout = sys.stdout):
  # Get the indices of the 'true' points
  iceindices = icemask.nonzero()
  mland      = landmask.nonzero()
  water      = watermask.nonzero()
  kindex     = known.nonzero()

  nicepts    = len(iceindices[0])
  nlandpts   = len(mland[0])
  nwaterpts  = len(water[0])
  knownpts   = len(kindex[0])
  #debug print("n ice, land, water, known, nobs ",nicepts, nlandpts, nwaterpts, knownpts, nobs, flush=True)

  nstat  = (nicepts + nlandpts + nwaterpts)
  #debug print(knownpts," known points, tot=", nstat + knownpts, flush=True)
  pice   = nicepts/float(nstat)
  pland  = nlandpts/float(nstat)
  pwater = nwaterpts/float(nstat)
  #debug print("p ice, land, water, nobs ",pice, pland, pwater, nobs, flush=True)
  del iceindices, mland, water, kindex
  return (nicepts, nlandpts, nwaterpts, pice, pland, pwater)


def makemasks(allmatch, known=0):
# Constructing the masks
# RG: better to have a set of masks, rather than specified names in specified orders
  npts = len(allmatch)
  #debug: print("in makemasks, npts = ",npts)
  icec  = np.zeros((npts))
  land  = np.zeros((npts))
  for i in range(0,npts):
    icec[i] = allmatch[i].icec
    land[i] = allmatch[i].land

  icemask   = ma.masked_array(icec > 0)
  landmask  = ma.masked_array(land == 1.0)
  watermask = ma.masked_array(land == 0.0)
  if (isinstance(known, int)):
    known = ma.masked_array(land > 0)
    known = ma.logical_and(known, land < 1)
  #debug: else:
  #debug:   print("working with an input 'known' mask", flush=True)

  unknown   = ma.logical_not(known)
  icemask   = ma.logical_and(icemask, unknown)
  landmask  = ma.logical_and(landmask, unknown)
  watermask = ma.logical_and(watermask, unknown)
  del unknown

  #Distinguish between water and ice-covered water
  not_ice   = np.logical_not(icemask)
  watermask = ma.logical_and(watermask, not_ice)
  del not_ice
  
  #debug print("icemask pt 5 ",icemask[5])
  return(icemask, landmask, watermask, known)

