import sys
import numpy as np
import numpy.ma as ma

#----------------------------------------------
# satellite to ice/ocean matchup class
from match import *

#generalizing 2021 filtering process
#----------------------------------------------
# llgrid tools

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
  dlon =  1./12.
  firstlat = 90. - dlat/2.
  firstlon = dlon/2.
  if (lon < 0):
    lon += 360.
  j = round( (lat - firstlat)/dlat )
  i = round( (lon - firstlon)/dlon )
  return (j,i)

#----------------------------------------------
# satellite 
# delta ratio
def delta(x,y):
  return (x-y)/(x+y)

#---------------------------------------------------------------
# Define utilities for doing the assessment:
from masks import *

def rerun_bayes(allmatch, stats, landmask, icemask, watermask, known, filt, fout = sys.stdout ):
  nicepts   = stats[0]
  nlandpts  = stats[1]
  nwaterpts = stats[2]
  pice      = stats[3]
  pland     = stats[4]
  pwater    = stats[5]
  bayes_stats = np.zeros((3))

  filters = []
  unknown = ma.logical_not(known)
  warm = unknown
  # n.b.: 'warm', here, doesn't refer to type of test, just that it's a test
  for i in range(0, len(allmatch)):
    if (unknown[i]):
        if (not filt.apply(allmatch[i])):
          warm = False
  nwarm = len(warm.nonzero()[0])

  lmask = np.logical_and(landmask, warm)
  imask = np.logical_and(icemask, warm)
  omask = np.logical_and(watermask, warm)

  pwarm = float(nwarm)/float(nicepts+nlandpts+nwaterpts)
  pover_land  = 0.
  pover_water = 0.
  pover_ice   = 0.
  if (nlandpts > 0):
      pover_land  = len(lmask.nonzero()[0])/nlandpts
  if (nwaterpts > 0):
      pover_water = len(omask.nonzero()[0])/nwaterpts
  if (nicepts > 0):
      pover_ice   = len(imask.nonzero()[0])/nicepts
  if (pwarm > 0):     #ignore if the filter is never tripped
    bayes_stats[0] = pover_ice * pice / pwarm
    bayes_stats[1] = pover_land * pland / pwarm
    bayes_stats[2] = pover_water * pwater / pwarm

  if (((bayes_stats[0] + bayes_stats[1] + bayes_stats[2])-1.) > 1.e-3):
    print("bayes", bayes_stats, (bayes_stats[0] + bayes_stats[1] + bayes_stats[2]) )
    print("pover ",pover_ice, pover_land, pover_water, pwarm)
    print("p     ",pice, pland, pwater, 1.-(pice+pland+pwater) )
    print("n     ",nicepts, nlandpts, nwaterpts, (nicepts + nlandpts + nwaterpts), nobs )
    exit(1)

  filt.npts = nwarm
  filt.stats = bayes_stats


def bayes(xvec, xcrit, label, nobs, stats, landmask, icemask, watermask, 
          known, channel, fout = sys.stdout ):
  #stats = mask_stats(nobs, landmask, icemask, watermask, known, fout = sys.stdout )
  #debug print("stats = ",stats)
  nicepts   = stats[0]
  nlandpts  = stats[1]
  nwaterpts = stats[2]
  pice      = stats[3]
  pland     = stats[4]
  pwater    = stats[5]
  bayes_stats = np.zeros((3))

  filters = []
  unknown = ma.logical_not(known)

  warm = ma.masked_array(xvec > xcrit)
  warm = ma.logical_and(warm, unknown)
  nwarm = len(warm.nonzero()[0])

  lmask = np.logical_and(landmask, warm)
  imask = np.logical_and(icemask, warm)
  omask = np.logical_and(watermask, warm)

  pwarm = float(nwarm)/float(nicepts+nlandpts+nwaterpts)
  pover_land  = 0.
  pover_water = 0.
  pover_ice   = 0.
  if (nlandpts > 0):
      pover_land  = len(lmask.nonzero()[0])/nlandpts
  if (nwaterpts > 0):
      pover_water = len(omask.nonzero()[0])/nwaterpts
  if (nicepts > 0):
      pover_ice   = len(imask.nonzero()[0])/nicepts
  if (pwarm > 0):     #ignore if the filter is never tripped
    bayes_stats[0] = pover_ice * pice / pwarm
    bayes_stats[1] = pover_land * pland / pwarm
    bayes_stats[2] = pover_water * pwater / pwarm

    #place to collect the filters
    x = filter(label, "hot", xcrit, bayes_stats, chan = channel, npts = nwarm)
    filters.append(x)
  if (((bayes_stats[0] + bayes_stats[1] + bayes_stats[2])-1.) > 1.e-3):
    print("bayes", bayes_stats, (bayes_stats[0] + bayes_stats[1] + bayes_stats[2]) )
    print("pover ",pover_ice, pover_land, pover_water, pwarm)
    print("p     ",pice, pland, pwater, 1.-(pice+pland+pwater) )
    print("n     ",nicepts, nlandpts, nwaterpts, (nicepts + nlandpts + nwaterpts), nobs )
    exit(1)

  #debug return filters

  cold = ma.masked_array(xvec < xcrit)
  cold = ma.logical_and(cold, unknown)
  ncold = len(cold.nonzero()[0])
  pcold = float(ncold)/float(nicepts+nlandpts+nwaterpts)

  lmask = np.logical_and(landmask, cold)
  imask = np.logical_and(icemask, cold)
  omask = np.logical_and(watermask, cold)

  pover_land  = 0.
  pover_water = 0.
  pover_ice   = 0.
  if (nlandpts > 0):
      pover_land  = len(lmask.nonzero()[0])/nlandpts
  if (nwaterpts > 0):
      pover_water = len(omask.nonzero()[0])/nwaterpts
  if (nicepts > 0):
      pover_ice   = len(imask.nonzero()[0])/nicepts
  
  if (pcold > 0):
    bayes_stats[0] = pover_ice * pice / pcold
    bayes_stats[1] = pover_land * pland / pcold
    bayes_stats[2] = pover_water * pwater / pcold

    #collect the filters
    x = filter(label, "cold", xcrit, bayes_stats, chan = channel, npts = ncold)
    filters.append(x)

  del unknown, bayes_stats
  return filters


def dr(x, y, c1, c2, label, nobs, stats, landmask, icemask, watermask, known, granularity = 25, fout = sys.stdout):
  filters = []

  ratio = delta(x,y)
  tc = np.linspace(ratio.min(), ratio.max(), num=granularity)
  for i in range(0,len(tc)):
    tmp = bayes(ratio, tc[i], label, nobs, stats, landmask, icemask, watermask, known, [c1,c2], fout) 
    if (len(tmp) > 0):
      for i in range(0,len(tmp)):
        filters.append(tmp[i])
      del tmp
  del ratio

  return filters

#----------------------------------------------------------------

class filter:
# order in the bayes_stats
  ice = 0
  land = 1
  water = 2
# dictionary of names to indices:

  def __init__(self, name, type, value, bayes_stats , npts = 0, chan = 0):
    self.name = name
    self.type = type # 'hot' or 'cold', > or <
    self.chan = chan
    self.value = value
    self.npts  = npts
    self.stats = np.zeros((3))
    self.stats[0] = bayes_stats[0]
    self.stats[1] = bayes_stats[1]
    self.stats[2] = bayes_stats[2]

  def show(self, fout = sys.stdout):
    print(self.name, self.type, self.chan, self.value, 
       ('{:7.5f} '*3).format(self.stats[0], self.stats[1], self.stats[2]),
       self.npts, file=fout)

  def perfect(self):
    return (np.any(self.stats == 1.0) or np.any(self.stats == 0.0)) 

  def add(filters, tmp):
    #debug print("adding ",len(tmp)," filters", flush=True)
    for i in range (0,len(tmp)):
      filters.append(tmp[i])

  def better(self, other):
# return true if self is a better filter than the other for filter type 'index'
# perfect_is --> 1.0
# perfect_isnot --> 0.0
# for each ice, land, water
# better : self.npts > other.npts
    if (self.perfect() and other.perfect() ):
      return(self.npts > other.npts)
    else:
      return False

  def apply(self, x):
  #x being a matchup or satellite
    #debug: print("self chan = ",self.chan, isinstance(self.chan,int), isinstance(self.chan, list) )

    if (isinstance(self.chan,int)):    
        #debug: print("chan is an int")
      if (isinstance(x, match)):
        #debug: print("match class",x.obs[self.chan], flush=True)
        if (self.type == "hot"):
          return (x.obs[self.chan] > self.value)
        elif (self.type == "cold"):
          return (x.obs[self.chan] < self.value)
        else:
          print("self.type=",self.type,"--")
        
      elif (isinstance(x, satobs)):
        #debug: print("satobs class", flush=True)
        if (self.type == "hot"):
          return (x.tb[self.chan] > self.value)
        elif (self.type == "cold"):
          return (x.tb[self.chan] < self.value)
        else:
          print("self.type=",self.type,"--")

    elif (isinstance(self.chan, list)):
      #debug: print("dr ",self.type)
      #debug: print("dr ",self.chan[0], self.chan[1])
      #debug: print("dr ",self.type, self.chan[0], self.chan[1], 
      #debug:       delta(x.obs.tb[self.chan[0]], x.obs.tb[self.chan[1]])  )
      if (self.type == "hot"):
          return( delta(x.obs.tb[self.chan[0]], x.obs.tb[self.chan[1]]) > self.value)
      elif (self.type == "cold"):
          return( delta(x.obs.tb[self.chan[0]], x.obs.tb[self.chan[1]]) < self.value)
      else:
          print("dr self.type=",self.type,"--")


    return False

