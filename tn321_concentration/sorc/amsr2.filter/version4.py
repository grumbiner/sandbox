import os
import sys
import numpy as np
import numpy.ma as ma
import netCDF4
from netCDF4 import Dataset

from filtering import *

#--------------------------------------------------------
all = []
npts = nobs
for k in range(0,npts):
  tmp = match(satid = satid[k], longitude = longitude[k], latitude = latitude[k], 
                quality = quality[k], land = land[k], icec = icec[k])
  all.append(tmp) 
  tb[0] = t19v[k]
  tb[1] = t19h[k]
  tb[2] = t22v[k]
  tb[3] = t37v[k]
  tb[4] = t37h[k]
  tb[5] = t85v[k]
  tb[6] = t85h[k]
  all[k].add_tb(tb)

print("done reading in",flush=True)
# create logical masks:
unknown = ma.masked_array(satid != 248) # unknown points, which is anything not F15
known = ma.logical_not(unknown)
print("unknown, known lens: ",len(unknown.nonzero()[0]), len(known.nonzero()[0])  , flush=True)
nobs = len(unknown.nonzero()[0])

del icec

#----------------------------------------------
# Add in the ice points:

for k in range(0,len(all)):
  all[k].add_icefix(ice_land, ice_post, ice_distance)
print("done adding in ice fixed",flush=True)

del ice_longitude, ice_latitude, ice_distance
del ice_land
#--------------------------------------------------------
# Add in SST from qdoi v2, including its sea ice cover

for k in range(0,len(all)):
  all[k].add_oiv2(sst, ice_sst)

print("done adding in sst ",flush=True)
del sst
del ice_sst
#---------------------------------------------------------------------
# Print matchups to plain text
f1 = open("allmatch","w")
for k in range(0,len(all)):
  all[k].show(f1)
fclose(f1)
exit(0)

#---------------------------------------------------------------------
#construct masks:

ice_land = np.zeros((npts))
icec = np.zeros((npts))
sst  = np.zeros((npts))
post = np.zeros((npts))
for i in range(0,npts):
  ice_land[i] = all[i].ice_land
  icec[i] = all[i].ice_sst
  sst[i]  = all[i].sst
  post[i] = all[i].ice_post

#include coast points as being land (sidelobe issues)
icemask   = ma.masked_array(icec > 0)
landmask  = ma.masked_array(ice_land >= 157 )
watermask = ma.masked_array(ice_land < 100)

postmask  = ma.masked_array(post == post_use[0])
for i in range(1, len(post_use)):
  postmask = ma.logical_or(postmask, post==post_use[i])

icemask   = ma.logical_and(icemask, unknown)
landmask  = ma.logical_and(landmask, unknown)
watermask = ma.logical_and(watermask, unknown)

icemask   = ma.logical_and(icemask, postmask)
landmask  = ma.logical_and(landmask, postmask)
watermask = ma.logical_and(watermask, postmask)

#Distinguish between water and ice-covered water
not_ice   = np.logical_not(icemask)
watermask = ma.logical_and(watermask, not_ice)

# Recheck now with a posteriori filtering:
unknown = ma.logical_or(landmask, watermask)
unknown = ma.logical_or(unknown, icemask)
print("unknown len: ",len(unknown.nonzero()[0]))
nobs = len(unknown.nonzero()[0])

del not_ice
del postmask
#---------------------------------------------------------------------
stats = mask_stats(nobs, landmask, icemask, watermask)

#print("n ice, land, water, nobs ",nicepts, nlandpts, nwaterpts, nobs)
#print("p ice, land, water, nobs ",pice, pland, pwater, nobs, flush=True)
print(stats,flush=True)

#----------------------------------------------------------------
fout = open("round1","w")
bfilters = []
for thot in range (75, 315):
  tmp = bayes(t19v, thot, "t19v", unknown, nobs, landmask, icemask, watermask, fout)
  filter.add(bfilters, tmp)
  tmp = bayes(t19h, thot, "t19h", unknown, nobs, landmask, icemask, watermask, fout)
  filter.add(bfilters, tmp)
  tmp = bayes(t22v, thot, "t22v", unknown, nobs, landmask, icemask, watermask, fout)
  filter.add(bfilters, tmp)
  tmp = bayes(t37v, thot, "t37v", unknown, nobs, landmask, icemask, watermask, fout)
  filter.add(bfilters, tmp)
  tmp = bayes(t37h, thot, "t37h", unknown, nobs, landmask, icemask, watermask, fout)
  filter.add(bfilters, tmp)
  tmp = bayes(t85v, thot, "t85v", unknown, nobs, landmask, icemask, watermask, fout)
  filter.add(bfilters, tmp)
  tmp = bayes(t85h, thot, "t85h", unknown, nobs, landmask, icemask, watermask, fout)
  filter.add(bfilters,tmp)

print(len(bfilters),'simple filters evaluated')


drfilters = []
tmp = dr(t19v, t19h, "drt19vt19h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19v, t22v, "drt19vt22v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19v, t37v, "drt19vt37v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19v, t37h, "drt19vt37h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19v, t85v, "drt19vt85v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19v, t85h, "drt19vt85h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19h, t22v, "drt19ht22v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19h, t37v, "drt19ht37v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19h, t37h, "drt19ht37h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19h, t85v, "drt19ht85v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t19h, t85h, "drt19ht85h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t22v, t37v, "drt22vt37v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t22v, t37h, "drt22vt37h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t22v, t85v, "drt22vt85v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t22v, t85h, "drt22vt85h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t37v, t37h, "drt37vt37h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t37v, t85v, "drt37vt85v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t37v, t85h, "drt37vt85h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t37h, t85v, "drt37ht85v", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t37h, t85h, "drt37ht85h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)
tmp = dr(t85v, t85h, "drt85vt85h", unknown, nobs, landmask, icemask, watermask, fout)
filter.add(drfilters, tmp)

print("drfilters: ",len(drfilters))

count = 0
#print out all filters to fout, and new bests to screen
for i in range (0,len(bfilters)):
  bfilters[i].show(fout)
  if (bfilters[i].perfect()):
    if (count == 0):
      best = bfilters[i]
    if (bfilters[i].better(best)):
      best = bfilters[i]
      print("better bfilter = ",i," ",end="")
      best.show()
    count += 1

for i in range (0,len(drfilters)):
  drfilters[i].show(fout)
  if (drfilters[i].perfect()):
    if (count == 0):
      best = drfilters[i]
    if (drfilters[i].better(best)):
      best = drfilters[i]
      print("better drfilter = ",i," ",end="")
      best.show()
    count += 1

fout.close()

print(count," perfect filters")
print("best filter: ",end="")
best.show()
