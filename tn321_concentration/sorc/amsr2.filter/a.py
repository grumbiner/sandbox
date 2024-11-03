import numpy as np
import numpy.ma as ma

from filtering import *

tb_lr = np.zeros((amsr2_lr.ntb))
tb_hr = np.zeros((amsr2_hr.ntb))
sat_lr = amsr2_lr()

#----------------------------------------------------------
# Restart with result of last write:
fin = open(sys.argv[1], "r")
allmatch = []
x = amsr2_lr()
for line in fin:
  x.read(line)

  words = line.split()
  icec      = float(words[15])
  land_frac = float(words[16]) 

  tmp = match(x, land = land_frac, icec = icec)
  allmatch.append(tmp)
  #debug: print(tmp.obs[9],tmp.icec, tmp.land, flush=True)
fin.close()

npts = len(allmatch)
#----------------------------------------------------------
# Constructing the masks
# RG: better to have a set of masks, rather than specified names in specified orders
icec  = np.zeros((npts))
land  = np.zeros((npts))
for i in range(0,npts):
  icec[i] = allmatch[i].icec
  land[i] = allmatch[i].land

print("icec ",icec.max(), icec.min(), "land ",land.max(), land.min() )

icemask   = ma.masked_array(icec > 0)
landmask  = ma.masked_array(land == 1.0)
watermask = ma.masked_array(land == 0.0)
coastmask = ma.masked_array(land > 0)
coastmask = ma.logical_and(coastmask, land < 1)

#Distinguish between water and ice-covered water
not_ice   = np.logical_not(icemask)
watermask = ma.logical_and(watermask, not_ice)
del not_ice

stats = mask_stats(npts, landmask, icemask, watermask)
print(stats,flush=True)
#return(icemask, landmask, watermask, ...)

#---------------------------------------------------------------------
fout = open("perfect3","w")
foute = open("imperfect3","w")

tbfilters = []
chobs = np.zeros((npts))

for channel in range(0,amsr2_lr.ntb):
  tag = "ch"+"{:d}".format(channel)
  for i in range(0,npts):
    #chobs[i] = allmatch[i].obs.tb[channel]
    chobs[i] = allmatch[i].obs[channel]
  
  for tc in range(50, 285):
    thot = float(tc)
    tmp  = bayes(chobs, thot, tag, npts, stats, landmask, icemask, watermask)
    filter.add(tbfilters, tmp)

print("len of tbfilters: ",len(tbfilters), flush=True)
perfect = 0
for i in range(0, len(tbfilters)):
    if (tbfilters[i].perfect()):
      tbfilters[i].show(fout)
      perfect += 1
    else:
      tbfilters[i].show(foute)

fout.close()
print("found ",perfect," perfect filters", flush=True)
#return perfect

#---------------------------------------------------------------------


