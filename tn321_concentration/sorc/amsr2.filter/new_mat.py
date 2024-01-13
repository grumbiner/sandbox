import copy
#note use of 'deepcopy' for the compounded type 'match'

import numpy as np
import numpy.ma as ma

import match
from filtering import *

# noodle satobs class and descendents
sat_lr = match.amsr2_lr()

#----------------------------------------------------------
#  Read in from the standardized 'match' class
fin = open(sys.argv[1], "r")
lrmatch = match.matched_read(fin)
fin.close()

npts = len(lrmatch)
print("npts = ",npts,flush=True)

#----------------------------------------------------------
# Constructing the masks
# RG: better to have a set of masks, rather than specified names in specified orders
(icemask, landmask, watermask, known) = makemasks(lrmatch)
stats = mask_stats(npts, landmask, icemask, watermask, known)
print(stats,flush=True)


for k in range(0,15):
  tbfilters = []
  tbfilters = filter_scan(lrmatch, icemask, landmask, watermask, known, stats)
  print("len of tbfilters: ",len(tbfilters), flush=True)

  fout = open("perfect"+"{:d}".format(k),"w")
  foute = open("all"+"{:d}".format(k),"w")
  
  perfect = 0
  perfect_filters = []
  for i in range(0, len(tbfilters)):
      tbfilters[i].show(foute)
      if (tbfilters[i].stats[0] == 0.):
        perfect_filters.append(tbfilters[i])
        tbfilters[i].show(fout)
        perfect += 1
  fout.close()
  foute.close()
  
  print(perfect," perfect filters", flush=True)
  if (perfect == 0):
    break

  best = perfect_filters[0]
  for i in range (1, perfect):
    if (perfect_filters[i].npts > best.npts):
      best = perfect_filters[i]
  print("best: ",end="")
  best.show()
  
  tcount = 0
  fcount = 0
  for i in range (0, len(lrmatch)):
    if ( (not known[i]) and best.apply(lrmatch[i]) ):
      tcount += 1
      known[i] = True
    else:
      fcount += 1
  #debug: print("applied ",tcount,"times", flush=True)
  
  (icemask, landmask, watermask, known) = makemasks(lrmatch, known)
  stats = mask_stats(npts, landmask, icemask, watermask, known)
  print("round",k,"stats: ",stats)

  # quit iteration if we get to the point of 'not many' points being affected
  if (best.npts < npts / 200.):
    break

fout = open("postperfect","w")
for i in range(0, len(lrmatch)):
    if (not known[i]):
        lrmatch[i].show(fout)
fout.close()

#exit(0)
#---------------------------------------------------------------------
# Now accept imperfect matches

print("starting on imperfect matches",flush=True)

(icemask, landmask, watermask, known) = makemasks(lrmatch, known)
stats = mask_stats(npts, landmask, icemask, watermask, known)
base_rate = stats[3]
print(stats,flush=True)

for k in range(1,4):
  tbfilters = []
  tbfilters = filter_scan(lrmatch, icemask, landmask, watermask, known, stats)
  print("len of tbfilters: ",len(tbfilters), flush=True)

  fout = open("vgood"+"{:d}".format(k),"w")
  foute = open("allvg"+"{:d}".format(k),"w")
  
  perfect = 0
  perfect_filters = []
  for i in range(0, len(tbfilters)):
      tbfilters[i].show(foute)
      if (tbfilters[i].stats[0] <= base_rate/100. ):
        perfect_filters.append(tbfilters[i])
        tbfilters[i].show(fout)
        perfect += 1
  fout.close()
  foute.close()
  print(perfect," very good filters for ice")  

  if (perfect == 0):
      break

  best = perfect_filters[0]
  for i in range (1, perfect):
    if (perfect_filters[i].npts > best.npts):
      best = perfect_filters[i]
  print("best: ",end="")
  best.show()
  
  tcount = 0
  fcount = 0
  for i in range (0, len(lrmatch)):
    if ( (not known[i]) and best.apply(lrmatch[i]) ):
      tcount += 1
      known[i] = True
    else:
      fcount += 1
  print("applied ",tcount,"times")
  
  (icemask, landmask, watermask, known) = makemasks(lrmatch, known)
  stats = mask_stats(npts, landmask, icemask, watermask, known)
  base_rate = stats[3]
  print("round",k,"stats: ",stats)

fout = open("postvg","w")
for i in range(0, len(lrmatch)):
    if (not known[i]):
        lrmatch[i].show(fout)
fout.close()

#---------------------------------------------------------------------
# Now accept less perfect matches

(icemask, landmask, watermask, known) = makemasks(lrmatch, known)
stats = mask_stats(npts, landmask, icemask, watermask, known)
base_rate = stats[3]
print(stats,flush=True)

for k in range(1,4):
  tbfilters = []
  tbfilters = filter_scan(lrmatch, icemask, landmask, watermask, known, stats)
  print("len of tbfilters: ",len(tbfilters), flush=True)

  fout = open("good"+"{:d}".format(k),"w")
  foute = open("allg"+"{:d}".format(k),"w")
  
  perfect = 0
  perfect_filters = []
  for i in range(0, len(tbfilters)):
      tbfilters[i].show(foute)
      if (tbfilters[i].stats[0] <= base_rate/30. ):
        perfect_filters.append(tbfilters[i])
        tbfilters[i].show(fout)
        perfect += 1
  fout.close()
  foute.close()
  print(perfect," pretty good filters for ice")  

  if (perfect == 0):
      break

  best = perfect_filters[0]
  for i in range (1, perfect):
    if (perfect_filters[i].npts > best.npts):
      best = perfect_filters[i]
  print("best: ",end="")
  best.show()
  
  tcount = 0
  fcount = 0
  for i in range (0, len(lrmatch)):
    if ( (not known[i]) and best.apply(lrmatch[i]) ):
      tcount += 1
      known[i] = True
    else:
      fcount += 1
  print("applied ",tcount,"times")
  
  (icemask, landmask, watermask, known) = makemasks(lrmatch, known)
  stats = mask_stats(npts, landmask, icemask, watermask, known)
  base_rate = stats[3]
  print("round",k,"stats: ",stats)

fout = open("postg","w")
for i in range(0, len(lrmatch)):
    if (not known[i]):
        lrmatch[i].show(fout)
fout.close()

#---------------------------------------------------------------------

