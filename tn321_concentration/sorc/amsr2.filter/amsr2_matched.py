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
lrmatch = []
count = 0
tmp = match.match(sat = sat_lr)
x = match.match(sat = sat_lr)
for line in fin:
  tmp.lr_read(line)
  #tmp.show()
  lrmatch.append( x )
  lrmatch[len(lrmatch)-1] = copy.deepcopy(tmp)
  #lrmatch[len(lrmatch)-1].show()
  #lrmatch[0].show()
  count += 1
fin.close()

npts = len(lrmatch)
print("npts = ",npts, "count = ",count, flush=True)
#debug: lrmatch[0].show()
#debug: lrmatch[-1].show()
#debug: exit(0)


#---------------------------------------------------------------------
def filter_scan(allmatch, icemask, landmask, watermask, known, stats):

  tbfilters = []
  npts = len(allmatch)
  c1obs = np.zeros((npts))
  c2obs = np.zeros((npts))
  
  for c1 in range(0,match.amsr2_lr.ntb):
    c1_save = c1
    tag = "ch"+"{:d}".format(c1)
    for i in range(0,npts):
      c1obs[i] = allmatch[i].obs.tb[c1]
    #debug: print("channel",c1,"stats","{:6.2f}".format(c1obs.max()), 
    #debug:       "{:6.2f}".format(c1obs.min()), "{:6.2f}".format(c1obs.mean()), flush=True )
    
    # Scan a temperature range
    for tc in range(50, 285, 1):
      thot = float(tc)
      tmp  = bayes(c1obs, thot, tag, npts, stats, landmask, icemask, 
                   watermask, known, c1)
      filter.add(tbfilters, tmp)
  
    # Check delta ratio (dr) w.r.t this channel:
    #debug: print("dr2 ",c1_save, match.amsr2_lr.ntb, flush=True)
    for c2 in range (c1_save+1, match.amsr2_lr.ntb):
      #debug: print("c2 = ",c2,flush=True)
      if (c1_save != c2):
        #debug: print("c1_save, c2 ",c1_save, c2, flush=True)
        for i in range(0,npts):
          c2obs[i] = allmatch[i].obs.tb[c2]
  
        #tag2 = "dr"+"{:d}".format(c1_save)+"{:d}".format(c2)
        #tmp = dr(c1obs, c2obs, c1_save, c2, tag2, npts, stats, 
        tag2 = "dr2"+"{:d}".format(c1_save)+"{:d}".format(c2)
        tmp = dr2(c1obs, c2obs, c1_save, c2, tag2, npts, stats, 
                 landmask, icemask, watermask, known, 
                 granularity = int(100/1) )
        filter.add(tbfilters, tmp)

  return tbfilters
#--- end of scan -----------------------------------------------------------
def appfilter(allmatch, known, best):
  tcount = 0
  fcount = 0
  for i in range (0, len(allmatch)):
    if ( (not known[i]) and best.apply(allmatch[i]) ):
      tcount += 1
      known[i] = True
    else:
      fcount += 1
  #debug: print("applied ",tcount,"times", flush = True)
  return tcount

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

