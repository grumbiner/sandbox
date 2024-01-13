import numpy as np
import numpy.ma as ma

import match
from filtering import *

#----------------------------------------------------------
# Read in data, customized for each different sort of scan/match
fin = open(sys.argv[1], "r")
#lrmatch = match.raw_read(fin)
lrmatch = match.matched_read(fin)
fin.close()
npts = len(lrmatch)
print("npts = ",npts, flush=True)
if (npts < 10):
    print("not enough observations to work with")
    exit(1)

#---------------------------------------------------------------------
# In conceptual analogue to simulated annealing, allow for incremental
#   changes to the quality
def anneal(best_tag, all_tag, out_name, quality, lrmatch, known, posteriori=False):

  (icemask, landmask, watermask, known) = makemasks(lrmatch, known)
  stats = mask_stats(npts, landmask, icemask, watermask, known)
  base_rate = stats[3]
  #debug: 
  print(stats,flush=True)

  tbfilters = []
  tbfilters = filter_scan(lrmatch, icemask, landmask, watermask, known, stats)
  #debug: 
  print("len of tbfilters: ",len(tbfilters), flush=True)

  fout = open(best_tag,"w")
  foute = open(all_tag,"w")
  
  perfect = 0
  perfect_filters = []
  for i in range(0, len(tbfilters)):
      tbfilters[i].show(foute)
      # high quality 'not ice': if (tbfilters[i].stats[0] <= base_rate/quality ):
      # high quality 'not ice' and 'is water':
      if (tbfilters[i].stats[0] <= base_rate/quality and tbfilters[i].stats[2] > 0.98):

        perfect_filters.append(tbfilters[i])
        tbfilters[i].show(fout)
        perfect += 1
  fout.close()
  foute.close()
  #debug print(perfect," filters for ice",flush=True)

  best = perfect_filters[0]
  for i in range (1, perfect):
    if (perfect_filters[i].npts > best.npts):
      best = perfect_filters[i]
  print("best: ",end="",flush=True)
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
  base_rate = stats[3]
  print("stats: ",stats[0],stats[1],stats[2], "{:.5f}".format(stats[3]), "{:.5f}".format(stats[4]), "{:.5f}".format(stats[5]), flush=True)
  print("\n",flush=True)

  if (posteriori):
    fout = open(out_name,"w")
    for i in range(0, len(lrmatch)):
        if (not known[i]):
            lrmatch[i].show(fout)
    fout.close()

  return tcount
#---------------------------------------------------------------------
def endoutput(out_name, lrmatch):
    fout = open(out_name,"w")
    for i in range(0, len(lrmatch)):
        if (not known[i]):
            lrmatch[i].show(fout)
    fout.close()


#---------------------------------------------------------------------
# Main:
(icemask, landmask, watermask, known) = makemasks(lrmatch)
stats = mask_stats(npts, landmask, icemask, watermask, known)
print("initial stats: ",stats[0],stats[1],stats[2], "{:.5f}".format(stats[3]), "{:.5f}".format(stats[4]), "{:.5f}".format(stats[5]), flush=True)

quality =  50.
fraction = 40.
print("quality = ",quality, " fraction = ",fraction, flush=True)

iter = 0
ptsknown = 0
while (quality > 3.):
  for k in range(0,25):
  
    best_tag = "perfect"+"{:d}".format(iter) 
    all_tag  = "all"+"{:d}".format(iter) 
    out_name = "post"+"{:d}".format(iter) 
    iter += 1
    
    tcount = anneal(best_tag, all_tag, out_name, quality, lrmatch, known)
    ptsknown += tcount
  
    # quit iteration if we get to the point of 'not many' points being affected
    # RG: better to use fraction of unknown pts
    if (tcount < (npts-ptsknown) / fraction):
      break
  quality /= 2.
  print("quality = ",quality, " fraction = ",fraction)


endoutput(out_name, lrmatch)

(icemask, landmask, watermask, known) = makemasks(lrmatch, known)
stats = mask_stats(npts, landmask, icemask, watermask, known)
print("final stats: ",stats[0],stats[1],stats[2], "{:.5f}".format(stats[3]), "{:.5f}".format(stats[4]), "{:.5f}".format(stats[5]), flush=True)

