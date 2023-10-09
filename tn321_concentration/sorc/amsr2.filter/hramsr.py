import numpy as np
import numpy.ma as ma

from filtering import *

# noodle satobs class and descendents
tb_lr = np.zeros((amsr2_lr.ntb))
tb_hr = np.zeros((amsr2_hr.ntb))
sat_lr = amsr2_lr()

#----------------------------------------------------------
# Read in data, customized for each different sort of scan/match
# Later, read in from the standardized 'match' class
fin = open(sys.argv[1], "r")
tmp = match(sat_lr)
lrmatch = []
hrmatch  = []

for line in fin:
  words = line.split()
  if ("hr" in line):

    icec  = int(words[0])
    land_frac = 1. - float(words[5]) # original has 0 == full land

    satid = int(words[2])
    lat   = float(words[3])
    lon   = float(words[4])
    x = amsr2_hr(satid = satid, latitude = lat, longitude = lon)
    tb_hr[0] = float(words[6  ])
    tb_hr[1] = float(words[6+2])
    x.add_tb(tb_hr)
    
    tmp = match(x, land = land_frac, icec = icec)
    hrmatch.append(tmp)

  elif ("lr" in line):

    icec  = int(words[0])
    land_frac = 1. - float(words[5]) # original has 0 == full land

    satid = int(words[2])
    lat   = float(words[3])
    lon   = float(words[4])
    x = amsr2_lr(satid = satid, latitude = lat, longitude = lon)
    for i in range(0,x.ntb):
      tb_lr[i] = float(words[6+i])
    x.add_tb(tb_lr)
    
    tmp = match(x, land = land_frac, icec = icec)
    lrmatch.append(tmp)
fin.close()

#npts = len(lrmatch)
npts = len(hrmatch)


#---------------------------------------------------------------------
def filter_scan(allmatch, icemask, landmask, watermask, known, stats, ntb):

  tbfilters = []
  c1obs = np.zeros((npts))
  c2obs = np.zeros((npts))

  for c1 in range(0,ntb):
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
    for c2 in range (c1_save+1, ntb):
      if (c1_save != c2):
          #debug: print("c1_save, c2 ",c1_save, c2, flush=True)
        for i in range(0,npts):
          c2obs[i] = allmatch[i].obs.tb[c2]
  
        tag2 = "dr"+"{:d}".format(c1_save)+"{:d}".format(c2)
        tmp = dr(c1obs, c2obs, c1_save, c2, tag2, npts, stats, 
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
  #debug: 
  print("applied ",tcount,"times", flush = True)

#----------------------------------------------------------
# Constructing the masks
# RG: better to have a set of masks, rather than specified names in specified orders
(icemask, landmask, watermask, known) = makemasks(hrmatch)
stats = mask_stats(npts, landmask, icemask, watermask, known)
print(stats,flush=True)

#for k in range(1,8):
for k in range(1,5):
  print("k = ",k,flush=True)
  tbfilters = []
  #tbfilters = filter_scan(lrmatch, icemask, landmask, watermask, known, stats, amsr2_lr.ntb)
  tbfilters = filter_scan(hrmatch, icemask, landmask, watermask, known, stats, amsr2_hr.ntb)
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
    exit(0)

  best = perfect_filters[0]
  for i in range (1, perfect):
    if (perfect_filters[i].npts > best.npts):
      best = perfect_filters[i]
  print("best at step ",k,end="")
  best.show()
  
  tcount = 0
  fcount = 0
  for i in range (0, len(hrmatch)):
    if ( (not known[i]) and best.apply(hrmatch[i]) ):
      tcount += 1
      known[i] = True
    else:
      fcount += 1
  print("applied ",tcount,"times")
  
  (icemask, landmask, watermask, known) = makemasks(hrmatch, known)
  stats = mask_stats(npts, landmask, icemask, watermask, known)
  print("round",k,"stats: ",stats)

exit(0)
#---------------------------------------------------------------------

