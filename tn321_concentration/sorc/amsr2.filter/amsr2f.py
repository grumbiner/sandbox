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
for line in fin:
  words = line.split()
  if ("lr" in line):

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
    #tmp.show()
    lrmatch.append(tmp)
fin.close()

npts = len(lrmatch)


#---------------------------------------------------------------------
def filter_scan(allmatch, icemask, landmask, watermask, known, stats):

  tbfilters = []
  c1obs = np.zeros((npts))
  c2obs = np.zeros((npts))
  
  for c1 in range(0,amsr2_lr.ntb):
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
    for c2 in range (c1_save+1, amsr2_lr.ntb):
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
(icemask, landmask, watermask, known) = makemasks(lrmatch)
stats = mask_stats(npts, landmask, icemask, watermask, known)
print(stats,flush=True)

# Apply some simple filters to get started:
bstats = np.zeros((3))
used = []
used.append( filter("ch1" , "hot", 261.0 , bstats, chan = 1) )
used.append( filter("ch0" , "hot", 250.0 , bstats, chan = 0) )
used.append( filter("ch2" , "hot", 252.0 , bstats, chan = 2) )
used.append( filter("ch6" , "hot", 252.0 , bstats, chan = 6) )
used.append( filter("ch4" , "hot", 252.0 , bstats, chan = 4) )
used.append( filter("ch7" , "hot", 264.0 , bstats, chan = 7) )
used.append( filter("ch8" , "hot", 255.0 , bstats, chan = 8) )
used.append( filter("ch10" , "hot", 255.0 , bstats, chan = 10) )
used.append( filter("ch3" , "hot", 269.0 , bstats, chan = 3) )
used.append( filter("ch5" , "hot", 264.0 , bstats, chan = 5) )
used.append( filter("ch9"  , "hot", 266.0 , bstats, chan =  9) )
used.append( filter("ch11" , "hot", 267.0 , bstats, chan = 11) )
for i in range(0, len(used)):
  used[i].show()
  appfilter(lrmatch, known, used[i])

(icemask, landmask, watermask, known) = makemasks(lrmatch, known)
stats = mask_stats(npts, landmask, icemask, watermask, known)
print(stats,flush=True)

for k in range(1,7):
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
    exit(0)

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
  print("round",k,"stats: ",stats)

fout = open("postperfect","w")
for i in range(0, len(lrmatch)):
    if (not known[i]):
        lrmatch[i].show(fout)
fout.close()
#exit(0)
#---------------------------------------------------------------------
# Now accept imperfect matches

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
  print(perfect," pretty good filters for ice")  

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
# Now accept imperfect matches

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

