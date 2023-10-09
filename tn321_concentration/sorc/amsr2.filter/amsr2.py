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
allmatch = []
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
    allmatch.append(tmp)
fin.close()

npts = len(allmatch)
#debug -- show the data that have been read in:
#debug print("len of allmatch = ",len(allmatch))
#debug for i in range(0,len(allmatch)):
#debug   allmatch[i].show()
#demo, print 4th tb:
#for i in range(0,len(allmatch)):
#  print(allmatch[i][3])

#----------------------------------------------------------
#def makemasks(allmatch, npts):
# Constructing the masks
# RG: better to have a set of masks, rather than specified names in specified orders
icec  = np.zeros((npts))
land  = np.zeros((npts))
for i in range(0,npts):
  icec[i] = allmatch[i].icec
  land[i] = allmatch[i].land

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
#def find_perfect(allmatch)
fout = open("perfect1","w")

tbfilters = []
chobs = np.zeros((npts))

for channel in range(0,amsr2_lr.ntb):
  tag = "ch"+"{:d}".format(channel)
  for i in range(0,npts):
    chobs[i] = allmatch[i].obs.tb[channel]
  
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

fout.close()
print("found ",perfect," perfect filters", flush=True)
#return perfect

#---------------------------------------------------------------------

"""
could be ice, land == 0.
is_not_land:  ch5 < 188.
is_not_land:  ch4 < 110.
is_not_land:  ch9 < 207.
is_not_land:  ch7 < 189.
is_not_land:  ch8 < 136.
is_not_land:  ch6 < 110.
is_not_land:  ch10 < 132.


could be ice, land = unknown
is_not_water: ch0 > 233.
is_not_water: ch4 > 245.
is_not_water: ch1 > 263.
is_not_water: ch3 > 265.
is_not_water: ch5 > 266.

is_not_water: ch11 < 196.

Cannot be ice:
not_water AND not_ice: ch2 > 256.
not_water AND not_ice: ch6 > 258.
not_water AND not_ice: ch8 > 265.
not_water AND not_ice: ch7 > 271.

not_ice:       ch9 > 272.
not_ice:      ch10 > 267.
not_ice:      ch11 > 272.

n.b.: have been ignoring 'coast'

"""

fname = "maybe_ice_match"
fout = open(fname,"w")
fcount = 0
tcount = 0
bstats = np.zeros((3))
f1 = filter("ch2", "hot", 256., bstats, chan = 2) 
f2 = filter("ch6", "hot", 258., bstats, chan = 6) 
f3 = filter("ch8", "hot", 265., bstats, chan = 8) 
f4 = filter("ch7", "hot", 271., bstats, chan = 7) 
f5 = filter("ch9", "hot", 272., bstats, chan = 9) 
f6 = filter("ch10", "hot", 267., bstats, chan = 10) 
f7 = filter("ch11", "hot", 272., bstats, chan = 11) 
for i in range(0, len(allmatch)):
    #debug: print(
    #      f1.apply(allmatch[i],2),
    #      f2.apply(allmatch[i],6),
    #      f3.apply(allmatch[i],8),
    #      f4.apply(allmatch[i],7),
    #      f5.apply(allmatch[i],9),
    #      f6.apply(allmatch[i],10),
    #      f7.apply(allmatch[i],11) 
    #     ) 
    if (
          f1.apply(allmatch[i]) or
          f2.apply(allmatch[i]) or
          f3.apply(allmatch[i]) or
          f4.apply(allmatch[i]) or
          f5.apply(allmatch[i]) or
          f6.apply(allmatch[i]) or
          f7.apply(allmatch[i]) 
       ):
      tcount += 1
    else:
      fcount += 1
      allmatch[i].show(fout)

print("true ",tcount, "false ",fcount)

fout.close()
del allmatch
del icemask   
del landmask  
del watermask 
del coastmask 
del tbfilters

#----------------------------------------------------------
# Restart with result of last write:
fin = open(fname, "r")
allmatch = []
for line in fin:
  x = amsr2_lr()
  x.read(line)

  words = line.split()
  icec      = float(words[15])
  land_frac = float(words[16]) 

  tmp = match(x, land = land_frac, icec = icec)
  del x 
  allmatch.append(tmp)
fin.close()

npts = len(allmatch)
print("found ",npts," matchups")

# Re-masking -------------------------------------------------------------
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
#while (find_perfect(allmatch) != 0):
#  
#def find_perfect(allmatch)
foutp = open("perfect2","w")
foute = open("imperfect2","w")

tbfilters = []
chobs = np.zeros((npts))

for channel in range(0,amsr2_lr.ntb):
  tag = "ch"+"{:d}".format(channel)
  for i in range(0,npts):
    chobs[i] = allmatch[i].obs.tb[channel]
  print("channel ",channel,"stats ",chobs.max(), chobs.min(), chobs.mean(), flush=True )
  
  for tc in range(50, 285):
    thot = float(tc)
    tmp  = bayes(chobs, thot, tag, npts, stats, landmask, icemask, watermask)
    filter.add(tbfilters, tmp)

print("len of tbfilters: ",len(tbfilters), flush=True)
perfect = 0
for i in range(0, len(tbfilters)):
    if (tbfilters[i].perfect()):
      tbfilters[i].show(foutp)
      perfect += 1
    else:
      tbfilters[i].show(foute)

foutp.close()
foute.close()
print("round2 found ",perfect," perfect filters", flush=True)

if (perfect > 0):
  fname = "maybe_ice_match2"
  fout = open(fname,"w")
  fcount = 0
  tcount = 0
  bstats = np.zeros((3))
  f21 = filter("ch4", "hot", 254., bstats, chan = 4) 
  for i in range(0, len(allmatch)):

    if ( f21.apply(allmatch[i]) 
       ):
      tcount += 1
    else:
      fcount += 1
      allmatch[i].show(fout)

print("true ",tcount, "false ",fcount)

fout.close()
del allmatch
del icemask   
del landmask  
del watermask 
del coastmask 
del tbfilters







