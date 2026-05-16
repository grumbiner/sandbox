import os
import time
#Robert Grumbine
#1 June 2018


from latpt import *

# Make vectors of latpts
model = []
observations = []
x = latpt()

#py3  start = time.process_time()
start = time.time()
print "start = ",start
###################################################
#read in model's latpt locations for edge
count = 0
fin = open('modeledge')
for line in fin:
  words = line.split()
#  print words
  lat = float(words[1])
  lon = float(words[0])
  while (lon < 0):
    lon += 360.
  model.append(x)
  model[count] = latpt(lat, lon)
  #model[count].show()
  count = count + 1
fin.close()

nm = len(model)
print "model points count ", len(model)

## Show what got read in
#for i in range (0,nm):
#  model[i].show()

###################################################
#read in observed latpt locations for edge

count = 0
fin = open('obsedge')
for line in fin:
  words = line.split() 
  lon = float(words[0])
  while (lon < 0):
    lon += 360.
  lat = float(words[1])
  observations.append(x)
  observations[count] = latpt(lat, lon)
  #observations[count].show()
  count += 1
fin.close()

print "observation count ", len(observations)
npts = len(observations)

#for i in range (0,npts,1):
#  observations[i].show()
end = time.time()
print "readin time = ",end - start
start = end
###################################################

###################################################
####   Conduct the scoring ########################
tolerance = 500
sumsq = 0.0
count = 0
for imod in range (0,nm):
  rmin = 9.e6
  tdist = 0.0
  for iobs in range (0,npts):
  #for iobs in range (0,30):
    tdist = observations[iobs].distance(model[imod])
    if (tdist < rmin):
      rmin = tdist
  #print imod, rmin
  if (rmin < tolerance):
    sumsq += rmin*rmin
    count += 1

print "score: rms (km) ", sqrt(sumsq/count), " with ",count," valid matchups"
###################################################
