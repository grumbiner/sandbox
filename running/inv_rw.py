## python
import re
import sys

from running import *
#----------------------------------==--------------------------------------

# Given a walking pace, estimate running pace for different ratios
#   equivalents for all distances
tau = str_to_time(sys.argv[1])

sw = 600. #seconds per km
minrun = 300. #seconds per km -- fastest allowed/plausible km run

import numpy
ratios = range(7,100,1) #tenths since ranges must be ints
sr = numpy.zeros(len(ratios))
rr = numpy.zeros(len(ratios))

k=0
f = open('rw_table','w+')
# R:1 run:walk, the 5s below means hardcoded to 5k races
for r in ratios:
  tw = (1./(1.+float(r)/10.))*tau
  tr = tau - tw
  tmp = 1./((1./tr)*(5.-tw/sw))
  if (tmp > minrun):
    sr[k] = tmp
    rr[k] = float(r)/10.
    f.write(str(rr[k]) + ' ' + str(round(sr[k],2))+'\n')
    k += 1

f.flush()

smin = tau / 5. #run all the way
smax = str(round(min(sr),2))

smedian = 0.5*(float(smax) + float(smin))
print("average race pace: ","{:6.2f}".format(smin), "/km")
print("median: ",str(smedian), "{:6.2f}".format(1.6*smedian) )

import matplotlib
import matplotlib.pyplot as plt
fig, ax = plt.subplots()
ax.set(xlabel='Run:walk run:1 min walk', ylabel='seconds per km')
ax.set(title='Run pace estimates inverted from a 5k race')
ax.plot(rr, sr)
ax.grid()
plt.show()

