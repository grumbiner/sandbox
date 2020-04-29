## python
import sys

from running import *
#-----------------------------------------------------------
distance = str_to_distance('5k')
tau      = str_to_time    ('21:22')

#x = runner(distance, tau)
x = runner(str_to_distance('5k'), str_to_time('21:22') )

#----------------------------------==--------------------------------------
# Given a walking pace, and running pace, illustrate times for given other
#   distance (marathon).
# RG: dictionary for name -> distance, not just standard distances.

#marathon:
distance = marathon #meter
distance /= 1000. #km

t     = 20. #min/mile
sw    = t/mi_to_km * 60. #seconds per km
w_kps = 1./sw
dw    = w_kps*60. #meters per minute

k  = 42.195
t  = from_reference(k*1000., x.l_ref, x.t_ref) #running at k km pace 
sr = t / k             #seconds per km
r_kps = 1./sr

tmax = float(distance) /w_kps / 3600.
tmin = float(distance) /r_kps / 3600.
print(round(tmax,2), round(tmin,2), round(0.5*(tmax+tmin),2) )

fraction=250.
ratios = range(1,int(fraction*50),1) #fractional since ranges must be ints

import numpy
sr = numpy.zeros(len(ratios))
rr = numpy.zeros(len(ratios))

k=0
for r in ratios:
  dr    = r_kps*float(r)/fraction*60.
  alpha = distance / (dw+dr)          #number of cycles, allowing continuum
  sr[k] = alpha * (1.+r/fraction)*60.
  rr[k] = r/fraction
  k += 1

#-----------------------------------------------
import matplotlib
import matplotlib.pyplot as plt
fig, ax = plt.subplots()
ax.set(title='Marathon time for continuum run-walk ratios')
plt.ylim([0,1.0])
plt.xlim([0,len(ratios)/fraction])
ax.grid()

#ax.set(xlabel='X run:1 min walk', ylabel='marathon time (hours)')
#ax.plot(rr, sr/3600.)

ax.set(xlabel='X run:1 min walk', ylabel='fraction of way to 100% running')
ax.plot(rr, 1. - (sr-tmin*3600.)/(tmax - tmin)/3600.)
plt.show()
# output a png plot

