## python
import sys

from running import *
#----------------------------------==--------------------------------------
# Given time and distance, convert to reference and then compute 
#   equivalents for all distances

distance = str_to_distance(sys.argv[1])
tau      = str_to_time(sys.argv[2])
x        = runner(distance, tau) # Implicitly computes x.t_ref
print(x.t_ref, x.l_ref)

import numpy
times = numpy.zeros((len(standard_distances)))
dists = numpy.zeros((len(standard_distances)))

t = 0.0
k = 0
for l in standard_distances:
  dists[k] = l
  times[k] = from_reference(l, x.t_ref, x.l_ref)
  show(l, times[k])
  k += 1

