import os
import sys

from math import *

def dew_point(t, rh):
  if (t > 0):
    const = dict(b=17.368, c=238.88)
  else:
    const = dict(b=17.966, c=247.15)

  pa = rh / 100. * exp(const['b']*t / (const['c'] + t))
  dp = const['c'] * log(pa) / (const['b'] - log(pa))

  return dp

for i in range (1, 100):
  rh = float(i)
  print(i, dew_point(32, rh ))
 
