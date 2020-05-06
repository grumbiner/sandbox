# after installing anaconda.  (python math.so needed)
import math
#print math
print 'Hello world!'
rpdg = 3.141592654 / 180.;
print math.sin(30*rpdg), math.sin(45*rpdg), math.sin(60*rpdg)
# 
#alternate import, which obviates using math.
from math import *
rpdg = pi / 180.
print sin(30*rpdg), sin(45*rpdg), sin(60*rpdg)
print sin(30*rpdg)*2, sin(45*rpdg)*sqrt(2), sin(60*rpdg)*sqrt(3)


def printsin(x) :
  print sin(x)

printsin(pi)
