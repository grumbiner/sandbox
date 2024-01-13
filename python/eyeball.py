import os
import sys
import time
from math import *
import numpy as np
import numpy.ma as ma
from netCDF4 import Dataset


class eyeball_limits:
  def __init__(self, hard_low = -9999., hard_high = 9999.):
    self.hard_low  = hard_low
    self.hard_high = hard_high
    self.lows = [0., 0., 0., 0.]
    self.highs = [0., 0., 0., 0.]
    print("hard bounds ",self.hard_low, self.hard_high)


#hard bounds:
#sst(-2.3,40.0) -- freezing point at depth, higher than any observed
#sss( 0.0, 357.) -- fresh, saturation g/kg
#u, v (-3, +3 )  -- arbitrary (but faster than Gulf Stream) 
#speed (0, 3.5)  -- highest velocities are at 0 or 90 degrees, not 45.

sst_hard_limits     = eyeball_limits(-2.3,  40.0)
sss_hard_limits     = eyeball_limits( 0.0, 357.0)
u_ocean_hard_limits = eyeball_limits(-3.0,   3.0)
v_ocean_hard_limits = eyeball_limits(-3.0,   3.0)
ocean_speed_hard_limits = eyeball_limits(0.0, 3.5)

icec_hard_limits = eyeball_limits(0.0, 1.0)




class eyeball_grid:
  def __init__(self, x, param):
# eyeball grids are masked arrays with ancillary statistical information
    self.name = param
    self.ma   = x
    self.n    = self.ma.count()
    self.getstats()
    #self.show()

  def getstats(self):
    self.min  = self.ma.min()
    self.max  = self.ma.max()
    self.tot  = self.ma.sum()
    self.mean = self.tot / self.n

    self.tot2 = (self.ma*self.ma).sum()
    self.var  = self.tot2/self.n - self.mean*self.mean
    self.sd   = sqrt(self.var)
    self.tot3 = (self.ma*self.ma*self.ma).sum()
    self.tot4 = (self.ma*self.ma*self.ma*self.ma).sum()
    self.var3 = (self.tot3 - 3.*self.mean*self.tot2 + 3.*self.mean**2*self.tot - self.n*self.mean**3)/self.n
    self.skew = self.var3 / (self.var**1.5)
    self.var4 = (self.tot4 - 4.*self.mean*self.tot3 + 6.*self.mean**2*self.tot2 - 4.*self.mean**3*self.tot + self.n*self.mean**4)/self.n
    self.kurtosis = (self.var4 / self.var / self.var) - 3.0 # equals 3 for gaussian
    self.show()

  def show(self):
    print(self.name, "eyeball grid stats ","{:8.5f}".format(self.min), 
          "{:8.5f}".format(self.max), "{:8.5f}".format(self.mean), 
          "{:8.5f}".format(self.var), "{:8.5f}".format(sqrt(self.var)), 
          "skew = ", "{:8.5f}".format(self.skew), 'kurt = ', "{:8.5f}".format(self.kurtosis)  )


# The gaussian functions check for field being vaguely gaussian --
#    1) mean is 'near' the center of the range
#    3) skew is 'near' 0
#    4) kurtosis is 'near' 0 (the 3 of a true gaussian)

  def gaussian(self, multiplier = 0.5):
    toler = self.sd*multiplier
    if (abs((self.max - self.min)/2. - self.mean) < toler):
      #print('field is vaguely symmetric')
      return True
    else:
      #print('field is not even remotely symmetric')
      return False

  def gaussian3(self, toler = 0.5):
    if (abs(self.skew) < toler):
      return True
    else:
      return False

  def gaussian4(self, toler = 3.):
    if (abs(self.kurtosis) < toler):
      return True
    else:
      return False



  def bounded(self, lats, lons, bounds):
    i = int(0)
    j = int(0)
    nout = int(0)
    y = ma.masked_outside(self.ma, bounds.hard_low, bounds.hard_high)
    if (not (self.n == y.count() )):
      nout = self.n - y.count()
      print(self.name, self.n - y.count(), ' points outside bounds of ', 
                 bounds.hard_low, bounds.hard_high, flush=True)
      for j in range (0,y.shape[0]):
        for i in range (0, y.shape[1]):
          if (y.mask[j,i] and not self.ma.mask[j,i]):
            print(i,j,lats[j,i], lons[j,i], self.ma[j,i], self.name, 'out of bounds')
    print(self.name, y.max(), y.min(),flush=True)
    return nout
    
