import pstats
import sys
from math import *

import numpy as np

class astronomy:
   m_earth = 5.9722e24 
   m_jupiter = m_earth*317.8
   m_sun   = 1.98847e30 

   rmoon   = 3.84e8
   au      = 1.49597870700e11 
   ly      = 9.46e15 
   parsec  = 3.26*ly 

   G       = 6.6743e-11

   mean_solar_day = 86400.

def dist(x,y):
  dx = y.x - x.x
  r = sqrt(dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2])
  return r

#keplerian speed for distance around body y:
def kepler(x, y):
  dx = y.x - x.x
  r = sqrt(dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2])
  return sqrt(y.k/r)

#-----------------------------------------------------------------
class point_mass: 
  def __init__(self):
     self.x = np.zeros((3), dtype=np.float64)
     self.u = np.zeros((3), dtype=np.float64)
     self.a = np.zeros((3), dtype=np.float64)
     self.dx = np.zeros((3), dtype=np.float64)
     self.m = 0.
     self.k = 0.

  def init_loc(self, x1, x2, x3):
    self.x[0] = x1
    self.x[1] = x2
    self.x[2] = x3

  def init_vel(self, v1, v2, v3):
    self.u[0] = v1
    self.u[1] = v2
    self.u[2] = v3

  def ke(self):
    return 0.5*(self.u[0]*self.u[0] + self.u[1]*self.u[1] + self.u[2]*self.u[2])*self.m

  def show(self, l = 1., fout = sys.stdout):
    print(self.x/l, self.u, self.m, file = fout)
     
  #################################################################################
  # Most of time is spent in update_loc, update_vel, gravity
  # Gravity ~= 2*(_loc + _vel) time
  # explicit listing of elements is about 2x faster than using vector notation
  def update_loc(self, dt = 1.):
    #slower: self.x  += dt*self.u
    self.x[0]  += dt*self.u[0] + 0.5*dt*dt*self.a[0]
    self.x[1]  += dt*self.u[1] + 0.5*dt*dt*self.a[1]
    self.x[2]  += dt*self.u[2] + 0.5*dt*dt*self.a[2]
    #about the same: self.x[0]  += dt*(self.u[0] + 0.5*dt*self.a[0])
    #about the same: self.x[1]  += dt*(self.u[1] + 0.5*dt*self.a[1])
    #about the same: self.x[2]  += dt*(self.u[2] + 0.5*dt*self.a[2])

  def update_vel(self, dt = 1.):
    self.u[0]  += dt*self.a[0]
    self.u[1]  += dt*self.a[1]
    self.u[2]  += dt*self.a[2]

    self.a[0]  = 0.
    self.a[1]  = 0.
    self.a[2]  = 0.

  def gravity(self, y):
    #maybe 7% slower: self.dx = y.x - self.x
    self.dx[0] = y.x[0] - self.x[0]
    self.dx[1] = y.x[1] - self.x[1]
    self.dx[2] = y.x[2] - self.x[2]

    # These two are equivalent in time
    r = sqrt(self.dx[0]*self.dx[0] + self.dx[1]*self.dx[1] + self.dx[2]*self.dx[2])
    a0 = y.k / r / r / r
    #e2 r = pow(self.dx[0]*self.dx[0] + self.dx[1]*self.dx[1] + 
    #           self.dx[2]*self.dx[2], 1.5)
    #e2 a0 = y.k / r

    #adds ~60% to run time: self.a += a0*self.dx
    self.a[0] += a0*self.dx[0]
    self.a[1] += a0*self.dx[1]
    self.a[2] += a0*self.dx[2]
  #################################################################################


################################################################################
