import pstats
import sys
from math import *

import numpy as np

class astronomy:
   m_earth = 5.9722e24 
   m_sun   = 1.98847e30 
   au      = 1.49597870700e11 
   ly      = 9.46e15 
   parsec  = 3.26*ly 
   G       = 6.67e-11
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
class body: 
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
    return 0.5*(self.u[0]*self.u[0] + self.u[1]*self.u[1] + self.u[2]*self.u[2])

  def show(self, l = 1., fout = sys.stdout):
    print(self.x/l, self.u, self.m, file = fout)
     
  #################################################################################
  # Most of time is spent in update_loc, update_vel, gravity
  # Gravity ~= 2*(_loc + _vel) time
  # explicit listing of elements is about 2x faster than using vector notation
  def update_loc(self, dt = 1.):
    #self.x  += dt*self.u
    self.x[0]  += dt*self.u[0] + 0.5*dt*dt*self.a[0]
    self.x[1]  += dt*self.u[1] + 0.5*dt*dt*self.a[1]
    self.x[2]  += dt*self.u[2] + 0.5*dt*dt*self.a[2]

  def update_vel(self, dt = 1.):
    self.u[0]  += dt*self.a[0]
    self.u[1]  += dt*self.a[1]
    self.u[2]  += dt*self.a[2]

    self.a[0]  = 0.
    self.a[1]  = 0.
    self.a[2]  = 0.

  def gravity(self, y):
    self.dx = y.x - self.x
    #self.dx[0] = y.x[0] - self.x[0]
    #self.dx[1] = y.x[1] - self.x[1]
    #self.dx[2] = y.x[2] - self.x[2]
    r = sqrt(self.dx[0]*self.dx[0] + self.dx[1]*self.dx[1] + self.dx[2]*self.dx[2])

    a0 = y.k / r / r / r
    #self.a += a0*self.dx
    self.a[0] += a0*self.dx[0]
    self.a[1] += a0*self.dx[1]
    self.a[2] += a0*self.dx[2]
  #################################################################################


################################################################################
# Establish some objects and their initial conditions:

earth   = body()
sun     = body()
jupiter = body()
com     = body()

earth.m   = astronomy.m_earth
sun.m     = astronomy.m_sun
jupiter.m = astronomy.m_sun/1047.
com.m     = earth.m + sun.m + jupiter.m

earth.k   = earth.m * astronomy.G
sun.k     = sun.m * astronomy.G
jupiter.k = jupiter.m * astronomy.G
com.k     = com.m * astronomy.G

earth.init_loc(0., astronomy.au, 0)
jupiter.init_loc(astronomy.au*5.2, 0., 0.)

com.x  = earth.x*earth.m
com.x += jupiter.x*jupiter.m
sun.x  = -com.x/sun.m
com.x += sun.x*sun.m
com.x /= com.m
#debug sun.show()

earth.u[0]   =  kepler(earth, sun)
jupiter.u[1] = -kepler(jupiter, sun) * sqrt(1. - jupiter.m / sun.m )
#jupiter.u[1] = -kepler(jupiter, sun) 

com.u  =   earth.u*earth.m
com.u += jupiter.u*jupiter.m
sun.u  = -com.u / sun.m
com.u += sun.u*sun.m
com.u /= com.m

r0 = np.zeros((4))
r0[0] = dist(com,com)
r0[1] = dist(earth,com)
r0[2] = dist(jupiter,com)
r0[3] = dist(sun, com)  

print("r0 (AU) = ",r0/astronomy.au, flush=True)


ratio = 4*8640.
freq  = 4*8640
dt = astronomy.mean_solar_day/ratio
dpy = (366)
nyears = 1

for i in range (0, dpy*int(ratio)*nyears + 1):
  earth.gravity(sun)
  earth.gravity(jupiter)

  jupiter.gravity(sun)
  jupiter.gravity(earth)

  sun.gravity(earth)
  sun.gravity(jupiter)

  earth.update_loc(dt)
  jupiter.update_loc(dt)
  sun.update_loc(dt)

  earth.update_vel(dt)
  jupiter.update_vel(dt)
  sun.update_vel(dt)

  if (i%freq == 0):
    print(i*dt/astronomy.mean_solar_day/dpy,
      earth.x[0]/astronomy.au, earth.x[1]/astronomy.au,
      1.e6*(dist(earth, com)   - r0[1])/astronomy.au ,
      1.e6*(dist(jupiter, com) - r0[2])/astronomy.au ,
      1.e6*(dist(sun, com)     - r0[3])/astronomy.au, flush=True 
    )

print(i*dt/astronomy.mean_solar_day/dpy,
  earth.x[0]/astronomy.au, earth.x[1]/astronomy.au,
  1.e6*(dist(earth, com)   - r0[1])/astronomy.au ,
  1.e6*(dist(jupiter, com) - r0[2])/astronomy.au ,
  1.e6*(dist(sun, com)     - r0[3])/astronomy.au, flush=True 
)
