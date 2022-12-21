from point_mass import *

################################################################################
# Establish some objects and their initial conditions:

earth   = point_mass()
sun     = point_mass()
jupiter = point_mass()
com     = point_mass()

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
