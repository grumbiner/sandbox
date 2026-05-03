MODULE astronomy
  IMPLICIT none
PUBLIC
  REAL m_earth, m_sun, au, ly, parsec, G, mean_solar_day
  PUBLIC astronomy_init
CONTAINS
  SUBROUTINE astronomy_init 
    m_earth = 5.9722e24 
    m_sun   = 1.98847e30 
    au      = 1.49597870700e11 
    ly      = 9.46e15 
    parsec  = 3.26*ly 
    G       = 6.67e-11
    mean_solar_day = 86400.
  RETURN
  END SUBROUTINE astronomy_init


end MODULE astronomy


!def dist(x,y):
!  dx = y.x - x.x
!  r = sqrt(dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2])
!  return r
!
!#keplerian speed for distance around body y:
!def kepler(x, y):
!  dx = y.x - x.x
!  r = sqrt(dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2])
!  return sqrt(astronomy.G*y.m/r)
!


MODULE point_mass
  IMPLICIT none
  PUBLIC
  TYPE body
    REAL x(3), u(3), a(3)
    REAL m, k
  END TYPE body

  PUBLIC init, init_loc, init_vel

  CONTAINS
    SUBROUTINE init
      x = 0
      u = 0
      a = 0
      m = 0
      k = 0
    RETURN
    END
    SUBROUTINE init_loc(loc1, loc2, loc3)
      REAL loc1, loc2, loc3
      x(1) = loc1
      x(2) = loc2
      x(3) = loc3
    RETURN
    END
    SUBROUTINE init_vel(vel1, vel2, vel3)
      REAL vel1, vel2, vel3
      u(1) = vel1
      u(2) = vel2
      u(3) = vel3
    RETURN
    END
    SUBROUTINE update_loc(dt)
      REAL dt
    RETURN
    END
    SUBROUTINE update_vel(dt)
      REAL dt
    RETURN
    END

!  def update_loc(self, dt = 1.):
!    self.x  += dt*self.u
!
!  def update_vel(self, dt = 1.):
!    self.u += self.a*dt
!    self.a  = 0 
!
!  def ke(self):
!    return 0.5*(self.u[0]*self.u[0] + self.u[1]*self.u[1] + self.u[2]*self.u[2])
!
!  def show(self, l = 1., fout = sys.stdout):
!    print(self.x[0]/l, self.x[1]/l, self.x[2]/l, self.u, self.m, file = fout)
!     
!# for astronomy:
!  def gravity(self, y):
!    dx = y.x - self.x
!    r = sqrt(dx[0]*dx[0] + dx[1]*dx[1] + dx[2]*dx[2])
!    a0 = y.k / r/r/r
!    self.a += a0*dx
!
END MODULE point_mass

PROGRAM solar_system
  USE astronomy
  USE point_mass

  IMPLICIT none
  INTEGER i, isun, iearth, nbody
  REAL ke0, ratio, dt
  INTEGER freq

  CALL astronomy_init
  i = 0
  isun = 0
  nbody = 30
  iearth = 10
  freq = 1
  ratio = 100.
  dt = 0.1*mean_solar_day/ratio

!x = body()
!system = []
!system.append(x)
!system[i].m = astronomy.m_sun
!system[i].k = system[i].m*astronomy.G
!system[isun].show()
!print("len of system ",len(system))
!
!for i in range(1,nbody):
!  system.append(body() )
!  system[i].m = astronomy.m_earth*0.1
!  system[i].k = astronomy.G*system[i].m
!  print(i,0.1*i*astronomy.au, flush=True)
!  system[i].init_loc(0.1*i*astronomy.au, 0, 0)
!  system[i].u[1]  = kepler(system[i], system[isun])
!  
!ke0 = system[iearth].ke()
!
!for i in range (0, 7300*int(ratio)*2):
!
!  #gravitation on bodies:
!  for j in range(1, nbody):
!    system[j].gravity(system[0])
!    for k in range(1, nbody):
!      if (k != j):
!        system[j].gravity(system[k])
!
!  #update velocity, position
!  for j in range(1, nbody):  
!    system[j].update_vel(dt)
!    system[j].update_loc(dt)
!
!  if (i%freq == 0):
!    print(i*dt/astronomy.mean_solar_day, 
!      system[iearth].x[0]/astronomy.au, system[iearth].x[1]/astronomy.au, 
!      ke0 - system[iearth].ke(), dist(system[iearth], system[0])/astronomy.au -1. )
!
!system[iearth].show(l = astronomy.au)
!print("earth-sun x:",(system[iearth].x-system[0].x)/astronomy.au, dist(system[iearth], system[0]) )
!print("earth-system[0] u:",(system[iearth].u - system[0].u) )
!print("earth ke",ke0, (ke0-earth.ke())/ke0 )

END
