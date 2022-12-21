from math import *
#import numpy as np
import numpy 

#use dictionary with multiple names (g, gee, ...) for constants
#  then users can proceed with their preferences
# One dimensional shallow water gravity waves
NX  = int(int(256/2)*5+1)
dx  = 2.e4 #meters
dt  = 1.0  #seconds
gee = 9.8 
h_0 = 4000. #mean depth

def step(nbase, u, eta, tmpeta, h):
  if (nbase == 2) :
    n = 2;
    np = 0;
    nm = 1;
  elif (nbase == 1) :
    n = 1;
    np = 2;
    nm = 0;
  elif (nbase == 0) :
    n = 0;
    np = 1;
    nm = 2;
  else :
    print("nbase is out of range, = ",nbase);
    return -1;
  nx = len(u)

  nu = 3.e+4*dt/dx/dx
#// Centered in time 
  tmpeta = eta[:,np]
  tmpu   = numpy.zeros((int(nx)))
  eta[:,np] = eta[:,nm]
  c0 = 2.*dt/(2.*dx)
  #Faster to use slices than for loop
  eta[1:nx-1,np] -= c0*(u[2:nx]*h[2:nx] - u[0:nx-2]*h[0:nx-2])
  #next order differencing:
  #eta[2:nx-2,np] -= c0*(u[4:nx]*h[4:nx]*(-1./12.) + (1./6.)*u[3:nx-1]*h[3:nx-1] 
  #       - (1./6.)*u[1:nx-3]*h[1:nx-3] + (1./12.)*u[0:nx-4]*h[0:nx-4] )
  #diffusion to suppress noise
  eta[1:nx-1,np] += nu*(eta[2:nx, n] - 2.*eta[1:nx-1, n] + eta[0:nx-2, n]) 
#  eta[nx-1,np] = 0.0
#  eta[0,np]    = 0.0
# No slope at wall / insulating conditions:
#  eta[nx-1,np] = eta[nx-2,np]
#  eta[0,np]    = eta[1,np]
  eta[0,np]    =  (4./3.)*eta[1,np]    - (1./3.)*eta[2,np]
  eta[nx-1,np] = +(4./3.)*eta[nx-2,np] - (1./3.)*eta[nx-3,np]

  c1 = gee * dt / 2. / dx;
  #Much faster than for loop
  u[1:nx-1] -= c1*(eta[2:nx,n] - eta[0:nx-2,n])
  u[2:nx-2] -= c1*(eta[4:nx,n]*(-1./12.) + (1./6.)*eta[3:nx-1,n] - (1./6.)*eta[1:nx-3,n] + (1./12.)*eta[0:nx-4,n])
  #diffusion:
  tmpu[1:nx-1] = nu*(u[2:nx] - 2.*u[1:nx-1] + u[0:nx-2]) 
  #rayleigh damping: tmp = -r*u
  u[1:nx-1]   += tmpu[1:nx-1]

#  u[0]    = 0.0
#  u[nx-1] = 0.0
  u[0]    = u[1]/2.
  u[nx-1] = u[nx-2]/2.
  

  eta[:,nm] = eta[:,n]
  eta[:,n]  = tmpeta

  return 0
    
#-----------------------------------
u   = numpy.zeros((NX))
h   = numpy.zeros((NX))
tmpeta = numpy.zeros((NX))
eta = numpy.zeros((NX,3))
h[:] = h_0

center = NX/2
sigma = 9.0
for i in range(0,NX):
  eta[i,1] = exp(-(i-center)**2/2./sigma/sigma)
#  h[i]    -= 2000.*eta[i,1]

freq  = 1200
nstep = freq*3*25*20
pleta = numpy.zeros((nstep))
plu   = numpy.zeros((nstep))
ts    = numpy.zeros((nstep)) 
x     = range(0,NX)
plot  = bool(True)

import matplotlib 
import matplotlib.pyplot as plt

for i in range(0, nstep):
  nbase = (i+2)%3
  step(nbase, u, eta, tmpeta, h)
  pleta[i] = eta[:,nbase].max()
  plu[i]   = u.max()
  ts[i]    = float(i*dt/3600.)
  if (i%freq == 0):
    print(i, int(i/freq), eta[:,nbase].max(), u.max(), eta[1:-1,nbase].sum()  )
    if (plot):
      fig,ax = plt.subplots()
      ax.plot(x,eta[:,nbase])
      plt.ylim([-0.5,1.01])
      plt.grid()
      plt.savefig("eta"+str(int(i/freq))+".png")
      plt.close()
      fig,ax = plt.subplots()
      ax.plot(x,u)
      plt.savefig("u"+str(int(i/freq))+".png")
      plt.close()

fig,ax = plt.subplots()
ax.plot(ts, pleta)
#fig.show()
plt.grid()
ax.set(xlabel="hours")
plt.savefig("eta.png")
plt.clf()
fig,ax = plt.subplots()
ax.plot(ts, plu)
plt.grid()
ax.set(xlabel="hours")
plt.savefig("u.png")
#fig.show()
