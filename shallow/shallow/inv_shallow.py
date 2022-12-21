from math import *
import numpy 

#use dictionary with multiple names (g, gee, ...) for constants
#  then users can proceed with their preferences
gee = 9.8 
h_0 = 4000. #mean depth

# One dimensional shallow water gravity waves
NY  = int(64)
dy  = 1.e-3
dx  = 2.e4 #meters
L   = 10*dx

dt  = 1.0  #seconds

def step(nbase, eta, y, tmpeta):
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
  nx = len(eta[:,np]) 

  c0 = gee*h_0/L/L*dt*dt 

  #Faster to use slices than for loop
  tmpeta[1:nx-1]  = y[1:nx-1]*y[1:nx-1]/dy/dy*(eta[2:nx,n]-2.*eta[1:nx-1,n]+eta[0:nx-2,n]) 
  tmpeta[1:nx-1] += y[1:nx-1]*(eta[2:nx, n] - eta[0:nx-2, n])/2./dy
  eta[:,np] = c0*y*y*tmpeta
  eta[:,np] += -eta[:,nm] + 2.*eta[:, n]

  eta[0,np]    = 0.
  eta[nx-1,np] = 0.


  return 0
    
#-----------------------------------
tmpeta = numpy.zeros((NY))
eta    = numpy.zeros((NY,3))
x      = range(0,NY)
y      = numpy.zeros((NY))
lx     = numpy.zeros((NY))
xgeom  = numpy.zeros((NY))
for i in x:
  y[i] = (i+1)*dy
  lx[i] = L / ((i+1) * dx)
  xgeom[i] = i*dx
  print(i,lx[i], y[i])
#print(y[1], y[256])

#exit(0)
eta[3,:] = 1.0

freq  = 600
nstep = 100*6000+1
pleta = numpy.zeros((nstep))
ts    = numpy.zeros((nstep)) 
plot  = bool(True)

import matplotlib 
import matplotlib.pyplot as plt

for i in range(0, nstep):
  nbase = (i+2)%3
  step(nbase, eta, y, tmpeta)
  pleta[i] = eta[:,nbase].max()
  ts[i]    = float(i*dt/3600.)
  if (i%freq == 0):
    print(i, int(i/freq), eta[:,nbase].max(), eta[1:-1,nbase].sum()  )
    if (plot):
      fig,ax = plt.subplots()
      ax.plot(y,eta[:,nbase])
      plt.ylim([-0.5,2.01])
      plt.grid()
      plt.savefig("eta"+str(int(i/freq))+".png")
      plt.close()

fig,ax = plt.subplots()
ax.plot(ts, pleta)
#fig.show()
plt.grid()
ax.set(xlabel="hours")
plt.savefig("eta.png")
plt.clf()
