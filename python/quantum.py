import copy
from math import *
import numpy as np

hbar = 6.626e-34/2./pi
m = 9.1e-31             # M electron

dt = 2.5e-22/3.
dx = 2.5e-12

sigma = 32*dx

N = int(4*1000+1)
x0 = N/2.*dx

psi_r = np.zeros((N))
psi_i = np.zeros((N))
x = np.zeros((N))

#-----------------------------------------------------------
def sumsq(psi_r, psi_i):
  sum = (psi_r*psi_r).sum()
  sum += (psi_i*psi_i).sum()
  return sum

dd = np.zeros((N))
def ddx2(psi):
  # ~100x faster
  dd[1:N-1] = psi[2:N] - 2.*psi[1:N-1] + psi[0:N-2]
  #for i in range(1, N-1):
  #  dd[i] = psi[i+1] - 2.*psi[i] + psi[i-1]
  return dd

import matplotlib
import matplotlib.pyplot as plt
matplotlib.use('Agg') #batch mode

def show(x, psi_r, psi_i, i):
  sq = psi_r*psi_r
  sq += psi_i*psi_i
  sq = np.sqrt(sq)
  fig,ax = plt.subplots()
  plt.plot(x, psi_r, color = "blue", label="r" )
  plt.plot(x, psi_i, color = "green", label="i" )
  plt.plot(x, sq, color = "black", label="sq" )
  ax.grid()
  ax.legend()
  plt.savefig("psi_"+"{:d}".format(int(i))+".png")
  plt.close()



#-----------------------------------------------------------
for i in range(0,N):
  psi_r[i] = exp(-(i*dx - x0)**2 / 2. / sigma**2) 
  x[i] = i
psi_r /= sqrt(2.*pi*sigma**2)
x *= dx

# Potential
V = copy.deepcopy(x)
k = hbar**2 / 2./m/x0
V *= +k/(10.*dx**2)


chi = sumsq(psi_r,psi_i)*dx
psi_r /= sqrt(chi)
chi = sumsq(psi_r,psi_i)*dx
print(chi, psi_i.max(), psi_i.min(), psi_r.max(), psi_r.min() )

#-----------------------------------------------------------
freq = 10000
c1 = (dt*hbar/2./m/dx/dx)
c2 = (dt/hbar)
V *= c2
for i in range(0,int(800*freq*2 + 1) ):
  psi_r_old = copy.deepcopy(psi_r)
  psi_i_old = copy.deepcopy(psi_i)
  #psi_i += c1*ddx2(psi_r_old) + c2*V*psi_r_old
  #psi_r -= c1*ddx2(psi_i_old) + c2*V*psi_i_old
  psi_i += c1*ddx2(psi_r_old) + V*psi_r_old
  psi_r -= c1*ddx2(psi_i_old) + V*psi_i_old
  psi_i[0] = psi_i[1]
  psi_r[0] = psi_r[1]
  psi_i[N-1] = psi_i[N-2]
  psi_r[N-1] = psi_r[N-2]

  if (i % freq == 0):
    chi = sumsq(psi_r,psi_i)*dx
    if (abs(chi-1) > 1):
      print(i/freq, chi, psi_i.max(), psi_i.min(), psi_r.max(), psi_r.min())
      show(x, psi_r, psi_i, i)
      exit(1)
    print(i/freq, chi, psi_i.max(), psi_i.min(), psi_r.max(), psi_r.min())
    show(x, psi_r, psi_i, i/freq)

