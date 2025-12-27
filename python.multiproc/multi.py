from multiprocessing import Process, Queue, Pool, shared_memory
import os
import time
from math import *
import numpy as np

def info(title):
  print(title)
  print('module name:',__name__)
  print('parent proc:',os.getppid() )
  print('process id :',os.getpid() )

def f(name):
  info('function f')
  print('hello ',name)

def gas_density(T,p):
  R = 287.
  #debug: print("density ",p/(R*T), T)
  #debug: info('density')
  rho = p/(R*T)
  return rho

def tbb(Sc):
  sigma = 5.67e-8
  T = sqrt(sqrt(Sc*(1-0.3)/4/sigma))
  #debug: print("tbb ", T)
  return T

def sunage(Sc):
  Sc += .001
  return Sc

def temperature(tvol, T, sname):
  #debug: info('temperature')
  existing = shared_memory.SharedMemory(name = sname)
  c = np.ndarray(tvol.shape, dtype=tvol.dtype, buffer = existing.buf)
  start = time.perf_counter()
  c[:,:,:] = T
  print("T = ",T, c[5,5,5],time.perf_counter() - start )
  #return tvol
  

if __name__ == '__main__':
  info('main line')

  p = 1013.25e2
  T = 288
  Sc = 961.
  rho = 0
  nz = 128
  ny = 1440
  nx = 2880
  a = np.zeros((nz,ny,nx))

  # Set up a shared memory for processing the array:
  shm = shared_memory.SharedMemory(create=True, size=a.nbytes)
  tvol = np.ndarray(a.shape, dtype=a.dtype, buffer=shm.buf)
  tvol[:] = a[:]
  sname = shm.name

  p = Process(target=temperature, args=(tvol,T,sname, ))
  p.start()
  p.join()
  print("tvol ",tvol[5,5,5],T)

  with Pool(processes=4) as pool:
    for i in range(0,int(1e2)+1):
      start = time.perf_counter()
      res1 = pool.apply_async(gas_density, (T,p,))
      res2 = pool.apply_async(tbb, (Sc,))
      res3 = pool.apply_async(sunage, (Sc,))
      res4 = pool.apply_async(temperature, (tvol,T,sname,))
      end1 = time.perf_counter()
      print("end1",end1-start)

      start = time.perf_counter()
      rho = res1.get() 
      T   = res2.get()
      Sc  = res3.get()
      end2 = time.perf_counter()
      print("end2",end2-start)
  
      #if (i%1 == 0):
      print(i, T, rho, Sc, tvol[5,5,5] )

  pool.join()

  shm.close()
  shm.unlink()
