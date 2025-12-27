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
  #rho = p/(R*T)
  return  p/(R*T)

def tbb(Sc):
  sigma = 5.67e-8
  T = sqrt(sqrt(Sc*(1-0.3)/4/sigma))
  #debug: print("tbb ", T)
  return T

def sunage(Sc):
  Sc += .001
  return Sc

# don't pass the array itself, just the necessary descriptors to construct c
#    passing array apparently makes system communicate whole thing, io binding routine
def temperature(shape, type, T, sname):
  #debug: info('temperature')
  #debug: 
  start = time.perf_counter()
  existing = shared_memory.SharedMemory(name = sname)
  c = np.ndarray(shape, dtype=type, buffer = existing.buf)
  c[:,:,:] = T
  #debug: 
  print("T = ",T, c[5,5,5],time.perf_counter() - start )
  #return tvol
  return 0 

if __name__ == '__main__':
  info('main line')

  press = 1013.25e2
  T = 288
  Sc = 961.
  rho = 0
  #nz = 128
  #ny = 1440
  #nx = 2880
  nz = 128
  ny = 180*8
  nx = 360*8

  # Set up a shared memory for processing the array:
  a = np.zeros((nz,ny,nx))
  shm = shared_memory.SharedMemory(create=True, size=a.nbytes)
  #tvol = np.zeros((nz,ny,nx))
  tvol = np.ndarray(a.shape, dtype=a.dtype, buffer=shm.buf)
  tvol[:,:,:] = a[:,:,:]
  sname = shm.name
  print("sname = ",sname)

  #p = Process(target=temperature, args=(tvol,T,sname, ))
  #p.start()
  #p.join()
  #print("tvol ",tvol[5,5,5],T)
  #shm.close()
  #shm.unlink()
  #exit(0)

  with Pool(processes=4) as pool:
    for i in range(0,int(1e3)+1):
      start = time.perf_counter()
      res1 = pool.apply_async(gas_density, (T,press,))
      res2 = pool.apply_async(tbb, (Sc,))
      res3 = pool.apply_async(sunage, (Sc,))
      end1 = time.perf_counter()
      print("end1",end1-start)

      #start = time.perf_counter()
      T   = res2.get()
      #end1 = time.perf_counter()
      rho = res1.get() 
      #end2 = time.perf_counter()
      Sc  = res3.get()
      #end3 = time.perf_counter()
      #print("end1",end1-start)
      #print("end2",end2-start)
      #print("end3",end3-start)

      start = time.perf_counter()
      res4 = pool.apply_async(temperature, (tvol.shape, tvol.dtype, T,sname,))
      x = res4.get()
      print("res4 ",time.perf_counter()-start)

  
      #if (i%1 == 0):
      print(i, T, rho, Sc, tvol[5,5,5] )

  pool.join()

  shm.close()
  shm.unlink()
