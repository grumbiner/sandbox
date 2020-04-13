from math import *
import numpy as np

nx = 24
low = np.zeros((nx))
high = np.zeros((nx))
ratios = np.zeros((2*nx))
count = int(0)

for i in range (1,74):
  if ((5400%i) == 0):
    low[count] = i
    high[count] = 5400/i
    count += 1

for i in range (0,count):
  ratios[i+count] = high[count-i-1]
  ratios[i] = low[i]

for i in range (0,len(ratios)):
  print(i,ratios[i])

#print(low[23]/30.)
#print(ratios)
