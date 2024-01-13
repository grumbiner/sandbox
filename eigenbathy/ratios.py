from math import *
import numpy as np

ny = 2160
nratios = int( sqrt(ny))

low = np.zeros((nratios))
high = np.zeros((nratios))

count = int(0)
for i in range (1,nratios):
  if ((ny%i) == 0):
    low[count] = i
    high[count] = ny/i
    count += 1

ratios = np.zeros((2*count))
for i in range (0,count):
  ratios[i+count] = high[count-i-1]
  ratios[i] = low[i]

for i in range (0,len(ratios)):
  print(i,ratios[i])

