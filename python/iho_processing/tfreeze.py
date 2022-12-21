from math import *

def tf(salinity):
    a1 = -0.0575
    a2 =  1.710523e-3
    a3 = -2.154996e-4
  
    return( salinity*(a1+a2*sqrt(salinity)+a3*salinity))

for i in range (0,50):
  salinity = 34.0 + 0.1*i
  print(salinity, tf(salinity))
