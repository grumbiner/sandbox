from math import *

def tfreeze(salinity):
# Millero formula as given in Gill, 1982
 
  a1 = -0.0575
  a2 =  1.710523E-3
  a3 = -2.154996E-4

  return(salinity*(a1+a2*sqrt(salinity)+a3*salinity))

for i in range(0, 420):
  print(float(i)/10., tfreeze(float(i)/10.))
