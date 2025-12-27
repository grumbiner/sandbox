
from math import *

epsilon = 0.622

def lv(t):
  return 3151378. - 2386.*(t+273.15)

def ps(t):
  top = exp(34.494 - 4924.99/(t+237.1) )
  return top / pow(t+105, 1.57)

def latent(t):
  return epsilon*lv(t)*ps(t) / 1013.25e2

def sensible(t):
  return 1004.0*t

p0 = ps(26.0)
for i in range (0, 14):
  t = 26.0 + 0.5*i
  print(t, "{:.2f}".format(ps(t)), "{:.2f}".format(lv(t)),
           "{:.2e}".format(latent(t)), "{:.2e}".format(sensible(t)) )
 
# 1 kg TNT = 4.184 Megajoules
# 1 kiloton = 1000 tons = 1000* 1000 kg = 1 million kg
