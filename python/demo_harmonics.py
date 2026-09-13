from math import *
import numpy as np

rng = np.random.default_rng()
x = rng.normal(loc = 0.0, scale = 0.5, size = 10592)
t = np.linspace(0,len(x),len(x))
#wave = np.sin(2.*pi*t/365.2422)
wave = np.sin(2.*pi*t/365.2422*2)
print(len(wave), len(t), len(x))
x += wave
print(x.mean(), 0.5/sqrt(10592) )

import scipy
y = scipy.fft.fft(x)
y *= 2./len(y)
z = scipy.fft.fftfreq(len(y), 1.0)
for i in range(0, len(y)):
  print(i, z[i], 1./z[i], abs(y[i]))

w = abs(y)
print(w.mean() )
