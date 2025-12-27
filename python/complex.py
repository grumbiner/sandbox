import numpy as np

a = np.zeros((20),dtype=complex)
a[0] = 1+2j
print( a[0])
print( a[0].real)
print( a[0].imag)
print( np.absolute(a[0]) )   #magnitude
print( np.angle(a[0]) ) #phase
