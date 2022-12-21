#!/usr/bin/python3

import numpy as np

print("hello")

x = np.zeros( (360,180) )

#Must say print x, rather than just x, when in executing script vs. interactive
print(x)
print(x.shape)
print(x.ndim)
print(x.dtype.name)
print(x.itemsize)
print(x.size)
print(type(x))

x[2,2] = 3

print ("bye")
