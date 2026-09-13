import sqr_mod
import vectorize

import numpy as np

print(sqr_mod.square(2.))

npts = 30
u = np.zeros((npts), dtype=np.float32)
v = np.zeros((npts), dtype=np.float32)
r = np.zeros((npts), dtype=np.float32)
theta = np.zeros((npts), dtype=np.float32)
for i in range (0,npts):
  r[i] = i
  theta[i] = 3.1416/(i+1)

vectorize.vectorize(u, v, r, theta, npts)

for i in range(0,npts):
  print(i,u[i], v[i], r[i], theta[i])
