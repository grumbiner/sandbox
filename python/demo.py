from latpt import *
from const import *

#print const.rpdg

x = latpt(39.0, -77.0)
y = latpt(41.0, -89.0)
x.show()
y.show()
print y.distance(x)

#   Trying to get a vector of latpts
#z = x
#z = np.arange( npoints, dtype=latpt )
#z[2].show()
#z.array(latpt)
#    Solution
npoints = 10
z = []
for i in range (0, npoints):
  z.append(x)


#Set up dummy locations:
for i in range (1,npoints,1):
  z[i-1] = latpt(i*9., i*15.)
#  z[i-1].show()


# Compute distance between successive points (demo of utility)
for i in range (0,npoints-2,1):
  print z[i].distance(z[i+1])
