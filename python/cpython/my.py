from Math import *

from math import *
def distance(lat1, lon1, lat2, lon2):
  earth_radius = 6371.2 #km
  rpdg = 3.1416 / 180.

  ab = (90.-lat1)*rpdg
  ac = (90.-lat2)   *rpdg
  bc = abs(lon1 - lon2)*rpdg

  arg = cos(ab)*cos(ac)+sin(ab)*sin(ac)*cos(bc)
  if (arg > 1):
    arg = 1
  if (arg < -1):
    arg = -1

  return earth_radius * acos(arg)


print "hello:", hello()
for i in range (0,1000*1000):
 x = myarcdis(4.0, 0.0, 6., 0.)

print "myarcdis(4.0, 0.0, 6., 0.):", myarcdis(4.0, 0.0, 6., 0.)

for i in range (0,1000*1000):
 x = distance(4.0, 0.0, 6., 0.)
