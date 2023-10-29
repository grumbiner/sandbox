import matplotlib.pyplot as plt
import matplotlib.colors as colors
import numpy as np

import pygrib

#from mpl_toolkits.basemap import Basemap
#from mpl_toolkits.basemap import shiftgrid

grbs = pygrib.open("seaice_gland5min.grib")
#print("grbs = ",grbs)

grb = grbs.select()[0]
#print("grb_ = ",grb)

icec = grb.values
#print(icec.max(), icec.min() )
#print(icec)


grbs = pygrib.open("ice5min.grib2.201810")
for i in range(0,31):
  grb = grbs.select()[i]
  icec = grb.values
  print(icec.max(), icec.min(), icec.mean() )

