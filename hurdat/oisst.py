from math import *
import numpy as np
import netCDF4



#-----------------------------------------------------------------
base="v2.1.nc/"
fbase="oisst-avhrr-v02r01."
dtg="20210620"
fname = base+fbase+dtg+".nc"

#-----------------------------------------------------------------
orig = netCDF4.Dataset(fname, "r") 

#  This stage takes some manual inspection of the above output file
#Dimensions:
name_of_x_direction = "lon" #longitudes, nx
name_of_y_direction = "lat" #latitudes, ny
# Names of special grids:
name_of_latitudes  = "lat"
name_of_longitudes = "lon"
name_of_landmask   = "land"     # can run without one
name_of_cellarea   = ""         # can run without one

# ------------- below here should be generic to all systems -------------------

nx = orig.dimensions[name_of_x_direction].size
ny = orig.dimensions[name_of_y_direction].size
lats = orig.variables[name_of_latitudes][:]
lons = orig.variables[name_of_longitudes][:]
print(nx , "nx")
print(ny , "ny")
print("lats max min ",lats.max(), lats.min() )
print("lons max min ",lons.max(), lons.min() )

try:
  tmask = orig.variables[name_of_landmask][:,:]
except:
  tmask = np.zeros((ny, nx))

try:
  tarea = orig.variables[name_of_cellarea][:,:]
except:
  #pi = 3.1415926535898
  tarea = np.cos(pi/180.*lats)
  print(tarea.shape,tarea.max(), tarea.min() )

# ----------------------
#also err, ice, anom
sst = orig.variables["sst"][:,:]
print("sst: ",sst.max(), sst.min() )
err = orig.variables["err"][:,:]
print("err: ",err.max(), err.min() )
ice = orig.variables["ice"][:,:]
print("ice: ",ice.max(), ice.min() )
anom = orig.variables["anom"][:,:]
print("anom: ",anom.max(), anom.min() )
orig.close()

