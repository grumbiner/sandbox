import numpy
print("need >=1.15.4 numpy version: ",numpy.__version__)

import scipy
print("need >= 1.2.0 scipy version: ",scipy.__version__)

import netCDF4
print("need >= 1.4.3 netCDF4 version: ",netCDF4.__version__)

import pyproj
print("need >= 2.2 pyproj version: ",pyproj.__version__)

# not version sensitive, part of rdhpcs standard download:
import matplotlib
print("matplotlib version: ",matplotlib.__version__)

import cartopy
print("hello cartopy v. ",cartopy.__version__)

# Should be available, at least from manual install:
import future
print("hello future v. ",future.__version__)
import rasterio
print("hello rasterio v. ",rasterio.__version__)
#doesn't look to be available by list at : 
#    https://rdhpcs-common-docs.rdhpcs.noaa.gov/wiki/index.php/Anaconda 
import configobj
print("hello configobj v. ",configobj.__version__)
# (but it seems that it is)

# Should be available, at least from manual install. But looks like it isn't:
import pillow
print("hello pillow v. ",pillow.__version__)
