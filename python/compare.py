import os
import sys

import numpy
import netCDF4 

stresses = ["uvel", "vvel", "strocnxT", "strocnyT", "stressp_1", "stressp_2", "stressp_3", "stressp_4", "stressm_1", "stressm_2", "stressm_3", "stressm_4", "stress12_1", "stress12_2", "stress12_3", "stress12_4"]

# For reading: -------------------------------------------
orig  = netCDF4.Dataset(sys.argv[1], 'r')
test  = netCDF4.Dataset(sys.argv[2], "r")

#After https://stackoverflow.com/questions/13936563/copy-netcdf-file-using-python
for name, var in orig.variables.items():

    delta = (orig.variables[name][:] - test.variables[name][:]) 
    print(name, orig.variables[name][:].max() )
    #name = aicen, vicen, vvel, uvel, ...
    if (delta.max() != 0.0 or delta.min() != 0.0):
      print(name, delta.max(), delta.min(), delta.sum(), flush=True)
    
    #if name in stresses:
    #  model_out.variables[name][:] = 0.0 
