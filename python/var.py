import sys


import netCDF4

fname = "../../phyf006.tile1.nc"
fout = sys.stdout

orig  = netCDF4.Dataset(fname, "r")

#After https://stackoverflow.com/questions/13936563/copy-netcdf-file-using-python
print("Dimensions", file=fout)
for name, dim in orig.dimensions.items():
  print(name, dim, file=fout)

print("Variables, assumed gridded", file = fout)
for name, var in orig.variables.items():
  print("{:14s}".format(name), orig.variables[name][:].max(), orig.variables[name][:].min() )
  print( var.units )
  print( var.shape, len(var.shape) ) 


fout.close()
orig.close()

