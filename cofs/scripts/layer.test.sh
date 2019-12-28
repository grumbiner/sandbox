#!/bin/sh
#Script illustration of using the layer interpolator for
#  going from COFS native grid to cofs regular grid
#Robert Grumbine 6 January 2000

#Construct a binary file for input:
wgrib 2000010600f.agb | grep WTMP | grep 23140 | wgrib -i 2000010600f.agb -o a

#Simply transfer to regular grid, unformatted binary output:
./layer a binout

#Put on regular grid, grib output with date but defaults on other parameters
                   #yr  mon day hour
./layer a grib1.out 2000 01 06 00 

#As above, but specify precision
./layer a grib2.out 2000 01 06 00 0.001

#As above, also specify parameter number:
./layer a grib3.out  2000 01 06 00 0.001 80

#As above, with depth 
#-- note two arguments must be provided layer type and depth value
#   note too that, alas, this isn't behaving well for layer type 108
./layer a grib4.out  2000 01 06 00 0.001 80 128 12900

#In the event you have to use the OMB grib table for a parameter,
# you'll have to specify that whole set above (program is strictly incremental
# w.r.t. the argument list) plus the table number of 128.  Note that WTMP isn't
# really an OMB table, but for syntactic illustration:
./layer a grib5.out  2000 01 06 00 0.001 80 128 12900 128
